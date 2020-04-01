#lang racket/base
;; Demonstrates the ws-serve interface.
;; Public Domain.

(module+ main
  (require net/rfc6455)

  (require racket/cmdline)
  (require racket/control)
  (require racket/port)
  (require racket/serialize)

  (define port 8081)
  (define idle-timeout #f)
  (command-line #:once-each
                ["--timeout" SECONDS "Set per-connection idle timeout"
                 (set! idle-timeout (string->number SECONDS))]
                ["--port" PORT "Set service port"
                          (set! port (string->number PORT))])

  (define (connection-handler c state)
    (let loop ()
      (sync (handle-evt (alarm-evt (+ (current-inexact-milliseconds) 1000))
                        (lambda _
                          (ws-send! c "Waited another second")
                          (loop)))
            (handle-evt (ws-recv-evt c #:payload-type 'text)
                        (lambda (m)
                          (unless (eof-object? m)
                            (let ((data (dispatch c m)))
                              (ws-send! c (with-output-to-string (λ () (write data))))
                              (loop)))))))
    (ws-close! c))

  (serializable-struct game-room (host members))

  ;; hash from host email to game-room struct
  (define *game-rooms* (make-hash))

  (define (add-game-room host-email)
    (hash-set! *game-rooms* host-email (game-room host-email #f))
    '(room-created))

  (define (get-room-details host-email)
    (serialize (hash-ref *game-rooms* host-email)))

  (define (dispatch connection message)
    (let ((message (read (open-input-string message))))
      (case (car message)
        ((make-room) (add-game-room (cdr message)))
        ((get-room-details) (get-room-details (cdr message)))
        (else 'invalid-request))))

  (when idle-timeout
    (ws-idle-timeout idle-timeout))
  (define stop-service
    (ws-serve #:port port connection-handler))

  (printf "Server running. Hit enter to stop service.\n")
  (void (read-line))
  (stop-service))
