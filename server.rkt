#lang racket/base
;; Demonstrates the ws-serve interface.
;; Public Domain.

(module+ main
  (require net/rfc6455)

  (require racket/cmdline)
  (require racket/control)
  (require racket/port)
  (require racket/serialize)
  (require racket/match)

  (define port 8081)
  (define idle-timeout #f)
  (command-line #:once-each
                ["--timeout" SECONDS "Set per-connection idle timeout"
                 (set! idle-timeout (string->number SECONDS))]
                ["--port" PORT "Set service port"
                          (set! port (string->number PORT))])

  (define (connection-handler c state)
    (displayln  (format "handler called ~a" c))
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

  ;; Users

  ;; represents a connected user
  ;; connection => connection corres to the user connected
  ;; email => email of the users
  (struct user (connection email))

  (define *connected-users* (make-hash))

  (define (connect-new-user connection email)
    (hash-set! *connected-users* email (user connection email)))

  
  ;; Game rooms

  ;; game room is always public
  ;; name => is the game romm name
  ;; members => list of members who have joined the game room and are online
  (serializable-struct game-room (host name members))

  ;; hash from host email to game-room struct
  (define *game-rooms* (make-hash))

  (define (add-game-room host-email room-name)
    (hash-set! *game-rooms* host-email (game-room host-email room-name #f))
    '(room-created))

  (define (get-room-details host-email)
    (serialize (hash-ref *game-rooms* host-email)))

  (define (join-room message)
    (match message
      ((list _ room-name email)
       (match (findf (lambda (room)
                       (equal? room-name (game-room-name room)))
                     (hash-values *game-rooms*))
         ((game-room host name members)
          (cond
            ((member email (hash-keys *connected-users*))
             (hash-update! *game-rooms*
                           host
                           (lambda (room-details)
                             (game-room host name (cons email members)))))
            (else (error "user not connected"))))
         (_ (error "room not found"))))
      (_ (error "message is invalid"))))

  ;; accepted messages:
  ;; 
  ;; (connect-user <email>) => connect a new user to the system
  ;;
  ;; (make-room <host-email> <room-name>) => create a new game room
  ;;
  ;; (join-room <room-name> <email>) => adds email to the game room
  ;;
  ;; (get-room-details <host-email>) => get game room data associated with host email
  ;;
  ;; (get-active-rooms) => list of active rooms
  ;; returns: (listof <room-name>)
  ;;
  ;; 
  (define (dispatch connection message)
    (let ((message (read (open-input-string message))))
      (case (car message)
        ((connect-user) (connect-new-user connection (cadr message)))
        ((make-room) (add-game-room (cadr message) (caddr message)))
        ((join-room) (join-room message))
        ((get-room-details) (get-room-details (cadr message)))
        (else 'invalid-request))))

  (when idle-timeout
    (ws-idle-timeout idle-timeout))
  ;; (define stop-service
  ;;   (ws-serve connection-handler #:port port))

  ;; (printf "Server running. Hit enter to stop service.\n")
  ;; (void (read-line))
  ;; (stop-service)
  )

(dispatch #f "(connect-user ashakdwipeea@gmail.com)")
(dispatch #f "(make-room ashakdwipeea@gmail.com MyRoom)")

(dispatch #f "(connect-user raghav@gmail.com)")
(dispatch #f "(join-room MyRoom raghav@gmail.com)")
(dispatch #f "(get-room-details ashakdwipeea@gmail.com)")
(game-room-members (hash-ref *game-rooms* 'ashakdwipeea@gmail.com))

