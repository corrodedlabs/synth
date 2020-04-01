#lang racket
;; Demonstrates the ws-connect interface.
;; Public Domain.

(require net/rfc6455)
(require net/url)

(require racket/port)
(require racket/serialize)

(define (recv/print c)
  (displayln (format "got message ~a " (ws-recv c))))

(define send-ws-message
  (λ (connection data)
    (ws-send! connection (with-output-to-string (λ () (write data))))))

(define (do-connection protocol)
  (printf "Connecting using protocol ~a...\n" protocol)
  (define c (ws-connect (string->url "ws://localhost:8081/test")
                        #:protocol protocol))

  (send-ws-message c '(connect-user akash my-room))
  (recv/print c)
  
  (send-ws-message c '(connect-user raghav my-room))
  (recv/print c)

  (send-ws-message c '(make-room akash my-room))
  (recv/print c)

  (send-ws-message c '(join-room my-room raghav))
  (recv/print c)

  (send-ws-message c '(get-room-details akash))
  (recv/print c)

  (ws-close! c))

(do-connection 'rfc6455)
