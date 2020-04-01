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

(define users '(akash raghav bade malla))

(define (connect-users connection)
  (for-each (λ (user)
              (send-ws-message connection `(connect-user ,user))
              (recv/print connection))
            users))

(define (setup-game-room connection room-name)
  (send-ws-message connection `(make-room ,(car users) ,room-name))
  (recv/print connection)
  (for-each (λ (user)
              (send-ws-message connection `(join-room ,room-name ,user))
              (recv/print connection))
            (cdr users)))

(define (do-connection protocol)
  (printf "Connecting using protocol ~a...\n" protocol)
  (define c (ws-connect (string->url "ws://localhost:8081/test")
                        #:protocol protocol))

  (connect-users c)
  
  (setup-game-room c 'my-room)

  (send-ws-message c '(get-room-details akash))
  (recv/print c)

  (send-ws-message c '(start-game my-room))
  (recv/print c)
  
  (ws-close! c))

(do-connection 'rfc6455)
