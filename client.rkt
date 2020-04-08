#lang racket

(require net/rfc6455)
(require net/url)

(require racket/port)
(require racket/serialize)

(require "./server.rkt")

(define +ws-url+ "ws://localhost:8081/test")
(define protocol 'rfc6455)

(define connect-to-ws
  (λ ()
    (ws-connect (string->url +ws-url+) #:protocol protocol)))

(define (recv/print c)
  (displayln (format "got message ~a " (ws-recv c))))

(define write-data-to-string
  (λ (data) (with-output-to-string (λ ()  (write data)))))

(define send-ws-message
  (λ (connection data)
    (ws-send! connection (write-data-to-string data))))

(define users '(akash raghav bade malla))

;; we create a new connection for each user
(define connect-users
  (λ ()
    (map (λ (user)
           (let ((connection (connect-to-ws)))
             (send-ws-message connection `(connect-user ,user))
             (recv/print connection)
             connection))
         users)))

(define (setup-game-room connections room-name)
  ;; the first user is the host
  (send-ws-message (car connections) `(make-room ,(car users) ,room-name))
  (recv/print (car connections))
  (for-each (λ (user connection)
              (send-ws-message connection `(join-room ,room-name ,user))
              (recv/print connection))
            (cdr users)
            (cdr connections)))

(define simulate-game
  (λ ()
    (displayln "now simulating game")
    (let ((connections (connect-users)))
      (setup-game-room connections 'my-room)
      (send-ws-message (car connections) '(start-game my-room))
      (begin (for-each recv/print connections))
      (send-ws-message (car connections) '(put-bid 28))
      (for-each recv/print connections)
      (for-each ws-close! connections))))

(define stop-service (start-service))
(sleep 3)
(simulate-game)
(stop-service)
