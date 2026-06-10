#lang racket

(require rackunit
         rackunit/text-ui
         "./server.rkt"
         "./client.rkt")

(define *service* #f)

;; off the default port so tests don't collide with a dev server
(define test-port 18081)

(define user1-id 'user1-email)
(define room-name 'my-test-room)

(define-test-suite game-tests
  (around
   ;; start server before tests
   (begin (displayln "starting server")
          (putenv "GAME-PORT" (number->string test-port))
          (set! *service* (start-service test-port))
          (sleep 3))

   ;; test cases
   (test-begin
     (let ((connection (connect-to-ws)))
       ;; replies are tagged: (active-rooms <rooms>)
       (check-pred null? (cadr (get-active-rooms connection)))

       (connect-user connection user1-id)

       (check-equal? (create-room connection user1-id room-name)
                     'room-created)

       (let ((rooms (cadr (get-active-rooms connection))))
         (check-equal? (length rooms) 1))))

   ;; stop server after tests
   (*service*)))

(run-tests game-tests)
