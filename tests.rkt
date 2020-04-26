#lang racket

(require rackunit
         rackunit/text-ui
         "./server.rkt"
         "./client.rkt")

(define *service* #f)

(define user1-id 'user1-email)
(define room-name 'my-test-room)

(define-test-suite game-tests
  (around
   ;; start server before tests
   (begin (displayln "starting server")
          (set! *service* (start-service))
          (sleep 3))

   ;; test cases
   (test-begin
     (let ((connection (connect-to-ws)))
       (check-pred null? (get-active-rooms connection))

       (connect-user connection user1-id)

       (check-equal? (create-room connection user1-id room-name)
                     'room-created)

       (let ((rooms (get-active-rooms connection)))
         (check-equal? (length rooms) 1))))

   ;; stop server after tests
   (*service*)))

(run-tests game-tests)
