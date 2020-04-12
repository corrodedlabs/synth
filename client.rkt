#lang racket

(require net/rfc6455)
(require net/url)

(require racket/port)
(require racket/serialize)

(require "./game.rkt")
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

(struct user (id connection comm))
(struct state (user hand bid-value selected-trump))

(define print-state
  (lambda (obj)
    (match obj
      ((state _ hand bid-value selected-trump)
       (displayln (format "state is ~a" (list hand bid-value selected-trump)))))))

(define default-state
  (λ (user) (state user '() #f #f)))

(define receive-data
  (lambda (user)
    (let ((data (ws-recv (user-connection user))))
      (if (or (eof-object? data) (equal? "#<void>" data))
          #f
          (begin (displayln (format "received data is ~a" data))
                 (read (open-input-string data)))))))

(define send-data
  (lambda (user data)
    (send-ws-message (user-connection user)
                     (append (list (car data) (user-id user))
                             (cdr data)))))

(define client-lambda
  (λ (user)
    (let loop ((client-state (default-state user))
               (server-msg (receive-data user)))
      (if server-msg
          (case (car server-msg)
            ((hand)
             (let ((new-cards (caddr server-msg)))
               (loop (struct-copy state client-state
                                  (hand (append (state-hand client-state) new-cards)))
                     (receive-data user))))
            ((request-bid)
             (let ((current-bid (cadr server-msg)))
               (send-data (state-user client-state)
                          (list 'put-bid
                                (if (< current-bid 27) (+ current-bid 1) 'pass)))
               (loop client-state (receive-data user))))
            ((bid-result)
             (match (cadr server-msg)
               ((cons set-bid-value player-index)
                (loop (struct-copy state client-state
                                   (bid-value set-bid-value))
                      (receive-data user)))))
            ((choose-trump)
             (begin (send-data (state-user client-state) `(selected-trump diamond))
                    (loop (struct-copy state client-state
                                       (selected-trump 'diamond))
                          (receive-data user))))
            ((play-card)
             (let ((cards-played (cdadr server-msg))
                   (game-state (cdddr server-msg)))
               (send-data (state-user client-state)
                          `(card-played ,(car (state-hand client-state))))
               (loop client-state (receive-data user))))
            (else
             (begin (displayln (format "unhandled message ~a" server-msg))
                    (loop client-state (receive-data user)))))
          (loop client-state (receive-data user))))))

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
  (let ([host-connection (car connections)]
        [host-id (car users)])
    (send-ws-message host-connection `(make-room ,host-id ,room-name))
    (recv/print (car connections))
    (cons (user host-id host-connection (make-channel))
          (map (λ (id connection)
                 (send-ws-message connection `(join-room ,room-name ,id))
                 (recv/print connection)
                 (user id connection (make-channel)))
               (cdr users)
               (cdr connections)))))

(define simulate-game
  (λ ()
    (displayln "now simulating game")
    (let* ((connections (connect-users))
           (users (setup-game-room connections 'my-room)))
      (send-ws-message (car connections) '(start-game my-room))
      ;; (recv/print (car connections))
      (for-each (lambda (user)
                  (thread (lambda ()
                            (client-lambda user))))
                users)
      ;; (for-each ws-close! connections)
      )))

(define stop-service (start-service))
(sleep 3)
(simulate-game)
(void (read-line))
(stop-service)
