#lang racket

(require net/rfc6455)
(require net/url)

(require racket/port)
(require racket/serialize)

(require "./game.rkt")
(require "./server.rkt")

(provide connect-to-ws
         connect-user
         get-active-rooms
         create-room
         join-room)

(define +ws-url+ "ws://localhost:8080/test")
(define protocol 'rfc6455)

(define connect-to-ws
  (λ ()
    (ws-connect (string->url +ws-url+) #:protocol protocol)))

(define (recv/print c)
  (let ((msg (ws-recv c)))
    (displayln (format "got message ~a " msg))
    msg))

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
  (lambda (connection)
    (let ((data (ws-recv connection)))
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

    (define select-card
      (λ (hand cards-played-in-round first-suit)
        (findf (λ (card)
                (valid-card? card first-suit cards-played-in-round #f hand))
               hand)))

    (define connection (user-connection user))
    
    (let loop ((client-state (default-state user))
               (server-msg (receive-data connection)))
      (if server-msg
          (case (car server-msg)
            ((hand)
             (let ((new-cards (caddr server-msg)))
               (loop (struct-copy state client-state
                                  (hand (append (state-hand client-state) new-cards)))
                     (receive-data connection))))
            ((request-bid)
             (let ((current-bid (cadr server-msg)))
               (send-data (state-user client-state)
                          (list 'put-bid
                                (if (< current-bid 27) (+ current-bid 1) 'pass)))
               (loop client-state
                     (receive-data connection))))
            ((bid-result)
             (match (cadr server-msg)
               ((cons set-bid-value player-index)
                (loop (struct-copy state client-state
                                   (bid-value set-bid-value))
                      (receive-data connection)))))
            ((choose-trump)
             (begin (send-data (state-user client-state) `(selected-trump diamond))
                    (loop (struct-copy state client-state
                                       (selected-trump 'diamond))
                          (receive-data connection))))
            ((play-card)
             (let* ((cards-played (cdadr server-msg))
                    (game-state (cdaddr server-msg))
                    (hand (state-hand client-state))
                    (card (select-card hand
                                       cards-played
                                       (cdr (assoc 'first-suit game-state)))))
               (send-data (state-user client-state)
                          `(card-played ,card))
               (loop (struct-copy state client-state
                                  (hand (remove card hand)))
                     (receive-data connection))))
            (else
             (begin (displayln (format "unhandled message ~a" server-msg))
                    (loop client-state (receive-data connection)))))
          (loop client-state (receive-data connection))))))

(define get-active-rooms
  (lambda (connection)
    (send-ws-message connection '(get-active-rooms))
    (receive-data connection)))

(define connect-user
  (lambda (connection user-id)
    (send-ws-message connection `(connect-user ,user-id))
    (receive-data connection)))

(define create-room
  (lambda (connection user-id room-name)
    (send-ws-message connection `(make-room ,user-id ,room-name))
    (receive-data connection)))

(define join-room
  (lambda (connection room-name user-email)
    (send-ws-message connection `(join-room ,room-name ,user-email))
    (receive-data connection)
    (user user-email connection (make-channel))))

;; we create a new connection for each user
(define connect-users
  (λ ()
    (map (λ (user)
           (let ((connection (connect-to-ws)))
             (connect-user connection user)
             connection))
         users)))

(define (setup-game-room connections room-name)
  ;; the first user is the host
  (let ([host-connection (car connections)]
        [host-id (car users)])
    (send-ws-message host-connection `(make-room ,host-id ,room-name))
    (recv/print (car connections))
    (cons (user host-id host-connection (make-channel))
          (map (λ (id connection) (join-room connection room-name id))
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


;; (define connection (connect-to-ws))

;; (define user-email 'bot-1)

;; (define active-rooms  (get-active-rooms connection))
;; (displayln (format "active rooms ~a" active-rooms))
;; (define choosen-room-name (cdr (assoc 'name (car active-rooms))))

;; (connect-user connection user-email)
;; (join-room connection choosen-room-name user-email)

;; (get-active-rooms connection)
