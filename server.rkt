#lang racket

(require net/rfc6455)

(require racket/cmdline)
(require racket/control)
(require racket/port)
(require racket/serialize)
(require racket/match)

(provide start-service)

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
    (sync ;; (handle-evt (alarm-evt (+ (current-inexact-milliseconds) 1000))
          ;;             (lambda _
          ;;               (ws-send! c "Waited another second")
          ;;               (loop)))
          (handle-evt (ws-recv-evt c #:payload-type 'text)
                      (lambda (m)
                        (displayln (format "m is ~a" m))
                        (if (eof-object? m)
                            (loop)
                            (begin
                              (thread
                               (λ ()
                                 (let ((data (dispatch c m)))
                                   (ws-send! c (with-output-to-string (λ () (write data)))))))
                              (loop)))))))
  (ws-close! c))

;; Users

(module user racket
  (provide connect-new-user
           get-connected-users-email
           user
           user-connection
           user-email
           user-hand
           user-comm
           get-user-by-email)

  ;; represents a connected user
  ;; connection => connection corres to the user connected
  ;; email => email of the users
  ;; hand => list of cards that the player has
  ;; comm => channel for synchronisation
  (struct user (connection email hand comm))

  (define *connected-users* (make-hash))

  (define (connect-new-user connection email)
    (hash-set! *connected-users* email (user connection email '() (make-channel))))

  (define (get-user-by-email email)
    (hash-ref *connected-users* email #f))
  
  (define get-connected-users-email
    (λ () (hash-keys *connected-users*))))


;; Game rooms

(module room racket
  (require racket/serialize)
  (require (submod ".." user))
  
  (provide add-game-room
           get-room-details
           find-room-by-name
           add-user-to-game-room
           game-room)
  
  ;; game room is always public
  ;; name => is the game romm name
  ;; members => list of members who have joined the game room and are online
  (serializable-struct game-room (host name members))

  ;; hash from host email to game-room struct
  (define *game-rooms* (make-hash))

  (define (add-game-room host-email room-name)
    (hash-set! *game-rooms*
               host-email
               (game-room (get-user-by-email host-email) room-name '()))
    'room-created)

  (define (get-room-details host-email)
    (serialize (game-room-members (hash-ref *game-rooms* host-email))))

  (define find-room-by-name
    (λ (room-name)
      (findf (lambda (room)
               (equal? room-name (game-room-name room)))
             (hash-values *game-rooms*))))

  ;; adds user to room's member controlled by host
  (define add-user-to-game-room
    (λ (room user)
      (match room
        [(game-room host name members)
         (hash-update! *game-rooms*
                       (user-email host)
                       (λ (room-details)
                         (game-room host name (cons user members))))]))))


;; Game

(module game racket
  (provide start-game)

  (require net/rfc6455)
  (require racket/control)
  
  (require (submod ".." user))
  (require (submod ".." room))
  (require "./game.rkt")

  (struct game-data (players deck game-state trump-suit))

  ;; contains running games keyed against room name
  (define *running-games* (make-hash))

  (define +max-players+ 4)

  ;; send data over connection
  (define send-datum
    (λ (player data)
      (ws-send! (user-connection player)
                (with-output-to-string (λ () (write data))))))

  (define send-datum-to-all
    (λ (players data)
      (for-each (λ (player)
                  (send-datum player data))
                players)))

  (define distribute-cards-to-players
    (case-lambda
      [(players) (distribute-cards-to-players players initial-deck)]
      [(players deck)
       (match/values (distribute-cards deck 4)
         [(new-cards-for-players remaining-deck)
          (let ([players1 (map (λ (new-cards player)
                                (let ([conn (user-connection player)])
                                  (cond
                                    [(ws-conn-closed? conn)
                                     (error "connection closed for player" player)]
                                    [else
                                     (begin
                                       (send-datum player
                                                   `(hand ,(user-email player) ,new-cards))
                                       (struct-copy user player
                                                    (hand (append (user-hand player)
                                                                  new-cards))))])))
                              new-cards-for-players
                              players)])
            (cons players1 remaining-deck))])]))

 
  (define receive-datum (compose1 channel-get user-comm))

  ;; performs the bidding process and returns the final bid value
  (define perform-bidding
    (λ (players)
      (start-bidding (λ (player-index current-bid-value)
                       (let* ((player (list-ref players player-index)))
                         (send-datum player `(request-bid ,current-bid-value))
                         (caddr (receive-datum player))))
                     (λ (player-index error-msg)
                       (send-datum (list-ref players player-index) `(error ,error-msg))))))

  
  ;; player index is the index of the player who won the bid and will choose the trump
  (define choose-trump-suit
    (λ (players player-index)
      (let* ((bidder (list-ref players player-index)))
        (send-datum bidder '(choose-trump))
        (let ([selected-trump-suit (caddr (receive-datum bidder))])
          (send-datum-to-all players '(trump-selected))
          selected-trump-suit))))
  
  (define start-game
    (λ (room-name)
      (match (find-room-by-name room-name)
        [(game-room host name members)
         (cond
           [(hash-ref *running-games* name #f)
            'game-already-started]
           
           [(and (equal? (length members)
                         ;; host should always be there
                         (- +max-players+ 1))
                 (not (ormap (λ (member)
                               (ws-conn-closed? (user-connection member)))
                             members)))
            (let* ((players (cons host members))
                   (players+deck (distribute-cards-to-players players))
                   (bid-result (perform-bidding players))
                   (trump-suit (choose-trump-suit players (cdr bid-result))))
              (begin
                (for-each (λ (player)
                            (send-datum player `(bid-result ,bid-result)))
                          players)
                (match (distribute-cards-to-players (car players+deck) (cdr players+deck))
                  [(cons players deck)
                   (displayln "huytrer")
                   (let ((points-won
                          (play-game (map user-hand players)
                                     trump-suit
                                     (λ (player-index cards-played-in-round game-state)
                                       (displayln (format "waiting for player ~a " player-index))
                                       (let ((player (list-ref players player-index)))
                                         (send-datum player
                                                     `(play-card . ((cards-played . ,cards-played-in-round)
                                                                    (game-state . ,game-state))))
                                                     
                                         (caddr (receive-datum player)))))))
                     (send-datum-to-all players `(points-won ,points-won)))
                   ;; (hash-set! *running-games* name (game-data players deck 'init trump-suit))
                   'game-started]
                  (else (error "fuck")))))]
           
           [else 'room-not-ready])]))))

(require 'user)
(require 'room)
(require 'game)

(define (join-room message)
  (match message
    ((list _ room-name email)
     (let ([room (find-room-by-name room-name)])
       (cond
         ((member email (get-connected-users-email))
          (add-user-to-game-room room (get-user-by-email email)))
         (else (error "user not connected")))))
    (_ (error "message is invalid"))))

;; accepted messages:
;;
;; User messages:
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
;; Games messages:
;;
;; (start-game <room-name>)
;;
;; the following messages are sent to the channel of <user-email>'s user
;; and are handled via the game logic
;;
;; (put-bid <user-email> <bid-value>) => make a bid
;; [selected-trump <user-email> <trump-suit>] => select a trump suit
;; 
(define (dispatch connection message)
  (let ((message (read (open-input-string message))))
    (displayln (format "message is ~a" message))
    (case (car message)
      ;; user messages
      ((connect-user) (connect-new-user connection (cadr message)))

      ;; room messages
      ((make-room) (add-game-room (cadr message) (caddr message)))
      ((join-room) (join-room message))
      ((get-room-details) (get-room-details (cadr message)))

      ;; game messages
      ((start-game)
       (begin (thread (λ () (start-game (cadr message))))
       '(game-started)))

      ((put-bid selected-trump card-played)
       (begin (displayln (format "got card from ~a" message))
       (channel-put (user-comm (get-user-by-email (cadr message))) message)
       '(done)))

      ;; catch all
      (else 'invalid-request))))

(when idle-timeout
  (ws-idle-timeout idle-timeout))

(define start-service
  (λ ()
    (ws-serve connection-handler #:port port)))


;; (printf "Server running. Hit enter to stop service.\n")
;; (void (read-line))
;; (stop-service)
