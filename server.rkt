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

  (module user racket
    (provide connect-new-user
             get-connected-users-email
             user
             user-connection
             user-email
             get-user-by-email)

    ;; represents a connected user
    ;; connection => connection corres to the user connected
    ;; email => email of the users
    (struct user (connection email))

    (define *connected-users* (make-hash))

    (define (connect-new-user connection email)
      (hash-set! *connected-users* email (user connection email)))

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
    (require (submod ".." user))
    (require (submod ".." room))
    (require "./game.rkt")

    (struct game-data (players deck game-state))

    ;; contains running games keyed against room name
    (define *running-games* (make-hash))

    (define +max-players+ 4)

    (define distribute-cards-to-players
      (λ (players)
        (match/values (distribute-cards deck 4)
          [(player-hands remaining-deck)
           (map (λ (player-hand player)
                  (let ([conn (user-connection player)])
                    (cond
                      [(ws-conn-closed? conn) (error "connection closed for player" player)]
                      [else (ws-send! conn
                                      (with-output-to-string (λ () (write `(hand ,player-hand)))))])))
                player-hands
                players)
           remaining-deck])))

    
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
                                 (ws-conn-closed? (user-connection (get-user-by-email member))))
                               members)))
              (let ([deck (distribute-cards-to-players (cons host members))])
                (hash-set! *running-games*
                           name
                           (game-data members deck 'init))
                'game-started)]
            
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
            (add-user-to-game-room room email))
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
  (define (dispatch connection message)
    (let ((message (read (open-input-string message))))
      (case (car message)
        ;; user messages
        ((connect-user) (connect-new-user connection (cadr message)))

        ;; room messages
        ((make-room) (add-game-room (cadr message) (caddr message)))
        ((join-room) (join-room message))
        ((get-room-details) (get-room-details (cadr message)))

        ;; game messages
        ((start-game) (start-game (cadr message)))

        ;; catch all
        (else 'invalid-request))))

  (when idle-timeout
    (ws-idle-timeout idle-timeout))
  (define stop-service
    (ws-serve connection-handler #:port port))

  (printf "Server running. Hit enter to stop service.\n")
  (void (read-line))
  (stop-service))
