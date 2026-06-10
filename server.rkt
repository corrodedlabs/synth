#lang racket

(require net/rfc6455)

(require racket/async-channel)
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
                        (set! port (string->number PORT))
                        ;; bots in client.rkt dial back into this server
                        (putenv "GAME-PORT" PORT)])

(define (connection-handler c state)
  (displayln  (format "handler called ~a" c))
  (let loop ()
    (sync
     (handle-evt
      (ws-recv-evt c #:payload-type 'text)
      (lambda (m)
        (cond
          ;; eof means the peer is gone: clean up instead of spinning
          ((eof-object? m)
           (displayln "connection closed")
           (handle-disconnect c))
          (else
           (thread
            (λ ()
              (displayln (format "m is ~a" m))
              (let ((data (dispatch c m)))
                (with-handlers ((exn:fail? void)) ; peer may be gone by now
                  (ws-send! c
                            (with-output-to-string (λ () (write data))))))))
           (loop)))))))
  (with-handlers ((exn:fail? void))
    (ws-close! c)))

;; Users

(module user racket
  (provide connect-new-user
           get-connected-users-email
           user
           user-connection
           user-email
           user-hand
           user-comm
           get-user-by-email
           remove-user
           users-with-connection
           bot-user?
           release-user
           create-bot-user)

  (require net/rfc6455)
  (require racket/async-channel)
  (require "./client.rkt")

  ;; represents a connected user
  ;; connection => connection corres to the user connected
  ;; email => email of the users
  ;; hand => list of cards that the player has
  ;; comm => async channel for game messages (puts never block the listener)
  (struct user (connection email hand comm pic-url))

  (define *connected-users* (make-hash))

  (define (connect-new-user connection email pic-url)
    (hash-set! *connected-users* email
               (user connection email '() (make-async-channel) pic-url)))

  (define (get-user-by-email email)
    (hash-ref *connected-users* email #f))

  (define get-connected-users-email
    (λ () (hash-keys *connected-users*)))

  (define (remove-user email)
    (hash-remove! *connected-users* email))

  ;; all users registered over a given websocket connection
  (define (users-with-connection connection)
    (filter (λ (u) (eq? (user-connection u) connection))
            (hash-values *connected-users*)))

  (define (bot-user? u)
    (regexp-match? #rx"^bot-[0-9]+$" (format "~a" (user-email u))))

  ;; closes a user's connection (bots only have a server-side one) and
  ;; forgets them entirely
  (define (release-user u)
    (with-handlers ((exn:fail? void))
      (ws-close! (user-connection u)))
    (remove-user (user-email u)))

  (define *bot-counter* (box 0))

  ;; bots dial back into the server as clients. Their sockets and AI threads
  ;; are created while dispatching a message from the host's websocket, and
  ;; web-server shuts down each connection's custodian on disconnect — so
  ;; without a detached custodian every bot would die with the host.
  (define *bot-custodian* (make-custodian))

  (define create-bot-user
    (lambda ()
      (parameterize ((current-custodian *bot-custodian*))
        (let* ((n (add1 (unbox *bot-counter*)))
               (bot-name (string->symbol (format "bot-~a" n)))
               (connection (connect-to-ws)))
          (set-box! *bot-counter* n)
          (connect-user connection bot-name)
          (spawn-bot bot-name connection)
          (get-user-by-email bot-name))))))


;; Game rooms

(module room racket
  (require racket/serialize)
  (require (submod ".." user))
  
  (provide add-game-room
           get-active-rooms
           get-room-details
           find-room-by-name
           add-user-to-game-room
           add-bot-to-game-room
           room-update-members!
           close-room!
           rooms-with-member
           game-room)
  
  ;; game room is always public
  ;; name => is the game romm name
  ;; members => list of members who have joined the game room and are online
  (serializable-struct game-room (host name members))

  (define game-room->list
    (lambda (room)
      (match room
        ((game-room host name members)
         `((host . ,(user-email host))
           (name . ,name)
           (members . ,(map user-email members)))))))

  ;; hash from host email to game-room struct
  (define *game-rooms* (make-hash))

  (define (add-game-room host-email room-name)
    (let ((host-user (get-user-by-email host-email)))
      (hash-set! *game-rooms*
                 host-email
                 (game-room host-user room-name (list host-user)))
      'room-created))

  (define (get-room-details host-email)
    (game-room->list (hash-ref *game-rooms* host-email)))

  (define (get-active-rooms)
    (map game-room->list (hash-values *game-rooms*)))

  (define find-room-by-name
    (λ (room-name)
      (findf (lambda (room)
               (equal? room-name (game-room-name room)))
             (hash-values *game-rooms*))))

  ;; adds user to room's member controlled by host
  ;; returns the members of the game room
  (define add-user-to-game-room
    (λ (room user)
      (match room
        [(game-room host name members)
         (let ((new-members (cons user members)))
           (hash-update! *game-rooms* (user-email host)
                         (λ (room-details)
                           (game-room host name new-members)))
           new-members)])))

  (define add-bot-to-game-room
    (lambda (room-name)
      (add-user-to-game-room (find-room-by-name room-name) (create-bot-user))))

  (define (room-update-members! room new-members)
    (match room
      ((game-room host name _)
       (hash-set! *game-rooms* (user-email host) (game-room host name new-members)))))

  (define (close-room! room)
    (match room
      ((game-room host _ _) (hash-remove! *game-rooms* (user-email host)))))

  ;; every room a user (by email, symbol or string) is currently seated in
  (define (rooms-with-member email)
    (define (same? a b) (equal? (format "~a" a) (format "~a" b)))
    (filter (λ (room)
              (match room
                ((game-room _ _ members)
                 (findf (λ (m) (same? (user-email m) email)) members))))
            (hash-values *game-rooms*))))



;; Game

(module game racket
  (provide start-game
           send-datum
           send-datum-to-all)

  (require net/rfc6455)
  (require racket/async-channel)
  (require racket/control)

  (require (submod ".." user))
  (require (submod ".." room))
  (require "./game.rkt")

  (struct game-data (players deck game-state trump-suit))

  ;; contains running games keyed against room name
  (define *running-games* (make-hash))

  (define +max-players+ 4)

  ;; game points a team needs to take the match; the MATCH-TARGET environment
  ;; variable shortens matches for tests (same pattern as GAME-PORT)
  (define (match-target)
    (let ([configured (getenv "MATCH-TARGET")])
      (or (and configured (string->number configured)) +match-target+)))

  ;; send data over connection; a dead member must not break a broadcast
(define send-datum
  (λ (player data)
    (displayln (format "sending datum ~a to player ~a" (user-email player) data))
    (with-handlers ((exn:fail? (λ (e)
                                 (displayln (format "send to ~a failed: ~a"
                                                    (user-email player)
                                                    (exn-message e))))))
      (ws-send! (user-connection player)
                (with-output-to-string (λ () (write data)))))))

(define send-datum-to-all
  (λ (players data)
    (for-each (λ (player)
                (send-datum player data))
              players)))

  (define distribute-cards-to-players
    (λ (players deck)
      (match/values
          (distribute-cards deck 4)
        [(new-cards-for-players remaining-deck)
         (let ([players1
                (map (λ (new-cards player)
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
           (cons players1 remaining-deck))])))

  ;; next message of the expected kind from this player's channel; other
  ;; messages are dropped. Raises if any watched player's socket dies while
  ;; we wait — not just the one we are waiting on, so a player who leaves
  ;; or drops while someone else is thinking aborts the game within a
  ;; second instead of when their own turn comes round.
  (define receive-datum
    (λ (player expected-tag (watched (list player)))
      (let loop ()
        (let ([message (sync/timeout 1 (user-comm player))])
          (cond
            [(not message)
             (let ([gone (findf (λ (p) (ws-conn-closed? (user-connection p)))
                                watched)])
               (if gone
                   (error 'receive-datum
                          (format "player ~a disconnected" (user-email gone)))
                   (loop)))]
            [(and (pair? message) (equal? (car message) expected-tag))
             message]
            [else
             (displayln (format "ignoring unexpected ~a from ~a"
                                message (user-email player)))
             (loop)])))))

  ;; runs the auction; first-seat opens. returns (cons winning-bid winner-index)
  (define perform-bidding
    (λ (players first-seat)
      (start-bidding (λ (player-index min-bid)
                       (let ((player (list-ref players player-index)))
                         (send-datum-to-all players `(turn ,player-index))
                         (send-datum player `(request-bid ,min-bid))
                         (caddr (receive-datum player 'put-bid players))))
                     (λ (player-index bid-value)
                       (send-datum-to-all players
                                          `(bid-placed ,player-index ,bid-value)))
                     (λ (player-index error-msg offending-value)
                       (send-datum (list-ref players player-index)
                                   `(error ,error-msg)))
                     #:first-player first-seat)))

  ;; player index is the index of the player who won the bid and will choose the trump
  (define choose-trump-suit
    (λ (players player-index)
      (let* ((bidder (list-ref players player-index)))
        (send-datum bidder '(choose-trump))
        (let ([selected-trump-suit
               (caddr (receive-datum bidder 'selected-trump players))])
          (send-datum-to-all players '(trump-selected))
          selected-trump-suit))))

  ;; one complete hand: deal 4, auction, concealed trump, deal 4 more, eight
  ;; tricks. first-seat opens the bidding and leads the first trick.
  ;; returns (values points-won bid-value bid-winner)
  (define play-one-hand
    (λ (players first-seat)
      (let* ((players+deck (distribute-cards-to-players players (fresh-deck)))
             (bid-result (perform-bidding players first-seat))
             (trump-suit (choose-trump-suit players (cdr bid-result))))
        (send-datum-to-all players `(bid-result ,bid-result))
        (match (distribute-cards-to-players (car players+deck) (cdr players+deck))
          [(cons players deck)
           (let ((points-won
                  (play-game (map user-hand players)
                             trump-suit
                             (λ (player-index cards-in-round game-state)
                               (let ((player (list-ref players player-index)))
                                 (send-datum-to-all players `(turn ,player-index))
                                 (send-datum player
                                             `(play-card . ((cards-played . ,cards-in-round)
                                                            (game-state . ,game-state))))
                                 (caddr (receive-datum player 'card-played players))))
                             ;; broadcast accepted plays only — clients
                             ;; render every played message they see
                             #:notify-play
                             (λ (player-index card)
                               (send-datum-to-all players
                                                  `(played ,player-index ,card)))
                             #:error-func
                             (λ (player-index error-msg)
                               (send-datum (list-ref players player-index)
                                           `(error ,error-msg)))
                             #:first-leader first-seat)))
             (send-datum-to-all players `(points-won ,points-won))
             (values points-won (car bid-result) (cdr bid-result)))]
          (else (error "card distribution failed"))))))

  ;; empty a player's channel of stale messages (e.g. the second half of a
  ;; double-clicked "next hand")
  (define (drain-comm! player)
    (when (async-channel-try-get (user-comm player))
      (drain-comm! player)))

  ;; between-hands gate: block until the host asks for the next deal. The
  ;; host channel was drained *before* hand-result went out (a genuine
  ;; next-hand can only follow that broadcast), so whatever arrives now is a
  ;; real click. Any player's socket closing raises, aborting the match.
  (define wait-for-next-hand
    (λ (host members)
      (let loop ()
        (let ([message (sync/timeout 1 (user-comm host))])
          (cond
            [(and (pair? message) (equal? (car message) 'next-hand))
             (void)]
            [else
             (when message
               (displayln (format "ignoring unexpected ~a from ~a at the gate"
                                  message (user-email host))))
             (for-each (λ (member)
                         (when (ws-conn-closed? (user-connection member))
                           (error 'wait-for-next-hand
                                  (format "player ~a disconnected"
                                          (user-email member)))))
                       members)
             (loop)])))))

  (define start-game
    (λ (room-name)
      (match (find-room-by-name room-name)
        [#f (if (hash-ref *running-games* room-name #f)
                'game-already-started
                'room-not-ready)]
        [(and room (game-room host name members))
         (cond
           [(hash-ref *running-games* name #f)
            'game-already-started]

           [(and (equal? (length members) +max-players+)
                 (not (ormap (λ (member)
                               (ws-conn-closed? (user-connection member)))
                             members)))
            ;; the table is in play now: delist it so it stops showing as
            ;; joinable, and remember the running game by name
            (hash-set! *running-games* name #t)
            (close-room! room)
            ;; a failure anywhere in the match (typically a disconnect, caught
            ;; mid-hand by receive-datum or between hands by the gate)
            ;; abandons it: free the table name and the bots, tell the humans
            (with-handlers ((exn:fail?
                             (λ (e)
                               (displayln (format "game ~a aborted: ~a"
                                                  name (exn-message e)))
                               (hash-remove! *running-games* name)
                               (for-each (λ (member)
                                           (if (bot-user? member)
                                               (release-user member)
                                               (send-datum member
                                                           `(game-aborted ,name))))
                                         members)
                               'game-aborted)))
              ;; members already includes the host. Hand N's opener and first
              ;; leader sit at seat (N-1) mod 4, so the deal rotates.
              (let match-loop ((hand-number 1) (evens 0) (odds 0))
                ;; a player gone between hands must abort before the deal
                (for-each (λ (member)
                            (when (ws-conn-closed? (user-connection member))
                              (error "connection closed for player"
                                     (user-email member))))
                          members)
                ;; fresh per-hand player copies: every hand starts empty
                (let* ((players (map (λ (member)
                                       (struct-copy user member (hand '())))
                                     members))
                       (first-seat (remainder (sub1 hand-number) +max-players+))
                       (target (match-target)))
                  (let-values (((points-won bid-value bid-winner)
                                (play-one-hand players first-seat)))
                    (match (score-game points-won bid-value bid-winner)
                      [(list made? _ _)
                       (let* ((delta (hand-game-points bid-value made?))
                              (evens (if (even? bid-winner) (+ evens delta) evens))
                              (odds (if (odd? bid-winner) (+ odds delta) odds)))
                         ;; drain the host channel *before* the broadcast: a
                         ;; genuine next-hand can only be sent after
                         ;; hand-result lands, so anything in there is stale
                         (drain-comm! host)
                         (send-datum-to-all members
                                            `(hand-result ((hand . ,hand-number)
                                                           (bidder . ,bid-winner)
                                                           (bid . ,bid-value)
                                                           (made . ,made?)
                                                           (delta . ,delta)
                                                           (evens . ,evens)
                                                           (odds . ,odds)
                                                           (target . ,target))))
                         (cond
                           [(match-winner evens odds #:target target)
                            => (λ (winner)
                                 (send-datum-to-all members
                                                    `(match-over ((winner . ,winner)
                                                                  (evens . ,evens)
                                                                  (odds . ,odds)
                                                                  (hands . ,hand-number))))
                                 (hash-remove! *running-games* name)
                                 ;; bots are single-use: free their
                                 ;; connections and seats with the match
                                 (for-each (λ (member)
                                             (when (bot-user? member)
                                               (release-user member)))
                                           members)
                                 'game-started)]
                           [else
                            (wait-for-next-hand host members)
                            (match-loop (add1 hand-number) evens odds)]))])))))]

           [else 'room-not-ready])]))))

(require 'user)
(require 'room)
(require 'game)

;; home for threads that outlive the connection whose message spawned them
(define *game-custodian* (make-custodian))

(define (email=? a b)
  (equal? (format "~a" a) (format "~a" b)))

(define (find-member room email)
  (match room
    ((game-room _ _ members)
     (findf (λ (m) (email=? (user-email m) email)) members))))

;; Tears a room down: bots are released, humans are told the table is gone.
;; except: email of the member who triggered the close (no note to self).
(define (close-room-and-notify! room #:except (except #f))
  (match room
    ((game-room host name members)
     (close-room! room)
     (for-each (λ (member)
                 (cond
                   ((bot-user? member) (release-user member))
                   ((and except (email=? (user-email member) except)) (void))
                   (else (send-datum member `(room-closed ,name)))))
               members))))

;; Removes one member from a room, broadcasting the new seat list. The host
;; leaving closes the whole table. Returns a result symbol for the caller.
(define (depart-room! room email)
  (match room
    ((game-room host name members)
     (cond
       ((email=? (user-email host) email)
        (close-room-and-notify! room #:except email)
        'room-left)
       ((find-member room email)
        => (λ (target)
             (let ((new-members (remove target members)))
               (room-update-members! room new-members)
               (when (bot-user? target) (release-user target))
               (send-datum-to-all new-members
                                  `(room-members ,name ,(map user-email new-members)))
               'room-left)))
       (else '(error not-a-member))))))

(define (leave-room message)
  (match message
    ((list _ room-name email)
     (let ((room (find-room-by-name room-name)))
       (if room
           (depart-room! room email)
           '(error no-such-room))))
    (_ '(error invalid-message))))

(define (kick-from-room message)
  (match message
    ((list _ room-name requester-email target-email)
     (let ((room (find-room-by-name room-name)))
       (match room
         (#f '(error no-such-room))
         ((game-room host name _)
          (cond
            ((not (email=? (user-email host) requester-email)) '(error not-host))
            ((email=? requester-email target-email) '(error cannot-kick-host))
            ((find-member room target-email)
             => (λ (target)
                  (unless (bot-user? target)
                    (send-datum target `(removed-from-room ,name)))
                  (depart-room! room target-email)
                  'user-kicked))
            (else '(error not-a-member)))))))
    (_ '(error invalid-message))))

;; A websocket dropped: unseat every user behind it (closing their tables if
;; they were hosting) and forget them.
(define (handle-disconnect connection)
  (for-each
   (λ (u)
     (let ((email (user-email u)))
       (displayln (format "cleaning up disconnected user ~a" email))
       (for-each (λ (room) (depart-room! room email))
                 (rooms-with-member email))
       (remove-user email)))
   (users-with-connection connection)))

(define (join-room message)
  (match message
    ((list _ room-name email)
     (let ([room (find-room-by-name room-name)])
       (cond
         ((not room) '(error no-such-room))
         ((not (member email (get-connected-users-email))) '(error user-not-connected))
         (else
          (let ((members (add-user-to-game-room room (get-user-by-email email))))
            ;; same shape as the add-bot-to-room broadcast so clients have
            ;; one code path for member updates
            (send-datum-to-all members
                               `(room-members ,room-name ,(map user-email members)))
            'room-joined)))))
    (_ '(error invalid-message))))

(define add-bot-to-room
  (lambda (room-name)
    (let ((members (add-bot-to-game-room room-name)))
      (displayln (format "goe members ~a " members))
      (send-datum-to-all members
                         `(room-members ,room-name ,(map user-email members))))))

;; accepted messages:
;;
;; User messages:
;; 
;; (connect-user <email> <pic-url>) => connect a new user to the system
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
;; (card-played <user-email> <card>) => play a card (or call expose-trump)
;; (next-hand <host-email>) => host deals the next hand of the match; only
;;   the host's channel is read at the between-hands gate, so anyone else
;;   sending this is ignored
;;
(define (dispatch connection message)
  (with-handlers ((exn:fail? (λ (e)
                               (displayln (format "dispatch error: ~a" (exn-message e)))
                               `(error ,(exn-message e)))))
    (let ((message (read (open-input-string message))))
      (displayln (format "message is ~a" message))
      (case (car message)
      ;; liveness probe (keeps proxies from culling idle sockets)
      ((ping) 'pong)

      ;; user messages
      ((connect-user) (connect-new-user connection (cadr message) (caddr message)))

      ;; room messages
      ((make-room) (add-game-room (cadr message) (caddr message)))
      ((join-room) (join-room message))
      ((leave-room) (leave-room message))
      ((kick-from-room) (kick-from-room message))
      ;; tagged so clients can tell the reply apart from other lists
      ((get-active-rooms) `(active-rooms ,(get-active-rooms)))
      ((get-room-details) (get-room-details (cadr message)))
      ((add-bot-to-room) (add-bot-to-room (cadr message)))

      ;; game messages
      ((start-game)
       ;; the match thread must survive any single player's websocket:
       ;; web-server kills each connection's custodian on disconnect, and
       ;; this dispatch runs under the custodian of whoever clicked start
       (begin (parameterize ((current-custodian *game-custodian*))
                (thread (λ ()
                          ;; start-game runs the whole match; only failures need
                          ;; reporting back (success is visible as dealt hands)
                          (let ((result (start-game (cadr message))))
                            (when (memq result '(room-not-ready game-already-started))
                              (with-handlers ((exn:fail? void)) ; requester may be gone
                                (ws-send! connection
                                          (with-output-to-string
                                            (λ () (write `(start-game-failed ,result)))))))))))
              '(game-started)))

      ((put-bid selected-trump card-played next-hand)
       (begin (displayln (format "got game message ~a" message))
              ;; async put: a stray message must never block this thread; the
              ;; game loop drops anything it is not waiting for
              (async-channel-put (user-comm (get-user-by-email (cadr message)))
                                 message)
              '(done)))

      ;; catch all
      (else 'invalid-request)))))

(when idle-timeout
  (ws-idle-timeout idle-timeout))

(define start-service
  (case-lambda
    [() (start-service port)]
    [(service-port)
     (displayln (format "port is ~a" service-port))
     (ws-serve connection-handler #:port service-port)]))


(module+ main
  (define stop-service (start-service))
  (printf "Server running. Hit enter to stop service.\n")
  (let ((line (read-line)))
    ;; under a container/supervisor there is no stdin: serve forever
    (when (eof-object? line)
      (sync never-evt)))
  (stop-service))
 
