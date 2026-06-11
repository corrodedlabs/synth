#lang racket

(require net/rfc6455)

(require racket/async-channel)
(require racket/cmdline)
(require racket/control)
(require racket/port)
(require racket/serialize)
(require racket/match)

(provide start-service
         restore-live-matches!)

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
           set-user-hand!
           user-comm
           get-user-by-email
           remove-user
           users-with-connection
           bot-user?
           release-user
           create-bot-user
           convert-to-bot!
           make-ghost-user)

  (require net/rfc6455)
  (require racket/async-channel)
  (require "./client.rkt")

  ;; represents a connected user — one struct per identity, shared by the
  ;; room and any running match, so rebinding the connection in place is
  ;; visible everywhere at once
  ;; connection => websocket; rebound when the same email reconnects
  ;; email => stable identity (the client persists it across refreshes);
  ;;   mutable only so an abandoned seat can be converted into a bot
  ;; hand => cards the player currently holds (reset every hand)
  ;; comm => async channel for game messages (puts never block the listener)
  (struct user ([connection #:mutable] [email #:mutable] [hand #:mutable] comm pic-url))

  (define *connected-users* (make-hash))

  ;; first contact creates the user; the same email connecting again (a
  ;; refreshed page, a dropped link re-established) rebinds the existing
  ;; seat to the new socket — hand, channel, and any running match survive
  (define (connect-new-user connection email pic-url)
    (let ((existing (get-user-by-email email)))
      (cond
        (existing
         (set-user-connection! existing connection)
         'user-reconnected)
        (else
         (hash-set! *connected-users* email
                    (user connection email '() (make-async-channel) pic-url))
         'user-connected))))

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

  ;; every dispatch runs on its own thread, so two "add a bot" requests can
  ;; race; a compare-and-set keeps the names unique
  (define (next-bot-number!)
    (let retry ()
      (let ((n (unbox *bot-counter*)))
        (if (box-cas! *bot-counter* n (add1 n))
            (add1 n)
            (retry)))))

  ;; bots dial back into the server as clients. Their sockets and AI threads
  ;; are created while dispatching a message from the host's websocket, and
  ;; web-server shuts down each connection's custodian on disconnect — so
  ;; without a detached custodian every bot would die with the host.
  (define *bot-custodian* (make-custodian))

  (define create-bot-user
    (lambda ()
      (parameterize ((current-custodian *bot-custodian*))
        (let* ((bot-name (string->symbol (format "bot-~a" (next-bot-number!))))
               (connection (connect-to-ws)))
          (connect-user connection bot-name)
          (spawn-bot bot-name connection)
          (get-user-by-email bot-name)))))

  ;; a placeholder seat for a match restored from disk: registered and
  ;; rejoinable like anyone else, but its connection starts closed, so the
  ;; match counts the owner as inside their reconnect grace until their
  ;; browser comes back. One shared pre-closed connection serves them all.
  (define *ghost-connection* (box #f))

  (define (ghost-connection)
    (or (unbox *ghost-connection*)
        (let ((c (connect-to-ws)))
          (ws-close! c)
          (set-box! *ghost-connection* c)
          c)))

  (define (make-ghost-user email)
    (let ((u (user (ghost-connection) email '() (make-async-channel) "none")))
      (hash-set! *connected-users* email u)
      u))

  ;; an abandoned seat keeps its struct (the running match holds it
  ;; everywhere) but becomes a bot: new identity, new socket, new brain.
  ;; The bot's own connect-user rebinds the struct to the server-side
  ;; connection, exactly like a human reconnect. Returns the bot name.
  (define (convert-to-bot! seat-user)
    (parameterize ((current-custodian *bot-custodian*))
      (let* ((bot-name (string->symbol (format "bot-~a" (next-bot-number!))))
             (connection (connect-to-ws)))
        (remove-user (user-email seat-user))
        (set-user-email! seat-user bot-name)
        (set-user-connection! seat-user connection)
        (hash-set! *connected-users* bot-name seat-user)
        (connect-user connection bot-name)
        (spawn-bot bot-name connection)
        bot-name))))


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

  ;; concurrent joins (three "add a bot" clicks land on three dispatch
  ;; threads) must not lose each other's seat: the read-modify-write is
  ;; serialized and always reads the room's current members
  (define *join-lock* (make-semaphore 1))

  (define +table-seats+ 4)

  ;; adds user to room's member list; returns the updated members, or #f
  ;; when the table is already full (or vanished) — invite links bypass the
  ;; room browser's client-side capacity filter, so the seat count is
  ;; enforced here, inside the lock
  (define add-user-to-game-room
    (λ (room user)
      (match room
        [(game-room host _ _)
         (call-with-semaphore
          *join-lock*
          (λ ()
            (let ((current (hash-ref *game-rooms* (user-email host) #f)))
              (cond
                ((or (not current)
                     (>= (length (game-room-members current)) +table-seats+))
                 #f)
                (else
                 (let ((members (cons user (game-room-members current))))
                   (hash-set! *game-rooms* (user-email host)
                              (game-room host (game-room-name current) members))
                   members))))))])))

  ;; #f when the table is full: the bot is released, not left dangling
  (define add-bot-to-game-room
    (lambda (room-name)
      (let ((room (find-room-by-name room-name)))
        (and room
             (let ((bot (create-bot-user)))
               (or (add-user-to-game-room room bot)
                   (begin (release-user bot) #f)))))))

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
           send-datum-to-all
           request-leave!
           user-in-running-game?
           rejoin-snapshot
           broadcast-emote
           broadcast-room-list!
           restore-live-matches!)

  (require net/rfc6455)
  (require racket/async-channel)
  (require racket/control)
  (require racket/list)

  (require (submod ".." user))
  (require (submod ".." room))
  (require "./game.rkt")
  (require "./storage.rkt")

  ;; running games: room name → players in seat order
  (define *running-games* (make-hash))

  (define +max-players+ 4)

  ;; game points a team needs to take the match; the MATCH-TARGET environment
  ;; variable shortens matches for tests (same pattern as GAME-PORT)
  (define (match-target)
    (let ([configured (getenv "MATCH-TARGET")])
      (or (and configured (string->number configured)) +match-target+)))

  ;; how long a dead socket may wait for its owner to come back before the
  ;; match is abandoned; RECONNECT-GRACE (seconds) is the test hook
  (define (grace-seconds)
    (let ([configured (getenv "RECONNECT-GRACE")])
      (or (and configured (string->number configured)) 45)))

  (define (email=? a b)
    (equal? (format "~a" a) (format "~a" b)))

  (define (user-in-running-game? email)
    (for/first ([(name players) (in-hash *running-games*)]
                #:when (findf (λ (p) (email=? (user-email p) email)) players))
      name))

  ;; email → ms timestamp when the socket was first seen dead
  (define *gone-since* (make-hash))
  ;; emails that asked to leave: no grace, the match aborts at once
  (define *leavers* (make-hash))

  ;; --- table emotes ---
  ;; a fixed vocabulary (anything else is dropped) and a per-player
  ;; cooldown so the table cannot be flooded

  (define +emotes+ '(nice ouch wow think laugh fire))
  (define *last-emote* (make-hash))

  (define (broadcast-emote email emote)
    (cond
      ((not (memq emote +emotes+)) 'invalid-emote)
      (else
       (let ((name (user-in-running-game? email)))
         (cond
           ;; a bare symbol: clients must Ignore this, never confuse it
           ;; with the rejoin flow's (no-running-game) reply
           ((not name) 'not-in-a-game)
           (else
            (let ((now (current-inexact-milliseconds))
                  (last (hash-ref *last-emote* email 0)))
              (cond
                ((< (- now last) 1500) 'emote-throttled)
                (else
                 (hash-set! *last-emote* email now)
                 (let* ((players (hash-ref *running-games* name))
                        (seat (index-where players
                                           (λ (p) (email=? (user-email p) email)))))
                   (send-datum-to-all players `(emote-played ,seat ,emote))
                   'emote-sent))))))))))

  (define (request-leave! email)
    (hash-set! *leavers* email #t))

  (define (forget-liveness! players)
    (for-each (λ (player)
                (hash-remove! *gone-since* (user-email player))
                (hash-remove! *leavers* (user-email player)))
              players))

  (define (human? u) (not (bot-user? u)))

  ;; the deal button (and other host duties) belong to the original host
  ;; while their seat is still human; a botted host seat passes the duty
  ;; to the first human in seat order
  (define (acting-host players fallback)
    (if (and fallback (human? fallback) (memq fallback players))
        fallback
        (or (findf human? players) fallback)))

  ;; called from every 1s wait poll. Tracks drops and returns, telling the
  ;; table about both; a seat lost for good (its owner left, or stayed dead
  ;; past the grace window) goes to the abandonment flow, which either
  ;; converts it to a bot or raises to end the match. Bots get no grace:
  ;; their sockets only close when the server releases them.
  (define (check-liveness! players)
    (for-each
     (λ (player)
       (let ((email (user-email player)))
         (cond
           ((hash-ref *leavers* email #f)
            (if (human? player)
                (lose-seat! players player)
                (hash-remove! *leavers* email))) ; junk flag on a bot seat
           ((ws-conn-closed? (user-connection player))
            (cond
              ((bot-user? player)
               (error 'check-liveness (format "bot ~a died" email)))
              ((hash-ref *gone-since* email #f)
               => (λ (since)
                    (when (> (- (current-inexact-milliseconds) since)
                             (* 1000 (grace-seconds)))
                      (lose-seat! players player))))
              (else
               (hash-set! *gone-since* email (current-inexact-milliseconds))
               (send-datum-to-all players
                                  `(player-disconnected ,email ,(grace-seconds))))))
           ((hash-ref *gone-since* email #f)
            ;; the seat's socket is alive again (connect-user rebound it)
            (hash-remove! *gone-since* email)
            (send-datum-to-all players `(player-reconnected ,email))))))
     players))

  ;; a human seat is gone for good. With no live human left at the table
  ;; the match simply closes; otherwise the survivors are told who
  ;; abandoned and choose between a bot replacement and ending the match
  ;; (first answer wins).
  (define (lose-seat! players lost)
    (let* ((lost-email (user-email lost))
           (seat (index-where players (λ (p) (eq? p lost))))
           (live-others (filter (λ (p) (and (human? p)
                                            (not (eq? p lost))
                                            (not (ws-conn-closed?
                                                  (user-connection p)))))
                                players)))
      ;; either way this seat's liveness flags are spent
      (hash-remove! *leavers* lost-email)
      (hash-remove! *gone-since* lost-email)
      (when (null? live-others)
        (error 'check-liveness
               (format "player ~a abandoned the match" lost-email)))
      (send-datum-to-all players `(seat-abandoned ,seat ,lost-email))
      (let decision-loop ()
        (let ((message (apply sync/timeout 1 (map user-comm live-others))))
          (cond
            ((and (pair? message) (eq? (car message) 'replace-with-bot))
             (replace-seat! players seat lost))
            ((and (pair? message) (eq? (car message) 'close-game))
             (error 'check-liveness
                    (format "the table ended the match after ~a left"
                            lost-email)))
            (else
             (when message
               (displayln (format "ignoring unexpected ~a at the abandon gate"
                                  message)))
             ;; deciders can vanish too; with nobody left to answer, close
             (if (andmap (λ (p) (ws-conn-closed? (user-connection p)))
                         live-others)
                 (error 'check-liveness
                        "everyone left during the abandon decision")
                 (decision-loop))))))))

  ;; the seat lives on under a bot: same struct, channel, and hand — new
  ;; identity and brain. The bot is privately re-dealt its current cards
  ;; and, when the game is waiting on this very seat, re-asked the pending
  ;; request so play resumes by itself.
  (define (replace-seat! players seat lost)
    (let* ((bot-name (convert-to-bot! lost))
           (name (user-in-running-game? bot-name))
           (state (and name (hash-ref *match-states* name #f))))
      (displayln (format "seat ~a taken over by ~a" seat bot-name))
      (send-datum lost `(hand ,bot-name ,(user-hand lost)))
      (send-datum-to-all players
                         `(seat-replaced ,seat ,bot-name
                                         ,(map user-email players)))
      (when (and state
                 (match-state-awaiting state)
                 (equal? seat (car (match-state-awaiting state))))
        (send-datum lost (cdr (match-state-awaiting state))))))

  ;; the live "open tables" push: everyone connected who is neither seated
  ;; at a table nor playing a match sees room changes as they happen
  (define (broadcast-room-list!)
    (let ((rooms `(active-rooms ,(get-active-rooms))))
      (for-each (λ (email)
                  (let ((u (get-user-by-email email)))
                    (when (and u
                               (human? u)
                               (not (user-in-running-game? email))
                               (null? (rooms-with-member email)))
                      (send-datum u rooms))))
                (get-connected-users-email))))

  ;; --- live match view, for reconnecting clients ---
  ;;
  ;; The game thread keeps this current as it plays; rejoin-snapshot reads
  ;; it from a dispatch thread. stage: bidding | choosing-trump | playing |
  ;; between-hands. awaiting: (seat . request-message) while the game waits
  ;; on someone. trick entries: (list seat card counts-as-trump?).
  (struct match-state (players
                       [hand-number #:mutable]
                       [evens #:mutable]
                       [odds #:mutable]
                       [stage #:mutable]
                       [high-bid #:mutable]   ; (value . seat) or #f
                       [bid-result #:mutable] ; (value . seat) once the auction ends
                       [trump-suit #:mutable]
                       [trump-exposed? #:mutable]
                       [trick #:mutable]
                       [trick-leader #:mutable]
                       [points #:mutable]       ; alist seat → card points taken
                       [tricks-taken #:mutable] ; alist seat → tricks taken
                       [last-result #:mutable]  ; hand-result body, between hands
                       [awaiting #:mutable]))

  ;; room name → match-state
  (define *match-states* (make-hash))

  (define (fresh-seat-alist)
    (map (λ (seat) (cons seat 0)) (range +max-players+)))

  (define (reset-state-for-hand! state hand-number first-seat)
    (set-match-state-hand-number! state hand-number)
    (set-match-state-stage! state 'bidding)
    (set-match-state-high-bid! state #f)
    (set-match-state-bid-result! state #f)
    (set-match-state-trump-suit! state #f)
    (set-match-state-trump-exposed?! state #f)
    (set-match-state-trick! state '())
    (set-match-state-trick-leader! state first-seat)
    (set-match-state-points! state (fresh-seat-alist))
    (set-match-state-tricks-taken! state (fresh-seat-alist))
    (set-match-state-last-result! state #f)
    (set-match-state-awaiting! state #f))

  (define (alist-add alist key delta)
    (map (λ (entry)
           (if (equal? (car entry) key)
               (cons key (+ (cdr entry) delta))
               entry))
         alist))

  ;; a completed trick folds into points/tricks and the winner leads next
  (define (note-play! state seat card)
    (cond
      ((eq? card 'expose-trump)
       (set-match-state-trump-exposed?! state #t))
      (else
       (let* ((trump? (and (match-state-trump-exposed? state)
                           (equal? (card-suit card)
                                   (match-state-trump-suit state))))
              (trick (append (match-state-trick state)
                             (list (list seat card trump?)))))
         (cond
           ((= (length trick) +max-players+)
            (let ((winner (trick-winner trick))
                  (trick-points (apply + (map (compose1 card-point second) trick))))
              (set-match-state-points!
               state (alist-add (match-state-points state) winner trick-points))
              (set-match-state-tricks-taken!
               state (alist-add (match-state-tricks-taken state) winner 1))
              (set-match-state-trick-leader! state winner)
              (set-match-state-trick! state '())))
           (else (set-match-state-trick! state trick)))))))

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

  ;; deals four cards to everyone, mutating the shared user structs so the
  ;; rejoin snapshot always sees live hands. Sends are best-effort: a player
  ;; inside their reconnect grace window misses the live deal and is caught
  ;; up by the snapshot instead.
  (define distribute-cards-to-players
    (λ (players deck)
      (match/values
          (distribute-cards deck 4)
        [(new-cards-for-players remaining-deck)
         (for-each (λ (new-cards player)
                     (send-datum player `(hand ,(user-email player) ,new-cards))
                     (set-user-hand! player (append (user-hand player) new-cards)))
                   new-cards-for-players
                   players)
         remaining-deck])))

  ;; next message of the expected kind from this player's channel; other
  ;; messages are dropped. Every 1s poll runs the liveness check over the
  ;; whole table, so leavers abort at once and dropped players get their
  ;; grace window whether or not it is their turn.
  (define receive-datum
    (λ (player expected-tag (watched (list player)))
      (let loop ()
        (let ([message (sync/timeout 1 (user-comm player))])
          (cond
            [(not message)
             (check-liveness! watched)
             (loop)]
            [(and (pair? message) (equal? (car message) expected-tag))
             message]
            [else
             (displayln (format "ignoring unexpected ~a from ~a"
                                message (user-email player)))
             (loop)])))))

  ;; runs the auction; first-seat opens. returns (cons winning-bid winner-index)
  (define perform-bidding
    (λ (players first-seat state)
      (start-bidding (λ (player-index min-bid)
                       (let ((player (list-ref players player-index))
                             (request `(request-bid ,min-bid)))
                         (set-match-state-awaiting! state (cons player-index request))
                         (send-datum-to-all players `(turn ,player-index))
                         (send-datum player request)
                         (caddr (receive-datum player 'put-bid players))))
                     (λ (player-index bid-value)
                       (set-match-state-awaiting! state #f)
                       (unless (equal? bid-value 'pass)
                         (set-match-state-high-bid! state (cons bid-value player-index)))
                       (send-datum-to-all players
                                          `(bid-placed ,player-index ,bid-value)))
                     (λ (player-index error-msg offending-value)
                       (send-datum (list-ref players player-index)
                                   `(error ,error-msg)))
                     #:first-player first-seat)))

  ;; player index is the index of the player who won the bid and will choose the trump
  (define choose-trump-suit
    (λ (players player-index state)
      (let* ((bidder (list-ref players player-index)))
        (set-match-state-awaiting! state (cons player-index '(choose-trump)))
        (send-datum bidder '(choose-trump))
        (let ([selected-trump-suit
               (caddr (receive-datum bidder 'selected-trump players))])
          (set-match-state-awaiting! state #f)
          (send-datum-to-all players '(trump-selected))
          selected-trump-suit))))

  ;; one complete hand: deal 4, auction, concealed trump, deal 4 more, eight
  ;; tricks. first-seat opens the bidding and leads the first trick.
  ;; returns (values points-won bid-value bid-winner)
  (define play-one-hand
    (λ (players first-seat state)
      (let* ((deck (distribute-cards-to-players players (fresh-deck)))
             (bid-result (perform-bidding players first-seat state)))
        (set-match-state-bid-result! state bid-result)
        (set-match-state-stage! state 'choosing-trump)
        (let ((trump-suit (choose-trump-suit players (cdr bid-result) state)))
          (set-match-state-trump-suit! state trump-suit)
          (send-datum-to-all players `(bid-result ,bid-result))
          (distribute-cards-to-players players deck)
          (set-match-state-stage! state 'playing)
          (let ((points-won
                 (play-game (map user-hand players)
                            trump-suit
                            (λ (player-index cards-in-round game-state)
                              (let ((player (list-ref players player-index))
                                    (request `(play-card . ((cards-played . ,cards-in-round)
                                                            (game-state . ,game-state)))))
                                (set-match-state-awaiting!
                                 state (cons player-index request))
                                (send-datum-to-all players `(turn ,player-index))
                                (send-datum player request)
                                (caddr (receive-datum player 'card-played players))))
                            ;; broadcast accepted plays only — clients
                            ;; render every played message they see
                            #:notify-play
                            (λ (player-index card)
                              (set-match-state-awaiting! state #f)
                              (note-play! state player-index card)
                              (unless (eq? card 'expose-trump)
                                (let ((player (list-ref players player-index)))
                                  (set-user-hand! player
                                                  (remove card (user-hand player)))))
                              (send-datum-to-all players
                                                 `(played ,player-index ,card)))
                            #:error-func
                            (λ (player-index error-msg)
                              (send-datum (list-ref players player-index)
                                          `(error ,error-msg)))
                            #:first-leader first-seat)))
            (send-datum-to-all players `(points-won ,points-won))
            (values points-won (car bid-result) (cdr bid-result)))))))

  ;; empty a player's channel of stale messages (e.g. the second half of a
  ;; double-clicked "next hand")
  (define (drain-comm! player)
    (when (async-channel-try-get (user-comm player))
      (drain-comm! player)))

  ;; between-hands gate: block until the acting host asks for the next
  ;; deal (recomputed every tick — the original host's seat may have been
  ;; botted, passing the deal button to the next human). The decider's
  ;; channel was drained *before* hand-result went out, so whatever
  ;; arrives now is a real click. Liveness gives dropped players their
  ;; grace and routes abandonments through the replacement flow.
  (define wait-for-next-hand
    (λ (host members)
      (let loop ()
        (let* ([decider (acting-host members host)]
               [message (sync/timeout 1 (user-comm decider))])
          (cond
            [(and (pair? message) (equal? (car message) 'next-hand))
             (void)]
            [else
             (when message
               (displayln (format "ignoring unexpected ~a from ~a at the gate"
                                  message (user-email decider))))
             (check-liveness! members)
             (loop)])))))

  ;; a match is over, won or abandoned: free the table name, the state
  ;; views, the resume row on disk, the bots, and any seat whose owner is
  ;; no longer connected
  (define (end-match! name members)
    (hash-remove! *running-games* name)
    (hash-remove! *match-states* name)
    (clear-live-match! name)
    (forget-liveness! members)
    (for-each (λ (member)
                (hash-remove! *last-emote* (user-email member))
                (cond
                  ((bot-user? member) (release-user member))
                  ((ws-conn-closed? (user-connection member))
                   (remove-user (user-email member)))))
              members))

  ;; everything a restart needs to pick a match back up at the top of the
  ;; hand it was in: who sits where (and which seats are bots), the hand
  ;; number, and the running game points
  (define (live-snapshot room started hand evens odds members)
    `((room . ,room)
      (started . ,started)
      (hand . ,hand)
      (evens . ,evens)
      (odds . ,odds)
      (members . ,(map (λ (m) (cons (user-email m) (bot-user? m))) members))))

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
            ;; joinable, and remember who is seated for rejoins
            (hash-set! *running-games* name members)
            (close-room! room)
            (broadcast-room-list!)
            (run-match name host members)]

           [else 'room-not-ready])])))

  ;; plays a whole match on the current thread: deal/bid/play hand after
  ;; hand until a team reaches the target. A resume row is persisted before
  ;; every hand, so a match interrupted by a crash or deploy restarts at
  ;; the top of the hand it was in (the partial hand is re-dealt). A lost
  ;; seat goes through the abandonment flow; an unresolved loss raises
  ;; into the abort handler here.
  (define (run-match name host members
                     #:first-hand (first-hand 1)
                     #:evens (evens0 0)
                     #:odds (odds0 0)
                     #:started-at (started-at (current-seconds)))
    (let ((state (match-state members first-hand evens0 odds0 'bidding
                              #f #f #f #f '() 0 (fresh-seat-alist)
                              (fresh-seat-alist) #f #f)))
      (hash-set! *match-states* name state)
      (with-handlers ((exn:fail?
                       (λ (e)
                         (displayln (format "game ~a aborted: ~a"
                                            name (exn-message e)))
                         (end-match! name members)
                         (for-each (λ (member)
                                     (unless (bot-user? member)
                                       (send-datum member
                                                   `(game-aborted ,name))))
                                   members)
                         'game-aborted)))
        ;; members already includes the host. Hand N's opener and
        ;; first leader sit at seat (N-1) mod 4, so the deal rotates.
        (let match-loop ((hand-number first-hand) (evens evens0) (odds odds0))
          (save-live-match! name (live-snapshot name started-at hand-number
                                                evens odds members))
          (check-liveness! members)
          ;; the shared user structs carry live hands: empty them
          ;; and reset the state view for the new hand
          (for-each (λ (member) (set-user-hand! member '())) members)
          (let ((first-seat (remainder (sub1 hand-number) +max-players+))
                (target (match-target)))
            (reset-state-for-hand! state hand-number first-seat)
            (let-values (((points-won bid-value bid-winner)
                          (play-one-hand members first-seat state)))
              (match (score-game points-won bid-value bid-winner)
                [(list made? _ _)
                 (let* ((delta (hand-game-points bid-value made?))
                        (evens (if (even? bid-winner) (+ evens delta) evens))
                        (odds (if (odd? bid-winner) (+ odds delta) odds))
                        (result-body `((hand . ,hand-number)
                                       (bidder . ,bid-winner)
                                       (bid . ,bid-value)
                                       (made . ,made?)
                                       (delta . ,delta)
                                       (evens . ,evens)
                                       (odds . ,odds)
                                       (target . ,target))))
                   (set-match-state-evens! state evens)
                   (set-match-state-odds! state odds)
                   (set-match-state-stage! state 'between-hands)
                   (set-match-state-last-result! state result-body)
                   ;; persist the hand's outcome before anyone sees it: a
                   ;; crash at the gate must resume AFTER this hand, not
                   ;; replay a result the table already watched
                   (save-live-match! name (live-snapshot name started-at
                                                         (add1 hand-number)
                                                         evens odds members))
                   ;; drain the deciding seat *before* the broadcast:
                   ;; a genuine next-hand can only be sent after
                   ;; hand-result lands, so anything in there is stale
                   (drain-comm! (acting-host members host))
                   (send-datum-to-all members `(hand-result ,result-body))
                   (cond
                     [(match-winner evens odds #:target target)
                      => (λ (winner)
                           (send-datum-to-all members
                                              `(match-over ((winner . ,winner)
                                                            (evens . ,evens)
                                                            (odds . ,odds)
                                                            (hands . ,hand-number))))
                           ;; the books: who finished the match, who won
                           (record-match! name started-at hand-number winner
                                          evens odds (map user-email members))
                           (end-match! name members)
                           'game-started)]
                     [else
                      (wait-for-next-hand host members)
                      (match-loop (add1 hand-number) evens odds)]))])))))))

  ;; boot-time recovery: every resume row becomes a running match again.
  ;; Human seats come back as ghosts — registered, inside their reconnect
  ;; grace, claimable by the auto-rejoining browser that owns the email —
  ;; and bot seats get fresh bots. A match nobody reclaims aborts itself
  ;; when the ghosts' grace runs out.
  (define (restore-live-matches!)
    (for-each
     (λ (snapshot)
       (with-handlers ((exn:fail?
                        (λ (e)
                          (displayln (format "restore failed: ~a"
                                             (exn-message e))))))
         (let* ((room (cdr (assq 'room snapshot)))
                (started (cdr (assq 'started snapshot)))
                (hand-number (cdr (assq 'hand snapshot)))
                (evens (cdr (assq 'evens snapshot)))
                (odds (cdr (assq 'odds snapshot)))
                (members (map (λ (seat)
                                (if (cdr seat)
                                    (create-bot-user)
                                    (make-ghost-user (car seat))))
                              (cdr (assq 'members snapshot)))))
           (displayln (format "restoring match ~a at hand ~a (~a — ~a)"
                              room hand-number evens odds))
           (hash-set! *running-games* room members)
           (thread (λ ()
                     (run-match room (last members) members
                                #:first-hand hand-number
                                #:evens evens
                                #:odds odds
                                #:started-at started))))))
     (load-live-matches)))

  ;; everything a reconnecting client needs to redraw mid-match; #f fields
  ;; mean "not known/decided yet". Trump stays hidden from everyone but the
  ;; bidder until the exposure.
  (define (rejoin-snapshot email)
    (let ((name (user-in-running-game? email)))
      (cond
        ((not name) '(no-running-game))
        (else
         (let ((players (hash-ref *running-games* name))
               (state (hash-ref *match-states* name #f)))
           (cond
             ((not state) '(no-running-game))
             (else
              (let* ((seat (index-where players
                                        (λ (p) (email=? (user-email p) email))))
                     (me (list-ref players seat))
                     (bid-result (match-state-bid-result state))
                     (exposed? (match-state-trump-exposed? state))
                     (trump (match-state-trump-suit state))
                     (awaiting (match-state-awaiting state)))
                `(game-snapshot
                  ((room . ,name)
                   (members . ,(map user-email players))
                   (your-seat . ,seat)
                   (hand-number . ,(match-state-hand-number state))
                   (evens . ,(match-state-evens state))
                   (odds . ,(match-state-odds state))
                   (target . ,(match-target))
                   (stage . ,(match-state-stage state))
                   (high-bid . ,(match-state-high-bid state))
                   (bid-result . ,bid-result)
                   (trump . ,(and trump
                                  (or exposed?
                                      (and bid-result
                                           (equal? seat (cdr bid-result))))
                                  trump))
                   (trump-exposed . ,exposed?)
                   (your-hand . ,(user-hand me))
                   (trick-leader . ,(match-state-trick-leader state))
                   (trick . ,(map (λ (play) (cons (first play) (second play)))
                                  (match-state-trick state)))
                   (points . ,(match-state-points state))
                   (tricks . ,(match-state-tricks-taken state))
                   (last-result . ,(match-state-last-result state))
                   (awaiting . ,(and awaiting (car awaiting)))
                   (request . ,(and awaiting
                                    (equal? seat (car awaiting))
                                    (cdr awaiting))))))))))))))

(require 'user)
(require 'room)
(require 'game)
(require "./storage.rkt")

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

;; A websocket dropped. Users seated in a running match keep their seat —
;; the match gives them a grace window to reconnect (the same email rebinds
;; the seat, so a user already rebound to a newer socket is not matched
;; here), and end-match! forgets them if they never come back. Everyone
;; else is unseated (closing their tables if they were hosting) and
;; forgotten.
(define (handle-disconnect connection)
  (for-each
   (λ (u)
     (let ((email (user-email u)))
       (cond
         ((user-in-running-game? email)
          (displayln (format "user ~a dropped mid-match — holding their seat"
                             email)))
         (else
          (displayln (format "cleaning up disconnected user ~a" email))
          (for-each (λ (room) (depart-room! room email))
                    (rooms-with-member email))
          (remove-user email)))))
   (users-with-connection connection))
  (broadcast-room-list!))

(define (join-room message)
  (match message
    ((list _ room-name email)
     (let ([room (find-room-by-name room-name)])
       (cond
         ((not room) '(error no-such-room))
         ((not (member email (get-connected-users-email))) '(error user-not-connected))
         (else
          (let ((members (add-user-to-game-room room (get-user-by-email email))))
            (cond
              ((not members) '(error room-full))
              (else
               ;; same shape as the add-bot-to-room broadcast so clients have
               ;; one code path for member updates
               (send-datum-to-all members
                                  `(room-members ,room-name ,(map user-email members)))
               'room-joined)))))))
    (_ '(error invalid-message))))

(define add-bot-to-room
  (lambda (room-name)
    (let ((members (add-bot-to-game-room room-name)))
      (cond
        ((not members) '(error room-full))
        (else
         (send-datum-to-all members
                            `(room-members ,room-name ,(map user-email members)))
         'bot-added)))))

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
;;   the acting host's channel is read at the between-hands gate, so anyone
;;   else sending this is ignored
;;
;; (replace-with-bot <email>) / (close-game <email>) => answers to the
;;   abandon gate after (seat-abandoned <seat> <email>) is broadcast: keep
;;   playing with a bot in the empty seat, or end the match for everyone
;;
;; (leave-game <email>) => walk out of a running match: it aborts for the
;;   whole table at once (no reconnect grace)
;;
;; (rejoin <email>) => after reconnecting (connect-user with the same email
;;   rebinds the seat), fetch the full mid-match snapshot; replies
;;   (game-snapshot <alist>) or (no-running-game)
;;
;; (emote <email> <emote-id>) => broadcast (emote-played <seat> <emote-id>)
;;   to the sender's table; ids outside the fixed vocabulary are dropped and
;;   senders are rate-limited
;;
;; (get-leaderboard) => (leaderboard ((name played won) ...)) from the
;;   match history (humans only; an abandoned seat's bot earns nothing)
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

      ;; room messages — each change pushes the fresh table list to every
      ;; connected browser-screen user, so "open tables" stays live
      ((make-room)
       (let ((result (add-game-room (cadr message) (caddr message))))
         (broadcast-room-list!)
         result))
      ((join-room)
       (let ((result (join-room message)))
         (broadcast-room-list!)
         result))
      ((leave-room)
       (let ((result (leave-room message)))
         (broadcast-room-list!)
         result))
      ((kick-from-room)
       (let ((result (kick-from-room message)))
         (broadcast-room-list!)
         result))
      ;; tagged so clients can tell the reply apart from other lists
      ((get-active-rooms) `(active-rooms ,(get-active-rooms)))
      ((get-room-details) (get-room-details (cadr message)))
      ((add-bot-to-room)
       (let ((result (add-bot-to-room (cadr message))))
         (broadcast-room-list!)
         result))

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

      ((put-bid selected-trump card-played next-hand replace-with-bot close-game)
       (begin (displayln (format "got game message ~a" message))
              ;; async put: a stray message must never block this thread; the
              ;; game loop drops anything it is not waiting for
              (async-channel-put (user-comm (get-user-by-email (cadr message)))
                                 message)
              '(done)))

      ((leave-game)
       (let ((email (cadr message)))
         (cond
           ((user-in-running-game? email)
            (request-leave! email)
            '(leaving))
           ;; bare symbol: an Ignored no-op, not the rejoin teardown reply
           (else 'not-in-a-game))))

      ((rejoin) (rejoin-snapshot (cadr message)))

      ((emote) (broadcast-emote (cadr message) (caddr message)))

      ;; ((name played won) ...) from completed matches, best first
      ((get-leaderboard) `(leaderboard ,(leaderboard 10)))

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
  ;; container logs are useless when block-buffered: flush per line
  (file-stream-buffer-mode (current-output-port) 'line)
  ;; matches survive restarts: open the book, start serving, then put
  ;; every interrupted match back on the table (its players' browsers
  ;; auto-rejoin within the grace window)
  (storage-start! (or (getenv "GAME-DB") "game.db"))
  (define stop-service (start-service))
  (restore-live-matches!)
  (printf "Server running. Hit enter to stop service.\n")
  (let ((line (read-line)))
    ;; under a container/supervisor there is no stdin: serve forever
    (when (eof-object? line)
      (sync never-evt)))
  (stop-service))
 
