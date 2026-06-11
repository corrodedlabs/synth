#lang racket

(require rackunit
         rackunit/text-ui
         net/rfc6455
         racket/port
         "./server.rkt"
         "./client.rkt"
         "./game.rkt")

(define *service* #f)

;; off the default port so tests don't collide with a dev server
(define test-port 18081)

(define user1-id 'user1-email)
(define room-name 'my-test-room)

;; --- raw protocol helpers (client.rkt keeps its own private copies) ---

(define (send-msg connection data)
  (ws-send! connection (with-output-to-string (λ () (write data)))))

(define (recv-msg connection)
  (let ((data (ws-recv connection)))
    (cond
      ((eof-object? data) 'connection-closed)
      ((equal? "#<void>" data) #f)
      (else (read (open-input-string data))))))

;; --- scripted match players ---
;;
;; A deterministic human stand-in: bids 16 when opening and passes every
;; raise, picks diamond as trump, always plays the first valid card. Every
;; parsed server message lands in `events` (oldest first) for assertions.
;; Tags in `mute` are recorded but never answered — a player gone AFK.

(struct scripted (email connection events))

;; #:smart? plays with the real bot heuristics (and the per-hand memory
;; they need); the default stays rigidly predictable — open at 16, pass
;; every raise, take diamond, first valid card — for the deterministic
;; rotation/arithmetic scenarios.
(define (spawn-scripted email connection
                        #:mute (mute '())
                        #:initial-hand (initial-hand '())
                        #:auto-next? (auto-next? #f)
                        #:smart? (smart? #f))
  (define events (box '()))
  (define hand (box initial-hand))
  (define seen (box '()))
  (define trump (box #f))
  (define chose? (box #f))
  (define exposed? (box #f))
  (define must-trump? (box #f))
  (define (record! message)
    (set-box! events (append (unbox events) (list message))))
  (thread
   (λ ()
     (let loop ()
       (let ((message (recv-msg connection)))
         (cond
           ((eq? message 'connection-closed) (record! 'connection-closed))
           ((not message) (loop))
           (else
            (record! message)
            (when (and (pair? message) (not (memq (car message) mute)))
              (case (car message)
                ((hand)
                 (set-box! hand (append (unbox hand) (caddr message))))
                ((hand-result)
                 ;; a hosting script can keep the match moving by itself
                 (when auto-next?
                   (send-msg connection `(next-hand ,email))))
                ((bid-result)
                 (set-box! seen '())
                 (set-box! exposed? #f)
                 (set-box! must-trump? #f)
                 (unless (unbox chose?) (set-box! trump #f))
                 (set-box! chose? #f))
                ((played)
                 (let ((what (caddr message)))
                   (if (eq? what 'expose-trump)
                       (set-box! exposed? #t)
                       (set-box! seen (cons what (unbox seen))))))
                ((request-bid)
                 (let ((min-bid (cadr message)))
                   (send-msg connection
                             `(put-bid ,email
                                       ,(if smart?
                                            (choose-bid (unbox hand) min-bid)
                                            (if (= min-bid +min-bid+)
                                                +min-bid+
                                                'pass))))))
                ((choose-trump)
                 (let ((suit (if smart?
                                 (choose-trump-suit (unbox hand))
                                 'diamond)))
                   (set-box! trump suit)
                   (set-box! chose? #t)
                   (send-msg connection `(selected-trump ,email ,suit))))
                ((play-card)
                 (let* ((cards-played (cdr (cadr message)))
                        (game-state (cdr (caddr message)))
                        (first-suit (cdr (assoc 'first-suit game-state)))
                        (server-trump (cdr (assoc 'trump-suit game-state)))
                        (choice
                         (if smart?
                             (choose-play (unbox hand) cards-played game-state
                                          #:trump (unbox trump)
                                          #:exposed? (unbox exposed?)
                                          #:seen (unbox seen)
                                          #:must-trump? (unbox must-trump?))
                             (findf (λ (c) (valid-card? c first-suit
                                                        cards-played
                                                        (unbox hand)))
                                    (unbox hand)))))
                   (when server-trump
                     (set-box! trump server-trump)
                     (set-box! exposed? #t))
                   (set-box! must-trump? (eq? choice 'expose-trump))
                   (unless (eq? choice 'expose-trump)
                     (set-box! hand (remove choice (unbox hand))))
                   (send-msg connection `(card-played ,email ,choice))))
                (else (void))))
            (loop)))))))
  (scripted email connection events))

;; connects four scripted users and seats them at a new table; returns them
;; in *seat order* — the server seats the newest joiner first and the host
;; last, so (list seat0 seat1 seat2 host). mute maps email -> muted tags.
(define (seat-scripted-table table-name emails #:mute (mute (hash)))
  (match-let (((list host-email join1 join2 join3) emails))
    (define (fresh email)
      (let ((connection (connect-to-ws)))
        (connect-user connection email)
        (cons email connection)))
    (let ((host (fresh host-email)))
      (check-equal? (create-room (cdr host) host-email table-name)
                    'room-created)
      (let ((joined (map (λ (email)
                           (let ((joiner (fresh email)))
                             ;; consumes the joiner's room-members broadcast;
                             ;; the join is complete server-side by then
                             (join-room (cdr joiner) table-name email)
                             joiner))
                         (list join1 join2 join3))))
        ;; seats: 0 = last joiner … 3 = host
        (map (λ (pair) (spawn-scripted (car pair) (cdr pair)
                                       #:mute (hash-ref mute (car pair) '())))
             (reverse (cons host joined)))))))

(define (close-scripted! player)
  (with-handlers ((exn:fail? void))
    (ws-close! (scripted-connection player))))

;; polls a player's event log until extract returns non-#f
(define (await player extract #:timeout (timeout 30) #:label (label 'await))
  (let loop ((waited 0))
    (let ((found (extract (unbox (scripted-events player)))))
      (cond
        (found found)
        ((> waited timeout)
         (error label (format "timed out after ~as; events: ~a"
                              timeout (unbox (scripted-events player)))))
        (else (sleep 0.1) (loop (+ waited 0.1)))))))

(define ((tagged tag) message)
  (and (pair? message) (eq? (car message) tag)))

(define ((events-tagged tag) events)
  (filter (tagged tag) events))

(define (nth-hand-result events n)
  (let ((results ((events-tagged 'hand-result) events)))
    (and (>= (length results) n) (list-ref results (sub1 n)))))

;; (hand-result ((hand . 1) (bidder . 0) …)) → value for key
(define (result-ref result key)
  (cdr (assoc key (cadr result))))

;; reads frames off a raw connection until one satisfies pred
(define (recv-until connection pred #:attempts (attempts 10))
  (let loop ((n attempts))
    (when (zero? n)
      (error 'recv-until "expected frame never arrived"))
    (let ((message (recv-msg connection)))
      (if (and message (pred message))
          message
          (loop (sub1 n))))))

(define-test-suite game-tests
  (around
   ;; start server before tests; a short reconnect grace keeps the
   ;; disconnect-abort scenarios fast (production default is much longer)
   (begin (displayln "starting server")
          (putenv "GAME-PORT" (number->string test-port))
          (putenv "RECONNECT-GRACE" "2")
          (set! *service* (start-service test-port))
          (sleep 3))

   ;; test cases
   (test-begin
     (let ((connection (connect-to-ws))
           (watcher (connect-to-ws)))
       ;; replies are tagged: (active-rooms <rooms>)
       (check-pred null? (cadr (get-active-rooms connection)))

       (connect-user connection user1-id)
       ;; a connected user on the start screen gets the table list pushed
       ;; whenever it changes — no Refresh clicks needed
       (connect-user watcher 'watcher-email)

       (check-equal? (create-room connection user1-id room-name)
                     'room-created)
       (let ((push (recv-until watcher (tagged 'active-rooms))))
         (check-equal? (length (cadr push)) 1 "creation reaches the watcher"))

       (let ((rooms (cadr (get-active-rooms connection))))
         (check-equal? (length rooms) 1))

       ;; hosting user leaves: the table must not outlive them and pollute
       ;; the room list the later tests assert on — and the watcher learns
       ;; about that live, too
       (ws-close! connection)
       (let ((push (recv-until watcher (λ (m)
                                         (and ((tagged 'active-rooms) m)
                                              (null? (cadr m)))))))
         (check-pred null? (cadr push) "the closed table vanishes live"))
       (ws-close! watcher)
       (sleep 0.5)))

   ;; match flow: per-hand game points accumulate and the deal rotates
   (test-begin
     (let* ((players (seat-scripted-table 'match-room
                                          '(match-host match-j1 match-j2 match-j3)))
            (host (last players)))
       ;; a fifth chair does not exist: late joins (stale invite links)
       ;; bounce off the full table instead of wedging the lobby
       (let ((extra (connect-to-ws)))
         (connect-user extra 'match-extra)
         (send-msg extra '(join-room match-room match-extra))
         (check-equal? (recv-until extra (λ (m) (and (pair? m) (eq? (car m) 'error))))
                       '(error room-full))
         (ws-close! extra)
         (sleep 0.3))

       (send-msg (scripted-connection host) '(start-game match-room))

       ;; hand 1: seat 0 opens at 16, everyone passes
       (let ((result (await host (λ (events) (nth-hand-result events 1))
                            #:label 'hand-1)))
         (check-equal? (result-ref result 'hand) 1)
         (check-equal? (result-ref result 'bidder) 0)
         (check-equal? (result-ref result 'bid) 16)
         (check-equal? (result-ref result 'target) +match-target+)
         (let ((made (result-ref result 'made))
               (delta (result-ref result 'delta)))
           (check-equal? delta (if made 1 -2))
           ;; seat 0 is on the evens team; only their score moves
           (check-equal? (result-ref result 'evens) delta)
           (check-equal? (result-ref result 'odds) 0))
         ;; every seat saw the same broadcast
         (check-equal? (await (first players)
                              (λ (events) (nth-hand-result events 1)))
                       result))
       (check-equal? (take ((events-tagged 'bid-placed)
                            (unbox (scripted-events host)))
                           4)
                     '((bid-placed 0 16) (bid-placed 1 pass)
                       (bid-placed 2 pass) (bid-placed 3 pass)))

       ;; emotes: the whitelist broadcasts to every seat, junk and rapid
       ;; repeats are dropped (per-sender cooldown)
       (let ((guest (first players)))
         (send-msg (scripted-connection guest) '(emote match-j3 laugh))
         (send-msg (scripted-connection guest) '(emote match-j3 fire))   ; throttled
         (send-msg (scripted-connection guest) '(emote match-j3 hack!))  ; not a word
         (check-equal? (await host (λ (events) (findf (tagged 'emote-played) events))
                              #:label 'emote-seen)
                       '(emote-played 0 laugh))
         (sleep 0.5)
         (check-equal? (length ((events-tagged 'emote-played)
                                (unbox (scripted-events host))))
                       1
                       "the cooldown swallows the second emote"))

       ;; only the host can advance the match
       (send-msg (scripted-connection host) '(next-hand match-host))

       ;; hand 2: the opener rotates to seat 1 (observed on the wire)
       (let* ((result (await host (λ (events) (nth-hand-result events 2))
                             #:label 'hand-2))
              (evens-after-1 (result-ref
                              (nth-hand-result (unbox (scripted-events host)) 1)
                              'evens))
              (made (result-ref result 'made)))
         (check-equal? (result-ref result 'hand) 2)
         (check-equal? (result-ref result 'bidder) 1)
         (check-equal? (result-ref result 'bid) 16)
         ;; seat 1 is on the odds team: evens carries over, odds moves
         (check-equal? (result-ref result 'evens) evens-after-1)
         (check-equal? (result-ref result 'odds) (if made 1 -2)))
       (check-equal? (list-ref ((events-tagged 'bid-placed)
                                (unbox (scripted-events host)))
                               4)
                     '(bid-placed 1 16))

       ;; abandon the match (everyone leaves)
       (for-each close-scripted! players)
       (sleep 1.5)))

   ;; a guest dead past their grace at the between-hands gate: the table is
   ;; told who abandoned, and choosing to end the match aborts it
   (test-begin
     (let* ((players (seat-scripted-table 'abort-room
                                          '(abort-host abort-j1 abort-j2 abort-j3)))
            (host (last players))
            (guest (first players)))
       (send-msg (scripted-connection host) '(start-game abort-room))
       (await guest (λ (events) (nth-hand-result events 1)) #:label 'abort-hand-1)
       (close-scripted! guest)
       (check-equal? (await host (λ (events)
                                   (findf (tagged 'seat-abandoned) events))
                            #:label 'guest-abandons)
                     '(seat-abandoned 0 abort-j3))
       (send-msg (scripted-connection host) '(close-game abort-host))
       (check-pred pair?
                   (await host (λ (events)
                                 (findf (tagged 'game-aborted) events))
                          #:label 'game-aborted))
       ;; the table was delisted when the game started and stays gone
       (let ((probe (connect-to-ws)))
         (check-pred null? (cadr (get-active-rooms probe)))
         (ws-close! probe))
       (for-each close-scripted! (remove guest players))
       (sleep 1.5)))

   ;; the HOST dead past grace at the gate — regression for the custodian
   ;; teardown (the match thread is spawned from the host's dispatch) and
   ;; now also for the abandonment flow reaching the guests
   (test-begin
     (let* ((players (seat-scripted-table 'host-abort-room
                                          '(habort-host habort-j1 habort-j2 habort-j3)))
            (host (last players))
            (guest (first players)))
       (send-msg (scripted-connection host) '(start-game host-abort-room))
       (await host (λ (events) (nth-hand-result events 1)) #:label 'habort-hand-1)
       (close-scripted! host)
       (check-equal? (await guest (λ (events)
                                    (findf (tagged 'seat-abandoned) events))
                            #:label 'host-abandons)
                     '(seat-abandoned 3 habort-host))
       (send-msg (scripted-connection guest) '(close-game habort-j3))
       (check-pred pair?
                   (await guest (λ (events)
                                  (findf (tagged 'game-aborted) events))
                          #:label 'host-game-aborted))
       (for-each close-scripted! (remove host players))
       (sleep 1.5)))

   ;; the full abandonment arc, while someone ELSE is being waited on: the
   ;; drop is noticed promptly, a bot replaces the seat on request, the
   ;; hand plays out, and a second abandonment answered with close-game
   ;; ends the match for everyone
   (test-begin
     (let* ((players (seat-scripted-table
                      'leave-room
                      '(leave-host leave-j1 leave-j2 leave-j3)
                      ;; seat 0 (the last joiner) goes AFK on the opening
                      ;; bid, parking the game thread on their channel
                      #:mute (hash 'leave-j3 '(request-bid))))
            (host (last players))
            (afk (first players))
            (leaver (caddr players))) ; seat 2, never the active player here
       (send-msg (scripted-connection host) '(start-game leave-room))
       (await afk (λ (events) (findf (tagged 'request-bid) events))
              #:label 'afk-asked)

       ;; an explicit leave skips the grace window entirely
       (send-msg (scripted-connection leaver) '(leave-game leave-j1))
       (check-equal? (await host (λ (events)
                                   (findf (tagged 'seat-abandoned) events))
                            #:timeout 10
                            #:label 'leaver-noticed)
                     '(seat-abandoned 2 leave-j1))

       ;; replace them: the seat becomes a bot, everyone learns the new name
       (send-msg (scripted-connection host) '(replace-with-bot leave-host))
       (let ((replaced (await host (λ (events)
                                     (findf (tagged 'seat-replaced) events))
                              #:label 'seat-replaced)))
         (check-equal? (cadr replaced) 2)
         (check-pred (λ (n) (regexp-match? #rx"^bot-" (format "~a" n)))
                     (caddr replaced))
         (check-equal? (length (cadddr replaced)) 4))

       ;; wake the parked opener — and keep answering on their behalf: the
       ;; replacement bot bids for real, so the auction can come back round
       ;; to the muted seat before it settles
       (send-msg (scripted-connection afk) '(put-bid leave-j3 16))
       (let answer-loop ((answered 1))
         (let* ((events (unbox (scripted-events afk)))
                (asks (length ((events-tagged 'request-bid) events)))
                (settled (findf (tagged 'bid-result) events)))
           (cond
             (settled (void))
             ((> asks answered)
              (send-msg (scripted-connection afk) '(put-bid leave-j3 pass))
              (answer-loop asks))
             (else (sleep 0.2) (answer-loop answered)))))
       (check-pred pair?
                   (await host (λ (events) (nth-hand-result events 1))
                          #:label 'hand-completes-with-bot))

       ;; a second abandonment, answered with close-game, ends the match
       (send-msg (scripted-connection (cadr players)) '(leave-game leave-j2))
       (await host (λ (events)
                     (findf (λ (m) (and ((tagged 'seat-abandoned) m)
                                        (equal? (cadr m) 1)))
                            events))
              #:timeout 10
              #:label 'second-abandonment)
       (send-msg (scripted-connection host) '(close-game leave-host))
       (check-pred pair?
                   (await host (λ (events) (findf (tagged 'game-aborted) events))
                          #:timeout 10
                          #:label 'table-chose-to-end))
       (for-each close-scripted! players)
       (sleep 1.5)))

   ;; a dropped player reconnects mid-hand: the seat survives, the snapshot
   ;; restores their view, the match plays on — and a deliberate leave
   ;; afterwards ignores the grace window entirely
   (test-begin
     (putenv "RECONNECT-GRACE" "30")
     (let* ((players (seat-scripted-table
                      'resume-room
                      '(resume-host resume-j1 resume-j2 resume-j3)
                      ;; park the game on seat 0's opening bid so the drop
                      ;; and the rejoin happen at a known point
                      #:mute (hash 'resume-j3 '(request-bid))))
            (host (last players))
            (afk (first players))
            (dropper (caddr players)) ; seat 2 = resume-j1
            (rejoined #f))
       (send-msg (scripted-connection host) '(start-game resume-room))
       (await afk (λ (events) (findf (tagged 'request-bid) events))
              #:label 'resume-asked)
       (close-scripted! dropper)
       (check-pred pair?
                   (await host (λ (events)
                                 (findf (tagged 'player-disconnected) events))
                          #:label 'drop-noticed))

       ;; reconnect: same email on a fresh socket rebinds the seat
       (let* ((connection (connect-to-ws)))
         (check-equal? (connect-user connection 'resume-j1) 'user-reconnected)
         (send-msg connection '(rejoin resume-j1))
         (let* ((snapshot (recv-until connection (tagged 'game-snapshot)))
                (body (cadr snapshot)))
           (check-equal? (cdr (assoc 'your-seat body)) 2)
           (check-equal? (cdr (assoc 'stage body)) 'bidding)
           (check-equal? (cdr (assoc 'hand-number body)) 1)
           (check-equal? (cdr (assoc 'awaiting body)) 0)
           (check-equal? (cdr (assoc 'request body)) #f)
           (check-equal? (length (cdr (assoc 'your-hand body))) 4)
           (check-equal? (cdr (assoc 'trump body)) #f)
           ;; live again, playing from the restored hand
           (set! rejoined
                 (spawn-scripted 'resume-j1 connection
                                 #:initial-hand (cdr (assoc 'your-hand body))))))
       (check-pred pair?
                   (await host (λ (events)
                                 (findf (tagged 'player-reconnected) events))
                          #:label 'return-noticed))

       ;; wake the parked opener; the hand must now play out normally for
       ;; everyone, the reconnected seat included
       (send-msg (scripted-connection afk) '(put-bid resume-j3 16))
       (check-pred pair?
                   (await host (λ (events) (nth-hand-result events 1))
                          #:label 'resumed-hand-result))
       (check-pred pair?
                   (await rejoined (λ (events) (nth-hand-result events 1))
                          #:label 'rejoiner-sees-result))

       ;; leaving on purpose skips the grace: the table is asked at once,
       ;; and closing ends the match despite the long reconnect window
       (send-msg (scripted-connection host) '(leave-game resume-host))
       (check-pred pair?
                   (await afk (λ (events) (findf (tagged 'seat-abandoned) events))
                          #:timeout 10
                          #:label 'leave-asks-immediately))
       (send-msg (scripted-connection afk) '(close-game resume-j3))
       (check-pred pair?
                   (await afk (λ (events) (findf (tagged 'game-aborted) events))
                          #:timeout 10
                          #:label 'closing-ends-the-match))
       (close-scripted! rejoined)
       (for-each close-scripted! players)
       (sleep 1.5))
     (putenv "RECONNECT-GRACE" "2"))

   ;; MATCH-TARGET=0 ends the match after exactly one hand, whatever the
   ;; cards: a made bid lifts the bidders to the target, a set leaves the
   ;; defenders already on it. (Target 1 would be probabilistic — under
   ;; +1/−2 scoring a team of random players may never climb to +1.)
   (test-begin
     (putenv "MATCH-TARGET" "0")
     (let* ((players (seat-scripted-table 'target-room
                                          '(target-host target-j1 target-j2 target-j3)))
            (host (last players)))
       (send-msg (scripted-connection host) '(start-game target-room))
       (let* ((over (await host (λ (events) (findf (tagged 'match-over) events))
                           #:label 'match-over))
              (body (cadr over))
              (result (nth-hand-result (unbox (scripted-events host)) 1))
              (made (result-ref result 'made)))
         (check-equal? (result-ref result 'target) 0)
         (check-equal? (cdr (assoc 'hands body)) 1)
         (check-equal? (cdr (assoc 'winner body)) (if made 'evens 'odds))
         ;; match-over repeats the totals the final hand-result reported
         (check-equal? (cdr (assoc 'evens body)) (result-ref result 'evens))
         (check-equal? (cdr (assoc 'odds body)) (result-ref result 'odds)))
       (for-each close-scripted! players))
     (putenv "MATCH-TARGET" ""))

   ;; with hand-strength bidding, a table of bots (plus one bot-brained
   ;; host) makes its bids at a healthy rate — the old always-chase bots
   ;; were set every single hand, so their score could only fall and a
   ;; match could never end
   (test-begin
     (let ((connection (connect-to-ws)))
       (connect-user connection 'botmatch-host)
       (check-equal? (create-room connection 'botmatch-host 'botmatch-room)
                     'room-created)
       (let ((host (spawn-scripted 'botmatch-host connection
                                   #:auto-next? #t #:smart? #t)))
         (for-each (λ (_) (send-msg connection '(add-bot-to-room botmatch-room)))
                   '(1 2 3))
         (await host
                (λ (events)
                  (findf (λ (m) (and (pair? m)
                                     (eq? (car m) 'room-members)
                                     (= (length (caddr m)) 4)))
                         events))
                #:label 'bots-seated)
         (send-msg connection '(start-game botmatch-room))
         (let ((found (await host
                             (λ (events)
                               (let ((results ((events-tagged 'hand-result) events)))
                                 (or (findf (λ (r) (equal? (result-ref r 'made) #t))
                                            results)
                                     (and (>= (length results) 15) 'never-made))))
                             #:timeout 90
                             #:label 'bots-make-a-bid)))
           (check-pred pair? found "a bid is made within fifteen hands"))
         (close-scripted! host)
         ;; the host vanishing tears the match down (suite grace is 2s)
         (sleep 3))))

   ;; stop server after tests
   (*service*)))

(run-tests game-tests)
