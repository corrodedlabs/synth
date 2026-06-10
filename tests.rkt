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

(define (spawn-scripted email connection
                        #:mute (mute '())
                        #:initial-hand (initial-hand '()))
  (define events (box '()))
  (define hand (box initial-hand))
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
                ((request-bid)
                 (let ((min-bid (cadr message)))
                   (send-msg connection
                             `(put-bid ,email ,(if (= min-bid +min-bid+)
                                                   +min-bid+
                                                   'pass)))))
                ((choose-trump)
                 (send-msg connection `(selected-trump ,email diamond)))
                ((play-card)
                 (let* ((cards-played (cdr (cadr message)))
                        (game-state (cdr (caddr message)))
                        (first-suit (cdr (assoc 'first-suit game-state)))
                        (card (findf (λ (c) (valid-card? c first-suit
                                                         cards-played
                                                         (unbox hand)))
                                     (unbox hand))))
                   (set-box! hand (remove card (unbox hand)))
                   (send-msg connection `(card-played ,email ,card))))
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
     (let ((connection (connect-to-ws)))
       ;; replies are tagged: (active-rooms <rooms>)
       (check-pred null? (cadr (get-active-rooms connection)))

       (connect-user connection user1-id)

       (check-equal? (create-room connection user1-id room-name)
                     'room-created)

       (let ((rooms (cadr (get-active-rooms connection))))
         (check-equal? (length rooms) 1))

       ;; hosting user leaves: the table must not outlive them and pollute
       ;; the room list the later tests assert on
       (ws-close! connection)
       (sleep 0.5)))

   ;; match flow: per-hand game points accumulate and the deal rotates
   (test-begin
     (let* ((players (seat-scripted-table 'match-room
                                          '(match-host match-j1 match-j2 match-j3)))
            (host (last players)))
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

   ;; a guest disconnecting at the between-hands gate aborts the match
   (test-begin
     (let* ((players (seat-scripted-table 'abort-room
                                          '(abort-host abort-j1 abort-j2 abort-j3)))
            (host (last players))
            (guest (first players)))
       (send-msg (scripted-connection host) '(start-game abort-room))
       (await guest (λ (events) (nth-hand-result events 1)) #:label 'abort-hand-1)
       (close-scripted! guest)
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

   ;; the HOST disconnecting at the gate must abort too — regression test:
   ;; the match thread is spawned while dispatching the host's start-game,
   ;; so without a detached custodian the host's connection teardown would
   ;; silently kill it and strand the guests at the gate forever
   (test-begin
     (let* ((players (seat-scripted-table 'host-abort-room
                                          '(habort-host habort-j1 habort-j2 habort-j3)))
            (host (last players))
            (guest (first players)))
       (send-msg (scripted-connection host) '(start-game host-abort-room))
       (await host (λ (events) (nth-hand-result events 1)) #:label 'habort-hand-1)
       (close-scripted! host)
       (check-pred pair?
                   (await guest (λ (events)
                                  (findf (tagged 'game-aborted) events))
                          #:label 'host-game-aborted))
       (for-each close-scripted! (remove host players))
       (sleep 1.5)))

   ;; leaving (= closing the socket) while someone ELSE is being waited on
   ;; must still abort promptly: every wait watches all four seats, not
   ;; just the player whose turn it is
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
       (close-scripted! leaver)
       (check-pred pair?
                   (await host (λ (events) (findf (tagged 'game-aborted) events))
                          #:timeout 10
                          #:label 'leaver-aborts-promptly))
       (for-each close-scripted! (remove leaver players))
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

       ;; leaving on purpose aborts at once, long grace notwithstanding
       (send-msg (scripted-connection host) '(leave-game resume-host))
       (check-pred pair?
                   (await afk (λ (events) (findf (tagged 'game-aborted) events))
                          #:timeout 10
                          #:label 'leave-aborts-immediately))
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

   ;; stop server after tests
   (*service*)))

(run-tests game-tests)
