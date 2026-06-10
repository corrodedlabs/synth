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

(struct scripted (email connection events))

(define (spawn-scripted email connection)
  (define events (box '()))
  (define hand (box '()))
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
            (when (pair? message)
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
;; last, so (list seat0 seat1 seat2 host)
(define (seat-scripted-table table-name emails)
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
        (map (λ (pair) (spawn-scripted (car pair) (cdr pair)))
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

   ;; with MATCH-TARGET=1 the first made hand ends the match
   (test-begin
     (putenv "MATCH-TARGET" "1")
     (let* ((players (seat-scripted-table 'target-room
                                          '(target-host target-j1 target-j2 target-j3)))
            (host (last players)))
       (send-msg (scripted-connection host) '(start-game target-room))
       (let loop ((hand-number 1))
         (check-true (<= hand-number 25)
                     "a target-1 match should end within 25 hands")
         (await host (λ (events) (nth-hand-result events hand-number))
                #:label 'target-hand)
         (sleep 0.3) ; match-over follows hand-result immediately when due
         (let ((over (findf (tagged 'match-over)
                            (unbox (scripted-events host)))))
           (cond
             (over
              (let ((winner (cdr (assoc 'winner (cadr over))))
                    (evens (cdr (assoc 'evens (cadr over))))
                    (odds (cdr (assoc 'odds (cadr over)))))
                (check-not-false (memq winner '(evens odds)) "winner is a team")
                (check-equal? (cdr (assoc 'hands (cadr over))) hand-number)
                (check-true (>= (if (eq? winner 'evens) evens odds) 1)
                            "the winner reached the target")
                ;; the hand-result that ended it reports the same totals
                (let ((final (nth-hand-result (unbox (scripted-events host))
                                              hand-number)))
                  (check-equal? (result-ref final 'evens) evens)
                  (check-equal? (result-ref final 'odds) odds))))
             (else
              (send-msg (scripted-connection host) '(next-hand target-host))
              (loop (add1 hand-number))))))
       (for-each close-scripted! players))
     (putenv "MATCH-TARGET" ""))

   ;; stop server after tests
   (*service*)))

(run-tests game-tests)
