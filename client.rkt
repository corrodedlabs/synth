#lang racket

(require net/rfc6455)
(require net/url)

(require racket/port)
(require racket/serialize)

(require "./game.rkt")

(provide connect-to-ws
         connect-user
         get-active-rooms
         create-room
         join-room
         client-lambda
         spawn-bot)

;; resolved lazily so server.rkt can set GAME-PORT from its --port flag
;; before the first bot connects
(define (+ws-url+)
  (format "ws://localhost:~a/test" (or (getenv "GAME-PORT") "8081")))
(define protocol 'rfc6455)

(define connect-to-ws
  (λ ()
    (ws-connect (string->url (+ws-url+)) #:protocol protocol)))

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

;; per-hand bot memory:
;; hand => cards we hold; trump => the trump suit when we know it (we chose
;; it, or it was exposed); chose? => we are this hand's bidder; exposed? =>
;; the trump is public; seen => every card played this hand (broadcasts);
;; must-trump? => we just called the exposure, the next play must be a trump
(struct state (user hand trump chose? exposed? seen must-trump?))

(define default-state
  (λ (user) (state user '() #f #f #f '() #f)))

;; --- hand evaluation (pure helpers, exercised by the test submodule) ---

(define (cards-of-suit hand suit)
  (filter (λ (c) (equal? (card-suit c) suit)) hand))

(define +suits+ '(club diamond heart spade))

(define (longest-suit hand)
  (argmax (λ (suit) (length (cards-of-suit hand suit))) +suits+))

(define (hand-points hand)
  (apply + (map card-point hand)))

;; cheapest first: prefer giving away no points, then the weakest rank
(define (lowest-value cards)
  (argmin (λ (c) (+ (* 10 (card-point c)) (card-rank c))) cards))

(define (highest-rank cards) (argmax card-rank cards))
(define (lowest-rank cards) (argmin card-rank cards))

;; how far this 4-card hand should chase the auction. Holding P of the 28
;; card points in the first four cards, the expected team total is about
;; 14 + P/2 (the unseen 24 cards split evenly); a long suit adds a little
;; trump control. Bids beyond 21 are double-stakes gambles — never chase.
(define (bid-appetite hand)
  (min 21
       (+ 14
          (quotient (hand-points hand) 2)
          (max 0 (sub1 (length (cards-of-suit hand (longest-suit hand))))))))

;; the server's minimum is 16 exactly when the auction is unopened, and the
;; opener is not allowed to pass
(define (choose-bid hand min-bid)
  (if (or (= min-bid +min-bid+) (<= min-bid (bid-appetite hand)))
      min-bid
      'pass))

;; trump: our longest suit; points break ties (chosen from the first four
;; cards — the auction precedes the second deal)
(define (choose-trump-suit hand)
  (argmax (λ (suit)
            (+ (* 10 (length (cards-of-suit hand suit)))
               (hand-points (cards-of-suit hand suit))))
          +suits+))

;; the card currently winning the trick (#f when leading). Approximation:
;; once the trump is public every trump-suit card in the trick counts.
(define (trick-best cards-played first-suit trump exposed?)
  (let ((trumps (if (and exposed? trump)
                    (filter (λ (c) (equal? (card-suit c) trump)) cards-played)
                    '())))
    (cond
      ((pair? trumps) (highest-rank trumps))
      (else (let ((followers (filter (λ (c) (equal? (card-suit c) first-suit))
                                     cards-played)))
              (and (pair? followers) (highest-rank followers)))))))

;; our partner played two cards before us in the trick
(define (partner-winning? cards-played best)
  (and best
       (>= (length cards-played) 2)
       (equal? best (list-ref cards-played (- (length cards-played) 2)))))

;; is this card the strongest of its suit still out there? (everything
;; higher is already seen or in our own hand)
(define (boss-card? card seen hand)
  (let ((suit (card-suit card)))
    (andmap (λ (other)
              (or (<= (card-rank other) (card-rank card))
                  (not (equal? (card-suit other) suit))
                  (member other seen)
                  (member other hand)))
            initial-deck)))

;; choose a lead: bank a sure trick when we have one (prefer the fattest
;; boss outside the trump suit), otherwise develop our longest suit cheaply
(define (choose-lead hand trump exposed? seen)
  (let* ((non-trump (filter (λ (c) (not (equal? (card-suit c) trump))) hand))
         (pool (if (pair? non-trump) non-trump hand))
         (bosses (filter (λ (c) (boss-card? c seen hand))
                         (if exposed? hand pool))))
    (cond
      ((pair? bosses) (argmax (λ (c) (+ (* 10 (card-point c)) (card-rank c))) bosses))
      (else
       (let ((developing (cards-of-suit pool (longest-suit pool))))
         (lowest-value (if (pair? developing) developing pool)))))))

;; choose the card (or the symbol expose-trump) for this trick
(define (choose-play hand cards-played game-state
                     #:trump (trump #f) #:exposed? (exposed? #f)
                     #:seen (seen '()) #:must-trump? (must-trump? #f))
  (let* ((first-suit (cdr (assoc 'first-suit game-state)))
         (trump (or (cdr (assoc 'trump-suit game-state)) trump))
         (exposed? (or exposed?
                       (and (cdr (assoc 'trump-suit game-state)) #t)))
         (followers (and first-suit (cards-of-suit hand first-suit)))
         (trick-points (apply + (map card-point cards-played)))
         (best (and first-suit
                    (trick-best cards-played first-suit trump exposed?))))
    (cond
      ;; we called the exposure: honour the obligation if we hold a trump
      ((and must-trump? trump (pair? (cards-of-suit hand trump)))
       (let* ((trumps (cards-of-suit hand trump))
              (best-trump (and best (equal? (card-suit best) trump) best))
              (winners (if best-trump
                           (filter (λ (c) (> (card-rank c) (card-rank best-trump)))
                                   trumps)
                           trumps)))
         (if (pair? winners) (lowest-rank winners) (lowest-rank trumps))))

      ;; leading
      ((not first-suit) (choose-lead hand trump exposed? seen))

      ;; we can follow suit
      ((pair? followers)
       (let ((beatable (and best
                            (equal? (card-suit best) first-suit)
                            (filter (λ (c) (> (card-rank c) (card-rank best)))
                                    followers))))
         (cond
           ((partner-winning? cards-played best) (lowest-value followers))
           ;; best is a trump: a follower can never win — save the cheap one
           ((and best (not (equal? (card-suit best) first-suit)))
            (lowest-value followers))
           ((and beatable (pair? beatable)) (lowest-rank beatable))
           (else (lowest-value followers)))))

      ;; void with the trump still hidden: call for it when the trick is
      ;; worth fighting for, or when we are the bidder sitting on trumps
      ((and (not exposed?)
            (or (>= trick-points 2)
                (and trump (>= (length (cards-of-suit hand trump)) 2))))
       'expose-trump)

      ;; void with a public trump: ruff a worthwhile trick we can win
      ((and exposed? trump (pair? (cards-of-suit hand trump))
            (not (partner-winning? cards-played best))
            (>= trick-points 2)
            (or (not best)
                (not (equal? (card-suit best) trump))
                (> (card-rank (highest-rank (cards-of-suit hand trump)))
                   (card-rank best))))
       (let* ((trumps (cards-of-suit hand trump))
              (winners (if (and best (equal? (card-suit best) trump))
                           (filter (λ (c) (> (card-rank c) (card-rank best))) trumps)
                           trumps)))
         (lowest-rank winners)))

      ;; nothing to win: throw the cheapest card, clinging to known trumps
      (else
       (let ((spareable (filter (λ (c) (not (equal? (card-suit c) trump))) hand)))
         (lowest-value (if (pair? spareable) spareable hand)))))))

(define receive-data
  (lambda (connection)
    (let ((data (ws-recv connection)))
      (cond
        ((eof-object? data) 'connection-closed)
        ((equal? "#<void>" data) #f)
        (else (displayln (format "received data is ~a" data))
              (read (open-input-string data)))))))

(define send-data
  (lambda (user data)
    (send-ws-message (user-connection user)
                     (append (list (car data) (user-id user))
                             (cdr data)))))

(define client-lambda
  (λ (user)
    (define connection (user-connection user))

    (let loop ((client-state (default-state user))
               (server-msg (receive-data connection)))
      (cond
        ;; server hung up (kicked, room closed, game over) — stop the bot
        ((eq? server-msg 'connection-closed)
         (displayln (format "bot ~a: connection closed, stopping" (user-id user))))
        (server-msg
          (case (car server-msg)
            ((hand)
             (let ((new-cards (caddr server-msg)))
               (loop (struct-copy state client-state
                                  (hand (append (state-hand client-state) new-cards)))
                     (receive-data connection))))
            ((request-bid)
             (let ((min-bid (cadr server-msg)))
               (send-data (state-user client-state)
                          (list 'put-bid
                                (choose-bid (state-hand client-state) min-bid)))
               (loop client-state
                     (receive-data connection))))
            ((choose-trump)
             (let ((trump (choose-trump-suit (state-hand client-state))))
               (send-data (state-user client-state) `(selected-trump ,trump))
               (loop (struct-copy state client-state (trump trump) (chose? #t))
                     (receive-data connection))))
            ((bid-result)
             ;; the auction is settled: per-hand memory starts fresh; the
             ;; trump survives only if we are the bidder who just chose it
             (loop (struct-copy state client-state
                                (trump (and (state-chose? client-state)
                                            (state-trump client-state)))
                                (chose? #f)
                                (exposed? #f)
                                (seen '())
                                (must-trump? #f))
                   (receive-data connection)))
            ((played)
             ;; broadcasts feed the card memory; the exposure marker tells
             ;; everyone trumping is on (the suit arrives with our next ask)
             (let ((what (caddr server-msg)))
               (loop (if (eq? what 'expose-trump)
                         (struct-copy state client-state (exposed? #t))
                         (struct-copy state client-state
                                      (seen (cons what (state-seen client-state)))))
                     (receive-data connection))))
            ((play-card)
             (let* ((cards-played (cdadr server-msg))
                    (game-state (cdaddr server-msg))
                    (hand (state-hand client-state))
                    (server-trump (cdr (assoc 'trump-suit game-state)))
                    (choice (choose-play hand cards-played game-state
                                         #:trump (state-trump client-state)
                                         #:exposed? (state-exposed? client-state)
                                         #:seen (state-seen client-state)
                                         #:must-trump? (state-must-trump? client-state))))
               (send-data (state-user client-state) `(card-played ,choice))
               (loop (struct-copy state client-state
                                  (hand (if (eq? choice 'expose-trump)
                                            hand
                                            (remove choice hand)))
                                  (trump (or server-trump (state-trump client-state)))
                                  (exposed? (or (state-exposed? client-state)
                                                (and server-trump #t)))
                                  (must-trump? (eq? choice 'expose-trump)))
                     (receive-data connection))))
            (else
             (begin (displayln (format "unhandled message ~a" server-msg))
                    (loop client-state (receive-data connection))))))
        (else (loop client-state (receive-data connection)))))))

;; runs the bot AI loop on its own thread for an already-connected user
(define spawn-bot
  (lambda (bot-name connection)
    (thread (λ () (client-lambda (user bot-name connection (make-channel)))))))

(define get-active-rooms
  (lambda (connection)
    (send-ws-message connection '(get-active-rooms))
    (receive-data connection)))

(define connect-user
  (lambda (connection user-id)
    (send-ws-message connection `(connect-user ,user-id "./bot.jpeg"))
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


(module+ test
  (require rackunit)

  (define (find-card name suit)
    (findf (λ (k) (and (equal? (card-name k) name)
                       (equal? (card-suit k) suit)))
           initial-deck))

  (define (cards . name-suit-pairs)
    (map (λ (p) (find-card (car p) (cdr p))) name-suit-pairs))

  ;; --- bidding ---

  (test-case "the opener always bids, whatever the hand"
    (check-equal? (choose-bid (cards '(seven . club) '(eight . heart)
                                     '(queen . spade) '(king . diamond))
                              16)
                  16))

  (test-case "a bare hand never volunteers above the opening bid"
    (let ((junk (cards '(seven . club) '(eight . heart)
                       '(queen . spade) '(king . diamond))))
      (check-equal? (choose-bid junk 17) 'pass)))

  (test-case "a loaded hand chases the auction, but not forever"
    ;; J + 9 + 7 of clubs with an outside jack: 8 points, 3-long suit
    (let ((monster (cards '(jack . club) '(nine . club)
                          '(jack . heart) '(seven . club))))
      (check-equal? (bid-appetite monster) 20 "14 + 8/2 + 2")
      (check-equal? (choose-bid monster 19) 19)
      (check-equal? (choose-bid monster 20) 20)
      (check-equal? (choose-bid monster 21) 'pass)))

  (test-case "an average hand stays in the teens"
    ;; 4 points, two-card suit: 14 + 2 + 1 = 17
    (let ((average (cards '(nine . club) '(seven . club)
                          '(ace . heart) '(ten . spade))))
      (check-equal? (bid-appetite average) 17)
      (check-equal? (choose-bid average 17) 17)
      (check-equal? (choose-bid average 18) 'pass)))

  ;; --- trump choice ---

  (test-case "trump is the longest suit, points breaking ties"
    (check-equal? (choose-trump-suit (cards '(seven . spade) '(eight . spade)
                                            '(jack . heart) '(nine . diamond)))
                  'spade)
    (check-equal? (choose-trump-suit (cards '(seven . spade) '(eight . spade)
                                            '(jack . heart) '(nine . heart)))
                  'heart))

  ;; --- play ---

  (define no-trump-state '((trump-suit . #f) (first-suit . heart)))
  (define (exposed-club first-suit)
    `((trump-suit . club) (first-suit . ,first-suit)))

  (test-case "wins the trick with the cheapest sufficient card"
    (check-equal? (choose-play (cards '(jack . heart) '(king . heart))
                               (cards '(queen . heart) '(eight . heart))
                               no-trump-state)
                  (find-card 'king 'heart)))

  (test-case "feeds the partner's winning card the cheapest follower"
    ;; partner (two back) holds the trick with the hJ
    (check-equal? (choose-play (cards '(king . heart) '(eight . heart))
                               (cards '(jack . heart) '(seven . heart))
                               no-trump-state)
                  (find-card 'eight 'heart)))

  (test-case "dumps the cheapest card when the trick is lost"
    ;; the J is out: save the ten, give up the king
    (check-equal? (choose-play (cards '(ten . heart) '(king . heart))
                               (cards '(jack . heart))
                               no-trump-state)
                  (find-card 'king 'heart)))

  (test-case "void on a rich trick with the trump hidden: call for it"
    (check-equal? (choose-play (cards '(seven . club) '(queen . diamond))
                               (cards '(jack . heart) '(nine . heart))
                               no-trump-state)
                  'expose-trump))

  (test-case "void on a bare trick: just discard"
    (check-equal? (choose-play (cards '(king . club) '(queen . diamond))
                               (cards '(seven . heart) '(eight . heart))
                               no-trump-state)
                  (find-card 'queen 'diamond)))

  (test-case "must-trump honours the exposure with the lowest winning trump"
    (check-equal? (choose-play (cards '(jack . club) '(queen . club) '(king . spade))
                               (cards '(ace . heart))
                               (exposed-club 'heart)
                               #:must-trump? #t)
                  (find-card 'queen 'club)))

  (test-case "ruffs a fat trick with the cheapest winning trump"
    (check-equal? (choose-play (cards '(nine . club) '(seven . club) '(king . spade))
                               (cards '(ten . heart) '(ace . heart))
                               (exposed-club 'heart)
                               #:exposed? #t #:trump 'club)
                  (find-card 'seven 'club)))

  (test-case "never ruffs the partner's trick"
    (check-equal? (choose-play (cards '(nine . club) '(king . spade))
                               (cards '(jack . heart) '(seven . heart))
                               (exposed-club 'heart)
                               #:exposed? #t #:trump 'club)
                  (find-card 'king 'spade)))

  (test-case "leads a boss card when holding one"
    ;; the heart jack is the strongest heart out
    (check-equal? (choose-play (cards '(jack . heart) '(seven . spade))
                               '()
                               '((trump-suit . #f) (first-suit . #f)))
                  (find-card 'jack 'heart)))

  (test-case "a card becomes boss once everything above it is seen"
    (let ((hand (cards '(nine . heart) '(seven . spade))))
      (check-false (boss-card? (find-card 'nine 'heart) '() hand))
      (check-true (boss-card? (find-card 'nine 'heart)
                              (cards '(jack . heart))
                              hand))))

  (test-case "develops the longest suit cheaply without a boss"
    ;; no boss anywhere: lead from the 2-card spade suit, cheapest first
    (check-equal? (choose-play (cards '(eight . spade) '(seven . spade)
                                      '(queen . heart))
                               '()
                               '((trump-suit . #f) (first-suit . #f))
                               #:trump 'club)
                  (find-card 'seven 'spade))))
