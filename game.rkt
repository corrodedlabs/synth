#lang racket

(provide initial-deck
         fresh-deck
         distribute-cards
         check-for-redeal
         start-bidding
         valid-card?
         trick-winner
         play-game
         score-game
         card
         card-name
         card-rank
         card-suit
         card-point
         print-card
         +min-bid+
         +max-bid+)

;; 28 is usually played by four players in fixed partnerships, partners facing each other.
;;
;; 32 cards from a standard 52-card pack are used for play.
;; There are eight cards in each of the usual "French" suits: hearts, diamonds, clubs and
;; spades.
;; The cards in every suit rank from high to low: J-9-A-10-K-Q-8-7. The aim of the game is to win
;; tricks containing valuable cards. The values of the cards are:
;;
;; - Jacks	3 points each
;; - Nines	2 points each
;; - Aces	1 point each
;; - Tens	1 point each
;; - Other cards (K, Q, 8, 7)      	no points
;;
;; This gives a total of 28 points for cards, hence the name of the game.

;; struct to represent a card
(struct card (name rank suit point) #:prefab)

(define print-card
  (lambda (obj)
    (match obj
      [(card name rank suit point)
       (displayln (format "card details: ~a" (list name rank suit point)))])))

(define suits '(club diamond heart spade))

;; listed strongest first; the numeric rank doubles as trick strength,
;; so jack=7 down to seven=0 (J > 9 > A > 10 > K > Q > 8 > 7)
(define card-names '(jack nine ace ten king queen eight seven))
(define card-points '((jack . 3) (nine . 2) (ace . 1) (ten . 1)))

(define card-names->ranks (compose1 reverse range length))

(define +num-players+ 4)

;; canonical (unshuffled) 32-card deck; deal from (fresh-deck), never this
(define initial-deck
  (apply append
         (map (lambda (name rank)
                (map (lambda (suit)
                       (let ([point (assoc name card-points)])
                         (card name rank suit (if point (cdr point) 0))))
                     suits))
              card-names
              (card-names->ranks card-names))))

;; a newly shuffled deck — call once per game
(define (fresh-deck) (shuffle initial-deck))


;; Deal and Bidding

;; Deal and play are counter-clockwise;
;; the cards are shuffled by the dealer and cut by the player to dealer's left.
;; Four cards are then dealt to each player, one at a time.

;; distribute num-cards from deck to each of +num-players+ players, one at a
;; time round-robin; returns (values hands remaining-deck) where hands is a
;; list of +num-players+ card lists. With #:shuffle? the deck is shuffled
;; first and the remainder comes from that same shuffled deck.
(define distribute-cards
  (λ (deck num-cards #:shuffle? (shuffle? #f))
    (let* ([deck (if shuffle? (shuffle deck) deck)]
           [num-cards-dealt (* num-cards +num-players+)])
      (let loop ([cards-to-deal (take deck num-cards-dealt)]
                 [dealt-cards (make-list +num-players+ '())])
        (cond
          [(null? cards-to-deal)
           (values (map reverse dealt-cards) (drop deck num-cards-dealt))]
          [else
           (let-values ([(current-cards rest-cards)
                         (split-at cards-to-deal +num-players+)])
             (loop rest-cards
                   (map cons current-cards dealt-cards)))])))))


;; Based on their first four cards, players bid for the right to choose trumps.
;; The player to dealer's right speaks first, and must bid at least 16. The
;; only exception: if that player has no point cards at all (only K, Q, 8, 7)
;; they may demand a redeal instead.

(define no-points-card? (λ (c) (zero? (card-point c))))

;; a redeal may be demanded only when the first bidder's hand is pointless
;; dealt-cards: list of +num-players+ hands, first bidder's hand first
(define check-for-redeal
  (λ (dealt-cards)
    (andmap no-points-card? (car dealt-cards))))

;; After the first player has bid, subsequent players, in counter-clockwise
;; order, may either bid *higher* or *pass*. The auction ends when three
;; players pass in succession (or someone bids the maximum, 28).

(define +min-bid+ 16)
(define +max-bid+ 28)

;; request-bid-func: (player-index min-bid) -> integer bid or 'pass
;; notify-bid-func: (player-index bid-or-pass) -> void, only for accepted bids
;; error-func: (player-index error-symbol offending-value) -> void; the same
;;   player is then asked again
;; returns (cons winning-bid winner-index)
(define start-bidding
  (λ (request-bid-func notify-bid-func error-func)
    (let loop ([player-num 0]
               [high-bid #f] ; (cons value bidder) once the auction opens
               [consecutive-passes 0])
      (cond
        [(and high-bid (or (equal? (car high-bid) +max-bid+)
                           (>= consecutive-passes (- +num-players+ 1))))
         high-bid]

        [else
         (let* ([min-bid (if high-bid (add1 (car high-bid)) +min-bid+)]
                [bid-value (request-bid-func player-num min-bid)]
                [next-player (remainder (add1 player-num) +num-players+)])
           (cond
             [(equal? bid-value 'pass)
              (cond
                [high-bid
                 (notify-bid-func player-num 'pass)
                 (loop next-player high-bid (add1 consecutive-passes))]
                [else ; the opener must bid
                 (error-func player-num 'must-open-bid min-bid)
                 (loop player-num high-bid consecutive-passes)])]

             [(and (exact-integer? bid-value)
                   (<= min-bid bid-value +max-bid+))
              (notify-bid-func player-num bid-value)
              (loop next-player (cons bid-value player-num) 0)]

             [else
              (error-func player-num 'invalid-bid bid-value)
              (loop player-num high-bid consecutive-passes)]))]))))


;; A card is playable when it is in hand and either leads the trick, follows
;; the suit led, or the hand has no card of the suit led. (Trumping is never
;; an excuse to break suit: the trump suit only matters for who *wins*.)
(define valid-card?
  (λ (c first-suit cards-played-in-round hand)
    (define (of-suit? k suit) (equal? suit (card-suit k)))
    (and (member c hand)
         (or (null? cards-played-in-round)
             (of-suit? c first-suit)
             (not (ormap (λ (k) (of-suit? k first-suit)) hand)))
         #t)))


;; The Play
;; ==================================================================================================
;;
;; The play has two phases: before and after the bidder's face-down trump card
;; is exposed.
;;
;; First phase: the player to the dealer's right leads to the first trick;
;; players must follow suit if possible, the highest card of the suit led wins
;; the trick, and the winner of each trick leads to the next. With no card of
;; the suit led you may discard anything, or first call for the bidder's
;; face-down trump to be exposed — after calling you must play a trump to this
;; trick if you have one. Cards of the (concealed) trump suit have no special
;; effect during this phase.
;;
;; Second phase: from the moment of exposure each trick is won by the highest
;; trump in it; tricks without trumps go to the highest card of the suit led.
;;
;; Cards of the trump suit played to the current trick *before* the exposure
;; do not count as trumps:
;;
;; Example: in phase 1 South leads the heart8. Having no hearts, East discards
;; the clubA without asking for trumps. North also has no hearts and asks for
;; trumps: the bidder exposes a club and North trumps with the clubQ. West
;; follows suit with the heartA. North wins: the clubQ is a trump but the
;; clubA does not count as one because it was played before the exposure.

;; plays: non-empty list of (list seat card counts-as-trump?) in play order;
;; counts-as-trump? marks cards of the trump suit played after the exposure.
;; Returns the seat that wins the trick.
(define trick-winner
  (λ (plays)
    (define play-card second)
    (define led-suit (card-suit (play-card (first plays))))
    (define trumps (filter third plays))
    (define candidates
      (if (null? trumps)
          (filter (λ (p) (equal? led-suit (card-suit (play-card p)))) plays)
          trumps))
    (first (argmax (compose1 card-rank play-card) candidates))))

;; player-cards: list of +num-players+ hands (same length each)
;; selected-trump-suit: the bidder's concealed trump suit
;; request-play-func: (seat cards-played-in-round game-state) -> card or
;;   'expose-trump; game-state is ((trump-suit . suit-or-#f) (first-suit . …))
;;   where trump-suit stays #f until exposed
;; notify-play-func: (seat card-or-'expose-trump) -> void, accepted plays only
;; error-func: (seat error-symbol) -> void; the same seat is then asked again
;; returns an immutable hash seat -> card points taken
(define play-game
  (λ (player-cards selected-trump-suit request-play-func
                   #:notify-play (notify-play-func void)
                   #:error-func (error-func void))

    (define (has-suit? hand suit)
      (ormap (λ (k) (equal? suit (card-suit k))) hand))

    (let loop ([hands player-cards]
               [exposed? #f]
               [plays '()] ; current trick, play order: (list seat card trump?)
               [leader 0]
               [tricks-left (length (car player-cards))]
               [must-trump-seat #f] ; seat that just exposed, if any
               [points-earned (make-immutable-hash
                               (map (λ (i) (cons i 0)) (range +num-players+)))])
      (cond
        [(zero? tricks-left) points-earned]

        [(equal? (length plays) +num-players+)
         (let ([winner (trick-winner plays)]
               [trick-points (apply + (map (compose1 card-point second) plays))])
           (loop hands exposed? '() winner (sub1 tricks-left) #f
                 (hash-update points-earned winner
                              (λ (current) (+ current trick-points)))))]

        [else
         (let* ([seat (remainder (+ leader (length plays)) +num-players+)]
                [first-suit (if (null? plays)
                                #f
                                (card-suit (second (first plays))))]
                [cards-in-round (map second plays)]
                [response (request-play-func
                           seat cards-in-round
                           `((trump-suit . ,(and exposed? selected-trump-suit))
                             (first-suit . ,first-suit)))]
                [hand (list-ref hands seat)])
           (define (retry error-symbol)
             (error-func seat error-symbol)
             (loop hands exposed? plays leader tricks-left must-trump-seat
                   points-earned))
           (cond
             [(equal? response 'expose-trump)
              ;; only a player unable to follow suit may call for the trump
              (if (and (not exposed?)
                       first-suit
                       (not (has-suit? hand first-suit)))
                  (begin
                    (notify-play-func seat 'expose-trump)
                    (loop hands #t plays leader tricks-left seat points-earned))
                  (retry 'invalid-expose))]

             [(not (valid-card? response first-suit cards-in-round hand))
              (retry 'invalid-card)]

             ;; after calling for the exposure you must trump if you can
             [(and (equal? must-trump-seat seat)
                   (has-suit? hand selected-trump-suit)
                   (not (equal? (card-suit response) selected-trump-suit)))
              (retry 'must-play-trump)]

             [else
              (notify-play-func seat response)
              (loop (list-set hands seat (remove response hand))
                    exposed?
                    (append plays
                            (list (list seat response
                                        (and exposed?
                                             (equal? (card-suit response)
                                                     selected-trump-suit)))))
                    leader tricks-left #f points-earned)]))]))))


;; Scoring
;; ==================================================================================================
;;
;; When all eight tricks have been played, each side counts the card points in
;; the tricks it has won. The bidding team needs at least as many card points
;; as the bid; otherwise they are set. Partners face each other: seats 0 and 2
;; against seats 1 and 3.
;;
;; game-points: hash seat -> points won
;; returns (list made? bidding-team-points defending-team-points)
(define score-game
  (λ (game-points bid-value bid-player)
    (let* ([team-points (λ (a b) (+ (hash-ref game-points a 0)
                                    (hash-ref game-points b 0)))]
           [evens (team-points 0 2)]
           [odds (team-points 1 3)]
           [bidder-team (if (even? bid-player) evens odds)]
           [defender-team (if (even? bid-player) odds evens)])
      (list (>= bidder-team bid-value) bidder-team defender-team))))


(module+ test
  (require rackunit)

  (define (find-card name suit)
    (findf (λ (k) (and (equal? (card-name k) name)
                       (equal? (card-suit k) suit)))
           initial-deck))

  ;; --- deck ---

  (test-case "deck has 32 distinct cards, 8 per suit, 28 points total"
    (check-equal? (length initial-deck) 32)
    (check-equal? (length (remove-duplicates initial-deck)) 32)
    (for ([suit suits])
      (check-equal? (count (λ (k) (equal? suit (card-suit k))) initial-deck) 8))
    (check-equal? (apply + (map card-point initial-deck)) 28))

  (test-case "numeric rank follows trick strength J > 9 > A > 10 > K > Q > 8 > 7"
    (define (rank-of name) (card-rank (find-card name 'heart)))
    (check-true (apply > (map rank-of '(jack nine ace ten king queen eight seven)))))

  (test-case "fresh-deck reshuffles without losing cards"
    (define deck-a (fresh-deck))
    (define deck-b (fresh-deck))
    (check-equal? (sort-deck deck-a) (sort-deck initial-deck))
    ;; two shuffles agreeing is a 1-in-32! event
    (check-not-equal? deck-a deck-b))

  ;; --- dealing ---

  (define (check-deal-invariants deck #:shuffle? shuffle?)
    (define-values (hands rest) (distribute-cards deck 4 #:shuffle? shuffle?))
    (check-equal? (map length hands) '(4 4 4 4))
    (check-equal? (length rest) 16)
    (define dealt (apply append hands))
    (check-true (null? (filter (λ (k) (member k rest)) dealt))
                "no card may be both dealt and still in the deck")
    (check-equal? (sort-deck (append dealt rest)) (sort-deck deck)
                  "dealing must preserve the whole deck"))

  (test-case "distribute-cards splits 16/16 with no overlap"
    (check-deal-invariants initial-deck #:shuffle? #f)
    (check-deal-invariants initial-deck #:shuffle? #t)
    (check-deal-invariants (fresh-deck) #:shuffle? #f))

  (test-case "second deal of the remainder covers the full deck"
    (define-values (first-hands rest) (distribute-cards (fresh-deck) 4))
    (define-values (second-hands empty-rest) (distribute-cards rest 4))
    (check-equal? empty-rest '())
    (check-equal? (sort-deck (apply append (append first-hands second-hands)))
                  (sort-deck initial-deck)))

  ;; --- redeal ---

  (test-case "redeal only when the first bidder holds no point cards"
    (define pointless (map (λ (suit) (find-card 'king suit)) suits))
    (define pointy (list (find-card 'jack 'heart)))
    (check-true (check-for-redeal (list pointless pointy pointy pointy)))
    (check-false (check-for-redeal
                  (list (cons (find-card 'ten 'club) (cdr pointless))
                        pointy pointy pointy)))
    ;; other players' weak hands give no right to a redeal
    (check-false (check-for-redeal (list pointy pointless pointless pointless))))

  ;; --- bidding ---

  ;; drives start-bidding from a per-player queue of responses
  (define (run-auction scripts)
    (define remaining (make-vector +num-players+ '()))
    (for ([i (range +num-players+)])
      (vector-set! remaining i (list-ref scripts i)))
    (define bids '())
    (define errors '())
    (define result
      (start-bidding
       (λ (player min-bid)
         (let ([script (vector-ref remaining player)])
           (check-false (null? script)
                        (format "player ~a asked with nothing scripted" player))
           (vector-set! remaining player (cdr script))
           (let ([entry (car script)])
             (if (procedure? entry) (entry min-bid) entry))))
       (λ (player bid) (set! bids (append bids (list (cons player bid)))))
       (λ (player err value) (set! errors (append errors (list (cons player err)))))))
    (values result bids errors))

  (test-case "auction ends after three consecutive passes, not one"
    (define-values (result bids errors)
      (run-auction '((16) (pass) (pass) (pass))))
    (check-equal? result '(16 . 0))
    (check-equal? bids '((0 . 16) (1 . pass) (2 . pass) (3 . pass)))
    (check-equal? errors '()))

  (test-case "a single pass does not end the auction"
    ;; p0 16, p1 pass, p2 17, p3 pass, p0 pass, p1 pass -> p2 wins at 17
    (define-values (result bids errors)
      (run-auction '((16 pass) (pass pass) (17) (pass))))
    (check-equal? result '(17 . 2)))

  (test-case "the winning bid belongs to the player who made it"
    ;; p0..p3 raise 16 17 18 19, then three passes
    (define-values (result bids errors)
      (run-auction (list (list 16 'pass) (list 17 'pass) (list 18 'pass) (list 19))))
    (check-equal? result '(19 . 3)))

  (test-case "a bid of 28 ends the auction immediately"
    (define-values (result bids errors)
      (run-auction '((28) () () ())))
    (check-equal? result '(28 . 0)))

  (test-case "the opener must bid: pass and junk are rejected and re-asked"
    (define-values (result bids errors)
      (run-auction '((pass 15 16) (pass) (pass) (pass))))
    (check-equal? result '(16 . 0))
    (check-equal? errors '((0 . must-open-bid) (0 . invalid-bid))))

  (test-case "later bids must be strictly higher than the standing bid"
    (define-values (result bids errors)
      (run-auction '((17) (17 pass) (29 pass) (pass))))
    (check-equal? result '(17 . 0))
    (check-equal? errors '((1 . invalid-bid) (2 . invalid-bid))))

  (test-case "the asked minimum tracks the auction"
    ;; p0 opens 20, p1 passes, p2 raises to 25, then everyone passes
    (define asked '())
    (define (record-min min-bid) (set! asked (append asked (list min-bid))))
    (run-auction (list (list (λ (m) (record-min m) 20) 'pass)
                       (list (λ (m) (record-min m) 'pass) 'pass)
                       (list (λ (m) (record-min m) 25) 'pass)
                       (list (λ (m) (record-min m) 'pass))))
    (check-equal? (take asked 4) '(16 21 21 26)))

  ;; --- card legality ---

  (let ([h8 (find-card 'eight 'heart)]
        [hk (find-card 'king 'heart)]
        [c7 (find-card 'seven 'club)]
        [cj (find-card 'jack 'club)])
    (test-case "any held card may lead"
      (check-true (valid-card? c7 #f '() (list c7 hk)))
      (check-false (valid-card? cj #f '() (list c7 hk)) "card not in hand"))

    (test-case "must follow suit when able"
      (check-true (valid-card? hk 'heart (list h8) (list hk c7)))
      (check-false (valid-card? c7 'heart (list h8) (list hk c7))
                   "club is no discard while holding hearts — even a trump club"))

    (test-case "void in the suit led, anything goes"
      (check-true (valid-card? c7 'heart (list h8) (list c7 cj)))))

  ;; --- trick winner ---

  (define (play seat name suit #:trump? (trump? #f))
    (list seat (find-card name suit) trump?))

  (test-case "highest card of the suit led wins; rank order is J 9 A 10 K Q 8 7"
    (check-equal? (trick-winner (list (play 0 'eight 'heart)
                                      (play 1 'ace 'heart)
                                      (play 2 'nine 'heart)
                                      (play 3 'ten 'heart)))
                  2)
    (check-equal? (trick-winner (list (play 1 'seven 'spade)
                                      (play 2 'king 'spade)
                                      (play 3 'queen 'spade)
                                      (play 0 'ten 'spade)))
                  0))

  (test-case "discards never win, whatever their points"
    (check-equal? (trick-winner (list (play 2 'seven 'heart)
                                      (play 3 'jack 'club) ; off-suit discard
                                      (play 0 'jack 'diamond)
                                      (play 1 'eight 'heart)))
                  1))

  (test-case "exposed trumps beat the suit led; highest trump wins"
    (check-equal? (trick-winner (list (play 0 'ace 'heart)
                                      (play 1 'seven 'club #:trump? #t)
                                      (play 2 'king 'heart)
                                      (play 3 'queen 'club #:trump? #t)))
                  3))

  (test-case "trump-suit cards played before the exposure do not count"
    ;; the documented example: South h8, East clubA (before), North clubQ
    ;; (after exposing), West heartA -> North wins
    (check-equal? (trick-winner (list (play 0 'eight 'heart)
                                      (play 1 'ace 'club)
                                      (play 2 'queen 'club #:trump? #t)
                                      (play 3 'ace 'heart)))
                  2))

  ;; --- play-game ---

  ;; drives play-game from per-seat response queues
  (define (run-hand hands trump scripts)
    (define remaining (make-vector +num-players+ '()))
    (for ([i (range +num-players+)])
      (vector-set! remaining i (list-ref scripts i)))
    (define notified '())
    (define errors '())
    (define points
      (play-game hands trump
                 (λ (seat cards-in-round game-state)
                   (let ([script (vector-ref remaining seat)])
                     (check-false (null? script)
                                  (format "seat ~a asked with nothing scripted" seat))
                     (vector-set! remaining seat (cdr script))
                     (car script)))
                 #:notify-play (λ (seat what)
                                 (set! notified (append notified (list (cons seat what)))))
                 #:error-func (λ (seat err)
                                (set! errors (append errors (list (cons seat err)))))))
    (values points notified errors))

  (define (cards . name-suit-pairs)
    (map (λ (p) (find-card (car p) (cdr p))) name-suit-pairs))

  (test-case "tricks go to the winner, who leads next; points add up"
    ;; trick 1: sA sK s7 s8 -> seat 0 wins 1 point and leads
    ;; trick 2: h7 h9 h8 hJ -> seat 3 wins 5 points
    (define hands (list (cards '(ace . spade) '(seven . heart))
                        (cards '(king . spade) '(nine . heart))
                        (cards '(seven . spade) '(eight . heart))
                        (cards '(eight . spade) '(jack . heart))))
    (define-values (points notified errors)
      (run-hand hands 'diamond
                (list (cards '(ace . spade) '(seven . heart))
                      (cards '(king . spade) '(nine . heart))
                      (cards '(seven . spade) '(eight . heart))
                      (cards '(eight . spade) '(jack . heart)))))
    (check-equal? errors '())
    (check-equal? points (hash 0 1 1 0 2 0 3 5))
    (check-equal? (apply + (hash-values points)) 6))

  (test-case "the full documented exposure example plays out"
    ;; South(0) h8, East(1) discards clubA, North(2) exposes then clubQ,
    ;; West(3) heartA -> North takes the 2 points
    (define hands (list (cards '(eight . heart))
                        (cards '(ace . club))
                        (cards '(queen . club))
                        (cards '(ace . heart))))
    (define-values (points notified errors)
      (run-hand hands 'club
                (list (cards '(eight . heart))
                      (cards '(ace . club))
                      (append '(expose-trump) (cards '(queen . club)))
                      (cards '(ace . heart)))))
    (check-equal? errors '())
    (check-equal? points (hash 0 0 1 0 2 2 3 0))
    (check-equal? (cadr notified) (cons 1 (find-card 'ace 'club)))
    (check-equal? (caddr notified) (cons 2 'expose-trump)))

  (test-case "after exposing you must trump while you can"
    (define hands (list (cards '(eight . heart) '(seven . heart))
                        (cards '(ace . club) '(seven . club))
                        (cards '(queen . club) '(seven . spade))
                        (cards '(ace . heart) '(king . heart))))
    (define-values (points notified errors)
      (run-hand hands 'club
                (list (cards '(eight . heart) '(seven . heart))
                      (cards '(ace . club) '(seven . club))
                      (append '(expose-trump)
                              (cards '(seven . spade)) ; rejected: holds a trump
                              (cards '(queen . club) '(seven . spade)))
                      (cards '(ace . heart) '(king . heart)))))
    (check-equal? errors '((2 . must-play-trump)))
    ;; trick 1 (2 pts) to North's clubQ; trick 2: s7 lead, hK h7 discards,
    ;; East's club7 is now a live trump and takes the 0-point trick
    (check-equal? points (hash 0 0 1 0 2 2 3 0)))

  (test-case "breaking suit is rejected and the seat re-asked"
    (define short-hands (list (cards '(eight . heart) '(seven . spade))
                              (cards '(king . heart) '(ace . spade))
                              (cards '(nine . heart) '(eight . spade))
                              (cards '(ten . heart) '(nine . spade))))
    (define-values (points notified errors)
      (run-hand short-hands 'club
                (list (cards '(eight . heart) '(seven . spade))
                      (append (cards '(ace . spade)) ; rejected: holds hearts
                              (cards '(king . heart) '(ace . spade)))
                      (cards '(nine . heart) '(eight . spade))
                      (cards '(ten . heart) '(nine . spade)))))
    (check-equal? errors '((1 . invalid-card)))
    ;; the h9 takes trick 1 (h9+h10 = 3 pts); the s9 outranks the sA and
    ;; takes trick 2 (s9+sA = 3 pts)
    (check-equal? points (hash 0 0 1 0 2 3 3 3)))

  (test-case "exposing is only for players unable to follow suit"
    (define hands (list (cards '(eight . heart))
                        (cards '(king . heart))
                        (cards '(nine . heart))
                        (cards '(ace . heart))))
    (define-values (points notified errors)
      (run-hand hands 'club
                (list (cards '(eight . heart))
                      (append '(expose-trump) (cards '(king . heart)))
                      (cards '(nine . heart))
                      (cards '(ace . heart)))))
    (check-equal? errors '((1 . invalid-expose))))

  ;; --- scoring ---

  (test-case "the bidding team is scored against the bid"
    (define points (hash 0 10 1 6 2 9 3 3))
    (check-equal? (score-game points 19 0) '(#t 19 9))
    (check-equal? (score-game points 20 2) '(#f 19 9))
    (check-equal? (score-game points 9 1) '(#t 9 19))
    (check-equal? (score-game points 10 3) '(#f 9 19))))

;; sorts a deck into a canonical order for set comparisons in tests
(define (sort-deck deck)
  (sort deck
        (λ (a b)
          (let ([ka (format "~a~a" (card-suit a) (card-rank a))]
                [kb (format "~a~a" (card-suit b) (card-rank b))])
            (string<? ka kb)))))
