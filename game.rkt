#lang racket

(provide initial-deck
         distribute-cards
         start-bidding
         play-game
         card
         card-name
         card-rank
         card-suit
         card-point
         print-card
         valid-card?)

(require racket/struct)
(require racket/control)

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
(struct card (name rank suit point)
  #:prefab
  ;; #:methods gen:custom-write
  ;; ((define write-proc
  ;;    (make-constructor-style-printer
  ;;     (λ (obj) 'card)
  ;;     (λ (obj) (match obj
  ;;                [(card name rank suit point) (list name rank suit point)])))))
  )

(define print-card
  (lambda (obj)
    (match obj
      [(card name rank suit point)
       (displayln (format "card details: ~a" (list name rank suit point)))])))

(define suits '(club diamond heart spade))

(define card-names '(jack queen king ace ten nine eight seven))
(define card-points '((jack . 3) (nine . 2) (ace . 1) (ten . 1)))

(define card-names->ranks (compose1 reverse range length))

(define +num-players+ 4)


(define shuffle-deck shuffle)

;; applicable deck for the game
(define initial-deck
  (shuffle (apply append
                  (map (lambda (name rank)
                         (map (lambda (suit)
                                (let [(point (assoc name card-points))]
                                  (card name rank suit (if point (cdr point) 0))))
                              suits))
                       card-names
                       (card-names->ranks card-names)))))



;; Deal and Bidding

;; Deal and play are counter-clockwise;
;; the cards are shuffled by the dealer and cut by the player to dealer's left.
;; Four cards are then dealt to each player, one at a time.
;;

;;
;; distribute num-cards from deck to each of +num-players+ players
;; returns a cons pair where
;; car is list of list containing the cards distributed
;; cdr is the remaining deck
(define distribute-cards
  (λ (deck num-cards #:shuffle? (shuffle? #f))
    (let ([num-cards-dealt (* num-cards +num-players+)])
      (let loop ([cards-to-shuffle (take (if shuffle? (shuffle-deck deck) deck)
                                         num-cards-dealt)]
                 [dealt-cards (map (λ (i) '()) (range +num-players+))])
        (cond
          [(null? cards-to-shuffle)
           (values dealt-cards (drop deck num-cards-dealt))]
          [else
           (let-values ([(current-cards rest-cards)
                         (split-at cards-to-shuffle +num-players+)])
             (loop rest-cards
                   (map cons current-cards dealt-cards)))])))))


;;
;; Based on these four cards, players bid for the right to choose trumps.
;; Each bid is a number, and the highest bidder undertakes that his or
;; her side will win in tricks at least the number of points bid.
;; The player to dealer's right speaks first, and must bid at least 16.
;; The only exception is if the first player has no point cards at all
;; - only Kings, Queens, 8s and 7s.
;;

(define no-points-card? (λ (card) (> 0 (card-point card))))

;;
;; In this case instead of bidding 14 this player may (but is not obliged to) demand a redeal.
;; All players throw in their cards, there is a new shuffle and cut and the same dealer deals again.
;; The other three players have no right to demand a redeal at this stage however poor
;; their cards may be.

;; check if we want to do a redeal
;; a redeal is possible only if someone receives a hand which does not
;; have any cards with point
;; dealt-cards: list of length +num-players+ where each element
;;   represents the hand of the player
;; returns a boolean representing if redeal is possible

(define check-for-redeal
  (λ (dealt-cards)
    (andmap (λ (cards) (ormap no-points-card? cards)) dealt-cards)))

(define-values (dealt-cards deck1) (distribute-cards initial-deck 4))
(check-for-redeal dealt-cards)

(define (player-rebid-func k pnum bid) (+ 1 bid))

;;
;; After the first player has bid, subsequent players, in counter-clockwise order, may either bid
;; *higher* or *pass*. The auction continues for as many rounds as necessary until three players pass
;; in succession.
;;
;; bidding starts at 16 and can go upto 28
;;
(define start-bidding
  (λ (player-bid-func error-func)
    (let loop ([current-bid-value 16]
               [prev-bid-values '()]
               [player-num 0])
      (displayln (format "prev bid values ~a" prev-bid-values))
      (cond
        [(ormap (λ (bid-value) (equal? bid-value 'pass))
                (if (> 3 (length prev-bid-values))
                    (reverse prev-bid-values)
                    (take (reverse prev-bid-values) 3)))
         (cons current-bid-value player-num)]

        [else
         (let ([bid-value (player-bid-func player-num current-bid-value)])
           (displayln (format "received bid value of ~a from player ~a " bid-value player-num))
           (cond
             [(or (equal? bid-value 28))
              (cons bid-value player-num)]
         
             [(and (integer? bid-value)
                   (or (> bid-value 28)
                       (< bid-value current-bid-value)))
              (begin (error-func player-num 'invalid-bid bid-value)
                     (loop current-bid-value prev-bid-values player-num))]
         
             [else (loop (if (equal? bid-value 'pass) current-bid-value bid-value)
                         (append prev-bid-values (list bid-value))
                         (remainder (+ 1 player-num) +num-players+))]))]))))

(start-bidding
 (λ (player-number current-bid-value)
   (displayln (format "player number ~a current bid value ~a " player-number current-bid-value))
   (+ 1 current-bid-value))
 (λ (player-number error arg)
   (displayln (format "pnum ~a error ~a arg ~a" player-number error arg))))

;; The final bidder chooses a trump suit on the basis of his or her four cards,
;; and places a card of this suit from the set of non-playable cards(2-5) ace down.
;; The trump indicator card is not shown to the other players,
;; who therefore will not know at first what suit is trumps: it remains face down in front of the
;; bidder until at some point during the play someone calls for the trump suit to be exposed.
(define choose-trump-suit
  (λ (player-func player-num)
    (player-func player-num 'choose-trump)))

;; if its the first card then it is valid, otherwise it should be of the suit of the
;; first card played or the trump-suit if trump-suit is exposed
;; or else he can play a card if he does not have any card of first suit
(define valid-card?
  (λ (card first-suit cards-played-in-round trump-suit hand)

    (define (is-card-of-suit? card suit) (equal? suit (card-suit card)))
    
    (and (member card hand)
         (or (equal? (length cards-played-in-round) 0)
             (is-card-of-suit? card first-suit)
             (is-card-of-suit? card trump-suit)
             (andmap (λ (card)
                       (not (is-card-of-suit? card first-suit)))
                     hand)))))


;;
;; The Play
;; ==================================================================================================
;;
;; The play can be divided into two phases: before and after the bidder's face down trump card is
;; exposed.
;;
;; First phase
;; --------------------------------------------------------------------------------------------------
;; The player to the dealer's right leads to the first trick; players must follow suit if possible,
;; the highest card of the suit led wins the trick, and the winner of each trick leads to the next.
;; If you have no card of the suit led you have two options:
;;
;; You may discard any card. This card cannot win the trick.
;; Before playing a card, you may call for the bidder's face down trump to be exposed.
;; In this case, the bidder must turn this trump card face up for all to see.
;; Having called for the trump to be exposed, you must play a trump to this trick if you have one;
;; if you have no trump you may discard any card. From the moment when the trump is exposed,
;; the play enters the second phase -see below.
;;
;; During the first phase, cards of the (concealed) trump suit have no special effect: each trick is
;; won by the highest card of the suit led, even if it also contains cards of the suit that is
;; subsequently revealed as trumps.
;;
;;     Second phase:
;;---------------------------------------------------------------------------------------------------
;; At the moment when the bidder's face down card is exposed, this suit becomes trumps.
;; Each trick is now won by the highest trump in it. Tricks that contain no trumps are won by the
;; highest card of the suit led. Players must follow suit if possible: if unable to follow,
;; they may play a trump or discard a card of another suit, as they like.
;; As before, the winner of each trick leads to the next.
;;     
;; ##  Notes:
;;
;; Cards of the trump suit only become trumps from the moment that the trump card is exposed.
;; Any cards of that suit that were previously played to the trick do not count as trumps.
;;
;; Example: In phase 1 South leads the heart8. Having no hearts, East discards the clubA without
;; asking for trumps, hoping that West will win the trick. North also has no hearts and asks for
;; trumps: the bidder exposes a club and North trumps with the clubQ. West follows suit with the
;; heartA. This trick is won by North.
;; The Queen of clubs is a trump but the Ace of clubs does not count as a trump because it was
;; played before the trump was exposed.
;;
;; player-cards => list of length +num-players+ whose element represents the cards in hand
;; selected-trump-suit => suit selected to be trump
;; player-func => this function will be called with the player number,
;;      list of cards played in the current turn and the state of the game
;;      the return value of this function will represent the card played by the player
;;
;; returns => list of length +num-players+ whose element represent the points won after all the cards
;; have been played
(define play-game
  (λ (player-cards selected-trump-suit player-func)

    ;; returns a cons cell [index-with-max-points . total-points-earned]
    (define calculate-points
      (λ (trump cards)
        (let ([leading-suit (or trump (card-suit (car cards)))]
              [total-points (foldl + 0 (map card-point cards))])
          (let loop ([cards cards]
                     [leading-hand (cons 0 (card-point (car cards)))]
                     [index 0])
            (cond
              [(null? cards) (cons (car leading-hand) total-points)]

              [(< (cdr leading-hand) (card-point (car cards)))
               (loop (cdr cards)
                     (cons index (card-point (car cards)))
                     (+ 1 index))]

              [else (loop (cdr cards) leading-hand (+ 1 index))])))))
          
    (let loop ([player-cards player-cards]
               [trump-suit #f] ;; contains the trump suit when exposed
               [rounds-to-be-played (length (car player-cards))]
               [active-player 0]
               [cards-played-in-round '()]
               [first-suit #f]
               [points-earned (make-immutable-hash (map (λ (i) (cons i 0))
                                                        (range (length player-cards))))])
      (displayln (format "cards in round ~a points-earner ~a "
                         cards-played-in-round
                         points-earned))
      (cond
        [(equal? rounds-to-be-played 0) points-earned]

        [(equal? (length cards-played-in-round) +num-players+)
         (match (calculate-points trump-suit cards-played-in-round)
           [(cons winning-player-index points-won)
            (displayln (format "winner index is ~a" winning-player-index))
            (loop player-cards
                  trump-suit
                  (- rounds-to-be-played 1)
                  winning-player-index
                  '()
                  #f
                  (hash-update points-earned
                               winning-player-index
                               (λ (current-points)
                                 (+ current-points points-won))))])]

        [else
         (displayln (format "esle called ~a" active-player))
         (let ([card-played (player-func active-player
                                         cards-played-in-round
                                         `((trump-suit . ,trump-suit)
                                           (first-suit . ,first-suit)))]
               [player-hand (list-ref player-cards active-player)])
           (cond
             [(equal? card-played 'expose-trump)
              (loop player-cards
                    selected-trump-suit
                    rounds-to-be-played
                    active-player
                    cards-played-in-round
                    first-suit
                    points-earned)]
             
             [(valid-card? card-played
                           first-suit
                           cards-played-in-round
                           trump-suit
                           player-hand)
              (loop (list-set player-cards
                              active-player
                              (remove card-played player-hand))
                    trump-suit
                    rounds-to-be-played
                    (remainder (+ 1 active-player) +num-players+)
                    (cons card-played cards-played-in-round)
                    (if (equal? (length cards-played-in-round) 0)
                        (card-suit card-played)
                        first-suit)
                    points-earned)]

             [else (begin
                     (displayln "invalid card played")
                     (player-func active-player cards-played-in-round trump-suit)
                     (loop trump-suit
                           rounds-to-be-played
                           active-player
                           cards-played-in-round
                           first-suit
                           points-earned))]))]))))

;; Scoring
;; ==================================================================================================
;;     
;; When all eight tricks have been played, each side counts the card points in the tricks it has won.
;; The bidding team needs at least as many card points as the bid to win; otherwise they lose.
;;
;; game-points => list of length +num-players+ whose element represent the points won
;; bid-value => bid points
;; bid-player => player number who set the bid
(define score-game
  (λ (game-points bid-value bid-player)
    #f))
