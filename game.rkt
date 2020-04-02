#lang racket

(provide deck)

(require racket/struct)

;; 28 is usually played by four players in fixed partnerships, partners facing each other.
;; 
;; 32 cards from a standard 52-card pack are used for play.
;; There are eight cards in each of the usual "French" suits: hearts, diamonds, clubs and spades.
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
  #:methods gen:custom-write
  ((define write-proc
     (make-constructor-style-printer
      (λ (obj) 'card)
      (λ (obj) (match obj
                 [(card name rank suit point) (list name rank suit point)]))))))

(define suits '(club diamond heart spade))

(define card-names '(jack queen king ace ten nine eight seven))
(define card-points '((jack . 3) (nine . 2) (ace . 1) (ten . 1)))

(define card-names->ranks (compose1 reverse range length))

(define +num-players+ 4)

;; applicable deck for the game
(define deck (apply append
                    (map (lambda (name rank)
                           (map (lambda (suit)
                                  (let [(point (assoc card card-points))]
                                    (card name rank suit (if point (cdr point) 0))))
                                suits))
                         card-names
                         (card-names->ranks card-names))))

(define shuffle-deck shuffle)


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

(define-values (dealt-cards deck1) (distribute-cards deck 4))
(check-for-redeal dealt-cards)

;;
;; After the first player has bid, subsequent players, in counter-clockwise order, may either bid
;; *higher* or *pass*. The auction continues for as many rounds as necessary until three players pass
;; in succession.
;;
(define start-bidding
  (λ () #f))

;; The final bidder chooses a trump suit on the basis of his or her four cards,
;; and places a card of this suit from the set of non-playable cards(2-5) ace down.
;; The trump indicator card is not shown to the other players,
;; who therefore will not know at first what suit is trumps: it remains face down in front of the
;; bidder until at some point during the play someone calls for the trump suit to be exposed.
;; 