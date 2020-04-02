#lang racket

(provide deck)

(require racket/struct)

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
          [(null? cards-to-shuffle) (cons dealt-cards
                                          (drop deck num-cards-dealt))]
          [else
           (let-values ([(current-cards rest-cards)
                         (split-at cards-to-shuffle +num-players+)])
             (loop rest-cards
                   (map cons current-cards dealt-cards)))])))))

(distribute-cards deck 4)