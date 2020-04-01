#lang racket

(require net/rfc6455)

;; struct to represent a card
(struct card (name rank suit point))

(define suits '(club diamond heart spade))

(define card-names '(jack queen king ace ten nine eight seven))
(define card-points '((jack . 3) (nine . 2) (ace . 1) (ten . 1)))

(define card-names->ranks (compose1 reverse range length))

(define deck (apply append
		    (map (lambda (name rank)
			   (map (lambda (suit)
				  (let [(point (assoc card card-points))]
				    (card name rank suit (if point (cdr point) 0))))
				suits))
			 card-names
			 (card-names->ranks card-names))))

(define stop-service
  (ws-serve #:port 8081 (lambda (c s) (ws-send! c "Hello world!"))))
