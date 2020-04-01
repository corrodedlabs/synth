#lang racket
;; Demonstrates the ws-connect interface.
;; Public Domain.

(require net/rfc6455)
(require net/url)

(require racket/port)
(require racket/serialize)

(define (recv/print c)
  (ws-recv c))

(define (do-connection protocol)
  (printf "Connecting using protocol ~a...\n" protocol)
  (define c (ws-connect (string->url "ws://localhost:8081/test")
                        #:protocol protocol))
  (ws-send! c (with-output-to-string
                (λ () (write '(make-room ashakdwipeea@gmail.com my-room)))))
  (recv/print c)
  (ws-send! c (with-output-to-string
                (λ () (write '(get-room-details ashakdwipeea@gmail.com)))))
  (recv/print c)
  (ws-close! c))

(do-connection 'rfc6455)
