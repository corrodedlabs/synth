#lang racket/base

;; Prints the newest diagnostics reports from the Android app, one per line:
;; id, when it arrived (UTC), then the report's JSON. On fly.io:
;;
;;   fly ssh console -C "racket /app/reports.rkt 20"
;;
;; It opens the database read-only beside the running server; see
;; save-report! in storage.rkt for what is kept and for how long.

(require db
         racket/date)

(provide print-reports)

(define (print-reports db-path count [out (current-output-port)])
  (define connection (sqlite3-connect #:database db-path #:mode 'read-only))
  (parameterize ([date-display-format 'iso-8601])
    (for ([row (query-rows connection
                           "SELECT id, received_at, body FROM reports ORDER BY id DESC LIMIT $1"
                           count)])
      (fprintf out "~a ~aZ ~a\n"
               (vector-ref row 0)
               (date->string (seconds->date (vector-ref row 1) #f) #t)
               (vector-ref row 2))))
  (disconnect connection))

(module+ main
  (require racket/cmdline)
  (define count
    (command-line
     #:program "reports.rkt"
     #:args ([n "20"])
     (or (string->number n) 20)))
  (print-reports (or (getenv "GAME-DB")
                     (if (file-exists? "/data/game.db") "/data/game.db" "game.db"))
                 count))

(module+ test
  (require rackunit
           racket/file
           racket/port
           racket/string
           "./storage.rkt")
  (define db (make-temporary-file "game28-reports-~a.db"))
  (delete-file db)
  (storage-start! db)
  (save-report! "{\"report\":1,\"n\":1}")
  (save-report! "{\"report\":1,\"n\":2}")
  (storage-stop!)
  (define lines (string-split (with-output-to-string (λ () (print-reports db 1))) "\n"))
  (check-equal? (length lines) 1 "only the newest, when asked for one")
  (check-true (string-suffix? (car lines) "Z {\"report\":1,\"n\":2}"))
  (delete-file db))
