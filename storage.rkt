#lang racket

;; SQLite persistence for the game server: running matches survive
;; restarts and deploys, and completed matches feed a leaderboard.
;;
;; The server is a single process with a single writer, so one SQLite file
;; behind one worker thread is all the database this game needs. Every
;; public function is safe to call before storage-start! (or if SQLite is
;; unavailable): writes become no-ops and reads return empty results, so
;; the game itself never depends on the disk.

(provide storage-start!
         storage-stop!
         storage-running?
         save-live-match!
         clear-live-match!
         load-live-matches
         record-match!
         leaderboard
         email->player-id
         email->player-name)

(require db)
(require racket/async-channel)

;; the worker's job channel, or #f when storage is off
(define *jobs* (box #f))

(define (storage-running?)
  (and (unbox *jobs*) #t))

;; ---------------------------------------------------------------------
;; identity: emails look like "asha-x7k2p9@game.local" — the trailing id
;; is stable across renames (it lives in the browser), so it anchors a
;; player's history; the slug in front is their latest display name.
;; Anything that doesn't match (test users, odd clients) keeps the whole
;; email as both id and name. Bots are never recorded.

(define (email->player-id email)
  (let ((s (format "~a" email)))
    (match (regexp-match #rx"-([a-z0-9]+)@game\\.local$" s)
      ((list _ id) id)
      (_ s))))

(define (email->player-name email)
  (let ((s (format "~a" email)))
    (match (regexp-match #rx"^(.*)-[a-z0-9]+@game\\.local$" s)
      ((list _ name) name)
      (_ s))))

(define (bot-email? email)
  (regexp-match? #rx"^bot-[0-9]+$" (format "~a" email)))

;; ---------------------------------------------------------------------
;; worker plumbing: jobs are (cons reply-channel-or-#f proc-of-connection);
;; a dead disk must never take the game down with it

(define (run-job connection job)
  (match job
    ((cons reply proc)
     (let ((result (with-handlers ((exn:fail?
                                    (λ (e)
                                      (displayln (format "storage error: ~a"
                                                         (exn-message e)))
                                      #f)))
                     (proc connection))))
       (when reply (channel-put reply result))))))

;; fire-and-forget write
(define (post! proc)
  (let ((jobs (unbox *jobs*)))
    (when jobs (async-channel-put jobs (cons #f proc)))))

;; synchronous read; fallback when storage is off
(define (ask fallback proc)
  (let ((jobs (unbox *jobs*)))
    (cond
      ((not jobs) fallback)
      (else
       (let ((reply (make-channel)))
         (async-channel-put jobs (cons reply proc))
         (or (channel-get reply) fallback))))))

(define (storage-start! db-path)
  (unless (unbox *jobs*)
    (with-handlers ((exn:fail?
                     (λ (e)
                       (displayln (format "storage disabled (~a)" (exn-message e)))
                       #f)))
      (let ((connection (sqlite3-connect #:database db-path #:mode 'create))
            (jobs (make-async-channel)))
        (query-exec connection "PRAGMA journal_mode=WAL")
        (query-exec connection "PRAGMA busy_timeout=2000")
        (for-each (λ (ddl) (query-exec connection ddl)) +schema+)
        (set-box! *jobs* jobs)
        (thread (λ ()
                  (let loop ()
                    (let ((job (async-channel-get jobs)))
                      (cond
                        ((eq? job 'stop) (disconnect connection))
                        (else (run-job connection job)
                              (loop)))))))
        (displayln (format "storage ready at ~a" db-path))))))

;; mainly for tests: flush pending writes and release the file
(define (storage-stop!)
  (let ((jobs (unbox *jobs*)))
    (when jobs
      ;; drain marker: everything queued before this ran first
      (ask #f (λ (_) 'drained))
      (async-channel-put jobs 'stop)
      (set-box! *jobs* #f))))

(define +schema+
  (list
   "CREATE TABLE IF NOT EXISTS live_matches (
      room TEXT PRIMARY KEY,
      snapshot TEXT NOT NULL,
      updated_at INTEGER NOT NULL)"
   "CREATE TABLE IF NOT EXISTS matches (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      room TEXT NOT NULL,
      started_at INTEGER NOT NULL,
      ended_at INTEGER NOT NULL,
      hands INTEGER NOT NULL,
      winner TEXT NOT NULL,
      evens INTEGER NOT NULL,
      odds INTEGER NOT NULL,
      members TEXT NOT NULL)"
   "CREATE TABLE IF NOT EXISTS players (
      id TEXT PRIMARY KEY,
      name TEXT NOT NULL,
      played INTEGER NOT NULL DEFAULT 0,
      won INTEGER NOT NULL DEFAULT 0,
      updated_at INTEGER NOT NULL)"))

;; ---------------------------------------------------------------------
;; live matches: one row per running match, replaced wholesale at match
;; start and after every hand — restores resume at the top of a hand, so
;; a hand interrupted by a crash is simply re-dealt

(define (save-live-match! room snapshot)
  (let ((room-s (format "~a" room))
        (snapshot-s (format "~s" snapshot))
        (now (current-seconds)))
    (post! (λ (c)
             (query-exec c
                         "INSERT INTO live_matches (room, snapshot, updated_at)
                          VALUES ($1, $2, $3)
                          ON CONFLICT(room) DO UPDATE
                          SET snapshot = $2, updated_at = $3"
                         room-s snapshot-s now)))))

(define (clear-live-match! room)
  (let ((room-s (format "~a" room)))
    (post! (λ (c)
             (query-exec c "DELETE FROM live_matches WHERE room = $1" room-s)))))

;; list of snapshot s-expressions, oldest first
(define (load-live-matches)
  (ask '()
       (λ (c)
         (map (λ (row) (read (open-input-string (vector-ref row 0))))
              (query-rows c "SELECT snapshot FROM live_matches
                             ORDER BY updated_at ASC")))))

;; ---------------------------------------------------------------------
;; history: a finished match credits every human in a FINAL seat (an
;; abandoned seat belongs to its replacement bot, which earns nothing)

(define (record-match! room started-at hands winner evens odds member-emails)
  (let ((room-s (format "~a" room))
        (winner-s (format "~a" winner))
        (members-s (format "~s" (map (λ (m) (format "~a" m)) member-emails)))
        (now (current-seconds))
        (humans (for/list ((email member-emails)
                           (seat (in-naturals))
                           #:unless (bot-email? email))
                  (list (email->player-id email)
                        (email->player-name email)
                        (if (eq? (even? seat) (eq? winner 'evens)) 1 0)))))
    (post!
     (λ (c)
       (call-with-transaction
        c
        (λ ()
          (query-exec c
                      "INSERT INTO matches
                       (room, started_at, ended_at, hands, winner, evens, odds, members)
                       VALUES ($1, $2, $3, $4, $5, $6, $7, $8)"
                      room-s started-at now hands winner-s evens odds members-s)
          (for-each
           (λ (entry)
             (match entry
               ((list id name won)
                (query-exec c
                            "INSERT INTO players (id, name, played, won, updated_at)
                             VALUES ($1, $2, 1, $3, $4)
                             ON CONFLICT(id) DO UPDATE
                             SET name = $2,
                                 played = played + 1,
                                 won = won + $3,
                                 updated_at = $4"
                            id name won now))))
           humans)))))))

;; ((name played won) ...) — most wins first, fewer games breaking ties
(define (leaderboard limit)
  (ask '()
       (λ (c)
         (map (λ (row)
                (list (vector-ref row 0) (vector-ref row 1) (vector-ref row 2)))
              (query-rows c
                          "SELECT name, played, won FROM players
                           ORDER BY won DESC, played ASC, name ASC
                           LIMIT $1"
                          limit)))))

(module+ test
  (require rackunit)

  (define test-db (build-path (find-system-path 'temp-dir)
                              (format "game28-test-~a.db" (current-seconds))))
  (when (file-exists? test-db) (delete-file test-db))

  (test-case "identity extraction"
    (check-equal? (email->player-id "asha-x7k2p9@game.local") "x7k2p9")
    (check-equal? (email->player-name "asha-x7k2p9@game.local") "asha")
    (check-equal? (email->player-name "two-words-ab12cd@game.local") "two-words")
    (check-equal? (email->player-id 'scripted-user) "scripted-user")
    (check-equal? (email->player-name 'scripted-user) "scripted-user"))

  (test-case "everything no-ops gracefully before storage starts"
    (check-false (storage-running?))
    (save-live-match! 'a-room '((hand . 1)))
    (clear-live-match! 'a-room)
    (record-match! 'a-room 0 1 'evens 6 0 '(a b c d))
    (check-equal? (load-live-matches) '())
    (check-equal? (leaderboard 10) '()))

  (storage-start! test-db)

  (test-case "live rows round-trip and clear"
    (save-live-match! 'room-a '((room . room-a) (hand . 3) (evens . -2) (odds . 1)))
    (save-live-match! 'room-b '((room . room-b) (hand . 1) (evens . 0) (odds . 0)))
    ;; the second save for the same room replaces the first
    (save-live-match! 'room-a '((room . room-a) (hand . 4) (evens . -2) (odds . 2)))
    (let ((rows (load-live-matches)))
      (check-equal? (length rows) 2)
      (let ((row-a (findf (λ (r) (eq? (cdr (assq 'room r)) 'room-a)) rows)))
        (check-equal? (cdr (assq 'hand row-a)) 4)
        (check-equal? (cdr (assq 'odds row-a)) 2)))
    (clear-live-match! 'room-a)
    (clear-live-match! 'room-b)
    (check-equal? (load-live-matches) '()))

  (test-case "finished matches build the leaderboard, bots invisible"
    ;; asha (evens, seats 0/2 side) wins twice, badri once; bots never appear
    (record-match! 'r1 100 5 'evens 6 -2
                   '("asha-aaa111@game.local" "badri-bbb222@game.local"
                     "bot-3" "bot-4"))
    (record-match! 'r2 200 4 'evens 6 0
                   '("asha-aaa111@game.local" "bot-5"
                     "bot-6" "badri-bbb222@game.local"))
    (record-match! 'r3 300 7 'odds -4 6
                   '("bot-7" "badri-bbb222@game.local"
                     "asha-aaa111@game.local" "bot-8"))
    (let ((rows (leaderboard 10)))
      (check-equal? (length rows) 2)
      ;; asha: won r1 (seat 0, evens won) and r2 (seat 0, evens) = 2 of 3
      (check-equal? (first rows) (list "asha" 3 2))
      ;; badri: won r3 (seat 1, odds won) = 1 of 3
      (check-equal? (second rows) (list "badri" 3 1))
      (check-false (findf (λ (r) (regexp-match? #rx"bot" (first r))) rows))))

  (test-case "a rename keeps the history under the latest name"
    (record-match! 'r4 400 3 'evens 6 1
                   '("ashes-aaa111@game.local" "bot-9" "bot-10" "bot-11"))
    (let ((top (first (leaderboard 1))))
      (check-equal? top (list "ashes" 4 3))))

  (storage-stop!)
  (delete-file test-db))
