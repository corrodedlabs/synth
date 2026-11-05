# Backend architecture

Read this before changing `game.rkt`, `server.rkt`, `storage.rkt` or `client.rkt`, or the message protocol.

### Backend (Racket)

Four main files with clear responsibilities:

- **`game.rkt`** — Pure game logic: card definitions (`card` prefab struct with name/rank/suit/point; the numeric rank doubles as trick strength J>9>A>10>K>Q>8>7), per-game shuffled decks (`fresh-deck`), card distribution, the auction (opener must bid ≥16, raises strictly higher, three consecutive passes end it; `#:first-player` rotates the opener), play validation (follow suit; must trump right after calling for the exposure), trick resolution honouring trump-exposure timing (`#:first-leader` rotates the lead), team scoring (`score-game`), and match scoring (`hand-game-points`: +1/−2 under bid 20, +2/−4 from 20; `match-winner` against `+match-target+` 6). No external dependencies; rackunit tests live in its `test` submodule.

- **`server.rkt`** — WebSocket service with three internal modules:
  - **User module**: `user` struct (connection, email, hand, comm-channel, pic-url), global `*connected-users*` hash
  - **Room module**: `game-room` serializable struct (host, name, members), global `*game-rooms*` hash keyed by host email
  - **Game module**: orchestrates a whole match per table — hand N's opener/leader is seat (N−1) mod 4, `hand-result` broadcasts game points after every hand, the game thread then blocks until the acting host sends `(next-hand <email>)`, and `match-over` ends it (target 6; the `MATCH-TARGET` env var shortens matches in tests). A dropped player keeps their seat for `RECONNECT-GRACE` seconds (default 45): `connect-user` with the same email rebinds the seat's socket, `(rejoin <email>)` returns a `game-snapshot` built from the live `match-state` shadow the game thread maintains, and `player-disconnected`/`player-reconnected` keep the table informed. A seat lost for good (grace expiry or `(leave-game <email>)`) goes to the abandonment gate while other humans remain: `seat-abandoned` is broadcast and the survivors answer `(replace-with-bot …)` — `convert-to-bot!` re-identifies the seat's shared user struct in place, re-deals it its hand, and re-asks any pending request — or `(close-game …)` → `game-aborted`. The last human leaving (or a dead bot) closes the match. While an email is seated in a running match, `make-room`/`join-room` answer `(error already-in-match)` — a second game on the shared user struct would corrupt both — and the client responds by rejoining the match. Room changes push `(active-rooms …)` to every connected, unseated user so table lists stay live. Spawns threads for long-running game operations, uses Racket channels for async player communication

- **`storage.rkt`** — SQLite persistence (Racket `db` lib, one worker thread
  fed by an async-channel; every call no-ops gracefully when storage is
  off). Three tables: `live_matches` (one resume row per running match,
  written before every hand and at each hand's end — a crash resumes at the
  top of the interrupted hand), `matches` (history), `players` (leaderboard;
  keyed on the stable id suffix of the email so renames keep history; bots
  never recorded). `restore-live-matches!` at boot turns rows back into
  running matches: human seats become rejoinable "ghosts" inside their
  reconnect grace, bot seats get fresh bots.

- **`client.rkt`** — Bot client. `client-lambda` is the AI loop; per-hand memory lives in the `state` struct (hand, known trump, exposure, cards seen, must-trump obligation). The heuristics are pure, unit-tested helpers: `bid-appetite`/`choose-bid` (expected team points ≈ 14 + P/2 from the first four cards, long-suit bonus, never past 21; the opener always bids), `choose-trump-suit` (longest suit, points break ties), and `choose-play` (cheapest sufficient winner, feed a winning partner, dump low value saving tens, ruff rich tricks, call the exposure when void on a worthwhile trick, lead boss cards tracked against `seen`). `simulate-game` runs a full 4-player game for testing.

### Concurrency Model

Each player has an async-channel on their `user` struct; the WebSocket
listener `async-channel-put`s game messages into it, and the match thread
reads the active player's channel with 1-second `sync/timeout` polls. Every
timeout tick runs `check-liveness!` over all four seats — that is where
disconnect grace, reconnection notices, and the abandonment gate live, so
no wait can hang forever. One `user` struct per identity is shared by the
room and the match and mutated in place (connection rebinds on reconnect,
hands reset per hand, email changes on bot takeover) — never copy it.

**Custodian rule (learned the hard way):** web-server shuts down each
connection's custodian on disconnect, silently killing any thread or socket
created while dispatching that connection's messages. Anything long-lived
spawned from `dispatch` — match threads, bot sockets/threads — MUST be
wrapped in `(parameterize ((current-custodian *game-custodian*)) …)` (or
`*bot-custodian*`); a bare `(thread …)` there dies with the websocket that
sent the message. Regression coverage: the host-at-gate scenario in
tests.rkt.

### Message Protocol

All communication uses s-expressions over WebSocket text frames:
- Requests: `(symbol arg1 arg2 ...)` — e.g., `(connect-user "email" "pic-url")`
- Responses: symbols or lists — e.g., `'room-created`, `(room-members ...)`
- Dispatch in `server.rkt` uses `case` on the leading symbol
- User-scoped messages include email as first argument
