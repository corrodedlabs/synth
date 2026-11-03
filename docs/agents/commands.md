# Commands

Read this for how to start, build and configure the server and frontend.

### Racket (from repo root)
- `racket server.rkt` — start WebSocket server on port 8081
- `racket server.rkt --port 9000` — custom port
- `raco test game.rkt` — run game-logic unit tests (rackunit submodule)
- `raco test client.rkt` — run bot-heuristic unit tests
- `raco test storage.rkt` — run persistence/leaderboard unit tests
- `raco test -t tests.rkt` — run integration tests
- `racket client.rkt` — run bot simulation helpers

Server env vars (all optional):
- `GAME-DB` — SQLite path (default `game.db`, gitignored; prod uses
  `/data/game.db`). Booting also restores any interrupted matches from it.
- `MATCH-TARGET` — overrides every table's match target (tests use 0).
  Normally the target is per-table: the host picks Quick (first to 2) or
  Classic (first to 6, the default) at creation; `(make-room host name
  [target])` carries it, `room-members`/`active-rooms` broadcast it, and
  it rides the resume row so restored matches keep their type.
- `RECONNECT-GRACE` — seconds a dropped seat waits for its owner
  (default 45; the integration suite runs at 2).
- `GAME-PORT` — set automatically by `--port`; bots dial back through it.

### Frontend (from `app/`)
- `npm run dev` — Vite dev server with HMR
- `npm run build` — typecheck (tsc) + production build
- `npm test` — vitest unit suite (codecs, protocol, reducer, play legality)
- Effect is available in the frontend via the `effect` npm package
