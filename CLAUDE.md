# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

Real-time 28 card game with a Racket WebSocket server and a Vite + Three.js TypeScript frontend. Deployed: frontend on Netlify (`netlify deploy --build --prod` from repo root), backend on fly.io (`fly deploy --remote-only`) — see `docs/deployment.md`.

## Vendored Repositories

External repositories may be vendored under `repos/` for agent reference.

- Treat vendored repositories as read-only unless explicitly asked to edit them.
- Do not import application code from `repos/`; use normal package dependencies.
- Prefer examples and patterns from vendored source over guessing from memory.
- When writing Effect code, inspect `repos/effect/` for idiomatic usage, tests, module structure, and API design.
- If `repos/effect/LLMS.md` exists, read it before writing Effect code.

## Commands

### Racket (from repo root)
- `racket server.rkt` — start WebSocket server on port 8081
- `racket server.rkt --port 9000` — custom port
- `raco test game.rkt` — run game-logic unit tests (rackunit submodule)
- `raco test -t tests.rkt` — run integration tests
- `racket client.rkt` — run bot simulation helpers

### Frontend (from `app/`)
- `npm run dev` — Vite dev server with HMR
- `npm run build` — typecheck (tsc) + production build
- `npm test` — vitest unit suite (s-expression codec, protocol)
- Effect is available in the frontend via the `effect` npm package

## Testing

Four layers; run all of them after non-trivial changes. CI
(`.github/workflows/ci.yml`) runs every layer — including the browser
suites, headless — on each push.

1. **Game logic unit** — `raco test game.rkt` (rackunit: dealing, bidding,
   trick resolution incl. trump-exposure timing, scoring).
2. **Frontend unit** — `cd app && npm test` (vitest: sexpr/protocol codecs,
   game model reducer and play-legality rules).
3. **Server integration** — `raco test -t tests.rkt` (boots a server on port
   18081 so it never collides with a dev server).
4. **Browser end-to-end** — Playwright scripts under `app/scripts/`, run from
   `app/` with both servers up (`racket server.rkt --port 8082` and
   `npm run dev -- --port 5179`; pass the page URL as the argument).
   **Run these headed (`HEADED=1`) by default** — a visible browser window —
   so layout/positioning problems are actually seen; drop the flag only in
   CI or sandboxes without a display:

   ```sh
   HEADED=1 node scripts/play-game.mjs           "http://localhost:5179/?port=8082"
   HEADED=1 node scripts/play-multiplayer.mjs    "http://localhost:5179/?port=8082"
   HEADED=1 node scripts/test-match.mjs          "http://localhost:5179/?port=8082"
   HEADED=1 node scripts/test-lobby-controls.mjs "http://localhost:5179/?port=8082"
   HEADED=1 node scripts/test-multi-tables.mjs   "http://localhost:5179/?port=8082"
   HEADED=1 node scripts/test-mobile.mjs         "http://localhost:5179/?port=8082"
   ```

   - `play-game.mjs` — one human + 3 bots: lobby → bid → trump → all 8 tricks
     → the between-hands result; first plays use real drag/click gestures.
   - `play-multiplayer.mjs` — two browser pages play one table through the
     first hand.
   - `test-match.mjs` — host + guest + 2 bots play a two-hand match: the
     between-hands panel (host deal button vs guest waiting note), match
     scores mirroring, dealer rotation, and the leave-match button (two-step
     confirm; leaver gets a fresh start screen, the rest are aborted).
   - `test-lobby-controls.mjs` — kick bot/human, leave, close table,
     disconnect cleanup.
   - `test-multi-tables.mjs` — several concurrent tables, room browser, join.
   - `test-mobile.mjs` — phone viewport (390×844, touch emulation): panels and
     hand must fit the screen, full hand played with real taps.
   - All scripts honour `HEADED=1`. Screenshots land in `/tmp/game-shots*`;
     scripts exit non-zero on console errors. Any new test script must follow
     the same pattern (`headless: !process.env.HEADED`).
   - Inspect the screenshots after a run — they're the positioning record
     (hand fan, trick cross, seat name labels, overlays).

### Multi-user testing pattern

Simulate N users by opening N pages from one `chromium` instance — each page
is an independent user (own websocket, own identity):

```js
const page = await browser.newPage();
await page.goto(url);
await page.fill("#player-name", "Asha");
await page.click("#create-button");        // or #browse-button + .room-row button
```

Drive decisions through the `window.__game` debug bridge instead of 3D
gestures: `state()` (the reducer model — poll it with `waitForFunction`),
`legalCards()`, `play(id)`, `bid(n)`, `pass()`, `trump(suit)`, `expose()`,
`nextHand()`, `addBot()`, `startGame()`, `leaveTable()`, `leaveMatch()`,
`kick(member)`, `roomName()`.
Launch with `channel: "chrome"` locally (`/usr/bin/chromium` in CI sandboxes)
and `--enable-unsafe-swiftshader --use-angle=swiftshader` for headless WebGL.

## Architecture

### Backend (Racket)

Three main files with clear responsibilities:

- **`game.rkt`** — Pure game logic: card definitions (`card` prefab struct with name/rank/suit/point; the numeric rank doubles as trick strength J>9>A>10>K>Q>8>7), per-game shuffled decks (`fresh-deck`), card distribution, the auction (opener must bid ≥16, raises strictly higher, three consecutive passes end it; `#:first-player` rotates the opener), play validation (follow suit; must trump right after calling for the exposure), trick resolution honouring trump-exposure timing (`#:first-leader` rotates the lead), team scoring (`score-game`), and match scoring (`hand-game-points`: +1/−2 under bid 20, +2/−4 from 20; `match-winner` against `+match-target+` 6). No external dependencies; rackunit tests live in its `test` submodule.

- **`server.rkt`** — WebSocket service with three internal modules:
  - **User module**: `user` struct (connection, email, hand, comm-channel, pic-url), global `*connected-users*` hash
  - **Room module**: `game-room` serializable struct (host, name, members), global `*game-rooms*` hash keyed by host email
  - **Game module**: orchestrates a whole match per table — hand N's opener/leader is seat (N−1) mod 4, `hand-result` broadcasts game points after every hand, the game thread then blocks until the host sends `(next-hand <email>)`, and `match-over` ends it (target 6; the `MATCH-TARGET` env var shortens matches in tests). Any disconnect — mid-hand or at the gate — aborts the match via `game-aborted`. Spawns threads for long-running game operations, uses Racket channels for async player communication

- **`client.rkt`** — Bot client with `state` struct (user, hand, bid-value, selected-trump). `client-lambda` is the main bot AI loop. `simulate-game` runs a full 4-player game for testing.

### Concurrency Model

Players communicate with the game thread via Racket channels (`channel-put`/`channel-get`). The WebSocket listener fills a player's channel when their message arrives; the game thread blocks on `channel-get` waiting for the active player's decision. Long-running game operations run on separate threads to avoid blocking the listener.

### Message Protocol

All communication uses s-expressions over WebSocket text frames:
- Requests: `(symbol arg1 arg2 ...)` — e.g., `(connect-user "email" "pic-url")`
- Responses: symbols or lists — e.g., `'room-created`, `(room-members ...)`
- Dispatch in `server.rkt` uses `case` on the leading symbol
- User-scoped messages include email as first argument

### Frontend (TypeScript/Three.js)

Server events drive a pure reducer; rendering reacts to dispatched actions
(no rules are implemented client-side — the server is the authority):

- `app/src/net/` — `sexpr.ts` (s-expression codec), `protocol.ts` (typed
  `ServerEvent`/`ClientCommand` ↔ s-expr), `SocketService.ts` (Effect-based
  WebSocket client exposing an event `Queue`)
- `app/src/game/` — `GameModel.ts` (reducer + model), `GameSession.ts`
  (Effect program: lobby ops, event loop, seat translation, heartbeat),
  `GameState.ts` (dispatch → Three.js/DOM rendering), `Card.ts`/`Hand.ts`/
  `PlayArea.ts` (scene objects), `MockGameProgram.ts` (`?mock=1` offline mode)
- `app/src/ui/UiOverlay.ts` — DOM overlay: start screen, room browser, lobby,
  bid/trump panels, help overlay
- `app/src/utils/Sound.ts` — synthesized Web Audio effects (card swishes,
  turn chimes, results) reacting to dispatched actions in `GameState`; mute
  toggle persisted in localStorage; `__game.sounds()` exposes attempt counters
- `app/src/main.ts` — bootstrapping plus the `window.__game` debug bridge
  used by the Playwright suites
- URL params: `?port=`/`?server=` (game server), `?mock=1`, `?debug`

## Code Style

### Racket
- Naming: `+name+` constants, `*name*` mutable globals, `name?` predicates, `name->value` conversions
- 2-space indent; use `match` over nested `car`/`cdr`; use `case` for message dispatch
- Pure functions in `game.rkt`; mutation confined to server state modules
- `serializable-struct` for data sent over the wire

### TypeScript
- 2-space indent, semicolons, double quotes
- `const`/`let` only (no `var`); `import type` for type-only imports
- `noUnusedParameters: true` in tsconfig
- Scene objects scoped to scene builder; no global state
