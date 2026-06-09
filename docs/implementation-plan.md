# Twenty-Eight (28) — Implementation Plan

Status: draft · Branch: `feat/game-implementation` · Last updated: 2026-06-09

## Where We Are Today

### Backend (Racket)

| Area | State |
|---|---|
| Deck, dealing | ✅ 32-card deck, `distribute-cards` deals 4 + 4 |
| Bidding | ✅ `start-bidding` auction loop (16–28, pass handling) |
| Trump selection | ✅ `choose-trump-suit` |
| Trick play | ✅ `play-game` with follow-suit validation, trick winner, points |
| Rooms / users | ✅ `make-room`, `join-room`, `add-bot-to-room`, channels-based concurrency |
| Bot client | ✅ `client.rkt` — naive but functional (bids +1, trumps diamond, plays first valid card) |
| Hand scoring vs bid | ❌ `score-game` is a stub (`game.rkt:362`) — game never declares a winner |
| Multi-round play | ❌ Game ends after one deal of 8 tricks |
| Trump exposure rules | ⚠️ `'expose-trump` exists, but concealed-phase rules aren't enforced |
| Tests | ⚠️ Room management only (~5 assertions); no game-logic coverage |

**Known bugs:**
- `game.rkt:113` — `no-points-card?` logic inverted: `(> 0 (card-point card))` should test for zero points
- `game.rkt:129` — `check-for-redeal` uses `andmap` where `ormap` is intended
- `game.rkt:346` — invalid-card recovery in `play-game` recurses with unbound loop variables → crash

### Frontend (`app/` — Three.js + Effect, **not** BabylonJS anymore)

| Area | State |
|---|---|
| Rendering | ✅ Zen-aesthetic scene: fan-arc hand, play area, enso turn markers, soft shadows |
| Interaction | ✅ Hover preview, drag-to-play, drop-back-to-hand |
| State | ✅ Pure reducer (`GameModel.ts`: `gameReducer`, tagged `GameAction` union) |
| Effect | ✅ `Effect.gen`/`runFork` drive the mock game program |
| Game flow | ⚠️ `MockGameProgram.ts` only — random cards, fixed delays, no rules |
| Networking | ❌ No WebSocket client, no s-expression codec |
| Lobby UI | ❌ None (no connect / room / start-game screens) |
| Rules on client | ❌ No legal-move validation, no bidding or trump UI |

### Protocol (already defined by the server)

Client → server: `connect-user`, `make-room`, `join-room`, `get-room-details`, `get-active-rooms`, `add-bot-to-room`, `start-game`, `put-bid`, `selected-trump`, `card-played`.
Server → client: `hand`, `request-bid`, `error`, `choose-trump`, `trump-selected`, `room-members`, `bid-result`, `play-card`, `points-won`.
All s-expressions over WebSocket text frames (port 8081).

---

## Milestones

### M1 — Backend correctness (small, do first)

The server is the source of truth; fix it before building a client against it.

- [ ] Fix `no-points-card?` inverted comparison (`game.rkt:113`)
- [ ] Fix `check-for-redeal` `andmap` → `ormap` (`game.rkt:129`)
- [ ] Fix invalid-card recovery crash in `play-game` (`game.rkt:346`)
- [ ] Implement `score-game`: bidder's team points vs bid → win/loss result
- [ ] Broadcast a `game-result` message after `points-won` (extend protocol)
- [ ] Add `raco test` coverage for bidding, trick resolution, and scoring (pure functions in `game.rkt` are easy to unit-test)

**Done when:** `simulate-game` in `client.rkt` plays a full hand and the server announces which team won.

### M2 — Frontend networking layer

- [ ] `src/net/sexpr.ts` — s-expression parser/serializer (symbols, strings, numbers, lists; round-trips every protocol message)
- [ ] `src/net/SocketService.ts` — Effect-based WebSocket client: connect, send typed requests, expose incoming messages as a stream/queue; reconnect-on-drop
- [ ] `src/net/protocol.ts` — typed encode/decode for every protocol message ↔ a `ServerEvent` / `ClientCommand` union (mirrors the `GameAction` style already in `GameModel.ts`)
- [ ] Smoke test path: connect to `racket server.rkt`, `connect-user`, `get-active-rooms`, log round-trip

**Done when:** the browser console can drive a full room-creation handshake against the live server.

### M3 — Lobby & room flow

- [ ] Minimal DOM overlay screens (match existing zen styling): enter name → room list → create/join room → member list → start game
- [ ] Wire `room-members` updates live
- [ ] "Add bot" button (`add-bot-to-room`) so a single human can start a 4-player game
- [ ] Transition: `start-game` accepted → tear down lobby → enter game scene

**Done when:** one human + 3 bots can go from page load to a dealt hand.

### M4 — Server-driven game loop (replaces mock)

- [ ] Extend `GameModel` for the full game: bidding state, trump, current trick, team points, phase (`dealing | bidding | choosing-trump | playing | scored`)
- [ ] Map every `ServerEvent` to `GameAction`s; the reducer + `renderAction` pattern stays as-is
- [ ] Bidding UI: bid/pass prompt on `request-bid`, show running bid and winner (`bid-result`)
- [ ] Trump selection UI on `choose-trump` (suit picker)
- [ ] Card play: drag-to-play sends `card-played`; lock interaction unless it's our turn; gray out illegal cards (follow-suit) client-side, trust server as authority
- [ ] Render opponents' played cards (server `play-card` broadcasts) and second 4-card deal
- [ ] Trick end: collect-cards animation, update tricks/points display from `points-won`
- [ ] Keep `MockGameProgram` behind a `?mock=1` flag for offline UI work

**Done when:** a human can play a complete hand against 3 bots end-to-end.

### M5 — Full 28 rules

- [ ] Trump exposure flow: request-expose action, concealed vs exposed phase enforced server-side, reveal animation client-side
- [ ] Hand result screen (bid made / set) from M1's `game-result`
- [ ] Multi-round games: cumulative team score across hands, rotating dealer, play to a target
- [ ] Redeal handling (no point cards) surfaced in UI

**Done when:** rules match a standard game of 28 across multiple hands.

### M6 — Polish & resilience

- [ ] Reconnection: resync state on socket drop (likely needs a server-side `game-state` snapshot message)
- [ ] Opponent hand representation (card backs / counts)
- [ ] Sound, deal animations, card flip on reveal (`Card.flip()` exists but is unused)
- [ ] Error toasts for server `error` messages
- [ ] Frontend `npm run build` clean; basic integration test of sexpr codec + reducer
- [ ] Update `CLAUDE.md` (frontend is Three.js, not BabylonJS; document `src/` layout)

---

## Sequencing & Risks

- M1 → M2 → M3 → M4 are strictly ordered; M5/M6 items can interleave once M4 lands.
- **Protocol gaps** (no game-result, no state snapshot, undocumented `cards-played` ordering in `play-card`) will surface in M4 — budget for small server protocol additions alongside frontend work.
- **Server authority**: client never enforces rules beyond UX hints; all validation stays in `game.rkt` to keep one source of truth.
- Vendored `repos/effect/` is the reference for idiomatic Effect (streams/queues for the socket service in M2).
