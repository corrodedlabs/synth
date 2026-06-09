# Twenty-Eight (28) — Frontend-First Plan

Status: active · Branch: `feat/game-implementation` · Last updated: 2026-06-09

Goal: a complete, polished game of 28 playable in the browser **against the existing Racket server's bots** — one human + 3 bots via `add-bot-to-room`. No game rules are reimplemented in TypeScript; the server stays the single source of truth. The full lobby/multiplayer experience (humans joining rooms) is deferred — for now the client silently auto-creates a room behind a "Play" button.

## Guiding Architecture

The frontend already has the right seam: a pure reducer (`GameModel.ts`) fed by `GameAction`s, with `GameState.dispatch` → `renderAction` doing visuals. The server becomes the engine driving it:

```
  ┌────────────────────┐    ServerEvent → GameAction   ┌─────────┐    ┌──────────────┐
  │ Racket server      │ ────────────────────────────▶ │ reducer │ ─▶ │ renderAction │
  │ (rules + 3 bots)   │ ◀──────────────────────────── │  model  │    │  (Three.js)  │
  └────────────────────┘    PlayerInput → s-expr msg   └─────────┘    └──────────────┘
```

- **No rules in TS.** Move legality, trick winners, and scoring are the server's job. The client renders what it's told and sends what the player chose. UX hints (e.g., dimming off-suit cards) come from trivially reading the led suit out of the model — not from a ported rules engine; the server remains the validator.
- **One driver interface.** Server events arrive as `GameAction`s; player intents (bid, pass, choose trump, play card) leave through a single `PlayerInput` interface. The later lobby/multiplayer phase reuses all of this unchanged.
- Effect (`Effect.gen`, fibers, `Stream`/`Queue` — see `repos/effect/` for idioms) runs the socket loop, replacing the timer-based `MockGameProgram.ts`.

Dev workflow: `racket server.rkt` running locally alongside `npm run dev`.

---

## M1 — Socket & protocol layer

The minimum plumbing to talk to the server (required before anything can be played).

- [ ] `src/net/sexpr.ts` — s-expression parser/serializer (symbols, strings, numbers, nested lists, dotted pairs); round-trips every protocol message
- [ ] `src/net/protocol.ts` — typed codec: every server message (`hand`, `request-bid`, `choose-trump`, `trump-selected`, `bid-result`, `play-card`, `points-won`, `room-members`, `error`) ↔ a `ServerEvent` union; every client message (`connect-user`, `make-room`, `add-bot-to-room`, `start-game`, `put-bid`, `selected-trump`, `card-played`) ↔ a `ClientCommand` union
- [ ] `src/net/SocketService.ts` — Effect-based WebSocket client: connect to `ws://localhost:8081/test`, send `ClientCommand`s, expose incoming `ServerEvent`s as a `Stream`/`Queue`
- [ ] Card representation mapping: server card s-exprs ↔ the existing `CardModel` (suit/rank naming differs: server uses `'heart`, frontend uses `"hearts"`, etc.)
- [ ] Unit tests for the sexpr codec with vitest (add as dev dependency — `app/` has no test runner)

**Done when:** the browser console shows a decoded, typed event log of a full bot game (connect → make room → 3 bots → start → watch events stream in).

## M2 — Quick-play game session (replaces the mock)

Wire the socket to the reducer; delete the timer-based mock as the default path.

- [ ] Extend `GameModel`: `phase` (`connecting | dealing | bidding | choosing-trump | playing | trick-end | hand-end`), seats (we are seat 0; teams 0&2 vs 1&3), bidding state (current bid, who bid), trump (suit, exposed?), current trick, tricks/points per team
- [ ] Extend `GameAction` to mirror `ServerEvent`s 1:1 (`HandDealt`, `BidRequested`, `BidResult`, `TrumpChosen`, `CardPlayRequested`, `CardPlayed`, `PointsWon`, …)
- [ ] `GameSession.ts` (Effect program): on "play" — `connect-user` → `make-room` → `add-bot-to-room` ×3 → `start-game`, then loop: decode events → dispatch actions; when the server requests our bid/trump/card, await `PlayerInput` from the UI and reply
- [ ] Human input plumbing: `Deferred`/`Queue` bridge so DragControls and UI buttons resolve the pending server request
- [ ] Map server `play-card` broadcasts (other players' cards) into the play area
- [ ] Keep `MockGameProgram` behind a `?mock=1` flag for offline UI iteration only

**Done when:** a full hand plays end-to-end against 3 server bots, human acting at every decision point (even with placeholder buttons).

## M3 — Bidding & trump selection UI

First new screens; match the existing zen aesthetic (cream, Cormorant Garamond, brush textures).

- [ ] Bidding panel: appears on `request-bid` — current bid, raise/pass controls; opponents' enso markers pulse while they bid (reuse `PlayArea.setActivePlayer`)
- [ ] Bid announcements for opponents (small floating label near their marker)
- [ ] Auction result banner from `bid-result`: winning bid + bidder
- [ ] Trump selection: suit picker (4 brush-style suit glyphs) on `choose-trump`; "choosing trump…" state when a bot wins
- [ ] Phase-aware interaction locking: hand cards undraggable until `playing` phase and our turn

**Done when:** the pre-play flow (deal → auction → trump) is fully playable and readable without looking at the console.

## M4 — Table play, perfected

The core feel of the game. This is where most of the "perfecting the UI" time goes.

- [ ] Opponent hands as card-back fans (counts shrink as they play) — currently opponents are only enso markers
- [ ] Deal animation: cards fly from a deck position to each seat (4 on `hand`, 4 more after the auction)
- [ ] Turn-locked play: drag only when the server asks us for a card; off-suit cards dimmed as a hint (led suit read from the model — server still validates); a server `error` reply snaps the card back with a shake
- [ ] Opponent card play animation: card-back flies from their fan to the play area and flips face-up (`Card.flip()` exists, unused)
- [ ] Trick end: pause to read the trick, then cards sweep to the winning side; trick/point counters update
- [ ] Trump exposure: "show trump" affordance when void in led suit (sends `'expose-trump` as the card play); reveal animation
- [ ] Played-card stagger/rotation so the trick reads naturally (slight random tilt per card)
- [ ] Layout/camera pass: verify hand fan, play area, and markers at common aspect ratios (the current camera is tuned for one viewport)

**Done when:** a full 8-trick hand looks and feels finished — every state change is visible as motion, nothing teleports.

## M5 — Scoring & game shell

- [ ] Hand result screen from `points-won`: points per team, bid vs taken, made/set — zen-styled overlay
  - ⚠️ depends on a small backend addition: `score-game` in `game.rkt` is a stub, so the server never declares made/set. Either render points-only until then, or make that one Racket fix when we get here
- [ ] Title/start screen: replaces the auto-starting mock; "Play" runs the M2 quick-play session; keep dev flags (`?mock=1`) for jumping straight into a phase
- [ ] "Play again" → tear down session, start a fresh room/game
- [ ] Persistent HUD: trump indicator (hidden vs exposed), bid target, team points — replace the bare `Score | Tricks` text overlay
- [ ] Connection states surfaced: connecting, server unreachable ("is `racket server.rkt` running?"), dropped mid-game

**Done when:** you can sit down, play a hand against bots to a result, and immediately play another.

## M6 — Polish & hardening

- [ ] Mobile/touch pass: drag thresholds, card sizes, layout on portrait screens (touch handlers exist but are untuned)
- [ ] Sound: card slide, flip, trick win, bid chime (subtle, zen)
- [ ] Micro-interactions: hover lift tuning, marker pulses, score count-up animation
- [ ] Performance: texture loading states, no jank during deal animations
- [ ] Debug tooling: extend `DebugUI.ts` with phase-jump / replay-event-log controls for fast iteration
- [ ] Cleanup: remove dead code (`CardTextures.ts` fallback if unused, `@ts-ignore`s in `DragControls.ts`/`GameState.ts`), update `CLAUDE.md` (Three.js, not BabylonJS; new `src/` layout)
- [ ] `npm run build` clean; vitest suite (sexpr codec + reducer) green

**Done when:** the game feels shippable as a single-player-vs-bots experience.

---

## Explicitly Deferred

- **Lobby & multiplayer UI** — room lists, joining friends' rooms, member lists. The protocol work from M1 already covers it; this is purely additional screens later.
- **Backend fixes** (`implementation-plan.md` M1): `score-game` stub, `no-points-card?`/`check-for-redeal`/`play-game` bugs, multi-round games. Picked up after the frontend feels right — except the one-off `score-game` fix if we want made/set in M5.
- **Reconnection/resync** — needs a server-side state-snapshot message; not worth it before multiplayer.

## Sequencing

M1 → M2 are strictly ordered foundation. M3 and M4 can interleave once M2 lands (both consume the same event stream). M5 needs M4's trick flow. M6 is continuous but gets a dedicated pass at the end.

## Risks

- **Protocol ambiguities** will surface in M2: ordering of `cards-played` in the `play-card` message, exact card s-expr shape, how `'expose-trump` is acknowledged. Resolve by reading `server.rkt`/`client.rkt` (the bot client is the reference consumer) and testing against the live server.
- **Bot pacing**: server bots reply instantly, which may make turns unreadable. The client should pace animations itself (queue events, play them out at a readable tempo) rather than rendering events the moment they arrive.
