# Frontend Build Guide

This document describes how to build the Twenty-Eight frontend in `app/`.
The backend exists in Racket, but this guide focuses on the Vite, TypeScript,
Three.js client.

## Current Frontend Shape

The app is a Vite project:

```text
app/
  index.html
  src/main.ts
  src/scene/
  src/game/
  src/interaction/
  src/utils/
  public/textures/
```

Primary entry points:

- `app/index.html`: page shell and score UI.
- `app/src/main.ts`: creates `SceneManager`, `GameState`, and debug controls.
- `app/src/scene/SceneManager.ts`: owns the Three.js scene, camera, renderer,
  resize handling, and render loop.
- `app/src/game/GameState.ts`: coordinates scene objects, model updates, and
  the mock game runtime.
- `app/src/game/GameModel.ts`: stores the frontend game model and reducer.
- `app/src/game/MockGameProgram.ts`: drives the current mock deal and trick.
- `app/src/game/Card.ts`: renders an individual 3D card.
- `app/src/game/Hand.ts`: lays out the local player's hand.
- `app/src/game/PlayArea.ts`: lays out played cards and active-player markers.
- `app/src/interaction/DragControls.ts`: handles mouse and touch card dragging.

## Running Locally

Install dependencies:

```sh
cd app
npm install
```

Start the dev server:

```sh
npm run dev
```

Build the production bundle:

```sh
npm run build
```

Preview the production build:

```sh
npm run preview
```

There is no frontend lint or unit test runner configured yet.

## Current Runtime

The frontend currently runs without the Racket server.

`GameState` starts `mockGameProgram`, which:

1. Generates a random eight-card hand.
2. Deals those cards to the local player.
3. Marks the local player active.
4. Plays a local card.
5. Plays three opponent cards.
6. Updates score and trick count.

This is useful for visual iteration, but it is not complete game logic. Real
Twenty-Eight behavior still needs bidding, trump, legal move validation, trick
resolution, and server synchronization.

## Target Frontend Architecture

Keep the frontend split into four layers:

1. Model layer
2. Runtime layer
3. Scene layer
4. Interaction layer

The model layer should own pure game state and actions. Extend
`GameModel.ts` before adding UI-specific state to scene classes.

Suggested model additions:

```ts
export interface BidState {
  readonly currentBid: number | null;
  readonly highBidder: PlayerIndex | null;
  readonly passedPlayers: readonly PlayerIndex[];
}

export interface TrumpState {
  readonly suit: Suit | null;
  readonly selectedBy: PlayerIndex | null;
  readonly exposed: boolean;
}

export interface TrickState {
  readonly leader: PlayerIndex;
  readonly ledSuit: Suit | null;
  readonly cards: readonly PlayedCardModel[];
}
```

The runtime layer should translate async events into model actions. Today this
is `MockGameProgram.ts`; later it should include a WebSocket client for server
messages.

The scene layer should render the current model. It should not decide whether a
card play is legal.

The interaction layer should collect user intent: select a bid, choose trump,
drag a card, expose trump, or pass. It should ask the model/runtime whether an
action is allowed.

## Implementation Slices

Build the frontend in small, visible slices.

### 1. Make the Mock Game Deterministic

Replace random cards with a valid Twenty-Eight deck fixture.

Goals:

- Use only ranks `J`, `9`, `A`, `10`, `K`, `Q`, `8`, `7`.
- Use only suits `hearts`, `diamonds`, `clubs`, `spades`.
- Prevent duplicate cards.
- Deal eight cards to each player in the model.
- Render only the local player's hand face up.

### 2. Add Trick Rules

Implement frontend helpers for:

- Card rank comparison.
- Card point value.
- Led suit detection.
- Follow-suit validation.
- Exposed-trump trick winner calculation.
- Trick point total.

Keep these helpers pure so they can be unit tested later.

### 3. Gate Card Dragging by Turn and Legality

`DragControls` currently allows any local hand card to be dragged into the play
area. Update it so:

- Cards are draggable only when `activePlayer === 0`.
- Illegal cards remain in hand.
- Dropping outside the play area returns the card to its hand position.
- A valid drop dispatches one `CardPlayed` action.

### 4. Add Bidding UI

Add a compact UI panel for the bidding phase.

Required controls:

- Current bid display.
- Bid higher button or stepper.
- Pass button.
- Active bidder indicator.
- Final bid and bidder summary.

Do not put game instructions inside the play surface. The UI should expose
available actions, not explain the rules.

### 5. Add Trump Selection and Exposure

After bidding, let the winning bidder choose trump.

Frontend states:

- Trump chooser visible only for the winning bidder.
- Hidden trump indicator visible to all players.
- Expose-trump action available only when a player cannot follow suit.
- Exposed trump suit visible for the rest of the hand.

### 6. Replace the Mock Runtime with a Server Runtime

The Racket server expects WebSocket text frames containing s-expressions.
Frontend integration should use a small boundary module, for example:

```text
app/src/network/
  sexp.ts
  GameSocket.ts
  protocol.ts
```

Responsibilities:

- Serialize frontend requests into server s-expressions.
- Parse server responses into typed TypeScript objects.
- Convert protocol events into `GameAction` values.
- Keep WebSocket reconnect and error handling outside scene classes.

Start the server separately when testing integration:

```sh
racket server.rkt
```

The default server port is `8081`.

## Project Plan and Milestones

The frontend should be built in milestones that each produce something usable.
Do not wait for full multiplayer integration before validating the game feel.
Start with a deterministic local game, then connect it to the server once the
client model and interactions are stable.

### Milestone 0: Product and Rule Alignment

Goal: make sure the team agrees on the exact version of Twenty-Eight being
implemented before code starts expanding.

Deliverables:

- Confirm the final player order and team mapping.
- Confirm the exact bidding rules, including minimum bid, maximum bid, pass
  behavior, and redeal behavior.
- Confirm when trump can be exposed.
- Confirm how round score, hand score, and match score should be displayed.
- Confirm whether the first release is local mock play, online multiplayer, or
  both.

Required before completion:

- A short protocol note for all game phases: lobby, deal, bid, trump, play,
  trick complete, hand complete, game complete.
- Agreement on which rules are enforced by the server and which are mirrored in
  the frontend for fast feedback.

### Milestone 1: Frontend Game Model

Goal: create a reliable TypeScript model for a complete Twenty-Eight hand.

Deliverables:

- Deterministic 32-card deck.
- Player hands for all four players.
- Bid state.
- Trump state.
- Current trick state.
- Completed trick history.
- Team score state.
- Pure helper functions for rank order, card points, legal moves, trick winner,
  and trick points.

Required before completion:

- Add a lightweight frontend test runner, preferably Vitest.
- Cover the pure game helpers with unit tests.
- Keep all game-rule functions outside Three.js scene classes.

### Milestone 2: Local Playable Prototype

Goal: make one complete hand playable in the browser without the server.

Deliverables:

- A deterministic or scripted mock runtime that deals valid cards.
- Bidding flow with simple bot decisions for non-local players.
- Trump selection for the local player when the local player wins the bid.
- Automated trump selection when a bot wins the bid.
- Legal local card dragging.
- Bot card play for other players.
- Trick resolution after four cards are played.
- Score updates after each trick.
- Hand completion after eight tricks.

Required before completion:

- The local player cannot play out of turn.
- Illegal cards cannot be dropped into the play area.
- A completed hand can be replayed from a reset button or dev command.
- `npm run build` passes.

### Milestone 3: Core UI and Interaction Polish

Goal: make the game understandable and comfortable on desktop and mobile.

Deliverables:

- Compact bidding controls.
- Trump chooser and hidden/exposed trump indicator.
- Active player indicator for all four seats.
- Local hand states for playable, blocked, selected, and dragging cards.
- Clear center trick layout.
- Completed trick animation or cleanup.
- Responsive layout pass for phone, tablet, and desktop.

Required before completion:

- No instructional text inside the game surface.
- UI controls expose only currently valid actions.
- Text does not overlap or clip on common mobile widths.
- Cards remain easy to inspect and drag on touch screens.

### Milestone 4: Server Protocol Integration

Goal: replace the mock runtime with WebSocket-driven game events.

Deliverables:

- `app/src/network/sexp.ts` for parsing and writing server s-expressions.
- `app/src/network/protocol.ts` for typed frontend protocol messages.
- `app/src/network/GameSocket.ts` for WebSocket connection lifecycle.
- Runtime adapter that maps server messages to `GameAction` values.
- Outgoing command functions for join room, bid, pass, choose trump, expose
  trump, and play card.
- Error and disconnect states.

Required before completion:

- Server messages have stable documented shapes.
- Frontend validates every incoming message before updating state.
- Reconnect behavior is defined, even if the first version only shows a
  reconnect prompt.
- The mock runtime still exists as a dev mode for visual work.

### Milestone 5: Multiplayer Game Flow

Goal: support real users playing a complete hand together.

Deliverables:

- Connect or join room flow.
- Seat assignment.
- Waiting-for-players state.
- Ready/start game state.
- Synchronized bidding.
- Synchronized trump selection and exposure.
- Synchronized trick play.
- Hand result display.
- New hand flow.

Required before completion:

- The frontend handles slow, duplicate, and out-of-order-looking messages
  defensively.
- The active player shown in the UI always comes from authoritative game state.
- The client does not assume that a local drag means a card was accepted until
  the runtime confirms the play.

### Milestone 6: Quality, Performance, and Release Readiness

Goal: make the frontend stable enough for regular playtesting.

Deliverables:

- Production build checks.
- Browser smoke tests for desktop and mobile viewports.
- Basic Playwright coverage for loading, bidding, card drag, and trick
  completion.
- Performance pass for renderer settings, texture sizes, and shadow quality.
- Error logging that is useful during playtests.
- Updated docs for running the frontend with and without the server.

Required before completion:

- `npm run build` passes consistently.
- The app can recover or clearly fail when the server is unavailable.
- A new contributor can run the app from the docs.
- Known gameplay gaps are listed before release or playtest.

## What Else Is Required

The frontend can move forward with the current repository, but a few decisions
and additions will make the work much smoother.

### Rule Decisions

- Exact redeal behavior.
- Whether a player may call trump only when void in the led suit.
- Whether the UI should show team card points, team tricks, or both.
- Whether match scoring is needed beyond one hand.
- Whether bots are needed for development-only mock play or as a real feature.

### Design and UX Requirements

- Final seat labels or player identity treatment.
- Visual treatment for hidden trump.
- Visual treatment for exposed trump.
- Bidding control layout for small screens.
- End-of-hand result treatment.
- Loading, disconnected, reconnecting, and invalid-action states.

### Technical Requirements

- A frontend unit test runner.
- A browser smoke-test path, preferably Playwright.
- A typed protocol boundary for WebSocket messages.
- A development mode switch between mock runtime and server runtime.
- A stable fixture deck for repeatable local testing.
- A small state-debug view for inspecting model state during development.

### Backend Contract Requirements

- Message names and arities for every frontend action.
- Response shapes for lobby, deal, bid, trump, play, trick result, and hand
  result.
- Error response shapes.
- Whether the server sends full snapshots, incremental events, or both.
- Whether card IDs are server-generated or frontend-derived.
- Reconnection and resume behavior.

### Assets and Content Requirements

- Complete card face textures for all 32 cards.
- Card back texture.
- Trump indicator asset or UI treatment.
- Optional seat markers for partner and opponents.
- Final app title and favicon.

### Suggested First Work Order

Start with this order:

1. Add Vitest and tests for pure card/rule helpers.
2. Replace random mock cards with a deterministic 32-card deck fixture.
3. Extend `GameModel.ts` for bid, trump, trick, and team score state.
4. Build the local complete-hand mock runtime.
5. Gate drag/drop through legal move checks.
6. Add bidding and trump controls.
7. Add the WebSocket protocol boundary.
8. Swap the runtime from mock events to server events.

## Suggested Game Actions

Extend `GameAction` gradually as features land:

```ts
| { readonly _tag: "BidPlaced"; readonly playerIndex: PlayerIndex; readonly bid: number }
| { readonly _tag: "PlayerPassed"; readonly playerIndex: PlayerIndex }
| { readonly _tag: "TrumpSelected"; readonly playerIndex: PlayerIndex; readonly suit: Suit }
| { readonly _tag: "TrumpExposed" }
| { readonly _tag: "TrickCompleted"; readonly winner: PlayerIndex; readonly points: number }
| { readonly _tag: "HandCompleted"; readonly biddingTeamWon: boolean }
```

Prefer model actions that describe game events, not rendering instructions.
Rendering should be a consequence of state changes.

## Visual Direction

Follow `app/THEME.md`:

- Minimal zen table.
- Cream paper tones.
- Black ink card art.
- Soft shadows.
- Calm, direct animations.
- No saturated colors or noisy effects.

The current card assets live in:

```text
app/public/textures/
```

The card texture loader is in:

```text
app/src/utils/ZenCardTextures.ts
```

## Verification Checklist

Before considering a frontend change complete:

- Run `npm run build` from `app/`.
- Confirm the app starts with `npm run dev`.
- Check desktop and mobile viewport sizes.
- Confirm cards render nonblank.
- Confirm drag, hover, and drop interactions still work.
- Confirm score and trick UI update from model actions.
- Confirm no scene class contains duplicated game-rule logic.

For larger UI changes, use a browser screenshot pass so overlap, clipping, and
mobile layout issues are caught before handoff.
