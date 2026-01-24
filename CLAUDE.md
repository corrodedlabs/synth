# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

Real-time 28 card game with a Racket WebSocket server and a Vite + BabylonJS TypeScript frontend.

## Commands

### Racket (from repo root)
- `racket server.rkt` — start WebSocket server on port 8081
- `racket server.rkt --port 9000` — custom port
- `raco test -t tests.rkt` — run integration tests
- `racket client.rkt` — run bot simulation helpers

### Frontend (from `app/`)
- `npm run dev` — Vite dev server with HMR
- `npm run build` — typecheck (tsc) + production build
- No lint or unit test runner is configured for the frontend

## Architecture

### Backend (Racket)

Three main files with clear responsibilities:

- **`game.rkt`** — Pure game logic: card definitions (`card` prefab struct with name/rank/suit/point), deck generation (32 cards, 4 suits, 8 ranks), card distribution, bidding validation (16–28 range), trump selection, trick resolution, and point calculation. No external dependencies.

- **`server.rkt`** — WebSocket service with three internal modules:
  - **User module**: `user` struct (connection, email, hand, comm-channel, pic-url), global `*connected-users*` hash
  - **Room module**: `game-room` serializable struct (host, name, members), global `*game-rooms*` hash keyed by host email
  - **Game module**: orchestrates gameplay, spawns threads for long-running game operations, uses Racket channels for async player communication

- **`client.rkt`** — Bot client with `state` struct (user, hand, bid-value, selected-trump). `client-lambda` is the main bot AI loop. `simulate-game` runs a full 4-player game for testing.

### Concurrency Model

Players communicate with the game thread via Racket channels (`channel-put`/`channel-get`). The WebSocket listener fills a player's channel when their message arrives; the game thread blocks on `channel-get` waiting for the active player's decision. Long-running game operations run on separate threads to avoid blocking the listener.

### Message Protocol

All communication uses s-expressions over WebSocket text frames:
- Requests: `(symbol arg1 arg2 ...)` — e.g., `(connect-user "email" "pic-url")`
- Responses: symbols or lists — e.g., `'room-created`, `(room-members ...)`
- Dispatch in `server.rkt` uses `case` on the leading symbol
- User-scoped messages include email as first argument

### Frontend (TypeScript/BabylonJS)

- `app/src/app.ts` — Canvas creation, Babylon engine/scene init, render loop, debug inspector toggle (Shift+Ctrl+Alt+I)
- `app/src/gameScene.ts` — Scene mesh construction (currently minimal)
- `app/src/socketService.ts`, `app/src/gameRoom.ts` — Placeholders for WebSocket client and room state

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
