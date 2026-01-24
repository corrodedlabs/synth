# AGENTS.md

This repo contains a Racket game server and game logic, plus a Vite + BabylonJS
frontend in `app/`. Use this guide when making changes or running tasks.

## Repo layout
- `server.rkt`: WebSocket service, room management, and gameplay orchestration.
- `client.rkt`: Racket bot client and simulation helpers.
- `game.rkt`: Core game rules, cards, bidding, and play loop.
- `tests.rkt`: RackUnit integration test that boots the server.
- `app/`: Vite TypeScript client (BabylonJS demo scene).

## Commands

### Racket (run from repo root)
- `racket server.rkt`: start the WebSocket server on port 8081.
- `racket server.rkt --port 9000`: start server on another port.
- `racket server.rkt --timeout 30`: set idle timeout (seconds).
- `racket client.rkt`: run client helpers (used manually for simulations).

### Racket tests
- `raco test -t tests.rkt`: run the RackUnit suite in `tests.rkt`.
- `racket tests.rkt`: also works (calls `run-tests`).
- Single test: there is only one suite in `tests.rkt`; isolate or add a new
  `test-case` or file and run `raco test -t <file>` for targeted runs.

### Frontend (run from `app/`)
- `npm install`: install dependencies.
- `npm run dev`: run Vite dev server.
- `npm run build`: typecheck with `tsc` and build with Vite.
- `npm run preview`: preview production build.
- Lint/test: no lint or unit test runner is configured for the frontend.

## Runtime notes
- Server expects WebSocket text frames containing s-expressions.
- Dispatch happens in `server.rkt` via `case` on the leading symbol.
- Game rooms are keyed by host email; running games are keyed by room name.
- Each connected user has a `comm` channel for server-to-game messaging.
- Client bots connect to `ws://localhost:8081/test` by default.
- Use `with-output-to-string`/`read` for serialization and parsing.

## Message protocol conventions
- Requests are lists whose first element is a symbol (ex: `(connect-user ...)`).
- Responses are lists or symbols (ex: `'room-created`, `(room-members ...)`).
- Server replies are serialized with `write`, so avoid non-serializable structs.
- Use `serializable-struct` when sending custom data over the socket.
- Include user email as the first argument for user-scoped messages.
- When adding a new message, update the dispatch comment block in `server.rkt`.
- Validate message arity before `cadr`/`caddr` access in handlers.

## Frontend entry points
- Vite entry is `app/index.html` which loads `app/src/app.ts`.
- `app/src/app.ts` owns canvas creation, engine setup, and render loop.
- `app/src/gameScene.ts` hosts the Babylon scene helpers.
- `app/src/socketService.ts` and `app/src/gameRoom.ts` are placeholders for
  networking/room helpers; keep code modular when expanding them.

## Racket code style
- Prefer `#lang racket` at top of each file.
- Organize `require` blocks: Racket stdlib, third-party, then local files.
- Keep `provide` lists near the top; update exports when adding new public APIs.
- Use `struct` or `serializable-struct` for data models shared over the wire.
- Keep module-level mutable state near its definition (hashes, registries).
- Naming conventions:
  - `+name+` for constants (ex: `+max-players+`).
  - `*name*` for mutable globals or registries.
  - `name?` for predicates.
  - `name->value` for conversion helpers.
- Prefer `define` for named functions, `λ` for inline lambdas.
- Use `case-lambda` when functions accept multiple arities.
- Use `match` / `match/values` for destructuring; keep patterns explicit.
- Prefer `match` over nested `car`/`cdr` when processing messages.
- Favor pure functions in `game.rkt`; limit mutation to server state modules.
- When mutating hashes, use `hash-set!`/`hash-update!` and keep updates local.
- Use `case` for message dispatch with explicit fallthrough `else`.
- Keep list operations explicit (`map`, `foldl`, `filter`, `findf`).
- Formatting: 2 spaces indentation, trailing parens aligned with `define`.
- Avoid overly long lines for nested `cond`/`let`; split into helper functions.
- Prefer `let`-based loops over mutation-heavy recursion.
- Logging uses `displayln` + `format`; keep logs brief and actionable.
- Error handling:
  - Use `(error "message" context ...)` with descriptive text.
  - Include relevant values (user id, room name, etc.) in error context.
  - Avoid placeholder or profane error messages in new code.
- Concurrency:
  - Use `channel-put`/`channel-get` for player messaging.
  - Guard WebSocket send paths with connection checks.
  - Run long-lived game operations on `thread` to avoid blocking the listener.
- Networking:
  - Messages are s-expressions serialized via `write`/`read`.
  - Keep message shapes documented in `server.rkt` dispatch section.
  - Validate message arity before destructuring in handlers.

## TypeScript/BabylonJS code style
- Use ES module imports; group by:
  - side-effect imports (Babylon inspector, debug layers)
  - external packages
  - local modules
- Use 2-space indentation, semicolons, and double quotes for strings.
- Favor `const` and `let`; `var` appears in older code but avoid for new code.
- Prefer named exports; use `export default` only when a file truly owns a
  single responsibility.
- Use `import type` when adding new type-only imports.
- Naming conventions:
  - `PascalCase` for classes and types.
  - `camelCase` for functions, variables, and file names.
  - `UPPER_SNAKE_CASE` only for true constants (rare in this codebase).
- Types:
  - Rely on inference for simple locals.
  - Add explicit types when interacting with BabylonJS or DOM APIs.
  - Keep exported function signatures typed.
- Keep scene objects scoped to the scene builder to avoid global state.
- Prefer descriptive IDs for meshes (ex: "table", "card-1").
- Keep rendering setup in `app.ts` and scene construction in `gameScene.ts`.
- Prefer small scene helpers over monolithic constructors.
- Avoid unused parameters (TS config has `noUnusedParameters: true`).
- When adding async logic, avoid top-level `await` in module scope.
- Error handling:
  - Validate external inputs (websocket messages, DOM presence).
  - Wrap async or network code with `try/catch` and surface errors.
  - Prefer explicit user-facing errors over silent failures in the UI.

## BabylonJS patterns
- Scene setup is currently minimal; keep `initGameScene` focused on mesh setup.
- Avoid DOM access inside `gameScene.ts`; leave it to `app.ts`.
- Pass `Engine`/`Scene` references into helpers if new modules need them.
- Prefer helper functions for lights, cameras, and materials.
- Keep inspector/debug toggles centralized in `app.ts`.

## Testing notes
- `tests.rkt` starts the server and exercises WebSocket flows.
- Tests assume `server.rkt` defaults to port 8081 and localhost.
- Keep test timeouts conservative; the suite currently sleeps 3 seconds.
- If you add more tests, ensure the server is started/stopped once per suite.

## Workflow expectations
- Keep changes scoped to relevant files; avoid formatting unrelated sections.
- Update `AGENTS.md` if you add new commands or repo rules.
- Run `raco test -t tests.rkt` after server changes when practical.
- Run `npm run build` after frontend changes when practical.
- Do not commit secrets (tokens, private keys) to the repo.

## Cursor/Copilot rules
- No `.cursor/rules`, `.cursorrules`, or `copilot-instructions` files found.
- If new rules are added later, mirror them in this document.
