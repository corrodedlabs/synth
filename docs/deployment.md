# Deployment — Netlify (frontend) + fly.io (backend)

Last updated: 2026-06-10

**Live now:** https://twenty-eight-game.netlify.app (frontend) ·
`wss://twenty-eight-game.fly.dev/test` (backend, fly app `twenty-eight-game`,
region bom). Redeploy with `fly deploy --remote-only` (repo root) and
`netlify deploy --build --prod` (repo root — path resolution breaks if run
from `app/`).

```
Browser ── https ──▶ Netlify (static Vite build, app/dist)
   │
   └── wss://<app>.fly.dev/test ──▶ fly.io (Racket server, 1 machine)
```

## Backend → fly.io

The server is containerised by the root `Dockerfile` and configured by
`fly.toml`. fly terminates TLS, so the browser speaks `wss://` while the
server itself only knows plain WebSockets on port 8080.

```sh
fly auth login
fly launch --no-deploy     # creates the app; accept the existing fly.toml
fly deploy
```

Notes:

- **Exactly one machine.** All state (users, rooms, running games) is in
  process memory. `fly.toml` pins `min_machines_running = 1` and
  `auto_stop_machines = "off"`; do not `fly scale count 2`.
- The app name in `fly.toml` (`twenty-eight-game`) must be globally unique on
  fly — change it if taken, and update `VITE_SERVER_URL` to match.
- Verify after deploy: `fly logs` should show `port is 8080`, and
  `wss://<app>.fly.dev/test` should accept a WebSocket connection.
- The in-process bots dial back into the server via `GAME-PORT`, which the
  `--port` flag sets automatically — nothing to configure.
- Clients ping every 25 s, so fly's proxy will not cull quiet lobby sockets.

## Frontend → Netlify

`netlify.toml` does everything: base `app/`, build `npm run build`, publish
`app/dist`, and bakes `VITE_SERVER_URL` into the bundle at build time.

Either connect the repo in the Netlify UI (it picks up `netlify.toml`), or:

```sh
netlify init      # link the repo/site once
netlify deploy --build --prod
```

If your fly app name differs from `twenty-eight-game`, set
`VITE_SERVER_URL=wss://<your-app>.fly.dev/test` — in `netlify.toml` or as a
site environment variable (UI value wins over the toml at build time only if
you remove it from the toml; prefer keeping one source).

## Server URL resolution (client)

Most specific wins (`app/src/game/GameSession.ts`):

1. `?server=ws://host:port/path` query param (any environment)
2. `?port=9000` → `ws://localhost:9000/test` (local dev convenience)
3. `VITE_SERVER_URL` baked at build time (production)
4. `ws://localhost:8081/test` (dev default)

So a Netlify deploy can still be pointed at a local or staging server with
`?server=…` for debugging.

## Local production rehearsal

```sh
docker build -t game-28 . && docker run -p 8080:8080 game-28
cd app && VITE_SERVER_URL=ws://localhost:8080/test npm run build && npm run preview
```

## Production behaviours to know

- A dropped connection unseats the player everywhere: their rooms are updated
  (or closed, if they hosted) and other members see the seat list change.
- Tables disappear from the "open tables" list the moment their game starts,
  and bots are released (connections closed) when a game ends, when they are
  kicked, or when their table closes.
- There is still no auth: identity is the self-reported email the client
  generates. Fine for a friendly game server; do not store anything sensitive.
- Mid-game reconnection: a refreshed or briefly dropped player has
  `RECONNECT-GRACE` seconds (default 45) to come back. Their identity lives
  in localStorage, so the reloaded page auto-rejoins: the server rebinds the
  seat's socket and replies to `(rejoin <email>)` with a live snapshot
  (hand, trick, scores, whose turn, pending request). Everyone else sees
  "X lost connection… / X is back" in the status line and play simply
  resumes. The server notices a dead socket within ~1s (every wait watches
  all four seats); only when the grace expires does it abort the game
  (`game-aborted`), release the bots, and return the remaining humans to
  the start screen.
- Leaving deliberately skips the grace: the in-game "leave match" corner
  button (two-step confirm) sends `(leave-game <email>)`, which aborts the
  match for the whole table at once, then resets the leaver's page to a
  working start screen.
