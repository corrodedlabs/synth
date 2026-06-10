# Twenty-Eight (28) — Game Features

Status: implemented, production-ready · Branch: `feat/game-implementation` · Last updated: 2026-06-10

A complete, browser-playable game of 28 against the Racket server. The server
remains the single source of truth for rules; the frontend renders state and
sends player intents. This doc lists everything the game does today, with the
lobby/multiplayer flow added on top of the original quick-play implementation.

## Running it

```sh
racket server.rkt                 # WebSocket server, port 8081 (or --port 9000)
cd app && npm run dev             # Vite dev server
```

Open the dev URL in a browser. If the game server is not on 8081, point the
client at it with `?port=9000` or `?server=ws://host:port/test`.

---

## Lobby & multiplayer

The game no longer auto-starts. Sitting down is an explicit, social flow:

- **Title screen with name entry** — type your name (it becomes your identity
  at the table; a random suffix keeps emails unique server-side), then either
  create or join a table.
- **Create a table** — you become the host of a named table
  (e.g. *"Antash's table #x7k"*) and land in the lobby. Any number of tables
  can run at once (one per host); they all appear in the room browser.
- **Join a table** — a room browser lists every open table on the server with
  its name and seat count (`2 of 4`), with a Refresh button. Joining a full
  table is prevented client-side (full tables are filtered out).
- **Lobby with live seats** — all four seats are shown, filling in real time
  as players join. Each row shows the player's name plus `host` / `you` tags;
  bots appear as *Bot 1*, *Bot 2*, … Everyone in the room sees membership
  updates instantly.
- **Add a bot** (host only) — fills the next seat with a server-side bot.
  Disabled once the table is full. Any mix works: 1 human + 3 bots,
  2 humans + 2 bots, etc.
- **Start game** (host only) — disabled until exactly 4 players are seated.
  The game begins only when the host clicks it; guests see *"Waiting for the
  host to start"*. Guards exist at every layer: the button is disabled, the
  client refuses to send a premature start, and the server reports
  `start-game-failed` (surfaced as a status message) if a start slips through
  anyway.
- **Leave / close table** — every player has a *Leave table* button; for the
  host it reads *Close table* and dissolves the room, returning all members to
  the title screen with a "host closed the table" notice. Bots seated at a
  closed table are released server-side.
- **Kick players** (host only) — each occupied seat (bot or human) shows a ✕
  button. Kicked humans are told "the host removed you from the table" and land
  back on the title screen, free to rejoin; kicked bots are disconnected and
  cleaned up.
- **Disconnect cleanup** — closing a tab (or losing the connection) unseats
  that player everywhere: remaining members see the seat free up, and a
  disconnected *host's* table closes and vanishes from the room browser.
- **Error handling** — joining a vanished room, a dropped connection, or an
  unreachable server all surface a readable status line and return you to the
  title screen.
- **How to play** — a persistent *how to play* link (bottom-right, available
  on the title screen and mid-game) opens a short rules overlay: teams, card
  points, bidding, hidden trump, following suit, and made/set scoring.

## Gameplay (one human + any mix of humans/bots)

- **Player names at the table** — once the hand is dealt, each opponent's
  name floats by their seat marker (partner is tagged "your partner"), and all
  announcements use real names: "priya bids 17", "Bot 2 takes the trick (+3)",
  "priya's team bid 23 and took 9 — set".
- **Dealing** — 4 cards before the auction, 4 after, rendered as a fanned hand
  with hover lift and preview.
- **Bidding** — a panel appears on your turn showing the standing bid, with
  raise and pass controls; opponents' bids and passes are announced near
  their seat markers. The auction result (winner + final bid) is announced.
- **Trump selection** — if you win the auction, a brush-style suit picker
  appears; if a bot or another player wins, you see their choice was made
  without revealing the suit.
- **Turn-locked play** — cards are draggable (or clickable) only when the
  server asks you for a card. Illegal plays are blocked client-side as a UX
  hint; the server stays the validator.
- **Trump exposure** — when you cannot follow suit and trump is hidden, a
  *Reveal trump* button appears (sends `expose-trump`); the trump indicator
  flips from hidden to revealed for everyone.
- **Trick flow** — played cards land in the play area per seat; completed
  tricks pause to be read, then are credited to the winner with a
  *"X takes the trick (+points)"* announcement and live score/trick counters.
- **Hand result** — a zen-styled overlay shows team points, the bid target,
  whether the bidding team made it or got set, and a *Play again* button that
  tears the session down and returns to a fresh title screen.
- **Pacing** — server bots answer instantly, so the client paces incoming
  events (bid/card/trick pauses) to keep the game readable.

## Production readiness

Deployable as-is: frontend to Netlify, backend to fly.io — see
[deployment.md](./deployment.md) for the step-by-step. What makes it
production-grade:

- **Containerised backend** — root `Dockerfile` + `fly.toml` (TLS terminated
  by fly, single always-on machine because game state is in memory); the
  server now survives running without a TTY (it used to exit instantly when
  stdin was absent).
- **Netlify frontend** — `netlify.toml` builds `app/` and bakes the server
  endpoint in via `VITE_SERVER_URL` (must be `wss://…` behind https).
- **Connection hygiene** — a dropped websocket used to spin the handler in a
  busy loop forever; it now cleans up the user, their rooms, and notifies
  remaining members. Broadcasts skip dead connections instead of crashing.
- **Heartbeat** — clients ping every 25 s (server answers `pong`) so hosting
  proxies don't cull quiet lobby sockets.
- **Room lifecycle** — running games are actually tracked (double-start is
  rejected), tables are delisted the moment their game begins, and bots are
  released when a game ends, a bot is kicked, or its table closes.
- **Robust dispatch** — a malformed or failing message returns an `(error …)`
  reply instead of silently killing the handler thread.

## Infrastructure & configuration

- **Server URL resolution** — `?server=ws://…` beats `?port=…` (localhost)
  beats `VITE_SERVER_URL` (production build) beats the dev default
  `ws://localhost:8081/test`.
- **Bots follow the server port** — `racket server.rkt --port 9000` now works
  end-to-end: the in-process bots dial back into the right port (previously
  hardcoded to 8081 in `client.rkt`).
- **`start-service` accepts a port** — `(start-service 18081)` lets tests run
  on a port that doesn't collide with a dev server.
- **Mock mode** — `?mock=1` still runs the offline timer-driven mock for UI
  iteration without a server; `?debug` opens the lighting/layout tuning panel.

## Server protocol additions

All lobby traffic uses the existing s-expression protocol:

| Message | Direction | Notes |
| --- | --- | --- |
| `(join-room <room> <email>)` | client → server | now broadcasts `(room-members <room> <emails>)` to every member — same shape as the add-bot broadcast — and acks the joiner with `room-joined`; unknown rooms get `(error no-such-room)` instead of a silent hang |
| `(leave-room <room> <email>)` | client → server | unseats the member (acked `room-left`, members broadcast updated); the host leaving closes the table |
| `(kick-from-room <room> <host> <target>)` | client → server | host-only; target gets `(removed-from-room <room>)`, bots are released |
| `(room-closed <room>)` | server → client | the table was dissolved (host left/closed it or disconnected) |
| `(get-active-rooms)` | client → server | reply is now tagged `(active-rooms <rooms>)` so clients can distinguish it |
| `(start-game-failed <reason>)` | server → client | sent to the requester when a start is rejected (`room-not-ready`, `game-already-started`) |
| `(ping)` | client → server | liveness heartbeat, answered with `pong` |

Frontend protocol layer (`app/src/net/protocol.ts`) gained matching
`JoinRoom`/`GetActiveRooms` commands and `RoomJoined`/`ActiveRooms`/
`StartGameFailed` events, all covered by vitest round-trip tests.

## Testing

- **`app/scripts/play-game.mjs`** — Playwright drives a real browser through
  the whole single-player flow: title screen → create table → verifies the
  start button is gated (and that a forced premature start does nothing) →
  seats 3 bots → starts → bids/raises/passes → trump → plays all 8 tricks
  (first two via real drag and click gestures) → result screen. Screenshots
  land in `/tmp/game-shots`.
- **`app/scripts/test-lobby-controls.mjs`** — two browsers exercising every
  membership control: kick a bot, kick a human (who sees the notice and can
  rejoin), voluntary leave, host closing the table (guest notified, table gone
  from the browser list), and disconnect cleanup (closing the host's tab frees
  the room). Screenshots in `/tmp/game-shots-lobby`.
- **`app/scripts/test-multi-tables.mjs`** — two hosts create tables
  simultaneously; a third user sees both in the browser and joins one without
  affecting the other.
- **`app/scripts/play-multiplayer.mjs`** — two browser pages: host creates a
  table, guest finds it in the room browser and joins, both lobbies show the
  live seat list, guest is verified to have no host controls, host adds 2 bots
  and starts, and *both* browsers play the hand to completion; final scores
  are cross-checked between the two views. Screenshots in `/tmp/game-shots-mp`.
- **`cd app && npm test`** — vitest suite for the s-expression codec and
  protocol (including the new lobby messages).
- **`raco test -t tests.rkt`** — server integration tests, now on a dedicated
  test port (18081).
- **`cd app && npm run build`** — typecheck + production build, clean.

Both Playwright scripts take the app URL as an argument, e.g.
`node scripts/play-game.mjs "http://localhost:5173/?port=9000"`, and exit
non-zero on any console error or stuck state.

## Known limitations

- One hand per game: the server's `score-game` is still a stub, so there are
  no multi-round games; *Play again* starts a fresh room.
- No reconnection/resync if a player drops mid-game — the lobby cleans up,
  but an in-flight hand stalls for the remaining players.
- No authentication: identity is the client-generated email. Kick/start are
  validated against the room host's email, which a hostile client could spoof.
  Acceptable for a friendly game; not for anything sensitive.
