# Frontend architecture

Read this before changing anything under `app/`.

### Frontend (TypeScript/Three.js)

Server events drive a pure reducer; rendering reacts to dispatched actions
(no rules are implemented client-side — the server is the authority):

- `app/src/net/` — `sexpr.ts` (s-expression codec), `protocol.ts` (typed
  `ServerEvent`/`ClientCommand` ↔ s-expr), `SocketService.ts` (Effect-based
  WebSocket client exposing an event `Queue`)
- `app/src/game/` — `GameModel.ts` (reducer + model; phase `idle` wipes the
  model so a dead session leaves no stale hand behind), `GameSession.ts`
  (Effect program: lobby ops, event loop, seat translation, heartbeat;
  identity persists in localStorage and a dropped mid-match socket retries
  its rejoin every ~3s — behind the reconnecting holding screen, not the
  start screen — until the snapshot restores or the match is gone),
  `GameState.ts` (dispatch → Three.js/DOM rendering), `Card.ts`/`Hand.ts`/
  `PlayArea.ts` (scene objects), `MockGameProgram.ts` (`?mock=1` offline mode)
- `app/src/ui/UiOverlay.ts` — DOM overlay: start screen (incl. the `?join=`
  invite-link entry and the standings panel), room browser (list is pushed
  live by the server), lobby (share-invite button), bid/trump panels, the
  abandon (replace-with-bot) panel, the reconnecting holding screen (with
  an abandon-the-match escape), emote strip + bubbles, help overlay
- `app/src/utils/Sound.ts` — synthesized Web Audio effects (card swishes,
  turn chimes, results) reacting to dispatched actions in `GameState`; mute
  toggle persisted in localStorage; `__game.sounds()` exposes attempt counters
- `app/src/main.ts` — bootstrapping plus the `window.__game` debug bridge
  used by the Playwright suites
- URL params: `?port=`/`?server=` (game server), `?join=<table>` (invite
  link), `?mock=1`, `?debug`
- Touch plays are two-step (first tap arms/raises the card, second commits);
  mouse clicks and drags play directly. Unplayable cards dim while the
  server waits on the player.
- Installable as a PWA (`app/public/manifest.webmanifest` + icons; no
  service worker by design — the game needs the server anyway).
