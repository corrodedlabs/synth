# Testing

Read this before running tests, or writing or changing a test.

Four layers; run all of them after non-trivial changes. CI
(`.github/workflows/ci.yml`) runs every layer — including the browser
suites, headless — on each push.

1. **Racket unit** — `raco test game.rkt` (dealing, bidding, trick
   resolution incl. trump-exposure timing, scoring), `raco test client.rkt`
   (bot bidding/trump/play heuristics), and `raco test storage.rkt`
   (persistence rows, history, leaderboard).
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
   HEADED=1 node scripts/test-reconnect.mjs      "http://localhost:5179/?port=8082"
   HEADED=1 node scripts/test-lobby-controls.mjs "http://localhost:5179/?port=8082"
   HEADED=1 node scripts/test-multi-tables.mjs   "http://localhost:5179/?port=8082"
   HEADED=1 node scripts/test-mobile.mjs         "http://localhost:5179/?port=8082"
   HEADED=1 node scripts/test-restart.mjs        "http://localhost:5179/?port=8082"  # local only
   HEADED=1 node scripts/test-layout-matrix.mjs  "http://localhost:5179/?port=8082" "http://localhost:5179/?port=8083"  # local only
   ```

   - `play-game.mjs` — one human + 3 bots: lobby → bid → trump → all 8 tricks
     → the between-hands result; first plays use real drag/click gestures.
   - `play-multiplayer.mjs` — two browser pages play one table through the
     first hand; an emote bubble crosses the table on the way.
   - `test-match.mjs` — host + guest + 2 bots play a two-hand match: the
     between-hands panel (host deal button vs guest waiting note), match
     scores mirroring, dealer rotation, then the guest leaves (two-step
     confirm) — the host gets the abandon panel, replaces the seat with a
     bot, plays hand 3 against it, and the last human's leave closes the
     match.
   - `test-reconnect.mjs` — mid-hand page reload: localStorage identity
     auto-rejoins the seat, the snapshot restores hand/trick/scores/labels;
     then a mid-hand socket drop (backgrounded phone) shows the reconnecting
     holding screen — never the start screen's create/join — and auto-rejoins
     before the hand plays out; leaving clears the stored match.
   - `test-lobby-controls.mjs` — standings panel, invite links (incl. a
     stale invite recovering to the start screen), kick bot/human, leave,
     close table, disconnect cleanup.
   - `test-multi-tables.mjs` — several concurrent tables, room browser, join.
   - `test-mobile.mjs` — phone viewport (390×844, touch emulation): panels and
     hand must fit the screen, full hand played with real taps.
   - `test-restart.mjs` — LOCAL ONLY (not in CI: it kills and respawns the
     dev game server): proves a match survives a restart — the page
     auto-rejoins the restored match with its score carried.
   - `test-layout-matrix.mjs` — LOCAL ONLY (not in CI: it takes about 13
     minutes): every screen at 14 window sizes against a real server, measured
     for overlaps, cut-offs and small targets, with stale status lines too. The
     second URL is a `MATCH-TARGET=0` server, for the match-over panel. `SIZES=`
     and `ONLY=` narrow a run.
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
`resolveAbandon('replace'|'close')`, `kick(member)`, `roomName()`,
`cardStates()` (dimming), `emote(id)`, `inviteLink()`,
`joinByLink(name, room)`, `standings(name)`, `dropSocket()` (sever the
websocket as a backgrounded phone would; tests the reconnect screen).
Launch with `channel: "chrome"` locally (`/usr/bin/chromium` in CI sandboxes)
and `--enable-unsafe-swiftshader --use-angle=swiftshader` for headless WebGL.
