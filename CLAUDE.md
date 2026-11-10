# CLAUDE.md

Guidance for coding agents working in this repository. (`AGENTS.md` is a
symlink to this file, so every tool reads the same source of truth — keep
it that way.)

## Project Overview

Real-time 28 card game with a Racket WebSocket server and a Vite + Three.js TypeScript frontend. Deployed: frontend on Netlify, backend on fly.io. Pushes to `master` deploy both automatically once CI passes (`.github/workflows/deploy.yml`) — see `docs/deployment.md`.

## Rules that always apply

- **Pushing to `master` deploys to production** once CI passes, and restarts the backend. Work on a branch and open a PR.
- **The server holds every rule.** The client never implements game rules.
- **Anything long-lived spawned from `dispatch` must run under `*game-custodian*` or `*bot-custodian*`,** or it dies with the websocket that sent the message.
- **Share one `user` struct per identity,** and mutate it in place. Never copy it.
- **Run all test layers after non-trivial changes,** with browser tests headed (`HEADED=1`) by default.
- **Treat `repos/` as read-only** unless explicitly asked to edit it.

## Read when you need it

These files hold the full detail. Read the matching one before starting that kind of work.

| When you are | Read |
|---|---|
| Starting or configuring the server or frontend | [`docs/agents/commands.md`](docs/agents/commands.md) |
| Running or writing tests | [`docs/agents/testing.md`](docs/agents/testing.md) |
| Changing the Racket server, rules, bots, storage or protocol | [`docs/agents/architecture-backend.md`](docs/agents/architecture-backend.md) |
| Changing anything under `app/` | [`docs/agents/architecture-frontend.md`](docs/agents/architecture-frontend.md) |
| Writing code | [`docs/agents/code-style.md`](docs/agents/code-style.md) |
| Using `repos/` or writing Effect code | [`docs/agents/vendored-repos.md`](docs/agents/vendored-repos.md) |
| Changing `android/` or `native/` | [`android/README.md`](android/README.md) |
| Deploying | [`docs/deployment.md`](docs/deployment.md) |

## Android

The Android app is in `android/` (Kotlin) and `native/` (the table, C17, on the shared Apsis engine in the
`apsis` submodule). Read `android/README.md` before changing them. It still follows the rule above: the
server holds every rule, and the app only presents them.

## Logs, diagnostics and crashes

These read production. Reading is fine. Don't change anything on the fly.io machine without asking. The
`fly` CLI on this machine is already logged in to the app's account (`fly auth whoami`).

| What | How |
|---|---|
| Diagnostics reports from the Android app, newest first: id, time (UTC), JSON | `fly ssh console -a twenty-eight-game -C "racket /app/reports.rkt 20"` |
| The same, as JSON only, for `jq` | add `\| cut -d' ' -f3- \| jq .` |
| Game server logs, live | `fly logs -a twenty-eight-game` |
| Game server logs, recent only, without following | `fly logs -a twenty-eight-game --no-tail` |
| Android crashes and ANRs, across players | Play Console → Monitor and improve → Android vitals → Crashes and ANRs |
| One tester's crash | Their bug report (Developer options → Take bug report): `logcat` and `tombstones/` in the zip |

- **The app sends no crash reports.** A diagnostics report goes 20 s after the table starts, after each hand
  and on going to the background. A crash shows up only in Android vitals or in a bug report.
- **Reading a report:** `table` holds the GPU path and the swapchain, window and view sizes. `taps` and
  `touches` show where touches landed, and why a touch didn't count as a tap. `android/README.md`
  ("Diagnostics") lists everything in one.
- **Where they live:** the `reports` table of `/data/game.db`, for 30 days.
- **Keep them anonymous:** reports hold no player ID or name. Keep it that way, and change the privacy
  policy (`app/public/privacy/index.html`) along with anything a report holds.
