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
| Deploying | [`docs/deployment.md`](docs/deployment.md) |

## Shared engine

A planned Android version will use the shared Apsis engine (`~/workspace/apsis`, plan in its
`docs/PLAN.md`). That work hasn't started.
