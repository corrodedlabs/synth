# Vendored repositories

Read this before using anything under `repos/`, or writing Effect code.

External repositories may be vendored under `repos/` for agent reference.

- Treat vendored repositories as read-only unless explicitly asked to edit them.
- Do not import application code from `repos/`; use normal package dependencies.
- Prefer examples and patterns from vendored source over guessing from memory.
- When writing Effect code, inspect `repos/effect/` for idiomatic usage, tests, module structure, and API design.
- If `repos/effect/LLMS.md` exists, read it before writing Effect code.
