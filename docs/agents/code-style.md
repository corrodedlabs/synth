# Code style

Read this before writing code.

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
