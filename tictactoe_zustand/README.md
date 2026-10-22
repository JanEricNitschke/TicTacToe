# TicTacToe-Zustand

Minimal React project for the
[Zustand tic-tac-toe tutorial](https://zustand.docs.pmnd.rs/learn/guides/tutorial-tic-tac-toe.html).

## Setup

```bash
pnpm install
pnpm dev
```

The app is in `src/App.tsx`.

## Format, typecheck and lint

```bash
pnpm format
pnpm format:check
pnpm typecheck
pnpm lint
pnpm lint:fix
```

Formatting uses Oxfmt with semicolons. TypeScript runs in strict mode with
select additional checks. Oxlint is configured with all non-nursery categories
as errors, plus a curated set of restriction rules and zero tolerance for
warnings. A few rules that conflict with idiomatic React are disabled.
