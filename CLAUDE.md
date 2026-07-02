# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Overview

`migrator` is a standalone CLI (`migrate`) for low-level, "up"-only PostgreSQL migrations. It is now fully independent — it does **not** depend on `prosumma` or `fundamental` (the `prosumma` extra-dep was removed; see git history around the `Prosumma.PG` → `Migrator.PG` port). It deliberately stays on `postgresql-simple` even though sibling projects are moving to `hasql` — see `../CLAUDE.md` for that context.

## Commands

```sh
stack build                              # build library + migrate executable
stack test                                # run test suite (currently a non-functional placeholder — see below)
stack exec migrate -- <args>              # run the CLI
PGDATABASE=foo PGUSER=bob migrate up --drop --directory migrations   # example invocation
```

Connection info comes from standard `psql` env vars (`PGHOST`, `PGPORT`, `PGUSER`, `PGPASSWORD`, `PGDATABASE`), read directly with `System.Environment.lookupEnv` in `Migrator.Migrate.readConnectInfo` — not from CLI flags. Every one of these has a hardcoded default (see that function), so there's no "missing required env var" failure mode here.

`test/Spec.hs` is an un-implemented stub (`putStrLn "Test suite not yet implemented"`) that doesn't even compile under `NoImplicitPrelude` (no `import RIO`, so `IO` isn't in scope). This is pre-existing, not something to "fix" incidentally — flag it if asked to touch tests here.

## Architecture

- **`app/Main.hs`** — `optparse-applicative` CLI wiring. Two subcommands: `up` (`Migrate`) and `new` (`New`), dispatching to `Migrator.migrate` / `Migrator.createNewMigration`.
- **`src/Migrator.hs`** — thin re-export of `Migrator.Migrate` (`migrate`) and `Migrator.New` (`createNewMigration`).
- **`src/Migrator/Migrate.hs`** — the actual migration runner: reads `ConnectInfo` from env, optionally creates the database/`__migrations` schema/`__migrations.migrations` table (gated by `--no-init`/`shouldInit`), then walks the migrations directory in sorted order and applies any `.psql` file not yet recorded in `__migrations.migrations`, skipping-with-a-warning any file whose timestamp prefix is lower than the current high-water mark (out-of-order backfill protection).
- **`src/Migrator/New.hs`** — scaffolds a new `<timestamp>_<description>.psql` file under the migrations directory.
- **`src/Migrator/Internal.hs`** — shared trivia: `logSource` ("MIGRATE"), `fileRegex` (`^[0-9]{14}_[a-zA-Z0-9_-]+\.psql$` — the filename convention migrations must follow), and `uformat` (formats a `Formatting` `Format` straight to a RIO `Utf8Builder`; ported from `Prosumma.Util`/`Fundamental.Formatting`, kept as a 3-line local copy rather than a dependency).
- **`src/Migrator/PG.hs`** — self-contained `postgresql-simple` access layer (`PG r`/`RPG`, `execute`/`query`/`value1`/`query1`/`withTransaction`, `parseConnectInfo` for parsing a libpq-style connection string). This was ported from `Fundamental.PG` but is deliberately simplified: it inlines what used to be the separate `Fundamental.PG.QueryRunner` module and **drops `ConnectionPool` support entirely**, because migrator only ever runs against a single `Connection` (see `runConnection` in `Migrate.hs`) — there's no pooling here and there shouldn't be. If you're tempted to reintroduce pooling or split `QueryRunner` back into its own module, don't, unless migrator actually grows a use case for concurrent connections.

## Conventions

- `NoImplicitPrelude` + `rio` as the prelude, like the other projects in this collection.
- `-Wall`-and-friends warning-clean (see `package.yaml` `ghc-options`) — this includes `Migrator.PG`, which needs `NamedFieldPuns`/`RecordWildCards` enabled locally via a file-level pragma (not project-wide default-extensions, since it's the only module that needs them).
- Migrations are plain `.psql` files, timestamp-prefixed (`YYYYMMDDHHMMSS_description.psql`), applied in filename-sorted order, tracked by filename (minus extension) in `__migrations.migrations`.
