# Migrator

A low-level, slightly idiosyncratic PostgreSQL migration tool written in Haskell for my personal use.

- It supports PostgreSQL only.
- It supports only "up" migrations, not "down" migrations.

## Installation

```sh
stack build && stack install
```

This installs an executable called `migrate`.

## Usage

For usage, execute `migrate --help`. For usage on any individual command, `migrate --help COMMAND`, e.g., `migrate --help up`.

## Connection

Connection information is not passed with command line arguments, but using the same environment variables used by the `psql` command line tool, e.g., `PGHOST`, `PGDATABASE`, `PGUSER`, etc.

```sh
PGDATABASE=foo PGUSER=bob migrate up --drop --directory migrations
```

The specified user must have the appropriate permissions to perform the migration. What exactly those are depends on the migration and options (such as `--drop`), but in general, a migration should be run by a superuser, since it involves extensive use of DDL.
