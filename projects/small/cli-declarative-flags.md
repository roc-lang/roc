# Declarative CLI Flags (Struct + Parser + Help From One Table)

## Problem

Every CLI subcommand's flag set is encoded three times in
`src/cli/cli_args.zig`: an args struct (e.g. `BuildArgs`,
`:154-181`), a hand-rolled parser of `mem.eql`/`startsWith` chains
(`parseBuild`, `:428-560`), and a hand-written help string listing
the same flags (`:446-472`) — with the top-level command roster
spelled twice more (`main_help` at `:296-310` and the dispatch chain
at `:271-284`). The same triple exists for `check`, `test`, `fmt`,
`bundle`, `unbundle`, `repl`, `glue`, `version`, `docs`, `bump`, and
`experimental-lsp`. Nothing checks that a parsed flag appears in
help or vice versa; the divergence is already latent (internal flags
like `--watch-inputs-file` are parsed but unlisted — acceptable, but
proof the copies are free to disagree).

Satellite restatements: the default opt levels exist as constants
(`default_dev_opt`/`default_build_opt`, `:82-86`) and again as prose
inside help strings ("`speed (default LLVM optimized)`", `:456`;
"`dev (default…)`", `:317`); the set of valid `--opt` values is the
`OptLevel.from_str` chain (`:62-67`) and again as help prose; and
the user-facing target rosters are string literals in help
(`:318, 457`) and in `src/cli/targets_validator.zig:406-444` (with
`roc.toml` example snippets at `:257-289, 670-819`) — all disjoint
from the `RocTarget` enum (`src/target/mod.zig:77-101`) that actually
defines the names, so a target added to the enum appears in no help
text until someone remembers.

## Background

The repo already solved the analogous problem for targets themselves:
`RocTarget` is one enum with `fromString`/`toTriple` that both the
build system and CLI consume. The flag layer never got the same
treatment because the parser is hand-rolled — but the parser's shape
is uniform enough (boolean flags, `--key=value`, positional args)
that a declarative table can generate both the matcher and the help
text without changing behavior.

## Evidence

- The `build` triple cited above; the same pattern repeats per
  subcommand through `cli_args.zig`.
- `:456` vs `:86` — the default restated in prose.
- `targets_validator.zig:406-444` — a fuller, second user-facing
  target roster with descriptions, unlinked to `RocTarget`.

## Solution design

1. **Per-command flag table.** For each subcommand, a comptime array
   of `{ long: []const u8, field: []const u8, kind: enum { flag,
   value, path }, help: ?[]const u8 }` (help `null` = internal,
   deliberately unlisted — the `--watch-inputs-file` case becomes an
   explicit decision). A generic `parseArgsFor(T, table, args)` fills
   the struct via `@field`; a generic `helpFor(name, table)` renders
   the help text. A comptime check asserts every table `field` names
   a real struct field and every non-internal struct field has a
   table entry.
2. **Interpolate the facts prose restates.** Help strings reference
   `default_build_opt` and enumerate `OptLevel` variants via
   `@tagName` loops instead of hand-listing them.
3. **Generate the target rosters.** Add a per-target metadata table
   (description, grouping) next to `RocTarget`; the help lines and
   `targets_validator`'s roster/examples render from it. A target
   added to the enum without metadata is a compile error; the roster
   can no longer omit or misspell one.
4. **One command roster.** The dispatch chain and `main_help`'s
   Commands section derive from one command table (name, summary,
   parse fn).

Keep the hand-rolled parsing *semantics* (no new dependency, no
behavior change): the table drives the same `mem.eql`/`startsWith`
comparisons the chains do today.

## What success looks like

Every criterion below must hold; the project is not done until all do:

- Adding a user-facing flag is one table entry; it parses and appears
  in help with no second edit, and omitting the struct field is a
  compile error.
- `roc build --help` and friends are byte-identical to before, except
  where the old text was already wrong/stale (each such diff called
  out in the change).
- No default value or `--opt` roster appears as prose;
  `grep -n 'default LLVM optimized' src/cli` is empty.
- A `RocTarget` variant added in a scratch build shows up in
  `--target` help and the validator's suggestions with no CLI-side
  edit.

## How to evaluate the result

### Correctness ideal

The accepted surface, the documented surface, and the struct are one
declaration; help cannot lie about flags, defaults, or targets.

### Performance ideal

Comptime table expansion produces the same runtime comparisons as the
hand chains — argument parsing cost unchanged (it runs once per
invocation; verify no measurable startup delta).

## Tests to add

- The comptime table↔struct bidirectional check (is the test).
- Golden help-text tests per subcommand (pinning the byte-identity
  claim, then serving as the change-review surface).
- A parser property test: every table entry round-trips
  (`--flag=value` → struct field set).

## Related projects

- [build-and-ci-single-lists.md](build-and-ci-single-lists.md) — the
  build-side inventory getting the same treatment.
