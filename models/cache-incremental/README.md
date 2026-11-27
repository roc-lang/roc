# Cache-incremental model

A differential model of the incremental build cache: the checked, canonicalized and pairing caches
(`src/compile/coordinator.zig`, `src/check/checked_artifact.zig`, `src/compile/cache_versioning.zig`), the `.boxy`
object and unit-image cache (`src/compile/boxy_object_cache.zig`, `src/compile/boxy_interface.zig`) and `roc test`'s
outcome files. It decides one judgment: after any sequence of edits, every command run with the warm cache behaves
exactly like the same command run on the same sources with an empty cache directory. A wrong reuse produces a wrong
program with no error, and nothing else checks for one; minici's cache-reuse audit only counts how much was reused.
The model works as a generator. It writes a random multi-module program, then applies random small edits one at a
time, and after each edit runs a few commands twice, once with the sequence's warm cache and once with a fresh empty
one, and compares them.

## Requirements

- Python 3.10 or later, standard library only.
- A `roc` built from this checkout (`zig build roc`). The warm runs of half the sequences set the existing developer
  diagnostic `ROC_VERIFY_BOXY_OBJECT_REUSE`, which only a Debug or `-Ddev-diagnostics` compiler reads; with a release
  compiler those runs are simply unverified. The model adds no compiler hook.

## Quick start

```bash
./run.sh selftest
```

```bash
./run.sh fuzz 10
```

```bash
./run.sh replay out/fuzz/<seed>
```

`ROC` selects the compiler (default: `zig-out/bin/roc` at the repository root). Output goes to `out/`.

`fuzz N` runs N edit sequences (seeds `--seed S` onward, default 1) of `--steps K` edits (default 12), `--jobs J` at
a time (default 2, or `JOBS`). Each sequence picks its own configs unless `--configs` names them; `--no-llvm` leaves
out the LLVM ones and `--no-verify` never sets `ROC_VERIFY_BOXY_OBJECT_REUSE`. A sequence that agrees everywhere is
deleted unless `--keep`. One that disagrees stays in `out/fuzz/<seed>/`: `edits.txt` (one line per edit),
`states/<i>/` (each state's files and its `prog.pickle`), `configs.txt`, and `verdict.txt` (the first difference and
both runs' full output). `replay DIR` rebuilds the states in order with a new warm cache; `--states 0,5` replays a
subset. `reduce DIR` keeps the failing state last and drops other states and configs while the disagreement remains,
and writes the result to `DIR/min/`, which `replay` accepts. `gen SEED --steps K [--all]` prints a sequence's edits and
its states' files without running anything.

A disagreement is rerun on a second empty cache before it is reported. If the two clean runs differ from each other,
the sequence is reported as `flaky` (nondeterministic output), not as a cache bug.

Every edit sequence is a function of its seed, so `fuzz 1 --seed S` regenerates it; the saved states make it
independent of later generator changes.

## How it works

1. `gen.py` builds a program from small typed constructs. The expression language, its generator and its reference
   evaluator come from the backend-diff model (functions, closures, higher-order functions, tail loops, non-tail
   recursion, mutually tail-recursive groups, polymorphic functions, over `I64`, `Str`, `Bool`, `List(I64)`, a record
   and a tag union). This copy adds:
   - 2 to 5 modules, the lowest 0 to 2 of them in a path package `lib/` that the app imports as `lib.M<i>`; local
     modules are type modules (`M :: [].{ ... }`) or `module [...]` headers;
   - the record and the tag union as nominal types `Rec := { a : I64, b : Str }` and `Tag := [P(I64), Q(Str), W]` in
     their own type modules, built only through `Rec.mk` and `Tag.P(...)`, and matched with a final `_` branch, so
     their declarations can change without any importer's source changing;
   - top-level constants (evaluated at compile time, including function-valued ones), private definitions (outside a
     type module's block, or left out of a header's exposing list), and top-level `expect`s that compare an
     expression with the reference's value for it (a quarter of them with another value, so they fail).
2. `edit.py` changes a program value: a literal (a constant's value, a step of a body, an output, an expected value);
   a whole body; add or remove a definition, an output or an expect; move a definition (or recursive group) to another
   module; add, remove or reorder `Rec` fields and `Tag` tags; drop or restore an annotation on a definition or a
   binding; comments and blank lines only; an unused import; hide or expose a definition; switch a module between
   type module and header; move a module into or out of the package; inject or fix a type error. The driver also
   reverts to an earlier state about one step in ten. An edit is kept only if the reference still evaluates the
   result, so every state runs unless it carries an injected error.
3. `driver.py` writes each state into one workspace and runs every chosen config on it twice in parallel: the warm
   chain uses the sequence's cache directory, the clean chain a new empty one. Configs: `check`, `test`, `test-spec`
   (`roc test --opt=dev --specialize=yes`), `run` (`roc run`, dev, `.boxy`), `run-spec`, `interp`
   (`--opt=interpreter`), `dev`, `dev-spec`, `speed`, `size` and `size-boxy` (`roc build`, then the program is run).
   Each sequence runs `check`, two to four of the dev ones, and half the time one LLVM one.
4. A command's result is its stdout and stderr with timings and the private directories removed, its exit status, and
   for `build` the built program's output and exit status. Warm and clean must be equal at every state.
5. The cache directory is chosen with `ROC_CACHE_DIR` (and `XDG_CACHE_HOME`, for the package cache), set per run, so
   the user's real cache is never read or written (`CacheConfig.getDefaultCacheDir` in
   `src/compile/cache_config.zig`).
6. The clean run's program output is also compared with the reference. A mismatch there is not a cache finding (the
   clean build is wrong, or the generator is); the summary line names it so it can be looked at separately.

## Layout

| Path | What it is |
|---|---|
| `run.sh` | entry point |
| `gen.py` | the program generator, the reference evaluator, the emitter |
| `edit.py` | the edits |
| `driver.py` | warm and clean runs, comparison, `fuzz`, `replay`, `reduce`, `check`, `selftest`, `gen` |
| `tests/*/` | fixtures: `states/<i>/` (the files of each state, in order), `configs.txt`, and a `README.txt` saying what changes |

## Rules

`RULES.md` lists the reuse rules a disagreement would break, each tied to design.md and the compiler code. There is no
`FORMAT.md`: the model reads no compiler dump.

## Limits

- Only what commands print and what programs do is compared. A stale artifact that happens to compute the same output
  is not seen, except for `.boxy` objects under `ROC_VERIFY_BOXY_OBJECT_REUSE`.
- One compiler, one target (the host): the model never switches compiler versions, corrupts cache files, runs two
  builds on one cache at once, or builds for wasm32.
- No platform other than the built-in Echo Platform, so no hosted functions, platform/app pairings beyond the echo
  one, or effects beyond printing. No URL packages, no package depending on another package.
- The type edits only add, remove or reorder fields and tags nothing reads or builds; no edit changes a field's type or
  a tag's payload while its users stay the same.
- No `dbg`, inline `expect`, `crash`, `Dec`, floats, other integer widths, `Dict`, `Set`, abilities, `where` clauses
  or recursive types, as in backend-diff.
- `reduce` shrinks the sequence and the configs, not the programs. A finding's programs are reduced by hand.
- `roc check` output is compared byte for byte (after timings and paths are removed), so a report order that depends
  on thread scheduling would show up as `flaky`, not as agreement.

## Bugs found

None yet.
