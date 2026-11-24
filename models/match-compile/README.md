# Match-compile model

A reference for what a compiled `match` does at runtime: for every value of the scrutinee's type, the
first arm in source order whose pattern matches and whose guard is true runs, with the pattern's
variables bound to the right parts of the value. It checks the decision-tree compiler
(`src/postcheck/match_tree.zig`), the two lowerers that feed it (`.boxy` in `src/postcheck/boxy/match.zig`,
`.lss` in `src/postcheck/solved_lir_lower.zig`), and every LIR pass and backend that runs the tree. It works
as a generator: it builds random types, pattern sets and values, computes the expected arm and bindings
for each value, emits a Roc program that runs the match on each value and reports any difference, and
runs that program on ten configurations. No compiler dump or hook is used.

The `exhaustive` model checks the checker's exhaustiveness and redundancy verdicts from `roc check`. This
model checks the code that runs after those verdicts.

## Requirements

- Python 3.10 or newer, standard library only.
- Node.js with WebAssembly (tested with 22) to run wasm32 output. `NODE` selects it.
- `platform/targets/wasm32/host.wasm`, the host of `test/wasm/platform`, which `zig build` writes to
  `test/wasm/platform/targets/wasm32/host.wasm`. Copy it here:
  `mkdir -p platform/targets/wasm32 && cp ../../test/wasm/platform/targets/wasm32/host.wasm platform/targets/wasm32/`.
  It is not checked in.
- A `roc` built from this checkout (`zig build roc`). A Debug build also runs the compiler's debug
  assertions (for example the ARC certifier), which catch some miscompilations before they run.

## Quick start

```bash
./run.sh selftest
```

```bash
./run.sh fuzz 100
```

```bash
./run.sh gen 7
```

`ROC` selects the compiler (default: `zig-out/bin/roc` at the repository root). `JOBS` (default 3) is how
many configurations of one program build at once. Output goes to `out/`.

`fuzz N [--seed S] [--configs a,b] [--no-reduce]` runs programs `S` to `S+N-1`, appends one line per
program to `out/fuzz/results.tsv`, keeps each disagreeing program in `out/fuzz/<seed>/` (with
`case.txt`, the model's input) and reduces it into `out/fuzz/<seed>/min/`. `check` reruns saved
programs, `reduce` shrinks one.

## How it works

1. `gen.py` builds a random type from small typed pieces: structural tag unions with payloads, `Bool`,
   `Try`, nominal tag unions (`:=` and opaque `::`), recursive nominals (`Leaf | Node(Rec, x)` and
   `Nil | One(x) | Two(Rec, Rec)`), nominals backed by a number, `Str`, tuple or record, records, tuples,
   lists, `Str`, and `U8`, `I64` and `U64`.
2. It builds one to five branches of random patterns of that type: variables and `_`, tags (bare or
   qualified), records with `..` and `..rest`, tuples, exact lists and lists with `..` or `.. as rest`
   anywhere, integer literals including extreme and negative ones, string literals, string patterns
   with one or two captures (`"a${s1}/${_}"`), nominal backing patterns `N.(p)`, and `as` at any depth.
   A quarter of the branches have two or three alternatives that bind the same names. A third have a
   guard on the match's `k : U64` argument or on one of the branch's variables. The branch list is
   completed with witnesses from the exhaustiveness model ([usefulness.py](usefulness.py), copied from
   the `exhaustive` model) or a final variable, so every match is exhaustive and most need no catch-all.
3. It picks values: two values built to match each pattern (wildcards filled at random) and twelve
   random small values, each with `k` = 0, 1 or both. `model.py:choose` computes the expected arm and
   bindings (RULES.md).
4. `program.py` emits a program with a few such units. Each unit's `match` returns `ArmI(v1, v2, ..)`
   with the branch's variables sorted by name; a list `cases` holds each value, `k` and the expected
   result; `checkI` runs the match on every case (with `k` offset by a runtime zero so nothing folds
   away) and reports each case whose result is not equal to the expected one, with the arm taken. The
   program prints `done` when every case agrees. The `test` variant adds one top-level `expect` per case.
   Nominals derive `is_eq` (`.{ is_eq : _ }`), so results can be compared with `==`.
5. `driver.py` runs `roc check` first (an error there is a generator bug, status `invalid`), then the
   configurations below, and compares each output with `done`.

| Config | Command |
|---|---|
| `interp` | `roc --opt=interpreter app.roc` |
| `test` | `roc test app.roc` (the `expect`s, dev backend) |
| `dev`, `dev-spec` | `roc build --opt=dev --specialize=no` / `yes`, then run |
| `speed`, `speed-spec` | `roc build --opt=speed --specialize=no` / `yes` (LLVM), then run |
| `wasm-dev`, `wasm-dev-spec`, `wasm-speed`, `wasm-speed-spec` | the same with `--target=wasm32` on `test/wasm/platform`, run with `run_wasm.js` |

`--specialize=no` is `.boxy` lowering and `--specialize=yes` is `.lss`, so both match-tree lowerers run.

A wrong result is reported as `wrong arm` or `wrong bindings` with the value, `k`, the expected arm and
bindings and the arm taken. A compiler panic or a runtime crash is `crash`; a failed build is `build`.

## Layout

| Path | What it is |
|---|---|
| `run.sh` | entry point |
| `model.py` | the reference: pattern matching, string captures, guards, arm choice, binder types |
| `gen.py` | type, value, pattern, guard and branch generator |
| `usefulness.py` | the `exhaustive` model's usefulness check, used to complete branch lists |
| `program.py` | units and programs, and their Roc text for each target |
| `driver.py` | configurations, comparison, reduction, `selftest`, `check` |
| `run_wasm.js` | runs a wasm32 build on Node (from the `backend-diff` model) |
| `platform/` | a copy of `test/wasm/platform` (the `host.wasm` is copied in, see Requirements) |
| `tests/*/` | saved programs for `selftest` (`case.txt`, the emitted `app.roc` for reading, and `known.txt` naming configs an open issue makes disagree): `rich` agrees everywhere and is also falsified to check that a wrong answer is caught; the others pin the open issues under "Bugs found" |

## Rules and format

- [RULES.md](RULES.md): the rules, each tied to the language reference, design.md and the compiler code.

There is no FORMAT.md, because the model reads no dump.

## Limits

- Programs are single-module apps. Opaque types are only used in their own module.
- Not generated: fractional and `Dec` literals, other integer widths, single-quote literals, `Box`,
  open unions and type variables in the scrutinee, functions in data, uninhabited payloads, matches on a
  value built in the same function (so known-tag jump threading is reached only through inlining),
  `match` in a destructuring `=`, a lambda argument or a `for` pattern.
- Guards are simple comparisons; a guard never has an effect, so the model cannot see a guard run twice
  or out of order, only its outcome.
- Values are small (lists of up to four items, recursion depth about three).
- A string capture is only ever compared through the bound `Str`, never its sharing with the source.

## Known-bug classes

`fuzz` sorts four open findings into their own buckets instead of reporting them again:

| Status | What it means |
|---|---|
| `known-#13427` | Every disagreeing config panics with "payload read of discriminant" or "published outcome discriminant", and printing every tag pattern bare (`A(..)` instead of `N.A(..)`) makes them agree. |
| `known-#13426` | Only `wasm-dev` and `wasm-dev-spec` disagree, with wrong results, and a unit has a string pattern with two captures. |
| `known-#13425` | Each wrong result is a wrong arm (not just wrong bindings), any other disagreeing config crashed with a known panic, and each such unit has two or more branches that test a string. |
| `known-#13428` | Every disagreeing config panics with "did not preserve the exact entry unit", and a branch binds a string capture or is a guarded branch with alternatives. |

The fixtures `tests/mixed-tags`, `tests/rich2`, `tests/string-retry` and `tests/escaping-capture` pin them; their `known.txt` lists the configs each one
makes disagree. Remove a class and its `known.txt` when its issue is fixed.

The classes are heuristics over the symptom: #13426, #13425 and #13428 do not prove the cause, so a class
can hide a different bug with the same symptom. Only #13427 is tested by a program transform.

## Bugs found

- [#13427](https://github.com/roc-lang/roc/issues/13427): a match that mixes bare and qualified tag patterns of a nominal union (`A(_)` and `N.A(3)`)
  tests the tag twice under `.boxy`, and the Debug ARC certifier panics on every `--specialize=no` path.
- [#13426](https://github.com/roc-lang/roc/issues/13426): on wasm32 with `--opt=dev`, a string pattern with two captures that misses on its second
  delimiter overwrites a value the next branch binds (both lowerings).
- [#13425](https://github.com/roc-lang/roc/issues/13425): when a branch whose string pattern matched fails afterwards (a false guard, or another part
  of its pattern), later branches with a different string pattern are skipped (every backend, both lowerings): the match tree treats string shapes as
  disjoint constructors.
- [#13428](https://github.com/roc-lang/roc/issues/13428): a string capture, or a variable of a guarded branch with alternatives, returned inside a tag makes the Debug ARC certifier panic on outcome
  restitution ("did not preserve the exact entry unit of parameter 0") with `--opt=dev --specialize=no`.
