# Rows and evidence model

A Lean 4 model of two things. The first is how the checker handles tag rows (open, closed and bounded tails, `?` error-row
composition, nominal lifts) and static-dispatch evidence, and how Monotype replays both when it instantiates templates. The
second is an independent typing check over the compiler's Monotype Lifted IR. The model works as a generator: it makes Roc
programs, predicts their verdict and `expect` values, and compares with `roc check` and `roc test`. It also works as an
oracle: it checks the Lifted IR the compiler exports for real programs.

## Requirements

- Lean 4.36.0 and Lake, through elan. `lean/lean-toolchain` pins the version, and elan fetches it on first build.
  Install elan with `curl https://raw.githubusercontent.com/leanprover/elan/master/elan-init.sh -sSf | sh`, or
  `brew install elan-init` on macOS. No Lake packages are needed.
- bash and `timeout` (GNU coreutils; on macOS `brew install coreutils`).
- Python 3.10 or later, standard library only, for `./run.sh mutate`.
- A Debug `roc` built from this checkout (`zig build roc`). `selftest` and `fuzz` need nothing else. `check`, and the
  Lifted IR column of `fuzz`, need a compiler that reads `ROC_LIFTED_IR_EXPORT`. That variable comes from
  [#13403](https://github.com/roc-lang/roc/pull/13403). Without it, `fuzz` shows `-` in that column and `check` reports that no export was written.

## Quick start

```bash
./run.sh selftest
```

```bash
./run.sh check path/to/program.roc
```

```bash
./run.sh fuzz 200
```

```bash
./run.sh mutate 400 out/check/*/*.lifted
```

`ROC` selects the compiler (default: `zig-out/bin/roc` at the repository root). Output goes to `out/`. The first run builds
the Lean project (`lake build` in `lean/`), which takes under a minute.

## How it works

**Generator.** `Gen.lean` plans each definition at a random ground type and builds its body by composing small typed
constructs at random: tags, records, lists, `Try` with `?`, a user nominal with methods, a parameterized nominal, calls,
branches, matches, lambdas, records of closures, point-free aliases and self-recursion. A value of a narrower row may flow
into a wider one at any depth. `Print.lean` prints the program as a Roc module. `Eval.lean` computes each function's value at
a few arguments, and these become the program's `expect`s, along with a `Json` encode-then-parse round trip at each planned
type.

**Model verdict.** `Check.lean` re-infers the program from scratch with the rules in [RULES.md](RULES.md) part 1. It does
not trust the plan. `Replay.lean` then replays every Monotype request and runs four checks: instantiation monotonicity,
evidence completeness, unreachable-dispatch soundness and codec contracts.

**Compiler verdict.** `fuzz` runs `roc check`, then `roc test` under `--specialize=yes` and `--specialize=no`. When the
compiler has the exporter, it also checks the Lifted IR of the `--specialize=yes` run. A program agrees when the model
accepts it with a clean replay, `roc check` accepts it, both test runs pass, and the Lifted IR checks. Any other outcome is
a disagreement: a compiler false reject, a compiler panic or wrong value, a Lifted IR typing violation, or a gap in the
model. `out/fuzz/<seed>/<program>/` holds the program and every log.

**Lifted IR oracle.** `check` compiles a program with `ROC_LIFTED_IR_EXPORT` (`roc test` for a module, `roc build` for an
app; both with `--specialize=yes`) and runs `Lifted.lean` on every export ([FORMAT.md](FORMAT.md),
[RULES.md](RULES.md) part 2). A violation means the compiler, the export or the checker is wrong.

**Selftest.** `Calibrate.lean` writes known bugs as model terms. For each fixed bug, a `Cfg` toggle restores the pre-fix
rule. The current rules must pass, and the toggle must make the matching check fail. `selftest` runs that calibration,
checks the two fixtures in `tests/`, and runs three generated programs through the compiler. A Lifted IR violation on those
three programs is printed, but it does not fail the selftest, because it is a compiler finding.

## Layout

| Path | What it is |
|---|---|
| `run.sh` | entry point: `selftest`, `check`, `fuzz`, `mutate` |
| `mutate_lifted.py` | mutation test for the Lifted IR checker |
| `lean/Main.lean` | the `rows-evidence` executable: `gen`, `model`, `calibrate`, `lifted` |
| `lean/RowsEvidence/Syntax.lean` | the surface calculus: ground types, expressions, programs |
| `lean/RowsEvidence/Print.lean` | prints a program as a Roc module |
| `lean/RowsEvidence/Eval.lean` | reference evaluator for `expect` values |
| `lean/RowsEvidence/Gen.lean` | the random generator |
| `lean/RowsEvidence/Types.lean` | type store, row unification in checker and Monotype modes |
| `lean/RowsEvidence/Check.lean` | the modeled checker: inference, `?` composition, generalization |
| `lean/RowsEvidence/Replay.lean` | Monotype replay, evidence publication, codec contracts, the four checks |
| `lean/RowsEvidence/Calibrate.lean` | known bugs with the verdict the model must give |
| `lean/RowsEvidence/Lifted.lean` | Lifted IR reader and typing check |
| `tests/all-kinds.lifted` | hand-written export using every record kind; must pass |
| `tests/call-result-is-backing.lifted` | the shape of #13264; must fail T-CallProc |

## Rules and format

- [RULES.md](RULES.md): the rules the model checks, each mapped to design.md and to the compiler code that enforces it.
- [FORMAT.md](FORMAT.md): the Lifted IR export format, and what the export drops.

## Limits

- No theorems are proved. The four checks and the Lifted IR check are executable decision procedures.
- Part 1 covers a small calculus. It leaves out numeric literal settling (every literal is `U64`), field kinds, optional
  fields, tuples, opaque nominals, imports, effects, `var`, builtins, platform rows, lacks constraints and runtime layouts.
  RULES.md lists these with the bugs each one hides.
- The generator draws tag names from a fixed pool of eight. It always includes the same nominal `Nm` and the same `Box(a)`,
  and it uses a fixed set of eleven small polymorphic helpers (`idv`, `pick`, `mapok`, and so on), each instantiated
  wherever the generator calls it.
- The Lifted IR checker does not apply the iterator-tier relation, the per-use lift through an inspectable backing, or the
  builtin `Bool` and `Fields` exemptions, because the export drops the fields they read. It does not check low-level
  operations, `break`/`continue`/`jump` arity, string patterns or pre-lift forms (RULES.md, "Not checked").
- Lifted IR exists only under `--specialize=yes`, so the boxy strategy is checked only by running its tests.
- A compiler that has a bug the generator hits often gives many disagreements of one kind. For example, a compiler without
  the fix for #13264 fails T-CallProc on most programs, because every program has a `Json` round trip of a `Try`.

## Bugs found

- [#13235](https://github.com/roc-lang/roc/issues/13235): boxy lowering panics on `Json.to_str` of a tag whose payload is a
  single-tag zero-sized union (found by `fuzz`, as a `--specialize=no` panic).
- [#13236](https://github.com/roc-lang/roc/issues/13236): a parameter that shares a type with an unmatchable tag's payload is
  generalized as `[]` (found by `fuzz`: the model accepts, `roc check` rejects).
- [#13237](https://github.com/roc-lang/roc/issues/13237): a `Try(ok, _)` annotation closes the `_` error row to `[]` when the
  body joins two or more result branches (found by `fuzz`: the model accepts, `roc check` rejects).
- [#13264](https://github.com/roc-lang/roc/issues/13264): decoding a `Try` types a direct call at the backing union instead
  of the callee's nominal result (found by the Lifted IR checker, rule T-CallProc).
