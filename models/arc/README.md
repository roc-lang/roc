# ARC model

A Lean model of reference counting in final LIR: ARC insertion, `unique_args` stamping, in-place reuse and dismantling.
It reads the LIR the compiler prints with `ROC_LIR_DUMP` and decides three things about it. A heap machine runs every
test root on a refcounted heap and reports use after free, double free, leaks, in-place writes to shared cells, and reads
of a cell after it was reused in place. A certifier judgment restates the compiler's debug borrow certifier
(`src/lir/arc_certify.zig`) independently. R-Equiv checks that turning in-place reuse off does not change any root's
result. The model is used as an oracle on real compilations (`check`, `corpus`) and on generated programs (`fuzz`).

## Requirements

- Lean 4.36.0, through elan, which reads `lean/lean-toolchain` and installs that version on first build:
  `curl https://raw.githubusercontent.com/leanprover/elan/master/elan-init.sh -sSf | sh` (or `brew install elan-init`).
- Python 3.10 or later, standard library only (the generator, `gen.py`).
- A Debug `roc` built from this checkout (`zig build roc`). The model reads `ROC_LIR_DUMP`, and `run.sh` also sets
  `ROC_VERIFY_ARC`, so a release build made with `-Ddev-diagnostics` works too. Both are upstream.
- Zig, only for `./run.sh optable`.

## Quick start

```bash
./run.sh selftest
```

```bash
./run.sh check path/to/program.roc
```

```bash
./run.sh fuzz 20 --seed 1
```

```bash
./run.sh corpus test/cli
```

`ROC` selects the compiler (default: `zig-out/bin/roc` at the repository root). Output goes to `out/`. Every command
builds the Lean project first (`lake build`, a few seconds after the first time). The build prints three `declaration
uses 'sorry'` warnings; they are the three unproven theorems listed in RULES.md.

`selftest` takes a few seconds. `fuzz` takes 10 to 20 seconds per program, most of it in the heap machine.

## How it works

1. `roc test --no-cache` runs with `ROC_LIR_DUMP=` (empty: every procedure) at `--specialize=yes` and at `--opt=speed`.
   Those two lower almost every program without `.boxy` dynamic values, which the model does not interpret.
2. `rocarc` (`lean/Main.lean`) parses the dump ([FORMAT.md](FORMAT.md)), splits it into programs, and runs:
   - `rocarc oracle DUMP`: the heap machine on every parameterless root, exploring both sides of unknown branches up to
     a fork limit. Stamped in-place ops write in place unconditionally, as the backends do.
   - `rocarc oracle DUMP --checked`: the same, with every stamp replaced by its runtime check.
   - `rocarc equiv DUMP`: the stamped run against the R-Pure run, where nothing is written in place and nothing freed.
   - `rocarc certify DUMP`: the certifier judgment on every procedure.
3. `fuzz` generates a program with `gen.py`, which also computes every name's value in Python and ends the program with
   `expect`s comparing them. It runs the program at `--opt=interpreter`, dev, `--specialize=yes` and `--opt=speed`, then
   the model on the two dumps.

What a report means:

- A machine report (USE-AFTER-FREE, DOUBLE-FREE, LEAK, SHARED-MUTATION, STALE-REUSE, VALUE-MISMATCH) means the compiler
  emitted wrong counts or an unsafe in-place write, or the dump or the model is wrong. All three are bugs.
- A compiler panic or a failed generated `expect` is a compiler bug. A Debug compiler runs its own ARC certifier on every
  compilation, so a certifier abort shows up here.
- A `certify:` line is a disagreement with the compiler's certifier, which accepted the same program. One of the two
  certifiers is wrong, and so far it has mostly been the model (see Limits). `check` and `fuzz` print these lines but do
  not count them as findings.

`./run.sh optable` regenerates `lean/RocArc/OpTable.lean`, the `RcEffect` row of every low-level op, from
`src/base/LowLevel.zig` with `tools/optable.zig`. Run it, then `./run.sh selftest`, whenever `LowLevel.zig` changes.
It runs `zig run` on the tool with `src/base/LowLevel.zig` as its `lowlevel` module and needs no build of the compiler.

## Layout

| Path | What it is |
|---|---|
| `run.sh` | entry point |
| `gen.py` | the generator: small typed constructs composed at random, with reference values |
| `lean/Main.lean` | the `rocarc` command line |
| `lean/RocArc/Syntax.lean` | LIR as the model sees it |
| `lean/RocArc/Parse.lean` | the dump parser (the abstraction function) |
| `lean/RocArc/OpTable.lean` | generated: every op's `RcEffect` row |
| `lean/RocArc/Machine.lean` | the heap machine, R-Reuse, R-Pure and R-Equiv |
| `lean/RocArc/Certify.lean` | the certifier judgment |
| `lean/RocArc/Theorems.lean` | soundness statements and two proven heap lemmas |
| `tools/optable.zig` | prints `OpTable.lean` from `src/base/LowLevel.zig` |
| `tests/` | hand-written dumps pinning each judgment's accepted and rejected side, and one small Roc program |

## Rules and format

- [RULES.md](RULES.md): the rules the model checks, each mapped to design.md and to the compiler code that enforces it,
  and the theorems.
- [FORMAT.md](FORMAT.md): the dump format the model reads, and what the export drops.

## Limits

- Everything FORMAT.md lists as dropped. In particular each statement is modeled with its op's default `RcEffect`
  row, string and float values are unknown, and erased calls and `.boxy` dynamic values are not interpreted.
- The dump is post-ARC. A rewrite that makes a read name a different value, still with balanced counts (the class of
  [#13279](https://github.com/roc-lang/roc/issues/13279)), gives LIR that the machine, R-Pure and the certifier all
  accept. Only `fuzz`'s comparison of results across modes and with the generator's values catches it.
- R-Equiv compares structure, not strings (strings are unknown scalars). It compares a root only when the stamped and
  R-Pure runs finish the same number of paths, which unknown branches can prevent.
- Exploration is bounded by steps, paths and forks per path. After the fork limit the machine takes the first branch,
  so some parser-heavy programs report "read of unbound local" paths as stuck.
- `box_prepare_update` and the `ptr_*` ops follow their generic rows; no generated or corpus program has reached them.
- The certifier model enumerates join entry states where the compiler's certifier merges them, so it gives up on some
  procedures (`GAVE-UP`). It also rejects some programs the compiler's certifier accepts: births of uniqueness through
  Dict internals are not fully followed, the remaining slots of a residual container are not balanced, and a struct or
  tag layout no RC helper names is treated as not refcounted (FORMAT.md G2). Dumps from compilers older than
  `borrowed=` in the header make every parameter look owned.
- Theorems: `certifier_sound`, `unique_args_equiv` and `Reuse.reuse_sound` are stated with `sorry`. They fix what each
  run checks, not a proof that it is enough.
- `gen.py` produces `expect`-only modules; it does not exercise platforms, effects or hosted calls.

## Bugs found

Found by running the repository's own `test/cli` modules through `check` at `--specialize=yes` and `--opt=speed`, modes CI
does not run them in:

- [#13239](https://github.com/roc-lang/roc/issues/13239) (closed): Debug borrow certifier aborts on derived `is_eq` of a recursive nominal tag union under `--specialize=yes`. The heap machine runs the same LIR without a violation, which showed the certifier, not ARC, was wrong.
- [#13240](https://github.com/roc-lang/roc/issues/13240) (closed): SpecConstr panics at `--opt=speed` on `parse_tag_union` tests
- [#13241](https://github.com/roc-lang/roc/issues/13241) (closed): `roc test --specialize=yes` panics instead of reporting the type error in `ParserMismatchedSpecState.roc`
- [#13242](https://github.com/roc-lang/roc/issues/13242) (closed): `boxy_stored_scoped_captures` panics under `--specialize=yes`

Found by `fuzz`:

- [#13303](https://github.com/roc-lang/roc/issues/13303) (open): ARC moves a record field's tag into an owned call, then reads it again through a dismantle alias (use after free)
- [#13304](https://github.com/roc-lang/roc/issues/13304) (open): ARC certifier rejects a valid `--opt=speed` program (state from an infeasible switch arm flows through a join)
- [#13305](https://github.com/roc-lang/roc/issues/13305) (open): ARC certifier rejects a valid program when a record field is read twice, borrowed and owned, around joins

All seven surfaced as compiler panics or certifier aborts during the runs. The heap machine has reported no violation on
a dump from a current compiler. On older compilers it catches the fixed bugs
[#11830](https://github.com/roc-lang/roc/issues/11830) (USE-AFTER-FREE), [#12221](https://github.com/roc-lang/roc/issues/12221)
(SHARED-MUTATION on static data, stamped mode only), [#12375](https://github.com/roc-lang/roc/issues/12375) (DOUBLE-FREE)
and [#12436](https://github.com/roc-lang/roc/issues/12436) (USE-AFTER-FREE, on a dump from the pre-fix compiler).
