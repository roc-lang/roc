# LIR pass model

Checks that every LIR-to-LIR pass before ARC preserves the meaning of the program. The compiler dumps each procedure
before and after every procedure-local phase (TRMC, tag-case fusion, join scalarization and pruning, loop append
promotion, range proving, box reuse) and the whole program after every stage, including the whole-program steps
(return slots, string-append fusion, tag reachability). A small reference interpreter for pre-ARC LIR runs each `pre`
and `post` on the same inputs and reports any difference. The model is an oracle on real compilations, and comes with a
generator of Roc programs aimed at these passes.

## Requirements

- Python 3.10 or later, standard library only.
- A Debug `roc` built from this checkout (`zig build roc`) that reads `ROC_LIR_PASS_DUMP`. That developer diagnostic
  (`src/lir/pass_dump.zig`) comes from [#13369](https://github.com/roc-lang/roc/pull/13369); until it merges, build `roc` from that branch.

## Quick start

```bash
./run.sh selftest
```

```bash
./run.sh check path/to/program.roc
```

```bash
./run.sh fuzz 20
```

`ROC` selects the compiler (default: `zig-out/bin/roc` at the repository root). Output goes to `out/`.

`check` takes `--rand N` (random inputs per procedure, default 6) and `--all-stages` (check every program step per
procedure, not only the whole-program ones). `fuzz` takes `--seed S` (first seed, default 1) and keeps the files of
any program with a finding in `out/fuzz/`.

## How it works

1. `roc test --no-cache --opt=speed` runs with `ROC_LIR_PASS_DUMP=out/.../X.dump`. The dump holds program blocks (every
   procedure at a stage boundary) and phase blocks (one procedure before and after one phase that changed it).
2. `check.py` parses the dump with `lir.py` and, for each lowering run:
   - runs every zero-argument procedure (the test roots) on every program snapshot and compares consecutive snapshots;
   - for each phase block, runs the program with that procedure's `pre` and then its `post` on the arguments the
     procedure was called with while the roots ran, plus random values of its argument layouts;
   - checks the tail-call structure TRMC must leave.
   Every input runs twice, with arguments unique and shared. See [RULES.md](RULES.md).
3. `fuzz` generates programs with `gen.py`, checks them as above, and also runs `opt_diff.py`, which compares the `dbg`
   output and test summary of `--opt=dev`, `--opt=speed` and `--opt=dev --specialize=yes`. That differential covers what
   the dumps do not: ARC, the later passes and the backends.

A finding means the pass changed the program's meaning, or the export or the model is wrong. All three are bugs.

## Layout

| Path | What it is |
|---|---|
| `run.sh` | entry point: `selftest`, `check`, `fuzz` |
| `lir.py` | dump parser and reference interpreter for pre-ARC LIR |
| `check.py` | the per-pass and per-stage checker; prints `FINDING` lines and a `STATS` line |
| `gen.py` | random program generator: small typed expressions and function definitions composed at random |
| `opt_diff.py` | output differential between optimization levels |
| `tests/selftest.py` | offline model tests: string patterns, a clean fixture, two broken copies of it |
| `tests/tce.roc`, `tests/tce.dump` | a tail-recursive program and part of its dump (lowered, trmc phases, after trmc) |

## Rules and format

- [RULES.md](RULES.md): the rules the model checks, each mapped to design.md and to the compiler code that has to keep it.
- [FORMAT.md](FORMAT.md): the dump format the model reads, and what the export drops.

## Limits

- Only passes before ARC are dumped. ARC, frame callables, tail drive, immortal locals and the backends are covered only
  by `opt_diff.py`, which compares outputs and not passes.
- The model is value-level: layout sizes, alignment and memory representation are invisible to it.
- Procedures that use dynamic (`.boxy`) values, erased calls, `switch_initialized_payload`, opaque static data and some
  low-level ops are unsupported, and inputs that reach them are skipped. `STATS` counts them (`pre_unsup`,
  `root_pre_unsup`; `--verbose` on `check.py` breaks them down by reason).
- Inputs are only those the test roots produce plus a few random values, so a pass bug on an input neither reaches is
  missed.
- Uniqueness is an external choice (all unique or all shared), not a refcount, so a promotion that is wrong only for
  a mix of unique and shared arguments can be missed.
- Hosted calls return values from a deterministic oracle, not from a platform.

## Bugs found

- [roc-lang/roc#13301](https://github.com/roc-lang/roc/issues/13301): a trailing empty string literal is relocated as
  if shard-local under `--opt=speed`. Found when the exporter crashed printing a procedure-rewrite shard; the exporter
  reads that string from the coordinator so it can keep printing.
- [roc-lang/roc#13302](https://github.com/roc-lang/roc/issues/13302): a string pattern with a multi-byte delimiter
  misses when the delimiter's first byte appears earlier.
