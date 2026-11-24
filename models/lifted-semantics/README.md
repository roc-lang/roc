# Lifted semantics model

An interpreter for Monotype Lifted IR, run on the programs the compiler really builds. It decides two things. First,
SpecConstr preserves meaning: every exported program, run before and after SpecConstr on the same inputs, gives the same
values, crashes and effect traces ([RULES.md](RULES.md) S1 to S3). Second, lambda sets are sound: every closure that
reaches a value call at run time is one of the members direct LIR lowering dispatches over at that call, and every
closure's captures are bound (L1 to L3). It talks to the compiler as an oracle on real compilations, of its own randomly
generated programs and of the repository's `test/` programs.

## Requirements

- Python 3.10 or later, standard library only.
- A Debug `roc` built from this checkout (`zig build roc`). The model reads two developer diagnostics:
  `ROC_LIFTED_IR_EXPORT`, added by [#13403](https://github.com/roc-lang/roc/pull/13403), in format version 3 (literal
  values and roots), and `ROC_LAMBDA_SET_EXPORT`. Version 3 and the second variable come from [#13550](https://github.com/roc-lang/roc/pull/13550). See
  [FORMAT.md](FORMAT.md).

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
./run.sh repo 'test/cli/*.roc'
```

`ROC` selects the compiler (default: `zig-out/bin/roc` at the repository root). Output goes to `out/`. `fuzz` takes
`--seed S` (program i uses seed S+i). `./run.sh dir DIR` checks exports already in `DIR`.

## How it works

1. `drive.py` compiles each file with `roc test --opt=speed --specialize=yes` (or `roc build` for an app) with both
   exports pointed at a fresh directory. SpecConstr runs only in the optimizing inline modes, hence `--opt=speed`.
2. `check.py` pairs `<n>-after_lift.lifted` with `<n>-after_spec_constr.lifted` and runs every root of both on
   `interp.py`, on a few inputs per parameter type (none for a root with no parameters, which `expect`s and
   compile-time constants are). Different outcomes are an S1 finding. Rule violations (a capture not bound, a local read
   before it is bound, a jump out of scope) are S3 findings, attributed to the stage that first shows them.
3. `lsets.py` runs every root of each `<n>-lambda_solved.lifted` (the program Lambda Solved consumed) and checks each
   executed `call_value` against the `c` records lowering wrote for its callee (L1), and that every called closure's
   capture slots are bound (L2).
4. `drive.py` also compares the `[dbg]` lines and the failed-test count `roc test` printed with the interpreter's run of
   the after-SpecConstr test program (E1), which observes every later stage, lambda-set dispatch included, from the
   outside.
5. `gen.py` writes programs of annotated top-level functions over `I64`, `Bool`, `I64 -> I64`, `List(I64)`, a record
   holding a closure and a nominal tag union holding one, built by composing typed constructs at random: lambdas that
   capture, `if`- and `match`-selected functions, closures in records and tag payloads, curried calls, composition,
   higher-order calls, generic combinators (`apply`, `comp`, `konst`, `choose`) used at several types, fuel-bounded
   recursion, structural recursion over `Chain := [End, Link(I64 -> I64, Chain), Add(I64, Chain)]`, `Try` with `?`,
   `for` and `while` loops whose state may be a closure (with a second carried integer the result may not read),
   `List.map` and `List.fold`. Its entry functions are run from `expect { dbg entry(k); Bool.True }`, so the trace carries their values.

A finding means the compiler, the export or the interpreter is wrong. Both programs of a pair run on one interpreter, so
an interpreter choice can cause an S1 divergence only if SpecConstr depends on it; RULES.md "Semantics" lists those
choices.

### Where lambda sets are observed

design.md "Lambda Set Instances" and "Lambda Mono Decisions": a lambda set is not a property of a Lifted expression but of
a solved type read under an instance, and the dispatch a call really uses is the list of `FnVariant`s Lambda Mono gives
the callee's type in the specialization being lowered. The Lifted export carries no lambda sets, and the solved store is
not enough either (a scheme node's members live in its instantiations' images). So the judgment is observed where it is
consumed: `solved_lir_lower.zig` `valueCallStep`, which records `(callee expression, variant targets' lifted functions)`
for every value call it lowers, and the program it lowers from. That is the `ROC_LAMBDA_SET_EXPORT` hook: a field on the
lowerer, one call in `valueCallStep`, and a writer that reuses the Lifted exporter. The interpreter runs the same Lifted
program Lambda Solved consumed (after `own_captures.zig`), so expression ids agree.

## Layout

| Path | What it is |
|---|---|
| `run.sh` | entry point |
| `lifted.py` | reader for the export, format version 3 |
| `interp.py` | the interpreter: values, control flow, patterns, the low-level operations it models |
| `check.py` | S1 to S3 on a directory of after-lift and after-SpecConstr pairs |
| `lsets.py` | L1 to L3 on a directory of `lambda_solved` exports |
| `gen.py` | the program generator |
| `drive.py` | compiles files with the exports on and runs both checks |
| `selftest.py` | unit checks on hand-written exports |
| `tests/` | fixture programs for `selftest` |

## Rules and format

- [RULES.md](RULES.md): the rules the model checks, each mapped to design.md and to the compiler code that enforces it.
- [FORMAT.md](FORMAT.md): the export format the model reads, and what the export drops.

## Limits

- Only the roots are entry points, and a root with parameters runs only on the few inputs `check.py` makes for integer,
  string and list parameters. A root that takes a function is skipped.
- Low-level operations: arithmetic, comparisons, conversions between integers, common string and list operations, boxes.
  Anything else (hashing, SIMD, float parsing and printing, and others) stops that run as a model limit, counted in the
  summary. Floats are Python floats (`f32` rounded after each operation).
- Hosted functions are known only by id and return a fixed value of their type, so a platform's effects are modeled
  only as a trace of calls.
- A stored static list (`static_data_candidate` with no initializer) cannot run: the export does not write its items.
- L1 checks each call against the union of the variants of every specialization that lowered it, so a member missing
  from one instance's set while present in another's is not caught. It also cannot see calls lowering inlined away.
- `drive.py` compiles at `--opt=speed` by default, where SpecConstr runs. `OPT=dev` compiles at dev instead: SpecConstr
  does not run there (the two programs are identical, so S1 is trivial), but more value calls survive to lowering,
  which gives L1 more to check.
- E1 compares only test programs the interpreter ran completely, and compares `dbg` lines as a multiset.

## Bugs found

None. Runs on 2026-11-24 (ET) against a Debug build of `lifted-ir-export` (d80080d2c4) with the hooks above: 380
generated programs at `--opt=speed` (seeds 100, 1000 and 5000), 100 more at speed with the `Try` and loop-state constructs
(seed 20000), 80 at dev (seed 9000), and the 708 programs of `test/cli`. None gave an S1, S2, S3, L1, L2 or E1
finding. Every model mistake the runs exposed (let expressions after SpecConstr, loop fall-through, packed lists,
recursive lets, `List.replace`'s `prev` field, `Str.inspect` quoting) was fixed in the model.
