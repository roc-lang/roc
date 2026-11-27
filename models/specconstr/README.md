# SpecConstr model

A Maude model of Monotype Lifted SpecConstr (`src/postcheck/monotype_lifted/spec_constr.zig`): call-pattern workers,
known-constructor matches, inlining with the active-stack guard, case-of-case, let-of-case, loop scalarization and loop
exit selection, over a first-order fragment of Lifted IR. It decides three things about the pass: the output is well
typed and mentions no new type, the output computes what the source computes, and the worker drain terminates under
every order the coordinator could pick. It also states the code-growth behavior of case-of-case and of the
inline-productivity demand paths.

The model does not read the compiler's IR; there is no exporter. It talks to the compiler as a generator: it makes
random typed Roc programs, gets the expected results from the Maude evaluator (or, for constructs outside the
fragment, a Python evaluator), and runs `roc test` at `--opt=dev` and `--opt=speed`. SpecConstr runs only at
`--opt=speed`, so a failure that shows only there points at the optimizer.

## Requirements

- Maude 3.5.1. Download the build for your platform from https://github.com/maude-lang/Maude/releases/tag/Maude3.5.1
  (for example `Maude-3.5.1-macos-arm64.zip`), unzip it, put the `maude` binary on `PATH`, and set `MAUDE_LIB` to the
  unzipped directory (it holds `prelude.maude`). Or set `MAUDE` to a wrapper script that does both. Check with
  `maude --version`.
- Python 3.10 or later, standard library only.
- A Debug `roc` built from this checkout (`zig build roc`). It needs no `dev_diagnostics` variable. `selftest` and
  `check` need it; `fuzz --no-roc` does not.

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

`ROC` selects the compiler (default: `zig-out/bin/roc` at the repository root). `MAUDE` selects Maude (default:
`maude` on `PATH`). Output goes to `out/`.

`fuzz` takes `--seed S` (program i uses seed S+i, so a run can be repeated), `--family model|wide|both` (default
`both`, alternating), and `--no-roc` (generate and run only the Maude pass check, about 200 programs a second). It
exits nonzero when it finds anything.

`check` runs a file's `expect`s at both opt levels and reports each verdict. It does not run the Maude model.

## How it works

1. `gen.py` builds a program by choosing a construct for the type it needs and filling the construct's parts with
   fresh random constructs. Two families share this structure:
   - `model`: only constructs in the Maude fragment. Each expression is rendered as Roc and as a Maude term.
   - `wide`: closures, captures, a closure in a tag payload, `Str`, `List(I64)`, `for` loops, and function bodies that
     open with a run of non-tag lets (the shape SpecConstr's prefix split copies). Each expression carries a Python
     closure that evaluates it.
2. For `model` programs, `driver.py` writes one Maude file for the batch. Maude evaluates `main(0..4)` on the source
   (the expected results) and runs `check`: the model's SpecConstr pass, then checks (1) and (2) from RULES.md. A failed
   check is a disagreement between the model's rules and its own typing or evaluation: either the rules as written
   (design.md) are unsound on that program or the model is wrong.
3. Every program goes into one Roc file with `expect main(k) == expected`. `roc test` runs it at dev and at speed.
   If either run fails, each program is rerun alone and reported as `speed-only`, `dev-only` or `both`.
   `speed-only` points at SpecConstr or a later optimizing stage. `both` points at the generator's oracle or at a
   stage both pipelines share.
4. `tests/*.maude` are the selftest suites. Each command is a `reduce` that must be `true` or a `search` that must
   find no solution. `term.maude` searches every coordinator order for a step that does not decrease the termination
   measure and for a final program that fails a check. `calib.maude` and `growth.maude` run each known, fixed bug with
   its pre-fix rule switched back on (see "Bug flags" in RULES.md) and require that the check catches it.

## Layout

| Path | What it is |
|---|---|
| `run.sh` | entry point |
| `driver.py` | runs Maude suites, the fuzz loop, and `check` |
| `gen.py` | the two program generators and the shared Roc prelude |
| `maude/ir.maude` | IR syntax, typing judgment, the frozen type store, evaluator |
| `maude/sc.maude` | the cloner and the pass phases, with bug flags |
| `maude/pass.maude` | pass driver, checks (1) and (2), the nondeterministic drain and its termination measure |
| `maude/paths.maude` | the inline-productivity demand-path fixed point |
| `tests/*.maude` | selftest suites |
| `tests/loops.roc` | a small program for `check` in selftest |

## Rules and format

- [RULES.md](RULES.md): the rules the model checks, each mapped to design.md and to the compiler code that enforces it.

There is no FORMAT.md: the model reads no compiler dump.

## Limits

- The model never sees the compiler's SpecConstr output. It checks the rules as modeled, and it checks the compiler
  only through program results at dev and speed. A SpecConstr change that keeps results but breaks an invariant
  (a new type, worse growth) is invisible to `fuzz` unless it also crashes or changes a result.
- Not modeled: callables and captures, iterator fusion, `return`, `typed_boundary`, nominal wrappers, strings and
  lists, prefix splits, tail-recursive worker localization, record update, result crossing, `renormalizeSplitBodies`,
  `is_cold`. The `wide` family exercises several of these through `roc` only.
- Approximations: argument use is a depth-3 fixpoint, productivity is "a known constructor at a consumed parameter",
  wrapper cycles are found only through self-calls, and nested inner cases decline case-of-case. See RULES.md.
- The `'B10760` flag has no fixture that tells it apart from the current rule.
- Semantic preservation is checked on inputs 0..4 at a fixed fuel; inputs where the source runs out of fuel are skipped.

## Bugs found

The generator found these while it lived out of tree:

- [#13243](https://github.com/roc-lang/roc/issues/13243): `roc test` panics on an anonymous-recursion type error that
  `roc check` reports cleanly (closed).
- [#13244](https://github.com/roc-lang/roc/issues/13244): `roc check` panics building an unmatchable-pattern report for
  a `match` replaced by a runtime error (closed).
- [#13255](https://github.com/roc-lang/roc/issues/13255): specialized lowering drops the capture of a parenthesized
  lambda (closed).
- [#13256](https://github.com/roc-lang/roc/issues/13256): ARC emits a release for a demand-hole argument of an
  `if`-selected closure (open). `./run.sh fuzz 400 --seed 20000` still hits it: two `wide` programs panic at speed
  only with "ARC requested a refcount operation on a value holding a demand hole".
