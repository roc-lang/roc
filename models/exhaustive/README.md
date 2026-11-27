# Exhaustiveness model

A reference checker for pattern exhaustiveness, redundancy and unmatchable patterns, written in Python
from the usefulness algorithm (Maranget 2007) and the rules in design.md and the language reference. It
checks `src/check/exhaustive.zig`. It works as a generator: it builds random types and pattern sets, emits
each as a Roc program, runs `roc check` on it, and compares the compiler's warnings and errors with its
own verdict. No compiler dump is needed; the model reads the diagnostics.

## Requirements

- Python 3.10 or newer, standard library only.
- A `roc` built from this checkout (`zig build roc`). No `dev_diagnostics` variable is needed.

## Quick start

```bash
./run.sh selftest
```

```bash
./run.sh check tests/agree.roc
```

```bash
./run.sh fuzz 200
```

`ROC` selects the compiler (default: `zig-out/bin/roc` at the repository root). Output goes to `out/`.

`fuzz N [--seed S] [--ctx C] [--jobs J]` writes each case that does not agree to
`out/fuzz/<status>/c<seed>.roc`, with the compiler's output next to it in `c<seed>.txt`, and appends one line
per case to `out/fuzz/results.tsv`. It exits nonzero if any case has status `disagree` or `crash`.
`reduce <case.roc>` shrinks such a case while its status stays the same and writes `out/reduced.roc`.
`check` reruns the comparison on case files; it only reads files written by `fuzz` or `reduce` (see Limits).

## How it works

1. `gen.py` builds a random type from small typed pieces (closed and open unions, `Try`, `Bool`, nominals
   including recursive and empty ones, records, tuples, lists, `Str`, integers, `[]`, a type variable),
   then random patterns of that type. Half the time it completes the pattern set with witnesses of
   missing values from the model, so many cases sit right at the exhaustive boundary.
2. It places the patterns in one of 11 contexts: a `match` in a pure or effectful function, in a nested
   `if`, or on a call result; a destructuring `=` in a pure or effectful function or on a call result;
   a function argument; a lambda argument; a `for` pattern. It records the line and column of every
   alternative.
3. `model.py` computes the expected verdict. `compare.py` runs `roc check`, maps each `redundant pattern`
   and `unmatchable pattern` warning back to its row by line and column, and reads `non exhaustive` errors.
4. `compare.py:classify` gives each case a status:

| Status | Meaning |
|---|---|
| `ok` | The compiler and the model agree. |
| `label` | Same verdict and same useless rows, but a row is called redundant on one side and unmatchable on the other. |
| `known-<flags>` | They agree once the model emulates the known bugs named by the flags (RULES.md). |
| `known-lambda_reject` | An exhaustive lambda argument is rejected with a type mismatch (#13293). |
| `lambda-nonexh` | A non-exhaustive lambda argument is rejected with a type mismatch. This is expected. |
| `disagree` | Not explained. Either the compiler, the model or the reporting policy is wrong. |
| `invalid` | The compiler reported some other error, so the program is outside what the generator should emit. |
| `crash` | `roc check` crashed or timed out. |

A case file is the generated program followed by a comment trailer (`# exhaustive-model case`) holding the
model's input as Python literals: the context, the type, the nominals, the branches and the row positions.

## Layout

| Path | What it is |
|---|---|
| `run.sh` | entry point |
| `cli.py` | subcommands, selftest |
| `model.py` | the reference checker and the known-bug emulations |
| `gen.py` | type, pattern and branch generator, and the Roc emitter for each context |
| `compare.py` | runs `roc check`, parses its verdict, classifies, reads and writes case files |
| `reduce.py` | greedy reducer for non-ok cases |
| `tests/` | case files for selftest: two agreeing cases and one for each of #13289, #13290, #13294 |
| `RULES.md` | the rules, the reporting policy and the known-bug flags |

The compiler's own design notes for exhaustiveness are in
[CONTRIBUTING/exhaustiveness/](../../CONTRIBUTING/exhaustiveness/). The most relevant here are
[004_flex_rigid_extensions.md](../../CONTRIBUTING/exhaustiveness/004_flex_rigid_extensions.md) and
[006_flex_extension_usefulness.md](../../CONTRIBUTING/exhaustiveness/006_flex_extension_usefulness.md)
(flex tails, #13291 and #13294),
[001_wildcard_inhabitedness.md](../../CONTRIBUTING/exhaustiveness/001_wildcard_inhabitedness.md) and
[004_worklist_inhabitedness_algorithm.md](../../CONTRIBUTING/exhaustiveness/004_worklist_inhabitedness_algorithm.md)
(rule INHAB).

## Rules and format

- [RULES.md](RULES.md): the rules the model checks, each mapped to design.md and to the compiler code that enforces it.

There is no FORMAT.md, because the model reads no dump.

## Limits

- `check` cannot read an arbitrary Roc program. There is no exporter, so the model's input (the type and
  the patterns) is known only for programs the generator wrote.
- The comparison drops the text of each diagnostic, including the missing patterns a `non exhaustive`
  error lists, and any warning other than redundant and unmatchable. Bugs in that text, like #13295, are
  not caught.
- A small share of fuzz cases (about 1%) end as `disagree`. So far they are unmatchable rows the compiler
  does not flag, or flags as redundant, mostly on a scrutinee that is itself uninhabited (the #13290
  family) or when combined with a guard. The compiler's unmatchable test (`isSketchedPatternInhabited`)
  looks less deep than rule UNM.
- Not generated: string patterns with captures, fractional literals, literals spelled two ways (`1` and
  `0x1`), optional record fields, `Box`, opaque types from other modules, finite number domains (every
  `U8` literal), top-level destructures, and nested `|` alternatives (Roc allows `|` only at branch level).
- A destructuring statement cannot start with a list pattern, a bare tag or a literal: `[.. as all] = xs`,
  `Unit = u` and `1 = x` do not parse. Whether that is intended is an open question. The generator uses a
  wildcard in those positions instead.
- The type variable is not generated for call results, lambda arguments, `for` loops or nested `if`, and
  the rigid `..` is not generated for lambda arguments. On a call result every structural union is
  closed or flex (rule POL), never rigid open.

## Bugs found

- [#13289](https://github.com/roc-lang/roc/issues/13289) a constructor with an uninhabited payload switches off the checking of its siblings' payloads
- [#13290](https://github.com/roc-lang/roc/issues/13290) matching on an uninhabited type: `_` is unmatchable and the match is non-exhaustive
- [#13291](https://github.com/roc-lang/roc/issues/13291) non-exhaustive match on an implicitly open union is accepted when the missing value is in a payload
- [#13292](https://github.com/roc-lang/roc/issues/13292) a guarded match is accepted as exhaustive when the scrutinee's first tag is uninhabited
- [#13293](https://github.com/roc-lang/roc/issues/13293) a lambda argument that is exhaustive only because a constructor is uninhabited is rejected
- [#13294](https://github.com/roc-lang/roc/issues/13294) redundant branches are never reported on an implicitly open union
- [#13295](https://github.com/roc-lang/roc/issues/13295) the missing-pattern report shows the internal `#others` field (seen in the output, not checked by the model)
