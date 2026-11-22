# error-recovery model

A generator of ill-typed Roc programs, used to check the error-recovery boundary: Roc publishes modules with type errors
to post-check lowering on purpose, so every construct the checker rejects must reach lowering as explicit recovery data
(design.md "Every Rejection Is Explicit Recovery"), and no erroneous checked type may reach Monotype, Boxy or a backend.
The model builds well-typed programs by composing small typed constructs at random, injects one to three type errors at
random positions as typed mutations, and runs `roc check`, `roc test` and `roc build` in every lowering mode on each
mutant. Every command must report the errors normally, and all of them must agree that the program has errors. It reads
no compiler dump.

## Requirements

- Python 3.10 or newer, standard library only (`brew install python` or your system package).
- A Debug `roc` built from this checkout (`zig build roc`). Programs are headerless apps on the built-in default
  platform, so no platform host has to be built.
- Optional: a compiler with the seal-time error-fence check (branch `debug-check-error-fence`), run with
  `ROC_VERIFY_ERROR_FENCE=1`. Then a construct that skipped recovery fails in checking, at the construct, instead of
  crashing a later stage or passing silently.

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

`ROC` selects the compiler (default: `zig-out/bin/roc` at the repository root). Output goes to `out/`.

`fuzz [N] [--seed S] [-j J] [--variants V] [--size F] [--no-reduce] [--no-run] [--commands C,...]` runs seeds `S` to
`S+N-1` (default 100 seeds from 0) with `J` parallel jobs (default 4) and `V` mutants per program (default 2).
`--commands check,test-dev` runs only those modes, which is much faster and still finds checker and Boxy crashes. Each
command runs with `--no-cache`, so a run leaves nothing in the roc cache. `OUT` may point the run at another directory
under `out/`, to keep two runs apart.

To run against the seal-time error fence:

```bash
ROC_VERIFY_ERROR_FENCE=1 ROC=<fence worktree>/zig-out/bin/roc OUT=$PWD/out/fence ./run.sh fuzz 300 --commands check --no-run
```

## How it works

1. `gen.py` declares one or two record aliases, one or two tag-union aliases, and up to two nominal types with one or
   two methods each, then two to four top-level functions
   and constants with random signatures over `I64`, `Str`, `Bool`, `List`, tuples, the aliases, `Try(_, [Oops])` and
   function types. Each body is a random typed expression: literals, names in scope, calls, `if`, local bindings,
   tuple and record destructuring, `match` on unions, lists and integers, field and tuple access, record update,
   arithmetic and comparisons, `Str` and `List` builtins, lambdas, `var`/`for` accumulation, and `?` in functions that
   return `Try`, nominal constructors and method calls (dot and qualified). A `match` on a union reads its scrutinee
   through an annotated local, since a tag literal alone would close the union. It adds `expect`s and a `main!` that
   prints every definition. Every node records its type and scope.
2. The well-typed base program goes through `roc check` once. A base program that has errors is a generator bug and is
   kept in `out/fuzz/gen-errors/`.
3. `mutate.py` applies one to three typed mutations at random sites: an expression of another type, an unbound name, a
   call with the wrong arity, a call of a non-function, a missing method, a missing or extra record field, a tuple index
   out of range, a tag outside its union or with the wrong payload arity, a pattern of the wrong shape or type, a
   destructure of the wrong shape, a rebinding of a name in scope, `?` on a non-`Try` or outside a `Try` function, an
   effectful call in a pure definition, a non-`Bool` condition or mismatched branches, and an annotation changed at one
   position or naming an undeclared type.
4. `oracle.py` runs the mutant in seven modes (`check`; `test` at `--opt=dev`, `--opt=dev --specialize=yes` and
   `--opt=speed`; `build` at the same three) and runs every executable the builds produce. See [RULES.md](RULES.md).
5. A failing mutant is kept in `out/fuzz/fail/<seed>-<variant>/` with each command's output, and shrunk to
   `reduced.roc`: the reducer drops `expect`s, `main!` lines and definitions, and replaces subtrees that hold no
   mutation with literals of their type, while the failure keeps the same label and panic message.

A failure whose output contains the panic message of an open issue is labeled `known=issue:<number>` in
`results.tsv` (`oracle.KNOWN`); anything else is new.

`check` runs the same judgment on any `.roc` file. A fixture's `# expect:` line says what `selftest` requires of it:
`pass clean`, `pass errors`, or `fail <label>` for a known bug.

## Layout

| Path | What it is |
|---|---|
| `run.sh` | entry point |
| `main.py` | the `selftest`, `check` and `fuzz` commands, and the reducer |
| `gen.py` | types, the typed expression generator, and the Roc printer |
| `mutate.py` | the typed mutations and the site selection |
| `oracle.py` | the command matrix, output classification, and the verdict |
| `tests/clean.roc` | a well-typed program: no mode reports errors |
| `tests/errors.roc` | several common errors: every mode reports them |
| `tests/issue-13285.roc` | an open issue the model reports |
| `tests/known-*.roc`, `tests/regress-*.roc` | one program per open issue, and one per fixed issue to keep it fixed (see Bugs found) |
| `out/fuzz/results.tsv` | every mutant of the last fuzz run: seed, variant, mutations, per-command error counts, verdict |
| `out/fuzz/fail/` | one directory per failing mutant: `H.roc`, `log.txt`, `reduced.roc` |

## Rules and format

- [RULES.md](RULES.md): the rules the oracle checks, each mapped to design.md and to the compiler code that enforces it.

## Limits

- One module per program, on the default platform. Imports, packages, platform modules (where #13342 lives), hosted
  functions and type modules are not generated.
- Nominal types are single-tag wrappers of base types with methods; there are no opaque records, `where` clauses,
  custom `from_numeral`, codecs, recursion or mutual recursion. Literals are small `I64`s and short strings.
- The oracle compares only whether a program has errors. Error counts differ legitimately between commands (`roc test`
  and `roc build` also check what `roc check` does not need), so they are recorded but not compared. Diagnostic text and
  locations are not checked.
- A mutant that `roc check` accepts is labeled `accepted`, not a failure: a mutation can land where it is still well
  typed (an unannotated definition, a value only `Str.inspect` reads).
- Running an executable built from an erroneous program must end in a Roc runtime error or normally; the model does not
  check which.
- `--opt=size` and `--opt=interpreter` are not run.

## Bugs found

- [#13359](https://github.com/roc-lang/roc/issues/13359): `roc test` panics "a hosted procedure has no host binding" on
  a function whose constant block performs an effect (`tests/known-13359.roc`).
- A top-level constant reading a tuple item or field next to an unbound name, `x = (1, nope).0`, panicked every
  command in Boxy lowering. Fixed upstream by #13352 (the class of #13342) before it was filed; `tests/regress-13352.roc`
  keeps it fixed.
- Also reproduced #13285, since fixed; `tests/regress-13285.roc` keeps it fixed.
