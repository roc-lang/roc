# infer model

A reference type inference for a small core of Roc, written in Python, used as a differential oracle for the type checker.
For each module it decides whether `roc check` should accept it and what principal type each generalized top-level
definition should have, printed the way the compiler's type printer prints it. It generates modules by composing small typed
constructs at random, plus annotated variants of each, and compares its answers with `roc check` and REPL `:t`. It reads no
compiler dump.

## Requirements

- Python 3.10 or newer, standard library only (`brew install python` or your system package).
- A Debug `roc` built from this checkout (`zig build roc`). The model only runs `roc check` and `roc repl`, so it needs no
  `dev_diagnostics` variable.

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

`fuzz [N] [--seed S] [-j J]` runs seeds `S` to `S+N-1` (default 100 seeds from 0) with `J` parallel jobs (default 4).

## How it works

1. `gen.py` builds a module: optionally the effectful primitive `e!`, a few top-level values, one to three library functions
   built from random typed expressions, and use sites whose arguments are built from the model's type of the library function.
2. `diff.py` runs the model (`lang.py` over `ref.py`) and `roc check` on the module. When both accept, it feeds the
   definitions to `roc repl` and compares each `:t` with the model's printed type, up to renaming.
3. For a module the model accepts, `fuzz.py` derives variants that change only annotations: the model's own principal
   annotation, that annotation with one or two subterms replaced by `_`, local lambdas annotated with names shared with the
   outer annotation, and mutations (close or open a row, add or drop a tag, make a variable concrete, merge two variables).
   Each variant is compared the same way.
4. Every disagreement is labeled with the open issue it matches, or `unclassified`. A disagreement means the compiler, the
   model, or design.md is wrong.

`check` parses `.roc` files written in the fragment (see [RULES.md](RULES.md)) and compares them the same way. No program in
the repository is inside the fragment, so `check` is for fuzz output, the fixtures, and hand-written cases. A file may hold
several modules: each `# === label` line starts a new one under the file's header, and `# expect: agree` or
`# expect: disagree <issue URL>` in a section is what `selftest` requires of it.

`selftest` first checks, without the compiler, that the parser reads back every generated module and variant from 300 seeds
and that the model raises nothing but type errors on them. Then it runs the fixtures in `tests/` against the compiler.

## Layout

| Path | What it is |
|---|---|
| `run.sh` | entry point |
| `main.py` | the `selftest`, `check` and `fuzz` commands |
| `ref.py` | types, unification with levels, rows, effect kinds, the type printer |
| `lang.py` | the fragment's AST, its Roc printer, and the inference judgment |
| `gen.py` | the generator, and the conversion from a model type to an annotation |
| `parse.py` | the parser for fragment `.roc` files |
| `diff.py` | one comparison of the model with the compiler, and the issue labels |
| `fuzz.py` | annotation variants and the fuzz loop |
| `roc.py` | runs `roc check` and `roc repl`; type canonicalization |
| `tests/types.roc` | principal types of small functions |
| `tests/effects.roc` | effect-slot verdicts |
| `tests/value-rows.roc` | annotated and unannotated local values (see Limits) |
| `tests/known-bugs.roc` | one program per open issue the model reports |
| `out/fuzz/results.tsv` | every comparison of the last fuzz run: seed, variant, kind, model, compiler, model error, label |
| `out/fuzz/disagree/` | one `.roc` file per disagreement, ready for `./run.sh check` |

## Rules and format

- [RULES.md](RULES.md): the fragment, the rules the model checks, each mapped to design.md and to the compiler code that
  enforces it, and the printing convention.

## Limits

- Outside the fragment: `Str` literals (they are `from_quote`-polymorphic and print `where` clauses), number literals without
  a suffix, `==` and static dispatch, user nominal types, record update, `return`, recursion, imports, and every effectful
  function other than `e!`.
- A rejection caused by a match on a scrutinee known at compile time is reported as `uncertain` and not compared
  (R-KnownScrutinee): the compiler checks such a match against the value, which the model does not evaluate.
- Only the types of generalized top-level definitions are compared. Weak values' types, local types and error messages are
  not. A REPL that prints no type for a name is counted as skipped.
- The model prints an instantiated effect kind with unresolved dependencies as `->`, as the compiler does, so it cannot catch
  [#13299](https://github.com/roc-lang/roc/issues/13299).
- design.md seems to say two things about annotated local values. The table in "Polarity: Output-Position Tag Unions Are
  Implicitly Open" says a LOCAL VALUE binding's opened row is one weak flex that uses may widen. "Value Rows: Uses Share One
  Row" says an annotated value's rows are the annotation's and no use widens them. The model implements the "Value Rows"
  reading (R-ValueRowBound), which is what the compiler does today (`tests/value-rows.roc`). If design.md settles on the other
  reading, the model and that fixture change.
- The compiler sites for R-MatchClose and R-KnownScrutinee are not traced in RULES.md.

## Bugs found

- [#13296](https://github.com/roc-lang/roc/issues/13296): `roc check` panics in Boxy lowering ("erroneous checked type") when
  a matched call binds a row extension to a non-row type.
- [#13297](https://github.com/roc-lang/roc/issues/13297): an annotated output row bound is bypassed by forwarding an input
  `..` row: `[B, ..] -> [B]` accepts `|x| x`.
- [#13298](https://github.com/roc-lang/roc/issues/13298): an annotation's rigid type variable escapes into a top-level weak
  value; a later, unrelated use is then rejected.
- [#13299](https://github.com/roc-lang/roc/issues/13299): the type printer shows a function whose effect depends on its
  arguments as pure (`->`). Found while calibrating the effect rules against REPL `:t`; see Limits.
- [#13300](https://github.com/roc-lang/roc/issues/13300): the type printer gives two different type variables the same name
  (`Try(ok, err), Try(ok, err)`).
