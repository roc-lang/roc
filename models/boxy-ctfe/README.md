# Boxy compile-time evaluation model

A differential model of Boxy, the compile-time evaluator and its lowering (`src/postcheck/boxy/`,
`src/eval/compile_time_finalization.zig`, and the root graph in `src/postcheck/comptime_root_graph.zig`). It decides
one judgment: a value computed at compile time equals the same expression computed at runtime. It talks to the
compiler only as a generator. It writes random typed programs in which every expression appears twice, once as a
top-level constant (which the compiler must evaluate while checking) and once where no part of it can be evaluated
before the program runs, and checks that both print the same thing on every path the compiler can execute a program.

## Requirements

- Python 3.10 or later, standard library only.
- A `roc` built from this checkout (`zig build roc`). A Debug build is needed for `reach`, which reads the developer
  diagnostics `ROC_TRACE_BOXY_WORK` and `ROC_HOIST_ROOT_STATS`, and for the debug assertions; every run sets
  `ROC_VERIFY_BOXY_OBJECT_REUSE`, which a release compiler ignores. The model adds no compiler hook.

## Quick start

```bash
./run.sh selftest
```

```bash
./run.sh check tests/shapes
```

```bash
./run.sh fuzz 200
```

`ROC` selects the compiler (default: `zig-out/bin/roc` at the repository root). `JOBS` (default 4) is how many
configurations of one program run at once. Output goes to `out/`. `fuzz` takes `--seed S` (program i uses seed S+i),
`--configs a,b` (default: `test`, `run-interp`, `dev`, `dev-spec`, `speed` and `speed-boxy` for every program, plus
one of `cache-dev`, `run-dev` and `test-spec` chosen by seed), `--no-reduce`, `--keep`, and `--with-known`, which
lets the generator produce the shapes of filed bugs it otherwise leaves out (`gen.AVOID_KNOWN`). A failure whose
panic message is a filed one (`driver.KNOWN`) is reported as `known` and not reduced. A failing program is saved as `out/fuzz/<seed>/prog.pickle` with its
verdicts in `verdict.txt`, and reduced into `out/fuzz/<seed>/min/`. `reduce` takes a seed or a `prog.pickle`. `gen
SEED` prints a program, and `reach SEED` lists the compile-time roots `roc check` evaluates for it.

## How it works

1. `gen.py` builds probes. A probe is one closed expression of a random type, directed by that type and composed from
   small constructs: literals of every integer width, `F32`, `F64`, `Dec`, `Bool`, `Str` (small and heap) and `{}`;
   records, tuples, structural tag unions (payloads include `{}` and other zero-sized types), `List`, `Try`, `Box`,
   nominal types backed by a record or a tag union, and recursive nominal chains (`R := [More(T, R), Stop]`);
   `let`, `if`, `match` (on tag unions, `Try` and nominal types, including record patterns), field and tuple access,
   wrapping integer arithmetic, conversions, `Str` and `List` builtins (`List.with_capacity` among them, whose
   empty result compile-time evaluation never stores), string interpolation, `Str.inspect`, local
   closures, top-level helpers, helpers returning closures that capture their parameters, tail and non-tail
   self-recursive helpers, and reads of earlier probes. A probe's type can also be a function, or a record holding a
   function next to a data field.
2. Each probe `i` is emitted as a constant `c<i>` and a runtime function `r<i> : U64 -> T`, from the same tree:
   - In most probes (tainted), every leaf of `r<i>`'s body (a literal, `{}`, a tag without payload, an empty list) is
     written `rt(z, leaf)`, where `rt` returns its second argument behind a branch on the first. Nothing in the body
     is then closed, so nothing in it is a compile-time root, and the helpers it calls are `<name>_r` copies treated
     the same way. Reads of an earlier probe become `r<j>(z)`.
   - In the others (guarded), `r<i>` is `if z != 0 { crash "rt" } else { E }`. `E` is then a guarded hoisted root:
     evaluated at compile time by a different root kind than the constant, and restored at runtime.
3. About a third of programs put the constants, their helpers and the nominal types in a type module `Lib`, so every
   constant is read across a module boundary.
4. `main!` prints, per probe: `Str.inspect(c<i>)` (itself a compile-time root), the constant inspected at runtime, and
   `r<i>(z)` inspected at runtime, where `z` is `args.len() * 0`; and `c<i> == r<i>(z)` at runtime when the type has
   structural equality. A list probe also prints `v.concat(v).drop_first(1)` at runtime for both sides, which
   updates the stored constant (never unique, so the update copies it). A function probe is applied to two literal
   arguments at compile time, as a restored constant at runtime, and as the runtime closure; a value holding
   functions elsewhere (in a list, a tag payload, a `Try`, a tuple, a `Box`, a curried result) is shown by an observer
   that applies each function it reaches to a literal. Each typed probe also gets a top-level `expect c<i> == r<i>(0)`.
5. `driver.py` runs `roc check` first (a rejection is a generator bug, a panic or a hang is a finding), then the
   configurations (see `--configs` above for which ones a fuzz run uses): `roc test` with both `--specialize` values; `roc app.roc` on the interpreter and on dev; `roc build`
   with `--opt=dev` and `--opt=speed`, each with both `--specialize` values; and `cache-dev`, which builds twice into a
   private `ROC_CACHE_DIR` and runs the second, restored build. `ROC_VERIFY_BOXY_OBJECT_REUSE` is set for all of them.
6. A configuration fails if it panics, does not build, crashes, prints a probe whose lines disagree
   (`inconsistent`), or prints something another configuration does not (`differs`). A failure is reduced: probes are
   dropped, and expressions replaced by the smallest literal of their type or by a same-typed child, while the same
   configurations keep failing the same way.

`RULES.md` maps each check to design.md and to the compiler code.

## Layout

| Path | What it is |
|---|---|
| `run.sh` | entry point |
| `gen.py` | the generator and the emitter |
| `driver.py` | building and running every configuration, the verdicts, the reducer, `check`, `selftest` and `reach` |
| `tests/shapes/` | a fixed program in the generator's shape and its expected output |

The generator borrows its shape (typed expression trees, a driver that runs a configuration table and reduces) from
the backend-diff model; the code is new because this model compares two placements of one expression instead of
the compiler against a reference evaluator.

## Limits

- No reference evaluator: if the compile-time and runtime sides are wrong in the same way, nothing disagrees.
- Native only. The wasm32 paths of the backend-diff model need its test platform; this model's programs are
  headerless apps.
- No `Dict`, `Set`, opaque (`::`) types (`Str.inspect` prints `<opaque>`), abilities, `where` clauses, parsers or
  codecs, `dbg`, `expect` inside expressions, mutable variables, loops, early `return` or `?`, effects, or
  packages. Generic helpers are not generated.
- Arithmetic is wrapping or bounded by construction, so compile-time overflow and crash diagnostics are not
  compared, and a probe never crashes.
- Recursion counts are at most 4, or (one recursion in seven) up to 300 with a step that reads the accumulator once.
- `roc test` evaluates `expect c<i> == r<i>(0)` entirely at compile time (`r<i>(0)` is closed), so the `test`
  configurations compare the constant with a compile-time call of the runtime function, not with runtime code.
- `Str.inspect` is the comparison, so a value that inspects the same on both sides but differs in memory (capacity,
  a reference count) is not caught.

## Bugs found

- [#13423](https://github.com/roc-lang/roc/issues/13423): a zero-sized record or tuple with fields inside a literal in a function body panics
  static data materialization under `--specialize=no` ("static initializer ZST struct had runtime fields"). The
  generator leaves this shape out unless `--with-known` is given.
- [#13424](https://github.com/roc-lang/roc/issues/13424): the string-append fusion pass (`src/lir/str_append.zig`, `cloneJoinReturn`), run by
  the runtime lowering of a Boxy unit, reads a borrowed span of `LirStore.local_ids` after appending to that store,
  which panics a Debug build ("guarded list invalidated: LirStore.local_ids") under `--opt=dev`. It depends on the
  store's growth, so only some programs reach it.
