# Backend-diff model

A differential model of every way the compiler can execute a program split across several modules. It decides one
judgment: every execution path prints what a Python reference evaluator computes for the program. The paths are the
interpreter, the dev backend (in process and as a built executable, with boxy lowering and with lambda-set
specialization), LLVM (`--opt=speed`, and `--opt=size` with boxy lowering), and wasm32 (dev with both lowerings, and
LLVM). It talks to the compiler only as a generator: it writes random typed programs, builds and runs them, and
compares outputs. Every other generator in `models/` writes a single module; this one exists because cross-module
calls are compiled per unit (one unit per module under boxy lowering), and #13340 and #13356 were both bugs at the
boundary between units.

## Requirements

- Python 3.10 or later, standard library only.
- Node.js (any version with WebAssembly; tested with 22) to run wasm32 output. `NODE` selects it.
- `platform/targets/wasm32/host.wasm`: the host of `test/wasm/platform`, which `zig build` writes to
  `test/wasm/platform/targets/wasm32/host.wasm` as a test fixture. Copy it here:
  `cp ../../test/wasm/platform/targets/wasm32/host.wasm platform/targets/wasm32/`. It is not checked in.
- A `roc` built from this checkout (`zig build roc`). No `dev_diagnostics` variable is needed, so a release build works
  too.

## Quick start

```bash
./run.sh selftest
```

```bash
./run.sh check tests/cross-closures
```

```bash
./run.sh fuzz 200
```

`ROC` selects the compiler (default: `zig-out/bin/roc` at the repository root). `JOBS` (default 4) is how many paths
of one program build at once. Output goes to `out/`. A program takes about 30 seconds on a Debug compiler, most of it
the wasm and LLVM builds; `--configs run-interp,dev,wasm-dev` picks a subset.

`fuzz` takes `--seed S` (program i uses seed S+i), `--no-reduce`, `--keep` (keep the files of agreeing programs), and
`--wrap`, which applies the #13356 wrapper to every program before running it, so #13356 cannot hide another `wasm-dev`
bug. Each disagreement's program is saved as `out/fuzz/<seed>/prog.pickle`; `reduce` takes that path, so a finding
can be reduced after the generator changes.
It exits nonzero when it finds a new disagreement. `gen SEED` prints a program, its expected output, and its
cross-module calls into tail groups. `reduce SEED` regenerates a program and reduces it.

## How it works

1. `gen.py` builds a program from small typed constructs: plain functions; functions returning closures that capture
   their parameters; higher-order functions; self tail loops; non-tail recursion; groups of two or three functions in a
   cycle of tail calls (sometimes choosing between two members); a generic parameter passed through every recursive
   call (the shape of #13340); polymorphic functions over `a` with an `a -> a` parameter, whose bodies apply it, build
   and fold `List(a)`, and call other polymorphic functions, instantiated at each concrete type by their callers; and
   tail loops and groups whose accumulator is `a`. A top-level `I64 -> I64` function can also be passed as a value. Bodies compose `I64`, `Str`, `Bool`, `List(I64)`, a record `{ a : I64, b : Str }`, the tag
   union `[P(I64), Q(Str), W]` and `I64 -> I64` closures through arithmetic, `rem_by`, `if`, blocks, `match`, record
   fields, `map`, `keep_if`, `fold`, `sum`, `append`, `concat`, `count_utf8_bytes`, `to_str` and `join_with`.
2. Each definition is placed in one of 2 to 5 modules (the last is the app). A call may go only to a module at or
   below the caller's, so imports form a DAG; a tail group stays in one module. Calls prefer other modules and recursive
   definitions, so most programs call a tail group from another module. Library modules are type modules
   (`M :: [].{ ... }`) or, sometimes, deprecated `module [...]` headers. Every definition is reachable from `main!`.
3. `main!` binds each output to an annotated local and prints one line per output. Recursion counts and the values
   they meet are offset by a runtime zero (`args.len() * 0` natively, the host's JSON input length `* 0` on wasm), so
   the builds cannot fold the calls away.
4. `gen.evaluate` is the reference. A program whose reference leaves `I64` range, underflows a `U64`, or runs too
   long is discarded before anything is built, so every compared program has one correct output.
5. `driver.py` first runs `roc check` (a generator bug, not a finding, if it reports an error), then builds and runs
   the program on every path and compares stdout with the reference. Native programs are headerless apps that print
   with `echo!`. Wasm programs are the same modules on a copy of `test/wasm/platform` (`platform/`); `run_wasm.js`
   prints the `Str` that `wasm_main` returns and exits 3 on `roc_panic`.
6. A disagreement is classified. `13356` means only `wasm-dev` (dev, boxy lowering) disagrees, the program calls a
   recursive function from another module, and routing each such call through a plain wrapper defined beside it
   (`gen.wrap_cross_group_calls`, so no importer calls a recursive function directly) makes `wasm-dev` agree. Non-tail
   recursion counts too, because a later pass can turn its call into a tail call (`tests/cross-self-recursion`). Anything else is
   `new` and is reduced: drop outputs, move definitions between modules, switch header modules to type modules, and
   replace expressions with literals or same-typed children, keeping every disagreeing path's verdict and one
   agreeing path as a control. The minimized program lands in `out/fuzz/<seed>/min/`.

## Layout

| Path | What it is |
|---|---|
| `run.sh` | entry point |
| `gen.py` | the generator, the reference evaluator, the emitter, the #13356 wrapper transform |
| `driver.py` | building and running every path, classification, reduction, `check` and `selftest` |
| `run_wasm.js` | runs a wasm32 build on Node |
| `platform/` | a copy of `test/wasm/platform` (the `host.wasm` is copied in, see Requirements) |
| `tests/*/` | fixtures (each one's `app.roc` says what it covers): `app.roc` and modules (native), `wasm/` (the same program on the wasm platform), `expected.txt`, and `known.txt` naming paths an open issue makes disagree |

## Rules and format

There is no `RULES.md` or `FORMAT.md`: the model reads no compiler dump, and its one rule is that every path computes
what the reference computes.

## Limits

- No floats, `Dec`, other integer widths, `Dict`, `Set`, effects beyond printing, `expect`, nested modules, packages,
  opaque or nominal types, abilities or `where` clauses, or user-defined recursive types.
- Arithmetic stays in range by construction; overflow and crash behavior are never compared.
- Generic parameters are only passed through; no generic function uses its type variable.
- The wasm path needs the test host; when it is missing, `run.sh` stops.
- While #13356 is open it masks every other `wasm-dev` disagreement in a program that calls a recursive function across
  modules: the classifier checks only that the wrapper makes `wasm-dev` agree. Run `fuzz --wrap` to look past it.

## Bugs found

- Reproduces #13356 in about half of all programs. Two more shapes of it, both removed by the wrapper:
  - a generic tail group (`tests/cross-generic-tail-group`) fails with a `roc_panic`, "cast causes pointer to be null",
    instead of a wrong result;
  - a self-recursive function whose call becomes a tail call only after an unused binding is dropped
    (`tests/cross-self-recursion`) traps with "null function or function signature mismatch".
