# Range prover model

A translation validator for the LIR range prover (`src/lir/range_prove.zig`). Each proof round removes checks: a comparison
becomes a constant, a checked add becomes `*_proven_cannot_overflow`, a checked division becomes plain, a switch is folded.
The compiler dumps each round's procedure as the round found it, together with the rewrites it made. The model then
re-proves every rewrite independently with Z3. It is an oracle on real compilations, and it comes with a generator of Roc
programs aimed at the prover's fact sources. Generated programs are also built at `--opt=dev` and `--opt=speed` and their
output compared.

## Requirements

- Python 3.10 or newer.
- Z3's Python bindings (tested with 5.1.0): `pip install z3-solver`.
- A Debug `roc` built from this checkout (`zig build roc`) that reads `ROC_RANGE_PROVE_DUMP`
  (`base.dev_diagnostics.Var.range_prove_dump`). The exporter comes from [#13362](https://github.com/roc-lang/roc/pull/13362); until it merges, build `roc` from that branch.
- For `fuzz` and the end-to-end part of `selftest`: the `test/fx` platform with its host built for your target (a full
  `zig build` builds it). `FX_PLATFORM` points at another copy of that platform's `main.roc`.

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
./run.sh fuzz 20 --domain u8 --seed 100
```

```bash
./run.sh prove saved-stderr.txt
```

`ROC` selects the compiler (default: `zig-out/bin/roc` at the repository root). Output goes to `out/`. `check` takes apps
(files with an `app` header), because it builds them.

A `u8` program takes about 15 seconds. A `general` program takes from seconds to over ten minutes, mostly in the havoc
pass, so start with small counts.

## How it works

1. `driver.py` builds the program at `--opt=speed` with `ROC_RANGE_PROVE_DUMP=1` and cuts the dump blocks out of stderr
   ([FORMAT.md](FORMAT.md)).
2. `rpv.py` parses each block into the procedure and its rewrites. It walks the procedure symbolically: every local is a Z3
   bit-vector of its real width, lists carry a length, and every path is followed with its path condition. Each loop is
   unrolled up to 3 times. When the walk reaches a rewritten statement, it asks Z3 whether the rewrite's claim can be false
   there ([RULES.md](RULES.md)). A satisfiable query is a countermodel: concrete inputs on which the compiler removed a
   check it needed.
3. Rewrites that no path reached within the bound get a second pass that havocs loop state at the bound, so later
   iterations are covered.
4. `fuzz` also runs the dev and speed builds on the generator's inputs and compares exit code and stdout. A difference there
   is a miscompilation whether or not the range prover caused it.

A countermodel means one of three things is wrong: the pass, the export, or the model. RULES.md lists the model and export
mistakes found so far.

### The generator

`gen.py` composes small typed expressions and statements at random, per type: unsigned arithmetic (checked, wrapping,
saturating, `*_try`, `*_overflows`), signed wrapping, division and masks converted back to unsigned, masks and shifts,
comparisons feeding `if`, list updates with known length effects, counted loops, and `List.get` at derived indices. Each
function guards its body with a recursive call no input reaches, so it stays its own procedure and its arguments stay
unknown to the prover. There are two domains:

- `general` (default): functions over `List(U64)`, `List(U8)` and three `U64`s. The program reads one input line and the
  runner feeds it 8 lines.
- `u8` and `u16`: functions over three U8 (or U16) values using only operations that cannot crash. `main` loops over
  every (a, b) pair for U8, or a stride-7 grid up to 4095 for U16, and prints one checksum per function.

## Layout

| Path | What it is |
|---|---|
| `run.sh` | entry point |
| `rpv.py` | the validator: dump parser, symbolic executor, claim checks; also a CLI (`./run.sh prove`) |
| `gen.py` | program generator, both domains |
| `driver.py` | builds, runs and compares programs for `check` and `fuzz` |
| `selftest.py` | offline checks on the fixtures |
| `tests/basic.roc` | small program the fixture was dumped from |
| `tests/basic.dump` | its dump: 13 rewrites of 5 kinds, all valid. `selftest` also falsifies each one and expects a countermodel. |

## Rules and format

- [RULES.md](RULES.md): the rules the model checks, each mapped to design.md and to the compiler code that enforces it.
- [FORMAT.md](FORMAT.md): the dump format the model reads, and what the export drops.

## Limits

Everything here is something the model cannot catch.

- **Calls.** Every call is havoc: its result is an unknown value of its layout. The model never assumes anything about
  what a callee returns, so a rewrite that is valid only because of a callee's result would get a spurious countermodel
  (none has). Callees are checked as procedures of their own.
- **Strings and floats.** String contents and float literals are dropped by the export and unknown to the model.
- **Signed and 128-bit values.** The model's semantics cover them, but the generator only produces unsigned arithmetic plus
  signed values converted back to unsigned, and `chain` folds are only checked as unsigned. 128-bit arithmetic is never
  generated and is slow in Z3.
- **Loop-threading rounds.** A round that threads Bool joins does not walk and is not dumped, so its restructuring is never
  checked. Only the walk rounds that follow it are.
- **The bound.** Bounded execution checks paths with up to 3 iterations of each loop. Havoc mode covers more but forgets
  loop invariants, so it is only used where the bounded pass found no path. Rewrites still `unreached` after both passes
  are not checked.
- **`ok` includes vacuous.** A rewrite on a statement only infeasible paths reach is reported `ok`.
- **Big programs.** Each round has a 60-second budget (`check` and `fuzz`: 30), and exhausted rounds are reported as
  `budget`. Very large programs, such as `test/repros/issue-13056_main.roc`, time out.

## Bugs found

None. The model re-proved about 2,830 rewrites from real programs (`test/fx`, `test/repros`, `test/int`, `test/str`,
`test/json-decoder`, `test/perf`) and about 3,543 from generated programs, with no countermodel and no dev/speed
difference. Those runs cut dumps out of stderr by lines, so a block that started after a warning's partial line was lost;
for generated programs with unused-variable warnings, that was usually the first block of the build. The current driver cuts
blocks by their markers.
