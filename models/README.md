# Compiler models

Small executable models of parts of the Roc compiler, used to find compiler bugs. Each model states one judgment about one
stage, for example "this match is exhaustive" or "every refcount operation in this LIR is balanced". It then checks the
compiler against that judgment, either on real compilations or on programs it generates.

None of this runs in CI or `minici`. Each model is run by hand, from its own directory, with `./run.sh`.

## The models

| Model | Stage it checks | How | Toolchain | Compiler hook |
|---|---|---|---|---|
| [arc](arc/) | ARC insertion, reuse and dismantle in LIR; the debug ARC certifier | oracle on LIR dumps, generator | Lean 4, Python | `ROC_LIR_DUMP`, `ROC_VERIFY_ARC` |
| [exhaustive](exhaustive/) | match exhaustiveness and redundancy in the checker | generator, differential against `roc check` | Python | none |

Every hook is a developer diagnostic (`base.dev_diagnostics`), so the models need a Debug `roc` (`zig build roc`) or one built
with `-Ddev-diagnostics`.

## Using a model

Every model directory has the same shape:

| Path | What it is |
|---|---|
| `README.md` | what the model checks, what it needs installed, how to run it, its limits, the bugs it found |
| `RULES.md` | the rules it checks, each tied to its design.md section and to the compiler code that enforces it |
| `FORMAT.md` | the compiler dump it reads, and what the dump leaves out (only for models that read one) |
| `run.sh` | the entry point |
| `tests/` | small fixtures for `selftest` |

And the same commands:

```bash
./run.sh selftest
```

```bash
./run.sh check path/to/program.roc
```

```bash
./run.sh fuzz 200
```

`ROC` picks the compiler (default `zig-out/bin/roc` at the repository root). Everything a run writes goes to the model's
`out/` directory, which git ignores. Models that build apps against `test/fx/platform` need its host built first; their
READMEs say so.

## How a model connects to the compiler

There is no tool that checks Zig against Lean or Maude, so each model is tied to the compiler by explicit links:

1. **One judgment, two implementations.** The model is the executable reference. Where the compiler has a matching debug
   check, both use the same rule names, listed in the model's `RULES.md`.
2. **An export as the abstraction.** The model never sees Zig data structures. A debug-only exporter prints the IR at a
   stage boundary in a small line format, and the model parses it. `FORMAT.md` lists what the export drops, because the
   model cannot catch anything it does not see.
3. **Three ways to find a bug.**
   - *Oracle:* compile real programs with the exporter on and run the model on every dump. A rejection means the compiler,
     the export or the model is wrong; all three are worth fixing.
   - *Generator:* generate small well-typed programs by composing typed constructs at random, then compare the compiler's
     verdict or output with the model's.
   - *Proof failure:* if a theorem about the rules will not go through, the stuck goal is the shape of a counterexample.

A model checks the compilations it is run on. It does not prove the compiler correct.

## Adding a model

Copy [TEMPLATE.md](TEMPLATE.md) to `<model>/README.md`, follow the layout above, and keep generated output in `out/`.
Generators must follow the fuzzer rule in AGENTS.md: compose small typed constructs at random, with no catalogs of
handwritten scenarios.
