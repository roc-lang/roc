# <Name> model

<One paragraph. What judgment the model decides, which compiler stage it checks, and how it talks to the compiler:
oracle on real compilations, generator of Roc programs, or both.>

## Requirements

- <toolchain and version, with an install command>
- A Debug `roc` built from this checkout (`zig build roc`). <Name the `dev_diagnostics` variable it reads, and the PR that adds it if it is not merged yet.>

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

## How it works

<Short. The pipeline from Roc source to verdict, and what a disagreement means.>

## Layout

| Path | What it is |
|---|---|
| `run.sh` | entry point |
| ... | ... |

## Rules and format

- `RULES.md`: the rules the model checks, each mapped to design.md and to the compiler code that enforces it.
- `FORMAT.md`: the dump format the model reads, and what the export drops.

## Limits

<What is not modeled, and what the export drops. Everything here is something the model cannot catch.>

## Bugs found

<Links to the issues this model found.>
