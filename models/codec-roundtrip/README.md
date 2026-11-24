# Codec-roundtrip model

A model of the compiler's derived abilities: the JSON encoder and parser it derives for structural and nominal types
(`encoder_for`, `parser_for`), and derived equality and hashing (`is_eq`, `to_hash`). It decides, for a random type and
two random values `v` and `w` of it, five things: what `Json.to_str(v)` writes (a reference encoder built from the
builtin `Json` source), that parsing that text gives `v` back, that parsing the reference text gives `v` back, whether
`v == w`, and whether `v` and `w` land in one set entry or two. It talks to the compiler only as a generator: it writes
programs, runs them through `roc test` and as built apps on every backend, and compares what they compute with the
model.

## Requirements

- Python 3.10 or later, standard library only.
- Node.js (any version with WebAssembly) to run wasm32 output. `NODE` selects it.
- `platform/targets/wasm32/host.wasm`: the host of `test/wasm/platform`, which `zig build` writes to
  `test/wasm/platform/targets/wasm32/host.wasm` as a test fixture. Copy it here:
  `mkdir -p platform/targets/wasm32 && cp ../../test/wasm/platform/targets/wasm32/host.wasm platform/targets/wasm32/`.
  It is not checked in.
- A `roc` built from this checkout (`zig build roc`). No `dev_diagnostics` variable is read; a Debug build is what
  turns compiler invariants into panics the model reports.

## Quick start

```bash
./run.sh selftest
```

```bash
./run.sh check tests/smoke
```

```bash
./run.sh fuzz 20
```

`ROC` selects the compiler (default: `zig-out/bin/roc` at the repository root). `JOBS` (default 3) is how many configs
of one program run at once. Output goes to `out/`. `--configs` picks a subset of the twelve configs.

`fuzz` takes `--seed S` (program i uses seed S+i), `--cases K` (cases per program, default 5), `--keep` (keep the
files of agreeing programs) and `--no-reduce`. It exits nonzero when a program disagrees with the model. `gen SEED`
prints a program as a test module and the app output the model expects; `reduce SEED` regenerates a program and
reduces its disagreement.

## How it works

1. `gen.py` builds a type from small typed constructs: `Str`, `Bool`, every integer width, `Dec`, `{}`, a single
   payload-free tag (both zero-sized), lists, tuples, sets and dicts (keyed by `Str` or an integer), records with
   required and optional (`Try(x, [Missing])`) fields, structural tag unions with zero, one or several payloads,
   `Try(ok, err)`, nullable `Try(ok, [Null])`, and nominal types. A nominal is declared `:=` or opaque `::`, derives
   all four of `is_eq`, `to_hash`, `parser_for` and `encoder_for`, and is plain, generic in one parameter, recursive
   (through a tag payload or a list), or has a hand-written Json codec that writes a one-field record as its bare
   scalar. Values favor each number type's extremes and zero, and strings mix quotes, backslashes, control bytes,
   DEL, combining marks, U+2028, a BOM and an emoji. `w` is a copy of `v`, a value with one leaf changed, a set or
   dict in another order, or a fresh value.
2. `gen.encode` is the reference encoder and `gen.equal` is structural equality (RULES.md says where each comes
   from).
3. The emitter writes each case as functions `v<i>`, `w<i> : U64 -> T<i>`, and then
   a test module (`test/Prog.roc`) with one `expect` per judgment, and an app (`native/app.roc`, and the same on
   `test/wasm/platform` in `wasm/`) that prints one line per case: round trip, decode of the reference text, `==`,
   `!=`, the set size, and the JSON text. The app chooses its values and the text it decodes with a runtime value, so
   they are not compile-time constants; the test module has no runtime input, so its values may be evaluated at compile
   time.
4. `driver.py` runs `roc check` (a program that does not check is a generator bug, unless the compiler panics), then
   every config, and compares: `roc test` failures map back to a case and judgment by line, and app output is
   compared field by field. A disagreement is reduced to one case and smaller values, kept in `out/fuzz/<seed>/min/`.

The configs are `roc test` at `--opt=dev` and `--opt=speed` with both `--specialize` settings (`test-dev`,
`test-dev-spec`, `test-speed`, `test-speed-boxy`), the interpreter (`run-interp`), native builds (`dev`, `dev-spec`,
`speed`, `speed-boxy`) and wasm32 builds (`wasm-dev`, `wasm-dev-spec`, `wasm-speed`), as the backend-diff model runs
them.

## Layout

| Path | What it is |
|---|---|
| `run.sh` | entry point |
| `gen.py` | type and value generator, reference encoder, structural equality, Roc emitter |
| `driver.py` | configs, running and comparing, reduction, `check`, `selftest` |
| `run_wasm.js`, `platform/` | the wasm runner and a copy of `test/wasm/platform` (from the backend-diff model) |
| `tests/*/case.py` | fixtures: a `gen.Prog` built from the model's constructors, with an optional `known.txt` |

## Rules and format

- `RULES.md`: the rules (ENC, RT, DEC, EQ, HASH, NOPANIC), each tied to design.md, the language reference and the
  code that implements it.
- There is no `FORMAT.md`: the model reads no compiler dump.

## Limits

- No `F32`/`F64` (`Json.to_str` needs an empty error type, and float text round trips are a separate question), no
  `Box` (it has no `is_eq`), no `Str.inspect`, no `Json.parse_trailing_commas` or `parser_camel`, and no formats other
  than JSON.
- `Try(x, [Missing])` appears only as a record field, as design.md "Structural Serialization Methods" says, and never
  directly inside another optional field.
- A fuzz disagreement is labelled `known #n` by its panic message alone (`KNOWN` in `driver.py`), and is then not
  reduced. Another bug with the same message would get the same label, and a known panic hides whatever else that
  config would have shown.
- Reduction keeps unused nominal declarations, and keeps the whole program when no single case fails alone.
- No parse of malformed or reordered input: the decoder is fed the encoder's own text and the reference text only.
- No custom `is_eq` or `to_hash`, no open rows or `where` clauses, and no codec used through a generic function.
- Dict keys are `Str` and integers only; dict and set values are built with `from_list`, so their order is insertion
  order.
- The HASH rule sees a wrong hash only when it puts equal values in different set entries.

## Bugs found

Each has a fixture here:

- [#13429](https://github.com/roc-lang/roc/issues/13429) (`tests/eq-recursive-in-record`): derived equality of a structural value holding a recursive nominal panics
  in Monotype under `--specialize=yes` ("instantiation graph relation changed after final relation production").
- [#13430](https://github.com/roc-lang/roc/issues/13430) (`tests/zst-multi-payload`): under `--specialize=no`, encoding a tag whose two or more payloads are all
  zero-sized, in a union of two or more tags, reads fields out of a zero-sized payload; the dev backend, wasm and the
  interpreter panic and LLVM fails to compile.
- [#13431](https://github.com/roc-lang/roc/issues/13431) (`tests/parse-try-tuple`): parsing a builtin `Try` with a tuple in a payload panics in Monotype under
  `--specialize=yes`.
