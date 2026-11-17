# Issue 13057: derived Dict parsing of keys equal under the key type

Issue: https://github.com/roc-lang/roc/issues/13057

`Probe.roc` is a dependency-free reduction of the issue's signed-zero case.
It uses the counted-format protocol (as in `test/cli/ParserCountedFormat.roc`)
instead of CBOR, with two alternating key-bit-pattern/value pairs. Both keys
decode faithfully as F64. A derived nominal bitwise-key parser preserves both
entries. Under numeric equality the two zeros are one key, and a derived dict
parser always keeps the value given last for a repeated key, so the numeric-key
parser returns one entry holding the second value. The issue comment's broader
composite-key case is not exercised here.

The first two expects establish numeric Dict equality and successful two-entry
decoding under lawful bitwise equality; the third pins the last-value rule.

The CLI suite runs the probe under both lowering strategies ("issue 13057"
in `src/cli/test/parallel_cli_runner.zig`).

## Focused validation

Run from the repository root:

```sh
zig build roc
zig-out/bin/roc test --no-cache test/repros/issue-13057/Probe.roc
zig-out/bin/roc test --specialize=no --no-cache test/repros/issue-13057/Probe.roc
```

Both runs pass all three expects.
