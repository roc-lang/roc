# Issue 13057: derived Dict parsing must not silently lose typed-equal keys

Issue: https://github.com/roc-lang/roc/issues/13057

Reproduced on the inherited omnibus PR #13086 baseline
`ae9f9dbdb7274edf247dc225824d5007309fd859`, as supplied by the parent task.
No reset, Git diagnostics, or compiler changes were performed.

`Probe.roc` is a dependency-free reduction of the issue's signed-zero case.
It uses the existing counted-format protocol (as in
`test/cli/ParserCountedFormat.roc`) instead of CBOR, with two alternating
key-bit-pattern/value pairs. Both keys decode faithfully as F64. A derived
nominal bitwise-key parser preserves both entries, whereas the numeric-key
parser must refuse the collision rather than return a lossy dictionary.
The issue comment's broader composite-key case is not exercised here.

This is a standalone `roc test` regression, not registered in the CLI suite.
The final expect asserts the issue's normative nonloss requirement: `Err`.
There are no skips, expected failures, or assertions accepting silent loss.
The two preceding expects establish numeric Dict equality and successful
two-entry decoding under lawful bitwise equality.

## Focused validation

Run from the repository root:

```sh
zig build roc -j2
zig-out/bin/roc check --no-cache test/repros/issue-13057/Probe.roc
zig-out/bin/roc test --no-cache test/repros/issue-13057/Probe.roc
```

Observed with Zig 0.17.1 on macOS ARM64:

- Build: exit 0, `Build succeeded!`
- Check: exit 0, `No errors found in 67ms for test/repros/issue-13057/Probe.roc`.
- Test: exit 1, with the numeric-key nonloss expect at line 82 failing:

```text
expect {
    result : Try(Dict(F64, U64), Error)
    result = decode([0, 1, 0x8000000000000000, 2])
    result.is_err()
}

Ran 3 tests in 244.2 ms.:
    2 passed
    1 failed
    0 compiler errors
```

Thus the decoded numeric dictionary is accepted rather than refused; both
controls pass. This records only reproduction evidence, not a root-cause
investigation or a proposed API/fix.
