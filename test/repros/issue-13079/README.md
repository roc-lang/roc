# Issue 13079 reproduction

Issue: https://github.com/roc-lang/roc/issues/13079

Inherited baseline: omnibus PR #13086 head
`ae9f9dbdb7274edf247dc225824d5007309fd859`, as supplied by the parent.
No reset or Git diagnostic commands were used.

The issue body and comments were read with
`gh issue view 13079 --repo roc-lang/roc --comments`; there were no comments.

## Regression

`src/check/test/issue_13079_test.zig` preserves the issue's reduced input,
including all three top-level expects. It uses the existing checker `TestEnv`
and asserts no diagnostics, without evaluating the deliberately crashing
stub functions. It is registered in `src/check/mod.zig`.
There are no skip or expected-failure flags.

## Validation

Toolchain: `zig version` reports `0.17.1`.

```sh
zig build -j2 run-test-zig -- --test-filter 'issue 13079'
```

Validation is in progress; the result will be recorded here when it finishes.

```sh
zig fmt --check src/check/test/issue_13079_test.zig src/check/mod.zig
```

Formatting check passed (exit 0).

Only reproduction coverage was added; no compiler fixes or fix analysis.
