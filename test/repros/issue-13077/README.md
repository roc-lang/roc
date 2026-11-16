# Issue #13077 reproduction

Issue: https://github.com/roc-lang/roc/issues/13077 (body read; no comments).

Baseline supplied by the parent: inherited omnibus PR #13086,
`ae9f9dbdb7274edf247dc225824d5007309fd859`. No reset or Git diagnostics were
performed.

The regression test is
`src/canonicalize/test/value_binding_selection_test.zig`, named
`issue 13077 nested derived declarations have distinct owners`.
It parses the issue's nested nominal declarations and asserts that
canonicalization emits zero diagnostics. Runtime equality expectations are
unnecessary to reproduce this declaration diagnostic, so the test stops before
type checking or execution. It has no skip or expected-failure mechanism.

Run from the repository root:

```sh
zig build run-test-zig-module-can -j2 -- --test-filter 'issue 13077'
```

Observed result: exit 1, exactly one selected test, zero passed and one failed.
Parsing succeeds; the zero-diagnostic assertion fails with:

```text
unexpected diagnostic: shadowing_warning
unexpected diagnostic: shadowing_warning
expected 0, found 2
```

This reproduces the issue's two unwanted declaration warnings at the
canonicalization layer. The initial attempt used the nonexistent build step
`run-test-zig-module-canonicalize` and failed before running tests; the command
above uses the actual module suite name and reached the regression assertion.

Formatting check:

```sh
zig fmt --check src/canonicalize/test/value_binding_selection_test.zig
```

Observed result: exit 0.

Only reproduction coverage is intended here; no compiler changes or fix analysis.
