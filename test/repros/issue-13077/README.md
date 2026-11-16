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

Formatting check:

```sh
zig fmt --check src/canonicalize/test/value_binding_selection_test.zig
```

Only reproduction coverage is intended here; no compiler changes or fix analysis.
