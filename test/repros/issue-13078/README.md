# Issue #13078 reproduction

Issue: https://github.com/roc-lang/roc/issues/13078 (body read; no comments).

Inherited baseline supplied by the parent: omnibus PR #13086,
`ae9f9dbdb7274edf247dc225824d5007309fd859`. No reset or Git diagnostics
were performed. Validation uses macOS ARM64 and Zig 0.17.1.

The issue reports a checker panic for a destructured empty list used to
initialize a mutable variable, followed by two method-call appends and an
immutable binding of the result. Correct behavior is successful checking with
`f : U64 -> List(U64)`.

## Regression test

The `issue 13078:` case in `src/cli/test/parallel_cli_runner.zig` checks
`Repro.roc`, preserving every reported ingredient. It asserts exit success and
`No errors found`, without skips or expected-failure handling.

```sh
zig build run-test-cli -j2 -Dplatform=none -- --suite subcommands --filter 'issue 13078' --verbose
```

Observed: exit 1, exactly one selected case, `0 passed, 1 run failed (1 total)`,
and `subcommands 1 run, 1 failed, 0 skipped`. The compiler terminated with
signal 6 during type checking and printed the invariant panic quoted below.
`-Dplatform=none` avoids building unrelated platform hosts; this check-only
fixture requires no platform. The earlier `-Dtest-filter='issue 13078:'`
harness attempt did not reach execution and is not counted as reproduction
evidence.

`zig fmt --check src/cli/test/parallel_cli_runner.zig` passed.

## Original CLI scenario

`Repro.roc` contains the issue's source, with only blank lines reduced.

```sh
zig build run-roc -j2 -- check --no-cache test/repros/issue-13078/Repro.roc
```

Observed: build command exit 1; compiler terminated with signal ABRT during
`type-check module Repro`, with the exact issue symptom:

```text
checked artifact invariant violated: polymorphic procedure target evidence had no checked instantiation record
```

## Test-layer selection

An initial checker-only `TestEnv` test passed:
`zig build run-test-zig-module-check -j2 -Dtest-filter='issue 13078:' --summary all`
reported `13/13 steps succeeded; 2/2 tests passed`. That insufficient test and
its registration were removed in favor of the CLI regression above.
The runtime-filter spelling of that checker command also exited 0.
The initial `zig build run-test-zig -j2 -- --test-filter 'issue 13078:'`
was stopped while building unrelated test modules and replaced by the focused
module command.

No compiler fixes or root-cause investigation are included.
