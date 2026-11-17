# Issue 13079 reproduction

Issue: https://github.com/roc-lang/roc/issues/13079

Inherited baseline: omnibus PR #13086 head
`ae9f9dbdb7274edf247dc225824d5007309fd859`, as supplied by the parent.
No reset or Git diagnostic commands were used.

The issue body and comments were read with
`gh issue view 13079 --repo roc-lang/roc --comments`; there were no comments.

## Regression

`test/cli/issue_13079/Repro.roc` preserves the issue's reduced input,
including all three top-level expects. The case in
`src/cli/test/parallel_cli_runner.zig` asserts successful `check --no-cache`
and absence of panic/invariant output. It does not evaluate the deliberately
crashing stub functions. There are no skip or expected-failure flags.

**Not reproduced on the supplied baseline:** both checker-only and full CLI
checks pass. This is passing regression coverage, not a confirmed failing repro.

## Validation

Toolchain: `zig version` reports `0.17.1`.

```sh
zig build -j2 run-test-cli -- --filter 'issue 13079'
```

Exit 0:

```text
1 passed (1 total) in 3551ms using 1 worker(s)
subcommands     1 run,    0 failed,    0 skipped
```

The original command shape was also checked directly:

```sh
zig-out/bin/roc check --no-cache test/cli/issue_13079/Repro.roc
```

Exit 0:

```text
No errors found in 57ms for test/cli/issue_13079/Repro.roc
```

A checker-only `TestEnv` trial with the same input also passed:

```sh
zig build -j2 run-test-zig-module-check -Dtest-filter='issue 13079' -- --test-filter 'issue 13079'
```

That trial was removed in favor of CLI coverage of the actual reported command.
An earlier broad `run-test-zig` attempt did not reach a usable test result;
its retry was stopped in favor of the focused checker leaf.

```sh
zig fmt --check src/cli/test/parallel_cli_runner.zig
```

Formatting check passed (exit 0).

Only reproduction coverage was added; no compiler fixes or fix analysis.
