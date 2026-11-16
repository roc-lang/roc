# Issue 13032 reproduction attempt

Issue: https://github.com/roc-lang/roc/issues/13032 (body and comments read;
there were no comments). Inherited baseline supplied by the task: omnibus PR
#13086, `ae9f9dbdb7274edf247dc225824d5007309fd859`. No reset or Git diagnostics
were performed.

The candidate regression is `test/alloc-count/app_try_field_read_callback.roc`,
registered in `src/cli/test/parallel_cli_runner.zig` as the `issue 13032` speed
case. It uses the existing allocation-count host, whose 16-byte runtime input
selects 512 appends. It retains the issue's scalar field read, function-value
call, `?`, and loop carrying `Try(W, _)`. The harness requires length 512 and
at most 32 allocation/reallocation calls, allowing geometric capacity growth
but rejecting a copy per append. There are no skips or expected-failure flags.

## Validation

On macOS ARM64 with Zig 0.17.1:

```sh
zig build run-test-cli -j2 -Dplatform=alloc-count -- --include-llvm --filter 'issue 13032' --verbose
```

Result: exit 1; 1 test run, 1 failed, 0 skipped. Compilation aborts with signal 6:

```text
post-check    spec_constr
postcheck invariant violated: SpecConstr analysis emitted a non-reservation function
```

The stack names `restoreAnalysisIds` at
`src/postcheck/monotype_lifted/spec_constr.zig:2037`. This prevents execution
and **does not confirm the issue's excessive-allocation symptom**. No allocation
count was obtained. The same command and failure occurred with the original
loop directly in `run!`, with a `while` loop, and with the final pure `grow`
helper. Compiler fixes and root-cause investigation were intentionally not
undertaken.

Initial invocations without `--include-llvm` (including a `--summary all` and a
`--verbose` invocation) exited 0 but selected no speed cases; they are not
validation. A direct invocation:

```sh
zig-out/bin/roc --opt=speed --no-cache test/alloc-count/app_try_field_read_callback.roc
```

failed because the source directory has no built `arm64mac/libhost.a`; the
build-managed CLI harness prepares platform libraries in its isolated fixture
directory. Use the focused harness command above.

This is a pending reproduction, not a verified reproduction of quadratic
copying. Re-run after the compilation obstruction is resolved; do not interpret
the compiler crash as evidence of the reported performance bug.

## Original report input

`main.roc` beside this README preserves the report's program (only whitespace
normalized), including its default platform and command-line argument. This
rules out the allocation-count platform and measurement calls as prerequisites
of the observed compilation obstruction:

```sh
zig-out/bin/roc build --opt=speed test/repros/issue-13032/main.roc --output=zig-out/issue-13032
```

Result: exit 1, the same `spec_constr` invariant above. No executable was
produced, so the report's `400000`-append timing could not be run.

The report's dev control does compile and run:

```sh
zig-out/bin/roc build --opt=dev test/repros/issue-13032/main.roc --output=zig-out/issue-13032-dev && zig-out/issue-13032-dev 512
```

Result: exit 0, zero errors/warnings, output `512`. This checks the original
source's validity, not the speed allocation bound. The original speed command,
instrumented original loop, equivalent while loop, and pure loop helper have
all been tried without reaching speed runtime execution. No optimization pass
or invariant was disabled to force execution.
