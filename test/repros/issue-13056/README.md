# Issue 13056: empty published worker preparation

Issue: https://github.com/roc-lang/roc/issues/13056 (body and all comments
read; there were no comments).

Baseline supplied by the parent: omnibus PR #13086,
`ae9f9dbdb7274edf247dc225824d5007309fd859`. The inherited worktree was
preserved; no Git diagnostics, resets, fixes, commits, or publication were
performed. Reproduced on Darwin arm64 using Zig 0.17.1 and a Debug compiler.

## Fixture and assertion

`main.roc` is the issue's exact application: register one
`Worker.Preparation({}, {}, Worker.Never)` with no task entries. Compilation
must finish without invoking the deliberately crashing view initializer.
The CLI regression case is named
`issue 13056: published empty worker preparation builds without initializing its view`
in `src/cli/test/parallel_cli_runner.zig`.

It requires the successful-build message and exit 2 (the published platform's
12 duplicate-definition warnings), and rejects panic, SIGSEGV, and execution
of the view initializer. It does not accept a crash or mark the case skipped.
Explicit `--specialize=yes --no-cache` selects the Monotype path rather than
letting the current default's separate Boxy failure mask it.

This is a published-platform integration reproduction, not a reduced
standalone language test. The issue explicitly reports that its simple
zero-import closure/list reduction did not reproduce. The ten-line application
retains the exact content-addressed dependency rather than substituting an
unproven local approximation.

The first dependency resolution requires access to the pinned GitHub asset;
subsequent runs use Roc's package cache. This is a limitation of this fixture,
not an offline, dependency-free unit test. An independent download verified
the asset's SHA-256:

```text
8134fe1b069bd1360e40340304d30d4530813f1501cce6c8aceb688304d3b14b
```

No package-cache sources were edited.

## Commands and observed evidence

From the repository root:

```sh
zig build -j2 roc
zig-out/bin/roc check test/repros/issue-13056/main.roc
zig-out/bin/roc build --opt=dev --target=arm64mac --timings test/repros/issue-13056/main.roc --output=zig-out/issue-13056
zig-out/bin/roc build --opt=dev --specialize=yes --no-cache --target=arm64mac test/repros/issue-13056/main.roc --output=zig-out/issue-13056-specialized
zig build -j2 run-test-cli -- --suite subcommands --filter 'issue 13056:' --threads 1
zig fmt --check src/cli/test/parallel_cli_runner.zig
```

Compiler build and formatting check succeeded. `roc check` reported
`0 errors and 12 warnings`, exit 2, matching the issue.

The focused CLI command selected exactly one case and exited 1 after about
34 seconds of test execution:

```text
run failed   issue 13056: published empty worker preparation builds without initializing its view
process terminated by signal
signal 6
post-check    monotype
postcheck invariant violated: nested procedure runtime capture was not bound where the procedure is built

0 passed, 1 run failed (1 total)
subcommands     1 run,    1 failed,    0 skipped
dev             1 run,    1 failed
```

This validates the final registered test, including its explicit specialization
and uncached options, rather than only a standalone manual command.

The exact default build command aborted with exit 134 at `post-check
boxy_lowering`:

```text
Boxy lowering invariant violated: static data was taken before the demand closure was complete
```

The explicit specialized, uncached build aborted with exit 134:

```text
Work in progress on thread 10723269 (compile worker 0), outermost first:
  post-check    monotype

thread 10723269 panic: postcheck invariant violated: nested procedure runtime capture was not bound where the procedure is built
```

The emitted stack named `nestedSiteCaptureEntryNodes`, `stepNestedFn`, and
`restoreCapturingConstFn`. These are recorded failure observations, not a
root-cause analysis. The issue's release compiler reported SIGSEGV/139 in
Monotype; this Debug baseline instead reports the invariant above. No claim
about identical internal causes is made.

As a control, a generated copy under `zig-out/` changed only
`.with_workers([owner.registration([])])` to `.with_workers([])` and was built
with the same explicit specialized, uncached options. It passed Monotype but
aborted later at `post-check arc`, reporting
`ARC: proc=204 stmt=74147: use of dead refcounted local 3643`.
Thus the empty registration specifically exposes the earlier Monotype failure,
but the control is not a successful-build control on this baseline. Other
compiler defects may still prevent this integration test from passing after
only the Monotype defect is repaired.

The application was never executed. Root-cause investigation and proposed or
implemented fixes are intentionally out of scope.
