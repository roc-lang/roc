# Eight issue reproduction attempts

These fixtures were first tested against omnibus PR #13086,
`ae9f9dbdb7274edf247dc225824d5007309fd859`. Each issue directory records its
focused command, observed result, and limitations. They are reproduction
coverage, not compiler fixes. A passing case or an earlier compiler failure
must not be described as confirmation of the originally reported bug.

## Revalidation after merging main

The combined worktree merged main `98bac3cc6a` at local merge commit
`77b425e80093d9185f4dae73caf690cb84516054`.

The following commands were run in the combined worktree:

```sh
zig fmt --check src/canonicalize/test/value_binding_selection_test.zig src/cli/test/parallel_cli_runner.zig
zig build run-test-cli -j2 -Dplatform=none -- --suite subcommands --filter 'issue 13078' --filter 'issue 13079' --verbose
zig build run-test-cli -j2 -Dplatform=alloc-count -- --suite subcommands --filter 'issue 13081' --filter 'issue 13032' --llvm-only --verbose
```

Formatting passed. The first CLI command selected two cases: #13078 failed
with signal 6 and the exact reported panic, `polymorphic procedure target
evidence had no checked instantiation record`; #13079 passed. The second
selected three cases, all failing:

- #13081 speed: `correct True, amortized False, allocations 196 3076`.
- #13081 size: `correct True, amortized False, allocations 477 7197`.
- #13032: signal 6, `SpecConstr analysis emitted a non-reservation function`.
  This still blocks verification of the reported runtime copying symptom.

Both CLI commands exited 1, with zero skipped cases. The issue-specific
reports for the other cases describe their original-baseline validation;
those results should not be mistaken for revalidation after the main merge.

Subsequent analysis also revalidated the following on the merged worktree:

- #13077: `zig build run-test-zig-module-can -j2 -- --test-filter 'issue 13077'`
  still fails with exactly two `shadowing_warning` diagnostics.
- #13057: `zig build roc -j2` and
  `zig-out/bin/roc check --no-cache test/repros/issue-13057/Probe.roc` succeed.
  `zig-out/bin/roc test --no-cache test/repros/issue-13057/Probe.roc` still
  reports two passing controls and one failing nonloss expectation.
- #13056: `zig-out/bin/roc build --opt=dev --specialize=yes --no-cache
  --target=arm64mac test/repros/issue-13056/main.roc
  --output=zig-out/issue-13056-specialized` exits 1 with the same missing
  nested runtime capture invariant.
- #13035: `python3 test/repros/issue-13035/benchmark.py --loops 100000
  --samples 3` produces correct output for both variants, with median
  times 0.041684 s (empty row) and 0.092974 s (inhabited row), a 2.230x ratio.
  These are noisy runtime measurements, not a deterministic threshold test.
