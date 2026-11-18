# Issue 13043: liveness groups are not ownership places

Issue: https://github.com/roc-lang/roc/issues/13043

`Repro.roc` preserves the reported application, adjusting only its platform
path and adding result assertions after first checking the original input.
The five assertions cover both node variants and every branch of `pick` and
`name`.

## Local result

A Debug compiler built with `zig build roc` checks the reported input without
errors. With the result assertions added, checking still succeeds and all
five tests pass. The reported panic does not reproduce in this worktree;
this is not a failing regression test.

Run from the repository root:

```sh
zig build roc
zig-out/bin/roc check --no-cache test/repros/issue-13043/Repro.roc
for specialize in yes no; do
  zig-out/bin/roc check --no-cache --specialize="$specialize" \
    test/repros/issue-13043/Repro.roc
  for opt in interpreter dev; do
    zig-out/bin/roc test --no-cache --specialize="$specialize" --opt="$opt" \
      test/repros/issue-13043/Repro.roc
  done
done
```

All checks reported `No errors found`; each test invocation reported
`All (5) tests passed`. This fixture is manually runnable and is not
registered with the CLI suite. The full suite was not run.

## Root cause and ideal fix

The reported assertion conflated two different relations:

- `Solution.adoptHeldBindings` gives held bindings independent solved
  liveness groups, but intentionally preserves their borrow sources
  (`src/lir/arc_solve.zig:502–522`). A binding owns its taken unit only in
  emissions where the take applies.
- `ownershipPlaceLeader` follows those borrow sources and projection aliases
  until it reaches the current emission's owned binding, or the terminal
  solved leader (`src/lir/arc.zig`, `ownershipPlaceLeader`).

Thus two arguments with different solved leaders can still reach the same
ownership place in the borrowed emission. In the issue's example, both fields
reach the borrowed node parameter. The old leader prefilter described in the
issue rejects precisely this legal state. Its assertion fails in Debug;
removing only the assertion would still leave an incorrect sharing query.

The ideal fix is for `groupSharesOtherOperand` to compare the operands'
emission-specific ownership places directly, using `ownershipPlaceLeader`
as the single source of truth. That exact implementation is already present
in `arc.zig`. Correct sharing information prevents an argument's unit
from moving while another argument still borrows from it
(`callArgOwnership`).

Do not cut held bindings' borrow sources to make the prefilter valid: those
links are required to locate the actual container in non-take emissions.
Nor should solved liveness groups be treated as an approximation of ownership
identity. No performance bottleneck was measured, and no additional caching or
compiler changes were introduced during the initial reproduction investigation.

## Performance follow-up

The subsequent optimization preserves the same place equality relation.
Each binding-override context memoizes resolved places by frame ordinal and
operand sharing by immutable local span, including duplicate occurrences.
Source preparation and emitted variants initialize independent indexes.
Sharing is computed in linear work per queried span rather than a chain walk
for each pair of operands on each planning visit. Cached revisits allocate
nothing. Tests exercise deep chains, held bindings, repeated contexts,
same-start spans of different lengths, and operands beyond position 64.

`benchmark.py` compares optimized compiler executables using the CI syntax
fixtures. It records randomized pairs, separate warm source trees/caches,
both lowering strategies, elapsed time, and peak RSS. It is a local Linux
substitute, not the CI benchmark: the nightly pinned by `ci/perf/bench.roc`
was unavailable when the follow-up was measured. In particular, this script
does not cover the CI driver's cold-empty-cache scenario.

See [PERFORMANCE.md](PERFORMANCE.md) for the correctness checks, controlled
comparison results, hardware counters, and limits of those measurements.
