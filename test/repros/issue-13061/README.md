# Issue 13061: discriminant read after a stored field unit leaves

Issue: https://github.com/roc-lang/roc/issues/13061

`Describe.roc` preserves the issue comment's whole-record call reproducer.
`FieldTake.roc` exercises its field-take variant. Both assert the correct
result, `"named n k"`, without disabling certification or allowing failure.

## Local result

A Debug compiler built with `zig build roc` passes both inputs: each reports
`All (1) tests passed`. Neither reported panic reproduces in this worktree.
This is not a failing regression test. The original 84-module app is not
included in the issue, so its failure remains unverified.

Run from the repository root:

```sh
zig build roc
zig-out/bin/roc test --no-cache test/repros/issue-13061/Describe.roc
zig-out/bin/roc test --no-cache test/repros/issue-13061/FieldTake.roc
for specialize in yes no; do
  for opt in interpreter dev; do
    for input in Describe FieldTake; do
      zig-out/bin/roc test --no-cache --specialize="$specialize" --opt="$opt" \
        "test/repros/issue-13061/$input.roc"
    done
  done
done
```

All these invocations passed. These fixtures are manually runnable and are
not registered with the CLI suite.

The existing producer and certifier tests also passed (8 tests):

```sh
zig build run-test-zig-module-lir \
  -Dtest-filter='lir tests' -Dtest-filter='tag union' --summary all \
  -- --test-filter 'tag union'
```

The `lir tests` compile filter includes the declaration-discovery test;
the runtime filter selects only the tag-union tests.

## Root cause and ideal fix

The issue comment identifies a missing distinction between an aggregate's
inline representation and its owned RC slots. The current code confirms the
relevant contracts:

- A borrowed view definition copies representation without observing its
  slots (`src/lir/arc_dismantle.zig:4355`). A discriminant read observes only
  inline tag bytes, not payload ownership (`arc_dismantle.zig:2463`).
- Consequently a field's stored unit may legitimately leave before that
  view is created. Whole-move release masks exclude future ownership
  observations (`arc_dismantle.zig:4515`).
- A producer that omits the view's residual-shell metadata then emits what
  appears to be an ordinary RC-bearing read of the spent field.
  `bindPayloadReadWithShell` must reject that through `requireFieldUntaken`
  (`src/lir/arc_certify.zig:6320`). The certifier is detecting an incomplete
  ownership contract, not authorizing a new payload read.

The ideal fix is to publish tag-union representation views alongside struct
views and carry their exact absent ownership slots from ARC's path state into
LIR. This is already implemented in `arc_dismantle.zig:5137`,
`src/lir/arc.zig`'s `processSolveSegment` and
`materializeResidualShellAbsentFields`. A union shell lists its own slot
0 as absent. Certification admits the representation-only read while retaining
the prohibition on payload reads through that shell
(`arc_certify.zig:6280`).

This preserves early release and avoids extra retains, runtime drop flags,
or backend ownership inference. Treating every discriminant read as payload
liveness would unnecessarily keep units alive; weakening the certifier would
lose its protection against actual reads of spent payloads.

Existing tests pin emission of the absent slot (`arc.zig`, "RC tag union view
read after its slot was taken names that slot absent"), acceptance
of a discriminant read, rejection of missing metadata, and rejection of a
payload read through a shell (`arc_certify.zig:10354–10374`).
No compiler fix was needed during the initial reproduction investigation. The original app is needed
to establish whether it has another trigger outside this now-passing case.

## Performance follow-up

ARC now memoizes each queried view's container-relative piece mask within
one binding-override context. It still computes absence from each path's
current residual state; an intact path must never inherit another path's
absent mask. The tag-union shell's singleton slot is written directly without
a temporary growable array. The LIR contract and certifier remain unchanged.
A focused test checks path separation, nested-holder placement changes across
ownership contexts, and allocation-free singleton materialization.

The whole-compiler comparison script is
`test/repros/issue-13043/benchmark.py`; it measures these changes together
with ownership-place memoization.
See [the measurement report](../issue-13043/PERFORMANCE.md) for results and
limitations.
