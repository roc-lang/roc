# Issue 13115: restitution at a nested join

Issue: https://github.com/roc-lang/roc/issues/13115

## Result: not reproduced on the current workspace

Built with Zig 0.17.1 using `zig build roc` (Debug compiler).
The issue's program is preserved in
`test/cli/Issue13115NestedJoinRestitution.roc`; the only source formatting
adjustment is parentheses around the `if` condition.

All of these commands exited successfully:

```sh
zig-out/bin/roc --specialize=yes --no-cache test/cli/Issue13115NestedJoinRestitution.roc a b c
zig-out/bin/roc --opt=interpreter --specialize=yes --no-cache test/cli/Issue13115NestedJoinRestitution.roc a b c
zig-out/bin/roc --opt=speed --specialize=yes --no-cache test/cli/Issue13115NestedJoinRestitution.roc a b c
zig-out/bin/roc --specialize=yes --no-cache test/cli/Issue13115NestedJoinRestitution.roc
```

The first three print `Ok(["a"])`; the error-path invocation prints nothing.
The CLI runner regression asserts successful specialized execution and the
exact success-path output. It is passing coverage, not a failing reproduction.

The specialized success-path invocation also succeeds using the ReleaseFast
compiler built with
`zig build roc -Doptimize=ReleaseFast --prefix zig-out/release`.
The Debug runs are the important evidence for this issue because they run
the ARC certifier.

## Current-code analysis

The reported failure would mean that a parameter needed by outcome
restitution had been omitted from the nested join's state summary.
`arc_certify.Certifier.addSummaryDomainCandidate` excludes an entry local
when the join region does not mention it, even if it is otherwise relevant.

The current `collectProcLocals` explicitly records restituted parameter
mentions both at returns and at jumps to return-only joins. The latter is
the boundary at which `runSegment` calls `applyOutcomeRestitution`.
`computeReadsBeforeRebind` also includes restituted parameters at returns
and follows jump successors. These are the relevant safeguards for the
shape in the issue.

## Recommendation

Retain the regression and the explicit restitution-boundary uses. There is
no demonstrated current failure for which to recommend an additional
compiler change. A new failing input or evidence that the failing procedure
shape is not being exercised is needed before claiming a current root
cause or proposing a fix. Passing this example does not prove every nested
join correct. No historical checkout comparison or compiler fix was made.
