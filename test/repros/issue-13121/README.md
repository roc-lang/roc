# Issue 13121: aliased function returning Try with Iter

Issue: https://github.com/roc-lang/roc/issues/13121

## Result: not reproduced on the current workspace

Built with Zig 0.17.1 using `zig build roc` (Debug compiler).
`test/cli/Issue13121AliasedIteratorTry.roc` preserves the issue's source,
including the unannotated alias, closed `[Null]` error row, nested pattern,
and iterator.

All of these commands exited successfully:

```sh
zig-out/bin/roc check --no-cache test/cli/Issue13121AliasedIteratorTry.roc
zig-out/bin/roc check --specialize=yes --no-cache test/cli/Issue13121AliasedIteratorTry.roc
zig-out/bin/roc --specialize=yes --no-cache test/cli/Issue13121AliasedIteratorTry.roc
zig-out/bin/roc --opt=interpreter --specialize=yes --no-cache test/cli/Issue13121AliasedIteratorTry.roc
zig-out/bin/roc --opt=speed --specialize=yes --no-cache test/cli/Issue13121AliasedIteratorTry.roc
```

Checks report no errors. Executions print `1.0`, the current formatting of
the inferred numeric result, rather than the `1` stated in the older report.
The CLI runner tests assert successful specialized checking and that exact
execution result. These are passing regression tests, not failing repros.

Also built a ReleaseFast compiler:

```sh
zig build roc -Doptimize=ReleaseFast --prefix zig-out/release
zig-out/release/bin/roc check --no-cache test/cli/Issue13121AliasedIteratorTry.roc
zig-out/release/bin/roc check --specialize=yes --no-cache test/cli/Issue13121AliasedIteratorTry.roc
zig-out/release/bin/roc --specialize=yes --no-cache test/cli/Issue13121AliasedIteratorTry.roc
```

Both release checks pass, and specialized execution again prints `1.0`.

## Current-code analysis and recommendation

Current Monotype lowering handles exact top-level procedure aliases through
`forwardedCallableEvalCalleeAtNode`. It enters the alias's checked wrapper
scope, seeds the use-site substitution, relates the checked function roots
to the requested interface, and follows the explicitly recorded lookup.
The executed specialized runs reach Monotype lowering without the reported
segfault.

A segfault's root cause cannot be established from the issue's stage name
alone. No failing current execution or invalid access was found, so there
is no justified compiler fix to recommend. Keep the regression; further
investigation requires a current failing input or a reproducible
configuration difference. The reported crash was not observed in either
the Debug or ReleaseFast compiler; passing these inputs does not prove the
absence of other alias-lowering defects.

No historical checkout comparison or compiler fix was made.
