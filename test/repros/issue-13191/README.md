# Hosted Box round-trip and joined rows

Issue: https://github.com/roc-lang/roc/issues/13191

## Regression

`src/compile/test/issue_11387_test.zig` contains the focused
`issue 13191: generic hosted Box round trip preserves a joined tag row` test.
It uses the existing checked-app/LIR interpreter harness, with real hosted
functions returning a positive seed, selecting the first callable, and
returning the same box pointer.

```sh
zig build run-test-zig-module-compile -j2 -Dtest-filter='issue 13191' \
  -- --test-filter 'issue 13191'
```

The correct result is 1, meaning the returned problem is B. The second
callable remains in the program to widen the row, but is not selected.
The test runs LSS first and Boxy second and asserts the correct result;
it does not bless the incorrect tag or skip the failing strategy.

Without the fix the LSS run fails with `expected 1, found 0`
(`TestExpectedEqual`): checking and lowering complete, and the failure is the
returned value, not a source diagnostic or a missing host.

## Root cause

The missing information is declaration-variable occurrence correspondence:
both occurrences of `a` in `Box(a) => Box(a)` must have the same runtime
representation, even if row joins change that representation.

`src/postcheck/monotype/lower.zig:48083-48101` requests ordinary procedure
specializations with independent signature roots (only `Iter.fromStep` takes
the exact-graph path here). Hosted completion at `lower.zig:9551-9600`
checks the declared host ABI, but does not supply a relation connecting
the generic slots. `monotype_lifted/lift.zig:628-631` consequently clears
the retained signature root.

`lambda_solved/solve.zig:1684-1691` imports argument locals and a fresh
result independently. `solveFn` does nothing for a hosted body
(`solve.zig:1726`), so no body supplies the missing relation. When row
joining makes the contained tag row mutable, `isCloneInvariant`
(`solve.zig:2864-2866`) correctly stops globally sharing it. The argument
can then acquire `[A, B, C]`, while the returned box retains `[B, C]`.
An unchanged discriminator 1 is B on input and C on output.

## Recommended fix

Make generic hosted representation correspondence explicit producer-owned
metadata. While the checked declaration still distinguishes quantified
variables, author occurrence classes for those variables through the full
signature (including nominal arguments/backings and nested containers).
Carry their instantiated slot mapping through Monotype specialization,
freezing, lifting, and ABI-changing transformations.

Lambda Solved must import each hosted specialization using those explicit
occurrence classes and relate exactly the slots belonging to one declared
variable. A row join or callable-erasure requirement on any such slot must
propagate to every corresponding occurrence. Different declaration variables
remain independent, even if both instantiate to equal concrete Monotypes.
ABI rewrites must transform the mapping alongside the arguments/results;
they cannot silently discard it.

Use compact producer-authored spans and representative ids, consumed once
when registering a hosted function, with the existing solver unification
and row-join machinery propagating changes. This avoids downstream graph
searches, extra runtime conversions, and payload copies at host boundaries.

Do not mark every hosted signature `exact_graph` using the existing
Monotype-id sharing mechanism: that can conflate `a` and `b` in
`Box(a), Box(b) => Box(a)` when their instantiated shapes are equal.
Do not repair tag ordinals in the backend or globally share all mutable
rows; both lose the source-level representation relation.

Follow-up coverage should include independent equal-shaped variables,
repeated occurrences in nested/recursive types, callable payloads, multiple
instantiations, and retained metadata through specialization.

The fix follows this recommendation: Monotype's hosted completion records
the occurrence paths (`Mono.FnBody.hosted`, built by `hostedSignatureVars`),
lifting carries them, and Lambda Solved registration unifies each variable's
positions (`relateSignatureVars`). See design.md "Lambda Solved Types".
