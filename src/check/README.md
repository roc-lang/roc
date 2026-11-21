# Check Types

Performs Hindley-Milner type inference with constraint solving and unification on the Canonical Intermediate Representation (CIR).

The check module is the third stage of the Roc compiler pipeline. It performs type inference using the Hindley-Milner algorithm, ensuring type safety and generating the necessary type information for code generation. This stage catches type errors and ensures the program is well-typed before proceeding to evaluation or compilation.

Exhaustiveness uses read-only nominal views so declaration substitutions cannot
mutate solver types or escape as solver blockers. Exact template dependencies
allow irrelevant substitutions to share identity while private unknowns retain
their application scope.

Inhabitedness uses a greatest fixed point to preserve conservative recursive
payload semantics without enumerating shared paths. Row-only cycles provide no
constructor witness. General, constructor-payload, and known-absent queries retain
distinct leaf and open-row policies; only settled answers are memoized.

Diagnostic snapshots freeze type evidence before error poisoning. They preserve
sharing, recursive identities, and display policy, but render only requested
roots and tags. Cached checking outcomes retain this frozen evidence so warm and
cold diagnostics follow the same presentation path.

A rejected branch supplies no result type, but does not erase either a sibling's
type or the effects of evaluating the selected branch. Diagnostics that depend
on a join or on a `?` payload use those same producer relationships after deferred
rejections settle. A Boolean eligible for compile-time evaluation does not justify
a known-condition warning if obtaining it requires a rejected operand.

Host-boundary validation uses the same scoped read-only views so generic user
types cannot hide an unboxed callable or lose a parameter's boxing position.
Functions cross that boundary only inside a `Box`, either as its immediate
payload or inside a boxed value the host only holds; `Iter` and `Stream` cross
only as the immediate payload of their own `Box`.
