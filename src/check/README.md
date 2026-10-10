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
