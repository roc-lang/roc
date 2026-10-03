# Check Types

Performs Hindley-Milner type inference with constraint solving and unification on the Canonical Intermediate Representation (CIR).

The check module is the third stage of the Roc compiler pipeline. It performs type inference using the Hindley-Milner algorithm, ensuring type safety and generating the necessary type information for code generation. This stage catches type errors and ensures the program is well-typed before proceeding to evaluation or compilation.

`type_view.zig` gives read-only exhaustiveness analysis scoped nominal
substitutions without retaining solver scratch in checked modules. Its analysis
identities cannot escape as mutable solver blockers.

View normalization ignores a substitution only when exact template dependencies
show it is irrelevant. Private unknowns retain their full application scope:
otherwise an empty-payload assumption could accidentally constrain another
application. Immutable closed arguments can share identity, allowing recursive
argument resets and permutations to converge without declaration-only cutoffs.

View-based inhabitedness queries solve a finite AND/OR graph to its greatest
fixed point. This preserves conservative recursive-payload semantics without
enumerating shared paths through recursive or acyclic graphs. General,
constructor-payload, and known-absent queries keep distinct leaf and open-row
policies; only fully settled answers can enter a query memo.
Row-only cycles are contracted separately: an extension cycle contributes no
inhabited witness without an explicit constructor. Shared row tails and
nominal/alias union-shape lookups are decoded once per query.
