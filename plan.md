# Cor Alignment Plan

Status: complete.

Final audit status:

- recursive non-function top-level defs are poisoned before MIR: clean
- monomorphic top-level constant misuse is rewritten to runtime-error use-sites: clean
- MIR named-const placeholder registration path: clean
- dead `erroneous_exprs` side-tagging path: clean
- first artifact audit: clean
- second artifact audit: clean

This plan replaces the stale MIR-only completion claim. The remaining gap with
`~/code/cor/` is no longer primarily in `corecir`; it is in the frontend
guarantees that feed MIR and in the last MIR accommodations for illegal value
recursion.

Non-negotiable rules for this plan:

- Do not run `zig` at any point while executing this plan.
- Do not add workarounds.
- Do not add fallbacks.
- Do not add heuristics.
- Do not preserve backwards-compatible transitional APIs unless they are the
  final architectural API.
- Do not “patch over” missing facts by reconstructing them later.
- Every compiler stage must consume explicit facts produced by earlier stages.
- Parsing and error reporting are the only places where recovery behavior is
  allowed.

This plan is not complete until:

1. Illegal recursive non-function top-level defs are rejected before MIR and
   rewritten to erroneous CIR/runtime-error nodes.
2. Top-level non-function defs are monomorphic only; conflicting uses must
   produce an explicit type-checking error and erroneous use-sites must be
   rewritten to crash/runtime-error nodes.
3. MIR no longer contains named-const placeholder registration that exists only
   to accommodate illegal value recursion.
4. The MIR callable/value architecture remains aligned with `cor` after the new
   frontend guarantees are in place.
5. An artifact audit finds no lingering traces of the old recursive-value or
   pattern-fact systems.
6. A second artifact audit after cleanup also comes back clean.


## Phase 1: Enforce cor-Style Recursive Value Rules Before MIR

Target architecture:

- Only function defs are recursive at the value level.
- Recursive nominal/tag-union shape is handled syntactically at the type level,
  not via value-level recursion machinery.
- Recursive non-function top-level defs must never reach MIR as ordinary values.

Detailed changes:

1. Canonicalization SCC enforcement.
   - In `src/canonicalize/Can.zig`, after SCC computation, identify recursive
     SCCs that contain non-function defs.
   - Rewrite each invalid non-function def in those SCCs to a malformed
     runtime-error expr.
   - Add an explicit canonicalization diagnostic for recursive non-function
     value cycles instead of reusing a misleading direct-self-reference message.

2. Function-vs-constant classification cleanup.
   - In `src/canonicalize/DependencyGraph.zig`, stop treating zero-arg lambdas
     as top-level constants.
   - Top-level lambdas are still functions even when arity is zero.

3. Direct self-reference remains a frontend error.
   - Preserve the direct self-reference check in canonicalization, but make sure
     its semantics match the SCC-level rule: only functions can be recursive.


## Phase 2: Make Monomorphic Top-Level Constant Failures Explicit And Fatal

Target architecture:

- Non-function top-level defs are monomorphic because only lambdas are
  generalized.
- If a monomorphic top-level constant is used at multiple incompatible types,
  type checking must report an explicit compile-time error.
- Those erroneous use-sites must be rewritten to crash/runtime-error nodes so
  execution cannot proceed as if the value were valid.

Detailed changes:

1. Track top-level non-function lookups during checking.
   - In `src/check/Check.zig`, record lookup exprs that refer to top-level
     non-function defs.

2. After solving, poison erroneous uses.
   - After all type solving/infinite-type detection is complete, rewrite any
     tracked lookup whose referenced top-level value resolved to `.err` into a
     runtime-error expr.
   - Add an explicit diagnostic for “this use-site was replaced with a crash
     because the referenced top-level value is erroneous”.

3. Stop relying on dead `erroneous_exprs`-only tagging for this behavior.
   - Use actual CIR mutation to runtime-error nodes, not side tagging.


## Phase 3: Delete MIR’s Named-Const Recursion Accommodation

Target architecture:

- MIR should assume illegal recursive values are already impossible because the
  frontend poisoned them.
- Top-level non-function constants lower directly as monomorphic const defs.
- There is no placeholder const registration path for value recursion.

Detailed changes:

1. Delete named-const placeholder lowering.
   - Remove `ensureNamedConstDefRegistered(...)` from `src/mir/Lower.zig`.
   - Replace it with direct const lowering/registration that assumes no value
     cycle can exist.

2. Tighten MIR invariants.
   - If MIR lowering ever encounters a value-level recursion situation that
     would previously have required a placeholder const, treat it as an
     unreachable invariant failure.

3. Re-audit MIR comments and APIs.
   - Remove stale wording that still implies recursive non-function consts are a
     supported executable concept.


## Phase 4: Add Explicit Regression Tests

The plan is not complete until the following cases have explicit tests:

1. Recursive non-function top-level defs are rejected before MIR.
   - Add canonicalization/type-checking coverage for direct and SCC-based
     recursive value defs.

2. Polymorphic top-level numeric constant misuse.
   - Example shape: `x = 5`, used at two incompatible types.
   - Must verify:
     - compile-time type-checking error is reported
     - execution crashes because the erroneous use-site(s) were rewritten to
       runtime-error/crash nodes

3. Polymorphic top-level empty list misuse.
   - Example shape: `xs = []`, used at two incompatible element types.
   - Must verify:
     - compile-time type-checking error is reported
     - execution crashes because the erroneous use-site(s) were rewritten to
       runtime-error/crash nodes

Suggested files:

- `src/check/test/type_checking_integration.zig`
- `src/eval/test/comptime_eval_test.zig`
- `src/eval/test/helpers.zig` only if its helper behavior needs to be fixed to
  actually assert the runtime crash instead of skipping execution


## Phase 5: Artifact Audit And Deletion Pass

After the architectural changes are in place, run a static audit and delete any
remaining traces of the old system.

Audit targets include:

- recursive non-function value accommodation in MIR
- named-const placeholder registration paths
- stale comments claiming MIR must cope with value recursion
- stale pattern/provenance/pattern-monotype/value-side-table names
- `erroneous_exprs`-only tagging paths that should now be real CIR mutation
- any remaining helper names that describe “recovery”, “repair”, “fallback”, or
  “best effort” value handling outside parsing/error reporting

Then run the audit again and delete anything the first pass missed.


## Success Criteria

This plan is complete only when all of the following are true:

- recursive non-function top-level defs are poisoned before MIR
- monomorphic top-level constant misuse reports an explicit type error and also
  crashes at runtime because use-sites were rewritten
- MIR no longer has named-const placeholder registration for value recursion
- the regression tests for numeric and list constants exist
- the first static artifact audit is clean
- the second static artifact audit is also clean
