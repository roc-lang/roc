# Enroll NodeStore in the Serde Contracts

## Problem

`src/canonicalize/NodeStore.zig` backs the CIR with ~20 parallel
`SafeList`s, and that field list is written out by hand, in matching
order, in eight places: the struct definition (`NodeStore.zig:23-43`),
`clone` (`:354`), `deinit` (`:383`), `relocate` (`:410`), the
`Serialized` struct (`:5184`), `serialize` (`:5208`),
`deserializeInto` (`:5258`), and `deserializeWithCopy` (`:5286`).
Adding one list means eight coordinated edits; forgetting the
`serialize` or `relocate` edit is silent (a zeroed or dangling
sub-store, not a compile error).

The enforcement machinery this file needs already exists thirty lines
of import away and is used by its sibling: `ModuleEnv.Serialized`
invokes `assertBidirectionalFieldSet`
(`src/canonicalize/ModuleEnv.zig:3296`, helper in
`src/collections/serde_validation.zig:40`), making owner/Serialized
field drift a compile error. `CheckedModuleArtifact` is enrolled too
(`src/check/checked_artifact.zig:22908`). NodeStore — the largest
hand-enumerated serialization root in the codebase — is not.

A same-family loose end in the parser: `src/parse/NodeStore.zig:150-158`
hardcodes `AST_HEADER_NODE_COUNT = 6`, `AST_STATEMENT_NODE_COUNT = 13`,
`AST_PATTERN_NODE_COUNT = 17`, and `AST_TYPE_ANNO_NODE_COUNT = 11` as
literals, while `AST_EXPR_NODE_COUNT` on the adjacent line is
correctly `std.meta.fields(AST.Expr).len`. The four literals are
consumed as test-coverage floors and can silently drift from the real
variant counts; the canonicalize-side equivalents
(`src/canonicalize/NodeStore.zig:437-474`) are all comptime-asserted.

## Background

The existing round-trip tests (`NodeStore.zig:5314, 5355, 5430`) only
exercise the lists they populate, so an omitted field passes. The
layout-hash tripwire (`MODULE_ENV_VERSION_HASH`) catches layout changes
to the `Serialized` struct for cache invalidation, but not a field
present in both structs yet missing from a function body. Note the
residual gap even where enrollment exists: `assertBidirectionalFieldSet`
checks field-*set* equality, not that `serialize`'s body touches every
field — which is why step 2 below (comptime iteration driving the
bodies) is the real fix and the audit is the floor.

## Evidence

- The eight hand-lists cited above; diff any two to see the manual
  ordering contract.
- `grep -rn assertBidirectionalFieldSet src/` — exactly two enrolled
  roots today; NodeStore absent.
- `src/parse/NodeStore.zig:150-158` — four literals beside one derived
  count in the same block.

## Solution design

1. **Enroll.** Add the `assertBidirectionalFieldSet` comptime audit to
   `NodeStore.Serialized` (with the owner-only fields — `gpa`,
   `scratch` — declared, following `ModuleEnv.zig:3277-3289`).
2. **Drive the bodies from the type.** The ~20 lists are uniform
   (`SafeList(T)` fields); `clone`/`deinit`/`relocate`/`serialize`/
   `deserializeInto`/`deserializeWithCopy` become
   `inline for (@typeInfo(NodeStore).@"struct".fields)` loops with the
   two non-uniform fields special-cased. After this, adding a list is
   one field declaration; the eight-way contract ceases to exist
   rather than being audited.
3. **Fix the parse counts.** Replace the four literals with
   `std.meta.fields(...).len`, matching `AST_EXPR_NODE_COUNT`.
4. Coordinate with the serialization-root registry proposed in
   [cache-and-identity-residuals.md](cache-and-identity-residuals.md)
   item 2 — NodeStore is the standing example of the unenrolled-root
   hazard that registry exists to close; enrolling it should go
   through the registry if that lands first.

## What success looks like

Every criterion below must hold; the project is not done until all do:

- Adding a `SafeList` field to `NodeStore` and recompiling either
  just works (comptime-driven bodies) or fails at comptime naming the
  field — demonstrated by adding a dummy field in a scratch build.
- No function in `NodeStore.zig` enumerates the backing lists by hand.
- Round-trip tests populate every list (comptime-enumerated fixture so
  a new field fails the test until covered).
- The four parse-side counts are derived;
  `grep -n 'NODE_COUNT = [0-9]' src/parse/NodeStore.zig` is empty.
- `MODULE_ENV_VERSION_HASH` golden value and cache tests updated
  intentionally if the `Serialized` layout changes as a side effect
  (it should not — field order is preserved).

## How to evaluate the result

### Correctness ideal

The serialized form and the runtime form of the CIR store cannot
disagree about what fields exist, and the serialize/deserialize/
relocate/clone bodies cannot omit a field, because they are generated
from the same declaration the struct is.

### Performance ideal

Byte-identical serialization output for unchanged inputs (assert via
the existing round-trip tests plus a digest comparison on a snapshot
corpus). The `inline for` monomorphizes to the same per-field code the
hand-written version contains; no runtime dispatch is introduced.

## Tests to add

- Comptime-enumerated full-population round-trip (every list
  non-empty, byte-compare after round-trip).
- The dummy-field drill (documented, run manually or as a
  compile-error test if the harness supports it).

## Related projects

- [cache-and-identity-residuals.md](cache-and-identity-residuals.md)
  — item 2 (auto-enrolled serialization roots) is the general form of
  this project's step 1.
