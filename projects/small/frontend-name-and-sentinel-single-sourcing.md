# Frontend Name and Sentinel Single-Sourcing

## Problem

The frontend pipeline has one genuinely enforced registry for builtin
types — `builtin_type_specs` in `src/canonicalize/CIR.zig:156` with
its registry hash validated at builtin load
(`src/canonicalize/BuiltinStatic.zig:59-90`) — but several downstream
sites re-encode facts the registry (or another single source) already
owns. A July 2026 duplication audit found six such seams; each is a
small deletion or redirection, and each is the exact
name-keyed/hand-mirrored pattern that produced previous bug rounds.

1. **A duplicate ident→NumKind map.** `builtinNumKindFromTypeName`
   (`src/check/Check.zig:4521`) is a hand-written chain re-deriving
   what `BuiltinIndices.numKindFromIdent` (`CIR.zig:124`) already
   computes from the spec table. A renamed or added numeric type
   silently stops being recognized by the hand copy.
2. **Bool's discriminant hardcoded.** `src/postcheck/solved_lir_lower.zig:6083`
   and `:6088` emit `CFSwitchBranch{ .value = 1, .body = true_body }`
   — baking "True == 1", which is true only because tag discriminants
   come from alphabetical sorting and "False" < "True" lexically.
   Nothing links the literal to the sort convention or to `mkBool`'s
   construction order (`src/types/store.zig:582-585`). A collation
   change would silently invert every boolean branch.
3. **Method-name strings bypass their own registry.**
   `structural_method_kinds` (`src/check/static_dispatch_registry.zig:849`,
   doc comment at `:841-848`: "this single source") is not consulted
   everywhere: postcheck re-types `"is_eq"`
   (`src/postcheck/monotype/lower.zig:8990, 23331, 27191, 27211`),
   `"to_hash"` (`:23432`), `"parser_for"` (`:22967-23006`), and
   `"encoder_for"` (`:23027-23050`) as raw literals;
   `"from_numeral"`/`"from_quote"` are separately re-interned in
   `src/check/canonical_type_keys.zig:727-728` and
   `static_dispatch_registry.zig:1313, 1353`.
4. **A hand-written bidirectional builtin-name map.**
   `TypeAnnotation.Builtin.toBytes`/`fromBytes`
   (`src/canonicalize/TypeAnnotation.zig:419-455`) spell every builtin
   type name twice more; the qualified-name strings in `CommonIdents`
   (`src/canonicalize/ModuleEnv.zig:135-181`) spell them again, as
   does the deliberately-partial numeric list `flat_types` in
   `src/postcheck/monotype/lower.zig:10046`.
5. **Row-sort comparators ×5.** Record-field/tag canonical ordering
   (ascending byte order of names) is implemented independently in
   `src/types/types.zig:672, 721`, inline lambdas in
   `src/check/unify.zig:1998-2004, 2430-2436`,
   `src/check/canonical_type_keys.zig:431-485, 557`,
   `src/check/canonical_names.zig:535-551`, and
   `src/check/snapshot.zig:104`. They agree only because each bottoms
   out in byte comparison; discriminant assignment depends on that
   agreement.
6. **Default-cased cross-phase lowering switches.** The
   checked→monotype lowering (`lowerExpr` at
   `src/postcheck/monotype/lower.zig:9206` and `lowerExprWithType` at
   `:9378`) uses `else => {}`/partial
   switches where the codebase's own parallel-enum conversions
   (`reconstructCheckedExprData`, `snapshot.zig`'s deep-copies) are
   exhaustive — the one cross-phase seam where a new checked variant
   slips through silently.

## Background

The healthy anchors: the spec-table/`BuiltinIndices` machinery is
comptime-wired (`@field` over spec names) and hash-validated;
`Ident.textLessThan` (`src/base/Ident.zig:35`) already exists as the
canonical comparator; `Ident.zig:25-27` shows the named-constant
pattern for method names (`PLUS_METHOD_NAME`) that the rest of the
method strings never adopted; and `tagIndex` in `solved_lir_lower.zig`
already computes discriminants from tag names in the general case —
the Bool fast path just doesn't use it.

## Evidence

- Cited file:line pairs above; each duplication is verifiable by
  reading the two sites side by side.
- `static_dispatch_registry.zig:786-793` — the "single source" doc
  comment contradicted by postcheck's literals.
- `mkBool`'s `{ False, True }` order plus the alphabetical-sort
  discriminant assignment (`solved_lir_lower.zig` `tagIndex`,
  `.discriminant = @intCast(i)`) — the two facts the `.value = 1`
  literal silently couples.

## Solution design

1. Delete `builtinNumKindFromTypeName`; call
   `indices.numKindFromIdent`.
2. Derive the Bool branch value via `tagIndex(bool_ty, true_tag)` (or,
   if the lookup is deemed too costly for this hot path, keep the
   literal but add a debug assertion at Bool-layout construction that
   `tagIndex(Bool, "True") == 1`, with a comment naming the
   alphabetical-order dependency).
3. Add `MethodNameId`-style named constants (or route through
   `structural_method_kinds`) for every compiler-known method name;
   postcheck and check consume them.
   `grep -rn '"is_eq"\|"to_hash"\|"parser_for"\|"encoder_for"\|"from_numeral"\|"from_quote"'
   src/check src/postcheck` should match only the constants module and
   tests.
4. Generate `toBytes`/`fromBytes` and the `CommonIdents` qualified
   strings from `builtin_type_specs` (comptime loops over the spec
   table).
5. One row comparator on `Ident.textLessThan`, consumed by all five
   sites.
6. Make the checked→monotype lowering switches exhaustive; the
   deleted `else` arms become explicit "unreachable for this phase"
   arms per variant, so a new checked variant is a compile error here
   as it already is in the reconstruct/deep-copy twins.

## What success looks like

Every criterion below must hold; the project is not done until all do:

- Each of the six greps/inspections above shows one encoding left.
- The Bool coupling is either derived or asserted-with-comment.
- A new `CheckedExprData` variant fails the monotype-lowering build
  until classified (demonstrated with a dummy variant in a scratch
  build).
- Snapshot corpus unchanged (`git diff test/snapshots` empty); all
  eval/backend tests green.

## How to evaluate the result

### Correctness ideal

No frontend fact about builtin names, method names, row order, or
Bool's representation exists in more than one place; the remaining
single sources are the ones the registry hashes and comptime wiring
already guard.

### Performance ideal

Neutral or better: comptime-generated maps replace hand chains;
the comparator unification is the same byte comparison; the Bool
derivation (if chosen over the assertion) is one layout lookup at
lowering time — measure with the stress-profiling harness to confirm
it is invisible, and take the assertion route if not.

## Tests to add

- Mutation drill: rename a numeric type in `builtin_type_specs` and
  confirm every consumer follows or fails loudly.
- A Bool-discriminant assertion test (True's discriminant equals the
  emitted branch value).
- Comptime totality: every `structural_method_kinds` entry has a
  consumer-visible constant and vice versa.

## Related projects

- [cross-phase-coverage-parity-tests.md](cross-phase-coverage-parity-tests.md)
  — the parity-suite pattern; item 6 here is the structural
  (exhaustiveness) complement to that project's divergence suite.
- The landed single-source builtin registration
  (`src/builtins/builtin_registry.zig`) — the same
  registry-consolidation move, already made for runtime symbols.
