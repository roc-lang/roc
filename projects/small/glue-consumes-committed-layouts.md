# Glue Consumes Committed Layouts

## Problem

`roc glue` generates host-language bindings (Zig, Rust, C headers) whose
struct layouts MUST byte-match the layouts the compiler commits for
runtime values — a wrong offset is silent memory corruption at the
host/app boundary. Today glue does not read those layouts; it
re-implements the layout arithmetic and hopes to agree.

The duplication is threefold. The layout store computes committed layouts
for codegen (`putNominalStructFields` in `src/layout/store.zig`). Glue's
`TypeTable` recomputes offsets, sizes, and padding in Zig
(`nominalRecordInDeclaredOrder` in `src/glue/glue.zig`, whose doc comment
says "matching the layout store's `putNominalStructFields`" — a comment
is the only binding). And the glue generator scripts recompute it a third
time in Roc: `src/glue/src/ZigGlue.roc` carries its own
`record_layout_from_fields`/`record_layout_from_fields_32`,
`type_layout_32`/`type_layout_64`, and `tag_union_layout_32` to emit both
64-bit and wasm32 struct variants and their comptime assertions.

History proves the hazard. PR 9721 changed nominal record layout
(declared field order plus explicit padding fields) and glue silently
kept emitting the OLD order the same day — shipped wrong bindings until
PR 9738. That fix shared only the field-ORDERING function; the
offset/size/padding arithmetic is still duplicated. Open issues in the
same area: 9824 (glue incorrectly sizes unresolved type variables as zero-sized
— visible in `getSizeAlign`'s out-of-range and `.unknown` fallbacks
returning `.{ .size = 0, ... }`) and 9645 (ZigGlue stale entrypoint ABI
declarations).

## Background

The compiler pipeline is: parse → canonicalize → type-check (producing
checked artifacts per module) → postcheck lowering → LIR → backends. Two
build-orchestration paths wrap the `Coordinator`
(`src/compile/coordinator.zig`): `roc check`/`roc build`/docs go through
`BuildEnv` (`src/compile/compile_build.zig`), and the dev-mode run path
goes through `lowerLirWithCoordinator` (`src/cli/main.zig`). Glue is a
BuildEnv consumer: `rocGlueInner` (`src/glue/glue.zig`) builds the
platform with a `BuildEnv`, walks the checked artifacts of the platform's
modules into a `TypeTable`, serializes that table into a Roc value, and
runs a generator script (`ZigGlue.roc`, `RustGlue.roc`, ...) that emits
the binding source text.

Layout facts a newcomer needs (`design.md` sections "Nominal Record Field
Order" and "Host Symbol ABI" are authoritative):

- Structural (anonymous) records lay out by a target-independent sort
  (descending sort key, then name); nominal records that opt in with an
  unnamed `_` field lay out in DECLARED order with C-style padding, so a
  Roc nominal record can mirror an exact C struct. PR 9738 extracted the
  ordering into `src/layout/field_order.zig`
  (`computeStructuralFieldOrder`), called by BOTH `src/layout/store.zig`
  and `src/glue/glue.zig`. (Older notes mention a
  `computeNominalFieldOrder` there; it does not exist — nominal
  declared-order layout is implemented separately in the store AND in
  glue, which is precisely this project's target.)
- Layouts are target-dependent via pointer width. PR 9831 ("consolidate
  the layout stores") made layout/LIR pointer-width independent with
  per-target resolution: the layout `Store` is parameterized by
  `target_usize` (`store.init(allocator, target_usize)`), and sizes
  resolve per target (`sizeAt(layout, target_usize)`). Glue's `TypeTable`
  is likewise parameterized but is constructed with
  `base.target.TargetUsize.native` only; the wasm32 numbers in generated
  Zig come from ZigGlue.roc's own Roc-side arithmetic.
- PR 9894 (issue 9865) is the model this project extends: glue used to
  resolve a nominal type's DECLARING MODULE by module-name text with a
  wrong-artifact fallback (indexing a CIR statement of the wrong kind →
  panic). The fix stores declared field order in checked artifacts
  (`CheckedNominalRecordField`, `declaredRecordFields` in
  `src/check/checked_artifact.zig`) and resolves declarations through the
  nominal REPRESENTATION keyed by artifact
  (`nominalDeclarationFor`/`artifactByKey` in `src/glue/glue.zig`,
  `importedNominalDeclarationModuleId`). Data glue needs now travels in
  the artifact instead of being re-derived — layout values should travel
  the same way.

Agreement today is checked only downstream: generated bindings embed
comptime size/align/offset assertions (see the `@compileError` emission
in ZigGlue.roc), and the glue CLI suite
(`src/cli/test/parallel_cli_runner.zig`, fixtures under `test/glue/` such
as `zig-layout-platform` and `platform-shapes`) compiles them. That
catches some drift at test time — it does not make drift impossible, and
it only covers shapes the fixtures happen to exercise.

## Evidence

- `src/glue/glue.zig`: `rocGlue`/`rocGlueInner`; `TypeTable.init(gpa,
  target_usize, &artifacts_by_key)` with `TargetUsize.native`;
  `nominalRecordInDeclaredOrder` (the duplicated offset walk: align-up
  per field, accumulate, round size up to max alignment);
  `getSizeAlignForRepr` (re-derives scalar/str/list/box sizes from
  `target_usize`; `.unknown` and out-of-range ids → size 0 — issue 9824);
  `nominalDeclarationFor` (PR 9894's artifact-keyed resolution).
- `src/layout/store.zig`: `putNominalStructFields`, `targetUsize`,
  `sizeAt`; tests "putNominalStructFields keeps a padding-free declared
  order verbatim" and "... inserts C-style padding".
- `src/layout/field_order.zig`: `computeStructuralFieldOrder`, shared by
  store and glue since PR 9738; its header comment states nominal
  declared-order layout is done "directly by the layout store and the
  glue generator, not here".
- `src/glue/src/ZigGlue.roc`: third copy of the arithmetic
  (`record_layout_from_fields`, `record_field_layout_32/64`,
  `type_layout_32/64`, `tag_union_layout_32`) and the emitted comptime
  assertions for `@sizeOf(usize) == 8` and `== 4`.
- `src/layout/abi/` (`x86_64.zig`, `aarch64.zig`, `wasm.zig`, `call.zig`):
  per-target C-ABI call classification lives compiler-side already.
- `src/check/checked_artifact.zig`: `CheckedNominalRecordField`,
  `declaredRecordFields`, `paddingFieldTypes`.
- PRs (roc-lang/roc): 9721, 9738, 9894, 9831. Issues: 9865, 9824, 9645.

## Solution design

Glue stops computing layout and CONSUMES the layout store's committed
layouts. Offsets, sizes, alignment, and padding for every glue-exposed
type come from the same source codegen uses.

1. **Expose committed layouts to glue.** Give glue access to a layout
   `Store` (or a serialized per-target layout view derived from one)
   resolved from the platform's checked artifacts — one store per target
   pointer width glue emits (today: native/64-bit and wasm32; the store
   is already `target_usize`-parameterized, so this is two
   instantiations, not new machinery). For each type reaching the
   `TypeTable`, record the committed `size`, `alignment`, and per-field
   offsets per target, alongside the existing shape description.
2. **DELETE glue's Zig-side arithmetic.** `nominalRecordInDeclaredOrder`'s
   offset walk (the align-up/accumulate/round-up block) and the
   size-deriving arms of `getSizeAlignForRepr` are deleted;
   declared-order field NAMES and padding spacers still come from
   `declaredRecordFields` (PR 9894's data), but every number attached to
   them is read from the store. An unresolved type variable has no
   committed layout — that becomes a glue-time error, fixing issue 9824's
   class (never a zero-sized field).
3. **DELETE the Roc-side arithmetic.** Extend the glue platform's
   `TypeTable`/`RecordField` Roc types (under `src/glue/platform/`) to
   carry per-target layout values, and delete
   `record_layout_from_fields*`, `type_layout_32/64`, and
   `tag_union_layout_32` from ZigGlue.roc (and any Rust/C equivalents).
   Generators format numbers; they do not derive them. The emitted
   comptime assertions REMAIN — they become a check of the host
   toolchain's agreement with committed layouts rather than of one
   duplicate against another.
4. **Keep per-language/per-ABI emission in glue, fed by committed
   values.** Deciding how to spell an extern struct, a discriminant type,
   or an entrypoint signature per language stays in the generators; call
   classification stays compiler-side in `src/layout/abi/`. Glue takes
   layout values as INPUTS everywhere.
5. **Direction (out of scope here): a host ABI manifest.** The long-term
   shape is a per-platform manifest — boundary symbol signatures plus
   committed layouts — derived once, hashed into the platform artifact,
   consumed by codegen AND glue, making host/app layout drift a load-time
   error rather than a codegen-time one. This project creates the
   committed-layout consumption path that manifest would formalize.

## What success looks like

- No offset/size/padding arithmetic exists under `src/glue/` — `grep -rn
  "alignment - rem\|% field.alignment" src/glue` matches nothing, and
  ZigGlue.roc contains no `*_layout_*` arithmetic functions.
- A compiler layout-rule change cannot produce stale bindings: glue reads
  the changed store, so regenerated bindings move in lockstep, and the
  emitted comptime assertions verify the host compiler agrees.
- Issue 9824's class is closed: a type with no committed layout is a
  reported glue error naming the type, never a zero-sized field.
- Every layout number in generated output is traceable to a layout-store
  read for an explicit target.

## How to evaluate the result

### Correctness ideal

End-to-end FFI layout probes: for EVERY glue-exposed type shape —
structural records, nominal records with the `_` padding opt-in, boxed
values, tag unions including single-variant and >u8 discriminant counts,
slices/strings — a generated host program asserts
`offsetof`/`sizeof`/`alignof` equality against compiler-emitted values,
per target, extending the FFI e2e coverage added around PR 9721
(`src/cli/test/parallel_cli_runner.zig` glue suite + `test/glue/`
fixtures). Plus repros for issues 9824 and 9865. Ideal state: it is not
possible to construct a platform type whose generated binding disagrees
with the committed layout, because there is exactly one producer of
layout numbers.

### Performance ideal

Glue generation time unchanged or better — reading committed layouts
beats recomputing them (and the per-target store instantiations are
amortized over the whole platform). Zero compiler-runtime cost: the
layout store already computes everything codegen needs; this project only
adds reads at glue time. Generated-binding compile time is unchanged (the
comptime assertions already exist).

## Tests to add

- The probe matrix above, parameterized over type shape × target
  (native 64-bit and wasm32 at minimum), asserting offset/size/alignment
  agreement end to end.
- A drift canary: mutate a layout rule in a test build (e.g. flip the
  structural sort tie-break) and verify glue output changes in lockstep —
  or, if the mutation is host-visible, that the generated comptime
  assertions fail loudly. Either outcome proves no silent stale-binding
  path exists.
- Issue 9824 repro: a platform exposing an unresolved type variable must
  produce a glue-time error naming the type, not a zero-sized field.
- Issue 9865 repro stays green: nominal record declared in one platform
  module, re-exported and used from another, glued and byte-checked.

## Related projects

- [Content-based nominal identity](../big/content-based-nominal-identity.md)
  — fully fixes the declaring-module resolution class PR 9894 patched;
  this project consumes its artifact-keyed resolution model.
- [Cache hardening](../small/cache-hardening.md) — committed layouts read
  from cached artifacts are only as trustworthy as the cache's
  enforcement edges.
