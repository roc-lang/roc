# Compile-Time Constants to Static Data Plan

## Goal

Roc should evaluate every concrete top-level value in every checked module during
checking finalization, even when that value is not reachable from any runtime
root. This preserves compile-time semantics: `dbg`, `expect`, compile-time
crashes, literal conversion errors, and static exhaustiveness diagnostics must be
reported for all top-level values that are valid compile-time roots.

Separately, code generation should only materialize reachable compile-time data
into the final binary. Unreachable constants should be evaluated and stored in
checked compiler metadata, but they should not become readonly data sections in
the object or wasm output.

Runtime code that uses a reachable compile-time constant should refer to one
static data object. It should not reconstruct that constant by allocating heap
lists, writing list bytes from inline immediates, or rebuilding surrounding
records in function bodies.

## Current Behavior

The checked artifact already has most of the semantic pieces:

- `CompileTimeRootTable.fromModule` publishes roots for all non-procedure
  top-level definitions in a module, and appends selected hoisted constants.
- `RootRequestTable.fromModule` adds compile-time requests for concrete roots
  that are not blocked by an unbound platform requirement.
- Checking finalization evaluates requested compile-time roots and stores their
  results in `ConstStore`.
- `ConstStore` can represent nested lists, records, tuples, tags, boxes,
  nominals, strings, function values, and crashes.

The problem is in the lowering and materialization boundary:

- Runtime lowering handles `top_level_const`, `imported_const`, and
  `selected_hoisted_const` by restoring the `ConstStore` node into a runtime
  expression tree.
- Restoring a `List(U8)` creates a list expression again, which later becomes a
  runtime list allocation plus stores of inline constants.
- Static data materialization is currently wired for provided data exports, not
  for arbitrary internal constants reachable from runtime roots.
- The hoisted-root selector is intentionally sparse. It finds many closed local
  values, but branch bodies are checked with hoist selection suppressed, so
  closed subexpressions inside runtime-shaped functions are often not selected.

For rocci-bird this means the sprite sheet top-level values are compile-time
data semantically, but runtime `update` still contains list allocations and
inline byte stores for those sprites. The final wasm has sprite bytes as code
immediates rather than as shared static data.

## Desired Model

There are three separate products of compilation:

1. Checked semantic results
   - Include every concrete top-level value in every checked module.
   - Include every selected top-level-equivalent hoisted constant.
   - Exist to report diagnostics and to make constants available to later
     compilation stages.

2. Reachable static-data roots
   - Include only compile-time constants reachable from runtime/export roots.
   - Include reachable hoisted constants that replace runtime subexpressions.
   - Exclude top-level constants that were evaluated only for semantic reasons.

3. Emitted readonly data
   - Materialize the reachable static-data roots and their reachable
     suballocations exactly once.
   - Preserve sharing between constants. If two static records point at the same
     list, the emitted static data should point at one shared list backing.
   - Be consumed by runtime lowering as explicit static references, not restored
     runtime construction.

Backends must not infer any of this from names, object symbols, wasm data
segments, or restored expression shape. Earlier stages must provide explicit
static-constant references and layout/materialization plans.

## Phase 1: Lock Down Existing Semantics

Add focused tests around checked finalization before changing emission:

- A module with an unused top-level constant containing `dbg` should still
  produce the expected compile-time `dbg` output.
- A module with an unused top-level `expect` should still run that `expect`.
- A module with an unused top-level constant that crashes during compile-time
  evaluation should report the compile-time crash.
- Imported modules should get the same treatment. If an imported module contains
  an unused top-level compile-time problem, compiling an app that imports it
  should still report that problem.
- Non-concrete roots and roots blocked by unbound platform requirements should
  keep their current explicit behavior until their type/relation data exists.

These tests should assert the semantic result, not binary contents. Their
purpose is to prevent confusing "only reachable constants are evaluated" with
"only reachable constants are emitted."

## Phase 2: Define Static Constant Reachability

Add an explicit post-check/lowering data structure for static constants reachable
from runtime code. It should be computed from root requests and checked
references, not from backend output.

Inputs:

- Runtime root requests selected for the build target.
- Checked resolved value refs used by reachable procedure templates.
- Top-level const refs.
- Imported const refs.
- Selected hoisted const refs.
- Platform-required const refs when they are part of a reachable runtime path.

Outputs:

- A stable list of reachable static const uses.
- Each entry should identify:
  - the owning checked module,
  - the `ConstRef`,
  - the checked type requested at the use site,
  - the const node stored by finalization,
  - whether it is externally visible or internal,
  - a stable static symbol id.

This list must include dependencies between static constants. If static const A
contains a function value or nested constant that requires another materialized
object, that dependency must be explicit in the static-data plan.

Do not use dead-code elimination after emitting all constants as the main design.
The compiler should emit only the static constants reachable from runtime roots.

## Phase 3: Extend Hoisted Constants

The existing hoist selector is too sparse for the rocci-bird cases. It should be
extended so pure, closed, compile-time-known subexpressions inside otherwise
runtime-dependent expressions can become selected hoisted constants.

Important case:

```roc
create_high_score_anim = |frame_count| {
    last_updated: frame_count,
    cells: [
        { frames: 5, sprite: Sprite.sub_or_crash(high_score_sprite_sheet, { src_x: 0, src_y: 0, width: 32, height: 16 }) },
        ...
    ],
}
```

The full `Animation` value is runtime-dependent because `last_updated` depends
on `frame_count`. The `Sprite.sub_or_crash(...)` calls and their surrounding cell
records do not depend on `frame_count`; those should become hoisted constants.

Implementation direction:

- Keep the existing rule that observable compile-time effects are not silently
  moved into ordinary runtime execution.
- Preserve explicit facts from checking. Do not make a backend or post-check
  stage rediscover purity from syntax.
- Stop treating all branch bodies and runtime-shaped enclosing expressions as
  complete hoist-selection barriers. Instead, allow closed child candidates to
  be selected when the enclosing expression cannot cover them.
- Add tests for:
  - closed call inside a record with one runtime field,
  - closed list element inside a runtime-dependent list,
  - closed call inside `if`/`match` branch bodies when the selected expression
    itself has no contextual dependency,
  - no hoist when the call body contains `dbg` or `expect`,
  - no hoist when the selected expression depends on branch-local binders or
    function parameters.

The result should be that `Sprite.sub_or_crash(sheet, literal_region)` becomes a
checked hoisted const root when all of its inputs are compile-time constants.

## Phase 4: Add Internal Static Data Materialization

Generalize `static_data_exports` into a static-data materializer that can handle
both:

- provided data exports, and
- internal reachable static constants.

The existing writer already knows how to materialize nested records and lists
from `ConstStore`; it should be reused rather than duplicated. The new API
should accept the reachable static-constant plan from Phase 2 and return static
data objects for the backend.

Required behavior:

- Generate private/internal symbols for ordinary reachable constants.
- Generate public symbols only for provided data exports.
- Materialize records, lists, strings, boxes, tags, nominals, and function values
  according to target layout.
- Preserve Roc refcount/header requirements for static allocations.
- Keep relocations explicit for pointers between static objects.
- Keep target pointer width and alignment target-specific.

This phase should not change runtime lowering yet. It should first be possible
to materialize reachable internal constants and inspect the produced static data
objects in tests.

## Phase 5: Intern Shared Static Suballocations

Static data must preserve sharing instead of duplicating large payloads.

For rocci-bird, all sub-sprites produced from a sprite sheet should point at the
same `List(U8)` backing bytes as the sprite sheet. `Sprite.sub_or_crash` returns
`{ ..sprite, region: new_region }`, so sharing is semantically correct: only the
region changes.

Implementation direction:

- Add a materialization cache keyed by checked module plus const node identity
  where identity is sufficient.
- For values that can be structurally equal but come from different evaluated
  roots, add structural interning where needed. Lists of bytes are the important
  first case.
- Extend `ConstStore` if necessary so `List(U8)` can store shared byte backing
  data similarly to how strings store shared backing data today.
- Avoid reusing static objects when the values are not layout-compatible for the
  requested type.

Tests:

- Two top-level records pointing at the same list should materialize one list
  payload.
- A hoisted subrecord produced from a top-level record should share the same
  nested list allocation.
- Two distinct but byte-identical `List(U8)` constants should share backing only
  if the chosen interning rule explicitly says they should.

## Phase 6: Lower Runtime Const Uses to Static References

Change Monotype/LIR lowering so runtime uses of reachable static constants do
not call `restoreConstNodeAtType`.

Instead:

- A `top_level_const`, `imported_const`, or `selected_hoisted_const` use that is
  in the reachable static-constant plan should lower to an explicit static const
  reference expression or LIR statement.
- The static reference must carry the static symbol id and layout chosen by the
  static-data plan.
- If a compile-time constant is not in the reachable static plan, runtime code
  should not reference it. If it does, that is a compiler bug in reachability
  collection.
- The old restore path remains appropriate for checking finalization, because
  finalization lowers compile-time roots to executable code in order to evaluate
  them. Runtime build lowering should use the static-reference path.

Backends should receive explicit LIR for "load/address this static constant."
They should not choose static-vs-runtime construction themselves.

Tests:

- A reachable top-level `List(U8)` used by `main!` emits no runtime list
  allocation for that list.
- A reachable top-level record containing a list emits a static record and
  static list payload.
- A reachable hoisted constant inside a runtime function emits a static object
  and runtime code references it.
- An unreachable top-level constant is evaluated during checking but does not
  appear in static data output.

## Phase 7: Backend Support

Wire the new static data objects into all relevant backend paths.

For wasm:

- Static objects should become wasm data segments and symbols.
- Runtime code should reference their addresses instead of constructing them.
- Imported memory and global base settings must still be honored.
- Binaryen optimization should be allowed to optimize around the static data,
  but correctness must not depend on Binaryen discovering and removing runtime
  reconstruction code.

For native/dev object paths:

- Static objects should go into readonly data sections when supported.
- Relocations between static objects should be represented through the existing
  relocation machinery.
- The interpreter/LirImage path should either support static refs explicitly or
  keep a separate finalization-only restore path, but it must not silently
  diverge from compiled semantics.

## Phase 8: Regression Tests for rocci-bird Shape

Add compiler tests that capture the rocci-bird pattern without depending on the
entire game as the only signal.

Minimum focused Roc fixture:

- A `Sprite` record with `data : List(U8)` and `region`.
- `Sprite.new`.
- `Sprite.sub_or_crash` or equivalent pure `sub` helper.
- A top-level `sprite_sheet`.
- A function that returns a runtime-dependent record containing a list of cells
  whose `sprite` fields are compile-time sub-sprites.

Expected assertions:

- `sprite_sheet` is evaluated into `ConstStore`.
- Each compile-time sub-sprite is selected as a hoisted const root.
- Runtime lowering references static constants for the sheet and cells.
- The static data output contains the sheet bytes once.
- The static data output contains the surrounding sprite/cell records.
- Runtime function body does not contain list allocation/stores for the sheet
  bytes.

This fixture should be small enough to debug quickly, but it should preserve the
specific shape that matters for rocci-bird.

## Phase 9: Full rocci-bird Verification

Build the current compiler in optimized mode:

```sh
zig build roc -Doptimize=ReleaseFast --summary all
```

Compile rocci-bird with the local wasm4 platform:

```sh
zig-out/bin/roc build --opt=size --no-cache \
    --output=/tmp/rocci-bird-static.wasm \
    /home/rtfeldman/code/roc-wasm4/examples/rocci-bird.roc
```

Disassemble the wasm:

```sh
/tmp/binaryen-size/binaryen-version_130/bin/wasm-dis \
    /tmp/rocci-bird-static.wasm \
    -o /tmp/rocci-bird-static.wat
```

Confirm the expected static-data result:

- The five top-level sprite sheets are present in wasm data segments or static
  data symbols:
  - `rocci_sprite_sheet`
  - `ground_sprite`
  - `pipe_sprite`
  - `plant_sprite_sheet`
  - `high_score_sprite_sheet`
- The surrounding `Sprite` records are present as static data, not rebuilt in
  `update`.
- The animation cell records derived from `Sprite.sub_or_crash(...)` are present
  as static data where they are reachable.
- The sub-sprite records point at the same static `List(U8)` backing bytes as
  their source sheet.
- `update` no longer contains repeated inline sprite byte immediates.
- `update` no longer calls `roc_builtins_list_with_capacity` for the static
  sprite sheets or animation cell lists.
- The wasm data size increases by the sprite/static-record payloads, while code
  size drops because byte-store sequences are gone.
- The final `--opt=size` wasm still runs in WASM-4.

Useful comparison checks:

```sh
wc -c /tmp/rocci-bird-static.wasm
rg "roc_builtins_list_with_capacity" /tmp/rocci-bird-static.wat
rg "i64.const 6148862581846747658" /tmp/rocci-bird-static.wat
rg "i64.const 2882315306433249320" /tmp/rocci-bird-static.wat
```

Those constants were previously evidence that sprite bytes were inline in code.
After this work, they should either be absent from function bodies or appear only
as static data bytes, depending on the disassembler representation.

The final acceptance condition is not just a smaller file. The acceptance
condition is that reachable Roc top-level and hoisted constants are represented
as shared static data, while unreachable top-level constants are still evaluated
for compile-time semantics but are not emitted into the binary.
