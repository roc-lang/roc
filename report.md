# Boxy LIR WIP Report

This report is a handoff for the current WIP state of the boxy LIR / `--specialize=no` implementation investigation. It is intentionally detailed and includes design context, implementation work completed so far, debugging history, current failures, and what I learned while working through the first real execution bugs.

## Current Snapshot

The current worktree is a WIP, not a finished implementation. The modified source files at the time this report was written are:

- `src/base/LowLevel.zig`
- `src/backend/dev/LirCodeGen.zig`
- `src/backend/llvm/MonoLlvmCodeGen.zig`
- `src/backend/wasm/WasmCodeGen.zig`
- `src/eval/interpreter.zig`
- `src/lir/arc.zig`
- `src/postcheck/boxy/lower.zig`
- `report.md`

There were no lingering `zig build`, `roc`, or `builtin_compiler` processes after the interrupted run. The final build/test command was interrupted by the user. Immediately before that interruption, I had corrected an instrumentation compile error, but I had not reverified that the current tree builds. This WIP commit therefore should be treated as a diagnostic checkpoint, not as a green build.

The current tree contains a large amount of temporary debug instrumentation. Some of it is broad and noisy, including hard-coded proc IDs, statement IDs, and layout IDs from the `test/echo/all_syntax_test.roc` debugging session. This instrumentation should be removed or converted into structured assertions before this work is considered for a normal PR.

## Project Goal

The original idea started as a new backend named `boxy`, but the design evolved during discussion. The long-term target is not a separate codegen backend. The target is a new post-check lowering mode that produces LIR without lambda-set specialization.

The desired external CLI shape is:

- Add a new CLI flag named `--specialize=yes` or `--specialize=no`.
- Internally, do not call these modes `.yes` and `.no`; use self-describing names:
  - `.lss`: lambda-set specialization, the current specialized pipeline.
  - `.boxy`: closure boxing and vtable/dictionary passing, the new unspecialized pipeline.
- Default `--opt=dev` to `--specialize=no`.
- Default `--opt=size` and `--opt=speed` to `--specialize=yes`.
- Apply the flag to build/run/test/codegen commands that compile executable code.
- Do not apply the flag to `roc check`; checking stays unaffected.

The new `.boxy` mode is intended to compile faster by skipping the expensive specialization and lambda-set machinery. In exchange, runtime code is expected to be slower because higher-order and polymorphic operations use boxed closures, erased calls, descriptors, and vtable/dictionary dispatch instead of compile-time specialization.

The end-state pipeline should look conceptually like this:

```text
checked modules / CIR + Types
  -> boxy representation planning
  -> boxy lowering directly to LIR
  -> LIR ARC
  -> existing LIR consumers
     -> LIR interpreter
     -> dev backend machine code
     -> LLVM backend machine code/object code
     -> wasm backend where supported
```

This is the important architectural shift: "boxy" is a LIR-lowering strategy, not a final code generator. Once LIR has been produced, the existing backend machinery should be reused as much as possible.

## Non-Negotiable Design Invariant: Host ABI Must Not Change

The most important invariant from the design discussion is that this work must not change the host boundary or ABI representation in any way.

That has several concrete implications:

- Boxy mode cannot "box everything" globally, because doing so would change host-visible layouts.
- Values crossing a Roc/host boundary must have the same runtime representation they have today.
- Existing explicit `Box(...)` ABI behavior must remain exactly as it is today.
- Polymorphic values that already require host-visible boxes must use the same representation as explicit `Box(...)`; boxy mode must not introduce double boxing.
- Vtables, dictionaries, descriptors, and other polymorphism metadata must not be stored alongside host-visible values in a way that changes their layout.
- If extra metadata is needed to interpret a value in boxy mode, it must be passed through compiler-controlled hidden parameters or LIR metadata paths, not embedded into host ABI values.
- Entry wrappers and host-shaped wrappers are required to materialize values into host-shaped layouts before they cross the boundary.
- Compile-time evaluation is unaffected and should keep using the existing compile-time path.

This invariant is the reason the design cannot be "everything becomes boxed everywhere." Boxy is allowed to use boxes internally for erased polymorphic or closure-captured values, but it must always respect the existing concrete ABI at boundaries.

## Long-Term Design Shape

### Specialization Modes

There should be one explicit specialization choice in compiler options:

```text
SpecializeMode.lss
SpecializeMode.boxy
```

The CLI spelling can be `--specialize=yes|no`, but the compiler internals should use `.lss` and `.boxy` so the code clearly says what semantic pipeline is selected.

The specialized path remains the current lambda-set-specializing pipeline. The boxy path should bypass specialization and lambda-set solving entirely. A correct boxy pipeline should never need to inspect a lambda set, because higher-order calls are represented with erased callables and runtime dispatch instead.

### Representation Planning

The boxy lowerer needs an explicit representation plan before lowering expressions to LIR. That plan needs to know, for each checked type and worker:

- The runtime layout used for the value itself.
- Whether the value needs a descriptor.
- How nested polymorphic fields are described.
- How tag payload variants are described.
- Which dictionary/vtable requirements are needed for operations on polymorphic values.
- Which function captures require descriptors.
- Which function arguments and return values need hidden descriptor arguments.
- Which wrappers are host-shaped and must preserve existing ABI layouts.

This representation planning must be explicit. Later compiler stages must not recover missing information by guessing from shapes, proc names, debug names, or layout coincidences. That is especially important in this codebase because heuristics and fallbacks are forbidden outside parsing and error reporting.

### Descriptors

The implementation uses boxy descriptors (`BoxyDesc`) as runtime metadata for values whose static monomorphic representation has been intentionally erased.

The descriptor model currently includes concepts such as:

- `payload_layout`: the runtime layout of the value being described.
- `nested_descs`: descriptors for nested polymorphic fields.
- `tag_variants`: dynamic tag-union variant metadata.
- `tag_ext_desc`: extension metadata for open tag unions.
- copy/drop helpers or equivalent RC information needed for dynamic payloads.
- optional debug checked type information used while diagnosing bugs.

Descriptors can come from different sources:

- Static descriptors emitted once because their shape is known statically.
- Template descriptors parameterized by hidden descriptor arguments.
- Runtime descriptors materialized from existing descriptor locals or copied from box payload metadata.

The important lesson from debugging is that descriptors are not ancillary. They are part of the semantic value in boxy mode. If ARC or lowering drops a descriptor reference, later materialization can produce a value with the right byte width but the wrong interpretation.

### Closure Representation

The long-term closure model is:

- Capturing functions are represented as erased callable values.
- The callable value contains a code pointer/proc identity plus boxed capture payload.
- Captures that are themselves polymorphic need descriptors.
- Non-capturing functions can still use the same erased callable shape if doing so simplifies compile time. A null or empty capture payload is acceptable if it makes the runtime shape uniform.
- Higher-order calls go through erased-call LIR, not lambda-set-specialized direct calls.

Runtime performance is expected to be slower than the specialized path. However, the long-term ideal is not "slow everywhere." The compiler can still use exact representations and direct calls where the checked type makes that explicit without invoking lambda-set specialization. For example, non-polymorphic non-capturing functions should not need avoidable heap allocation once the representation plan can prove their exact shape.

### Polymorphism and Vtable/Dictionary Passing

The boxy path needs explicit runtime metadata for polymorphic operations. For example:

- A value of type `a` needs descriptor metadata to be copied, dropped, boxed, unboxed, inspected, or compared if the operation depends on representation.
- Operations constrained by abilities need dictionaries or vtables.
- Dictionary/vtable arguments should be hidden compiler-managed values, not fields stored in ordinary user values.
- For functions such as `a -> a`, there is no way to compile one machine function that manipulates arbitrary `a` without either passing representation metadata or boxing the value. Boxy mode should pass the needed descriptor/vtable metadata explicitly.

The current direction is to lower vtable/dictionary dispatch in LIR rather than making each backend understand a high-level vtable-call concept. The advantage is one semantic implementation shared by all LIR consumers. The possible downside is that backend-specific optimizations, such as register pinning or call sequence specialization, are harder if all calls are already lowered to a generic LIR shape. Based on the current state of the codebase, correctness and keeping one dispatch semantics in LIR matter more at this stage.

### ARC Boundary

Backends are not allowed to invent reference-counting behavior. They must dumbly follow explicit LIR `incref` and `decref` statements.

That means boxy lowering and LIR ARC insertion must make all ownership transfers explicit. This is particularly delicate for:

- `assign_boxy_box`
- `assign_boxy_unbox`
- erased callable values
- runtime descriptors
- list elements that are dynamically boxed
- tag payloads with nested descriptors
- dictionary call arguments
- values that cross representation boundaries

One concrete bug found during this work was that ARC cloned an `assign_boxy_box` statement without preserving `source_desc`. That meant the runtime could later see a box with payload bytes but no source descriptor. The fix in this WIP preserves `assign.source_desc` when ARC clones that statement.

## Implementation Work Completed So Far

This WIP contains several kinds of changes: real semantic fixes, principled additions, and temporary debugging instrumentation.

### LowLevel Addition: `list_capacity`

The most principled new LIR operation added so far is `LowLevel.list_capacity`.

The reason it was needed:

1. Boxy representation boundaries sometimes rebuild lists element by element.
2. The old boundary code allocated the target list with capacity equal to source length.
3. `List.append` is implemented as `List.reserve(list, 1)` followed by `list_append_unsafe`.
4. If a representation boundary happened between reserve and append, the boundary discarded the reserved spare capacity.
5. The following `list_append_unsafe` then appended into an exact-capacity or zero-capacity list.
6. That produced invalid list shapes, including a non-empty list with a null bytes pointer.

The correct design is that a list representation boundary must preserve source list capacity, not just length. Capacity is real runtime state produced by earlier operations such as `List.reserve`; it is not a heuristic.

The WIP implements this by adding `list_capacity` to `src/base/LowLevel.zig` and treating it as a read-only primitive with no RC effect. It returns the same semantic capacity as `RocList.getCapacity()`:

- For normal lists, decode `capacity_or_alloc_ptr >> 1`.
- For seamless slices, return length.

The WIP wires this operation into:

- The LIR interpreter, by reading `RocList.getCapacity()`.
- The dev backend, by inline machine-code emission:
  - load length
  - load encoded capacity/allocation pointer
  - decode normal capacity with right shift
  - if the low bit indicates a seamless slice, select length instead
- The LLVM backend, by generating the same select between decoded capacity and length.
- The WASM backend, by generating equivalent `i32` logic and extending to `i64` when the Roc return layout expects U64.

The boxy list representation boundary now computes both:

- `len = list_len(source)`, used as the loop bound.
- `capacity = list_capacity(source)`, used for `list_with_capacity(capacity)`.

This is the right long-term design direction because the boundary consumes explicit runtime data instead of guessing how much spare capacity might be needed.

### Boxy List Representation Boundary

The boxy lowerer now uses the new `list_capacity` operation in `assignListRepresentationBoundary`.

Before:

```text
len = list_len(source)
initial_list = list_with_capacity(len)
loop over len and append converted elements
```

After:

```text
len = list_len(source)
capacity = list_capacity(source)
initial_list = list_with_capacity(capacity)
loop over len and append converted elements
```

This matters because `capacity` and `len` are not interchangeable. A source list can be length 2 with capacity 3 because `List.reserve(list, 1)` was called immediately before a boundary. Rebuilding it with capacity 2 destroys the reservation and makes a subsequent unsafe append invalid.

### ARC Preservation of `source_desc`

In `src/lir/arc.zig`, the WIP preserves `source_desc` when cloning `assign_boxy_box`.

This is a real semantic fix. `source_desc` is needed to understand the source value being boxed when payload descriptors are involved. Dropping it during ARC rewriting can produce boxes whose bytes survive but whose descriptor information is erased. That later causes incorrect dynamic materialization, incorrect drops, or wrong tag/list/record interpretation.

There is currently accidental whitespace churn around the touched `assign_ref` / `assign_boxy_box` block in `arc.zig`; this should be cleaned later.

### Record Projection and Pattern Fixes

Earlier in the WIP, `test/echo/all_syntax_test.roc` failed because record destructuring tried to project fields directly from a dynamically boxed source. The source layout was a box or erased/dynamic layout, but the field projection expected a concrete struct layout.

The boxy lowerer now routes record projection through a more explicit representation source model:

- `RecordProjectionSource`
- `RecordProjectionUnbox`
- helper functions for record pattern binding and field projection

The key design fix is:

- If a record pattern or record field access is working from a dynamic boxed source, unbox or materialize it into the descriptor payload layout first.
- Only project fields from a concrete struct-shaped value.
- Preserve both the source field representation and the target field representation at the boundary.

This fixed an invariant failure where field projection saw a source layout that was neither a struct nor a boxed struct.

The useful lesson is that in boxy mode, "record-looking" values cannot be assumed to be concrete records. A value can be descriptor-backed and must be materialized through the descriptor before field offsets are meaningful.

### List Literal Lowering With Expected Element Types

The boxy lowerer now lowers list literal elements with the list element type as the expected type.

This matters when list elements need a representation boundary. Without the expected type, an element can be lowered into a shape that is locally valid but not the element layout the list storage expects. This is especially risky for:

- boxed dynamic values
- tag literals
- ZST tag payloads
- polymorphic list literals

### Tag Domain and ZST Tag Descriptor Fixes

Several tag descriptor fixes went in during debugging:

- Bool descriptor extension handling avoids treating `.bool_tag_union` like an ordinary tag extension.
- Tag literals are only lowered into an expected tag domain if the expected domain actually has the tag locally.
- ZST singleton tag descriptors are supported more explicitly.
- Static and template tag descriptor builders now account for payload layouts that are `.zst`.
- ZST tag variants can still carry descriptor information even when the runtime payload has no bytes.

The lesson here is that zero-sized payloads still need descriptor semantics. A ZST payload does not mean "no type information"; it only means there are no payload bytes.

### Descriptor Propagation in the Interpreter

Several interpreter-side fixes were added to preserve descriptor metadata during execution:

- `assign_call_dict` hidden descriptor arguments now use the adapter's hidden descriptor sources.
- `set_local` propagates runtime descriptor metadata.
- Runtime descriptor copying reuses existing runtime descriptor spans where appropriate.
- Runtime descriptor subgraphs are not duplicated unnecessarily when already runtime-owned.
- `performBoxyListDrop` handles `.incref` cases.
- `assign_boxy_box` calls a helper to incref copied/borrowed boxy transfer sources when needed.

These changes were necessary because values in boxy mode often carry semantic meaning partly through frame-local descriptor metadata. If ordinary assignment or call plumbing loses that metadata, later unboxing/materialization can make a layout-correct but semantically wrong value.

### Materialization Fixes

The interpreter's dynamic materialization path received several important fixes:

- `materializeBoxyPayloadToLayoutWithTargetDesc` uses `boxyPayloadValueForTargetDesc` when boxed source values have target descriptors.
- `requireBoxyTagPayloadLayout` accepts `.zst` layouts with discriminant 0.
- Expected-box materialization is handled before tag matching.
- Actual `.box` / `.box_of_zst` values are handled before tag matching.
- Same-layout box fast paths were tightened.
- The expected-box branch now:
  - resolves the target allocation descriptor
  - handles `.box_of_zst` by allocating the canonical box-of-ZST value
  - handles concrete `.box` by materializing into the box element layout and calling the box operation
  - recursively materializes payloads using the target allocation descriptor when needed

This fixed a stack overflow/infinite recursion that appeared after simple echo programs completed. Before the fix, a source/target combination involving expected boxes and ZST-ish layouts recursively attempted to materialize the same conceptual value without making progress.

## Debugging Timeline

### Initial State

The codebase already had:

- A dev backend.
- An LLVM backend.
- A LIR interpreter backend.
- Existing LIR, ARC, and backend machinery.
- A partial boxy lowering path with descriptors and erased callable support.

The immediate task shifted from pure design documentation into implementation investigation because running the new path exposed real correctness bugs.

### First Major Failure: Record Projection From Dynamic Box

The first significant failure in `test/echo/all_syntax_test.roc` was an invariant around field projection:

```text
field projection source layout was not struct/boxed struct
```

The root cause was that record patterns projected fields directly from a dynamic boxed source rather than first materializing/unboxing the record payload using its descriptor.

The fix was to make record projection source-aware, with explicit unboxing/materialization before field access. This was successful.

### Second Major Failure: Stack Overflow After Simple Echo

After the record projection fix, simple echo tests printed output and then stack-overflowed. The recursion was inside dynamic materialization, specifically around expected boxed layouts and ZST-like payloads.

The root cause was that box materialization paths were being considered too late and tag matching was entered for layouts that should have been resolved as boxes first.

The fix was to prioritize expected-box and actual-box cases in `materializeBoxyPayloadToLayoutWithTargetDesc`. This made simple echo tests pass at that point.

Observed passing commands at that stage included:

```sh
zig build roc
zig-out/bin/roc --opt=interpreter test/echo/hello.roc
zig-out/bin/roc --opt=interpreter test/echo/multi.roc
```

### Third Major Failure: Unsafe Append After Representation Boundary

The next `all_syntax_test` failure was:

```text
LIR/interpreter invariant violated:
assigned local layout List(Box({})) invalid value shape:
non-empty list had null bytes pointer
```

The failing low-level operation was `list_append_unsafe`.

The investigation showed:

- The source list had gone through a boxy list representation boundary.
- The boundary rebuilt the list using `list_with_capacity(list_len(source))`.
- The original list had spare capacity from `List.reserve`.
- Rebuilding with capacity equal to length destroyed the spare capacity.
- A later `list_append_unsafe` assumed the spare capacity still existed.

This was not a `Known` / `Unknown` tag matching issue. It was a capacity preservation issue.

The principled fix was to add `list_capacity` to LIR and preserve source capacity across list representation boundaries. This was implemented and was the best design outcome from this debugging pass.

After that fix, the previous append failure disappeared, and `all_syntax_test` progressed to a later failure.

### Fourth Current Failure: `Str.join_with` After `List.map(Str.trim)`

The current active failure is in `format_names` in `test/echo/all_syntax_test.roc`:

```roc
format_names : List(Str) -> Str
format_names = |names|
    names
        .map(|name| name.trim())
        ->Str.join_with(", ")
        ->(|joined| {
            if joined.is_empty() "No names provided" else "Names: ${joined}"
        })
```

The source call is:

```roc
print!(format_names(["  Alice ", "Bob  ", " Charlie"]))
```

A direct isolated test of `Str.join_with(["  Alice ", "Bob  ", " Charlie"], ", ")` worked and showed valid small strings in the input list.

An isolated test of:

```roc
names = ["  Alice ", "Bob  ", " Charlie"]
trimmed = names.map(|name| name.trim())
echo!(Str.join_with(trimmed, ", "))
```

failed.

The temporary `str_join_with` trace showed the mapped list had corrupted `RocStr` elements. In one run it showed elements such as:

```text
index=0 len=42 bytes=null cap=0x...
index=1 len=42 bytes=null cap=0x0
index=2 len=0 bytes=null cap=0x0
```

These are not valid results for trimming `"  Alice "`, `"Bob  "`, and `" Charlie"`.

The most likely current suspect is the in-place `List.map` path:

- `Builtin.roc` uses `list_map_can_reuse`.
- If it returns 1, it casts the input list to the output list type using `list_map_cast_unsafe`.
- Then it repeatedly:
  - extracts the old element with `list_map_extract_unsafe`
  - applies the transform
  - writes the replacement with `list_map_write_unsafe`

In boxy mode, `list_map_can_reuse` currently decides interchangeability using size, alignment, and whether layouts contain refcounted values. That may be insufficient. For boxy, two element layouts can have the same size and RC-ness but still require different descriptors or materialization behavior. Even when both are `Str`, the descriptor/representation path around the erased transform may be producing a value whose bytes are not the expected concrete `Str` bytes at the point `list_map_write_unsafe` copies them.

I started adding targeted debug output around:

- `str_trim`
- `list_map_extract_unsafe`
- `list_map_write_unsafe`
- `str_join_with`

The first version of that instrumentation failed to compile because I incorrectly checked `layout.tag == .str`; `Str` is a layout index, not a layout tag. I corrected that to compare layout indexes (`ll.ret_layout == .str` and arg layout index equals `.str`). The subsequent build/run was interrupted by the user before verification completed.

The current tree therefore contains that instrumentation, but its build status after the correction is unverified.

## What Worked Well

### The Host ABI Invariant Helped Avoid Bad Designs

The insistence that host ABI cannot change prevented a tempting but incorrect simplification: boxing all values uniformly. That would have made internal lowering easier but would immediately break host interop and explicit `Box(...)` behavior.

The more disciplined design is harder but correct:

- Keep host-shaped values host-shaped.
- Use hidden descriptors/vtables for metadata.
- Use boxes where box representation is semantically required.
- Materialize at boundaries.

### Explicit LIR Operations Are Better Than Context-Specific Workarounds

The list capacity bug could have been hacked around by allocating `len + 1` at the boundary, or by trying to detect `List.append` contexts. That would have violated the codebase rules against heuristics and would not have been generally correct.

Adding `list_capacity` was the right move because:

- Capacity is real runtime state.
- The boundary needs that exact state.
- The operation is meaningful beyond this one bug.
- It can be implemented uniformly in interpreter/dev/LLVM/WASM.

### The Interpreter Is a Good Semantic Oracle

The LIR interpreter surfaced representation bugs very quickly. Its debug invariant checks caught invalid value shapes such as non-empty lists with null bytes. The interpreter is currently the fastest way to expose boxy semantic mistakes before involving machine-code backend bugs.

### Descriptor Loss Is Detectable With Focused Traces

Several bugs came down to descriptor propagation. Once debug output included payload layout, nested descriptor spans, tag variant spans, and local descriptor metadata, it became much easier to see when a value's bytes and descriptor diverged.

### Small Focused Repros Help

The isolated `/tmp` tests separated:

- Direct `Str.join_with` on a string literal list: worked.
- `List.map(|name| name.trim())` followed by `Str.join_with`: failed.

That narrowed the current failure from "string join is broken" to "map/trim path corrupts string list elements."

## What Has Not Worked Well

### Temporary Debug Instrumentation Is Too Large

The current `interpreter.zig` diff includes many hard-coded debug prints:

- specific proc IDs
- specific statement IDs
- specific layout IDs
- full proc dumps
- local value summaries
- list layout traces
- descriptor traces
- tag materialization traces

This was useful for investigation but is not maintainable. It should not survive into final implementation except for genuinely general-purpose invariant diagnostics.

### `all_syntax_test` Is Too Broad As The Inner Loop

`test/echo/all_syntax_test.roc` is useful because it exercises many features together, but it is a poor inner-loop test for fixing one semantic bug at a time. It produces huge traces and often fails downstream of the original bug.

The better next step is to create focused regression tests for:

- record destructuring from dynamic boxed records
- list representation boundary preserving capacity
- `List.map(Str.trim)` in boxy mode
- `Str.join_with` on mapped string lists
- dynamic tag materialization with ZST payloads

### In-Place `List.map` Is More Dangerous In Boxy Mode

The current `listMapInterchangeableAtWidth` check is probably too weak for boxy mode. It checks:

- nonzero input size
- equal output size
- compatible effective alignment
- same "contains refcounted" classification

That may be enough for some specialized monomorphic cases, but boxy mode has additional semantic constraints:

- element descriptors may differ
- runtime payload layout may be descriptor-dependent
- transform result may require materialization to the list element layout before raw byte write
- an erased callable result can have a concrete layout but still need descriptor-guided conversion

The current corruption around `List.map(|name| name.trim())` suggests the unsafe map path needs stronger proof or explicit materialization before `list_map_write_unsafe`.

### Debug Value Shape Checking Has A Weakness

While investigating, I noticed that the interpreter's debug value shape check for `.list` appears to recurse using `layout_val.getIdx()` directly as the element layout. Depending on layout semantics, this may be the unresolved list child rather than the runtime representation element layout. The interpreter has a helper `listElemLayout` that resolves list layouts more carefully, and the debug checker should likely use that.

This weakness may explain why corrupted `List(Str)` elements were not caught earlier by the shape checker before `Str.join_with` crashed.

### The Current Tree Is Not Cleanly Verified

The last completed successful build was before the final focused map/trim trace was added. After adding that trace:

1. A build failed due to the incorrect `.tag == .str` check.
2. I fixed that compile error.
3. The next `zig build roc && roc ...` command was interrupted before completion.

So the current WIP commit should be treated as possibly not building until rechecked.

## Detailed Current Findings

### Direct String List Construction Is Probably Fine

The isolated direct join case:

```roc
main! = |_args| {
    echo!(Str.join_with(["  Alice ", "Bob  ", " Charlie"], ", "))
    Ok({})
}
```

ran successfully. The trace showed three valid small string elements:

```text
index=0 len=8 small=true
index=1 len=5 small=true
index=2 len=8 small=true
```

That means basic string literal list construction is probably not the source of the current corruption.

### `List.map` Plus `Str.trim` Is The Current Repro

The isolated failing case is:

```roc
main! = |_args| {
    names = ["  Alice ", "Bob  ", " Charlie"]
    trimmed = names.map(|name| name.trim())
    echo!(Str.join_with(trimmed, ", "))
    Ok({})
}
```

The final `Str.join_with` saw a `List(Str)` of length 3 with corrupted elements. This suggests corruption occurs during map, during trim result materialization, or during map write.

### `RocStr.memcpy` Crashed On Corrupt Input

`strJoinWith` eventually called `RocStr.memcpy` on a string element. The crash was:

```text
cast causes pointer to be null
```

The immediate line was in `RocStr.asU8ptr`, where a non-small string casts `self.bytes` to a non-null pointer. For valid empty strings, `RocStr.empty()` is a small string with `length = SMALL_STR_BIT`, so `isSmallStr()` should be true. The corrupted element had `bytes=null` but was not encoded as a valid small string in all cases.

The runtime helper is not the primary bug here; it was handed invalid `RocStr` bytes.

### Capacity Preservation Fix Worked

After `list_capacity` was added and used by list representation boundaries, the previous invalid list append failure no longer appeared. The trace showed boundaries allocating capacity 3 for length 2 or 3 lists when source capacity was 3, and subsequent unsafe appends no longer appended into null/exact-capacity lists.

This is a strong signal that `list_capacity` is a correct and useful addition.

## Recommended Next Steps

These are not "MVP" steps; they are the shortest path toward the long-term correct design while preserving the invariants.

### 1. First Verify Or Repair The Current WIP Build

Run:

```sh
zig build roc
```

The current tree was not verified after the final instrumentation correction. If it fails, fix only syntax/type errors in the temporary instrumentation or remove that instrumentation.

### 2. Reduce The Current Failure To A Checked-In Focused Test

Create a focused regression around:

```roc
names = ["  Alice ", "Bob  ", " Charlie"]
trimmed = names.map(|name| name.trim())
Str.join_with(trimmed, ", ")
```

Expected output:

```text
Alice, Bob, Charlie
```

This should run through the boxy/interpreter path. It should fail before the fix and pass after.

### 3. Finish The Map/Trim Diagnosis

Use the targeted traces already started to answer these questions:

- Does `list_map_extract_unsafe` read a valid original `Str` for each index?
- Does `str_trim` receive valid input?
- Does `str_trim` return a valid concrete `Str`?
- Does the erased callable call materialize the returned `Str` to concrete `.str` layout before `list_map_write_unsafe`?
- Does `list_map_write_unsafe` copy exactly 24 bytes of the returned `Str` into the correct list slot?
- Does the list returned from `list_map_write_unsafe` preserve the same bytes pointer and capacity?

The key distinction is whether corruption occurs before transform return, during transform result materialization, or at raw list write.

### 4. Revisit Boxy `list_map_can_reuse`

The long-term fix may be one of these:

- Require exact runtime element layout identity for in-place map reuse in boxy mode, not merely same size/alignment/RC-ness.
- Require exact descriptor compatibility between input and output element representations.
- Force materialization of the transform result into the output list element layout before `list_map_write_unsafe`.
- Disable the unsafe in-place branch only when descriptor-dependent representation prevents a proof of exact interchangeability.

The correct design is not to blanket-disable in-place map forever if the compiler can prove exact representation compatibility. But the proof must include boxy descriptor semantics, not just byte size.

### 5. Fix Debug Value Shape Checking For Lists

The interpreter's value shape checker should use the resolved runtime element layout for list elements. This would likely catch corrupt `List(Str)` elements earlier and with a better diagnostic.

### 6. Clean Up Temporary Debug Instrumentation

Before turning this WIP into reviewable work:

- Remove hard-coded proc/stmt/layout debug traces.
- Keep only general invariant diagnostics that are useful independent of this repro.
- Convert important findings into tests.
- Clean accidental whitespace churn in `arc.zig`.

### 7. Add A Focused Test For List Capacity Boundaries

There should be a boxy lowering test or interpreter integration test that proves list representation boundaries preserve capacity:

```text
source list has len N and capacity M
representation boundary rebuilds target list with capacity M
loop copies only N elements
subsequent list_append_unsafe can append when M > N
```

This is the regression protected by `list_capacity`.

### 8. Finish CLI Integration Later

This WIP did not implement the user-facing `--specialize=yes|no` CLI flag. The implementation work has been focused on making the boxy lowering path semantically correct. The eventual CLI work should:

- Add CLI parsing for `--specialize=yes|no`.
- Store compiler options as `.lss` / `.boxy`.
- Default dev to `.boxy`.
- Default size/speed to `.lss`.
- Keep `roc check` unaffected.
- Ensure UI progress messages do not say "Specializing" for boxy mode; "Lowering" is acceptable.

## Current Modified File Notes

### `src/base/LowLevel.zig`

Adds `list_capacity` and marks it as RC-effect-free.

This is a good semantic addition.

### `src/backend/dev/LirCodeGen.zig`

Adds codegen for `list_capacity`.

The implementation computes semantic capacity, not raw encoded capacity. It handles seamless slices by returning length.

This should be kept, but it needs verification on both x86_64 and aarch64 because it uses architecture-specific conditional select/move instructions.

### `src/backend/llvm/MonoLlvmCodeGen.zig`

Adds `emitListCapacity`.

This should be kept. It generates a select between decoded capacity and length.

### `src/backend/wasm/WasmCodeGen.zig`

Adds WASM lowering for `list_capacity`.

This should be kept but needs wasm-specific tests eventually.

### `src/lir/arc.zig`

Preserves `source_desc` when cloning `assign_boxy_box`.

This should be kept. Whitespace should be cleaned.

### `src/postcheck/boxy/lower.zig`

Contains real fixes for:

- list boundary capacity preservation
- record projection from boxed dynamic sources
- list literal expected element lowering
- tag domain checks
- ZST tag descriptor support
- record descriptor indexing
- descriptor source propagation

It also contains temporary debug prints from the investigation. These should be audited carefully before review.

### `src/eval/interpreter.zig`

Contains real fixes for:

- `list_capacity`
- descriptor propagation
- runtime descriptor copying
- materialization of boxed/ZST/tag/list values
- `assign_boxy_box` ownership handling
- `set_local` descriptor propagation

It also contains a very large amount of temporary debug instrumentation. This file needs the most cleanup.

## Main Lessons Learned

1. Boxy mode is not "just use boxes." It is a representation system with explicit descriptors and boundaries.

2. Host ABI preservation must be designed in from the beginning. It cannot be patched in afterward.

3. Descriptor metadata is semantically part of erased values. Losing it is just as bad as corrupting payload bytes.

4. Representation boundaries must preserve all runtime state required by later operations. For lists, that includes capacity, not just length.

5. ZST values still need descriptors. Zero payload bytes do not mean zero semantic information.

6. ARC cloning must preserve boxy descriptor fields. ARC cannot treat boxy statements like ordinary byte-moving statements.

7. Dynamic record field projection must materialize/unbox to a concrete record payload before using field offsets.

8. In-place list map reuse requires a stronger proof in boxy mode than size/alignment/RC equivalence.

9. Small strings are easy to corrupt if a path copies the wrong bytes into a `Str` slot. The length marker lives in the final byte of the `RocStr` struct for small strings, so raw byte writes must be exactly right.

10. `all_syntax_test` is valuable as a broad integration test but should not be the only regression signal. Each discovered issue deserves a focused test.

11. The LIR interpreter is currently the best place to debug boxy semantics because it can report value-shape and descriptor invariants before backend-specific machine-code behavior enters the picture.

12. `list_capacity` is a general LIR operation the system needed anyway. It is not a boxy-only hack.

## Open Risks

- The current WIP may not build because the final build after instrumentation correction was interrupted.
- Temporary debug instrumentation may affect compile-time noise and performance.
- `list_map_can_reuse` may be unsound in boxy mode.
- The interpreter value-shape checker for lists may not validate element layouts correctly.
- Some fixes are only validated by ad hoc echo tests, not checked-in focused tests.
- The code still needs a careful host ABI audit once CLI integration reaches real entry wrappers.
- The code still needs cleanup to remove hard-coded proc and layout IDs.

## Bottom Line

The most solid completed design/implementation result in this WIP is the addition of `list_capacity` and the change to preserve list capacity across boxy list representation boundaries. That fixed a real correctness bug in the interaction between representation boundaries and `List.append`.

The current active bug is separate: `List.map(|name| name.trim())` corrupts `List(Str)` elements before `Str.join_with`. The best current hypothesis is that the in-place `List.map` path is unsound or insufficiently materialized under boxy semantics. The next implementation pass should focus there with a small regression test, then clean up the temporary tracing once the exact cause is confirmed.
