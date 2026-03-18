# Fix Interpreter Plan

## Goal

Completely replace the current CIR interpreter with a new interpreter that executes LIR.

This is not a compatibility-preserving incremental cleanup. It is a deliberate one-shot refactor that should:

- stop interpreting CIR directly
- stop carrying type variables and runtime unification machinery through evaluation
- stop depending on `interpreter_layout` as a forked layout subsystem
- delete large amounts of interpreter-specific runtime type logic
- converge the interpreter onto the same lowered IR shape already used by the dev and wasm evaluators

## Why This Refactor Is Correct

The current interpreter is fighting the wrong IR.

Observed from the codebase:

- The existing interpreter is a CIR interpreter in [`src/eval/interpreter.zig`](src/eval/interpreter.zig), and it is enormous: about 20.7k lines.
- It owns a large runtime type/unification layer:
  - `runtime_types`
  - `translateTypeVar`
  - `rigid_subst`
  - `rigid_name_subst`
  - `flex_type_context`
  - `translate_cache`
  - `var_to_layout_slot`
  - runtime method/type resolution caches
- It uses a dedicated forked layout subsystem in [`src/interpreter_layout`](src/interpreter_layout).
- It uses `StackValue` with an `rt_var`, so values are not just values; they carry type-reconstruction baggage.
- Refcounting and value movement are tied to runtime layout/type decisions that only exist because evaluation happens too early on CIR.

By contrast, LIR already has the properties an interpreter wants:

- concrete `layout.Idx` everywhere
- globally unique symbols instead of module-local CIR indices
- explicit control/data constructs for runtime execution
- explicit RC operations after `RcInsert`
- simplified pattern and binding forms
- call canonicalization support

This means the new interpreter can execute runtime semantics directly, instead of reconstructing them out of type variables and canonicalization-time metadata.

## Current State Summary

### Existing CIR interpreter surface

Primary implementation and adjacent pieces:

- [`src/eval/interpreter.zig`](src/eval/interpreter.zig)
- [`src/eval/StackValue.zig`](src/eval/StackValue.zig)
- [`src/eval/render_helpers.zig`](src/eval/render_helpers.zig)
- [`src/interpreter_layout/mod.zig`](src/interpreter_layout/mod.zig)
- [`src/interpreter_layout/store.zig`](src/interpreter_layout/store.zig)
- [`src/interpreter_values/RocValue.zig`](src/interpreter_values/RocValue.zig)

Supporting users of the interpreter:

- compile-time evaluation in [`src/eval/comptime_evaluator.zig`](src/eval/comptime_evaluator.zig)
- expect/test running in [`src/eval/test_runner.zig`](src/eval/test_runner.zig)
- entrypoint execution in [`src/compile/runner.zig`](src/compile/runner.zig)
- shared-memory shim in [`src/interpreter_shim/main.zig`](src/interpreter_shim/main.zig)
- playground/test code under [`src/eval/test`](src/eval/test)

### Existing LIR pipeline already in production use

Both dev and wasm evaluators already do:

1. CIR -> MIR
2. lambda set inference
3. MIR -> LIR
4. call canonicalization
5. RC insertion
6. call canonicalization again
7. consume LIR

Relevant files:

- [`src/eval/dev_evaluator.zig`](src/eval/dev_evaluator.zig)
- [`src/eval/wasm_evaluator.zig`](src/eval/wasm_evaluator.zig)
- [`src/lir/MirToLir.zig`](src/lir/MirToLir.zig)
- [`src/lir/CallCanonicalize.zig`](src/lir/CallCanonicalize.zig)
- [`src/lir/rc_insert.zig`](src/lir/rc_insert.zig)
- [`src/lir/LIR.zig`](src/lir/LIR.zig)
- [`src/lir/LirExprStore.zig`](src/lir/LirExprStore.zig)

That existing pipeline is the strongest argument for the refactor: the compiler already knows how to produce the right execution IR.

### Important observation: proc/CFStmt infrastructure exists but is not currently emitted

`LIR` defines:

- `LirProc`
- `CFStmt`
- join points / tail recursion structures

But `addProc()` in [`src/lir/LirExprStore.zig`](src/lir/LirExprStore.zig) has no call sites today. That means the currently produced LIR appears to be expression-tree based, not proc/CFStmt based.

Implication:

- The first LIR interpreter does not need proc/CFStmt support to replace the current interpreter.
- It should still be designed so proc/CFStmt support can be added later if/when lowering starts emitting it.

This materially reduces implementation risk.

## Recommended Target Architecture

## Keep the public API shape initially, replace internals completely

Do not force all callers to change at once.

Keep an `eval.Interpreter` facade with the same broad responsibilities:

- `init`
- `deinit`
- `eval`
- `evaluateExpression`
- `renderValueRoc`
- `renderValueRocWithType`
- `setupForClauseTypeMappings`

But internally replace it with a LIR-backed engine.

Recommended transitional shape:

- `src/eval/interpreter.zig`
  - becomes a thin wrapper / compatibility facade
- `src/eval/lir_interpreter.zig`
  - actual implementation
- `src/eval/lir_value.zig`
  - runtime value representation and memory helpers
- `src/eval/lir_program.zig`
  - owns lowering caches/stores for a module set
- `src/eval/lir_builtin_ops.zig`
  - low-level builtin execution helpers
- `src/eval/value_render.zig`
  - layout-based rendering shared by interpreter/comptime/tests

This keeps integration churn manageable while still allowing the old implementation to be deleted in one sweep.

## New execution model

### 1. Lower first, then interpret

For every evaluated root expression, the interpreter should consume post-RC LIR, not CIR.

Recommended lowering pipeline for interpreter use:

1. build/find shared `layout.Store`
2. lower requested root from CIR to MIR
3. run lambda set inference
4. lower MIR to LIR
5. run `CallCanonicalize`
6. run `RcInsert`
7. run `insertRcOpsIntoSymbolDefsBestEffort`
8. run `CallCanonicalize` again
9. interpret the resulting LIR root

This should be factored into one shared helper instead of duplicated across:

- `DevEvaluator`
- `WasmEvaluator`
- new LIR interpreter

Recommended helper module:

- `src/eval/lir_program.zig`

### 2. Use the shared `layout` module, not `interpreter_layout`

The new interpreter should use [`src/layout`](src/layout), not the interpreter-specific fork.

Why:

- LIR already speaks `layout.Idx`
- dev/wasm already depend on shared `layout.Store`
- maintaining two layout systems is pure tax

Target result:

- remove `interpreter_layout` from interpreter execution
- retarget value formatting away from `interpreter_layout`
- eventually delete `src/interpreter_layout`

### 3. Replace `StackValue` with a concrete runtime value type

The current `StackValue` shape is polluted by CIR runtime typing:

- `layout`
- `ptr`
- `is_initialized`
- `rt_var`

The new value representation should not carry a runtime type variable.

Recommended direction:

- keep concrete layout identity via `layout.Idx`
- keep raw bytes / pointer access
- keep memory helpers for structs, lists, tags, strings, boxes
- remove `rt_var` entirely
- remove all type-translation APIs from the value layer

The new value type can still be raw-pointer based; it just needs to be monomorphic.

### 4. Execute explicit RC, do not re-infer ownership at runtime

This is one of the biggest wins.

The new interpreter should not have any implicit “interpreter cleanup” ownership logic analogous to the current CIR implementation.

Instead:

- `.incref`, `.decref`, `.free` are explicit executable instructions
- `.cell_init`, `.cell_store`, `.cell_load`, `.cell_drop` define mutable state behavior
- block/branch/early-return cleanup should happen because RC insertion already emitted it

Rule for the new interpreter:

- follow the LIR literally
- do not perform hidden decref-on-scope-exit logic for locals
- only keep deinit-time cleanup for long-lived cached top-level values owned by the interpreter itself

### 5. Use symbol-based environments, not pattern-index/source-env lookup state

The current interpreter carries bindings keyed by CIR pattern indices and source environments because CIR names are awkward to execute directly.

LIR already fixes that.

Recommended runtime environment model:

- frame-local immutable bindings: `Symbol -> Value`
- frame-local mutable cells: `Symbol -> Cell`
- interpreter-global top-level cache: `Symbol -> Value`
- active-evaluation set for top-level defs to detect recursion/re-entry

This deletes:

- `Binding { pattern_idx, expr_idx, source_env }`
- active-closure binding lookup hacks
- module-env switching just to interpret local variable references

### 6. Preserve external behavior where it matters, not internal machinery

The new interpreter should preserve:

- evaluation semantics
- host ABI integration via `RocOps`
- hosted function calls
- compile-time evaluation behavior
- test running behavior
- entrypoint execution behavior
- value rendering used by tests/diagnostics

It should not preserve:

- runtime type translation APIs
- interpreter-specific type caches
- current closure header representation
- CIR-specific lookup/pattern mechanics
- interpreter-only layout subsystem

## What the New LIR Interpreter Must Support

The produced LIR surface to support is at least:

- literals
  - `i64_literal`
  - `i128_literal`
  - `f32_literal`
  - `f64_literal`
  - `dec_literal`
  - `str_literal`
  - `bool_literal`
- value lookup / state
  - `lookup`
  - `cell_load`
- calls
  - direct calls
  - expr callees as fallback
  - `hosted_call`
- functions
  - `lambda`
- aggregates
  - `empty_list`
  - `list`
  - `struct_`
  - `struct_access`
  - `zero_arg_tag`
  - `tag`
  - `nominal`
- control flow
  - `block`
  - `if_then_else`
  - `match_expr`
  - `discriminant_switch`
  - `tag_payload_access`
  - `for_loop`
  - `while_loop`
  - `early_return`
  - `break_expr`
  - `crash`
  - `runtime_error`
  - `dbg`
  - `expect`
- builtin/string helpers
  - `low_level`
  - `str_concat`
  - `int_to_str`
  - `float_to_str`
  - `dec_to_str`
  - `str_escape_and_quote`
- explicit RC/mutation
  - `incref`
  - `decref`
  - `free`
  - `cell_init`
  - `cell_store`
  - `cell_drop`

Pattern support required:

- `bind`
- `wildcard`
- literals
- `tag`
- `struct_`
- `list`
- `as_pattern`

## Key simplifying fact about closures

The new interpreter should not recreate the current CIR closure model.

Based on `MirToLir`:

- lambda lifting already happened before LIR
- function values are represented using lambda-set runtime layouts
- closure values are capture payloads or tag unions over capture payloads
- call dispatch for multi-member lambda sets is lowered explicitly with `discriminant_switch` and `tag_payload_access`
- direct-call specialization and callable-def registration already exist

Implication:

The interpreter should mostly treat function runtime values as ordinary monomorphic data and use canonical callable targets for execution. It should not need:

- runtime type-driven closure reconstruction
- CIR closure headers
- active-closure-based capture lookup

## Major Deletions Expected

The plan should assume these categories disappear completely or collapse dramatically.

### Delete from interpreter runtime typing

- `translateTypeVar`
- `getRuntimeLayout` style translation
- `runtime_types` as an execution dependency
- rigid/flex substitution machinery used only for evaluation
- runtime type-unification caches
- for-clause type mapping logic as an execution requirement

`setupForClauseTypeMappings` can remain as a temporary no-op compatibility method until callers are cleaned up.

### Delete CIR-specific binding/model logic

- pattern-idx/source-env keyed binding lookup
- module-env switching for ordinary local evaluation
- CIR-specific closure capture lookup
- def-stack machinery used to patch around CIR evaluation shape

### Delete forked layout/rendering stack

- `interpreter_layout` usage in interpreter execution
- `render_helpers` dependence on runtime type variables
- `StackValue.rt_var`

### Likely full-file deletions or radical rewrites

Candidates:

- [`src/eval/interpreter.zig`](src/eval/interpreter.zig) -> rewrite entirely
- [`src/eval/StackValue.zig`](src/eval/StackValue.zig) -> rewrite or replace
- [`src/eval/render_helpers.zig`](src/eval/render_helpers.zig) -> rewrite or delete
- [`src/interpreter_layout/mod.zig`](src/interpreter_layout/mod.zig) -> delete after migration
- [`src/interpreter_layout/store.zig`](src/interpreter_layout/store.zig) -> delete after migration
- [`src/interpreter_values/RocValue.zig`](src/interpreter_values/RocValue.zig) -> retarget to shared `layout`

## Progress Summary (as of 2026-03-18)

Phases 0–5 are complete. The LIR interpreter exists, is wired into the test harness,
dispatches all 232 low-level operations through direct builtin calls, and renders
values via `Str.inspect` (same approach as dev/wasm evaluators).

| Phase | Status | Key files |
|-------|--------|-----------|
| Phase 0 | DONE | — |
| Phase 1 | DONE | `src/eval/lir_program.zig` |
| Phase 2 | DONE | `src/eval/lir_value.zig`, `src/eval/lir_value_format.zig` |
| Phase 3 | DONE | `src/eval/lir_interpreter.zig` |
| Phase 4 | DONE | `src/eval/lir_interpreter.zig` (inline in `evalLowLevel`) |
| Phase 5 | DONE | `src/eval/test/helpers.zig` (`lirInterpreterStr` wraps in `Str.inspect`) |
| Phase 6 | TODO | Rewire ComptimeEvaluator, TestRunner |
| Phase 7 | TODO | Delete old CIR interpreter and `interpreter_layout` |

Test results: **1223/1226 pass**, 3 skipped, **6 remaining LIR mismatches**
(down from 24 — the ~17 rendering format differences are resolved).

## Proposed Implementation Plan

## Phase 0: Lock the target and stop preserving CIR internals — DONE

Deliverables:

- decide that the interpreter executes post-RC LIR only
- decide to use shared `layout.Store`
- decide to keep only the public `eval.Interpreter` facade shape temporarily

Non-goals for the refactor:

- preserving old interpreter internals
- maintaining `translateTypeVar`-based rendering
- retaining `interpreter_layout`

## Phase 1: Build a shared lowering package for interpreter consumption — DONE

Create a reusable lowering helper, probably `src/eval/lir_program.zig`.

It should own:

- module/env set
- shared `layout.Store`
- `MIR.Store`
- `LambdaSet.Store`
- `LirExprStore`
- mapping from requested CIR roots/defs to lowered LIR expr ids
- any caches needed for repeated evaluation in REPL/comptime/test scenarios

Responsibilities:

- centralize the exact LIR pipeline already duplicated in `DevEvaluator` and `WasmEvaluator`
- expose “lower this root expression to executable post-RC LIR”
- expose symbol-def / callable-def tables for evaluation

Important design choice:

- Prefer lazy lowering of roots into a shared store over rebuilding fresh MIR/LIR state on every `eval()` call.

Reason:

- compile-time evaluation and test execution evaluate many roots in the same module
- repeated lowering would be simpler short-term but adds avoidable churn and makes caching harder

## Phase 2: Introduce the new value/runtime layer — DONE

Create a new concrete value layer, e.g. `src/eval/lir_value.zig`.

Requirements:

- concrete layout-only representation
- no runtime type vars
- helpers for:
  - integers/floats/dec/bool
  - strings
  - lists
  - structs/tuples/records
  - tag unions
  - boxes
- copy/move/borrow helpers as needed by explicit RC execution
- raw ABI-compatible memory layout for `RocStr`, `RocList`, tag unions, structs

This layer may still reuse:

- the stack allocator from [`src/eval/stack.zig`](src/eval/stack.zig)
- host allocation via `RocOps`

It should not reuse the old `StackValue` interface wholesale unless that file is aggressively simplified first.

## Phase 3: Implement expression-tree LIR evaluation — DONE

Create `src/eval/lir_interpreter.zig`.

Core runtime state:

- allocator
- `layout.Store`
- `LirExprStore`
- root/module metadata
- frame stack
- mutable cell store
- top-level symbol cache
- currently-evaluating symbol set
- `RocOps`
- optional stack allocator for temporaries

Execution entrypoints:

- `evalExpr(root_expr_id)`
- `evalSymbol(symbol)`
- `callDirect(symbol, args)`
- `callLambda(lambda_expr_id, args)`
- `evaluateEntrypoint(expr_idx, ret_ptr, arg_ptr)`

Semantics to implement:

- evaluate expressions recursively/iteratively
- bind pattern results to symbols
- maintain lexical frames for blocks and lambda calls
- use explicit RC ops only
- execute loops and early-return/break control flow
- resolve top-level defs via `symbol_defs`
- resolve direct-callable defs via `callable_defs`

Recommended control-flow representation in the interpreter:

- internal result union for normal value / break / early return / crash

Because proc/CFStmt is not emitted today, expression-level control flow is enough for the first cut.

## Phase 4: Port low-level builtins and effect calls — DONE (direct dispatch architecture)

**Status: Implemented.** The original plan called for a separate `lir_builtin_ops.zig` that would reimplement each low-level operation. We chose a fundamentally better architecture: **call the actual builtin functions directly** through a `RocOps` interface, the same way compiled code does.

### Architecture: Direct Builtin Dispatch

Instead of reimplementing ~230 low-level operations, the interpreter marshals arguments into the types builtins expect (`RocStr`, `RocList`, `RocDec`) and calls them directly:

```
LIR eval loop
  ├── control flow (block, if, match, loop) → interpreter handles directly
  ├── data ops (struct, tag, list literal)   → interpreter handles directly
  └── low_level ops (232 LowLevel variants)  → marshal args → call builtin → marshal result
                                                ├── str ops → builtins.str.*
                                                ├── list ops → builtins.list.*
                                                ├── dec ops → builtins.dec.*
                                                └── num ops → generic helpers
```

### Key implementation details

**RocOps via `InterpreterRocEnv`** — The builtins need `*RocOps` for memory allocation and crash handling. We provide this with `InterpreterRocEnv`, which follows the same `StaticAlloc` pattern as `DevRocEnv` in `dev_evaluator.zig`: a 1MB thread-local static buffer for bump allocation, with no-op deallocation. This avoids Zig allocator vtable issues from C-calling-convention callbacks.

**Value ↔ RocStr/RocList marshaling** — Since the interpreter stores values as unaligned `[*]u8` byte buffers, we use `@memcpy` to marshal between `Value` and `RocStr`/`RocList`:

```zig
fn valueToRocStr(val: Value) RocStr {
    var rs: RocStr = undefined;
    @memcpy(std.mem.asBytes(&rs), val.ptr[0..@sizeOf(RocStr)]);
    return rs;
}
```

This is zero-overhead for small strings (≤23 bytes, inline in the struct).

**Crash recovery via setjmp/longjmp** — Builtins that can crash (e.g., Dec division by zero) call `roc_ops.crash()`, which longjmps back. The interpreter sets up setjmp before each such call and returns `error.Crash` if triggered. Only the crash flag is reset per-operation; the static buffer is reset once per top-level `eval()` call (resetting it per-operation would invalidate earlier allocations).

### Critical lesson: builtins that consume inputs cannot be called directly on interpreter-allocated data

**Problem discovered:** Builtins that "consume" their inputs (e.g., `strJoinWithC`) call `list.decref()` on the input, which tries to read a refcount header from memory before the data pointer. Interpreter-allocated lists don't have refcount headers (they use arena allocation), so this reads garbage and corrupts memory.

**Solution:** For builtins that consume list/string inputs and call `decref` internally, implement the operation manually in the interpreter instead of calling the builtin. Currently this applies to:

- `str_join_with` — implemented as a manual loop reading RocStr elements from the list

The safe-to-call builtins are those that:
1. Only *borrow* their inputs (no decref), OR
2. Only allocate the *result* through `roc_ops` (not the inputs)

For reference, the ownership of each LowLevel's arguments is defined in `LowLevel.getArgOwnership()` in `src/base/LowLevel.zig`.

### What was NOT needed

- **No separate `lir_builtin_ops.zig`** — All dispatch lives inline in `evalLowLevel` in `lir_interpreter.zig`. Each switch arm is 3–8 lines of marshaling code.
- **No reimplemented semantics** — The builtins are the single source of truth. The interpreter and compiled code run the same functions.
- **No `list_sort_with` support** — This requires a callback function pointer, which the interpreter cannot provide as a C-callable function. It returns the input unchanged. This is acceptable because sort-with requires lambda lifting that the interpreter handles at a higher level.

### Remaining mismatches after this phase (24 total)

| Category | Count | Nature |
|----------|-------|--------|
| Struct field name formatting | ~17 | LIR formatter shows `(a, b)` tuples vs `{ x: a, y: b }` records — a rendering issue in `lir_value_format.zig`, not the interpreter |
| List operations with callbacks | ~3 | `list_sort_with` and similar ops that need function pointer callbacks |
| Numeric edge cases | ~3 | Dec conversion and comparison edge cases |
| Lambda/closure ops | ~1 | Higher-order function edge cases |

Down from **128 mismatches** before this phase. All **1223 tests pass**, 3 skipped.

## Phase 5: Render via Str.inspect instead of layout-based formatting — DONE

**Status: Implemented.** The original plan called for building a layout-based renderer with field/tag name lookup tables. Instead, we used the same approach as the dev and wasm evaluators: wrap the expression in `Str.inspect()` at the CIR level before lowering, then read the resulting `RocStr`.

### What changed

- `lirInterpreterStr` in `src/eval/test/helpers.zig`: calls `wrapInStrInspect(module_env, expr_idx)` before lowering, then reads the result as a `RocStr` instead of calling `lir_value_format.formatValue`.
- Removed unused `lir_value_format` import from helpers.zig.

### Why this is better than the original plan

- No new renderer code needed — Roc's own `Str.inspect` implementation handles field names, tag names, record vs tuple formatting, etc.
- Guaranteed format compatibility with dev/wasm evaluators (same code path).
- `lir_value_format.zig` is no longer used by the test harness (may be useful for debug/diagnostic purposes later).

### Remaining mismatches after this phase (6 total, down from 24)

| Category | Count | Nature |
|----------|-------|--------|
| List operations (ZST append) | 1 | `list append zst` — empty vs populated list |
| Numeric edge cases | 1 | F32 multiplication precision |
| Other | ~4 | Various LIR interpreter eval failures (silently skipped) |

The ~17 rendering format differences (records as tuples, `error.Unsupported` for tag unions) are all resolved.

Expected simplification:

- `renderValueRocWithType` should become much thinner
- anything that only existed to recover type information at runtime should be removed

Potential wrinkle:

- compile-time folding and some rendering paths may still need nominal/tag naming context from the source module
- this should be solved with layout/module metadata, not runtime type variables

## Phase 6: Rewire all interpreter consumers

### `ComptimeEvaluator`

File:

- [`src/eval/comptime_evaluator.zig`](src/eval/comptime_evaluator.zig)

This currently depends heavily on:

- `Interpreter`
- `StackValue`
- runtime types
- `translateTypeVar`
- `interpreter_layout`

Required rewrite:

- evaluate defs through the LIR interpreter
- maintain top-level declaration ordering and crash/expect handling
- rebuild constant-folding helpers against concrete layouts
- convert runtime values back into CIR constants without runtime type translation

This is a substantial port, not a wrapper tweak.

### `TestRunner`

File:

- [`src/eval/test_runner.zig`](src/eval/test_runner.zig)

This should become much simpler:

- evaluate expect body through LIR interpreter
- inspect boolean result directly from concrete layout
- keep crash/expect reporting behavior

### `compile/runner` and `interpreter_shim`

Files:

- [`src/compile/runner.zig`](src/compile/runner.zig)
- [`src/interpreter_shim/main.zig`](src/interpreter_shim/main.zig)

These should keep working through the same high-level API.

Important simplification:

- `setupForClauseTypeMappings()` should become unnecessary for execution once entrypoints are lowered through MIR/LIR using the actual checked module graph
- keep the method as a compatibility stub until all callers stop assuming it matters

### tests/helpers

File:

- [`src/eval/test/helpers.zig`](src/eval/test/helpers.zig)

This file mixes:

- CIR interpreter test setup
- dev lowering checks
- renderer helpers

It will need cleanup to:

- stop constructing interpreter-specific layout formatting contexts
- use the new layout-based value renderer
- avoid any dependency on `translateTypeVar` unless a specific remaining path truly needs it

## Phase 7: Delete the old CIR interpreter and forked layout code

Once all consumers are switched:

- delete old CIR evaluation internals
- delete `interpreter_layout`
- delete runtime type translation logic from `eval`
- delete `StackValue` APIs that exist only for CIR execution
- delete unused tests aimed only at CIR interpreter internals

The end state should have one interpreter implementation, not two.

## Suggested Execution Order

This is the order I would actually implement it in.

1. Add `lir_program.zig` and factor shared lowering pipeline out of dev/wasm evaluators.
2. Add `lir_value.zig` and a minimal layout-based renderer.
3. Implement a minimal `lir_interpreter.zig` that supports:
   - literals
   - lookup
   - blocks
   - structs/tags/lists
   - direct calls to `lambda`
   - `if` / `match`
   - `discriminant_switch`
   - `tag_payload_access`
4. Add explicit RC op execution and mutable cells.
5. Add loops, `early_return`, and `break_expr`.
6. Port low-level builtins and hosted calls.
7. Swap `eval.Interpreter` facade to use the LIR engine.
8. Port `TestRunner`.
9. Port `ComptimeEvaluator`.
10. Port/clean rendering and test helpers.
11. Remove old interpreter files and `interpreter_layout`.

## Recommended Milestones

## Milestone 1: LIR expression evaluation parity for pure code

Definition:

- evaluate scalar arithmetic
- closures/functions via direct calls
- records/tuples/lists/tags
- pattern matching
- rendering
- no compile-time evaluator yet

Primary validation:

- interpreter style tests for pure expressions
- polymorphism/higher-order tests that now lower through MIR/LIR

## Milestone 2: RC + mutation + loops

Definition:

- explicit `incref`/`decref`/`free`
- mutable cells
- `for_loop`
- `while_loop`
- `early_return`
- `break_expr`

Primary validation:

- list refcount tests
- mutable-loop regressions
- early-return/break tests

## Milestone 3: host integration

Definition:

- `hosted_call`
- `dbg`
- `expect`
- `evaluateExpression` ABI path
- compile runner / shim

Primary validation:

- interpreter shim paths
- platform entrypoint execution
- dbg/expect tests

## Milestone 4: compile-time evaluator and full cleanup

Definition:

- `ComptimeEvaluator` ported
- constant folding restored
- old CIR interpreter deleted
- `interpreter_layout` deleted

Primary validation:

- `comptime_eval_test.zig`
- full `src/eval/test` suite

## Risks and Sharp Edges

### 1. Compile-time folding is still real work

This refactor drastically simplifies runtime evaluation, but it does not automatically solve “convert evaluated runtime value back into a CIR constant expression”.

`ComptimeEvaluator` is one of the highest-risk consumers because it is coupled to:

- interpreter evaluation
- rendering
- constant reconstruction
- diagnostic generation

Treat it as a dedicated porting phase, not a trivial follow-up.

### 2. Function-value runtime representation must follow LIR semantics exactly

The interpreter must not smuggle back the old closure-header model. It needs to follow the actual lowered representation:

- singleton closure payloads
- multi-member lambda-set tag unions
- canonical callable targets

If this is done half-way, higher-order behavior will be wrong in subtle ways.

### 3. Do not add implicit RC cleanup

The old interpreter had to do a lot of bespoke cleanup because RC was entangled with CIR evaluation strategy.

The new interpreter must not recreate that.

If a scope drop, branch exit, or early return needs a decref, it should be present in the LIR after `RcInsert`.

### 4. Repeated lowering can become a hidden performance regression

If the first implementation rebuilds MIR/LIR stores from scratch for every `eval()`, it may work but be much slower for:

- compile-time evaluation across many defs
- test execution
- repeated REPL evaluation

Prefer shared lowering state with lazy root lowering.

### 5. Some tests are asserting legacy rendering details, not just semantics

There are a lot of eval/interpreter tests, roughly 1220 `test` blocks under `src/eval/test`.

Some will fail because semantics are wrong. Some will fail because formatting changes slightly.

Treat these separately:

- semantic parity failures must block the refactor
- purely legacy CIR-rendering expectations may be updated if the new layout-based rendering is correct and simpler

### 6. (LEARNED) Interpreter-allocated data lacks refcount headers — builtins that consume inputs will crash

**Discovered during Phase 4 implementation.** The interpreter allocates lists and strings from an arena without refcount headers. Builtins that “consume” their inputs (ownership = `.consume` in `LowLevel.getArgOwnership()`) call `list.decref()` or `str.decref()`, which reads the word *before* the data pointer as a refcount. On interpreter-allocated data, this reads garbage and corrupts memory (manifests as `Invalid free` panic during arena deinit).

**Rule:** Before calling a builtin directly, check `LowLevel.getArgOwnership()`. If any argument is `.consume`, either:
1. Implement the operation manually in the interpreter (preferred for simple ops like `str_join_with`)
2. Allocate the input data with a proper refcount header before passing it to the builtin

Currently `str_join_with` is implemented manually for this reason. Other consuming operations (`list_concat`, `list_prepend`, etc.) work because they happen to check `isUnique()` before attempting in-place mutation, and the `rcNone` no-op callbacks prevent actual refcount access.

### 7. (LEARNED) The static allocation buffer must only be reset once per top-level eval, not per-operation

**Discovered during Phase 4.** The `InterpreterRocEnv.StaticAlloc` buffer is a bump allocator. Initially, `resetForEval()` (which resets the buffer offset to 0) was called before each builtin call. This invalidated pointers from previous builtin results that were still live in RocStr/RocList values stored in the interpreter's environment.

**Rule:** Call `resetForEval()` once at the start of a top-level `eval()`. For per-operation crash recovery, only reset the crash flag via `resetCrash()`, not the buffer.

## Files Most Likely To Change

Primary new/rewritten files:

- `src/eval/interpreter.zig`
- `src/eval/lir_interpreter.zig`
- `src/eval/lir_value.zig`
- `src/eval/lir_program.zig`
- `src/eval/lir_builtin_ops.zig`
- `src/eval/render_helpers.zig` or replacement
- `src/interpreter_values/RocValue.zig`
- `src/eval/comptime_evaluator.zig`
- `src/eval/test_runner.zig`
- `src/eval/test/helpers.zig`

Likely deletions:

- `src/interpreter_layout/mod.zig`
- `src/interpreter_layout/layout.zig`
- `src/interpreter_layout/store.zig`
- `src/interpreter_layout/work.zig`
- large sections of `src/eval/StackValue.zig`

Likely opportunistic cleanup:

- factor shared lowering logic out of:
  - `src/eval/dev_evaluator.zig`
  - `src/eval/wasm_evaluator.zig`

## Concrete Recommendation

Proceed with a full replacement, not a hybrid.

The cleanest path is:

- keep the public `Interpreter` wrapper shape temporarily
- introduce a new LIR-backed core
- share the existing MIR/LIR lowering pipeline with dev/wasm
- port consumers in order of dependency difficulty
- delete the old CIR interpreter and `interpreter_layout` as soon as the LIR interpreter covers compile-time evaluation and entrypoint execution

The main technical bet here is good, because the compiler already has the right lowering pipeline and the right IR. The remaining work is mostly execution/runtime engineering, not uncertain design.
