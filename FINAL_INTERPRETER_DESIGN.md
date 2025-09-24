# Roc Interpreter: Final Type-Carrying Design (with O(1) Var→Layout and Poly Instantiation Cache)

This document specifies the final interpreter design that implements:

- A type-carrying execution model with parallel value and type stacks
- A single runtime type store shared across modules
- O(1) type-variable to layout lookup using a per-var slot cache
- A cache for polymorphic function instantiations to avoid repeated freshening/unification

The design aims for correctness under polymorphism and cross-module calls while being fast enough for interactive use.

## Goals

- Unboxed value representation for performance
- Preserve polymorphic relationships at runtime (no premature conversion to layouts)
- Make Var→Layout resolution O(1) with zero hashing overhead on the hot path
- Avoid repeated instantiation/unification for common polymorphic call sites
- Keep the interpreter statefully simple and debuggable

## High-Level Architecture

- Parallel stacks
  - `value_stack`: logical values (unboxed bytes in `stack_memory`, with offsets)
  - `type_stack`: one `types.Var` per logical value, in lock-step with `value_stack`

- Stores
  - `runtime_types: types.Store` — a single, unified store for all runtime type variables (all modules)
  - `runtime_layout_store: layout.Store` — the layout store used to compute layouts from runtime types

- Translation
  - `translateTypeVar(module, compile_var) -> runtime_var` — deep-copies compile-time type structures into `runtime_types` (memoized)

- Unification (runtime)
  - Use the existing unifier, but on `runtime_types`, to constrain polymorphic calls when needed

## O(1) Var→Layout Cache

Layout lookups are extremely hot: field access, tuple indexing, memory allocation, copying, etc. To make these O(1) with no hash cost:

- Maintain an array `var_to_layout_slot: []u32` parallel to `runtime_types.slots` (same length semantics)
  - Slot value `0` means “layout not computed yet”
  - Any computed layout index `idx: layout.Idx` is stored as `@intFromEnum(idx) + 1`
    - We use a +1 bias so we can keep `0` as the sentinel even though `layout.Idx` 0 (e.g. bool) is a valid layout
  - The array is grown (and zero-initialized) any time `runtime_types` grows (e.g. after translation or fresh var creation)

- API
  - `fn ensureVarLayoutCapacity(self, min_len: usize)`: grows `var_to_layout_slot` to at least `min_len`, zero-filling new slots
  - `fn getRuntimeLayout(self, var: types.Var) !layout.Layout`:
    1. `const slot = var_to_layout_slot[@intFromEnum(root(var))]`
    2. If `slot != 0`, return `runtime_layout_store.getLayout(@enumFromInt(slot - 1))`
    3. Else, compute `idx = try runtime_layout_store.addTypeVar(root(var), /*empty scope*/)`; store `slot = @intFromEnum(idx) + 1`; return the layout
    - `root(var)` refers to the resolved representative var from `runtime_types.resolveVar(var).var_`

- Properties
  - Completely avoids hashing for Var→Layout
  - Works for any layout (records/tuples/closures/etc.)
  - Stable for the lifetime of the interpreter (no invalidation needed because `runtime_types` is append-only and unification redirects maintain a root representative)

## Polymorphic Instantiation Cache

Repeated polymorphic calls with the same argument types should not repeatedly freshen/unify. We cache per-call-site specializations:

- Key
  - `PolyKey = { func_id: u32, arity: u8, arg_roots: [MAX_ARITY]types.Var }`
  - `func_id` is a stable identifier for the function body (e.g., lambda expr index or def id)
  - `arg_roots[i] = runtime_types.resolveVar(arg_var_i).var_` (resolved root vars)
  - This makes identical argument types map to a stable key without structural hashing in the common case
  - Note: if two distinct runtime var graphs are isomorphic but not unified, keys may differ; we accept this initially. We can add an optional structural hash later if needed.

- Entry
  - `PolyEntry = { return_var: types.Var, return_layout_slot: u32 /*biased index for O(1) layout*/, /*optional stats*/ }`
  - We only need the return var/layout to pre-allocate the return slot; body evaluation uses actual argument values/types on the stack

- Cache
  - `poly_cache: std.AutoHashMap(PolyKey, PolyEntry)` — small and hot; lookups are per call site, not per value op
  - Optional LRU for bounded size; not required initially

- Miss path (first time for a signature)
  1. Translate the function’s compile-time type to `runtime_types` (memoized) to get a generalized template var
  2. Instantiate (freshen) the template: create fresh runtime vars for each generalized type variable in the function type
  3. Unify instantiated parameter vars with the actual argument runtime vars (using `unifyWithContext` on `runtime_types`)
  4. The instantiated return var is now constrained; compute its layout via the per-var layout slot cache (it will get a slot)
  5. Insert `PolyEntry{ return_var, return_layout_slot }` into `poly_cache`
  6. Use `return_layout_slot` to pre-allocate the return slot

- Hit path (subsequent calls with same signature)
  1. Lookup `PolyKey`
  2. Read `return_layout_slot`; pre-allocate return slot without any freshening or unification
  3. Use `return_var` for any typing of the returned value as needed

- Why caching instantiated vars is safe
  - Runtime evaluation does not further constrain type variables; layout needs are derived from actual values’ runtime vars
  - Reusing the same instantiated return var for repeated signatures is stable; it does not grow or change between calls
  - This avoids both the time cost (freshening + unification) and the memory cost (fresh instantiation graphs) on hot call sites

- Future improvement: structural type keys
  - If two different runtime var roots are structurally equal but not unified, we can miss cache hits
  - Add an optional structural hash for `types.Var` (shallow memoized hash that folds over resolved content) to build a `TypeKey` per var
  - Extend `PolyKey` to use `arg_keys: [MAX_ARITY]TypeKey` instead of `arg_roots` when needed

## Execution Flow With Caches

- Pushing any value
  1. Compute the expression’s runtime type var: `translateTypeVar(env, ModuleEnv.varFrom(expr_idx))`
  2. Push bytes (allocating via `getRuntimeLayout(var)`), push `var` onto `type_stack`

- Function call (arity = N)
  1. Gather argument runtime vars from `type_stack` (top N, respecting the calling convention)
  2. Build `PolyKey{ func_id, arity=N, arg_roots = resolveRoots(args) }`
  3. Lookup `poly_cache`
     - Hit: use `return_layout_slot` to pre-alloc the return slot; `return_var` is known if needed
     - Miss: instantiate + unify once, compute `return_layout_slot` via Var→Layout slot, cache entry, then pre-alloc
  4. Push captures view value (with its runtime var) and bind parameters to arguments at the value/type stacks level
  5. Evaluate body; on return, copy result bytes and mirror pops/moves on both stacks

## Cross-Module Types

- A single `runtime_types` store ensures polymorphic relations work across modules
- `translateTypeVar(module, var)` deep-copies compile-time content into `runtime_types`, recursively translating referenced vars
- A small translation cache `(module_id, compile_var) -> runtime_var` avoids repeated deep copies

## Data Structures (Interpreter Fields)

- Memory
  - `stack_memory: *stack.Stack`

- Value and type stacks
  - `value_stack: ArrayList(InternalStackValue { offset: u32 /*(layout removed in final refactor)*/ })`
  - `type_stack: ArrayList(types.Var)`

- Types and layouts
  - `runtime_types: types.Store`
  - `runtime_layout_store: layout.Store`
  - `var_to_layout_slot: []u32` (0 = unset; else layout_idx_plus_one)
  - `translate_cache: AutoHashMap(VarKey{module_id, compile_var}, types.Var)`

- Poly cache
  - `poly_cache: AutoHashMap(PolyKey, PolyEntry)`

- Unifier support (runtime)
  - `unify_scratch: check.unify.Scratch`
  - `problems: check.problem.Store`
  - `snapshots: check.snapshot.Store`

## API Outline

- Var→Layout
  - `fn ensureVarLayoutCapacity(self, min_len: usize) !void`
  - `fn getRuntimeLayout(self, var: types.Var) !layout.Layout`

- Translation
  - `fn translateTypeVar(self, module: *ModuleEnv, compile_var: types.Var) !types.Var`
  - `fn copyTypeToRuntime(self, module: *ModuleEnv, content: types.Content) !types.Var`

- Poly cache
  - `fn makePolyKey(self, func_id: u32, args: []const types.Var) PolyKey`
  - `fn lookupPoly(self, key: PolyKey) ?PolyEntry`
  - `fn insertPoly(self, key: PolyKey, entry: PolyEntry) !void`

- Calls
  - `fn prepareCall(self, func_expr_idx: CIR.Expr.Idx, arg_count: u32) !CallInfo` — handles key build, cache lookup/miss work, return slot alloc

## Migration Notes

1) Introduce `type_stack`, `runtime_types`, `runtime_layout_store`, and `var_to_layout_slot` (no behavior changes yet)
2) Start pushing runtime vars with every value; use `getRuntimeLayout(var)` lazily for allocation/offsets
3) Implement poly cache miss/hit paths in the call machinery; remove TypeScope from call path
4) Replace compile-time layout queries with `getRuntimeLayout` everywhere
5) Remove `layout` from `InternalStackValue`; compute layouts on-demand from `type_stack`
6) Optional: add structural `TypeKey` if needed for better cache hit rates

## Correctness and Performance

- Correctness
  - Type relations preserved as runtime vars; operations compute offsets/sizes from those via layout-on-demand
  - Cross-module polymorphism handled by a unified runtime store + translation cache
  - Calls either instantiate+unify once per signature or reuse cached result; no sharing of mutable instantiated var graphs across different signatures

- Performance
  - Var→Layout lookups are O(1), no hashing, with a +1 biased slot array
  - Polymorphic calls hit the cache in tight loops (no freshening or unification on hot paths)
  - Layout computations themselves are cached inside `runtime_layout_store`

## Open Questions / Future Work

- Add an optional structural hash to strengthen the poly cache keys when `resolveVar(var).var_` identity misses structurally-equal types
- Add an (optional) size cap + LRU for `poly_cache` if workloads generate many one-off specializations
- Consider recording per-var structural hashes to accelerate structural-key computation if adopted

## Summary

This design:

- Preserves polymorphism by carrying type vars alongside values
- Computes layouts only when needed and makes Var→Layout O(1)
- Avoids repeated polymorphic freshening/unification in hot loops via a poly cache
- Scales to cross-module calls using one runtime type store and a translation cache

These changes bring the interpreter in line with the desired type-carrying architecture while targeting the most critical performance bottlenecks.

