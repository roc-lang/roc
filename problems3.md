# Code Review: `lower-mir` Branch

## 1. CORRECTNESS ISSUES

### 1.1 [FIXED] Pattern alternatives silently dropped in `lowerMatch`

Fixed in commit 1747e08b73. All three lowering paths now iterate over all branch patterns.

### 1.2 [FIXED] RC insertion uses global use counts across control flow branches

Fixed in commit ac6bb6cabb. Implemented "branch-owns-its-RC" model: each branching construct (when/if) contributes 1 use per symbol to the enclosing scope. Per-branch RC ops adjust at branch entry (decref if unused, incref if used >1 time). Pattern-bound symbols are excluded from per-branch analysis. Both `src/lir/rc_insert.zig` and `src/mono/rc_insert.zig` updated. 4 new branch-aware tests added.

### 1.3 [FIXED] RC insertion doesn't process lambda bodies, loop bodies, or closures

Fixed in latest commit. `processExpr` now handles `.lambda`, `.closure`, `.for_loop`, and `.while_loop` in both `src/lir/rc_insert.zig` and `src/mono/rc_insert.zig`. Lambda/for-loop bodies get body-local use counting with RC ops for parameters/element bindings (decref if unused, incref if multi-use). `processBlock` was also fixed for reentrancy (local buffer instead of shared `self.stmt_buf`) and now processes `stmt.expr` values so lambdas bound in block statements get RC-processed. 6 new tests added.

### 1.4 [FIXED] `lowerRecordAccess` silently defaults to `field_idx = 0` for non-records

Fixed in latest commit. Changed `else => {}` to an exhaustive match listing all non-record monotype variants (`.func`, `.tag_union`, `.tuple`, `.list`, `.prim`, `.box`, `.unit`) as `unreachable`.

### 1.5 [NOT A BUG] `lowerInt` discards the monotype — always emits i64 or i128

This is by design. LIR integer literals use two "storage class" variants (`i64_literal` / `i128_literal`) based on value magnitude, not type. The actual type width is carried separately in `layout_idx` on patterns and variable bindings. The codegen always uses the destination layout's size when storing values, so truncation/extension is handled correctly. Adding a monotype lookup to every integer literal would be a performance regression with no observable benefit.

### 1.6 [FIXED] `break_expr` lowered to `runtime_error`

Fixed in latest commit. Added `break_expr: void` variant to LirExpr, lowered properly in MirToLir, added to RC insertion passes (countUsesInto + exhaustive processExpr/countRcOps switches in both lir and mono rc_insert.zig), and implemented codegen via `loop_break_patches` (forward-jump patched to loop exit offset) in both `generateForLoop` and `generateWhileLoop`.

## 2. BRITTLENESS / ERROR-PRONE ISSUES

### 2.1 [FIXED] No debug assertion that tag union tags are sorted by name

Fixed in latest commit. Added `std.debug.assert` in `tagDiscriminant` that verifies tags are sorted by name index.

### 2.2 [FIXED] Tag union layout uses proper `layout.tag_union` instead of tuple approximation

Fixed in latest commit. Added `putTagUnion` method to `layout.Store` and rewrote `layoutFromTagUnion` in MirToLir to use it, producing proper `.tag_union` layouts with correct discriminant offset/size and per-variant payload layouts. New test verifies multi-tag union layout correctness.

### 2.3 [FIXED] `roc_str_size` used instead of `roc_list_size` in empty list codegen

**File:** `src/backend/dev/LirCodeGen.zig:7794, 7821`

The empty list generation uses `roc_str_size` (24 bytes) instead of `roc_list_size` (24 bytes). Both happen to be 24 bytes, so this is not a bug today, but if either constant changes, it will silently break.

**Fix:** Replace `roc_str_size` with `roc_list_size` at those two sites.

### 2.4 [MEDIUM] Closure struct layout uses `ident_idx` as record field name

**File:** `src/lir/MirToLir.zig:494`

```zig
try cap_field_names.append(self.allocator, cap.symbol.ident_idx);
```

Using the captured variable's `ident_idx` as a record field name for the closure struct is fragile. If two captured variables from different modules share the same `ident_idx`, the layout store could treat them as the same field, leading to incorrect closure struct layout.

**Fix:** Use synthetic unique field names for closure struct fields (e.g., `_cap0`, `_cap1`), or verify that `ident_idx` values are unique per closure.

### 2.5 [MEDIUM] `propagating_defs` recursion guard in `lowerLookup` doesn't handle mutual recursion between symbol defs

**File:** `src/lir/MirToLir.zig:373-387`

The `propagating_defs` guard prevents infinite recursion for a single symbol, but if symbol A's def references symbol B whose def references symbol A, the guard only protects A's entry, not B's re-entry to A. However, the LIR store's `getSymbolDef` check at line 377 provides a second layer of protection since A would be registered by the first call. This is likely fine in practice but could be made more explicit.

## 3. PERFORMANCE ISSUES

### 3.1 [MEDIUM] `layoutFromMonotype` creates temporary ArrayLists for every record/tuple/tag

**File:** `src/lir/MirToLir.zig:141-246`

`layoutFromRecord`, `layoutFromTuple`, and `layoutFromTagUnion` each create fresh `std.ArrayList` instances that are immediately deinitialized. For a large program with many type references, this causes many small heap allocations.

**Fix:** Use scratch buffers (like the scratch buffers already on `Self` for expr/pattern IDs) for layout building. Add `scratch_layouts: std.ArrayList(layout.Layout)` and `scratch_field_names: std.ArrayList(Ident.Idx)` to `Self` and reuse them with the save/restore pattern.

### 3.2 [LOW] `tagDiscriminant` is O(n) linear scan

**File:** `src/lir/MirToLir.zig:250-264`

For every tag expression and tag pattern, `tagDiscriminant` scans the full tag list. For tag unions with many variants (e.g., 50+ tags in error types), this is O(n) per tag reference.

**Fix:** If this becomes a bottleneck, cache tag-name-to-discriminant mappings per union monotype. For now, this is likely fine since most tag unions are small.

### 3.3 [LOW] RC insertion `countUses` traverses entire tree then `processExpr` traverses again

**File:** `src/lir/rc_insert.zig:76-81`

Two full tree traversals (count phase + transform phase). For very large expression trees, this doubles the work. Note: the branch-aware fix (1.2) adds additional local counting per branch during both phases, making this slightly more expensive but correct.

**Fix:** Merge the two passes into a single bottom-up traversal that both counts and inserts RC operations.

## 4. TEST COVERAGE GAPS

### 4.1 [FIXED] Pattern alternatives (`A | B => body`) in match expressions
Tests added in MirToLir.zig, lower_test.zig, and eval_test.zig as part of 1.1 fix.

### 4.2 [FIXED] RC insertion for lambda bodies and closures
Tests added in `src/lir/rc_insert.zig` as part of 1.3 fix: lambda body with nested block, refcounted param used twice (incref), unused refcounted param (decref), for_loop element used twice, closure wrapping lambda, and block with lambda in stmt.expr.

### 4.3 [FIXED] RC insertion across control flow branches
Tests added in `src/lir/rc_insert.zig` as part of 1.2 fix: symbol used in both branches (no incref), one branch only (decref in unused), multiple times in one branch (incref+decref), and used both inside and outside branches.

### 4.4 Multi-tag union lowering (discriminant correctness)
The test for `zero_arg_tag` only tests a single-tag union (discriminant 0). There's no test for a multi-tag union verifying that different tags get different discriminants, especially one verifying alphabetical ordering.

### 4.5 Closure with captures lowering
No test in MirToLir verifies that a lambda with captures produces a `closure` LIR node with correct capture layouts and closure representation (single capture → `unwrapped_capture`, multiple → `struct_captures`).

### 4.6 Record/tuple access field index correctness
No test verifies that `lowerRecordAccess` finds the correct field index when the record has multiple fields.

### 4.7 Cross-module symbol def propagation in MirToLir
`lowerLookup` propagates MIR symbol defs to LIR, but no test verifies this works correctly for cross-module references.

### 4.8 [FIXED] `break_expr` behavior
Fixed as part of 1.6 — `break_expr` now properly lowers through MirToLir → LIR → codegen with forward-jump patching.

## 5. NAMING ISSUES

### 5.1 [MEDIUM] LIR/Mono IR types still use "when" instead of "match"

The surface language uses `match`, not `when`, but the LIR and Mono IR layers still use "when" naming throughout:
- `LirWhenBranch` / `LirWhenBranchSpan` (in `src/lir/LIR.zig`)
- `MonoWhenBranch` / `MonoWhenBranchSpan` (in `src/mono/MonoIR.zig`)
- `.when` variant in LIR/Mono expression enums
- `when_branches` field in `LirExprStore` and `MonoExprStore`
- `addWhenBranches` / `getWhenBranches` methods
- `scratch_lir_when_branches` in `MirToLir.zig`
- `lowerWhenBranches` in `src/lir/Lower.zig` and `src/mono/Lower.zig`
- `processWhen` in `src/lir/rc_insert.zig` and `src/mono/rc_insert.zig`
- `generateWhen` / `generateWhenBranches` in codegen files
- Stale comments referencing "WhenBranch" in `src/parse/NodeStore.zig`

**Fix:** Rename all to use "match" naming: `LirMatchBranch`, `MonoMatchBranch`, `.match`, `match_branches`, `addMatchBranches`, `lowerMatchBranches`, `processMatch`, `generateMatch`, etc.

## Summary of Recommended Priorities

| Priority | Issue | Impact |
|----------|-------|--------|
| P0 | 1.1 Pattern alternatives dropped | Wrong match behavior at runtime |
| ~~P0~~ | ~~1.2 Global RC use counts~~ | ~~Memory leaks~~ FIXED |
| ~~P0~~ | ~~1.3 RC doesn't process lambdas/loops~~ | ~~Missing RC ops → leaks or use-after-free~~ FIXED |
| ~~P1~~ | ~~1.4 Record access defaults to idx 0~~ | ~~Wrong field access (compiler bug path)~~ FIXED |
| ~~P1~~ | ~~2.1 No sorted-tags assertion~~ | ~~Silent wrong discriminants~~ FIXED |
| ~~P1~~ | ~~2.2 Tag union layout approximation~~ | ~~Potential layout mismatch with codegen~~ FIXED |
| ~~P1~~ | ~~1.6 break_expr → runtime_error~~ | ~~Break crashes at runtime~~ FIXED |
| ~~P2~~ | ~~1.5 Int literal type discarded~~ | ~~Potential wrong width in codegen~~ NOT A BUG |
| P2 | 2.3 roc_str_size for lists | Latent bug if constants diverge |
| P2 | 2.4 Closure field name collisions | Potential layout corruption |
| P2 | 3.1 Temp ArrayLists in layout | Performance waste |
