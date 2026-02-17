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

Fixed in latest commit. Replaced all three `roc_str_size` → `roc_list_size` in empty list and list literal codegen paths.

### 2.4 [FIXED] Closure struct layout uses `ident_idx` as record field name

Fixed in latest commit. Switched from `putRecord` (which required field names and could collide) to `putCaptureStruct` (tuple-based, positional), matching what the old CIR→LIR path already does. Codegen accesses captures by positional byte offset, never by name.

### 2.5 [MEDIUM] `propagating_defs` recursion guard in `lowerLookup` doesn't handle mutual recursion between symbol defs

**File:** `src/lir/MirToLir.zig:373-387`

The `propagating_defs` guard prevents infinite recursion for a single symbol, but if symbol A's def references symbol B whose def references symbol A, the guard only protects A's entry, not B's re-entry to A. However, the LIR store's `getSymbolDef` check at line 377 provides a second layer of protection since A would be registered by the first call. This is likely fine in practice but could be made more explicit.

## 3. PERFORMANCE ISSUES

### 3.1 [FIXED] `layoutFromMonotype` creates temporary ArrayLists for every record/tuple/tag

Fixed in latest commit. Added `scratch_layouts`, `scratch_layout_idxs`, and `scratch_field_names` scratch buffers to `Self`, replacing per-call `ArrayList` allocations in `layoutFromRecord`, `layoutFromTuple`, and `layoutFromTagUnion` with the save/restore pattern used by other scratch buffers.

### 3.2 [LOW] `tagDiscriminant` is O(n) linear scan

**File:** `src/lir/MirToLir.zig:250-264`

For every tag expression and tag pattern, `tagDiscriminant` scans the full tag list. For tag unions with many variants (e.g., 50+ tags in error types), this is O(n) per tag reference.

**Fix:** If this becomes a bottleneck, cache tag-name-to-discriminant mappings per union monotype. For now, this is likely fine since most tag unions are small.

### 3.3 [WON'T FIX] RC insertion `countUses` traverses entire tree then `processExpr` traverses again

**File:** `src/lir/rc_insert.zig:76-81`

Two full tree traversals (count phase + transform phase). Merging into a single pass is **not safe**: `processBlock` needs forward-looking use counts at the point of each binding (e.g., when it sees `x = make_list()`, it must already know x is used 2 times total to emit incref, but those uses appear in later statements). A single bottom-up pass would require fundamentally restructuring `processBlock` to defer all RC decisions until after all uses are seen — a much larger refactor with no correctness benefit. The two-pass approach is the standard design for this kind of analysis.

## 4. TEST COVERAGE GAPS

### 4.1 [FIXED] Pattern alternatives (`A | B => body`) in match expressions
Tests added in MirToLir.zig, lower_test.zig, and eval_test.zig as part of 1.1 fix.

### 4.2 [FIXED] RC insertion for lambda bodies and closures
Tests added in `src/lir/rc_insert.zig` as part of 1.3 fix: lambda body with nested block, refcounted param used twice (incref), unused refcounted param (decref), for_loop element used twice, closure wrapping lambda, and block with lambda in stmt.expr.

### 4.3 [FIXED] RC insertion across control flow branches
Tests added in `src/lir/rc_insert.zig` as part of 1.2 fix: symbol used in both branches (no incref), one branch only (decref in unused), multiple times in one branch (incref+decref), and used both inside and outside branches.

### 4.4 [FIXED] Multi-tag union lowering (discriminant correctness)
Test added: creates [Bar, Foo I64] union, lowers both tags, verifies Bar→discriminant 0 (zero_arg_tag) and Foo→discriminant 1 (tag with payload).

### 4.5 [FIXED] Closure with captures lowering
Two tests added: single capture → verifies `.closure` with `unwrapped_capture` representation; multiple captures → verifies `.closure` with `struct_captures` representation.

### 4.6 [FIXED] Record/tuple access field index correctness
Two tests added: record access on 3-field record verifies `field_idx == 2` for the third field; tuple access verifies `elem_idx == 2` for the third element.

### 4.7 [FIXED] Cross-module symbol def propagation in MirToLir
Test added: registers `x = 42` in MIR symbol defs, lowers a lookup to x, verifies the LIR store now has the propagated def as an `i64_literal` with value 42.

### 4.8 [FIXED] `break_expr` behavior
Fixed as part of 1.6 — `break_expr` now properly lowers through MirToLir → LIR → codegen with forward-jump patching.

## 5. NAMING ISSUES

### 5.1 [FIXED] LIR/Mono IR types still use "when" instead of "match"

Fixed in latest commit. Renamed all when-related identifiers across 13 files to use "match" naming: `LirMatchBranch`, `MonoMatchBranch`, `.match_expr` (not `.match` since `match` is a Zig keyword), `match_branches`, `addMatchBranches`, `lowerMatchBranches`, `processMatch`, `generateMatch`, etc.

### 5.2 [WON'T FIX] still have "src/mono/" dir, should be renamed

`src/mono/` and `src/lir/` are two distinct, parallel IR layers — not a rename candidate. `src/mono/` (MonoIR) is the actively-used path: it powers MonoExprCodeGen, WasmCodeGen, dev_evaluator, wasm_evaluator, snapshot_tool, and the CLI. `src/lir/` (LIR) is the newer MIR→LIR path with its own codegen (LirCodeGen) that exists alongside mono but isn't yet wired into the main pipeline. They serve different roles and both need to exist.

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
| ~~P2~~ | ~~2.3 roc_str_size for lists~~ | ~~Latent bug if constants diverge~~ FIXED |
| ~~P2~~ | ~~2.4 Closure field name collisions~~ | ~~Potential layout corruption~~ FIXED |
| ~~P2~~ | ~~3.1 Temp ArrayLists in layout~~ | ~~Performance waste~~ FIXED |
