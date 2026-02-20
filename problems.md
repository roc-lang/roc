# Code Review: `lower-mir` Branch

## HIGH — Likely to produce wrong code in specific scenarios

### 1. ~~`lowerRecord` silently drops fields if layout/MIR field names diverge~~ FIXED

**File:** `src/lir/MirToLir.zig:372-382`

Added `found` flag and `std.debug.assert(found)` to catch missing layout fields in debug builds.

---

### 2. ~~`emitZeroLiteral` outer `else` should be `unreachable`~~ FIXED

**File:** `src/lir/MirToLir.zig:774-783`

Made both inner and outer switches exhaustive — non-prim monotypes are now `unreachable`.

---

### 3. TailRecursion.zig is not wired into the LIR pipeline — DEFERRED

**File:** `src/lir/TailRecursion.zig` (760 lines of dead code)

Neither `MirToLir.zig` nor `LirCodeGen.zig` reference `TailRecursion` anywhere. Tail-recursive functions going through the MIR->LIR->LirCodeGen path will not be optimized, causing stack overflows on deep recursion.

**Fix:** Wire TailRecursion into the pipeline as a post-lowering pass before RC insertion. Requires writing `exprToCFStmt` conversion and `lowerProc` method.

---

### 4. ~~`processEarlyReturn` reuses `early_ret_id` as both statement and `final_expr`~~ FIXED

**File:** `src/lir/rc_insert.zig:1085-1095`

Replaced reuse of `early_ret_id` with a distinct `runtime_error` dead expression for `final_expr`.

---

### 5. ~~`enum_dispatch.tag` is `u8` but `num_functions` is `u16`~~ FALSE POSITIVE

**File:** `src/lir/LIR.zig:136-143`

Investigation showed `tag` was already `u16`. Updated misleading "single byte tag" comment.

---

### 6. ~~Tag union comparison `end_patches` uses fixed-size array of 64~~ NO CHANGE NEEDED

**File:** `src/backend/dev/LirCodeGen.zig:5903`

Already has `std.debug.assert(end_patch_count < end_patches.len)` which catches overflow in debug builds. Tag unions with >64 non-ZST variants are extremely unlikely.

---

### 7. ~~`generateStmt` ret handler accesses `.stack.offset` without checking ValueLocation variant~~ FALSE POSITIVE

**File:** `src/backend/dev/LirCodeGen.zig:14304-14363`

Investigation showed `.stack.offset` accesses are all inside `else if (value_loc == .stack)` guards. The `.stack_str` and `.list_stack` variants are handled in separate branches above.

---

### 8. ~~`generateMatch` potential null unwrap for ZST result types~~ FIXED

**File:** `src/backend/dev/LirCodeGen.zig:7731`

Added defensive null check — returns `immediate_i64(0)` for ZST results instead of unconditional unwrap.

---

### 9. ~~Float NaN comparison may be incorrect on x86_64~~ FIXED

**File:** `src/backend/dev/LirCodeGen.zig:6686-6757`

Fixed all four incorrect float comparison conditions on x86_64:
- **eq**: compound `sete + setnp + and` (true only if equal AND ordered)
- **neq**: compound `setne + setp + or` (true if not equal OR unordered)
- **lt**: swap operands + `above` condition (NaN-safe)
- **lte**: swap operands + `above_or_equal` condition (NaN-safe)
- **gt/gte**: already correct (kept as-is)

---

## MEDIUM — Brittleness, Error-proneness, Missing Functionality

### 10. `insertRcOps` doesn't clear state between calls

**File:** `src/lir/rc_insert.zig:116-122`

If `insertRcOps` is called multiple times on different expression trees, `symbol_use_counts` and `symbol_layouts` accumulate across calls, inflating counts.

**Fix:** Add `self.symbol_use_counts.clearRetainingCapacity()` and `self.symbol_layouts.clearRetainingCapacity()` at the start of `insertRcOps`.

---

### 11. Seven nearly-identical pattern-recursion functions in `rc_insert.zig`

**File:** `src/lir/rc_insert.zig`

Functions: `registerPatternSymbolInto`, `collectPatternSymbols`, `emitMutateDecrefsForPattern`, `trackLiveRcSymbolsForPattern`, `emitBlockIncrefsForPattern`, `emitBlockDecrefsForPattern`, `emitRcOpsForPatternInto`.

All 7 recurse through patterns identically, differing only in leaf behavior. If a new pattern variant is added, all 7 must be updated in lockstep.

**Fix:** Extract a generic `walkPatternBinds(pat_id, callback)` helper.

---

### 12. `layoutFromRecord` accesses `layout_store.all_module_envs` directly

**File:** `src/lir/MirToLir.zig:157`

```zig
const env = self.layout_store.all_module_envs[self.layout_store.current_module_idx];
```

Should use the accessor method `layout_store.currentModuleEnv()` instead of raw array indexing.

---

### 13. `addExprSpan` and similar span builders are not atomic on OOM

**File:** `src/lir/LirExprStore.zig:211-226`

If `append` fails mid-loop, orphaned items are left in `extra_data`. Inconsistent with the care taken in `addExpr`/`addPattern` which use `ensureUnusedCapacity`.

**Fix:** Use `ensureUnusedCapacity` + `appendAssumeCapacity`:
```zig
try self.extra_data.ensureUnusedCapacity(self.allocator, expr_ids.len);
for (expr_ids) |id| {
    self.extra_data.appendAssumeCapacity(@intFromEnum(id));
}
```
Apply to `addPatternSpan`, `addFieldNameSpan`, and `addLayoutIdxSpan` as well.

---

### 14. `list_split_first` and `list_split_last` unimplemented in LirCodeGen

**File:** `src/backend/dev/LirCodeGen.zig:3046-3055`

Both hit `unreachable`, crashing the compiler if any Roc code uses `List.splitFirst` or `List.splitLast`.

---

### 15. `lowerHosted` ignores `h.body` and `h.symbol_name`

**File:** `src/lir/MirToLir.zig:1133-1187`

The MIR `hosted` expression has `body` and `symbol_name` fields that are completely ignored. If MIR ever produces a hosted expression whose body does more than forward parameters, that logic is silently lost.

**Fix:** At minimum, add a debug assertion or comment documenting that `h.body` is intentionally ignored and why.

---

### 16. Massive code duplication between `generateMatch` and `generateMatchStmt`

**File:** `src/backend/dev/LirCodeGen.zig` — ~1000 lines duplicated

These functions share identical pattern-matching logic. The only difference is expression vs. statement body generation.

**Fix:** Extract shared pattern-matching helpers.

---

### 17. `processBlock` always allocates a new block even when nothing changed

**File:** `src/lir/rc_insert.zig:654-661`

When `!changed`, the code still calls `self.store.addExpr` to create a new identical block.

**Fix:** Thread the original `expr_id` through to `processBlock` and return it when unchanged.

---

### 18. `generateDiscriminantSwitch` holds `disc_reg` live across all branch bodies

**File:** `src/backend/dev/LirCodeGen.zig:9824`

If any branch body calls a C function (which clobbers caller-saved registers) and `disc_reg` is caller-saved, the value is corrupted for subsequent branches.

**Fix:** Spill `disc_reg` to a stack slot before generating branch bodies, reload for each comparison.

---

### 19. `copyChunked` with `size <= 8` does full 8-byte load/store

**File:** `src/backend/dev/LirCodeGen.zig:8889-8892`

For sizes 1-7, copies a full 8 bytes. For list elements packed at alignment < 8, this over-reads/writes adjacent memory.

**Fix:** Use sized loads/stores for the actual byte count, similar to `copyBytesToStackOffset`.

---

### 20. `countUsesInto` for `lambda` uses `target.contains(key)` to detect captures

**File:** `src/lir/rc_insert.zig:244-258`

If a lambda appears as the RHS of the *first* statement in a block and captures a symbol from an outer scope that hasn't been counted yet in `target`, it won't detect it as a capture.

**Fix:** Either document this assumption (captured symbols are always defined before the lambda in the tree), or use `self.symbol_layouts` (accumulated globally) as the oracle for "does this symbol exist in any outer scope."

---

### 21. `lowerLambda` closure always sets `recursion = .not_recursive`

**File:** `src/lir/MirToLir.zig:570-573,590-593`

Both single-capture and multi-capture closures are always emitted with `.recursion = .not_recursive` and `.self_recursive = .not_self_recursive`.

**Fix:** Add a comment: `// Recursion flags are set by the TailRecursion pass after lowering`.

---

### 22. `std.debug.panic` used in `storeResultToSlot` for lambda/closure

**File:** `src/backend/dev/LirCodeGen.zig:9927`

`std.debug.panic` is a no-op in release mode (becomes UB `unreachable`).

**Fix:** Replace with `unreachable` per the design principle.

---

## LOW — Performance, Robustness, Code Quality

### 23. `countUsesLocal` allocates a fresh HashMap per call

**File:** `src/lir/rc_insert.zig:132-137`

Called once per statement in `processBlock` and once per branch in match/if_then_else processing. O(N+M) HashMap allocations per function.

**Fix:** Pool or reuse a scratch map on the `RcInsertPass` struct.

---

### 24. Two-pass use counting: Phase 2 re-counts uses already counted in Phase 1

**File:** `src/lir/rc_insert.zig`

`countUsesInto` (Phase 1) counts the entire tree. Then `processIfThenElse`/`processMatch`/`processDiscriminantSwitch` (Phase 2) call `countUsesLocal` again per branch. Every expression in every branch is walked twice.

**Fix (future):** Cache per-branch use counts during Phase 1, keyed by expr ID.

---

### 25. TailRecursion uses `@intFromEnum(a) != @intFromEnum(b)` instead of `a != b`

**File:** `src/lir/TailRecursion.zig` — multiple locations

CFStmtId is an enum and supports direct `!=` comparison. The `@intFromEnum` pattern is unnecessarily verbose.

---

### 26. Wildcard placeholder in TailRecursion has incomplete layout handling

**File:** `src/lir/TailRecursion.zig:323-326`

The i128/u128/dec check is incomplete (misses f32, f64, str, list). Since the placeholder is dead code (never read at runtime), simplify to always use `i64_literal(0)` with a comment.

---

### 27. `@sizeOf(Symbol)` should be asserted equal to `@sizeOf(u64)`

**File:** `src/lir/rc_insert.zig`

Symbols are bitcast to `u64` for HashMap keys throughout. If `Symbol`'s layout changes, this silently breaks.

**Fix:** Add `comptime { std.debug.assert(@sizeOf(Symbol) == @sizeOf(u64)); }` to `init`.

---

### 28. TailRecursion `@import("base").Region.zero()` repeated inline

**File:** `src/lir/TailRecursion.zig:315,324,325`

Should import `Region` at the top of the file.

---

### 29. `lowerForLoop` and `lowerWhileLoop` don't assert result monotype is unit

**File:** `src/lir/MirToLir.zig:1215-1242`

Loops return unit. No assertion validates this.

**Fix:** Add `std.debug.assert(monotype == .unit)`.

---

### 30. `LirExpr` tagged union is ~56 bytes due to `closure` variant

**File:** `src/lir/LIR.zig:304`

Every expression in the store pays the cost of the largest variant. Same tradeoff as MonoIR.

**Fix (future):** Factor `closure` data into a separate side table, replacing inline data with an index.

---

## Test Coverage Gaps

### Most Impactful Gaps (prioritized)

1. **No end-to-end LIR pipeline test** — The full MIR->MirToLir->LIR RC Insert->LirCodeGen->execute chain is never tested as a unit. All fx tests and eval tests use the old mono path.

2. **No test for early_return inside a match branch** — Exercises the interaction between match per-branch RC ops and early_return cleanup.

3. **No test for nested lambdas with captures** — A lambda inside a lambda where the inner captures from the outer scope, exercising `countUsesInto` capture detection.

4. **No tests for `while_loop` with RC symbols** — Loop bodies with refcounted values across iterations.

5. **No tests for `crash`/`runtime_error` in if-then-else branches** — Would test `.noreturn` handling in codegen.

6. **No test for multiple `early_return` expressions in the same block** — Each should independently compute cleanup decrefs.

7. **No test for closure-based self-recursive calls through TailRecursion** — Tests the `isTailCallToTarget` path that checks `closure.self_recursive`.

8. **No aarch64-specific codegen tests** — All tests pick host architecture; no explicit cross-architecture coverage.

9. **No tests for `lowerLowLevel`** — The low-level op mapping (`mapLowLevel`, `lowLevelToBinop`), special cases (`num_is_negative`/`num_is_positive`/`num_is_zero`/`num_negate`), and `emitZeroLiteral`.

10. **No tests for match guards in MirToLir** — All match tests use `MIR.ExprId.none` for guards.

11. **No tests for `lowerForLoop`, `lowerWhileLoop`, `lowerReturn`, `lowerDbg`, `lowerExpect`** — None of the imperative control flow lowering is tested.

12. **No tests for multi-payload single-tag union** — `lowerTag` code path for single-tag unions with multiple payloads (emits tuple) is untested.

13. **No i128/u128 literal edge case tests** — The u128 path, "value fits in i64 but target is u128" path, and negative i128 values.

14. **LirCodeGen tests only verify "code was generated" (non-empty buffer)** — No execution tests that verify generated code produces correct results.

15. **No tests for `match_stmt` in TailRecursion** — `switch_stmt` is tested but `match_stmt` is not.

16. **No tests for float NaN comparison** — Would reveal the NaN equality bug (#9).

---

## Positive Changes Worth Noting

The branch includes several genuine correctness fixes to existing code:

- **`getI128Parts` signedness fix** — negative i64 values were zero-extended instead of sign-extended
- **`.rem` vs `.mod` rename** — corrects the semantic mismatch (truncated remainder, not mathematical modulus)
- **`putCaptureStruct`/`putCaptureUnion` alignment** — was summing sizes without alignment padding
- **`Span.empty()` initialization** — changed `start: undefined` to `start: 0`
- **`num_from_str` signedness detection** — replaced fragile arithmetic with explicit layout matching
- **Record field reordering** in MirToLir — ensures codegen writes fields to correct offsets
- **Deterministic RC op ordering** via sorted keys
- **LIR `addExpr`/`addPattern` atomicity** — use `ensureUnusedCapacity` + `appendAssumeCapacity` to prevent inconsistent state on OOM
- **LIR `LirStmt` as `union(enum)`** — enables RC insertion to distinguish mutations from declarations
- **Debug assertions** on `.none` IDs, duplicate symbol defs, span length mismatches
- **`dispatchUnionClosure` offset fix** — now reads discriminant from correct layout offset and accesses captures at payload offset 0, instead of hardcoding +8
- **`countUsesInto` match pattern scoping fix** — pattern-bound symbols in match branches now register into the branch-local scope instead of polluting the outer scope
