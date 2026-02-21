# Code Review: `lower-mir` Branch

## HIGH — Likely Correctness Bugs

### 2. LirCodeGen.zig: `generateDiscriminantSwitch` always loads discriminant as 8 bytes

**File:** `src/backend/dev/LirCodeGen.zig`, line ~9864

**Problem:** The discriminant load in `generateDiscriminantSwitch` uses `.w64` (8-byte load) regardless of the actual discriminant size. For a small tag union where the total struct size is, say, 5 bytes (4-byte payload + 1-byte discriminant), this reads 3 bytes past the end of the struct, potentially causing a segfault on page boundaries.

Compare with `generateMatch` which calls `loadAndMaskDiscriminant` that properly handles 1/2/4/8 byte discriminants, and `dispatchUnionClosure` which uses `disc_use_w32`.

**Fix:** Determine the discriminant size from the layout (derivable from the number of tags — 1 byte for <=256, 2 bytes for <=65536, etc.) and use the appropriate load width. Zero-extend to register width for the comparison.

### 3. LirCodeGen.zig: List decref/free hardcodes alignment=8

**File:** `src/backend/dev/LirCodeGen.zig`, lines ~15429-15431 and ~15457-15459

**Problem:** When freeing list memory during decref, the alignment is hardcoded to 8 via `try builder.addImmArg(8)`. But `List U8` should have alignment 1, `List U16` alignment 2, etc. Passing wrong alignment to the allocator can cause heap corruption on allocators that use alignment for bucket selection, and causes `decrefDataPtrC` to look for the refcount at the wrong offset relative to the data pointer.

The allocation path (e.g., `list_append`) correctly passes `elem_size_align.alignment.toByteUnits()`, so allocation uses the correct alignment but deallocation does not.

**Fix:** Store the element alignment in the `rc_op` LIR node, or look it up from the layout store during codegen. Pass the correct alignment to deallocation calls.

### 4. LirCodeGen.zig: x86_64 self-call patching scans raw bytes for 0xE8

**File:** `src/backend/dev/LirCodeGen.zig`, lines ~12724-12748

**Problem:** The self-recursive call patching mechanism scans generated machine code looking for `0xE8` bytes (the x86_64 CALL opcode) to find call sites that need their relative offset patched. But `0xE8` can appear as an immediate operand, a ModR/M byte, or part of other instructions. Patching a non-CALL byte would silently corrupt the generated code.

**Fix:** Record the offset of each self-call site when it's emitted (e.g., push the offset onto a fixup list during emission). Then iterate the fixup list to patch. This is the standard approach used by assemblers and JIT compilers.

### 5. MirToLir.zig: `int_literal` pattern uses `toI128()` for all integer kinds

**File:** `src/lir/MirToLir.zig`, in `lowerPattern`

**Problem:** In `lowerPattern`, the `int_literal` case calls `toI128()` on the value regardless of whether the type is signed or unsigned. For large unsigned values (e.g., a `u128` value > `i128.max`), `toI128()` will produce a negative number, which will then fail to match correctly.

**Fix:** Check the integer kind (signed vs unsigned) and use the appropriate conversion. For unsigned types, use the raw bit pattern and compare as unsigned.

---

## MEDIUM — Potential Correctness Issues

### 6. TailRecursion.zig: Wildcard placeholder uses `i64_literal` unconditionally

**File:** `src/lir/TailRecursion.zig`, lines ~319-324

**Problem:** When creating wildcard placeholder values for tail-recursive parameter updates, the code always emits an `i64_literal(0)`. For `i128`, `u128`, or `Dec` layouts (which are 16 bytes), this produces an 8-byte literal that doesn't match the expected layout size, which could cause codegen to emit wrong-sized stores.

The mono version correctly checks the parameter layout and uses `i128_literal` for these types.

**Fix:** Check the parameter layout size. For 16-byte types, emit `i128_literal(0, 0)` instead.

### 7. MirToLir.zig: `lowerDbg` drops the debug message

**File:** `src/lir/MirToLir.zig`

**Problem:** The `lowerDbg` function lowers the expression being debugged but discards the debug message string (the `dbg.message` field). The resulting LIR `dbg` statement has no message, so at runtime the user sees no label for what's being printed.

**Fix:** Thread the message through to the LIR `dbg` statement. This likely requires adding a message field to the LIR dbg representation if one doesn't exist.

### 8. MirToLir.zig: Two exhaustive switches must stay in sync

**File:** `src/lir/MirToLir.zig`

**Problem:** `mapLowLevel` and `lowLevelToBinop` both switch over `MIR.Expr.LowLevel` variants. If a new low-level op is added to MIR, both switches must be updated. Since they're in different functions with no compile-time coupling, it's easy to update one and forget the other.

**Fix:** Consider combining these into a single function that returns a struct `{ lir_op: LirLowLevel, binop: ?Binop }`, or add a compile-time assertion that both switches handle the same set of variants.

### 9. LirCodeGen.zig: `copyToStackSlot` only copies 8 bytes

**File:** `src/backend/dev/LirCodeGen.zig`, lines ~11606-11632

**Problem:** `copyToStackSlot` copies exactly 8 bytes from a register to a stack slot. For closure dispatch results or other multi-word values (e.g., 16-byte `i128`, 24-byte strings), this truncates the value silently.

**Fix:** Accept a size parameter (from the layout) and copy the appropriate number of bytes. Handle `stack_str`, `stack_i128`, `list_stack`, `immediate_i128` cases explicitly (similar to `storeResultToSlot`).

### 10. LirCodeGen.zig: `bindProcParams` doesn't handle multi-register struct parameters

**File:** `src/backend/dev/LirCodeGen.zig`, lines ~14236-14365

**Problem:** When binding procedure parameters, the code assumes each parameter fits in a single register. Struct parameters that are passed in two registers (per the calling convention) won't be correctly bound, leading to garbled values. In contrast, `bindLambdaParams` correctly handles these using `calcParamRegCount` and multi-register loading.

**Fix:** Align `bindProcParams` with `bindLambdaParams` by using `calcParamRegCount` and handling multi-register parameters.

### 11. LirCodeGen.zig: Dec-to-signed-integer truncation uses wrong shift

**File:** `src/backend/dev/LirCodeGen.zig`, lines ~2650-2663

**Problem:** For `dec_to_i8_trunc` through `dec_to_i32_trunc`, the code uses logical shift right (`emitLsrImm`) to truncate, which zero-extends rather than sign-extends. If the truncated value should be negative (e.g., Dec -1.0 truncated to i8 = -1), the upper bits would be zeroed instead of sign-extended, making sign tests fail.

**Fix:** Use `emitAsrImm` (arithmetic shift right) for signed truncation targets (`dec_to_i8_trunc`, `dec_to_i16_trunc`, `dec_to_i32_trunc`).

### 12. LirCodeGen.zig: `ensureOnStack` for `immediate_i64` always allocates 8 bytes

**File:** `src/backend/dev/LirCodeGen.zig`, line ~12041

**Problem:** When `ensureOnStack` is called with an `immediate_i64` location, it always allocates 8 bytes regardless of the `size` parameter. If `size > 8` (e.g., for a Dec/i128 that's represented as `immediate_i64`), the allocated stack slot will be too small.

**Fix:** Use `@max(8, size)` for the allocation: `const slot = self.codegen.allocStackSlot(@max(8, @intCast(size)));`

---

## LOW — Minor Issues & Brittleness

### 13. MirToLir.zig: O(N^2) field reordering in record lowering

**File:** `src/lir/MirToLir.zig`

**Problem:** `lowerRecord` does a linear scan through all field names for each field to find its sorted position. For records with many fields, this is O(N^2).

**Fix:** Pre-sort a mapping array once, then use it for all fields.

### 14. MirToLir.zig: Shared guard/body expr IDs across OR-pattern branches

**File:** `src/lir/MirToLir.zig`

**Problem:** When lowering match expressions with OR-patterns (`A | B -> body`), the same guard and body expression IDs are reused across the expanded branches. If any lowering pass assumes expression IDs are unique per branch, this could cause issues.

**Fix:** Document this as intentional if it is, or clone the body/guard expressions for each OR-pattern branch.

### 15. MirToLir.zig: `emitZeroLiteral` handles bool/str cases

**File:** `src/lir/MirToLir.zig`

**Problem:** `emitZeroLiteral` has cases for `bool` and `str` layouts, but these should never need a zero literal in normal RC paths. Having these cases suppresses the `unreachable` that would catch unexpected calls.

**Fix:** Replace with `unreachable` (or debug assert) to catch unexpected usage.

### 16. MirToLir.zig: Empty tag union returns `.zst`

**File:** `src/lir/MirToLir.zig`

**Problem:** `lowerTag` returns `.zst` for empty tag unions. A tag union with zero tags can never be constructed.

**Fix:** Replace with `unreachable`.

### 17. TailRecursion.zig: Closure self-recursive matching is likely dead code

**File:** `src/lir/TailRecursion.zig`

**Problem:** `isTailCallToTarget` checks `closure.self_recursive`, but the self-recursive flag on closures is likely not set before the tail recursion pass runs in the pipeline. This means the closure path is dead code.

**Fix:** Verify whether self-recursive closures can reach TailRecursion.zig. If not, replace with `unreachable` so any future pipeline change that makes it reachable will be caught.

### 18. TailRecursion.zig: Unbounded recursion on deep let-chains

**File:** `src/lir/TailRecursion.zig`

**Problem:** `findTailCallInLastPosition` recurses through let-chains. Extremely deep let-chains (thousands of nested lets) could overflow the stack.

**Fix:** Convert to an iterative loop. The let-chain walk is tail-recursive in structure, so it converts trivially to a `while` loop.

### 19. layout/store.zig: `putCaptureStruct` with empty captures creates invalid range

**File:** `src/layout/store.zig`, line ~565

**Problem:** If `putCaptureStruct` is called with an empty captures list, it creates a `NonEmptyRange` with `count=0`, which violates the type's invariant. Currently unreachable from existing callers, but the function's API doesn't document this requirement.

**Fix:** Add `std.debug.assert(capture_layout_idxs.len > 0)` at the top.

### 20. layout/store.zig: `putTagUnion` discriminant_offset u16 overflow

**File:** `src/layout/store.zig`, line ~515

**Problem:** `discriminant_offset` is stored as `u16`, so payloads larger than 64 KiB would overflow. While uncommon, a very large record-in-a-tag could hit this.

**Fix:** Either widen to `u32` or add a debug assertion that catches the overflow.

### 21. layout/store.zig: `putCaptureUnion` hardcodes 8-byte tag

**File:** `src/layout/store.zig`, line ~591

**Problem:** The capture union layout assumes the discriminant is always 8 bytes. If any capture has alignment > 8, the payload's start at offset 8 would violate that alignment.

**Fix:** Derive the discriminant size from the number of variants, same as regular tag unions, or add a debug assertion that `max_payload_alignment <= 8`.

### 22. LirCodeGen.zig: aarch64 `storeI128ToMem` uses scaled offset

**File:** `src/backend/dev/LirCodeGen.zig`, line ~12582

**Problem:** `strRegMemUoff(.w64, sign_reg, ptr_reg, 1)` uses a scaled unsigned offset where `1` means `1 * 8 = 8 bytes`. This is technically correct but extremely brittle — one refactor to a signed-offset store variant would change `1` to a 1-byte offset.

**Fix:** Add a comment explaining the scaled nature, or use the `emitStoreToPtr(.w64, ...)` helper which takes an explicit byte offset of `8`.

---

## Test Coverage Gaps

### Most Impactful Gaps (prioritized)

1. **No end-to-end LIR pipeline test** — The full MIR->MirToLir->LIR RC Insert->LirCodeGen->execute chain is never tested as a unit. All fx tests and eval tests use the old mono path. This is the single most valuable test to add — it would catch issues #2, #3, #5, #9, #10.

2. **No test for early_return inside a match branch** — Would directly expose issue #1 (the double-free bug).

3. **No test for nested lambdas with captures** — A lambda inside a lambda where the inner captures from the outer scope, exercising `countUsesInto` capture detection.

4. **No tests for `while_loop` with RC symbols** — Loop bodies with refcounted values across iterations.

5. **No tests for `crash`/`runtime_error` in if-then-else branches** — Would test `.noreturn` handling in codegen.

6. **No test for multiple `early_return` expressions in the same block** — Each should independently compute cleanup decrefs.

7. **No test for closure-based self-recursive calls through TailRecursion** — Tests the `isTailCallToTarget` path that checks `closure.self_recursive`.

8. **No aarch64-specific codegen tests** — All tests pick host architecture; no explicit cross-architecture coverage.

9. **No tests for `lowerLowLevel`** — The low-level op mapping (`mapLowLevel`, `lowLevelToBinop`), special cases (`num_is_negative`/`num_is_positive`/`num_is_zero`/`num_negate`), and `emitZeroLiteral`. Would catch issue #8 (sync between the two switches).

10. **No tests for match guards in MirToLir** — All match tests use `MIR.ExprId.none` for guards.

11. **No tests for `lowerForLoop`, `lowerWhileLoop`, `lowerReturn`, `lowerDbg`, `lowerExpect`** — None of the imperative control flow lowering is tested.

12. **No tests for multi-payload single-tag union** — `lowerTag` code path for single-tag unions with multiple payloads (emits tuple) is untested.

13. **No i128/u128 literal edge case tests** — The u128 path, "value fits in i64 but target is u128" path, and negative i128 values. Would catch issue #5 (unsigned int_literal) and issue #6 (wildcard placeholder sizing).

14. **LirCodeGen tests only verify "code was generated" (non-empty buffer)** — No execution tests that verify generated code produces correct results.

15. **No tests for `match_stmt` in TailRecursion** — `switch_stmt` is tested but `match_stmt` is not.

16. **No test for float NaN comparison** — NaN equality bug was fixed but no regression test exists yet.

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
