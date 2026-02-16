# Problems found in `lower-mir` branch review

## CRITICAL

### ~~[C1] `MirToLir.lowerBlock` accesses wrong union field~~ FIXED

### ~~[C2] Scratch buffer re-entrancy corruption in MirToLir~~ FIXED

### ~~[C3] Missing `ensureMethodLowered` in `lowerDotAccess` and `lowerTypeVarDispatch`~~ FIXED

---

## HIGH

### ~~[H1] `tagDiscriminant` silently returns 0 on lookup failure~~ FIXED

### ~~[H2] `num_is_eq` used for all non-nominal equality~~ DOCUMENTED + BUGS FIXED

Original concern: strings/booleans would use numeric equality. Analysis showed all primitive
types (Bool, Str, numeric) ARE nominal and resolve via method dispatch — only structural
types (records, tuples, tag unions) and flex vars reach the fallback. The `num_is_eq` fallback
correctly maps to LIR `.eq` which the backend handles structurally.

Fixes applied:
- Documented the two cases that reach the fallback in `src/mir/Lower.zig`
- Implemented `generateTupleComparisonByLayout` in both MonoExprCodeGen and LirCodeGen
  (was an unimplemented stub that crashed)
- Implemented `generateTagUnionComparisonByLayout` in both codegen files
  (tag unions with payloads fell through to scalar comparison, giving wrong results)
- Added 30+ eval tests covering tag union, tuple, record, and nested equality/inequality

### ~~[H3] `lowerUnaryMinus` has no type-directed dispatch~~ FIXED

Rewrote `lowerUnaryMinus` to use `resolveMethodForTypeVar` like `lowerBinop` does, with
fallback to `run_low_level` with `num_negate` for flex/rigid type vars. Added debug
assertion that panics on non-numeric types reaching the fallback. Also fixed
`MirTestEnv.initFull` to include the Builtin module in `all_module_envs` (was only
including the test module, which caused cross-module method lookups to fail).

### ~~[H4] `generateListContains` hardcodes 8-byte element size~~ FIXED

Rewrote `generateListContains` in both LirCodeGen and MonoExprCodeGen to:
- Accept an `elem_layout_idx` parameter and compute element size from layout
- Use `compareFieldByLayout` for layout-aware equality (handles strings, 128-bit,
  records, tuples, tag unions, and scalar types)
- Track byte offset instead of counter-times-constant
- Save all loop state to stack (safe against register clobbering from builtin calls)
- Handle ZST lists separately (`generateZstListContains`)
- Allocate 8-byte-aligned stack slots for element copy buffer

Note: `List.contains` in the builtins is implemented as `List.any(list, |x| x == needle)`,
so the `.list_contains` low-level is not currently exercised by end-to-end tests. The
codegen fix is correct and will be exercised when the full pipeline uses it.

### ~~[H5] `getI128Parts` zero-extends signed values~~ FIXED

Added `std.builtin.Signedness` parameter to `getI128Parts` in both LirCodeGen and
MonoExprCodeGen. For `.general_reg`, `.stack`, `.stack_str` locations: signed values use
ASR/SAR by 63 to sign-extend; unsigned values zero-extend. Extracted shared logic into
`emitSignExtendHighReg` helper. Updated all 20 callers with correct signedness. Added Dec
arithmetic eval tests as correctness baseline. The bug was latent (all current 128-bit
code paths use `.stack_i128`/`.immediate_i128`) but the fix prevents future miscompilation.

### [H6] `ensureInFloatReg` uses hardcoded stack offset -16
**File:** `src/backend/dev/LirCodeGen.zig` (line 11606)

Writes to `[RBP-16]` which can overlap with local variable slots allocated from the frame
pointer downward.

**Fix:** Use `self.codegen.allocStackSlot(8)` instead of a hardcoded offset.

### [H7] `TailRecursion` skips non-bind parameters
**File:** `src/lir/TailRecursion.zig` (line 272)

When building initial jump args, non-bind patterns (wildcards, destructuring) are silently
skipped, creating an arity mismatch between the jump point's expected args and provided
args. Any tail-recursive function with a wildcard parameter (e.g., `factorial = |_, acc| ...`)
will be miscompiled.

**Fix:** Include all parameters. For wildcards, use a zero-sized placeholder. For other
non-bind patterns, use `unreachable` (they shouldn't appear as function params after
desugaring).

### [H8] `rc_insert.zig` uses global use counts instead of scope-aware counts
**File:** `src/lir/rc_insert.zig` (lines 66-72, 330-412)

Use counts are global across the entire expression tree, not scoped to control flow. A
symbol used once in each branch of an `if` gets count=2, causing an unnecessary `incref`
that leaks memory (only one branch executes at runtime).

**Fix:** This is a fundamental design issue requiring per-scope tracking (Perceus-style).
At minimum, document as a known limitation that the current implementation is conservative
(leak-safe but not optimal).

### [H9] Recursion placeholder uses wrong monotype
**File:** `src/mir/Lower.zig` (line 1235)

The placeholder lookup for recursive definitions uses `self.store.monotype_store.unit_idx`
as its type. In release mode, any downstream code calling `store.typeOf(placeholder)` gets
`unit` instead of the real type (e.g., a function type for a recursive function). The
existing test at `lower_test.zig:664` asserts this wrong behavior as expected.

**Fix:** After the recursive `lowerExpr` completes and the real definition is cached, patch
the placeholder's monotype in the `type_map` to match the resolved definition's monotype.

### [H10] Known correctness regression masked on aarch64-windows
**Reference:** `eval_test.zig` (line 2802)

The issue 8927 regression test is skipped on Windows ARM64, so the backend mismatch is no
longer caught there. This leaves a real platform bug unguarded.

---

## MEDIUM-HIGH

### [MH1] `lowerExpect` uses same expression for both `cond` and `body`
**File:** `src/lir/MirToLir.zig` (line 627)

The MIR `expect` only has a `body` field. The lowering sets both `cond` and `body` to the
same lowered expression. For non-boolean expect bodies, the backend would try to branch on
a non-boolean value.

**Fix:** Determine correct semantics — if MIR's `expect.body` is the condition, then `body`
should be unit/empty_record. If MIR doesn't provide enough info, the `expect` node design
needs revisiting.

### [MH2] `findModuleForOrigin` uses bare `unreachable`
**File:** `src/mir/Lower.zig` (line 1139)

No fallback if the origin module isn't found. Compare with the LIR version (`src/lir/Lower.zig`
lines 218-257) which has a 3-step fallback including string comparison.

**Fix:** At minimum use `std.debug.panic` with a diagnostic message. Consider adding the
string-comparison fallback from the LIR version.

### [MH3] 128-bit wrap conversion: unused register allocation + premature free
**File:** `src/backend/dev/LirCodeGen.zig` (lines 2609-2624)

When `dst_bits == 128`, `parts.high` is freed before the branch, then inside the branch a
`high_reg` is allocated but never used. The fix is to move the `freeGeneral(parts.high)` into
the `else` branch so it's only freed when not needed for the 128-bit case.

---

## MEDIUM

### [M1] Heap buffer overflow for large string literals
**File:** `src/backend/dev/LirCodeGen.zig` (line 8388)

Tail bytes (1-7 remaining after 8-byte chunks) are written as a full 8-byte store past the
heap allocation boundary. Comment says "heap has space" but allocation is for exactly
`str_bytes.len` bytes.

**Fix:** Either allocate `alignForward(str_bytes.len, 8)` bytes, or use byte-sized stores
for the remainder.

### [M2] `copyStackToStack` overwrites adjacent data on aarch64
**File:** `src/backend/dev/LirCodeGen.zig` (line 3914)

Remaining 1-3 bytes after 8-byte and 4-byte chunks are copied as a 4-byte store, writing
1-3 extra bytes into the next stack slot. The x86_64 path correctly handles this byte-by-byte.

**Fix:** Use byte-by-byte copy on aarch64 for the remaining 1-3 bytes.

### [M3] `zeroStackArea` overwrites adjacent data for non-8-aligned sizes
**File:** `src/backend/dev/LirCodeGen.zig` (line 8290)

Remaining 1-7 bytes are zeroed with a full 8-byte store.

**Fix:** Use appropriately-sized stores for the remaining bytes.

### [M4] `copyBytesToStackOffset` overwrites adjacent data
**File:** `src/backend/dev/LirCodeGen.zig` (line 8278)

Same pattern — 8-byte chunks with no handling of the tail bytes.

**Fix:** Handle tail bytes with appropriately-sized loads/stores.

### [M5] `copyStackToPtr` drops 1-3 tail bytes on aarch64
**File:** `src/backend/dev/LirCodeGen.zig` (line 11938)

After the 4-byte store for the remainder, if 1-3 bytes still remain, they are not copied.

**Fix:** Handle the full remainder chain: 4 bytes, then 2 bytes, then 1 byte.

### [M6] `generateStructuralComparison` fixed-size array of 32, no bounds check
**File:** `src/backend/dev/LirCodeGen.zig` (line 5666)

Records with many fields or large fields expanding to many 8-byte slots can exceed 32
comparison points and write past the array bounds (UB).

**Fix:** Use an `ArrayList`, or add a bounds check with `unreachable`.

### [M7] `generateNumFromStr` signedness from enum parity
**File:** `src/backend/dev/LirCodeGen.zig` (line 5106)

`is_signed = (idx_int % 2 == 0)` depends on layout enum ordering. Breaks silently if enum
values are reordered.

**Fix:** Use an explicit switch for signedness, mirroring the width switch.

### [M8] Parallel array OOM desync in `addExpr`/`addPattern`
**Files:** `src/mir/MIR.zig` (line 530), `src/lir/LirExprStore.zig` (line 153)

Sequential appends to parallel arrays — if the first succeeds but the second OOMs, the
arrays are permanently out of sync. Any subsequent access panics.

**Fix:** Use `ensureUnusedCapacity` for all arrays first, then `appendAssumeCapacity`.

### [M9] `empty()` spans use `undefined` for `start` field in `extern struct`
**Files:** `src/mir/MIR.zig` (line 100), `src/lir/LIR.zig` (line 71), `src/base/DataSpan.zig` (line 10), `src/mono/MonoIR.zig` (multiple)

All span `empty()` functions set `start = undefined`. Since these are `extern struct` (for
serialization), the undefined bytes can cause issues in debuggers, bitwise comparisons, or
actual serialization. All accessors guard on `len == 0` so it's safe at runtime.

**Fix:** Use `start = 0` instead of `undefined`.

### [M10] Tag union layout always uses u64 discriminant
**File:** `src/lir/MirToLir.zig` (line 224)

Wastes 7 bytes per tag union value for the common case of <256 variants. If the LIR layout
disagrees with the backend's layout, field offsets will be inconsistent.

**Fix:** Choose discriminant size based on tag count, or document that the backend ignores
this layout.

### [M11] Tag union max payload selection ignores alignment
**File:** `src/lir/MirToLir.zig` (line 214)

Only compares `size`, but a smaller payload with higher alignment could require more total
space due to padding.

**Fix:** Compare by effective allocated size: `alignForward(size, alignment)`.

### [M12] Single-tag-with-payload union gets unnecessary discriminant
**File:** `src/lir/MirToLir.zig` (line 186)

A single-tag union with payload (e.g., `[Ok payload]`) still gets a `[u64, payload]` tuple
layout. No discriminant is needed.

**Fix:** Return just the payload layout for single-tag unions.

### [M13] `lowerLambda` always uses `struct_captures` closure representation
**File:** `src/lir/MirToLir.zig` (line 451)

Should use `unwrapped_capture` for 1 capture, `enum_dispatch` for no-capture lambda sets,
etc. Using `struct_captures` for everything means lambda set dispatch won't work correctly.

**Fix:** Use lambda set analysis results to choose the correct representation.

### [M14] `lowerLambda` uses `fn_layout` as `closure_layout`
**File:** `src/lir/MirToLir.zig` (line 451)

A function's layout describes `(args) -> ret`. A closure's layout should describe the
capture struct. Using the wrong layout causes incorrect memory allocation for closures.

**Fix:** Compute the actual capture struct layout from the capture fields.

### [M15] `layout_store.all_module_envs[0]` hardcoded
**File:** `src/lir/MirToLir.zig` (line 149)

Always uses module 0's environment for `putRecord`. Multi-module programs will use the
wrong module environment for record layout computation.

**Fix:** Use `self.layout_store.currentModuleEnv()` or track the current module index.

---

## TEST COVERAGE GAPS

### Zero behavioral tests
- `src/lir/MirToLir.zig` (1,096 lines) — 0 tests
- `src/lir/rc_insert.zig` (544 lines) — 1 comptime check only
- `src/lir/TailRecursion.zig` (324 lines) — 1 init check only

### Minimal tests
- `src/backend/dev/LirCodeGen.zig` (15,930 lines) — 4 tests that only check `code.len > 0`

### Missing test paths in `Lower.zig`
- Closures with captures
- Dot-access as method call
- Type var dispatch
- Unary minus / unary not
- For-loop expressions
- Record/list/tuple destructure patterns
- Multi-segment string interpolation
- `!=` operator (is_eq + negBool)
- Match with guards
- Typed fractional literals (f32/f64)
- `e_lookup_required` (platform requires)
