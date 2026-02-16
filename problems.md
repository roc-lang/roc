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

### ~~[H6] `ensureInFloatReg` uses hardcoded stack offset -16~~ FIXED

Replaced hardcoded `[RBP-16]` with `self.codegen.allocStackSlot(8)` in both LirCodeGen
and MonoExprCodeGen. The x86_64 path for loading float immediates now uses a properly
allocated stack slot instead of a fixed offset that could overlap with local variables.

### ~~[H7] `TailRecursion` skips non-bind parameters~~ FIXED

Changed the initial jump arg builder to use a switch on pattern type: `.bind` creates a
lookup expression (as before), `.wildcard` creates an `i64_literal(0)` placeholder to
maintain arity (rebindJoinPointParams skips non-bind patterns so the value is never used),
and all other patterns hit `unreachable` (shouldn't appear as function params after
desugaring).

### ~~[H8] `rc_insert.zig` uses global use counts instead of scope-aware counts~~ DOCUMENTED

Added "Known limitation: global use counts" documentation to the module doc comment in
`rc_insert.zig`. The current implementation is conservative (leak-safe, never frees too
early) but can cause memory to be retained longer than necessary. A full fix requires
per-scope tracking (Perceus-style).

### ~~[H9] Recursion placeholder uses wrong monotype~~ FIXED

Changed `debug_recursion_placeholders` to `recursion_placeholders` (always tracked, not
Debug-only). After `lowerExpr` completes and the real definition is cached, all
placeholders for that symbol have their monotype patched in `type_map` to match the
resolved definition's monotype. The Debug-mode assertion in `deinit` now verifies the
patching is correct.

### ~~[H10] Known correctness regression masked on aarch64-windows~~ NOT APPLICABLE

The issue 8927 regression test at `eval_test.zig:3132` does not have any platform-specific
skip conditions. The skip referenced in the original report no longer exists.

---

## MEDIUM-HIGH

### ~~[MH1] `lowerExpect` uses same expression for both `cond` and `body`~~ FIXED

The MIR `expect.body` is the boolean condition. Now `cond` is set to the lowered body
expression and `body` is set to a new `empty_record` expression (unit), since the result
of `expect` after the assertion is `{}`.

### ~~[MH2] `findModuleForOrigin` uses bare `unreachable`~~ FIXED

Replaced `unreachable` with `std.debug.panic` that includes the current_module_idx and
origin_ident for diagnostics.

### ~~[MH3] 128-bit wrap conversion: unused register allocation + premature free~~ FIXED

Moved `freeGeneral(parts.high)` after the 128-bit branch. The 128-bit path now uses
`parts.high` directly instead of freeing it early and re-loading via a redundant
`getI128Parts` call. Also removed the unused `high_reg` allocation. Fixed in both
LirCodeGen and MonoExprCodeGen.

---

## MEDIUM

### ~~[M1] Heap buffer overflow for large string literals~~ FIXED

Changed heap allocation size from `str_bytes.len` to `alignForward(str_bytes.len, 8)` in
both LirCodeGen and MonoExprCodeGen, since the tail write stores a full 8-byte word.

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

### ~~[M7] `generateNumFromStr` signedness from enum parity~~ FIXED

Replaced `idx_int % 2 == 0` parity check with explicit switch on `payload_idx` for
signedness determination in both LirCodeGen and MonoExprCodeGen.

### ~~[M8] Parallel array OOM desync in `addExpr`/`addPattern`~~ FIXED

Changed `addExpr` and `addPattern` in both MIR.zig and LirExprStore.zig to use
`ensureUnusedCapacity` for all parallel arrays first, then `appendAssumeCapacity`.

### ~~[M9] `empty()` spans use `undefined` for `start` field in `extern struct`~~ FIXED

Changed `start = undefined` to `start = 0` in all `empty()` functions across MIR.zig,
LIR.zig, DataSpan.zig, MonoIR.zig, and Monotype.zig (31 occurrences total).

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
