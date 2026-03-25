# Wasm Relocatable Object + Builtin Linking

## Goal

Make the wasm backend link against the same `roc_builtins.o` that the dev backend uses,
via `wasm-ld`. Currently the wasm backend reimplements builtins as ad-hoc host functions
in `helpers.zig`, causing behavioral divergence between backends.

## Architecture (Before → After)

**Before:**
```
WasmCodeGen → final .wasm (with host imports for builtins)
                ↓
bytebox instantiates with 43 host function implementations in helpers.zig
```

**After:**
```
WasmCodeGen → relocatable .o (with symbol references for builtins)
                ↓
wasm-ld links app.o + roc_builtins.o → final .wasm (builtins resolved)
                ↓
bytebox instantiates with only 6 RocOps host functions
```

## Prerequisites (Already Done)

- [x] `__multi3` / `__muloti4` provided in `compiler_rt_128.zig` for wasm32
  (commit 8c6a806c75) — builtins .o is fully self-contained, zero external deps
- [x] `WasmBuiltinsMerger.zig` created (commit 3b144ea725) — DELETE this file,
  we're using wasm-ld instead of runtime merging

## Key Insight: Wasm Relocatable Object Format

A wasm `.o` file (relocatable object) differs from a final `.wasm` in these ways:

1. **5-byte padded LEB128** for all relocatable indices (`call`, `global.get/set`)
2. **`linking` custom section** — symbol table (version 2) with defined/undefined symbols
3. **`reloc.CODE` custom section** — list of relocations in the code section
4. **Import section** lists undefined symbols (builtins the app calls)

Example from a real .o (hex):
```
call external_func  →  10 80 80 80 80 00   (5-byte padded LEB128, value 0)
global.get __sp     →  23 80 80 80 80 00   (5-byte padded LEB128, value 0)
```

The linker patches these 5-byte slots with the resolved indices.

---

## Implementation Steps

### Step 1: Clean up — delete WasmBuiltinsMerger.zig

Delete `src/backend/wasm/WasmBuiltinsMerger.zig` — we don't need runtime merging.

### Step 2: WasmModule.zig — add relocatable object output

**File: `src/backend/wasm/WasmModule.zig`**

Add the ability to produce a relocatable `.o` instead of a final `.wasm`.

#### 2a. Add relocation tracking state

```zig
/// A code relocation entry (for reloc.CODE section)
const CodeReloc = struct {
    reloc_type: u8,       // R_WASM_FUNCTION_INDEX_LEB=0, R_WASM_GLOBAL_INDEX_LEB=7
    offset: u32,          // byte offset within the code section
    symbol_index: u32,    // index into the symbol table
};

/// A symbol in the linking section's symbol table
const LinkingSymbol = struct {
    kind: u8,             // 0=FUNC, 1=DATA, 2=GLOBAL
    flags: u32,           // 0x10=undefined, 0x04=hidden, etc.
    name: []const u8,
    /// For defined function symbols: local function index
    /// For undefined function symbols: import index
    index: u32,
};

// New fields on WasmModule:
code_relocs: std.ArrayList(CodeReloc),
linking_symbols: std.ArrayList(LinkingSymbol),
/// Whether to produce a relocatable object (vs final linked module)
relocatable: bool = false,
```

#### 2b. Add `addUndefinedFunction()` method

For builtin references, the codegen calls this instead of `addImport`:

```zig
/// Add an undefined function symbol (resolved by linker).
/// Returns the symbol index for use in relocations.
/// In the wasm object, this becomes an import from "env".
pub fn addUndefinedFunction(self: *Self, name: []const u8, type_idx: u32) !u32 {
    // Add as import (undefined symbols appear in import section)
    const import_idx = try self.addImport("env", name, type_idx);
    // Add to symbol table
    const sym_idx: u32 = @intCast(self.linking_symbols.items.len);
    try self.linking_symbols.append(self.allocator, .{
        .kind = 0, // SYMTAB_FUNCTION
        .flags = 0x10, // WASM_SYM_UNDEFINED
        .name = name,
        .index = import_idx,
    });
    return sym_idx;
}
```

#### 2c. Add `leb128WriteU32Padded5()` helper

Writes a u32 as exactly 5 bytes of LEB128 (needed for relocation slots):

```zig
pub fn leb128WriteU32Padded5(gpa: Allocator, output: *std.ArrayList(u8), value: u32) !void {
    var val = value;
    for (0..4) |_| {
        try output.append(gpa, @as(u8, @truncate(val & 0x7f)) | 0x80);
        val >>= 7;
    }
    try output.append(gpa, @as(u8, @truncate(val & 0x7f)));
}
```

#### 2d. Modify `encode()` to support relocatable output

When `self.relocatable == true`:
- Emit code bodies using 5-byte padded LEB128 for relocatable indices
- After all standard sections, emit:
  - `linking` custom section (version 2 + WASM_SYMBOL_TABLE subsection)
  - `reloc.CODE` custom section (target section index + relocation entries)

The `encodeLinkingSection()` and `encodeRelocCodeSection()` are new private methods.

### Step 3: WasmCodeGen.zig — reference builtins by symbol name

**File: `src/backend/wasm/WasmCodeGen.zig`**

#### 3a. Replace import fields with symbol indices

Replace the 37 `?u32` import fields (everything except RocOps) with a
`BuiltinSymbols` struct that stores symbol indices (not function indices):

```zig
const BuiltinSymbols = struct {
    // Decimal/math
    dec_mul: u32 = undefined,
    dec_div: u32 = undefined,
    dec_div_trunc: u32 = undefined,
    dec_to_str: u32 = undefined,
    dec_from_str: u32 = undefined,
    dec_to_i128: u32 = undefined,
    dec_to_u128: u32 = undefined,
    dec_to_f32: u32 = undefined,
    // String ops
    str_eq: u32 = undefined,
    str_concat: u32 = undefined,
    str_repeat: u32 = undefined,
    str_trim: u32 = undefined,
    str_trim_start: u32 = undefined,
    str_trim_end: u32 = undefined,
    str_split: u32 = undefined,
    str_join_with: u32 = undefined,
    str_reserve: u32 = undefined,
    str_release_excess_capacity: u32 = undefined,
    str_with_capacity: u32 = undefined,
    str_drop_prefix: u32 = undefined,
    str_drop_suffix: u32 = undefined,
    str_with_ascii_lowercased: u32 = undefined,
    str_with_ascii_uppercased: u32 = undefined,
    str_caseless_ascii_equals: u32 = undefined,
    str_from_utf8: u32 = undefined,
    // List ops
    list_eq: u32 = undefined,
    list_str_eq: u32 = undefined,
    list_list_eq: u32 = undefined,
    list_append_unsafe: u32 = undefined,
    list_sort_with: u32 = undefined,
    list_reverse: u32 = undefined,
    // Integer ops
    i32_mod_by: u32 = undefined,
    i64_mod_by: u32 = undefined,
    i128_div_s: u32 = undefined,
    i128_mod_s: u32 = undefined,
    u128_div: u32 = undefined,
    u128_mod: u32 = undefined,
    i128_to_str: u32 = undefined,
    u128_to_str: u32 = undefined,
    i128_to_dec: u32 = undefined,
    u128_to_dec: u32 = undefined,
    // Float/parsing
    float_to_str: u32 = undefined,
    float_from_str: u32 = undefined,
    int_from_str: u32 = undefined,
};

builtin_syms: BuiltinSymbols = .{},
```

#### 3b. Rewrite `registerHostImports()`

Split into two phases:
1. Register RocOps as real imports (these stay as host imports, called via `call_indirect`)
2. Register builtins as undefined symbols via `module.addUndefinedFunction()`

```zig
fn registerHostImports(self: *Self) !void {
    self.module.relocatable = true;

    // Phase 1: RocOps imports (stay as runtime imports, used via call_indirect)
    const roc_ops_type = try self.module.addFuncType(&.{ .i32, .i32 }, &.{});
    // ... same as before for roc_alloc, roc_dealloc, etc.

    // Phase 2: Builtins as undefined symbols (resolved by wasm-ld)
    const i128_binop_type = try self.module.addFuncType(&.{ .i32, .i32, .i32 }, &.{});
    self.builtin_syms.dec_mul = try self.module.addUndefinedFunction("roc_builtins_dec_mul_saturated", i128_binop_type);
    self.builtin_syms.i128_div_s = try self.module.addUndefinedFunction("roc_builtins_num_div_trunc_i128", i128_binop_type);
    // ... etc for all 37 builtins
}
```

**IMPORTANT**: The symbol names must match the exports from `roc_builtins.o`. These are
the `dev_wrappers.zig` function names, NOT the `roc_xxx` names used by host imports.
The ABI is different — see Step 3c.

#### 3c. ABI adaptation at call sites

**This is the most critical and subtle part.**

The current host imports use a "pointer-to-struct" ABI:
```
roc_str_trim(str_ptr: i32, result_ptr: i32) -> void
```
Where `str_ptr` points to a 12-byte `{bytes, len, cap}` struct in memory.

The `dev_wrappers.zig` builtins use a decomposed C ABI:
```
roc_builtins_str_trim(out: *RocStr, bytes: ?[*]u8, len: usize, cap: usize, roc_ops: *RocOps) -> void
```
Which on wasm32 becomes:
```
(i32 out_ptr, i32 bytes, i32 len, i32 cap, i32 roc_ops_ptr) -> void
```

**Every call site must be adapted to decompose structs and pass roc_ops.**

Categories of ABI changes:

| Category | Current ABI | Builtin ABI | Examples |
|----------|-------------|-------------|----------|
| Str unary | `(str_ptr, result_ptr)` | `(result_ptr, bytes, len, cap, roc_ops)` | trim, trim_start, trim_end, lowercased, uppercased, release_excess |
| Str binary | `(str_a_ptr, str_b_ptr, result_ptr)` | `(result_ptr, a_bytes, a_len, a_cap, b_bytes, b_len, b_cap, roc_ops)` | drop_prefix, drop_suffix, split, join_with, concat, repeat, reserve |
| Str eq | `(str_a_ptr, str_b_ptr) -> i32` | `(a_bytes, a_len, a_cap, b_bytes, b_len, b_cap) -> i32` | str_eq, caseless_ascii_equals |
| i128 binop | `(lhs_ptr, rhs_ptr, result_ptr)` | `(result_ptr, lhs_lo, lhs_hi, rhs_lo, rhs_hi)` | i128_div_s, u128_div, dec_mul, dec_div |
| Dec to str | `(dec_ptr, buf_ptr) -> i32` | `(result: *RocStr, lo: u64, hi: u64, roc_ops: *RocOps)` | dec_to_str |
| List ops | `(list_ptr, ...)` | `(result_ptr, bytes, len, cap, ..., roc_ops)` | list_append, list_reverse |

**Recommendation**: Create helper functions in WasmCodeGen for each ABI pattern:

```zig
/// Emit a call to a builtin that takes one decomposed RocStr + roc_ops
fn emitBuiltinStrUnary(self: *Self, sym_idx: u32, str_local: u32) !void { ... }

/// Emit a call to a builtin that takes two decomposed RocStrs + roc_ops
fn emitBuiltinStrBinary(self: *Self, sym_idx: u32, a_local: u32, b_local: u32) !void { ... }

/// Emit a call to a builtin that takes two i128s (decomposed to lo/hi u64 pairs)
fn emitBuiltinI128BinOp(self: *Self, sym_idx: u32, lhs_local: u32, rhs_local: u32) !void { ... }
```

#### 3d. Emit relocatable `call` instructions

When emitting `call builtin_symbol`, use a new helper:

```zig
/// Emit a call instruction with a relocation (for linker resolution).
/// Uses 5-byte padded LEB128 and records a CODE relocation entry.
fn emitRelocatableCall(self: *Self, symbol_idx: u32) !void {
    self.body.append(self.allocator, Op.call) catch return error.OutOfMemory;
    const reloc_offset = ... ; // current offset in code section
    try self.module.code_relocs.append(self.allocator, .{
        .reloc_type = 0, // R_WASM_FUNCTION_INDEX_LEB
        .offset = reloc_offset,
        .symbol_index = symbol_idx,
    });
    // Write 5-byte padded placeholder
    WasmModule.leb128WriteU32Padded5(self.allocator, &self.body, 0);
}
```

### Step 4: helpers.zig — add wasm-ld invocation

**File: `src/eval/test/helpers.zig`**

After `WasmEvaluator.generateWasm()` returns the `.o` bytes, link with builtins:

```zig
fn wasmEvaluatorStr(...) ![]const u8 {
    // ... existing code to get wasm_result ...

    // Link app .o with builtins .o using wasm-ld
    const linked_wasm = try linkWasmWithBuiltins(
        allocator,
        wasm_result.wasm_bytes,  // app .o (relocatable)
        @embedFile("roc_builtins.o"),  // builtins .o (from build)
    );

    // Instantiate linked module in bytebox (only RocOps imports needed)
    // ...
}

fn linkWasmWithBuiltins(allocator: Allocator, app_obj: []const u8, builtins_obj: []const u8) ![]const u8 {
    // Write both .o files to temp paths
    // Invoke wasm-ld: wasm-ld --no-entry --export-all --no-gc-sections app.o builtins.o -o linked.wasm
    // Read and return linked.wasm
    // Clean up temp files
}
```

The `roc_builtins.o` should be embedded via `@embedFile` from
`src/cli/targets/wasm32/roc_builtins.o` (already in the source tree, built by `build.zig`).

#### 4a. Embed builtins .o

In `build.zig`, add the wasm32 builtins .o as an anonymous import to the eval test runner
module so it can `@embedFile` it. Or simpler: reference it via a relative path.

### Step 5: helpers.zig — remove host function implementations

**File: `src/eval/test/helpers.zig`**

Remove all `hostXxx` functions and their `addHostFunction` registrations EXCEPT:
- `hostRocAlloc`
- `hostRocDealloc`
- `hostRocRealloc`
- `hostRocDbg`
- `hostRocExpectFailed`
- `hostRocCrashed`

These 6 RocOps functions stay as host imports because they bridge to the host environment.

**Functions to remove** (~45 host implementations):
- `hostDecMul`, `hostDecDiv`, `hostDecDivTrunc`
- `hostDecToStr`, `hostDecFromStr`
- `hostStrEq`, `hostStrTrim`, `hostStrTrimStart`, `hostStrTrimEnd`
- `hostStrSplit`, `hostStrJoinWith`, `hostStrConcat`, `hostStrRepeat`
- `hostStrReserve`, `hostStrReleaseExcess`, `hostStrWithCapacity`
- `hostStrDropPrefix`, `hostStrDropSuffix`
- `hostStrAsciiLowercased`, `hostStrAsciiUppercased`
- `hostStrCaselessEquals`, `hostStrFromUtf8`
- `hostListEq`, `hostListStrEq`, `hostListListEq`
- `hostListAppendUnsafe`, `hostListSortWith`, `hostListReverse`
- `hostI128DivS`, `hostI128ModS`, `hostU128Div`, `hostU128Mod`
- `hostI128ToStr`, `hostU128ToStr`, `hostFloatToStr`
- `hostI128ToDec`, `hostU128ToDec`, `hostDecToI128`, `hostDecToU128`
- `hostDecToF32`
- `hostI32ModBy`, `hostI64ModBy`
- `hostIntFromStr`, `hostFloatFromStr`

### Step 6: wasm_runner.zig — same cleanup

**File: `src/repl/wasm_runner.zig`**

Apply the same changes as helpers.zig (wasm-ld linking + host function removal).

---

## ABI Reference: Host Import Names → Builtin Symbol Names

The host import names (`roc_xxx`) do NOT match the builtin export names
(`roc_builtins_xxx`). More importantly, the signatures differ (pointer-to-struct
vs decomposed C ABI). Here's the mapping:

| Host Import | Builtin Symbol | Signature Change |
|-------------|----------------|------------------|
| `roc_dec_mul` | `roc_builtins_dec_mul_saturated` | `(ptr,ptr,res)` → `(res,lo1,hi1,lo2,hi2)` |
| `roc_dec_div` | `roc_builtins_dec_div` | same pattern |
| `roc_dec_div_trunc` | `roc_builtins_dec_div_trunc` | same pattern |
| `roc_i128_div_s` | `roc_builtins_num_div_trunc_i128` | same pattern |
| `roc_i128_mod_s` | `roc_builtins_num_rem_trunc_i128` | same pattern |
| `roc_u128_div` | `roc_builtins_num_div_trunc_u128` | same pattern |
| `roc_u128_mod` | `roc_builtins_num_rem_trunc_u128` | same pattern |
| `roc_str_eq` | `roc_builtins_str_equal` | `(ptr,ptr)->i32` → `(b1,l1,c1,b2,l2,c2)->i32` |
| `roc_str_trim` | `roc_builtins_str_trim` | `(ptr,res)` → `(res,b,l,c,roc_ops)` |
| `roc_str_concat` | `roc_builtins_str_concat` | `(ptr1,ptr2,res)` → `(res,b1,l1,c1,b2,l2,c2,roc_ops)` |
| `roc_list_eq` | `roc_builtins_list_eq` (???) | Need to verify — list_eq may not be in builtins |
| `roc_dec_to_str` | `roc_builtins_dec_to_str` | `(ptr,buf)->i32` → `(res,lo,hi,roc_ops)` |
| `roc_i128_to_str` | `roc_builtins_int_to_str` | Need to verify signature |
| ... | ... | ... |

**Action**: Before implementing each category, verify the exact signature in
`src/builtins/dev_wrappers.zig` and confirm it matches the wasm32 C ABI.

---

## Migration Order (Incremental)

Migrate one category at a time. After each, run `zig build test-eval --summary all`
to verify no regressions.

### Phase A: Infrastructure
1. Delete `WasmBuiltinsMerger.zig`
2. Add relocation support to `WasmModule.zig` (Step 2)
3. Add `linkWasmWithBuiltins()` to `helpers.zig` (Step 4)
4. Verify: existing tests still pass (builtins still use host imports for now)

### Phase B: i128/Dec arithmetic (6 builtins)
Simplest ABI — all pointer-based i128 operations:
- `dec_mul`, `dec_div`, `dec_div_trunc`
- `i128_div_s`, `i128_mod_s`, `u128_div`, `u128_mod`

### Phase C: Dec/int conversions (8 builtins)
- `dec_to_str`, `dec_from_str`, `dec_to_i128`, `dec_to_u128`, `dec_to_f32`
- `i128_to_dec`, `u128_to_dec`
- `i128_to_str`, `u128_to_str`

### Phase D: Integer/float ops (5 builtins)
- `i32_mod_by`, `i64_mod_by`
- `float_to_str`, `float_from_str`, `int_from_str`

### Phase E: String pure ops (2 builtins)
- `str_eq`, `str_caseless_ascii_equals`

### Phase F: String mutating ops (13 builtins)
- `str_trim`, `str_trim_start`, `str_trim_end`
- `str_with_ascii_lowercased`, `str_with_ascii_uppercased`
- `str_release_excess_capacity`, `str_with_capacity`
- `str_concat`, `str_repeat`, `str_reserve`
- `str_drop_prefix`, `str_drop_suffix`
- `str_split`, `str_join_with`
- `str_from_utf8`

### Phase G: List ops (4 builtins)
- `list_eq`, `list_str_eq`, `list_list_eq`
- `list_append_unsafe`, `list_reverse`
- `list_sort_with` (currently unused — remove the import)

### Phase H: Cleanup
- Remove all leftover host functions from `helpers.zig` and `wasm_runner.zig`
- Remove unused `?u32` import fields from `WasmCodeGen.zig`
- Apply same changes to `src/repl/wasm_runner.zig`
- Update `TODO_FIX_INTERPRETER_PROMPT.md` to remove resolved items

---

## Key Files

| File | Changes |
|------|---------|
| `src/backend/wasm/WasmModule.zig` | Add relocation tracking, `addUndefinedFunction()`, `leb128WriteU32Padded5()`, `encodeLinkingSection()`, `encodeRelocCodeSection()` |
| `src/backend/wasm/WasmCodeGen.zig` | Replace 37 import fields with `BuiltinSymbols`, rewrite `registerHostImports()`, adapt all call sites for decomposed ABI, add `emitRelocatableCall()` |
| `src/eval/test/helpers.zig` | Add `linkWasmWithBuiltins()` (wasm-ld invocation), embed `roc_builtins.o`, remove ~45 host function implementations |
| `src/repl/wasm_runner.zig` | Same as helpers.zig |
| `src/backend/wasm/WasmBuiltinsMerger.zig` | DELETE (replaced by wasm-ld approach) |
| `src/builtins/dev_wrappers.zig` | Reference only — exact C ABI signatures |
| `build.zig` | May need to add `roc_builtins.o` as embedded resource for test runner |

## Verification

After each phase:
```sh
zig build test-eval --summary all     # All backends compared
zig build test -- --test-filter "fx"  # fx platform tests
```

Target: 1102+ passed, 0 failed, all backends producing identical results.
