# Playground WASM Debugging Notes

## Current Issue: 32-bit vs 64-bit Serialization (2025-10-11)

### The Real Problem

Tests are failing with garbage values like `0xAABBCCAA` (2863311530) when deserializing builtin modules in WASM.

**Root Cause:** We are serializing types that contain **pointers and slices**, which have different sizes on 32-bit (WASM) vs 64-bit (native):
- **64-bit native**: pointers = 8 bytes, slices = 16 bytes (ptr + len)
- **32-bit WASM**: pointers = 4 bytes, slices = 8 bytes (ptr + len)

When we compile builtins on 64-bit and try to deserialize on 32-bit WASM, the struct layouts don't match, causing garbage values.

### Why `.Serialized` Types Exist

The `.Serialized` versions of types exist **specifically to avoid serializing pointers/slices**. They should use:
- **Offsets** (i64) instead of pointers
- **Lengths** (u64) instead of slice length fields
- **Fixed-size types** that are the same size on all platforms

### The Wrong Solution: `extern struct`

Using `extern struct` does NOT fix this problem! It only prevents field reordering, but if the fields themselves are different sizes (like pointers), no amount of reordering will help.

**Example of what WON'T work:**
```zig
pub const Serialized = extern struct {
    items: []u32,  // ❌ WRONG! Slice is 16 bytes on 64-bit, 8 bytes on 32-bit
}
```

**Correct approach:**
```zig
pub const Serialized = extern struct {
    items_offset: i64,  // ✅ Always 8 bytes
    items_len: u64,     // ✅ Always 8 bytes
}
```

### What We Need to Fix

Find and eliminate ANY serialized types that contain:
1. Raw pointers (`*T`, `[*]T`)
2. Slices (`[]T`)
3. `std.mem.Allocator` (contains 2 pointers)
4. Any other platform-dependent types

ALL Serialized structs should use:
- Fixed-size integers (u8, u16, u32, u64, i64)
- Fixed-size arrays of fixed-size types
- OTHER Serialized types (which should themselves be platform-independent)

### Debugging the Current Failure

The error shows:
```
loadCompiledModule: bin_data.len=257756, @sizeOf(ModuleEnv.Serialized)=760
loadCompiledModule: raw all_statements.span.start=2863311530, .len=2863311530
compileSource: Bool module has 2863311530 statements
```

The value `2863311530` = `0xAABBCCAA` is the padding pattern from SafeList, which means we're reading from the wrong offset. This happens because the serialized struct on disk (from 64-bit) has a different layout than what WASM (32-bit) expects.

### Action Items

1. ❌ **Remove all `extern` keywords from Serialized structs** - they're papering over the issue
2. ✅ **Audit all `.Serialized` types** to ensure they contain NO pointers/slices
3. ✅ **Use offset-based serialization** consistently (i64 offsets instead of pointers)
4. ✅ **Test that @sizeOf(Serialized) is the same on 32-bit and 64-bit**

---

## Previous Status (2025-10-10)

### What's Working
- ✅ REPL tests: **22/22 passing**
- ✅ Empty source code test passes in playground WASM

### What Was Failing
- ❌ Playground WASM tests failed with garbage values when loading builtins

## Root Cause of Previous Issue - SOLVED!

### The Array Synchronization Bug
The original issue was an **array synchronization bug** introduced in commit `7ba3f684d5`:

**The Bug:**
```zig
// BROKEN CODE
const bool_stmt = bool_module.env.store.getStatement(builtin_indices.bool_type);
const actual_bool_idx = try env.store.addStatement(bool_stmt, base.Region.zero());
// Missing: type variable addition!
```

This caused `ModuleEnv.debugAssertArraysInSync()` to panic because `store.addStatement()` adds to `cir_nodes` and `region_nodes` but not `type_nodes`.

**The Fix:**
```zig
// FIXED CODE
const bool_stmt = bool_module.env.store.getStatement(builtin_indices.bool_type);
const actual_bool_idx = try env.addStatementAndTypeVar(bool_stmt, .err, base.Region.zero());
```

### Files Fixed
1. `/Users/rtfeldman/code/roc2/src/playground_wasm/main.zig:954,957`
2. `/Users/rtfeldman/code/roc2/src/repl/repl_test.zig:287,292`
3. `/Users/rtfeldman/code/roc2/src/check/test/TestEnv.zig` (2 locations)
4. `/Users/rtfeldman/code/roc2/src/eval/test/helpers.zig:381,384`
5. `/Users/rtfeldman/code/roc2/src/repl/Repl.zig` (2 locations)
6. `/Users/rtfeldman/code/roc2/src/repl/eval.zig:470,475`

## Critical Finding (2025-10-11 continued)

### The Real Root Cause

**FOUND IT:** The serialized struct has different sizes on different platforms due to **struct padding and alignment**:

```
Native (64-bit) compiler: @sizeOf(ModuleEnv.Serialized) = 976 bytes
WASM (32-bit):           @sizeOf(ModuleEnv.Serialized) = 760 bytes
Difference:              216 bytes!
```

Even though ALL fields use fixed-size types (u8, u16, u32, u64, i64, arrays), Zig adds padding between fields to meet alignment requirements, and those requirements differ between 32-bit and 64-bit platforms.

### Why This Causes Garbage Values

1. The builtin compiler (64-bit) serializes ModuleEnv with 976-byte layout
2. The `.bin` file contains data laid out for 976 bytes
3. WASM (32-bit) tries to read the data assuming 760-byte layout
4. Fields are at different offsets, so WASM reads from wrong locations
5. The value `0xAABBCCAA` (2863311530) is the 0xAA padding pattern used by SafeList

### The Wrong Solutions

1. ❌ **Using `extern struct`** - Only prevents field reordering, doesn't fix padding/alignment differences
2. ❌ **Using `packed struct`** - Would make both platforms have the same size but break pointer alignment
3. ❌ **Trying to make fields "more fixed-size"** - They're already as fixed as possible!

### The Right Solution

We need to **serialize field-by-field** in a platform-independent binary format, not just cast the struct to bytes. The current approach (`@ptrCast` from binary data to struct) fundamentally cannot work across platforms with different alignment rules.

Options:
1. Write each field explicitly to the binary format in a fixed order
2. Use a serialization format that doesn't depend on struct layout (like msgpack, bincode, etc.)
3. Add explicit padding fields to force consistent layout (hacky and fragile)

The current serialization in `CompactWriter` appears to write the struct as-is, which includes platform-specific padding.
