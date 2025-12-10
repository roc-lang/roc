# Debug Summary: wasm32 Debug Build Failure

## Overview

The Roc compiler can build WebAssembly (wasm32) applications. When building with **ReleaseFast** optimization, the wasm32 output works correctly. However, when building with **Debug** optimization, the wasm32 output fails at runtime with `Invalid entry_idx 0 >= entry_count 0`.

## Context

The wasm32 build uses a **cross-architecture serialization** system:
1. The **host** (64-bit macOS/Linux) compiles Roc code and serializes the module data into a portable format
2. This serialized data is embedded into the wasm32 binary
3. At runtime, the **wasm32 interpreter shim** deserializes this data and evaluates the Roc expressions

Key files:
- `/src/interpreter_shim/main.zig` - The wasm32 runtime that deserializes and evaluates
- `/src/base/SmallStringInterner.zig` - Has a `supports_inserts: bool` field in Debug builds (void in Release)
- `/src/canonicalize/ModuleEnv.zig` - Contains `ModuleEnv.Serialized` for portable serialization

## The Problem

**Reproducible facts:**
1. `zig build -Doptimize=ReleaseFast` followed by `roc build app.roc --target=wasm32` → **WORKS**
2. `zig build -Doptimize=Debug` followed by `roc build app.roc --target=wasm32` → **FAILS** with `entry_count 0`

The failure occurs because `global_entry_count` (a module-level variable) is 0 when it should be 1.

## Debugging Steps Taken

### Step 1: Added Debug Output

We added `roc_ops.dbg()` calls throughout the code to trace execution:

```zig
// In setupModuleEnvFromSerialized:
roc_ops.dbg("globals set: entry_count=...");

// In evaluateFromSharedMemory:
roc_ops.dbg("after initializeOnce: entry_count=...");
```

**Result:** With debug output, the Debug build **WORKS**:
```
[DBG] globals set: entry_count=1, def_offset=507112
[DBG] after initializeOnce: entry_count=1
Result: "Hello from Roc WASM!"
✅ WASM test completed successfully!
```

### Step 2: Removed Debug Output

When we removed all debug output to clean up the code:

**Result:** Debug build **FAILS** again:
```
[PANIC] Invalid entry_idx 0 >= entry_count 0
```

### Step 3: Verified This Isn't a Caching Issue

We did clean rebuilds with `rm -rf zig-cache zig-out ~/.cache/roc` - same behavior.

### Step 4: Observed the Pattern

| Build Mode | Debug Output | Result |
|------------|--------------|--------|
| ReleaseFast | None | ✅ Works |
| Debug | With `roc_ops.dbg()` calls | ✅ Works |
| Debug | Without debug output | ❌ Fails |

## What We Know (Facts)

1. **The globals are being set correctly** - When debug output is present, we can see `global_entry_count=1` being written
2. **The globals become 0 without debug output** - The check `entry_idx >= global_entry_count` fails because `global_entry_count` reads as 0
3. **File sizes differ** - Debug with output: ~10.1MB, Debug without: ~10.09MB, ReleaseFast: ~3.2MB
4. **The debug output acts as a "barrier"** - The `roc_ops.dbg()` call is a function call to the host that the compiler cannot optimize away

## Current Theories

### Theory 1: Compiler Optimization Issue

The Zig compiler might be:
- Reordering the writes to globals
- Optimizing away the writes because it thinks the globals are unused
- There's something about the function call to `roc_ops.dbg()` that acts as a memory barrier

**Evidence for:** Adding any debug output makes it work; removing it breaks it.

**Evidence against:** Debug builds typically have *fewer* optimizations than Release builds, yet Debug fails and Release works.

### Theory 2: Memory Corruption

Something might be corrupting the globals between when they're set and when they're read:
- Stack overflow overwriting globals
- Heap allocator allocating memory that overlaps with globals section
- Alignment issues in the wasm32 memory layout

**Evidence for:** We observed `global_entry_count` changing from 1 to 148 and `roc__serialized_size` changing from 507116 to 0 in one test run.

**Evidence against:** The corruption is consistent (always becomes 0), not random garbage.

### Theory 3: Code Path Issue

The code path that sets the globals might not be executing in certain builds:
- Some early return or error path
- The magic number check failing
- Different behavior in how `setupModuleEnvFromSerialized` is called

**Evidence against:** When we add debug output, we can confirm the code path IS executing and setting the correct values.

## Attempted Fixes

1. **Memory fence** - Tried `@fence(.seq_cst)` → Invalid builtin in Zig
2. **Calling convention** - Tried `callconv(.Unspecified)` → Invalid member
3. **Volatile writes** - In progress, trying:
   ```zig
   const entry_count_ptr: *volatile u32 = @ptrCast(&global_entry_count);
   entry_count_ptr.* = entry_count;
   ```

## Code Flow

```
roc_entrypoint()
  └── evaluateFromSharedMemory()
        └── initializeOnce()
              └── setupModuleEnv()
                    └── setupModuleEnvFromSerialized()
                          ├── Reads header: entry_count = 1
                          └── Sets: global_entry_count = entry_count  // <-- This write seems to get lost
        └── Reads global_entry_count  // <-- Reads 0 instead of 1
        └── if (entry_idx >= global_entry_count) PANIC
```

## Relevant Global Variables

Located in `/src/interpreter_shim/main.zig`:

```zig
var global_entry_count: u32 = 0;
var global_def_indices_offset: u64 = 0;
var global_is_serialized_format: bool = false;
```

These are set in `setupModuleEnvFromSerialized()` and read in `evaluateFromSharedMemory()`.

## Questions That Need Answering

1. Why does adding `roc_ops.dbg()` calls make Debug builds work?
2. Why do ReleaseFast builds work without any debug output?
3. Is this a Zig compiler bug specific to wasm32 Debug builds?
4. Is there something unique about how wasm32 globals work that we're missing?

## Suggested Next Steps

1. **Try volatile writes/reads** for the globals (in progress)
2. **Examine the generated wasm** with `wasm-objdump` to see if the global writes are present
3. **Test with a minimal reproducer** - strip down to just setting/reading one global
4. **Check Zig version** - might be a known bug in the Zig compiler for wasm32 targets (currently using zig-aarch64-macos-0.15.2)
5. **Add a single minimal debug call** to find the minimum "barrier" needed to make it work
6. **Compare wasm disassembly** between working and non-working builds

## How to Reproduce

```bash
# Working (ReleaseFast):
cd /Users/luke/Documents/GitHub/roc
zig build -Doptimize=ReleaseFast
cd test/wasm
rm -f app.wasm
../../zig-out/bin/roc build app.roc --target=wasm32
node test_wasm.mjs  # ✅ Works

# Failing (Debug):
cd /Users/luke/Documents/GitHub/roc
zig build -Doptimize=Debug
cd test/wasm
rm -f app.wasm
../../zig-out/bin/roc build app.roc --target=wasm32
node test_wasm.mjs  # ❌ Fails with "Invalid entry_idx 0 >= entry_count 0"
```

## Current State of Code

The file `/src/interpreter_shim/main.zig` currently has volatile writes added (untested):

```zig
// Store header info in globals for use during evaluation
// Use volatileStore to ensure these writes are not optimized away
const entry_count_ptr: *volatile u32 = @ptrCast(&global_entry_count);
entry_count_ptr.* = entry_count;
const def_offset_ptr: *volatile u64 = @ptrCast(&global_def_indices_offset);
def_offset_ptr.* = def_indices_offset;
const format_ptr: *volatile bool = @ptrCast(&global_is_serialized_format);
format_ptr.* = true;
```

This fix has not yet been tested.
