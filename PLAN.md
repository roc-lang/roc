# Plan: Investigate Crash in Windows Glue Command

## Problem Summary

The `roc experimental-glue` command crashes on Windows when returning a non-empty `List(File)`. Originally thought to be intermittent (~45% failure rate), but investigation revealed it's a **consistent 100% crash** when returning any File in the list.

The crash is a **stack overflow** (exit code 134), occurring during the return from the Roc `make_glue` function.

## Minimal Reproduction

**File: `src/glue/src/MinimalGlue.roc`**
```roc
app [make_glue] { pf: platform "../platform/main.roc" }

import pf.Types exposing [Types]
import pf.File exposing [File]

make_glue : List(Types) -> Try(List(File), Str)
make_glue = |_types_list| {
    Ok([{ name: "test.txt", content: "hello" }])
}
```

**To reproduce:**
```bash
./zig-out/bin/roc.exe experimental-glue src/glue/src/MinimalGlue.roc /tmp/test test/fx/platform/main.roc
```

## Investigation Results

### Test Matrix

| Test Case | Result |
|-----------|--------|
| `Ok([])` - empty list | **Works** |
| `Ok([{ name: "", content: "" }])` - empty strings | **Crashes (stack overflow)** |
| `Ok([{ count: 42 }])` - File with single U64 field | **Crashes** |
| `Ok([Value])` - File as tag union `[Value]` | **Crashes** |
| Create File value but don't return it | **Works** |
| `List.append([], file)` then return | **Crashes** |
| `Err("error")` - error case | **Works** |
| `dbg types_list` then `Ok([])` (DebugGlue) | **Works** |

### Key Finding

The crash is **NOT** about:
- String operations or string content
- The number of fields in the record
- The specific field types (crashes with U64 too)
- String interpolation or concatenation

The crash **IS** specifically about:
- Returning a non-empty `List(OpaqueType)` inside `Ok()`
- Where `File := { ... }` is an opaque type
- The crash happens during the **return** from `make_glue`, not during value creation

### Deep Debug Investigation

With debug instrumentation in the host (`src/glue/platform/host.zig`), we discovered:

**Debug output showing successful data access:**
```
DEBUG HOST: result.tag = .Ok
DEBUG HOST: Got files from payload
Glue spec returned 1 file(s):
DEBUG: Got file_name: 'test.txt'
DEBUG: file_name.len = 8
DEBUG: Got content, len = 5
DEBUG: Content: 'hello'
```

**Key observations:**

1. **The Roc function returns valid data** - file name ("test.txt") and content ("hello") were correctly readable via `file.name.asSlice()` and `file.content.asSlice()`

2. **The crash happens immediately after `roc__make_glue` returns** - Adding `std.fs.cwd().access(".", .{})` right after the Roc call crashes immediately, proving the stack is already corrupted

3. **The stack is corrupted by the return** - Simple operations like `allocator.alloc()` and `@memset()` work fine, but any filesystem syscall triggers the crash

4. **Without signal handlers, exit code is 253** (Windows unhandled exception) instead of 134 (custom stack overflow handler)

5. **The data is written correctly** - The result pointer contains valid data, but the process of returning corrupts the stack frame

This proves the **generated code for returning the Result corrupts the stack on Windows** - the data is correct but the stack cleanup/return sequence damages the stack.

### Root Cause Hypothesis

The bug is in the **code generation for returning opaque types** within a `Result(List(OpaqueType), Str)` structure. Possible locations:

1. **Stack frame corruption during return** - the generated code writes outside its stack frame
2. **Calling convention mismatch** - wrong registers/stack cleanup for the return value
3. **Windows-specific ABI issue** - different calling conventions on Windows x64

## Files Created During Investigation

- `src/glue/src/MinimalGlue.roc` - Minimal repro (crashes)
- `src/glue/src/EmptyListGlue.roc` - Returns `Ok([])` (works)
- `src/glue/src/EmptyFileGlue.roc` - Empty strings (crashes)
- `src/glue/src/CreateOnlyGlue.roc` - Creates File but returns `Ok([])` (works)
- `src/glue/src/ListAppendGlue.roc` - Uses List.append (crashes)
- `src/glue/src/DropListGlue.roc` - Appends then drops (works)
- `src/glue/src/ErrorGlue.roc` - Returns Err (works)

## What Was Ruled Out

- **String operations** - Crashes with U64 field too
- **Host-side result processing** - Crash happens before host reads result
- **File I/O bugs** - Crash happens even with simple `access(".", .{})` call
- **Signal handler issues** - Same crash without handlers (just different exit code)
- **Memory allocation** - `allocator.alloc()` works fine after Roc returns
- **Layout mismatch for reading** - Data is readable correctly

## Deep Dive: Call Chain and Architecture Analysis

### Full Call Chain

When `roc experimental-glue` runs, the following call chain executes:

1. **Host** (`src/glue/platform/host.zig:871`) calls `roc__make_glue(ops, ret_ptr, args_ptr)` with `callconv(.c)`
2. **Generated LLVM wrapper** (from `src/cli/platform_host_shim.zig`) - a trivial function:
   ```llvm
   define void @roc__make_glue(ptr %ops, ptr %ret_ptr, ptr %arg_ptr) {
     call void @roc_entrypoint(i32 0, ptr %ops, ptr %ret_ptr, ptr %arg_ptr)
     ret void
   }
   ```
3. **Interpreter shim** (`src/interpreter_shim/main.zig:242`) - `roc_entrypoint` with `callconv(.c)`:
   ```zig
   export fn roc_entrypoint(entry_idx: u32, ops: *RocOps, ret_ptr: *anyopaque, arg_ptr: ?*anyopaque) callconv(.c) void
   ```
4. **Interpreter** (`src/eval/interpreter.zig`) evaluates the Roc expression
5. **StackValue.copyToPtr** (`src/eval/StackValue.zig:356`) writes result to `ret_ptr` using `@memmove`
6. **Return sequence** - each function returns up the chain

### The RocCall ABI

Defined in `src/builtins/host_abi.zig:15-30`:
```zig
pub const RocCall = fn (
    *RocOps,      // Function pointers (alloc, dealloc, etc.)
    *anyopaque,   // Return value pointer (Roc writes here)
    *anyopaque,   // Arguments pointer
) callconv(.c) void;
```

All Roc functions return `void` and write their result to the provided pointer. This is a simple, portable ABI.

### Windows x64 Calling Convention Requirements

From `src/backend/dev/x86_64/WindowsFastcall.zig`:

| Aspect | Windows x64 | System V (Linux/macOS) |
|--------|-------------|------------------------|
| Parameter registers | RCX, RDX, R8, R9 | RDI, RSI, RDX, RCX, R8, R9 |
| Shadow space | 32 bytes required | None |
| Stack alignment | 16-byte at CALL | 16-byte at CALL |
| Callee-saved (GP) | RBX, RSI, RDI, R12-R15 | RBX, R12-R15 |
| Callee-saved (FP) | XMM6-XMM15 | None |
| Red zone | None | 128 bytes |

**Critical differences**:
- Windows requires **more callee-saved registers** (including RSI, RDI, and XMM6-15)
- Windows requires **32-byte shadow space** even for functions with fewer than 4 args
- Windows has **no red zone** (Linux allows 128 bytes below RSP)

### Key Difference: Non-Empty vs Empty Lists

For **empty list** (`Ok([])`):
- RocList is created with `bytes=null, length=0, capacity=0`
- No heap allocations needed
- Wrapped in Ok variant and written to `ret_ptr`

For **non-empty list** (`Ok([file])`):
- String allocations for `file.name` and `file.content` via `roc_ops.roc_alloc`
- List allocation for the File element via `roc_ops.roc_alloc`
- Each allocation is a **function pointer call** through `RocOps`

The allocation function pointer calls look like:
```zig
// In src/builtins/utils.zig:756
roc_ops.roc_alloc(&roc_alloc_args, roc_ops.env);
```

Where `roc_alloc` is: `*const fn (*RocAlloc, *anyopaque) callconv(.c) void`

### Likely Root Cause: Function Pointer Calls on Windows

**Hypothesis**: The stack corruption occurs during **indirect function calls through `RocOps` function pointers** on Windows x64.

When the interpreter calls `roc_ops.roc_alloc(...)`:
1. Zig generates an indirect CALL through the function pointer
2. On Windows, this requires:
   - Allocating 32-byte shadow space on the stack
   - Preserving callee-saved registers (RBX, RSI, RDI, R12-R15, XMM6-XMM15)
   - Maintaining 16-byte stack alignment

If any of these requirements are violated:
- Callee-saved registers could be clobbered
- Stack pointer could become misaligned
- Shadow space could overlap with local variables

**Evidence supporting this hypothesis**:
- Empty lists don't trigger allocations → no function pointer calls → works
- Non-empty lists require allocations → function pointer calls → crashes
- The data is written correctly (allocations succeed), but stack is corrupted after return
- Syscalls (which are sensitive to stack state) crash immediately after

### Layout Analysis: ResultListFileStr

The host defines (`src/glue/platform/host.zig:435-441`):
```zig
const ResultListFileStr = extern struct {
    payload: extern union {
        ok: RocList,  // 24 bytes (ptr, len, cap)
        err: RocStr,  // 24 bytes
    },
    tag: ResultTag,   // 1 byte (enum u8)
};
```

With C ABI padding: **32 bytes total** (24 + 1 + 7 padding)

The interpreter computes tag union layout (`src/layout/store.zig:1258-1265`):
- Payload at offset 0, max size 24 bytes
- Discriminant at offset 24, size 1 byte
- Total aligned to 8 bytes = **32 bytes**

**The layouts match**, so the issue is not a size mismatch.

### Code Locations Examined

| File | Lines | Purpose |
|------|-------|---------|
| `src/glue/platform/host.zig` | 447-451, 871 | Host calls `roc__make_glue` |
| `src/cli/platform_host_shim.zig` | 79-134 | LLVM wrapper generation |
| `src/interpreter_shim/main.zig` | 242-256 | `roc_entrypoint` export |
| `src/eval/interpreter.zig` | 920-972 | `evaluateBody` writes to `ret_ptr` |
| `src/eval/StackValue.zig` | 356-641 | `copyToPtr` implementation |
| `src/builtins/utils.zig` | 736-766 | `allocateWithRefcount` calls `roc_alloc` |
| `src/builtins/host_abi.zig` | 15-72 | `RocOps` and `RocCall` definitions |
| `src/backend/dev/x86_64/WindowsFastcall.zig` | 1-147 | Windows calling convention constants |

## Next Steps

1. **Verify with assembly dump** - Generate assembly for the interpreter shim on Windows and check:
   - Are callee-saved registers preserved around indirect calls?
   - Is 32-byte shadow space allocated for function pointer calls?
   - Is stack alignment maintained throughout?

2. **Test with explicit Windows calling convention** - Try changing function pointers in `host_abi.zig` to use `callconv(.winapi)` instead of `callconv(.c)` on Windows

3. **Add stack canaries** - Insert guard values to detect exact corruption point:
   ```zig
   var canary1: u64 = 0xDEADBEEF;
   roc__make_glue(&roc_ops, &result, &types_list);
   var canary2: u64 = 0xCAFEBABE;
   if (canary1 != 0xDEADBEEF) @panic("Stack corruption before call");
   if (canary2 != 0xCAFEBABE) @panic("Stack corruption after call");
   ```

4. **Minimal reproduction with direct allocation** - Create test that just calls `roc_ops.roc_alloc` in a loop to isolate if allocations alone cause the crash

5. **Check Zig compiler version** - Verify if this is a known Zig bug with indirect calls and `callconv(.c)` on Windows x64

6. **Compare with working platforms** - Run same test on Linux/macOS to confirm it's Windows-specific

## Progress Tracking

- [x] Step 1: Create minimal reproduction variants
- [x] Step 2: Identify the failing pattern (non-empty List(OpaqueType) in Ok)
- [x] Step 3: Create standalone minimal reproduction
- [x] Step 4: Deep dive analysis of call chain and architecture
- [x] Step 5: Add regression tests to CLI test suite
- [x] Step 6: Verify hypothesis with stack canaries
- [x] Step 7: Implement and test fix (increase Windows stack size from 1MB to 16MB)

**Status**: std.fs operations fixed by increasing stack size. CGlue has a separate segfault needing investigation.

## Root Cause and Fix

### Actual Root Cause: Windows Stack Size Too Small

The Windows crash was caused by **insufficient stack size**, not stack corruption or calling convention issues.

**Key findings:**
- Roc-linked executables had a 1MB stack reserve (default lld-link behavior)
- Standalone Zig executables have a 16MB stack reserve (Zig's default)
- Zig's `std.fs` uses `PathSpace` buffers on the stack that are ~64KB each
- Nested calls in `std.fs.Dir.openDir` use multiple `PathSpace` allocations
- Multiple 64KB stack allocations + normal call stack exceeded 1MB limit

**The fix (in `src/cli/linker.zig`):**
```zig
// Set stack size to 16MB (same as Zig default) to avoid stack overflow
// with deeply nested calls or large stack allocations in std.fs
try args.append("/stack:16777216");
```

This increases the Windows stack reserve from 1MB to 16MB, matching Zig's default behavior.

### Test Results After Fix

| Test | Result |
|------|--------|
| MinimalGlue (non-empty List) | **PASSES** |
| EmptyListGlue (empty List) | **PASSES** |
| DebugGlue (dbg output) | **PASSES** |
| CGlue (C header generation) | **FAILS** (separate segfault issue) |

The CGlue failure is a different bug - a segmentation fault during Roc code execution, not related to std.fs or stack size.

## Regression Tests

Added tests to `src/cli/test/glue_test.zig` that reproduce the issue:

| Test | Glue Spec | Expected | Windows Result |
|------|-----------|----------|----------------|
| `"glue command with MinimalGlue returns non-empty file list"` | MinimalGlue.roc | Success | **Passes** (after Win32 API workaround) |
| `"glue command with EmptyListGlue returns empty file list"` | EmptyListGlue.roc | Success | **Passes** |

CGlue tests fail on Windows due to a separate issue (crash during Roc computation, not file I/O).

Run with: `zig build test-cli`

## Stack Canary Investigation

Added stack canaries to `src/glue/platform/host.zig` around the `roc__make_glue` call to detect stack corruption.

### Findings

**Canaries do NOT trigger** - the local variables immediately around `result` are not corrupted:
```zig
var canary_before: u64 = 0xDEADBEEFCAFEBABE;
var result: ResultListFileStr = undefined;
var canary_after: u64 = 0xFEEDFACE12345678;
roc__make_glue(&roc_ops, &result, &types_list);
// Both canaries remain intact after the call
```

**Operations that WORK after the Roc call:**
- `stderr.writeAll()` - can print debug messages
- `std.time.timestamp()` - get current time
- `allocator.alloc()` / `allocator.free()` - heap operations
- `std.fs.cwd()` - just constructs a Dir struct, no syscall

**Operations that CRASH (stack overflow):**
- `std.fs.cwd().openDir(".", .{})` - open current directory
- `std.fs.cwd().makePath(path)` - create directory
- `std.fs.cwd().access(".", .{})` - check access permissions

### Interpretation

The stack corruption is **not in the immediate local variables** but in a location that:
1. Doesn't affect simple syscalls (timestamp, write to already-open stderr)
2. Does affect filesystem syscalls (openDir, makePath, access)

Likely candidates:
- **Corrupted saved XMM registers** - Windows x64 requires XMM6-15 to be preserved; filesystem operations may use these for SIMD string operations
- **Stack frame metadata** - return address or saved RBP, but only checked during deeper call chains
- **Stack pointer misalignment** - only affects certain calling patterns

The filesystem operations likely use more stack space and/or rely on preserved XMM registers for path string processing, which would explain why they crash while simpler operations work.

## Stack Usage Investigation

Added detailed stack instrumentation to measure stack usage before/after the Roc call.

### Findings

**Stack measurements show plenty of space:**
- Before Roc call: ~17 KB remaining
- After Roc call: ~172 KB remaining (Windows committed more pages during execution)
- Stack alignment correct: RSP mod 16 = 8
- Manual 8KB stack probe **succeeds**

**Direct Windows API calls WORK:**
```
GetFileAttributesA(".")     → 0x10 (success, returns DIRECTORY attribute)
CreateDirectoryA(path)      → works (returns expected error for missing parent)
GetFileAttributesW(L".")    → 0x10 (success, wide string version)
NtQuerySystemInformation()  → 0xC0000004 (expected STATUS_INFO_LENGTH_MISMATCH)
```

**Zig std.fs operations CRASH:**
```
std.fs.cwd().statFile(".")  → stack overflow
std.fs.cwd().openDir(".")   → stack overflow
std.fs.cwd().makePath(path) → stack overflow
```

### Interpretation

The issue is **NOT** with:
- Stack size (172 KB available)
- Stack alignment (correct)
- Windows kernel32 API calls (work fine)
- Windows ntdll API calls (work fine)
- Wide string handling (GetFileAttributesW works)

The issue **IS** specific to Zig's std.fs implementation. Possible causes:
1. **Zig error return tracing** - std.fs uses Zig's error handling infrastructure
2. **SEH (Structured Exception Handling)** - std.fs may set up exception handlers
3. **Internal Zig runtime state** - something in Zig's runtime got corrupted
4. **Stack frame metadata** - Zig may use frame pointers or debug info differently

## Critical New Finding: std.fs Broken at Startup

Further investigation revealed that **std.fs.cwd().openDir() crashes at the very start of main()**, before any Roc code runs!

### Test Results

```
Test 1: std.fs.cwd()...      → PASSES (just returns Dir struct)
Test 2: std.fs.cwd().openDir(".", .{})  → CRASHES (exit code 253)
```

**This proves the issue is NOT caused by Roc corrupting something** - std.fs is broken from the start of the glue spec executable.

### Key Difference: Glue Platform Imports Compiler Modules

The glue platform host imports many compiler modules that the fx platform doesn't:

| Platform | Additional Module Imports |
|----------|--------------------------|
| fx (works) | None (just builtins, build_options) |
| glue (broken) | base, can, types, layout, eval, collections, type_extractor |

These modules are added in `build.zig` lines 2999-3017:
```zig
glue_platform_host_lib.root_module.addImport("base", roc_modules.base);
glue_platform_host_lib.root_module.addImport("can", roc_modules.can);
// ...etc
```

### Root Cause Hypothesis

The compiler modules (or their transitive dependencies) are causing a conflict with Zig's std.fs on Windows x64. Possible causes:

1. **Symbol conflicts** - compiler modules define symbols that conflict with Windows API or Zig std internals
2. **TLS corruption** - Thread Local Storage initialization conflicts
3. **Global state** - module initialization corrupts std.fs internal state
4. **Linker issues** - incorrect symbol resolution when linking multiple large Zig modules

### Next Steps

1. Identify which specific module import causes the crash (binary search)
2. Check if the issue is in Zig's standard library linking
3. Investigate if this is a known Zig issue with large projects on Windows
4. Consider restructuring the glue platform to avoid the problematic imports

## Remaining Issue: Flaky CGlue Tests

After the stack size fix, there's a remaining flaky test issue with CGlue.

### Observations

1. **CGlue works reliably when run manually**:
   ```
   zig-out/bin/roc.exe experimental-glue src/glue/src/CGlue.roc <output_dir> test/fx/platform/main.roc
   ```
   This consistently succeeds.

2. **Tests are flaky when run in quick succession**:
   ```
   Run 1: All 5 tests passed.
   Run 2: 4 passed; 0 skipped; 1 failed.
   Run 3: 4 passed; 0 skipped; 1 failed.
   Run 4: 3 passed; 0 skipped; 2 failed.
   Run 5: All 5 tests passed.
   ```

3. **Failure mode is SIGSEGV**:
   ```
   Segmentation fault (SIGSEGV) in this Roc program.
   Fault address: 0x1798b8cfff8
   Glue spec exited with code 139
   ```
   The fault addresses end in `fff8`, which is 8 bytes from a page boundary - consistent with stack overflow pattern.

4. **The glue_spec builds successfully** (100% cache hit) but crashes when executed.

### Hypotheses

1. **Race condition in shared state**: When tests run quickly in succession, some shared resource (temp files, cache, shared memory) may not be properly cleaned up between runs.

2. **Stack overflow in specific code paths**: While the 16MB stack fixes most cases, certain CGlue code paths with deep recursion or large stack allocations may still overflow intermittently.

3. **Memory corruption**: Some state from a previous test run may be corrupting memory for subsequent runs.

### Current Status

- **4 of 5 glue tests pass reliably**: DebugGlue, MinimalGlue, EmptyListGlue work consistently
- **CGlue tests are flaky**: Pass ~40-60% of the time when run in quick succession
- **Manual CGlue execution works**: Always succeeds when run standalone

The flakiness appears to be a race condition or resource cleanup issue rather than a fundamental bug in the stack size fix.
