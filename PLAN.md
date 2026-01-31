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
- [ ] Step 6: Verify hypothesis with assembly inspection or stack canaries
- [ ] Step 7: Implement and test fix

## Regression Tests

Added tests to `src/cli/test/glue_test.zig` that reproduce the issue:

| Test | Glue Spec | Expected | Windows Result |
|------|-----------|----------|----------------|
| `"glue command with MinimalGlue returns non-empty file list"` | MinimalGlue.roc | Success | Stack overflow (exit 134) |
| `"glue command with EmptyListGlue returns empty file list"` | EmptyListGlue.roc | Success | **Passes** |

The existing CGlue tests also fail on Windows for the same reason (CGlue returns a non-empty file list).

Run with: `zig build test-cli`
