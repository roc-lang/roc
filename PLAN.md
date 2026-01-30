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

## Next Steps

1. **Compare generated assembly** between DebugGlue (`Ok([])`) and MinimalGlue (`Ok([file])`) to find the difference in return sequence

2. **Check Windows x64 calling convention** - The `roc__make_glue` function uses `callconv(.c)` which may have different stack cleanup rules on Windows

3. **Examine how the Result payload is written** - The non-empty List case writes more data; check if it overflows the return area

4. **Look at LLVM codegen for tag unions** - The Result type is a tag union; check how payloads are handled

## Progress Tracking

- [x] Step 1: Create minimal reproduction variants
- [x] Step 2: Identify the failing pattern (non-empty List(OpaqueType) in Ok)
- [x] Step 3: Create standalone minimal reproduction
- [ ] Step 4: Root cause analysis in codegen
- [ ] Step 5: Fix the bug
