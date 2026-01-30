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

### Root Cause Hypothesis

The bug is in the **code generation for returning opaque types** within a `Result(List(OpaqueType), Str)` structure. Possible locations:

1. **Opaque type unwrapping/wrapping during return** - infinite recursion in generated code
2. **Reference counting for opaque types in lists** - recursive decref loop
3. **ABI mismatch** - the host expects a different layout than Roc generates

## Files Created During Investigation

- `src/glue/src/MinimalGlue.roc` - Minimal repro (crashes)
- `src/glue/src/EmptyListGlue.roc` - Returns `Ok([])` (works)
- `src/glue/src/EmptyFileGlue.roc` - Empty strings (crashes)
- `src/glue/src/CreateOnlyGlue.roc` - Creates File but returns `Ok([])` (works)
- `src/glue/src/ListAppendGlue.roc` - Uses List.append (crashes)
- `src/glue/src/DropListGlue.roc` - Appends then drops (works)
- `src/glue/src/ErrorGlue.roc` - Returns Err (works)

## Next Steps

1. **Examine codegen for opaque types** - Look at `src/backend/llvm/codegen.zig` or equivalent for how opaque type returns are generated

2. **Check layout computation** - Verify the layout of `Result(List(File), Str)` matches between Roc and host

3. **Add debug tracing** - Instrument the generated code to find where the infinite recursion occurs

4. **Compare IR** - Generate IR for both DebugGlue (works) and MinimalGlue (crashes) to see the difference

## Progress Tracking

- [x] Step 1: Create minimal reproduction variants
- [x] Step 2: Identify the failing pattern (non-empty List(OpaqueType) in Ok)
- [x] Step 3: Create standalone minimal reproduction
- [ ] Step 4: Root cause analysis in codegen
- [ ] Step 5: Fix the bug
