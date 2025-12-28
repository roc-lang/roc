# Improve LLVM Object Ownership Clarity

## Problem

The LLVM JIT compilation code has complex ownership semantics for LLVM objects. The current code uses comments to document ownership transfers, but the comment structure can be confusing and could lead to bugs if someone modifies the code without fully understanding the ownership model.

## Location

`src/llvm_compile/compile.zig`, lines 40-65:

```zig
// Create thread-safe context
// Note: We don't dispose the context here - the JIT takes ownership through the ThreadSafeModule
const ts_context = bindings.OrcThreadSafeContext.create();

// Get the underlying LLVM context for parsing
const context = ts_context.getContext();

// Create memory buffer from bitcode
const mem_buf = bindings.MemoryBuffer.createMemoryBufferWithMemoryRange(
    bitcode_bytes.ptr,
    bitcode_bytes.len,
    "roc_bitcode",
    bindings.Bool.False,
);

// Parse bitcode into module (don't defer dispose - ownership transfers to ThreadSafeModule)
var module: *bindings.Module = undefined;
const parse_result = context.parseBitcodeInContext2(mem_buf, &module);
if (parse_result.toBool()) {
    mem_buf.dispose();  // <-- Dispose on error
    return error.BitcodeParseError;
}
// Note: mem_buf is consumed by parseBitcodeInContext2, don't dispose  <-- But this comment suggests we shouldn't dispose

// Wrap module in thread-safe module (takes ownership of module)
const ts_module = bindings.OrcThreadSafeModule.create(module, ts_context);
// Note: Don't dispose ts_module - ownership transfers to LLJIT
```

The comments are correct, but their placement and structure makes it easy to misread. Specifically:

1. "don't defer dispose" appears before the error path where we DO dispose
2. "mem_buf is consumed" appears after code that disposes on error
3. Multiple ownership transfers happen in sequence with similar-looking comments

## LLVM Ownership Model

Understanding the LLVM C API ownership model:

1. **MemoryBuffer**: Consumed by `parseBitcodeInContext2`. If parsing succeeds, LLVM takes ownership and will free it. If parsing fails, the caller must dispose.

2. **Module**: Created by parsing, then transferred to `ThreadSafeModule`. Don't dispose directly.

3. **ThreadSafeContext**: Transferred to `ThreadSafeModule` along with the module. Don't dispose directly.

4. **ThreadSafeModule**: Transferred to LLJIT via `addLLVMIRModule`. Don't dispose directly.

5. **LLJIT**: Must be disposed by the caller. This will clean up all owned objects.

## What Needs to Change

### Option 1: Restructure with Clearer Control Flow

```zig
// Parse bitcode - on success, mem_buf ownership transfers to LLVM
var module: *bindings.Module = undefined;
const parse_result = context.parseBitcodeInContext2(mem_buf, &module);
if (parse_result.toBool()) {
    // Parse failed - we still own mem_buf, so dispose it
    mem_buf.dispose();
    return error.BitcodeParseError;
}
// Parse succeeded - mem_buf was consumed by LLVM, do not dispose

// Create thread-safe module - this takes ownership of both module and ts_context
// After this point, neither should be disposed directly
const ts_module = bindings.OrcThreadSafeModule.create(module, ts_context);
```

### Option 2: Use Wrapper Types with Ownership Tracking

Create wrapper types that make ownership explicit:

```zig
const OwnedMemoryBuffer = struct {
    buffer: *bindings.MemoryBuffer,
    owned: bool,

    pub fn consumeInto(self: *OwnedMemoryBuffer) *bindings.MemoryBuffer {
        self.owned = false;
        return self.buffer;
    }

    pub fn deinit(self: *OwnedMemoryBuffer) void {
        if (self.owned) {
            self.buffer.dispose();
        }
    }
};
```

### Option 3: Add Explicit Ownership Comments Block

Add a clear ownership documentation block at the top of the function:

```zig
/// Ownership model for LLVM objects in this function:
///
/// 1. ts_context: Created here → transferred to ts_module → transferred to jit
/// 2. mem_buf: Created here → consumed by parseBitcodeInContext2 (or disposed on error)
/// 3. module: Created by parsing → transferred to ts_module → transferred to jit
/// 4. ts_module: Created here → transferred to jit
/// 5. jit: Created here → disposed in defer block
///
/// Only `jit` needs explicit disposal; everything else is transitively owned by it.
pub fn compileAndExecute(...) ![]const u8 {
```

## Additional Improvements

### Use errdefer for Error Cleanup

Instead of manual cleanup in if statements:

```zig
const mem_buf = bindings.MemoryBuffer.createMemoryBufferWithMemoryRange(...);
errdefer mem_buf.dispose(); // Only runs if function returns error

var module: *bindings.Module = undefined;
const parse_result = context.parseBitcodeInContext2(mem_buf, &module);
if (parse_result.toBool()) {
    return error.BitcodeParseError; // errdefer will dispose mem_buf
}
// Success - mem_buf was consumed, errdefer won't run on success path
// Need to "cancel" errdefer somehow... this is actually tricky
```

Actually, `errdefer` doesn't work well here because we need to NOT dispose on success. The current manual approach is correct, just needs clearer documentation.

## Files to Modify

- `src/llvm_compile/compile.zig` - Improve ownership documentation and code structure

## Testing

The existing snapshot tests exercise this code path. After making changes:
- Run `zig build snapshot` to verify JIT compilation still works
- Add a test that intentionally provides invalid bitcode to verify error path cleanup

## Commit Guidelines

Commit your changes frequently as you make progress. However, **never commit any files in the `planning/` directory** - these planning documents are for reference only and should not be checked into version control.
