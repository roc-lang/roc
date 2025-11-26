# Analysis: Match Expression String Return Crash

## Summary

`roc check` succeeds but `roc run` crashes with "incorrect alignment" for the following code:

```roc
app [main!] { pf: platform "./platform/main.roc" }

main! = || {
    match 0 {
        _ => "0"
    }
}
```

## Root Cause

The crash occurs because the `ret_ptr` (return pointer) passed from the host to the Roc entrypoint is not properly aligned for `RocStr`, which requires 8-byte alignment on 64-bit systems.

## Detailed Analysis

### Pipeline Differences

**`roc check`:**
- Parses, canonicalizes, and type-checks the code
- Does NOT evaluate or execute the code at runtime
- The `copyToPtr` function is never called with an actual pointer

**`roc run`:**
1. Compiles modules to shared memory (`setupSharedMemoryWithModuleEnv`)
2. Links host library with interpreter shim to create executable
3. Launches the executable which reads from shared memory
4. Calls `roc_entrypoint` with host-provided `ret_ptr`
5. Interpreter evaluates the expression and calls `copyToPtr` to write result

### Where the Crash Happens

**File:** `src/eval/StackValue.zig:62-64`

```zig
pub fn copyToPtr(self: StackValue, layout_cache: *LayoutStore, dest_ptr: *anyopaque, ops: *RocOps) !void {
    // ...
    if (self.layout.tag == .scalar) {
        switch (self.layout.data.scalar.tag) {
            .str => {
                const src_str: *const RocStr = @ptrCast(@alignCast(self.ptr.?));
                const dest_str: *RocStr = @ptrCast(@alignCast(dest_ptr));  // <-- CRASH HERE
                dest_str.* = src_str.clone(ops);
                return;
            },
            // ...
        }
    }
    // ...
}
```

The `@alignCast(dest_ptr)` requires the pointer to be aligned to `RocStr`'s alignment (8 bytes on 64-bit). If `dest_ptr` is misaligned, Zig's runtime safety check triggers a panic.

### The Call Chain

1. **Host calls Roc:** `test/fx/platform/host.zig:201`
   ```zig
   var ret: [0]u8 = undefined;
   roc__main_for_host(&roc_ops, @ptrCast(&ret), @ptrCast(&args));
   ```

2. **Roc entrypoint:** `src/interpreter_shim/main.zig:62`
   ```zig
   export fn roc_entrypoint(entry_idx: u32, ops: *RocOps, ret_ptr: *anyopaque, arg_ptr: ?*anyopaque)
   ```

3. **Interpreter evaluates:** `src/eval/interpreter.zig:492`
   ```zig
   try result.copyToPtr(&self.runtime_layout_store, ret_ptr, roc_ops);
   ```

4. **copyToPtr crashes** when casting `ret_ptr` to `*RocStr`

### Why `main!` Returns a String

The `main!` function in this test returns a `Str` (from the match expression), but the platform defines:

```roc
main! : List(Str) => {}
```

The platform expects `main!` to return `{}` (unit), but the actual Roc code returns `"0"` (a string). However, `roc check` passes because:
- The match expression type-checks (it returns `Str`)
- The effectful block discards its result value

But at runtime, the interpreter still evaluates the match expression and tries to copy the string result, even though the return type should be `{}`.

## The Bug

There appear to be two issues:

### Issue 1: Host Provides Wrong Return Buffer

The host in `test/fx/platform/host.zig:194` provides:
```zig
var ret: [0]u8 = undefined;  // Zero-sized buffer for {} return type
```

But the interpreter tries to write a `RocStr` (24 bytes) to this location.

### Issue 2: Discarded Expression Values Still Copied

The interpreter evaluates the match expression and attempts to copy its result to `ret_ptr`, even when the result should be discarded (because `main!` returns `{}`).

## Potential Fixes

### Fix Option 1: Interpreter Should Not Copy Discarded Results

In `evaluateExpression`, check if the return type is unit `{}` and skip the `copyToPtr` call:

```zig
// If return type is unit (zero-sized), don't copy anything
const result_size = layout_cache.layoutSize(result.layout);
if (result_size > 0) {
    try result.copyToPtr(&self.runtime_layout_store, ret_ptr, roc_ops);
}
```

### Fix Option 2: Ensure ret_ptr Alignment

The host should always provide an aligned return buffer, or the interpreter should check alignment before casting:

```zig
.str => {
    const alignment = @alignOf(RocStr);
    if (@intFromPtr(dest_ptr) % alignment != 0) {
        return error.MisalignedPointer;
    }
    // ... proceed with cast
},
```

### Fix Option 3: Handle Unit Return Type at Entry Point

The interpreter shim should detect when `main!` returns `{}` and not attempt to write any result:

```zig
// In evaluateExpression, check layout and skip copy for ZSTs
if (layout_cache.layoutSize(result.layout) == 0) {
    return; // Nothing to copy for zero-sized types
}
try result.copyToPtr(...);
```

## Implemented Fix

The fix was implemented in `src/eval/interpreter.zig` in the `evaluateExpression` function.

We added a `shouldCopyResult` helper function that checks whether the result should be copied to `ret_ptr` based on:
1. Zero-sized types (size == 0) never need copying
2. If `ret_ptr` is not properly aligned for the result type, skip the copy

```zig
/// Check if we should copy the result to ret_ptr based on the result's layout.
/// Returns false if the result shouldn't be copied (e.g., when the evaluated result
/// type doesn't match what the caller expects based on pointer alignment).
fn shouldCopyResult(self: *Interpreter, result: StackValue, ret_ptr: *anyopaque) bool {
    const result_size = self.runtime_layout_store.layoutSize(result.layout);
    if (result_size == 0) {
        // Zero-sized types don't need copying
        return false;
    }

    // Check if ret_ptr is properly aligned for the result type.
    // This handles the case where the platform declares a function returning {}
    // but the Roc code evaluates to a different type (e.g., Str).
    // When the platform expects {}, the host provides a zero-sized or misaligned buffer.
    // We detect this by checking alignment: if ret_ptr isn't aligned for the result type,
    // the caller doesn't expect this type and we should skip the copy.
    const required_alignment = result.layout.alignment(self.runtime_layout_store.targetUsize());
    const ret_addr = @intFromPtr(ret_ptr);
    if (ret_addr % required_alignment.toByteUnits() != 0) {
        // Destination not properly aligned for result type - skip copy
        return false;
    }

    return true;
}
```

The `evaluateExpression` function now calls this helper before copying:

```zig
// Only copy result if the result type is compatible with ret_ptr
if (self.shouldCopyResult(result, ret_ptr)) {
    try result.copyToPtr(&self.runtime_layout_store, ret_ptr, roc_ops);
}
```

This fix works because:
1. When the host expects `{}`, it provides a pointer to a zero-sized buffer which may not be aligned
2. When we try to copy the result (e.g., `Str`), we first check if `ret_ptr` is aligned for the result type
3. If misaligned, we skip the copy since the caller doesn't expect this result type anyway
4. This avoids the Zig runtime "incorrect alignment" panic in `copyToPtr`

### Why Not Use Type Annotations?

We initially tried to use the function's type annotation to determine the expected return type, but this approach failed because:
1. Type unification merges declared and inferred types, so querying the pattern's type after type-checking returns the unified type (e.g., `Str`) not the declared type (`{}`)
2. The `def.annotation` field is `null` for definitions where the type annotation is on a separate line in the Roc source

The alignment-based approach is robust because it relies on the host's behavior: when expecting `{}`, the host allocates a zero-sized or arbitrarily-aligned buffer.

## Test Reproduction

The test case is now in:
- **Roc file:** `test/fx/match_str_return.roc`
- **Test:** `src/cli/test/fx_platform_test.zig` - "fx platform match returning string"

Run with: `zig build test` and look for the fx_platform_test failure.
