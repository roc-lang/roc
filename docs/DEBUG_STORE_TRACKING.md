# Debug Store Tracking Implementation

## Problem Statement

We have a bug where `findIdent("Builtin.Bool.is_eq")` returns `null` during method lookup. The root cause is that an identifier index (`Ident.Idx`) is being looked up in a different identifier store than the one it was created in.

This is a subtle bug because:
1. Identifier indices are just integers (e.g., `packed struct(u32)`)
2. Looking up an index in the wrong store silently returns wrong data or null
3. There's no runtime check to verify an index belongs to a particular store

## Solution: Debug-Only Store Provenance Tracking

### Overview

Add debug-only tracking that:
1. Gives each store a unique identifier string when it first creates an Idx
2. Tracks all Idx values created by each store in a global map
3. Verifies on every lookup that the Idx was created by that store
4. Has **zero runtime cost in release builds** (all tracking code is gated by `if (enable_store_tracking)`)

### Actual Implementation

We couldn't embed tracking info directly in the Idx struct because:
- Idx is a `packed struct(u32)` used in extern unions
- Changing it to a non-packed struct breaks `@bitCast` operations
- It's used in equality comparisons that would break with extra fields

Instead, we use a **global debug-only map** from store addresses to tracking info:

```zig
const enable_store_tracking = builtin.mode == .Debug;

/// Debug-only info for store provenance tracking.
const StoreDebugInfo = struct {
    store_id: []const u8,
    known_idxs: std.AutoHashMapUnmanaged(u32, void),
};

/// Global map from Store address to debug info.
/// Protected by a mutex for thread safety.
var debug_store_map: if (enable_store_tracking) std.AutoHashMapUnmanaged(usize, StoreDebugInfo) else void = ...;
var debug_store_mutex: if (enable_store_tracking) std.Thread.Mutex else void = ...;
```

### Key Functions

#### trackIdx (called on insert/genUnique)
```zig
fn trackIdx(self: *Store, idx: Idx, src: std.builtin.SourceLocation) void {
    if (enable_store_tracking) {
        debug_store_mutex.lock();
        defer debug_store_mutex.unlock();

        // Auto-register store on first use, using @src() for unique ID
        const info = debug_store_map.getPtr(@intFromPtr(self)) orelse {
            // Register with ID like "Ident.zig:272:31"
            ...
        };
        info.known_idxs.put(..., @bitCast(idx), {});
    }
}
```

#### verifyIdx (called on getText)
```zig
fn verifyIdx(self: *const Store, idx: Idx) void {
    if (enable_store_tracking) {
        debug_store_mutex.lock();
        defer debug_store_mutex.unlock();

        const info = debug_store_map.get(@intFromPtr(self)) orelse return; // Skip deserialized stores

        if (!info.known_idxs.contains(@bitCast(idx))) {
            std.debug.panic("Ident.Idx lookup in wrong store: Idx {d} not found in store '{s}'", ...);
        }
    }
}
```

### Design Decisions

1. **Global map instead of per-Idx storage**: Keeps Idx at 4 bytes, maintains compatibility with extern unions and bitcasts.

2. **Mutex for thread safety**: The snapshot tool runs in parallel, so the global map needs protection.

3. **Auto-registration on first use**: Stores get registered when they first create an Idx, using `@src()` to generate a unique ID from the call site.

4. **Skip verification for deserialized stores**: Stores loaded from cache don't have tracking info, so verification is skipped for them.

5. **Store ID from source location**: Using `@src()` gives us file:line:column IDs like "Ident.zig:272:31", which helps identify which code path created the store.

## Current Status

### Issue 1: Ident.Idx lookup in wrong store - FIXED

**Problem**: Malformed type headers were being created with `invalid_ident` (Idx 0) which wasn't tracked in the ident store, causing panics when looked up.

**Fix**: Changed `canonicalizeTypeHeader` in `Can.zig` to use `pushMalformed` instead of creating TypeHeaders with invalid idents.

### Issue 2: e_closure: failed to resolve capture value - FIXED

**Problem**: Match branch patterns like `Ok(val) => val` were incorrectly generating closures with `val` as a capture, even though `val` was bound by the match pattern itself. This caused `Try.ok_or` and `Try.err_or` in Builtin to fail.

**Fix**: Added logic in `Can.zig` match expression canonicalization to filter pattern-bound variables from branch free_vars before propagating them up. Variables bound by a match pattern are no longer counted as free variables that become captures.

### Issue 3: e_tag: expected tag_union structure type - FIXED

**Problem**: When evaluating the `True` tag in closure bodies (e.g., `Str.contains` returns `True`), the type variable is `flex` instead of `tag_union`. The closure body was being evaluated without the expected return type, so the type couldn't be resolved.

**Fix**: Two changes in `src/eval/interpreter.zig`:
1. When evaluating closure/lambda bodies in `e_call`, pass `call_ret_rt_var` (the expected return type from the call expression) instead of `null`
2. In `e_tag` and `e_zero_argument_tag` handlers, if the type resolves to `flex` and the tag name is `True` or `False`, fall back to looking up the canonical Bool type using `getCanonicalBoolRuntimeVar()`

## Files Modified

1. `src/base/Ident.zig` - Added debug store tracking to IdentStore
2. `src/eval/interpreter.zig` - Fixed e_tag/e_zero_argument_tag to fall back to Bool for True/False tags with flex type; pass expected return type when evaluating closure bodies
3. `src/eval/test_runner.zig` - Removed NotImplemented case from error handling
4. `src/canonicalize/ModuleEnv.zig` - Adjusted size assertion to skip in debug builds
5. `src/canonicalize/Can.zig` - Fixed malformed type headers to use pushMalformed; filter pattern-bound variables from match branch free_vars

## Zero-Cost in Release Builds

All tracking code is gated by:
```zig
const enable_store_tracking = builtin.mode == .Debug;

if (enable_store_tracking) {
    // tracking code
}
```

In release builds:
- `enable_store_tracking` is `false` (comptime constant)
- All tracking code is eliminated by the compiler
- Global map and mutex are `void` types (zero size)
- Store struct size is identical to before
