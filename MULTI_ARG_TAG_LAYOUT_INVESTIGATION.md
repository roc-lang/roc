# Multi-Arg Tag Layout Mismatch Investigation

## Executive Summary

When creating tag unions with multiple payload arguments (e.g., `Pair(x, y)` where both `x` and `y` are lists), there's a layout mismatch between what the type system expects and what's actually stored. This causes memory corruption during cleanup (Signal 6 / SIGABRT). Attempts to fix this by correcting the layout cause cascading failures in unrelated tests (nested list tests), suggesting deeper issues with shared state in the layout store.

---

## The Problem in Detail

### What Works: Single-Arg Tags

The test `Some([1, 2])` works correctly after fixes were applied. Here's what happens:

1. **Creation:** `Some([1, 2])` creates a tag union
2. **Type System Says:** Payload is `List(generic_num)` or similar
3. **Actual Value:** Payload is `List(Dec)` because numeric literals default to Dec
4. **The Fix:** When layouts differ, we create a NEW outer tuple with the correct layout:
   ```zig
   var elem_layouts_fixed = [2]Layout{ arg_val.layout, tag_field.layout };
   const proper_tuple_idx = try self.runtime_layout_store.putTuple(&elem_layouts_fixed);
   // ... use this new layout
   ```

### What Doesn't Work: Multi-Arg Tags

The test `Pair(x, y)` where `x = [1, 2]` and `y = [3, 4]` crashes during cleanup:

1. **Creation:** `Pair(x, y)` creates a tag union with TWO list payloads
2. **Type System Says:** Payload tuple is `(List(generic_num), List(generic_num))`
3. **Actual Values:** Both lists are `List(Dec)`
4. **Current Code:** Uses type system layout, stores actual values
5. **Cleanup:** When decref iterates the outer tuple, it uses the wrong element layouts to interpret the payload, causing memory corruption

### The Crash Location

The crash happens in `StackValue.decref` when it tries to recursively decref tuple elements. The tuple's stored layout says element 0 has layout X, but the actual bytes stored there were written assuming layout Y. When the decref code interprets those bytes with layout X, it reads garbage pointers and crashes.

---

## Attempted Fixes and Their Failures

### Attempt 1: Use `val.layout` Instead of `getRuntimeLayout`

**Change Made:**
```zig
// Before:
elem_layouts[j] = try self.getRuntimeLayout(arg_rt_var);

// After:
elem_layouts[j] = val.layout;
```

**Result:** 432/436 tests pass (4 failures), crash moves to "three levels" nested list test

**Location:** `src/eval/interpreter.zig`, lines ~1534-1541

### Attempt 2: Create New Outer Tuple with Corrected Layout

**Change Made:**
```zig
// After storing values, create corrected outer tuple
var outer_elem_layouts = [2]Layout{ tuple_layout, tag_field.layout };
const proper_outer_tuple_idx = try self.runtime_layout_store.putTuple(&outer_elem_layouts);
const proper_outer_tuple_layout = self.runtime_layout_store.getLayout(proper_outer_tuple_idx);
var proper_dest = try self.pushRaw(proper_outer_tuple_layout, 0);
// ... copy data to proper_dest, return proper_dest
```

**Result:** Same 432/436 failures, same crash in "three levels"

### Attempt 3: Just Update dest.layout

**Change Made:**
```zig
// Keep using dest, but update its layout
var outer_elem_layouts = [2]Layout{ tuple_layout, tag_field.layout };
const corrected_outer_idx = try self.runtime_layout_store.putTuple(&outer_elem_layouts);
dest.layout = self.runtime_layout_store.getLayout(corrected_outer_idx);
return dest;
```

**Result:** Still crashes, though in a different place

---

## The Mysterious Behavior

### Why Do Nested List Tests Fail?

The "three levels" test that fails looks like this:
```roc
{
    a = [1]
    b = [a]
    c = [b]
    match c { [lst] => match lst { [lst2] => match lst2 { [x] => x, _ => 0 }, _ => 0 }, _ => 0 }
}
```

**This test has NO tags.** It's purely nested lists. Yet it only fails when we modify the multi-arg tag code in `e_tag`.

### The Connection: Layout Store Shared State

The layout store (`runtime_layout_store`) is shared across all evaluations. When we call `putTuple` to create a new layout, this modifies the layout store's internal state:

```zig
// From RuntimeLayoutStore
pub fn putTuple(self: *RuntimeLayoutStore, elem_layouts: []const Layout) !LayoutIdx {
    // This modifies internal ArrayLists and HashMaps
    // Creates new entries that persist across evaluations
}
```

**Hypothesis:** When we create new tuple layouts in the multi-arg tag code, we're either:
1. Creating layouts that collide with existing layouts due to deduplication logic
2. Changing indices that other code depends on
3. Corrupting some internal invariant in the layout store

### Evidence for Layout Store Corruption

1. **Skipping the problematic test makes things WORSE**: When I commented out the "tag with multiple list payloads" test body, we went from 436/436 (with cleanup crash) to 432/436 (with different crash). This suggests the test's execution somehow "fixes" state for later tests.

2. **The failing tests are unrelated**: The nested list tests don't use tags at all, yet they're affected by tag code changes.

3. **The crash location changes**: Depending on the fix attempted, the crash moves between different tests and locations.

---

## Code Locations for Reference

### Layout Store Definition
- File: `src/types/layout.zig` (likely location based on imports)
- The `RuntimeLayoutStore` struct manages tuple and record layouts

### Multi-Arg Tag Creation (Tuple Branch)
- File: `src/eval/interpreter.zig`
- Lines: ~1522-1555 (the `else` branch for `args_exprs.len > 1`)

```zig
} else {
    const arg_count = args_exprs.len;
    var elem_layouts = try self.allocator.alloc(Layout, arg_count);
    defer self.allocator.free(elem_layouts);
    var elem_values = try self.allocator.alloc(StackValue, arg_count);
    defer {
        for (elem_values[0..arg_count]) |val| {
            val.decref(&self.runtime_layout_store, roc_ops);
        }
        self.allocator.free(elem_values);
    }

    var j: usize = 0;
    while (j < arg_count) : (j += 1) {
        const arg_rt_var = arg_rt_vars[j];
        const val = try self.evalExprMinimal(args_exprs[j], roc_ops, arg_rt_var);
        elem_values[j] = val;
        elem_layouts[j] = try self.getRuntimeLayout(arg_rt_var);  // <-- TYPE SYSTEM LAYOUT
    }

    const tuple_layout_idx = try self.runtime_layout_store.putTuple(elem_layouts);
    const tuple_layout = self.runtime_layout_store.getLayout(tuple_layout_idx);

    if (payload_field.ptr) |payload_ptr| {
        var tuple_dest = StackValue{ .layout = tuple_layout, .ptr = payload_ptr, .is_initialized = true };
        var tup_acc = try tuple_dest.asTuple(&self.runtime_layout_store);
        j = 0;
        while (j < elem_values.len) : (j += 1) {
            try tup_acc.setElement(j, elem_values[j], roc_ops);  // <-- ACTUAL VALUES
        }
    }

    return dest;  // <-- dest has ORIGINAL layout, not tuple_layout!
}
```

### Multi-Arg Tag Creation (Record Branch)
- File: `src/eval/interpreter.zig`
- Lines: ~1411-1444
- Same pattern as tuple branch, also has the mismatch

### StackValue.decref (Tuple Case)
- File: `src/eval/StackValue.zig`
- Lines: ~1054-1079

```zig
.tuple => {
    if (self.ptr == null) return;
    const tuple_data = layout_cache.getTupleData(self.layout.data.tuple.idx);
    if (tuple_data.fields.count == 0) return;

    const element_layouts = layout_cache.tuple_fields.sliceRange(tuple_data.getFields());
    const base_ptr = @as([*]u8, @ptrCast(self.ptr.?));

    var elem_index: usize = 0;
    while (elem_index < element_layouts.len) : (elem_index += 1) {
        const elem_info = element_layouts.get(elem_index);
        const elem_layout = layout_cache.getLayout(elem_info.layout);  // <-- USES STORED LAYOUT

        const elem_offset = layout_cache.getTupleElementOffset(self.layout.data.tuple.idx, @intCast(elem_index));
        const elem_ptr = @as(*anyopaque, @ptrCast(base_ptr + elem_offset));

        const elem_value = StackValue{
            .layout = elem_layout,  // <-- IF THIS IS WRONG, CORRUPTION HAPPENS
            .ptr = elem_ptr,
            .is_initialized = true,
        };

        elem_value.decref(layout_cache, ops);  // <-- RECURSIVE DECREF WITH WRONG LAYOUT
    }
}
```

---

## Ideas for Fixing

### Option 1: Fix the Root Cause - Ensure Type System and Values Match

Instead of fixing layouts after creation, ensure the type system produces the correct layout from the start. This might involve:
- Tracking "runtime defaulted" types through the type system
- Having the type checker record that numeric literals become Dec
- Propagating concrete types earlier in the pipeline

**Pros:** Clean fix, no runtime patching
**Cons:** Requires understanding and modifying type inference

### Option 2: Store Layout WITH the Data

Instead of relying on the outer tuple's layout to know how to decref the payload, store the actual layout alongside the data:

```zig
// Instead of just storing bytes, store (layout_idx, bytes)
// Or use a "fat pointer" that includes layout info
```

**Pros:** Self-describing data, no mismatch possible
**Cons:** Memory overhead, significant refactoring

### Option 3: Isolate Layout Store Per-Test (for debugging)

To verify the shared state hypothesis, try creating a fresh `RuntimeLayoutStore` for each test. If the failures disappear, that confirms the issue.

**Implementation:**
```zig
// In test setup
var fresh_layout_store = RuntimeLayoutStore.init(allocator);
defer fresh_layout_store.deinit();
// Pass to interpreter
```

### Option 4: Deep Copy Layouts Instead of Sharing Indices

When creating a new tuple layout for multi-arg tags, instead of just getting a new index in the shared store, create a completely independent layout that can't interfere with others.

**Pros:** Isolation
**Cons:** Memory usage, complexity

### Option 5: Track Layout "Versions" or "Generations"

Add versioning to the layout store so that layouts created during one evaluation don't affect layouts from another. This is like a generational garbage collector approach.

---

## Reproduction Steps

1. Run `zig build test-eval`
2. Observe: 436/436 pass, then Signal 6 crash during cleanup
3. The crash happens after "tag with multiple list payloads" test

To see the cascading failure behavior:
1. In `src/eval/interpreter.zig`, find the multi-arg tag code (~line 1538)
2. Change `elem_layouts[j] = try self.getRuntimeLayout(arg_rt_var);` to `elem_layouts[j] = val.layout;`
3. Run tests again
4. Observe: 432/436 pass, crash now in "three levels" test

---

## Key Questions to Investigate

1. **Why does `putTuple` affect unrelated tests?** Is there deduplication that causes collisions?

2. **What invariants does the layout store maintain?** Are we violating any?

3. **Is the layout store meant to be modified after initialization?** Maybe it's designed to be immutable after type checking completes.

4. **How do tuple layout indices work?** When we create a new tuple layout, what happens to existing references to "similar" tuples?

5. **Is there caching based on layout equality?** If so, our "corrected" layouts might be getting deduplicated back to the original wrong layouts.

---

## Files to Examine

1. `src/types/layout.zig` - Layout store implementation
2. `src/eval/interpreter.zig` - Multi-arg tag creation (lines ~1411-1444 and ~1522-1555)
3. `src/eval/StackValue.zig` - `decref` implementation (lines ~940-1080)
4. `src/eval/StackValue.zig` - `copyToPtr` implementation (lines ~43-192)

---

## Test Files for Reference

- `src/eval/test/list_refcount_containers.zig` - Contains "tag with multiple list payloads" (line 131)
- `src/eval/test/list_refcount_nested.zig` - Contains "three levels" (line 57)
