# Add Reference Counting Infrastructure

## Problem

The Zig LLVM backend has no integration with Roc's reference counting system. This is acceptable for the current numeric-only scope, but becomes critical when adding support for heap-allocated types like Str, List, and boxed values.

## Context

Roc uses automatic reference counting (ARC) for memory management. Every heap-allocated value has a hidden reference count stored at a negative offset from the data pointer. When values are shared, the refcount is incremented; when they go out of scope, it's decremented; when it reaches zero, the memory is freed.

## How the Rust Backend Handles This

The Rust LLVM backend has a dedicated module: `crates/compiler/gen_llvm/src/llvm/refcounting.rs`

Key abstractions:

### PointerToRefcount

```rust
pub struct PointerToRefcount<'ctx> {
    value: PointerValue<'ctx>,
}

impl<'ctx> PointerToRefcount<'ctx> {
    /// The refcount is stored at offset -1 from the data pointer
    pub fn from_ptr_to_data(env: &Env<'_, 'ctx, '_>, data_ptr: PointerValue<'ctx>) -> Self {
        // GEP with index -1 to get refcount location
    }

    pub fn get_refcount(&self, env: &Env<'_, 'ctx, '_>) -> IntValue<'ctx> {
        // Load the refcount value
    }

    pub fn set_refcount(&self, env: &Env<'_, 'ctx, '_>, refcount: IntValue<'ctx>) {
        // Store the refcount value
    }

    pub fn is_1(&self, env: &Env<'_, 'ctx, '_>) -> IntValue<'ctx> {
        // Check if refcount == 1 (for unique check)
    }
}
```

### Increment/Decrement Operations

```rust
pub fn increment_refcount_layout<'a, 'ctx>(
    env: &Env<'a, 'ctx, '_>,
    layout_interner: &STLayoutInterner<'a>,
    layout_ids: &mut LayoutIds<'a>,
    value: BasicValueEnum<'ctx>,
    layout: InLayout<'a>,
) {
    // Recursively increment refcounts for nested heap values
}

pub fn decrement_refcount_layout<'a, 'ctx>(
    env: &Env<'a, 'ctx, '_>,
    layout_interner: &STLayoutInterner<'a>,
    layout_ids: &mut LayoutIds<'a>,
    value: BasicValueEnum<'ctx>,
    layout: InLayout<'a>,
) {
    // Recursively decrement refcounts
    // If refcount reaches 0, recursively decrement children then free
}
```

### Allocation with Refcount

```rust
pub fn allocate_with_refcount<'a, 'ctx>(
    env: &Env<'a, 'ctx, '_>,
    layout_interner: &STLayoutInterner<'a>,
    layout: InLayout<'a>,
    data: BasicValueEnum<'ctx>,
) -> PointerValue<'ctx> {
    // 1. Calculate size = refcount_size + data_size
    // 2. Call roc_alloc(size, alignment)
    // 3. Store initial refcount (1) at beginning
    // 4. Return pointer to data (after refcount)
}
```

## Memory Layout

For a heap-allocated value:

```
┌──────────────┬────────────────────────┐
│   refcount   │         data           │
│   (usize)    │                        │
└──────────────┴────────────────────────┘
^               ^
│               └── Pointer returned to Roc code
└── Actual allocation start (refcount location)
```

The refcount is at offset `-@sizeOf(usize)` from the data pointer.

## What Needs to Be Implemented

### Phase 1: Basic Infrastructure

Create `src/llvm_compile/refcounting.zig` (or add to existing module) with:

1. **Refcount pointer calculation**
   ```zig
   fn refcountPtrFromDataPtr(builder: *LlvmBuilder, data_ptr: Value) Value {
       // GEP with -1 offset (adjusted for refcount size)
   }
   ```

2. **Refcount load/store**
   ```zig
   fn loadRefcount(builder: *LlvmBuilder, refcount_ptr: Value) Value
   fn storeRefcount(builder: *LlvmBuilder, refcount_ptr: Value, value: Value) void
   ```

3. **Refcount check**
   ```zig
   fn isRefcountOne(builder: *LlvmBuilder, refcount_ptr: Value) Value // Returns i1
   ```

### Phase 2: Increment/Decrement

```zig
fn incrementRefcount(builder: *LlvmBuilder, data_ptr: Value) void {
    const rc_ptr = refcountPtrFromDataPtr(builder, data_ptr);
    const current = loadRefcount(builder, rc_ptr);
    const incremented = builder.add(current, 1);
    storeRefcount(builder, rc_ptr, incremented);
}

fn decrementRefcount(builder: *LlvmBuilder, data_ptr: Value, layout: Layout, on_zero: fn() void) void {
    const rc_ptr = refcountPtrFromDataPtr(builder, data_ptr);
    const current = loadRefcount(builder, rc_ptr);
    const decremented = builder.sub(current, 1);
    storeRefcount(builder, rc_ptr, decremented);

    // If decremented == 0, call on_zero (which frees memory)
    // Need to generate conditional branch
}
```

### Phase 3: Allocation

```zig
fn allocateWithRefcount(builder: *LlvmBuilder, data_size: usize, alignment: usize) Value {
    const refcount_size = @sizeOf(usize);
    const total_size = refcount_size + data_size;

    // Call roc_alloc
    const alloc_ptr = builder.call("roc_alloc", .{total_size, alignment});

    // Store initial refcount of 1
    storeRefcount(builder, alloc_ptr, 1);

    // Return pointer to data (after refcount)
    return builder.gep(alloc_ptr, refcount_size);
}
```

### Phase 4: Layout-Aware Operations

For compound types (records with heap fields, lists of lists, etc.), refcount operations need to be recursive:

```zig
fn incrementRefcountLayout(builder: *LlvmBuilder, value: Value, layout: Layout) void {
    switch (layout) {
        .Str, .List => incrementRefcount(builder, value),
        .Struct => |fields| {
            for (fields) |field| {
                if (field.isHeapAllocated()) {
                    const field_ptr = extractField(builder, value, field.index);
                    incrementRefcountLayout(builder, field_ptr, field.layout);
                }
            }
        },
        // ... other cases
    }
}
```

## External Functions Needed

The refcounting system needs to call these external functions (provided by the host):

- `roc_alloc(size: usize, alignment: usize) -> *anyopaque`
- `roc_dealloc(ptr: *anyopaque, alignment: usize)`
- `roc_realloc(ptr: *anyopaque, new_size: usize, old_size: usize, alignment: usize) -> *anyopaque`

These should be declared as external functions in the LLVM module.

## Files to Create/Modify

- Create `src/llvm_compile/refcounting.zig` - New module for refcounting
- Modify `src/llvm_compile/mod.zig` - Export the new module
- Modify `src/eval/llvm_evaluator.zig` - Use refcounting when emitting Str/List values

## When This Becomes Critical

This infrastructure is needed before implementing:
- String literals (Str type)
- List literals ([a, b, c])
- Record values with heap fields
- Tag unions with heap payloads
- Let bindings that share values
- Function calls that pass heap values

## Testing

Once basic Str support is added:
- `"hello"` should allocate with refcount 1
- Passing a string to a function should increment refcount
- String going out of scope should decrement and free

The dual-mode snapshot tests will help verify correctness by comparing LLVM output with interpreter output.

## Commit Guidelines

Commit your changes frequently as you make progress. However, **never commit any files in the `planning/` directory** - these planning documents are for reference only and should not be checked into version control.
