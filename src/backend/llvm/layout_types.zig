//! Layout to LLVM Type Conversion
//!
//! This module provides conversion from Roc's Layout system to LLVM types.
//! It bridges the Roc compiler's layout representation with the LLVM IR builder.
//!
//! Key conversions:
//! - Scalars → i8/i16/i32/i64/i128/float/double
//! - Records/Tuples → LLVM struct types
//! - Tag Unions → { payload_bytes, discriminant }
//! - Lists → { ptr, len, capacity }
//! - Boxes → ptr (opaque pointer)
//! - ZST (zero-sized types) → void/i1 placeholder

const std = @import("../../std.zig");
const Allocator = std.mem.Allocator;
const Builder = @import("Builder.zig");
const layout = @import("../../layout/mod.zig");
const types = @import("../../types/types.zig");

const Layout = layout.Layout;
const LayoutTag = layout.LayoutTag;
const Store = layout.Store;
const Idx = layout.Idx;

/// Errors that can occur during layout conversion
pub const Error = error{
    OutOfMemory,
    UnsupportedLayout,
    InvalidLayoutIndex,
};

/// Converts a Roc Layout to an LLVM Builder.Type
pub fn layoutToLlvmType(
    builder: *Builder,
    store: *const Store,
    layout_val: Layout,
) Error!Builder.Type {
    return switch (layout_val.tag) {
        .scalar => scalarToLlvmType(layout_val),
        .box, .box_of_zst => .ptr, // Boxes are just pointers
        .list, .list_of_zst => listLlvmType(builder),
        .record => try recordToLlvmType(builder, store, layout_val),
        .tuple => try tupleToLlvmType(builder, store, layout_val),
        .tag_union => try tagUnionToLlvmType(builder, store, layout_val),
        .closure => .ptr, // Closures are passed as pointers to their environment
        .zst => .i1, // ZST needs some representation; use i1 as minimal placeholder
    };
}

/// Convert a scalar layout to LLVM type
fn scalarToLlvmType(layout_val: Layout) Builder.Type {
    return switch (layout_val.data.scalar.tag) {
        .opaque_ptr => .ptr,
        .str => .ptr, // Str is a pointer to the string data (RocStr)
        .int => intPrecisionToLlvmType(layout_val.data.scalar.data.int),
        .frac => fracPrecisionToLlvmType(layout_val.data.scalar.data.frac),
    };
}

/// Convert integer precision to LLVM type
fn intPrecisionToLlvmType(precision: types.Int.Precision) Builder.Type {
    return switch (precision) {
        .u8, .i8 => .i8,
        .u16, .i16 => .i16,
        .u32, .i32 => .i32,
        .u64, .i64 => .i64,
        .u128, .i128 => .i128,
    };
}

/// Convert fraction precision to LLVM type
fn fracPrecisionToLlvmType(precision: types.Frac.Precision) Builder.Type {
    return switch (precision) {
        .f32 => .float,
        .f64 => .double,
        .dec => .i128, // Dec is stored as i128 (scaled by 10^18)
    };
}

/// Get the LLVM type for a Roc List (3-element struct: ptr, len, capacity)
fn listLlvmType(builder: *Builder) Error!Builder.Type {
    // List layout: { ptr: *T, len: u64, capacity: u64 }
    const fields = [_]Builder.Type{ .ptr, .i64, .i64 };
    return builder.structType(.normal, &fields) catch return error.OutOfMemory;
}

/// Get the LLVM type for a Roc Str (2-element struct: ptr, len)
/// Note: Str also has seamless small string optimization, but the LLVM type
/// is the same (the SSO is handled at runtime)
pub fn strLlvmType(builder: *Builder) Error!Builder.Type {
    const fields = [_]Builder.Type{ .ptr, .i64 };
    return builder.structType(.normal, &fields) catch return error.OutOfMemory;
}

/// Convert a record layout to LLVM struct type
fn recordToLlvmType(
    builder: *Builder,
    store: *const Store,
    layout_val: Layout,
) Error!Builder.Type {
    const record_layout = layout_val.data.record;
    const record_data = store.getRecord(record_layout.idx);

    const fields_range = record_data.getFields();
    if (fields_range.count == 0) {
        // Empty record - return a placeholder struct
        return builder.structType(.normal, &.{}) catch return error.OutOfMemory;
    }

    // Build LLVM types for each field
    var field_types = std.ArrayList(Builder.Type).init(builder.gpa);
    defer field_types.deinit();

    var iter = store.record_fields.iterate(fields_range);
    while (iter.next()) |field| {
        const field_layout = store.get(field.layout);
        const field_llvm_type = try layoutToLlvmType(builder, store, field_layout);
        field_types.append(field_llvm_type) catch return error.OutOfMemory;
    }

    return builder.structType(.normal, field_types.items) catch return error.OutOfMemory;
}

/// Convert a tuple layout to LLVM struct type
fn tupleToLlvmType(
    builder: *Builder,
    store: *const Store,
    layout_val: Layout,
) Error!Builder.Type {
    const tuple_layout = layout_val.data.tuple;
    const tuple_data = store.getTuple(tuple_layout.idx);

    const fields_range = tuple_data.getFields();
    if (fields_range.count == 0) {
        // Empty tuple - return a placeholder struct
        return builder.structType(.normal, &.{}) catch return error.OutOfMemory;
    }

    // Build LLVM types for each element
    var field_types = std.ArrayList(Builder.Type).init(builder.gpa);
    defer field_types.deinit();

    var iter = store.tuple_fields.iterate(fields_range);
    while (iter.next()) |field| {
        const field_layout = store.get(field.layout);
        const field_llvm_type = try layoutToLlvmType(builder, store, field_layout);
        field_types.append(field_llvm_type) catch return error.OutOfMemory;
    }

    return builder.structType(.normal, field_types.items) catch return error.OutOfMemory;
}

/// Convert a tag union layout to LLVM struct type
/// Layout: { payload: [max_size]i8, discriminant: iN }
/// The payload is stored as a byte array sized to the largest variant
fn tagUnionToLlvmType(
    builder: *Builder,
    store: *const Store,
    layout_val: Layout,
) Error!Builder.Type {
    const tu_layout = layout_val.data.tag_union;
    const tu_data = store.getTagUnion(tu_layout.idx);

    // Discriminant type based on size
    const disc_type: Builder.Type = switch (tu_data.discriminant_size) {
        1 => .i8,
        2 => .i16,
        4 => .i32,
        else => .i8, // Fallback
    };

    // Payload size in bytes (total size minus discriminant, accounting for alignment)
    const payload_size = tu_data.discriminant_offset;

    if (payload_size == 0) {
        // No payload, just the discriminant
        const fields = [_]Builder.Type{disc_type};
        return builder.structType(.normal, &fields) catch return error.OutOfMemory;
    }

    // Create array type for payload bytes
    const payload_type = builder.arrayType(payload_size, .i8) catch return error.OutOfMemory;

    // Struct with payload first, then discriminant
    const fields = [_]Builder.Type{ payload_type, disc_type };
    return builder.structType(.normal, &fields) catch return error.OutOfMemory;
}

/// Get the discriminant type for a tag union
pub fn getDiscriminantType(discriminant_size: u8) Builder.Type {
    return switch (discriminant_size) {
        1 => .i8,
        2 => .i16,
        4 => .i32,
        else => .i8,
    };
}

/// Helper for layout conversion from an index
pub fn layoutIdxToLlvmType(
    builder: *Builder,
    store: *const Store,
    idx: Idx,
) Error!Builder.Type {
    const layout_val = store.get(idx);
    return layoutToLlvmType(builder, store, layout_val);
}

/// Check if a layout represents a type that needs to be passed by pointer
/// (too large to pass in registers efficiently)
pub fn shouldPassByPointer(store: *const Store, layout_val: Layout) bool {
    return switch (layout_val.tag) {
        // Scalars are always passed by value
        .scalar => false,
        // ZST needs no actual passing
        .zst => false,
        // Pointers are passed by value
        .box, .box_of_zst => false,
        // Lists and closures are small structs, pass by value
        .list, .list_of_zst, .closure => false,
        // Records/tuples depend on size
        .record => blk: {
            const record_data = store.getRecord(layout_val.data.record.idx);
            // Pass by pointer if larger than 2 pointers (16 bytes on 64-bit)
            break :blk record_data.size > 16;
        },
        .tuple => blk: {
            const tuple_data = store.getTuple(layout_val.data.tuple.idx);
            break :blk tuple_data.size > 16;
        },
        .tag_union => blk: {
            const tu_data = store.getTagUnion(layout_val.data.tag_union.idx);
            break :blk tu_data.size > 16;
        },
    };
}

/// Get the size of a layout in bytes (for structs, this queries the store)
pub fn getLayoutSize(store: *const Store, layout_val: Layout, ptr_size: u32) u32 {
    return switch (layout_val.tag) {
        .scalar => getScalarSize(layout_val),
        .box, .box_of_zst => ptr_size,
        .list, .list_of_zst => ptr_size * 3, // { ptr, len, capacity }
        .closure => ptr_size, // Pointer to environment
        .zst => 0,
        .record => store.getRecord(layout_val.data.record.idx).size,
        .tuple => store.getTuple(layout_val.data.tuple.idx).size,
        .tag_union => store.getTagUnion(layout_val.data.tag_union.idx).size,
    };
}

/// Get the size of a scalar type
fn getScalarSize(layout_val: Layout) u32 {
    return switch (layout_val.data.scalar.tag) {
        .opaque_ptr => 8, // Pointer size (assume 64-bit)
        .str => 16, // { ptr, len }
        .int => switch (layout_val.data.scalar.data.int) {
            .u8, .i8 => 1,
            .u16, .i16 => 2,
            .u32, .i32 => 4,
            .u64, .i64 => 8,
            .u128, .i128 => 16,
        },
        .frac => switch (layout_val.data.scalar.data.frac) {
            .f32 => 4,
            .f64 => 8,
            .dec => 16,
        },
    };
}
