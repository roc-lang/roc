//! Reference Counting Code Generation
//!
//! This module generates LLVM IR for reference counting operations.
//! It works with the layout system to determine which types need
//! refcounting and generates appropriate calls to the Roc builtins.
//!
//! ## Refcounting Protocol
//!
//! Roc uses a standard refcount protocol:
//! - Refcount stored at `data_ptr - sizeof(isize)` (before the data)
//! - Value 0 = static/constant data (never freed)
//! - Value 1 = unique ownership (can mutate in place)
//! - Value >1 = shared ownership (must copy-on-write)
//!
//! ## Types That Need Refcounting
//!
//! - Str: RocStr with heap-allocated bytes
//! - List: RocList with heap-allocated elements
//! - Box: Heap-allocated boxed values
//! - Records/Tuples: Only if they contain refcounted fields
//! - Tag Unions: Based on active variant's payload
//!
//! ## Implementation Notes
//!
//! Refcount operations are generated as calls to builtins, not inline code.
//! This keeps the generated code smaller and leverages tested implementations.

const std = @import("std");
const Allocator = std.mem.Allocator;
const Builder = @import("Builder.zig");
const emit = @import("emit.zig");
const builtins = @import("builtins.zig");
const layout_mod = @import("../../layout/mod.zig");
const layout_types = @import("layout_types.zig");

const Layout = layout_mod.Layout;
const Store = layout_mod.Store;

/// Errors from refcount operations
pub const Error = error{
    OutOfMemory,
    NoActiveFunction,
    UnsupportedLayout,
};

/// Context for refcount code generation
pub const RefcountContext = struct {
    allocator: Allocator,
    builtin_ctx: *builtins.BuiltinContext,
    /// Pointer to RocOps struct (needed for all builtin calls)
    roc_ops_ptr: ?Builder.Value,

    pub fn init(allocator: Allocator, builtin_ctx: *builtins.BuiltinContext) RefcountContext {
        return .{
            .allocator = allocator,
            .builtin_ctx = builtin_ctx,
            .roc_ops_ptr = null,
        };
    }

    /// Set the RocOps pointer for builtin calls
    pub fn setRocOps(self: *RefcountContext, roc_ops: Builder.Value) void {
        self.roc_ops_ptr = roc_ops;
    }
};

/// Generate code to increment the refcount of a value.
///
/// Parameters:
/// - ctx: Refcount context with RocOps pointer
/// - emitter: LLVM emitter for generating instructions
/// - store: Layout store for looking up nested layouts
/// - value: The LLVM value to incref (could be a pointer or struct)
/// - layout_val: The Roc layout of the value
/// - amount: How much to increment by (usually 1)
pub fn emitIncref(
    ctx: *RefcountContext,
    emitter: *emit.LlvmEmitter,
    store: *const Store,
    value: Builder.Value,
    layout_val: Layout,
    amount: i64,
) Error!void {
    const roc_ops = ctx.roc_ops_ptr orelse return error.NoActiveFunction;

    switch (layout_val.tag) {
        .scalar => {
            // Only Str scalars need refcounting
            if (layout_val.data.scalar.tag == .str) {
                try emitStrIncref(ctx, emitter, value, amount, roc_ops);
            }
            // Other scalars (int, frac, etc.) don't need refcounting
        },

        .list, .list_of_zst => {
            // Lists need refcounting
            const elem_layout = if (layout_val.tag == .list) store.get(layout_val.data.list) else Layout.zst();
            const elements_refcounted = elem_layout.isRefcounted();
            try emitListIncref(ctx, emitter, value, amount, elements_refcounted, roc_ops);
        },

        .box => {
            // Boxes need refcounting
            try emitBoxIncref(ctx, emitter, value, amount, roc_ops);
        },

        .box_of_zst => {
            // Box of ZST still needs refcount for the allocation itself
            try emitBoxIncref(ctx, emitter, value, amount, roc_ops);
        },

        .record => {
            // Records: incref each refcounted field
            const record_layout = layout_val.data.record;
            const record_data = store.getRecord(record_layout.idx);
            try emitRecordIncref(ctx, emitter, store, value, record_data, amount);
        },

        .tuple => {
            // Tuples: same as records
            const tuple_layout = layout_val.data.tuple;
            const tuple_data = store.getTuple(tuple_layout.idx);
            try emitTupleIncref(ctx, emitter, store, value, tuple_data, amount);
        },

        .tag_union => {
            // Tag unions: need to check discriminant and incref active variant
            // This is complex because we need to dispatch based on runtime tag
            // For now, we'll skip this - it requires control flow generation
            // TODO: Implement tag union refcounting
        },

        .closure => {
            // Closures: incref the captured environment
            const captures_idx = layout_val.data.closure.captures_layout_idx;
            const captures_layout = store.get(captures_idx);
            if (captures_layout.isRefcounted()) {
                try emitIncref(ctx, emitter, store, value, captures_layout, amount);
            }
        },

        .zst => {
            // Zero-sized types don't need refcounting
        },
    }
}

/// Generate code to decrement the refcount of a value.
///
/// Decref may trigger deallocation if the refcount reaches zero.
pub fn emitDecref(
    ctx: *RefcountContext,
    emitter: *emit.LlvmEmitter,
    store: *const Store,
    value: Builder.Value,
    layout_val: Layout,
) Error!void {
    const roc_ops = ctx.roc_ops_ptr orelse return error.NoActiveFunction;

    switch (layout_val.tag) {
        .scalar => {
            if (layout_val.data.scalar.tag == .str) {
                try emitStrDecref(ctx, emitter, value, roc_ops);
            }
        },

        .list, .list_of_zst => {
            const elem_layout = if (layout_val.tag == .list) store.get(layout_val.data.list) else Layout.zst();
            const elements_refcounted = elem_layout.isRefcounted();
            const elem_size = layout_types.getLayoutSize(store, elem_layout, 8);
            const elem_align = @as(u32, 1) << @intFromEnum(elem_layout.alignment(.@"8"));
            try emitListDecref(ctx, emitter, value, elem_size, elem_align, elements_refcounted, roc_ops);
        },

        .box => {
            const elem_idx = layout_val.data.box;
            const elem_layout = store.get(elem_idx);
            const elements_refcounted = elem_layout.isRefcounted();
            const elem_align = @as(u32, 1) << @intFromEnum(elem_layout.alignment(.@"8"));
            try emitBoxDecref(ctx, emitter, value, elem_align, elements_refcounted, roc_ops);
        },

        .box_of_zst => {
            // Box of ZST: alignment is 1, not refcounted
            try emitBoxDecref(ctx, emitter, value, 1, false, roc_ops);
        },

        .record => {
            const record_layout = layout_val.data.record;
            const record_data = store.getRecord(record_layout.idx);
            try emitRecordDecref(ctx, emitter, store, value, record_data);
        },

        .tuple => {
            const tuple_layout = layout_val.data.tuple;
            const tuple_data = store.getTuple(tuple_layout.idx);
            try emitTupleDecref(ctx, emitter, store, value, tuple_data);
        },

        .tag_union => {
            // TODO: Implement tag union decref (requires control flow)
        },

        .closure => {
            const captures_idx = layout_val.data.closure.captures_layout_idx;
            const captures_layout = store.get(captures_idx);
            if (captures_layout.isRefcounted()) {
                try emitDecref(ctx, emitter, store, value, captures_layout);
            }
        },

        .zst => {},
    }
}

// Type-specific refcount helpers

fn emitStrIncref(
    ctx: *RefcountContext,
    emitter: *emit.LlvmEmitter,
    str_value: Builder.Value,
    amount: i64,
    roc_ops: Builder.Value,
) Error!void {
    // Call: roc_builtins_str_incref(str_ptr, amount, roc_ops)
    // Note: str_value is a pointer to the RocStr struct
    const amount_val = emitter.emitIntConst(.i64, amount) catch return error.OutOfMemory;

    _ = builtins.emitBuiltinCall(
        ctx.builtin_ctx,
        emitter,
        "roc_builtins_str_incref",
        .void,
        &.{ .ptr, .i64, .ptr },
        &.{ str_value, amount_val, roc_ops },
    ) catch return error.OutOfMemory;
}

fn emitStrDecref(
    ctx: *RefcountContext,
    emitter: *emit.LlvmEmitter,
    str_value: Builder.Value,
    roc_ops: Builder.Value,
) Error!void {
    _ = builtins.emitBuiltinCall(
        ctx.builtin_ctx,
        emitter,
        "roc_builtins_str_decref",
        .void,
        &.{ .ptr, .ptr },
        &.{ str_value, roc_ops },
    ) catch return error.OutOfMemory;
}

fn emitListIncref(
    ctx: *RefcountContext,
    emitter: *emit.LlvmEmitter,
    list_value: Builder.Value,
    amount: i64,
    elements_refcounted: bool,
    roc_ops: Builder.Value,
) Error!void {
    const amount_val = emitter.emitIntConst(.i64, amount) catch return error.OutOfMemory;
    const elems_rc_val = emitter.emitBoolConst(elements_refcounted) catch return error.OutOfMemory;

    _ = builtins.emitBuiltinCall(
        ctx.builtin_ctx,
        emitter,
        "roc_builtins_list_incref",
        .void,
        &.{ .ptr, .i64, .i1, .ptr },
        &.{ list_value, amount_val, elems_rc_val, roc_ops },
    ) catch return error.OutOfMemory;
}

fn emitListDecref(
    ctx: *RefcountContext,
    emitter: *emit.LlvmEmitter,
    list_value: Builder.Value,
    elem_size: u32,
    elem_align: u32,
    elements_refcounted: bool,
    roc_ops: Builder.Value,
) Error!void {
    const size_val = emitter.emitIntConst(.i64, elem_size) catch return error.OutOfMemory;
    const align_val = emitter.emitIntConst(.i32, elem_align) catch return error.OutOfMemory;
    const elems_rc_val = emitter.emitBoolConst(elements_refcounted) catch return error.OutOfMemory;

    _ = builtins.emitBuiltinCall(
        ctx.builtin_ctx,
        emitter,
        "roc_builtins_list_decref",
        .void,
        &.{ .ptr, .i64, .i32, .i1, .ptr },
        &.{ list_value, size_val, align_val, elems_rc_val, roc_ops },
    ) catch return error.OutOfMemory;
}

fn emitBoxIncref(
    ctx: *RefcountContext,
    emitter: *emit.LlvmEmitter,
    box_ptr: Builder.Value,
    amount: i64,
    roc_ops: Builder.Value,
) Error!void {
    // Use increfDataPtrC for boxes
    const amount_val = emitter.emitIntConst(.i64, amount) catch return error.OutOfMemory;

    _ = builtins.emitBuiltinCall(
        ctx.builtin_ctx,
        emitter,
        builtins.BuiltinNames.incref_rc_ptr,
        .void,
        &.{ .ptr, .i64, .ptr },
        &.{ box_ptr, amount_val, roc_ops },
    ) catch return error.OutOfMemory;
}

fn emitBoxDecref(
    ctx: *RefcountContext,
    emitter: *emit.LlvmEmitter,
    box_ptr: Builder.Value,
    alignment: u32,
    elements_refcounted: bool,
    roc_ops: Builder.Value,
) Error!void {
    const align_val = emitter.emitIntConst(.i32, alignment) catch return error.OutOfMemory;
    const elems_rc_val = emitter.emitBoolConst(elements_refcounted) catch return error.OutOfMemory;

    _ = builtins.emitBuiltinCall(
        ctx.builtin_ctx,
        emitter,
        builtins.BuiltinNames.decref_rc_ptr,
        .void,
        &.{ .ptr, .i32, .i1, .ptr },
        &.{ box_ptr, align_val, elems_rc_val, roc_ops },
    ) catch return error.OutOfMemory;
}

fn emitRecordIncref(
    ctx: *RefcountContext,
    emitter: *emit.LlvmEmitter,
    store: *const Store,
    record_ptr: Builder.Value,
    record_data: layout_mod.RecordData,
    amount: i64,
) Error!void {
    // Iterate through fields and incref any refcounted ones
    const fields_range = record_data.getFields();
    var field_idx: u32 = 0;
    var iter = store.record_fields.iterate(fields_range);
    while (iter.next()) |field| {
        const field_layout = store.get(field.layout);
        if (field_layout.isRefcounted()) {
            // Get pointer to this field
            const record_type = layout_types.layoutToLlvmType(emitter.builder, store, Layout.record(
                @enumFromInt(@intFromEnum(record_data.fields.count)),
                layout_mod.RecordIdx{ .int_idx = 0 }, // Placeholder
            )) catch return error.OutOfMemory;

            const field_ptr = emitter.emitStructGep(record_type, record_ptr, field_idx) catch return error.OutOfMemory;
            try emitIncref(ctx, emitter, store, field_ptr, field_layout, amount);
        }
        field_idx += 1;
    }
}

fn emitRecordDecref(
    ctx: *RefcountContext,
    emitter: *emit.LlvmEmitter,
    store: *const Store,
    record_ptr: Builder.Value,
    record_data: layout_mod.RecordData,
) Error!void {
    const fields_range = record_data.getFields();
    var field_idx: u32 = 0;
    var iter = store.record_fields.iterate(fields_range);
    while (iter.next()) |field| {
        const field_layout = store.get(field.layout);
        if (field_layout.isRefcounted()) {
            const record_type = layout_types.layoutToLlvmType(emitter.builder, store, Layout.record(
                @enumFromInt(@intFromEnum(record_data.fields.count)),
                layout_mod.RecordIdx{ .int_idx = 0 },
            )) catch return error.OutOfMemory;

            const field_ptr = emitter.emitStructGep(record_type, record_ptr, field_idx) catch return error.OutOfMemory;
            try emitDecref(ctx, emitter, store, field_ptr, field_layout);
        }
        field_idx += 1;
    }
}

fn emitTupleIncref(
    ctx: *RefcountContext,
    emitter: *emit.LlvmEmitter,
    store: *const Store,
    tuple_ptr: Builder.Value,
    tuple_data: layout_mod.TupleData,
    amount: i64,
) Error!void {
    const fields_range = tuple_data.getFields();
    var field_idx: u32 = 0;
    var iter = store.tuple_fields.iterate(fields_range);
    while (iter.next()) |field| {
        const field_layout = store.get(field.layout);
        if (field_layout.isRefcounted()) {
            const tuple_type = layout_types.layoutToLlvmType(emitter.builder, store, Layout.tuple(
                @enumFromInt(@intFromEnum(tuple_data.fields.count)),
                layout_mod.TupleIdx{ .int_idx = 0 },
            )) catch return error.OutOfMemory;

            const field_ptr = emitter.emitStructGep(tuple_type, tuple_ptr, field_idx) catch return error.OutOfMemory;
            try emitIncref(ctx, emitter, store, field_ptr, field_layout, amount);
        }
        field_idx += 1;
    }
}

fn emitTupleDecref(
    ctx: *RefcountContext,
    emitter: *emit.LlvmEmitter,
    store: *const Store,
    tuple_ptr: Builder.Value,
    tuple_data: layout_mod.TupleData,
) Error!void {
    const fields_range = tuple_data.getFields();
    var field_idx: u32 = 0;
    var iter = store.tuple_fields.iterate(fields_range);
    while (iter.next()) |field| {
        const field_layout = store.get(field.layout);
        if (field_layout.isRefcounted()) {
            const tuple_type = layout_types.layoutToLlvmType(emitter.builder, store, Layout.tuple(
                @enumFromInt(@intFromEnum(tuple_data.fields.count)),
                layout_mod.TupleIdx{ .int_idx = 0 },
            )) catch return error.OutOfMemory;

            const field_ptr = emitter.emitStructGep(tuple_type, tuple_ptr, field_idx) catch return error.OutOfMemory;
            try emitDecref(ctx, emitter, store, field_ptr, field_layout);
        }
        field_idx += 1;
    }
}
