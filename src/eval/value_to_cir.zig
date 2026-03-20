//! Reconstruct CIR constant expressions from raw evaluated values.
//!
//! This module converts runtime `Value` bytes back into CIR constant nodes
//! (e_num, e_frac_f32, e_frac_f64, e_zero_argument_tag, e_tag, e_tuple)
//! using the `FoldType` descriptor and layout information.
//!
//! It replaces the old `foldScalar`, `foldTagUnion*`, `foldTuple`, and
//! `createConstantExpr` logic that was embedded in comptime_evaluator.zig.

const std = @import("std");
const base = @import("base");
const can = @import("can");
const layout_mod = @import("layout");
const builtins = @import("builtins");
const fold_type_mod = @import("fold_type.zig");

const Allocator = std.mem.Allocator;
const ModuleEnv = can.ModuleEnv;
const CIR = can.CIR;
const Value = @import("value.zig").Value;
const LayoutHelper = @import("value.zig").LayoutHelper;
const FoldType = fold_type_mod.FoldType;
const Ident = base.Ident;

// Public API

/// Reconstruct a CIR constant expression from a raw evaluated value.
///
/// Replaces the expression at `expr_idx` in-place if possible.
/// Returns true if the expression was successfully folded, false if the type
/// is not supported for folding (e.g., closures, lists, strings).
///
/// For top-level expressions, use `replaceExpr` which modifies in-place.
/// For sub-expressions (tuple elements, tag payloads), use `createExpr` which
/// creates new expressions.
pub fn replaceExpr(
    allocator: Allocator,
    value: Value,
    layout_idx: layout_mod.Idx,
    fold_ty: FoldType,
    layout_store: *const layout_mod.Store,
    env: *ModuleEnv,
    expr_idx: CIR.Expr.Idx,
) error{OutOfMemory}!bool {
    switch (fold_ty) {
        .int => |kind| {
            const i128_val = readAsI128(value, kind);
            const int_value = CIR.IntValue{
                .bytes = @bitCast(i128_val),
                .kind = switch (kind) {
                    .u8, .u16, .u32, .u64, .u128 => .u128,
                    .i8, .i16, .i32, .i64, .i128 => .i128,
                },
            };
            const num_kind: CIR.NumKind = intKindToNumKind(kind);
            try env.store.replaceExprWithNum(expr_idx, int_value, num_kind);
            return true;
        },

        .float => |kind| {
            switch (kind) {
                .f32 => {
                    const f32_value = value.read(f32);
                    const node_idx: CIR.Node.Idx = @enumFromInt(@intFromEnum(expr_idx));
                    var node = CIR.Node.init(.expr_frac_f32);
                    node.setPayload(.{ .expr_frac_f32 = .{
                        .value = @bitCast(f32_value),
                        .has_suffix = true,
                    } });
                    env.store.nodes.set(node_idx, node);
                    return true;
                },
                .f64 => {
                    const f64_value = value.read(f64);
                    const f64_bits: u64 = @bitCast(f64_value);
                    const low: u32 = @truncate(f64_bits);
                    const high: u32 = @truncate(f64_bits >> 32);
                    const node_idx: CIR.Node.Idx = @enumFromInt(@intFromEnum(expr_idx));
                    var node = CIR.Node.init(.expr_frac_f64);
                    node.setPayload(.{ .expr_frac_f64 = .{
                        .value_lo = low,
                        .value_hi = high,
                        .has_suffix = true,
                    } });
                    env.store.nodes.set(node_idx, node);
                    return true;
                },
            }
        },

        .dec => {
            const scaled_value = value.read(i128);
            const unscaled = builtins.compiler_rt_128.divTrunc_i128(
                scaled_value,
                builtins.dec.RocDec.one_point_zero_i128,
            );
            const int_value = CIR.IntValue{
                .bytes = @bitCast(unscaled),
                .kind = .i128,
            };
            try env.store.replaceExprWithNum(expr_idx, int_value, .dec);
            return true;
        },

        .bool_type => |info| {
            const bool_val = value.read(u8);
            const is_true = bool_val != 0;
            const tag_name_str = if (is_true) "True" else "False";
            const tag_name_ident = try env.insertIdent(Ident.for_text(tag_name_str));
            try env.store.replaceExprWithZeroArgumentTag(
                expr_idx,
                tag_name_ident,
                info.variant_var,
                info.ext_var,
                tag_name_ident,
            );
            return true;
        },

        .tag_union => |tu_info| {
            return replaceTagUnion(allocator, value, layout_idx, tu_info, layout_store, env, expr_idx);
        },

        .tuple => |elements| {
            return replaceTuple(allocator, value, layout_idx, elements, layout_store, env, expr_idx);
        },

        .str, .unsupported, .unit => return false,
    }
}

/// Create a NEW CIR expression from a raw evaluated value.
///
/// Used for sub-expressions (tuple elements, tag payloads) that need
/// new expression nodes rather than in-place modification.
/// Returns the new expression index, or null if the type is not foldable.
pub fn createExpr(
    allocator: Allocator,
    value: Value,
    layout_idx: layout_mod.Idx,
    fold_ty: FoldType,
    layout_store: *const layout_mod.Store,
    env: *ModuleEnv,
    region: base.Region,
) error{OutOfMemory}!?CIR.Expr.Idx {
    switch (fold_ty) {
        .int => |kind| {
            const i128_val = readAsI128(value, kind);
            const int_value = CIR.IntValue{
                .bytes = @bitCast(i128_val),
                .kind = switch (kind) {
                    .u8, .u16, .u32, .u64, .u128 => .u128,
                    .i8, .i16, .i32, .i64, .i128 => .i128,
                },
            };
            const num_kind: CIR.NumKind = intKindToNumKind(kind);
            return try env.addExpr(.{
                .e_num = .{
                    .value = int_value,
                    .kind = num_kind,
                },
            }, region);
        },

        .float => |kind| {
            switch (kind) {
                .f32 => {
                    const f32_value = value.read(f32);
                    return try env.addExpr(.{
                        .e_frac_f32 = .{
                            .value = f32_value,
                            .has_suffix = true,
                        },
                    }, region);
                },
                .f64 => {
                    const f64_value = value.read(f64);
                    return try env.addExpr(.{
                        .e_frac_f64 = .{
                            .value = f64_value,
                            .has_suffix = true,
                        },
                    }, region);
                },
            }
        },

        .dec => {
            const scaled_value = value.read(i128);
            const unscaled = builtins.compiler_rt_128.divTrunc_i128(
                scaled_value,
                builtins.dec.RocDec.one_point_zero_i128,
            );
            const int_value = CIR.IntValue{
                .bytes = @bitCast(unscaled),
                .kind = .i128,
            };
            return try env.addExpr(.{
                .e_num = .{
                    .value = int_value,
                    .kind = .dec,
                },
            }, region);
        },

        .bool_type => |info| {
            const bool_val = value.read(u8);
            const is_true = bool_val != 0;
            const tag_name_str = if (is_true) "True" else "False";
            const tag_name_ident = try env.insertIdent(Ident.for_text(tag_name_str));
            return try env.addExpr(.{
                .e_zero_argument_tag = .{
                    .closure_name = tag_name_ident,
                    .variant_var = info.variant_var,
                    .ext_var = info.ext_var,
                    .name = tag_name_ident,
                },
            }, region);
        },

        .tag_union => |tu_info| {
            return createTagUnionExpr(allocator, value, layout_idx, tu_info, layout_store, env, region);
        },

        .tuple => |elements| {
            return createTupleExpr(allocator, value, layout_idx, elements, layout_store, env, region);
        },

        .str, .unsupported, .unit => return null,
    }
}

// Tag union helpers

/// Replace an expression in-place with a tag union constant.
fn replaceTagUnion(
    allocator: Allocator,
    value: Value,
    layout_idx: layout_mod.Idx,
    tu_info: FoldType.TagUnionInfo,
    layout_store: *const layout_mod.Store,
    env: *ModuleEnv,
    expr_idx: CIR.Expr.Idx,
) error{OutOfMemory}!bool {
    const layout = layout_store.getLayout(layout_idx);

    const disc: u16 = switch (layout.tag) {
        .tag_union => blk: {
            const helper = LayoutHelper.init(layout_store);
            break :blk helper.readTagDiscriminant(value, layout_idx);
        },
        .scalar => blk: {
            // Scalar-represented tag union: the entire value IS the discriminant.
            const int_kind = intKindFromScalarLayout(layout);
            break :blk @intCast(@as(u64, @bitCast(@as(i64, @truncate(readAsI128(value, int_kind))))));
        },
        .struct_ => blk: {
            // Struct-represented tag union: payload is field 0, discriminant is field 1 (last).
            const struct_idx = layout.data.struct_.idx;
            const disc_field_layout_idx = layout_store.getStructFieldLayoutByOriginalIndex(struct_idx, 1);
            const disc_field_offset = layout_store.getStructFieldOffsetByOriginalIndex(struct_idx, 1);
            const disc_value = value.offset(disc_field_offset);
            const disc_layout = layout_store.getLayout(disc_field_layout_idx);
            const disc_int_kind = intKindFromScalarLayout(disc_layout);
            break :blk @intCast(@as(u64, @bitCast(@as(i64, @truncate(readAsI128(disc_value, disc_int_kind))))));
        },
        // Layouts that cannot represent a foldable tag union.
        .list, .closure, .box, .box_of_zst, .list_of_zst, .zst => return false,
    };

    if (disc >= tu_info.tags.len) return false;
    const tag = tu_info.tags[disc];

    if (tag.payloads.len == 0) {
        // Zero-argument tag — tag.name is already an Ident.Idx from the type store
        try env.store.replaceExprWithZeroArgumentTag(
            expr_idx,
            tag.name,
            tu_info.variant_var,
            tu_info.ext_var,
            tag.name,
        );
        return true;
    }

    // Tag with payload — build sub-expressions for each payload argument.
    const region = env.store.getExprRegion(expr_idx);

    // Determine where the payload starts.
    const payload_value = switch (layout.tag) {
        .tag_union => value, // payload is at offset 0
        .struct_ => blk: {
            // payload is in field 0
            const struct_idx = layout.data.struct_.idx;
            const payload_offset = layout_store.getStructFieldOffsetByOriginalIndex(struct_idx, 0);
            break :blk value.offset(payload_offset);
        },
        else => value,
    };

    // Determine the payload layout index.
    const payload_layout_idx: layout_mod.Idx = switch (layout.tag) {
        .tag_union => blk: {
            const tu_data = layout_store.getTagUnionData(layout.data.tag_union.idx);
            const variants = layout_store.getTagUnionVariants(tu_data);
            const variant = variants.get(disc);
            break :blk variant.payload_layout;
        },
        .struct_ => layout_store.getStructFieldLayoutByOriginalIndex(layout.data.struct_.idx, 0),
        else => layout_idx,
    };

    var arg_indices = try allocator.alloc(CIR.Expr.Idx, tag.payloads.len);
    defer allocator.free(arg_indices);

    if (tag.payloads.len == 1) {
        // Single payload argument: the whole payload IS the argument.
        const arg_expr = try createExpr(
            allocator,
            payload_value,
            payload_layout_idx,
            tag.payloads[0],
            layout_store,
            env,
            region,
        ) orelse return false;
        arg_indices[0] = arg_expr;
    } else {
        // Multiple payload arguments: the payload is a struct (tuple).
        const pl_layout = layout_store.getLayout(payload_layout_idx);
        if (pl_layout.tag != .struct_) return false;
        const pl_struct_idx = pl_layout.data.struct_.idx;

        for (tag.payloads, 0..) |payload_fold_ty, i| {
            const field_offset = layout_store.getStructFieldOffsetByOriginalIndex(pl_struct_idx, @intCast(i));
            const field_layout = layout_store.getStructFieldLayoutByOriginalIndex(pl_struct_idx, @intCast(i));
            const elem_value = payload_value.offset(field_offset);
            const arg_expr = try createExpr(
                allocator,
                elem_value,
                field_layout,
                payload_fold_ty,
                layout_store,
                env,
                region,
            ) orelse return false;
            arg_indices[i] = arg_expr;
        }
    }

    try env.store.replaceExprWithTag(expr_idx, tag.name, arg_indices);
    return true;
}

/// Create a new CIR expression for a tag union constant.
fn createTagUnionExpr(
    allocator: Allocator,
    value: Value,
    layout_idx: layout_mod.Idx,
    tu_info: FoldType.TagUnionInfo,
    layout_store: *const layout_mod.Store,
    env: *ModuleEnv,
    region: base.Region,
) error{OutOfMemory}!?CIR.Expr.Idx {
    const layout = layout_store.getLayout(layout_idx);

    const disc: u16 = switch (layout.tag) {
        .tag_union => blk: {
            const helper = LayoutHelper.init(layout_store);
            break :blk helper.readTagDiscriminant(value, layout_idx);
        },
        .scalar => blk: {
            const int_kind = intKindFromScalarLayout(layout);
            break :blk @intCast(@as(u64, @bitCast(@as(i64, @truncate(readAsI128(value, int_kind))))));
        },
        .struct_ => blk: {
            const struct_idx = layout.data.struct_.idx;
            const disc_field_layout_idx = layout_store.getStructFieldLayoutByOriginalIndex(struct_idx, 1);
            const disc_field_offset = layout_store.getStructFieldOffsetByOriginalIndex(struct_idx, 1);
            const disc_value = value.offset(disc_field_offset);
            const disc_layout = layout_store.getLayout(disc_field_layout_idx);
            const disc_int_kind = intKindFromScalarLayout(disc_layout);
            break :blk @intCast(@as(u64, @bitCast(@as(i64, @truncate(readAsI128(disc_value, disc_int_kind))))));
        },
        .list, .closure, .box, .box_of_zst, .list_of_zst, .zst => return null,
    };

    if (disc >= tu_info.tags.len) return null;
    const tag = tu_info.tags[disc];

    if (tag.payloads.len == 0) {
        // Zero-argument tag — tag.name is already an Ident.Idx from the type store
        return try env.addExpr(.{
            .e_zero_argument_tag = .{
                .closure_name = tag.name,
                .variant_var = tu_info.variant_var,
                .ext_var = tu_info.ext_var,
                .name = tag.name,
            },
        }, region);
    }

    // Tag with payload
    const payload_value = switch (layout.tag) {
        .tag_union => value,
        .struct_ => blk: {
            const struct_idx = layout.data.struct_.idx;
            const payload_offset = layout_store.getStructFieldOffsetByOriginalIndex(struct_idx, 0);
            break :blk value.offset(payload_offset);
        },
        else => value,
    };

    const payload_layout_idx: layout_mod.Idx = switch (layout.tag) {
        .tag_union => blk: {
            const tu_data = layout_store.getTagUnionData(layout.data.tag_union.idx);
            const variants = layout_store.getTagUnionVariants(tu_data);
            const variant = variants.get(disc);
            break :blk variant.payload_layout;
        },
        .struct_ => layout_store.getStructFieldLayoutByOriginalIndex(layout.data.struct_.idx, 0),
        else => layout_idx,
    };

    var arg_indices = try allocator.alloc(CIR.Expr.Idx, tag.payloads.len);
    defer allocator.free(arg_indices);

    if (tag.payloads.len == 1) {
        const arg_expr = try createExpr(
            allocator,
            payload_value,
            payload_layout_idx,
            tag.payloads[0],
            layout_store,
            env,
            region,
        ) orelse return null;
        arg_indices[0] = arg_expr;
    } else {
        const pl_layout = layout_store.getLayout(payload_layout_idx);
        if (pl_layout.tag != .struct_) return null;
        const pl_struct_idx = pl_layout.data.struct_.idx;

        for (tag.payloads, 0..) |payload_fold_ty, i| {
            const field_offset = layout_store.getStructFieldOffsetByOriginalIndex(pl_struct_idx, @intCast(i));
            const field_layout = layout_store.getStructFieldLayoutByOriginalIndex(pl_struct_idx, @intCast(i));
            const elem_value = payload_value.offset(field_offset);
            const arg_expr = try createExpr(
                allocator,
                elem_value,
                field_layout,
                payload_fold_ty,
                layout_store,
                env,
                region,
            ) orelse return null;
            arg_indices[i] = arg_expr;
        }
    }

    // Build the tag expression with argument span.
    const index_data_start = env.store.index_data.len();
    for (arg_indices) |arg_idx| {
        _ = try env.store.index_data.append(env.store.gpa, @intFromEnum(arg_idx));
    }

    return try env.addExpr(.{
        .e_tag = .{
            .name = tag.name,
            .args = .{ .span = .{
                .start = @intCast(index_data_start),
                .len = @intCast(arg_indices.len),
            } },
        },
    }, region);
}

// Tuple helpers

/// Replace an expression in-place with a tuple constant.
fn replaceTuple(
    allocator: Allocator,
    value: Value,
    layout_idx: layout_mod.Idx,
    elements: []const FoldType,
    layout_store: *const layout_mod.Store,
    env: *ModuleEnv,
    expr_idx: CIR.Expr.Idx,
) error{OutOfMemory}!bool {
    if (elements.len == 0) return true; // empty tuple — nothing to fold

    const layout = layout_store.getLayout(layout_idx);
    if (layout.tag != .struct_) return false;
    const struct_idx = layout.data.struct_.idx;

    var elem_exprs = try allocator.alloc(CIR.Expr.Idx, elements.len);
    defer allocator.free(elem_exprs);

    const region = env.store.getExprRegion(expr_idx);

    for (elements, 0..) |elem_fold_ty, i| {
        const field_offset = layout_store.getStructFieldOffsetByOriginalIndex(struct_idx, @intCast(i));
        const field_layout = layout_store.getStructFieldLayoutByOriginalIndex(struct_idx, @intCast(i));
        const elem_value = value.offset(field_offset);

        const elem_expr = try createExpr(
            allocator,
            elem_value,
            field_layout,
            elem_fold_ty,
            layout_store,
            env,
            region,
        ) orelse return false;
        elem_exprs[i] = elem_expr;
    }

    try env.store.replaceExprWithTuple(expr_idx, elem_exprs);
    return true;
}

/// Create a new CIR tuple expression.
fn createTupleExpr(
    allocator: Allocator,
    value: Value,
    layout_idx: layout_mod.Idx,
    elements: []const FoldType,
    layout_store: *const layout_mod.Store,
    env: *ModuleEnv,
    region: base.Region,
) error{OutOfMemory}!?CIR.Expr.Idx {
    if (elements.len == 0) {
        // Empty tuple
        return try env.addExpr(.{
            .e_tuple = .{ .elems = .{ .span = .{ .start = 0, .len = 0 } } },
        }, region);
    }

    const layout = layout_store.getLayout(layout_idx);
    if (layout.tag != .struct_) return null;
    const struct_idx = layout.data.struct_.idx;

    var elem_exprs = try allocator.alloc(CIR.Expr.Idx, elements.len);
    defer allocator.free(elem_exprs);

    for (elements, 0..) |elem_fold_ty, i| {
        const field_offset = layout_store.getStructFieldOffsetByOriginalIndex(struct_idx, @intCast(i));
        const field_layout = layout_store.getStructFieldLayoutByOriginalIndex(struct_idx, @intCast(i));
        const elem_value = value.offset(field_offset);

        const elem_expr = try createExpr(
            allocator,
            elem_value,
            field_layout,
            elem_fold_ty,
            layout_store,
            env,
            region,
        ) orelse return null;
        elem_exprs[i] = elem_expr;
    }

    // Build span in index_data
    const index_data_start = env.store.index_data.len();
    for (elem_exprs) |elem_idx| {
        _ = try env.store.index_data.append(env.store.gpa, @intFromEnum(elem_idx));
    }

    return try env.addExpr(.{
        .e_tuple = .{
            .elems = .{ .span = .{
                .start = @intCast(index_data_start),
                .len = @intCast(elem_exprs.len),
            } },
        },
    }, region);
}

// Scalar reading utilities

/// Read a value as i128, sign-extending signed types and zero-extending unsigned types.
fn readAsI128(val: Value, kind: FoldType.IntKind) i128 {
    return switch (kind) {
        .i8 => val.read(i8),
        .i16 => val.read(i16),
        .i32 => val.read(i32),
        .i64 => val.read(i64),
        .i128 => val.read(i128),
        .u8 => val.read(u8),
        .u16 => val.read(u16),
        .u32 => val.read(u32),
        .u64 => val.read(u64),
        .u128 => @bitCast(val.read(u128)),
    };
}

/// Map a FoldType.IntKind to a CIR.NumKind.
fn intKindToNumKind(kind: FoldType.IntKind) CIR.NumKind {
    return switch (kind) {
        .i8 => .i8,
        .i16 => .i16,
        .i32 => .i32,
        .i64 => .i64,
        .i128 => .i128,
        .u8 => .u8,
        .u16 => .u16,
        .u32 => .u32,
        .u64 => .u64,
        .u128 => .u128,
    };
}

/// Derive a FoldType.IntKind from a scalar layout that is known to be an integer.
fn intKindFromScalarLayout(layout: layout_mod.Layout) FoldType.IntKind {
    const precision = layout.data.scalar.data.int;
    return switch (precision) {
        .i8 => .i8,
        .i16 => .i16,
        .i32 => .i32,
        .i64 => .i64,
        .i128 => .i128,
        .u8 => .u8,
        .u16 => .u16,
        .u32 => .u32,
        .u64 => .u64,
        .u128 => .u128,
    };
}
