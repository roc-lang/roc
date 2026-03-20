//! Semantic type descriptor for constant folding.
//!
//! Converts checked CIR type information into a compact reconstruction
//! descriptor. No runtime unification, no `rt_var`. Built once per
//! expression being folded from the checked type store.

const std = @import("std");
const base = @import("base");
const can = @import("can");
const types_mod = @import("types");
const layout_mod = @import("layout");

const Allocator = std.mem.Allocator;
const ModuleEnv = can.ModuleEnv;
const CIR = can.CIR;
const Var = types_mod.Var;
const Ident = base.Ident;
const TypesStore = types_mod.Store;

/// A simplified type representation used for folding/interpreting values.
pub const FoldType = union(enum) {
    int: IntKind,
    float: FloatKind,
    dec: void,
    str: void,
    bool_type: BoolInfo,
    tuple: []const FoldType,
    tag_union: TagUnionInfo,
    unit: void,
    unsupported: void,

    pub const IntKind = enum {
        i8,
        i16,
        i32,
        i64,
        i128,
        u8,
        u16,
        u32,
        u64,
        u128,
    };

    pub const FloatKind = enum { f32, f64 };

    pub const BoolInfo = struct {
        variant_var: Var,
        ext_var: Var,
    };

    pub const TagUnionInfo = struct {
        variant_var: Var,
        ext_var: Var,
        tags: []const TagInfo,
    };

    pub const TagInfo = struct {
        name: Ident.Idx,
        payloads: []const FoldType,
    };
};

const TagUnionResult = struct {
    variant_var: Var,
    ext_var: Var,
    tags_range: types_mod.Tag.SafeMultiList.Range,
};

/// Derives a `FoldType` from a CIR expression and its layout.
pub fn fromExpr(
    allocator: Allocator,
    env: *const ModuleEnv,
    expr_idx: CIR.Expr.Idx,
    layout_idx: layout_mod.Idx,
    layout_store: *const layout_mod.Store,
) error{OutOfMemory}!FoldType {
    const type_var = ModuleEnv.varFrom(expr_idx);
    return fromVar(allocator, &env.types, type_var, layout_idx, layout_store);
}

/// Derives a `FoldType` from a type variable and its layout.
pub fn fromVar(
    allocator: Allocator,
    types_store: *const TypesStore,
    type_var: Var,
    layout_idx: layout_mod.Idx,
    layout_store: *const layout_mod.Store,
) error{OutOfMemory}!FoldType {
    // First check predefined layout indices for scalars and special types.
    switch (layout_idx) {
        .bool => {
            if (resolveToTagUnion(types_store, type_var)) |tu_result| {
                return .{ .bool_type = .{
                    .variant_var = tu_result.variant_var,
                    .ext_var = tu_result.ext_var,
                } };
            } else {
                // Fallback: use the expression's own type var
                return .{ .bool_type = .{
                    .variant_var = type_var,
                    .ext_var = type_var,
                } };
            }
        },
        .u8 => return .{ .int = .u8 },
        .i8 => return .{ .int = .i8 },
        .u16 => return .{ .int = .u16 },
        .i16 => return .{ .int = .i16 },
        .u32 => return .{ .int = .u32 },
        .i32 => return .{ .int = .i32 },
        .u64 => return .{ .int = .u64 },
        .i64 => return .{ .int = .i64 },
        .u128 => return .{ .int = .u128 },
        .i128 => return .{ .int = .i128 },
        .f32 => return .{ .float = .f32 },
        .f64 => return .{ .float = .f64 },
        .dec => return .dec,
        .str => return .str,
        .zst => return .unit,
        _ => {},
    }

    // For non-predefined layouts, get the actual layout from the store.
    const layout = layout_store.getLayout(layout_idx);

    return switch (layout.tag) {
        .struct_ => {
            // Resolve the type variable to determine whether this is a tag union,
            // tuple, or record at the type level.
            const resolved = types_store.resolveVar(type_var);
            switch (resolved.desc.content) {
                .structure => |ft| switch (ft) {
                    .tag_union => |tu| {
                        // Struct-represented tag union (single variant with payload).
                        const tu_result = TagUnionResult{
                            .variant_var = resolved.var_,
                            .ext_var = tu.ext,
                            .tags_range = tu.tags,
                        };
                        return buildTagUnionInfo(allocator, types_store, tu_result, layout_idx, layout_store);
                    },
                    .tuple => |tup| {
                        const elem_vars = types_store.sliceVars(tup.elems);
                        const struct_idx = layout.data.struct_.idx;
                        var elems = try allocator.alloc(FoldType, elem_vars.len);
                        for (elem_vars, 0..) |ev, i| {
                            const elem_layout = layout_store.getStructFieldLayoutByOriginalIndex(struct_idx, @intCast(i));
                            elems[i] = try fromVar(allocator, types_store, ev, elem_layout, layout_store);
                        }
                        return .{ .tuple = elems };
                    },
                    .record => |rec| {
                        const fields_slice = types_store.getRecordFieldsSlice(rec.fields);
                        const field_names = fields_slice.items(.name);
                        const field_vars = fields_slice.items(.var_);
                        const struct_idx = layout.data.struct_.idx;
                        var elems = try allocator.alloc(FoldType, field_names.len);
                        for (field_vars, 0..) |fv, i| {
                            const field_layout = layout_store.getStructFieldLayoutByOriginalIndex(struct_idx, @intCast(i));
                            elems[i] = try fromVar(allocator, types_store, fv, field_layout, layout_store);
                        }
                        return .{ .tuple = elems };
                    },
                    .nominal_type => |nom| {
                        // Follow through nominal types to find the underlying structure.
                        const vars = types_store.sliceVars(nom.vars.nonempty);
                        for (vars) |v| {
                            if (resolveToTagUnion(types_store, v)) |tu_result| {
                                return buildTagUnionInfo(allocator, types_store, tu_result, layout_idx, layout_store);
                            }
                        }
                        return .unsupported;
                    },
                    .empty_record => return .unit,
                    else => return .unsupported,
                },
                .alias => |alias| {
                    // Follow through aliases to find the underlying structure.
                    const vars = types_store.sliceVars(alias.vars.nonempty);
                    for (vars) |v| {
                        // Try recursing on each alias var with the same layout.
                        const inner = try fromVar(allocator, types_store, v, layout_idx, layout_store);
                        if (inner != .unsupported) return inner;
                    }
                    return .unsupported;
                },
                else => return .unsupported,
            }
        },
        .tag_union => {
            // Full tag union layout. Resolve the type to get tag names.
            if (resolveToTagUnion(types_store, type_var)) |tu_result| {
                return buildTagUnionInfo(allocator, types_store, tu_result, layout_idx, layout_store);
            }
            return .unsupported;
        },
        .scalar => {
            // Scalar layout for a non-predefined index. Check if it's a tag union type
            // (e.g., an enum-like tag union that lowers to a scalar discriminant).
            if (resolveToTagUnion(types_store, type_var)) |tu_result| {
                return buildTagUnionInfo(allocator, types_store, tu_result, layout_idx, layout_store);
            }
            return .unsupported;
        },
        .zst => return .unit,
        .list, .closure, .box, .box_of_zst, .list_of_zst => return .unsupported,
    };
}

fn resolveToTagUnion(types_store: *const TypesStore, var_: Var) ?TagUnionResult {
    const resolved = types_store.resolveVar(var_);
    return switch (resolved.desc.content) {
        .structure => |ft| switch (ft) {
            .tag_union => |tu| .{
                .variant_var = resolved.var_,
                .ext_var = tu.ext,
                .tags_range = tu.tags,
            },
            .nominal_type => |nom| {
                const vars = types_store.sliceVars(nom.vars.nonempty);
                for (vars) |v| {
                    if (resolveToTagUnion(types_store, v)) |result| return result;
                }
                return null;
            },
            else => null,
        },
        .alias => |alias| {
            const vars = types_store.sliceVars(alias.vars.nonempty);
            for (vars) |v| {
                if (resolveToTagUnion(types_store, v)) |result| return result;
            }
            return null;
        },
        else => null,
    };
}

fn buildTagUnionInfo(
    allocator: Allocator,
    types_store: *const TypesStore,
    tu_result: TagUnionResult,
    _: layout_mod.Idx,
    layout_store: *const layout_mod.Store,
) error{OutOfMemory}!FoldType {
    const tags_slice = types_store.getTagsSlice(tu_result.tags_range);
    const tag_names = tags_slice.items(.name);
    const tag_args_ranges = tags_slice.items(.args);

    var tags = try allocator.alloc(FoldType.TagInfo, tag_names.len);

    for (tag_names, tag_args_ranges, 0..) |name, args_range, i| {
        const arg_vars = types_store.sliceVars(args_range);

        if (arg_vars.len == 0) {
            tags[i] = .{ .name = name, .payloads = &.{} };
        } else {
            // Build payload FoldTypes for each argument.
            // We pass .zst as a default layout since we may not know the exact
            // sub-layout; the value_to_cir module will use layout offsets directly.
            var payloads = try allocator.alloc(FoldType, arg_vars.len);
            for (arg_vars, 0..) |arg_var, j| {
                payloads[j] = try fromVar(allocator, types_store, arg_var, .zst, layout_store);
            }
            tags[i] = .{ .name = name, .payloads = payloads };
        }
    }

    return .{ .tag_union = .{
        .variant_var = tu_result.variant_var,
        .ext_var = tu_result.ext_var,
        .tags = tags,
    } };
}
