//! Helpers for rendering interpreter values back into readable Roc syntax.

const std = @import("std");
const types = @import("types");
const can = @import("can");
const layout = @import("interpreter_layout");
const interpreter_values = @import("interpreter_values.zig");
const builtins = @import("builtins");
const StackValue = @import("StackValue.zig");
const TypeScope = types.TypeScope;

/// Copy tags and sort them alphabetically, returning the tag at the given index.
/// This is necessary because tags stored in the runtime type store may not be
/// sorted consistently when the same source type is translated multiple times
/// with different cache generations. By sorting at render time, we ensure the
/// discriminant index maps to the correct tag name.
fn getSortedTag(
    ctx: *RenderCtx,
    tag_union: types.TagUnion,
    tag_index: usize,
) ?types.Tag {
    // Gather tags across the full extension chain.
    var all_tags = std.array_list.AlignedManaged(types.Tag, null).init(ctx.allocator);
    defer all_tags.deinit();

    const initial_tags = ctx.runtime_types.getTagsSlice(tag_union.tags);
    for (initial_tags.items(.name), initial_tags.items(.args)) |name, args| {
        all_tags.append(.{ .name = name, .args = args }) catch return null;
    }

    var ext = tag_union.ext;
    while (true) {
        const ext_resolved = ctx.runtime_types.resolveVar(ext);
        switch (ext_resolved.desc.content) {
            .structure => |st| switch (st) {
                .tag_union => |ext_tag_union| {
                    const ext_tags = ctx.runtime_types.getTagsSlice(ext_tag_union.tags);
                    for (ext_tags.items(.name), ext_tags.items(.args)) |name, args| {
                        all_tags.append(.{ .name = name, .args = args }) catch return null;
                    }
                    ext = ext_tag_union.ext;
                },
                .empty_tag_union => break,
                else => break,
            },
            .alias => |alias| {
                ext = ctx.runtime_types.getAliasBackingVar(alias);
            },
            else => break,
        }
    }

    if (all_tags.items.len == 0) return null;

    const ident_store = ctx.env.common.getIdentStore();
    std.mem.sort(types.Tag, all_tags.items, ident_store, types.Tag.sortByNameAsc);

    return if (tag_index < all_tags.items.len) all_tags.items[tag_index] else null;
}

fn toVarRange(range: anytype) types.Var.SafeList.Range {
    const RangeType = types.Var.SafeList.Range;
    if (comptime @hasField(@TypeOf(range), "nonempty")) {
        return @field(range, "nonempty");
    }
    return @as(RangeType, range);
}

/// Callback function type for checking and rendering nominal types with custom to_inspect methods.
/// Returns the rendered string if the type has a to_inspect method, null otherwise.
/// Ownership of the returned string is transferred to the caller.
pub const ToInspectCallback = *const fn (ctx: *anyopaque, value: StackValue, rt_var: types.Var) ?[]u8;

/// Shared rendering context that provides allocator, module environment, and runtime caches.
pub const RenderCtx = struct {
    allocator: std.mem.Allocator,
    env: *can.ModuleEnv,
    runtime_types: *types.store.Store,
    layout_store: *layout.Store,
    type_scope: *const TypeScope,
    /// Optional callback for handling nominal types with custom to_inspect methods.
    /// If set, this callback will be invoked when rendering nominal type values.
    to_inspect_callback: ?ToInspectCallback = null,
    /// Opaque context pointer passed to the to_inspect callback.
    callback_ctx: ?*anyopaque = null,
};

fn shouldPreferIntegerLayoutRendering(ctx: *RenderCtx, rt_var: types.Var) bool {
    var resolved = ctx.runtime_types.resolveVar(rt_var);
    while (true) {
        switch (resolved.desc.content) {
            .alias => |al| {
                const backing = ctx.runtime_types.getAliasBackingVar(al);
                resolved = ctx.runtime_types.resolveVar(backing);
            },
            // When the type is still generic, trust concrete runtime layout for ints.
            .flex, .rigid => return true,
            .structure => |st| switch (st) {
                .nominal_type => |nt| {
                    return nt.ident.ident_idx.eql(ctx.env.idents.builtin_numeral);
                },
                else => return false,
            },
            else => return false,
        }
    }
}

/// Render `value` using the supplied runtime type variable, following alias/nominal backing.
pub fn renderValueRocWithType(ctx: *RenderCtx, value: StackValue, rt_var: types.Var) ![]u8 {
    const gpa = ctx.allocator;
    var resolved = ctx.runtime_types.resolveVar(rt_var);

    // Check layout first for special rendering cases.
    // Str has a dedicated scalar layout; ordinary tag unions, including Bool,
    // are rendered structurally below using type information.
    if (value.layout.tag == .scalar) {
        const scalar = value.layout.data.scalar;
        if (scalar.tag == .str) {
            // Render strings with quotes
            const rs: *const builtins.str.RocStr = @ptrCast(@alignCast(value.ptr.?));
            const s = rs.asSlice();
            var buf = std.array_list.AlignedManaged(u8, null).init(gpa);
            errdefer buf.deinit();
            try buf.append('"');
            for (s) |ch| {
                switch (ch) {
                    '\\' => try buf.appendSlice("\\\\"),
                    '"' => try buf.appendSlice("\\\""),
                    else => try buf.append(ch),
                }
            }
            try buf.append('"');
            return buf.toOwnedSlice();
        }
        if (scalar.tag == .int and shouldPreferIntegerLayoutRendering(ctx, rt_var)) {
            return renderValueRoc(ctx, value);
        }
    }

    // unwrap aliases/nominals, but check for to_inspect callbacks on nominal types first
    unwrap: while (true) {
        switch (resolved.desc.content) {
            .alias => |al| {
                const backing = ctx.runtime_types.getAliasBackingVar(al);
                resolved = ctx.runtime_types.resolveVar(backing);
            },
            .structure => |st| switch (st) {
                .nominal_type => |nt| {
                    // Check if there's a to_inspect callback for this nominal type
                    if (ctx.to_inspect_callback) |callback| {
                        if (ctx.callback_ctx) |cb_ctx| {
                            // The callback returns the rendered string if the type has to_inspect,
                            // null otherwise
                            if (callback(cb_ctx, value, rt_var)) |rendered| {
                                return rendered;
                            }
                        }
                    }
                    // Special handling for Box before unwrapping
                    if (nt.ident.ident_idx.eql(ctx.env.idents.box)) {
                        // Use sliceNominalArgs which skips the backing var (first element)
                        const arg_vars = ctx.runtime_types.sliceNominalArgs(nt);
                        if (arg_vars.len != 1) {
                            return error.TypeMismatch;
                        }
                        const payload_var = arg_vars[0];

                        var out = std.array_list.AlignedManaged(u8, null).init(gpa);
                        errdefer out.deinit();
                        try out.appendSlice("Box(");

                        const payload_layout_idx = try ctx.layout_store.fromTypeVar(0, payload_var, ctx.type_scope, null);
                        const payload_layout = ctx.layout_store.getLayout(payload_layout_idx);
                        const payload_size = ctx.layout_store.layoutSize(payload_layout);

                        var payload_value = StackValue{
                            .layout = payload_layout,
                            .ptr = null,
                            .is_initialized = true,
                            .rt_var = payload_var,
                        };

                        switch (value.layout.tag) {
                            .box => {
                                const elem_layout = ctx.layout_store.getLayout(value.layout.data.box);
                                const data_ptr_opt = value.getBoxedData() orelse return error.TypeMismatch;
                                if (!elem_layout.eql(payload_layout)) {
                                    return error.TypeMismatch;
                                }
                                if (payload_size > 0) {
                                    payload_value.ptr = @as(*anyopaque, @ptrFromInt(@intFromPtr(data_ptr_opt)));
                                }
                                const rendered_payload = try renderValueRocWithType(ctx, payload_value, payload_var);
                                defer gpa.free(rendered_payload);
                                try out.appendSlice(rendered_payload);
                            },
                            .box_of_zst => {
                                if (payload_size != 0) return error.TypeMismatch;
                                const rendered_payload = try renderValueRocWithType(ctx, payload_value, payload_var);
                                defer gpa.free(rendered_payload);
                                try out.appendSlice(rendered_payload);
                            },
                            else => {
                                unreachable;
                            },
                        }

                        try out.append(')');
                        return out.toOwnedSlice();
                    }
                    // Special handling for List before unwrapping - render with element type info
                    if (nt.ident.ident_idx.eql(ctx.env.idents.list)) {
                        // Use sliceNominalArgs which skips the backing var (first element)
                        const arg_vars = ctx.runtime_types.sliceNominalArgs(nt);
                        if (arg_vars.len != 1) {
                            return error.TypeMismatch;
                        }

                        // Get element type from List's type argument
                        const elem_type_var = arg_vars[0];

                        var out = std.array_list.AlignedManaged(u8, null).init(gpa);
                        errdefer out.deinit();
                        try out.append('[');

                        // Handle list layout
                        if (value.layout.tag == .list) {
                            const roc_list: *const builtins.list.RocList = @ptrCast(@alignCast(value.ptr.?));
                            const len = roc_list.len();
                            if (len > 0) {
                                const elem_layout_idx = value.layout.data.list;
                                const elem_layout = ctx.layout_store.getLayout(elem_layout_idx);
                                const elem_size = ctx.layout_store.layoutSize(elem_layout);
                                var i: usize = 0;
                                while (i < len) : (i += 1) {
                                    if (roc_list.bytes) |bytes| {
                                        const elem_ptr: *anyopaque = @ptrCast(bytes + i * elem_size);
                                        const elem_val = StackValue{
                                            .layout = elem_layout,
                                            .ptr = elem_ptr,
                                            .is_initialized = true,
                                            .rt_var = elem_type_var,
                                        };
                                        // Use type-aware rendering to enable unbound numeral stripping
                                        const rendered = try renderValueRocWithType(ctx, elem_val, elem_type_var);
                                        defer gpa.free(rendered);
                                        try out.appendSlice(rendered);
                                        if (i + 1 < len) try out.appendSlice(", ");
                                    }
                                }
                            }
                        } else if (value.layout.tag == .list_of_zst) {
                            // list_of_zst - elements may have no data (true ZST), or the list
                            // may have been incorrectly classified as list_of_zst when the element
                            // type resolved to flex during type translation (e.g., List(Package.Idx)
                            // where Idx is an opaque type from another module). In the latter case,
                            // the list has real data bytes that we can render.
                            const roc_list: *const builtins.list.RocList = @ptrCast(@alignCast(value.ptr.?));
                            const len = roc_list.len();
                            if (len > 0) {
                                // Try to compute real element layout from the type var.
                                // If the element type has a concrete layout with non-zero size,
                                // use actual data bytes for rendering instead of null pointers.
                                const computed_elem_layout = if (roc_list.bytes != null) blk: {
                                    const elem_layout_idx = ctx.layout_store.fromTypeVar(0, elem_type_var, ctx.type_scope, null) catch break :blk null;
                                    const el = ctx.layout_store.getLayout(elem_layout_idx);
                                    const el_size = ctx.layout_store.layoutSize(el);
                                    if (el_size > 0) break :blk el else break :blk null;
                                } else null;

                                var i: usize = 0;
                                while (i < len) : (i += 1) {
                                    const elem_val = if (computed_elem_layout) |el| StackValue{
                                        .layout = el,
                                        .ptr = @ptrCast(roc_list.bytes.? + i * ctx.layout_store.layoutSize(el)),
                                        .is_initialized = true,
                                        .rt_var = elem_type_var,
                                    } else StackValue{
                                        .layout = layout.Layout.zst(),
                                        .ptr = null,
                                        .is_initialized = true,
                                        .rt_var = elem_type_var,
                                    };
                                    const rendered = try renderValueRocWithType(ctx, elem_val, elem_type_var);
                                    defer gpa.free(rendered);
                                    try out.appendSlice(rendered);
                                    if (i + 1 < len) try out.appendSlice(", ");
                                }
                            }
                        }

                        try out.append(']');
                        return out.toOwnedSlice();
                    }
                    // No custom to_inspect, unwrap to backing type
                    const backing = ctx.runtime_types.getNominalBackingVar(nt);
                    resolved = ctx.runtime_types.resolveVar(backing);
                },
                else => break :unwrap,
            },
            else => break :unwrap,
        }
    }

    if (resolved.desc.content == .structure) switch (resolved.desc.content.structure) {
        .tag_union => |tu| {
            var tag_index: usize = 0;
            var have_tag = false;
            if (value.layout.tag == .zst) {
                // Zero-sized tag union - must be the first (and only) tag with no payload
                // Use getSortedTag to ensure consistent tag ordering
                if (getSortedTag(ctx, tu, 0)) |sorted_tag| {
                    const tag_name = ctx.env.getIdent(sorted_tag.name);
                    var out = std.array_list.AlignedManaged(u8, null).init(gpa);
                    errdefer out.deinit();
                    try out.appendSlice(tag_name);
                    return out.toOwnedSlice();
                }
            } else if (value.layout.tag == .scalar) {
                if (value.layout.data.scalar.tag == .int) {
                    // Only treat as tag if value fits in usize (valid tag discriminants are small)
                    if (std.math.cast(usize, value.asI128())) |idx| {
                        tag_index = idx;
                        have_tag = true;
                    }
                }
                // Use getSortedTag to ensure consistent tag ordering
                if (have_tag) {
                    if (getSortedTag(ctx, tu, tag_index)) |sorted_tag| {
                        const tag_name = ctx.env.getIdent(sorted_tag.name);
                        var out = std.array_list.AlignedManaged(u8, null).init(gpa);
                        errdefer out.deinit();
                        try out.appendSlice(tag_name);
                        return out.toOwnedSlice();
                    }
                }
            } else if (value.layout.tag == .struct_) {
                // Struct representing a tag union - check if record-style (named fields) or tuple-style (indices)
                var rec_acc = try value.asRecord(ctx.layout_store);
                if (rec_acc.findFieldIndex(ctx.env.getIdent(ctx.env.idents.tag))) |tag_field_idx| {
                    // Record-style: { tag, payload }
                    const field_rt = try ctx.runtime_types.fresh();
                    const tag_field = try rec_acc.getFieldByIndex(tag_field_idx, field_rt);
                    if (tag_field.layout.tag == .scalar and tag_field.layout.data.scalar.tag == .int) {
                        const tmp_sv = StackValue{ .layout = tag_field.layout, .ptr = tag_field.ptr, .is_initialized = true, .rt_var = undefined };
                        if (std.math.cast(usize, tmp_sv.asI128())) |tag_idx| {
                            tag_index = tag_idx;
                            have_tag = true;
                        }
                    }
                    if (have_tag) {
                        if (getSortedTag(ctx, tu, tag_index)) |sorted_tag| {
                            const tag_name = ctx.env.getIdent(sorted_tag.name);
                            var out = std.array_list.AlignedManaged(u8, null).init(gpa);
                            errdefer out.deinit();
                            try out.appendSlice(tag_name);
                            if (rec_acc.findFieldIndex(ctx.env.getIdent(ctx.env.idents.payload))) |pidx| {
                                const payload_field_rt = try ctx.runtime_types.fresh();
                                const payload = try rec_acc.getFieldByIndex(pidx, payload_field_rt);
                                const arg_vars = ctx.runtime_types.sliceVars(toVarRange(sorted_tag.args));
                                if (arg_vars.len > 0) {
                                    try out.append('(');
                                    if (arg_vars.len == 1) {
                                        const arg_var = arg_vars[0];
                                        const payload_value = StackValue{
                                            .layout = payload.layout,
                                            .ptr = payload.ptr,
                                            .is_initialized = payload.is_initialized,
                                            .rt_var = arg_var,
                                        };
                                        const rendered = try renderValueRocWithType(ctx, payload_value, arg_var);
                                        defer gpa.free(rendered);
                                        try out.appendSlice(rendered);
                                    } else {
                                        const tuple_size = ctx.layout_store.layoutSize(payload.layout);
                                        if (tuple_size == 0 or payload.ptr == null) {
                                            var j: usize = 0;
                                            while (j < arg_vars.len) : (j += 1) {
                                                const rendered = try renderValueRocWithType(
                                                    ctx,
                                                    StackValue{
                                                        .layout = layout.Layout.zst(),
                                                        .ptr = null,
                                                        .is_initialized = true,
                                                        .rt_var = arg_vars[j],
                                                    },
                                                    arg_vars[j],
                                                );
                                                defer gpa.free(rendered);
                                                try out.appendSlice(rendered);
                                                if (j + 1 < arg_vars.len) try out.appendSlice(", ");
                                            }
                                        } else {
                                            var tuple_value = StackValue{
                                                .layout = payload.layout,
                                                .ptr = payload.ptr,
                                                .is_initialized = payload.is_initialized,
                                                .rt_var = undefined,
                                            };
                                            var tup_acc2 = try tuple_value.asTuple(ctx.layout_store);
                                            var j: usize = 0;
                                            while (j < arg_vars.len) : (j += 1) {
                                                const elem_value = try tup_acc2.getElement(j, arg_vars[j]);
                                                const rendered = try renderValueRocWithType(ctx, elem_value, arg_vars[j]);
                                                defer gpa.free(rendered);
                                                try out.appendSlice(rendered);
                                                if (j + 1 < arg_vars.len) try out.appendSlice(", ");
                                            }
                                        }
                                    }
                                    try out.append(')');
                                }
                            }
                            return out.toOwnedSlice();
                        }
                    }
                } else {
                    // Tuple-style: (payload, tag_index)
                    var tup_acc = try value.asTuple(ctx.layout_store);
                    const count = tup_acc.getElementCount();
                    if (count > 0) {
                        const tag_elem = try tup_acc.getElement(count - 1, undefined);
                        if (tag_elem.layout.tag == .scalar and tag_elem.layout.data.scalar.tag == .int) {
                            if (std.math.cast(usize, tag_elem.asI128())) |tag_idx| {
                                tag_index = tag_idx;
                                have_tag = true;
                            }
                        }
                    }
                    if (have_tag) {
                        if (getSortedTag(ctx, tu, tag_index)) |sorted_tag| {
                            const tag_name = ctx.env.getIdent(sorted_tag.name);
                            var out = std.array_list.AlignedManaged(u8, null).init(gpa);
                            errdefer out.deinit();
                            try out.appendSlice(tag_name);
                            const arg_vars = ctx.runtime_types.sliceVars(toVarRange(sorted_tag.args));
                            if (arg_vars.len > 0) {
                                try out.append('(');
                                if (arg_vars.len == 1) {
                                    const arg_var = arg_vars[0];
                                    const payload_elem = try tup_acc.getElement(0, arg_var);
                                    const payload_value = StackValue{
                                        .layout = payload_elem.layout,
                                        .ptr = payload_elem.ptr,
                                        .is_initialized = payload_elem.is_initialized,
                                        .rt_var = arg_var,
                                    };
                                    const rendered = try renderValueRocWithType(ctx, payload_value, arg_var);
                                    defer gpa.free(rendered);
                                    try out.appendSlice(rendered);
                                } else {
                                    const payload_elem = try tup_acc.getElement(0, undefined);
                                    if (payload_elem.layout.tag == .struct_) {
                                        var payload_tup = try payload_elem.asTuple(ctx.layout_store);
                                        var j: usize = 0;
                                        while (j < arg_vars.len) : (j += 1) {
                                            const elem_value = try payload_tup.getElement(j, arg_vars[j]);
                                            const rendered = try renderValueRocWithType(ctx, elem_value, arg_vars[j]);
                                            defer gpa.free(rendered);
                                            try out.appendSlice(rendered);
                                            if (j + 1 < arg_vars.len) try out.appendSlice(", ");
                                        }
                                    } else {
                                        const rendered = try renderValueRoc(ctx, payload_elem);
                                        defer gpa.free(rendered);
                                        try out.appendSlice(rendered);
                                    }
                                }
                                try out.append(')');
                            }
                            return out.toOwnedSlice();
                        }
                    }
                }
            } else if (value.layout.tag == .tag_union) {
                // Tag union with new proper layout: payload at offset 0, discriminant at discriminant_offset
                const tu_idx = value.layout.data.tag_union.idx;
                const tu_data = ctx.layout_store.getTagUnionData(tu_idx);
                const disc_offset = ctx.layout_store.getTagUnionDiscriminantOffset(tu_idx);
                if (value.ptr) |ptr| {
                    const base_ptr: [*]u8 = @ptrCast(ptr);
                    tag_index = tu_data.readDiscriminantFromPtr(base_ptr + disc_offset);
                    have_tag = true;
                }
                // Use getSortedTag to ensure consistent tag ordering
                if (have_tag) {
                    if (getSortedTag(ctx, tu, tag_index)) |sorted_tag| {
                        const tag_name = ctx.env.getIdent(sorted_tag.name);
                        var out = std.array_list.AlignedManaged(u8, null).init(gpa);
                        errdefer out.deinit();
                        try out.appendSlice(tag_name);
                        const arg_vars = ctx.runtime_types.sliceVars(toVarRange(sorted_tag.args));
                        if (arg_vars.len > 0) {
                            try out.append('(');
                            // Payload is at offset 0
                            const payload_ptr: *anyopaque = @ptrCast(value.ptr.?);
                            // Get the stored variant layout from the tag union data
                            // This ensures we use the layout that was actually used when creating the value,
                            // not a potentially different layout computed from type variables.
                            const variants = ctx.layout_store.getTagUnionVariants(tu_data);
                            const stored_payload_layout = ctx.layout_store.getLayout(variants.get(tag_index).payload_layout);
                            if (arg_vars.len == 1) {
                                const arg_var = arg_vars[0];
                                const payload_value = StackValue{
                                    .layout = stored_payload_layout,
                                    .ptr = payload_ptr,
                                    .is_initialized = true,
                                    .rt_var = arg_var,
                                };
                                const rendered = try renderValueRocWithType(ctx, payload_value, arg_var);
                                defer gpa.free(rendered);
                                try out.appendSlice(rendered);
                            } else {
                                // Multiple payloads: use the stored variant layout (should be a tuple)
                                const tuple_size = ctx.layout_store.layoutSize(stored_payload_layout);
                                if (tuple_size == 0) {
                                    var j: usize = 0;
                                    while (j < arg_vars.len) : (j += 1) {
                                        const rendered = try renderValueRocWithType(
                                            ctx,
                                            StackValue{
                                                .layout = layout.Layout.zst(),
                                                .ptr = null,
                                                .is_initialized = true,
                                                .rt_var = arg_vars[j],
                                            },
                                            arg_vars[j],
                                        );
                                        defer gpa.free(rendered);
                                        try out.appendSlice(rendered);
                                        if (j + 1 < arg_vars.len) try out.appendSlice(", ");
                                    }
                                } else {
                                    const tuple_value = StackValue{
                                        .layout = stored_payload_layout,
                                        .ptr = payload_ptr,
                                        .is_initialized = true,
                                        .rt_var = undefined, // not needed - type known from layout
                                    };
                                    var tup_acc = try tuple_value.asTuple(ctx.layout_store);
                                    var j: usize = 0;
                                    while (j < arg_vars.len) : (j += 1) {
                                        const elem_value = try tup_acc.getElement(j, arg_vars[j]);
                                        const rendered = try renderValueRocWithType(ctx, elem_value, arg_vars[j]);
                                        defer gpa.free(rendered);
                                        try out.appendSlice(rendered);
                                        if (j + 1 < arg_vars.len) try out.appendSlice(", ");
                                    }
                                }
                            }
                            try out.append(')');
                        }
                        return out.toOwnedSlice();
                    }
                }
            } else if (value.layout.tag == .list) {
                const elem_type = blk: {
                    const list_resolved = ctx.runtime_types.resolveVar(value.rt_var);
                    if (list_resolved.desc.content == .structure) {
                        if (list_resolved.desc.content.structure == .nominal_type) {
                            const list_nom = list_resolved.desc.content.structure.nominal_type;
                            const list_args = ctx.runtime_types.sliceNominalArgs(list_nom);
                            if (list_args.len > 0) {
                                // List(elem) - the first type arg is the element type
                                break :blk list_args[0];
                            }
                        }
                    }
                    // Fallback: couldn't extract element type, will render without type info
                    break :blk null;
                };

                if (elem_type == null) {
                    // Couldn't extract element type, fall through to layout-only rendering
                } else {
                    var out = std.array_list.AlignedManaged(u8, null).init(gpa);
                    errdefer out.deinit();
                    const roc_list: *const builtins.list.RocList = @ptrCast(@alignCast(value.ptr.?));
                    const len = roc_list.len();
                    try out.append('[');
                    if (len > 0) {
                        const elem_layout_idx = value.layout.data.list;
                        const elem_layout = ctx.layout_store.getLayout(elem_layout_idx);
                        const elem_size = ctx.layout_store.layoutSize(elem_layout);
                        var i: usize = 0;
                        while (i < len) : (i += 1) {
                            if (roc_list.bytes) |bytes| {
                                const elem_ptr: *anyopaque = @ptrCast(bytes + i * elem_size);
                                const elem_val = StackValue{
                                    .layout = elem_layout,
                                    .ptr = elem_ptr,
                                    .is_initialized = true,
                                    .rt_var = elem_type.?,
                                };
                                const rendered = try renderValueRocWithType(ctx, elem_val, elem_type.?);
                                defer gpa.free(rendered);
                                try out.appendSlice(rendered);
                                if (i + 1 < len) try out.appendSlice(", ");
                            }
                        }
                    }
                    try out.append(']');
                    return out.toOwnedSlice();
                }
            }
        },
        .record => |rec| {
            // Gather all record fields by following the extension chain
            var all_fields = std.array_list.AlignedManaged(types.RecordField, null).init(gpa);
            defer all_fields.deinit();

            // Add fields from the initial record
            const initial_fields = ctx.runtime_types.getRecordFieldsSlice(rec.fields);
            for (initial_fields.items(.name), initial_fields.items(.var_)) |name, var_| {
                try all_fields.append(.{ .name = name, .var_ = var_ });
            }

            // Follow the extension chain to gather all fields
            var ext = rec.ext;
            var is_valid = true;
            while (is_valid) {
                const ext_resolved = ctx.runtime_types.resolveVar(ext);
                switch (ext_resolved.desc.content) {
                    .structure => |flat_type| switch (flat_type) {
                        .record => |ext_record| {
                            const ext_fields = ctx.runtime_types.getRecordFieldsSlice(ext_record.fields);
                            for (ext_fields.items(.name), ext_fields.items(.var_)) |name, var_| {
                                try all_fields.append(.{ .name = name, .var_ = var_ });
                            }
                            ext = ext_record.ext;
                        },
                        .empty_record => break, // Reached the end of the extension chain
                        else => {
                            is_valid = false;
                        },
                    },
                    .alias => |alias| {
                        // Follow alias to its backing type
                        ext = ctx.runtime_types.getAliasBackingVar(alias);
                    },
                    else => {
                        is_valid = false;
                    },
                }
            }

            if (is_valid and all_fields.items.len > 0) {
                var out = std.array_list.AlignedManaged(u8, null).init(gpa);
                errdefer out.deinit();
                try out.appendSlice("{ ");
                var acc = try value.asRecord(ctx.layout_store);
                for (all_fields.items, 0..) |f, i| {
                    const name_text = ctx.env.getIdent(f.name);
                    try out.appendSlice(name_text);
                    try out.appendSlice(": ");
                    const idx = acc.findFieldIndex(name_text) orelse {
                        std.debug.panic("Record field not found in layout: type says field '{s}' exists but layout doesn't have it", .{name_text});
                    };
                    const field_rt = try ctx.runtime_types.fresh();
                    const field_val = try acc.getFieldByIndex(idx, field_rt);
                    const rendered = try renderValueRocWithType(ctx, field_val, f.var_);
                    defer gpa.free(rendered);
                    try out.appendSlice(rendered);
                    if (i + 1 < all_fields.items.len) try out.appendSlice(", ");
                }
                try out.appendSlice(" }");
                return out.toOwnedSlice();
            }
            // Handle empty records (zero fields)
            if (is_valid and all_fields.items.len == 0) {
                return try gpa.dupe(u8, "{}");
            }
            unreachable;
        },
        .tuple => |tuple| {
            const elem_types = ctx.runtime_types.sliceVars(tuple.elems);
            if (elem_types.len == 0) {
                return try gpa.dupe(u8, "{}");
            }

            var out = std.array_list.AlignedManaged(u8, null).init(gpa);
            errdefer out.deinit();
            try out.append('(');

            const tuple_size = ctx.layout_store.layoutSize(value.layout);
            if (tuple_size == 0 or value.ptr == null) {
                // Zero-sized tuple payloads (e.g. all-ZST elements) can have null pointers.
                for (elem_types, 0..) |elem_type, i| {
                    const rendered = try renderValueRocWithType(
                        ctx,
                        StackValue{
                            .layout = layout.Layout.zst(),
                            .ptr = null,
                            .is_initialized = true,
                            .rt_var = elem_type,
                        },
                        elem_type,
                    );
                    defer gpa.free(rendered);
                    try out.appendSlice(rendered);
                    if (i + 1 < elem_types.len) try out.appendSlice(", ");
                }
            } else {
                var tup_acc = try value.asTuple(ctx.layout_store);
                for (elem_types, 0..) |elem_type, i| {
                    const elem_value = try tup_acc.getElement(i, elem_type);
                    const rendered = try renderValueRocWithType(ctx, elem_value, elem_type);
                    defer gpa.free(rendered);
                    try out.appendSlice(rendered);
                    if (i + 1 < elem_types.len) try out.appendSlice(", ");
                }
            }

            try out.append(')');
            return out.toOwnedSlice();
        },
        .empty_record => {
            return try gpa.dupe(u8, "{}");
        },
        .fn_pure, .fn_effectful, .fn_unbound => {
            return try gpa.dupe(u8, "<function>");
        },
        .empty_tag_union => {
            return try gpa.dupe(u8, "<empty_tag_union>");
        },
        else => {
            // Tuple, record_unbound, etc. — fall through to layout-based rendering
        },
    };

    // Fallback: render using layout only (covers flex/rigid type vars, tuples, etc.)
    return renderValueRoc(ctx, value);
}

/// Render `value` using only its layout (without additional type information).
/// Delegates to the interpreter-specific `RocValue.format()` for canonical formatting.
pub fn renderValueRoc(ctx: *RenderCtx, value: StackValue) ![]u8 {
    // Unit values can be represented as zero-sized structs in runtime layouts.
    // Render these consistently as `{}` for Roc-facing output.
    if (value.layout.tag == .zst or
        (value.layout.tag == .struct_ and ctx.layout_store.layoutSize(value.layout) == 0))
    {
        return try ctx.allocator.dupe(u8, "{}");
    }

    const roc_val = interpreter_values.RocValue{
        .ptr = if (value.ptr) |p| @ptrCast(p) else null,
        .lay = value.layout,
    };
    const fmt_ctx = interpreter_values.RocValue.FormatContext{
        .layout_store = ctx.layout_store,
        .ident_store = ctx.env.getIdentStoreConst(),
    };
    return roc_val.format(ctx.allocator, fmt_ctx);
}
