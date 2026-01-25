//! Helpers for rendering interpreter values back into readable Roc syntax.

const std = @import("std");
const types = @import("types");
const can = @import("can");
const layout = @import("layout");
const builtins = @import("builtins");
const StackValue = @import("StackValue.zig");
const RocDec = builtins.dec.RocDec;
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
    const tags = ctx.runtime_types.getTagsSlice(tag_union.tags);
    if (tags.len == 0) return null;

    // Fast path: if tags are already sorted, just return directly
    // (check first two tags - if they're in order, likely all are)
    if (tags.len <= 1) {
        return if (tag_index < tags.len)
            types.Tag{ .name = tags.items(.name)[tag_index], .args = tags.items(.args)[tag_index] }
        else
            null;
    }

    // Get ident store for sorting
    const ident_store = ctx.env.common.getIdentStore();

    // Always copy and sort to ensure consistent ordering
    // We cannot rely on storage order because the same source type may be translated
    // multiple times with different cache generations, resulting in different
    // runtime type vars with potentially different tag ordering.

    // Tags are NOT sorted - need to copy and sort
    // Use a stack buffer for small tag unions, allocate for larger ones
    var stack_buf: [16]types.Tag = undefined;
    var sorted_tags: []types.Tag = undefined;
    var heap_allocated = false;

    if (tags.len <= stack_buf.len) {
        sorted_tags = stack_buf[0..tags.len];
    } else {
        sorted_tags = ctx.allocator.alloc(types.Tag, tags.len) catch return null;
        heap_allocated = true;
    }
    defer if (heap_allocated) ctx.allocator.free(sorted_tags);

    // Copy tags
    const names = tags.items(.name);
    const args = tags.items(.args);
    for (names, args, 0..) |name, arg, i| {
        sorted_tags[i] = types.Tag{ .name = name, .args = arg };
    }

    // Sort alphabetically
    std.mem.sort(types.Tag, sorted_tags, ident_store, types.Tag.sortByNameAsc);

    return if (tag_index < sorted_tags.len) sorted_tags[tag_index] else null;
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
    /// When true, render whole-number Dec values without .0 if the type is an unbound numeral.
    /// Used by the REPL for cleaner output.
    strip_unbound_numeral_decimal: bool = false,
};

/// Render `value` using the supplied runtime type variable, following alias/nominal backing.
pub fn renderValueRocWithType(ctx: *RenderCtx, value: StackValue, rt_var: types.Var) ![]u8 {
    const gpa = ctx.allocator;
    var resolved = ctx.runtime_types.resolveVar(rt_var);

    // Check layout first for special rendering cases
    // Str has .str layout, Bool has .int .u8 layout
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
                    if (nt.ident.ident_idx == ctx.env.idents.box) {
                        // Use sliceNominalArgs which skips the backing var (first element)
                        const arg_vars = ctx.runtime_types.sliceNominalArgs(nt);
                        if (arg_vars.len != 1) {
                            return error.TypeMismatch;
                        }
                        const payload_var = arg_vars[0];

                        var out = std.array_list.AlignedManaged(u8, null).init(gpa);
                        errdefer out.deinit();
                        try out.appendSlice("Box(");

                        const payload_layout_idx = try ctx.layout_store.addTypeVar(0, payload_var, ctx.type_scope);
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
                            .scalar => {
                                // Box might be stored as a scalar opaque_ptr when inside other structures
                                // Try to interpret it as a pointer to boxed data
                                if (value.layout.data.scalar.tag == .opaque_ptr and payload_size > 0) {
                                    if (value.ptr) |ptr| {
                                        const data_ptr: *usize = @ptrCast(@alignCast(ptr));
                                        payload_value.ptr = @as(*anyopaque, @ptrFromInt(data_ptr.*));
                                        const rendered_payload = try renderValueRocWithType(ctx, payload_value, payload_var);
                                        defer gpa.free(rendered_payload);
                                        try out.appendSlice(rendered_payload);
                                    } else {
                                        try out.appendSlice("<null>");
                                    }
                                } else {
                                    try out.appendSlice("<unsupported>");
                                }
                            },
                            else => {
                                // Fallback - render as unsupported
                                try out.appendSlice("<unsupported>");
                            },
                        }

                        try out.append(')');
                        return out.toOwnedSlice();
                    }
                    // Special handling for List before unwrapping - render with element type info
                    if (nt.ident.ident_idx == ctx.env.idents.list) {
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
                            // list_of_zst - elements have no data, just render count if non-empty
                            const roc_list: *const builtins.list.RocList = @ptrCast(@alignCast(value.ptr.?));
                            const len = roc_list.len();
                            if (len > 0) {
                                // For ZST lists, render each element using type info
                                const elem_layout = layout.Layout.zst();
                                var i: usize = 0;
                                while (i < len) : (i += 1) {
                                    const elem_val = StackValue{
                                        .layout = elem_layout,
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
            } else if (value.layout.tag == .tuple) {
                // Tag union stored as tuple: (payload, tag_index) or (payload_tuple, tag_index)
                // The last element of the tuple is the tag discriminant
                var tup_acc = try value.asTuple(ctx.layout_store);
                const count = tup_acc.getElementCount();
                if (count > 0) {
                    // Get tag index from the last element
                    // rt_var not needed for tag discriminant access (it's always an integer)
                    const tag_elem = try tup_acc.getElement(count - 1, undefined);
                    if (tag_elem.layout.tag == .scalar and tag_elem.layout.data.scalar.tag == .int) {
                        if (std.math.cast(usize, tag_elem.asI128())) |tag_idx| {
                            tag_index = tag_idx;
                            have_tag = true;
                        }
                    }
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
                            if (arg_vars.len == 1) {
                                // Single payload: first element
                                // Use the stored layout from the tuple element, not from type variables.
                                // This ensures we use the layout that was actually used when creating the value.
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
                                // Multiple payloads: first element is a nested tuple containing all payload args
                                // rt_var undefined for tuple access (we have the individual element types)
                                const payload_elem = try tup_acc.getElement(0, undefined);
                                if (payload_elem.layout.tag == .tuple) {
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
                                    // Fallback: render the raw payload
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
            } else if (value.layout.tag == .record) {
                var acc = try value.asRecord(ctx.layout_store);
                if (acc.findFieldIndex(ctx.env.idents.tag)) |idx| {
                    const field_rt = try ctx.runtime_types.fresh();
                    const tag_field = try acc.getFieldByIndex(idx, field_rt);
                    if (tag_field.layout.tag == .scalar and tag_field.layout.data.scalar.tag == .int) {
                        const tmp_sv = StackValue{ .layout = tag_field.layout, .ptr = tag_field.ptr, .is_initialized = true, .rt_var = undefined };
                        // Only treat as tag if value fits in usize (valid tag discriminants are small)
                        if (std.math.cast(usize, tmp_sv.asI128())) |tag_idx| {
                            tag_index = tag_idx;
                            have_tag = true;
                        }
                    }
                }
                // Use getSortedTag to ensure consistent tag ordering
                if (have_tag) {
                    if (getSortedTag(ctx, tu, tag_index)) |sorted_tag| {
                        const tag_name = ctx.env.getIdent(sorted_tag.name);
                        var out = std.array_list.AlignedManaged(u8, null).init(gpa);
                        errdefer out.deinit();
                        try out.appendSlice(tag_name);
                        if (acc.findFieldIndex(ctx.env.idents.payload)) |pidx| {
                            const field_rt = try ctx.runtime_types.fresh();
                            const payload = try acc.getFieldByIndex(pidx, field_rt);
                            const arg_vars = ctx.runtime_types.sliceVars(toVarRange(sorted_tag.args));
                            if (arg_vars.len > 0) {
                                try out.append('(');
                                if (arg_vars.len == 1) {
                                    const arg_var = arg_vars[0];
                                    // Use the stored payload layout from the record field, not from type variables.
                                    // This ensures we use the layout that was actually used when creating the value.
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
                                    // Multiple payloads: use the stored payload layout (should be a tuple)
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
                                            .rt_var = undefined, // not needed - type known from layout
                                        };
                                        var tup_acc = try tuple_value.asTuple(ctx.layout_store);
                                        var j: usize = 0;
                                        while (j < arg_vars.len) : (j += 1) {
                                            const sorted_idx = tup_acc.findElementIndexByOriginal(j) orelse return error.TypeMismatch;
                                            const elem_value = try tup_acc.getElement(sorted_idx, arg_vars[j]);
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
                                        const sorted_idx = tup_acc.findElementIndexByOriginal(j) orelse return error.TypeMismatch;
                                        const elem_value = try tup_acc.getElement(sorted_idx, arg_vars[j]);
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
                    const idx = acc.findFieldIndex(f.name) orelse {
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
            // Fall through to renderValueRoc which can use layout info
        },
        .empty_record => {
            return try gpa.dupe(u8, "{}");
        },
        .fn_pure, .fn_effectful, .fn_unbound => {
            return try gpa.dupe(u8, "<function>");
        },
        else => {},
    };

    // Handle Dec values specially when stripping unbound numeral decimals in REPL mode.
    // When enabled, whole-number Dec values (like 2.0) display without the decimal (as 2).
    if (ctx.strip_unbound_numeral_decimal and value.layout.tag == .scalar) {
        const scalar = value.layout.data.scalar;
        if (scalar.tag == .frac and scalar.data.frac == .dec) {
            const dec = @as(*const RocDec, @ptrCast(@alignCast(value.ptr.?))).*;
            // Check if this is a whole number (no fractional part)
            if (@rem(@abs(dec.num), RocDec.one_point_zero_i128) == 0) {
                const whole = @divTrunc(dec.num, RocDec.one_point_zero_i128);
                return try std.fmt.allocPrint(gpa, "{d}", .{whole});
            }
        }
    }

    return try renderValueRoc(ctx, value);
}

/// Render `value` using only its layout (without additional type information).
pub fn renderValueRoc(ctx: *RenderCtx, value: StackValue) ![]u8 {
    const gpa = ctx.allocator;
    if (value.layout.tag == .scalar) {
        const scalar = value.layout.data.scalar;
        switch (scalar.tag) {
            .str => {
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
            },
            .int => {
                // Check if this is an unsigned type that needs asU128
                const precision = value.getIntPrecision();
                return switch (precision) {
                    .u64, .u128 => try std.fmt.allocPrint(gpa, "{d}", .{value.asU128()}),
                    else => try std.fmt.allocPrint(gpa, "{d}", .{value.asI128()}),
                };
            },
            .frac => {
                std.debug.assert(value.ptr != null);
                return switch (scalar.data.frac) {
                    .f32 => {
                        const ptr = @as(*const f32, @ptrCast(@alignCast(value.ptr.?)));
                        return try std.fmt.allocPrint(gpa, "{d}", .{@as(f64, ptr.*)});
                    },
                    .f64 => {
                        const ptr = @as(*const f64, @ptrCast(@alignCast(value.ptr.?)));
                        return try std.fmt.allocPrint(gpa, "{d}", .{ptr.*});
                    },
                    .dec => {
                        const dec = @as(*const RocDec, @ptrCast(@alignCast(value.ptr.?))).*;
                        // In REPL mode, strip .0 from whole-number Dec values
                        if (ctx.strip_unbound_numeral_decimal and
                            @rem(@abs(dec.num), RocDec.one_point_zero_i128) == 0)
                        {
                            const whole = @divTrunc(dec.num, RocDec.one_point_zero_i128);
                            return try std.fmt.allocPrint(gpa, "{d}", .{whole});
                        }
                        var buf: [RocDec.max_str_length]u8 = undefined;
                        const slice = dec.format_to_buf(&buf);
                        return try gpa.dupe(u8, slice);
                    },
                };
            },
            else => {},
        }
    }
    if (value.layout.tag == .tuple) {
        var out = std.array_list.AlignedManaged(u8, null).init(gpa);
        errdefer out.deinit();
        try out.append('(');
        var acc = try value.asTuple(ctx.layout_store);
        const count = acc.getElementCount();
        var i: usize = 0;
        while (i < count) : (i += 1) {
            // rt_var undefined (no type info available in this context)
            const elem = try acc.getElement(i, undefined);
            const rendered = try renderValueRoc(ctx, elem);
            defer gpa.free(rendered);
            try out.appendSlice(rendered);
            if (i + 1 < count) try out.appendSlice(", ");
        }
        try out.append(')');
        return out.toOwnedSlice();
    }
    if (value.layout.tag == .list) {
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
                    const elem_val = StackValue{ .layout = elem_layout, .ptr = elem_ptr, .is_initialized = true, .rt_var = undefined };
                    const rendered = try renderValueRoc(ctx, elem_val);
                    defer gpa.free(rendered);
                    try out.appendSlice(rendered);
                    if (i + 1 < len) try out.appendSlice(", ");
                }
            }
        }
        try out.append(']');
        return out.toOwnedSlice();
    }
    if (value.layout.tag == .list_of_zst) {
        // list_of_zst is used for empty lists - render as []
        const roc_list: *const builtins.list.RocList = @ptrCast(@alignCast(value.ptr.?));
        const len = roc_list.len();
        if (len == 0) {
            return try gpa.dupe(u8, "[]");
        }
        // Non-empty list of ZST - show count
        return try std.fmt.allocPrint(gpa, "[<{d} zero-sized elements>]", .{len});
    }
    if (value.layout.tag == .record) {
        var out = std.array_list.AlignedManaged(u8, null).init(gpa);
        errdefer out.deinit();
        const rec_data = ctx.layout_store.getRecordData(value.layout.data.record.idx);
        if (rec_data.fields.count == 0) {
            try out.appendSlice("{}");
            return out.toOwnedSlice();
        }
        try out.appendSlice("{ ");
        const fields = ctx.layout_store.record_fields.sliceRange(rec_data.getFields());
        var i: usize = 0;
        while (i < fields.len) : (i += 1) {
            const fld = fields.get(i);
            const name_text = ctx.env.getIdent(fld.name);
            try out.appendSlice(name_text);
            try out.appendSlice(": ");
            const offset = ctx.layout_store.getRecordFieldOffset(value.layout.data.record.idx, @intCast(i));
            const field_layout = ctx.layout_store.getLayout(fld.layout);
            const base_ptr: [*]u8 = @ptrCast(@alignCast(value.ptr.?));
            const field_ptr: *anyopaque = @ptrCast(base_ptr + offset);
            const field_val = StackValue{ .layout = field_layout, .ptr = field_ptr, .is_initialized = true, .rt_var = undefined };
            const rendered = try renderValueRoc(ctx, field_val);
            defer gpa.free(rendered);
            try out.appendSlice(rendered);
            if (i + 1 < fields.len) try out.appendSlice(", ");
        }
        try out.appendSlice(" }");
        return out.toOwnedSlice();
    }
    if (value.layout.tag == .box) {
        // Layout-only Box rendering: we know it's a Box from layout but don't have type info
        var out = std.array_list.AlignedManaged(u8, null).init(gpa);
        errdefer out.deinit();
        try out.appendSlice("Box(");

        // Get the element layout and render the boxed value
        const elem_layout_idx = value.layout.data.box;
        const elem_layout = ctx.layout_store.getLayout(elem_layout_idx);
        const elem_size = ctx.layout_store.layoutSize(elem_layout);

        if (elem_size > 0) {
            if (value.getBoxedData()) |data_ptr| {
                const elem_val = StackValue{
                    .layout = elem_layout,
                    .ptr = @ptrCast(data_ptr),
                    .is_initialized = true,
                    .rt_var = undefined,
                };
                const rendered = try renderValueRoc(ctx, elem_val);
                defer gpa.free(rendered);
                try out.appendSlice(rendered);
            } else {
                try out.appendSlice("<null>");
            }
        } else {
            // Zero-sized element
            const elem_val = StackValue{
                .layout = elem_layout,
                .ptr = null,
                .is_initialized = true,
                .rt_var = undefined,
            };
            const rendered = try renderValueRoc(ctx, elem_val);
            defer gpa.free(rendered);
            try out.appendSlice(rendered);
        }

        try out.append(')');
        return out.toOwnedSlice();
    }
    if (value.layout.tag == .box_of_zst) {
        // Box of zero-sized type - render as Box({}) or similar
        return try gpa.dupe(u8, "Box({})");
    }
    if (value.layout.tag == .tag_union) {
        // Layout-only fallback for tag_union: show discriminant and raw payload
        const tu_idx = value.layout.data.tag_union.idx;
        const tu_data = ctx.layout_store.getTagUnionData(tu_idx);
        const disc_offset = ctx.layout_store.getTagUnionDiscriminantOffset(tu_idx);
        var out = std.array_list.AlignedManaged(u8, null).init(gpa);
        errdefer out.deinit();
        if (value.ptr) |ptr| {
            const base_ptr: [*]u8 = @ptrCast(ptr);
            const discriminant = tu_data.readDiscriminantFromPtr(base_ptr + disc_offset);
            try std.fmt.format(out.writer(), "<tag_union variant={d}>", .{discriminant});
        } else {
            try out.appendSlice("<tag_union>");
        }
        return out.toOwnedSlice();
    }
    return try gpa.dupe(u8, "<unsupported>");
}
