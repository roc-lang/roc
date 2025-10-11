//! Helpers for rendering interpreter values back into readable Roc syntax.

const std = @import("std");
const types = @import("types");
const can = @import("can");
const layout = @import("layout");
const builtins = @import("builtins");
const StackValue = @import("StackValue.zig");
const RocDec = builtins.dec.RocDec;
const TypeScope = types.TypeScope;

fn toVarRange(range: anytype) types.Var.SafeList.Range {
    const RangeType = types.Var.SafeList.Range;
    if (comptime @hasField(@TypeOf(range), "nonempty")) {
        return @field(range, "nonempty");
    }
    return @as(RangeType, range);
}

/// Shared rendering context that provides allocator, module environment, and runtime caches.
pub const RenderCtx = struct {
    allocator: std.mem.Allocator,
    env: *can.ModuleEnv,
    runtime_types: *types.store.Store,
    layout_store: *layout.Store,
    type_scope: *const TypeScope,
};

/// Render `value` using the supplied runtime type variable, following alias/nominal backing.
pub fn renderValueRocWithType(ctx: *RenderCtx, value: StackValue, rt_var: types.Var) ![]u8 {
    const gpa = ctx.allocator;
    var resolved = ctx.runtime_types.resolveVar(rt_var);

    // Check if this is Bool before unwrapping (special case for bool display)
    if (resolved.desc.content == .structure) {
        if (resolved.desc.content.structure == .nominal_type) {
            const nominal = resolved.desc.content.structure.nominal_type;
            const type_name = ctx.env.getIdent(nominal.ident.ident_idx);
            if (std.mem.eql(u8, type_name, "Bool")) {
                // Bool is represented as a scalar bool (0 or 1) - render as True/False
                if (value.layout.tag == .scalar and value.layout.data.scalar.tag == .bool) {
                    const b: *const u8 = @ptrCast(@alignCast(value.ptr.?));
                    return if (b.* != 0)
                        try gpa.dupe(u8, "True")
                    else
                        try gpa.dupe(u8, "False");
                }
            }
        }
    }

    // unwrap aliases/nominals
    unwrap: while (true) {
        switch (resolved.desc.content) {
            .alias => |al| {
                const backing = ctx.runtime_types.getAliasBackingVar(al);
                resolved = ctx.runtime_types.resolveVar(backing);
            },
            .structure => |st| switch (st) {
                .nominal_type => |nt| {
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
            const tags = ctx.runtime_types.getTagsSlice(tu.tags);
            var tag_index: usize = 0;
            var have_tag = false;
            if (value.layout.tag == .scalar) {
                if (value.layout.data.scalar.tag == .bool) {
                    const b: *const u8 = @ptrCast(@alignCast(value.ptr.?));
                    tag_index = if (b.* != 0) 1 else 0;
                    have_tag = true;
                } else if (value.layout.data.scalar.tag == .int) {
                    tag_index = @intCast(value.asI128());
                    have_tag = true;
                }
                if (have_tag and tag_index < tags.len) {
                    const tag_name = ctx.env.getIdent(tags.items(.name)[tag_index]);
                    var out = std.ArrayList(u8).init(gpa);
                    errdefer out.deinit();
                    try out.appendSlice(tag_name);
                    return out.toOwnedSlice();
                }
            } else if (value.layout.tag == .record) {
                var acc = try value.asRecord(ctx.layout_store);
                if (acc.findFieldIndex(ctx.env, "tag")) |idx| {
                    const tag_field = try acc.getFieldByIndex(idx);
                    if (tag_field.layout.tag == .scalar and tag_field.layout.data.scalar.tag == .int) {
                        const tmp_sv = StackValue{ .layout = tag_field.layout, .ptr = tag_field.ptr, .is_initialized = true };
                        tag_index = @intCast(tmp_sv.asI128());
                        have_tag = true;
                    } else if (tag_field.layout.tag == .scalar and tag_field.layout.data.scalar.tag == .bool) {
                        const b: *const u8 = @ptrCast(@alignCast(tag_field.ptr.?));
                        tag_index = if (b.* != 0) 1 else 0;
                        have_tag = true;
                    }
                }
                if (have_tag and tag_index < tags.len) {
                    const tag_name = ctx.env.getIdent(tags.items(.name)[tag_index]);
                    var out = std.ArrayList(u8).init(gpa);
                    errdefer out.deinit();
                    try out.appendSlice(tag_name);
                    if (acc.findFieldIndex(ctx.env, "payload")) |pidx| {
                        const payload = try acc.getFieldByIndex(pidx);
                        const args_range = tags.items(.args)[tag_index];
                        const arg_vars = ctx.runtime_types.sliceVars(toVarRange(args_range));
                        if (arg_vars.len > 0) {
                            try out.append('(');
                            if (arg_vars.len == 1) {
                                const arg_var = arg_vars[0];
                                const layout_idx = try ctx.layout_store.addTypeVar(arg_var, ctx.type_scope);
                                const arg_layout = ctx.layout_store.getLayout(layout_idx);
                                const payload_value = StackValue{
                                    .layout = arg_layout,
                                    .ptr = payload.ptr,
                                    .is_initialized = payload.is_initialized,
                                };
                                const rendered = try renderValueRocWithType(ctx, payload_value, arg_var);
                                defer gpa.free(rendered);
                                try out.appendSlice(rendered);
                            } else {
                                var elem_layouts = try ctx.allocator.alloc(layout.Layout, arg_vars.len);
                                defer ctx.allocator.free(elem_layouts);
                                var i: usize = 0;
                                while (i < arg_vars.len) : (i += 1) {
                                    const idx = try ctx.layout_store.addTypeVar(arg_vars[i], ctx.type_scope);
                                    elem_layouts[i] = ctx.layout_store.getLayout(idx);
                                }
                                const tuple_idx = try ctx.layout_store.putTuple(elem_layouts);
                                const tuple_layout = ctx.layout_store.getLayout(tuple_idx);
                                const tuple_size = ctx.layout_store.layoutSize(tuple_layout);
                                var tuple_value = StackValue{
                                    .layout = tuple_layout,
                                    .ptr = payload.ptr,
                                    .is_initialized = payload.is_initialized,
                                };
                                if (tuple_size == 0 or payload.ptr == null) {
                                    var j: usize = 0;
                                    while (j < arg_vars.len) : (j += 1) {
                                        const rendered = try renderValueRocWithType(
                                            ctx,
                                            StackValue{
                                                .layout = elem_layouts[j],
                                                .ptr = null,
                                                .is_initialized = true,
                                            },
                                            arg_vars[j],
                                        );
                                        defer gpa.free(rendered);
                                        try out.appendSlice(rendered);
                                        if (j + 1 < arg_vars.len) try out.appendSlice(", ");
                                    }
                                } else {
                                    var tup_acc = try tuple_value.asTuple(ctx.layout_store);
                                    var j: usize = 0;
                                    while (j < arg_vars.len) : (j += 1) {
                                        const sorted_idx = tup_acc.findElementIndexByOriginal(j) orelse return error.TypeMismatch;
                                        const elem_value = try tup_acc.getElement(sorted_idx);
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
        },
        .nominal_type => |nominal| {
            const type_name = ctx.env.getIdent(nominal.ident.ident_idx);
            if (std.mem.eql(u8, type_name, "Box")) {
                const args_range = nominal.vars;
                const arg_vars = ctx.runtime_types.sliceVars(toVarRange(args_range));
                if (arg_vars.len != 1) return error.TypeMismatch;
                const payload_var = arg_vars[0];

                var out = std.ArrayList(u8).init(gpa);
                errdefer out.deinit();
                try out.appendSlice("Box(");

                const payload_layout_idx = try ctx.layout_store.addTypeVar(payload_var, ctx.type_scope);
                const payload_layout = ctx.layout_store.getLayout(payload_layout_idx);
                const payload_size = ctx.layout_store.layoutSize(payload_layout);

                var payload_value = StackValue{
                    .layout = payload_layout,
                    .ptr = null,
                    .is_initialized = true,
                };

                switch (value.layout.tag) {
                    .box => {
                        const elem_layout = ctx.layout_store.getLayout(value.layout.data.box);
                        const data_ptr_opt = value.boxDataPointer() orelse return error.TypeMismatch;
                        if (!std.meta.eql(elem_layout, payload_layout)) {
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
                    else => return error.TypeMismatch,
                }

                try out.append(')');
                return out.toOwnedSlice();
            }
        },
        .record => |rec| {
            const ext_resolved = ctx.runtime_types.resolveVar(rec.ext);
            const use_placeholder = switch (ext_resolved.desc.content) {
                .structure => |st| st != .empty_record,
                else => true,
            };
            if (use_placeholder) {
                return try gpa.dupe(u8, "<record>");
            }
            var out = std.ArrayList(u8).init(gpa);
            errdefer out.deinit();
            try out.appendSlice("{ ");
            var acc = try value.asRecord(ctx.layout_store);
            const fields = ctx.runtime_types.getRecordFieldsSlice(rec.fields);
            var i: usize = 0;
            while (i < fields.len) : (i += 1) {
                const f = fields.get(i);
                const name_text = ctx.env.getIdent(f.name);
                try out.appendSlice(name_text);
                try out.appendSlice(": ");
                if (acc.findFieldIndex(ctx.env, name_text)) |idx| {
                    const field_val = try acc.getFieldByIndex(idx);
                    const rendered = try renderValueRoc(ctx, field_val);
                    defer gpa.free(rendered);
                    try out.appendSlice(rendered);
                } else {
                    try out.appendSlice("<missing>");
                }
                if (i + 1 < fields.len) try out.appendSlice(", ");
            }
            try out.appendSlice(" }");
            return out.toOwnedSlice();
        },
        else => {},
    };
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
                var buf = std.ArrayList(u8).init(gpa);
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
                const i = value.asI128();
                return try std.fmt.allocPrint(gpa, "{d}", .{i});
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
                        const ptr = @as(*const RocDec, @ptrCast(@alignCast(value.ptr.?)));
                        return try renderDecimal(gpa, ptr.*);
                    },
                };
            },
            else => {},
        }
    }
    if (value.layout.tag == .tuple) {
        var out = std.ArrayList(u8).init(gpa);
        errdefer out.deinit();
        try out.append('(');
        var acc = try value.asTuple(ctx.layout_store);
        const count = acc.getElementCount();
        var i: usize = 0;
        while (i < count) : (i += 1) {
            const elem = try acc.getElement(i);
            const rendered = try renderValueRoc(ctx, elem);
            defer gpa.free(rendered);
            try out.appendSlice(rendered);
            if (i + 1 < count) try out.appendSlice(", ");
        }
        try out.append(')');
        return out.toOwnedSlice();
    }
    if (value.layout.tag == .list_of_zst) {
        return try gpa.dupe(u8, "<list_of_zst>");
    }
    if (value.layout.tag == .record) {
        var out = std.ArrayList(u8).init(gpa);
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
            const field_val = StackValue{ .layout = field_layout, .ptr = field_ptr, .is_initialized = true };
            const rendered = try renderValueRoc(ctx, field_val);
            defer gpa.free(rendered);
            try out.appendSlice(rendered);
            if (i + 1 < fields.len) try out.appendSlice(", ");
        }
        try out.appendSlice(" }");
        return out.toOwnedSlice();
    }
    return try std.fmt.allocPrint(gpa, "<unsupported>", .{});
}

fn renderDecimal(gpa: std.mem.Allocator, dec: RocDec) ![]u8 {
    if (dec.num == 0) {
        return try gpa.dupe(u8, "0");
    }

    var out = std.ArrayList(u8).init(gpa);
    errdefer out.deinit();

    var num = dec.num;
    if (num < 0) {
        try out.append('-');
        num = -num;
    }

    const one = RocDec.one_point_zero_i128;
    const integer_part = @divTrunc(num, one);
    const fractional_part = @rem(num, one);

    try std.fmt.format(out.writer(), "{d}", .{integer_part});

    if (fractional_part == 0) {
        return out.toOwnedSlice();
    }

    try out.writer().writeByte('.');

    const decimal_places: usize = @as(usize, RocDec.decimal_places);
    var digits: [decimal_places]u8 = undefined;
    @memset(digits[0..], '0');
    var remaining = fractional_part;
    var idx: usize = decimal_places;
    while (idx > 0) : (idx -= 1) {
        const digit: u8 = @intCast(@mod(remaining, 10));
        digits[idx - 1] = digit + '0';
        remaining = @divTrunc(remaining, 10);
    }

    var end: usize = decimal_places;
    while (end > 1 and digits[end - 1] == '0') {
        end -= 1;
    }

    try out.writer().writeAll(digits[0..end]);
    return out.toOwnedSlice();
}
