const std = @import("std");
const types = @import("types");
const can = @import("can");
const layout = @import("layout");
const StackValue = @import("StackValue.zig");

pub const RenderCtx = struct {
    allocator: std.mem.Allocator,
    env: *can.ModuleEnv,
    runtime_types: *types.store.Store,
    layout_store: *layout.Store,
};

pub fn renderValueRocWithType(ctx: *RenderCtx, value: StackValue, rt_var: types.Var) ![]u8 {
    const gpa = ctx.allocator;
    var resolved = ctx.runtime_types.resolveVar(rt_var);
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
                        const psize = ctx.layout_store.layoutSize(payload.layout);
                        if (psize > 0) {
                            try out.append('(');
                            if (payload.layout.tag == .tuple) {
                                var tup = try payload.asTuple(ctx.layout_store);
                                const count = tup.getElementCount();
                                var k: usize = 0;
                                while (k < count) : (k += 1) {
                                    const elem = try tup.getElement(k);
                                    const r = try renderValueRoc(ctx, elem);
                                    defer gpa.free(r);
                                    try out.appendSlice(r);
                                    if (k + 1 < count) try out.appendSlice(", ");
                                }
                            } else {
                                const rendered = try renderValueRoc(ctx, payload);
                                defer gpa.free(rendered);
                                try out.appendSlice(rendered);
                            }
                            try out.append(')');
                        }
                    }
                    return out.toOwnedSlice();
                }
            }
        },
        .record => |rec| {
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

pub fn renderValueRoc(ctx: *RenderCtx, value: StackValue) ![]u8 {
    const gpa = ctx.allocator;
    if (value.layout.tag == .scalar) {
        switch (value.layout.data.scalar.tag) {
            .str => {
                const rs: *const @import("builtins").str.RocStr = @ptrCast(@alignCast(value.ptr.?));
                const s = rs.asSlice();
                var buf = std.ArrayList(u8).init(gpa);
                errdefer buf.deinit();
                try buf.append('"');
                for (s) |ch| {
                    switch (ch) {
                        '\\' => {
                            try buf.appendSlice("\\\\");
                        },
                        '"' => {
                            try buf.appendSlice("\\\"");
                        },
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
    if (value.layout.tag == .record) {
        var out = std.ArrayList(u8).init(gpa);
        errdefer out.deinit();
        try out.appendSlice("{ ");
        const rec_data = ctx.layout_store.getRecordData(value.layout.data.record.idx);
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
