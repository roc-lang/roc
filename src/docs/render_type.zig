//! Renders CIR type annotations to human-readable Roc syntax strings.
//!
//! This converts the internal `TypeAnno` representation into the syntax a Roc
//! programmer would write, e.g. `List(Str)`, `a -> b`, `{ name : Str }`.

const std = @import("std");
const CIR = @import("can").CIR;
const ModuleEnv = @import("can").ModuleEnv;

const Allocator = std.mem.Allocator;
const TypeAnno = CIR.TypeAnno;

/// A growable byte buffer used by the renderer.
pub const RenderBuffer = struct {
    list: std.ArrayList(u8),

    pub fn init() RenderBuffer {
        return .{ .list = std.ArrayList(u8).empty };
    }

    pub fn deinit(self: *RenderBuffer, gpa: Allocator) void {
        self.list.deinit(gpa);
    }

    pub fn toOwnedSlice(self: *RenderBuffer, gpa: Allocator) ![]u8 {
        return self.list.toOwnedSlice(gpa);
    }

    fn append(self: *RenderBuffer, gpa: Allocator, byte: u8) !void {
        try self.list.append(gpa, byte);
    }

    fn appendSlice(self: *RenderBuffer, gpa: Allocator, bytes: []const u8) !void {
        try self.list.appendSlice(gpa, bytes);
    }
};

/// Render a type annotation index to a newly allocated Roc syntax string.
pub fn renderTypeAnnoToString(
    gpa: Allocator,
    module_env: *const ModuleEnv,
    type_anno_idx: TypeAnno.Idx,
) ![]u8 {
    var buf = RenderBuffer.init();
    errdefer buf.deinit(gpa);
    try renderTypeAnno(&buf, gpa, module_env, type_anno_idx, false);
    return buf.toOwnedSlice(gpa);
}

/// Render a type annotation to Roc syntax into a buffer.
///
/// `needs_parens` indicates whether compound types (like functions) should be
/// wrapped in parentheses when used as arguments to other type constructors.
pub fn renderTypeAnno(
    buf: *RenderBuffer,
    gpa: Allocator,
    module_env: *const ModuleEnv,
    type_anno_idx: TypeAnno.Idx,
    needs_parens: bool,
) !void {
    const anno = module_env.store.getTypeAnno(type_anno_idx);
    switch (anno) {
        .apply => |a| {
            const name = module_env.getIdentText(a.name);
            try buf.appendSlice(gpa, name);
            const args_slice = module_env.store.sliceTypeAnnos(a.args);
            if (args_slice.len > 0) {
                try buf.append(gpa, '(');
                for (args_slice, 0..) |arg_idx, i| {
                    if (i > 0) try buf.appendSlice(gpa, ", ");
                    try renderTypeAnno(buf, gpa, module_env, arg_idx, false);
                }
                try buf.append(gpa, ')');
            }
        },
        .rigid_var => |tv| {
            try buf.appendSlice(gpa, module_env.getIdentText(tv.name));
        },
        .rigid_var_lookup => |rv| {
            try renderTypeAnno(buf, gpa, module_env, rv.ref, needs_parens);
        },
        .underscore => {
            try buf.append(gpa, '_');
        },
        .lookup => |t| {
            try buf.appendSlice(gpa, module_env.getIdentText(t.name));
        },
        .tag_union => |tu| {
            try buf.append(gpa, '[');
            const tags_slice = module_env.store.sliceTypeAnnos(tu.tags);
            for (tags_slice, 0..) |tag_idx, i| {
                if (i > 0) try buf.appendSlice(gpa, ", ");
                try renderTypeAnno(buf, gpa, module_env, tag_idx, false);
            }
            try buf.append(gpa, ']');
            if (tu.ext) |ext_idx| {
                try renderTypeAnno(buf, gpa, module_env, ext_idx, false);
            }
        },
        .tag => |t| {
            try buf.appendSlice(gpa, module_env.getIdentText(t.name));
            const args_slice = module_env.store.sliceTypeAnnos(t.args);
            if (args_slice.len > 0) {
                try buf.append(gpa, '(');
                for (args_slice, 0..) |arg_idx, i| {
                    if (i > 0) try buf.appendSlice(gpa, ", ");
                    try renderTypeAnno(buf, gpa, module_env, arg_idx, false);
                }
                try buf.append(gpa, ')');
            }
        },
        .tuple => |t| {
            try buf.append(gpa, '(');
            const elems_slice = module_env.store.sliceTypeAnnos(t.elems);
            for (elems_slice, 0..) |elem_idx, i| {
                if (i > 0) try buf.appendSlice(gpa, ", ");
                try renderTypeAnno(buf, gpa, module_env, elem_idx, false);
            }
            try buf.append(gpa, ')');
        },
        .record => |r| {
            try buf.appendSlice(gpa, "{ ");
            const fields_slice = module_env.store.sliceAnnoRecordFields(r.fields);
            for (fields_slice, 0..) |field_idx, i| {
                if (i > 0) try buf.appendSlice(gpa, ", ");
                const field = module_env.store.getAnnoRecordField(field_idx);
                try buf.appendSlice(gpa, module_env.getIdentText(field.name));
                try buf.appendSlice(gpa, " : ");
                try renderTypeAnno(buf, gpa, module_env, field.ty, false);
            }
            try buf.appendSlice(gpa, " }");
            if (r.ext) |ext_idx| {
                try renderTypeAnno(buf, gpa, module_env, ext_idx, false);
            }
        },
        .@"fn" => |f| {
            if (needs_parens) try buf.append(gpa, '(');
            const args_slice = module_env.store.sliceTypeAnnos(f.args);
            for (args_slice, 0..) |arg_idx, i| {
                if (i > 0) try buf.appendSlice(gpa, ", ");
                try renderTypeAnno(buf, gpa, module_env, arg_idx, true);
            }
            if (f.effectful) {
                try buf.appendSlice(gpa, " => ");
            } else {
                try buf.appendSlice(gpa, " -> ");
            }
            try renderTypeAnno(buf, gpa, module_env, f.ret, false);
            if (needs_parens) try buf.append(gpa, ')');
        },
        .parens => |p| {
            try buf.append(gpa, '(');
            try renderTypeAnno(buf, gpa, module_env, p.anno, false);
            try buf.append(gpa, ')');
        },
        .malformed => {
            try buf.appendSlice(gpa, "<malformed>");
        },
    }
}

/// Render a type header (e.g. `Maybe(a)`) to Roc syntax.
pub fn renderTypeHeader(
    buf: *RenderBuffer,
    gpa: Allocator,
    module_env: *const ModuleEnv,
    header_idx: CIR.TypeHeader.Idx,
) !void {
    const header = module_env.store.getTypeHeader(header_idx);
    try buf.appendSlice(gpa, module_env.getIdentText(header.relative_name));
    const args_slice = module_env.store.sliceTypeAnnos(header.args);
    if (args_slice.len > 0) {
        try buf.append(gpa, '(');
        for (args_slice, 0..) |arg_idx, i| {
            if (i > 0) try buf.appendSlice(gpa, ", ");
            try renderTypeAnno(buf, gpa, module_env, arg_idx, false);
        }
        try buf.append(gpa, ')');
    }
}
