//! TODO

const std = @import("std");
const base = @import("../base.zig");
const store = @import("../types/store.zig");
const types = @import("../types/types.zig");

const Allocator = std.mem.Allocator;

const SExpr = base.SExpr;
const ModuleEnv = base.ModuleEnv;
const Ident = base.Ident;

const Var = types.Var;
const Content = types.Content;
const FlatType = types.FlatType;
const Num = types.Num;
const Record = types.Record;
const RecordField = types.RecordField;
const TagUnion = types.TagUnion;
const Tag = types.Tag;

/// Helper that to writes variables as s-exprs
pub const SExprWriter = struct {
    /// Write all variables in the type store into the writer as an s-exprs
    pub fn allVarsToSExprStr(writer: std.io.AnyWriter, gpa: std.mem.Allocator, env: *const ModuleEnv) Allocator.Error!void {
        var root_node = SExpr.init(gpa, "types_store");
        defer root_node.deinit(gpa);

        if (env.types.slots.backing.items.len == 0) {
            root_node.appendStringAttr(gpa, "vars", "empty");
        }

        var buffer = std.ArrayList(u8).init(gpa);
        defer buffer.deinit();

        var type_writer = TypeWriter.init(buffer.writer(), env);

        for (0..env.types.slots.backing.items.len) |slot_idx| {
            const var_: Var = @enumFromInt(slot_idx);

            var var_node = SExpr.init(gpa, "var");

            buffer.clearRetainingCapacity();
            try buffer.writer().print("{}", .{slot_idx});
            var_node.appendStringAttr(gpa, "var", buffer.items);

            buffer.clearRetainingCapacity();
            try type_writer.writeVar(var_);
            var_node.appendStringAttr(gpa, "type", buffer.items);

            root_node.appendNode(gpa, &var_node);
        }

        root_node.toStringPretty(writer);
    }

    /// Convert some content to an s-expr node
    pub fn varToSExpr(gpa: std.mem.Allocator, type_writer: TypeWriter, var_: Var) SExpr {
        try type_writer.writerVar(var_);

        var node = SExpr.init(gpa, "type");
        node.appendStringAttr(gpa, type_writer.writer.context);
        return node;
    }
};

/// Helper that accepts a `Var` and write it as a nice string.
/// Entry point is `writeVar`
pub const TypeWriter = struct {
    const Self = @This();

    writer: std.ArrayList(u8).Writer,
    env: *const ModuleEnv,

    pub fn init(writer: std.ArrayList(u8).Writer, env: *const ModuleEnv) Self {
        return .{ .writer = writer, .env = env };
    }

    /// Convert a var to a type string
    pub fn writeVar(self: *Self, var_: types.Var) Allocator.Error!void {
        // Debug assert that the variable is in bounds - if not, we have a bug in type checking
        std.debug.assert(@intFromEnum(var_) < self.env.types.slots.backing.items.len);

        const resolved = self.env.types.resolveVar(var_);

        switch (resolved.desc.content) {
            .flex_var => |mb_ident_idx| {
                if (mb_ident_idx) |ident_idx| {
                    _ = try self.writer.write(self.env.idents.getText(ident_idx));
                } else {
                    _ = try self.writer.write("*");
                }
            },
            .rigid_var => |ident_idx| {
                _ = try self.writer.write(self.env.idents.getText(ident_idx));
            },
            .alias => |alias| {
                try self.writeAlias(alias, var_);
            },
            .structure => |flat_type| {
                try self.writeFlatType(flat_type, var_);
            },
            .err => {
                _ = try self.writer.write("Error");
            },
        }
    }

    /// Write an alias type
    pub fn writeAlias(self: *Self, alias: types.Alias, alias_var: types.Var) Allocator.Error!void {
        _ = try self.writer.write(self.env.idents.getText(alias.ident.ident_idx));
        if (alias.num_args > 0) {
            _ = try self.writer.write("(");
            var arg_iter = alias.argIterator(alias_var);
            // Write first arg without comma
            const first_arg = arg_iter.next();
            std.debug.assert(first_arg != null); // It shouldn't be null because we checked num_args > 0
            try self.writeVar(first_arg.?);
            // Write remaining args with comma prefix
            while (arg_iter.next()) |arg_var| {
                _ = try self.writer.write(", ");
                try self.writeVar(arg_var);
            }
            _ = try self.writer.write(")");
        }
    }

    /// Convert a flat type to a type string
    pub fn writeFlatType(self: *Self, flat_type: FlatType, var_: types.Var) Allocator.Error!void {
        switch (flat_type) {
            .str => {
                _ = try self.writer.write("Str");
            },
            .box => |sub_var| {
                _ = try self.writer.write("Box(");
                try self.writeVar(sub_var);
                _ = try self.writer.write(")");
            },
            .list => |sub_var| {
                _ = try self.writer.write("List(");
                try self.writeVar(sub_var);
                _ = try self.writer.write(")");
            },
            .list_unbound => {
                _ = try self.writer.write("List(*)");
            },
            .tuple => |tuple| {
                try self.writeTuple(tuple);
            },
            .num => |num| {
                try self.writeNum(num);
            },
            .nominal_type => |nominal_type| {
                try self.writeNominalType(nominal_type, var_);
            },
            .fn_pure => |func| {
                try self.writeFuncWithArrow(func, " -> ");
            },
            .fn_effectful => |func| {
                try self.writeFuncWithArrow(func, " => ");
            },
            .fn_unbound => |func| {
                try self.writeFuncWithArrow(func, " -> ");
            },
            .record => |record| {
                try self.writeRecord(record);
            },
            .record_unbound => |fields| {
                try self.writeRecordFields(fields);
            },
            .record_poly => |poly| {
                try self.writeRecord(poly.record);
                try self.writeVar(poly.var_);
            },
            .empty_record => {
                _ = try self.writer.write("{}");
            },
            .tag_union => |tag_union| {
                try self.writeTagUnion(tag_union);
            },
            .empty_tag_union => {
                _ = try self.writer.write("[]");
            },
        }
    }

    /// Write a tuple type
    pub fn writeTuple(self: *Self, tuple: types.Tuple) Allocator.Error!void {
        const elems = self.env.types.getTupleElemsSlice(tuple.elems);
        _ = try self.writer.write("(");
        for (elems, 0..) |elem, i| {
            if (i > 0) _ = try self.writer.write(", ");
            try self.writeVar(elem);
        }
        _ = try self.writer.write(")");
    }

    /// Write a nominal type
    pub fn writeNominalType(self: *Self, nominal_type: types.NominalType, nominal_var: types.Var) Allocator.Error!void {
        _ = try self.writer.write(self.env.idents.getText(nominal_type.ident.ident_idx));
        if (nominal_type.num_args > 0) {
            _ = try self.writer.write("(");
            var arg_iter = nominal_type.argIterator(nominal_var);
            // Write first arg without comma
            if (arg_iter.next()) |arg_var| {
                try self.writeVar(arg_var);
            }
            // Write remaining args with comma prefix
            while (arg_iter.next()) |arg_var| {
                _ = try self.writer.write(", ");
                try self.writeVar(arg_var);
            }
            _ = try self.writer.write(")");
        }
    }

    /// Write record fields without extension
    pub fn writeRecordFields(self: *Self, fields: types.RecordField.SafeMultiList.Range) Allocator.Error!void {
        if (fields.isEmpty()) {
            _ = try self.writer.write("{}");
            return;
        }

        _ = try self.writer.write("{ ");

        const fields_slice = self.env.types.getRecordFieldsSlice(fields);

        // Write first field - we already verified that there's at least one field
        _ = try self.writer.write(self.env.idents.getText(fields_slice.items(.name)[0]));
        _ = try self.writer.write(": ");
        try self.writeVar(fields_slice.items(.var_)[0]);

        // Write remaining fields
        for (fields_slice.items(.name)[1..], fields_slice.items(.var_)[1..]) |name, var_| {
            _ = try self.writer.write(", ");
            _ = try self.writer.write(self.env.idents.getText(name));
            _ = try self.writer.write(": ");
            try self.writeVar(var_);
        }

        _ = try self.writer.write(" }");
    }

    /// Write a function type with a specific arrow (`->` or `=>`)
    pub fn writeFuncWithArrow(self: *Self, func: types.Func, arrow: []const u8) Allocator.Error!void {
        const args = self.env.types.getFuncArgsSlice(func.args);

        // Write arguments
        if (args.len == 0) {
            _ = try self.writer.write("({})");
        } else if (args.len == 1) {
            try self.writeVar(args[0]);
        } else {
            for (args, 0..) |arg, i| {
                if (i > 0) _ = try self.writer.write(", ");
                try self.writeVar(arg);
            }
        }

        _ = try self.writer.write(arrow);

        try self.writeVar(func.ret);
    }

    /// Write a record type
    pub fn writeRecord(self: *Self, record: types.Record) Allocator.Error!void {
        const fields = self.env.types.getRecordFieldsSlice(record.fields);

        _ = try self.writer.write("{ ");
        for (fields.items(.name), fields.items(.var_), 0..) |field_name, field_var, i| {
            if (i > 0) _ = try self.writer.write(", ");
            _ = try self.writer.write(self.env.idents.getText(field_name));
            _ = try self.writer.write(": ");
            try self.writeVar(field_var);
        }

        // Show extension variable if it's not empty
        const ext_resolved = self.env.types.resolveVar(record.ext);
        switch (ext_resolved.desc.content) {
            .structure => |flat_type| switch (flat_type) {
                .empty_record => {}, // Don't show empty extension
                .record => |ext_record| {
                    // Flatten nested record extensions
                    const ext_fields = self.env.types.getRecordFieldsSlice(ext_record.fields);
                    for (ext_fields.items(.name), ext_fields.items(.var_)) |field_name, field_var| {
                        if (fields.len > 0 or ext_fields.len > 0) _ = try self.writer.write(", ");
                        _ = try self.writer.write(self.env.idents.getText(field_name));
                        _ = try self.writer.write(": ");
                        try self.writeVar(field_var);
                    }
                    // Recursively handle the extension's extension
                    try self.writeRecordExtension(ext_record.ext, fields.len + ext_fields.len);
                },
                else => {
                    if (fields.len > 0) _ = try self.writer.write(", ");
                    _ = try self.writer.write("* ");
                    try self.writeVar(record.ext);
                },
            },
            .flex_var => |mb_ident| {
                // Only show flex vars if they have a name
                if (mb_ident) |_| {
                    if (fields.len > 0) _ = try self.writer.write(", ");
                    _ = try self.writer.write("* ");
                    try self.writeVar(record.ext);
                }
                // Otherwise hide unnamed flex vars, so they render as no extension.
            },
            .rigid_var => |ident_idx| {
                // Show rigid vars with .. syntax
                if (fields.len > 0) _ = try self.writer.write(", ");
                _ = try self.writer.write("..");
                _ = try self.writer.write(self.env.idents.getText(ident_idx));
            },
            else => {
                // Show other types (aliases, errors, etc)
                if (fields.len > 0) _ = try self.writer.write(", ");
                _ = try self.writer.write("* ");
                try self.writeVar(record.ext);
            },
        }

        _ = try self.writer.write(" }");
    }

    /// Helper to write record extension, handling nested records
    fn writeRecordExtension(self: *Self, ext_var: types.Var, num_fields: usize) Allocator.Error!void {
        const ext_resolved = self.env.types.resolveVar(ext_var);
        switch (ext_resolved.desc.content) {
            .structure => |flat_type| switch (flat_type) {
                .empty_record => {}, // Don't show empty extension
                .record => |ext_record| {
                    // Flatten nested record extensions
                    const ext_fields = self.env.types.getRecordFieldsSlice(ext_record.fields);
                    for (ext_fields.items(.name), ext_fields.items(.var_)) |field_name, field_var| {
                        _ = try self.writer.write(", ");
                        _ = try self.writer.write(self.env.idents.getText(field_name));
                        _ = try self.writer.write(": ");
                        try self.writeVar(field_var);
                    }
                    // Recursively handle the extension's extension
                    try self.writeRecordExtension(ext_record.ext, num_fields + ext_fields.len);
                },
                else => {
                    if (num_fields > 0) _ = try self.writer.write(", ");
                    _ = try self.writer.write("* ");
                    try self.writeVar(ext_var);
                },
            },
            .flex_var => |mb_ident| {
                // Only show flex vars if they have a name
                if (mb_ident) |_| {
                    if (num_fields > 0) _ = try self.writer.write(", ");
                    _ = try self.writer.write("* ");
                    try self.writeVar(ext_var);
                }
                // Otherwise hide unnamed flex vars, so they render as no extension.
            },
            .rigid_var => |ident_idx| {
                // Show rigid vars with .. syntax
                if (num_fields > 0) _ = try self.writer.write(", ");
                _ = try self.writer.write("..");
                _ = try self.writer.write(self.env.idents.getText(ident_idx));
            },
            else => {
                // Show other types (aliases, errors, etc)
                if (num_fields > 0) _ = try self.writer.write(", ");
                _ = try self.writer.write("* ");
                try self.writeVar(ext_var);
            },
        }
    }

    /// Write a tag union type
    pub fn writeTagUnion(self: *Self, tag_union: types.TagUnion) Allocator.Error!void {
        _ = try self.writer.write("[");
        var iter = tag_union.tags.iterIndices();
        while (iter.next()) |tag_idx| {
            if (@intFromEnum(tag_idx) > @intFromEnum(tag_union.tags.start)) {
                _ = try self.writer.write(", ");
            }

            const tag = self.env.types.tags.get(tag_idx);
            try self.writeTag(tag);
        }

        _ = try self.writer.write("]");

        // Show extension variable if it's not empty
        const ext_resolved = self.env.types.resolveVar(tag_union.ext);
        switch (ext_resolved.desc.content) {
            .flex_var => _ = try self.writer.write("*"),
            .structure => |flat_type| switch (flat_type) {
                .empty_tag_union => {}, // Don't show empty extension
                else => {}, // TODO: Error?
            },
            else => {}, // TODO: Error?
        }
    }

    /// Write a single tag
    pub fn writeTag(self: *Self, tag: types.Tag) Allocator.Error!void {
        _ = try self.writer.write(self.env.idents.getText(tag.name));
        const args = self.env.types.getTagArgsSlice(tag.args);
        if (args.len > 0) {
            _ = try self.writer.write("(");
            for (args, 0..) |arg, i| {
                if (i > 0) _ = try self.writer.write(", ");
                try self.writeVar(arg);
            }
            _ = try self.writer.write(")");
        }
    }

    /// Convert a num type to a type string
    pub fn writeNum(self: *Self, num: Num) Allocator.Error!void {
        switch (num) {
            .num_poly => |poly| {
                _ = try self.writer.write("Num(");
                try self.writeVar(poly.var_);
                _ = try self.writer.write(")");
            },
            .int_poly => |poly| {
                _ = try self.writer.write("Int(");
                try self.writeVar(poly.var_);
                _ = try self.writer.write(")");
            },
            .frac_poly => |poly| {
                _ = try self.writer.write("Frac(");
                try self.writeVar(poly.var_);
                _ = try self.writer.write(")");
            },
            .num_unbound => |_| {
                _ = try self.writer.write("Num(*)");
            },
            .int_unbound => |_| {
                _ = try self.writer.write("Int(*)");
            },
            .frac_unbound => |_| {
                _ = try self.writer.write("Frac(*)");
            },
            .int_precision => |prec| {
                try self.writeIntType(prec);
            },
            .frac_precision => |prec| {
                try self.writeFracType(prec);
            },
            .num_compact => |compact| {
                switch (compact) {
                    .int => |prec| {
                        try self.writeIntType(prec);
                    },
                    .frac => |prec| {
                        try self.writeFracType(prec);
                    },
                }
            },
        }
    }

    pub fn writeIntType(self: *Self, prec: types.Num.Int.Precision) Allocator.Error!void {
        _ = switch (prec) {
            .u8 => try self.writer.write("U8"),
            .i8 => try self.writer.write("I8"),
            .u16 => try self.writer.write("U16"),
            .i16 => try self.writer.write("I16"),
            .u32 => try self.writer.write("U32"),
            .i32 => try self.writer.write("I32"),
            .u64 => try self.writer.write("U64"),
            .i64 => try self.writer.write("I64"),
            .u128 => try self.writer.write("U128"),
            .i128 => try self.writer.write("I128"),
        };
    }

    pub fn writeFracType(self: *Self, prec: types.Num.Frac.Precision) Allocator.Error!void {
        _ = switch (prec) {
            .f32 => try self.writer.write("F32"),
            .f64 => try self.writer.write("F64"),
            .dec => try self.writer.write("Dec"),
        };
    }
};
