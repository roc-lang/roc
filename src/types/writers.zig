//! Type serialization utilities for writing type information as S-expressions.
//!
//! This module provides functionality to serialize type store contents and
//! individual types into S-expression format for debugging, inspection, and
//! external tool integration. The serialized output helps visualize the
//! compiler's internal type representations.

const std = @import("std");
const base = @import("base");
const types = @import("types.zig");
const ModuleEnv = @import("compile").ModuleEnv;

const TypesStore = @import("store.zig").Store;

const Allocator = std.mem.Allocator;
const Desc = types.Descriptor;
const Var = types.Var;
const Content = types.Content;
const Rank = types.Rank;
const Mark = types.Mark;
const RecordField = types.RecordField;
const TagUnion = types.TagUnion;
const Tag = types.Tag;
const VarSafeList = Var.SafeList;
const RecordFieldSafeMultiList = RecordField.SafeMultiList;
const TagSafeMultiList = Tag.SafeMultiList;
const Descriptor = types.Descriptor;
const TypeIdent = types.TypeIdent;
const Alias = types.Alias;
const FlatType = types.FlatType;
const NominalType = types.NominalType;
const Record = types.Record;
const Num = types.Num;
const Tuple = types.Tuple;
const Func = types.Func;

// const SExpr = base.SExpr;
const Ident = base.Ident;

const TypeContext = enum {
    General,
    NumContent,
    ListContent,
    RecordExtension,
    TagUnionExtension,
    RecordFieldContent,
    TupleFieldContent,
    FunctionArgument,
    FunctionReturn,
};

/// Helper that accepts a `Var` and write it as a nice string.
/// Entry point is `writeVar`
pub const TypeWriter = struct {
    const Self = @This();

    types: *const TypesStore,
    idents: *const Ident.Store,
    buf: std.ArrayList(u8),
    seen: std.ArrayList(Var),
    next_name_index: u32,
    name_counters: std.EnumMap(TypeContext, u32),

    /// Initialize a TypeWriter with an immutable ModuleEnv reference.
    pub fn init(gpa: std.mem.Allocator, env: *const ModuleEnv) std.mem.Allocator.Error!Self {
        return TypeWriter.initFromParts(gpa, &env.types, &env.idents);
    }

    /// Initialize a TypeWriter with immutable types and idents references.
    pub fn initFromParts(gpa: std.mem.Allocator, types_store: *const TypesStore, idents: *const Ident.Store) std.mem.Allocator.Error!Self {
        return .{
            .types = types_store,
            .idents = idents,
            .buf = try std.ArrayList(u8).initCapacity(gpa, 32),
            .seen = try std.ArrayList(Var).initCapacity(gpa, 16),
            .next_name_index = 0,
            .name_counters = std.EnumMap(TypeContext, u32).init(.{}),
        };
    }

    pub fn deinit(self: *Self) void {
        self.buf.deinit();
        self.seen.deinit();
    }

    pub fn get(self: *const Self) []u8 {
        return self.buf.items[0..];
    }

    pub fn write(self: *Self, var_: Var) std.mem.Allocator.Error!void {
        self.buf.clearRetainingCapacity();
        self.next_name_index = 0;
        self.name_counters = std.EnumMap(TypeContext, u32).init(.{});
        try self.writeVar(var_, var_);
    }

    fn generateNextName(self: *Self) !void {
        // Generate name: a, b, ..., z, aa, ab, ..., az, ba, ...
        // Skip any names that already exist in the identifier store
        // We need at most one more name than the number of existing identifiers
        const max_attempts = self.idents.interner.entry_count + 1;
        var attempts: usize = 0;
        while (attempts < max_attempts) : (attempts += 1) {
            var n = self.next_name_index;
            self.next_name_index += 1;

            var name_buf: [8]u8 = undefined;
            var name_len: usize = 0;

            // Generate name in base-26: a, b, ..., z, aa, ab, ..., az, ba, ...
            while (name_len < name_buf.len) {
                name_buf[name_len] = @intCast('a' + (n % 26));
                name_len += 1;
                n = n / 26;
                if (n == 0) break;
                n -= 1;
            }

            // Names are generated in reverse order, so reverse the buffer
            std.mem.reverse(u8, name_buf[0..name_len]);

            // Check if this name already exists in the identifier store
            const candidate_name = name_buf[0..name_len];
            const exists = self.idents.interner.contains(candidate_name);

            if (!exists) {
                // This name is available, write it to the buffer
                for (candidate_name) |c| {
                    try self.buf.append(c);
                }
                break;
            }
            // Name already exists, try the next one
        }

        // This should never happen in practice, but let's handle it gracefully
        if (attempts >= max_attempts) {
            _ = try self.buf.writer().write("var");
            try self.buf.writer().print("{}", .{self.next_name_index});
        }
    }

    fn generateContextualName(self: *Self, context: TypeContext) std.mem.Allocator.Error!void {
        const base_name = switch (context) {
            .NumContent => "size",
            .ListContent => "elem",
            .RecordExtension => "others",
            .TagUnionExtension => "others",
            .RecordFieldContent => "field",
            .TupleFieldContent => "field",
            .FunctionArgument => "arg",
            .FunctionReturn => "ret",
            .General => {
                // Fall back to generic name generation
                try self.generateNextName();
                return;
            },
        };

        // Get the current counter for this context
        const current_counter = self.name_counters.get(context) orelse 0;

        // Generate name with counter (start numbering from 2 for the second occurrence)
        var buf: [32]u8 = undefined;
        const candidate_name = if (current_counter == 0)
            base_name
        else blk: {
            const name = std.fmt.bufPrint(&buf, "{s}{}", .{ base_name, current_counter + 1 }) catch {
                // Buffer too small, fall back to generic name
                try self.generateNextName();
                return;
            };
            break :blk name;
        };

        // Write the name
        for (candidate_name) |c| {
            try self.buf.append(c);
        }

        // Increment counter for next time
        self.name_counters.put(context, current_counter + 1);
    }

    fn writeNameCheckingCollisions(self: *Self, candidate_name: []const u8) std.mem.Allocator.Error!void {
        // Check if this name already exists in the identifier store
        var exists = false;

        // Check all identifiers in the store
        var i: u32 = 0;
        while (i < self.idents.interner.outer_indices.items.len) : (i += 1) {
            const ident_idx = Ident.Idx{ .idx = @truncate(i), .attributes = .{ .effectful = false, .ignored = false, .reassignable = false } };
            const existing_name = self.idents.getText(ident_idx);
            if (std.mem.eql(u8, existing_name, candidate_name)) {
                exists = true;
                break;
            }
        }

        if (!exists) {
            // This name is available, write it to the buffer
            for (candidate_name) |c| {
                try self.buf.append(c);
            }
        } else {
            // Name collision - we need to handle this differently
            // For now, just fall back to generic name generation
            try self.generateNextName();
        }
    }

    fn hasSeenVar(self: *const Self, var_: Var) bool {
        for (self.seen.items) |seen| {
            if (seen == var_) return true;
        }
        return false;
    }

    /// Count how many times a variable appears in a type
    fn countVarOccurrences(self: *const Self, search_var: Var, root_var: Var) usize {
        var count: usize = 0;
        self.countVar(search_var, root_var, &count);
        return count;
    }

    fn countVar(self: *const Self, search_var: Var, current_var: Var, count: *usize) void {
        if (current_var == search_var) {
            count.* += 1;
        }

        if (@intFromEnum(current_var) >= self.types.slots.backing.len()) return;

        const resolved = self.types.resolveVar(current_var);
        switch (resolved.desc.content) {
            .flex_var, .rigid_var, .err => {},
            .alias => |alias| {
                // For aliases, we only count occurrences in the type arguments
                var args_iter = self.types.iterAliasArgs(alias);
                while (args_iter.next()) |arg_var| {
                    self.countVar(search_var, arg_var, count);
                }
            },
            .structure => |flat_type| {
                self.countVarInFlatType(search_var, flat_type, count);
            },
        }
    }

    fn countVarInFlatType(self: *const Self, search_var: Var, flat_type: FlatType, count: *usize) void {
        switch (flat_type) {
            .str, .empty_record, .empty_tag_union => {},
            .box => |sub_var| self.countVar(search_var, sub_var, count),
            .list => |sub_var| self.countVar(search_var, sub_var, count),
            .list_unbound, .num => {},
            .tuple => |tuple| {
                const elems = self.types.sliceVars(tuple.elems);
                for (elems) |elem| {
                    self.countVar(search_var, elem, count);
                }
            },
            .nominal_type => |nominal_type| {
                var args_iter = self.types.iterNominalArgs(nominal_type);
                while (args_iter.next()) |arg_var| {
                    self.countVar(search_var, arg_var, count);
                }
            },
            .fn_pure, .fn_effectful, .fn_unbound => |func| {
                const args = self.types.sliceVars(func.args);
                for (args) |arg| {
                    self.countVar(search_var, arg, count);
                }
                self.countVar(search_var, func.ret, count);
            },
            .record => |record| {
                const fields = self.types.getRecordFieldsSlice(record.fields);
                for (fields.items(.var_)) |field_var| {
                    self.countVar(search_var, field_var, count);
                }
                self.countVar(search_var, record.ext, count);
            },
            .record_unbound => |fields| {
                const fields_slice = self.types.getRecordFieldsSlice(fields);
                for (fields_slice.items(.var_)) |field_var| {
                    self.countVar(search_var, field_var, count);
                }
            },
            .record_poly => |poly| {
                self.countVarInFlatType(search_var, FlatType{ .record = poly.record }, count);
                self.countVar(search_var, poly.var_, count);
            },
            .tag_union => |tag_union| {
                var iter = tag_union.tags.iterIndices();
                while (iter.next()) |tag_idx| {
                    const tag = self.types.tags.get(tag_idx);
                    const args = self.types.sliceVars(tag.args);
                    for (args) |arg_var| {
                        self.countVar(search_var, arg_var, count);
                    }
                }
                self.countVar(search_var, tag_union.ext, count);
            },
        }
    }

    /// Convert a var to a type string
    fn writeVarWithContext(self: *Self, var_: Var, context: TypeContext, root_var: Var) std.mem.Allocator.Error!void {
        if (@intFromEnum(var_) >= self.types.slots.backing.len()) {
            // Debug assert that the variable is in bounds - if not, we have a bug in type checking
            _ = try self.buf.writer().write("invalid_type");
        } else {
            const resolved = self.types.resolveVar(var_);
            if (self.hasSeenVar(var_)) {
                _ = try self.buf.writer().write("...");
            } else {
                try self.seen.append(var_);
                defer _ = self.seen.pop();

                switch (resolved.desc.content) {
                    .flex_var => |mb_ident_idx| {
                        if (mb_ident_idx) |ident_idx| {
                            _ = try self.buf.writer().write(self.idents.getText(ident_idx));
                        } else {
                            // Check if this variable appears multiple times
                            const occurrences = self.countVarOccurrences(var_, root_var);
                            if (occurrences == 1) {
                                _ = try self.buf.writer().write("_");
                            }
                            try self.generateContextualName(context);
                        }
                    },
                    .rigid_var => |ident_idx| {
                        _ = try self.buf.writer().write(self.idents.getText(ident_idx));
                    },
                    .alias => |alias| {
                        try self.writeAlias(alias, root_var);
                    },
                    .structure => |flat_type| {
                        try self.writeFlatType(flat_type, root_var);
                    },
                    .err => {
                        _ = try self.buf.writer().write("Error");
                    },
                }
            }
        }
    }

    fn writeVar(self: *Self, var_: Var, root_var: Var) std.mem.Allocator.Error!void {
        try self.writeVarWithContext(var_, .General, root_var);
    }

    /// Write an alias type
    fn writeAlias(self: *Self, alias: Alias, root_var: Var) std.mem.Allocator.Error!void {
        _ = try self.buf.writer().write(self.idents.getText(alias.ident.ident_idx));
        var args_iter = self.types.iterAliasArgs(alias);
        if (args_iter.count() > 0) {
            _ = try self.buf.writer().write("(");

            // Write first arg without comma
            if (args_iter.next()) |arg_var| {
                try self.writeVar(arg_var, root_var);
            }

            // Write remaining args with comma prefix
            while (args_iter.next()) |arg_var| {
                _ = try self.buf.writer().write(", ");
                try self.writeVar(arg_var, root_var);
            }
            _ = try self.buf.writer().write(")");
        }
    }

    /// Convert a flat type to a type string
    fn writeFlatType(self: *Self, flat_type: FlatType, root_var: Var) std.mem.Allocator.Error!void {
        switch (flat_type) {
            .str => {
                _ = try self.buf.writer().write("Str");
            },
            .box => |sub_var| {
                _ = try self.buf.writer().write("Box(");
                try self.writeVar(sub_var, root_var);
                _ = try self.buf.writer().write(")");
            },
            .list => |sub_var| {
                _ = try self.buf.writer().write("List(");
                try self.writeVarWithContext(sub_var, .ListContent, root_var);
                _ = try self.buf.writer().write(")");
            },
            .list_unbound => {
                _ = try self.buf.writer().write("List(_");
                try self.generateContextualName(.ListContent);
                _ = try self.buf.writer().write(")");
            },
            .tuple => |tuple| {
                try self.writeTuple(tuple, root_var);
            },
            .num => |num| {
                try self.writeNum(num, root_var);
            },
            .nominal_type => |nominal_type| {
                try self.writeNominalType(nominal_type, root_var);
            },
            .fn_pure => |func| {
                try self.writeFuncWithArrow(func, " -> ", root_var);
            },
            .fn_effectful => |func| {
                try self.writeFuncWithArrow(func, " => ", root_var);
            },
            .fn_unbound => |func| {
                try self.writeFuncWithArrow(func, " -> ", root_var);
            },
            .record => |record| {
                try self.writeRecord(record, root_var);
            },
            .record_unbound => |fields| {
                try self.writeRecordFields(fields, root_var);
            },
            .record_poly => |poly| {
                try self.writeRecord(poly.record, root_var);
                try self.writeVar(poly.var_, root_var);
            },
            .empty_record => {
                _ = try self.buf.writer().write("{}");
            },
            .tag_union => |tag_union| {
                try self.writeTagUnion(tag_union, root_var);
            },
            .empty_tag_union => {
                _ = try self.buf.writer().write("[]");
            },
        }
    }

    /// Write a tuple type
    fn writeTuple(self: *Self, tuple: Tuple, root_var: Var) std.mem.Allocator.Error!void {
        const elems = self.types.sliceVars(tuple.elems);
        _ = try self.buf.writer().write("(");
        for (elems, 0..) |elem, i| {
            if (i > 0) _ = try self.buf.writer().write(", ");
            try self.writeVarWithContext(elem, .TupleFieldContent, root_var);
        }
        _ = try self.buf.writer().write(")");
    }

    /// Write a nominal type
    fn writeNominalType(self: *Self, nominal_type: NominalType, root_var: Var) std.mem.Allocator.Error!void {
        _ = try self.buf.writer().write(self.idents.getText(nominal_type.ident.ident_idx));

        var args_iter = self.types.iterNominalArgs(nominal_type);
        if (args_iter.count() > 0) {
            _ = try self.buf.writer().write("(");

            // Write first arg without comma
            if (args_iter.next()) |arg_var| {
                try self.writeVar(arg_var, root_var);
            }
            // Write remaining args with comma prefix
            while (args_iter.next()) |arg_var| {
                _ = try self.buf.writer().write(", ");
                try self.writeVar(arg_var, root_var);
            }
            _ = try self.buf.writer().write(")");
        }
    }

    /// Write record fields without extension
    fn writeRecordFields(self: *Self, fields: RecordField.SafeMultiList.Range, root_var: Var) std.mem.Allocator.Error!void {
        if (fields.isEmpty()) {
            _ = try self.buf.writer().write("{}");
            return;
        }

        _ = try self.buf.writer().write("{ ");

        const fields_slice = self.types.getRecordFieldsSlice(fields);

        // Write first field - we already verified that there's at least one field
        _ = try self.buf.writer().write(self.idents.getText(fields_slice.items(.name)[0]));
        _ = try self.buf.writer().write(": ");
        try self.writeVarWithContext(fields_slice.items(.var_)[0], .RecordFieldContent, root_var);

        // Write remaining fields
        for (fields_slice.items(.name)[1..], fields_slice.items(.var_)[1..]) |name, var_| {
            _ = try self.buf.writer().write(", ");
            _ = try self.buf.writer().write(self.idents.getText(name));
            _ = try self.buf.writer().write(": ");
            try self.writeVarWithContext(var_, .RecordFieldContent, root_var);
        }

        _ = try self.buf.writer().write(" }");
    }

    /// Write a function type with a specific arrow (`->` or `=>`)
    fn writeFuncWithArrow(self: *Self, func: Func, arrow: []const u8, root_var: Var) std.mem.Allocator.Error!void {
        const args = self.types.sliceVars(func.args);

        // Write arguments
        if (args.len == 0) {
            _ = try self.buf.writer().write("({})");
        } else if (args.len == 1) {
            try self.writeVarWithContext(args[0], .FunctionArgument, root_var);
        } else {
            for (args, 0..) |arg, i| {
                if (i > 0) _ = try self.buf.writer().write(", ");
                try self.writeVarWithContext(arg, .FunctionArgument, root_var);
            }
        }

        _ = try self.buf.writer().write(arrow);

        try self.writeVarWithContext(func.ret, .FunctionReturn, root_var);
    }

    /// Write a record type
    fn writeRecord(self: *Self, record: Record, root_var: Var) std.mem.Allocator.Error!void {
        const fields = self.types.getRecordFieldsSlice(record.fields);

        _ = try self.buf.writer().write("{ ");
        for (fields.items(.name), fields.items(.var_), 0..) |field_name, field_var, i| {
            if (i > 0) _ = try self.buf.writer().write(", ");
            _ = try self.buf.writer().write(self.idents.getText(field_name));
            _ = try self.buf.writer().write(": ");
            try self.writeVarWithContext(field_var, .RecordFieldContent, root_var);
        }

        // Show extension variable if it's not empty
        const ext_resolved = self.types.resolveVar(record.ext);
        switch (ext_resolved.desc.content) {
            .structure => |flat_type| switch (flat_type) {
                .empty_record => {}, // Don't show empty extension
                .record => |ext_record| {
                    // Flatten nested record extensions
                    const ext_fields = self.types.getRecordFieldsSlice(ext_record.fields);
                    for (ext_fields.items(.name), ext_fields.items(.var_)) |field_name, field_var| {
                        if (fields.len > 0 or ext_fields.len > 0) _ = try self.buf.writer().write(", ");
                        _ = try self.buf.writer().write(self.idents.getText(field_name));
                        _ = try self.buf.writer().write(": ");
                        try self.writeVarWithContext(field_var, .RecordFieldContent, root_var);
                    }
                    // Recursively handle the extension's extension
                    try self.writeRecordExtension(ext_record.ext, fields.len + ext_fields.len, root_var);
                },
                else => {
                    if (fields.len > 0) _ = try self.buf.writer().write(", ");
                    try self.writeVarWithContext(record.ext, .RecordExtension, root_var);
                },
            },
            .flex_var => |mb_ident| {
                // Only show flex vars if they have a name
                if (mb_ident) |_| {
                    if (fields.len > 0) _ = try self.buf.writer().write(", ");
                    try self.writeVarWithContext(record.ext, .RecordExtension, root_var);
                }
                // Otherwise hide unnamed flex vars, so they render as no extension.
            },
            .rigid_var => |ident_idx| {
                // Show rigid vars with .. syntax
                if (fields.len > 0) _ = try self.buf.writer().write(", ");
                _ = try self.buf.writer().write("..");
                _ = try self.buf.writer().write(self.idents.getText(ident_idx));
            },
            else => {
                if (fields.len > 0) _ = try self.buf.writer().write(", ");
                try self.writeVarWithContext(record.ext, .RecordExtension, root_var);
            },
        }

        _ = try self.buf.writer().write(" }");
    }

    /// Helper to write record extension, handling nested records
    fn writeRecordExtension(self: *Self, ext_var: Var, num_fields: usize, root_var: Var) std.mem.Allocator.Error!void {
        const ext_resolved = self.types.resolveVar(ext_var);
        switch (ext_resolved.desc.content) {
            .structure => |flat_type| switch (flat_type) {
                .empty_record => {}, // Don't show empty extension
                .record => |ext_record| {
                    // Flatten nested record extensions
                    const ext_fields = self.types.getRecordFieldsSlice(ext_record.fields);
                    for (ext_fields.items(.name), ext_fields.items(.var_)) |field_name, field_var| {
                        _ = try self.buf.writer().write(", ");
                        _ = try self.buf.writer().write(self.idents.getText(field_name));
                        _ = try self.buf.writer().write(": ");
                        try self.writeVarWithContext(field_var, .RecordFieldContent, root_var);
                    }
                    // Recursively handle the extension's extension
                    try self.writeRecordExtension(ext_record.ext, num_fields + ext_fields.len, root_var);
                },
                else => {
                    if (num_fields > 0) _ = try self.buf.writer().write(", ");
                    try self.writeVarWithContext(ext_var, .RecordExtension, root_var);
                },
            },
            .flex_var => |mb_ident| {
                // Only show flex vars if they have a name
                if (mb_ident) |_| {
                    if (num_fields > 0) _ = try self.buf.writer().write(", ");
                    try self.writeVarWithContext(ext_var, .RecordExtension, root_var);
                }
                // Otherwise hide unnamed flex vars, so they render as no extension.
            },
            .rigid_var => |ident_idx| {
                // Show rigid vars with .. syntax
                if (num_fields > 0) _ = try self.buf.writer().write(", ");
                _ = try self.buf.writer().write("..");
                _ = try self.buf.writer().write(self.idents.getText(ident_idx));
            },
            else => {
                // Show other types (aliases, errors, etc)
                if (num_fields > 0) _ = try self.buf.writer().write(", ");
                try self.writeVarWithContext(ext_var, .RecordExtension, root_var);
            },
        }
    }

    /// Write a tag union type
    fn writeTagUnion(self: *Self, tag_union: TagUnion, root_var: Var) std.mem.Allocator.Error!void {
        _ = try self.buf.writer().write("[");
        var iter = tag_union.tags.iterIndices();
        while (iter.next()) |tag_idx| {
            if (@intFromEnum(tag_idx) > @intFromEnum(tag_union.tags.start)) {
                _ = try self.buf.writer().write(", ");
            }

            const tag = self.types.tags.get(tag_idx);
            try self.writeTag(tag, root_var);
        }

        _ = try self.buf.writer().write("]");

        // Show extension variable if it's not empty
        const ext_resolved = self.types.resolveVar(tag_union.ext);
        switch (ext_resolved.desc.content) {
            .flex_var => |mb_ident_idx| {
                if (mb_ident_idx) |ident_idx| {
                    _ = try self.buf.writer().write(self.idents.getText(ident_idx));
                } else {
                    // Check if this variable appears multiple times
                    const occurrences = self.countVarOccurrences(tag_union.ext, root_var);
                    if (occurrences == 1) {
                        _ = try self.buf.writer().write("_");
                    }
                    try self.generateContextualName(.TagUnionExtension);
                }
            },
            .structure => |flat_type| switch (flat_type) {
                .empty_tag_union => {}, // Don't show empty extension
                else => {
                    try self.writeVarWithContext(tag_union.ext, .TagUnionExtension, root_var);
                },
            },
            .rigid_var => |ident_idx| {
                _ = try self.buf.writer().write(self.idents.getText(ident_idx));
                _ = try self.buf.writer().write("(r)");
            },
            else => {
                try self.writeVarWithContext(tag_union.ext, .TagUnionExtension, root_var);
            },
        }
    }

    /// Write a single tag
    fn writeTag(self: *Self, tag: Tag, root_var: Var) std.mem.Allocator.Error!void {
        _ = try self.buf.writer().write(self.idents.getText(tag.name));
        const args = self.types.sliceVars(tag.args);
        if (args.len > 0) {
            _ = try self.buf.writer().write("(");
            for (args, 0..) |arg, i| {
                if (i > 0) _ = try self.buf.writer().write(", ");
                try self.writeVar(arg, root_var);
            }
            _ = try self.buf.writer().write(")");
        }
    }

    /// Convert a num type to a type string
    fn writeNum(self: *Self, num: Num, root_var: Var) std.mem.Allocator.Error!void {
        switch (num) {
            .num_poly => |poly| {
                _ = try self.buf.writer().write("Num(");
                try self.writeVarWithContext(poly.var_, .NumContent, root_var);
                _ = try self.buf.writer().write(")");
            },
            .int_poly => |poly| {
                _ = try self.buf.writer().write("Int(");
                try self.writeVarWithContext(poly.var_, .NumContent, root_var);
                _ = try self.buf.writer().write(")");
            },
            .frac_poly => |poly| {
                _ = try self.buf.writer().write("Frac(");
                try self.writeVarWithContext(poly.var_, .NumContent, root_var);
                _ = try self.buf.writer().write(")");
            },
            .num_unbound => |_| {
                _ = try self.buf.writer().write("Num(_");
                try self.generateContextualName(.NumContent);
                _ = try self.buf.writer().write(")");
            },
            .int_unbound => |_| {
                _ = try self.buf.writer().write("Int(_");
                try self.generateContextualName(.NumContent);
                _ = try self.buf.writer().write(")");
            },
            .frac_unbound => |_| {
                _ = try self.buf.writer().write("Frac(_");
                try self.generateContextualName(.NumContent);
                _ = try self.buf.writer().write(")");
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

    fn writeIntType(self: *Self, prec: Num.Int.Precision) std.mem.Allocator.Error!void {
        _ = switch (prec) {
            .u8 => try self.buf.writer().write("U8"),
            .i8 => try self.buf.writer().write("I8"),
            .u16 => try self.buf.writer().write("U16"),
            .i16 => try self.buf.writer().write("I16"),
            .u32 => try self.buf.writer().write("U32"),
            .i32 => try self.buf.writer().write("I32"),
            .u64 => try self.buf.writer().write("U64"),
            .i64 => try self.buf.writer().write("I64"),
            .u128 => try self.buf.writer().write("U128"),
            .i128 => try self.buf.writer().write("I128"),
        };
    }

    fn writeFracType(self: *Self, prec: Num.Frac.Precision) std.mem.Allocator.Error!void {
        _ = switch (prec) {
            .f32 => try self.buf.writer().write("F32"),
            .f64 => try self.buf.writer().write("F64"),
            .dec => try self.buf.writer().write("Dec"),
        };
    }
};
