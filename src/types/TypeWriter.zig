//! Type serialization utilities for writing type information as S-expressions.
//!
//! This module provides functionality to serialize type store contents and
//! individual types into S-expression format for debugging, inspection, and
//! external tool integration. The serialized output helps visualize the
//! compiler's internal type representations.

const std = @import("std");
const base = @import("base");
const types_mod = @import("types.zig");

const TypesStore = @import("store.zig").Store;
const Allocator = std.mem.Allocator;
const Desc = types_mod.Descriptor;
const Var = types_mod.Var;
const Content = types_mod.Content;
const Rank = types_mod.Rank;
const Mark = types_mod.Mark;
const RecordField = types_mod.RecordField;
const TagUnion = types_mod.TagUnion;
const Tag = types_mod.Tag;
const VarSafeList = Var.SafeList;
const RecordFieldSafeMultiList = RecordField.SafeMultiList;
const TagSafeMultiList = Tag.SafeMultiList;
const Descriptor = types_mod.Descriptor;
const TypeIdent = types_mod.TypeIdent;
const Alias = types_mod.Alias;
const FlatType = types_mod.FlatType;
const NominalType = types_mod.NominalType;
const Record = types_mod.Record;
const Num = types_mod.Num;
const Tuple = types_mod.Tuple;
const Func = types_mod.Func;

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
const TypeWriter = @This();

types: *const TypesStore,
idents: *const Ident.Store,
buf: std.ArrayList(u8),
seen: std.ArrayList(Var),
next_name_index: u32,
name_counters: std.EnumMap(TypeContext, u32),
flex_var_names_map: std.AutoHashMap(Var, FlexVarNameRange),
flex_var_names: std.ArrayList(u8),

const FlexVarNameRange = struct { start: usize, end: usize };

/// Initialize a TypeWriter with immutable types and idents references.
pub fn initFromParts(gpa: std.mem.Allocator, types_store: *const TypesStore, idents: *const Ident.Store) std.mem.Allocator.Error!TypeWriter {
    return .{
        .types = types_store,
        .idents = idents,
        .buf = try std.ArrayList(u8).initCapacity(gpa, 32),
        .seen = try std.ArrayList(Var).initCapacity(gpa, 16),
        .next_name_index = 0,
        .name_counters = std.EnumMap(TypeContext, u32).init(.{}),
        .flex_var_names_map = std.AutoHashMap(Var, FlexVarNameRange).init(gpa),
        .flex_var_names = try std.ArrayList(u8).initCapacity(gpa, 32),
    };
}

pub fn deinit(self: *TypeWriter) void {
    self.buf.deinit();
    self.seen.deinit();
    self.flex_var_names_map.deinit();
    self.flex_var_names.deinit();
}

/// Returns the current contents of the type writer's buffer as a slice.
/// This contains the formatted type representation built up by write operations.
pub fn get(self: *const TypeWriter) []u8 {
    return self.buf.items[0..];
}

/// Writes a type variable to the buffer, formatting it as a human-readable string.
/// This clears any existing content in the buffer before writing.
pub fn write(self: *TypeWriter, var_: Var) std.mem.Allocator.Error!void {
    self.buf.clearRetainingCapacity();
    self.next_name_index = 0;
    self.name_counters = std.EnumMap(TypeContext, u32).init(.{});
    try self.writeVar(var_, var_);
}

fn generateNextName(self: *TypeWriter) !void {
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
            // This name is available, use it
            for (candidate_name) |c| {
                try self.buf.writer().writeByte(c);
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

fn generateContextualName(self: *TypeWriter, context: TypeContext) std.mem.Allocator.Error!void {
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

    // Try to generate a name with increasing counters until we find one that doesn't collide
    var counter = self.name_counters.get(context) orelse 0;
    var found = false;

    // We need at most as many attempts as there are existing identifiers
    const max_attempts = self.idents.interner.entry_count;
    var attempts: usize = 0;
    while (!found and attempts < max_attempts) : (attempts += 1) {
        var buf: [32]u8 = undefined;
        const candidate_name = if (counter == 0)
            base_name
        else blk: {
            const name = std.fmt.bufPrint(&buf, "{s}{}", .{ base_name, counter + 1 }) catch {
                // Buffer too small, fall back to generic name
                try self.generateNextName();
                return;
            };
            break :blk name;
        };

        // Check if this name already exists in the identifier store
        const exists = self.idents.interner.contains(candidate_name);

        if (!exists) {
            // This name is available, write it to the buffer
            for (candidate_name) |c| {
                try self.buf.append(c);
            }
            found = true;
        } else {
            // Try next counter
            counter += 1;
        }
    }

    // If we couldn't find a unique contextual name, fall back to generic names
    if (!found) {
        try self.generateNextName();
        return;
    }

    self.name_counters.put(context, counter + 1);
}

fn writeNameCheckingCollisions(self: *TypeWriter, candidate_name: []const u8) std.mem.Allocator.Error!void {
    // Check if this name already exists in the identifier store
    var exists = false;

    // Check all identifiers in the store
    var i: u32 = 0;
    while (i < self.idents.interner.outer_indices.items.len) : (i += 1) {
        const ident_idx = Ident.Idx{ .idx = @truncate(i), .attributes = .{ .effectful = false, .ignored = false, .reassignable = false } };
        const existing_name = self.getIdent(ident_idx);
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

fn hasSeenVar(self: *const TypeWriter, var_: Var) bool {
    for (self.seen.items) |seen| {
        if (seen == var_) return true;
    }
    return false;
}

/// Convert a var to a type string
fn writeVarWithContext(self: *TypeWriter, var_: Var, context: TypeContext, root_var: Var) std.mem.Allocator.Error!void {
    if (@intFromEnum(var_) >= self.types.slots.backing.len()) {
        // Debug assert that the variable is in bounds - if not, we have a bug in type checking
        _ = try self.buf.writer().write("invalid_type");
    } else {
        const resolved = self.types.resolveVar(var_);
        if (self.hasSeenVar(resolved.var_)) {
            _ = try self.buf.writer().write("...");
        } else {
            try self.seen.append(var_);
            defer _ = self.seen.pop();

            switch (resolved.desc.content) {
                .flex_var => |mb_ident_idx| {
                    if (mb_ident_idx) |ident_idx| {
                        _ = try self.buf.writer().write(self.getIdent(ident_idx));
                    } else {
                        try self.writeFlexVarName(var_, context, root_var);
                    }
                },
                .rigid_var => |ident_idx| {
                    _ = try self.buf.writer().write(self.getIdent(ident_idx));
                    // Useful in debugging to see if a var is rigid or not
                    // _ = try self.buf.writer().write("[r]");
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

fn writeVar(self: *TypeWriter, var_: Var, root_var: Var) std.mem.Allocator.Error!void {
    try self.writeVarWithContext(var_, .General, root_var);
}

/// Write an alias type
fn writeAlias(self: *TypeWriter, alias: Alias, root_var: Var) std.mem.Allocator.Error!void {
    _ = try self.buf.writer().write(self.getIdent(alias.ident.ident_idx));
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
fn writeFlatType(self: *TypeWriter, flat_type: FlatType, root_var: Var) std.mem.Allocator.Error!void {
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
fn writeTuple(self: *TypeWriter, tuple: Tuple, root_var: Var) std.mem.Allocator.Error!void {
    const elems = self.types.sliceVars(tuple.elems);
    _ = try self.buf.writer().write("(");
    for (elems, 0..) |elem, i| {
        if (i > 0) _ = try self.buf.writer().write(", ");
        try self.writeVarWithContext(elem, .TupleFieldContent, root_var);
    }
    _ = try self.buf.writer().write(")");
}

/// Write a nominal type
fn writeNominalType(self: *TypeWriter, nominal_type: NominalType, root_var: Var) std.mem.Allocator.Error!void {
    _ = try self.buf.writer().write(self.getIdent(nominal_type.ident.ident_idx));

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
fn writeRecordFields(self: *TypeWriter, fields: RecordField.SafeMultiList.Range, root_var: Var) std.mem.Allocator.Error!void {
    if (fields.isEmpty()) {
        _ = try self.buf.writer().write("{}");
        return;
    }

    _ = try self.buf.writer().write("{ ");

    const fields_slice = self.types.getRecordFieldsSlice(fields);

    // Write first field - we already verified that there's at least one field
    _ = try self.buf.writer().write(self.getIdent(fields_slice.items(.name)[0]));
    _ = try self.buf.writer().write(": ");
    try self.writeVarWithContext(fields_slice.items(.var_)[0], .RecordFieldContent, root_var);

    // Write remaining fields
    for (fields_slice.items(.name)[1..], fields_slice.items(.var_)[1..]) |name, var_| {
        _ = try self.buf.writer().write(", ");
        _ = try self.buf.writer().write(self.getIdent(name));
        _ = try self.buf.writer().write(": ");
        try self.writeVarWithContext(var_, .RecordFieldContent, root_var);
    }

    _ = try self.buf.writer().write(" }");
}

/// Write a function type with a specific arrow (`->` or `=>`)
fn writeFuncWithArrow(self: *TypeWriter, func: Func, arrow: []const u8, root_var: Var) std.mem.Allocator.Error!void {
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
fn writeRecord(self: *TypeWriter, record: Record, root_var: Var) std.mem.Allocator.Error!void {
    const fields = self.types.getRecordFieldsSlice(record.fields);

    _ = try self.buf.writer().write("{ ");
    for (fields.items(.name), fields.items(.var_), 0..) |field_name, field_var, i| {
        if (i > 0) _ = try self.buf.writer().write(", ");
        _ = try self.buf.writer().write(self.getIdent(field_name));
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
                    _ = try self.buf.writer().write(self.getIdent(field_name));
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
            _ = try self.buf.writer().write(self.getIdent(ident_idx));
        },
        else => {
            if (fields.len > 0) _ = try self.buf.writer().write(", ");
            try self.writeVarWithContext(record.ext, .RecordExtension, root_var);
        },
    }

    _ = try self.buf.writer().write(" }");
}

/// Helper to write record extension, handling nested records
fn writeRecordExtension(self: *TypeWriter, ext_var: Var, num_fields: usize, root_var: Var) std.mem.Allocator.Error!void {
    const ext_resolved = self.types.resolveVar(ext_var);
    switch (ext_resolved.desc.content) {
        .structure => |flat_type| switch (flat_type) {
            .empty_record => {}, // Don't show empty extension
            .record => |ext_record| {
                // Flatten nested record extensions
                const ext_fields = self.types.getRecordFieldsSlice(ext_record.fields);
                for (ext_fields.items(.name), ext_fields.items(.var_)) |field_name, field_var| {
                    _ = try self.buf.writer().write(", ");
                    _ = try self.buf.writer().write(self.getIdent(field_name));
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
            _ = try self.buf.writer().write(self.getIdent(ident_idx));
        },
        else => {
            // Show other types (aliases, errors, etc)
            if (num_fields > 0) _ = try self.buf.writer().write(", ");
            try self.writeVarWithContext(ext_var, .RecordExtension, root_var);
        },
    }
}

/// Write a tag union type
fn writeTagUnion(self: *TypeWriter, tag_union: TagUnion, root_var: Var) std.mem.Allocator.Error!void {
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
                _ = try self.buf.writer().write(self.getIdent(ident_idx));
            } else {
                try self.writeFlexVarName(tag_union.ext, .TagUnionExtension, root_var);
            }
        },
        .structure => |flat_type| switch (flat_type) {
            .empty_tag_union => {}, // Don't show empty extension
            else => {
                try self.writeVarWithContext(tag_union.ext, .TagUnionExtension, root_var);
            },
        },
        .rigid_var => |ident_idx| {
            _ = try self.buf.writer().write(self.getIdent(ident_idx));
            // _ = try self.buf.writer().write("[r]");
        },
        else => {
            try self.writeVarWithContext(tag_union.ext, .TagUnionExtension, root_var);
        },
    }
}

/// Write a single tag
fn writeTag(self: *TypeWriter, tag: Tag, root_var: Var) std.mem.Allocator.Error!void {
    _ = try self.buf.writer().write(self.getIdent(tag.name));
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
fn writeNum(self: *TypeWriter, num: Num, root_var: Var) std.mem.Allocator.Error!void {
    switch (num) {
        .num_poly => |poly| {
            _ = try self.buf.writer().write("Num(");
            try self.writeVarWithContext(poly.var_, .NumContent, root_var);
            _ = try self.buf.writer().write(")");
        },
        .int_poly => |poly| {
            _ = try self.buf.writer().write("Int(");
            try self.writeVarWithContext(poly, .NumContent, root_var);
            _ = try self.buf.writer().write(")");
        },
        .frac_poly => |poly| {
            _ = try self.buf.writer().write("Frac(");
            try self.writeVarWithContext(poly, .NumContent, root_var);
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
            try self.writeIntType(prec, .precision);
        },
        .frac_precision => |prec| {
            try self.writeFracType(prec, .precision);
        },
        .num_compact => |compact| {
            switch (compact) {
                .int => |prec| {
                    try self.writeIntType(prec, .compacted);
                },
                .frac => |prec| {
                    try self.writeFracType(prec, .compacted);
                },
            }
        },
    }
}

const NumPrecType = enum { precision, compacted };

fn writeIntType(self: *TypeWriter, prec: Num.Int.Precision, num_type: NumPrecType) std.mem.Allocator.Error!void {
    switch (num_type) {
        .compacted => {
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
        },
        .precision => {
            _ = switch (prec) {
                .u8 => try self.buf.writer().write("Unsigned8"),
                .i8 => try self.buf.writer().write("Signed8"),
                .u16 => try self.buf.writer().write("Unsigned16"),
                .i16 => try self.buf.writer().write("Signed16"),
                .u32 => try self.buf.writer().write("Unsigned32"),
                .i32 => try self.buf.writer().write("Signed32"),
                .u64 => try self.buf.writer().write("Unsigned64"),
                .i64 => try self.buf.writer().write("Signed64"),
                .u128 => try self.buf.writer().write("Unsigned128"),
                .i128 => try self.buf.writer().write("Signed128"),
            };
        },
    }
}

fn writeFracType(self: *TypeWriter, prec: Num.Frac.Precision, num_type: NumPrecType) std.mem.Allocator.Error!void {
    switch (num_type) {
        .compacted => {
            _ = switch (prec) {
                .f32 => try self.buf.writer().write("F32"),
                .f64 => try self.buf.writer().write("F64"),
                .dec => try self.buf.writer().write("Dec"),
            };
        },
        .precision => {
            _ = switch (prec) {
                .f32 => try self.buf.writer().write("Float32"),
                .f64 => try self.buf.writer().write("Float64"),
                .dec => try self.buf.writer().write("Decimal"),
            };
        },
    }
}

/// Generate a name for a flex var that may appear mulitple times in the type
pub fn writeFlexVarName(self: *TypeWriter, var_: Var, context: TypeContext, root_var: Var) std.mem.Allocator.Error!void {
    const resolved_var = self.types.resolveVar(var_).var_;

    // Check if we've seen this flex var before.
    if (self.flex_var_names_map.get(resolved_var)) |range| {
        // If so, then use that name
        _ = try self.buf.writer().write(
            self.flex_var_names.items[range.start..range.end],
        );
    } else {
        // Check if this variable appears multiple times
        const occurrences = self.countVarOccurrences(resolved_var, root_var);
        if (occurrences == 1) {
            // If it appears once, then generate and write the name
            _ = try self.buf.writer().write("_");
            try self.generateContextualName(context);
        } else {
            // If it appears more than once, then we have to track the name we
            // assign it so it appears consistently across the type str

            // Generate a new general var name. We do not use the context here
            // because that may be the current context the var appears in, but
            // the var may later appear in a different context
            const buf_start = self.buf.items.len;
            try self.generateContextualName(.General);
            const buf_end = self.buf.items.len;

            // Then write down the name we generated for later
            const flex_start = self.flex_var_names.items.len;
            try self.flex_var_names.appendSlice(self.buf.items[buf_start..buf_end]);
            const flex_end = self.flex_var_names.items.len;
            try self.flex_var_names_map.put(resolved_var, .{ .start = flex_start, .end = flex_end });
        }
    }
}

/// Count how many times a variable appears in a type
fn countVarOccurrences(self: *const TypeWriter, search_var: Var, root_var: Var) usize {
    var count: usize = 0;
    self.countVar(search_var, root_var, &count);
    return count;
}

fn countVar(self: *const TypeWriter, search_var: Var, current_var: Var, count: *usize) void {
    if (@intFromEnum(current_var) >= self.types.slots.backing.len()) return;

    const resolved = self.types.resolveVar(current_var);
    if (resolved.var_ == search_var) {
        count.* += 1;
    }

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

fn countVarInFlatType(self: *const TypeWriter, search_var: Var, flat_type: FlatType, count: *usize) void {
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

/// Retrieves the text representation of an identifier by its index.
/// This is used when formatting types that reference named identifiers.
pub fn getIdent(self: *const TypeWriter, idx: Ident.Idx) []const u8 {
    return self.idents.getText(idx);
}
