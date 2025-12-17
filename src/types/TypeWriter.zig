//! Type serialization utilities for writing type information as S-expressions.
//!
//! This module provides functionality to serialize type store contents and
//! individual types into S-expression format for debugging, inspection, and
//! external tool integration. The serialized output helps visualize the
//! compiler's internal type representations.

const std = @import("std");
const base = @import("base");
const types_mod = @import("types.zig");
const import_mapping_mod = @import("import_mapping.zig");
const debug = @import("debug.zig");

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
buf: std.array_list.Managed(u8),
seen: std.array_list.Managed(Var),
seen_count_var_occurrences: std.array_list.Managed(Var),
next_name_index: u32,
name_counters: std.EnumMap(TypeContext, u32),
flex_var_names_map: std.AutoHashMap(Var, FlexVarNameRange),
flex_var_names: std.array_list.Managed(u8),
static_dispatch_constraints: std.array_list.Managed(ConstraintWithDispatcher),
scratch_record_fields: std.array_list.Managed(types_mod.RecordField),
/// Mapping from fully-qualified type identifiers to their display names based on top-level imports.
/// This allows error messages to show "Str" instead of "Builtin.Str" for auto-imported types,
/// "Bar" instead of "Foo.Bar" for nested imports, and aliases like "Baz" instead of "Foo".
import_mapping: ?*const import_mapping_mod.ImportMapping,
/// The allocator used to create owned fields
gpa: std.mem.Allocator,

const FlexVarNameRange = struct { start: usize, end: usize };

/// A constraint paired with its dispatcher variable (the type that has the constraint)
const ConstraintWithDispatcher = struct {
    dispatcher_var: Var,
    constraint: types_mod.StaticDispatchConstraint,
};

pub fn initFromParts(
    gpa: std.mem.Allocator,
    types_store: *const TypesStore,
    idents: *const Ident.Store,
    import_mapping: ?*const import_mapping_mod.ImportMapping,
) std.mem.Allocator.Error!TypeWriter {
    return .{
        .types = types_store,
        .idents = idents,
        .buf = try std.array_list.Managed(u8).initCapacity(gpa, 32),
        .seen = try std.array_list.Managed(Var).initCapacity(gpa, 16),
        .seen_count_var_occurrences = try std.array_list.Managed(Var).initCapacity(gpa, 16),
        .next_name_index = 0,
        .name_counters = std.EnumMap(TypeContext, u32).init(.{}),
        .flex_var_names_map = std.AutoHashMap(Var, FlexVarNameRange).init(gpa),
        .flex_var_names = try std.array_list.Managed(u8).initCapacity(gpa, 32),
        .static_dispatch_constraints = try std.array_list.Managed(ConstraintWithDispatcher).initCapacity(gpa, 32),
        .scratch_record_fields = try std.array_list.Managed(types_mod.RecordField).initCapacity(gpa, 32),
        .import_mapping = import_mapping,
        .gpa = gpa,
    };
}

/// Deinit type writer
pub fn deinit(self: *TypeWriter) void {
    self.buf.deinit();
    self.seen.deinit();
    self.seen_count_var_occurrences.deinit();
    self.flex_var_names_map.deinit();
    self.flex_var_names.deinit();
    self.static_dispatch_constraints.deinit();
    self.scratch_record_fields.deinit();
    // import_mapping is borrowed, not owned, so don't deinit it
}

/// Update the import_mapping pointer. This is needed when the owning struct
/// is returned by value, which invalidates the original pointer.
pub fn setImportMapping(self: *TypeWriter, import_mapping: ?*const import_mapping_mod.ImportMapping) void {
    self.import_mapping = import_mapping;
}

/// Reset type writer state
pub fn reset(self: *TypeWriter) void {
    self.buf.clearRetainingCapacity();
    self.seen.clearRetainingCapacity();
    self.seen_count_var_occurrences.clearRetainingCapacity();
    self.flex_var_names_map.clearRetainingCapacity();
    self.flex_var_names.clearRetainingCapacity();
    self.static_dispatch_constraints.clearRetainingCapacity();
    self.scratch_record_fields.clearRetainingCapacity();

    self.next_name_index = 0;
    self.name_counters = std.EnumMap(TypeContext, u32).init(.{});
}

/// Writes the current var into the the writers buffer and returns a bytes slice
pub fn writeGet(self: *TypeWriter, var_: Var) std.mem.Allocator.Error![]const u8 {
    try self.write(var_);
    return self.get();
}

/// Returns the current contents of the type writer's buffer as a slice.
/// This contains the formatted type representation built up by write operations.
pub fn get(self: *const TypeWriter) []const u8 {
    return self.buf.items;
}

/// Writes a type variable to the buffer, formatting it as a human-readable string.
/// This clears any existing content in the buffer before writing.
pub fn write(self: *TypeWriter, var_: Var) std.mem.Allocator.Error!void {
    self.reset();
    try self.writeVar(var_, var_);

    if (self.static_dispatch_constraints.items.len > 0) {
        _ = try self.buf.writer().write(" where [");
        // Use a while loop with index instead of for loop over slice, because
        // writeVar may collect additional constraints into this local display list
        // while printing existing ones. This is NOT unification - we're just reading
        // existing constraint data from nested types and gathering them for display.
        // (e.g., `!=` desugars to `is_eq().not()` - when printing the `is_eq` constraint's
        // return type `f`, we find that `f` has a `not` constraint which we also need to display)
        var i: usize = 0;
        while (i < self.static_dispatch_constraints.items.len) : (i += 1) {
            const item = self.static_dispatch_constraints.items[i];
            if (i > 0) {
                _ = try self.buf.writer().write(", ");
            }

            try self.writeVar(item.dispatcher_var, var_);
            _ = try self.buf.writer().write(".");
            _ = try self.buf.writer().write(self.idents.getText(item.constraint.fn_name));
            _ = try self.buf.writer().write(" : ");
            try self.writeVar(item.constraint.fn_var, var_);
        }
        _ = try self.buf.writer().write("]");
    }
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
        // Variable is out of bounds - this can happen with corrupted type data
        _ = try self.buf.writer().write("Error");
        return;
    }

    const resolved = self.types.resolveVar(var_);

    if (@intFromEnum(resolved.var_) >= self.types.slots.backing.len()) {
        // Variable is out of bounds - this can happen with corrupted type data
        _ = try self.buf.writer().write("Error");
        return;
    }

    // Check if resolution returned an error descriptor - bail immediately
    if (resolved.desc.content == .err) {
        _ = try self.buf.writer().write("Error");
        return;
    }

    if (self.hasSeenVar(resolved.var_)) {
        _ = try self.buf.writer().write("...");
    } else {
        try self.seen.append(resolved.var_);
        defer _ = self.seen.pop();

        switch (resolved.desc.content) {
            .flex => |flex| {
                if (flex.name) |ident_idx| {
                    _ = try self.buf.writer().write(self.getIdent(ident_idx));
                } else {
                    try self.writeFlexVarName(var_, context, root_var);
                }

                for (self.types.sliceStaticDispatchConstraints(flex.constraints)) |constraint| {
                    try self.appendStaticDispatchConstraint(var_, constraint);
                }
            },
            .rigid => |rigid| {
                _ = try self.buf.writer().write(self.getIdent(rigid.name));

                // Useful in debugging to see if a var is rigid or not
                // _ = try self.buf.writer().write("[r]");

                for (self.types.sliceStaticDispatchConstraints(rigid.constraints)) |constraint| {
                    try self.appendStaticDispatchConstraint(var_, constraint);
                }
            },
            .alias => |alias| {
                try self.writeAlias(alias, root_var);
            },
            .structure => |flat_type| {
                const should_wrap_in_parens = ((context == .FunctionArgument or context == .FunctionReturn) and (flat_type == .fn_effectful or flat_type == .fn_pure or flat_type == .fn_unbound));
                if (should_wrap_in_parens) {
                    _ = try self.buf.writer().write("(");
                }

                try self.writeFlatType(flat_type, root_var);

                if (should_wrap_in_parens) {
                    _ = try self.buf.writer().write(")");
                }
            },
            .recursion_var => |rec_var| {
                // Write the recursion var by writing the structure it points to
                try self.writeVar(rec_var.structure, root_var);
            },
            .err => {
                _ = try self.buf.writer().write("Error");
            },
        }

        // Useful in debugging to see the idx of a var
        // _ = try self.buf.writer().print("[{}]", .{@intFromEnum(resolved.var_)});
    }
}

fn writeVar(self: *TypeWriter, var_: Var, root_var: Var) std.mem.Allocator.Error!void {
    try self.writeVarWithContext(var_, .General, root_var);
}

/// Write an alias type
fn writeAlias(self: *TypeWriter, alias: Alias, root_var: Var) std.mem.Allocator.Error!void {
    _ = try self.buf.writer().write(self.getDisplayName(alias.ident.ident_idx));
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
        .tuple => |tuple| {
            try self.writeTuple(tuple, root_var);
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
    _ = try self.buf.writer().write(self.getDisplayName(nominal_type.ident.ident_idx));

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
    const scratch_fields_top = self.scratch_record_fields.items.len;
    defer self.scratch_record_fields.shrinkRetainingCapacity(scratch_fields_top);

    const ext = try self.gatherRecordFields(record.fields, record.ext);
    const gathered_fields = self.scratch_record_fields.items[scratch_fields_top..];
    const num_fields = gathered_fields.len;

    std.mem.sort(types_mod.RecordField, gathered_fields, self.idents, comptime types_mod.RecordField.sortByNameAsc);

    _ = try self.buf.writer().write("{ ");

    switch (ext) {
        .flex => |flex| {
            if (flex.payload.name) |ident_idx| {
                _ = try self.buf.writer().write("..");
                _ = try self.buf.writer().write(self.getIdent(ident_idx));
                if (num_fields > 0) _ = try self.buf.writer().write(", ");
            } else if (true) {
                // TODO: ^ here, we should consider polarity

                _ = try self.buf.writer().write("..");
                try self.writeFlexVarName(flex.var_, .RecordExtension, root_var);
                if (num_fields > 0) _ = try self.buf.writer().write(", ");
            }

            // Since don't recurse above, we must capture the static dispatch
            // constraints directly
            for (self.types.sliceStaticDispatchConstraints(flex.payload.constraints)) |constraint| {
                try self.appendStaticDispatchConstraint(flex.var_, constraint);
            }
        },
        .rigid => |rigid| {
            _ = try self.buf.writer().write("..");
            _ = try self.buf.writer().write(self.getIdent(rigid.name));
            if (num_fields > 0) _ = try self.buf.writer().write(", ");

            // Since don't recurse above, we must capture the static dispatch
            // constraints directly
            for (self.types.sliceStaticDispatchConstraints(rigid.constraints)) |constraint| {
                try self.appendStaticDispatchConstraint(record.ext, constraint);
            }
        },
        .unbound, .invalid, .empty_record => {},
    }

    for (gathered_fields, 0..) |field, i| {
        _ = try self.buf.writer().write(self.getIdent(field.name));
        _ = try self.buf.writer().write(": ");
        try self.writeVarWithContext(field.var_, .RecordFieldContent, root_var);

        if (i != gathered_fields.len - 1) _ = try self.buf.writer().write(", ");
    }

    _ = try self.buf.writer().write(" }");
}

/// Recursively unwrap all record fields
fn gatherRecordFields(self: *TypeWriter, fields: RecordField.SafeMultiList.Range, initial_ext: Var) std.mem.Allocator.Error!union(enum) {
    flex: struct { var_: Var, payload: types_mod.Flex },
    rigid: types_mod.Rigid,
    empty_record,
    unbound,
    invalid,
} {
    const slice = self.types.getRecordFieldsSlice(fields);
    try self.scratch_record_fields.ensureUnusedCapacity(fields.len());
    for (slice.items(.name), slice.items(.var_)) |name, var_| {
        self.scratch_record_fields.appendAssumeCapacity(.{ .name = name, .var_ = var_ });
    }

    var ext = initial_ext;
    var guard = debug.IterationGuard.init("TypeWriter.gatherRecordFields");
    while (true) {
        guard.tick();
        const resolved = self.types.resolveVar(ext);
        switch (resolved.desc.content) {
            .flex => |flex| {
                return .{ .flex = .{ .var_ = resolved.var_, .payload = flex } };
            },
            .rigid => |rigid| {
                return .{ .rigid = rigid };
            },
            .alias => |alias| {
                ext = self.types.getAliasBackingVar(alias);
            },
            .structure => |flat_type| {
                switch (flat_type) {
                    .record => |ext_record| {
                        const ext_slice = self.types.getRecordFieldsSlice(ext_record.fields);
                        try self.scratch_record_fields.ensureUnusedCapacity(ext_record.fields.len());
                        for (ext_slice.items(.name), ext_slice.items(.var_)) |name, var_| {
                            self.scratch_record_fields.appendAssumeCapacity(.{ .name = name, .var_ = var_ });
                        }
                        ext = ext_record.ext;
                    },
                    .record_unbound => |ext_fields| {
                        const ext_slice = self.types.getRecordFieldsSlice(ext_fields);
                        try self.scratch_record_fields.ensureUnusedCapacity(ext_fields.len());
                        for (ext_slice.items(.name), ext_slice.items(.var_)) |name, var_| {
                            self.scratch_record_fields.appendAssumeCapacity(.{ .name = name, .var_ = var_ });
                        }
                        return .unbound;
                    },
                    .empty_record => return .empty_record,
                    else => return .invalid,
                }
            },
            else => return .invalid,
        }
    }
}

/// Write a tag union type
fn writeTagUnion(self: *TypeWriter, tag_union: TagUnion, root_var: Var) std.mem.Allocator.Error!void {
    _ = try self.buf.writer().write("[");

    // Bounds check the tags range before iterating
    const tags_start_idx = @intFromEnum(tag_union.tags.start);
    const tags_len = self.types.tags.len();
    if (tags_start_idx >= tags_len or tags_start_idx + tag_union.tags.count > tags_len) {
        // Tags range is out of bounds - return error indicator
        _ = try self.buf.writer().write("Error]");
        return;
    }

    var iter = tag_union.tags.iterIndices();
    while (iter.next()) |tag_idx| {
        if (@intFromEnum(tag_idx) > @intFromEnum(tag_union.tags.start)) {
            _ = try self.buf.writer().write(", ");
        }

        const tag = self.types.tags.get(tag_idx);
        try self.writeTag(tag, root_var);
    }

    // Write extension variable inside the brackets with ".." prefix
    const ext_resolved = self.types.resolveVar(tag_union.ext);
    const has_tags = tag_union.tags.count > 0;

    switch (ext_resolved.desc.content) {
        .flex => |flex| {
            if (has_tags) _ = try self.buf.writer().write(", ");
            _ = try self.buf.writer().write("..");

            if (flex.name) |ident_idx| {
                _ = try self.buf.writer().write(self.getIdent(ident_idx));
            } else if (true) {
                // TODO: ^ here, we should consider polarity
                try self.writeFlexVarName(tag_union.ext, .TagUnionExtension, root_var);
            }

            _ = try self.buf.writer().write("]");

            for (self.types.sliceStaticDispatchConstraints(flex.constraints)) |constraint| {
                try self.appendStaticDispatchConstraint(tag_union.ext, constraint);
            }
        },
        .structure => |flat_type| switch (flat_type) {
            .empty_tag_union => {
                // Closed union - just close the bracket
                _ = try self.buf.writer().write("]");
            },
            else => {
                // Extension is a non-empty structure (e.g., another tag union)
                if (has_tags) _ = try self.buf.writer().write(", ");
                _ = try self.buf.writer().write("..");
                try self.writeVarWithContext(tag_union.ext, .TagUnionExtension, root_var);
                _ = try self.buf.writer().write("]");
            },
        },
        .rigid => |rigid| {
            if (has_tags) _ = try self.buf.writer().write(", ");
            _ = try self.buf.writer().write("..");
            _ = try self.buf.writer().write(self.getIdent(rigid.name));
            _ = try self.buf.writer().write("]");

            for (self.types.sliceStaticDispatchConstraints(rigid.constraints)) |constraint| {
                try self.appendStaticDispatchConstraint(tag_union.ext, constraint);
            }
        },
        .err => {
            // Extension resolved to error - write error indicator
            if (has_tags) _ = try self.buf.writer().write(", ");
            _ = try self.buf.writer().write("..Error]");
        },
        .alias => {
            if (has_tags) _ = try self.buf.writer().write(", ");
            _ = try self.buf.writer().write("..");
            try self.writeVarWithContext(tag_union.ext, .TagUnionExtension, root_var);
            _ = try self.buf.writer().write("]");
        },
        .recursion_var => {
            if (has_tags) _ = try self.buf.writer().write(", ");
            _ = try self.buf.writer().write("..");
            try self.writeVarWithContext(tag_union.ext, .TagUnionExtension, root_var);
            _ = try self.buf.writer().write("]");
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

/// Append a constraint with its dispatcher var to the list, if it doesn't already exist
fn appendStaticDispatchConstraint(self: *TypeWriter, dispatcher_var: Var, constraint_to_add: types_mod.StaticDispatchConstraint) std.mem.Allocator.Error!void {
    for (self.static_dispatch_constraints.items) |item| {
        if (item.constraint.fn_name == constraint_to_add.fn_name and item.constraint.fn_var == constraint_to_add.fn_var) {
            return;
        }
    }
    _ = try self.static_dispatch_constraints.append(.{
        .dispatcher_var = dispatcher_var,
        .constraint = constraint_to_add,
    });
}

/// Generate a name for a flex var that may appear multiple times in the type
pub fn writeFlexVarName(self: *TypeWriter, var_: Var, context: TypeContext, root_var: Var) std.mem.Allocator.Error!void {
    const resolved_var = self.types.resolveVar(var_).var_;

    // If resolved var is out of bounds, it's corrupted - just write a simple name
    if (@intFromEnum(resolved_var) >= self.types.slots.backing.len()) {
        _ = try self.buf.writer().write("_");
        try self.generateContextualName(context);
        return;
    }
    // Check if we've seen this flex var before.
    if (self.flex_var_names_map.get(resolved_var)) |range| {
        // If so, then use that name
        _ = try self.buf.writer().write(
            self.flex_var_names.items[range.start..range.end],
        );
    } else {
        // Check if this variable appears multiple times
        // Note: counting can fail with corrupted data, so we treat it as appearing once
        const occurrences = try self.countVarOccurrences(resolved_var, root_var);
        if (occurrences <= 1) {
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
fn countVarOccurrences(self: *TypeWriter, search_var: Var, root_var: Var) std.mem.Allocator.Error!usize {
    self.seen_count_var_occurrences.clearRetainingCapacity();

    var count: usize = 0;
    try self.countVar(search_var, root_var, &count);
    return count;
}

fn countVar(self: *TypeWriter, search_var: Var, current_var: Var, count: *usize) std.mem.Allocator.Error!void {
    if (@intFromEnum(current_var) >= self.types.slots.backing.len()) return;

    const resolved = self.types.resolveVar(current_var);

    // If resolution returned an error descriptor, stop traversing
    if (resolved.desc.content == .err) {
        return;
    }

    // Count if this is the search var

    // First, check if this is the var we are counting
    if (resolved.var_ == search_var) {
        count.* += 1;
    }

    // Check if we've already seen this var
    // This avoids infinite recursion
    for (self.seen_count_var_occurrences.items) |seen| {
        if (seen == resolved.var_) return;
    }

    // Record that we've seen this var
    try self.seen_count_var_occurrences.append(resolved.var_);
    defer _ = self.seen_count_var_occurrences.pop();

    // Then recurse
    switch (resolved.desc.content) {
        .flex => |flex| {
            const constraints = self.types.sliceStaticDispatchConstraints(flex.constraints);
            for (constraints) |constraint| {
                try self.countVar(search_var, constraint.fn_var, count);
            }
        },
        .rigid => |rigid| {
            const constraints = self.types.sliceStaticDispatchConstraints(rigid.constraints);
            for (constraints) |constraint| {
                try self.countVar(search_var, constraint.fn_var, count);
            }
        },
        .alias => |alias| {
            // For aliases, we only count occurrences in the type arguments
            var args_iter = self.types.iterAliasArgs(alias);
            while (args_iter.next()) |arg_var| {
                try self.countVar(search_var, arg_var, count);
            }
        },
        .structure => |flat_type| {
            try self.countVarInFlatType(search_var, flat_type, count);
        },
        .recursion_var => |rec_var| {
            // Count the structure the recursion var points to
            try self.countVar(search_var, rec_var.structure, count);
        },
        .err => {},
    }
}

fn countVarInFlatType(self: *TypeWriter, search_var: Var, flat_type: FlatType, count: *usize) std.mem.Allocator.Error!void {
    switch (flat_type) {
        .empty_record, .empty_tag_union => {},
        .tuple => |tuple| {
            const elems = self.types.sliceVars(tuple.elems);
            for (elems) |elem| {
                try self.countVar(search_var, elem, count);
            }
        },
        .nominal_type => |nominal_type| {
            var args_iter = self.types.iterNominalArgs(nominal_type);
            while (args_iter.next()) |arg_var| {
                try self.countVar(search_var, arg_var, count);
            }
        },
        .fn_pure, .fn_effectful, .fn_unbound => |func| {
            const args = self.types.sliceVars(func.args);
            for (args) |arg| {
                try self.countVar(search_var, arg, count);
            }
            try self.countVar(search_var, func.ret, count);
        },
        .record => |record| {
            const fields = self.types.getRecordFieldsSlice(record.fields);
            for (fields.items(.var_)) |field_var| {
                try self.countVar(search_var, field_var, count);
            }
            try self.countVar(search_var, record.ext, count);
        },
        .record_unbound => |fields| {
            const fields_slice = self.types.getRecordFieldsSlice(fields);
            for (fields_slice.items(.var_)) |field_var| {
                try self.countVar(search_var, field_var, count);
            }
        },
        .tag_union => |tag_union| {
            // Bounds check the tags range before iterating
            const tags_start_idx = @intFromEnum(tag_union.tags.start);
            const tags_len = self.types.tags.len();
            if (tags_start_idx >= tags_len or tags_start_idx + tag_union.tags.count > tags_len) {
                // Tags range is out of bounds - skip counting in corrupted data
                return;
            }

            var iter = tag_union.tags.iterIndices();
            while (iter.next()) |tag_idx| {
                const tag = self.types.tags.get(tag_idx);
                const args = self.types.sliceVars(tag.args);
                for (args) |arg_var| {
                    try self.countVar(search_var, arg_var, count);
                }
            }
            try self.countVar(search_var, tag_union.ext, count);
        },
    }
}

/// Retrieves the text representation of an identifier by its index.
/// This is used when formatting types that reference named identifiers.
pub fn getIdent(self: *const TypeWriter, idx: Ident.Idx) []const u8 {
    return self.idents.getText(idx);
}

/// Gets the display name for a type identifier, accounting for import mappings.
/// If the identifier is in the import_mapping, returns the mapped name.
/// Otherwise, returns the original identifier text.
fn getDisplayName(self: *const TypeWriter, idx: Ident.Idx) []const u8 {
    if (self.import_mapping) |mapping| {
        if (mapping.get(idx)) |display_idx| {
            return self.idents.getText(display_idx);
        }
    }

    const name = self.idents.getText(idx);

    // Strip "Builtin." prefix from builtin types for display
    // Types like "Builtin.Try" should display as "Try", "Builtin.Num.Numeral" as "Numeral"
    if (std.mem.startsWith(u8, name, "Builtin.")) {
        const without_builtin = name[8..]; // Skip "Builtin."
        // Also strip "Num." if present (e.g., "Builtin.Num.Numeral" -> "Numeral")
        if (std.mem.startsWith(u8, without_builtin, "Num.")) {
            return without_builtin[4..]; // Skip "Num."
        }
        return without_builtin;
    }

    // Strip "Num." prefix from builtin number types for display
    // Number types are stored as "Num.U8", "Num.F32", etc. but should display as "U8", "F32"
    if (std.mem.startsWith(u8, name, "Num.")) {
        return name[4..]; // Skip "Num."
    }

    return name;
}
