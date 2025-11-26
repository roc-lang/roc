//! Capture the state of an type at a point in time for the purpose of error reporting.

const std = @import("std");
const base = @import("base");
const collections = @import("collections");
const types = @import("types");
const can = @import("can");

const Allocator = std.mem.Allocator;
const ModuleEnv = can.ModuleEnv;
const TypesStore = types.Store;
const Ident = base.Ident;

/// Index enum for SnapshotContentList
pub const SnapshotContentIdx = SnapshotContentList.Idx;

const SnapshotContentList = collections.SafeList(SnapshotContent);
const SnapshotContentIdxSafeList = collections.SafeList(SnapshotContentIdx);
const SnapshotRecordFieldSafeList = collections.SafeMultiList(SnapshotRecordField);
const SnapshotTagSafeList = collections.SafeMultiList(SnapshotTag);
const SnapshotStaticDispatchConstraintSafeList = collections.SafeList(SnapshotStaticDispatchConstraint);
const MkSafeMultiList = collections.SafeMultiList;

/// The content of a type snapshot, mirroring types.Content for error reporting.
pub const SnapshotContent = union(enum) {
    flex: SnapshotFlex,
    rigid: SnapshotRigid,
    alias: SnapshotAlias,
    structure: SnapshotFlatType,
    recursion_var: SnapshotRecursionVar,
    /// A recursive type reference. Stores the name of the type variable if available.
    recursive: ?Ident.Idx,
    err,
};

/// A snapshotted recursion variable that points to its recursive structure.
pub const SnapshotRecursionVar = struct {
    structure: SnapshotContentIdx,
    name: ?base.Ident.Idx,
};

/// A snapshotted flex (unbound) type variable with optional name and constraints.
pub const SnapshotFlex = struct {
    name: ?Ident.Idx,
    var_: Var,
    constraints: SnapshotStaticDispatchConstraintSafeList.Range,
};

/// A snapshotted rigid (bound) type variable with name and constraints.
pub const SnapshotRigid = struct {
    name: Ident.Idx,
    constraints: SnapshotStaticDispatchConstraintSafeList.Range,
};

/// A snapshotted type alias with its backing type and type variables.
pub const SnapshotAlias = struct {
    ident: types.TypeIdent,
    backing: SnapshotContentIdx,
    vars: SnapshotContentIdxSafeList.Range,
};

/// A snapshotted flat type structure (non-variable types like records, functions, etc).
pub const SnapshotFlatType = union(enum) {
    box: SnapshotContentIdx,
    tuple: SnapshotTuple,
    nominal_type: SnapshotNominalType,
    fn_pure: SnapshotFunc,
    fn_effectful: SnapshotFunc,
    fn_unbound: SnapshotFunc,
    record: SnapshotRecord,
    record_unbound: SnapshotRecordFieldSafeList.Range,
    empty_record,
    tag_union: SnapshotTagUnion,
    empty_tag_union,
};

/// A snapshotted tuple type with its element types.
pub const SnapshotTuple = struct {
    elems: SnapshotContentIdxSafeList.Range,
};

/// A snapshotted nominal (named) type with its type parameters and origin module.
pub const SnapshotNominalType = struct {
    ident: types.TypeIdent,
    vars: SnapshotContentIdxSafeList.Range,
    origin_module: Ident.Idx,
};

/// A snapshotted function type with argument types, return type, and instantiation flag.
pub const SnapshotFunc = struct {
    args: SnapshotContentIdxSafeList.Range,
    ret: SnapshotContentIdx,
    needs_instantiation: bool,
};

/// A snapshotted record type with fields and extension variable.
pub const SnapshotRecord = struct {
    fields: SnapshotRecordFieldSafeList.Range,
    ext: SnapshotContentIdx,
};

/// A single field in a snapshotted record type.
pub const SnapshotRecordField = struct {
    name: Ident.Idx,
    content: SnapshotContentIdx,

    const Self = @This();

    /// Returns true if field `a` should sort before field `b` by name.
    pub fn sortByNameAsc(ident_store: *const Ident.Store, a: Self, b: Self) bool {
        return Self.orderByName(ident_store, a, b) == .lt;
    }

    /// Compares two record fields by their name for ordering.
    pub fn orderByName(store: *const Ident.Store, a: Self, b: Self) std.math.Order {
        const a_text = store.getText(a.name);
        const b_text = store.getText(b.name);
        return std.mem.order(u8, a_text, b_text);
    }
};

/// A snapshotted tag union type with its tags and extension variable.
pub const SnapshotTagUnion = struct {
    tags: SnapshotTagSafeList.Range,
    ext: SnapshotContentIdx,
};

/// A single tag in a snapshotted tag union with its name and argument types.
pub const SnapshotTag = struct {
    name: Ident.Idx,
    args: SnapshotContentIdxSafeList.Range,
};

/// A snapshotted static dispatch constraint for method resolution.
pub const SnapshotStaticDispatchConstraint = struct {
    fn_name: Ident.Idx,
    fn_content: SnapshotContentIdx,
    /// The type variable that has this constraint (the dispatcher).
    /// This is the type that the method is called on.
    dispatcher: SnapshotContentIdx,
};

const Var = types.Var;
const Content = types.Content;
const Flex = types.Flex;
const Rigid = types.Rigid;

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

/// Self-contained snapshot store with fully resolved content (ie no Vars)
///
/// Whenever a type error occurs, we update the `Var` in the type store to
/// have `.err` content. This is necessary to continue type-checking but
/// looses essential error information. So before doing this, we create a fully
/// resolved snapshot of the type that we can use in reporting
///
/// Entry point is `deepCopyVar`
pub const Store = struct {
    const Self = @This();

    gpa: Allocator,

    // Content storage
    contents: SnapshotContentList,

    // Catch recursive references
    seen_vars: base.Scratch(Var),

    /// Storage for compound type parts
    content_indexes: SnapshotContentIdxSafeList,
    record_fields: SnapshotRecordFieldSafeList,
    tags: SnapshotTagSafeList,
    static_dispatch_constraints: SnapshotStaticDispatchConstraintSafeList,

    // Scratch
    scratch_content: base.Scratch(SnapshotContentIdx),
    scratch_tags: base.Scratch(SnapshotTag),
    scratch_record_fields: base.Scratch(SnapshotRecordField),
    scratch_static_dispatch_constraints: base.Scratch(SnapshotStaticDispatchConstraint),

    pub fn initCapacity(gpa: Allocator, capacity: usize) std.mem.Allocator.Error!Self {
        return .{
            .gpa = gpa,
            .contents = try SnapshotContentList.initCapacity(gpa, capacity),
            .seen_vars = try base.Scratch(Var).init(gpa),
            .content_indexes = try SnapshotContentIdxSafeList.initCapacity(gpa, capacity),
            .record_fields = try SnapshotRecordFieldSafeList.initCapacity(gpa, 256),
            .tags = try SnapshotTagSafeList.initCapacity(gpa, 256),
            .static_dispatch_constraints = try SnapshotStaticDispatchConstraintSafeList.initCapacity(gpa, 64),
            .scratch_content = try base.Scratch(SnapshotContentIdx).init(gpa),
            .scratch_tags = try base.Scratch(SnapshotTag).init(gpa),
            .scratch_record_fields = try base.Scratch(SnapshotRecordField).init(gpa),
            .scratch_static_dispatch_constraints = try base.Scratch(SnapshotStaticDispatchConstraint).init(gpa),
        };
    }

    pub fn deinit(self: *Self) void {
        self.contents.deinit(self.gpa);
        self.seen_vars.deinit();
        self.content_indexes.deinit(self.gpa);
        self.record_fields.deinit(self.gpa);
        self.tags.deinit(self.gpa);
        self.static_dispatch_constraints.deinit(self.gpa);
        self.scratch_content.deinit();
        self.scratch_tags.deinit();
        self.scratch_record_fields.deinit();
        self.scratch_static_dispatch_constraints.deinit();
    }

    /// Create a deep snapshot from a Var, storing it in this SnapshotStore
    /// Deep copy a type variable's content into self-contained snapshot storage
    pub fn deepCopyVar(self: *Self, store: *const TypesStore, var_: types.Var) std.mem.Allocator.Error!SnapshotContentIdx {
        const resolved = store.resolveVar(var_);

        // Check if we've seen this variable
        var has_seen_var = false;
        for (self.seen_vars.items.items) |seen_var| {
            if (seen_var == resolved.var_) {
                has_seen_var = true;
                break;
            }
        }

        // If we've seen this variable, then return it as a recursive type
        // Try to extract the name from the content for better error messages
        if (has_seen_var) {
            const recursive_name: ?Ident.Idx = switch (resolved.desc.content) {
                .flex => |flex| flex.name,
                .rigid => |rigid| rigid.name,
                else => null,
            };
            return try self.contents.append(self.gpa, .{ .recursive = recursive_name });
        }

        // If not, add it to the seen list
        try self.seen_vars.append(resolved.var_);
        defer _ = self.seen_vars.pop();

        return try self.deepCopyContent(store, resolved.var_, resolved.desc.content);
    }

    fn deepCopyFlex(self: *Self, store: *const TypesStore, var_: types.Var, flex: types.Flex) std.mem.Allocator.Error!SnapshotFlex {
        return SnapshotFlex{
            .name = flex.name,
            .var_ = var_,
            .constraints = try self.deepCopyStaticDispatchConstraintRange(store, flex.constraints),
        };
    }

    fn deepCopyRigid(self: *Self, store: *const TypesStore, rigid: types.Rigid) std.mem.Allocator.Error!SnapshotRigid {
        return SnapshotRigid{
            .name = rigid.name,
            .constraints = try self.deepCopyStaticDispatchConstraintRange(store, rigid.constraints),
        };
    }

    fn deepCopyStaticDispatchConstraintRange(
        self: *Self,
        store: *const TypesStore,
        range: types.StaticDispatchConstraint.SafeList.Range,
    ) std.mem.Allocator.Error!SnapshotStaticDispatchConstraintSafeList.Range {
        const scratch_top = self.scratch_static_dispatch_constraints.top();
        defer self.scratch_static_dispatch_constraints.clearFrom(scratch_top);

        for (store.sliceStaticDispatchConstraints(range)) |constraint| {
            try self.scratch_static_dispatch_constraints.append(try self.deepCopyStaticDispatchConstraint(store, constraint));
        }

        return self.static_dispatch_constraints.appendSlice(self.gpa, self.scratch_static_dispatch_constraints.sliceFromStart(scratch_top));
    }

    fn deepCopyStaticDispatchConstraint(
        self: *Self,
        store: *const TypesStore,
        constraint: types.StaticDispatchConstraint,
    ) std.mem.Allocator.Error!SnapshotStaticDispatchConstraint {
        return SnapshotStaticDispatchConstraint{
            .fn_name = constraint.fn_name,
            .fn_content = try self.deepCopyVar(store, constraint.fn_var),
            // Dispatcher will be set when collecting constraints during write
            .dispatcher = @enumFromInt(0),
        };
    }

    fn deepCopyContent(self: *Self, store: *const TypesStore, var_: types.Var, content: Content) std.mem.Allocator.Error!SnapshotContentIdx {
        const deep_content = switch (content) {
            .flex => |flex| SnapshotContent{ .flex = try self.deepCopyFlex(store, var_, flex) },
            .rigid => |rigid| SnapshotContent{ .rigid = try self.deepCopyRigid(store, rigid) },
            .alias => |alias| SnapshotContent{ .alias = try self.deepCopyAlias(store, alias) },
            .structure => |flat_type| SnapshotContent{ .structure = try self.deepCopyFlatType(store, flat_type) },
            .recursion_var => |rec_var| blk: {
                // Snapshot the recursion var by snapshotting the structure it points to
                const structure_snapshot = try self.deepCopyVar(store, rec_var.structure);
                break :blk SnapshotContent{ .recursion_var = .{ .structure = structure_snapshot, .name = rec_var.name } };
            },
            .err => SnapshotContent.err,
        };

        return try self.contents.append(self.gpa, deep_content);
    }

    fn deepCopyFlatType(self: *Self, store: *const TypesStore, flat_type: types.FlatType) std.mem.Allocator.Error!SnapshotFlatType {
        return switch (flat_type) {
            .tuple => |tuple| SnapshotFlatType{ .tuple = try self.deepCopyTuple(store, tuple) },
            .nominal_type => |nominal_type| SnapshotFlatType{ .nominal_type = try self.deepCopyNominalType(store, nominal_type) },
            .fn_pure => |func| SnapshotFlatType{ .fn_pure = try self.deepCopyFunc(store, func) },
            .fn_effectful => |func| SnapshotFlatType{ .fn_effectful = try self.deepCopyFunc(store, func) },
            .fn_unbound => |func| SnapshotFlatType{ .fn_unbound = try self.deepCopyFunc(store, func) },
            .record => |record| SnapshotFlatType{ .record = try self.deepCopyRecord(store, record) },
            .record_unbound => |fields| SnapshotFlatType{ .record_unbound = try self.deepCopyRecordFields(store, fields) },
            .empty_record => SnapshotFlatType.empty_record,
            .tag_union => |tag_union| SnapshotFlatType{ .tag_union = try self.deepCopyTagUnion(store, tag_union) },
            .empty_tag_union => SnapshotFlatType.empty_tag_union,
        };
    }

    fn deepCopyAlias(self: *Self, store: *const TypesStore, alias: types.Alias) std.mem.Allocator.Error!SnapshotAlias {
        const backing_var = store.getAliasBackingVar(alias);
        const deep_backing = try self.deepCopyVar(store, backing_var);

        // Mark starting position in the scratch array
        const scratch_top = self.scratch_content.top();

        // Iterate and append to scratch array
        var arg_iter = store.iterAliasArgs(alias);
        while (arg_iter.next()) |arg_var| {
            const deep_arg = try self.deepCopyVar(store, arg_var);
            try self.scratch_content.append(deep_arg);
        }

        // Append scratch to backing array, and shrink scratch
        const args_range = try self.content_indexes.appendSlice(self.gpa, self.scratch_content.sliceFromStart(scratch_top));
        self.scratch_content.clearFrom(scratch_top);

        return SnapshotAlias{
            .ident = alias.ident,
            .backing = deep_backing,
            .vars = args_range,
        };
    }

    fn deepCopyTuple(self: *Self, store: *const TypesStore, tuple: types.Tuple) std.mem.Allocator.Error!SnapshotTuple {
        const elems_slice = store.sliceVars(tuple.elems);

        // Mark starting position in the scratch array
        const scratch_top = self.scratch_content.top();

        // Iterate and append to scratch array
        for (elems_slice) |elem_var| {
            const deep_elem = try self.deepCopyVar(store, elem_var);
            try self.scratch_content.append(deep_elem);
        }

        // Append scratch to backing array, and shrink scratch
        const elems_range = try self.content_indexes.appendSlice(self.gpa, self.scratch_content.sliceFromStart(scratch_top));
        self.scratch_content.clearFrom(scratch_top);

        return SnapshotTuple{
            .elems = elems_range,
        };
    }

    fn deepCopyNominalType(self: *Self, store: *const TypesStore, nominal_type: types.NominalType) std.mem.Allocator.Error!SnapshotNominalType {
        // Mark starting position in the scratch array
        const scratch_top = self.scratch_content.top();

        // Add backing var (must be first)
        const backing_var = store.getNominalBackingVar(nominal_type);
        const deep_var = try self.deepCopyVar(store, backing_var);
        try self.scratch_content.append(deep_var);

        // Add args after
        var arg_iter = store.iterNominalArgs(nominal_type);
        while (arg_iter.next()) |arg_var| {
            const deep_arg = try self.deepCopyVar(store, arg_var);
            try self.scratch_content.append(deep_arg);
        }

        // Append scratch to backing array, and shrink scratch
        const args_range = try self.content_indexes.appendSlice(self.gpa, self.scratch_content.sliceFromStart(scratch_top));
        self.scratch_content.clearFrom(scratch_top);

        return SnapshotNominalType{
            .ident = nominal_type.ident,
            .vars = args_range,
            .origin_module = nominal_type.origin_module,
        };
    }

    fn deepCopyFunc(self: *Self, store: *const TypesStore, func: types.Func) std.mem.Allocator.Error!SnapshotFunc {
        const args_slice = store.sliceVars(func.args);

        // Mark starting position in the scratch array
        const scratch_top = self.scratch_content.top();

        // Iterate and append directly
        for (args_slice) |arg_var| {
            const deep_arg = try self.deepCopyVar(store, arg_var);
            try self.scratch_content.append(deep_arg);
        }

        // Append scratch to backing array, and shrink scratch
        const args_range = try self.content_indexes.appendSlice(self.gpa, self.scratch_content.sliceFromStart(scratch_top));
        self.scratch_content.clearFrom(scratch_top);

        // Deep copy return type
        const deep_ret = try self.deepCopyVar(store, func.ret);

        return SnapshotFunc{
            .args = args_range,
            .ret = deep_ret,
            .needs_instantiation = func.needs_instantiation,
        };
    }

    fn deepCopyRecordFields(self: *Self, store: *const TypesStore, fields: types.RecordField.SafeMultiList.Range) std.mem.Allocator.Error!SnapshotRecordFieldSafeList.Range {
        // Mark starting position in the scratch array
        const scratch_top = self.scratch_record_fields.top();

        const fields_slice = store.getRecordFieldsSlice(fields);
        for (fields_slice.items(.name), fields_slice.items(.var_)) |name, var_| {
            const deep_field_content = try self.deepCopyVar(store, var_);

            const snapshot_field = SnapshotRecordField{
                .name = name,
                .content = deep_field_content,
            };

            try self.scratch_record_fields.append(snapshot_field);
        }

        // Append scratch to backing array, and shrink scratch
        const fields_range = try self.record_fields.appendSlice(self.gpa, self.scratch_record_fields.sliceFromStart(scratch_top));
        self.scratch_record_fields.clearFrom(scratch_top);

        return fields_range;
    }

    fn deepCopyRecord(self: *Self, store: *const TypesStore, record: types.Record) std.mem.Allocator.Error!SnapshotRecord {
        // Mark starting position in the scratch array
        const scratch_top = self.scratch_record_fields.top();

        // Iterate and append to scratch array
        var fields_iter = record.fields.iterIndices();
        while (fields_iter.next()) |field_idx| {
            const field = store.record_fields.get(field_idx);

            const deep_field_content = try self.deepCopyVar(store, field.var_);

            const snapshot_field = SnapshotRecordField{
                .name = field.name,
                .content = deep_field_content,
            };

            try self.scratch_record_fields.append(snapshot_field);
        }

        // Append scratch to backing array, and shrink scratch
        const fields_range = try self.record_fields.appendSlice(self.gpa, self.scratch_record_fields.sliceFromStart(scratch_top));
        self.scratch_content.clearFrom(scratch_top);

        // Deep copy extension type
        const deep_ext = try self.deepCopyVar(store, record.ext);

        return SnapshotRecord{
            .fields = fields_range,
            .ext = deep_ext,
        };
    }

    fn deepCopyTagUnion(self: *Self, store: *const TypesStore, tag_union: types.TagUnion) std.mem.Allocator.Error!SnapshotTagUnion {
        // Mark starting position in the scratch array for tags
        const tags_scratch_top = self.scratch_tags.top();

        // Iterate over tags and append to scratch array
        var tags_iter = tag_union.tags.iterIndices();
        while (tags_iter.next()) |tag_idx| {
            const tag = store.tags.get(tag_idx);

            const tag_args_slice = store.sliceVars(tag.args);

            // Mark starting position in the scratch array for this tag's arguments
            const content_scratch_top = self.scratch_content.top();

            // Iterate over tag arguments and append to scratch array
            for (tag_args_slice) |tag_arg_var| {
                const deep_tag_arg = try self.deepCopyVar(store, tag_arg_var);
                try self.scratch_content.append(deep_tag_arg);
            }

            // Append scratch to backing array, and shrink scratch
            const tag_args_range = try self.content_indexes.appendSlice(self.gpa, self.scratch_content.sliceFromStart(content_scratch_top));
            self.scratch_content.clearFrom(content_scratch_top);

            // Create and append the snapshot tag to scratch
            const snapshot_tag = SnapshotTag{
                .name = tag.name,
                .args = tag_args_range,
            };

            try self.scratch_tags.append(snapshot_tag);
        }

        // Append scratch tags to backing array, and shrink scratch
        const tags_range = try self.tags.appendSlice(self.gpa, self.scratch_tags.sliceFromStart(tags_scratch_top));
        self.scratch_tags.clearFrom(tags_scratch_top);

        // Deep copy extension type
        const deep_ext = try self.deepCopyVar(store, tag_union.ext);

        return SnapshotTagUnion{
            .tags = tags_range,
            .ext = deep_ext,
        };
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

            // Check all identifiers in the store
            const exists = self.idents.interner.contains(candidate_name);

            if (!exists) {
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

    fn generateContextualName(self: *Self, context: TypeContext) !void {
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
        const max_attempts = self.idents.interner.entry_count + 1;
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
                // This name is available, write it to the writer
                for (candidate_name) |c| {
                    try self.buf.writer().writeByte(c);
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

    /// Convert a content to a type string
    pub fn write(self: *Self, idx: SnapshotContentIdx) Allocator.Error!void {
        try self.writeWithContext(idx, .General, idx);

        if (self.static_dispatch_constraints.items.len > 0) {
            _ = try self.buf.writer().write(" where [");
            for (self.static_dispatch_constraints.items, 0..) |constraint, i| {
                if (i > 0) {
                    _ = try self.buf.writer().write(", ");
                }

                try self.writeWithContext(constraint.dispatcher, .General, idx);
                _ = try self.buf.writer().write(".");
                _ = try self.buf.writer().write(self.idents.getText(constraint.fn_name));
                _ = try self.buf.writer().write(" : ");
                try self.writeWithContext(constraint.fn_content, .General, idx);
            }
            _ = try self.buf.writer().write("]");
        }
    }

    /// Count how many times a content appears in a type
    /// Convert a content to a type string with context
    fn writeWithContext(self: *Self, idx: SnapshotContentIdx, context: TypeContext, root_idx: SnapshotContentIdx) Allocator.Error!void {
        const content = self.snapshots.contents.get(idx);
        return self.writeContent(content.*, context, idx, root_idx);
    }

    /// Convert a content to a type string
    fn writeContent(self: *Self, content: SnapshotContent, context: TypeContext, content_idx: SnapshotContentIdx, root_idx: SnapshotContentIdx) Allocator.Error!void {
        switch (content) {
            .flex => |flex| {
                if (flex.name) |ident_idx| {
                    _ = try self.buf.writer().write(self.idents.getText(ident_idx));
                } else {
                    // If this flex var has constraints, use .General context so it gets a general name
                    // that will match when the constraint is written in the where clause
                    const var_context = if (flex.constraints.len() > 0) .General else context;
                    _ = try self.writeFlexVarName(flex.var_, content_idx, var_context, root_idx);
                }

                for (self.snapshots.sliceStaticDispatchConstraints(flex.constraints)) |constraint| {
                    try self.appendStaticDispatchConstraint(constraint, content_idx);
                }
            },
            .rigid => |rigid| {
                _ = try self.buf.writer().write(self.idents.getText(rigid.name));

                for (self.snapshots.sliceStaticDispatchConstraints(rigid.constraints)) |constraint| {
                    try self.appendStaticDispatchConstraint(constraint, content_idx);
                }

                // Useful in debugging to see if a var is rigid or not
                // _ = try self.buf.writer().write("[r]");
            },
            .alias => |alias| {
                try self.writeAlias(alias, root_idx);
            },
            .structure => |flat_type| {
                const should_wrap_in_parens = ((context == .FunctionArgument or context == .FunctionReturn) and (flat_type == .fn_effectful or flat_type == .fn_pure or flat_type == .fn_unbound));
                if (should_wrap_in_parens) {
                    _ = try self.buf.writer().write("(");
                }

                try self.writeFlatType(flat_type, root_idx);

                if (should_wrap_in_parens) {
                    _ = try self.buf.writer().write(")");
                }
            },
            .recursion_var => |rec_var| {
                // Write the recursion var by writing the structure it points to
                const structure_content = self.snapshots.getContent(rec_var.structure);
                try self.writeContent(structure_content, context, rec_var.structure, root_idx);
            },
            .recursive => |maybe_name| {
                if (maybe_name) |name_idx| {
                    _ = try self.buf.writer().write(self.idents.getText(name_idx));
                } else {
                    // Fallback if no name was captured - generate a contextual name
                    _ = try self.buf.writer().write("_");
                    try self.generateContextualName(context);
                }
            },
            .err => {
                _ = try self.buf.writer().write("Error");
            },
        }
    }

    /// Write an alias type
    fn writeAlias(self: *Self, alias: SnapshotAlias, root_idx: SnapshotContentIdx) Allocator.Error!void {
        const display_idx = if (self.import_mapping.get(alias.ident.ident_idx)) |mapped_idx| mapped_idx else alias.ident.ident_idx;
        _ = try self.buf.writer().write(self.idents.getText(display_idx));

        const vars = self.snapshots.sliceVars(alias.vars);
        if (vars.len > 0) {
            _ = try self.buf.writer().write("(");
            for (vars, 0..) |arg, i| {
                if (i > 0) _ = try self.buf.writer().write(", ");
                try self.writeWithContext(arg, .General, root_idx);
            }
            _ = try self.buf.writer().write(")");
        }
    }

    /// Convert a flat type to a type string
    fn writeFlatType(self: *Self, flat_type: SnapshotFlatType, root_idx: SnapshotContentIdx) Allocator.Error!void {
        switch (flat_type) {
            .box => |sub_var| {
                _ = try self.buf.writer().write("Box(");
                try self.writeWithContext(sub_var, .General, root_idx);
                _ = try self.buf.writer().write(")");
            },
            .tuple => |tuple| {
                try self.writeTuple(tuple, root_idx);
            },
            .nominal_type => |nominal_type| {
                try self.writeNominalType(nominal_type, root_idx);
            },
            .fn_pure => |func| {
                try self.writeFuncWithArrow(func, " -> ", root_idx);
            },
            .fn_effectful => |func| {
                try self.writeFuncWithArrow(func, " => ", root_idx);
            },
            .fn_unbound => |func| {
                try self.writeFuncWithArrow(func, " -> ", root_idx);
            },
            .record => |record| {
                try self.writeRecord(record, root_idx);
            },
            .record_unbound => |fields| {
                try self.writeRecordFields(fields, root_idx);
            },
            .empty_record => {
                _ = try self.buf.writer().write("{}");
            },
            .tag_union => |tag_union| {
                try self.writeTagUnion(tag_union, root_idx);
            },
            .empty_tag_union => {
                _ = try self.buf.writer().write("[]");
            },
        }
    }

    /// Write a tuple type
    fn writeTuple(self: *Self, tuple: SnapshotTuple, root_idx: SnapshotContentIdx) Allocator.Error!void {
        const elems = self.snapshots.sliceVars(tuple.elems);
        _ = try self.buf.writer().write("(");
        for (elems, 0..) |elem, i| {
            if (i > 0) _ = try self.buf.writer().write(", ");
            try self.writeWithContext(elem, .TupleFieldContent, root_idx);
        }
        _ = try self.buf.writer().write(")");
    }

    /// Write a nominal type
    pub fn writeNominalType(self: *Self, nominal_type: SnapshotNominalType, root_idx: SnapshotContentIdx) Allocator.Error!void {
        const display_idx = if (self.import_mapping.get(nominal_type.ident.ident_idx)) |mapped_idx| mapped_idx else nominal_type.ident.ident_idx;
        _ = try self.buf.writer().write(self.idents.getText(display_idx));

        // The 1st var is the nominal type's backing var, so we skip it
        var vars = self.snapshots.sliceVars(nominal_type.vars);
        std.debug.assert(vars.len > 0);
        vars = vars[1..];

        if (vars.len > 0) {
            _ = try self.buf.writer().write("(");
            for (vars, 0..) |arg, i| {
                if (i > 0) _ = try self.buf.writer().write(", ");
                try self.writeWithContext(arg, .General, root_idx);
            }
            _ = try self.buf.writer().write(")");
        }

        // Add origin information if it's from a different module
        if (self.current_module_idx) |current_idx| {
            if (nominal_type.origin_module != current_idx) {
                const origin_module_name = self.idents.getText(nominal_type.origin_module);
                _ = try self.buf.writer().write(" (from ");
                _ = try self.buf.writer().write(origin_module_name);
                _ = try self.buf.writer().write(")");
            }
        }
    }

    /// Write a function type with a specific arrow
    fn writeFuncWithArrow(self: *Self, func: SnapshotFunc, arrow: []const u8, root_idx: SnapshotContentIdx) Allocator.Error!void {
        const args = self.snapshots.sliceVars(func.args);

        // Write arguments
        if (args.len == 0) {
            _ = try self.buf.writer().write("({})");
        } else if (args.len == 1) {
            try self.writeWithContext(args[0], .FunctionArgument, root_idx);
        } else {
            for (args, 0..) |arg, i| {
                if (i > 0) _ = try self.buf.writer().write(", ");
                try self.writeWithContext(arg, .FunctionArgument, root_idx);
            }
        }

        _ = try self.buf.writer().write(arrow);

        try self.writeWithContext(func.ret, .FunctionReturn, root_idx);
    }

    /// Write a record type
    fn writeRecord(self: *Self, record: SnapshotRecord, root_idx: SnapshotContentIdx) Allocator.Error!void {
        const scratch_fields_top = self.scratch_record_fields.items.len;
        defer self.scratch_record_fields.shrinkRetainingCapacity(scratch_fields_top);

        const ext = try self.gatherRecordFields(record.fields, record.ext);
        const gathered_fields = self.scratch_record_fields.items[scratch_fields_top..];
        const num_fields = gathered_fields.len;

        std.mem.sort(SnapshotRecordField, gathered_fields, self.idents, comptime SnapshotRecordField.sortByNameAsc);

        _ = try self.buf.writer().write("{ ");

        switch (ext) {
            .flex => |flex| {
                if (flex.payload.name) |ident_idx| {
                    _ = try self.buf.writer().write("..");
                    _ = try self.buf.writer().write(self.idents.getText(ident_idx));
                    if (num_fields > 0) _ = try self.buf.writer().write(", ");
                } else if (true) {
                    // TODO: ^ here, we should consider polarity

                    _ = try self.buf.writer().write("..");
                    try self.writeFlexVarName(flex.payload.var_, flex.idx, .RecordExtension, root_idx);
                    if (num_fields > 0) _ = try self.buf.writer().write(", ");
                }

                // Since don't recurse above, we must capture the static dispatch
                // constraints directly
                for (self.snapshots.sliceStaticDispatchConstraints(flex.payload.constraints)) |constraint| {
                    try self.appendStaticDispatchConstraint(constraint, flex.idx);
                }
            },
            .rigid => |rigid| {
                _ = try self.buf.writer().write("..");
                _ = try self.buf.writer().write(self.idents.getText(rigid.name));
                if (num_fields > 0) _ = try self.buf.writer().write(", ");

                // Since don't recurse above, we must capture the static dispatch
                // constraints directly
                for (self.snapshots.sliceStaticDispatchConstraints(rigid.constraints)) |constraint| {
                    try self.appendStaticDispatchConstraint(constraint, record.ext);
                }
            },
            .unbound, .invalid => {},
        }

        for (gathered_fields, 0..) |field, i| {
            _ = try self.buf.writer().write(self.idents.getText(field.name));
            _ = try self.buf.writer().write(": ");
            try self.writeWithContext(field.content, .RecordFieldContent, root_idx);

            if (i != gathered_fields.len - 1) _ = try self.buf.writer().write(", ");
        }

        _ = try self.buf.writer().write(" }");
    }

    /// Recursively unwrap all record fields
    fn gatherRecordFields(self: *Self, fields: SnapshotRecordFieldSafeList.Range, ext_var: SnapshotContentIdx) std.mem.Allocator.Error!union(enum) {
        flex: struct { idx: SnapshotContentIdx, payload: SnapshotFlex },
        rigid: SnapshotRigid,
        unbound,
        invalid,
    } {
        const slice = self.snapshots.sliceRecordFields(fields);
        try self.scratch_record_fields.ensureUnusedCapacity(fields.len());
        for (slice.items(.name), slice.items(.content)) |name, content| {
            self.scratch_record_fields.appendAssumeCapacity(.{ .name = name, .content = content });
        }

        var ext = ext_var;
        while (true) {
            const resolved = self.snapshots.contents.get(ext);
            switch (resolved.*) {
                .flex => |flex| {
                    return .{ .flex = .{ .idx = ext, .payload = flex } };
                },
                .rigid => |rigid| {
                    return .{ .rigid = rigid };
                },
                .alias => |alias| {
                    ext = alias.backing;
                },
                .structure => |flat_type| {
                    switch (flat_type) {
                        .record => |ext_record| {
                            const ext_slice = self.snapshots.sliceRecordFields(ext_record.fields);
                            try self.scratch_record_fields.ensureUnusedCapacity(ext_record.fields.len());
                            for (ext_slice.items(.name), ext_slice.items(.content)) |name, content| {
                                self.scratch_record_fields.appendAssumeCapacity(.{ .name = name, .content = content });
                            }
                            ext = ext_record.ext;
                        },
                        .record_unbound => |ext_fields| {
                            const ext_slice = self.snapshots.sliceRecordFields(ext_fields);
                            try self.scratch_record_fields.ensureUnusedCapacity(ext_fields.len());
                            for (ext_slice.items(.name), ext_slice.items(.content)) |name, content| {
                                self.scratch_record_fields.appendAssumeCapacity(.{ .name = name, .content = content });
                            }
                            return .unbound;
                        },
                        else => return .invalid,
                    }
                },
                else => return .invalid,
            }
        }
    }

    /// Write record fields without extension
    fn writeRecordFields(self: *Self, fields: SnapshotRecordFieldSafeList.Range, root_idx: SnapshotContentIdx) Allocator.Error!void {
        if (fields.isEmpty()) {
            _ = try self.buf.writer().write("{}");
            return;
        }

        const fields_slice = self.snapshots.record_fields.sliceRange(fields);

        _ = try self.buf.writer().write("{ ");

        // Write first field - we already verified that there is at least one field.
        _ = try self.buf.writer().write(self.idents.getText(fields_slice.items(.name)[0]));
        _ = try self.buf.writer().write(": ");
        try self.writeWithContext(fields_slice.items(.content)[0], .RecordFieldContent, root_idx);

        // Write remaining fields
        for (fields_slice.items(.name)[1..], fields_slice.items(.content)[1..]) |name, content| {
            _ = try self.buf.writer().write(", ");
            _ = try self.buf.writer().write(self.idents.getText(name));
            _ = try self.buf.writer().write(": ");
            try self.writeWithContext(content, .RecordFieldContent, root_idx);
        }

        _ = try self.buf.writer().write(" }");
    }

    /// Write a tag union
    fn writeTagUnion(self: *Self, tag_union: SnapshotTagUnion, root_idx: SnapshotContentIdx) Allocator.Error!void {
        _ = try self.buf.writer().write("[");
        var iter = tag_union.tags.iterIndices();
        while (iter.next()) |tag_idx| {
            if (@intFromEnum(tag_idx) > @intFromEnum(tag_union.tags.start)) {
                _ = try self.buf.writer().write(", ");
            }

            const tag = self.snapshots.tags.get(tag_idx);
            try self.writeTag(tag, root_idx);
        }

        _ = try self.buf.writer().write("]");

        // Show extension variable if it's not empty
        switch (self.snapshots.contents.get(tag_union.ext).*) {
            .flex => |flex| {
                if (flex.name) |ident_idx| {
                    _ = try self.buf.writer().write(self.idents.getText(ident_idx));
                } else if (true) {
                    // TODO: ^ here, we should consider polarity

                    _ = try self.writeFlexVarName(flex.var_, tag_union.ext, .TagUnionExtension, root_idx);
                }

                for (self.snapshots.sliceStaticDispatchConstraints(flex.constraints)) |constraint| {
                    try self.appendStaticDispatchConstraint(constraint, tag_union.ext);
                }
            },
            .structure => |flat_type| switch (flat_type) {
                .empty_tag_union => {}, // Don't show empty extension
                else => {
                    try self.writeWithContext(tag_union.ext, .TagUnionExtension, root_idx);
                },
            },
            .rigid => |rigid| {
                _ = try self.buf.writer().write(self.idents.getText(rigid.name));

                for (self.snapshots.sliceStaticDispatchConstraints(rigid.constraints)) |constraint| {
                    try self.appendStaticDispatchConstraint(constraint, tag_union.ext);
                }
            },
            else => {
                try self.writeWithContext(tag_union.ext, .TagUnionExtension, root_idx);
            },
        }
    }

    /// Write a single tag
    pub fn writeTag(self: *Self, tag: SnapshotTag, root_idx: SnapshotContentIdx) Allocator.Error!void {
        _ = try self.buf.writer().write(self.idents.getText(tag.name));
        const args = self.snapshots.sliceVars(tag.args);
        if (args.len > 0) {
            _ = try self.buf.writer().write("(");
            for (args, 0..) |arg, i| {
                if (i > 0) _ = try self.buf.writer().write(", ");
                try self.writeWithContext(arg, .General, root_idx);
            }
            _ = try self.buf.writer().write(")");
        }
    }

    /// Append a constraint to the list, if it doesn't already exist
    /// Deduplicates based on method name and dispatcher
    fn appendStaticDispatchConstraint(self: *Self, constraint_base: SnapshotStaticDispatchConstraint, dispatcher: SnapshotContentIdx) std.mem.Allocator.Error!void {
        // Create the full constraint with the dispatcher
        const constraint_to_add = SnapshotStaticDispatchConstraint{
            .fn_name = constraint_base.fn_name,
            .fn_content = constraint_base.fn_content,
            .dispatcher = dispatcher,
        };

        for (self.static_dispatch_constraints.items) |constraint| {
            if (constraint.fn_name == constraint_to_add.fn_name and constraint.dispatcher == constraint_to_add.dispatcher) {
                return; // Duplicate constraint
            }
        }
        _ = try self.static_dispatch_constraints.append(constraint_to_add);
    }

    /// Dispatcher identity for deduplication - either a Var (for flex), an Ident.Idx (for rigid), or recursive.
    /// Flex vars and rigid names carry unique identities, but recursive markers don't—they're just
    /// placeholders indicating "reference back to a recursive type." Which type they refer to is
    /// determined by position in the tree, not by data in the marker itself. So for deduplication,
    /// all recursive markers are treated as equivalent.
    const DispatcherIdentity = union(enum) {
        flex_var: Var,
        rigid_name: Ident.Idx,
        recursive: void,
    };

    /// Get the dispatcher (first argument) identity from a function content
    fn getDispatcherIdentity(self: *Self, fn_content: SnapshotContentIdx) ?DispatcherIdentity {
        const content = self.snapshots.getContent(fn_content);
        if (content != .structure) return null;

        const fn_args = switch (content.structure) {
            .fn_effectful => |func| func.args,
            .fn_pure => |func| func.args,
            .fn_unbound => |func| func.args,
            else => return null,
        };

        if (fn_args.len() == 0) return null;

        const first_arg_idx = self.snapshots.sliceVars(fn_args)[0];
        const first_arg_content = self.snapshots.getContent(first_arg_idx);

        return switch (first_arg_content) {
            .flex => |flex| DispatcherIdentity{ .flex_var = flex.var_ },
            .rigid => |rigid| DispatcherIdentity{ .rigid_name = rigid.name },
            .recursive => DispatcherIdentity{ .recursive = {} },
            else => null,
        };
    }

    fn dispatcherIdentitiesEqual(a: ?DispatcherIdentity, b: ?DispatcherIdentity) bool {
        if (a == null or b == null) return false;
        return switch (a.?) {
            .flex_var => |a_var| switch (b.?) {
                .flex_var => |b_var| a_var == b_var,
                else => false,
            },
            .rigid_name => |a_name| switch (b.?) {
                .rigid_name => |b_name| a_name == b_name,
                else => false,
            },
            .recursive => switch (b.?) {
                .recursive => true, // see DispatcherIdentity doc comment
                else => false,
            },
        };
    }

    /// Generate a name for a flex var that may appear multiple times in the type
    fn writeFlexVarName(self: *Self, flex_var: Var, _: SnapshotContentIdx, context: TypeContext, root_idx: SnapshotContentIdx) std.mem.Allocator.Error!void {
        // Check if we've seen this flex var before.
        if (self.flex_var_names_map.get(flex_var)) |range| {
            // If so, then use that name
            _ = try self.buf.writer().write(
                self.flex_var_names.items[range.start..range.end],
            );
        } else {

            // Check if this variable appears multiple times
            const occurrences = try self.countOccurrences(flex_var, root_idx);

            // Treat vars that will appear in the where clause as multi-occurrence
            // to ensure they get general names without context-specific prefixes
            const treat_as_multi = occurrences > 1 or context == .General;

            if (!treat_as_multi) {
                // If it appears once, then generate the contextual name
                const buf_start = self.buf.items.len;
                _ = try self.buf.writer().write("_");
                try self.generateContextualName(context);
                const buf_end = self.buf.items.len;

                // Store the name in the map so it can be reused if this var is written again
                const flex_start = self.flex_var_names.items.len;
                try self.flex_var_names.appendSlice(self.buf.items[buf_start..buf_end]);
                const flex_end = self.flex_var_names.items.len;
                try self.flex_var_names_map.put(flex_var, .{ .start = flex_start, .end = flex_end });
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
                try self.flex_var_names_map.put(flex_var, .{ .start = flex_start, .end = flex_end });
            }
        }
    }

    fn countOccurrences(self: *Self, search_flex_var: Var, root_idx: SnapshotContentIdx) std.mem.Allocator.Error!usize {
        self.count_seen_idxs.clearRetainingCapacity();

        var count: usize = 0;
        try self.countContent(search_flex_var, root_idx, &count);
        return count;
    }

    fn countContent(self: *Self, search_flex_var: Var, current_idx: SnapshotContentIdx, count: *usize) std.mem.Allocator.Error!void {
        const content = self.snapshots.contents.get(current_idx);

        // First, check if this is the var we are counting
        switch (content.*) {
            .flex => |cur_flex| {
                if (search_flex_var == cur_flex.var_) {
                    count.* += 1;
                }
            },
            else => {},
        }

        // Then, before we recurse, check if we've already seen this var and
        // if so, return. (Avoids infintely counting a recursive type)
        for (self.count_seen_idxs.items) |seen| {
            if (seen == current_idx) return;
        }

        // Record that we've seen this var
        try self.count_seen_idxs.append(current_idx);
        defer _ = self.count_seen_idxs.pop();

        // Then recurse
        switch (content.*) {
            .flex => |cur_flex| {
                for (self.snapshots.sliceStaticDispatchConstraints(cur_flex.constraints)) |constraint| {
                    try self.countContent(search_flex_var, constraint.fn_content, count);
                }
            },
            .rigid => |cur_rigid| {
                for (self.snapshots.sliceStaticDispatchConstraints(cur_rigid.constraints)) |constraint| {
                    try self.countContent(search_flex_var, constraint.fn_content, count);
                }
            },
            .alias => |alias| {
                const args = self.snapshots.sliceVars(alias.vars);
                for (args) |arg_idx| {
                    try self.countContent(search_flex_var, arg_idx, count);
                }
            },
            .structure => |flat_type| {
                try self.countInFlatType(search_flex_var, flat_type, count);
            },
            .recursion_var => |rec_var| {
                // Count the structure the recursion var points to
                try self.countContent(search_flex_var, rec_var.structure, count);
            },
            .recursive, .err => {},
        }
    }

    fn countInFlatType(self: *Self, search_flex_var: Var, flat_type: SnapshotFlatType, count: *usize) std.mem.Allocator.Error!void {
        switch (flat_type) {
            .empty_record, .empty_tag_union => {},
            .box => |sub_idx| try self.countContent(search_flex_var, sub_idx, count),
            .tuple => |tuple| {
                const elems = self.snapshots.sliceVars(tuple.elems);
                for (elems) |elem| {
                    try self.countContent(search_flex_var, elem, count);
                }
            },
            .nominal_type => |nominal_type| {
                const args = self.snapshots.sliceVars(nominal_type.vars);
                // Skip the first var which is the nominal type's backing var
                for (args[1..]) |arg_idx| {
                    try self.countContent(search_flex_var, arg_idx, count);
                }
            },
            .fn_pure, .fn_effectful, .fn_unbound => |func| {
                const args = self.snapshots.sliceVars(func.args);
                for (args) |arg| {
                    try self.countContent(search_flex_var, arg, count);
                }
                try self.countContent(search_flex_var, func.ret, count);
            },
            .record => |record| {
                const fields = self.snapshots.record_fields.sliceRange(record.fields);
                for (fields.items(.content)) |field_content| {
                    try self.countContent(search_flex_var, field_content, count);
                }
                try self.countContent(search_flex_var, record.ext, count);
            },
            .record_unbound => |fields| {
                const fields_slice = self.snapshots.record_fields.sliceRange(fields);
                for (fields_slice.items(.content)) |field_content| {
                    try self.countContent(search_flex_var, field_content, count);
                }
            },
            .tag_union => |tag_union| {
                var iter = tag_union.tags.iterIndices();
                while (iter.next()) |tag_idx| {
                    const tag = self.snapshots.tags.get(tag_idx);
                    const args = self.snapshots.sliceVars(tag.args);
                    for (args) |arg_idx| {
                        try self.countContent(search_flex_var, arg_idx, count);
                    }
                }
                try self.countContent(search_flex_var, tag_union.ext, count);
            },
        }
    }

    pub fn sliceVars(self: *const Self, range: SnapshotContentIdxSafeList.Range) []const SnapshotContentIdx {
        return self.content_indexes.sliceRange(range);
    }

    pub fn sliceRecordFields(self: *const Self, range: SnapshotRecordFieldSafeList.Range) SnapshotRecordFieldSafeList.Slice {
        return self.record_fields.sliceRange(range);
    }

    pub fn sliceStaticDispatchConstraints(self: *const Self, range: SnapshotStaticDispatchConstraintSafeList.Range) SnapshotStaticDispatchConstraintSafeList.Slice {
        return self.static_dispatch_constraints.sliceRange(range);
    }

    pub fn sliceTags(self: *const Self, range: SnapshotTagSafeList.Range) SnapshotTagSafeList.Slice {
        return self.tags.sliceRange(range);
    }

    pub fn getContent(self: *const Self, idx: SnapshotContentIdx) SnapshotContent {
        return self.contents.get(idx).*;
    }
};

/// Writer for serializing type snapshots to a human-readable string format.
pub const SnapshotWriter = struct {
    const Self = @This();

    buf: std.array_list.Managed(u8),
    snapshots: *const Store,
    idents: *const Ident.Store,
    import_mapping: *const @import("types").import_mapping.ImportMapping,
    current_module_idx: ?Ident.Idx,
    can_ir: ?*const ModuleEnv,
    other_modules: ?[]const *const ModuleEnv,
    next_name_index: u32,
    name_counters: std.EnumMap(TypeContext, u32),
    flex_var_names_map: std.AutoHashMap(Var, FlexVarNameRange),
    flex_var_names: std.array_list.Managed(u8),
    static_dispatch_constraints: std.array_list.Managed(SnapshotStaticDispatchConstraint),
    scratch_record_fields: std.array_list.Managed(SnapshotRecordField),
    count_seen_idxs: std.array_list.Managed(SnapshotContentIdx),

    const FlexVarNameRange = struct { start: usize, end: usize };

    pub fn init(gpa: std.mem.Allocator, snapshots: *const Store, idents: *const Ident.Store, import_mapping: *const @import("types").import_mapping.ImportMapping) Self {
        return .{
            .buf = std.array_list.Managed(u8).init(gpa),
            .snapshots = snapshots,
            .idents = idents,
            .import_mapping = import_mapping,
            .current_module_idx = null,
            .can_ir = null,
            .other_modules = null,
            .next_name_index = 0,
            .name_counters = std.EnumMap(TypeContext, u32).init(.{}),
            .flex_var_names_map = std.AutoHashMap(Var, FlexVarNameRange).init(gpa),
            .flex_var_names = std.array_list.Managed(u8).init(gpa),
            .static_dispatch_constraints = std.array_list.Managed(SnapshotStaticDispatchConstraint).init(gpa),
            .scratch_record_fields = std.array_list.Managed(SnapshotRecordField).init(gpa),
            .count_seen_idxs = std.array_list.Managed(SnapshotContentIdx).init(gpa),
        };
    }

    pub fn initWithContext(
        gpa: std.mem.Allocator,
        snapshots: *const Store,
        idents: *const Ident.Store,
        current_module_idx: Ident.Idx,
        can_ir: *const ModuleEnv,
        other_modules: []const *const ModuleEnv,
    ) Self {
        return .{
            .buf = std.array_list.Managed(u8).init(gpa),
            .snapshots = snapshots,
            .idents = idents,
            .current_module_idx = current_module_idx,
            .can_ir = can_ir,
            .other_modules = other_modules,
            .next_name_index = 0,
            .name_counters = std.EnumMap(TypeContext, u32).init(.{}),
            .flex_var_names_map = std.AutoHashMap(Var, FlexVarNameRange).init(gpa),
            .flex_var_names = std.array_list.Managed(u8).init(gpa),
            .static_dispatch_constraints = std.array_list.Managed(SnapshotStaticDispatchConstraint).init(gpa),
            .scratch_record_fields = std.array_list.Managed(SnapshotRecordField).init(gpa),
            .count_seen_idxs = try std.array_list.Managed(SnapshotContentIdx).init(gpa),
        };
    }

    pub fn deinit(self: *Self) void {
        self.buf.deinit();
        self.flex_var_names_map.deinit();
        self.flex_var_names.deinit();
        self.static_dispatch_constraints.deinit();
        self.scratch_record_fields.deinit();
        self.count_seen_idxs.deinit();
    }

    pub fn resetContext(self: *Self) void {
        self.next_name_index = 0;
        self.name_counters = std.EnumMap(TypeContext, u32).init(.{});
        self.buf.clearRetainingCapacity();
        self.flex_var_names_map.clearRetainingCapacity();
        self.flex_var_names.clearRetainingCapacity();
        self.static_dispatch_constraints.clearRetainingCapacity();
        self.scratch_record_fields.clearRetainingCapacity();
        self.count_seen_idxs.clearRetainingCapacity();
    }

    pub fn get(self: *const Self) []const u8 {
        return self.buf.items;
    }

    const GenerateNameMode = union(enum) {
        print,
        persist_flex_var: Var,
    };

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

            // Check all identifiers in the store
            const exists = self.idents.interner.contains(candidate_name);

            if (!exists) {
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

    fn generateContextualName(self: *Self, context: TypeContext) !void {
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
        const max_attempts = self.idents.interner.entry_count + 1;
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
                // This name is available, write it to the writer
                for (candidate_name) |c| {
                    try self.buf.writer().writeByte(c);
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

    /// Convert a content to a type string
    pub fn write(self: *Self, idx: SnapshotContentIdx) Allocator.Error!void {
        try self.writeWithContext(idx, .General, idx);

        if (self.static_dispatch_constraints.items.len > 0) {
            _ = try self.buf.writer().write(" where [");
            for (self.static_dispatch_constraints.items, 0..) |constraint, i| {
                if (i > 0) {
                    _ = try self.buf.writer().write(", ");
                }

                try self.writeWithContext(constraint.dispatcher, .General, idx);
                _ = try self.buf.writer().write(".");
                _ = try self.buf.writer().write(self.idents.getText(constraint.fn_name));
                _ = try self.buf.writer().write(" : ");
                try self.writeWithContext(constraint.fn_content, .General, idx);
            }
            _ = try self.buf.writer().write("]");
        }
    }

    /// Count how many times a content appears in a type
    /// Convert a content to a type string with context
    fn writeWithContext(self: *Self, idx: SnapshotContentIdx, context: TypeContext, root_idx: SnapshotContentIdx) Allocator.Error!void {
        const content = self.snapshots.contents.get(idx);
        return self.writeContent(content.*, context, idx, root_idx);
    }

    /// Convert a content to a type string
    fn writeContent(self: *Self, content: SnapshotContent, context: TypeContext, content_idx: SnapshotContentIdx, root_idx: SnapshotContentIdx) Allocator.Error!void {
        switch (content) {
            .flex => |flex| {
                if (flex.name) |ident_idx| {
                    _ = try self.buf.writer().write(self.idents.getText(ident_idx));
                } else {
                    // If this flex var has constraints, use .General context so it gets a general name
                    // that will match when the constraint is written in the where clause
                    const var_context = if (flex.constraints.len() > 0) .General else context;
                    _ = try self.writeFlexVarName(flex.var_, content_idx, var_context, root_idx);
                }

                for (self.snapshots.sliceStaticDispatchConstraints(flex.constraints)) |constraint| {
                    try self.appendStaticDispatchConstraint(constraint, content_idx);
                }
            },
            .rigid => |rigid| {
                _ = try self.buf.writer().write(self.idents.getText(rigid.name));

                for (self.snapshots.sliceStaticDispatchConstraints(rigid.constraints)) |constraint| {
                    try self.appendStaticDispatchConstraint(constraint, content_idx);
                }

                // Useful in debugging to see if a var is rigid or not
                // _ = try self.buf.writer().write("[r]");
            },
            .alias => |alias| {
                try self.writeAlias(alias, root_idx);
            },
            .structure => |flat_type| {
                const should_wrap_in_parens = ((context == .FunctionArgument or context == .FunctionReturn) and (flat_type == .fn_effectful or flat_type == .fn_pure or flat_type == .fn_unbound));
                if (should_wrap_in_parens) {
                    _ = try self.buf.writer().write("(");
                }

                try self.writeFlatType(flat_type, root_idx);

                if (should_wrap_in_parens) {
                    _ = try self.buf.writer().write(")");
                }
            },
            .recursion_var => |rec_var| {
                // Write the recursion var by writing the structure it points to
                const structure_content = self.snapshots.getContent(rec_var.structure);
                try self.writeContent(structure_content, context, rec_var.structure, root_idx);
            },
            .recursive => |maybe_name| {
                if (maybe_name) |name_idx| {
                    _ = try self.buf.writer().write(self.idents.getText(name_idx));
                } else {
                    // Fallback if no name was captured - generate a contextual name
                    _ = try self.buf.writer().write("_");
                    try self.generateContextualName(context);
                }
            },
            .err => {
                _ = try self.buf.writer().write("Error");
            },
        }
    }

    /// Write an alias type
    fn writeAlias(self: *Self, alias: SnapshotAlias, root_idx: SnapshotContentIdx) Allocator.Error!void {
        const display_idx = if (self.import_mapping.get(alias.ident.ident_idx)) |mapped_idx| mapped_idx else alias.ident.ident_idx;
        _ = try self.buf.writer().write(self.idents.getText(display_idx));

        const vars = self.snapshots.sliceVars(alias.vars);
        if (vars.len > 0) {
            _ = try self.buf.writer().write("(");
            for (vars, 0..) |arg, i| {
                if (i > 0) _ = try self.buf.writer().write(", ");
                try self.writeWithContext(arg, .General, root_idx);
            }
            _ = try self.buf.writer().write(")");
        }
    }

    /// Convert a flat type to a type string
    fn writeFlatType(self: *Self, flat_type: SnapshotFlatType, root_idx: SnapshotContentIdx) Allocator.Error!void {
        switch (flat_type) {
            .box => |sub_var| {
                _ = try self.buf.writer().write("Box(");
                try self.writeWithContext(sub_var, .General, root_idx);
                _ = try self.buf.writer().write(")");
            },
            .tuple => |tuple| {
                try self.writeTuple(tuple, root_idx);
            },
            .nominal_type => |nominal_type| {
                try self.writeNominalType(nominal_type, root_idx);
            },
            .fn_pure => |func| {
                try self.writeFuncWithArrow(func, " -> ", root_idx);
            },
            .fn_effectful => |func| {
                try self.writeFuncWithArrow(func, " => ", root_idx);
            },
            .fn_unbound => |func| {
                try self.writeFuncWithArrow(func, " -> ", root_idx);
            },
            .record => |record| {
                try self.writeRecord(record, root_idx);
            },
            .record_unbound => |fields| {
                try self.writeRecordFields(fields, root_idx);
            },
            .empty_record => {
                _ = try self.buf.writer().write("{}");
            },
            .tag_union => |tag_union| {
                try self.writeTagUnion(tag_union, root_idx);
            },
            .empty_tag_union => {
                _ = try self.buf.writer().write("[]");
            },
        }
    }

    /// Write a tuple type
    fn writeTuple(self: *Self, tuple: SnapshotTuple, root_idx: SnapshotContentIdx) Allocator.Error!void {
        const elems = self.snapshots.sliceVars(tuple.elems);
        _ = try self.buf.writer().write("(");
        for (elems, 0..) |elem, i| {
            if (i > 0) _ = try self.buf.writer().write(", ");
            try self.writeWithContext(elem, .TupleFieldContent, root_idx);
        }
        _ = try self.buf.writer().write(")");
    }

    /// Write a nominal type
    pub fn writeNominalType(self: *Self, nominal_type: SnapshotNominalType, root_idx: SnapshotContentIdx) Allocator.Error!void {
        const display_idx = if (self.import_mapping.get(nominal_type.ident.ident_idx)) |mapped_idx| mapped_idx else nominal_type.ident.ident_idx;
        _ = try self.buf.writer().write(self.idents.getText(display_idx));

        // The 1st var is the nominal type's backing var, so we skip it
        var vars = self.snapshots.sliceVars(nominal_type.vars);
        std.debug.assert(vars.len > 0);
        vars = vars[1..];

        if (vars.len > 0) {
            _ = try self.buf.writer().write("(");
            for (vars, 0..) |arg, i| {
                if (i > 0) _ = try self.buf.writer().write(", ");
                try self.writeWithContext(arg, .General, root_idx);
            }
            _ = try self.buf.writer().write(")");
        }

        // Add origin information if it's from a different module
        if (self.current_module_idx) |current_idx| {
            if (nominal_type.origin_module != current_idx) {
                const origin_module_name = self.idents.getText(nominal_type.origin_module);
                _ = try self.buf.writer().write(" (from ");
                _ = try self.buf.writer().write(origin_module_name);
                _ = try self.buf.writer().write(")");
            }
        }
    }

    /// Write a function type with a specific arrow
    fn writeFuncWithArrow(self: *Self, func: SnapshotFunc, arrow: []const u8, root_idx: SnapshotContentIdx) Allocator.Error!void {
        const args = self.snapshots.sliceVars(func.args);

        // Write arguments
        if (args.len == 0) {
            _ = try self.buf.writer().write("({})");
        } else if (args.len == 1) {
            try self.writeWithContext(args[0], .FunctionArgument, root_idx);
        } else {
            for (args, 0..) |arg, i| {
                if (i > 0) _ = try self.buf.writer().write(", ");
                try self.writeWithContext(arg, .FunctionArgument, root_idx);
            }
        }

        _ = try self.buf.writer().write(arrow);

        try self.writeWithContext(func.ret, .FunctionReturn, root_idx);
    }

    /// Write a record type
    fn writeRecord(self: *Self, record: SnapshotRecord, root_idx: SnapshotContentIdx) Allocator.Error!void {
        const scratch_fields_top = self.scratch_record_fields.items.len;
        defer self.scratch_record_fields.shrinkRetainingCapacity(scratch_fields_top);

        const ext = try self.gatherRecordFields(record.fields, record.ext);
        const gathered_fields = self.scratch_record_fields.items[scratch_fields_top..];
        const num_fields = gathered_fields.len;

        std.mem.sort(SnapshotRecordField, gathered_fields, self.idents, comptime SnapshotRecordField.sortByNameAsc);

        _ = try self.buf.writer().write("{ ");

        switch (ext) {
            .flex => |flex| {
                if (flex.payload.name) |ident_idx| {
                    _ = try self.buf.writer().write("..");
                    _ = try self.buf.writer().write(self.idents.getText(ident_idx));
                    if (num_fields > 0) _ = try self.buf.writer().write(", ");
                } else if (true) {
                    // TODO: ^ here, we should consider polarity

                    _ = try self.buf.writer().write("..");
                    try self.writeFlexVarName(flex.payload.var_, flex.idx, .RecordExtension, root_idx);
                    if (num_fields > 0) _ = try self.buf.writer().write(", ");
                }

                // Since don't recurse above, we must capture the static dispatch
                // constraints directly
                for (self.snapshots.sliceStaticDispatchConstraints(flex.payload.constraints)) |constraint| {
                    try self.appendStaticDispatchConstraint(constraint, flex.idx);
                }
            },
            .rigid => |rigid| {
                _ = try self.buf.writer().write("..");
                _ = try self.buf.writer().write(self.idents.getText(rigid.name));
                if (num_fields > 0) _ = try self.buf.writer().write(", ");

                // Since don't recurse above, we must capture the static dispatch
                // constraints directly
                for (self.snapshots.sliceStaticDispatchConstraints(rigid.constraints)) |constraint| {
                    try self.appendStaticDispatchConstraint(constraint, record.ext);
                }
            },
            .unbound, .invalid => {},
        }

        for (gathered_fields, 0..) |field, i| {
            _ = try self.buf.writer().write(self.idents.getText(field.name));
            _ = try self.buf.writer().write(": ");
            try self.writeWithContext(field.content, .RecordFieldContent, root_idx);

            if (i != gathered_fields.len - 1) _ = try self.buf.writer().write(", ");
        }

        _ = try self.buf.writer().write(" }");
    }

    /// Recursively unwrap all record fields
    fn gatherRecordFields(self: *Self, fields: SnapshotRecordFieldSafeList.Range, ext_var: SnapshotContentIdx) std.mem.Allocator.Error!union(enum) {
        flex: struct { idx: SnapshotContentIdx, payload: SnapshotFlex },
        rigid: SnapshotRigid,
        unbound,
        invalid,
    } {
        const slice = self.snapshots.sliceRecordFields(fields);
        try self.scratch_record_fields.ensureUnusedCapacity(fields.len());
        for (slice.items(.name), slice.items(.content)) |name, content| {
            self.scratch_record_fields.appendAssumeCapacity(.{ .name = name, .content = content });
        }

        var ext = ext_var;
        while (true) {
            const resolved = self.snapshots.contents.get(ext);
            switch (resolved.*) {
                .flex => |flex| {
                    return .{ .flex = .{ .idx = ext, .payload = flex } };
                },
                .rigid => |rigid| {
                    return .{ .rigid = rigid };
                },
                .alias => |alias| {
                    ext = alias.backing;
                },
                .structure => |flat_type| {
                    switch (flat_type) {
                        .record => |ext_record| {
                            const ext_slice = self.snapshots.sliceRecordFields(ext_record.fields);
                            try self.scratch_record_fields.ensureUnusedCapacity(ext_record.fields.len());
                            for (ext_slice.items(.name), ext_slice.items(.content)) |name, content| {
                                self.scratch_record_fields.appendAssumeCapacity(.{ .name = name, .content = content });
                            }
                            ext = ext_record.ext;
                        },
                        .record_unbound => |ext_fields| {
                            const ext_slice = self.snapshots.sliceRecordFields(ext_fields);
                            try self.scratch_record_fields.ensureUnusedCapacity(ext_fields.len());
                            for (ext_slice.items(.name), ext_slice.items(.content)) |name, content| {
                                self.scratch_record_fields.appendAssumeCapacity(.{ .name = name, .content = content });
                            }
                            return .unbound;
                        },
                        else => return .invalid,
                    }
                },
                else => return .invalid,
            }
        }
    }

    /// Write record fields without extension
    fn writeRecordFields(self: *Self, fields: SnapshotRecordFieldSafeList.Range, root_idx: SnapshotContentIdx) Allocator.Error!void {
        if (fields.isEmpty()) {
            _ = try self.buf.writer().write("{}");
            return;
        }

        const fields_slice = self.snapshots.record_fields.sliceRange(fields);

        _ = try self.buf.writer().write("{ ");

        // Write first field - we already verified that there is at least one field.
        _ = try self.buf.writer().write(self.idents.getText(fields_slice.items(.name)[0]));
        _ = try self.buf.writer().write(": ");
        try self.writeWithContext(fields_slice.items(.content)[0], .RecordFieldContent, root_idx);

        // Write remaining fields
        for (fields_slice.items(.name)[1..], fields_slice.items(.content)[1..]) |name, content| {
            _ = try self.buf.writer().write(", ");
            _ = try self.buf.writer().write(self.idents.getText(name));
            _ = try self.buf.writer().write(": ");
            try self.writeWithContext(content, .RecordFieldContent, root_idx);
        }

        _ = try self.buf.writer().write(" }");
    }

    /// Write a tag union
    fn writeTagUnion(self: *Self, tag_union: SnapshotTagUnion, root_idx: SnapshotContentIdx) Allocator.Error!void {
        _ = try self.buf.writer().write("[");
        var iter = tag_union.tags.iterIndices();
        while (iter.next()) |tag_idx| {
            if (@intFromEnum(tag_idx) > @intFromEnum(tag_union.tags.start)) {
                _ = try self.buf.writer().write(", ");
            }

            const tag = self.snapshots.tags.get(tag_idx);
            try self.writeTag(tag, root_idx);
        }

        _ = try self.buf.writer().write("]");

        // Show extension variable if it's not empty
        switch (self.snapshots.contents.get(tag_union.ext).*) {
            .flex => |flex| {
                if (flex.name) |ident_idx| {
                    _ = try self.buf.writer().write(self.idents.getText(ident_idx));
                } else if (true) {
                    // TODO: ^ here, we should consider polarity

                    _ = try self.writeFlexVarName(flex.var_, tag_union.ext, .TagUnionExtension, root_idx);
                }

                for (self.snapshots.sliceStaticDispatchConstraints(flex.constraints)) |constraint| {
                    try self.appendStaticDispatchConstraint(constraint, tag_union.ext);
                }
            },
            .structure => |flat_type| switch (flat_type) {
                .empty_tag_union => {}, // Don't show empty extension
                else => {
                    try self.writeWithContext(tag_union.ext, .TagUnionExtension, root_idx);
                },
            },
            .rigid => |rigid| {
                _ = try self.buf.writer().write(self.idents.getText(rigid.name));

                for (self.snapshots.sliceStaticDispatchConstraints(rigid.constraints)) |constraint| {
                    try self.appendStaticDispatchConstraint(constraint, tag_union.ext);
                }
            },
            else => {
                try self.writeWithContext(tag_union.ext, .TagUnionExtension, root_idx);
            },
        }
    }

    /// Write a single tag
    pub fn writeTag(self: *Self, tag: SnapshotTag, root_idx: SnapshotContentIdx) Allocator.Error!void {
        _ = try self.buf.writer().write(self.idents.getText(tag.name));
        const args = self.snapshots.sliceVars(tag.args);
        if (args.len > 0) {
            _ = try self.buf.writer().write("(");
            for (args, 0..) |arg, i| {
                if (i > 0) _ = try self.buf.writer().write(", ");
                try self.writeWithContext(arg, .General, root_idx);
            }
            _ = try self.buf.writer().write(")");
        }
    }

    /// Append a constraint to the list, if it doesn't already exist
    /// Deduplicates based on method name and dispatcher
    fn appendStaticDispatchConstraint(self: *Self, constraint_base: SnapshotStaticDispatchConstraint, dispatcher: SnapshotContentIdx) std.mem.Allocator.Error!void {
        // Create the full constraint with the dispatcher
        const constraint_to_add = SnapshotStaticDispatchConstraint{
            .fn_name = constraint_base.fn_name,
            .fn_content = constraint_base.fn_content,
            .dispatcher = dispatcher,
        };

        for (self.static_dispatch_constraints.items) |constraint| {
            if (constraint.fn_name == constraint_to_add.fn_name and constraint.dispatcher == constraint_to_add.dispatcher) {
                return; // Duplicate constraint
            }
        }
        _ = try self.static_dispatch_constraints.append(constraint_to_add);
    }

    /// Dispatcher identity for deduplication - either a Var (for flex), an Ident.Idx (for rigid), or recursive.
    /// Flex vars and rigid names carry unique identities, but recursive markers don't—they're just
    /// placeholders indicating "reference back to a recursive type." Which type they refer to is
    /// determined by position in the tree, not by data in the marker itself. So for deduplication,
    /// all recursive markers are treated as equivalent.
    const DispatcherIdentity = union(enum) {
        flex_var: Var,
        rigid_name: Ident.Idx,
        recursive: void,
    };

    /// Get the dispatcher (first argument) identity from a function content
    fn getDispatcherIdentity(self: *Self, fn_content: SnapshotContentIdx) ?DispatcherIdentity {
        const content = self.snapshots.getContent(fn_content);
        if (content != .structure) return null;

        const fn_args = switch (content.structure) {
            .fn_effectful => |func| func.args,
            .fn_pure => |func| func.args,
            .fn_unbound => |func| func.args,
            else => return null,
        };

        if (fn_args.len() == 0) return null;

        const first_arg_idx = self.snapshots.sliceVars(fn_args)[0];
        const first_arg_content = self.snapshots.getContent(first_arg_idx);

        return switch (first_arg_content) {
            .flex => |flex| DispatcherIdentity{ .flex_var = flex.var_ },
            .rigid => |rigid| DispatcherIdentity{ .rigid_name = rigid.name },
            .recursive => DispatcherIdentity{ .recursive = {} },
            else => null,
        };
    }

    fn dispatcherIdentitiesEqual(a: ?DispatcherIdentity, b: ?DispatcherIdentity) bool {
        if (a == null or b == null) return false;
        return switch (a.?) {
            .flex_var => |a_var| switch (b.?) {
                .flex_var => |b_var| a_var == b_var,
                else => false,
            },
            .rigid_name => |a_name| switch (b.?) {
                .rigid_name => |b_name| a_name == b_name,
                else => false,
            },
            .recursive => switch (b.?) {
                .recursive => true, // see DispatcherIdentity doc comment
                else => false,
            },
        };
    }

    /// Generate a name for a flex var that may appear multiple times in the type
    fn writeFlexVarName(self: *Self, flex_var: Var, _: SnapshotContentIdx, context: TypeContext, root_idx: SnapshotContentIdx) std.mem.Allocator.Error!void {
        // Check if we've seen this flex var before.
        if (self.flex_var_names_map.get(flex_var)) |range| {
            // If so, then use that name
            _ = try self.buf.writer().write(
                self.flex_var_names.items[range.start..range.end],
            );
        } else {

            // Check if this variable appears multiple times
            const occurrences = try self.countOccurrences(flex_var, root_idx);

            // Treat vars that will appear in the where clause as multi-occurrence
            // to ensure they get general names without context-specific prefixes
            const treat_as_multi = occurrences > 1 or context == .General;

            if (!treat_as_multi) {
                // If it appears once, then generate the contextual name
                const buf_start = self.buf.items.len;
                _ = try self.buf.writer().write("_");
                try self.generateContextualName(context);
                const buf_end = self.buf.items.len;

                // Store the name in the map so it can be reused if this var is written again
                const flex_start = self.flex_var_names.items.len;
                try self.flex_var_names.appendSlice(self.buf.items[buf_start..buf_end]);
                const flex_end = self.flex_var_names.items.len;
                try self.flex_var_names_map.put(flex_var, .{ .start = flex_start, .end = flex_end });
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
                try self.flex_var_names_map.put(flex_var, .{ .start = flex_start, .end = flex_end });
            }
        }
    }

    fn countOccurrences(self: *Self, search_flex_var: Var, root_idx: SnapshotContentIdx) std.mem.Allocator.Error!usize {
        self.count_seen_idxs.clearRetainingCapacity();

        var count: usize = 0;
        try self.countContent(search_flex_var, root_idx, &count);
        return count;
    }

    fn countContent(self: *Self, search_flex_var: Var, current_idx: SnapshotContentIdx, count: *usize) std.mem.Allocator.Error!void {
        const content = self.snapshots.contents.get(current_idx);

        // First, check if this is the var we are counting
        switch (content.*) {
            .flex => |cur_flex| {
                if (search_flex_var == cur_flex.var_) {
                    count.* += 1;
                }
            },
            else => {},
        }

        // Then, before we recurse, check if we've already seen this var and
        // if so, return. (Avoids infintely counting a recursive type)
        for (self.count_seen_idxs.items) |seen| {
            if (seen == current_idx) return;
        }

        // Record that we've seen this var
        try self.count_seen_idxs.append(current_idx);
        defer _ = self.count_seen_idxs.pop();

        // Then recurse
        switch (content.*) {
            .flex => |cur_flex| {
                for (self.snapshots.sliceStaticDispatchConstraints(cur_flex.constraints)) |constraint| {
                    try self.countContent(search_flex_var, constraint.fn_content, count);
                }
            },
            .rigid => |cur_rigid| {
                for (self.snapshots.sliceStaticDispatchConstraints(cur_rigid.constraints)) |constraint| {
                    try self.countContent(search_flex_var, constraint.fn_content, count);
                }
            },
            .alias => |alias| {
                const args = self.snapshots.sliceVars(alias.vars);
                for (args) |arg_idx| {
                    try self.countContent(search_flex_var, arg_idx, count);
                }
            },
            .structure => |flat_type| {
                try self.countInFlatType(search_flex_var, flat_type, count);
            },
            .recursion_var => |rec_var| {
                // Count the structure the recursion var points to
                try self.countContent(search_flex_var, rec_var.structure, count);
            },
            .recursive, .err => {},
        }
    }

    fn countInFlatType(self: *Self, search_flex_var: Var, flat_type: SnapshotFlatType, count: *usize) std.mem.Allocator.Error!void {
        switch (flat_type) {
            .empty_record, .empty_tag_union => {},
            .box => |sub_idx| try self.countContent(search_flex_var, sub_idx, count),
            .tuple => |tuple| {
                const elems = self.snapshots.sliceVars(tuple.elems);
                for (elems) |elem| {
                    try self.countContent(search_flex_var, elem, count);
                }
            },
            .nominal_type => |nominal_type| {
                const args = self.snapshots.sliceVars(nominal_type.vars);
                // Skip the first var which is the nominal type's backing var
                for (args[1..]) |arg_idx| {
                    try self.countContent(search_flex_var, arg_idx, count);
                }
            },
            .fn_pure, .fn_effectful, .fn_unbound => |func| {
                const args = self.snapshots.sliceVars(func.args);
                for (args) |arg| {
                    try self.countContent(search_flex_var, arg, count);
                }
                try self.countContent(search_flex_var, func.ret, count);
            },
            .record => |record| {
                const fields = self.snapshots.record_fields.sliceRange(record.fields);
                for (fields.items(.content)) |field_content| {
                    try self.countContent(search_flex_var, field_content, count);
                }
                try self.countContent(search_flex_var, record.ext, count);
            },
            .record_unbound => |fields| {
                const fields_slice = self.snapshots.record_fields.sliceRange(fields);
                for (fields_slice.items(.content)) |field_content| {
                    try self.countContent(search_flex_var, field_content, count);
                }
            },
            .tag_union => |tag_union| {
                var iter = tag_union.tags.iterIndices();
                while (iter.next()) |tag_idx| {
                    const tag = self.snapshots.tags.get(tag_idx);
                    const args = self.snapshots.sliceVars(tag.args);
                    for (args) |arg_idx| {
                        try self.countContent(search_flex_var, arg_idx, count);
                    }
                }
                try self.countContent(search_flex_var, tag_union.ext, count);
            },
        }
    }
};
