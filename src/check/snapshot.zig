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
                .recursion_var => |rec_var| rec_var.name,
                .alias => |alias| alias.ident.ident_idx,
                .structure => |flat_type| switch (flat_type) {
                    .nominal_type => |nominal| nominal.ident.ident_idx,
                    // Other structures can appear as backing vars for nominal types.
                    // E.g., List(a) := [Nil, Cons(a, List(a))] has a tag union as backing.
                    // These don't have a direct name, so we fall back to contextual naming.
                    .record, .record_unbound, .tuple, .fn_pure, .fn_effectful, .fn_unbound, .empty_record, .tag_union, .empty_tag_union => null,
                },
                // Error types shouldn't create cycles
                .err => unreachable,
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

};
