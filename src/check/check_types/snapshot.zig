//! Capture the state of an type at a point in time for the purpose of error reporting.

const std = @import("std");
const base = @import("base");
const collections = @import("collections");
const types = @import("types");
const ModuleEnv = @import("compile").ModuleEnv;

const TypesStore = types.Store;
const Allocator = std.mem.Allocator;
const Ident = base.Ident;

/// Index enum for SnapshotContentList
pub const SnapshotContentIdx = SnapshotContentList.Idx;

const SnapshotContentList = collections.SafeList(SnapshotContent);
const SnapshotContentIdxSafeList = collections.SafeList(SnapshotContentIdx);
const SnapshotRecordFieldSafeList = collections.SafeMultiList(SnapshotRecordField);
const SnapshotTagSafeList = collections.SafeMultiList(SnapshotTag);

const MkSafeMultiList = collections.SafeMultiList;

const Var = types.Var;
const Content = types.Content;

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

    /// Storage for compound type parts
    content_indexes: SnapshotContentIdxSafeList,
    record_fields: SnapshotRecordFieldSafeList,
    tags: SnapshotTagSafeList,

    // Scratch
    scratch_content: base.Scratch(SnapshotContentIdx),
    scratch_tags: base.Scratch(SnapshotTag),
    scratch_record_fields: base.Scratch(SnapshotRecordField),

    pub fn initCapacity(gpa: Allocator, capacity: usize) std.mem.Allocator.Error!Self {
        return .{
            .gpa = gpa,
            .contents = try SnapshotContentList.initCapacity(gpa, capacity),
            .content_indexes = try SnapshotContentIdxSafeList.initCapacity(gpa, capacity),
            .record_fields = try SnapshotRecordFieldSafeList.initCapacity(gpa, 256),
            .tags = try SnapshotTagSafeList.initCapacity(gpa, 256),
            .scratch_content = try base.Scratch(SnapshotContentIdx).init(gpa),
            .scratch_tags = try base.Scratch(SnapshotTag).init(gpa),
            .scratch_record_fields = try base.Scratch(SnapshotRecordField).init(gpa),
        };
    }

    pub fn deinit(self: *Self) void {
        self.contents.deinit(self.gpa);
        self.content_indexes.deinit(self.gpa);
        self.record_fields.deinit(self.gpa);
        self.tags.deinit(self.gpa);
        self.scratch_content.deinit(self.gpa);
        self.scratch_tags.deinit(self.gpa);
        self.scratch_record_fields.deinit(self.gpa);
    }

    /// Create a deep snapshot from a Var, storing it in this SnapshotStore
    /// Deep copy a type variable's content into self-contained snapshot storage
    pub fn deepCopyVar(self: *Self, store: *const TypesStore, var_: types.Var) std.mem.Allocator.Error!SnapshotContentIdx {
        const resolved = store.resolveVar(var_);
        return try self.deepCopyContent(store, resolved.desc.content);
    }

    fn deepCopyContent(self: *Self, store: *const TypesStore, content: Content) std.mem.Allocator.Error!SnapshotContentIdx {
        const deep_content = switch (content) {
            .flex_var => |ident| SnapshotContent{ .flex_var = ident },
            .rigid_var => |ident| SnapshotContent{ .rigid_var = ident },
            .alias => |alias| SnapshotContent{ .alias = try self.deepCopyAlias(store, alias) },
            .structure => |flat_type| SnapshotContent{ .structure = try self.deepCopyFlatType(store, flat_type) },
            .err => SnapshotContent.err,
        };

        return try self.contents.append(self.gpa, deep_content);
    }

    fn deepCopyFlatType(self: *Self, store: *const TypesStore, flat_type: types.FlatType) std.mem.Allocator.Error!SnapshotFlatType {
        return switch (flat_type) {
            .str => SnapshotFlatType.str,
            .box => |box_var| {
                const resolved = store.resolveVar(box_var);
                const deep_content = try self.deepCopyContent(store, resolved.desc.content);
                return SnapshotFlatType{ .box = deep_content };
            },
            .list => |list_var| {
                const resolved = store.resolveVar(list_var);
                const deep_content = try self.deepCopyContent(store, resolved.desc.content);
                return SnapshotFlatType{ .list = deep_content };
            },
            .list_unbound => {
                return SnapshotFlatType.list_unbound;
            },
            .tuple => |tuple| SnapshotFlatType{ .tuple = try self.deepCopyTuple(store, tuple) },
            .num => |num| SnapshotFlatType{ .num = try self.deepCopyNum(store, num) },
            .nominal_type => |nominal_type| SnapshotFlatType{ .nominal_type = try self.deepCopyNominalType(store, nominal_type) },
            .fn_pure => |func| SnapshotFlatType{ .fn_pure = try self.deepCopyFunc(store, func) },
            .fn_effectful => |func| SnapshotFlatType{ .fn_effectful = try self.deepCopyFunc(store, func) },
            .fn_unbound => |func| SnapshotFlatType{ .fn_unbound = try self.deepCopyFunc(store, func) },
            .record => |record| SnapshotFlatType{ .record = try self.deepCopyRecord(store, record) },
            .record_unbound => |fields| SnapshotFlatType{ .record_unbound = try self.deepCopyRecordFields(store, fields) },
            .record_poly => |poly| SnapshotFlatType{ .record_poly = .{
                .record = try self.deepCopyRecord(store, poly.record),
                .var_ = try self.deepCopyContent(store, store.resolveVar(poly.var_).desc.content),
            } },
            .empty_record => SnapshotFlatType.empty_record,
            .tag_union => |tag_union| SnapshotFlatType{ .tag_union = try self.deepCopyTagUnion(store, tag_union) },
            .empty_tag_union => SnapshotFlatType.empty_tag_union,
        };
    }

    fn deepCopyAlias(self: *Self, store: *const TypesStore, alias: types.Alias) std.mem.Allocator.Error!SnapshotAlias {
        // Mark starting position in the scratch array
        const scratch_top = self.scratch_content.top();

        const backing_var = store.getAliasBackingVar(alias);
        const backing_resolved = store.resolveVar(backing_var);
        const deep_backing = try self.deepCopyContent(store, backing_resolved.desc.content);
        _ = try self.scratch_content.append(self.gpa, deep_backing);

        // Iterate and append to scratch array
        var arg_iter = store.iterAliasArgs(alias);
        while (arg_iter.next()) |arg_var| {
            const arg_resolved = store.resolveVar(arg_var);
            const deep_arg = try self.deepCopyContent(store, arg_resolved.desc.content);
            _ = try self.scratch_content.append(self.gpa, deep_arg);
        }

        // Append scratch to backing array, and shrink scratch
        const args_range = try self.content_indexes.appendSlice(self.gpa, self.scratch_content.sliceFromStart(scratch_top));
        self.scratch_content.clearFrom(scratch_top);

        return SnapshotAlias{
            .ident = alias.ident,
            .vars = args_range,
        };
    }

    fn deepCopyTuple(self: *Self, store: *const TypesStore, tuple: types.Tuple) std.mem.Allocator.Error!SnapshotTuple {
        const elems_slice = store.sliceVars(tuple.elems);

        // Mark starting position in the scratch array
        const scratch_top = self.scratch_content.top();

        // Iterate and append to scratch array
        for (elems_slice) |elem_var| {
            const elem_resolved = store.resolveVar(elem_var);
            const deep_elem = try self.deepCopyContent(store, elem_resolved.desc.content);
            _ = try self.scratch_content.append(self.gpa, deep_elem);
        }

        // Append scratch to backing array, and shrink scratch
        const elems_range = try self.content_indexes.appendSlice(self.gpa, self.scratch_content.sliceFromStart(scratch_top));
        self.scratch_content.clearFrom(scratch_top);

        return SnapshotTuple{
            .elems = elems_range,
        };
    }

    fn deepCopyNum(self: *Self, store: *const TypesStore, num: types.Num) std.mem.Allocator.Error!SnapshotNum {
        switch (num) {
            .num_poly => |poly| {
                const resolved_poly = store.resolveVar(poly.var_);
                const deep_poly = try self.deepCopyContent(store, resolved_poly.desc.content);
                return SnapshotNum{ .num_poly = deep_poly };
            },
            .int_poly => |poly| {
                const resolved_poly = store.resolveVar(poly.var_);
                const deep_poly = try self.deepCopyContent(store, resolved_poly.desc.content);
                return SnapshotNum{ .int_poly = deep_poly };
            },
            .num_unbound => |requirements| {
                // For unbound types, we don't have a var to resolve, just return the requirements
                return SnapshotNum{ .num_unbound = requirements };
            },
            .int_unbound => |requirements| {
                // For unbound types, we don't have a var to resolve, just return the requirements
                return SnapshotNum{ .int_unbound = requirements };
            },
            .frac_unbound => |requirements| {
                // For unbound types, we don't have a var to resolve, just return the requirements
                return SnapshotNum{ .frac_unbound = requirements };
            },
            .frac_poly => |poly| {
                const resolved_poly = store.resolveVar(poly.var_);
                const deep_poly = try self.deepCopyContent(store, resolved_poly.desc.content);
                return SnapshotNum{ .frac_poly = deep_poly };
            },
            .int_precision => |prec| {
                return SnapshotNum{ .int_precision = prec };
            },
            .frac_precision => |prec| {
                return SnapshotNum{ .frac_precision = prec };
            },
            .num_compact => |compact| {
                return SnapshotNum{ .num_compact = compact };
            },
        }
    }

    fn deepCopyNominalType(self: *Self, store: *const TypesStore, nominal_type: types.NominalType) std.mem.Allocator.Error!SnapshotNominalType {
        // Mark starting position in the scratch array
        const scratch_top = self.scratch_content.top();

        // Add backing var (must be first)
        const backing_var = store.getNominalBackingVar(nominal_type);
        const backing_resolved = store.resolveVar(backing_var);
        const deep_var = try self.deepCopyContent(store, backing_resolved.desc.content);
        _ = try self.scratch_content.append(self.gpa, deep_var);

        // Add args after
        var arg_iter = store.iterNominalArgs(nominal_type);
        while (arg_iter.next()) |arg_var| {
            const arg_resolved = store.resolveVar(arg_var);
            const deep_arg = try self.deepCopyContent(store, arg_resolved.desc.content);
            _ = try self.scratch_content.append(self.gpa, deep_arg);
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
            const arg_resolved = store.resolveVar(arg_var);
            const deep_arg = try self.deepCopyContent(store, arg_resolved.desc.content);
            _ = try self.scratch_content.append(self.gpa, deep_arg);
        }

        // Append scratch to backing array, and shrink scratch
        const args_range = try self.content_indexes.appendSlice(self.gpa, self.scratch_content.sliceFromStart(scratch_top));
        self.scratch_content.clearFrom(scratch_top);

        // Deep copy return type
        const ret_resolved = store.resolveVar(func.ret);
        const deep_ret = try self.deepCopyContent(store, ret_resolved.desc.content);

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
            const field_resolved = store.resolveVar(var_);
            const deep_field_content = try self.deepCopyContent(store, field_resolved.desc.content);

            const snapshot_field = SnapshotRecordField{
                .name = name,
                .content = deep_field_content,
            };

            _ = try self.scratch_record_fields.append(self.gpa, snapshot_field);
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

            const field_resolved = store.resolveVar(field.var_);
            const deep_field_content = try self.deepCopyContent(store, field_resolved.desc.content);

            const snapshot_field = SnapshotRecordField{
                .name = field.name,
                .content = deep_field_content,
            };

            _ = try self.scratch_record_fields.append(self.gpa, snapshot_field);
        }

        // Append scratch to backing array, and shrink scratch
        const fields_range = try self.record_fields.appendSlice(self.gpa, self.scratch_record_fields.sliceFromStart(scratch_top));
        self.scratch_record_fields.clearFrom(scratch_top);

        // Deep copy extension type
        const ext_resolved = store.resolveVar(record.ext);
        const deep_ext = try self.deepCopyContent(store, ext_resolved.desc.content);

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
                const tag_arg_resolved = store.resolveVar(tag_arg_var);
                const deep_tag_arg = try self.deepCopyContent(store, tag_arg_resolved.desc.content);
                _ = try self.scratch_content.append(self.gpa, deep_tag_arg);
            }

            // Append scratch to backing array, and shrink scratch
            const tag_args_range = try self.content_indexes.appendSlice(self.gpa, self.scratch_content.sliceFromStart(content_scratch_top));
            self.scratch_content.clearFrom(content_scratch_top);

            // Create and append the snapshot tag to scratch
            const snapshot_tag = SnapshotTag{
                .name = tag.name,
                .args = tag_args_range,
            };

            _ = try self.scratch_tags.append(self.gpa, snapshot_tag);
        }

        // Append scratch tags to backing array, and shrink scratch
        const tags_range = try self.tags.appendSlice(self.gpa, self.scratch_tags.sliceFromStart(tags_scratch_top));
        self.scratch_tags.clearFrom(tags_scratch_top);

        // Deep copy extension type
        const ext_resolved = store.resolveVar(tag_union.ext);
        const deep_ext = try self.deepCopyContent(store, ext_resolved.desc.content);

        return SnapshotTagUnion{
            .tags = tags_range,
            .ext = deep_ext,
        };
    }

    // Getter methods (similar to Store)
    pub fn sliceVars(self: *const Self, range: SnapshotContentIdxSafeList.Range) []const SnapshotContentIdx {
        return self.content_indexes.sliceRange(range);
    }

    pub fn getRecordFieldsSlice(self: *const Self, range: SnapshotRecordFieldSafeList.Range) SnapshotRecordFieldSafeList.Slice {
        return self.record_fields.sliceRange(range);
    }

    pub fn getTagsSlice(self: *const Self, range: SnapshotTagSafeList.Range) []const SnapshotTag {
        return self.tags.sliceRange(range);
    }

    pub fn getContent(self: *const Self, idx: SnapshotContentIdx) SnapshotContent {
        return self.contents.get(idx).*;
    }
};

/// Snapshot types (no Var references!)
pub const SnapshotContent = union(enum) {
    flex_var: ?Ident.Idx,
    rigid_var: Ident.Idx,
    alias: SnapshotAlias,
    structure: SnapshotFlatType,
    err,
};

/// TODO
pub const SnapshotAlias = struct {
    ident: types.TypeIdent,
    vars: SnapshotContentIdxSafeList.Range, // The 1st variable is the backing var, rest are args
};

/// TODO
pub const SnapshotFlatType = union(enum) {
    str,
    box: SnapshotContentIdx, // Index into SnapshotStore.contents
    list: SnapshotContentIdx,
    list_unbound,
    tuple: SnapshotTuple,
    num: SnapshotNum,
    nominal_type: SnapshotNominalType,
    fn_pure: SnapshotFunc,
    fn_effectful: SnapshotFunc,
    fn_unbound: SnapshotFunc,
    record: SnapshotRecord,
    record_unbound: SnapshotRecordFieldSafeList.Range,
    record_poly: struct { record: SnapshotRecord, var_: SnapshotContentIdx },
    empty_record,
    tag_union: SnapshotTagUnion,
    empty_tag_union,
};

/// TODO
pub const SnapshotTuple = struct {
    elems: SnapshotContentIdxSafeList.Range, // Range into SnapshotStore.vars
};

/// TODO
pub const SnapshotNum = union(enum) {
    num_poly: SnapshotContentIdx,
    int_poly: SnapshotContentIdx,
    frac_poly: SnapshotContentIdx,
    num_unbound: types.Num.IntRequirements,
    int_unbound: types.Num.IntRequirements,
    frac_unbound: types.Num.FracRequirements,
    int_precision: types.Num.Int.Precision,
    frac_precision: types.Num.Frac.Precision,
    num_compact: types.Num.Compact,
};

/// TODO
pub const SnapshotNominalType = struct {
    ident: types.TypeIdent,
    vars: SnapshotContentIdxSafeList.Range, // The 1st variable is the backing var, rest are args
    origin_module: Ident.Idx,
};

/// TODO
pub const SnapshotFunc = struct {
    args: SnapshotContentIdxSafeList.Range, // Range into SnapshotStore.func_args
    ret: SnapshotContentIdx, // Index into SnapshotStore.contents
    needs_instantiation: bool,
};

/// TODO
pub const SnapshotRecord = struct {
    fields: SnapshotRecordFieldSafeList.Range, // Range into SnapshotStore.record_fields
    ext: SnapshotContentIdx, // Index into SnapshotStore.contents
};

/// TODO
pub const SnapshotRecordField = struct {
    name: Ident.Idx,
    content: SnapshotContentIdx, // Instead of var_
};

/// TODO
pub const SnapshotTagUnion = struct {
    tags: SnapshotTagSafeList.Range, // Range into SnapshotStore.tags
    ext: SnapshotContentIdx, // Index into SnapshotStore.contents
};

/// TODO
pub const SnapshotTag = struct {
    name: Ident.Idx,
    args: SnapshotContentIdxSafeList.Range, // Range into SnapshotStore.tag_args
};

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
/// Entry point is `writeContent`
pub const SnapshotWriter = struct {
    const Self = @This();

    writer: std.ArrayList(u8).Writer,
    snapshots: *const Store,
    idents: *const Ident.Store,
    current_module_name: ?[]const u8,
    can_ir: ?*const ModuleEnv,
    other_modules: ?[]const *const ModuleEnv,
    next_name_index: u32,
    name_counters: std.EnumMap(TypeContext, u32),

    pub fn init(writer: std.ArrayList(u8).Writer, snapshots: *const Store, idents: *const Ident.Store) Self {
        return .{
            .writer = writer,
            .snapshots = snapshots,
            .idents = idents,
            .current_module_name = null,
            .can_ir = null,
            .other_modules = null,
            .next_name_index = 0,
            .name_counters = std.EnumMap(TypeContext, u32).init(.{}),
        };
    }

    pub fn initWithContext(
        writer: std.ArrayList(u8).Writer,
        snapshots: *const Store,
        idents: *const Ident.Store,
        current_module_name: []const u8,
        can_ir: *const ModuleEnv,
        other_modules: []const *const ModuleEnv,
    ) Self {
        return .{
            .writer = writer,
            .snapshots = snapshots,
            .idents = idents,
            .current_module_name = current_module_name,
            .can_ir = can_ir,
            .other_modules = other_modules,
            .next_name_index = 0,
            .name_counters = std.EnumMap(TypeContext, u32).init(.{}),
        };
    }

    fn generateNextName(self: *Self) !void {
        // Generate name: a, b, ..., z, aa, ab, ..., az, ba, ...
        // Skip any names that already exist in the identifier store
        // We need at most one more name than the number of existing identifiers
        const max_attempts = self.idents.interner.strings.size + 1;
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
                // This name is available, write it to the writer
                for (candidate_name) |c| {
                    try self.writer.writeByte(c);
                }
                break;
            }
            // Name already exists, try the next one
        }

        // This should never happen in practice, but let's handle it gracefully
        if (attempts >= max_attempts) {
            _ = try self.writer.write("var");
            try self.writer.print("{}", .{self.next_name_index});
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
        const max_attempts = self.idents.interner.strings.size + 1;
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
                    try self.writer.writeByte(c);
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

    /// Count how many times a content appears in a type
    fn countOccurrences(self: *const Self, search_idx: SnapshotContentIdx, root_idx: SnapshotContentIdx) usize {
        var count: usize = 0;
        self.countContent(search_idx, root_idx, &count);
        return count;
    }

    fn countContent(self: *const Self, search_idx: SnapshotContentIdx, current_idx: SnapshotContentIdx, count: *usize) void {
        if (current_idx == search_idx) {
            count.* += 1;
        }

        const content = self.snapshots.contents.get(current_idx);
        switch (content.*) {
            .flex_var, .rigid_var, .err => {},
            .alias => |alias| {
                const args = self.snapshots.sliceVars(alias.vars);
                for (args) |arg_idx| {
                    self.countContent(search_idx, arg_idx, count);
                }
            },
            .structure => |flat_type| {
                self.countInFlatType(search_idx, flat_type, count);
            },
        }
    }

    fn countInFlatType(self: *const Self, search_idx: SnapshotContentIdx, flat_type: SnapshotFlatType, count: *usize) void {
        switch (flat_type) {
            .str, .empty_record, .empty_tag_union => {},
            .box => |sub_idx| self.countContent(search_idx, sub_idx, count),
            .list => |sub_idx| self.countContent(search_idx, sub_idx, count),
            .list_unbound, .num => {},
            .tuple => |tuple| {
                const elems = self.snapshots.sliceVars(tuple.elems);
                for (elems) |elem| {
                    self.countContent(search_idx, elem, count);
                }
            },
            .nominal_type => |nominal_type| {
                const args = self.snapshots.sliceVars(nominal_type.vars);
                // Skip the first var which is the nominal type's backing var
                for (args[1..]) |arg_idx| {
                    self.countContent(search_idx, arg_idx, count);
                }
            },
            .fn_pure, .fn_effectful, .fn_unbound => |func| {
                const args = self.snapshots.sliceVars(func.args);
                for (args) |arg| {
                    self.countContent(search_idx, arg, count);
                }
                self.countContent(search_idx, func.ret, count);
            },
            .record => |record| {
                const fields = self.snapshots.record_fields.sliceRange(record.fields);
                for (fields.items(.content)) |field_content| {
                    self.countContent(search_idx, field_content, count);
                }
                self.countContent(search_idx, record.ext, count);
            },
            .record_unbound => |fields| {
                const fields_slice = self.snapshots.record_fields.sliceRange(fields);
                for (fields_slice.items(.content)) |field_content| {
                    self.countContent(search_idx, field_content, count);
                }
            },
            .record_poly => |poly| {
                self.countInFlatType(search_idx, SnapshotFlatType{ .record = poly.record }, count);
                self.countContent(search_idx, poly.var_, count);
            },
            .tag_union => |tag_union| {
                var iter = tag_union.tags.iterIndices();
                while (iter.next()) |tag_idx| {
                    const tag = self.snapshots.tags.get(tag_idx);
                    const args = self.snapshots.sliceVars(tag.args);
                    for (args) |arg_idx| {
                        self.countContent(search_idx, arg_idx, count);
                    }
                }
                self.countContent(search_idx, tag_union.ext, count);
            },
        }
    }

    /// Convert a content to a type string with context
    pub fn writeWithContext(self: *Self, idx: SnapshotContentIdx, context: TypeContext, root_idx: SnapshotContentIdx) Allocator.Error!void {
        const content = self.snapshots.contents.get(idx);
        return self.writeContent(content.*, context, idx, root_idx);
    }

    /// Convert a content to a type string
    pub fn writeContent(self: *Self, content: SnapshotContent, context: TypeContext, current_idx: SnapshotContentIdx, root_idx: SnapshotContentIdx) Allocator.Error!void {
        switch (content) {
            .flex_var => |mb_ident_idx| {
                if (mb_ident_idx) |ident_idx| {
                    _ = try self.writer.write(self.idents.getText(ident_idx));
                } else {
                    // Check if this variable appears multiple times
                    const occurrences = self.countOccurrences(current_idx, root_idx);
                    if (occurrences == 1) {
                        _ = try self.writer.write("_");
                    }
                    try self.generateContextualName(context);
                }
            },
            .rigid_var => |ident_idx| {
                _ = try self.writer.write(self.idents.getText(ident_idx));
            },
            .alias => |alias| {
                try self.writeAlias(alias, root_idx);
            },
            .structure => |flat_type| {
                try self.writeFlatType(flat_type, root_idx);
            },
            .err => {
                _ = try self.writer.write("Error");
            },
        }
    }

    /// Write an alias type
    pub fn writeAlias(self: *Self, alias: SnapshotAlias, root_idx: SnapshotContentIdx) Allocator.Error!void {
        _ = try self.writer.write(self.idents.getText(alias.ident.ident_idx));

        // The 1st var is the alias type's backing var, so we skip it
        var vars = self.snapshots.sliceVars(alias.vars);
        std.debug.assert(vars.len > 0);
        vars = vars[1..];

        if (vars.len > 0) {
            _ = try self.writer.write("(");
            for (vars, 0..) |arg, i| {
                if (i > 0) _ = try self.writer.write(", ");
                try self.writeWithContext(arg, .General, root_idx);
            }
            _ = try self.writer.write(")");
        }
    }

    /// Convert a flat type to a type string
    pub fn writeFlatType(self: *Self, flat_type: SnapshotFlatType, root_idx: SnapshotContentIdx) Allocator.Error!void {
        switch (flat_type) {
            .str => {
                _ = try self.writer.write("Str");
            },
            .box => |sub_var| {
                _ = try self.writer.write("Box(");
                try self.writeWithContext(sub_var, .General, root_idx);
                _ = try self.writer.write(")");
            },
            .list => |sub_var| {
                _ = try self.writer.write("List(");
                try self.writeWithContext(sub_var, .ListContent, root_idx);
                _ = try self.writer.write(")");
            },
            .list_unbound => {
                _ = try self.writer.write("List(_");
                try self.generateContextualName(.ListContent);
                _ = try self.writer.write(")");
            },
            .tuple => |tuple| {
                try self.writeTuple(tuple, root_idx);
            },
            .num => |num| {
                try self.writeNum(num, root_idx);
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
            .record_poly => |poly| {
                try self.writeRecord(poly.record, root_idx);
                try self.writeWithContext(poly.var_, .General, root_idx);
            },
            .empty_record => {
                _ = try self.writer.write("{}");
            },
            .tag_union => |tag_union| {
                try self.writeTagUnion(tag_union, root_idx);
            },
            .empty_tag_union => {
                _ = try self.writer.write("[]");
            },
        }
    }

    /// Write a tuple type
    pub fn writeTuple(self: *Self, tuple: SnapshotTuple, root_idx: SnapshotContentIdx) Allocator.Error!void {
        const elems = self.snapshots.sliceVars(tuple.elems);
        _ = try self.writer.write("(");
        for (elems, 0..) |elem, i| {
            if (i > 0) _ = try self.writer.write(", ");
            try self.writeWithContext(elem, .TupleFieldContent, root_idx);
        }
        _ = try self.writer.write(")");
    }

    /// Write a nominal type
    pub fn writeNominalType(self: *Self, nominal_type: SnapshotNominalType, root_idx: SnapshotContentIdx) Allocator.Error!void {
        _ = try self.writer.write(self.idents.getText(nominal_type.ident.ident_idx));

        // The 1st var is the nominal type's backing var, so we skip it
        var vars = self.snapshots.sliceVars(nominal_type.vars);
        std.debug.assert(vars.len > 0);
        vars = vars[1..];

        if (vars.len > 0) {
            _ = try self.writer.write("(");
            for (vars, 0..) |arg, i| {
                if (i > 0) _ = try self.writer.write(", ");
                try self.writeWithContext(arg, .General, root_idx);
            }
            _ = try self.writer.write(")");
        }

        // Add origin information if it's from a different module
        if (self.current_module_name) |current_module| {
            const origin_module_name = self.idents.getText(nominal_type.origin_module);

            // Only show origin if it's different from the current module
            if (!std.mem.eql(u8, origin_module_name, current_module)) {
                _ = try self.writer.write(" (from ");
                _ = try self.writer.write(origin_module_name);
                _ = try self.writer.write(")");
            }
        }
    }

    /// Convert a content to a type string
    pub fn write(self: *Self, idx: SnapshotContentIdx) Allocator.Error!void {
        try self.writeWithContext(idx, .General, idx);
    }

    /// Write a function type with a specific arrow
    pub fn writeFuncWithArrow(self: *Self, func: SnapshotFunc, arrow: []const u8, root_idx: SnapshotContentIdx) Allocator.Error!void {
        const args = self.snapshots.sliceVars(func.args);

        // Write arguments
        if (args.len == 0) {
            _ = try self.writer.write("({})");
        } else if (args.len == 1) {
            try self.writeWithContext(args[0], .FunctionArgument, root_idx);
        } else {
            for (args, 0..) |arg, i| {
                if (i > 0) _ = try self.writer.write(", ");
                try self.writeWithContext(arg, .FunctionArgument, root_idx);
            }
        }

        _ = try self.writer.write(arrow);

        try self.writeWithContext(func.ret, .FunctionReturn, root_idx);
    }

    /// Write a record type
    pub fn writeRecord(self: *Self, record: SnapshotRecord, root_idx: SnapshotContentIdx) Allocator.Error!void {
        _ = try self.writer.write("{ ");

        const fields_slice = self.snapshots.record_fields.sliceRange(record.fields);

        if (fields_slice.len > 0) {
            // Write first field
            _ = try self.writer.write(self.idents.getText(fields_slice.items(.name)[0]));
            _ = try self.writer.write(": ");
            try self.writeWithContext(fields_slice.items(.content)[0], .RecordFieldContent, root_idx);

            // Write remaining fields
            for (fields_slice.items(.name)[1..], fields_slice.items(.content)[1..]) |name, content| {
                _ = try self.writer.write(", ");
                _ = try self.writer.write(self.idents.getText(name));
                _ = try self.writer.write(": ");
                try self.writeWithContext(content, .RecordFieldContent, root_idx);
            }
        }

        // Show extension variable if it's not empty
        switch (self.snapshots.contents.get(record.ext).*) {
            .structure => |flat_type| switch (flat_type) {
                .empty_record => {}, // Don't show empty extension
                else => {
                    if (fields_slice.len > 0) _ = try self.writer.write(", ");
                    try self.writeWithContext(record.ext, .RecordExtension, root_idx);
                },
            },
            else => {
                if (fields_slice.len > 0) _ = try self.writer.write(", ");
                try self.writeWithContext(record.ext, .RecordExtension, root_idx);
            },
        }

        _ = try self.writer.write(" }");
    }

    /// Write record fields without extension
    pub fn writeRecordFields(self: *Self, fields: SnapshotRecordFieldSafeList.Range, root_idx: SnapshotContentIdx) Allocator.Error!void {
        if (fields.isEmpty()) {
            _ = try self.writer.write("{}");
            return;
        }

        const fields_slice = self.snapshots.record_fields.sliceRange(fields);

        _ = try self.writer.write("{ ");

        // Write first field - we already verified that there is at least one field.
        _ = try self.writer.write(self.idents.getText(fields_slice.items(.name)[0]));
        _ = try self.writer.write(": ");
        try self.writeWithContext(fields_slice.items(.content)[0], .RecordFieldContent, root_idx);

        // Write remaining fields
        for (fields_slice.items(.name)[1..], fields_slice.items(.content)[1..]) |name, content| {
            _ = try self.writer.write(", ");
            _ = try self.writer.write(self.idents.getText(name));
            _ = try self.writer.write(": ");
            try self.writeWithContext(content, .RecordFieldContent, root_idx);
        }

        _ = try self.writer.write(" }");
    }

    /// Write a tag union
    pub fn writeTagUnion(self: *Self, tag_union: SnapshotTagUnion, root_idx: SnapshotContentIdx) Allocator.Error!void {
        _ = try self.writer.write("[");
        var iter = tag_union.tags.iterIndices();
        while (iter.next()) |tag_idx| {
            if (@intFromEnum(tag_idx) > @intFromEnum(tag_union.tags.start)) {
                _ = try self.writer.write(", ");
            }

            const tag = self.snapshots.tags.get(tag_idx);
            try self.writeTag(tag, root_idx);
        }

        _ = try self.writer.write("]");

        // Show extension variable if it's not empty
        switch (self.snapshots.contents.get(tag_union.ext).*) {
            .flex_var => |mb_ident| {
                if (mb_ident) |ident_idx| {
                    _ = try self.writer.write(self.idents.getText(ident_idx));
                } else {
                    // Check if this variable appears multiple times
                    const occurrences = self.countOccurrences(tag_union.ext, root_idx);
                    if (occurrences == 1) {
                        _ = try self.writer.write("_");
                    }
                    try self.generateContextualName(.TagUnionExtension);
                }
            },
            .structure => |flat_type| switch (flat_type) {
                .empty_tag_union => {}, // Don't show empty extension
                else => {
                    try self.writeWithContext(tag_union.ext, .TagUnionExtension, root_idx);
                },
            },
            .rigid_var => |ident_idx| {
                _ = try self.writer.write(self.idents.getText(ident_idx));
            },
            else => {
                try self.writeWithContext(tag_union.ext, .TagUnionExtension, root_idx);
            },
        }
    }

    /// Write a single tag
    pub fn writeTag(self: *Self, tag: SnapshotTag, root_idx: SnapshotContentIdx) Allocator.Error!void {
        _ = try self.writer.write(self.idents.getText(tag.name));
        const args = self.snapshots.sliceVars(tag.args);
        if (args.len > 0) {
            _ = try self.writer.write("(");
            for (args, 0..) |arg, i| {
                if (i > 0) _ = try self.writer.write(", ");
                try self.writeWithContext(arg, .General, root_idx);
            }
            _ = try self.writer.write(")");
        }
    }

    /// Convert a num type to a type string
    pub fn writeNum(self: *Self, num: SnapshotNum, root_idx: SnapshotContentIdx) Allocator.Error!void {
        switch (num) {
            .num_poly => |sub_var| {
                _ = try self.writer.write("Num(");
                try self.writeWithContext(sub_var, .NumContent, root_idx);
                _ = try self.writer.write(")");
            },
            .int_poly => |sub_var| {
                _ = try self.writer.write("Int(");
                try self.writeWithContext(sub_var, .NumContent, root_idx);
                _ = try self.writer.write(")");
            },
            .frac_poly => |sub_var| {
                _ = try self.writer.write("Frac(");
                try self.writeWithContext(sub_var, .NumContent, root_idx);
                _ = try self.writer.write(")");
            },
            .num_unbound => |_| {
                _ = try self.writer.write("Num(_");
                try self.generateContextualName(.NumContent);
                _ = try self.writer.write(")");
            },
            .int_unbound => |_| {
                _ = try self.writer.write("Int(_");
                try self.generateContextualName(.NumContent);
                _ = try self.writer.write(")");
            },
            .frac_unbound => |_| {
                _ = try self.writer.write("Frac(_");
                try self.generateContextualName(.NumContent);
                _ = try self.writer.write(")");
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
