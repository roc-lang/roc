//! Capture the state of an type at a point in time for the purpose of error reporting.

const std = @import("std");
const base = @import("../../base.zig");
const collections = @import("../../collections.zig");
const types = @import("../../types/types.zig");
const store_mod = @import("../../types/store.zig");

const TypesStore = store_mod.Store;
const Allocator = std.mem.Allocator;
const Ident = base.Ident;
const exitOnOutOfMemory = collections.utils.exitOnOom;

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

    // Backing arrays for ranges (like Store)
    alias_args: SnapshotContentIdxSafeList,
    tuple_elems: SnapshotContentIdxSafeList,
    nominal_type_args: SnapshotContentIdxSafeList,
    func_args: SnapshotContentIdxSafeList,
    record_fields: SnapshotRecordFieldSafeList,
    tags: SnapshotTagSafeList,
    tag_args: SnapshotContentIdxSafeList,

    // Scratch
    scratch_content: base.Scratch(SnapshotContentIdx),
    scratch_tags: base.Scratch(SnapshotTag),
    scratch_record_fields: base.Scratch(SnapshotRecordField),

    pub fn initCapacity(gpa: Allocator, capacity: usize) Self {
        return .{
            .gpa = gpa,
            .contents = SnapshotContentList.initCapacity(gpa, capacity),
            .alias_args = SnapshotContentIdxSafeList.initCapacity(gpa, capacity),
            .tuple_elems = SnapshotContentIdxSafeList.initCapacity(gpa, capacity),
            .nominal_type_args = SnapshotContentIdxSafeList.initCapacity(gpa, capacity),
            .func_args = SnapshotContentIdxSafeList.initCapacity(gpa, capacity),
            .record_fields = SnapshotRecordFieldSafeList.initCapacity(gpa, 256),
            .tags = SnapshotTagSafeList.initCapacity(gpa, 256),
            .tag_args = SnapshotContentIdxSafeList.initCapacity(gpa, 256),
            .scratch_content = base.Scratch(SnapshotContentIdx).init(gpa),
            .scratch_tags = base.Scratch(SnapshotTag).init(gpa),
            .scratch_record_fields = base.Scratch(SnapshotRecordField).init(gpa),
        };
    }

    pub fn deinit(self: *Self) void {
        self.contents.deinit(self.gpa);
        self.alias_args.deinit(self.gpa);
        self.tuple_elems.deinit(self.gpa);
        self.nominal_type_args.deinit(self.gpa);
        self.func_args.deinit(self.gpa);
        self.record_fields.deinit(self.gpa);
        self.tags.deinit(self.gpa);
        self.tag_args.deinit(self.gpa);
        self.scratch_content.deinit(self.gpa);
        self.scratch_tags.deinit(self.gpa);
        self.scratch_record_fields.deinit(self.gpa);
    }

    /// Create a deep snapshot from a Var, storing it in this SnapshotStore
    /// Deep copy a type variable's content into self-contained snapshot storage
    pub fn deepCopyVar(self: *Self, store: *const TypesStore, var_: types.Var) SnapshotContentIdx {
        const resolved = store.resolveVar(var_);
        return self.deepCopyContent(store, resolved.desc.content);
    }

    fn deepCopyContent(self: *Self, store: *const TypesStore, content: Content) SnapshotContentIdx {
        const deep_content = switch (content) {
            .flex_var => |ident| SnapshotContent{ .flex_var = ident },
            .rigid_var => |ident| SnapshotContent{ .rigid_var = ident },
            .alias => |alias| SnapshotContent{ .alias = self.deepCopyAlias(store, alias) },
            .structure => |flat_type| SnapshotContent{ .structure = self.deepCopyFlatType(store, flat_type) },
            .err => SnapshotContent.err,
        };

        return self.contents.append(self.gpa, deep_content);
    }

    fn deepCopyFlatType(self: *Self, store: *const TypesStore, flat_type: types.FlatType) SnapshotFlatType {
        return switch (flat_type) {
            .str => SnapshotFlatType.str,
            .box => |box_var| {
                const resolved = store.resolveVar(box_var);
                const deep_content = self.deepCopyContent(store, resolved.desc.content);
                return SnapshotFlatType{ .box = deep_content };
            },
            .list => |list_var| {
                const resolved = store.resolveVar(list_var);
                const deep_content = self.deepCopyContent(store, resolved.desc.content);
                return SnapshotFlatType{ .list = deep_content };
            },
            .list_unbound => {
                return SnapshotFlatType.list_unbound;
            },
            .tuple => |tuple| SnapshotFlatType{ .tuple = self.deepCopyTuple(store, tuple) },
            .num => |num| SnapshotFlatType{ .num = self.deepCopyNum(store, num) },
            .nominal_type => |nominal_type| SnapshotFlatType{ .nominal_type = self.deepCopyNominalType(store, nominal_type) },
            .fn_pure => |func| SnapshotFlatType{ .fn_pure = self.deepCopyFunc(store, func) },
            .fn_effectful => |func| SnapshotFlatType{ .fn_effectful = self.deepCopyFunc(store, func) },
            .fn_unbound => |func| SnapshotFlatType{ .fn_unbound = self.deepCopyFunc(store, func) },
            .record => |record| SnapshotFlatType{ .record = self.deepCopyRecord(store, record) },
            .record_unbound => |fields| SnapshotFlatType{ .record_unbound = self.deepCopyRecordFields(store, fields) },
            .record_poly => |poly| SnapshotFlatType{ .record_poly = .{
                .record = self.deepCopyRecord(store, poly.record),
                .var_ = self.deepCopyContent(store, store.resolveVar(poly.var_).desc.content),
            } },
            .empty_record => SnapshotFlatType.empty_record,
            .tag_union => |tag_union| SnapshotFlatType{ .tag_union = self.deepCopyTagUnion(store, tag_union) },
            .empty_tag_union => SnapshotFlatType.empty_tag_union,
        };
    }

    fn deepCopyAlias(self: *Self, store: *const TypesStore, alias: types.Alias) SnapshotAlias {
        // Mark starting position in the scratch array
        const scratch_top = self.scratch_content.top();

        const backing_var = store.getAliasBackingVar(alias);
        const backing_resolved = store.resolveVar(backing_var);
        const deep_backing = self.deepCopyContent(store, backing_resolved.desc.content);
        _ = self.scratch_content.append(self.gpa, deep_backing);

        // Iterate and append to scratch array
        var arg_iter = store.iterAliasArgs(alias);
        while (arg_iter.next()) |arg_var| {
            const arg_resolved = store.resolveVar(arg_var);
            const deep_arg = self.deepCopyContent(store, arg_resolved.desc.content);
            _ = self.scratch_content.append(self.gpa, deep_arg);
        }

        // Append scratch to backing array, and shrink scratch
        const args_range = self.alias_args.appendSlice(self.gpa, self.scratch_content.sliceFromStart(scratch_top));
        self.scratch_content.clearFrom(scratch_top);

        return SnapshotAlias{
            .ident = alias.ident,
            .vars = args_range,
        };
    }

    fn deepCopyTuple(self: *Self, store: *const TypesStore, tuple: types.Tuple) SnapshotTuple {
        const elems_slice = store.getTupleElemsSlice(tuple.elems);

        // Mark starting position in the scratch array
        const scratch_top = self.scratch_content.top();

        // Iterate and append to scratch array
        for (elems_slice) |elem_var| {
            const elem_resolved = store.resolveVar(elem_var);
            const deep_elem = self.deepCopyContent(store, elem_resolved.desc.content);
            _ = self.scratch_content.append(self.gpa, deep_elem);
        }

        // Append scratch to backing array, and shrink scratch
        const elems_range = self.tuple_elems.appendSlice(self.gpa, self.scratch_content.sliceFromStart(scratch_top));
        self.scratch_content.clearFrom(scratch_top);

        return SnapshotTuple{
            .elems = elems_range,
        };
    }

    fn deepCopyNum(self: *Self, store: *const TypesStore, num: types.Num) SnapshotNum {
        switch (num) {
            .num_poly => |poly| {
                const resolved_poly = store.resolveVar(poly.var_);
                const deep_poly = self.deepCopyContent(store, resolved_poly.desc.content);
                return SnapshotNum{ .num_poly = deep_poly };
            },
            .int_poly => |poly| {
                const resolved_poly = store.resolveVar(poly.var_);
                const deep_poly = self.deepCopyContent(store, resolved_poly.desc.content);
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
                const deep_poly = self.deepCopyContent(store, resolved_poly.desc.content);
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

    fn deepCopyNominalType(self: *Self, store: *const TypesStore, nominal_type: types.NominalType) SnapshotNominalType {
        // Mark starting position in the scratch array
        const scratch_top = self.scratch_content.top();

        // Add backing var (must be first)
        const backing_var = store.getNominalBackingVar(nominal_type);
        const backing_resolved = store.resolveVar(backing_var);
        const deep_var = self.deepCopyContent(store, backing_resolved.desc.content);
        _ = self.scratch_content.append(self.gpa, deep_var);

        // Add args after
        var arg_iter = store.iterNominalArgs(nominal_type);
        while (arg_iter.next()) |arg_var| {
            const arg_resolved = store.resolveVar(arg_var);
            const deep_arg = self.deepCopyContent(store, arg_resolved.desc.content);
            _ = self.scratch_content.append(self.gpa, deep_arg);
        }

        // Append scratch to backing array, and shrink scratch
        const args_range = self.nominal_type_args.appendSlice(self.gpa, self.scratch_content.sliceFromStart(scratch_top));
        self.scratch_content.clearFrom(scratch_top);

        return SnapshotNominalType{
            .ident = nominal_type.ident,
            .vars = args_range,
            .origin_module = nominal_type.origin_module,
        };
    }

    fn deepCopyFunc(self: *Self, store: *const TypesStore, func: types.Func) SnapshotFunc {
        const args_slice = store.getFuncArgsSlice(func.args);

        // Mark starting position in the scratch array
        const scratch_top = self.scratch_content.top();

        // Iterate and append directly
        for (args_slice) |arg_var| {
            const arg_resolved = store.resolveVar(arg_var);
            const deep_arg = self.deepCopyContent(store, arg_resolved.desc.content);
            _ = self.scratch_content.append(self.gpa, deep_arg);
        }

        // Append scratch to backing array, and shrink scratch
        const args_range = self.func_args.appendSlice(self.gpa, self.scratch_content.sliceFromStart(scratch_top));
        self.scratch_content.clearFrom(scratch_top);

        // Deep copy return type
        const ret_resolved = store.resolveVar(func.ret);
        const deep_ret = self.deepCopyContent(store, ret_resolved.desc.content);

        return SnapshotFunc{
            .args = args_range,
            .ret = deep_ret,
            .needs_instantiation = func.needs_instantiation,
        };
    }

    fn deepCopyRecordFields(self: *Self, store: *const TypesStore, fields: types.RecordField.SafeMultiList.Range) SnapshotRecordFieldSafeList.Range {
        // Mark starting position in the scratch array
        const scratch_top = self.scratch_record_fields.top();

        const fields_slice = store.getRecordFieldsSlice(fields);
        for (fields_slice.items(.name), fields_slice.items(.var_)) |name, var_| {
            const field_resolved = store.resolveVar(var_);
            const deep_field_content = self.deepCopyContent(store, field_resolved.desc.content);

            const snapshot_field = SnapshotRecordField{
                .name = name,
                .content = deep_field_content,
            };

            _ = self.scratch_record_fields.append(self.gpa, snapshot_field);
        }

        // Append scratch to backing array, and shrink scratch
        const fields_range = self.record_fields.appendSlice(self.gpa, self.scratch_record_fields.sliceFromStart(scratch_top));
        self.scratch_record_fields.clearFrom(scratch_top);

        return fields_range;
    }

    fn deepCopyRecord(self: *Self, store: *const TypesStore, record: types.Record) SnapshotRecord {
        // Mark starting position in the scratch array
        const scratch_top = self.scratch_record_fields.top();

        // Iterate and append to scratch array
        var fields_iter = record.fields.iterIndices();
        while (fields_iter.next()) |field_idx| {
            const field = store.record_fields.get(field_idx);

            const field_resolved = store.resolveVar(field.var_);
            const deep_field_content = self.deepCopyContent(store, field_resolved.desc.content);

            const snapshot_field = SnapshotRecordField{
                .name = field.name,
                .content = deep_field_content,
            };

            _ = self.scratch_record_fields.append(self.gpa, snapshot_field);
        }

        // Append scratch to backing array, and shrink scratch
        const fields_range = self.record_fields.appendSlice(self.gpa, self.scratch_record_fields.sliceFromStart(scratch_top));
        self.scratch_record_fields.clearFrom(scratch_top);

        // Deep copy extension type
        const ext_resolved = store.resolveVar(record.ext);
        const deep_ext = self.deepCopyContent(store, ext_resolved.desc.content);

        return SnapshotRecord{
            .fields = fields_range,
            .ext = deep_ext,
        };
    }

    fn deepCopyTagUnion(self: *Self, store: *const TypesStore, tag_union: types.TagUnion) SnapshotTagUnion {
        // Mark starting position in the scratch array for tags
        const tags_scratch_top = self.scratch_tags.top();

        // Iterate over tags and append to scratch array
        var tags_iter = tag_union.tags.iterIndices();
        while (tags_iter.next()) |tag_idx| {
            const tag = store.tags.get(tag_idx);

            const tag_args_slice = store.getTagArgsSlice(tag.args);

            // Mark starting position in the scratch array for this tag's arguments
            const content_scratch_top = self.scratch_content.top();

            // Iterate over tag arguments and append to scratch array
            for (tag_args_slice) |tag_arg_var| {
                const tag_arg_resolved = store.resolveVar(tag_arg_var);
                const deep_tag_arg = self.deepCopyContent(store, tag_arg_resolved.desc.content);
                _ = self.scratch_content.append(self.gpa, deep_tag_arg);
            }

            // Append scratch to backing array, and shrink scratch
            const tag_args_range = self.tag_args.appendSlice(self.gpa, self.scratch_content.sliceFromStart(content_scratch_top));
            self.scratch_content.clearFrom(content_scratch_top);

            // Create and append the snapshot tag to scratch
            const snapshot_tag = SnapshotTag{
                .name = tag.name,
                .args = tag_args_range,
            };

            _ = self.scratch_tags.append(self.gpa, snapshot_tag);
        }

        // Append scratch tags to backing array, and shrink scratch
        const tags_range = self.tags.appendSlice(self.gpa, self.scratch_tags.sliceFromStart(tags_scratch_top));
        self.scratch_tags.clearFrom(tags_scratch_top);

        // Deep copy extension type
        const ext_resolved = store.resolveVar(tag_union.ext);
        const deep_ext = self.deepCopyContent(store, ext_resolved.desc.content);

        return SnapshotTagUnion{
            .tags = tags_range,
            .ext = deep_ext,
        };
    }

    // Getter methods (similar to Store)
    pub fn getAliasArgsSlice(self: *const Self, range: SnapshotContentIdxSafeList.Range) []const SnapshotContentIdx {
        return self.alias_args.rangeToSlice(range);
    }

    pub fn getTupleElemsSlice(self: *const Self, range: SnapshotContentIdxSafeList.Range) []const SnapshotContentIdx {
        return self.tuple_elems.rangeToSlice(range);
    }

    pub fn getNominalTypeArgsSlice(self: *const Self, range: SnapshotContentIdxSafeList.Range) []const SnapshotContentIdx {
        return self.nominal_type_args.rangeToSlice(range);
    }

    pub fn getFuncArgsSlice(self: *const Self, range: SnapshotContentIdxSafeList.Range) []const SnapshotContentIdx {
        return self.func_args.rangeToSlice(range);
    }

    pub fn getRecordFieldsSlice(self: *const Self, range: SnapshotRecordFieldSafeList.Range) SnapshotRecordFieldSafeList.Slice {
        return self.record_fields.rangeToSlice(range);
    }

    pub fn getTagsSlice(self: *const Self, range: SnapshotTagSafeList.Range) []const SnapshotTag {
        return self.tags.rangeToSlice(range);
    }

    pub fn getTagArgsSlice(self: *const Self, range: SnapshotContentIdxSafeList.Range) []const SnapshotContentIdx {
        return self.tag_args.rangeToSlice(range);
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
    elems: SnapshotContentIdxSafeList.Range, // Range into SnapshotStore.tuple_elems
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
    FunctionArgument,
};

/// Helper that accepts a `Var` and write it as a nice string.
/// Entry point is `writeContent`
pub const SnapshotWriter = struct {
    const Self = @This();

    writer: std.ArrayList(u8).Writer,
    snapshots: *const Store,
    idents: *const Ident.Store,
    current_module_name: ?[]const u8,
    can_ir: ?*const @import("../canonicalize/CIR.zig"),
    other_modules: ?[]const *const @import("../canonicalize/CIR.zig"),
    next_name_index: u32,
    context_stack: std.ArrayList(TypeContext),
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
            .context_stack = std.ArrayList(TypeContext).init(writer.context.allocator),
            .name_counters = std.EnumMap(TypeContext, u32).init(.{}),
        };
    }

    pub fn initWithContext(
        writer: std.ArrayList(u8).Writer,
        snapshots: *const Store,
        idents: *const Ident.Store,
        current_module_name: []const u8,
        can_ir: *const @import("../canonicalize/CIR.zig"),
        other_modules: []const *const @import("../canonicalize/CIR.zig"),
    ) Self {
        return .{
            .writer = writer,
            .snapshots = snapshots,
            .idents = idents,
            .current_module_name = current_module_name,
            .can_ir = can_ir,
            .other_modules = other_modules,
            .next_name_index = 0,
            .context_stack = std.ArrayList(TypeContext).init(writer.context.allocator),
            .name_counters = std.EnumMap(TypeContext, u32).init(.{}),
        };
    }

    fn generateNextName(self: *Self) !void {
        // Generate name: a, b, ..., z, aa, ab, ..., az, ba, ...
        // Skip any names that already exist in the identifier store
        // We need at most one more name than the number of existing identifiers
        const max_attempts = self.idents.interner.outer_indices.items.len + 1;
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

    fn generateContextualName(self: *Self) !void {
        const context = if (self.context_stack.items.len > 0)
            self.context_stack.items[self.context_stack.items.len - 1]
        else
            TypeContext.General;

        const base_name = switch (context) {
            .NumContent => "size",
            .ListContent => "elem",
            .RecordExtension => "others",
            .FunctionArgument => "arg",
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
        const max_attempts = self.idents.interner.outer_indices.items.len;
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
            var exists = false;
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

    /// Convert a content to a type string
    pub fn write(self: *Self, idx: SnapshotContentIdx) Allocator.Error!void {
        const content = self.snapshots.contents.get(idx);
        return self.writeContent(content.*);
    }

    /// Convert a content to a type string
    pub fn writeContent(self: *Self, content: SnapshotContent) Allocator.Error!void {
        switch (content) {
            .flex_var => |mb_ident_idx| {
                if (mb_ident_idx) |ident_idx| {
                    _ = try self.writer.write(self.idents.getText(ident_idx));
                } else {
                    try self.generateContextualName();
                }
            },
            .rigid_var => |ident_idx| {
                _ = try self.writer.write(self.idents.getText(ident_idx));
            },
            .alias => |alias| {
                try self.writeAlias(alias);
            },
            .structure => |flat_type| {
                try self.writeFlatType(flat_type);
            },
            .err => {
                _ = try self.writer.write("Error");
            },
        }
    }

    /// Write an alias type
    pub fn writeAlias(self: *Self, alias: SnapshotAlias) Allocator.Error!void {
        _ = try self.writer.write(self.idents.getText(alias.ident.ident_idx));

        // The 1st var is the alias type's backing var, so we skip it
        var vars = self.snapshots.getAliasArgsSlice(alias.vars);
        std.debug.assert(vars.len > 0);
        vars = vars[1..];

        if (vars.len > 0) {
            _ = try self.writer.write("(");
            for (vars, 0..) |arg, i| {
                if (i > 0) _ = try self.writer.write(", ");
                try self.write(arg);
            }
            _ = try self.writer.write(")");
        }
    }

    /// Convert a flat type to a type string
    pub fn writeFlatType(self: *Self, flat_type: SnapshotFlatType) Allocator.Error!void {
        switch (flat_type) {
            .str => {
                _ = try self.writer.write("Str");
            },
            .box => |sub_var| {
                _ = try self.writer.write("Box(");
                try self.write(sub_var);
                _ = try self.writer.write(")");
            },
            .list => |sub_var| {
                _ = try self.writer.write("List(");
                try self.context_stack.append(.ListContent);
                try self.write(sub_var);
                _ = self.context_stack.pop();
                _ = try self.writer.write(")");
            },
            .list_unbound => {
                _ = try self.writer.write("List(");
                try self.context_stack.append(.ListContent);
                try self.generateContextualName();
                _ = self.context_stack.pop();
                _ = try self.writer.write(")");
            },
            .tuple => |tuple| {
                try self.writeTuple(tuple);
            },
            .num => |num| {
                try self.writeNum(num);
            },
            .nominal_type => |nominal_type| {
                try self.writeNominalType(nominal_type);
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
                try self.write(poly.var_);
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
    pub fn writeTuple(self: *Self, tuple: SnapshotTuple) Allocator.Error!void {
        const elems = self.snapshots.getTupleElemsSlice(tuple.elems);
        _ = try self.writer.write("(");
        for (elems, 0..) |elem, i| {
            if (i > 0) _ = try self.writer.write(", ");
            try self.write(elem);
        }
        _ = try self.writer.write(")");
    }

    /// Write a nominal type
    pub fn writeNominalType(self: *Self, nominal_type: SnapshotNominalType) Allocator.Error!void {
        _ = try self.writer.write(self.idents.getText(nominal_type.ident.ident_idx));

        // The 1st var is the nominal type's backing var, so we skip it
        var vars = self.snapshots.getNominalTypeArgsSlice(nominal_type.vars);
        std.debug.assert(vars.len > 0);
        vars = vars[1..];

        if (vars.len > 0) {
            _ = try self.writer.write("(");
            for (vars, 0..) |arg, i| {
                if (i > 0) _ = try self.writer.write(", ");
                try self.write(arg);
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

    /// Write a function type with a specific arrow
    pub fn writeFuncWithArrow(self: *Self, func: SnapshotFunc, arrow: []const u8) Allocator.Error!void {
        const args = self.snapshots.getFuncArgsSlice(func.args);

        // Write arguments
        if (args.len == 0) {
            _ = try self.writer.write("({})");
        } else if (args.len == 1) {
            try self.context_stack.append(.FunctionArgument);
            try self.write(args[0]);
            _ = self.context_stack.pop();
        } else {
            for (args, 0..) |arg, i| {
                if (i > 0) _ = try self.writer.write(", ");
                try self.context_stack.append(.FunctionArgument);
                try self.write(arg);
                _ = self.context_stack.pop();
            }
        }

        _ = try self.writer.write(arrow);

        try self.write(func.ret);
    }

    /// Write a record type
    pub fn writeRecord(self: *Self, record: SnapshotRecord) Allocator.Error!void {
        _ = try self.writer.write("{ ");

        const fields_slice = self.snapshots.record_fields.rangeToSlice(record.fields);

        if (fields_slice.len > 0) {
            // Write first field
            _ = try self.writer.write(self.idents.getText(fields_slice.items(.name)[0]));
            _ = try self.writer.write(": ");
            try self.write(fields_slice.items(.content)[0]);

            // Write remaining fields
            for (fields_slice.items(.name)[1..], fields_slice.items(.content)[1..]) |name, content| {
                _ = try self.writer.write(", ");
                _ = try self.writer.write(self.idents.getText(name));
                _ = try self.writer.write(": ");
                try self.write(content);
            }
        }

        // Show extension variable if it's not empty
        switch (self.snapshots.contents.get(record.ext).*) {
            .structure => |flat_type| switch (flat_type) {
                .empty_record => {}, // Don't show empty extension
                else => {
                    if (fields_slice.len > 0) _ = try self.writer.write(", ");
                    try self.context_stack.append(.RecordExtension);
                    try self.write(record.ext);
                    _ = self.context_stack.pop();
                },
            },
            else => {
                if (fields_slice.len > 0) _ = try self.writer.write(", ");
                try self.context_stack.append(.RecordExtension);
                try self.write(record.ext);
                _ = self.context_stack.pop();
            },
        }

        _ = try self.writer.write(" }");
    }

    /// Write record fields without extension
    pub fn writeRecordFields(self: *Self, fields: SnapshotRecordFieldSafeList.Range) Allocator.Error!void {
        if (fields.isEmpty()) {
            _ = try self.writer.write("{}");
            return;
        }

        const fields_slice = self.snapshots.record_fields.rangeToSlice(fields);

        _ = try self.writer.write("{ ");

        // Write first field - we already verified that there is at least one field.
        _ = try self.writer.write(self.idents.getText(fields_slice.items(.name)[0]));
        _ = try self.writer.write(": ");
        try self.write(fields_slice.items(.content)[0]);

        // Write remaining fields
        for (fields_slice.items(.name)[1..], fields_slice.items(.content)[1..]) |name, content| {
            _ = try self.writer.write(", ");
            _ = try self.writer.write(self.idents.getText(name));
            _ = try self.writer.write(": ");
            try self.write(content);
        }

        _ = try self.writer.write(" }");
    }

    /// Write a tag union
    pub fn writeTagUnion(self: *Self, tag_union: SnapshotTagUnion) Allocator.Error!void {
        _ = try self.writer.write("[");
        var iter = tag_union.tags.iterIndices();
        while (iter.next()) |tag_idx| {
            if (@intFromEnum(tag_idx) > @intFromEnum(tag_union.tags.start)) {
                _ = try self.writer.write(", ");
            }

            const tag = self.snapshots.tags.get(tag_idx);
            try self.writeTag(tag);
        }

        _ = try self.writer.write("]");

        // Show extension variable if it's not empty
        switch (self.snapshots.contents.get(tag_union.ext).*) {
            .flex_var => |mb_ident| {
                if (mb_ident) |ident_idx| {
                    _ = try self.writer.write(self.idents.getText(ident_idx));
                } else {
                    try self.generateContextualName();
                }
            },
            .structure => |flat_type| switch (flat_type) {
                .empty_tag_union => {}, // Don't show empty extension
                else => {}, // TODO: Error?
            },
            else => {}, // TODO: Error?
        }
    }

    /// Write a single tag
    pub fn writeTag(self: *Self, tag: SnapshotTag) Allocator.Error!void {
        _ = try self.writer.write(self.idents.getText(tag.name));
        const args = self.snapshots.getTagArgsSlice(tag.args);
        if (args.len > 0) {
            _ = try self.writer.write("(");
            for (args, 0..) |arg, i| {
                if (i > 0) _ = try self.writer.write(", ");
                try self.write(arg);
            }
            _ = try self.writer.write(")");
        }
    }

    /// Convert a num type to a type string
    pub fn writeNum(self: *Self, num: SnapshotNum) Allocator.Error!void {
        switch (num) {
            .num_poly => |sub_var| {
                _ = try self.writer.write("Num(");
                try self.context_stack.append(.NumContent);
                try self.write(sub_var);
                _ = self.context_stack.pop();
                _ = try self.writer.write(")");
            },
            .int_poly => |sub_var| {
                _ = try self.writer.write("Int(");
                try self.context_stack.append(.NumContent);
                try self.write(sub_var);
                _ = self.context_stack.pop();
                _ = try self.writer.write(")");
            },
            .frac_poly => |sub_var| {
                _ = try self.writer.write("Frac(");
                try self.context_stack.append(.NumContent);
                try self.write(sub_var);
                _ = self.context_stack.pop();
                _ = try self.writer.write(")");
            },
            .num_unbound => |_| {
                _ = try self.writer.write("Num(");
                try self.context_stack.append(.NumContent);
                try self.generateContextualName();
                _ = self.context_stack.pop();
                _ = try self.writer.write(")");
            },
            .int_unbound => |_| {
                _ = try self.writer.write("Int(");
                try self.context_stack.append(.NumContent);
                try self.generateContextualName();
                _ = self.context_stack.pop();
                _ = try self.writer.write(")");
            },
            .frac_unbound => |_| {
                _ = try self.writer.write("Frac(");
                try self.context_stack.append(.NumContent);
                try self.generateContextualName();
                _ = self.context_stack.pop();
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
