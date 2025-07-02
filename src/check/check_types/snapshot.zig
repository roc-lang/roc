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
    }

    /// Create a deep snapshot from a Var, storing it in this SnapshotStore
    /// Deep copy a type variable's content into self-contained snapshot storage
    pub fn deepCopyVar(self: *Self, store: *const TypesStore, var_: types.Var) SnapshotContentIdx {
        const resolved = store.resolveVar(var_);
        return self.deepCopyContent(store, resolved.desc.content, var_);
    }

    fn deepCopyContent(self: *Self, store: *const TypesStore, content: Content, var_: types.Var) SnapshotContentIdx {
        const deep_content = switch (content) {
            .flex_var => |ident| SnapshotContent{ .flex_var = ident },
            .rigid_var => |ident| SnapshotContent{ .rigid_var = ident },
            .alias => |alias| SnapshotContent{ .alias = self.deepCopyAlias(store, alias, var_) },
            .structure => |flat_type| SnapshotContent{ .structure = self.deepCopyFlatType(store, flat_type, var_) },
            .err => SnapshotContent.err,
        };

        return self.contents.append(self.gpa, deep_content);
    }

    fn deepCopyFlatType(self: *Self, store: *const TypesStore, flat_type: types.FlatType, var_: types.Var) SnapshotFlatType {
        return switch (flat_type) {
            .str => SnapshotFlatType.str,
            .box => |box_var| {
                const resolved = store.resolveVar(box_var);
                const deep_content = self.deepCopyContent(store, resolved.desc.content, box_var);
                return SnapshotFlatType{ .box = deep_content };
            },
            .list => |list_var| {
                const resolved = store.resolveVar(list_var);
                const deep_content = self.deepCopyContent(store, resolved.desc.content, list_var);
                return SnapshotFlatType{ .list = deep_content };
            },
            .list_unbound => {
                return SnapshotFlatType.list_unbound;
            },
            .tuple => |tuple| SnapshotFlatType{ .tuple = self.deepCopyTuple(store, tuple) },
            .tuple_unbound => |tuple| SnapshotFlatType{ .tuple_unbound = self.deepCopyTuple(store, tuple) },
            .num => |num| SnapshotFlatType{ .num = self.deepCopyNum(store, num) },
            .nominal_type => |nominal_type| SnapshotFlatType{ .nominal_type = self.deepCopyNominalType(store, nominal_type, var_) },
            .fn_pure => |func| SnapshotFlatType{ .fn_pure = self.deepCopyFunc(store, func) },
            .fn_effectful => |func| SnapshotFlatType{ .fn_effectful = self.deepCopyFunc(store, func) },
            .fn_unbound => |func| SnapshotFlatType{ .fn_unbound = self.deepCopyFunc(store, func) },
            .record => |record| SnapshotFlatType{ .record = self.deepCopyRecord(store, record) },
            .record_unbound => |fields| SnapshotFlatType{ .record_unbound = self.deepCopyRecordFields(store, fields) },
            .record_poly => |poly| SnapshotFlatType{ .record_poly = .{
                .record = self.deepCopyRecord(store, poly.record),
                .var_ = self.deepCopyContent(store, store.resolveVar(poly.var_).desc.content, poly.var_),
            } },
            .empty_record => SnapshotFlatType.empty_record,
            .tag_union => |tag_union| SnapshotFlatType{ .tag_union = self.deepCopyTagUnion(store, tag_union) },
            .empty_tag_union => SnapshotFlatType.empty_tag_union,
        };
    }

    fn deepCopyAlias(self: *Self, store: *const TypesStore, alias: types.Alias, alias_var: types.Var) SnapshotAlias {
        // Mark starting position in the centralized array
        const start_idx = self.alias_args.len();

        // Iterate and append directly to centralized array
        var arg_iter = alias.argIterator(alias_var);
        while (arg_iter.next()) |arg_var| {
            const arg_resolved = store.resolveVar(arg_var);
            const deep_arg = self.deepCopyContent(store, arg_resolved.desc.content, arg_var);
            _ = self.alias_args.append(self.gpa, deep_arg);
        }

        // Create range using SafeList's Range structure
        const args_range = SnapshotContentIdxSafeList.Range{
            .start = @enumFromInt(start_idx),
            .end = @enumFromInt(self.alias_args.len()),
        };

        return SnapshotAlias{
            .ident = alias.ident,
            .args = args_range,
        };
    }

    fn deepCopyTuple(self: *Self, store: *const TypesStore, tuple: types.Tuple) SnapshotTuple {
        const elems_slice = store.getTupleElemsSlice(tuple.elems);

        // Mark starting position
        const start_idx = self.tuple_elems.len();

        // Iterate and append directly
        for (elems_slice) |elem_var| {
            const elem_resolved = store.resolveVar(elem_var);
            const deep_elem = self.deepCopyContent(store, elem_resolved.desc.content, elem_var);
            _ = self.tuple_elems.append(self.gpa, deep_elem);
        }

        // Create range
        const elems_range = SnapshotContentIdxSafeList.Range{
            .start = @enumFromInt(start_idx),
            .end = @enumFromInt(self.tuple_elems.len()),
        };

        return SnapshotTuple{
            .elems = elems_range,
        };
    }

    fn deepCopyNum(self: *Self, store: *const TypesStore, num: types.Num) SnapshotNum {
        switch (num) {
            .num_poly => |poly| {
                const resolved_poly = store.resolveVar(poly.var_);
                const deep_poly = self.deepCopyContent(store, resolved_poly.desc.content, poly.var_);
                return SnapshotNum{ .num_poly = deep_poly };
            },
            .int_poly => |poly| {
                const resolved_poly = store.resolveVar(poly.var_);
                const deep_poly = self.deepCopyContent(store, resolved_poly.desc.content, poly.var_);
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
                const deep_poly = self.deepCopyContent(store, resolved_poly.desc.content, poly.var_);
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

    fn deepCopyNominalType(self: *Self, store: *const TypesStore, nominal_type: types.NominalType, nominal_var: types.Var) SnapshotNominalType {
        // Mark starting position in the centralized array
        const start_idx = self.nominal_type_args.len();

        // // Iterate and append directly
        var arg_iter = nominal_type.argIterator(nominal_var);
        while (arg_iter.next()) |arg_var| {
            const arg_resolved = store.resolveVar(arg_var);
            const deep_arg = self.deepCopyContent(store, arg_resolved.desc.content, arg_var);
            _ = self.nominal_type_args.append(self.gpa, deep_arg);
        }

        // Create range
        const args_range = SnapshotContentIdxSafeList.Range{
            .start = @enumFromInt(start_idx),
            .end = @enumFromInt(self.nominal_type_args.len()),
        };

        return SnapshotNominalType{
            .ident = nominal_type.ident,
            .args = args_range,
        };
    }

    fn deepCopyFunc(self: *Self, store: *const TypesStore, func: types.Func) SnapshotFunc {
        const args_slice = store.getFuncArgsSlice(func.args);

        // Mark starting position for function arguments
        const start_idx = self.func_args.len();

        // Iterate and append directly
        for (args_slice) |arg_var| {
            const arg_resolved = store.resolveVar(arg_var);
            const deep_arg = self.deepCopyContent(store, arg_resolved.desc.content, arg_var);
            _ = self.func_args.append(self.gpa, deep_arg);
        }

        // Create range
        const args_range = SnapshotContentIdxSafeList.Range{
            .start = @enumFromInt(start_idx),
            .end = @enumFromInt(self.func_args.len()),
        };

        // Deep copy return type
        const ret_resolved = store.resolveVar(func.ret);
        const deep_ret = self.deepCopyContent(store, ret_resolved.desc.content, func.ret);

        return SnapshotFunc{
            .args = args_range,
            .ret = deep_ret,
        };
    }

    fn deepCopyRecordFields(self: *Self, store: *const TypesStore, fields: types.RecordField.SafeMultiList.Range) SnapshotRecordFieldSafeList.Range {
        const fields_start = @as(SnapshotRecordFieldSafeList.Idx, @enumFromInt(self.record_fields.len()));
        const fields_slice = store.getRecordFieldsSlice(fields);
        for (fields_slice.items(.name), fields_slice.items(.var_)) |name, var_| {
            const field_resolved = store.resolveVar(var_);
            const deep_field_content = self.deepCopyContent(store, field_resolved.desc.content, var_);

            const snapshot_field = SnapshotRecordField{
                .name = name,
                .content = deep_field_content,
            };

            _ = self.record_fields.append(self.gpa, snapshot_field);
        }

        const fields_end = @as(SnapshotRecordFieldSafeList.Idx, @enumFromInt(self.record_fields.len()));
        return .{ .start = fields_start, .end = fields_end };
    }

    fn deepCopyRecord(self: *Self, store: *const TypesStore, record: types.Record) SnapshotRecord {
        // Mark starting position
        const start_idx = self.record_fields.len();

        // Iterate and append directly
        var fields_iter = record.fields.iterIndices();
        while (fields_iter.next()) |field_idx| {
            const field = store.record_fields.get(field_idx);

            const field_resolved = store.resolveVar(field.var_);
            const deep_field_content = self.deepCopyContent(store, field_resolved.desc.content, field.var_);

            const snapshot_field = SnapshotRecordField{
                .name = field.name,
                .content = deep_field_content,
            };

            _ = self.record_fields.append(self.gpa, snapshot_field);
        }

        // Create range
        const fields_range = SnapshotRecordFieldSafeList.Range{
            .start = @enumFromInt(start_idx),
            .end = @enumFromInt(self.record_fields.len()),
        };

        // Deep copy extension type
        const ext_resolved = store.resolveVar(record.ext);
        const deep_ext = self.deepCopyContent(store, ext_resolved.desc.content, record.ext);

        return SnapshotRecord{
            .fields = fields_range,
            .ext = deep_ext,
        };
    }

    fn deepCopyTagUnion(self: *Self, store: *const TypesStore, tag_union: types.TagUnion) SnapshotTagUnion {
        // Mark starting position for tags
        const tags_start_idx = self.tags.len();

        // Iterate over tags and append directly
        var tags_iter = tag_union.tags.iterIndices();
        while (tags_iter.next()) |tag_idx| {
            const tag = store.tags.get(tag_idx);

            const tag_args_slice = store.getTagArgsSlice(tag.args);

            // Mark starting position for this tag's arguments
            const tag_args_start_idx = self.tag_args.len();

            // Iterate over tag arguments and append directly
            for (tag_args_slice) |tag_arg_var| {
                const tag_arg_resolved = store.resolveVar(tag_arg_var);
                const deep_tag_arg = self.deepCopyContent(store, tag_arg_resolved.desc.content, tag_arg_var);
                _ = self.tag_args.append(self.gpa, deep_tag_arg);
            }

            // Create range for this tag's arguments
            const tag_args_range = SnapshotContentIdxSafeList.Range{
                .start = @enumFromInt(tag_args_start_idx),
                .end = @enumFromInt(self.tag_args.len()),
            };

            // Create and append the snapshot tag
            const snapshot_tag = SnapshotTag{
                .name = tag.name,
                .args = tag_args_range,
            };

            _ = self.tags.append(self.gpa, snapshot_tag);
        }

        // Create range for all tags
        const tags_range = SnapshotTagSafeList.Range{
            .start = @enumFromInt(tags_start_idx),
            .end = @enumFromInt(self.tags.len()),
        };

        // Deep copy extension type
        const ext_resolved = store.resolveVar(tag_union.ext);
        const deep_ext = self.deepCopyContent(store, ext_resolved.desc.content, tag_union.ext);

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
        return self.contents.get(idx);
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
    args: SnapshotContentIdxSafeList.Range, // Range into SnapshotStore.alias_args
};

/// TODO
pub const SnapshotFlatType = union(enum) {
    str,
    box: SnapshotContentIdx, // Index into SnapshotStore.contents
    list: SnapshotContentIdx,
    list_unbound,
    tuple: SnapshotTuple,
    tuple_unbound: SnapshotTuple,
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
    args: SnapshotContentIdxSafeList.Range, // Range into SnapshotStore.nominal_type_args
};

/// TODO
pub const SnapshotFunc = struct {
    args: SnapshotContentIdxSafeList.Range, // Range into SnapshotStore.func_args
    ret: SnapshotContentIdx, // Index into SnapshotStore.contents
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

/// Helper that accepts a `Var` and write it as a nice string.
/// Entry point is `writeContent`
pub const SnapshotWriter = struct {
    const Self = @This();

    writer: std.ArrayList(u8).Writer,
    snapshots: *const Store,
    idents: *const Ident.Store,

    pub fn init(writer: std.ArrayList(u8).Writer, snapshots: *const Store, idents: *const Ident.Store) Self {
        return .{ .writer = writer, .snapshots = snapshots, .idents = idents };
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
                    _ = try self.writer.write("*");
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
        const args = self.snapshots.getAliasArgsSlice(alias.args);
        if (args.len > 0) {
            _ = try self.writer.write("(");
            for (args, 0..) |arg, i| {
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
                try self.write(sub_var);
                _ = try self.writer.write(")");
            },
            .list_unbound => {
                _ = try self.writer.write("List(*)");
            },
            .tuple => |tuple| {
                try self.writeTuple(tuple);
            },
            .tuple_unbound => |tuple| {
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
        const args = self.snapshots.getNominalTypeArgsSlice(nominal_type.args);
        if (args.len > 0) {
            _ = try self.writer.write("(");
            for (args, 0..) |arg, i| {
                if (i > 0) _ = try self.writer.write(", ");
                try self.write(arg);
            }
            _ = try self.writer.write(")");
        }
    }

    /// Write a function type with a specific arrow
    pub fn writeFuncWithArrow(self: *Self, func: SnapshotFunc, arrow: []const u8) Allocator.Error!void {
        const args = self.snapshots.getFuncArgsSlice(func.args);

        // Write arguments
        if (args.len == 0) {
            _ = try self.writer.write("({})");
        } else if (args.len == 1) {
            try self.write(args[0]);
        } else {
            for (args, 0..) |arg, i| {
                if (i > 0) _ = try self.writer.write(", ");
                try self.write(arg);
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
                    _ = try self.writer.write("* ");
                    try self.write(record.ext);
                },
            },
            else => {
                if (fields_slice.len > 0) _ = try self.writer.write(", ");
                _ = try self.writer.write("* ");
                try self.write(record.ext);
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
            .flex_var => _ = try self.writer.write("*"),
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
                try self.write(sub_var);
                _ = try self.writer.write(")");
            },
            .int_poly => |sub_var| {
                _ = try self.writer.write("Int(");
                try self.write(sub_var);
                _ = try self.writer.write(")");
            },
            .frac_poly => |sub_var| {
                _ = try self.writer.write("Frac(");
                try self.write(sub_var);
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
