//! Checked compile-time constant store.

const std = @import("std");

const checked_ids = @import("checked_ids.zig");
const names = @import("canonical_names.zig");
const artifact_serialize = @import("artifact_serialize.zig");
const static_dispatch = @import("static_dispatch_registry.zig");

const Allocator = std.mem.Allocator;

/// Identifier for a node in the checked const store.
pub const ConstNodeId = enum(u32) { _ };
/// Identifier for a function value in the checked const store.
pub const ConstFnId = enum(u32) { _ };
/// Identifier for stored string backing bytes in the checked const store.
pub const ConstStrDataId = enum(u32) { _ };
/// Identifier for a stored monomorphic type used by checked constants.
pub const ConstTypeId = enum(u32) { _ };

/// Scalar value stored by compile-time evaluation.
pub const ConstScalar = union(enum) {
    i8: i8,
    i16: i16,
    i32: i32,
    i64: i64,
    i128: i128,
    u8: u8,
    u16: u16,
    u32: u32,
    u64: u64,
    u128: u128,
    f32_bits: u32,
    f64_bits: u64,
    dec_bits: i128,
};

/// Identity for a captured value inside a compile-time function value. This is
/// the same canonical/generated CaptureId carried through every post-check IR;
/// a compile-time closure's captures are either canonical (a checked binder) or
/// generated (a compiler-synthesized capturable local, minted during CTFE).
pub const CaptureId = checked_ids.CaptureId;

/// Primitive type stored at the ConstStore boundary. This mirrors
/// `CheckedPrimitive` without importing `checked_artifact.zig`, which owns the
/// full checked artifact and itself imports this file.
pub const Primitive = enum {
    bool,
    str,
    u8,
    i8,
    u16,
    i16,
    u32,
    i32,
    u64,
    i64,
    u128,
    i128,
    f32,
    f64,
    dec,
};

/// Named type definition owner for stored monomorphic type evidence.
pub const TypeDef = struct {
    module_name: names.ModuleNameId,
    type_name: names.TypeNameId,
    source_decl: ?u32 = null,
};

/// How much of a stored named type's backing type later stages may inspect.
pub const TypeBackingUse = enum {
    inspectable,
    runtime_layout_only,
};

/// Backing type for a stored named type.
pub const TypeBacking = struct {
    ty: ConstTypeId,
    use: TypeBackingUse,
};

/// Kind of stored named type.
pub const TypeNamedKind = enum {
    nominal,
    @"opaque",
    alias,
};

/// Record field entry for stored monomorphic type evidence.
pub const TypeField = struct {
    name: names.RecordFieldNameId,
    ty: ConstTypeId,
};

/// Tag-union variant entry for stored monomorphic type evidence.
pub const TypeTag = struct {
    name: names.TagNameId,
    checked_name: names.TagNameId,
    payloads: ConstRange,
};

/// Declared field-order entry for stored nominal record type evidence.
pub const TypeDeclaredField = union(enum) {
    named: names.RecordFieldNameId,
    padding: ConstTypeId,
};

/// Monomorphic type evidence stored with compile-time function captures.
pub const ConstType = union(enum) {
    primitive: Primitive,
    named: struct {
        named_type: NamedType,
        def: TypeDef,
        kind: TypeNamedKind,
        builtin_owner: ?static_dispatch.BuiltinOwner = null,
        args: ConstRange,
        backing: ?TypeBacking = null,
        declared_order: ConstRange = .{},
    },
    record: ConstRange,
    tuple: ConstRange,
    tag_union: ConstRange,
    list: ConstTypeId,
    box: ConstTypeId,
    func: struct {
        args: ConstRange,
        ret: ConstTypeId,
    },
    erased: names.TypeDigest,
    zst,
};

/// Captured checked value inside a compile-time function value.
pub const ConstCapture = struct {
    id: CaptureId,
    ty: ConstTypeId,
    value: ConstNodeId,
};

/// Function value stored by compile-time evaluation.
pub const ConstFn = struct {
    fn_def: FnDef,
    source_fn_ty: checked_ids.CheckedTypeId,
    source_fn_key: names.TypeDigest,
    captures: []const ConstCapture = &.{},
};

/// Named type owner for a stored nominal constant.
pub const NamedType = struct {
    module: names.CheckedModuleDigest,
    ty: checked_ids.CheckedTypeId,
};

/// Checked function definition referenced by a stored function value.
pub const FnDef = union(enum) {
    local_template: names.ProcTemplate,
    imported_template: names.ProcTemplate,
    nested: struct {
        owner: names.ProcTemplate,
        site: names.ProcSiteId,
        context_fn_key: names.TypeDigest,
    },
    local_hosted: names.ProcTemplate,
    imported_hosted: names.ProcTemplate,
    checked_generated: names.ProcTemplate,
    parser_runtime: struct {
        owner: names.ProcTemplate,
        expr: checked_ids.CheckedExprId,
    },
    encode_to_runtime: struct {
        owner: names.ProcTemplate,
        expr: checked_ids.CheckedExprId,
    },
};

/// Stored string value.
///
/// `data` identifies the backing bytes. `offset` and `len` describe the string
/// view into those bytes. This lets checked constants keep the sharing needed
/// for readonly static slices while still storing only checked Roc values.
pub const ConstStr = struct {
    data: ConstStrDataId,
    offset: u32,
    len: u32,
};

/// Compile-time constant stored in checked module data.
pub const ConstValue = union(enum) {
    pending,
    zst,
    scalar: ConstScalar,
    str: ConstStr,
    list: []const ConstNodeId,
    box: ConstNodeId,
    tuple: []const ConstNodeId,
    record: []const ConstNodeId,
    crash: ConstStr,
    tag: struct {
        tag_name: []const u8,
        payloads: []const ConstNodeId,
    },
    nominal: struct {
        named_type: NamedType,
        backing: ConstNodeId,
    },
    fn_value: ConstFnId,
};

/// `(start, len)` range into one of `ConstStore`'s flat side pools (transform B).
pub const ConstRange = extern struct { start: u32 = 0, len: u32 = 0 };

/// Internal, relocation-invariant (POD) form of `ConstValue`: variant slices are
/// replaced by `ConstRange`s into the store's flat pools. The public `ConstValue`
/// (with slices) is reconstructed on demand by `get`.
const StoredValue = union(enum) {
    pending,
    zst,
    scalar: ConstScalar,
    str: ConstStr,
    list: ConstRange,
    box: ConstNodeId,
    tuple: ConstRange,
    record: ConstRange,
    crash: ConstStr,
    tag: struct { tag_name: ConstRange, payloads: ConstRange },
    nominal: struct { named_type: NamedType, backing: ConstNodeId },
    fn_value: ConstFnId,
};

/// POD form of `ConstFn`: captures slice → range into `capture_pool`.
const StoredFn = struct {
    fn_def: FnDef,
    source_fn_ty: checked_ids.CheckedTypeId,
    source_fn_key: names.TypeDigest,
    captures: ConstRange = .{},
};

/// Store of monomorphic type evidence attached to compile-time constants.
pub const ConstTypeStore = struct {
    allocator: Allocator,
    types: std.ArrayList(ConstType),
    /// Flat pool of `ConstTypeId`s for tuple/function args and tag payloads.
    type_pool: std.ArrayList(ConstTypeId),
    /// Flat pool of record fields.
    field_pool: std.ArrayList(TypeField),
    /// Flat pool of tag-union variants.
    tag_pool: std.ArrayList(TypeTag),
    /// Flat pool of nominal declared field order entries.
    declared_field_pool: std.ArrayList(TypeDeclaredField),
    /// True for a store reconstructed from a serialized buffer.
    serialized: bool = false,

    pub fn init(allocator: Allocator) ConstTypeStore {
        return .{
            .allocator = allocator,
            .types = .empty,
            .type_pool = .empty,
            .field_pool = .empty,
            .tag_pool = .empty,
            .declared_field_pool = .empty,
        };
    }

    pub fn reserve(self: *ConstTypeStore) Allocator.Error!ConstTypeId {
        const id: ConstTypeId = @enumFromInt(@as(u32, @intCast(self.types.items.len)));
        try self.types.append(self.allocator, .zst);
        return id;
    }

    pub fn fill(self: *ConstTypeStore, id: ConstTypeId, ty: ConstType) void {
        self.types.items[@intFromEnum(id)] = ty;
    }

    pub fn append(self: *ConstTypeStore, ty: ConstType) Allocator.Error!ConstTypeId {
        const id = try self.reserve();
        self.fill(id, ty);
        return id;
    }

    pub fn appendTypeSpan(self: *ConstTypeStore, ids: []const ConstTypeId) Allocator.Error!ConstRange {
        return artifact_serialize.appendSpan(ConstRange, ConstTypeId, &self.type_pool, self.allocator, ids);
    }

    pub fn appendFieldSpan(self: *ConstTypeStore, fields: []const TypeField) Allocator.Error!ConstRange {
        return artifact_serialize.appendSpan(ConstRange, TypeField, &self.field_pool, self.allocator, fields);
    }

    pub fn appendTagSpan(self: *ConstTypeStore, tags: []const TypeTag) Allocator.Error!ConstRange {
        return artifact_serialize.appendSpan(ConstRange, TypeTag, &self.tag_pool, self.allocator, tags);
    }

    pub fn appendDeclaredFieldSpan(self: *ConstTypeStore, fields: []const TypeDeclaredField) Allocator.Error!ConstRange {
        return artifact_serialize.appendSpan(ConstRange, TypeDeclaredField, &self.declared_field_pool, self.allocator, fields);
    }

    pub fn get(self: *const ConstTypeStore, id: ConstTypeId) ConstType {
        return self.types.items[@intFromEnum(id)];
    }

    pub fn typeSpan(self: *const ConstTypeStore, range: ConstRange) []const ConstTypeId {
        return self.type_pool.items[range.start .. range.start + range.len];
    }

    pub fn fieldSpan(self: *const ConstTypeStore, range: ConstRange) []const TypeField {
        return self.field_pool.items[range.start .. range.start + range.len];
    }

    pub fn tagSpan(self: *const ConstTypeStore, range: ConstRange) []const TypeTag {
        return self.tag_pool.items[range.start .. range.start + range.len];
    }

    pub fn declaredFieldSpan(self: *const ConstTypeStore, range: ConstRange) []const TypeDeclaredField {
        return self.declared_field_pool.items[range.start .. range.start + range.len];
    }

    pub fn cloneTypeFrom(self: *ConstTypeStore, source: *const ConstTypeStore, ty: ConstTypeId) Allocator.Error!ConstTypeId {
        var map = std.AutoHashMap(ConstTypeId, ConstTypeId).init(self.allocator);
        defer map.deinit();
        return try self.cloneTypeFromInner(source, null, ty, &map);
    }

    pub fn cloneTypeFromTranslated(
        self: *ConstTypeStore,
        source: *const ConstTypeStore,
        source_names: *const names.NameStore,
        target_names: *names.NameStore,
        ty: ConstTypeId,
    ) Allocator.Error!ConstTypeId {
        var map = std.AutoHashMap(ConstTypeId, ConstTypeId).init(self.allocator);
        defer map.deinit();
        return try self.cloneTypeFromInner(source, .{
            .source = source_names,
            .target = target_names,
        }, ty, &map);
    }

    const NameTranslation = struct {
        source: *const names.NameStore,
        target: *names.NameStore,
    };

    fn cloneTypeFromInner(
        self: *ConstTypeStore,
        source: *const ConstTypeStore,
        name_translation: ?NameTranslation,
        ty: ConstTypeId,
        map: *std.AutoHashMap(ConstTypeId, ConstTypeId),
    ) Allocator.Error!ConstTypeId {
        if (map.get(ty)) |existing| return existing;

        const out = try self.reserve();
        try map.put(ty, out);

        const cloned = switch (source.get(ty)) {
            .primitive => |primitive| ConstType{ .primitive = primitive },
            .zst => .zst,
            .erased => |erased| ConstType{ .erased = erased },
            .list => |elem| ConstType{ .list = try self.cloneTypeFromInner(source, name_translation, elem, map) },
            .box => |elem| ConstType{ .box = try self.cloneTypeFromInner(source, name_translation, elem, map) },
            .tuple => |span| blk: {
                const children = source.typeSpan(span);
                const cloned_children = try self.allocator.alloc(ConstTypeId, children.len);
                defer self.allocator.free(cloned_children);
                for (children, 0..) |child, i| cloned_children[i] = try self.cloneTypeFromInner(source, name_translation, child, map);
                break :blk ConstType{ .tuple = try self.appendTypeSpan(cloned_children) };
            },
            .func => |function| blk: {
                const args = source.typeSpan(function.args);
                const cloned_args = try self.allocator.alloc(ConstTypeId, args.len);
                defer self.allocator.free(cloned_args);
                for (args, 0..) |arg, i| cloned_args[i] = try self.cloneTypeFromInner(source, name_translation, arg, map);
                break :blk ConstType{ .func = .{
                    .args = try self.appendTypeSpan(cloned_args),
                    .ret = try self.cloneTypeFromInner(source, name_translation, function.ret, map),
                } };
            },
            .record => |span| blk: {
                const fields = source.fieldSpan(span);
                const cloned_fields = try self.allocator.alloc(TypeField, fields.len);
                defer self.allocator.free(cloned_fields);
                for (fields, 0..) |field, i| {
                    cloned_fields[i] = .{
                        .name = try translateRecordFieldName(name_translation, field.name),
                        .ty = try self.cloneTypeFromInner(source, name_translation, field.ty, map),
                    };
                }
                break :blk ConstType{ .record = try self.appendFieldSpan(cloned_fields) };
            },
            .tag_union => |span| blk: {
                const tags = source.tagSpan(span);
                const cloned_tags = try self.allocator.alloc(TypeTag, tags.len);
                defer self.allocator.free(cloned_tags);
                for (tags, 0..) |tag, i| {
                    const payloads = source.typeSpan(tag.payloads);
                    const cloned_payloads = try self.allocator.alloc(ConstTypeId, payloads.len);
                    defer self.allocator.free(cloned_payloads);
                    for (payloads, 0..) |payload, j| cloned_payloads[j] = try self.cloneTypeFromInner(source, name_translation, payload, map);
                    cloned_tags[i] = .{
                        .name = try translateTagName(name_translation, tag.name),
                        .checked_name = try translateTagName(name_translation, tag.checked_name),
                        .payloads = try self.appendTypeSpan(cloned_payloads),
                    };
                }
                break :blk ConstType{ .tag_union = try self.appendTagSpan(cloned_tags) };
            },
            .named => |named| blk: {
                const args = source.typeSpan(named.args);
                const cloned_args = try self.allocator.alloc(ConstTypeId, args.len);
                defer self.allocator.free(cloned_args);
                for (args, 0..) |arg, i| cloned_args[i] = try self.cloneTypeFromInner(source, name_translation, arg, map);

                const declared = source.declaredFieldSpan(named.declared_order);
                const cloned_declared = try self.allocator.alloc(TypeDeclaredField, declared.len);
                defer self.allocator.free(cloned_declared);
                for (declared, 0..) |entry, i| {
                    cloned_declared[i] = switch (entry) {
                        .named => |name| .{ .named = try translateRecordFieldName(name_translation, name) },
                        .padding => |padding| .{ .padding = try self.cloneTypeFromInner(source, name_translation, padding, map) },
                    };
                }

                break :blk ConstType{ .named = .{
                    .named_type = named.named_type,
                    .def = try translateTypeDef(name_translation, named.def),
                    .kind = named.kind,
                    .builtin_owner = named.builtin_owner,
                    .args = try self.appendTypeSpan(cloned_args),
                    .backing = if (named.backing) |backing| .{
                        .ty = try self.cloneTypeFromInner(source, name_translation, backing.ty, map),
                        .use = backing.use,
                    } else null,
                    .declared_order = try self.appendDeclaredFieldSpan(cloned_declared),
                } };
            },
        };
        self.fill(out, cloned);
        return out;
    }

    fn translateRecordFieldName(name_translation: ?NameTranslation, id: names.RecordFieldNameId) Allocator.Error!names.RecordFieldNameId {
        const translation = name_translation orelse return id;
        return translation.target.internRecordFieldLabel(translation.source.recordFieldLabelText(id));
    }

    fn translateTagName(name_translation: ?NameTranslation, id: names.TagNameId) Allocator.Error!names.TagNameId {
        const translation = name_translation orelse return id;
        return translation.target.internTagLabel(translation.source.tagLabelText(id));
    }

    fn translateTypeDef(name_translation: ?NameTranslation, def: TypeDef) Allocator.Error!TypeDef {
        const translation = name_translation orelse return def;
        return .{
            .module_name = try translation.target.internModuleName(translation.source.moduleNameText(def.module_name)),
            .type_name = try translation.target.internTypeName(translation.source.typeNameText(def.type_name)),
            .source_decl = def.source_decl,
        };
    }

    pub const Serialized = extern struct {
        types: artifact_serialize.SerializedSlice(ConstType) = .{},
        type_pool: artifact_serialize.SerializedSlice(ConstTypeId) = .{},
        field_pool: artifact_serialize.SerializedSlice(TypeField) = .{},
        tag_pool: artifact_serialize.SerializedSlice(TypeTag) = .{},
        declared_field_pool: artifact_serialize.SerializedSlice(TypeDeclaredField) = .{},

        comptime {
            std.debug.assert(artifact_serialize.relocatablePointerCount(Serialized) == 5);
        }

        const Serde = artifact_serialize.SliceStoreSerde(ConstTypeStore, @This());
        pub const serialize = Serde.serialize;
        pub const deserializeWithAllocator = Serde.deserializeWithAllocator;
        pub const deserialize = Serde.deserializeWithAllocator;
    };

    pub fn deinit(self: *ConstTypeStore) void {
        if (!self.serialized) {
            self.types.deinit(self.allocator);
            self.type_pool.deinit(self.allocator);
            self.field_pool.deinit(self.allocator);
            self.tag_pool.deinit(self.allocator);
            self.declared_field_pool.deinit(self.allocator);
        }
        self.* = ConstTypeStore.init(self.allocator);
    }
};

/// Store of compile-time constants completed by checking finalization.
pub const ConstStore = struct {
    const VisitState = enum { unseen, active, done };

    allocator: Allocator,
    values: std.ArrayList(StoredValue),
    fns: std.ArrayList(StoredFn),
    /// Flat pool of `ConstNodeId`s for list/tuple/record/tag-payload ranges.
    node_pool: std.ArrayList(ConstNodeId),
    /// Flat pool of tag-name bytes.
    tag_name_pool: std.ArrayList(u8),
    /// Monomorphic type evidence for function captures.
    type_store: ConstTypeStore,
    /// Flat pool of function captures.
    capture_pool: std.ArrayList(ConstCapture),
    /// Flat pool of all string backing bytes; `str_views` indexes into it.
    str_backing: std.ArrayList(u8),
    /// `ConstStrDataId` -> range into `str_backing`.
    str_views: std.ArrayList(ConstRange),
    /// True for a store reconstructed from a serialized buffer (pools point into
    /// buffer-owned memory and must not be freed).
    serialized: bool = false,

    pub fn init(allocator: Allocator) ConstStore {
        return .{
            .allocator = allocator,
            .values = .empty,
            .fns = .empty,
            .node_pool = .empty,
            .tag_name_pool = .empty,
            .type_store = ConstTypeStore.init(allocator),
            .capture_pool = .empty,
            .str_backing = .empty,
            .str_views = .empty,
        };
    }

    fn appendNodes(self: *ConstStore, nodes: []const ConstNodeId) Allocator.Error!ConstRange {
        return artifact_serialize.appendSpan(ConstRange, ConstNodeId, &self.node_pool, self.allocator, nodes);
    }

    pub fn reserve(self: *ConstStore) Allocator.Error!ConstNodeId {
        const id: ConstNodeId = @enumFromInt(@as(u32, @intCast(self.values.items.len)));
        try self.values.append(self.allocator, .pending);
        return id;
    }

    /// Store `value` at `id`. Any slices in `value` are copied into the store's
    /// pools; the caller retains ownership of the input slices and frees them.
    pub fn fill(self: *ConstStore, id: ConstNodeId, value: ConstValue) void {
        const slot = &self.values.items[@intFromEnum(id)];
        switch (slot.*) {
            .pending => {},
            else => constStoreInvariant("const node filled more than once"),
        }
        slot.* = self.storeValue(value) catch constStoreInvariant("out of memory storing const value");
    }

    fn storeValue(self: *ConstStore, value: ConstValue) Allocator.Error!StoredValue {
        return switch (value) {
            // `reserve` writes the `.pending` placeholder directly; `fill` always supplies
            // a concrete value, so filling a node *with* `.pending` is an invariant break.
            .pending => constStoreInvariant("cannot fill a const node with a pending value"),
            .zst => .zst,
            .scalar => |s| .{ .scalar = s },
            .str => |s| .{ .str = s },
            .crash => |s| .{ .crash = s },
            .box => |n| .{ .box = n },
            .nominal => |n| .{ .nominal = .{ .named_type = n.named_type, .backing = n.backing } },
            .fn_value => |f| .{ .fn_value = f },
            .list => |items| .{ .list = try self.appendNodes(items) },
            .tuple => |items| .{ .tuple = try self.appendNodes(items) },
            .record => |items| .{ .record = try self.appendNodes(items) },
            .tag => |tag| blk: {
                const name_range = try artifact_serialize.appendSpan(ConstRange, u8, &self.tag_name_pool, self.allocator, tag.tag_name);
                const payloads_range = try self.appendNodes(tag.payloads);
                break :blk .{ .tag = .{ .tag_name = name_range, .payloads = payloads_range } };
            },
        };
    }

    pub fn append(self: *ConstStore, value: ConstValue) Allocator.Error!ConstNodeId {
        const id = try self.reserve();
        self.fill(id, value);
        return id;
    }

    /// Store `fn_value`; its `captures` are copied into the pool. The caller
    /// retains ownership of the input `captures` slice and frees it.
    pub fn appendFn(self: *ConstStore, fn_value: ConstFn) Allocator.Error!ConstFnId {
        const id: ConstFnId = @enumFromInt(@as(u32, @intCast(self.fns.items.len)));
        const captures_range = try artifact_serialize.appendSpan(ConstRange, ConstCapture, &self.capture_pool, self.allocator, fn_value.captures);
        try self.fns.append(self.allocator, .{
            .fn_def = fn_value.fn_def,
            .source_fn_ty = fn_value.source_fn_ty,
            .source_fn_key = fn_value.source_fn_key,
            .captures = captures_range,
        });
        return id;
    }

    pub fn addStrData(self: *ConstStore, bytes: []const u8) Allocator.Error!ConstStrDataId {
        const id: ConstStrDataId = @enumFromInt(@as(u32, @intCast(self.str_views.items.len)));
        const view = try artifact_serialize.appendSpan(ConstRange, u8, &self.str_backing, self.allocator, bytes);
        try self.str_views.append(self.allocator, view);
        return id;
    }

    fn nodeSlice(self: *const ConstStore, range: ConstRange) []const ConstNodeId {
        return self.node_pool.items[range.start .. range.start + range.len];
    }

    pub fn get(self: *const ConstStore, id: ConstNodeId) ConstValue {
        return switch (self.values.items[@intFromEnum(id)]) {
            .pending => .pending,
            .zst => .zst,
            .scalar => |s| .{ .scalar = s },
            .str => |s| .{ .str = s },
            .crash => |s| .{ .crash = s },
            .box => |n| .{ .box = n },
            .nominal => |n| .{ .nominal = .{ .named_type = n.named_type, .backing = n.backing } },
            .fn_value => |f| .{ .fn_value = f },
            .list => |r| .{ .list = self.nodeSlice(r) },
            .tuple => |r| .{ .tuple = self.nodeSlice(r) },
            .record => |r| .{ .record = self.nodeSlice(r) },
            .tag => |tag| .{ .tag = .{
                .tag_name = self.tag_name_pool.items[tag.tag_name.start .. tag.tag_name.start + tag.tag_name.len],
                .payloads = self.nodeSlice(tag.payloads),
            } },
        };
    }

    pub fn getFn(self: *const ConstStore, id: ConstFnId) ConstFn {
        const stored = self.fns.items[@intFromEnum(id)];
        return .{
            .fn_def = stored.fn_def,
            .source_fn_ty = stored.source_fn_ty,
            .source_fn_key = stored.source_fn_key,
            .captures = self.capture_pool.items[stored.captures.start .. stored.captures.start + stored.captures.len],
        };
    }

    pub fn strData(self: *const ConstStore, id: ConstStrDataId) []const u8 {
        const index = @intFromEnum(id);
        if (@import("builtin").mode == .Debug and index >= self.str_views.items.len) {
            constStoreInvariant("string backing id is out of range");
        }
        const view = self.str_views.items[index];
        return self.str_backing.items[view.start .. view.start + view.len];
    }

    /// Relocatable serialized form. Every field is a `SafeList`-equivalent POD
    /// slice, so the store relocates with a fixed number of base-pointer fixups.
    pub const Serialized = extern struct {
        values: artifact_serialize.SerializedSlice(StoredValue) = .{},
        fns: artifact_serialize.SerializedSlice(StoredFn) = .{},
        node_pool: artifact_serialize.SerializedSlice(ConstNodeId) = .{},
        tag_name_pool: artifact_serialize.SerializedSlice(u8) = .{},
        type_store: ConstTypeStore.Serialized = .{},
        capture_pool: artifact_serialize.SerializedSlice(ConstCapture) = .{},
        str_backing: artifact_serialize.SerializedSlice(u8) = .{},
        str_views: artifact_serialize.SerializedSlice(ConstRange) = .{},

        comptime {
            // 7 value/function side lists + 5 nested type-store lists.
            std.debug.assert(artifact_serialize.relocatablePointerCount(Serialized) == 12);
        }

        const Serde = artifact_serialize.SliceStoreSerde(ConstStore, @This());
        pub const serialize = Serde.serialize;
        pub const deserializeWithAllocator = Serde.deserializeWithAllocator;
        pub const deserialize = Serde.deserializeWithAllocator;
    };

    pub fn strBytes(self: *const ConstStore, str: ConstStr) []const u8 {
        const backing = self.strData(str.data);
        const offset: usize = str.offset;
        const len: usize = str.len;
        if (@import("builtin").mode == .Debug and (offset > backing.len or len > backing.len - offset)) {
            constStoreInvariant("string view is outside backing data");
        }
        return backing[offset..][0..len];
    }

    pub fn verifyComplete(self: *const ConstStore) Allocator.Error!void {
        if (@import("builtin").mode != .Debug) return;
        for (self.values.items) |value| {
            switch (value) {
                .pending => std.debug.panic("const store invariant violated: completed store contains a pending node", .{}),
                else => {},
            }
        }
        const value_state = try self.allocator.alloc(VisitState, self.values.items.len);
        defer self.allocator.free(value_state);
        @memset(value_state, .unseen);

        const fn_state = try self.allocator.alloc(VisitState, self.fns.items.len);
        defer self.allocator.free(fn_state);
        @memset(fn_state, .unseen);

        for (self.values.items, 0..) |_, index| {
            self.verifyAcyclic(@enumFromInt(@as(u32, @intCast(index))), value_state, fn_state);
        }
        for (self.fns.items, 0..) |_, index| {
            self.verifyFnAcyclic(@enumFromInt(@as(u32, @intCast(index))), value_state, fn_state);
        }
    }

    pub fn deinit(self: *ConstStore) void {
        self.type_store.deinit();
        if (!self.serialized) {
            self.values.deinit(self.allocator);
            self.fns.deinit(self.allocator);
            self.node_pool.deinit(self.allocator);
            self.tag_name_pool.deinit(self.allocator);
            self.capture_pool.deinit(self.allocator);
            self.str_backing.deinit(self.allocator);
            self.str_views.deinit(self.allocator);
        }
        self.* = ConstStore.init(self.allocator);
    }

    fn verifyAcyclic(
        self: *const ConstStore,
        id: ConstNodeId,
        value_state: []VisitState,
        fn_state: []VisitState,
    ) void {
        const index = @intFromEnum(id);
        if (index >= self.values.items.len) constStoreInvariant("completed store contains an out-of-range value id");
        switch (value_state[index]) {
            .done => return,
            .active => constStoreInvariant("completed store contains a cycle in const node edges"),
            .unseen => {},
        }

        value_state[index] = .active;
        switch (self.get(id)) {
            .pending => constStoreInvariant("completed store contains a pending node"),
            .zst, .scalar => {},
            .str, .crash => |str| {
                _ = self.strBytes(str);
            },
            .fn_value => |fn_id| self.verifyFnAcyclic(fn_id, value_state, fn_state),
            .box => |child| self.verifyAcyclic(child, value_state, fn_state),
            .nominal => |nominal| self.verifyAcyclic(nominal.backing, value_state, fn_state),
            .list,
            .tuple,
            .record,
            => |children| {
                for (children) |child| self.verifyAcyclic(child, value_state, fn_state);
            },
            .tag => |tag| {
                for (tag.payloads) |payload| self.verifyAcyclic(payload, value_state, fn_state);
            },
        }
        value_state[index] = .done;
    }

    fn verifyFnAcyclic(
        self: *const ConstStore,
        id: ConstFnId,
        value_state: []VisitState,
        fn_state: []VisitState,
    ) void {
        const index = @intFromEnum(id);
        if (index >= self.fns.items.len) constStoreInvariant("completed store contains an out-of-range function id");
        switch (fn_state[index]) {
            .done => return,
            .active => constStoreInvariant("completed store contains a recursive function value"),
            .unseen => {},
        }

        fn_state[index] = .active;
        for (self.getFn(id).captures) |capture| {
            if (@intFromEnum(capture.ty) >= self.type_store.types.items.len) {
                constStoreInvariant("completed store contains an out-of-range capture type id");
            }
            self.verifyAcyclic(capture.value, value_state, fn_state);
        }
        fn_state[index] = .done;
    }
};

fn constStoreInvariant(comptime message: []const u8) noreturn {
    if (@import("builtin").mode == .Debug) {
        std.debug.panic("const store invariant violated: {s}", .{message});
    }
    unreachable;
}

test "const store declarations are referenced" {
    std.testing.refAllDecls(@This());
}

test "ConstStore: build, serialize/relocate, and read back values, fns, strings" {
    const gpa = std.testing.allocator;
    const CompactWriter = @import("collections").CompactWriter;

    var store = ConstStore.init(gpa);
    defer store.deinit();

    // Scalars + a list + a tag (exercises node_pool + tag_name_pool).
    const a = try store.append(.{ .scalar = .{ .u64 = 7 } });
    const b = try store.append(.{ .scalar = .{ .i32 = -3 } });
    // The store copies inputs into its pools and never frees them, so this test
    // owns and frees the slices it hands to `append`/`appendFn`.
    const list_items = try gpa.dupe(ConstNodeId, &.{ a, b });
    defer gpa.free(list_items);
    const list = try store.append(.{ .list = list_items });
    const tag_payloads = try gpa.dupe(ConstNodeId, &.{a});
    defer gpa.free(tag_payloads);
    const tag_name = try gpa.dupe(u8, "Ok");
    defer gpa.free(tag_name);
    const tag = try store.append(.{ .tag = .{ .tag_name = tag_name, .payloads = tag_payloads } });
    // A string backing + a str value (exercises str_backing + str_views).
    const sd = try store.addStrData("hello world");
    const str = try store.append(.{ .str = .{ .data = sd, .offset = 0, .len = 5 } });
    // A function value with a capture (exercises capture_pool).
    const capture_ty = try store.type_store.append(.{ .primitive = .u64 });
    const caps = try gpa.dupe(ConstCapture, &.{.{ .id = CaptureId.fromBinder(@enumFromInt(1)), .ty = capture_ty, .value = a }});
    defer gpa.free(caps);
    const fn_id = try store.appendFn(.{
        // Distinct non-zero ids: this test asserts captures round-trip; the fn_def
        // fields just need to survive, not be specific values.
        .fn_def = .{ .local_template = .{ .proc_base = @enumFromInt(1), .template = @enumFromInt(2) } },
        .source_fn_ty = @enumFromInt(3),
        .source_fn_key = .{},
        .captures = caps,
    });

    // Serialize → aligned buffer → deserialize.
    var arena = std.heap.ArenaAllocator.init(gpa);
    defer arena.deinit();
    const aa = arena.allocator();
    var writer = CompactWriter.init();
    const hdr = try writer.appendAlloc(aa, ConstStore.Serialized);
    try hdr.serialize(&store, aa, &writer);

    const buffer = try gpa.alignedAlloc(u8, std.mem.Alignment.@"16", writer.total_bytes);
    defer gpa.free(buffer);
    _ = try writer.writeToBuffer(buffer);

    const ser: *const ConstStore.Serialized = @ptrCast(@alignCast(buffer.ptr));
    var loaded = ser.deserialize(@intFromPtr(buffer.ptr), gpa);
    defer loaded.deinit();

    // Scalars
    try std.testing.expectEqual(@as(u64, 7), loaded.get(a).scalar.u64);
    try std.testing.expectEqual(@as(i32, -3), loaded.get(b).scalar.i32);
    // List range resolves to the same node ids
    try std.testing.expectEqualSlices(ConstNodeId, &.{ a, b }, loaded.get(list).list);
    // Tag name + payloads
    const loaded_tag = loaded.get(tag).tag;
    try std.testing.expectEqualStrings("Ok", loaded_tag.tag_name);
    try std.testing.expectEqualSlices(ConstNodeId, &.{a}, loaded_tag.payloads);
    // String backing
    try std.testing.expectEqualStrings("hello", loaded.strBytes(loaded.get(str).str));
    // Function captures
    const loaded_fn = loaded.getFn(fn_id);
    try std.testing.expectEqual(@as(usize, 1), loaded_fn.captures.len);
    try std.testing.expectEqual(capture_ty, loaded_fn.captures[0].ty);
    try std.testing.expectEqual(ConstType{ .primitive = .u64 }, loaded.type_store.get(loaded_fn.captures[0].ty));
    try std.testing.expectEqual(a, loaded_fn.captures[0].value);
}

test "ConstStore.appendFn: no leak or double-free under allocation failure" {
    // `appendFn` copies `captures` into the pool and does not free the input; the
    // caller owns it. Drive every allocation in that path to fail in turn and assert
    // no leak and no double-free (the testing allocator panics on a double-free, so
    // this would have caught the prior `defer free` + caller `errdefer free` overlap).
    const Helper = struct {
        fn run(allocator: Allocator) Allocator.Error!void {
            var store = ConstStore.init(allocator);
            defer store.deinit();
            const a = try store.append(.{ .scalar = .{ .u64 = 7 } });
            const capture_ty = try store.type_store.append(.{ .primitive = .u64 });
            const caps = try allocator.dupe(ConstCapture, &.{
                .{ .id = CaptureId.fromBinder(@enumFromInt(1)), .ty = capture_ty, .value = a },
                .{ .id = CaptureId.fromBinder(@enumFromInt(2)), .ty = capture_ty, .value = a },
            });
            defer allocator.free(caps);
            _ = try store.appendFn(.{
                .fn_def = .{ .local_template = .{ .proc_base = @enumFromInt(1), .template = @enumFromInt(2) } },
                .source_fn_ty = @enumFromInt(3),
                .source_fn_key = .{},
                .captures = caps,
            });
        }
    };
    try std.testing.checkAllAllocationFailures(std.testing.allocator, Helper.run, .{});
}
