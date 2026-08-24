//! Checked compile-time constant store.

const std = @import("std");
const collections = @import("collections");

const checked_ids = @import("checked_ids.zig");
const names = @import("canonical_names.zig");
const artifact_serialize = @import("artifact_serialize.zig");
const static_dispatch = @import("static_dispatch_registry.zig");

const Allocator = std.mem.Allocator;

/// Identifier for a node in the checked const store.
pub const ConstNodeId = enum(u32) { _ };
/// Identifier for a function value in the checked const store.
pub const ConstFnId = enum(u32) { _ };
/// Identifier for stored immutable backing bytes in the checked const store.
pub const ConstBlobDataId = enum(u32) { _ };
/// Identifier for a stored monomorphic type used by checked constants.
pub const ConstTypeId = enum(u32) { _ };

/// `(start, len)` range into one of `ConstStore`'s flat side pools (transform B).
pub const ConstRange = extern struct { start: u32 = 0, len: u32 = 0 };

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

/// Target-independent scalar encoding used by a packed constant list.
/// Multi-byte values use canonical little-endian bytes in `ConstStore`.
pub const ConstPackedScalar = enum(u8) {
    i8,
    i16,
    i32,
    i64,
    i128,
    u8,
    u16,
    u32,
    u64,
    u128,
    f32,
    f64,
    dec,
    u8x16,
    i8x16,
    u16x8,
    i16x8,
    u32x4,
    i32x4,
    u64x2,
    i64x2,

    pub fn byteWidth(self: ConstPackedScalar) u32 {
        return switch (self) {
            .i8, .u8 => 1,
            .i16, .u16 => 2,
            .i32, .u32, .f32 => 4,
            .i64, .u64, .f64 => 8,
            .i128,
            .u128,
            .dec,
            .u8x16,
            .i8x16,
            .u16x8,
            .i16x8,
            .u32x4,
            .i32x4,
            .u64x2,
            .i64x2,
            => 16,
        };
    }
};

/// Identity for a captured value inside a compile-time function value. This is
/// the same canonical/generated CaptureId carried through every post-check IR;
/// a compile-time closure's captures are either canonical (a checked binder) or
/// generated (a compiler-synthesized capturable local, minted during CTFE).
pub const CaptureId = checked_ids.CaptureId;

/// Primitive type stored at the ConstStore boundary. Aliased to the checked
/// artifact's `CheckedPrimitive` so the two cannot drift; Zig permits the
/// circular file import (`checked_artifact.zig` itself imports this file).
pub const Primitive = @import("checked_artifact.zig").CheckedPrimitive;

/// Checker-authored identities for the public iterator representation.
pub const IteratorTopology = struct {
    len_field: names.RecordFieldNameId,
    step_field: names.RecordFieldNameId,
    known_tag: names.TagNameId,
    unknown_tag: names.TagNameId,
    done_tag: names.TagNameId,
    one_tag: names.TagNameId,
    skip_tag: names.TagNameId,
    item_field: names.RecordFieldNameId,
    rest_field: names.RecordFieldNameId,
};

/// Named type definition owner for stored monomorphic type evidence.
pub const TypeDef = struct {
    /// Deep content identity of the declaring module (dense id in the owning
    /// name store's module identity table).
    module: names.ModuleIdentityId,
    /// Declared (module-relative) type name.
    type_name: names.TypeNameId,
    /// Declaring statement: within-module discriminator for same-named
    /// block-local declarations.
    source_decl: ?u32 = null,
    /// Compiler-generated specialization identity for internal nominals minted
    /// after checking while preserving the public declaration identity.
    generated: ?names.TypeDigest = null,
    iterator_representation: IteratorRepresentation = .none,
    iterator_kind: IteratorKind = .none,
    iterator_depth: u8 = 0,
    iterator_topology: ?IteratorTopology = null,
};

/// Target-independent Monotype iterator tier preserved across constant storage.
pub const IteratorRepresentation = enum(u8) {
    none,
    minted,
    forced_dynamic,
};

/// Producer or adapter that minted a stored iterator representation.
pub const IteratorKind = static_dispatch.IteratorKind;

/// How much of a stored named type's backing type later stages may inspect.
pub const TypeBackingUse = enum {
    inspectable,
    runtime_layout_only,
};

/// Authority retained for a stored named backing.
pub const TypeBackingAuthority = enum {
    checked_public,
    generated_private,
};

/// Backing type for a stored named type.
pub const TypeBacking = struct {
    ty: ConstTypeId,
    use: TypeBackingUse,
    authority: TypeBackingAuthority = .checked_public,
};

/// Kind of stored named type.
pub const TypeNamedKind = enum {
    nominal,
    @"opaque",
    alias,
};

/// `??` default identity carried on stored record type evidence, mirroring
/// Monotype `Type.FieldDefault`: rows disagreeing about defaults are
/// distinct monotypes, so restored evidence must reproduce the default to
/// reproduce the type.
pub const TypeFieldDefault = struct {
    module: names.ModuleIdentityId,
    expr_node: u32,
};

/// Record field entry for stored monomorphic type evidence.
pub const TypeField = struct {
    name: names.RecordFieldNameId,
    ty: ConstTypeId,
    value_ty: ?ConstTypeId = null,
    default: ?TypeFieldDefault,
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

/// Monomorphic type evidence stored for compile-time roots and function captures.
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

/// Checked capture identity, type, and stored value for a compile-time
/// function value.
pub const ConstCapture = struct {
    id: CaptureId,
    ty: ConstTypeId,
    value: ConstNodeId,
};

/// Durable source of a stored target's own evidence vector.
pub const ConstFnNestedEvidence = union(enum(u8)) {
    /// Flattened child-vector bounds for resolved nested evidence.
    resolved: struct {
        count: u32,
        subtree_len: u32,
    },
    /// Derive the target's declared requirements from the concrete callable
    /// supplied when this stored function is specialized.
    from_callable,
};

/// Exact checked callable relation attached to a stored target edge.
pub const ConstFnCallableInstantiation = struct {
    view: names.CheckedModuleDigest,
    /// Stable checked identity of `callable_ty`. The raw checked id remains
    /// replay payload and may differ between equivalent fresh instantiations.
    callable_key: names.CanonicalTypeKey,
    callable_ty: checked_ids.CheckedTypeId,
};

/// Dispatch evidence selected for a stored compile-time function value. Target
/// module identities make every checked id explicitly relative to its owning
/// checked module when the function is restored in another compilation.
pub const ConstFnEvidence = union(enum(u8)) {
    target: struct {
        view: names.CheckedModuleDigest,
        method: static_dispatch.MethodTarget,
        /// Stable checked identity of `method.callable_ty`.
        method_callable_key: names.CanonicalTypeKey,
        instantiation: ?ConstFnCallableInstantiation,
        nested: ConstFnNestedEvidence,
    },
    structural: static_dispatch.StructuralDerivation,
    unreachable_value,
    checked_error,
};

/// Stable checked dispatch scope identity stored without depending on checked
/// artifact implementation types.
pub const ConstFnEvidenceScope = union(enum(u8)) {
    root,
    generalized: u32,
};

/// One lexical evidence frame. Root indexes address the enclosing function's
/// flat evidence vector; parent indexes address its `evidence_frames` slice.
pub const ConstFnEvidenceFrame = struct {
    scope_id: ConstFnEvidenceScope,
    parent: ?u32,
    roots_start: u32,
    roots_len: u32,

    pub fn init(scope_id: ConstFnEvidenceScope, parent: ?u32, roots_start: u32, roots_len: u32) ConstFnEvidenceFrame {
        return .{ .scope_id = scope_id, .parent = parent, .roots_start = roots_start, .roots_len = roots_len };
    }

    pub fn scope(self: ConstFnEvidenceFrame) ConstFnEvidenceScope {
        return self.scope_id;
    }
};

/// Function value stored by compile-time evaluation.
pub const ConstFn = struct {
    fn_def: FnDef,
    source_fn_ty: checked_ids.CheckedTypeId,
    source_fn_key: names.TypeDigest,
    captures: []const ConstCapture = &.{},
    evidence: []const ConstFnEvidence = &.{},
    evidence_frames: []const ConstFnEvidenceFrame = &.{},
    evidence_frame_head: ?u32 = null,
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
        local_proc_context_digest: ?names.TypeDigest = null,
    },
    local_hosted: names.ProcTemplate,
    imported_hosted: names.ProcTemplate,
    checked_generated: names.ProcTemplate,
    parser_runtime: struct {
        owner: names.ProcTemplate,
        expr: checked_ids.CheckedExprId,
    },
    encoder_for_runtime: struct {
        owner: names.ProcTemplate,
        expr: checked_ids.CheckedExprId,
    },
};

/// A view into immutable bytes shared by strings and packed scalar lists.
pub const ConstBlob = struct {
    data: ConstBlobDataId,
    offset: u32,
    len: u32,
};

/// String view into immutable shared backing bytes.
pub const ConstStr = ConstBlob;

/// Packed scalar list whose bytes use the canonical encoding named by
/// `element`. `bytes.len == len * element.byteWidth()`.
pub const ConstPackedList = struct {
    bytes: ConstBlob,
    len: u32,
    element: ConstPackedScalar,
};

/// List data stored either as child nodes or packed scalar bytes.
pub const ConstList = union(enum) {
    nodes: []const ConstNodeId,
    scalar_bytes: ConstPackedList,
};

/// Compile-time constant stored in checked module data.
pub const ConstValue = union(enum) {
    pending,
    zst,
    scalar: ConstScalar,
    str: ConstStr,
    list: ConstList,
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

/// Internal, relocation-invariant (POD) form of `ConstValue`: variant slices are
/// replaced by `ConstRange`s into the store's flat pools. The public `ConstValue`
/// (with slices) is reconstructed on demand by `get`.
const StoredValue = union(enum) {
    pending,
    zst,
    scalar: ConstScalar,
    str: ConstStr,
    list: union(enum) {
        nodes: ConstRange,
        scalar_bytes: ConstPackedList,
    },
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
    evidence: ConstRange = .{},
    evidence_frames: ConstRange = .{},
    evidence_frame_head: ?u32 = null,
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
    /// Flat pool of nominal declared field entries.
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
        var map = collections.DenseMap(ConstTypeId, ConstTypeId).init(self.allocator);
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
        var map = collections.DenseMap(ConstTypeId, ConstTypeId).init(self.allocator);
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
        map: *collections.DenseMap(ConstTypeId, ConstTypeId),
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
                        .value_ty = if (field.value_ty) |value_ty|
                            try self.cloneTypeFromInner(source, name_translation, value_ty, map)
                        else
                            null,
                        .default = try translateFieldDefault(name_translation, field.default),
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
                        .authority = backing.authority,
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

    fn translateFieldDefault(name_translation: ?NameTranslation, default: ?TypeFieldDefault) Allocator.Error!?TypeFieldDefault {
        const field_default = default orelse return null;
        const translation = name_translation orelse return field_default;
        return .{
            .module = try translation.target.internModuleIdentity(translation.source.moduleIdentityBytes(field_default.module)),
            .expr_node = field_default.expr_node,
        };
    }

    fn translateTypeDef(name_translation: ?NameTranslation, def: TypeDef) Allocator.Error!TypeDef {
        const translation = name_translation orelse return def;
        return .{
            .module = try translation.target.internModuleIdentity(translation.source.moduleIdentityBytes(def.module)),
            .type_name = try translation.target.internTypeName(translation.source.typeNameText(def.type_name)),
            .source_decl = def.source_decl,
            .generated = def.generated,
            .iterator_representation = def.iterator_representation,
            .iterator_kind = def.iterator_kind,
            .iterator_depth = def.iterator_depth,
            .iterator_topology = if (def.iterator_topology) |topology| .{
                .len_field = try translateRecordFieldName(name_translation, topology.len_field),
                .step_field = try translateRecordFieldName(name_translation, topology.step_field),
                .known_tag = try translateTagName(name_translation, topology.known_tag),
                .unknown_tag = try translateTagName(name_translation, topology.unknown_tag),
                .done_tag = try translateTagName(name_translation, topology.done_tag),
                .one_tag = try translateTagName(name_translation, topology.one_tag),
                .skip_tag = try translateTagName(name_translation, topology.skip_tag),
                .item_field = try translateRecordFieldName(name_translation, topology.item_field),
                .rest_field = try translateRecordFieldName(name_translation, topology.rest_field),
            } else null,
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
    /// Monomorphic type evidence for roots and function captures.
    type_store: ConstTypeStore,
    /// Flat pool of function captures.
    capture_pool: std.ArrayList(ConstCapture),
    /// Flat evidence vectors referenced by stored functions.
    evidence_pool: std.ArrayList(ConstFnEvidence),
    evidence_frame_pool: std.ArrayList(ConstFnEvidenceFrame),
    /// Flat pool of immutable backing bytes shared by strings and packed lists.
    blob_backing: std.ArrayList(u8),
    /// `ConstBlobDataId` -> range into `blob_backing`.
    blob_views: std.ArrayList(ConstRange),
    /// Build-only content index. Keys own separate bytes because `blob_backing`
    /// may move while the store is being built.
    blob_index: std.StringHashMap(ConstBlobDataId),
    blob_index_keys: std.ArrayList([]u8) = .empty,
    /// True for a store reconstructed from a serialized buffer (pools point into
    /// buffer-owned memory and must not be freed).
    serialized: bool = false,

    pub const serde_transient_fields = .{ "blob_index", "blob_index_keys" };

    pub fn init(allocator: Allocator) ConstStore {
        return .{
            .allocator = allocator,
            .values = .empty,
            .fns = .empty,
            .node_pool = .empty,
            .tag_name_pool = .empty,
            .type_store = ConstTypeStore.init(allocator),
            .capture_pool = .empty,
            .evidence_pool = .empty,
            .evidence_frame_pool = .empty,
            .blob_backing = .empty,
            .blob_views = .empty,
            .blob_index = std.StringHashMap(ConstBlobDataId).init(allocator),
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
        if (slot.* != .pending) constStoreInvariant("const node filled more than once");
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
            .list => |list| .{ .list = switch (list) {
                .nodes => |items| .{ .nodes = try self.appendNodes(items) },
                .scalar_bytes => |scalar_bytes| .{ .scalar_bytes = scalar_bytes },
            } },
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
        validateEvidenceFrames(fn_value);
        const id: ConstFnId = @enumFromInt(@as(u32, @intCast(self.fns.items.len)));
        const captures_range = try artifact_serialize.appendSpan(ConstRange, ConstCapture, &self.capture_pool, self.allocator, fn_value.captures);
        const evidence_range = try artifact_serialize.appendSpan(ConstRange, ConstFnEvidence, &self.evidence_pool, self.allocator, fn_value.evidence);
        const evidence_frames = try artifact_serialize.appendSpan(ConstRange, ConstFnEvidenceFrame, &self.evidence_frame_pool, self.allocator, fn_value.evidence_frames);
        try self.fns.append(self.allocator, .{
            .fn_def = fn_value.fn_def,
            .source_fn_ty = fn_value.source_fn_ty,
            .source_fn_key = fn_value.source_fn_key,
            .captures = captures_range,
            .evidence = evidence_range,
            .evidence_frames = evidence_frames,
            .evidence_frame_head = fn_value.evidence_frame_head,
        });
        return id;
    }

    fn validateEvidenceFrames(fn_value: ConstFn) void {
        if (!evidenceFramesValid(fn_value)) {
            constStoreInvariant("stored function evidence frames were not one explicit lexical chain");
        }
    }

    fn evidenceFramesValid(fn_value: ConstFn) bool {
        if (fn_value.evidence_frames.len == 0) {
            return switch (fn_value.fn_def) {
                .parser_runtime, .encoder_for_runtime => fn_value.evidence_frame_head == null and fn_value.evidence.len == 0,
                .local_template,
                .imported_template,
                .nested,
                .local_hosted,
                .imported_hosted,
                .checked_generated,
                => false,
            };
        }
        const head = fn_value.evidence_frame_head orelse return false;
        if (head != fn_value.evidence_frames.len - 1) return false;

        var cursor: usize = 0;
        for (fn_value.evidence_frames, 0..) |frame, index| {
            if (index == 0) {
                if (frame.scope() != .root or frame.parent != null) return false;
            } else {
                switch (frame.scope()) {
                    .root => return false,
                    .generalized => {},
                }
                if (frame.parent == null or frame.parent.? != index - 1) return false;
            }
            if (frame.roots_start != cursor) return false;
            cursor = evidenceVectorEnd(fn_value.evidence, cursor, frame.roots_len) orelse return false;
        }
        return cursor == fn_value.evidence.len;
    }

    fn evidenceVectorEnd(nodes: []const ConstFnEvidence, start: usize, count: u32) ?usize {
        var cursor = start;
        for (0..count) |_| {
            if (cursor >= nodes.len) return null;
            const node = nodes[cursor];
            cursor += 1;
            switch (node) {
                .target => |target| {
                    switch (target.nested) {
                        .resolved => |nested| {
                            const nested_start = cursor;
                            cursor = evidenceVectorEnd(nodes, cursor, nested.count) orelse return null;
                            if (cursor - nested_start != nested.subtree_len) return null;
                        },
                        .from_callable => {},
                    }
                },
                .structural, .unreachable_value, .checked_error => {},
            }
        }
        return cursor;
    }

    test "callable-derived function evidence has no flattened child vector" {
        const evidence = [_]ConstFnEvidence{.{ .target = .{
            .view = .{},
            .method = undefined,
            .method_callable_key = .{},
            .instantiation = null,
            .nested = .from_callable,
        } }};
        try std.testing.expectEqual(@as(?usize, 1), evidenceVectorEnd(&evidence, 0, 1));
    }

    pub fn addBlobData(self: *ConstStore, bytes: []const u8) Allocator.Error!ConstBlobDataId {
        if (self.serialized) constStoreInvariant("cannot add blob data to a serialized const store");
        if (self.blob_index.get(bytes)) |existing| return existing;

        const id: ConstBlobDataId = @enumFromInt(@as(u32, @intCast(self.blob_views.items.len)));
        const view = try artifact_serialize.appendSpan(ConstRange, u8, &self.blob_backing, self.allocator, bytes);
        try self.blob_views.append(self.allocator, view);

        const key = try self.allocator.dupe(u8, bytes);
        errdefer self.allocator.free(key);
        try self.blob_index_keys.append(self.allocator, key);
        errdefer _ = self.blob_index_keys.pop();
        try self.blob_index.put(key, id);
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
            .list => |list| .{ .list = switch (list) {
                .nodes => |r| .{ .nodes = self.nodeSlice(r) },
                .scalar_bytes => |scalar_bytes| .{ .scalar_bytes = scalar_bytes },
            } },
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
            .evidence = self.evidence_pool.items[stored.evidence.start .. stored.evidence.start + stored.evidence.len],
            .evidence_frames = self.evidence_frame_pool.items[stored.evidence_frames.start .. stored.evidence_frames.start + stored.evidence_frames.len],
            .evidence_frame_head = stored.evidence_frame_head,
        };
    }

    pub fn blobData(self: *const ConstStore, id: ConstBlobDataId) []const u8 {
        const index = @intFromEnum(id);
        if (@import("builtin").mode == .Debug and index >= self.blob_views.items.len) {
            constStoreInvariant("blob backing id is out of range");
        }
        const view = self.blob_views.items[index];
        return self.blob_backing.items[view.start .. view.start + view.len];
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
        evidence_pool: artifact_serialize.SerializedSlice(ConstFnEvidence) = .{},
        evidence_frame_pool: artifact_serialize.SerializedSlice(ConstFnEvidenceFrame) = .{},
        blob_backing: artifact_serialize.SerializedSlice(u8) = .{},
        blob_views: artifact_serialize.SerializedSlice(ConstRange) = .{},

        comptime {
            // 9 value/function side lists + 5 nested type-store lists.
            std.debug.assert(artifact_serialize.relocatablePointerCount(Serialized) == 14);
        }

        const Serde = artifact_serialize.SliceStoreSerde(ConstStore, @This());
        pub const serialize = Serde.serialize;
        pub const deserializeWithAllocator = Serde.deserializeWithAllocator;
        pub const deserialize = Serde.deserializeWithAllocator;
    };

    pub fn strBytes(self: *const ConstStore, str: ConstStr) []const u8 {
        return self.blobBytes(str);
    }

    pub fn blobBytes(self: *const ConstStore, blob: ConstBlob) []const u8 {
        const backing = self.blobData(blob.data);
        const offset: usize = blob.offset;
        const len: usize = blob.len;
        if (@import("builtin").mode == .Debug and (offset > backing.len or len > backing.len - offset)) {
            constStoreInvariant("blob view is outside backing data");
        }
        return backing[offset..][0..len];
    }

    pub fn verifyComplete(self: *const ConstStore) Allocator.Error!void {
        if (@import("builtin").mode != .Debug) return;
        for (self.values.items) |value| {
            if (value == .pending) std.debug.panic("const store invariant violated: completed store contains a pending node", .{});
        }
        const value_state = try self.allocator.alloc(VisitState, self.values.items.len);
        defer self.allocator.free(value_state);
        @memset(value_state, .unseen);

        const fn_state = try self.allocator.alloc(VisitState, self.fns.items.len);
        defer self.allocator.free(fn_state);
        @memset(fn_state, .unseen);

        const value_delayed_depth = try self.allocator.alloc(usize, self.values.items.len);
        defer self.allocator.free(value_delayed_depth);
        @memset(value_delayed_depth, 0);

        const fn_delayed_depth = try self.allocator.alloc(usize, self.fns.items.len);
        defer self.allocator.free(fn_delayed_depth);
        @memset(fn_delayed_depth, 0);

        for (self.values.items, 0..) |_, index| {
            self.verifyGraph(
                @enumFromInt(@as(u32, @intCast(index))),
                value_state,
                fn_state,
                value_delayed_depth,
                fn_delayed_depth,
                0,
            );
        }
        for (self.fns.items, 0..) |_, index| {
            validateEvidenceFrames(self.getFn(@enumFromInt(@as(u32, @intCast(index)))));
            self.verifyFnGraph(
                @enumFromInt(@as(u32, @intCast(index))),
                value_state,
                fn_state,
                value_delayed_depth,
                fn_delayed_depth,
                0,
            );
        }
    }

    pub fn deinit(self: *ConstStore) void {
        self.type_store.deinit();
        for (self.blob_index_keys.items) |key| self.allocator.free(key);
        self.blob_index_keys.deinit(self.allocator);
        self.blob_index.deinit();
        if (!self.serialized) {
            self.values.deinit(self.allocator);
            self.fns.deinit(self.allocator);
            self.node_pool.deinit(self.allocator);
            self.tag_name_pool.deinit(self.allocator);
            self.capture_pool.deinit(self.allocator);
            self.evidence_pool.deinit(self.allocator);
            self.evidence_frame_pool.deinit(self.allocator);
            self.blob_backing.deinit(self.allocator);
            self.blob_views.deinit(self.allocator);
        }
        self.* = ConstStore.init(self.allocator);
    }

    fn verifyGraph(
        self: *const ConstStore,
        id: ConstNodeId,
        value_state: []VisitState,
        fn_state: []VisitState,
        value_delayed_depth: []usize,
        fn_delayed_depth: []usize,
        delayed_depth: usize,
    ) void {
        const index = @intFromEnum(id);
        if (index >= self.values.items.len) constStoreInvariant("completed store contains an out-of-range value id");
        switch (value_state[index]) {
            .done => return,
            .active => {
                if (delayed_depth > value_delayed_depth[index]) return;
                constStoreInvariant("completed store contains a cycle without a delayed function capture");
            },
            .unseen => {},
        }

        value_state[index] = .active;
        value_delayed_depth[index] = delayed_depth;
        switch (self.get(id)) {
            .pending => constStoreInvariant("completed store contains a pending node"),
            .zst, .scalar => {},
            .str, .crash => |str| {
                _ = self.strBytes(str);
            },
            .fn_value => |fn_id| self.verifyFnGraph(fn_id, value_state, fn_state, value_delayed_depth, fn_delayed_depth, delayed_depth),
            .box => |child| self.verifyGraph(child, value_state, fn_state, value_delayed_depth, fn_delayed_depth, delayed_depth),
            .nominal => |nominal| self.verifyGraph(nominal.backing, value_state, fn_state, value_delayed_depth, fn_delayed_depth, delayed_depth),
            .list => |list| switch (list) {
                .nodes => |children| for (children) |child| {
                    self.verifyGraph(child, value_state, fn_state, value_delayed_depth, fn_delayed_depth, delayed_depth);
                },
                .scalar_bytes => |scalar_bytes| {
                    const bytes = self.blobBytes(scalar_bytes.bytes);
                    const expected_len = @as(u64, scalar_bytes.len) * scalar_bytes.element.byteWidth();
                    if (bytes.len != expected_len) {
                        constStoreInvariant("packed list byte length differs from its element encoding");
                    }
                },
            },
            .tuple,
            .record,
            => |children| {
                for (children) |child| self.verifyGraph(child, value_state, fn_state, value_delayed_depth, fn_delayed_depth, delayed_depth);
            },
            .tag => |tag| {
                for (tag.payloads) |payload| self.verifyGraph(payload, value_state, fn_state, value_delayed_depth, fn_delayed_depth, delayed_depth);
            },
        }
        value_state[index] = .done;
    }

    fn verifyFnGraph(
        self: *const ConstStore,
        id: ConstFnId,
        value_state: []VisitState,
        fn_state: []VisitState,
        value_delayed_depth: []usize,
        fn_delayed_depth: []usize,
        delayed_depth: usize,
    ) void {
        const index = @intFromEnum(id);
        if (index >= self.fns.items.len) constStoreInvariant("completed store contains an out-of-range function id");
        switch (fn_state[index]) {
            .done => return,
            .active => {
                if (delayed_depth > fn_delayed_depth[index]) return;
                constStoreInvariant("completed store contains a function cycle without a delayed capture");
            },
            .unseen => {},
        }

        fn_state[index] = .active;
        fn_delayed_depth[index] = delayed_depth;
        for (self.getFn(id).captures) |capture| {
            if (@intFromEnum(capture.ty) >= self.type_store.types.items.len) {
                constStoreInvariant("completed store contains an out-of-range capture type id");
            }
            self.verifyGraph(capture.value, value_state, fn_state, value_delayed_depth, fn_delayed_depth, delayed_depth + 1);
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
    const list = try store.append(.{ .list = .{ .nodes = list_items } });
    const tag_payloads = try gpa.dupe(ConstNodeId, &.{a});
    defer gpa.free(tag_payloads);
    const tag_name = try gpa.dupe(u8, "Ok");
    defer gpa.free(tag_name);
    const tag = try store.append(.{ .tag = .{ .tag_name = tag_name, .payloads = tag_payloads } });
    // Strings and packed lists share one content-deduplicated blob backing.
    const sd = try store.addBlobData("hello world");
    try std.testing.expectEqual(sd, try store.addBlobData("hello world"));
    const str = try store.append(.{ .str = .{ .data = sd, .offset = 0, .len = 5 } });
    const packed_list = try store.append(.{ .list = .{ .scalar_bytes = .{
        .bytes = .{ .data = sd, .offset = 0, .len = 11 },
        .len = 11,
        .element = .u8,
    } } });
    // A function value with a capture (exercises capture_pool).
    const capture_ty = try store.type_store.append(.{ .primitive = .u64 });
    const private_backing_ty = try store.type_store.append(.{ .record = .{} });
    const private_named_ty = try store.type_store.append(.{ .named = .{
        .named_type = .{ .module = .{}, .ty = @enumFromInt(8) },
        .def = .{ .module = @enumFromInt(9), .type_name = @enumFromInt(10) },
        .kind = .@"opaque",
        .args = .{},
        .backing = .{
            .ty = private_backing_ty,
            .use = .runtime_layout_only,
            .authority = .generated_private,
        },
    } });
    const caps = try gpa.dupe(ConstCapture, &.{
        .{ .id = CaptureId.fromBinder(@enumFromInt(1)), .ty = capture_ty, .value = a },
        .{ .id = CaptureId.fromBinder(@enumFromInt(2)), .ty = capture_ty, .value = a },
    });
    defer gpa.free(caps);
    var target_view: names.CheckedModuleDigest = .{};
    target_view.bytes[0] = 0xA1;
    var instantiation_view: names.CheckedModuleDigest = .{};
    instantiation_view.bytes[0] = 0xB2;
    var method_callable_key: names.CanonicalTypeKey = .{};
    method_callable_key.bytes[0] = 0xC3;
    var instantiation_callable_key: names.CanonicalTypeKey = .{};
    instantiation_callable_key.bytes[0] = 0xD4;
    const evidence = [_]ConstFnEvidence{
        .{ .target = .{
            .view = target_view,
            .method = .{
                .module_idx = 4,
                .def_idx = @enumFromInt(5),
                .kind = .{ .local_proc = .{
                    .binder = @enumFromInt(8),
                    .expr = @enumFromInt(9),
                    .context_anchor = @enumFromInt(10),
                } },
                .callable_ty = @enumFromInt(6),
            },
            .method_callable_key = method_callable_key,
            .instantiation = .{
                .view = instantiation_view,
                .callable_key = instantiation_callable_key,
                .callable_ty = @enumFromInt(7),
            },
            .nested = .{ .resolved = .{ .count = 1, .subtree_len = 1 } },
        } },
        .{ .structural = .equality },
        .checked_error,
    };
    const evidence_frames = [_]ConstFnEvidenceFrame{
        ConstFnEvidenceFrame.init(.root, null, 0, 1),
        ConstFnEvidenceFrame.init(.{ .generalized = 9 }, 0, 2, 1),
    };
    const fn_id = try store.appendFn(.{
        // Distinct non-zero ids: this test asserts captures round-trip; the fn_def
        // fields just need to survive, not be specific values.
        .fn_def = .{ .local_template = .{ .proc_base = @enumFromInt(1), .template = @enumFromInt(2) } },
        .source_fn_ty = @enumFromInt(3),
        .source_fn_key = .{},
        .captures = caps,
        .evidence = &evidence,
        .evidence_frames = &evidence_frames,
        .evidence_frame_head = 1,
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
    try std.testing.expectEqualSlices(ConstNodeId, &.{ a, b }, loaded.get(list).list.nodes);
    // Tag name + payloads
    const loaded_tag = loaded.get(tag).tag;
    try std.testing.expectEqualStrings("Ok", loaded_tag.tag_name);
    try std.testing.expectEqualSlices(ConstNodeId, &.{a}, loaded_tag.payloads);
    // String backing
    try std.testing.expectEqualStrings("hello", loaded.strBytes(loaded.get(str).str));
    const loaded_packed = loaded.get(packed_list).list.scalar_bytes;
    try std.testing.expectEqual(sd, loaded_packed.bytes.data);
    try std.testing.expectEqualStrings("hello world", loaded.blobBytes(loaded_packed.bytes));
    // Function captures
    const loaded_fn = loaded.getFn(fn_id);
    try std.testing.expectEqual(@as(usize, 2), loaded_fn.captures.len);
    try std.testing.expectEqual(capture_ty, loaded_fn.captures[0].ty);
    try std.testing.expectEqual(ConstType{ .primitive = .u64 }, loaded.type_store.get(loaded_fn.captures[0].ty));
    try std.testing.expectEqual(TypeBackingAuthority.generated_private, loaded.type_store.get(private_named_ty).named.backing.?.authority);
    try std.testing.expectEqual(a, loaded_fn.captures[0].value);
    try std.testing.expectEqual(a, loaded_fn.captures[1].value);
    try loaded.verifyComplete();
    try std.testing.expectEqual(evidence.len, loaded_fn.evidence.len);
    const loaded_target = loaded_fn.evidence[0].target;
    try std.testing.expectEqualSlices(u8, &target_view.bytes, &loaded_target.view.bytes);
    try std.testing.expectEqual(@as(u32, 4), loaded_target.method.module_idx);
    try std.testing.expectEqual(@as(u32, 5), @intFromEnum(loaded_target.method.def_idx));
    try std.testing.expectEqual(evidence[0].target.method.kind, loaded_target.method.kind);
    try std.testing.expectEqual(@as(checked_ids.CheckedTypeId, @enumFromInt(6)), loaded_target.method.callable_ty);
    try std.testing.expectEqual(method_callable_key, loaded_target.method_callable_key);
    const loaded_instantiation = loaded_target.instantiation.?;
    try std.testing.expectEqualSlices(u8, &instantiation_view.bytes, &loaded_instantiation.view.bytes);
    try std.testing.expectEqual(instantiation_callable_key, loaded_instantiation.callable_key);
    try std.testing.expectEqual(@as(checked_ids.CheckedTypeId, @enumFromInt(7)), loaded_instantiation.callable_ty);
    const loaded_nested = loaded_target.nested.resolved;
    try std.testing.expectEqual(@as(u32, 1), loaded_nested.count);
    try std.testing.expectEqual(@as(u32, 1), loaded_nested.subtree_len);
    try std.testing.expectEqual(ConstFnEvidence{ .structural = .equality }, loaded_fn.evidence[1]);
    try std.testing.expectEqual(ConstFnEvidence.checked_error, loaded_fn.evidence[2]);
    try std.testing.expectEqualSlices(ConstFnEvidenceFrame, &evidence_frames, loaded_fn.evidence_frames);
    try std.testing.expectEqual(@as(?u32, 1), loaded_fn.evidence_frame_head);
    try std.testing.expectEqual(ConstFnEvidenceScope{ .generalized = 9 }, loaded_fn.evidence_frames[1].scope());

    const empty_frames = [_]ConstFnEvidenceFrame{
        ConstFnEvidenceFrame.init(.root, null, 0, 0),
        ConstFnEvidenceFrame.init(.{ .generalized = 3 }, 0, 0, 0),
        ConstFnEvidenceFrame.init(.{ .generalized = 4 }, 1, 0, 0),
    };
    var empty_chain = loaded_fn;
    empty_chain.evidence = &.{};
    empty_chain.evidence_frames = &empty_frames;
    empty_chain.evidence_frame_head = 2;
    try std.testing.expect(ConstStore.evidenceFramesValid(empty_chain));

    var absent_chain = loaded_fn;
    absent_chain.evidence = &.{};
    absent_chain.evidence_frames = &.{};
    absent_chain.evidence_frame_head = null;
    try std.testing.expect(!ConstStore.evidenceFramesValid(absent_chain));

    absent_chain.fn_def = .{ .parser_runtime = .{
        .owner = .{ .proc_base = @enumFromInt(1), .template = @enumFromInt(2) },
        .expr = @enumFromInt(11),
    } };
    try std.testing.expect(ConstStore.evidenceFramesValid(absent_chain));

    var corrupt_head = loaded_fn;
    corrupt_head.evidence_frame_head = 0;
    try std.testing.expect(!ConstStore.evidenceFramesValid(corrupt_head));

    var corrupt_parent_frames = evidence_frames;
    corrupt_parent_frames[1].parent = 1;
    var corrupt_parent = loaded_fn;
    corrupt_parent.evidence_frames = &corrupt_parent_frames;
    try std.testing.expect(!ConstStore.evidenceFramesValid(corrupt_parent));

    var corrupt_range_frames = evidence_frames;
    corrupt_range_frames[1].roots_start = 99;
    var corrupt_range = loaded_fn;
    corrupt_range.evidence_frames = &corrupt_range_frames;
    try std.testing.expect(!ConstStore.evidenceFramesValid(corrupt_range));
}

test "ConstStore: exact function capture back-edge survives serialization" {
    const gpa = std.testing.allocator;
    const CompactWriter = @import("collections").CompactWriter;

    var store = ConstStore.init(gpa);
    defer store.deinit();

    const unit_ty = try store.type_store.append(.zst);
    const fn_ty = try store.type_store.append(.{ .func = .{ .args = .{}, .ret = unit_ty } });
    const fn_node = try store.reserve();
    const captures = [_]ConstCapture{.{
        .id = CaptureId.fromBinder(@enumFromInt(1)),
        .ty = fn_ty,
        .value = fn_node,
    }};
    const evidence_frames = [_]ConstFnEvidenceFrame{
        ConstFnEvidenceFrame.init(.root, null, 0, 0),
    };
    const fn_id = try store.appendFn(.{
        .fn_def = .{ .local_template = .{ .proc_base = @enumFromInt(1), .template = @enumFromInt(2) } },
        .source_fn_ty = @enumFromInt(3),
        .source_fn_key = .{},
        .captures = &captures,
        .evidence_frames = &evidence_frames,
        .evidence_frame_head = 0,
    });
    store.fill(fn_node, .{ .fn_value = fn_id });
    try store.verifyComplete();

    var arena = std.heap.ArenaAllocator.init(gpa);
    defer arena.deinit();
    var writer = CompactWriter.init();
    const header = try writer.appendAlloc(arena.allocator(), ConstStore.Serialized);
    try header.serialize(&store, arena.allocator(), &writer);

    const buffer = try gpa.alignedAlloc(u8, std.mem.Alignment.@"16", writer.total_bytes);
    defer gpa.free(buffer);
    _ = try writer.writeToBuffer(buffer);

    const serialized: *const ConstStore.Serialized = @ptrCast(@alignCast(buffer.ptr));
    var loaded = serialized.deserialize(@intFromPtr(buffer.ptr), gpa);
    defer loaded.deinit();

    const loaded_fn_id = loaded.get(fn_node).fn_value;
    try std.testing.expectEqual(fn_id, loaded_fn_id);
    try std.testing.expectEqual(fn_node, loaded.getFn(loaded_fn_id).captures[0].value);
    try loaded.verifyComplete();
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
            const evidence_frames = [_]ConstFnEvidenceFrame{
                ConstFnEvidenceFrame.init(.root, null, 0, 0),
            };
            _ = try store.appendFn(.{
                .fn_def = .{ .local_template = .{ .proc_base = @enumFromInt(1), .template = @enumFromInt(2) } },
                .source_fn_ty = @enumFromInt(3),
                .source_fn_key = .{},
                .captures = caps,
                .evidence_frames = &evidence_frames,
                .evidence_frame_head = 0,
            });
        }
    };
    try std.testing.checkAllAllocationFailures(std.testing.allocator, Helper.run, .{});
}
