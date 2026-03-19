//! Monomorphic type system for MIR.
//!
//! Types are fully concrete — no type variables, extensions, aliases, or
//! nominal/opaque/structural distinction. The TypeInterner provides canonical
//! type identity: structurally equal types always share the same TypeId.

const std = @import("std");
const base = @import("base");
const can = @import("can");
const types = @import("types");

const Ident = base.Ident;
const Allocator = std.mem.Allocator;
const ModuleEnv = can.ModuleEnv;
const CommonIdents = can.ModuleEnv.CommonIdents;
const StaticDispatchConstraint = types.StaticDispatchConstraint;

// ═══════════════════════════════════════════════════════════════
// Type Handles
// ═══════════════════════════════════════════════════════════════

/// Canonical type handle — equality IS semantic type equality.
/// 32-bit packed: 29-bit per-kind index + 3-bit kind tag.
pub const TypeId = packed struct(u32) {
    index: u29,
    kind: Kind,

    pub const Kind = enum(u3) {
        builtin, // unit + all Prim variants (encoded in index)
        list, // payload: TypeId (elem)
        box, // payload: TypeId (inner)
        tuple, // payload: IdxListId
        record, // payload: FieldListId
        tag_union, // payload: TagListId
        func, // payload: FuncPayload
        rec, // recursive type indirection
    };

    pub const none: TypeId = .{ .index = std.math.maxInt(u29), .kind = .builtin };

    pub fn isNone(self: TypeId) bool {
        return @as(u32, @bitCast(self)) == @as(u32, @bitCast(none));
    }

    pub fn eql(a: TypeId, b: TypeId) bool {
        return @as(u32, @bitCast(a)) == @as(u32, @bitCast(b));
    }

    /// True if this TypeId represents the unit type (builtin index 0).
    pub fn isUnit(self: TypeId) bool {
        return self.kind == .builtin and self.index == 0;
    }

    /// If this TypeId is a primitive, return which one.
    pub fn builtinPrim(self: TypeId) ?Prim {
        if (self.kind != .builtin or self.index == 0) return null;
        const prim_count = @typeInfo(Prim).@"enum".fields.len;
        if (self.index > prim_count) return null;
        return @enumFromInt(self.index - 1);
    }
};

/// Backward-compat alias so existing `Monotype.Idx` references compile.
pub const Idx = TypeId;

/// Canonical list of TypeIds (function args, tuple elems, tag payloads).
pub const IdxListId = enum(u32) {
    empty = 0,
    _,

    pub fn isEmpty(self: IdxListId) bool {
        return self == .empty;
    }
};

/// Canonical list of record fields.
pub const FieldListId = enum(u32) {
    empty = 0,
    _,

    pub fn isEmpty(self: FieldListId) bool {
        return self == .empty;
    }
};

/// Canonical list of tag-union tags.
pub const TagListId = enum(u32) {
    empty = 0,
    _,

    pub fn isEmpty(self: TagListId) bool {
        return self == .empty;
    }
};

/// Canonical structural name (record field names, tag names).
pub const StructuralNameId = enum(u32) { _ };

// ═══════════════════════════════════════════════════════════════
// Payload / Key Types
// ═══════════════════════════════════════════════════════════════

/// Primitive type kinds.
pub const Prim = enum {
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

/// A record field key: canonical name + canonical type.
pub const FieldKey = struct {
    name: StructuralNameId,
    ty: TypeId,

    pub fn sortByNameAsc(pool: *const StructuralNamePool, a: FieldKey, b: FieldKey) bool {
        return std.mem.order(u8, pool.getText(a.name), pool.getText(b.name)) == .lt;
    }
};

/// A tag-union tag key: canonical name + canonical payload list.
pub const TagKey = struct {
    name: StructuralNameId,
    payloads: IdxListId,

    pub fn sortByNameAsc(pool: *const StructuralNamePool, a: TagKey, b: TagKey) bool {
        return std.mem.order(u8, pool.getText(a.name), pool.getText(b.name)) == .lt;
    }
};

/// Function type payload: canonical arg list + return type + effectfulness.
pub const FuncPayload = struct {
    args: IdxListId,
    ret: TypeId,
    effectful: bool,
};

// ═══════════════════════════════════════════════════════════════
// Structural Name Pool
// ═══════════════════════════════════════════════════════════════

/// Compilation-wide pool for canonical structural field/tag names.
pub const StructuralNamePool = struct {
    alloc: Allocator,
    map: std.StringHashMapUnmanaged(StructuralNameId),
    texts: std.ArrayListUnmanaged([]const u8),

    fn init(allocator: Allocator) StructuralNamePool {
        return .{ .alloc = allocator, .map = .{}, .texts = .empty };
    }

    fn deinit(self: *StructuralNamePool) void {
        for (self.texts.items) |text| self.alloc.free(text);
        self.texts.deinit(self.alloc);
        self.map.deinit(self.alloc);
    }

    pub fn intern(self: *StructuralNamePool, text: []const u8) !StructuralNameId {
        const gop = try self.map.getOrPut(self.alloc, text);
        if (gop.found_existing) return gop.value_ptr.*;
        const owned = try self.alloc.dupe(u8, text);
        const id: StructuralNameId = @enumFromInt(self.texts.items.len);
        try self.texts.append(self.alloc, owned);
        gop.key_ptr.* = owned;
        gop.value_ptr.* = id;
        return id;
    }

    pub fn getText(self: *const StructuralNamePool, id: StructuralNameId) []const u8 {
        return self.texts.items[@intFromEnum(id)];
    }
};

/// Key identifying a nominal type instance by its origin, type name, and type arguments.
pub const NominalInstanceKey = struct {
    origin_name_id: StructuralNameId,
    type_name_id: StructuralNameId,
    args: IdxListId,
};

const NominalCacheEntry = union(enum) {
    /// Lowering is in progress. rec_id is allocated lazily on recursive hit.
    in_progress: TypeId, // TypeId.none until recursion detected
    /// Fully lowered canonical type.
    resolved: TypeId,
};

// ═══════════════════════════════════════════════════════════════
// Internal helpers
// ═══════════════════════════════════════════════════════════════

fn hasNumeralConstraint(types_store: *const types.Store, constraints: StaticDispatchConstraint.SafeList.Range) bool {
    if (constraints.isEmpty()) return false;
    for (types_store.sliceStaticDispatchConstraints(constraints)) |constraint| {
        switch (constraint.origin) {
            .from_numeral, .desugared_binop, .desugared_unaryop => return true,
            .method_call, .where_clause => {},
        }
    }
    return false;
}

/// Hash a slice of TypeIds for list interning.
fn hashTypeIds(items: []const TypeId) u64 {
    var h = std.hash.Wyhash.init(0);
    for (items) |item| std.hash.autoHash(&h, @as(u32, @bitCast(item)));
    std.hash.autoHash(&h, items.len);
    return h.final();
}

/// Hash a slice of FieldKeys for field-list interning.
fn hashFieldKeys(items: []const FieldKey) u64 {
    var h = std.hash.Wyhash.init(1);
    for (items) |item| {
        std.hash.autoHash(&h, @intFromEnum(item.name));
        std.hash.autoHash(&h, @as(u32, @bitCast(item.ty)));
    }
    std.hash.autoHash(&h, items.len);
    return h.final();
}

/// Hash a slice of TagKeys for tag-list interning.
fn hashTagKeys(items: []const TagKey) u64 {
    var h = std.hash.Wyhash.init(2);
    for (items) |item| {
        std.hash.autoHash(&h, @intFromEnum(item.name));
        std.hash.autoHash(&h, @intFromEnum(item.payloads));
    }
    std.hash.autoHash(&h, items.len);
    return h.final();
}

/// Compare two TypeId slices for equality.
fn eqlTypeIds(a: []const TypeId, b: []const TypeId) bool {
    if (a.len != b.len) return false;
    return std.mem.eql(u8, std.mem.sliceAsBytes(a), std.mem.sliceAsBytes(b));
}

/// Compare two FieldKey slices for equality.
fn eqlFieldKeys(a: []const FieldKey, b: []const FieldKey) bool {
    if (a.len != b.len) return false;
    return std.mem.eql(u8, std.mem.sliceAsBytes(a), std.mem.sliceAsBytes(b));
}

/// Compare two TagKey slices for equality.
fn eqlTagKeys(a: []const TagKey, b: []const TagKey) bool {
    if (a.len != b.len) return false;
    return std.mem.eql(u8, std.mem.sliceAsBytes(a), std.mem.sliceAsBytes(b));
}

const NamedSpecialization = struct {
    name_text: []const u8,
    type_idx: TypeId,
};

// ═══════════════════════════════════════════════════════════════
// TypeInterner
// ═══════════════════════════════════════════════════════════════

/// Compilation-wide canonical type interner. Every structurally equal
/// monotype receives exactly one TypeId.
pub const TypeInterner = struct {
    allocator: Allocator,

    // Per-kind payload arenas (indexed by TypeId.index for that kind)
    list_payloads: std.ArrayListUnmanaged(TypeId), // elem type
    box_payloads: std.ArrayListUnmanaged(TypeId), // inner type
    tuple_payloads: std.ArrayListUnmanaged(IdxListId),
    record_payloads: std.ArrayListUnmanaged(FieldListId),
    union_payloads: std.ArrayListUnmanaged(TagListId),
    func_payloads: std.ArrayListUnmanaged(FuncPayload),
    rec_targets: std.ArrayListUnmanaged(TypeId), // recursive indirection

    // Canonical list storage
    idx_list_metas: std.ArrayListUnmanaged(IdxListMeta),
    idx_list_items: std.ArrayListUnmanaged(TypeId),
    field_list_metas: std.ArrayListUnmanaged(FieldListMeta),
    field_list_items: std.ArrayListUnmanaged(FieldKey),
    tag_list_metas: std.ArrayListUnmanaged(TagListMeta),
    tag_list_items: std.ArrayListUnmanaged(TagKey),

    // Per-kind intern maps (canonical key → TypeId)
    list_map: std.AutoHashMapUnmanaged(u32, TypeId), // @bitCast(elem TypeId)
    box_map: std.AutoHashMapUnmanaged(u32, TypeId), // @bitCast(inner TypeId)
    tuple_map: std.AutoHashMapUnmanaged(IdxListId, TypeId),
    record_map: std.AutoHashMapUnmanaged(FieldListId, TypeId),
    union_map: std.AutoHashMapUnmanaged(TagListId, TypeId),
    func_map: std.AutoHashMapUnmanaged(u96, TypeId), // packed func key

    // Structural name pool
    name_pool: StructuralNamePool,

    // Nominal instance cache
    nominal_cache: std.AutoHashMapUnmanaged(NominalInstanceKey, NominalCacheEntry),

    // Pre-interned builtins
    unit_idx: TypeId,
    prim_idxs: [prim_count]TypeId,
    bool_tag_union_idx: TypeId,

    const prim_count = @typeInfo(Prim).@"enum".fields.len;

    // --- Internal list metadata ---

    const IdxListMeta = struct { start: u32, len: u32, fingerprint: u64 };
    const FieldListMeta = struct { start: u32, len: u32, fingerprint: u64 };
    const TagListMeta = struct { start: u32, len: u32, fingerprint: u64 };

    // --- Scratches (reusable temporary buffers) ---

    pub const Scratches = struct {
        fields: base.Scratch(FieldKey),
        tags: base.Scratch(TagKey),
        idxs: base.Scratch(TypeId),
        named_specializations: base.Scratch(NamedSpecialization),
        /// Ident store for resolving tag/field name text.
        /// Updated when switching modules during cross-module lowering.
        ident_store: ?*const Ident.Store = null,
        /// Module env owning the current types_store / ident_store.
        module_env: ?*const ModuleEnv = null,
        /// Shared module env slice for resolving nominal definitions.
        all_module_envs: ?[]const *ModuleEnv = null,

        pub fn init(allocator: Allocator) Allocator.Error!Scratches {
            return .{
                .fields = try base.Scratch(FieldKey).init(allocator),
                .tags = try base.Scratch(TagKey).init(allocator),
                .idxs = try base.Scratch(TypeId).init(allocator),
                .named_specializations = try base.Scratch(NamedSpecialization).init(allocator),
            };
        }

        pub fn deinit(self: *Scratches) void {
            self.fields.deinit();
            self.tags.deinit();
            self.idxs.deinit();
            self.named_specializations.deinit();
        }
    };

    pub fn init(allocator: Allocator) Allocator.Error!TypeInterner {
        var self = TypeInterner{
            .allocator = allocator,
            .list_payloads = .empty,
            .box_payloads = .empty,
            .tuple_payloads = .empty,
            .record_payloads = .empty,
            .union_payloads = .empty,
            .func_payloads = .empty,
            .rec_targets = .empty,
            .idx_list_metas = .empty,
            .idx_list_items = .empty,
            .field_list_metas = .empty,
            .field_list_items = .empty,
            .tag_list_metas = .empty,
            .tag_list_items = .empty,
            .list_map = .{},
            .box_map = .{},
            .tuple_map = .{},
            .record_map = .{},
            .union_map = .{},
            .func_map = .{},
            .name_pool = StructuralNamePool.init(allocator),
            .nominal_cache = .{},
            .unit_idx = undefined,
            .prim_idxs = undefined,
            .bool_tag_union_idx = TypeId.none,
        };

        // Pre-intern unit (builtin index 0) and prims (builtin indices 1..14).
        self.unit_idx = TypeId{ .kind = .builtin, .index = 0 };
        for (0..prim_count) |i| {
            self.prim_idxs[i] = TypeId{ .kind = .builtin, .index = @intCast(i + 1) };
        }

        return self;
    }

    pub fn deinit(self: *TypeInterner, allocator: Allocator) void {
        _ = allocator; // TypeInterner uses self.allocator
        self.list_payloads.deinit(self.allocator);
        self.box_payloads.deinit(self.allocator);
        self.tuple_payloads.deinit(self.allocator);
        self.record_payloads.deinit(self.allocator);
        self.union_payloads.deinit(self.allocator);
        self.func_payloads.deinit(self.allocator);
        self.rec_targets.deinit(self.allocator);
        self.idx_list_metas.deinit(self.allocator);
        self.idx_list_items.deinit(self.allocator);
        self.field_list_metas.deinit(self.allocator);
        self.field_list_items.deinit(self.allocator);
        self.tag_list_metas.deinit(self.allocator);
        self.tag_list_items.deinit(self.allocator);
        self.list_map.deinit(self.allocator);
        self.box_map.deinit(self.allocator);
        self.tuple_map.deinit(self.allocator);
        self.record_map.deinit(self.allocator);
        self.union_map.deinit(self.allocator);
        self.func_map.deinit(self.allocator);
        self.name_pool.deinit();
        self.nominal_cache.deinit(self.allocator);
    }

    /// Look up the pre-interned TypeId for a primitive type.
    pub fn primIdx(self: *const TypeInterner, prim: Prim) TypeId {
        return self.prim_idxs[@intFromEnum(prim)];
    }

    /// Get or create the Bool tag union ([False, True]).
    pub fn addBoolTagUnion(self: *TypeInterner) !TypeId {
        if (!self.bool_tag_union_idx.isNone()) return self.bool_tag_union_idx;
        const false_name = try self.name_pool.intern("False");
        const true_name = try self.name_pool.intern("True");
        const result = try self.internTagUnion(&.{
            .{ .name = false_name, .payloads = .empty },
            .{ .name = true_name, .payloads = .empty },
        });
        self.bool_tag_union_idx = result;
        return result;
    }

    pub fn internList(self: *TypeInterner, elem: TypeId) !TypeId {
        const key: u32 = @bitCast(elem);
        if (self.list_map.get(key)) |existing| return existing;
        const index: u29 = @intCast(self.list_payloads.items.len);
        try self.list_payloads.append(self.allocator, elem);
        const id = TypeId{ .kind = .list, .index = index };
        try self.list_map.put(self.allocator, key, id);
        return id;
    }

    pub fn internBox(self: *TypeInterner, inner: TypeId) !TypeId {
        const key: u32 = @bitCast(inner);
        if (self.box_map.get(key)) |existing| return existing;
        const index: u29 = @intCast(self.box_payloads.items.len);
        try self.box_payloads.append(self.allocator, inner);
        const id = TypeId{ .kind = .box, .index = index };
        try self.box_map.put(self.allocator, key, id);
        return id;
    }

    pub fn internTuple(self: *TypeInterner, elems: []const TypeId) !TypeId {
        const elem_list = try self.internIdxList(elems);
        if (self.tuple_map.get(elem_list)) |existing| return existing;
        const index: u29 = @intCast(self.tuple_payloads.items.len);
        try self.tuple_payloads.append(self.allocator, elem_list);
        const id = TypeId{ .kind = .tuple, .index = index };
        try self.tuple_map.put(self.allocator, elem_list, id);
        return id;
    }

    pub fn internFunc(self: *TypeInterner, args: []const TypeId, ret: TypeId, effectful: bool) !TypeId {
        const arg_list = try self.internIdxList(args);
        const key = packFuncKey(arg_list, ret, effectful);
        if (self.func_map.get(key)) |existing| return existing;
        const index: u29 = @intCast(self.func_payloads.items.len);
        try self.func_payloads.append(self.allocator, .{
            .args = arg_list,
            .ret = ret,
            .effectful = effectful,
        });
        const id = TypeId{ .kind = .func, .index = index };
        try self.func_map.put(self.allocator, key, id);
        return id;
    }

    pub fn internRecord(self: *TypeInterner, fields: []const FieldKey) !TypeId {
        const field_list = try self.internFieldList(fields);
        if (self.record_map.get(field_list)) |existing| return existing;
        const index: u29 = @intCast(self.record_payloads.items.len);
        try self.record_payloads.append(self.allocator, field_list);
        const id = TypeId{ .kind = .record, .index = index };
        try self.record_map.put(self.allocator, field_list, id);
        return id;
    }

    pub fn internTagUnion(self: *TypeInterner, tag_keys: []const TagKey) !TypeId {
        const tag_list = try self.internTagList(tag_keys);
        if (self.union_map.get(tag_list)) |existing| return existing;
        const index: u29 = @intCast(self.union_payloads.items.len);
        try self.union_payloads.append(self.allocator, tag_list);
        const id = TypeId{ .kind = .tag_union, .index = index };
        try self.union_map.put(self.allocator, tag_list, id);
        return id;
    }

    fn packFuncKey(args: IdxListId, ret: TypeId, effectful: bool) u96 {
        return @as(u96, @intFromEnum(args)) |
            (@as(u96, @as(u32, @bitCast(ret))) << 32) |
            (@as(u96, @intFromBool(effectful)) << 64);
    }

    pub fn internIdxList(self: *TypeInterner, items: []const TypeId) !IdxListId {
        if (items.len == 0) return .empty;
        const fp = hashTypeIds(items);
        for (self.idx_list_metas.items, 0..) |meta, i| {
            if (meta.fingerprint != fp or meta.len != items.len) continue;
            if (eqlTypeIds(self.idx_list_items.items[meta.start..][0..meta.len], items))
                return @enumFromInt(@as(u32, @intCast(i)) + 1);
        }
        const start: u32 = @intCast(self.idx_list_items.items.len);
        try self.idx_list_items.appendSlice(self.allocator, items);
        try self.idx_list_metas.append(self.allocator, .{ .start = start, .len = @intCast(items.len), .fingerprint = fp });
        return @enumFromInt(@as(u32, @intCast(self.idx_list_metas.items.len)));
    }

    pub fn internFieldList(self: *TypeInterner, items: []const FieldKey) !FieldListId {
        if (items.len == 0) return .empty;
        const fp = hashFieldKeys(items);
        for (self.field_list_metas.items, 0..) |meta, i| {
            if (meta.fingerprint != fp or meta.len != items.len) continue;
            if (eqlFieldKeys(self.field_list_items.items[meta.start..][0..meta.len], items))
                return @enumFromInt(@as(u32, @intCast(i)) + 1);
        }
        const start: u32 = @intCast(self.field_list_items.items.len);
        try self.field_list_items.appendSlice(self.allocator, items);
        try self.field_list_metas.append(self.allocator, .{ .start = start, .len = @intCast(items.len), .fingerprint = fp });
        return @enumFromInt(@as(u32, @intCast(self.field_list_metas.items.len)));
    }

    pub fn internTagList(self: *TypeInterner, items: []const TagKey) !TagListId {
        if (items.len == 0) return .empty;
        const fp = hashTagKeys(items);
        for (self.tag_list_metas.items, 0..) |meta, i| {
            if (meta.fingerprint != fp or meta.len != items.len) continue;
            if (eqlTagKeys(self.tag_list_items.items[meta.start..][0..meta.len], items))
                return @enumFromInt(@as(u32, @intCast(i)) + 1);
        }
        const start: u32 = @intCast(self.tag_list_items.items.len);
        try self.tag_list_items.appendSlice(self.allocator, items);
        try self.tag_list_metas.append(self.allocator, .{ .start = start, .len = @intCast(items.len), .fingerprint = fp });
        return @enumFromInt(@as(u32, @intCast(self.tag_list_metas.items.len)));
    }

    pub fn getIdxListItems(self: *const TypeInterner, id: IdxListId) []const TypeId {
        if (id == .empty) return &.{};
        const meta = self.idx_list_metas.items[@intFromEnum(id) - 1];
        return self.idx_list_items.items[meta.start..][0..meta.len];
    }

    pub fn getFieldListItems(self: *const TypeInterner, id: FieldListId) []const FieldKey {
        if (id == .empty) return &.{};
        const meta = self.field_list_metas.items[@intFromEnum(id) - 1];
        return self.field_list_items.items[meta.start..][0..meta.len];
    }

    pub fn getTagListItems(self: *const TypeInterner, id: TagListId) []const TagKey {
        if (id == .empty) return &.{};
        const meta = self.tag_list_metas.items[@intFromEnum(id) - 1];
        return self.tag_list_items.items[meta.start..][0..meta.len];
    }

    pub fn listElem(self: *const TypeInterner, id: TypeId) TypeId {
        std.debug.assert(id.kind == .list);
        return self.list_payloads.items[id.index];
    }

    pub fn boxInner(self: *const TypeInterner, id: TypeId) TypeId {
        std.debug.assert(id.kind == .box);
        return self.box_payloads.items[id.index];
    }

    pub fn tupleElems(self: *const TypeInterner, id: TypeId) []const TypeId {
        std.debug.assert(id.kind == .tuple);
        return self.getIdxListItems(self.tuple_payloads.items[id.index]);
    }

    pub fn getFuncPayload(self: *const TypeInterner, id: TypeId) FuncPayload {
        std.debug.assert(id.kind == .func);
        return self.func_payloads.items[id.index];
    }

    pub fn funcArgs(self: *const TypeInterner, id: TypeId) []const TypeId {
        return self.getIdxListItems(self.getFuncPayload(id).args);
    }

    pub fn funcRet(self: *const TypeInterner, id: TypeId) TypeId {
        return self.getFuncPayload(id).ret;
    }

    pub fn funcEffectful(self: *const TypeInterner, id: TypeId) bool {
        return self.getFuncPayload(id).effectful;
    }

    pub fn recordFields(self: *const TypeInterner, id: TypeId) []const FieldKey {
        std.debug.assert(id.kind == .record);
        return self.getFieldListItems(self.record_payloads.items[id.index]);
    }

    pub fn tagUnionTags(self: *const TypeInterner, id: TypeId) []const TagKey {
        std.debug.assert(id.kind == .tag_union);
        return self.getTagListItems(self.union_payloads.items[id.index]);
    }

    /// Resolve a TypeId through any `.rec` indirection.
    pub fn resolve(self: *const TypeInterner, id: TypeId) TypeId {
        if (id.kind != .rec) return id;
        const target = self.rec_targets.items[id.index];
        // At most one level of indirection
        std.debug.assert(target.kind != .rec);
        return target;
    }

    /// Reserve a recursive type slot. Returns a `.rec` TypeId that can be
    /// stored in cycle-breaker maps. Must be finalized later.
    pub fn reserveRecursive(self: *TypeInterner) !TypeId {
        const index: u29 = @intCast(self.rec_targets.items.len);
        try self.rec_targets.append(self.allocator, TypeId.none);
        return TypeId{ .kind = .rec, .index = index };
    }

    /// Finalize a reserved recursive slot with the actual canonical TypeId.
    pub fn finalizeRecursive(self: *TypeInterner, rec_id: TypeId, actual: TypeId) void {
        std.debug.assert(rec_id.kind == .rec);
        self.rec_targets.items[rec_id.index] = actual;
    }

    /// Intern a structural name from an Ident.Idx (resolves text via ident_store).
    pub fn internNameFromIdent(self: *TypeInterner, ident_store: *const Ident.Store, ident: Ident.Idx) !StructuralNameId {
        return try self.name_pool.intern(ident_store.getText(ident));
    }

    /// Get the text for a structural name.
    pub fn getNameText(self: *const TypeInterner, id: StructuralNameId) []const u8 {
        return self.name_pool.getText(id);
    }

    /// Convert a CIR type variable to a canonical TypeId, recursively
    /// resolving all type structure.
    pub fn fromTypeVar(
        self: *TypeInterner,
        allocator: Allocator,
        types_store: *const types.Store,
        type_var: types.Var,
        common_idents: CommonIdents,
        specializations: *const std.AutoHashMap(types.Var, TypeId),
        nominal_cycle_breakers: *std.AutoHashMap(types.Var, TypeId),
        scratches: *Scratches,
    ) Allocator.Error!TypeId {
        _ = allocator; // TypeInterner uses self.allocator
        const resolved = types_store.resolveVar(type_var);

        if (specializations.get(resolved.var_)) |cached| return cached;
        if (nominal_cycle_breakers.get(resolved.var_)) |cached| return cached;

        return switch (resolved.desc.content) {
            .flex => |flex| {
                if (flex.name) |name| {
                    if (lookupNamedSpecialization(scratches, name)) |specialized| return specialized;
                }
                if (hasNumeralConstraint(types_store, flex.constraints))
                    return self.primIdx(.dec);
                return self.unit_idx;
            },
            .rigid => |rigid| {
                if (lookupNamedSpecialization(scratches, rigid.name)) |specialized| return specialized;
                if (hasNumeralConstraint(types_store, rigid.constraints))
                    return self.primIdx(.dec);
                return self.unit_idx;
            },
            .alias => |alias| {
                const backing_var = types_store.getAliasBackingVar(alias);
                return try self.fromTypeVar(self.allocator, types_store, backing_var, common_idents, specializations, nominal_cycle_breakers, scratches);
            },
            .structure => |flat_type| {
                return try self.fromFlatType(types_store, flat_type, common_idents, specializations, nominal_cycle_breakers, scratches);
            },
            .err => return self.unit_idx,
        };
    }

    fn fromFlatType(
        self: *TypeInterner,
        types_store: *const types.Store,
        flat_type: types.FlatType,
        common_idents: CommonIdents,
        specializations: *const std.AutoHashMap(types.Var, TypeId),
        nominal_cycle_breakers: *std.AutoHashMap(types.Var, TypeId),
        scratches: *Scratches,
    ) Allocator.Error!TypeId {
        return switch (flat_type) {
            .nominal_type => |nominal| {
                return try self.fromNominalType(types_store, nominal, common_idents, specializations, nominal_cycle_breakers, scratches);
            },
            .empty_record => self.unit_idx,
            .empty_tag_union => try self.internTagUnion(&.{}),
            .record => |record| {
                const scratch_top = scratches.fields.top();
                defer scratches.fields.clearFrom(scratch_top);

                var current_row = record;
                rows: while (true) {
                    const fields_slice = types_store.getRecordFieldsSlice(current_row.fields);
                    const names = fields_slice.items(.name);
                    const vars = fields_slice.items(.var_);

                    for (names, vars) |name, field_var| {
                        var seen_name = false;
                        for (scratches.fields.sliceFromStart(scratch_top)) |existing| {
                            const existing_text = self.name_pool.getText(existing.name);
                            const name_text = scratches.ident_store.?.getText(name);
                            if (std.mem.eql(u8, existing_text, name_text)) {
                                seen_name = true;
                                break;
                            }
                        }
                        if (seen_name) continue;

                        const field_type = try self.fromTypeVar(self.allocator, types_store, field_var, common_idents, specializations, nominal_cycle_breakers, scratches);
                        const sname = try self.internNameFromIdent(scratches.ident_store.?, name);
                        try scratches.fields.append(.{ .name = sname, .ty = field_type });
                    }

                    var ext_var = current_row.ext;
                    while (true) {
                        const ext_resolved = types_store.resolveVar(ext_var);
                        switch (ext_resolved.desc.content) {
                            .alias => |alias| {
                                ext_var = types_store.getAliasBackingVar(alias);
                                continue;
                            },
                            .structure => |ext_flat| switch (ext_flat) {
                                .record => |next_row| {
                                    current_row = next_row;
                                    continue :rows;
                                },
                                .record_unbound => |fields_range| {
                                    const ext_fields = types_store.getRecordFieldsSlice(fields_range);
                                    const ext_names = ext_fields.items(.name);
                                    const ext_vars = ext_fields.items(.var_);
                                    for (ext_names, ext_vars) |name, field_var| {
                                        var seen_name = false;
                                        for (scratches.fields.sliceFromStart(scratch_top)) |existing| {
                                            const existing_text = self.name_pool.getText(existing.name);
                                            const name_text = scratches.ident_store.?.getText(name);
                                            if (std.mem.eql(u8, existing_text, name_text)) {
                                                seen_name = true;
                                                break;
                                            }
                                        }
                                        if (seen_name) continue;

                                        const field_type = try self.fromTypeVar(self.allocator, types_store, field_var, common_idents, specializations, nominal_cycle_breakers, scratches);
                                        const sname = try self.internNameFromIdent(scratches.ident_store.?, name);
                                        try scratches.fields.append(.{ .name = sname, .ty = field_type });
                                    }
                                    break :rows;
                                },
                                .empty_record => break :rows,
                                else => {
                                    if (std.debug.runtime_safety) {
                                        std.debug.panic(
                                            "Monotype.fromTypeVar(record): unexpected row extension flat type '{s}'",
                                            .{@tagName(ext_flat)},
                                        );
                                    }
                                    unreachable;
                                },
                            },
                            .flex => {
                                if (findNamedRowExtensionMonotype(scratches, ext_var, types_store)) |specialized| {
                                    try self.appendSpecializedRecordFields(specialized, scratch_top, scratches);
                                }
                                break :rows;
                            },
                            .rigid => {
                                if (findNamedRowExtensionMonotype(scratches, ext_var, types_store)) |specialized| {
                                    try self.appendSpecializedRecordFields(specialized, scratch_top, scratches);
                                }
                                break :rows;
                            },
                            .err => {
                                if (std.debug.runtime_safety) {
                                    std.debug.panic("Monotype.fromTypeVar(record): error row extension tail", .{});
                                }
                                unreachable;
                            },
                        }
                    }
                }

                const collected_fields = scratches.fields.sliceFromStart(scratch_top);
                std.mem.sort(FieldKey, collected_fields, &self.name_pool, FieldKey.sortByNameAsc);
                return try self.internRecord(collected_fields);
            },
            .record_unbound => |fields_range| {
                const fields_slice = types_store.getRecordFieldsSlice(fields_range);
                const names = fields_slice.items(.name);
                const vars = fields_slice.items(.var_);

                const scratch_top = scratches.fields.top();
                defer scratches.fields.clearFrom(scratch_top);

                for (names, vars) |name, field_var| {
                    const field_type = try self.fromTypeVar(self.allocator, types_store, field_var, common_idents, specializations, nominal_cycle_breakers, scratches);
                    const sname = try self.internNameFromIdent(scratches.ident_store.?, name);
                    try scratches.fields.append(.{ .name = sname, .ty = field_type });
                }

                const collected = scratches.fields.sliceFromStart(scratch_top);
                std.mem.sort(FieldKey, collected, &self.name_pool, FieldKey.sortByNameAsc);
                return try self.internRecord(collected);
            },
            .tuple => |tuple| {
                const elem_vars = types_store.sliceVars(tuple.elems);
                const scratch_top = scratches.idxs.top();
                defer scratches.idxs.clearFrom(scratch_top);

                for (elem_vars) |elem_var| {
                    const elem_type = try self.fromTypeVar(self.allocator, types_store, elem_var, common_idents, specializations, nominal_cycle_breakers, scratches);
                    try scratches.idxs.append(elem_type);
                }

                return try self.internTuple(scratches.idxs.sliceFromStart(scratch_top));
            },
            .tag_union => |tag_union_row| {
                const tags_top = scratches.tags.top();
                defer scratches.tags.clearFrom(tags_top);

                var current_row = tag_union_row;
                rows: while (true) {
                    const tags_slice = types_store.getTagsSlice(current_row.tags);
                    const tag_names = tags_slice.items(.name);
                    const tag_args = tags_slice.items(.args);

                    for (tag_names, tag_args) |name, args_range| {
                        const arg_vars = types_store.sliceVars(args_range);
                        const idxs_top = scratches.idxs.top();
                        defer scratches.idxs.clearFrom(idxs_top);

                        for (arg_vars) |arg_var| {
                            const payload_type = try self.fromTypeVar(self.allocator, types_store, arg_var, common_idents, specializations, nominal_cycle_breakers, scratches);
                            try scratches.idxs.append(payload_type);
                        }

                        const payloads_list = try self.internIdxList(scratches.idxs.sliceFromStart(idxs_top));
                        const sname = try self.internNameFromIdent(scratches.ident_store.?, name);
                        try scratches.tags.append(.{ .name = sname, .payloads = payloads_list });
                    }

                    var ext_var = current_row.ext;
                    while (true) {
                        const ext_resolved = types_store.resolveVar(ext_var);
                        switch (ext_resolved.desc.content) {
                            .alias => |alias| {
                                ext_var = types_store.getAliasBackingVar(alias);
                                continue;
                            },
                            .structure => |ext_flat| switch (ext_flat) {
                                .tag_union => |next_row| {
                                    current_row = next_row;
                                    continue :rows;
                                },
                                .empty_tag_union => break :rows,
                                else => {
                                    if (std.debug.runtime_safety) {
                                        std.debug.panic(
                                            "Monotype.fromTypeVar(tag_union): unexpected row extension flat type '{s}'",
                                            .{@tagName(ext_flat)},
                                        );
                                    }
                                    unreachable;
                                },
                            },
                            .flex => {
                                if (findNamedRowExtensionMonotype(scratches, ext_var, types_store)) |specialized| {
                                    try self.appendSpecializedTagUnionTags(specialized, scratches);
                                }
                                break :rows;
                            },
                            .rigid => {
                                if (findNamedRowExtensionMonotype(scratches, ext_var, types_store)) |specialized| {
                                    try self.appendSpecializedTagUnionTags(specialized, scratches);
                                }
                                break :rows;
                            },
                            .err => {
                                if (std.debug.runtime_safety) {
                                    std.debug.panic("Monotype.fromTypeVar(tag_union): error row extension tail", .{});
                                }
                                unreachable;
                            },
                        }
                    }
                }

                const collected_tags = scratches.tags.sliceFromStart(tags_top);
                std.mem.sort(TagKey, collected_tags, &self.name_pool, TagKey.sortByNameAsc);
                return try self.internTagUnion(collected_tags);
            },
            .fn_pure => |func| try self.fromFuncType(types_store, func, false, common_idents, specializations, nominal_cycle_breakers, scratches),
            .fn_effectful => |func| try self.fromFuncType(types_store, func, true, common_idents, specializations, nominal_cycle_breakers, scratches),
            .fn_unbound => |func| try self.fromFuncType(types_store, func, false, common_idents, specializations, nominal_cycle_breakers, scratches),
        };
    }

    fn fromFuncType(
        self: *TypeInterner,
        types_store: *const types.Store,
        func: types.Func,
        effectful: bool,
        common_idents: CommonIdents,
        specializations: *const std.AutoHashMap(types.Var, TypeId),
        nominal_cycle_breakers: *std.AutoHashMap(types.Var, TypeId),
        scratches: *Scratches,
    ) Allocator.Error!TypeId {
        const arg_vars = types_store.sliceVars(func.args);
        const scratch_top = scratches.idxs.top();
        defer scratches.idxs.clearFrom(scratch_top);

        for (arg_vars) |arg_var| {
            const arg_type = try self.fromTypeVar(self.allocator, types_store, arg_var, common_idents, specializations, nominal_cycle_breakers, scratches);
            try scratches.idxs.append(arg_type);
        }

        const ret = try self.fromTypeVar(self.allocator, types_store, func.ret, common_idents, specializations, nominal_cycle_breakers, scratches);
        return try self.internFunc(scratches.idxs.sliceFromStart(scratch_top), ret, effectful);
    }

    fn buildNominalCacheKey(
        self: *TypeInterner,
        types_store: *const types.Store,
        nominal: types.NominalType,
        common_idents: CommonIdents,
        specializations: *const std.AutoHashMap(types.Var, TypeId),
        nominal_cycle_breakers: *std.AutoHashMap(types.Var, TypeId),
        scratches: *Scratches,
    ) Allocator.Error!NominalInstanceKey {
        const ident_store = if (scratches.module_env) |env| env.getIdentStoreConst() else scratches.ident_store.?;
        const origin_text = ident_store.getText(nominal.origin_module);
        const type_text = ident_store.getText(nominal.ident.ident_idx);
        const origin_name_id = try self.name_pool.intern(origin_text);
        const type_name_id = try self.name_pool.intern(type_text);

        const actual_args = types_store.sliceNominalArgs(nominal);
        const idx_top = scratches.idxs.top();
        defer scratches.idxs.clearFrom(idx_top);
        for (actual_args) |arg_var| {
            const arg_mono = try self.fromTypeVar(
                self.allocator,
                types_store,
                arg_var,
                common_idents,
                specializations,
                nominal_cycle_breakers,
                scratches,
            );
            try scratches.idxs.append(arg_mono);
        }
        const args = try self.internIdxList(scratches.idxs.sliceFromStart(idx_top));

        return .{
            .origin_name_id = origin_name_id,
            .type_name_id = type_name_id,
            .args = args,
        };
    }

    fn fromNominalType(
        self: *TypeInterner,
        types_store: *const types.Store,
        nominal: types.NominalType,
        common_idents: CommonIdents,
        specializations: *const std.AutoHashMap(types.Var, TypeId),
        nominal_cycle_breakers: *std.AutoHashMap(types.Var, TypeId),
        scratches: *Scratches,
    ) Allocator.Error!TypeId {
        const ident = nominal.ident.ident_idx;
        const origin = nominal.origin_module;

        if (origin.eql(common_idents.builtin_module)) {
            if (ident.eql(common_idents.str)) return self.primIdx(.str);
            if (ident.eql(common_idents.bool)) return try self.addBoolTagUnion();
        }

        if (origin.eql(common_idents.builtin_module)) {
            if (ident.eql(common_idents.list)) {
                const type_args = types_store.sliceNominalArgs(nominal);
                if (type_args.len > 0) {
                    const elem_type = try self.fromTypeVar(self.allocator, types_store, type_args[0], common_idents, specializations, nominal_cycle_breakers, scratches);
                    return try self.internList(elem_type);
                }
                return self.unit_idx;
            }

            if (ident.eql(common_idents.box)) {
                const type_args = types_store.sliceNominalArgs(nominal);
                if (type_args.len > 0) {
                    const inner_type = try self.fromTypeVar(self.allocator, types_store, type_args[0], common_idents, specializations, nominal_cycle_breakers, scratches);
                    return try self.internBox(inner_type);
                }
                return self.unit_idx;
            }

            if (ident.eql(common_idents.i64_type)) return self.primIdx(.i64);
            if (ident.eql(common_idents.u8_type)) return self.primIdx(.u8);
            if (ident.eql(common_idents.i8_type)) return self.primIdx(.i8);
            if (ident.eql(common_idents.u16_type)) return self.primIdx(.u16);
            if (ident.eql(common_idents.i16_type)) return self.primIdx(.i16);
            if (ident.eql(common_idents.u32_type)) return self.primIdx(.u32);
            if (ident.eql(common_idents.i32_type)) return self.primIdx(.i32);
            if (ident.eql(common_idents.u64_type)) return self.primIdx(.u64);
            if (ident.eql(common_idents.u128_type)) return self.primIdx(.u128);
            if (ident.eql(common_idents.i128_type)) return self.primIdx(.i128);
            if (ident.eql(common_idents.f32_type)) return self.primIdx(.f32);
            if (ident.eql(common_idents.f64_type)) return self.primIdx(.f64);
            if (ident.eql(common_idents.dec_type)) return self.primIdx(.dec);
        }

        // Non-builtin nominal: use nominal instance cache for dedup.
        // 1. Build canonical cache key (lowers args as side effect).
        const cache_key = try self.buildNominalCacheKey(
            types_store,
            nominal,
            common_idents,
            specializations,
            nominal_cycle_breakers,
            scratches,
        );

        // 2. Check nominal instance cache.
        if (self.nominal_cache.get(cache_key)) |entry| {
            switch (entry) {
                .resolved => |type_id| return type_id,
                .in_progress => |rec_id| {
                    // Recursive hit: allocate .rec lazily if first time.
                    if (rec_id.isNone()) {
                        const new_rec = try self.reserveRecursive();
                        // Must re-fetch after potential map growth from reserveRecursive.
                        self.nominal_cache.getPtr(cache_key).?.* = .{ .in_progress = new_rec };
                        return new_rec;
                    }
                    return rec_id;
                },
            }
        }

        // 3. Cache miss: mark in-progress before recursing.
        try self.nominal_cache.put(self.allocator, cache_key, .{ .in_progress = TypeId.none });

        // 4. Push nominal arg specializations (args already lowered by buildNominalCacheKey).
        const named_specializations_top = try self.pushNominalArgSpecializations(
            types_store,
            nominal,
            common_idents,
            specializations,
            nominal_cycle_breakers,
            scratches,
        );
        defer scratches.named_specializations.clearFrom(named_specializations_top);

        // 5. Lower the backing var.
        const backing_var = types_store.getNominalBackingVar(nominal);
        const backing_idx = try self.fromTypeVar(
            self.allocator,
            types_store,
            backing_var,
            common_idents,
            specializations,
            nominal_cycle_breakers,
            scratches,
        );
        std.debug.assert(!backing_idx.isNone());

        // 6. Finalize: check if recursion occurred.
        //    Must re-fetch pointer because recursive lowering may have grown the map.
        const entry_ptr = self.nominal_cache.getPtr(cache_key).?;
        const in_progress_rec = entry_ptr.in_progress;

        if (!in_progress_rec.isNone()) {
            // Recursive: finalize the .rec slot, cache the .rec TypeId.
            self.finalizeRecursive(in_progress_rec, backing_idx);
            entry_ptr.* = .{ .resolved = in_progress_rec };
            return in_progress_rec;
        } else {
            // Non-recursive: cache the backing type directly.
            entry_ptr.* = .{ .resolved = backing_idx };
            return backing_idx;
        }
    }

    fn lookupNamedSpecialization(scratches: *const Scratches, name: Ident.Idx) ?TypeId {
        const ident_store = scratches.ident_store orelse return null;
        const name_text = ident_store.getText(name);
        const items = scratches.named_specializations.items.items;

        var i = items.len;
        while (i > 0) {
            i -= 1;
            if (std.mem.eql(u8, items[i].name_text, name_text)) {
                return items[i].type_idx;
            }
        }
        return null;
    }

    fn findNamedRowExtensionMonotype(scratches: *const Scratches, ext_var: types.Var, types_store: *const types.Store) ?TypeId {
        const resolved = types_store.resolveVar(ext_var);
        return switch (resolved.desc.content) {
            .flex => |flex| if (flex.name) |name| lookupNamedSpecialization(scratches, name) else null,
            .rigid => |rigid| lookupNamedSpecialization(scratches, rigid.name),
            else => null,
        };
    }

    fn appendSpecializedRecordFields(
        self: *TypeInterner,
        specialized: TypeId,
        scratch_top: u32,
        scratches: *Scratches,
    ) Allocator.Error!void {
        const resolved = self.resolve(specialized);
        if (resolved.isUnit()) return;
        if (resolved.kind != .record) {
            if (std.debug.runtime_safety) {
                std.debug.panic(
                    "Monotype.fromTypeVar(record): nominal row specialization must be record or unit, found kind={d}",
                    .{@intFromEnum(resolved.kind)},
                );
            }
            unreachable;
        }
        for (self.recordFields(resolved)) |field| {
            var seen_name = false;
            for (scratches.fields.sliceFromStart(scratch_top)) |existing| {
                if (@intFromEnum(existing.name) == @intFromEnum(field.name)) {
                    seen_name = true;
                    break;
                }
            }
            if (seen_name) continue;
            try scratches.fields.append(field);
        }
    }

    fn appendSpecializedTagUnionTags(
        self: *TypeInterner,
        specialized: TypeId,
        scratches: *Scratches,
    ) Allocator.Error!void {
        const resolved = self.resolve(specialized);
        if (resolved.kind != .tag_union) {
            if (std.debug.runtime_safety) {
                std.debug.panic(
                    "Monotype.fromTypeVar(tag_union): nominal row specialization must be tag_union, found kind={d}",
                    .{@intFromEnum(resolved.kind)},
                );
            }
            unreachable;
        }
        for (self.tagUnionTags(resolved)) |tag| {
            try scratches.tags.append(tag);
        }
    }

    fn pushNominalArgSpecializations(
        self: *TypeInterner,
        types_store: *const types.Store,
        nominal: types.NominalType,
        common_idents: CommonIdents,
        specializations: *const std.AutoHashMap(types.Var, TypeId),
        nominal_cycle_breakers: *std.AutoHashMap(types.Var, TypeId),
        scratches: *Scratches,
    ) Allocator.Error!u32 {
        const top = scratches.named_specializations.top();

        const source_env = scratches.module_env orelse return top;
        const all_module_envs = scratches.all_module_envs orelse return top;
        const definition_env = findNominalDefinitionEnv(source_env, all_module_envs, nominal.origin_module) orelse return top;
        const type_name = source_env.getIdent(nominal.ident.ident_idx);
        const definition_nominal = findDefinitionNominal(definition_env, type_name) orelse return top;

        const formal_args = definition_env.types.sliceNominalArgs(definition_nominal);
        const actual_args = types_store.sliceNominalArgs(nominal);
        if (formal_args.len != actual_args.len) {
            if (std.debug.runtime_safety) {
                std.debug.panic(
                    "Monotype.fromNominalType: arg arity mismatch for nominal '{s}' (formal={d}, actual={d})",
                    .{ type_name, formal_args.len, actual_args.len },
                );
            }
            unreachable;
        }

        for (formal_args, actual_args) |formal_arg, actual_arg| {
            const formal_name_text = resolvedTypeVarNameText(&definition_env.types, definition_env, formal_arg) orelse continue;
            const actual_mono = try self.fromTypeVar(
                self.allocator,
                types_store,
                actual_arg,
                common_idents,
                specializations,
                nominal_cycle_breakers,
                scratches,
            );
            try scratches.named_specializations.append(.{
                .name_text = formal_name_text,
                .type_idx = actual_mono,
            });
        }

        return top;
    }

    fn resolvedTypeVarNameText(
        types_store: *const types.Store,
        module_env: *const ModuleEnv,
        var_: types.Var,
    ) ?[]const u8 {
        const resolved = types_store.resolveVar(var_);
        return switch (resolved.desc.content) {
            .rigid => |rigid| module_env.getIdent(rigid.name),
            .flex => |flex| if (flex.name) |name| module_env.getIdent(name) else null,
            else => null,
        };
    }

    fn findNominalDefinitionEnv(
        source_env: *const ModuleEnv,
        all_module_envs: []const *ModuleEnv,
        origin_module: Ident.Idx,
    ) ?*const ModuleEnv {
        const origin_name = source_env.getIdent(origin_module);
        for (all_module_envs) |candidate_env| {
            const candidate_name = candidate_env.getIdent(candidate_env.qualified_module_ident);
            if (std.mem.eql(u8, origin_name, candidate_name)) return candidate_env;
        }
        return null;
    }

    fn findDefinitionNominal(definition_env: *const ModuleEnv, type_name: []const u8) ?types.NominalType {
        for (definition_env.store.sliceStatements(definition_env.all_statements)) |stmt_idx| {
            const stmt = definition_env.store.getStatement(stmt_idx);
            switch (stmt) {
                .s_nominal_decl => |nominal_decl| {
                    const header = definition_env.store.getTypeHeader(nominal_decl.header);
                    if (!std.mem.eql(u8, definition_env.getIdent(header.relative_name), type_name)) continue;

                    const resolved = definition_env.types.resolveVar(ModuleEnv.varFrom(stmt_idx));
                    if (resolved.desc.content == .structure and resolved.desc.content.structure == .nominal_type) {
                        return resolved.desc.content.structure.nominal_type;
                    }
                },
                else => {},
            }
        }
        return null;
    }
};

/// Backward-compat alias.
pub const Store = TypeInterner;
