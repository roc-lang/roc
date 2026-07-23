//! Monomorphic type store used by Monotype and Monotype Lifted IR.
//!
//! This store contains closed checked types after static dispatch and numeric
//! defaulting have been finalized. It has no lambda sets and no layout data.

const std = @import("std");
const check = @import("check");
const collections = @import("collections");

const Common = @import("../common.zig");
const census = @import("census.zig");
const names = check.CheckedNames;
const checked = check.CheckedModule;
const static_dispatch = check.StaticDispatchRegistry;
const GuardedList = collections.GuardedList;

fn StoreList(comptime T: type, comptime field_name: []const u8) type {
    return GuardedList.List(T, "monotype.Type.Store." ++ field_name);
}

/// Guarded immutable span borrow for a named Monotype type-store list.
pub fn StoreSpanBorrow(comptime T: type, comptime field_name: []const u8) type {
    return GuardedList.BorrowSpan(T, "monotype.Type.Store." ++ field_name);
}

/// Identifier for a monomorphic type in this store.
pub const TypeId = enum(u32) { _ };

/// Slice descriptor for type, field, or tag arrays in this store.
pub const SidePoolSpan = extern struct {
    start: u32,
    len: u32,

    pub fn empty() SidePoolSpan {
        return .{ .start = 0, .len = 0 };
    }
};

/// Compatibility name for existing Monotype type side-pool spans.
pub const Span = SidePoolSpan;

/// Cached structural digest stored beside a durable Monotype type node.
pub const MonoTypeDigest = names.TypeDigest;

/// Primitive type copied from checked module data.
pub const Primitive = checked.CheckedPrimitive;

/// Named type definition owner.
pub const TypeDef = struct {
    /// Deep content identity of the declaring module (dense id in the owning
    /// name store's module identity table).
    module: names.ModuleIdentityId,
    /// Declared (module-relative) type name.
    type_name: names.TypeNameId,
    /// Declaring statement in the (content-identified) module: the
    /// within-module discriminator for same-named block-local declarations.
    source_decl: ?u32 = null,
    /// Compiler-generated specialization identity for internal nominals minted
    /// from a public source nominal. Null means this is the source nominal.
    generated: ?names.TypeDigest = null,
    /// Representation decision produced when an internal iterator nominal is
    /// created. Later stages consume the recorded tier and mint depth directly.
    iterator_representation: IteratorRepresentation = .none,
    /// Exact producer or adapter that minted this iterator representation.
    /// Consumers use this evidence instead of reconstructing an operation from
    /// the generated function body's shape.
    iterator_kind: IteratorKind = .none,
    /// Producer-computed minted-chain depth. Meaningful only for `.minted`.
    iterator_depth: u8 = 0,
};

/// Explicit representation tier assigned when an iterator nominal is created.
pub const IteratorRepresentation = enum(u8) {
    none,
    minted,
    forced_dynamic,
};

/// Producer-owned identity of an internal iterator representation.
pub const IteratorKind = enum(u8) {
    none,
    custom,
    list,
    single,
    range_exclusive,
    range_inclusive,
    map,
    keep_if,
    drop_if,
    take_first,
    drop_first,
    concat,
    append,
    forced_dynamic,
};

/// Exceptional relation between two named iterator types. Equal identities
/// and unrelated named types use ordinary named-type unification.
pub const IteratorRelation = enum(u8) {
    ordinary,
    public_minted,
    forced_dynamic,
    minted_join,
};

/// Classifies the representation-tier relation shared by Monotype
/// instantiation and Lambda Solved unification.
pub fn iteratorRelation(left: anytype, right: anytype) IteratorRelation {
    if (left.kind != right.kind) return .ordinary;
    if (left.def.module != right.def.module or
        left.def.type_name != right.def.type_name or
        left.def.source_decl != right.def.source_decl)
    {
        return .ordinary;
    }
    if (!iteratorOwnerPair(left.builtin_owner, right.builtin_owner)) return .ordinary;

    const left_representation = left.def.iterator_representation;
    const right_representation = right.def.iterator_representation;
    if ((left_representation == .forced_dynamic) != (right_representation == .forced_dynamic)) {
        return .forced_dynamic;
    }
    if ((left_representation == .minted and right_representation == .none) or
        (left_representation == .none and right_representation == .minted))
    {
        return .public_minted;
    }
    if (left_representation == .minted and
        right_representation == .minted and
        !optionalDigestEql(left.def.generated, right.def.generated))
    {
        return .minted_join;
    }
    return .ordinary;
}

fn iteratorOwnerPair(
    left: ?static_dispatch.BuiltinOwner,
    right: ?static_dispatch.BuiltinOwner,
) bool {
    const owner = left orelse right orelse return false;
    if (!static_dispatch.isIteratorOwner(owner)) return false;
    if (left) |left_owner| {
        if (left_owner != owner) return false;
    }
    if (right) |right_owner| {
        if (right_owner != owner) return false;
    }
    return true;
}

/// Named checked type instance.
pub const NamedType = struct {
    module: names.CheckedModuleDigest,
    ty: checked.CheckedTypeId,
};

/// How much of a named type's backing type later stages may inspect.
pub const BackingUse = enum(u8) {
    inspectable,
    runtime_layout_only,
};

/// Backing type for a named type when checking output one.
pub const NamedBacking = struct {
    ty: TypeId,
    use: BackingUse,
};

/// Kind of named type visible after checking.
pub const NamedKind = enum(u8) {
    nominal,
    @"opaque",
    alias,
};

/// Record field type entry.
pub const MonoTypeField = struct {
    name: names.RecordFieldNameId,
    ty: TypeId,
};

/// Compatibility name for existing Monotype record field entries.
pub const Field = MonoTypeField;

/// Tag-union variant type entry.
pub const MonoTypeTag = struct {
    name: names.TagNameId,
    checked_name: names.TagNameId,
    payloads: Span,
};

/// Compatibility name for existing Monotype tag-union variant entries.
pub const Tag = MonoTypeTag;

/// One entry of a nominal record's declared layout order. The backing row is
/// always lexicographic (for name resolution and digests); a nominal type
/// additionally carries this declared order, which the layout commit consumes to
/// place fields in source order with no internal padding. See design.md
/// "Nominal Record Field Order".
pub const DeclaredField = union(enum(u8)) {
    /// A named backing field, matched against the lexicographic backing row by
    /// name at layout time.
    named: names.RecordFieldNameId,
    /// An unnamed padding field reserving `sizeof(ty)` bytes at alignment 1. Its
    /// bytes are uninitialized and it is not accessible.
    padding: TypeId,
};

/// Durable monomorphic type node.
pub const MonoTypeNode = union(enum(u8)) {
    primitive: Primitive,
    named: struct {
        named_type: NamedType,
        def: TypeDef,
        kind: NamedKind,
        builtin_owner: ?static_dispatch.BuiltinOwner = null,
        args: Span,
        backing: ?NamedBacking = null,
        /// Declared field order for a nominal/opaque record backing; empty for
        /// every other named type (consumed only by layout).
        declared_order: Span = Span.empty(),
    },
    record: Span,
    tuple: Span,
    tag_union: Span,
    list: TypeId,
    box: TypeId,
    func: struct {
        args: Span,
        ret: TypeId,
    },
    erased: names.TypeDigest,
    zst,
};

/// Compatibility name for existing Monotype type-node content.
pub const Content = MonoTypeNode;

/// Payload stored by `MonoTypeNode.named`.
pub const NamedContent = std.meta.fieldInfo(MonoTypeNode, .named).type;

const ContentTag = std.meta.Tag(Content);
const DeclaredFieldTag = std.meta.Tag(DeclaredField);

const NamedDigestMode = enum {
    full,
    identity_only,
};

const IdentityTypeSpan = enum {
    named_args,
    tuple_items,
    func_args,
};

const IdentityBacking = enum {
    none,
    some,
};

/// Store for monomorphic types and their shared spans.
pub const Store = struct {
    allocator: std.mem.Allocator,
    types: StoreList(Content, "types"),
    type_digests: StoreList(?names.TypeDigest, "type_digests"),
    specialization_digests: StoreList(?names.TypeDigest, "specialization_digests"),
    type_digest_generations: StoreList(u64, "type_digest_generations"),
    specialization_digest_generations: StoreList(u64, "specialization_digest_generations"),
    /// Unique allocation epoch for each live TypeId. Store restoration can
    /// recycle an id after discarding an interner candidate, so caches keyed
    /// by TypeId validate this epoch as well as mutable-view generations.
    type_epochs: StoreList(u64, "type_epochs"),
    next_type_epoch: u64,
    digest_cache_generation: u64,
    spans: StoreList(TypeId, "spans"),
    fields: StoreList(Field, "fields"),
    tags: StoreList(Tag, "tags"),
    declared_fields: StoreList(DeclaredField, "declared_fields"),
    frozen: bool,
    /// Content-dedup buckets (digest -> id list). When null, the `intern*`
    /// constructor methods behave as plain adds: they build the node and return
    /// its fresh id with no dedup or registration. When present, structurally
    /// equal nodes share one id. Head/alias canonicalization intrinsic to the
    /// constructors (row sorting, storage-transparent alias erase) applies in
    /// both modes; only dedup is gated by this field. Defaults to null so a
    /// store cloned by field-wise copy starts as a plain (non-interning) store.
    intern_buckets: ?std.AutoHashMap(InternerLookupDigest, std.ArrayList(TypeId)) = null,

    pub fn init(allocator: std.mem.Allocator) Store {
        return .{
            .allocator = allocator,
            .types = .empty,
            .type_digests = .empty,
            .specialization_digests = .empty,
            .type_digest_generations = .empty,
            .specialization_digest_generations = .empty,
            .type_epochs = .empty,
            .next_type_epoch = 1,
            .digest_cache_generation = 1,
            .spans = .empty,
            .fields = .empty,
            .tags = .empty,
            .declared_fields = .empty,
            .frozen = false,
            .intern_buckets = null,
        };
    }

    /// Turn on content dedup for this store's `intern*` constructor methods.
    /// Idempotent. Stage C flips production stores through this so the same
    /// construction boundary that plain-adds today starts hash-consing.
    pub fn enableInterning(self: *Store) void {
        if (self.intern_buckets == null) {
            self.intern_buckets = std.AutoHashMap(InternerLookupDigest, std.ArrayList(TypeId)).init(self.allocator);
        }
    }

    /// Whether this store's constructors dedup structurally equal nodes.
    pub fn internEnabled(self: *const Store) bool {
        return self.intern_buckets != null;
    }

    pub fn deinit(self: *Store) void {
        if (self.intern_buckets) |*buckets| {
            var lists = buckets.valueIterator();
            while (lists.next()) |list| list.deinit(self.allocator);
            buckets.deinit();
        }
        self.declared_fields.deinit(self.allocator);
        self.tags.deinit(self.allocator);
        self.fields.deinit(self.allocator);
        self.spans.deinit(self.allocator);
        self.specialization_digest_generations.deinit(self.allocator);
        self.type_digest_generations.deinit(self.allocator);
        self.type_epochs.deinit(self.allocator);
        self.specialization_digests.deinit(self.allocator);
        self.type_digests.deinit(self.allocator);
        self.types.deinit(self.allocator);
    }

    pub fn freeze(self: *Store) void {
        self.frozen = true;
    }

    pub fn isFrozen(self: *const Store) bool {
        return self.frozen;
    }

    pub fn addSpan(self: *Store, values: []const TypeId) std.mem.Allocator.Error!Span {
        self.assertMutable();
        if (values.len == 0) return .empty();
        const start: u32 = @intCast(self.spans.len());
        try self.spans.appendSlice(self.allocator, values);
        return .{ .start = start, .len = @intCast(values.len) };
    }

    pub fn addFields(self: *Store, values: []const Field) std.mem.Allocator.Error!Span {
        self.assertMutable();
        if (values.len == 0) return .empty();
        const start: u32 = @intCast(self.fields.len());
        try self.fields.appendSlice(self.allocator, values);
        return .{ .start = start, .len = @intCast(values.len) };
    }

    /// Normalize record fields by label text before appending a durable span.
    pub fn addRecordFields(self: *Store, name_store: *const names.NameStore, values: []const Field) std.mem.Allocator.Error!Span {
        if (values.len == 0) return .empty();
        const normalized = try self.allocator.dupe(Field, values);
        defer self.allocator.free(normalized);
        std.mem.sort(Field, normalized, name_store, recordFieldLessThan);
        assertNoDuplicateRecordFields(name_store, normalized);
        return try self.addFields(normalized);
    }

    pub fn addTags(self: *Store, values: []const Tag) std.mem.Allocator.Error!Span {
        self.assertMutable();
        if (values.len == 0) return .empty();
        const start: u32 = @intCast(self.tags.len());
        try self.tags.appendSlice(self.allocator, values);
        return .{ .start = start, .len = @intCast(values.len) };
    }

    /// Normalize tag-union variants by label text before appending a durable span.
    pub fn addTagVariants(self: *Store, name_store: *const names.NameStore, values: []const Tag) std.mem.Allocator.Error!Span {
        if (values.len == 0) return .empty();
        const normalized = try self.allocator.dupe(Tag, values);
        defer self.allocator.free(normalized);
        std.mem.sort(Tag, normalized, name_store, tagLessThan);
        assertNoDuplicateTags(name_store, normalized);
        return try self.addTags(normalized);
    }

    pub fn add(self: *Store, content: Content) std.mem.Allocator.Error!TypeId {
        if (census.enabled) {
            switch (content) {
                .named => |named| if (named.kind == .alias and named.builtin_owner != null) {
                    census.bump("builtin_owned_alias_created");
                },
                else => {},
            }
        }
        self.assertMutable();
        const index = self.types.len();
        try self.types.append(self.allocator, content);
        errdefer _ = self.types.pop();
        try self.type_digests.append(self.allocator, null);
        errdefer _ = self.type_digests.pop();
        try self.specialization_digests.append(self.allocator, null);
        errdefer _ = self.specialization_digests.pop();
        try self.type_digest_generations.append(self.allocator, 0);
        errdefer _ = self.type_digest_generations.pop();
        try self.specialization_digest_generations.append(self.allocator, 0);
        errdefer _ = self.specialization_digest_generations.pop();
        if (self.next_type_epoch == std.math.maxInt(u64)) Common.invariant("Monotype type epoch exhausted");
        try self.type_epochs.append(self.allocator, self.next_type_epoch);
        self.next_type_epoch += 1;
        return @enumFromInt(@as(u32, @intCast(index)));
    }

    /// Add one recursive type without returning its id to the caller until the
    /// node content has been installed. The callback receives the private id so
    /// the content can point back to itself or register it in an in-progress
    /// recursive sealer.
    pub fn addRecursive(
        self: *Store,
        context: anytype,
        comptime fill: fn (@TypeOf(context), TypeId) std.mem.Allocator.Error!Content,
    ) std.mem.Allocator.Error!TypeId {
        const mark_ = self.mark();
        errdefer self.restore(mark_);
        const reserved = try self.reserveSlot();
        const content = try fill(context, reserved);
        self.fillReservedSlot(reserved, content);
        return reserved;
    }

    fn reserveSlot(self: *Store) std.mem.Allocator.Error!TypeId {
        return try self.add(.zst);
    }

    fn fillReservedSlot(self: *Store, ty: TypeId, content: Content) void {
        self.assertMutable();
        self.types.set(@intFromEnum(ty), content);
        self.clearTypeDigestCache();
    }

    pub fn get(self: *const Store, ty: TypeId) Content {
        return self.types.unsafeRawItemsForView()[@intFromEnum(ty)];
    }

    pub fn typeEpoch(self: *const Store, ty: TypeId) u64 {
        return self.type_epochs.unsafeRawItemsForView()[@intFromEnum(ty)];
    }

    pub fn span(self: *const Store, span_: Span) StoreSpanBorrow(TypeId, "spans") {
        return self.spans.borrowSpan(span_.start, span_.len);
    }

    pub fn fieldSpan(self: *const Store, span_: Span) StoreSpanBorrow(Field, "fields") {
        return self.fields.borrowSpan(span_.start, span_.len);
    }

    pub fn tagSpan(self: *const Store, span_: Span) StoreSpanBorrow(Tag, "tags") {
        return self.tags.borrowSpan(span_.start, span_.len);
    }

    pub fn addDeclaredFields(self: *Store, values: []const DeclaredField) std.mem.Allocator.Error!Span {
        self.assertMutable();
        if (values.len == 0) return .empty();
        const start: u32 = @intCast(self.declared_fields.len());
        try self.declared_fields.appendSlice(self.allocator, values);
        return .{ .start = start, .len = @intCast(values.len) };
    }

    pub fn declaredFieldSpan(self: *const Store, span_: Span) StoreSpanBorrow(DeclaredField, "declared_fields") {
        return self.declared_fields.borrowSpan(span_.start, span_.len);
    }

    // Hash-consing construction boundary.
    //
    // These constructors are child-first for acyclic types: callers pass
    // already-built child ids, and every successful call returns an id whose
    // content the store never mutates afterward. Recursive groups reserve
    // private slots, fill each once, and expose member ids only after the
    // whole group has a rooted digest and equality bucket. With
    // `intern_buckets` present the result is deduplicated; without it each call
    // is a plain add. The opaque `Interner` below is a thin delegate over a
    // store with interning enabled.

    pub const TagInput = struct {
        name: names.TagNameId,
        checked_name: names.TagNameId,
        payloads: []const TypeId,
    };

    pub const NamedInput = struct {
        named_type: NamedType,
        def: TypeDef,
        kind: NamedKind,
        builtin_owner: ?static_dispatch.BuiltinOwner = null,
        args: []const TypeId = &.{},
        backing: ?NamedBacking = null,
        declared_order: []const DeclaredField = &.{},
    };

    pub fn internPrimitive(self: *Store, name_store: *const names.NameStore, primitive: Primitive) std.mem.Allocator.Error!TypeId {
        const mark_ = self.mark();
        const ty = try self.add(.{ .primitive = primitive });
        return try self.internCandidate(name_store, mark_, ty);
    }

    pub fn internZst(self: *Store, name_store: *const names.NameStore) std.mem.Allocator.Error!TypeId {
        const mark_ = self.mark();
        const ty = try self.add(.zst);
        return try self.internCandidate(name_store, mark_, ty);
    }

    pub fn internList(self: *Store, name_store: *const names.NameStore, elem: TypeId) std.mem.Allocator.Error!TypeId {
        const mark_ = self.mark();
        const ty = try self.add(.{ .list = elem });
        return try self.internCandidate(name_store, mark_, ty);
    }

    pub fn internBox(self: *Store, name_store: *const names.NameStore, elem: TypeId) std.mem.Allocator.Error!TypeId {
        const mark_ = self.mark();
        const ty = try self.add(.{ .box = elem });
        return try self.internCandidate(name_store, mark_, ty);
    }

    pub fn internTuple(self: *Store, name_store: *const names.NameStore, items: []const TypeId) std.mem.Allocator.Error!TypeId {
        const mark_ = self.mark();
        errdefer self.restore(mark_);
        const span_ = try self.addSpan(items);
        const ty = try self.add(.{ .tuple = span_ });
        return try self.internCandidate(name_store, mark_, ty);
    }

    pub fn internFunc(self: *Store, name_store: *const names.NameStore, args: []const TypeId, ret: TypeId) std.mem.Allocator.Error!TypeId {
        const mark_ = self.mark();
        errdefer self.restore(mark_);
        const span_ = try self.addSpan(args);
        const ty = try self.add(.{ .func = .{ .args = span_, .ret = ret } });
        return try self.internCandidate(name_store, mark_, ty);
    }

    pub fn internRecord(self: *Store, name_store: *const names.NameStore, raw_fields: []const Field) std.mem.Allocator.Error!TypeId {
        const mark_ = self.mark();
        errdefer self.restore(mark_);
        const span_ = try self.addRecordFields(name_store, raw_fields);
        const ty = try self.add(.{ .record = span_ });
        return try self.internCandidate(name_store, mark_, ty);
    }

    pub fn internTagUnion(self: *Store, name_store: *const names.NameStore, raw_tags: []const TagInput) std.mem.Allocator.Error!TypeId {
        const mark_ = self.mark();
        errdefer self.restore(mark_);

        const lowered = try self.allocator.alloc(Tag, raw_tags.len);
        defer self.allocator.free(lowered);
        for (raw_tags, 0..) |tag, index| {
            lowered[index] = .{
                .name = tag.name,
                .checked_name = tag.checked_name,
                .payloads = try self.addSpan(tag.payloads),
            };
        }

        const span_ = try self.addTagVariants(name_store, lowered);
        const ty = try self.add(.{ .tag_union = span_ });
        return try self.internCandidate(name_store, mark_, ty);
    }

    pub fn internNamed(self: *Store, name_store: *const names.NameStore, named: NamedInput) std.mem.Allocator.Error!TypeId {
        // A storage-transparent alias — backed, with no builtin dispatch owner —
        // is erased before insertion: the already-built backing is returned so
        // no id names the alias node. A backing-less alias stays a retained
        // marker, and a builtin-owned alias retains its dispatch owner. This
        // canonicalization is independent of `intern_buckets`.
        if (named.kind == .alias and named.builtin_owner == null) {
            if (named.backing) |backing| {
                return backing.ty;
            }
        }

        const mark_ = self.mark();
        errdefer self.restore(mark_);

        const args = try self.addSpan(named.args);
        const declared_order = try self.addDeclaredFields(named.declared_order);
        const content: NamedContent = .{
            .named_type = named.named_type,
            .def = named.def,
            .kind = named.kind,
            .builtin_owner = named.builtin_owner,
            .args = args,
            .backing = named.backing,
            .declared_order = declared_order,
        };
        const ty = try self.add(.{ .named = content });
        return try self.internCandidate(name_store, mark_, ty);
    }

    pub fn internErased(self: *Store, name_store: *const names.NameStore, digest: names.TypeDigest) std.mem.Allocator.Error!TypeId {
        const mark_ = self.mark();
        const ty = try self.add(.{ .erased = digest });
        return try self.internCandidate(name_store, mark_, ty);
    }

    pub const RecursiveLink = union(enum(u8)) {
        interned: TypeId,
        node: RecursiveNodeId,
        root,
    };

    pub const RecursiveNodeId = enum(u32) { _ };

    pub fn recursiveNodeId(index: usize) RecursiveNodeId {
        return @enumFromInt(@as(u32, @intCast(index)));
    }

    pub const RecursiveField = struct {
        name: names.RecordFieldNameId,
        ty: RecursiveLink,
    };

    pub const RecursiveTag = struct {
        name: names.TagNameId,
        checked_name: names.TagNameId,
        payloads: []const RecursiveLink,
    };

    pub const RecursiveNamedBacking = struct {
        ty: RecursiveLink,
        use: BackingUse,
    };

    pub const RecursiveNamed = struct {
        named_type: NamedType,
        def: TypeDef,
        kind: NamedKind,
        builtin_owner: ?static_dispatch.BuiltinOwner = null,
        args: []const RecursiveLink,
        backing: ?RecursiveNamedBacking = null,
        declared_order: Span = Span.empty(),
    };

    pub const RecursiveContent = union(enum(u8)) {
        primitive: Primitive,
        named: RecursiveNamed,
        record: []const RecursiveField,
        tuple: []const RecursiveLink,
        tag_union: []const RecursiveTag,
        list: RecursiveLink,
        box: RecursiveLink,
        func: struct {
            args: []const RecursiveLink,
            ret: RecursiveLink,
        },
        erased: names.TypeDigest,
        zst,
    };

    /// Intern one recursive root without exposing the reserved root id before
    /// its content has been sealed. The input may refer to the root with
    /// `RecursiveLink.root`; every other child must already be an immutable
    /// interned `TypeId`.
    pub fn internRecursiveRoot(self: *Store, name_store: *const names.NameStore, content: RecursiveContent) std.mem.Allocator.Error!TypeId {
        return try self.internRecursiveGroupRoot(name_store, &.{content}, recursiveNodeId(0));
    }

    /// Intern one public root from a private recursive group. Group nodes may
    /// reference each other through `RecursiveLink.node`; only the selected root
    /// is returned to the caller, and it is returned only after every private
    /// node has been filled exactly once.
    ///
    /// With interning enabled, every member of the group is registered by its
    /// rooted digest, so a later equivalent rooted graph deduplicates to an
    /// existing member regardless of which node it is entered from, and members
    /// equal as roots share one id. Without interning, the group is built into
    /// fresh slots and the selected root returned with no dedup or registration.
    pub fn internRecursiveGroupRoot(
        self: *Store,
        name_store: *const names.NameStore,
        contents: []const RecursiveContent,
        root_node: RecursiveNodeId,
    ) std.mem.Allocator.Error!TypeId {
        const root_index = @intFromEnum(root_node);
        if (root_index >= contents.len) {
            Common.invariant("Monotype recursive type group root is outside the group");
        }
        const ids = try self.allocator.alloc(TypeId, contents.len);
        defer self.allocator.free(ids);
        try self.internRecursiveGroupInto(name_store, contents, ids);
        return ids[root_index];
    }

    /// Build a whole recursive group and write every member's resolved id into
    /// `out_ids` (one per `contents` entry, same order). Callers that need more
    /// than one member's id — moving a cyclic component with several entry
    /// points into the pool — read them all from one build instead of
    /// re-entering the group per member, which a symmetric collapse would
    /// otherwise duplicate (a collapsed self-referential member and its
    /// un-collapsed cyclic form are equal but digest differently).
    pub fn internRecursiveGroupInto(
        self: *Store,
        name_store: *const names.NameStore,
        contents: []const RecursiveContent,
        out_ids: []TypeId,
    ) std.mem.Allocator.Error!void {
        if (out_ids.len != contents.len) {
            Common.invariant("Monotype recursive group output length must match the group");
        }

        const allocator = self.allocator;

        // Phase A: build the group into provisional slots so each member has a
        // rooted digest. `.node` links resolve to sibling members. `.root`
        // links are meaningful only for a single-node group, so they resolve to
        // member zero.
        const provisional_mark = self.mark();
        errdefer self.restore(provisional_mark);

        const provisional = try allocator.alloc(TypeId, contents.len);
        defer allocator.free(provisional);
        for (provisional) |*id| id.* = try self.reserveSlot();
        for (contents, 0..) |content, index| {
            const lowered = try self.lowerRecursiveContent(name_store, provisional, provisional[0], content);
            self.fillReservedSlot(provisional[index], lowered);
        }

        // Without dedup the provisional slots are the final ids for every member.
        if (self.intern_buckets == null) {
            @memcpy(out_ids, provisional);
            return;
        }

        // Phase B: resolve each member to an existing id where possible. A
        // member equal as a root to an existing pool type reuses it; a member
        // equal to an earlier member of this same group shares that member's
        // id; the rest are new.
        const existing_id = try allocator.alloc(?TypeId, contents.len);
        defer allocator.free(existing_id);
        const representative = try allocator.alloc(usize, contents.len);
        defer allocator.free(representative);
        var has_dedup = false;
        for (provisional, 0..) |member, index| {
            existing_id[index] = null;
            representative[index] = index;
            for (0..index) |earlier| {
                if (try self.typeEql(name_store, provisional[earlier], member)) {
                    representative[index] = representative[earlier];
                    has_dedup = true;
                    break;
                }
            }
            if (representative[index] == index) {
                existing_id[index] = try self.findExistingRoot(name_store, member);
                if (existing_id[index] != null) has_dedup = true;
            }
        }

        // Common case: every member is a distinct new root. Keep the provisional
        // slots and register each so any entry node deduplicates a future graph.
        if (!has_dedup) {
            for (provisional) |member| try self.registerRoot(name_store, member);
            census.bump("intern_miss");
            @memcpy(out_ids, provisional);
            return;
        }

        // Phase C: some member deduplicated. Drop the provisional slots and
        // build only the genuinely new representatives, resolving every
        // reference through the resolved-id mapping.
        self.restore(provisional_mark);

        const final_ids = try allocator.alloc(TypeId, contents.len);
        defer allocator.free(final_ids);
        for (representative, 0..) |rep, index| {
            if (rep != index) continue;
            if (existing_id[index]) |existing| {
                final_ids[index] = existing;
            } else {
                final_ids[index] = try self.reserveSlot();
            }
        }
        for (representative, 0..) |rep, index| {
            if (rep != index) final_ids[index] = final_ids[rep];
        }
        for (representative, 0..) |rep, index| {
            if (rep != index or existing_id[index] != null) continue;
            const lowered = try self.lowerRecursiveContent(name_store, final_ids, final_ids[0], contents[index]);
            self.fillReservedSlot(final_ids[index], lowered);
        }
        for (representative, 0..) |rep, index| {
            if (rep != index or existing_id[index] != null) continue;
            try self.registerRoot(name_store, final_ids[index]);
        }
        census.bump("intern_miss");
        @memcpy(out_ids, final_ids);
    }

    /// Look up a type equal as a root to `member` among the registered pool
    /// types, returning it if one exists. Only pre-registered types are probed,
    /// so a provisional group member never matches itself or a sibling here.
    fn findExistingRoot(self: *Store, name_store: *const names.NameStore, member: TypeId) std.mem.Allocator.Error!?TypeId {
        const digest = self.typeDigestCached(name_store, member, null);
        const key = InternerLookupDigest.from(digest);
        if (self.intern_buckets.?.getPtr(key)) |bucket| {
            for (bucket.items) |existing| {
                if (try self.typeEql(name_store, existing, member)) {
                    return existing;
                }
            }
        }
        return null;
    }

    /// Register `member` under its rooted digest so a future equivalent graph
    /// entered at this node deduplicates to it.
    fn registerRoot(self: *Store, name_store: *const names.NameStore, member: TypeId) std.mem.Allocator.Error!void {
        const digest = self.typeDigestCached(name_store, member, null);
        const key = InternerLookupDigest.from(digest);
        const gop = try self.intern_buckets.?.getOrPut(key);
        if (!gop.found_existing) gop.value_ptr.* = std.ArrayList(TypeId).empty;
        try gop.value_ptr.append(self.allocator, member);
    }

    fn lowerRecursiveLink(_: *Store, ids: []const TypeId, root: TypeId, link: RecursiveLink) TypeId {
        return switch (link) {
            .interned => |ty| ty,
            .node => |node| blk: {
                const raw = @intFromEnum(node);
                if (raw >= ids.len) Common.invariant("Monotype recursive type reference is outside the group");
                break :blk ids[raw];
            },
            .root => root,
        };
    }

    fn lowerRecursiveLinkSpan(
        self: *Store,
        ids: []const TypeId,
        root: TypeId,
        links: []const RecursiveLink,
    ) std.mem.Allocator.Error!Span {
        if (links.len == 0) return .empty();
        const lowered = try self.allocator.alloc(TypeId, links.len);
        defer self.allocator.free(lowered);
        for (links, 0..) |link, index| {
            lowered[index] = self.lowerRecursiveLink(ids, root, link);
        }
        return try self.addSpan(lowered);
    }

    fn lowerRecursiveFields(
        self: *Store,
        name_store: *const names.NameStore,
        ids: []const TypeId,
        root: TypeId,
        fields: []const RecursiveField,
    ) std.mem.Allocator.Error!Span {
        if (fields.len == 0) return .empty();
        const lowered = try self.allocator.alloc(Field, fields.len);
        defer self.allocator.free(lowered);
        for (fields, 0..) |field, index| {
            lowered[index] = .{
                .name = field.name,
                .ty = self.lowerRecursiveLink(ids, root, field.ty),
            };
        }
        return try self.addRecordFields(name_store, lowered);
    }

    fn lowerRecursiveTags(
        self: *Store,
        name_store: *const names.NameStore,
        ids: []const TypeId,
        root: TypeId,
        tags_: []const RecursiveTag,
    ) std.mem.Allocator.Error!Span {
        if (tags_.len == 0) return .empty();
        const lowered = try self.allocator.alloc(Tag, tags_.len);
        defer self.allocator.free(lowered);
        for (tags_, 0..) |tag, index| {
            lowered[index] = .{
                .name = tag.name,
                .checked_name = tag.checked_name,
                .payloads = try self.lowerRecursiveLinkSpan(ids, root, tag.payloads),
            };
        }
        return try self.addTagVariants(name_store, lowered);
    }

    fn lowerRecursiveNamed(
        self: *Store,
        ids: []const TypeId,
        root: TypeId,
        named: RecursiveNamed,
    ) std.mem.Allocator.Error!NamedContent {
        return .{
            .named_type = named.named_type,
            .def = named.def,
            .kind = named.kind,
            .builtin_owner = named.builtin_owner,
            .args = try self.lowerRecursiveLinkSpan(ids, root, named.args),
            .backing = if (named.backing) |backing| .{
                .ty = self.lowerRecursiveLink(ids, root, backing.ty),
                .use = backing.use,
            } else null,
            .declared_order = named.declared_order,
        };
    }

    fn lowerRecursiveContent(
        self: *Store,
        name_store: *const names.NameStore,
        ids: []const TypeId,
        root: TypeId,
        content: RecursiveContent,
    ) std.mem.Allocator.Error!Content {
        return switch (content) {
            .primitive => |primitive| .{ .primitive = primitive },
            .named => |named| .{ .named = try self.lowerRecursiveNamed(ids, root, named) },
            .record => |fields| .{ .record = try self.lowerRecursiveFields(name_store, ids, root, fields) },
            .tuple => |items| .{ .tuple = try self.lowerRecursiveLinkSpan(ids, root, items) },
            .tag_union => |tags_| .{ .tag_union = try self.lowerRecursiveTags(name_store, ids, root, tags_) },
            .list => |elem| .{ .list = self.lowerRecursiveLink(ids, root, elem) },
            .box => |elem| .{ .box = self.lowerRecursiveLink(ids, root, elem) },
            .func => |function| .{ .func = .{
                .args = try self.lowerRecursiveLinkSpan(ids, root, function.args),
                .ret = self.lowerRecursiveLink(ids, root, function.ret),
            } },
            .erased => |digest| .{ .erased = digest },
            .zst => .zst,
        };
    }

    fn internCandidate(self: *Store, name_store: *const names.NameStore, mark_: Mark, candidate: TypeId) std.mem.Allocator.Error!TypeId {
        // Plain-add mode: the candidate is already installed; hand back its id.
        if (self.intern_buckets == null) return candidate;

        errdefer self.restore(mark_);
        const digest = self.typeDigestCached(name_store, candidate, null);
        const key = InternerLookupDigest.from(digest);
        const buckets = &self.intern_buckets.?;
        if (buckets.getPtr(key)) |bucket| {
            for (bucket.items) |existing| {
                if (try self.typeEql(name_store, existing, candidate)) {
                    self.restore(mark_);
                    census.bump("intern_hit");
                    return existing;
                }
            }
            try bucket.append(self.allocator, candidate);
            census.bump("intern_miss");
            return candidate;
        }

        var bucket = std.ArrayList(TypeId).empty;
        errdefer bucket.deinit(self.allocator);
        try bucket.append(self.allocator, candidate);
        try buckets.put(key, bucket);
        census.bump("intern_miss");
        return candidate;
    }

    const Mark = struct {
        types_len: usize,
        type_digests_len: usize,
        specialization_digests_len: usize,
        type_digest_generations_len: usize,
        specialization_digest_generations_len: usize,
        type_epochs_len: usize,
        spans_len: usize,
        fields_len: usize,
        tags_len: usize,
        declared_fields_len: usize,
    };

    fn mark(self: *const Store) Mark {
        return .{
            .types_len = self.types.len(),
            .type_digests_len = self.type_digests.len(),
            .specialization_digests_len = self.specialization_digests.len(),
            .type_digest_generations_len = self.type_digest_generations.len(),
            .specialization_digest_generations_len = self.specialization_digest_generations.len(),
            .type_epochs_len = self.type_epochs.len(),
            .spans_len = self.spans.len(),
            .fields_len = self.fields.len(),
            .tags_len = self.tags.len(),
            .declared_fields_len = self.declared_fields.len(),
        };
    }

    fn restore(self: *Store, mark_: Mark) void {
        self.assertMutable();
        self.types.restoreLen(mark_.types_len);
        self.type_digests.restoreLen(mark_.type_digests_len);
        self.specialization_digests.restoreLen(mark_.specialization_digests_len);
        self.type_digest_generations.restoreLen(mark_.type_digest_generations_len);
        self.specialization_digest_generations.restoreLen(mark_.specialization_digest_generations_len);
        self.type_epochs.restoreLen(mark_.type_epochs_len);
        self.spans.restoreLen(mark_.spans_len);
        self.fields.restoreLen(mark_.fields_len);
        self.tags.restoreLen(mark_.tags_len);
        self.declared_fields.restoreLen(mark_.declared_fields_len);
    }

    /// Resolve `ty` through alias `named` nodes to the content that names its
    /// dispatch head. Aliases are transparent for static dispatch, mirroring
    /// the alias-transparent digest path: alias-over-alias and
    /// alias-over-nominal unwrap uniformly (the backing of an
    /// alias-over-nominal is itself a `named` node carrying the nominal's
    /// identity). An alias `named` node carrying a builtin owner, or one with
    /// no backing, is returned as-is. The walk terminates because alias
    /// chains in checked output are finite.
    pub fn dispatchHeadContent(self: *const Store, ty: TypeId) Content {
        var current = ty;
        while (true) {
            const content = self.get(current);
            switch (content) {
                .named => |named| {
                    if (named.builtin_owner == null and named.kind == .alias) {
                        if (named.backing) |backing| {
                            current = backing.ty;
                            continue;
                        }
                    }
                    return content;
                },
                else => return content,
            }
        }
    }

    pub fn typeDigest(self: *const Store, name_store: *const names.NameStore, ty: TypeId) names.TypeDigest {
        var hasher = std.crypto.hash.sha2.Sha256.init(.{});
        var visiting = DigestVisiting{};
        var strategy = UncachedDigestStrategy{ .store = self, .name_store = name_store, .visiting = &visiting };
        strategy.child(&hasher, ty, .full);
        return .{ .bytes = hasher.finalResult() };
    }

    pub fn specializationDigest(self: *const Store, name_store: *const names.NameStore, ty: TypeId) names.TypeDigest {
        var hasher = std.crypto.hash.sha2.Sha256.init(.{});
        var visiting = DigestVisiting{};
        var strategy = UncachedDigestStrategy{ .store = self, .name_store = name_store, .visiting = &visiting };
        strategy.child(&hasher, ty, .identity_only);
        return .{ .bytes = hasher.finalResult() };
    }

    pub const DigestStats = struct {
        cache_hits: u64 = 0,
        cache_misses: u64 = 0,
        nodes_visited: u64 = 0,
    };

    pub const VerifyError = enum {
        type_digest_count_mismatch,
        type_span_out_of_bounds,
        field_span_out_of_bounds,
        tag_span_out_of_bounds,
        declared_field_span_out_of_bounds,
        type_ref_out_of_bounds,
        record_fields_not_sorted,
        tag_union_tags_not_sorted,
    };

    pub const View = struct {
        types: []const Content,
        type_digests: []const ?names.TypeDigest,
        spans: []const TypeId,
        fields: []const Field,
        tags: []const Tag,
        declared_fields: []const DeclaredField,
        frozen: bool,

        pub fn get(self: View, ty: TypeId) Content {
            return self.types[@intFromEnum(ty)];
        }

        pub fn span(self: View, span_: Span) []const TypeId {
            return self.spans[span_.start..][0..span_.len];
        }

        pub fn fieldSpan(self: View, span_: Span) []const Field {
            return self.fields[span_.start..][0..span_.len];
        }

        pub fn tagSpan(self: View, span_: Span) []const Tag {
            return self.tags[span_.start..][0..span_.len];
        }

        pub fn declaredFieldSpan(self: View, span_: Span) []const DeclaredField {
            return self.declared_fields[span_.start..][0..span_.len];
        }

        pub fn typeEql(
            self: View,
            allocator: std.mem.Allocator,
            name_store: *const names.NameStore,
            lhs: TypeId,
            rhs: TypeId,
        ) std.mem.Allocator.Error!bool {
            return try typeViewEql(self, allocator, name_store, lhs, rhs);
        }

        pub fn verify(self: View, name_store: *const names.NameStore) ?VerifyError {
            if (self.type_digests.len != self.types.len) return .type_digest_count_mismatch;

            for (self.spans) |ty| {
                if (!self.typeRefInBounds(ty)) return .type_ref_out_of_bounds;
            }
            for (self.fields) |field| {
                if (!self.typeRefInBounds(field.ty)) return .type_ref_out_of_bounds;
            }
            for (self.tags) |tag| {
                if (!self.spanInBounds(self.spans.len, tag.payloads)) return .type_span_out_of_bounds;
                if (self.verifyTypeSpan(tag.payloads)) |err| return err;
            }
            for (self.declared_fields) |field| {
                switch (field) {
                    .named => {},
                    .padding => |ty| if (!self.typeRefInBounds(ty)) return .type_ref_out_of_bounds,
                }
            }

            for (self.types) |content| {
                switch (content) {
                    .primitive, .erased, .zst => {},
                    .list, .box => |ty| if (!self.typeRefInBounds(ty)) return .type_ref_out_of_bounds,
                    .tuple => |span_| if (self.verifyTypeSpan(span_)) |err| return err,
                    .record => |span_| if (self.verifyFieldSpan(name_store, span_)) |err| return err,
                    .tag_union => |span_| if (self.verifyTagSpan(name_store, span_)) |err| return err,
                    .func => |func| {
                        if (self.verifyTypeSpan(func.args)) |err| return err;
                        if (!self.typeRefInBounds(func.ret)) return .type_ref_out_of_bounds;
                    },
                    .named => |named| {
                        if (self.verifyTypeSpan(named.args)) |err| return err;
                        if (named.backing) |backing| {
                            if (!self.typeRefInBounds(backing.ty)) return .type_ref_out_of_bounds;
                        }
                        if (self.verifyDeclaredFieldSpan(named.declared_order)) |err| return err;
                    },
                }
            }

            return null;
        }

        fn typeRefInBounds(self: View, ty: TypeId) bool {
            return @intFromEnum(ty) < self.types.len;
        }

        fn spanInBounds(_: View, len: usize, span_: Span) bool {
            const start: usize = span_.start;
            const span_len: usize = span_.len;
            return start <= len and span_len <= len - start;
        }

        fn verifyTypeSpan(self: View, span_: Span) ?VerifyError {
            if (!self.spanInBounds(self.spans.len, span_)) return .type_span_out_of_bounds;
            for (self.span(span_)) |ty| {
                if (!self.typeRefInBounds(ty)) return .type_ref_out_of_bounds;
            }
            return null;
        }

        fn verifyFieldSpan(self: View, name_store: *const names.NameStore, span_: Span) ?VerifyError {
            if (!self.spanInBounds(self.fields.len, span_)) return .field_span_out_of_bounds;
            const fields_ = self.fieldSpan(span_);
            for (fields_) |field| {
                if (!self.typeRefInBounds(field.ty)) return .type_ref_out_of_bounds;
            }
            if (fields_.len > 1) {
                for (fields_[1..], 1..) |field, index| {
                    if (!name_store.recordFieldLabelTextLessThan(fields_[index - 1].name, field.name)) {
                        return .record_fields_not_sorted;
                    }
                }
            }
            return null;
        }

        fn verifyTagSpan(self: View, name_store: *const names.NameStore, span_: Span) ?VerifyError {
            if (!self.spanInBounds(self.tags.len, span_)) return .tag_span_out_of_bounds;
            const tags_ = self.tagSpan(span_);
            for (tags_) |tag| {
                if (self.verifyTypeSpan(tag.payloads)) |err| return err;
            }
            if (tags_.len > 1) {
                for (tags_[1..], 1..) |tag, index| {
                    if (!name_store.tagLabelTextLessThan(tags_[index - 1].name, tag.name)) {
                        return .tag_union_tags_not_sorted;
                    }
                }
            }
            return null;
        }

        fn verifyDeclaredFieldSpan(self: View, span_: Span) ?VerifyError {
            if (!self.spanInBounds(self.declared_fields.len, span_)) return .declared_field_span_out_of_bounds;
            for (self.declaredFieldSpan(span_)) |field| {
                switch (field) {
                    .named => {},
                    .padding => |ty| if (!self.typeRefInBounds(ty)) return .type_ref_out_of_bounds,
                }
            }
            return null;
        }
    };

    pub fn view(self: *const Store) View {
        return .{
            .types = self.types.unsafeRawItemsForView(),
            .type_digests = self.type_digests.unsafeRawItemsForView(),
            .spans = self.spans.unsafeRawItemsForView(),
            .fields = self.fields.unsafeRawItemsForView(),
            .tags = self.tags.unsafeRawItemsForView(),
            .declared_fields = self.declared_fields.unsafeRawItemsForView(),
            .frozen = self.frozen,
        };
    }

    pub fn verify(self: *const Store, name_store: *const names.NameStore) ?VerifyError {
        return self.view().verify(name_store);
    }

    pub fn specializationDigestsView(self: *const Store) []const ?names.TypeDigest {
        return self.specialization_digests.unsafeRawItemsForView();
    }

    pub fn typeDigestCached(
        self: *Store,
        name_store: *const names.NameStore,
        ty: TypeId,
        stats: ?*DigestStats,
    ) names.TypeDigest {
        var ctx = CachedDigestContext{};
        return self.cachedDigestInner(name_store, ty, .full, &ctx, stats);
    }

    pub fn specializationDigestCached(
        self: *Store,
        name_store: *const names.NameStore,
        ty: TypeId,
        stats: ?*DigestStats,
    ) names.TypeDigest {
        var ctx = CachedDigestContext{};
        return self.cachedDigestInner(name_store, ty, .identity_only, &ctx, stats);
    }

    /// Exact structural equality for closed Monotype types.
    ///
    /// Equality consumes the same identity-field visitor as the digest paths.
    /// Pair memoization and alias unwrapping stay local to the comparator, so
    /// equality remains the exact confirmation step before one specialization
    /// can reuse another.
    pub fn typeEql(
        self: *const Store,
        name_store: *const names.NameStore,
        lhs: TypeId,
        rhs: TypeId,
    ) std.mem.Allocator.Error!bool {
        return try self.view().typeEql(self.allocator, name_store, lhs, rhs);
    }

    /// Stack of types currently being digested. Recursive types reference a
    /// type already on this stack; the digest encodes such a back reference by
    /// stack position so cyclic content digests deterministically.
    const DigestVisiting = struct {
        items: [digest_visiting_max]TypeId = undefined,
        len: usize = 0,
    };

    const digest_visiting_max = 256;

    const CachedDigestContext = struct {
        items: [digest_visiting_max]TypeId = undefined,
        len: usize = 0,
        saw_cycle: bool = false,
    };

    fn clearTypeDigestCache(self: *Store) void {
        if (self.digest_cache_generation == std.math.maxInt(u64)) Common.invariant("Monotype digest cache generation exhausted");
        self.digest_cache_generation += 1;
    }

    fn assertMutable(self: *const Store) void {
        if (self.frozen) Common.invariant("frozen Monotype type store cannot be mutated");
    }

    fn typeRefInBounds(self: *const Store, ty: TypeId) bool {
        return @intFromEnum(ty) < self.types.len();
    }

    fn spanInBounds(_: *const Store, len: usize, span_: Span) bool {
        const start: usize = span_.start;
        const span_len: usize = span_.len;
        return start <= len and span_len <= len - start;
    }

    fn verifyTypeSpan(self: *const Store, span_: Span) ?VerifyError {
        if (!self.spanInBounds(self.spans.len(), span_)) return .type_span_out_of_bounds;
        for (self.span(span_)) |ty| {
            if (!self.typeRefInBounds(ty)) return .type_ref_out_of_bounds;
        }
        return null;
    }

    fn verifyFieldSpan(self: *const Store, name_store: *const names.NameStore, span_: Span) ?VerifyError {
        if (!self.spanInBounds(self.fields.len(), span_)) return .field_span_out_of_bounds;
        const fields_ = self.fieldSpan(span_);
        for (fields_) |field| {
            if (!self.typeRefInBounds(field.ty)) return .type_ref_out_of_bounds;
        }
        if (fields_.len > 1) {
            for (fields_[1..], 1..) |field, index| {
                if (!name_store.recordFieldLabelTextLessThan(fields_[index - 1].name, field.name)) {
                    return .record_fields_not_sorted;
                }
            }
        }
        return null;
    }

    fn verifyTagSpan(self: *const Store, name_store: *const names.NameStore, span_: Span) ?VerifyError {
        if (!self.spanInBounds(self.tags.len(), span_)) return .tag_span_out_of_bounds;
        const tags_ = self.tagSpan(span_);
        for (tags_) |tag| {
            if (self.verifyTypeSpan(tag.payloads)) |err| return err;
        }
        if (tags_.len > 1) {
            for (tags_[1..], 1..) |tag, index| {
                if (!name_store.tagLabelTextLessThan(tags_[index - 1].name, tag.name)) {
                    return .tag_union_tags_not_sorted;
                }
            }
        }
        return null;
    }

    fn verifyDeclaredFieldSpan(self: *const Store, span_: Span) ?VerifyError {
        if (!self.spanInBounds(self.declared_fields.len(), span_)) return .declared_field_span_out_of_bounds;
        for (self.declaredFieldSpan(span_)) |field| {
            switch (field) {
                .named => {},
                .padding => |ty| if (!self.typeRefInBounds(ty)) return .type_ref_out_of_bounds,
            }
        }
        return null;
    }

    fn cachedDigestInner(
        self: *Store,
        name_store: *const names.NameStore,
        ty: TypeId,
        named_mode: NamedDigestMode,
        ctx: *CachedDigestContext,
        stats: ?*DigestStats,
    ) names.TypeDigest {
        for (ctx.items[0..ctx.len], 0..) |open_ty, position| {
            if (open_ty == ty) {
                ctx.saw_cycle = true;
                return cycleDigest(@intCast(position));
            }
        }

        const index = @intFromEnum(ty);
        switch (named_mode) {
            .full => {
                if (self.type_digest_generations.unsafeRawItemsForView()[index] == self.digest_cache_generation) {
                    if (self.type_digests.unsafeRawItemsForView()[index]) |digest| {
                        if (stats) |s| s.cache_hits += 1;
                        return digest;
                    }
                }
            },
            .identity_only => {
                if (self.specialization_digest_generations.unsafeRawItemsForView()[index] == self.digest_cache_generation) {
                    if (self.specialization_digests.unsafeRawItemsForView()[index]) |digest| {
                        if (stats) |s| s.cache_hits += 1;
                        return digest;
                    }
                }
            },
        }

        if (stats) |s| {
            s.cache_misses += 1;
            s.nodes_visited += 1;
        }

        if (ctx.len == digest_visiting_max) {
            ctx.saw_cycle = true;
            return deepDigest(std.meta.activeTag(self.get(ty)));
        }

        ctx.items[ctx.len] = ty;
        ctx.len += 1;
        const saw_cycle_before = ctx.saw_cycle;
        var hasher = std.crypto.hash.sha2.Sha256.init(.{});
        var strategy = CachedDigestStrategy{ .store = self, .name_store = name_store, .ctx = ctx, .stats = stats };
        const direct_digest = self.writeIdentityDigest(name_store, &hasher, ty, named_mode, &strategy);
        ctx.len -= 1;

        const digest: names.TypeDigest = direct_digest orelse .{ .bytes = hasher.finalResult() };
        if (ctx.saw_cycle == saw_cycle_before) {
            switch (named_mode) {
                .full => {
                    self.type_digests.set(index, digest);
                    self.type_digest_generations.set(index, self.digest_cache_generation);
                },
                .identity_only => {
                    self.specialization_digests.set(index, digest);
                    self.specialization_digest_generations.set(index, self.digest_cache_generation);
                },
            }
        }
        return digest;
    }

    /// Folds `child_ty` into `hasher` by inlining the child's identity bytes
    /// directly into the running hash, tracking the visiting stack so a
    /// recursive reference back to an in-progress type digests as a stable
    /// back reference by stack position. This is the cycle-tracking and
    /// child-emission strategy behind `typeDigest` / `specializationDigest`.
    const UncachedDigestStrategy = struct {
        store: *const Store,
        name_store: *const names.NameStore,
        visiting: *DigestVisiting,

        fn child(
            self: *UncachedDigestStrategy,
            hasher: *std.crypto.hash.sha2.Sha256,
            child_ty: TypeId,
            named_mode: NamedDigestMode,
        ) void {
            for (self.visiting.items[0..self.visiting.len], 0..) |open_ty, position| {
                if (open_ty == child_ty) {
                    writeBytes(hasher, "cycle");
                    writeU32(hasher, @intCast(position));
                    return;
                }
            }
            if (self.visiting.len == digest_visiting_max) {
                writeDeepShape(hasher, std.meta.activeTag(self.store.get(child_ty)));
                return;
            }
            self.visiting.items[self.visiting.len] = child_ty;
            self.visiting.len += 1;
            defer self.visiting.len -= 1;
            _ = self.store.writeIdentityDigest(self.name_store, hasher, child_ty, named_mode, self);
        }

        fn transparent(
            self: *UncachedDigestStrategy,
            hasher: *std.crypto.hash.sha2.Sha256,
            child_ty: TypeId,
            named_mode: NamedDigestMode,
        ) ?names.TypeDigest {
            self.child(hasher, child_ty, named_mode);
            return null;
        }
    };

    /// Folds `child_ty` into `hasher` as a nested sub-digest: the tag
    /// `"type-digest"` followed by the child's own cached digest. Computing
    /// parent digests from cached child digests is what lets structurally
    /// growing records and function types reuse child work instead of walking
    /// their whole prefix, and it is the cycle-tracking and child-emission
    /// strategy behind `typeDigestCached` / `specializationDigestCached`.
    const CachedDigestStrategy = struct {
        store: *Store,
        name_store: *const names.NameStore,
        ctx: *CachedDigestContext,
        stats: ?*DigestStats,

        fn child(
            self: *CachedDigestStrategy,
            hasher: *std.crypto.hash.sha2.Sha256,
            child_ty: TypeId,
            named_mode: NamedDigestMode,
        ) void {
            writeBytes(hasher, "type-digest");
            const digest = self.store.cachedDigestInner(self.name_store, child_ty, named_mode, self.ctx, self.stats);
            hasher.update(&digest.bytes);
        }

        fn transparent(
            self: *CachedDigestStrategy,
            _: *std.crypto.hash.sha2.Sha256,
            child_ty: TypeId,
            named_mode: NamedDigestMode,
        ) ?names.TypeDigest {
            return self.store.cachedDigestInner(self.name_store, child_ty, named_mode, self.ctx, self.stats);
        }
    };

    fn DigestIdentityDriver(comptime Strategy: type) type {
        return struct {
            const Self = @This();
            const Error = error{};

            store: *const Store,
            name_store: *const names.NameStore,
            hasher: *std.crypto.hash.sha2.Sha256,
            content: Content,
            named_mode: NamedDigestMode,
            strategy: Strategy,
            direct_digest: ?names.TypeDigest = null,

            fn contentTag(self: *Self) ?ContentTag {
                return std.meta.activeTag(self.content);
            }

            fn alias(self: *Self) Error!bool {
                const named = self.content.named;
                if (named.kind != .alias) return false;
                // A builtin-owned alias carries an explicit checked dispatch
                // owner that must survive interning, so it is nontransparent:
                // fall through to the named identity walk, which hashes the
                // owner. Only a storage-transparent alias — backed, with no
                // builtin owner — unwraps to its backing here.
                if (named.builtin_owner != null) return false;
                const backing = named.backing orelse {
                    writeBytes(self.hasher, "alias-without-backing");
                    return true;
                };
                self.direct_digest = self.strategy.transparent(self.hasher, backing.ty, self.named_mode);
                return true;
            }

            fn beginVariant(self: *Self, comptime label: []const u8) Error!bool {
                writeBytes(self.hasher, label);
                return true;
            }

            fn primitive(self: *Self) Error!bool {
                writeBytes(self.hasher, @tagName(self.content.primitive));
                return true;
            }

            fn namedModule(self: *Self) Error!bool {
                self.hasher.update(&self.content.named.named_type.module.bytes);
                return true;
            }

            fn namedDefModule(self: *Self) Error!bool {
                writeBytes(self.hasher, self.name_store.moduleIdentityBytes(self.content.named.def.module));
                return true;
            }

            fn namedSourceDecl(self: *Self) Error!bool {
                writeOptionalU32(self.hasher, self.content.named.def.source_decl);
                return true;
            }

            fn namedSourceDeclIsAbsent(self: *Self) bool {
                return self.content.named.def.source_decl == null;
            }

            fn namedTypeName(self: *Self) Error!bool {
                writeBytes(self.hasher, self.name_store.typeNameText(self.content.named.def.type_name));
                return true;
            }

            fn namedGenerated(self: *Self) Error!bool {
                writeOptionalDigest(self.hasher, self.content.named.def.generated);
                return true;
            }

            fn namedIteratorRepresentation(self: *Self) Error!bool {
                writeBytes(self.hasher, @tagName(self.content.named.def.iterator_representation));
                return true;
            }

            fn namedIteratorKind(self: *Self) Error!bool {
                writeBytes(self.hasher, @tagName(self.content.named.def.iterator_kind));
                return true;
            }

            fn namedIteratorDepth(self: *Self) Error!bool {
                writeU32(self.hasher, self.content.named.def.iterator_depth);
                return true;
            }

            fn namedKind(self: *Self) Error!bool {
                writeBytes(self.hasher, @tagName(self.content.named.kind));
                return true;
            }

            fn namedBuiltinOwnerField(self: *Self) Error!bool {
                if (self.content.named.builtin_owner) |owner| {
                    writeBytes(self.hasher, "builtin");
                    writeBytes(self.hasher, @tagName(owner));
                } else {
                    writeBytes(self.hasher, "not-builtin");
                }
                return true;
            }

            fn namedBuiltinOwnerValue(self: *Self) ?static_dispatch.BuiltinOwner {
                return self.content.named.builtin_owner;
            }

            fn namedMode(self: *Self) NamedDigestMode {
                return self.named_mode;
            }

            fn specializationBuiltinBackingMarker(self: *Self) Error!bool {
                writeBytes(self.hasher, "specialization-builtin-backing");
                return true;
            }

            fn specializationNamedIdentityMarker(self: *Self) Error!bool {
                writeBytes(self.hasher, "specialization-named-identity");
                return true;
            }

            fn typeSpan(self: *Self, role: IdentityTypeSpan) Span {
                return switch (role) {
                    .named_args => self.content.named.args,
                    .tuple_items => self.content.tuple,
                    .func_args => self.content.func.args,
                };
            }

            fn typeSpanLen(self: *Self, role: IdentityTypeSpan) Error!?usize {
                const values = self.store.span(self.typeSpan(role));
                writeU32(self.hasher, @intCast(values.len));
                return values.len;
            }

            fn typeSpanChild(self: *Self, role: IdentityTypeSpan, index: usize) Error!bool {
                const values = self.store.span(self.typeSpan(role));
                self.strategy.child(self.hasher, GuardedList.at(values, index), self.named_mode);
                return true;
            }

            fn beginBacking(self: *Self) Error!bool {
                writeBytes(self.hasher, "backing");
                return true;
            }

            fn backingPresence(self: *Self) Error!?IdentityBacking {
                if (self.content.named.backing == null) {
                    writeBytes(self.hasher, "none");
                    return .none;
                }
                return .some;
            }

            fn backingUse(self: *Self) Error!bool {
                writeBytes(self.hasher, @tagName(self.content.named.backing.?.use));
                return true;
            }

            fn backingType(self: *Self) Error!bool {
                self.strategy.child(self.hasher, self.content.named.backing.?.ty, .full);
                return true;
            }

            fn beginDeclaredOrder(self: *Self) Error!bool {
                writeBytes(self.hasher, "declared_order");
                return true;
            }

            fn declaredOrderLen(self: *Self) Error!?usize {
                const entries = self.store.declaredFieldSpan(self.content.named.declared_order);
                writeU32(self.hasher, @intCast(entries.len));
                return entries.len;
            }

            fn declaredFieldTag(self: *Self, index: usize) Error!?DeclaredFieldTag {
                const entries = self.store.declaredFieldSpan(self.content.named.declared_order);
                return std.meta.activeTag(GuardedList.at(entries, index));
            }

            fn declaredFieldName(self: *Self, index: usize) Error!bool {
                const entries = self.store.declaredFieldSpan(self.content.named.declared_order);
                writeBytes(self.hasher, "named");
                writeBytes(self.hasher, self.name_store.recordFieldLabelText(GuardedList.at(entries, index).named));
                return true;
            }

            fn declaredPaddingType(self: *Self, index: usize) Error!bool {
                const entries = self.store.declaredFieldSpan(self.content.named.declared_order);
                writeBytes(self.hasher, "padding");
                self.strategy.child(self.hasher, GuardedList.at(entries, index).padding, .full);
                return true;
            }

            fn recordLen(self: *Self) Error!?usize {
                const field_slice = self.store.fieldSpan(self.content.record);
                writeU32(self.hasher, @intCast(field_slice.len));
                return field_slice.len;
            }

            fn recordFieldName(self: *Self, index: usize) Error!bool {
                const field_slice = self.store.fieldSpan(self.content.record);
                writeBytes(self.hasher, self.name_store.recordFieldLabelText(GuardedList.at(field_slice, index).name));
                return true;
            }

            fn recordFieldType(self: *Self, index: usize) Error!bool {
                const field_slice = self.store.fieldSpan(self.content.record);
                self.strategy.child(self.hasher, GuardedList.at(field_slice, index).ty, self.named_mode);
                return true;
            }

            fn tagUnionLen(self: *Self) Error!?usize {
                const tag_slice = self.store.tagSpan(self.content.tag_union);
                writeU32(self.hasher, @intCast(tag_slice.len));
                return tag_slice.len;
            }

            fn tagName(self: *Self, index: usize) Error!bool {
                const tag_slice = self.store.tagSpan(self.content.tag_union);
                writeBytes(self.hasher, self.name_store.tagLabelText(GuardedList.at(tag_slice, index).name));
                return true;
            }

            fn tagPayloadLen(self: *Self, tag_index: usize) Error!?usize {
                const tag_slice = self.store.tagSpan(self.content.tag_union);
                const payloads = self.store.span(GuardedList.at(tag_slice, tag_index).payloads);
                writeU32(self.hasher, @intCast(payloads.len));
                return payloads.len;
            }

            fn tagPayloadType(self: *Self, tag_index: usize, payload_index: usize) Error!bool {
                const tag_slice = self.store.tagSpan(self.content.tag_union);
                const payloads = self.store.span(GuardedList.at(tag_slice, tag_index).payloads);
                self.strategy.child(self.hasher, GuardedList.at(payloads, payload_index), self.named_mode);
                return true;
            }

            fn listElem(self: *Self) Error!bool {
                self.strategy.child(self.hasher, self.content.list, self.named_mode);
                return true;
            }

            fn boxElem(self: *Self) Error!bool {
                self.strategy.child(self.hasher, self.content.box, self.named_mode);
                return true;
            }

            fn funcRet(self: *Self) Error!bool {
                self.strategy.child(self.hasher, self.content.func.ret, self.named_mode);
                return true;
            }

            fn erased(self: *Self) Error!bool {
                self.hasher.update(&self.content.erased.bytes);
                return true;
            }
        };
    }

    /// Digest identity fields through the same visitor used by equality.
    /// Cached and uncached callers differ only in child folding and cycle state.
    fn writeIdentityDigest(
        self: *const Store,
        name_store: *const names.NameStore,
        hasher: *std.crypto.hash.sha2.Sha256,
        ty: TypeId,
        named_mode: NamedDigestMode,
        strategy: anytype,
    ) ?names.TypeDigest {
        var driver = DigestIdentityDriver(@TypeOf(strategy)){
            .store = self,
            .name_store = name_store,
            .hasher = hasher,
            .content = self.get(ty),
            .named_mode = named_mode,
            .strategy = strategy,
        };
        const ok = visitTypeIdentity(@TypeOf(driver), &driver) catch unreachable;
        if (!ok) unreachable;
        return driver.direct_digest;
    }
};

fn visitTypeIdentity(comptime Driver: type, driver: *Driver) Driver.Error!bool {
    const tag = driver.contentTag() orelse return false;
    return switch (tag) {
        .primitive => blk: {
            if (!try driver.beginVariant("primitive")) break :blk false;
            break :blk try driver.primitive();
        },
        .named => blk: {
            if (try driver.alias()) break :blk true;
            if (!try driver.beginVariant("named")) break :blk false;
            if (!try visitNamedIdentity(Driver, driver)) break :blk false;
            break :blk true;
        },
        .record => blk: {
            if (!try driver.beginVariant("record")) break :blk false;
            const len = (try driver.recordLen()) orelse break :blk false;
            for (0..len) |index| {
                if (!try driver.recordFieldName(index)) break :blk false;
                if (!try driver.recordFieldType(index)) break :blk false;
            }
            break :blk true;
        },
        .tuple => blk: {
            if (!try driver.beginVariant("tuple")) break :blk false;
            break :blk try visitTypeSpanIdentity(Driver, driver, .tuple_items);
        },
        .tag_union => blk: {
            if (!try driver.beginVariant("tag_union")) break :blk false;
            const len = (try driver.tagUnionLen()) orelse break :blk false;
            for (0..len) |tag_index| {
                if (!try driver.tagName(tag_index)) break :blk false;
                if (!try visitTagPayloadIdentity(Driver, driver, tag_index)) break :blk false;
            }
            break :blk true;
        },
        .list => blk: {
            if (!try driver.beginVariant("list")) break :blk false;
            break :blk try driver.listElem();
        },
        .box => blk: {
            if (!try driver.beginVariant("box")) break :blk false;
            break :blk try driver.boxElem();
        },
        .func => blk: {
            if (!try driver.beginVariant("func")) break :blk false;
            if (!try visitTypeSpanIdentity(Driver, driver, .func_args)) break :blk false;
            break :blk try driver.funcRet();
        },
        .erased => blk: {
            if (!try driver.beginVariant("erased")) break :blk false;
            break :blk try driver.erased();
        },
        .zst => try driver.beginVariant("zst"),
    };
}

fn visitNamedIdentity(comptime Driver: type, driver: *Driver) Driver.Error!bool {
    if (!try driver.namedModule()) return false;
    if (!try driver.namedDefModule()) return false;
    if (!try driver.namedSourceDecl()) return false;
    if (driver.namedSourceDeclIsAbsent()) {
        if (!try driver.namedTypeName()) return false;
    }
    if (!try driver.namedGenerated()) return false;
    if (!try driver.namedIteratorRepresentation()) return false;
    if (!try driver.namedIteratorKind()) return false;
    if (!try driver.namedIteratorDepth()) return false;
    if (!try driver.namedKind()) return false;
    if (!try driver.namedBuiltinOwnerField()) return false;
    if (!try visitTypeSpanIdentity(Driver, driver, .named_args)) return false;

    switch (driver.namedMode()) {
        .full => {
            if (!try visitNamedBackingIdentity(Driver, driver)) return false;
            return try visitDeclaredOrderIdentity(Driver, driver);
        },
        .identity_only => {
            if (driver.namedBuiltinOwnerValue()) |owner| {
                if (generatedEvidenceOwnerUsesBacking(owner)) {
                    if (!try driver.specializationBuiltinBackingMarker()) return false;
                    return try visitNamedBackingIdentity(Driver, driver);
                }
            }
            return try driver.specializationNamedIdentityMarker();
        },
    }
}

fn visitTypeSpanIdentity(comptime Driver: type, driver: *Driver, role: IdentityTypeSpan) Driver.Error!bool {
    const len = (try driver.typeSpanLen(role)) orelse return false;
    for (0..len) |index| {
        if (!try driver.typeSpanChild(role, index)) return false;
    }
    return true;
}

fn visitNamedBackingIdentity(comptime Driver: type, driver: *Driver) Driver.Error!bool {
    if (!try driver.beginBacking()) return false;
    switch ((try driver.backingPresence()) orelse return false) {
        .none => return true,
        .some => {
            if (!try driver.backingUse()) return false;
            return try driver.backingType();
        },
    }
}

fn visitDeclaredOrderIdentity(comptime Driver: type, driver: *Driver) Driver.Error!bool {
    if (!try driver.beginDeclaredOrder()) return false;
    const len = (try driver.declaredOrderLen()) orelse return false;
    for (0..len) |index| {
        switch ((try driver.declaredFieldTag(index)) orelse return false) {
            .named => if (!try driver.declaredFieldName(index)) return false,
            .padding => if (!try driver.declaredPaddingType(index)) return false,
        }
    }
    return true;
}

fn visitTagPayloadIdentity(comptime Driver: type, driver: *Driver, tag_index: usize) Driver.Error!bool {
    const len = (try driver.tagPayloadLen(tag_index)) orelse return false;
    for (0..len) |payload_index| {
        if (!try driver.tagPayloadType(tag_index, payload_index)) return false;
    }
    return true;
}

fn TypeViewIdentityEqlDriver(comptime TypeView: type) type {
    return struct {
        const Self = @This();
        const Error = std.mem.Allocator.Error;

        type_view: TypeView,
        name_store: *const names.NameStore,
        lhs_content: Content,
        rhs_content: Content,
        named_mode: NamedDigestMode,
        visited: *std.AutoHashMap(u128, void),

        fn contentTag(self: *Self) ?ContentTag {
            const lhs_tag = std.meta.activeTag(self.lhs_content);
            if (lhs_tag != std.meta.activeTag(self.rhs_content)) return null;
            return lhs_tag;
        }

        fn alias(_: *Self) Error!bool {
            return false;
        }

        fn beginVariant(_: *Self, comptime _: []const u8) Error!bool {
            return true;
        }

        fn primitive(self: *Self) Error!bool {
            return self.lhs_content.primitive == self.rhs_content.primitive;
        }

        fn namedModule(self: *Self) Error!bool {
            return std.mem.eql(u8, self.lhs_content.named.named_type.module.bytes[0..], self.rhs_content.named.named_type.module.bytes[0..]);
        }

        fn namedDefModule(self: *Self) Error!bool {
            return std.mem.eql(u8, self.name_store.moduleIdentityBytes(self.lhs_content.named.def.module), self.name_store.moduleIdentityBytes(self.rhs_content.named.def.module));
        }

        fn namedSourceDecl(self: *Self) Error!bool {
            return self.lhs_content.named.def.source_decl == self.rhs_content.named.def.source_decl;
        }

        fn namedSourceDeclIsAbsent(self: *Self) bool {
            return self.lhs_content.named.def.source_decl == null;
        }

        fn namedTypeName(self: *Self) Error!bool {
            return std.mem.eql(u8, self.name_store.typeNameText(self.lhs_content.named.def.type_name), self.name_store.typeNameText(self.rhs_content.named.def.type_name));
        }

        fn namedGenerated(self: *Self) Error!bool {
            return optionalDigestEql(self.lhs_content.named.def.generated, self.rhs_content.named.def.generated);
        }

        fn namedIteratorRepresentation(self: *Self) Error!bool {
            return self.lhs_content.named.def.iterator_representation == self.rhs_content.named.def.iterator_representation;
        }

        fn namedIteratorKind(self: *Self) Error!bool {
            return self.lhs_content.named.def.iterator_kind == self.rhs_content.named.def.iterator_kind;
        }

        fn namedIteratorDepth(self: *Self) Error!bool {
            return self.lhs_content.named.def.iterator_depth == self.rhs_content.named.def.iterator_depth;
        }

        fn namedKind(self: *Self) Error!bool {
            return self.lhs_content.named.kind == self.rhs_content.named.kind;
        }

        fn namedBuiltinOwnerField(self: *Self) Error!bool {
            return self.lhs_content.named.builtin_owner == self.rhs_content.named.builtin_owner;
        }

        fn namedBuiltinOwnerValue(self: *Self) ?static_dispatch.BuiltinOwner {
            return self.lhs_content.named.builtin_owner;
        }

        fn namedMode(self: *Self) NamedDigestMode {
            return self.named_mode;
        }

        fn specializationBuiltinBackingMarker(_: *Self) Error!bool {
            return true;
        }

        fn specializationNamedIdentityMarker(_: *Self) Error!bool {
            return true;
        }

        fn lhsTypeSpan(self: *Self, role: IdentityTypeSpan) Span {
            return switch (role) {
                .named_args => self.lhs_content.named.args,
                .tuple_items => self.lhs_content.tuple,
                .func_args => self.lhs_content.func.args,
            };
        }

        fn rhsTypeSpan(self: *Self, role: IdentityTypeSpan) Span {
            return switch (role) {
                .named_args => self.rhs_content.named.args,
                .tuple_items => self.rhs_content.tuple,
                .func_args => self.rhs_content.func.args,
            };
        }

        fn typeSpanLen(self: *Self, role: IdentityTypeSpan) Error!?usize {
            const lhs = self.type_view.span(self.lhsTypeSpan(role));
            const rhs = self.type_view.span(self.rhsTypeSpan(role));
            if (lhs.len != rhs.len) return null;
            return lhs.len;
        }

        fn typeSpanChild(self: *Self, role: IdentityTypeSpan, index: usize) Error!bool {
            const lhs = self.type_view.span(self.lhsTypeSpan(role));
            const rhs = self.type_view.span(self.rhsTypeSpan(role));
            return try typeViewEqlInner(self.type_view, self.name_store, lhs[index], rhs[index], self.named_mode, self.visited);
        }

        fn beginBacking(_: *Self) Error!bool {
            return true;
        }

        fn backingPresence(self: *Self) Error!?IdentityBacking {
            if (self.lhs_content.named.backing == null and self.rhs_content.named.backing == null) return .none;
            if (self.lhs_content.named.backing == null or self.rhs_content.named.backing == null) return null;
            return .some;
        }

        fn backingUse(self: *Self) Error!bool {
            return self.lhs_content.named.backing.?.use == self.rhs_content.named.backing.?.use;
        }

        fn backingType(self: *Self) Error!bool {
            return try typeViewEqlInner(
                self.type_view,
                self.name_store,
                self.lhs_content.named.backing.?.ty,
                self.rhs_content.named.backing.?.ty,
                .full,
                self.visited,
            );
        }

        fn beginDeclaredOrder(_: *Self) Error!bool {
            return true;
        }

        fn declaredOrderLen(self: *Self) Error!?usize {
            const lhs = self.type_view.declaredFieldSpan(self.lhs_content.named.declared_order);
            const rhs = self.type_view.declaredFieldSpan(self.rhs_content.named.declared_order);
            if (lhs.len != rhs.len) return null;
            return lhs.len;
        }

        fn declaredFieldTag(self: *Self, index: usize) Error!?DeclaredFieldTag {
            const lhs = self.type_view.declaredFieldSpan(self.lhs_content.named.declared_order);
            const rhs = self.type_view.declaredFieldSpan(self.rhs_content.named.declared_order);
            const lhs_tag = std.meta.activeTag(lhs[index]);
            if (lhs_tag != std.meta.activeTag(rhs[index])) return null;
            return lhs_tag;
        }

        fn declaredFieldName(self: *Self, index: usize) Error!bool {
            const lhs = self.type_view.declaredFieldSpan(self.lhs_content.named.declared_order);
            const rhs = self.type_view.declaredFieldSpan(self.rhs_content.named.declared_order);
            return std.mem.eql(u8, self.name_store.recordFieldLabelText(lhs[index].named), self.name_store.recordFieldLabelText(rhs[index].named));
        }

        fn declaredPaddingType(self: *Self, index: usize) Error!bool {
            const lhs = self.type_view.declaredFieldSpan(self.lhs_content.named.declared_order);
            const rhs = self.type_view.declaredFieldSpan(self.rhs_content.named.declared_order);
            return try typeViewEqlInner(self.type_view, self.name_store, lhs[index].padding, rhs[index].padding, .full, self.visited);
        }

        fn recordLen(self: *Self) Error!?usize {
            const lhs = self.type_view.fieldSpan(self.lhs_content.record);
            const rhs = self.type_view.fieldSpan(self.rhs_content.record);
            if (lhs.len != rhs.len) return null;
            return lhs.len;
        }

        fn recordFieldName(self: *Self, index: usize) Error!bool {
            const lhs = self.type_view.fieldSpan(self.lhs_content.record);
            const rhs = self.type_view.fieldSpan(self.rhs_content.record);
            return std.mem.eql(u8, self.name_store.recordFieldLabelText(lhs[index].name), self.name_store.recordFieldLabelText(rhs[index].name));
        }

        fn recordFieldType(self: *Self, index: usize) Error!bool {
            const lhs = self.type_view.fieldSpan(self.lhs_content.record);
            const rhs = self.type_view.fieldSpan(self.rhs_content.record);
            return try typeViewEqlInner(self.type_view, self.name_store, lhs[index].ty, rhs[index].ty, self.named_mode, self.visited);
        }

        fn tagUnionLen(self: *Self) Error!?usize {
            const lhs = self.type_view.tagSpan(self.lhs_content.tag_union);
            const rhs = self.type_view.tagSpan(self.rhs_content.tag_union);
            if (lhs.len != rhs.len) return null;
            return lhs.len;
        }

        fn tagName(self: *Self, index: usize) Error!bool {
            const lhs = self.type_view.tagSpan(self.lhs_content.tag_union);
            const rhs = self.type_view.tagSpan(self.rhs_content.tag_union);
            return std.mem.eql(u8, self.name_store.tagLabelText(lhs[index].name), self.name_store.tagLabelText(rhs[index].name));
        }

        fn tagPayloadLen(self: *Self, tag_index: usize) Error!?usize {
            const lhs_tags = self.type_view.tagSpan(self.lhs_content.tag_union);
            const rhs_tags = self.type_view.tagSpan(self.rhs_content.tag_union);
            const lhs = self.type_view.span(lhs_tags[tag_index].payloads);
            const rhs = self.type_view.span(rhs_tags[tag_index].payloads);
            if (lhs.len != rhs.len) return null;
            return lhs.len;
        }

        fn tagPayloadType(self: *Self, tag_index: usize, payload_index: usize) Error!bool {
            const lhs_tags = self.type_view.tagSpan(self.lhs_content.tag_union);
            const rhs_tags = self.type_view.tagSpan(self.rhs_content.tag_union);
            const lhs = self.type_view.span(lhs_tags[tag_index].payloads);
            const rhs = self.type_view.span(rhs_tags[tag_index].payloads);
            return try typeViewEqlInner(self.type_view, self.name_store, lhs[payload_index], rhs[payload_index], self.named_mode, self.visited);
        }

        fn listElem(self: *Self) Error!bool {
            return try typeViewEqlInner(self.type_view, self.name_store, self.lhs_content.list, self.rhs_content.list, self.named_mode, self.visited);
        }

        fn boxElem(self: *Self) Error!bool {
            return try typeViewEqlInner(self.type_view, self.name_store, self.lhs_content.box, self.rhs_content.box, self.named_mode, self.visited);
        }

        fn funcRet(self: *Self) Error!bool {
            return try typeViewEqlInner(self.type_view, self.name_store, self.lhs_content.func.ret, self.rhs_content.func.ret, self.named_mode, self.visited);
        }

        fn erased(self: *Self) Error!bool {
            return std.mem.eql(u8, self.lhs_content.erased.bytes[0..], self.rhs_content.erased.bytes[0..]);
        }
    };
}

fn typeViewEql(
    type_view: anytype,
    allocator: std.mem.Allocator,
    name_store: *const names.NameStore,
    lhs: TypeId,
    rhs: TypeId,
) std.mem.Allocator.Error!bool {
    var visited = std.AutoHashMap(u128, void).init(allocator);
    defer visited.deinit();
    return try typeViewEqlInner(type_view, name_store, lhs, rhs, .identity_only, &visited);
}

fn typeViewEqlInner(
    type_view: anytype,
    name_store: *const names.NameStore,
    raw_lhs: TypeId,
    raw_rhs: TypeId,
    named_mode: NamedDigestMode,
    visited: *std.AutoHashMap(u128, void),
) std.mem.Allocator.Error!bool {
    if (raw_lhs == raw_rhs) return true;

    // A storage-transparent alias — backed, with no builtin owner — unwraps to
    // its backing. A builtin-owned alias is nontransparent: its checked dispatch
    // owner is part of its interning identity, so it falls through to the named
    // identity comparison, which compares the owner.
    const lhs_content = type_view.get(raw_lhs);
    if (lhs_content == .named and lhs_content.named.kind == .alias and lhs_content.named.builtin_owner == null) {
        if (lhs_content.named.backing) |backing| {
            return try typeViewEqlInner(type_view, name_store, backing.ty, raw_rhs, named_mode, visited);
        }
    }

    const rhs_content = type_view.get(raw_rhs);
    if (rhs_content == .named and rhs_content.named.kind == .alias and rhs_content.named.builtin_owner == null) {
        if (rhs_content.named.backing) |backing| {
            return try typeViewEqlInner(type_view, name_store, raw_lhs, backing.ty, named_mode, visited);
        }
    }

    const pair = typePairKey(raw_lhs, raw_rhs, named_mode);
    const gop = try visited.getOrPut(pair);
    if (gop.found_existing) return true;

    var driver = TypeViewIdentityEqlDriver(@TypeOf(type_view)){
        .type_view = type_view,
        .name_store = name_store,
        .lhs_content = lhs_content,
        .rhs_content = rhs_content,
        .named_mode = named_mode,
        .visited = visited,
    };
    return try visitTypeIdentity(@TypeOf(driver), &driver);
}

fn typePairKey(lhs: TypeId, rhs: TypeId, named_mode: NamedDigestMode) u128 {
    const lhs_int = @intFromEnum(lhs);
    const rhs_int = @intFromEnum(rhs);
    const low = @min(lhs_int, rhs_int);
    const high = @max(lhs_int, rhs_int);
    return (@as(u128, @intFromEnum(named_mode)) << 64) | (@as(u128, low) << 32) | @as(u128, high);
}

/// Read-only type-store view backed by durable cache sections.
pub const DurableView = struct {
    types: []const Content,
    type_digests: []const names.TypeDigest,
    spans: []const TypeId,
    fields: []const Field,
    tags: []const Tag,
    declared_fields: []const DeclaredField,

    pub fn get(self: DurableView, ty: TypeId) Content {
        return self.types[@intFromEnum(ty)];
    }

    pub fn span(self: DurableView, span_: Span) []const TypeId {
        return self.spans[span_.start..][0..span_.len];
    }

    pub fn fieldSpan(self: DurableView, span_: Span) []const Field {
        return self.fields[span_.start..][0..span_.len];
    }

    pub fn tagSpan(self: DurableView, span_: Span) []const Tag {
        return self.tags[span_.start..][0..span_.len];
    }

    pub fn declaredFieldSpan(self: DurableView, span_: Span) []const DeclaredField {
        return self.declared_fields[span_.start..][0..span_.len];
    }

    pub fn verify(self: DurableView, name_store: *const names.NameStore) ?Store.VerifyError {
        if (self.type_digests.len != self.types.len) return .type_digest_count_mismatch;

        for (self.spans) |ty| {
            if (!self.typeRefInBounds(ty)) return .type_ref_out_of_bounds;
        }
        for (self.fields) |field| {
            if (!self.typeRefInBounds(field.ty)) return .type_ref_out_of_bounds;
        }
        for (self.tags) |tag| {
            if (!self.spanInBounds(self.spans.len, tag.payloads)) return .type_span_out_of_bounds;
            if (self.verifyTypeSpan(tag.payloads)) |err| return err;
        }
        for (self.declared_fields) |field| {
            switch (field) {
                .named => {},
                .padding => |ty| if (!self.typeRefInBounds(ty)) return .type_ref_out_of_bounds,
            }
        }

        for (self.types) |content| {
            switch (content) {
                .primitive, .erased, .zst => {},
                .list, .box => |ty| if (!self.typeRefInBounds(ty)) return .type_ref_out_of_bounds,
                .tuple => |span_| if (self.verifyTypeSpan(span_)) |err| return err,
                .record => |span_| if (self.verifyFieldSpan(name_store, span_)) |err| return err,
                .tag_union => |span_| if (self.verifyTagSpan(name_store, span_)) |err| return err,
                .func => |func| {
                    if (self.verifyTypeSpan(func.args)) |err| return err;
                    if (!self.typeRefInBounds(func.ret)) return .type_ref_out_of_bounds;
                },
                .named => |named| {
                    if (self.verifyTypeSpan(named.args)) |err| return err;
                    if (named.backing) |backing| {
                        if (!self.typeRefInBounds(backing.ty)) return .type_ref_out_of_bounds;
                    }
                    if (self.verifyDeclaredFieldSpan(named.declared_order)) |err| return err;
                },
            }
        }

        return null;
    }

    fn typeRefInBounds(self: DurableView, ty: TypeId) bool {
        return @intFromEnum(ty) < self.types.len;
    }

    fn spanInBounds(_: DurableView, len: usize, span_: Span) bool {
        const start: usize = span_.start;
        const span_len: usize = span_.len;
        return start <= len and span_len <= len - start;
    }

    fn verifyTypeSpan(self: DurableView, span_: Span) ?Store.VerifyError {
        if (!self.spanInBounds(self.spans.len, span_)) return .type_span_out_of_bounds;
        for (self.span(span_)) |ty| {
            if (!self.typeRefInBounds(ty)) return .type_ref_out_of_bounds;
        }
        return null;
    }

    fn verifyFieldSpan(self: DurableView, name_store: *const names.NameStore, span_: Span) ?Store.VerifyError {
        if (!self.spanInBounds(self.fields.len, span_)) return .field_span_out_of_bounds;
        const fields_ = self.fieldSpan(span_);
        for (fields_) |field| {
            if (!self.typeRefInBounds(field.ty)) return .type_ref_out_of_bounds;
        }
        if (fields_.len > 1) {
            for (fields_[1..], 1..) |field, index| {
                if (!name_store.recordFieldLabelTextLessThan(fields_[index - 1].name, field.name)) {
                    return .record_fields_not_sorted;
                }
            }
        }
        return null;
    }

    fn verifyTagSpan(self: DurableView, name_store: *const names.NameStore, span_: Span) ?Store.VerifyError {
        if (!self.spanInBounds(self.tags.len, span_)) return .tag_span_out_of_bounds;
        const tags_ = self.tagSpan(span_);
        for (tags_) |tag| {
            if (self.verifyTypeSpan(tag.payloads)) |err| return err;
        }
        if (tags_.len > 1) {
            for (tags_[1..], 1..) |tag, index| {
                if (!name_store.tagLabelTextLessThan(tags_[index - 1].name, tag.name)) {
                    return .tag_union_tags_not_sorted;
                }
            }
        }
        return null;
    }

    fn verifyDeclaredFieldSpan(self: DurableView, span_: Span) ?Store.VerifyError {
        if (!self.spanInBounds(self.declared_fields.len, span_)) return .declared_field_span_out_of_bounds;
        for (self.declaredFieldSpan(span_)) |field| {
            switch (field) {
                .named => {},
                .padding => |ty| if (!self.typeRefInBounds(ty)) return .type_ref_out_of_bounds,
            }
        }
        return null;
    }
};

fn AcrossStoresIdentityEqlDriver(comptime LhsView: type, comptime RhsView: type) type {
    return struct {
        const Self = @This();
        const Error = std.mem.Allocator.Error;

        name_store: *const names.NameStore,
        lhs_view: LhsView,
        lhs_content: Content,
        rhs_view: RhsView,
        rhs_content: Content,
        named_mode: NamedDigestMode,
        visited: *std.AutoHashMap(u128, void),

        fn contentTag(self: *Self) ?ContentTag {
            const lhs_tag = std.meta.activeTag(self.lhs_content);
            if (lhs_tag != std.meta.activeTag(self.rhs_content)) return null;
            return lhs_tag;
        }

        fn alias(_: *Self) Error!bool {
            return false;
        }

        fn beginVariant(_: *Self, comptime _: []const u8) Error!bool {
            return true;
        }

        fn primitive(self: *Self) Error!bool {
            return self.lhs_content.primitive == self.rhs_content.primitive;
        }

        fn namedModule(self: *Self) Error!bool {
            return std.mem.eql(u8, self.lhs_content.named.named_type.module.bytes[0..], self.rhs_content.named.named_type.module.bytes[0..]);
        }

        fn namedDefModule(self: *Self) Error!bool {
            return std.mem.eql(u8, self.name_store.moduleIdentityBytes(self.lhs_content.named.def.module), self.name_store.moduleIdentityBytes(self.rhs_content.named.def.module));
        }

        fn namedSourceDecl(self: *Self) Error!bool {
            return self.lhs_content.named.def.source_decl == self.rhs_content.named.def.source_decl;
        }

        fn namedSourceDeclIsAbsent(self: *Self) bool {
            return self.lhs_content.named.def.source_decl == null;
        }

        fn namedTypeName(self: *Self) Error!bool {
            return std.mem.eql(u8, self.name_store.typeNameText(self.lhs_content.named.def.type_name), self.name_store.typeNameText(self.rhs_content.named.def.type_name));
        }

        fn namedGenerated(self: *Self) Error!bool {
            return optionalDigestEql(self.lhs_content.named.def.generated, self.rhs_content.named.def.generated);
        }

        fn namedIteratorRepresentation(self: *Self) Error!bool {
            return self.lhs_content.named.def.iterator_representation == self.rhs_content.named.def.iterator_representation;
        }

        fn namedIteratorKind(self: *Self) Error!bool {
            return self.lhs_content.named.def.iterator_kind == self.rhs_content.named.def.iterator_kind;
        }

        fn namedIteratorDepth(self: *Self) Error!bool {
            return self.lhs_content.named.def.iterator_depth == self.rhs_content.named.def.iterator_depth;
        }

        fn namedKind(self: *Self) Error!bool {
            return self.lhs_content.named.kind == self.rhs_content.named.kind;
        }

        fn namedBuiltinOwnerField(self: *Self) Error!bool {
            return self.lhs_content.named.builtin_owner == self.rhs_content.named.builtin_owner;
        }

        fn namedBuiltinOwnerValue(self: *Self) ?static_dispatch.BuiltinOwner {
            return self.lhs_content.named.builtin_owner;
        }

        fn namedMode(self: *Self) NamedDigestMode {
            return self.named_mode;
        }

        fn specializationBuiltinBackingMarker(_: *Self) Error!bool {
            return true;
        }

        fn specializationNamedIdentityMarker(_: *Self) Error!bool {
            return true;
        }

        fn lhsTypeSpan(self: *Self, role: IdentityTypeSpan) Span {
            return switch (role) {
                .named_args => self.lhs_content.named.args,
                .tuple_items => self.lhs_content.tuple,
                .func_args => self.lhs_content.func.args,
            };
        }

        fn rhsTypeSpan(self: *Self, role: IdentityTypeSpan) Span {
            return switch (role) {
                .named_args => self.rhs_content.named.args,
                .tuple_items => self.rhs_content.tuple,
                .func_args => self.rhs_content.func.args,
            };
        }

        fn typeSpanLen(self: *Self, role: IdentityTypeSpan) Error!?usize {
            const lhs = self.lhs_view.span(self.lhsTypeSpan(role));
            const rhs = self.rhs_view.span(self.rhsTypeSpan(role));
            if (lhs.len != rhs.len) return null;
            return lhs.len;
        }

        fn typeSpanChild(self: *Self, role: IdentityTypeSpan, index: usize) Error!bool {
            const lhs = self.lhs_view.span(self.lhsTypeSpan(role));
            const rhs = self.rhs_view.span(self.rhsTypeSpan(role));
            return try typeEqlAcrossStoresInner(self.name_store, self.lhs_view, lhs[index], self.rhs_view, rhs[index], self.named_mode, self.visited);
        }

        fn beginBacking(_: *Self) Error!bool {
            return true;
        }

        fn backingPresence(self: *Self) Error!?IdentityBacking {
            if (self.lhs_content.named.backing == null and self.rhs_content.named.backing == null) return .none;
            if (self.lhs_content.named.backing == null or self.rhs_content.named.backing == null) return null;
            return .some;
        }

        fn backingUse(self: *Self) Error!bool {
            return self.lhs_content.named.backing.?.use == self.rhs_content.named.backing.?.use;
        }

        fn backingType(self: *Self) Error!bool {
            return try typeEqlAcrossStoresInner(
                self.name_store,
                self.lhs_view,
                self.lhs_content.named.backing.?.ty,
                self.rhs_view,
                self.rhs_content.named.backing.?.ty,
                .full,
                self.visited,
            );
        }

        fn beginDeclaredOrder(_: *Self) Error!bool {
            return true;
        }

        fn declaredOrderLen(self: *Self) Error!?usize {
            const lhs = self.lhs_view.declaredFieldSpan(self.lhs_content.named.declared_order);
            const rhs = self.rhs_view.declaredFieldSpan(self.rhs_content.named.declared_order);
            if (lhs.len != rhs.len) return null;
            return lhs.len;
        }

        fn declaredFieldTag(self: *Self, index: usize) Error!?DeclaredFieldTag {
            const lhs = self.lhs_view.declaredFieldSpan(self.lhs_content.named.declared_order);
            const rhs = self.rhs_view.declaredFieldSpan(self.rhs_content.named.declared_order);
            const lhs_tag = std.meta.activeTag(lhs[index]);
            if (lhs_tag != std.meta.activeTag(rhs[index])) return null;
            return lhs_tag;
        }

        fn declaredFieldName(self: *Self, index: usize) Error!bool {
            const lhs = self.lhs_view.declaredFieldSpan(self.lhs_content.named.declared_order);
            const rhs = self.rhs_view.declaredFieldSpan(self.rhs_content.named.declared_order);
            return std.mem.eql(u8, self.name_store.recordFieldLabelText(lhs[index].named), self.name_store.recordFieldLabelText(rhs[index].named));
        }

        fn declaredPaddingType(self: *Self, index: usize) Error!bool {
            const lhs = self.lhs_view.declaredFieldSpan(self.lhs_content.named.declared_order);
            const rhs = self.rhs_view.declaredFieldSpan(self.rhs_content.named.declared_order);
            return try typeEqlAcrossStoresInner(self.name_store, self.lhs_view, lhs[index].padding, self.rhs_view, rhs[index].padding, .full, self.visited);
        }

        fn recordLen(self: *Self) Error!?usize {
            const lhs = self.lhs_view.fieldSpan(self.lhs_content.record);
            const rhs = self.rhs_view.fieldSpan(self.rhs_content.record);
            if (lhs.len != rhs.len) return null;
            return lhs.len;
        }

        fn recordFieldName(self: *Self, index: usize) Error!bool {
            const lhs = self.lhs_view.fieldSpan(self.lhs_content.record);
            const rhs = self.rhs_view.fieldSpan(self.rhs_content.record);
            return std.mem.eql(u8, self.name_store.recordFieldLabelText(lhs[index].name), self.name_store.recordFieldLabelText(rhs[index].name));
        }

        fn recordFieldType(self: *Self, index: usize) Error!bool {
            const lhs = self.lhs_view.fieldSpan(self.lhs_content.record);
            const rhs = self.rhs_view.fieldSpan(self.rhs_content.record);
            return try typeEqlAcrossStoresInner(self.name_store, self.lhs_view, lhs[index].ty, self.rhs_view, rhs[index].ty, self.named_mode, self.visited);
        }

        fn tagUnionLen(self: *Self) Error!?usize {
            const lhs = self.lhs_view.tagSpan(self.lhs_content.tag_union);
            const rhs = self.rhs_view.tagSpan(self.rhs_content.tag_union);
            if (lhs.len != rhs.len) return null;
            return lhs.len;
        }

        fn tagName(self: *Self, index: usize) Error!bool {
            const lhs = self.lhs_view.tagSpan(self.lhs_content.tag_union);
            const rhs = self.rhs_view.tagSpan(self.rhs_content.tag_union);
            return std.mem.eql(u8, self.name_store.tagLabelText(lhs[index].name), self.name_store.tagLabelText(rhs[index].name));
        }

        fn tagPayloadLen(self: *Self, tag_index: usize) Error!?usize {
            const lhs_tags = self.lhs_view.tagSpan(self.lhs_content.tag_union);
            const rhs_tags = self.rhs_view.tagSpan(self.rhs_content.tag_union);
            const lhs = self.lhs_view.span(lhs_tags[tag_index].payloads);
            const rhs = self.rhs_view.span(rhs_tags[tag_index].payloads);
            if (lhs.len != rhs.len) return null;
            return lhs.len;
        }

        fn tagPayloadType(self: *Self, tag_index: usize, payload_index: usize) Error!bool {
            const lhs_tags = self.lhs_view.tagSpan(self.lhs_content.tag_union);
            const rhs_tags = self.rhs_view.tagSpan(self.rhs_content.tag_union);
            const lhs = self.lhs_view.span(lhs_tags[tag_index].payloads);
            const rhs = self.rhs_view.span(rhs_tags[tag_index].payloads);
            return try typeEqlAcrossStoresInner(self.name_store, self.lhs_view, lhs[payload_index], self.rhs_view, rhs[payload_index], self.named_mode, self.visited);
        }

        fn listElem(self: *Self) Error!bool {
            return try typeEqlAcrossStoresInner(self.name_store, self.lhs_view, self.lhs_content.list, self.rhs_view, self.rhs_content.list, self.named_mode, self.visited);
        }

        fn boxElem(self: *Self) Error!bool {
            return try typeEqlAcrossStoresInner(self.name_store, self.lhs_view, self.lhs_content.box, self.rhs_view, self.rhs_content.box, self.named_mode, self.visited);
        }

        fn funcRet(self: *Self) Error!bool {
            return try typeEqlAcrossStoresInner(self.name_store, self.lhs_view, self.lhs_content.func.ret, self.rhs_view, self.rhs_content.func.ret, self.named_mode, self.visited);
        }

        fn erased(self: *Self) Error!bool {
            return std.mem.eql(u8, self.lhs_content.erased.bytes[0..], self.rhs_content.erased.bytes[0..]);
        }
    };
}

/// Exact structural equality for closed Monotype types that live in two
/// different type stores. Type ids are interpreted only against the view they
/// came from; equality follows the same identity rules as `Store.typeEql`.
pub fn typeEqlAcrossStores(
    allocator: std.mem.Allocator,
    name_store: *const names.NameStore,
    lhs_view: anytype,
    lhs: TypeId,
    rhs_view: anytype,
    rhs: TypeId,
) std.mem.Allocator.Error!bool {
    var visited = std.AutoHashMap(u128, void).init(allocator);
    defer visited.deinit();
    return try typeEqlAcrossStoresInner(name_store, lhs_view, lhs, rhs_view, rhs, .identity_only, &visited);
}

fn typeEqlAcrossStoresInner(
    name_store: *const names.NameStore,
    lhs_view: anytype,
    raw_lhs: TypeId,
    rhs_view: anytype,
    raw_rhs: TypeId,
    named_mode: NamedDigestMode,
    visited: *std.AutoHashMap(u128, void),
) std.mem.Allocator.Error!bool {
    // Match the storage/digest alias split: a storage-transparent alias (backed,
    // no builtin owner) unwraps to its backing; a builtin-owned alias is
    // nontransparent and compares through the named identity walk.
    const lhs_content = lhs_view.get(raw_lhs);
    if (lhs_content == .named and lhs_content.named.kind == .alias and lhs_content.named.builtin_owner == null) {
        if (lhs_content.named.backing) |backing| {
            return try typeEqlAcrossStoresInner(name_store, lhs_view, backing.ty, rhs_view, raw_rhs, named_mode, visited);
        }
    }

    const rhs_content = rhs_view.get(raw_rhs);
    if (rhs_content == .named and rhs_content.named.kind == .alias and rhs_content.named.builtin_owner == null) {
        if (rhs_content.named.backing) |backing| {
            return try typeEqlAcrossStoresInner(name_store, lhs_view, raw_lhs, rhs_view, backing.ty, named_mode, visited);
        }
    }

    const pair = directionalTypePair(raw_lhs, raw_rhs, named_mode);
    const gop = try visited.getOrPut(pair);
    if (gop.found_existing) return true;

    var driver = AcrossStoresIdentityEqlDriver(@TypeOf(lhs_view), @TypeOf(rhs_view)){
        .name_store = name_store,
        .lhs_view = lhs_view,
        .lhs_content = lhs_content,
        .rhs_view = rhs_view,
        .rhs_content = rhs_content,
        .named_mode = named_mode,
        .visited = visited,
    };
    return try visitTypeIdentity(@TypeOf(driver), &driver);
}

fn directionalTypePair(lhs: TypeId, rhs: TypeId, named_mode: NamedDigestMode) u128 {
    return (@as(u128, @intFromEnum(named_mode)) << 64) | (@as(u128, @intFromEnum(lhs)) << 32) | @as(u128, @intFromEnum(rhs));
}

/// Interning state: a store with content dedup enabled plus the name store its
/// digests and row ordering consult.
const InternerState = struct {
    allocator: std.mem.Allocator,
    name_store: *const names.NameStore,
    store: Store,
};

/// Opaque builder handle for interning immutable Monotype type ids.
///
/// This is a thin delegate over a `Store` with interning enabled: every method
/// forwards to the matching `Store.intern*` constructor, threading the name
/// store. The hash-consing logic and the recursive-group builder live on
/// `Store` so production stores (Stage C) can dedup through the same boundary
/// without an interner handle. The `Recursive*` input types are re-exported
/// here for callers that still address them through `Interner`.
pub const Interner = opaque {
    pub const TagInput = Store.TagInput;
    pub const NamedInput = Store.NamedInput;
    pub const RecursiveLink = Store.RecursiveLink;
    pub const RecursiveNodeId = Store.RecursiveNodeId;
    pub const RecursiveField = Store.RecursiveField;
    pub const RecursiveTag = Store.RecursiveTag;
    pub const RecursiveNamedBacking = Store.RecursiveNamedBacking;
    pub const RecursiveNamed = Store.RecursiveNamed;
    pub const RecursiveContent = Store.RecursiveContent;

    pub fn recursiveNodeId(index: usize) RecursiveNodeId {
        return Store.recursiveNodeId(index);
    }

    fn state(self: *Interner) *InternerState {
        return @ptrCast(@alignCast(self));
    }

    fn constState(self: *const Interner) *const InternerState {
        return @ptrCast(@alignCast(self));
    }

    fn store(self: *Interner) *Store {
        return &self.state().store;
    }

    fn constStore(self: *const Interner) *const Store {
        return &self.constState().store;
    }

    fn nameStore(self: *Interner) *const names.NameStore {
        return self.state().name_store;
    }

    pub fn init(allocator: std.mem.Allocator, name_store: *const names.NameStore) std.mem.Allocator.Error!*Interner {
        const state_ = try allocator.create(InternerState);
        state_.* = .{
            .allocator = allocator,
            .name_store = name_store,
            .store = Store.init(allocator),
        };
        state_.store.enableInterning();
        return @ptrCast(state_);
    }

    pub fn deinit(self: *Interner) void {
        const state_ = self.state();
        state_.store.deinit();
        const allocator = state_.allocator;
        allocator.destroy(state_);
    }

    pub fn view(self: *const Interner) Store.View {
        return self.constStore().view();
    }

    pub fn get(self: *const Interner, ty: TypeId) Content {
        return self.constStore().get(ty);
    }

    pub fn span(self: *const Interner, span_: Span) StoreSpanBorrow(TypeId, "spans") {
        return self.constStore().span(span_);
    }

    pub fn fieldSpan(self: *const Interner, span_: Span) StoreSpanBorrow(Field, "fields") {
        return self.constStore().fieldSpan(span_);
    }

    pub fn tagSpan(self: *const Interner, span_: Span) StoreSpanBorrow(Tag, "tags") {
        return self.constStore().tagSpan(span_);
    }

    pub fn typeDigest(self: *Interner, ty: TypeId) names.TypeDigest {
        const state_ = self.state();
        return state_.store.typeDigestCached(state_.name_store, ty, null);
    }

    pub fn typeEql(self: *const Interner, lhs: TypeId, rhs: TypeId) std.mem.Allocator.Error!bool {
        const state_ = self.constState();
        return try state_.store.typeEql(state_.name_store, lhs, rhs);
    }

    pub fn verify(self: *const Interner) ?Store.VerifyError {
        const state_ = self.constState();
        return state_.store.verify(state_.name_store);
    }

    pub fn internPrimitive(self: *Interner, primitive: Primitive) std.mem.Allocator.Error!TypeId {
        return try self.store().internPrimitive(self.nameStore(), primitive);
    }

    pub fn internZst(self: *Interner) std.mem.Allocator.Error!TypeId {
        return try self.store().internZst(self.nameStore());
    }

    pub fn internList(self: *Interner, elem: TypeId) std.mem.Allocator.Error!TypeId {
        return try self.store().internList(self.nameStore(), elem);
    }

    pub fn internBox(self: *Interner, elem: TypeId) std.mem.Allocator.Error!TypeId {
        return try self.store().internBox(self.nameStore(), elem);
    }

    pub fn internTuple(self: *Interner, items: []const TypeId) std.mem.Allocator.Error!TypeId {
        return try self.store().internTuple(self.nameStore(), items);
    }

    pub fn internFunc(self: *Interner, args: []const TypeId, ret: TypeId) std.mem.Allocator.Error!TypeId {
        return try self.store().internFunc(self.nameStore(), args, ret);
    }

    pub fn internRecord(self: *Interner, raw_fields: []const Field) std.mem.Allocator.Error!TypeId {
        return try self.store().internRecord(self.nameStore(), raw_fields);
    }

    pub fn internTagUnion(self: *Interner, raw_tags: []const TagInput) std.mem.Allocator.Error!TypeId {
        return try self.store().internTagUnion(self.nameStore(), raw_tags);
    }

    pub fn internNamed(self: *Interner, named: NamedInput) std.mem.Allocator.Error!TypeId {
        return try self.store().internNamed(self.nameStore(), named);
    }

    pub fn internErased(self: *Interner, digest: names.TypeDigest) std.mem.Allocator.Error!TypeId {
        return try self.store().internErased(self.nameStore(), digest);
    }

    pub fn internRecursiveRoot(self: *Interner, content: RecursiveContent) std.mem.Allocator.Error!TypeId {
        return try self.store().internRecursiveRoot(self.nameStore(), content);
    }

    pub fn internRecursiveGroupRoot(
        self: *Interner,
        contents: []const RecursiveContent,
        root_node: RecursiveNodeId,
    ) std.mem.Allocator.Error!TypeId {
        return try self.store().internRecursiveGroupRoot(self.nameStore(), contents, root_node);
    }
};

/// Re-intern the type rooted at `root` in `source` into the interning `dest`
/// store, producing the equivalent (deduplicated) type. `source` and `dest`
/// share `name_store`, so row/tag/name ids carry over directly.
///
/// The reachable subgraph is split into strongly connected components (Tarjan,
/// explicit-stack). Components are emitted children-first: each acyclic node is
/// built through the plain `intern*` constructors with its children already
/// resolved, and each cyclic component is built through the recursive-group
/// builder, with in-component edges as `RecursiveLink.node` and edges leaving
/// the component as `RecursiveLink.interned`. A component with more than one
/// externally-referenced member is resolved by rooting the recursive-group
/// build at each member in turn; dedup makes the repeated builds share ids, so
/// `dest` must have interning enabled.
pub fn reintern(
    dest: *Store,
    name_store: *const names.NameStore,
    source: Store.View,
    root: TypeId,
) std.mem.Allocator.Error!TypeId {
    if (!dest.internEnabled()) {
        Common.invariant("reintern target store must have interning enabled");
    }
    var arena_impl = std.heap.ArenaAllocator.init(dest.allocator);
    defer arena_impl.deinit();
    var reinterner = Reintern{
        .arena = arena_impl.allocator(),
        .dest = dest,
        .name_store = name_store,
        .source = source,
        .index_of = std.AutoHashMap(TypeId, u32).init(arena_impl.allocator()),
    };
    const root_index = try reinterner.collect(root);
    try reinterner.run();
    return reinterner.resolved.items[root_index] orelse
        Common.invariant("reintern left the root component unresolved");
}

/// Working state for `reintern`. Local indices densely number the reachable
/// source nodes; every buffer below is indexed by local index.
const Reintern = struct {
    arena: std.mem.Allocator,
    dest: *Store,
    name_store: *const names.NameStore,
    source: Store.View,
    index_of: std.AutoHashMap(TypeId, u32),
    nodes: std.ArrayList(TypeId) = .empty,
    children: std.ArrayList([]const u32) = .empty,
    resolved: std.ArrayList(?TypeId) = .empty,

    /// Discover every reachable source node, assign it a dense local index, and
    /// record its child edges. Returns the root's local index.
    fn collect(self: *Reintern, root: TypeId) std.mem.Allocator.Error!u32 {
        // Explicit-stack post-order: reserve a node's index on first sight, then
        // record its child edges once children are known.
        const Frame = struct { ty: TypeId, index: u32 };
        var stack = std.ArrayList(Frame).empty;
        defer stack.deinit(self.arena);

        const root_index = try self.reserveNode(root);
        try self.index_of.put(root, root_index);
        try stack.append(self.arena, .{ .ty = root, .index = root_index });
        while (stack.items.len != 0) {
            const frame = stack.pop().?;
            var child_list = std.ArrayList(u32).empty;
            var it = ChildIterator.init(self.source, self.source.get(frame.ty));
            while (it.next()) |child| {
                const gop = try self.index_of.getOrPut(child);
                if (!gop.found_existing) {
                    // reserveNode never touches `index_of`, so `gop.value_ptr`
                    // stays valid while the reservation appends to the buffers.
                    const child_index = try self.reserveNode(child);
                    gop.value_ptr.* = child_index;
                    try stack.append(self.arena, .{ .ty = child, .index = child_index });
                }
                try child_list.append(self.arena, gop.value_ptr.*);
            }
            self.children.items[frame.index] = try child_list.toOwnedSlice(self.arena);
        }
        return root_index;
    }

    fn reserveNode(self: *Reintern, ty: TypeId) std.mem.Allocator.Error!u32 {
        const index: u32 = @intCast(self.nodes.items.len);
        try self.nodes.append(self.arena, ty);
        try self.children.append(self.arena, &.{});
        try self.resolved.append(self.arena, null);
        return index;
    }

    /// Split the reachable graph into strongly connected components (Tarjan with
    /// an explicit work stack) and build each into `dest` children-first.
    fn run(self: *Reintern) std.mem.Allocator.Error!void {
        const n = self.nodes.items.len;
        const disc = try self.arena.alloc(i64, n);
        const low = try self.arena.alloc(i64, n);
        const on_stack = try self.arena.alloc(bool, n);
        @memset(disc, -1);
        @memset(low, 0);
        @memset(on_stack, false);

        var scc_stack = std.ArrayList(u32).empty;
        defer scc_stack.deinit(self.arena);
        const Frame = struct { node: u32, child_pos: usize };
        var work = std.ArrayList(Frame).empty;
        defer work.deinit(self.arena);

        var counter: i64 = 0;
        for (0..n) |start| {
            if (disc[start] != -1) continue;
            try work.append(self.arena, .{ .node = @intCast(start), .child_pos = 0 });
            while (work.items.len != 0) {
                const frame = &work.items[work.items.len - 1];
                const v = frame.node;
                if (frame.child_pos == 0) {
                    disc[v] = counter;
                    low[v] = counter;
                    counter += 1;
                    try scc_stack.append(self.arena, v);
                    on_stack[v] = true;
                }
                var recursed = false;
                const kids = self.children.items[v];
                while (frame.child_pos < kids.len) {
                    const w = kids[frame.child_pos];
                    frame.child_pos += 1;
                    if (disc[w] == -1) {
                        try work.append(self.arena, .{ .node = w, .child_pos = 0 });
                        recursed = true;
                        break;
                    } else if (on_stack[w]) {
                        low[v] = @min(low[v], disc[w]);
                    }
                }
                if (recursed) continue;

                if (low[v] == disc[v]) {
                    var component = std.ArrayList(u32).empty;
                    defer component.deinit(self.arena);
                    while (true) {
                        const member = scc_stack.pop().?;
                        on_stack[member] = false;
                        try component.append(self.arena, member);
                        if (member == v) break;
                    }
                    try self.buildComponent(component.items);
                }

                _ = work.pop();
                if (work.items.len != 0) {
                    const parent = work.items[work.items.len - 1].node;
                    low[parent] = @min(low[parent], low[v]);
                }
            }
        }
    }

    fn hasSelfEdge(self: *Reintern, node: u32) bool {
        for (self.children.items[node]) |child| {
            if (child == node) return true;
        }
        return false;
    }

    fn buildComponent(self: *Reintern, component: []const u32) std.mem.Allocator.Error!void {
        if (component.len == 1 and !self.hasSelfEdge(component[0])) {
            const node = component[0];
            self.resolved.items[node] = try self.buildAcyclic(node);
            return;
        }
        try self.buildCyclic(component);
    }

    /// Build one acyclic node through the plain constructors; every child is in
    /// an earlier component and therefore already resolved.
    fn buildAcyclic(self: *Reintern, node: u32) std.mem.Allocator.Error!TypeId {
        const ty = self.nodes.items[node];
        return switch (self.source.get(ty)) {
            .primitive => |primitive| try self.dest.internPrimitive(self.name_store, primitive),
            .zst => try self.dest.internZst(self.name_store),
            .erased => |digest| try self.dest.internErased(self.name_store, digest),
            .list => |elem| try self.dest.internList(self.name_store, self.resolvedChild(elem)),
            .box => |elem| try self.dest.internBox(self.name_store, self.resolvedChild(elem)),
            .tuple => |span_| blk: {
                const items = try self.resolvedSpan(span_);
                break :blk try self.dest.internTuple(self.name_store, items);
            },
            .func => |func| blk: {
                const args = try self.resolvedSpan(func.args);
                break :blk try self.dest.internFunc(self.name_store, args, self.resolvedChild(func.ret));
            },
            .record => |span_| blk: {
                const source_fields = self.source.fieldSpan(span_);
                const fields = try self.arena.alloc(Field, source_fields.len);
                for (source_fields, 0..) |field, index| {
                    fields[index] = .{ .name = field.name, .ty = self.resolvedChild(field.ty) };
                }
                break :blk try self.dest.internRecord(self.name_store, fields);
            },
            .tag_union => |span_| blk: {
                const source_tags = self.source.tagSpan(span_);
                const tags = try self.arena.alloc(Store.TagInput, source_tags.len);
                for (source_tags, 0..) |tag, index| {
                    tags[index] = .{
                        .name = tag.name,
                        .checked_name = tag.checked_name,
                        .payloads = try self.resolvedSpan(tag.payloads),
                    };
                }
                break :blk try self.dest.internTagUnion(self.name_store, tags);
            },
            .named => |named| try self.dest.internNamed(self.name_store, .{
                .named_type = named.named_type,
                .def = named.def,
                .kind = named.kind,
                .builtin_owner = named.builtin_owner,
                .args = try self.resolvedSpan(named.args),
                .backing = if (named.backing) |backing| .{
                    .ty = self.resolvedChild(backing.ty),
                    .use = backing.use,
                } else null,
                .declared_order = try self.resolvedDeclaredOrderSlice(named.declared_order),
            }),
        };
    }

    /// Build a cyclic component through the recursive-group builder in one pass,
    /// reading every member's resolved id so each externally-referenced member
    /// is placed without re-entering the group per member.
    fn buildCyclic(self: *Reintern, component: []const u32) std.mem.Allocator.Error!void {
        var group_index = std.AutoHashMap(u32, u32).init(self.arena);
        defer group_index.deinit();
        for (component, 0..) |node, position| {
            try group_index.put(node, @intCast(position));
        }

        const contents = try self.arena.alloc(Store.RecursiveContent, component.len);
        for (component, 0..) |node, position| {
            contents[position] = try self.recursiveContent(node, &group_index);
        }

        const member_ids = try self.arena.alloc(TypeId, component.len);
        try self.dest.internRecursiveGroupInto(self.name_store, contents, member_ids);
        for (component, 0..) |node, position| {
            self.resolved.items[node] = member_ids[position];
        }
    }

    fn recursiveContent(
        self: *Reintern,
        node: u32,
        group_index: *std.AutoHashMap(u32, u32),
    ) std.mem.Allocator.Error!Store.RecursiveContent {
        const ty = self.nodes.items[node];
        return switch (self.source.get(ty)) {
            .primitive => |primitive| .{ .primitive = primitive },
            .zst => .zst,
            .erased => |digest| .{ .erased = digest },
            .list => |elem| .{ .list = self.recursiveLink(elem, group_index) },
            .box => |elem| .{ .box = self.recursiveLink(elem, group_index) },
            .tuple => |span_| .{ .tuple = try self.recursiveLinkSpan(span_, group_index) },
            .func => |func| .{ .func = .{
                .args = try self.recursiveLinkSpan(func.args, group_index),
                .ret = self.recursiveLink(func.ret, group_index),
            } },
            .record => |span_| blk: {
                const source_fields = self.source.fieldSpan(span_);
                const fields = try self.arena.alloc(Store.RecursiveField, source_fields.len);
                for (source_fields, 0..) |field, index| {
                    fields[index] = .{ .name = field.name, .ty = self.recursiveLink(field.ty, group_index) };
                }
                break :blk .{ .record = fields };
            },
            .tag_union => |span_| blk: {
                const source_tags = self.source.tagSpan(span_);
                const tags = try self.arena.alloc(Store.RecursiveTag, source_tags.len);
                for (source_tags, 0..) |tag, index| {
                    tags[index] = .{
                        .name = tag.name,
                        .checked_name = tag.checked_name,
                        .payloads = try self.recursiveLinkSpan(tag.payloads, group_index),
                    };
                }
                break :blk .{ .tag_union = tags };
            },
            .named => |named| .{
                .named = .{
                    .named_type = named.named_type,
                    .def = named.def,
                    .kind = named.kind,
                    .builtin_owner = named.builtin_owner,
                    .args = try self.recursiveLinkSpan(named.args, group_index),
                    .backing = if (named.backing) |backing| .{
                        .ty = self.recursiveLink(backing.ty, group_index),
                        .use = backing.use,
                    } else null,
                    // Declared-order padding types are always leaf representations
                    // outside the cycle, so they resolve as an already-built span.
                    .declared_order = try self.resolvedDeclaredOrderSpan(named.declared_order),
                },
            },
        };
    }

    fn recursiveLink(
        self: *Reintern,
        child: TypeId,
        group_index: *std.AutoHashMap(u32, u32),
    ) Store.RecursiveLink {
        const child_index = self.index_of.get(child) orelse
            Common.invariant("reintern child was not collected");
        if (group_index.get(child_index)) |position| {
            return .{ .node = Store.recursiveNodeId(position) };
        }
        return .{ .interned = self.resolved.items[child_index] orelse
            Common.invariant("reintern cyclic child left an out-of-component node unresolved") };
    }

    fn recursiveLinkSpan(
        self: *Reintern,
        span_: Span,
        group_index: *std.AutoHashMap(u32, u32),
    ) std.mem.Allocator.Error![]const Store.RecursiveLink {
        const children = self.source.span(span_);
        if (children.len == 0) return &.{};
        const links = try self.arena.alloc(Store.RecursiveLink, children.len);
        for (children, 0..) |child, index| {
            links[index] = self.recursiveLink(child, group_index);
        }
        return links;
    }

    fn resolvedChild(self: *Reintern, child: TypeId) TypeId {
        const child_index = self.index_of.get(child) orelse
            Common.invariant("reintern child was not collected");
        return self.resolved.items[child_index] orelse
            Common.invariant("reintern acyclic child was not resolved before its parent");
    }

    fn resolvedSpan(self: *Reintern, span_: Span) std.mem.Allocator.Error![]const TypeId {
        const children = self.source.span(span_);
        if (children.len == 0) return &.{};
        const out = try self.arena.alloc(TypeId, children.len);
        for (children, 0..) |child, index| {
            out[index] = self.resolvedChild(child);
        }
        return out;
    }

    fn resolvedDeclaredOrderSlice(self: *Reintern, span_: Span) std.mem.Allocator.Error![]const DeclaredField {
        const source_fields = self.source.declaredFieldSpan(span_);
        if (source_fields.len == 0) return &.{};
        const out = try self.arena.alloc(DeclaredField, source_fields.len);
        for (source_fields, 0..) |field, index| {
            out[index] = switch (field) {
                .named => |name| .{ .named = name },
                .padding => |ty| .{ .padding = self.resolvedChild(ty) },
            };
        }
        return out;
    }

    fn resolvedDeclaredOrderSpan(self: *Reintern, span_: Span) std.mem.Allocator.Error!Span {
        const slice = try self.resolvedDeclaredOrderSlice(span_);
        return try self.dest.addDeclaredFields(slice);
    }
};

/// Yields the child `TypeId`s of one source content in a stable order, so the
/// reachability walk and the component builders agree on the edge set.
const ChildIterator = struct {
    view: Store.View,
    content: Content,
    phase: usize = 0,
    cursor: usize = 0,
    tag_cursor: usize = 0,

    fn init(view: Store.View, content: Content) ChildIterator {
        return .{ .view = view, .content = content };
    }

    fn next(self: *ChildIterator) ?TypeId {
        switch (self.content) {
            .primitive, .erased, .zst => return null,
            .list => |elem| return self.oneShot(elem),
            .box => |elem| return self.oneShot(elem),
            .tuple => |span_| return self.fromSpan(self.view.span(span_)),
            .func => |func| {
                const args = self.view.span(func.args);
                if (self.cursor < args.len) {
                    const child = args[self.cursor];
                    self.cursor += 1;
                    return child;
                }
                return self.oneShot(func.ret);
            },
            .record => |span_| {
                const fields = self.view.fieldSpan(span_);
                if (self.cursor < fields.len) {
                    const child = fields[self.cursor].ty;
                    self.cursor += 1;
                    return child;
                }
                return null;
            },
            .tag_union => |span_| {
                const tags = self.view.tagSpan(span_);
                while (self.tag_cursor < tags.len) {
                    const payloads = self.view.span(tags[self.tag_cursor].payloads);
                    if (self.cursor < payloads.len) {
                        const child = payloads[self.cursor];
                        self.cursor += 1;
                        return child;
                    }
                    self.tag_cursor += 1;
                    self.cursor = 0;
                }
                return null;
            },
            .named => |named| {
                const args = self.view.span(named.args);
                if (self.cursor < args.len) {
                    const child = args[self.cursor];
                    self.cursor += 1;
                    return child;
                }
                if (self.phase == 0) {
                    self.phase = 1;
                    if (named.backing) |backing| return backing.ty;
                }
                const declared = self.view.declaredFieldSpan(named.declared_order);
                while (self.tag_cursor < declared.len) {
                    const entry = declared[self.tag_cursor];
                    self.tag_cursor += 1;
                    switch (entry) {
                        .named => {},
                        .padding => |ty| return ty,
                    }
                }
                return null;
            },
        }
    }

    fn oneShot(self: *ChildIterator, child: TypeId) ?TypeId {
        if (self.phase != 0) return null;
        self.phase = 1;
        return child;
    }

    fn fromSpan(self: *ChildIterator, items: []const TypeId) ?TypeId {
        if (self.cursor < items.len) {
            const child = items[self.cursor];
            self.cursor += 1;
            return child;
        }
        return null;
    }
};

const InternerLookupDigest = struct {
    bytes: [32]u8,

    fn from(digest: names.TypeDigest) InternerLookupDigest {
        return .{ .bytes = digest.bytes };
    }
};

fn recordFieldLessThan(name_store: *const names.NameStore, lhs: Field, rhs: Field) bool {
    return name_store.recordFieldLabelTextLessThan(lhs.name, rhs.name);
}

fn tagLessThan(name_store: *const names.NameStore, lhs: Tag, rhs: Tag) bool {
    return name_store.tagLabelTextLessThan(lhs.name, rhs.name);
}

fn assertNoDuplicateRecordFields(name_store: *const names.NameStore, fields: []const Field) void {
    if (fields.len < 2) return;
    for (fields[1..], 1..) |field, index| {
        if (name_store.recordFieldLabelTextEql(fields[index - 1].name, field.name)) {
            Common.invariant("Monotype record type was constructed with duplicate fields");
        }
    }
}

fn assertNoDuplicateTags(name_store: *const names.NameStore, tags_: []const Tag) void {
    if (tags_.len < 2) return;
    for (tags_[1..], 1..) |tag, index| {
        if (name_store.tagLabelTextEql(tags_[index - 1].name, tag.name)) {
            Common.invariant("Monotype tag union type was constructed with duplicate tags");
        }
    }
}

fn cycleDigest(position: u32) names.TypeDigest {
    var hasher = std.crypto.hash.sha2.Sha256.init(.{});
    writeBytes(&hasher, "cycle");
    writeU32(&hasher, position);
    return .{ .bytes = hasher.finalResult() };
}

/// Digest the shape of a type whose walk has exhausted the fixed visiting
/// stack. The stack tracks cycles by stack position, so a walk deeper than the
/// stack cannot contain an unrecorded cycle shorter than the stack, and a
/// finite type of depth greater than the stack digests by its content shape
/// rather than recursing further. The shape is the content variant, never the
/// allocation id, so the digest is independent of construction order; any
/// resulting bucket collision is resolved by exact `typeEql`.
fn writeDeepShape(hasher: *std.crypto.hash.sha2.Sha256, content_tag: ContentTag) void {
    census.bump("digest_stack_depth_exceeded");
    writeBytes(hasher, "deep");
    writeBytes(hasher, @tagName(content_tag));
}

fn deepDigest(content_tag: ContentTag) names.TypeDigest {
    var hasher = std.crypto.hash.sha2.Sha256.init(.{});
    writeDeepShape(&hasher, content_tag);
    return .{ .bytes = hasher.finalResult() };
}

fn writeBytes(hasher: *std.crypto.hash.sha2.Sha256, bytes: []const u8) void {
    writeU32(hasher, @intCast(bytes.len));
    hasher.update(bytes);
}

/// Whether a builtin owner is a compiler-generated evidence carrier whose
/// backing structure distinguishes otherwise same-named applications: the
/// backing then participates in type digests, identity comparison, and
/// public-type equivalence. One definition serves every postcheck stage.
pub fn generatedEvidenceOwnerUsesBacking(owner: static_dispatch.BuiltinOwner) bool {
    return switch (owner) {
        .fields,
        .field,
        .parse_tag_union_spec,
        // `Iter`/`Stream` instances of one item type share a nominal but
        // carry different step captures per chain, so their layouts must be
        // distinguished by backing rather than by nominal identity alone.
        .iter,
        .stream,
        => true,
        else => false,
    };
}

fn writeU32(hasher: *std.crypto.hash.sha2.Sha256, value: u32) void {
    const little = std.mem.nativeToLittle(u32, value);
    hasher.update(std.mem.asBytes(&little));
}

fn writeOptionalU32(hasher: *std.crypto.hash.sha2.Sha256, value: ?u32) void {
    const present: u8 = if (value == null) 0 else 1;
    hasher.update(std.mem.asBytes(&present));
    if (value) |v| writeU32(hasher, v);
}

fn writeOptionalDigest(hasher: *std.crypto.hash.sha2.Sha256, value: ?names.TypeDigest) void {
    const present: u8 = if (value == null) 0 else 1;
    hasher.update(std.mem.asBytes(&present));
    if (value) |v| hasher.update(&v.bytes);
}

fn optionalDigestEql(lhs: ?names.TypeDigest, rhs: ?names.TypeDigest) bool {
    if (lhs == null and rhs == null) return true;
    if (lhs == null or rhs == null) return false;
    return std.mem.eql(u8, lhs.?.bytes[0..], rhs.?.bytes[0..]);
}

/// The builtin method owner a primitive monotype belongs to, by definition.
pub fn builtinOwnerForPrimitive(primitive: Primitive) static_dispatch.BuiltinOwner {
    return switch (primitive) {
        .bool => .bool,
        .str => .str,
        .u8 => .u8,
        .i8 => .i8,
        .u16 => .u16,
        .i16 => .i16,
        .u32 => .u32,
        .i32 => .i32,
        .u64 => .u64,
        .i64 => .i64,
        .u128 => .u128,
        .i128 => .i128,
        .f32 => .f32,
        .f64 => .f64,
        .dec => .dec,
    };
}

test "monotype type declarations are referenced" {
    std.testing.refAllDecls(@This());
}

test "monotype type epochs distinguish recycled ids" {
    var store = Store.init(std.testing.allocator);
    defer store.deinit();

    _ = try store.add(.{ .primitive = .u64 });
    const mark_ = store.mark();
    const discarded = try store.add(.{ .primitive = .i64 });
    const discarded_epoch = store.typeEpoch(discarded);

    store.restore(mark_);
    const replacement = try store.add(.{ .primitive = .str });

    try std.testing.expectEqual(discarded, replacement);
    try std.testing.expect(discarded_epoch != store.typeEpoch(replacement));
}

test "monotype type interner reuses child-first function nodes" {
    var name_store = names.NameStore.init(std.testing.allocator);
    defer name_store.deinit();

    const interner = try Interner.init(std.testing.allocator, &name_store);
    defer interner.deinit();

    const unit = try interner.internZst();
    const first = try interner.internFunc(&.{unit}, unit);
    const second = try interner.internFunc(&.{unit}, unit);

    try std.testing.expectEqual(first, second);
    try std.testing.expectEqual(@as(usize, 2), interner.view().types.len);
}

test "monotype type interner normalizes record and tag rows" {
    var name_store = names.NameStore.init(std.testing.allocator);
    defer name_store.deinit();

    const a_field = try name_store.internRecordFieldLabel("a");
    const b_field = try name_store.internRecordFieldLabel("b");
    const a_tag = try name_store.internTagLabel("A");
    const b_tag = try name_store.internTagLabel("B");

    const interner = try Interner.init(std.testing.allocator, &name_store);
    defer interner.deinit();

    const unit = try interner.internZst();
    const first_record = try interner.internRecord(&.{
        .{ .name = b_field, .ty = unit },
        .{ .name = a_field, .ty = unit },
    });
    const second_record = try interner.internRecord(&.{
        .{ .name = a_field, .ty = unit },
        .{ .name = b_field, .ty = unit },
    });
    try std.testing.expectEqual(first_record, second_record);

    const first_tags = try interner.internTagUnion(&.{
        .{ .name = b_tag, .checked_name = b_tag, .payloads = &.{unit} },
        .{ .name = a_tag, .checked_name = a_tag, .payloads = &.{unit} },
    });
    const second_tags = try interner.internTagUnion(&.{
        .{ .name = a_tag, .checked_name = a_tag, .payloads = &.{unit} },
        .{ .name = b_tag, .checked_name = b_tag, .payloads = &.{unit} },
    });
    try std.testing.expectEqual(first_tags, second_tags);

    const record_fields = interner.fieldSpan(interner.get(first_record).record);
    try std.testing.expectEqual(a_field, GuardedList.at(record_fields, 0).name);
    try std.testing.expectEqual(b_field, GuardedList.at(record_fields, 1).name);
    const tag_fields = interner.tagSpan(interner.get(first_tags).tag_union);
    try std.testing.expectEqual(a_tag, GuardedList.at(tag_fields, 0).name);
    try std.testing.expectEqual(b_tag, GuardedList.at(tag_fields, 1).name);
}

test "monotype type interner preserves tag payload order" {
    var name_store = names.NameStore.init(std.testing.allocator);
    defer name_store.deinit();

    const tag_name = try name_store.internTagLabel("Pair");

    const interner = try Interner.init(std.testing.allocator, &name_store);
    defer interner.deinit();

    const first = try interner.internPrimitive(.i64);
    const second = try interner.internPrimitive(.str);
    const tag_ty = try interner.internTagUnion(&.{
        .{ .name = tag_name, .checked_name = tag_name, .payloads = &.{ first, second } },
    });

    const tags_ = interner.tagSpan(interner.get(tag_ty).tag_union);
    const stored_payloads = interner.span(GuardedList.at(tags_, 0).payloads);
    try std.testing.expectEqual(first, GuardedList.at(stored_payloads, 0));
    try std.testing.expectEqual(second, GuardedList.at(stored_payloads, 1));
}

test "monotype type interner checks exact equality after digest match" {
    var name_store = names.NameStore.init(std.testing.allocator);
    defer name_store.deinit();

    const module_identity = try name_store.internModuleIdentity(&([_]u8{0xAB} ** 32));
    const first_name = try name_store.internTypeName("First");
    const second_name = try name_store.internTypeName("Second");

    const interner = try Interner.init(std.testing.allocator, &name_store);
    defer interner.deinit();

    const first = try interner.internNamed(.{
        .named_type = .{ .module = .{}, .ty = @enumFromInt(1) },
        .def = .{ .module = module_identity, .type_name = first_name },
        .kind = .alias,
        .backing = null,
    });
    const second = try interner.internNamed(.{
        .named_type = .{ .module = .{}, .ty = @enumFromInt(2) },
        .def = .{ .module = module_identity, .type_name = second_name },
        .kind = .alias,
        .backing = null,
    });

    const first_digest = interner.typeDigest(first);
    const second_digest = interner.typeDigest(second);
    try std.testing.expectEqualSlices(u8, first_digest.bytes[0..], second_digest.bytes[0..]);
    try std.testing.expect(first != second);
}

test "monotype interner erases a storage-transparent alias to its backing" {
    var name_store = names.NameStore.init(std.testing.allocator);
    defer name_store.deinit();

    const module_identity = try name_store.internModuleIdentity(&([_]u8{0xCD} ** 32));
    const type_name = try name_store.internTypeName("Transparent");

    const interner = try Interner.init(std.testing.allocator, &name_store);
    defer interner.deinit();

    const backing = try interner.internPrimitive(.i64);
    const aliased = try interner.internNamed(.{
        .named_type = .{ .module = .{}, .ty = @enumFromInt(1) },
        .def = .{ .module = module_identity, .type_name = type_name },
        .kind = .alias,
        .builtin_owner = null,
        .backing = .{ .ty = backing, .use = .inspectable },
    });

    // No interned id names the storage-transparent alias node: the already
    // interned backing is returned, and the pool holds only the backing.
    try std.testing.expectEqual(backing, aliased);
    try std.testing.expect(interner.get(aliased) == .primitive);
    try std.testing.expectEqual(@as(usize, 1), interner.view().types.len);
}

test "monotype interner keeps a builtin-owned alias nontransparent" {
    var name_store = names.NameStore.init(std.testing.allocator);
    defer name_store.deinit();

    const module_identity = try name_store.internModuleIdentity(&([_]u8{0xEF} ** 32));
    const type_name = try name_store.internTypeName("Owned");

    const interner = try Interner.init(std.testing.allocator, &name_store);
    defer interner.deinit();

    const backing = try interner.internPrimitive(.i64);

    const owned = try interner.internNamed(.{
        .named_type = .{ .module = .{}, .ty = @enumFromInt(1) },
        .def = .{ .module = module_identity, .type_name = type_name },
        .kind = .alias,
        .builtin_owner = .str,
        .backing = .{ .ty = backing, .use = .inspectable },
    });

    // Accepted: an identical builtin-owned alias interns to the same id.
    const owned_again = try interner.internNamed(.{
        .named_type = .{ .module = .{}, .ty = @enumFromInt(1) },
        .def = .{ .module = module_identity, .type_name = type_name },
        .kind = .alias,
        .builtin_owner = .str,
        .backing = .{ .ty = backing, .use = .inspectable },
    });
    try std.testing.expectEqual(owned, owned_again);

    // The alias node survives insertion; it is not erased to its backing.
    try std.testing.expect(owned != backing);
    try std.testing.expect(interner.get(owned) == .named);

    // Rejected: the alias is not equal to its bare backing, and its digest
    // differs, because the checked dispatch owner is part of its identity.
    try std.testing.expect(!try interner.typeEql(owned, backing));
    const owned_digest = interner.typeDigest(owned);
    const backing_digest = interner.typeDigest(backing);
    try std.testing.expect(!std.mem.eql(u8, owned_digest.bytes[0..], backing_digest.bytes[0..]));

    // Rejected: a same-shape alias with a different owner is a distinct id.
    const other_owner = try interner.internNamed(.{
        .named_type = .{ .module = .{}, .ty = @enumFromInt(1) },
        .def = .{ .module = module_identity, .type_name = type_name },
        .kind = .alias,
        .builtin_owner = .bool,
        .backing = .{ .ty = backing, .use = .inspectable },
    });
    try std.testing.expect(owned != other_owner);
    try std.testing.expect(!try interner.typeEql(owned, other_owner));
}

test "monotype type interner seals recursive root before exposing type id" {
    var name_store = names.NameStore.init(std.testing.allocator);
    defer name_store.deinit();

    const field_name = try name_store.internRecordFieldLabel("next");

    const interner = try Interner.init(std.testing.allocator, &name_store);
    defer interner.deinit();

    const root = try interner.internRecursiveRoot(.{ .record = &.{
        .{ .name = field_name, .ty = .root },
    } });

    const fields = interner.fieldSpan(interner.get(root).record);
    try std.testing.expectEqual(@as(usize, 1), fields.len);
    try std.testing.expectEqual(root, GuardedList.at(fields, 0).ty);
    try std.testing.expectEqual(@as(?Store.VerifyError, null), interner.verify());
}

test "monotype type interner reuses equivalent recursive roots" {
    var name_store = names.NameStore.init(std.testing.allocator);
    defer name_store.deinit();

    const field_name = try name_store.internRecordFieldLabel("next");

    const interner = try Interner.init(std.testing.allocator, &name_store);
    defer interner.deinit();

    const first = try interner.internRecursiveRoot(.{ .record = &.{
        .{ .name = field_name, .ty = .root },
    } });
    const second = try interner.internRecursiveRoot(.{ .record = &.{
        .{ .name = field_name, .ty = .root },
    } });

    try std.testing.expectEqual(first, second);
    try std.testing.expectEqual(@as(usize, 1), interner.view().types.len);
}

test "monotype type interner seals multi-node recursive group privately" {
    var name_store = names.NameStore.init(std.testing.allocator);
    defer name_store.deinit();

    const field_name = try name_store.internRecordFieldLabel("step");

    const interner = try Interner.init(std.testing.allocator, &name_store);
    defer interner.deinit();

    const record_node = Interner.recursiveNodeId(0);
    const func_node = Interner.recursiveNodeId(1);
    const first = try interner.internRecursiveGroupRoot(&.{
        .{ .record = &.{
            .{ .name = field_name, .ty = .{ .node = func_node } },
        } },
        .{ .func = .{
            .args = &.{},
            .ret = .{ .node = record_node },
        } },
    }, record_node);
    const second = try interner.internRecursiveGroupRoot(&.{
        .{ .record = &.{
            .{ .name = field_name, .ty = .{ .node = func_node } },
        } },
        .{ .func = .{
            .args = &.{},
            .ret = .{ .node = record_node },
        } },
    }, record_node);

    try std.testing.expectEqual(first, second);
    try std.testing.expectEqual(@as(usize, 2), interner.view().types.len);

    const fields = interner.fieldSpan(interner.get(first).record);
    const step_ty = GuardedList.at(fields, 0).ty;
    const step_fn = interner.get(step_ty).func;
    try std.testing.expectEqual(first, step_fn.ret);

    // Every member is registered as a root, so re-interning the group entered
    // at the func node deduplicates to the func member reachable from `first`,
    // and re-interning entered at the record node deduplicates to `first`. No
    // new ids are created either way.
    const record_rooted = try interner.internRecursiveGroupRoot(&.{
        .{ .record = &.{
            .{ .name = field_name, .ty = .{ .node = func_node } },
        } },
        .{ .func = .{
            .args = &.{},
            .ret = .{ .node = record_node },
        } },
    }, record_node);
    const func_rooted = try interner.internRecursiveGroupRoot(&.{
        .{ .record = &.{
            .{ .name = field_name, .ty = .{ .node = func_node } },
        } },
        .{ .func = .{
            .args = &.{},
            .ret = .{ .node = record_node },
        } },
    }, func_node);
    try std.testing.expectEqual(first, record_rooted);
    try std.testing.expectEqual(step_ty, func_rooted);
    try std.testing.expectEqual(@as(usize, 2), interner.view().types.len);
}

test "monotype type interner interns a recursive group independent of node order" {
    var name_store = names.NameStore.init(std.testing.allocator);
    defer name_store.deinit();

    const field_name = try name_store.internRecordFieldLabel("step");

    const interner = try Interner.init(std.testing.allocator, &name_store);
    defer interner.deinit();

    // Node order [record, func]: record.step -> func, func.ret -> record.
    const record_first = try interner.internRecursiveGroupRoot(&.{
        .{ .record = &.{
            .{ .name = field_name, .ty = .{ .node = Interner.recursiveNodeId(1) } },
        } },
        .{ .func = .{
            .args = &.{},
            .ret = .{ .node = Interner.recursiveNodeId(0) },
        } },
    }, Interner.recursiveNodeId(0));
    const func_first = try interner.internRecursiveGroupRoot(&.{
        .{ .record = &.{
            .{ .name = field_name, .ty = .{ .node = Interner.recursiveNodeId(1) } },
        } },
        .{ .func = .{
            .args = &.{},
            .ret = .{ .node = Interner.recursiveNodeId(0) },
        } },
    }, Interner.recursiveNodeId(1));

    // Swapped node order [func, record]: same logical graph, different indices.
    const record_second = try interner.internRecursiveGroupRoot(&.{
        .{ .func = .{
            .args = &.{},
            .ret = .{ .node = Interner.recursiveNodeId(1) },
        } },
        .{ .record = &.{
            .{ .name = field_name, .ty = .{ .node = Interner.recursiveNodeId(0) } },
        } },
    }, Interner.recursiveNodeId(1));
    const func_second = try interner.internRecursiveGroupRoot(&.{
        .{ .func = .{
            .args = &.{},
            .ret = .{ .node = Interner.recursiveNodeId(1) },
        } },
        .{ .record = &.{
            .{ .name = field_name, .ty = .{ .node = Interner.recursiveNodeId(0) } },
        } },
    }, Interner.recursiveNodeId(0));

    // Each node interns to the same id regardless of construction order.
    try std.testing.expectEqual(record_first, record_second);
    try std.testing.expectEqual(func_first, func_second);
    try std.testing.expectEqual(@as(usize, 2), interner.view().types.len);
}

test "monotype type interner shares one id for symmetric recursive members" {
    var name_store = names.NameStore.init(std.testing.allocator);
    defer name_store.deinit();

    const field_name = try name_store.internRecordFieldLabel("x");

    const interner = try Interner.init(std.testing.allocator, &name_store);
    defer interner.deinit();

    // A = { x: B }, B = { x: A }: two nodes whose rooted graphs are equal, so
    // they denote one rooted type and must share a single id.
    const a_node = Interner.recursiveNodeId(0);
    const b_node = Interner.recursiveNodeId(1);
    const root = try interner.internRecursiveGroupRoot(&.{
        .{ .record = &.{
            .{ .name = field_name, .ty = .{ .node = b_node } },
        } },
        .{ .record = &.{
            .{ .name = field_name, .ty = .{ .node = a_node } },
        } },
    }, a_node);

    // The symmetric group collapses to a single self-referential record id.
    try std.testing.expectEqual(@as(usize, 1), interner.view().types.len);
    const fields = interner.fieldSpan(interner.get(root).record);
    try std.testing.expectEqual(root, GuardedList.at(fields, 0).ty);
    try std.testing.expectEqual(@as(?Store.VerifyError, null), interner.verify());
}

test "monotype type interner keeps distinct recursive roots with different children" {
    var name_store = names.NameStore.init(std.testing.allocator);
    defer name_store.deinit();

    const next_name = try name_store.internRecordFieldLabel("next");
    const done_name = try name_store.internRecordFieldLabel("done");

    const interner = try Interner.init(std.testing.allocator, &name_store);
    defer interner.deinit();

    const bool_ty = try interner.internPrimitive(.bool);
    const recursive_only = try interner.internRecursiveRoot(.{ .record = &.{
        .{ .name = next_name, .ty = .root },
    } });
    const recursive_with_bool = try interner.internRecursiveRoot(.{ .record = &.{
        .{ .name = next_name, .ty = .root },
        .{ .name = done_name, .ty = .{ .interned = bool_ty } },
    } });

    try std.testing.expect(recursive_only != recursive_with_bool);
    try std.testing.expect(!try interner.typeEql(recursive_only, recursive_with_bool));
}

test "monotype interner interns empty rows and heads to one id" {
    var name_store = names.NameStore.init(std.testing.allocator);
    defer name_store.deinit();

    const interner = try Interner.init(std.testing.allocator, &name_store);
    defer interner.deinit();

    // Empty record: every zero-field construction interns to a single id.
    const empty_record_a = try interner.internRecord(&.{});
    const empty_record_b = try interner.internRecord(&.{});
    try std.testing.expectEqual(empty_record_a, empty_record_b);
    try std.testing.expect(interner.get(empty_record_a) == .record);
    try std.testing.expectEqual(@as(u32, 0), interner.get(empty_record_a).record.len);

    // Empty tag union: interns to a single id for the other head.
    const empty_tags_a = try interner.internTagUnion(&.{});
    const empty_tags_b = try interner.internTagUnion(&.{});
    try std.testing.expectEqual(empty_tags_a, empty_tags_b);
    try std.testing.expect(interner.get(empty_tags_a) == .tag_union);
    try std.testing.expectEqual(@as(u32, 0), interner.get(empty_tags_a).tag_union.len);

    // The empty record and empty tag union are distinct interned heads.
    try std.testing.expect(empty_record_a != empty_tags_a);
    try std.testing.expectEqual(@as(usize, 2), interner.view().types.len);
}

test "monotype store empty field span equals a zero-length record-field span" {
    var name_store = names.NameStore.init(std.testing.allocator);
    defer name_store.deinit();
    var store = Store.init(std.testing.allocator);
    defer store.deinit();

    // A zero-length field row normalizes to the empty span, whether it goes
    // through the label-normalizing appender or the raw one, so no record head
    // is distinguished by which empty form built its row.
    const from_record_fields = try store.addRecordFields(&name_store, &.{});
    const from_fields = try store.addFields(&.{});
    try std.testing.expectEqual(Span.empty(), from_record_fields);
    try std.testing.expectEqual(Span.empty(), from_fields);
}

test "monotype store keeps function-containing shapes distinct" {
    var store = Store.init(std.testing.allocator);
    defer store.deinit();

    const unit = try store.add(.zst);
    const args = try store.addSpan(&.{unit});

    const fn_a = try store.add(.{ .func = .{ .args = args, .ret = unit } });
    const fn_b = try store.add(.{ .func = .{ .args = args, .ret = unit } });
    try std.testing.expect(fn_a != fn_b);

    const list_a = try store.add(.{ .list = fn_a });
    const list_b = try store.add(.{ .list = fn_a });
    try std.testing.expect(list_a != list_b);
}

test "monotype row entries retain checked label ids" {
    var name_store = names.NameStore.init(std.testing.allocator);
    defer name_store.deinit();

    const field_name = try name_store.internRecordFieldLabel("age");
    const tag_name = try name_store.internTagLabel("Adult");

    var store = Store.init(std.testing.allocator);
    defer store.deinit();

    const i64_ty = try store.add(.{ .primitive = .i64 });
    const fields = try store.addFields(&.{.{ .name = field_name, .ty = i64_ty }});
    const payloads = try store.addSpan(&.{i64_ty});
    const tags = try store.addTags(&.{.{ .name = tag_name, .checked_name = tag_name, .payloads = payloads }});

    try std.testing.expectEqual(field_name, GuardedList.at(store.fieldSpan(fields), 0).name);
    try std.testing.expectEqual(tag_name, GuardedList.at(store.tagSpan(tags), 0).name);
}

test "monotype empty spans use shared empty descriptor" {
    var store = Store.init(std.testing.allocator);
    defer store.deinit();

    const unit = try store.add(.zst);
    const nonempty_span = try store.addSpan(&.{unit});
    const nonempty_fields = try store.addFields(&.{.{ .name = @enumFromInt(1), .ty = unit }});
    const nonempty_tags = try store.addTags(&.{.{ .name = @enumFromInt(2), .checked_name = @enumFromInt(2), .payloads = nonempty_span }});
    try std.testing.expect(nonempty_span.len == 1);
    try std.testing.expect(nonempty_fields.len == 1);
    try std.testing.expect(nonempty_tags.len == 1);

    try std.testing.expectEqual(Span.empty(), try store.addSpan(&.{}));
    try std.testing.expectEqual(Span.empty(), try store.addFields(&.{}));
    try std.testing.expectEqual(Span.empty(), try store.addTags(&.{}));
}

test "monotype type verifier accepts normalized rows" {
    var name_store = names.NameStore.init(std.testing.allocator);
    defer name_store.deinit();

    var store = Store.init(std.testing.allocator);
    defer store.deinit();

    const a_field = try name_store.internRecordFieldLabel("a");
    const b_field = try name_store.internRecordFieldLabel("b");
    const a_tag = try name_store.internTagLabel("A");
    const b_tag = try name_store.internTagLabel("B");

    const i64_ty = try store.add(.{ .primitive = .i64 });
    const fields = try store.addFields(&.{
        .{ .name = a_field, .ty = i64_ty },
        .{ .name = b_field, .ty = i64_ty },
    });
    const payloads = try store.addSpan(&.{i64_ty});
    const tags = try store.addTags(&.{
        .{ .name = a_tag, .checked_name = a_tag, .payloads = payloads },
        .{ .name = b_tag, .checked_name = b_tag, .payloads = Span.empty() },
    });
    _ = try store.add(.{ .record = fields });
    _ = try store.add(.{ .tag_union = tags });

    try std.testing.expectEqual(@as(?Store.VerifyError, null), store.verify(&name_store));
}

test "monotype type verifier rejects malformed rows and references" {
    var name_store = names.NameStore.init(std.testing.allocator);
    defer name_store.deinit();

    const a_field = try name_store.internRecordFieldLabel("a");
    const b_field = try name_store.internRecordFieldLabel("b");

    {
        var store = Store.init(std.testing.allocator);
        defer store.deinit();

        const i64_ty = try store.add(.{ .primitive = .i64 });
        const unsorted = try store.addFields(&.{
            .{ .name = b_field, .ty = i64_ty },
            .{ .name = a_field, .ty = i64_ty },
        });
        _ = try store.add(.{ .record = unsorted });
        try std.testing.expectEqual(Store.VerifyError.record_fields_not_sorted, store.verify(&name_store).?);
    }

    {
        var store = Store.init(std.testing.allocator);
        defer store.deinit();

        const bad_fields = try store.addFields(&.{.{ .name = a_field, .ty = @enumFromInt(99) }});
        _ = try store.add(.{ .record = bad_fields });
        try std.testing.expectEqual(Store.VerifyError.type_ref_out_of_bounds, store.verify(&name_store).?);
    }
}

test "monotype cached digest reuses acyclic child digests and invalidates on reserved refill" {
    var name_store = names.NameStore.init(std.testing.allocator);
    defer name_store.deinit();

    var store = Store.init(std.testing.allocator);
    defer store.deinit();

    const inner_field = try name_store.internRecordFieldLabel("inner");
    const outer_field = try name_store.internRecordFieldLabel("outer");

    const i64_ty = try store.add(.{ .primitive = .i64 });
    const inner_fields = try store.addFields(&.{.{ .name = inner_field, .ty = i64_ty }});
    const inner = try store.add(.{ .record = inner_fields });

    var inner_stats: Store.DigestStats = .{};
    const inner_digest = store.typeDigestCached(&name_store, inner, &inner_stats);
    try std.testing.expectEqual(@as(u64, 0), inner_stats.cache_hits);
    try std.testing.expectEqual(@as(u64, 2), inner_stats.cache_misses);
    try std.testing.expectEqual(@as(u64, 2), inner_stats.nodes_visited);

    const outer_fields = try store.addFields(&.{.{ .name = outer_field, .ty = inner }});
    const outer = try store.add(.{ .record = outer_fields });

    var outer_stats: Store.DigestStats = .{};
    _ = store.typeDigestCached(&name_store, outer, &outer_stats);
    try std.testing.expectEqual(@as(u64, 1), outer_stats.cache_hits);
    try std.testing.expectEqual(@as(u64, 1), outer_stats.cache_misses);
    try std.testing.expectEqual(@as(u64, 1), outer_stats.nodes_visited);

    store.fillReservedSlot(inner, .{ .record = Span.empty() });

    var after_refill_stats: Store.DigestStats = .{};
    const after_refill = store.typeDigestCached(&name_store, inner, &after_refill_stats);
    try std.testing.expect(!std.mem.eql(u8, inner_digest.bytes[0..], after_refill.bytes[0..]));
    try std.testing.expectEqual(@as(u64, 0), after_refill_stats.cache_hits);
    try std.testing.expectEqual(@as(u64, 1), after_refill_stats.cache_misses);
    try std.testing.expectEqual(@as(u64, 1), after_refill_stats.nodes_visited);
}

test "monotype cached and uncached digests agree on type identity" {
    // Guards the shared identity walker: the cached digest path is an
    // optimization of the uncached path, so both must make exactly the same
    // identity decisions. Their absolute bytes intentionally differ (the cached
    // path folds children in as nested sub-digests for incrementality; the
    // uncached path inlines them), so the invariant is not byte equality but
    // that both induce the same equivalence relation on types, that each is
    // deterministic, and that a digest match is always confirmable by the
    // authoritative `typeEql`.
    var name_store = names.NameStore.init(std.testing.allocator);
    defer name_store.deinit();

    var store = Store.init(std.testing.allocator);
    defer store.deinit();

    const module_identity = try name_store.internModuleIdentity(&([_]u8{0xC3} ** 32));
    const box_name = try name_store.internTypeName("Box");
    const unit_name = try name_store.internTypeName("Unit");
    const padded_name = try name_store.internTypeName("Padded");
    const wrap_name = try name_store.internTypeName("Wrap");
    const checked_ty: checked.CheckedTypeId = @enumFromInt(1);

    const f_a = try name_store.internRecordFieldLabel("a");
    const f_b = try name_store.internRecordFieldLabel("b");
    const f_step = try name_store.internRecordFieldLabel("step");
    const f_other = try name_store.internRecordFieldLabel("other");
    const t_a = try name_store.internTagLabel("A");
    const t_b = try name_store.internTagLabel("B");

    // Deterministic, enumerated corpus. Every entry carries a group id: entries
    // that are structurally equal under `typeEql` share a group (they must all
    // digest equally); entries in different groups are distinct under full
    // identity (their full digests must differ). The corpus is kept "clean" --
    // no two groups differ only in a field the digest sees but `typeEql`
    // ignores (declared order or non-builtin backing) -- so the group id
    // cleanly encodes both relations at once.
    var types: [64]TypeId = undefined;
    var groups: [64]usize = undefined;
    var count: usize = 0;
    var next_group: usize = 0;
    const H = struct {
        fn push(ts: *[64]TypeId, gs: *[64]usize, n: *usize, ty: TypeId, group: usize) void {
            ts[n.*] = ty;
            gs[n.*] = group;
            n.* += 1;
        }
    };

    // Primitives, zst, erased.
    const i64_ty = try store.add(.{ .primitive = .i64 });
    const str_ty = try store.add(.{ .primitive = .str });
    const bool_ty = try store.add(.{ .primitive = .bool });
    const i64_group = next_group;
    H.push(&types, &groups, &count, i64_ty, i64_group);
    next_group += 1;
    H.push(&types, &groups, &count, str_ty, next_group);
    next_group += 1;
    H.push(&types, &groups, &count, bool_ty, next_group);
    next_group += 1;
    H.push(&types, &groups, &count, try store.add(.zst), next_group);
    next_group += 1;
    H.push(&types, &groups, &count, try store.add(.{ .erased = .{ .bytes = [_]u8{1} ** 32 } }), next_group);
    next_group += 1;
    H.push(&types, &groups, &count, try store.add(.{ .erased = .{ .bytes = [_]u8{2} ** 32 } }), next_group);
    next_group += 1;

    // Lists and boxes.
    H.push(&types, &groups, &count, try store.add(.{ .list = i64_ty }), next_group);
    next_group += 1;
    H.push(&types, &groups, &count, try store.add(.{ .list = str_ty }), next_group);
    next_group += 1;
    H.push(&types, &groups, &count, try store.add(.{ .box = i64_ty }), next_group);
    next_group += 1;

    // Tuples (order-sensitive).
    H.push(&types, &groups, &count, try store.add(.{ .tuple = try store.addSpan(&.{ i64_ty, str_ty }) }), next_group);
    next_group += 1;
    H.push(&types, &groups, &count, try store.add(.{ .tuple = try store.addSpan(&.{ str_ty, i64_ty }) }), next_group);
    next_group += 1;

    // Records: empty, one field, two fields, nested.
    H.push(&types, &groups, &count, try store.add(.{ .record = try store.addFields(&.{}) }), next_group);
    next_group += 1;
    const rec_a_i64 = try store.add(.{ .record = try store.addFields(&.{.{ .name = f_a, .ty = i64_ty }}) });
    H.push(&types, &groups, &count, rec_a_i64, next_group);
    next_group += 1;
    H.push(&types, &groups, &count, try store.add(.{ .record = try store.addFields(&.{
        .{ .name = f_a, .ty = i64_ty },
        .{ .name = f_b, .ty = str_ty },
    }) }), next_group);
    next_group += 1;
    H.push(&types, &groups, &count, try store.add(.{ .record = try store.addFields(&.{.{ .name = f_a, .ty = rec_a_i64 }}) }), next_group);
    next_group += 1;

    // Tag unions.
    H.push(&types, &groups, &count, try store.add(.{ .tag_union = try store.addTags(&.{}) }), next_group);
    next_group += 1;
    H.push(&types, &groups, &count, try store.add(.{ .tag_union = try store.addTags(&.{
        .{ .name = t_a, .checked_name = t_a, .payloads = Span.empty() },
    }) }), next_group);
    next_group += 1;
    H.push(&types, &groups, &count, try store.add(.{ .tag_union = try store.addTags(&.{
        .{ .name = t_a, .checked_name = t_a, .payloads = try store.addSpan(&.{i64_ty}) },
    }) }), next_group);
    next_group += 1;
    H.push(&types, &groups, &count, try store.add(.{ .tag_union = try store.addTags(&.{
        .{ .name = t_a, .checked_name = t_a, .payloads = Span.empty() },
        .{ .name = t_b, .checked_name = t_b, .payloads = Span.empty() },
    }) }), next_group);
    next_group += 1;

    // Functions.
    H.push(&types, &groups, &count, try store.add(.{ .func = .{ .args = try store.addSpan(&.{i64_ty}), .ret = str_ty } }), next_group);
    next_group += 1;
    H.push(&types, &groups, &count, try store.add(.{ .func = .{ .args = Span.empty(), .ret = i64_ty } }), next_group);
    next_group += 1;
    H.push(&types, &groups, &count, try store.add(.{ .func = .{ .args = try store.addSpan(&.{ i64_ty, str_ty }), .ret = bool_ty } }), next_group);
    next_group += 1;

    // Named types: distinct by args, name, and kind.
    const named_box_i64 = try store.add(.{ .named = .{
        .named_type = .{ .module = .{}, .ty = checked_ty },
        .def = .{ .module = module_identity, .type_name = box_name },
        .kind = .nominal,
        .args = try store.addSpan(&.{i64_ty}),
    } });
    H.push(&types, &groups, &count, named_box_i64, next_group);
    next_group += 1;
    H.push(&types, &groups, &count, try store.add(.{ .named = .{
        .named_type = .{ .module = .{}, .ty = checked_ty },
        .def = .{ .module = module_identity, .type_name = box_name },
        .kind = .nominal,
        .args = try store.addSpan(&.{str_ty}),
    } }), next_group);
    next_group += 1;
    H.push(&types, &groups, &count, try store.add(.{ .named = .{
        .named_type = .{ .module = .{}, .ty = checked_ty },
        .def = .{ .module = module_identity, .type_name = unit_name },
        .kind = .nominal,
        .args = Span.empty(),
    } }), next_group);
    next_group += 1;
    H.push(&types, &groups, &count, try store.add(.{ .named = .{
        .named_type = .{ .module = .{}, .ty = checked_ty },
        .def = .{ .module = module_identity, .type_name = box_name },
        .kind = .@"opaque",
        .args = try store.addSpan(&.{i64_ty}),
    } }), next_group);
    next_group += 1;

    // Named type carrying a declared padding order (exercises that walk arm).
    H.push(&types, &groups, &count, try store.add(.{ .named = .{
        .named_type = .{ .module = .{}, .ty = checked_ty },
        .def = .{ .module = module_identity, .type_name = padded_name },
        .kind = .nominal,
        .args = Span.empty(),
        .declared_order = try store.addDeclaredFields(&.{ .{ .named = f_a }, .{ .padding = i64_ty } }),
    } }), next_group);
    next_group += 1;

    // Named type carrying a backing (exercises the backing walk arm).
    H.push(&types, &groups, &count, try store.add(.{ .named = .{
        .named_type = .{ .module = .{}, .ty = checked_ty },
        .def = .{ .module = module_identity, .type_name = wrap_name },
        .kind = .nominal,
        .args = Span.empty(),
        .backing = .{ .ty = i64_ty, .use = .inspectable },
    } }), next_group);
    next_group += 1;

    // Isomorphic recursive records at different ids: structurally equal, so
    // they share a group. Cycles are tied by reserving a slot, building
    // children that reference the reserved id, then filling the slot.
    const cyc_group = next_group;
    next_group += 1;
    {
        const rec = try store.reserveSlot();
        const fn_ret = try store.add(.{ .func = .{ .args = Span.empty(), .ret = rec } });
        const flds = try store.addFields(&.{.{ .name = f_step, .ty = fn_ret }});
        store.fillReservedSlot(rec, .{ .record = flds });
        H.push(&types, &groups, &count, rec, cyc_group);
    }
    {
        const rec = try store.reserveSlot();
        const fn_ret = try store.add(.{ .func = .{ .args = Span.empty(), .ret = rec } });
        const flds = try store.addFields(&.{.{ .name = f_step, .ty = fn_ret }});
        store.fillReservedSlot(rec, .{ .record = flds });
        H.push(&types, &groups, &count, rec, cyc_group);
    }
    // A different cycle (distinct field name) is its own group.
    {
        const rec = try store.reserveSlot();
        const fn_ret = try store.add(.{ .func = .{ .args = Span.empty(), .ret = rec } });
        const flds = try store.addFields(&.{.{ .name = f_other, .ty = fn_ret }});
        store.fillReservedSlot(rec, .{ .record = flds });
        H.push(&types, &groups, &count, rec, next_group);
        next_group += 1;
    }

    // Alias-transparent chains participate in the main equivalence corpus: an
    // alias with backing has the same identity as the backing, including cached
    // digest identity.
    const alias_i64 = try store.add(.{ .named = .{
        .named_type = .{ .module = .{}, .ty = checked_ty },
        .def = .{ .module = module_identity, .type_name = box_name },
        .kind = .alias,
        .args = Span.empty(),
        .backing = .{ .ty = i64_ty, .use = .inspectable },
    } });
    const alias_chain = try store.add(.{ .named = .{
        .named_type = .{ .module = .{}, .ty = checked_ty },
        .def = .{ .module = module_identity, .type_name = unit_name },
        .kind = .alias,
        .args = Span.empty(),
        .backing = .{ .ty = alias_i64, .use = .inspectable },
    } });
    H.push(&types, &groups, &count, alias_i64, i64_group);
    H.push(&types, &groups, &count, alias_chain, i64_group);

    const items = types[0..count];
    const item_groups = groups[0..count];

    // Determinism: every public digest function returns the same bytes when
    // called twice for the same type.
    for (items) |ty| {
        try std.testing.expect(std.mem.eql(u8, store.typeDigest(&name_store, ty).bytes[0..], store.typeDigest(&name_store, ty).bytes[0..]));
        try std.testing.expect(std.mem.eql(u8, store.specializationDigest(&name_store, ty).bytes[0..], store.specializationDigest(&name_store, ty).bytes[0..]));
        try std.testing.expect(std.mem.eql(u8, store.typeDigestCached(&name_store, ty, null).bytes[0..], store.typeDigestCached(&name_store, ty, null).bytes[0..]));
        try std.testing.expect(std.mem.eql(u8, store.specializationDigestCached(&name_store, ty, null).bytes[0..], store.specializationDigestCached(&name_store, ty, null).bytes[0..]));
    }

    for (items, item_groups, 0..) |lhs, lhs_group, i| {
        for (items[i..], item_groups[i..]) |rhs, rhs_group| {
            const same_group = lhs_group == rhs_group;

            const td_eq = std.mem.eql(u8, store.typeDigest(&name_store, lhs).bytes[0..], store.typeDigest(&name_store, rhs).bytes[0..]);
            const tdc_eq = std.mem.eql(u8, store.typeDigestCached(&name_store, lhs, null).bytes[0..], store.typeDigestCached(&name_store, rhs, null).bytes[0..]);
            const sd_eq = std.mem.eql(u8, store.specializationDigest(&name_store, lhs).bytes[0..], store.specializationDigest(&name_store, rhs).bytes[0..]);
            const sdc_eq = std.mem.eql(u8, store.specializationDigestCached(&name_store, lhs, null).bytes[0..], store.specializationDigestCached(&name_store, rhs, null).bytes[0..]);

            // Cached and uncached paths induce the same equivalence relation.
            try std.testing.expectEqual(td_eq, tdc_eq);
            try std.testing.expectEqual(sd_eq, sdc_eq);

            const eql = try store.typeEql(&name_store, lhs, rhs);

            // Full type digests match the authoritative equality exactly for
            // this clean corpus: same group means equal digests, distinct group
            // means differing digests.
            try std.testing.expectEqual(same_group, td_eq);
            try std.testing.expectEqual(same_group, tdc_eq);
            try std.testing.expectEqual(same_group, eql);

            // Protocol soundness: a digest match is always confirmable by
            // `typeEql`.
            if (td_eq) try std.testing.expect(eql);
            if (tdc_eq) try std.testing.expect(eql);

            // Equal types agree on the specialization digest too (it is a
            // coarsening of the full identity).
            if (same_group) {
                try std.testing.expect(sd_eq);
                try std.testing.expect(sdc_eq);
            }
        }
    }

    try std.testing.expect(try store.typeEql(&name_store, alias_i64, i64_ty));
    try std.testing.expect(try store.typeEql(&name_store, alias_chain, i64_ty));

    // Alias transparency: every public digest path digests an alias exactly as
    // its backing.
    inline for (.{ alias_i64, alias_chain }) |alias_ty| {
        try std.testing.expect(std.mem.eql(u8, store.typeDigest(&name_store, alias_ty).bytes[0..], store.typeDigest(&name_store, i64_ty).bytes[0..]));
        try std.testing.expect(std.mem.eql(u8, store.specializationDigest(&name_store, alias_ty).bytes[0..], store.specializationDigest(&name_store, i64_ty).bytes[0..]));
        try std.testing.expect(std.mem.eql(u8, store.typeDigestCached(&name_store, alias_ty, null).bytes[0..], store.typeDigestCached(&name_store, i64_ty, null).bytes[0..]));
        try std.testing.expect(std.mem.eql(u8, store.specializationDigestCached(&name_store, alias_ty, null).bytes[0..], store.specializationDigestCached(&name_store, i64_ty, null).bytes[0..]));
    }
}

test "monotype type equality compares exact types across stores" {
    const allocator = std.testing.allocator;

    var name_store = names.NameStore.init(allocator);
    defer name_store.deinit();

    var current = Store.init(allocator);
    defer current.deinit();
    var loaded = Store.init(allocator);
    defer loaded.deinit();

    const field_name = try name_store.internRecordFieldLabel("value");
    const module_identity = try name_store.internModuleIdentity(&([_]u8{0xAB} ** 32));
    const type_name = try name_store.internTypeName("Alias");

    const current_unit = try current.add(.zst);
    const current_fields = try current.addFields(&.{.{ .name = field_name, .ty = current_unit }});
    const current_record = try current.add(.{ .record = current_fields });
    const current_args = try current.addSpan(&.{current_record});
    const current_fn = try current.add(.{ .func = .{
        .args = current_args,
        .ret = current_unit,
    } });
    const current_alias = try current.add(.{ .named = .{
        .named_type = .{ .module = .{}, .ty = @enumFromInt(1) },
        .def = .{ .module = module_identity, .type_name = type_name },
        .kind = .alias,
        .args = Span.empty(),
        .backing = .{ .ty = current_record, .use = .inspectable },
    } });

    _ = try loaded.add(.{ .primitive = .str });
    const loaded_unit = try loaded.add(.zst);
    const loaded_fields = try loaded.addFields(&.{.{ .name = field_name, .ty = loaded_unit }});
    const loaded_record = try loaded.add(.{ .record = loaded_fields });
    const loaded_args = try loaded.addSpan(&.{loaded_record});
    const loaded_fn = try loaded.add(.{ .func = .{
        .args = loaded_args,
        .ret = loaded_unit,
    } });

    const loaded_view = loaded.view();
    const loaded_digests = try allocator.alloc(names.TypeDigest, loaded_view.types.len);
    defer allocator.free(loaded_digests);
    for (loaded_digests, 0..) |*digest, index| {
        digest.* = loaded.typeDigest(&name_store, @enumFromInt(@as(u32, @intCast(index))));
    }
    const loaded_durable = DurableView{
        .types = loaded_view.types,
        .type_digests = loaded_digests,
        .spans = loaded_view.spans,
        .fields = loaded_view.fields,
        .tags = loaded_view.tags,
        .declared_fields = loaded_view.declared_fields,
    };

    try std.testing.expect(try typeEqlAcrossStores(allocator, &name_store, current.view(), current_fn, loaded_durable, loaded_fn));
    try std.testing.expect(try typeEqlAcrossStores(allocator, &name_store, current.view(), current_alias, loaded_durable, loaded_record));
    try std.testing.expect(!try typeEqlAcrossStores(allocator, &name_store, current.view(), current_fn, loaded_durable, loaded_record));
}

test "monotype type equality rejects digest-equal aliases without backing" {
    var name_store = names.NameStore.init(std.testing.allocator);
    defer name_store.deinit();

    var store = Store.init(std.testing.allocator);
    defer store.deinit();

    const module_identity = try name_store.internModuleIdentity(&([_]u8{0xAB} ** 32));
    const first_name = try name_store.internTypeName("First");
    const second_name = try name_store.internTypeName("Second");

    const first = try store.add(.{ .named = .{
        .named_type = .{ .module = .{}, .ty = @enumFromInt(1) },
        .def = .{ .module = module_identity, .type_name = first_name },
        .kind = .alias,
        .args = Span.empty(),
        .backing = null,
    } });
    const second = try store.add(.{ .named = .{
        .named_type = .{ .module = .{}, .ty = @enumFromInt(2) },
        .def = .{ .module = module_identity, .type_name = second_name },
        .kind = .alias,
        .args = Span.empty(),
        .backing = null,
    } });

    const first_digest = store.typeDigest(&name_store, first);
    const second_digest = store.typeDigest(&name_store, second);
    try std.testing.expect(std.mem.eql(u8, first_digest.bytes[0..], second_digest.bytes[0..]));
    try std.testing.expect(!try store.typeEql(&name_store, first, second));
}

test "monotype named type digest includes backing" {
    var name_store = names.NameStore.init(std.testing.allocator);
    defer name_store.deinit();

    var store = Store.init(std.testing.allocator);
    defer store.deinit();

    const module_identity = try name_store.internModuleIdentity(&([_]u8{0xAB} ** 32));
    const type_name = try name_store.internTypeName("Wrap");
    const checked_ty: checked.CheckedTypeId = @enumFromInt(1);
    const i64_ty = try store.add(.{ .primitive = .i64 });
    const str_ty = try store.add(.{ .primitive = .str });

    const named_i64 = try store.add(.{ .named = .{
        .named_type = .{ .module = .{}, .ty = checked_ty },
        .def = .{ .module = module_identity, .type_name = type_name },
        .kind = .nominal,
        .args = Span.empty(),
        .backing = .{ .ty = i64_ty, .use = .inspectable },
    } });
    const named_str = try store.add(.{ .named = .{
        .named_type = .{ .module = .{}, .ty = checked_ty },
        .def = .{ .module = module_identity, .type_name = type_name },
        .kind = .nominal,
        .args = Span.empty(),
        .backing = .{ .ty = str_ty, .use = .inspectable },
    } });

    const i64_digest = store.typeDigest(&name_store, named_i64);
    const str_digest = store.typeDigest(&name_store, named_str);
    try std.testing.expect(!std.mem.eql(u8, i64_digest.bytes[0..], str_digest.bytes[0..]));

    const i64_spec_digest = store.specializationDigest(&name_store, named_i64);
    const str_spec_digest = store.specializationDigest(&name_store, named_str);
    try std.testing.expect(std.mem.eql(u8, i64_spec_digest.bytes[0..], str_spec_digest.bytes[0..]));
}

test "monotype specialization digest includes builtin evidence backing" {
    var name_store = names.NameStore.init(std.testing.allocator);
    defer name_store.deinit();

    var store = Store.init(std.testing.allocator);
    defer store.deinit();

    const module_identity = try name_store.internModuleIdentity(&([_]u8{0xAB} ** 32));
    const type_name = try name_store.internTypeName("FieldNames");
    const checked_ty: checked.CheckedTypeId = @enumFromInt(1);
    const i64_ty = try store.add(.{ .primitive = .i64 });
    const str_ty = try store.add(.{ .primitive = .str });

    const fields_i64 = try store.add(.{ .named = .{
        .named_type = .{ .module = .{}, .ty = checked_ty },
        .def = .{ .module = module_identity, .type_name = type_name },
        .kind = .@"opaque",
        .builtin_owner = .fields,
        .args = Span.empty(),
        .backing = .{ .ty = i64_ty, .use = .runtime_layout_only },
    } });
    const fields_str = try store.add(.{ .named = .{
        .named_type = .{ .module = .{}, .ty = checked_ty },
        .def = .{ .module = module_identity, .type_name = type_name },
        .kind = .@"opaque",
        .builtin_owner = .fields,
        .args = Span.empty(),
        .backing = .{ .ty = str_ty, .use = .runtime_layout_only },
    } });

    const i64_spec_digest = store.specializationDigest(&name_store, fields_i64);
    const str_spec_digest = store.specializationDigest(&name_store, fields_str);
    try std.testing.expect(!std.mem.eql(u8, i64_spec_digest.bytes[0..], str_spec_digest.bytes[0..]));
}

test "monotype named type digest includes nested named backing" {
    var name_store = names.NameStore.init(std.testing.allocator);
    defer name_store.deinit();

    var store = Store.init(std.testing.allocator);
    defer store.deinit();

    const module_identity = try name_store.internModuleIdentity(&([_]u8{0xAB} ** 32));
    const outer_type_name = try name_store.internTypeName("Outer");
    const inner_type_name = try name_store.internTypeName("Inner");
    const outer_checked_ty: checked.CheckedTypeId = @enumFromInt(1);
    const inner_checked_ty: checked.CheckedTypeId = @enumFromInt(2);
    const i64_ty = try store.add(.{ .primitive = .i64 });
    const str_ty = try store.add(.{ .primitive = .str });

    const inner_i64 = try store.add(.{ .named = .{
        .named_type = .{ .module = .{}, .ty = inner_checked_ty },
        .def = .{ .module = module_identity, .type_name = inner_type_name },
        .kind = .nominal,
        .args = Span.empty(),
        .backing = .{ .ty = i64_ty, .use = .inspectable },
    } });
    const inner_str = try store.add(.{ .named = .{
        .named_type = .{ .module = .{}, .ty = inner_checked_ty },
        .def = .{ .module = module_identity, .type_name = inner_type_name },
        .kind = .nominal,
        .args = Span.empty(),
        .backing = .{ .ty = str_ty, .use = .inspectable },
    } });
    const outer_i64 = try store.add(.{ .named = .{
        .named_type = .{ .module = .{}, .ty = outer_checked_ty },
        .def = .{ .module = module_identity, .type_name = outer_type_name },
        .kind = .nominal,
        .args = Span.empty(),
        .backing = .{ .ty = inner_i64, .use = .inspectable },
    } });
    const outer_str = try store.add(.{ .named = .{
        .named_type = .{ .module = .{}, .ty = outer_checked_ty },
        .def = .{ .module = module_identity, .type_name = outer_type_name },
        .kind = .nominal,
        .args = Span.empty(),
        .backing = .{ .ty = inner_str, .use = .inspectable },
    } });

    const i64_digest = store.typeDigest(&name_store, outer_i64);
    const str_digest = store.typeDigest(&name_store, outer_str);
    try std.testing.expect(!std.mem.eql(u8, i64_digest.bytes[0..], str_digest.bytes[0..]));
}

test "monotype named type digest includes declared field order" {
    var name_store = names.NameStore.init(std.testing.allocator);
    defer name_store.deinit();

    var store = Store.init(std.testing.allocator);
    defer store.deinit();

    const module_identity = try name_store.internModuleIdentity(&([_]u8{0xAB} ** 32));
    const type_name = try name_store.internTypeName("Pair");
    const field_a = try name_store.internRecordFieldLabel("a");
    const field_b = try name_store.internRecordFieldLabel("b");
    const checked_ty: checked.CheckedTypeId = @enumFromInt(1);
    const i64_ty = try store.add(.{ .primitive = .i64 });
    const fields = try store.addFields(&.{
        .{ .name = field_a, .ty = i64_ty },
        .{ .name = field_b, .ty = i64_ty },
    });
    const backing = try store.add(.{ .record = fields });
    const order_ab = try store.addDeclaredFields(&.{
        .{ .named = field_a },
        .{ .named = field_b },
    });
    const order_ba = try store.addDeclaredFields(&.{
        .{ .named = field_b },
        .{ .named = field_a },
    });

    const named_ab = try store.add(.{ .named = .{
        .named_type = .{ .module = .{}, .ty = checked_ty },
        .def = .{ .module = module_identity, .type_name = type_name },
        .kind = .nominal,
        .args = Span.empty(),
        .backing = .{ .ty = backing, .use = .inspectable },
        .declared_order = order_ab,
    } });
    const named_ba = try store.add(.{ .named = .{
        .named_type = .{ .module = .{}, .ty = checked_ty },
        .def = .{ .module = module_identity, .type_name = type_name },
        .kind = .nominal,
        .args = Span.empty(),
        .backing = .{ .ty = backing, .use = .inspectable },
        .declared_order = order_ba,
    } });

    const ab_digest = store.typeDigest(&name_store, named_ab);
    const ba_digest = store.typeDigest(&name_store, named_ba);
    try std.testing.expect(!std.mem.eql(u8, ab_digest.bytes[0..], ba_digest.bytes[0..]));
}

test "monotype named type digest includes padding backing" {
    var name_store = names.NameStore.init(std.testing.allocator);
    defer name_store.deinit();

    var store = Store.init(std.testing.allocator);
    defer store.deinit();

    const module_identity = try name_store.internModuleIdentity(&([_]u8{0xAB} ** 32));
    const type_name = try name_store.internTypeName("Padded");
    const checked_ty: checked.CheckedTypeId = @enumFromInt(1);
    const i64_ty = try store.add(.{ .primitive = .i64 });
    const str_ty = try store.add(.{ .primitive = .str });
    const order_i64 = try store.addDeclaredFields(&.{.{ .padding = i64_ty }});
    const order_str = try store.addDeclaredFields(&.{.{ .padding = str_ty }});

    const named_i64 = try store.add(.{ .named = .{
        .named_type = .{ .module = .{}, .ty = checked_ty },
        .def = .{ .module = module_identity, .type_name = type_name },
        .kind = .nominal,
        .args = Span.empty(),
        .declared_order = order_i64,
    } });
    const named_str = try store.add(.{ .named = .{
        .named_type = .{ .module = .{}, .ty = checked_ty },
        .def = .{ .module = module_identity, .type_name = type_name },
        .kind = .nominal,
        .args = Span.empty(),
        .declared_order = order_str,
    } });

    const i64_digest = store.typeDigest(&name_store, named_i64);
    const str_digest = store.typeDigest(&name_store, named_str);
    try std.testing.expect(!std.mem.eql(u8, i64_digest.bytes[0..], str_digest.bytes[0..]));
}

test "reintern rebuilds acyclic nested types and dedups equal leaves" {
    var name_store = names.NameStore.init(std.testing.allocator);
    defer name_store.deinit();
    const a_name = try name_store.internRecordFieldLabel("a");
    const b_name = try name_store.internRecordFieldLabel("b");

    // Source: List { a: I64, b: I64 } with the two I64 leaves built as
    // distinct nodes so dedup has something to collapse.
    var source = Store.init(std.testing.allocator);
    defer source.deinit();
    const i64_a = try source.add(.{ .primitive = .i64 });
    const i64_b = try source.add(.{ .primitive = .i64 });
    const record = try source.add(.{ .record = try source.addRecordFields(&name_store, &.{
        .{ .name = a_name, .ty = i64_a },
        .{ .name = b_name, .ty = i64_b },
    }) });
    const list = try source.add(.{ .list = record });

    var dest = Store.init(std.testing.allocator);
    dest.enableInterning();
    defer dest.deinit();
    const root = try reintern(&dest, &name_store, source.view(), list);

    try std.testing.expect(dest.get(root) == .list);
    const elem = dest.get(root).list;
    try std.testing.expect(dest.get(elem) == .record);
    const fields = dest.fieldSpan(dest.get(elem).record);
    try std.testing.expectEqual(@as(usize, 2), fields.len);
    // Both fields resolved to the one deduplicated I64 leaf.
    try std.testing.expectEqual(GuardedList.at(fields, 0).ty, GuardedList.at(fields, 1).ty);
    try std.testing.expect(dest.get(GuardedList.at(fields, 0).ty) == .primitive);
    // Exactly three types: the leaf, the record, and the list.
    try std.testing.expectEqual(@as(usize, 3), dest.view().types.len);
    try std.testing.expectEqual(@as(?Store.VerifyError, null), dest.verify(&name_store));
}

test "reintern rebuilds a self-recursive type" {
    var name_store = names.NameStore.init(std.testing.allocator);
    defer name_store.deinit();
    const next_name = try name_store.internRecordFieldLabel("next");

    // Source: a self-referential record { next: self } built with addRecursive.
    var source = Store.init(std.testing.allocator);
    defer source.deinit();
    const Ctx = struct {
        source: *Store,
        name_store: *const names.NameStore,
        field: names.RecordFieldNameId,
        fn fill(ctx: @This(), reserved: TypeId) std.mem.Allocator.Error!Content {
            return .{ .record = try ctx.source.addRecordFields(ctx.name_store, &.{
                .{ .name = ctx.field, .ty = reserved },
            }) };
        }
    };
    const source_root = try source.addRecursive(
        Ctx{ .source = &source, .name_store = &name_store, .field = next_name },
        Ctx.fill,
    );

    var dest = Store.init(std.testing.allocator);
    dest.enableInterning();
    defer dest.deinit();
    const root = try reintern(&dest, &name_store, source.view(), source_root);

    try std.testing.expect(dest.get(root) == .record);
    const fields = dest.fieldSpan(dest.get(root).record);
    try std.testing.expectEqual(@as(usize, 1), fields.len);
    try std.testing.expectEqual(root, GuardedList.at(fields, 0).ty);
    try std.testing.expectEqual(@as(usize, 1), dest.view().types.len);
    try std.testing.expectEqual(@as(?Store.VerifyError, null), dest.verify(&name_store));
}

test "reintern rebuilds a mutually recursive group" {
    var name_store = names.NameStore.init(std.testing.allocator);
    defer name_store.deinit();
    const a_name = try name_store.internRecordFieldLabel("a");
    const b_name = try name_store.internRecordFieldLabel("b");

    // Source: A = { a: B }, B = { b: A } — distinct rooted types. Built through
    // the plain (non-interning) group builder so the cycle exists in `source`.
    var source = Store.init(std.testing.allocator);
    defer source.deinit();
    const node0 = Store.recursiveNodeId(0);
    const node1 = Store.recursiveNodeId(1);
    const source_a = try source.internRecursiveGroupRoot(&name_store, &.{
        .{ .record = &.{.{ .name = a_name, .ty = .{ .node = node1 } }} },
        .{ .record = &.{.{ .name = b_name, .ty = .{ .node = node0 } }} },
    }, node0);

    var dest = Store.init(std.testing.allocator);
    dest.enableInterning();
    defer dest.deinit();
    const root = try reintern(&dest, &name_store, source.view(), source_a);

    // A -> B -> A, with A != B and both distinct records preserved.
    try std.testing.expect(dest.get(root) == .record);
    const a_fields = dest.fieldSpan(dest.get(root).record);
    const b = GuardedList.at(a_fields, 0).ty;
    try std.testing.expect(b != root);
    try std.testing.expect(dest.get(b) == .record);
    const b_fields = dest.fieldSpan(dest.get(b).record);
    try std.testing.expectEqual(root, GuardedList.at(b_fields, 0).ty);
    try std.testing.expectEqual(@as(usize, 2), dest.view().types.len);
    try std.testing.expectEqual(@as(?Store.VerifyError, null), dest.verify(&name_store));
}

test "reintern collapses a symmetric recursive group to one id" {
    var name_store = names.NameStore.init(std.testing.allocator);
    defer name_store.deinit();
    const field_name = try name_store.internRecordFieldLabel("x");

    // Source: A = { x: B }, B = { x: A } — symmetric, so one rooted type.
    var source = Store.init(std.testing.allocator);
    defer source.deinit();
    const node0 = Store.recursiveNodeId(0);
    const node1 = Store.recursiveNodeId(1);
    const source_a = try source.internRecursiveGroupRoot(&name_store, &.{
        .{ .record = &.{.{ .name = field_name, .ty = .{ .node = node1 } }} },
        .{ .record = &.{.{ .name = field_name, .ty = .{ .node = node0 } }} },
    }, node0);

    var dest = Store.init(std.testing.allocator);
    dest.enableInterning();
    defer dest.deinit();
    const root = try reintern(&dest, &name_store, source.view(), source_a);

    // The symmetric members share one self-referential record id.
    try std.testing.expectEqual(@as(usize, 1), dest.view().types.len);
    const fields = dest.fieldSpan(dest.get(root).record);
    try std.testing.expectEqual(root, GuardedList.at(fields, 0).ty);
    try std.testing.expectEqual(@as(?Store.VerifyError, null), dest.verify(&name_store));
}

test "reintern canonicalizes empty row and head forms" {
    var name_store = names.NameStore.init(std.testing.allocator);
    defer name_store.deinit();

    // Source: an empty record built as a general zero-length record row, and an
    // empty tag union. §8.4 requires one interned id per head.
    var source = Store.init(std.testing.allocator);
    defer source.deinit();
    const empty_record = try source.add(.{ .record = try source.addRecordFields(&name_store, &.{}) });
    const empty_tags = try source.add(.{ .tag_union = Span.empty() });

    var dest = Store.init(std.testing.allocator);
    dest.enableInterning();
    defer dest.deinit();

    const reint_record = try reintern(&dest, &name_store, source.view(), empty_record);
    const reint_tags = try reintern(&dest, &name_store, source.view(), empty_tags);
    // A freshly interned empty record shares the reinterned one's id.
    const direct_record = try dest.internRecord(&name_store, &.{});
    const direct_tags = try dest.internTagUnion(&name_store, &.{});
    try std.testing.expectEqual(reint_record, direct_record);
    try std.testing.expectEqual(reint_tags, direct_tags);
    try std.testing.expect(reint_record != reint_tags);
    try std.testing.expectEqual(@as(usize, 2), dest.view().types.len);
}

test "reintern is construction-order independent and dedups across roots" {
    var name_store = names.NameStore.init(std.testing.allocator);
    defer name_store.deinit();
    const field_name = try name_store.internRecordFieldLabel("v");

    // Two sources build the same logical type { v: Box I64 } in different node
    // orders (leaf-first vs parent-first allocation), so their internal ids
    // differ.
    var source_a = Store.init(std.testing.allocator);
    defer source_a.deinit();
    const a_i64 = try source_a.add(.{ .primitive = .i64 });
    const a_box = try source_a.add(.{ .box = a_i64 });
    const a_record = try source_a.add(.{ .record = try source_a.addRecordFields(&name_store, &.{
        .{ .name = field_name, .ty = a_box },
    }) });

    var source_b = Store.init(std.testing.allocator);
    defer source_b.deinit();
    // Reserve the record slot first, then fill its children, inverting order.
    const RecordCtx = struct {
        source: *Store,
        name_store: *const names.NameStore,
        field: names.RecordFieldNameId,
        fn fill(ctx: @This(), _: TypeId) std.mem.Allocator.Error!Content {
            const i64_ty = try ctx.source.add(.{ .primitive = .i64 });
            const box_ty = try ctx.source.add(.{ .box = i64_ty });
            return .{ .record = try ctx.source.addRecordFields(ctx.name_store, &.{
                .{ .name = ctx.field, .ty = box_ty },
            }) };
        }
    };
    const b_record = try source_b.addRecursive(
        RecordCtx{ .source = &source_b, .name_store = &name_store, .field = field_name },
        RecordCtx.fill,
    );

    var dest = Store.init(std.testing.allocator);
    dest.enableInterning();
    defer dest.deinit();
    const from_a = try reintern(&dest, &name_store, source_a.view(), a_record);
    const type_count_after_a = dest.view().types.len;
    const from_b = try reintern(&dest, &name_store, source_b.view(), b_record);

    // Both entry orders reintern to the same id, and the second reintern adds
    // no new types.
    try std.testing.expectEqual(from_a, from_b);
    try std.testing.expectEqual(type_count_after_a, dest.view().types.len);
    try std.testing.expectEqual(@as(?Store.VerifyError, null), dest.verify(&name_store));
}
