//! Monomorphic type store used by Monotype and Monotype Lifted IR.
//!
//! This store contains closed checked types after static dispatch and numeric
//! defaulting have been finalized. It has no lambda sets and no layout data.

const std = @import("std");
const check = @import("check");
const collections = @import("collections");

const Common = @import("../common.zig");
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

/// Static-dispatch owner head for a monomorphic receiver type.
pub const OwnerHead = union(enum(u8)) {
    none,
    builtin: static_dispatch.BuiltinOwner,
    named_type: TypeDef,
};

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
    /// Checker-authored identities for the iterator representation.
    /// Generated iterator types retain these as the exact representation roles
    /// consumed by post-check stages.
    iterator_topology: ?IteratorTopology = null,
};

/// Explicit representation tier assigned when an iterator nominal is created.
pub const IteratorRepresentation = check.ConstStore.IteratorRepresentation;

/// Producer-owned identity shared across checked and Monotype storage.
pub const IteratorKind = static_dispatch.IteratorKind;

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

/// Authority carried by a named backing. Checked-public backings describe the
/// interface produced by checking. Generated-private backings carry explicit
/// post-check specialization evidence and must never be merged into that
/// public interface.
pub const BackingAuthority = enum(u8) {
    checked_public,
    generated_private,
};

/// Backing type for a named type when checking output one.
pub const NamedBacking = struct {
    ty: TypeId,
    use: BackingUse,
    authority: BackingAuthority = .checked_public,
};

/// Kind of named type visible after checking.
pub const NamedKind = enum(u8) {
    nominal,
    @"opaque",
    alias,
};

/// Identity of a `??` field's default value, carried on the Monotype record
/// field itself so rows that disagree about defaults are distinct monotypes:
/// distinct digests, hence distinct specializations and derived-codec defs.
/// This keeps "same monotype ⇒ same behavior" an invariant instead of an
/// approximation—a required row and a defaulted row of the same shape must
/// not share a derived parser. The slot encoding stays kind-free (`defaulted`
/// fields remain plain inline slots) and layout never reads this. `module` is
/// the declaring module's interned identity in the program name store;
/// `expr_node` is the default expression's CIR node index in that module—
/// the same identity the checked side carries in `CheckedFieldDefault`.
pub const FieldDefault = struct {
    module: names.ModuleIdentityId,
    expr_node: u32,
};

/// Whether a Monotype-shaped record field already has a committed runtime
/// slot. Durable Monotypes are always `resolved`. Interface-replay memoization
/// also uses immutable provisional Type views; an `undetermined` field in one
/// of those views records that its checked presence variable still owns the
/// slot decision. In that state `ty` mirrors `value_ty` for structural identity
/// only and must not reach layout or completed Monotype output.
pub const FieldKindState = enum(u8) {
    resolved,
    undetermined,
};

/// Record field type entry.
pub const MonoTypeField = struct {
    name: names.RecordFieldNameId,
    ty: TypeId,
    /// Source value type for an optional slot. This is explicit post-check
    /// evidence retained alongside the runtime slot so a finished Monotype
    /// can participate in later specialization without reconstructing field
    /// presence from the slot's shape.
    value_ty: ?TypeId = null,
    /// Explicitly distinguishes a finished slot from a provisional field whose
    /// checked presence variable has not selected inline or optional encoding.
    kind_state: FieldKindState = .resolved,
    /// Present exactly for `??`-defaulted fields; see `FieldDefault`.
    default: ?FieldDefault,
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

/// One entry of a nominal record's declared fields. The backing row is always
/// lexicographic; this separate source order supports boxy descriptor planning,
/// and layout selection consumes it only when padding opts into declared order.
/// See design.md "Nominal Record Field Order".
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
        /// Declared fields for a nominal/opaque record backing; empty for other
        /// named types.
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

/// Store for monomorphic types and their shared spans.
pub const Store = struct {
    allocator: std.mem.Allocator,
    types: StoreList(Content, "types"),
    type_digests: StoreList(?names.TypeDigest, "type_digests"),
    specialization_digests: StoreList(?names.TypeDigest, "specialization_digests"),
    equality_digests: StoreList(?names.TypeDigest, "equality_digests"),
    /// Newly reserved recursive slots may be referenced while their content is
    /// being built, but they are not observable types until filled. Filled
    /// nodes are immutable, which makes their cached digests permanently valid.
    constructing: StoreList(bool, "constructing"),
    unfinished_type_count: usize,
    /// The sole owner allowed to truncate and rewrite the current speculative
    /// suffix. Epochs make a completed transaction's idempotent abort harmless
    /// without allowing a stale handle to abort a later owner.
    active_transaction: ?u64,
    transaction_epoch: u64,
    /// Cached immutable answer for whether a finished type contains an Iter or
    /// Stream interface at any structural depth.
    iterator_interface_cache: StoreList(?bool, "iterator_interface_cache"),
    /// Reusable exact walk state. Type ids are dense, so epochs provide cycle
    /// detection without allocating a dense map sized to the largest type id
    /// on every closed direct call.
    iterator_interface_pending: std.ArrayList(TypeId),
    iterator_interface_visited: std.ArrayList(TypeId),
    iterator_interface_visit_epochs: StoreList(u32, "iterator_interface_visit_epochs"),
    iterator_interface_visit_epoch: u32,
    /// One-step unfoldings of every digested recursive-group position: the
    /// hash of a member's node encoding with all children rendered as their
    /// finalized digests, mapped to that member's group digest. An acyclic
    /// node whose rendering matches an entry is a rolled-out prefix of the
    /// same infinite type and adopts the member's digest, which keeps digests
    /// independent of how deeply a recursive knot is tied—including across
    /// separate digest calls. Keys and values are content-addressed, so
    /// entries stay valid across `restore` truncations and need no rollback.
    recursive_digest_unfoldings: std.AutoHashMap([32]u8, names.TypeDigest),
    /// Full-content-identity buckets for store-level acyclic interning: full
    /// digest bytes to every committed candidate that hashed to them,
    /// resolved to exact equality with `typeEql`. Keys and stored ids are
    /// content-addressed, so entries stay valid across `restore`
    /// truncations and need no rollback. A transaction seal reserves the
    /// entries it will index before it mutates anything, so a bucket
    /// may exist while empty, which reads identically to an absent one.
    full_digest_interned: std.AutoHashMap(DigestBucketKey, std.ArrayList(TypeId)),
    spans: StoreList(TypeId, "spans"),
    fields: StoreList(Field, "fields"),
    tags: StoreList(Tag, "tags"),
    declared_fields: StoreList(DeclaredField, "declared_fields"),
    frozen: bool,

    pub fn init(allocator: std.mem.Allocator) Store {
        return .{
            .allocator = allocator,
            .types = .empty,
            .type_digests = .empty,
            .specialization_digests = .empty,
            .equality_digests = .empty,
            .constructing = .empty,
            .unfinished_type_count = 0,
            .active_transaction = null,
            .transaction_epoch = 0,
            .iterator_interface_cache = .empty,
            .iterator_interface_pending = .empty,
            .iterator_interface_visited = .empty,
            .iterator_interface_visit_epochs = .empty,
            .iterator_interface_visit_epoch = 0,
            .recursive_digest_unfoldings = std.AutoHashMap([32]u8, names.TypeDigest).init(allocator),
            .full_digest_interned = std.AutoHashMap(DigestBucketKey, std.ArrayList(TypeId)).init(allocator),
            .spans = .empty,
            .fields = .empty,
            .tags = .empty,
            .declared_fields = .empty,
            .frozen = false,
        };
    }

    pub fn deinit(self: *Store) void {
        self.declared_fields.deinit(self.allocator);
        self.tags.deinit(self.allocator);
        self.fields.deinit(self.allocator);
        self.spans.deinit(self.allocator);
        var full_digest_buckets = self.full_digest_interned.valueIterator();
        while (full_digest_buckets.next()) |bucket| bucket.deinit(self.allocator);
        self.full_digest_interned.deinit();
        self.recursive_digest_unfoldings.deinit();
        self.iterator_interface_visit_epochs.deinit(self.allocator);
        self.iterator_interface_visited.deinit(self.allocator);
        self.iterator_interface_pending.deinit(self.allocator);
        self.iterator_interface_cache.deinit(self.allocator);
        self.constructing.deinit(self.allocator);
        self.equality_digests.deinit(self.allocator);
        self.specialization_digests.deinit(self.allocator);
        self.type_digests.deinit(self.allocator);
        self.types.deinit(self.allocator);
    }

    pub fn freeze(self: *Store) void {
        if (self.hasSpeculativeConstruction()) {
            Common.compilerBug("cannot freeze Monotype types with an unfinished construction owner");
        }
        self.frozen = true;
    }

    /// Whether new nodes belong to an owner that has not sealed its suffix yet.
    /// Nested producers append unindexed nodes for that owner to rewrite.
    pub fn hasSpeculativeConstruction(self: *const Store) bool {
        return self.active_transaction != null or self.unfinished_type_count != 0;
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
        self.assertMutable();
        const index = self.types.len();
        try self.types.append(self.allocator, content);
        errdefer _ = self.types.pop();
        try self.type_digests.append(self.allocator, null);
        errdefer _ = self.type_digests.pop();
        try self.specialization_digests.append(self.allocator, null);
        errdefer _ = self.specialization_digests.pop();
        try self.equality_digests.append(self.allocator, null);
        errdefer _ = self.equality_digests.pop();
        try self.constructing.append(self.allocator, false);
        errdefer _ = self.constructing.pop();
        try self.iterator_interface_cache.append(self.allocator, null);
        errdefer _ = self.iterator_interface_cache.pop();
        try self.iterator_interface_visit_epochs.append(self.allocator, 0);
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
        const reserved = try self.add(.zst);
        self.constructing.set(@intFromEnum(reserved), true);
        self.unfinished_type_count += 1;
        return reserved;
    }

    fn fillReservedSlot(self: *Store, ty: TypeId, content: Content) void {
        self.assertMutable();
        const index = @intFromEnum(ty);
        if (!self.constructing.unsafeRawItemsForView()[index]) {
            Common.invariant("filled a Monotype type slot that was not under construction");
        }
        self.types.set(index, content);
        self.constructing.set(index, false);
        if (self.unfinished_type_count == 0) {
            Common.compilerBug("filled a Monotype slot without an unfinished construction");
        }
        self.unfinished_type_count -= 1;
        self.iterator_interface_cache.set(index, null);
    }

    pub fn get(self: *const Store, ty: TypeId) Content {
        return self.types.unsafeRawItemsForView()[@intFromEnum(ty)];
    }

    /// Whether an immutable Monotype contains the public iterator interface at
    /// any structural depth. Closed-call lowering uses this directly so an
    /// ordinary return type never has to be imported into a live graph merely
    /// to answer an ownership question.
    ///
    /// `InstGraph.containsIteratorInterface` answers the same question for the
    /// live graph. The two walk different representations, so they cannot
    /// share code, but they must agree for every pair of corresponding types:
    /// this walk gates skipping graph construction entirely, so a structural
    /// position it declines to descend into would drop a producer's minted
    /// representation while the graph walk still claims to protect it. The
    /// test "iterator-interface containment agrees between Monotype and graph"
    /// in `solve.zig` pins the two together position by position.
    pub fn containsIteratorInterface(self: *Store, root: TypeId) std.mem.Allocator.Error!bool {
        self.requireConstructed(root);
        const root_index = @intFromEnum(root);
        if (self.iterator_interface_cache.unsafeRawItemsForView()[root_index]) |cached| return cached;

        self.iterator_interface_pending.clearRetainingCapacity();
        self.iterator_interface_visited.clearRetainingCapacity();
        defer self.iterator_interface_pending.clearRetainingCapacity();
        defer self.iterator_interface_visited.clearRetainingCapacity();
        if (self.iterator_interface_visit_epoch == std.math.maxInt(u32)) {
            @memset(self.iterator_interface_visit_epochs.unsafeRawItemsMutForStore(), 0);
            self.iterator_interface_visit_epoch = 1;
        } else {
            self.iterator_interface_visit_epoch += 1;
        }
        const visit_epoch = self.iterator_interface_visit_epoch;
        try self.iterator_interface_pending.append(self.allocator, root);
        while (self.iterator_interface_pending.pop()) |ty| {
            const ty_index = @intFromEnum(ty);
            if (self.iterator_interface_visit_epochs.unsafeRawItemsForView()[ty_index] == visit_epoch) continue;
            self.iterator_interface_visit_epochs.set(ty_index, visit_epoch);
            try self.iterator_interface_visited.append(self.allocator, ty);
            self.requireConstructed(ty);
            if (ty != root) {
                if (self.iterator_interface_cache.unsafeRawItemsForView()[ty_index]) |cached| {
                    if (cached) {
                        self.iterator_interface_cache.set(root_index, true);
                        return true;
                    }
                    continue;
                }
            }
            switch (self.get(ty)) {
                .primitive, .erased, .zst => {},
                .list, .box => |child| try self.iterator_interface_pending.append(self.allocator, child),
                .tuple => |items| {
                    const item_types = self.span(items);
                    for (0..GuardedList.borrowLen(item_types)) |index| {
                        try self.iterator_interface_pending.append(self.allocator, GuardedList.at(item_types, index));
                    }
                },
                .func => |function| {
                    const arg_types = self.span(function.args);
                    for (0..GuardedList.borrowLen(arg_types)) |index| {
                        try self.iterator_interface_pending.append(self.allocator, GuardedList.at(arg_types, index));
                    }
                    try self.iterator_interface_pending.append(self.allocator, function.ret);
                },
                .tag_union => |tags| {
                    const variants = self.tagSpan(tags);
                    for (0..GuardedList.borrowLen(variants)) |variant_index| {
                        const tag = GuardedList.at(variants, variant_index);
                        const payloads = self.span(tag.payloads);
                        for (0..GuardedList.borrowLen(payloads)) |payload_index| {
                            try self.iterator_interface_pending.append(self.allocator, GuardedList.at(payloads, payload_index));
                        }
                    }
                },
                .record => |fields| {
                    const record_fields = self.fieldSpan(fields);
                    for (0..GuardedList.borrowLen(record_fields)) |index| {
                        const field = GuardedList.at(record_fields, index);
                        try self.iterator_interface_pending.append(self.allocator, field.ty);
                        if (field.value_ty) |value_ty| {
                            try self.iterator_interface_pending.append(self.allocator, value_ty);
                        }
                    }
                },
                .named => |named| {
                    if (named.builtin_owner) |owner| {
                        if (static_dispatch.isIteratorOwner(owner)) {
                            self.iterator_interface_cache.set(ty_index, true);
                            self.iterator_interface_cache.set(root_index, true);
                            return true;
                        }
                    }
                    const args = self.span(named.args);
                    for (0..GuardedList.borrowLen(args)) |index| {
                        try self.iterator_interface_pending.append(self.allocator, GuardedList.at(args, index));
                    }
                    if (named.backing) |backing| try self.iterator_interface_pending.append(self.allocator, backing.ty);
                    const declared_fields = self.declaredFieldSpan(named.declared_order);
                    for (0..GuardedList.borrowLen(declared_fields)) |index| {
                        switch (GuardedList.at(declared_fields, index)) {
                            .named => {},
                            .padding => |padding| try self.iterator_interface_pending.append(self.allocator, padding),
                        }
                    }
                },
            }
        }
        for (self.iterator_interface_visited.items) |visited| {
            self.iterator_interface_cache.set(@intFromEnum(visited), false);
        }
        return false;
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

    const Mark = struct {
        types_len: usize,
        type_digests_len: usize,
        specialization_digests_len: usize,
        equality_digests_len: usize,
        constructing_len: usize,
        unfinished_type_count: usize,
        iterator_interface_cache_len: usize,
        iterator_interface_visit_epochs_len: usize,
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
            .equality_digests_len = self.equality_digests.len(),
            .constructing_len = self.constructing.len(),
            .unfinished_type_count = self.unfinished_type_count,
            .iterator_interface_cache_len = self.iterator_interface_cache.len(),
            .iterator_interface_visit_epochs_len = self.iterator_interface_visit_epochs.len(),
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
        self.equality_digests.restoreLen(mark_.equality_digests_len);
        self.constructing.restoreLen(mark_.constructing_len);
        self.unfinished_type_count = mark_.unfinished_type_count;
        self.iterator_interface_cache.restoreLen(mark_.iterator_interface_cache_len);
        // A reserved slot that survives this restore may have been filled
        // after the mark with children that are now truncated and whose ids
        // can be reused, so clear every retained containment answer to force
        // those new children to be walked. Only a mark taken during recursive
        // construction can see that: when the mark recorded no unfinished
        // slot, every surviving node was filled before the mark and reaches
        // only surviving ids, so retained answers stay valid. Skipping the
        // wipe then keeps interning hits and transaction seals -- the hot
        // restore callers -- from discarding the whole cache.
        // Digest caches need no equivalent clearing: a truncated id's cache
        // row goes with it, sealing only ever digests ids inside the suffix,
        // and the unfolding index is content-addressed. Only digesting a
        // *surviving* slot's mid-transaction fill before the rollback decision
        // would defeat that, and no caller does.
        if (mark_.unfinished_type_count != 0) {
            @memset(self.iterator_interface_cache.unsafeRawItemsMutForStore(), null);
        }
        self.iterator_interface_visit_epochs.restoreLen(mark_.iterator_interface_visit_epochs_len);
        self.spans.restoreLen(mark_.spans_len);
        self.fields.restoreLen(mark_.fields_len);
        self.tags.restoreLen(mark_.tags_len);
        self.declared_fields.restoreLen(mark_.declared_fields_len);
    }

    /// Speculative recursive construction boundary. Callers may reserve and
    /// fill mutually recursive slots and append any side-pool data before
    /// sealing the transaction.
    ///
    /// Ownership: the store owns the speculative suffix, the transaction owns
    /// only the boundary it was opened at. Exactly one of `commitTransaction`
    /// or `abort` ends a transaction, and both leave the store in a state a
    /// later transaction may be opened on. Because commit is all-or-nothing
    /// (it rolls the suffix back itself on failure), the intended construction
    /// shape is `errdefer transaction.abort(store)` while building, disarmed
    /// once commit returns successfully. A completed owner's abort is harmless;
    /// an older handle cannot abort a later owner because epochs distinguish
    /// them.
    pub const Transaction = struct {
        mark_: Mark,
        epoch: u64,

        fn requireOwner(self: Transaction, store: *const Store) void {
            if (store.active_transaction != self.epoch) {
                Common.compilerBug("Monotype transaction operation did not own the speculative suffix");
            }
        }

        pub fn reserve(self: Transaction, store: *Store) std.mem.Allocator.Error!TypeId {
            self.requireOwner(store);
            return try store.reserveSlot();
        }

        pub fn fill(self: Transaction, store: *Store, ty: TypeId, content: Content) void {
            self.requireOwner(store);
            const index = @intFromEnum(ty);
            if (index < self.mark_.types_len or index >= store.types.len()) {
                Common.compilerBug("recursive transaction filled a type outside its suffix");
            }
            store.fillReservedSlot(ty, content);
        }

        /// Discard the speculative suffix, leaving the store as
        /// `beginTransaction` found it. Idempotent, so it stays correct after
        /// a failed `commitTransaction` (which has already rolled back).
        pub fn abort(self: Transaction, store: *Store) void {
            const active_epoch = store.active_transaction orelse return;
            if (active_epoch != self.epoch) {
                Common.compilerBug("stale Monotype transaction tried to abort a later owner");
            }
            store.restore(self.mark_);
            store.active_transaction = null;
        }
    };

    /// Interned ids produced by sealing a recursive transaction.
    pub const TransactionResult = struct {
        allocator: std.mem.Allocator,
        root: TypeId,
        /// First id the sealed transaction owned. Ids below it predate the
        /// transaction and are already interned.
        suffix_start: u32,
        /// One interned id for each original suffix id, in original order.
        remap: []TypeId,

        /// Interned id for any id a caller held while building the sealed
        /// transaction. Pre-transaction ids pass through unchanged and
        /// speculative ids resolve through the remap, so callers translate
        /// held ids without ever handling the store's private mark.
        pub fn remapType(self: TransactionResult, ty: TypeId) TypeId {
            const index = @intFromEnum(ty);
            if (index < self.suffix_start) return ty;
            const offset = index - self.suffix_start;
            if (offset >= self.remap.len) {
                Common.compilerBug("recursive transaction remap requested for an id outside its suffix");
            }
            return self.remap[offset];
        }

        pub fn deinit(self: *TransactionResult) void {
            self.allocator.free(self.remap);
            self.* = undefined;
        }
    };

    /// Begin the sole recursive construction owner for the current store
    /// suffix. Nested producers append into this owner instead of sealing an
    /// unfinished prefix independently.
    pub fn beginTransaction(self: *Store) Transaction {
        self.assertMutable();
        if (self.hasSpeculativeConstruction()) {
            Common.compilerBug("nested Monotype type transaction must participate in the unfinished owner");
        }
        self.transaction_epoch +%= 1;
        self.active_transaction = self.transaction_epoch;
        return .{ .mark_ = self.mark(), .epoch = self.transaction_epoch };
    }

    /// Seal an arbitrarily recursive speculative suffix into durable interned
    /// nodes. Equality, rather than digest coincidence, remains authoritative.
    ///
    /// Atomic in both directions. On success every surviving representative is
    /// committed *and* indexed; on any allocator failure the store is left as
    /// `beginTransaction` found it, with the speculative suffix discarded and
    /// no id indexed in a digest bucket. That holds because the indexing step
    /// consumes only capacity reserved before the first destructive step, so
    /// no branch can return an error from a half-indexed state -- the state
    /// that would strand ids in a bucket after the rollback truncated them.
    pub fn commitTransaction(
        self: *Store,
        name_store: *const names.NameStore,
        transaction: Transaction,
        root: TypeId,
    ) std.mem.Allocator.Error!TransactionResult {
        if (self.active_transaction != transaction.epoch) {
            Common.compilerBug("Monotype transaction commit did not own the speculative suffix");
        }
        const mark_ = transaction.mark_;
        const suffix_len = self.types.len() - mark_.types_len;
        const root_index = @intFromEnum(root);
        if (root_index < mark_.types_len or root_index >= self.types.len()) {
            Common.compilerBug("recursive transaction root is outside its suffix");
        }
        for (self.constructing.unsafeRawItemsForView()[mark_.constructing_len..]) |unfinished| {
            if (unfinished) Common.compilerBug("recursive transaction contains an unfinished slot");
        }

        // Every failure below abandons the whole transaction, so a caller that
        // gets an error never has to reason about which half of the seal ran.
        errdefer {
            self.restore(mark_);
            self.active_transaction = null;
        }

        const interned_original = try self.allocator.alloc(TypeId, suffix_len);
        defer self.allocator.free(interned_original);
        const digests = try self.allocator.alloc(names.TypeDigest, suffix_len);
        defer self.allocator.free(digests);
        // Durable index of each speculative offset that survives as a
        // representative, relative to the transaction's first id. Recording it
        // during classification keeps remap construction linear.
        const representative_of_offset = try self.allocator.alloc(u32, suffix_len);
        defer self.allocator.free(representative_of_offset);
        var representatives = std.ArrayList(TypeId).empty;
        defer representatives.deinit(self.allocator);

        // Speculative offsets grouped by digest. A candidate can only be equal
        // to a digest-equal earlier candidate, so grouping replaces a scan of
        // the whole suffix per node.
        var suffix_by_digest = std.AutoHashMap(DigestBucketKey, std.ArrayList(u32)).init(self.allocator);
        defer {
            var groups = suffix_by_digest.valueIterator();
            while (groups.next()) |group| group.deinit(self.allocator);
            suffix_by_digest.deinit();
        }

        for (0..suffix_len) |offset| {
            const candidate: TypeId = @enumFromInt(@as(u32, @intCast(mark_.types_len + offset)));
            // Fallible rather than `typeDigestCached`: inside a transaction an
            // exhausted allocator has a correct answer (roll the seal back),
            // so it must not become the digest path's panic.
            digests[offset] = try self.computeDigest(name_store, candidate, .full, null);
            const key = DigestBucketKey.from(digests[offset]);
            const group = try suffix_by_digest.getOrPut(key);
            if (!group.found_existing) group.value_ptr.* = .empty;

            var interned: ?TypeId = null;
            if (self.full_digest_interned.get(key)) |bucket| {
                for (bucket.items) |existing| {
                    if (try self.bucketHit(name_store, key, existing, candidate)) {
                        interned = existing;
                        break;
                    }
                }
            }
            if (interned == null) {
                for (group.value_ptr.items) |earlier| {
                    const earlier_ty: TypeId = @enumFromInt(@as(u32, @intCast(mark_.types_len + earlier)));
                    if (try self.typeEql(name_store, earlier_ty, candidate)) {
                        interned = interned_original[earlier];
                        break;
                    }
                }
            }
            if (interned) |found| {
                interned_original[offset] = found;
            } else {
                interned_original[offset] = candidate;
                representative_of_offset[offset] = @intCast(representatives.items.len);
                try representatives.append(self.allocator, candidate);
            }
            // Re-fetched rather than reusing `group.value_ptr`: nothing above
            // inserts into this map today, but a future one would move it.
            // Every offset is grouped, not just representatives, so a later
            // candidate compares against the same set the flat scan did.
            try suffix_by_digest.getPtr(key).?.append(self.allocator, @intCast(offset));
        }

        // Owned copies of just the representatives' rows. Construction reads
        // them after the speculative suffix is truncated, so the seal costs
        // memory proportional to the surviving speculative nodes instead of to
        // a duplicate of every durable pool.
        var captured = try std.ArrayList(CapturedNode).initCapacity(self.allocator, representatives.items.len);
        defer {
            for (captured.items) |*node| node.deinit(self.allocator);
            captured.deinit(self.allocator);
        }
        for (representatives.items) |original| {
            captured.appendAssumeCapacity(try self.captureNode(original));
        }

        const result_remap = try self.allocator.alloc(TypeId, suffix_len);
        errdefer self.allocator.free(result_remap);

        // Durable ids are decided before anything is mutated: representative
        // `i` takes the `i`th id at the transaction's mark.
        for (interned_original, 0..) |interned, offset| {
            const interned_index = @intFromEnum(interned);
            if (interned_index < mark_.types_len) {
                result_remap[offset] = interned;
            } else {
                const representative_index = representative_of_offset[interned_index - mark_.types_len];
                result_remap[offset] = @enumFromInt(@as(u32, @intCast(mark_.types_len + representative_index)));
            }
        }

        // Preflight the durable index. Reserving the map slot and the bucket
        // room for every representative here -- while failing is still free --
        // is what lets the indexing step below run without a fallible call.
        try self.reserveInternedCapacity(mark_, interned_original, &suffix_by_digest, representatives.items.len);

        self.restore(mark_);

        for (representatives.items) |_| _ = try self.reserveSlot();

        for (captured.items, 0..) |node, representative_index| {
            const rebuilt = try self.rebuildTransactionContent(mark_, node, result_remap);
            const durable: TypeId = @enumFromInt(@as(u32, @intCast(mark_.types_len + representative_index)));
            self.fillReservedSlot(durable, rebuilt);
        }

        // Indexing into preflighted capacity is infallible, which keeps
        // "committed" and "indexed" from ever disagreeing. Validate every
        // reconstructed representative first so no digest bucket can observe a
        // node whose rewritten graph denotes different content.
        for (representatives.items, 0..) |original, representative_index| {
            const durable: TypeId = @enumFromInt(@as(u32, @intCast(mark_.types_len + representative_index)));
            const digest = digests[@intFromEnum(original) - mark_.types_len];
            const rebuilt_digest = try self.computeDigest(name_store, durable, .full, null);
            if (!std.mem.eql(u8, &rebuilt_digest.bytes, &digest.bytes)) {
                Common.compilerBug("recursive transaction changed a representative's content while rewriting references");
            }
            _ = try self.computeDigest(name_store, durable, .identity_only, null);
        }
        for (representatives.items, 0..) |original, representative_index| {
            const durable: TypeId = @enumFromInt(@as(u32, @intCast(mark_.types_len + representative_index)));
            const digest = digests[@intFromEnum(original) - mark_.types_len];
            const bucket = self.full_digest_interned.getPtr(DigestBucketKey.from(digest)) orelse
                Common.compilerBug("recursive transaction indexed a digest with no preflighted bucket");
            bucket.appendAssumeCapacity(durable);
        }

        self.active_transaction = null;
        return .{
            .allocator = self.allocator,
            .root = result_remap[root_index - mark_.types_len],
            .suffix_start = @intCast(mark_.types_len),
            .remap = result_remap,
        };
    }

    /// Reserve every durable-index slot the indexing step of a commit needs.
    ///
    /// A bucket created here but never filled (because a later step failed) is
    /// an empty entry, which is indistinguishable from an absent one to every
    /// reader, so this preflight needs no rollback of its own.
    fn reserveInternedCapacity(
        self: *Store,
        mark_: Mark,
        interned_original: []const TypeId,
        suffix_by_digest: *std.AutoHashMap(DigestBucketKey, std.ArrayList(u32)),
        representative_count: usize,
    ) std.mem.Allocator.Error!void {
        try self.full_digest_interned.ensureUnusedCapacity(@intCast(representative_count));
        var groups = suffix_by_digest.iterator();
        while (groups.next()) |entry| {
            var needed: usize = 0;
            for (entry.value_ptr.items) |offset| {
                const candidate: TypeId = @enumFromInt(@as(u32, @intCast(mark_.types_len + offset)));
                if (interned_original[offset] == candidate) needed += 1;
            }
            if (needed == 0) continue;
            const bucket = try self.full_digest_interned.getOrPut(entry.key_ptr.*);
            if (!bucket.found_existing) bucket.value_ptr.* = .empty;
            try bucket.value_ptr.ensureUnusedCapacity(self.allocator, needed);
        }
    }

    pub fn ownerHead(self: *const Store, ty: TypeId) OwnerHead {
        return switch (self.get(ty)) {
            .primitive => |primitive| .{ .builtin = checked.builtinOwnerForPrimitive(primitive) },
            .list => .{ .builtin = .list },
            .box => .{ .builtin = .box },
            .named => |named| if (named.builtin_owner) |owner|
                .{ .builtin = owner }
            else if (named.kind == .alias)
                // Aliases are transparent for static dispatch: the owner is
                // the backing's owner. (Content digests keep aliases opaque;
                // dispatch is a representation question, so it unwraps.) This
                // handles alias-over-alias and alias-over-nominal uniformly
                // (the backing of an alias-over-nominal is itself a `named`
                // node carrying the nominal's owner). The recursion
                // terminates because alias chains in checked output are
                // finite.
                (if (named.backing) |backing|
                    self.ownerHead(backing.ty)
                else
                    .none)
            else
                .{ .named_type = named.def },
            .record, .tuple, .tag_union, .func, .erased, .zst => .none,
        };
    }

    /// Full content-identity digest for `ty` (cached on first computation).
    ///
    /// Full digests answer "is this the same stored type?": every field that
    /// deep structural comparison observes participates, and aliases digest
    /// as opaque nodes rather than as their backing.
    pub fn typeDigest(self: *Store, name_store: *const names.NameStore, ty: TypeId) names.TypeDigest {
        return self.typeDigestCached(name_store, ty, null);
    }

    /// Public-interface specialization digest for `ty` (cached on first
    /// computation). This intentionally omits exactly declared field order
    /// and checked-public backing details, because it answers "may these two
    /// types share a specialization?" rather than "are these the same stored
    /// type?".
    pub fn specializationDigest(self: *Store, name_store: *const names.NameStore, ty: TypeId) names.TypeDigest {
        return self.specializationDigestCached(name_store, ty, null);
    }

    /// Digest of the exact equivalence relation implemented by `typeEql`.
    /// Unlike the stored-identity digest, this unwraps aliases and omits
    /// checked-node provenance that does not participate in type equality.
    pub fn equalityDigest(self: *Store, name_store: *const names.NameStore, ty: TypeId) names.TypeDigest {
        return self.computeDigest(name_store, ty, .equality, null) catch digestOutOfMemory();
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
            return try typeViewEql(self, allocator, name_store, lhs, rhs, .exact);
        }

        pub fn typeMatches(
            self: View,
            allocator: std.mem.Allocator,
            name_store: *const names.NameStore,
            lhs: TypeId,
            rhs: TypeId,
            mode: TypeMatchMode,
        ) std.mem.Allocator.Error!bool {
            return try typeViewEql(self, allocator, name_store, lhs, rhs, mode);
        }

        pub fn verify(self: View, name_store: *const names.NameStore) ?VerifyError {
            if (self.type_digests.len != self.types.len) return .type_digest_count_mismatch;

            for (self.spans) |ty| {
                if (!self.typeRefInBounds(ty)) return .type_ref_out_of_bounds;
            }
            for (self.fields) |field| {
                if (!self.typeRefInBounds(field.ty)) return .type_ref_out_of_bounds;
                if (field.value_ty) |value_ty| {
                    if (!self.typeRefInBounds(value_ty)) return .type_ref_out_of_bounds;
                }
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
                if (field.value_ty) |value_ty| {
                    if (!self.typeRefInBounds(value_ty)) return .type_ref_out_of_bounds;
                }
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
        if (self.specialization_digests.len() != self.types.len() or self.equality_digests.len() != self.types.len()) {
            return .type_digest_count_mismatch;
        }
        return self.view().verify(name_store);
    }

    pub fn specializationDigestsView(self: *const Store) []const ?names.TypeDigest {
        return self.specialization_digests.unsafeRawItemsForView();
    }

    /// `typeDigest` with optional cache statistics.
    pub fn typeDigestCached(
        self: *Store,
        name_store: *const names.NameStore,
        ty: TypeId,
        stats: ?*DigestStats,
    ) names.TypeDigest {
        return self.computeDigest(name_store, ty, .full, stats) catch digestOutOfMemory();
    }

    /// `specializationDigest` with optional cache statistics.
    pub fn specializationDigestCached(
        self: *Store,
        name_store: *const names.NameStore,
        ty: TypeId,
        stats: ?*DigestStats,
    ) names.TypeDigest {
        return self.computeDigest(name_store, ty, .identity_only, stats) catch digestOutOfMemory();
    }

    /// Exact structural equality for closed Monotype types.
    ///
    /// Aliases with backing compare as their backing, non-alias named types
    /// compare by named identity and arguments, and structural rows compare by
    /// label text and ordered children. Equal full digests imply equality
    /// here—the direction interning relies on. The converse can fail one way:
    /// aliases digest as opaque nodes (deliberately), and the digest observes
    /// identity fields this comparison does not (`named_type.ty`,
    /// `tag.checked_name`, `type_name` under a `source_decl`, checked-public
    /// backing content), which production constructs consistently for equal
    /// types. This is the authoritative check before one specialization can
    /// reuse another.
    pub fn typeEql(
        self: *const Store,
        name_store: *const names.NameStore,
        lhs: TypeId,
        rhs: TypeId,
    ) std.mem.Allocator.Error!bool {
        return try self.view().typeEql(self.allocator, name_store, lhs, rhs);
    }

    /// `typeEql` under an explicit match mode. See `TypeMatchMode`.
    pub fn typeMatches(
        self: *const Store,
        name_store: *const names.NameStore,
        lhs: TypeId,
        rhs: TypeId,
        mode: TypeMatchMode,
    ) std.mem.Allocator.Error!bool {
        return try self.view().typeMatches(self.allocator, name_store, lhs, rhs, mode);
    }

    /// Whether `ty` is already a durable representative in the full-digest
    /// index. Draft construction may append immutable-looking nodes whose
    /// children are not durable yet; final sealing uses this distinction to
    /// copy and intern them even when they contain no active graph snapshot.
    pub fn isInterned(
        self: *Store,
        name_store: *const names.NameStore,
        ty: TypeId,
    ) std.mem.Allocator.Error!bool {
        self.requireConstructed(ty);
        const digest = try self.computeDigest(name_store, ty, .full, null);
        const bucket = self.full_digest_interned.get(DigestBucketKey.from(digest)) orelse return false;
        for (bucket.items) |existing| {
            if (existing == ty) return true;
        }
        return false;
    }

    const DigestBucketKey = struct {
        bytes: [32]u8,

        fn from(digest: names.TypeDigest) DigestBucketKey {
            return .{ .bytes = digest.bytes };
        }
    };

    /// Store-level acyclic interning: child-first construction of immutable
    /// Monotype nodes directly on `Store`. Outside a transaction, callers
    /// provide already-interned child `TypeId`s and receive an immutable,
    /// interned result. During a transaction these calls append unindexed nodes
    /// to its suffix instead; the owner's `TransactionResult` then supplies
    /// their durable ids. A recursive group has no child-first order and is
    /// sealed as a whole by `beginTransaction` / `commitTransaction`, which
    /// shares this same digest index.
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

    pub const TagInput = struct {
        name: names.TagNameId,
        checked_name: names.TagNameId,
        payloads: []const TypeId,
    };

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

    pub const NamedInput = struct {
        named_type: NamedType,
        def: TypeDef,
        kind: NamedKind,
        builtin_owner: ?static_dispatch.BuiltinOwner = null,
        args: []const TypeId = &.{},
        backing: ?NamedBacking = null,
        declared_order: []const DeclaredField = &.{},
    };

    pub fn internNamed(self: *Store, name_store: *const names.NameStore, named: NamedInput) std.mem.Allocator.Error!TypeId {
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

    /// Owned copy of one speculative node together with only the side-pool
    /// rows it references, every span rebased onto those owned buffers.
    ///
    /// Sealing truncates the speculative suffix before constructing durable
    /// nodes, so the old rows must outlive the truncation. Capturing them
    /// per surviving representative bounds that cost by the speculative
    /// suffix; copying the durable pools would bound it by store size.
    const CapturedNode = struct {
        content: Content,
        /// Referenced type ids: tuple elements, function arguments, named
        /// arguments, or every tag's payloads concatenated in tag order.
        type_refs: []const TypeId,
        fields: []const Field,
        tags: []const Tag,
        declared: []const DeclaredField,

        fn deinit(self: *CapturedNode, allocator: std.mem.Allocator) void {
            allocator.free(self.type_refs);
            allocator.free(self.fields);
            allocator.free(self.tags);
            allocator.free(self.declared);
            self.* = undefined;
        }
    };

    fn captureNode(self: *const Store, ty: TypeId) std.mem.Allocator.Error!CapturedNode {
        var captured: CapturedNode = .{
            .content = .zst,
            .type_refs = &.{},
            .fields = &.{},
            .tags = &.{},
            .declared = &.{},
        };
        errdefer captured.deinit(self.allocator);

        switch (self.get(ty)) {
            .primitive, .erased, .zst, .list, .box => captured.content = self.get(ty),
            .tuple => |span_| {
                captured.type_refs = try self.spans.dupeSpan(self.allocator, span_.start, span_.len);
                captured.content = .{ .tuple = .{ .start = 0, .len = span_.len } };
            },
            .func => |func| {
                captured.type_refs = try self.spans.dupeSpan(self.allocator, func.args.start, func.args.len);
                captured.content = .{ .func = .{
                    .args = .{ .start = 0, .len = func.args.len },
                    .ret = func.ret,
                } };
            },
            .record => |span_| {
                captured.fields = try self.fields.dupeSpan(self.allocator, span_.start, span_.len);
                captured.content = .{ .record = .{ .start = 0, .len = span_.len } };
            },
            .tag_union => |span_| {
                const tags_ = try self.tags.dupeSpan(self.allocator, span_.start, span_.len);
                captured.tags = tags_;
                var payload_len: usize = 0;
                for (tags_) |tag| payload_len += tag.payloads.len;
                const payloads = try self.allocator.alloc(TypeId, payload_len);
                captured.type_refs = payloads;
                const stored = self.spans.unsafeRawItemsForView();
                var next: u32 = 0;
                for (tags_) |*tag| {
                    const len = tag.payloads.len;
                    @memcpy(payloads[next..][0..len], stored[tag.payloads.start..][0..len]);
                    tag.payloads = .{ .start = next, .len = len };
                    next += len;
                }
                captured.content = .{ .tag_union = .{ .start = 0, .len = span_.len } };
            },
            .named => |named| {
                captured.type_refs = try self.spans.dupeSpan(self.allocator, named.args.start, named.args.len);
                captured.declared = try self.declared_fields.dupeSpan(
                    self.allocator,
                    named.declared_order.start,
                    named.declared_order.len,
                );
                var rebuilt = named;
                rebuilt.args = .{ .start = 0, .len = named.args.len };
                rebuilt.declared_order = .{ .start = 0, .len = named.declared_order.len };
                captured.content = .{ .named = rebuilt };
            },
        }
        return captured;
    }

    fn transactionType(mark_: Mark, remap: []const TypeId, ty: TypeId) TypeId {
        const index = @intFromEnum(ty);
        if (index < mark_.types_len) return ty;
        const offset = index - mark_.types_len;
        // A reference past the remap means the node escaped the suffix the
        // remap describes, which would silently rewrite it to garbage.
        if (offset >= remap.len) {
            Common.compilerBug("recursive transaction reference is outside its suffix");
        }
        return remap[offset];
    }

    fn rebuildTransactionTypeSpan(
        self: *Store,
        mark_: Mark,
        old: Span,
        snapshot: []const TypeId,
        remap: []const TypeId,
    ) std.mem.Allocator.Error!Span {
        if (old.len == 0) return .empty();
        const rewritten = try self.allocator.alloc(TypeId, old.len);
        defer self.allocator.free(rewritten);
        for (snapshot[old.start..][0..old.len], rewritten) |ty, *out| {
            out.* = transactionType(mark_, remap, ty);
        }
        return try self.addSpan(rewritten);
    }

    fn rebuildTransactionContent(
        self: *Store,
        mark_: Mark,
        captured: CapturedNode,
        remap: []const TypeId,
    ) std.mem.Allocator.Error!Content {
        const snapshot_spans = captured.type_refs;
        const snapshot_fields = captured.fields;
        const snapshot_tags = captured.tags;
        const snapshot_declared = captured.declared;
        return switch (captured.content) {
            .primitive => |value| .{ .primitive = value },
            .erased => |value| .{ .erased = value },
            .zst => .zst,
            .list => |ty| .{ .list = transactionType(mark_, remap, ty) },
            .box => |ty| .{ .box = transactionType(mark_, remap, ty) },
            .tuple => |span_| .{ .tuple = try self.rebuildTransactionTypeSpan(mark_, span_, snapshot_spans, remap) },
            .func => |func| .{ .func = .{
                .args = try self.rebuildTransactionTypeSpan(mark_, func.args, snapshot_spans, remap),
                .ret = transactionType(mark_, remap, func.ret),
            } },
            .record => |span_| blk: {
                const old_fields = snapshot_fields[span_.start..][0..span_.len];
                const fields_ = try self.allocator.dupe(Field, old_fields);
                defer self.allocator.free(fields_);
                for (fields_) |*field| {
                    field.ty = transactionType(mark_, remap, field.ty);
                    if (field.value_ty) |value_ty| {
                        field.value_ty = transactionType(mark_, remap, value_ty);
                    }
                }
                break :blk .{ .record = try self.addFields(fields_) };
            },
            .tag_union => |span_| blk: {
                const old_tags = snapshot_tags[span_.start..][0..span_.len];
                const tags_ = try self.allocator.dupe(Tag, old_tags);
                defer self.allocator.free(tags_);
                for (tags_) |*tag| {
                    tag.payloads = try self.rebuildTransactionTypeSpan(mark_, tag.payloads, snapshot_spans, remap);
                }
                break :blk .{ .tag_union = try self.addTags(tags_) };
            },
            .named => |named| blk: {
                var rebuilt = named;
                rebuilt.args = try self.rebuildTransactionTypeSpan(mark_, named.args, snapshot_spans, remap);
                if (rebuilt.backing) |*backing| {
                    backing.ty = transactionType(mark_, remap, backing.ty);
                }
                if (named.declared_order.len != 0) {
                    const declared = try self.allocator.dupe(
                        DeclaredField,
                        snapshot_declared[named.declared_order.start..][0..named.declared_order.len],
                    );
                    defer self.allocator.free(declared);
                    for (declared) |*field| {
                        switch (field.*) {
                            .named => {},
                            .padding => |ty| field.* = .{ .padding = transactionType(mark_, remap, ty) },
                        }
                    }
                    rebuilt.declared_order = try self.addDeclaredFields(declared);
                }
                break :blk .{ .named = rebuilt };
            },
        };
    }

    fn indexInterned(self: *Store, digest: names.TypeDigest, candidate: TypeId) std.mem.Allocator.Error!void {
        const key = DigestBucketKey.from(digest);
        if (self.full_digest_interned.getPtr(key)) |bucket| {
            try bucket.append(self.allocator, candidate);
            return;
        }
        var bucket = std.ArrayList(TypeId).empty;
        errdefer bucket.deinit(self.allocator);
        try bucket.append(self.allocator, candidate);
        try self.full_digest_interned.put(key, bucket);
    }

    /// Decide whether one digest-bucket entry really is `candidate`.
    ///
    /// A bucket only groups digest-equal candidates: `typeEql` stays the sole
    /// authority for collapsing two ids, because a digest collision must never
    /// alias distinct types. The debug assertions check the index's own
    /// invariants around a hit -- the entry carries the bucket's digest, and
    /// structural equality is symmetric on the pair about to be collapsed --
    /// so a corrupted bucket or an asymmetric comparison surfaces here instead
    /// of as a mysteriously shared type downstream.
    fn bucketHit(
        self: *Store,
        name_store: *const names.NameStore,
        key: DigestBucketKey,
        existing: TypeId,
        candidate: TypeId,
    ) std.mem.Allocator.Error!bool {
        if (!try self.typeEql(name_store, existing, candidate)) return false;
        if (std.debug.runtime_safety) {
            // Allocation failure inside these checks propagates like any
            // other digest or equality allocation failure; the entry's digest
            // is already cached from interning, so this does not allocate in
            // practice.
            const existing_digest = try self.computeDigest(name_store, existing, .full, null);
            std.debug.assert(std.mem.eql(u8, &existing_digest.bytes, &key.bytes));
            std.debug.assert(try self.typeEql(name_store, candidate, existing));
        }
        return true;
    }

    fn internCandidate(self: *Store, name_store: *const names.NameStore, mark_: Mark, candidate: TypeId) std.mem.Allocator.Error!TypeId {
        errdefer self.restore(mark_);
        if (self.hasSpeculativeConstruction()) return candidate;

        const digest = try self.computeDigest(name_store, candidate, .full, null);
        _ = try self.computeDigest(name_store, candidate, .identity_only, null);
        const key = DigestBucketKey.from(digest);
        if (self.full_digest_interned.getPtr(key)) |bucket| {
            for (bucket.items) |existing| {
                if (try self.bucketHit(name_store, key, existing, candidate)) {
                    self.restore(mark_);
                    return existing;
                }
            }
            try bucket.append(self.allocator, candidate);
            return candidate;
        }

        try self.indexInterned(digest, candidate);
        return candidate;
    }

    /// Which digest question is being answered. The two modes are separate
    /// versioned domains and must never produce byte-confusable answers.
    const NamedDigestMode = enum {
        full,
        identity_only,
        equality,
    };

    /// Versioned digest-domain prefix written at the start of every node
    /// encoding. Digest bytes are serialized and compared across builds, so
    /// changing a domain (or any encoding detail) is a specialization-cache
    /// format change.
    fn digestDomain(mode: NamedDigestMode) []const u8 {
        return switch (mode) {
            .full => "roc.monotype.type.identity.v1",
            .identity_only => "roc.monotype.type.interface.v1",
            .equality => "roc.monotype.type.equality.v1",
        };
    }

    fn assertMutable(self: *const Store) void {
        if (self.frozen) Common.invariant("frozen Monotype type store cannot be mutated");
    }

    fn requireConstructed(self: *const Store, ty: TypeId) void {
        const index = @intFromEnum(ty);
        if (index >= self.constructing.len() or self.constructing.unsafeRawItemsForView()[index]) {
            Common.invariant("Monotype digest requested for an unfinished type slot");
        }
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
            if (field.value_ty) |value_ty| {
                if (!self.typeRefInBounds(value_ty)) return .type_ref_out_of_bounds;
            }
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

    /// Cache lookup for one digest mode. Filled nodes are immutable, so a
    /// cached digest is permanently valid.
    fn cachedDigest(self: *const Store, ty: TypeId, mode: NamedDigestMode) ?names.TypeDigest {
        const index = @intFromEnum(ty);
        return switch (mode) {
            .full => self.type_digests.unsafeRawItemsForView()[index],
            .identity_only => self.specialization_digests.unsafeRawItemsForView()[index],
            .equality => self.equality_digests.unsafeRawItemsForView()[index],
        };
    }

    /// Sole writer of the digest caches. Digests are content-addressed, so a
    /// re-write must agree with the existing entry.
    fn setCachedDigest(self: *Store, ty: TypeId, mode: NamedDigestMode, digest: names.TypeDigest) void {
        if (self.cachedDigest(ty, mode)) |existing| {
            std.debug.assert(std.mem.eql(u8, &existing.bytes, &digest.bytes));
        }
        const index = @intFromEnum(ty);
        switch (mode) {
            .full => self.type_digests.set(index, digest),
            .identity_only => self.specialization_digests.set(index, digest),
            .equality => self.equality_digests.set(index, digest),
        }
    }

    fn digestType(self: *const Store, raw_ty: TypeId, mode: NamedDigestMode) TypeId {
        if (mode != .equality) return raw_ty;
        var ty = raw_ty;
        while (true) {
            const content = self.get(ty);
            if (content != .named or content.named.kind != .alias) return ty;
            const backing = content.named.backing orelse return ty;
            ty = backing.ty;
        }
    }

    /// One digest implementation for every mode: serve the request from the
    /// cache or reduce the uncached reachable subgraph and cache every digest
    /// it settles.
    fn computeDigest(
        self: *Store,
        name_store: *const names.NameStore,
        ty: TypeId,
        mode: NamedDigestMode,
        stats: ?*DigestStats,
    ) std.mem.Allocator.Error!names.TypeDigest {
        self.requireConstructed(ty);
        if (self.cachedDigest(ty, mode)) |digest| {
            if (stats) |s| s.cache_hits += 1;
            return digest;
        }
        var engine = DigestEngine.init(self, name_store, stats);
        defer engine.deinit();
        return try engine.run(ty, mode);
    }

    /// The digest byte encoding of one type node in one digest mode.
    ///
    /// This is the single source of digest bytes: graph discovery,
    /// recursive-group reduction, and final digest rendering all replay this
    /// encoding with different child-reference sinks, so no consumer can
    /// observe two byte encodings for the same mode. Every node starts with
    /// its versioned digest domain, then a content-kind discriminator, every
    /// non-reference value the mode observes, and its ordered typed edges via
    /// `sink.child`.
    ///
    /// Aliases are opaque named nodes rather than digesting as their backing.
    /// `def.type_name` text is always hashed (also when `source_decl` is
    /// present), `named_type.ty` is hashed because it survives into
    /// `ConstStore`, and `tag.checked_name` is hashed in addition to
    /// `tag.name`. The interface mode omits exactly declared field order and
    /// checked-public backing details; backing children always digest in full
    /// mode because a backing is a stored type identity, not an interface.
    fn encodeTypeNode(
        self: *const Store,
        name_store: *const names.NameStore,
        sink: anytype,
        ty: TypeId,
        mode: NamedDigestMode,
    ) std.mem.Allocator.Error!void {
        try sink.writeBytes(digestDomain(mode));
        switch (self.get(ty)) {
            .primitive => |primitive| {
                try sink.writeBytes("primitive");
                try sink.writeBytes(@tagName(primitive));
            },
            .named => |named| {
                try sink.writeBytes("named");
                try sink.writeBytes(&named.named_type.module.bytes);
                if (mode != .equality) try sink.writeU32(@intFromEnum(named.named_type.ty));
                try sink.writeBytes(name_store.moduleIdentityBytes(named.def.module));
                try sinkOptionalU32(sink, named.def.source_decl);
                if (mode != .equality or named.def.source_decl == null) {
                    try sink.writeBytes(name_store.typeNameText(named.def.type_name));
                }
                try sinkOptionalDigest(sink, named.def.generated);
                try sink.writeBytes(@tagName(named.def.iterator_representation));
                try sink.writeBytes(@tagName(named.def.iterator_kind));
                try sink.writeU32(named.def.iterator_depth);
                try sinkIteratorTopology(sink, name_store, named.def.iterator_topology);
                try sink.writeBytes(@tagName(named.kind));
                if (named.builtin_owner) |owner| {
                    try sink.writeBytes("builtin");
                    try sink.writeBytes(@tagName(owner));
                } else {
                    try sink.writeBytes("not-builtin");
                }
                try self.encodeTypeSpan(sink, named.args, mode);
                switch (mode) {
                    .full => {
                        try encodeNamedBacking(sink, named.backing);
                        try self.encodeDeclaredOrder(name_store, sink, named.declared_order);
                    },
                    .identity_only => if (specializationUsesBacking(named.backing)) {
                        try sink.writeBytes("specialization-generated-backing");
                        try encodeNamedBacking(sink, named.backing);
                    } else {
                        try sink.writeBytes("specialization-named-identity");
                    },
                    .equality => if (specializationUsesBacking(named.backing)) {
                        try sink.writeBytes("equality-generated-backing");
                        const backing = named.backing orelse unreachable;
                        try sink.writeBytes(@tagName(backing.use));
                        try sink.writeBytes(@tagName(backing.authority));
                        try sink.child(backing.ty, .equality);
                    } else {
                        try sink.writeBytes("equality-named-identity");
                    },
                }
            },
            .record => |fields| {
                try sink.writeBytes("record");
                const field_slice = self.fieldSpan(fields);
                try sink.writeU32(@intCast(field_slice.len));
                for (0..field_slice.len) |index| {
                    const field = GuardedList.at(field_slice, index);
                    try sink.writeBytes(name_store.recordFieldLabelText(field.name));
                    try sinkFieldDefault(sink, name_store, field.default);
                    try sink.writeBytes(@tagName(field.kind_state));
                    if (field.value_ty) |value_ty| {
                        try sink.writeBytes("field-optional-value");
                        try sink.child(value_ty, mode);
                    } else {
                        try sink.writeBytes("field-inline-value");
                    }
                    try sink.child(field.ty, mode);
                }
            },
            .tuple => |items| {
                try sink.writeBytes("tuple");
                try self.encodeTypeSpan(sink, items, mode);
            },
            .tag_union => |tags| {
                try sink.writeBytes("tag_union");
                const tag_slice = self.tagSpan(tags);
                try sink.writeU32(@intCast(tag_slice.len));
                for (0..tag_slice.len) |index| {
                    const tag = GuardedList.at(tag_slice, index);
                    try sink.writeBytes(name_store.tagLabelText(tag.name));
                    if (mode != .equality) try sink.writeBytes(name_store.tagLabelText(tag.checked_name));
                    try self.encodeTypeSpan(sink, tag.payloads, mode);
                }
            },
            .list => |elem| {
                try sink.writeBytes("list");
                try sink.child(elem, mode);
            },
            .box => |elem| {
                try sink.writeBytes("box");
                try sink.child(elem, mode);
            },
            .func => |function| {
                try sink.writeBytes("func");
                try self.encodeTypeSpan(sink, function.args, mode);
                try sink.child(function.ret, mode);
            },
            .erased => |erased| {
                try sink.writeBytes("erased");
                try sink.writeBytes(&erased.bytes);
            },
            .zst => try sink.writeBytes("zst"),
        }
    }

    fn encodeTypeSpan(
        self: *const Store,
        sink: anytype,
        span_: Span,
        mode: NamedDigestMode,
    ) std.mem.Allocator.Error!void {
        const values = self.span(span_);
        try sink.writeU32(@intCast(values.len));
        for (0..values.len) |index| {
            try sink.child(GuardedList.at(values, index), mode);
        }
    }

    fn encodeDeclaredOrder(
        self: *const Store,
        name_store: *const names.NameStore,
        sink: anytype,
        declared_order: Span,
    ) std.mem.Allocator.Error!void {
        try sink.writeBytes("declared_order");
        const entries = self.declaredFieldSpan(declared_order);
        try sink.writeU32(@intCast(entries.len));
        for (0..entries.len) |index| {
            switch (GuardedList.at(entries, index)) {
                .named => |field_name| {
                    try sink.writeBytes("named");
                    try sink.writeBytes(name_store.recordFieldLabelText(field_name));
                },
                .padding => |padding_ty| {
                    try sink.writeBytes("padding");
                    try sink.child(padding_ty, .full);
                },
            }
        }
    }

    /// One digest-request node: a store type digested under one mode. The
    /// interface mode reaches full-mode nodes through generated backings, so
    /// the pair is the graph key, not the type id alone.
    const DigestNode = struct {
        ty: TypeId,
        mode: NamedDigestMode,
        link_start: u32 = 0,
        link_len: u32 = 0,
        digest: names.TypeDigest = undefined,
        resolved: bool = false,
    };

    /// A child reference during digesting: either an already-finalized digest
    /// or a node of the currently discovered graph. Whether a child arrived as
    /// a cache hit or as a discovered-then-resolved node must never influence
    /// digest bytes or partitioning, so every consumer of a `ChildLink` treats
    /// a resolved node and an external digest identically.
    const ChildLink = union(enum) {
        external: names.TypeDigest,
        node: u32,
    };

    /// Graph-reducing digest computation for one requested type.
    ///
    /// Discovery walks the uncached reachable subgraph iteratively, pruning
    /// at children whose digest for the required mode is already cached.
    /// Iterative Tarjan SCC discovery then resolves the condensation in
    /// reverse topological order: an acyclic node hashes its encoding with
    /// finalized child digests, and each cyclic SCC is reduced by
    /// bisimulation refinement, linearized by the minimum-over-entry-points
    /// rotation with group-relative back-references, and digested per reduced
    /// position. Every settled digest is cached unconditionally, and each
    /// reduced position's one-step unfolding enters the store's unfolding
    /// index so later rolled-out prefixes of the same group fold to the same
    /// digests.
    ///
    /// Known conservative incompleteness: per-SCC reduction cannot identify
    /// an in-SCC position with an equivalent position outside its own SCC, so
    /// a knot entangled with a separately tied equivalent knot digests apart
    /// from it (see the "entangled equivalent knots" test). Equal digests
    /// still always imply structural equality; the miss only costs reuse.
    const DigestEngine = struct {
        store: *Store,
        name_store: *const names.NameStore,
        gpa: std.mem.Allocator,
        stats: ?*DigestStats,
        nodes: std.ArrayList(DigestNode),
        node_lookup: std.AutoHashMap(u64, u32),
        link_pool: std.ArrayList(ChildLink),
        render_buf: std.ArrayList(u8),

        const no_scc_position = std.math.maxInt(u32);
        const unvisited = std.math.maxInt(u32);

        fn init(store: *Store, name_store: *const names.NameStore, stats: ?*DigestStats) DigestEngine {
            return .{
                .store = store,
                .name_store = name_store,
                .gpa = store.allocator,
                .stats = stats,
                .nodes = .empty,
                .node_lookup = std.AutoHashMap(u64, u32).init(store.allocator),
                .link_pool = .empty,
                .render_buf = .empty,
            };
        }

        fn deinit(self: *DigestEngine) void {
            self.render_buf.deinit(self.gpa);
            self.link_pool.deinit(self.gpa);
            self.node_lookup.deinit();
            self.nodes.deinit(self.gpa);
        }

        fn run(self: *DigestEngine, ty: TypeId, mode: NamedDigestMode) std.mem.Allocator.Error!names.TypeDigest {
            const root = try self.internNode(self.store.digestType(ty, mode), mode);
            std.debug.assert(root == 0);
            var next: usize = 0;
            while (next < self.nodes.items.len) : (next += 1) {
                try self.collectNode(@intCast(next));
            }
            try self.resolveSccs();
            const root_node = self.nodes.items[root];
            std.debug.assert(root_node.resolved);
            return root_node.digest;
        }

        fn nodeKey(ty: TypeId, mode: NamedDigestMode) u64 {
            return (@as(u64, @intFromEnum(ty)) << 2) | @as(u64, @intFromEnum(mode));
        }

        fn internNode(self: *DigestEngine, raw_ty: TypeId, mode: NamedDigestMode) std.mem.Allocator.Error!u32 {
            const ty = self.store.digestType(raw_ty, mode);
            const gop = try self.node_lookup.getOrPut(nodeKey(ty, mode));
            if (gop.found_existing) return gop.value_ptr.*;
            const index: u32 = @intCast(self.nodes.items.len);
            gop.value_ptr.* = index;
            try self.nodes.append(self.gpa, .{ .ty = ty, .mode = mode });
            if (self.stats) |s| {
                s.cache_misses += 1;
                s.nodes_visited += 1;
            }
            return index;
        }

        /// Classify one child reference during discovery: already-cached
        /// children participate as finalized digests, everything else becomes
        /// a node of the discovered graph.
        fn childLink(self: *DigestEngine, raw_ty: TypeId, mode: NamedDigestMode) std.mem.Allocator.Error!ChildLink {
            const ty = self.store.digestType(raw_ty, mode);
            self.store.requireConstructed(ty);
            if (self.store.cachedDigest(ty, mode)) |digest| return .{ .external = digest };
            return .{ .node = try self.internNode(ty, mode) };
        }

        /// `childLink` for rendering passes after discovery: never grows the
        /// graph, and nodes this engine already finalized may resolve through
        /// the store cache with identical bytes.
        fn resolvedChildLink(self: *DigestEngine, raw_ty: TypeId, mode: NamedDigestMode) ChildLink {
            const ty = self.store.digestType(raw_ty, mode);
            if (self.store.cachedDigest(ty, mode)) |digest| return .{ .external = digest };
            return .{
                .node = self.node_lookup.get(nodeKey(ty, mode)) orelse
                    Common.invariant("Monotype digest rendering reached a child that discovery never visited"),
            };
        }

        fn collectNode(self: *DigestEngine, index: u32) std.mem.Allocator.Error!void {
            const ty = self.nodes.items[index].ty;
            const mode = self.nodes.items[index].mode;
            const link_start: u32 = @intCast(self.link_pool.items.len);
            const sink = CollectSink{ .engine = self };
            try self.store.encodeTypeNode(self.name_store, sink, ty, mode);
            const node = &self.nodes.items[index];
            node.link_start = link_start;
            node.link_len = @intCast(self.link_pool.items.len - link_start);
        }

        fn linksOf(self: *const DigestEngine, index: u32) []const ChildLink {
            const node = self.nodes.items[index];
            return self.link_pool.items[node.link_start..][0..node.link_len];
        }

        /// Discovery sink: collects the node's ordered child references while
        /// expanding the graph. Scalars are ignored here; they participate
        /// through the rendering sinks once the graph shape is known.
        const CollectSink = struct {
            engine: *DigestEngine,

            fn writeBytes(_: CollectSink, _: []const u8) std.mem.Allocator.Error!void {}

            fn writeU32(_: CollectSink, _: u32) std.mem.Allocator.Error!void {}

            fn child(self: CollectSink, ty: TypeId, mode: NamedDigestMode) std.mem.Allocator.Error!void {
                const ref = try self.engine.childLink(ty, mode);
                if (ref == .external) {
                    if (self.engine.stats) |s| s.cache_hits += 1;
                }
                try self.engine.link_pool.append(self.engine.gpa, ref);
            }
        };

        /// Initial-partition sink for one cyclic SCC: hashes the member's
        /// full encoding with every out-of-SCC child rendered as its
        /// finalized digest and every in-SCC child as a bare positional
        /// marker. In-SCC identities are refined afterwards; folding them
        /// into this label would bake discovery order into the partition.
        const SccLabelSink = struct {
            engine: *DigestEngine,
            hasher: *std.crypto.hash.sha2.Sha256,
            member_pos_of_node: []const u32,

            fn writeBytes(self: SccLabelSink, bytes: []const u8) std.mem.Allocator.Error!void {
                hashBytes(self.hasher, bytes);
            }

            fn writeU32(self: SccLabelSink, value: u32) std.mem.Allocator.Error!void {
                hashU32(self.hasher, value);
            }

            fn child(self: SccLabelSink, ty: TypeId, mode: NamedDigestMode) std.mem.Allocator.Error!void {
                switch (self.engine.resolvedChildLink(ty, mode)) {
                    .external => |digest| {
                        hashBytes(self.hasher, "type-digest");
                        self.hasher.update(&digest.bytes);
                    },
                    .node => |node_index| {
                        if (self.member_pos_of_node[node_index] != no_scc_position) {
                            hashBytes(self.hasher, "in-scc");
                            return;
                        }
                        const node = self.engine.nodes.items[node_index];
                        std.debug.assert(node.resolved);
                        hashBytes(self.hasher, "type-digest");
                        self.hasher.update(&node.digest.bytes);
                    },
                }
            }
        };

        /// Group-relative reference context while rendering one cyclic SCC.
        const SccRenderContext = struct {
            /// Member position within the SCC per engine node, or
            /// `no_scc_position` for nodes outside the SCC.
            member_pos_of_node: []const u32,
            blocks: []const u32,
            order_of_block: []const u32,
        };

        /// Rendering sink: writes the encoding into a byte buffer, resolving
        /// children to their finalized digests, or to group-relative
        /// back-references inside a cyclic SCC.
        const RenderSink = struct {
            engine: *DigestEngine,
            out: *std.ArrayList(u8),
            scc: ?*const SccRenderContext,

            fn writeBytes(self: RenderSink, bytes: []const u8) std.mem.Allocator.Error!void {
                try renderBytes(self.engine.gpa, self.out, bytes);
            }

            fn writeU32(self: RenderSink, value: u32) std.mem.Allocator.Error!void {
                try renderU32(self.engine.gpa, self.out, value);
            }

            fn child(self: RenderSink, ty: TypeId, mode: NamedDigestMode) std.mem.Allocator.Error!void {
                switch (self.engine.resolvedChildLink(ty, mode)) {
                    .external => |digest| try self.emitDigest(digest),
                    .node => |node_index| {
                        if (self.scc) |ctx| {
                            const member_pos = ctx.member_pos_of_node[node_index];
                            if (member_pos != no_scc_position) {
                                try renderBytes(self.engine.gpa, self.out, "group-ref");
                                try renderU32(self.engine.gpa, self.out, ctx.order_of_block[ctx.blocks[member_pos]]);
                                return;
                            }
                        }
                        const node = self.engine.nodes.items[node_index];
                        std.debug.assert(node.resolved);
                        try self.emitDigest(node.digest);
                    },
                }
            }

            fn emitDigest(self: RenderSink, digest: names.TypeDigest) std.mem.Allocator.Error!void {
                try renderBytes(self.engine.gpa, self.out, "type-digest");
                try renderBytes(self.engine.gpa, self.out, &digest.bytes);
            }
        };

        /// Iterative Tarjan SCC discovery. SCCs pop in reverse topological
        /// order of the condensation, so every edge leaving a popped SCC
        /// already points at finalized digests and it can resolve on the spot.
        fn resolveSccs(self: *DigestEngine) std.mem.Allocator.Error!void {
            const node_count = self.nodes.items.len;
            const visit_index = try self.gpa.alloc(u32, node_count);
            defer self.gpa.free(visit_index);
            @memset(visit_index, unvisited);
            const low_link = try self.gpa.alloc(u32, node_count);
            defer self.gpa.free(low_link);
            const on_stack = try self.gpa.alloc(bool, node_count);
            defer self.gpa.free(on_stack);
            @memset(on_stack, false);

            var scc_stack: std.ArrayList(u32) = .empty;
            defer scc_stack.deinit(self.gpa);
            const Frame = struct { node: u32, edge_cursor: u32 };
            var frames: std.ArrayList(Frame) = .empty;
            defer frames.deinit(self.gpa);
            var scc_members: std.ArrayList(u32) = .empty;
            defer scc_members.deinit(self.gpa);

            var next_visit: u32 = 0;
            for (0..node_count) |start| {
                if (visit_index[start] != unvisited) continue;
                visit_index[start] = next_visit;
                low_link[start] = next_visit;
                next_visit += 1;
                try scc_stack.append(self.gpa, @intCast(start));
                on_stack[start] = true;
                try frames.append(self.gpa, .{ .node = @intCast(start), .edge_cursor = 0 });

                while (frames.items.len > 0) {
                    const frame = &frames.items[frames.items.len - 1];
                    const v = frame.node;
                    const refs = self.linksOf(v);
                    if (frame.edge_cursor < refs.len) {
                        const ref = refs[frame.edge_cursor];
                        frame.edge_cursor += 1;
                        const w = switch (ref) {
                            .external => continue,
                            .node => |node_index| node_index,
                        };
                        if (visit_index[w] == unvisited) {
                            visit_index[w] = next_visit;
                            low_link[w] = next_visit;
                            next_visit += 1;
                            try scc_stack.append(self.gpa, w);
                            on_stack[w] = true;
                            try frames.append(self.gpa, .{ .node = w, .edge_cursor = 0 });
                        } else if (on_stack[w]) {
                            low_link[v] = @min(low_link[v], visit_index[w]);
                        }
                        continue;
                    }
                    _ = frames.pop();
                    if (frames.items.len > 0) {
                        const parent = frames.items[frames.items.len - 1].node;
                        low_link[parent] = @min(low_link[parent], low_link[v]);
                    }
                    if (low_link[v] != visit_index[v]) continue;
                    scc_members.clearRetainingCapacity();
                    while (true) {
                        const w = scc_stack.pop() orelse
                            Common.invariant("Monotype digest SCC stack underflowed");
                        on_stack[w] = false;
                        try scc_members.append(self.gpa, w);
                        if (w == v) break;
                    }
                    try self.resolveScc(scc_members.items);
                }
            }
        }

        fn resolveScc(self: *DigestEngine, members: []const u32) std.mem.Allocator.Error!void {
            if (members.len == 1 and !self.hasSelfEdge(members[0])) {
                try self.resolveAcyclicNode(members[0]);
                return;
            }
            try self.resolveCyclicScc(members);
        }

        fn hasSelfEdge(self: *const DigestEngine, index: u32) bool {
            for (self.linksOf(index)) |ref| {
                switch (ref) {
                    .external => {},
                    .node => |target| if (target == index) return true,
                }
            }
            return false;
        }

        fn resolveAcyclicNode(self: *DigestEngine, index: u32) std.mem.Allocator.Error!void {
            self.render_buf.clearRetainingCapacity();
            const sink = RenderSink{ .engine = self, .out = &self.render_buf, .scc = null };
            const node = self.nodes.items[index];
            try self.store.encodeTypeNode(self.name_store, sink, node.ty, node.mode);
            var digest: names.TypeDigest = .{ .bytes = sha256Of(self.render_buf.items) };
            // An acyclic store node whose one-step rendering matches a known
            // recursive-group unfolding is a rolled-out prefix of the same
            // infinite type: same label, and children digest-equal to the
            // member's children. It adopts the member's digest so knot depth
            // cannot influence identity.
            if (self.store.recursive_digest_unfoldings.get(digest.bytes)) |member_digest| {
                digest = member_digest;
            }
            self.finalizeNode(index, digest);
        }

        fn finalizeNode(self: *DigestEngine, index: u32, digest: names.TypeDigest) void {
            const node = &self.nodes.items[index];
            std.debug.assert(!node.resolved);
            node.digest = digest;
            node.resolved = true;
            self.store.setCachedDigest(node.ty, node.mode, digest);
        }

        /// Reduce one cyclic SCC by bisimulation refinement, linearize the
        /// reduced graph with the minimum-over-entry-points rotation, and
        /// digest every member as its reduced position.
        fn resolveCyclicScc(self: *DigestEngine, members: []const u32) std.mem.Allocator.Error!void {
            const member_count = members.len;
            const member_pos_of_node = try self.gpa.alloc(u32, self.nodes.items.len);
            defer self.gpa.free(member_pos_of_node);
            @memset(member_pos_of_node, no_scc_position);
            for (members, 0..) |node_index, pos| {
                member_pos_of_node[node_index] = @intCast(pos);
            }

            // Refine to the stable bisimulation partition. The initial
            // partition renders every member with its finalized out-of-SCC
            // child digests—computed now, not at discovery, so whether a
            // child arrived cached or was resolved in this run cannot split
            // bisimilar members. Refinement then splits by the partition
            // identities reached by every ordered in-SCC reference.
            var blocks = try self.gpa.alloc(u32, member_count);
            defer self.gpa.free(blocks);
            var next_blocks = try self.gpa.alloc(u32, member_count);
            defer self.gpa.free(next_blocks);
            var block_count: u32 = 0;
            {
                var by_label = std.AutoHashMap([32]u8, u32).init(self.gpa);
                defer by_label.deinit();
                for (members, 0..) |node_index, pos| {
                    var hasher = std.crypto.hash.sha2.Sha256.init(.{});
                    const sink = SccLabelSink{
                        .engine = self,
                        .hasher = &hasher,
                        .member_pos_of_node = member_pos_of_node,
                    };
                    const node = self.nodes.items[node_index];
                    try self.store.encodeTypeNode(self.name_store, sink, node.ty, node.mode);
                    const gop = try by_label.getOrPut(hasher.finalResult());
                    if (!gop.found_existing) {
                        gop.value_ptr.* = block_count;
                        block_count += 1;
                    }
                    blocks[pos] = gop.value_ptr.*;
                }
            }
            while (true) {
                var by_signature = std.AutoHashMap([32]u8, u32).init(self.gpa);
                defer by_signature.deinit();
                var next_count: u32 = 0;
                for (members, 0..) |node_index, pos| {
                    var hasher = std.crypto.hash.sha2.Sha256.init(.{});
                    hashU32(&hasher, blocks[pos]);
                    for (self.linksOf(node_index)) |ref| {
                        const target = switch (ref) {
                            .external => continue,
                            .node => |node_target| node_target,
                        };
                        const target_pos = member_pos_of_node[target];
                        if (target_pos == no_scc_position) continue;
                        hashU32(&hasher, blocks[target_pos]);
                    }
                    const gop = try by_signature.getOrPut(hasher.finalResult());
                    if (!gop.found_existing) {
                        gop.value_ptr.* = next_count;
                        next_count += 1;
                    }
                    next_blocks[pos] = gop.value_ptr.*;
                }
                const stable = next_count == block_count;
                std.mem.swap([]u32, &blocks, &next_blocks);
                block_count = next_count;
                if (stable) break;
            }

            // Quotient graph: one representative and its ordered in-SCC block
            // edges per block. The partition is stable, so any member
            // represents its block.
            const block_rep = try self.gpa.alloc(u32, block_count);
            defer self.gpa.free(block_rep);
            {
                const filled = try self.gpa.alloc(bool, block_count);
                defer self.gpa.free(filled);
                @memset(filled, false);
                for (members, 0..) |node_index, pos| {
                    const block = blocks[pos];
                    if (!filled[block]) {
                        filled[block] = true;
                        block_rep[block] = node_index;
                    }
                }
            }
            var block_edges_pool: std.ArrayList(u32) = .empty;
            defer block_edges_pool.deinit(self.gpa);
            const block_edge_start = try self.gpa.alloc(u32, block_count);
            defer self.gpa.free(block_edge_start);
            const block_edge_len = try self.gpa.alloc(u32, block_count);
            defer self.gpa.free(block_edge_len);
            for (0..block_count) |block| {
                const start: u32 = @intCast(block_edges_pool.items.len);
                for (self.linksOf(block_rep[block])) |ref| {
                    const target = switch (ref) {
                        .external => continue,
                        .node => |node_target| node_target,
                    };
                    const target_pos = member_pos_of_node[target];
                    if (target_pos == no_scc_position) continue;
                    try block_edges_pool.append(self.gpa, blocks[target_pos]);
                }
                block_edge_start[block] = start;
                block_edge_len[block] = @intCast(block_edges_pool.items.len - start);
            }

            // Deterministic order: preorder DFS from every entry block,
            // keeping the lexicographically smallest rendering. The reduced
            // graph has no two equivalent positions, so the minimum is
            // unique.
            const order_of_block = try self.gpa.alloc(u32, block_count);
            defer self.gpa.free(order_of_block);
            const best_order = try self.gpa.alloc(u32, block_count);
            defer self.gpa.free(best_order);
            const DfsFrame = struct { block: u32, edge_cursor: u32 };
            var dfs: std.ArrayList(DfsFrame) = .empty;
            defer dfs.deinit(self.gpa);
            var candidate_order: std.ArrayList(u32) = .empty;
            defer candidate_order.deinit(self.gpa);
            var candidate_buf: std.ArrayList(u8) = .empty;
            defer candidate_buf.deinit(self.gpa);
            var best_buf: std.ArrayList(u8) = .empty;
            defer best_buf.deinit(self.gpa);
            var have_best = false;

            for (0..block_count) |entry| {
                @memset(order_of_block, no_scc_position);
                candidate_order.clearRetainingCapacity();
                dfs.clearRetainingCapacity();
                order_of_block[entry] = 0;
                try candidate_order.append(self.gpa, @intCast(entry));
                try dfs.append(self.gpa, .{ .block = @intCast(entry), .edge_cursor = 0 });
                while (dfs.items.len > 0) {
                    const frame = &dfs.items[dfs.items.len - 1];
                    const edges = block_edges_pool.items[block_edge_start[frame.block]..][0..block_edge_len[frame.block]];
                    if (frame.edge_cursor >= edges.len) {
                        _ = dfs.pop();
                        continue;
                    }
                    const target = edges[frame.edge_cursor];
                    frame.edge_cursor += 1;
                    if (order_of_block[target] != no_scc_position) continue;
                    order_of_block[target] = @intCast(candidate_order.items.len);
                    try candidate_order.append(self.gpa, target);
                    try dfs.append(self.gpa, .{ .block = target, .edge_cursor = 0 });
                }
                if (candidate_order.items.len != block_count) {
                    Common.invariant("Monotype digest SCC quotient was not strongly connected");
                }

                candidate_buf.clearRetainingCapacity();
                const ctx = SccRenderContext{
                    .member_pos_of_node = member_pos_of_node,
                    .blocks = blocks,
                    .order_of_block = order_of_block,
                };
                const sink = RenderSink{ .engine = self, .out = &candidate_buf, .scc = &ctx };
                for (candidate_order.items) |block| {
                    const rep = self.nodes.items[block_rep[block]];
                    try self.store.encodeTypeNode(self.name_store, sink, rep.ty, rep.mode);
                }

                if (have_best) {
                    // Two entry points rendering identical bytes would be
                    // bisimilar, contradicting the reduced partition.
                    std.debug.assert(!std.mem.eql(u8, candidate_buf.items, best_buf.items));
                }
                if (!have_best or std.mem.order(u8, candidate_buf.items, best_buf.items) == .lt) {
                    std.mem.swap(std.ArrayList(u8), &candidate_buf, &best_buf);
                    @memcpy(best_order, order_of_block);
                    have_best = true;
                }
            }

            // Every reduced position digests as its index in the minimum
            // group encoding; every original member adopts its position's
            // digest.
            const block_digest = try self.gpa.alloc(names.TypeDigest, block_count);
            defer self.gpa.free(block_digest);
            for (0..block_count) |block| {
                var hasher = std.crypto.hash.sha2.Sha256.init(.{});
                hashBytes(&hasher, "recursive-member");
                hashU32(&hasher, best_order[block]);
                hashBytes(&hasher, best_buf.items);
                block_digest[block] = .{ .bytes = hasher.finalResult() };
            }
            for (members, 0..) |node_index, pos| {
                self.finalizeNode(node_index, block_digest[blocks[pos]]);
            }

            // Record each reduced position's one-step unfolding in the
            // store's unfolding index so later rolled-out prefixes of this
            // group fold to the same digests. Members are finalized, so a
            // plain rendering resolves in-SCC children to their new group
            // digests.
            for (0..block_count) |block| {
                self.render_buf.clearRetainingCapacity();
                const sink = RenderSink{ .engine = self, .out = &self.render_buf, .scc = null };
                const rep = self.nodes.items[block_rep[block]];
                try self.store.encodeTypeNode(self.name_store, sink, rep.ty, rep.mode);
                const unfolding = sha256Of(self.render_buf.items);
                const gop = try self.store.recursive_digest_unfoldings.getOrPut(unfolding);
                if (gop.found_existing) {
                    // Reduced-group digests are intrinsic, so an equivalent
                    // group digested earlier must agree.
                    std.debug.assert(std.mem.eql(u8, &gop.value_ptr.bytes, &block_digest[block].bytes));
                } else {
                    gop.value_ptr.* = block_digest[block];
                }
            }
        }
    };
};

/// How `typeMatches` compares its two sides.
pub const TypeMatchMode = enum {
    /// Both sides must describe the same type everywhere.
    exact,
    /// As `exact`, except that an empty tag union on the left accepts any right
    /// type. An unresolved checked type variable lowers to an empty tag union,
    /// so this compares a generic declaration against one of its
    /// instantiations: every position the declaration made concrete still has
    /// to match, and only its variable slots are open. A left side that is
    /// concretely uninhabited is indistinguishable from a variable slot here,
    /// so callers use this mode only for a declaration known to be generic.
    declared_variable_slots_match_any,
};

fn typeViewEql(
    type_view: anytype,
    allocator: std.mem.Allocator,
    name_store: *const names.NameStore,
    lhs: TypeId,
    rhs: TypeId,
    mode: TypeMatchMode,
) std.mem.Allocator.Error!bool {
    var visited = std.AutoHashMap(u64, void).init(allocator);
    defer visited.deinit();
    return try typeViewEqlInner(type_view, name_store, lhs, rhs, &visited, mode);
}

fn typeViewEqlInner(
    type_view: anytype,
    name_store: *const names.NameStore,
    raw_lhs: TypeId,
    raw_rhs: TypeId,
    visited: *std.AutoHashMap(u64, void),
    mode: TypeMatchMode,
) std.mem.Allocator.Error!bool {
    if (raw_lhs == raw_rhs) return true;

    const lhs_content = type_view.get(raw_lhs);
    if (lhs_content == .named and lhs_content.named.kind == .alias) {
        if (lhs_content.named.backing) |backing| {
            return try typeViewEqlInner(type_view, name_store, backing.ty, raw_rhs, visited, mode);
        }
    }

    const rhs_content = type_view.get(raw_rhs);
    if (rhs_content == .named and rhs_content.named.kind == .alias) {
        if (rhs_content.named.backing) |backing| {
            return try typeViewEqlInner(type_view, name_store, raw_lhs, backing.ty, visited, mode);
        }
    }

    if (mode == .declared_variable_slots_match_any and
        lhs_content == .tag_union and
        type_view.tagSpan(lhs_content.tag_union).len == 0)
    {
        return true;
    }

    // An asymmetric mode reaches the same pair from both directions with
    // different meanings, so it keeps the two orderings apart.
    const pair = switch (mode) {
        .exact => typePairKey(raw_lhs, raw_rhs),
        .declared_variable_slots_match_any => orderedTypePairKey(raw_lhs, raw_rhs),
    };
    const gop = try visited.getOrPut(pair);
    if (gop.found_existing) return true;

    if (std.meta.activeTag(lhs_content) != std.meta.activeTag(rhs_content)) return false;

    return switch (lhs_content) {
        .primitive => |lhs| lhs == rhs_content.primitive,
        .named => |lhs| try namedTypeViewEql(type_view, name_store, lhs, rhs_content.named, visited, mode),
        .record => |lhs| try fieldSpanViewEql(type_view, name_store, lhs, rhs_content.record, visited, mode),
        .tuple => |lhs| try typeSpanViewEql(type_view, name_store, lhs, rhs_content.tuple, visited, mode),
        .tag_union => |lhs| try tagSpanViewEql(type_view, name_store, lhs, rhs_content.tag_union, visited, mode),
        .list => |lhs| try typeViewEqlInner(type_view, name_store, lhs, rhs_content.list, visited, mode),
        .box => |lhs| try typeViewEqlInner(type_view, name_store, lhs, rhs_content.box, visited, mode),
        .func => |lhs| blk: {
            const rhs = rhs_content.func;
            if (!try typeSpanViewEql(type_view, name_store, lhs.args, rhs.args, visited, mode)) break :blk false;
            break :blk try typeViewEqlInner(type_view, name_store, lhs.ret, rhs.ret, visited, mode);
        },
        .erased => |lhs| std.mem.eql(u8, lhs.bytes[0..], rhs_content.erased.bytes[0..]),
        .zst => true,
    };
}

fn namedTypeViewEql(
    type_view: anytype,
    name_store: *const names.NameStore,
    lhs: anytype,
    rhs: anytype,
    visited: *std.AutoHashMap(u64, void),
    mode: TypeMatchMode,
) std.mem.Allocator.Error!bool {
    if (lhs.kind != rhs.kind) return false;
    if (!std.mem.eql(u8, lhs.named_type.module.bytes[0..], rhs.named_type.module.bytes[0..])) return false;
    if (!std.mem.eql(u8, name_store.moduleIdentityBytes(lhs.def.module), name_store.moduleIdentityBytes(rhs.def.module))) return false;
    if (lhs.def.source_decl != rhs.def.source_decl) return false;
    if (lhs.def.source_decl == null and
        !std.mem.eql(u8, name_store.typeNameText(lhs.def.type_name), name_store.typeNameText(rhs.def.type_name)))
    {
        return false;
    }
    if (!optionalDigestEql(lhs.def.generated, rhs.def.generated)) return false;
    if (lhs.def.iterator_representation != rhs.def.iterator_representation) return false;
    if (lhs.def.iterator_kind != rhs.def.iterator_kind) return false;
    if (lhs.def.iterator_depth != rhs.def.iterator_depth) return false;
    if (!std.meta.eql(lhs.def.iterator_topology, rhs.def.iterator_topology)) return false;
    if (lhs.builtin_owner != rhs.builtin_owner) return false;
    if (!try typeSpanViewEql(type_view, name_store, lhs.args, rhs.args, visited, mode)) return false;

    if (lhs.kind == .alias) {
        const lhs_backing = lhs.backing orelse return rhs.backing == null;
        const rhs_backing = rhs.backing orelse return false;
        return try typeViewEqlInner(type_view, name_store, lhs_backing.ty, rhs_backing.ty, visited, mode);
    }

    if (specializationUsesBacking(lhs.backing) or specializationUsesBacking(rhs.backing)) {
        const lhs_backing = lhs.backing orelse return false;
        const rhs_backing = rhs.backing orelse return false;
        if (lhs_backing.use != rhs_backing.use or lhs_backing.authority != rhs_backing.authority) return false;
        return try typeViewEqlInner(type_view, name_store, lhs_backing.ty, rhs_backing.ty, visited, mode);
    }

    return true;
}

fn typeSpanViewEql(
    type_view: anytype,
    name_store: *const names.NameStore,
    lhs_span: Span,
    rhs_span: Span,
    visited: *std.AutoHashMap(u64, void),
    mode: TypeMatchMode,
) std.mem.Allocator.Error!bool {
    const lhs = type_view.span(lhs_span);
    const rhs = type_view.span(rhs_span);
    if (lhs.len != rhs.len) return false;
    for (lhs, rhs) |lhs_ty, rhs_ty| {
        if (!try typeViewEqlInner(type_view, name_store, lhs_ty, rhs_ty, visited, mode)) return false;
    }
    return true;
}

fn fieldDefaultEql(name_store: *const names.NameStore, lhs: ?FieldDefault, rhs: ?FieldDefault) bool {
    const lhs_default = lhs orelse return rhs == null;
    const rhs_default = rhs orelse return false;
    if (lhs_default.expr_node != rhs_default.expr_node) return false;
    return std.mem.eql(u8, name_store.moduleIdentityBytes(lhs_default.module), name_store.moduleIdentityBytes(rhs_default.module));
}

/// Fold one record field's `??` default identity (or its absence) into a
/// type digest; shared by the Monotype and lambda-mono digest writers so
/// rows disagreeing about defaults digest differently at every stage.
pub fn writeFieldDefaultDigest(name_store: *const names.NameStore, hasher: *std.crypto.hash.sha2.Sha256, default: ?FieldDefault) void {
    if (default) |field_default| {
        writeBytes(hasher, "field-default");
        writeBytes(hasher, name_store.moduleIdentityBytes(field_default.module));
        writeU32(hasher, field_default.expr_node);
    } else {
        writeBytes(hasher, "field-no-default");
    }
}

fn fieldSpanViewEql(
    type_view: anytype,
    name_store: *const names.NameStore,
    lhs_span: Span,
    rhs_span: Span,
    visited: *std.AutoHashMap(u64, void),
    mode: TypeMatchMode,
) std.mem.Allocator.Error!bool {
    const lhs = type_view.fieldSpan(lhs_span);
    const rhs = type_view.fieldSpan(rhs_span);
    if (lhs.len != rhs.len) return false;
    for (lhs, rhs) |lhs_field, rhs_field| {
        if (!std.mem.eql(u8, name_store.recordFieldLabelText(lhs_field.name), name_store.recordFieldLabelText(rhs_field.name))) return false;
        if (!fieldDefaultEql(name_store, lhs_field.default, rhs_field.default)) return false;
        if (lhs_field.kind_state != rhs_field.kind_state) return false;
        if ((lhs_field.value_ty == null) != (rhs_field.value_ty == null)) return false;
        if (lhs_field.value_ty) |lhs_value_ty| {
            if (!try typeViewEqlInner(type_view, name_store, lhs_value_ty, rhs_field.value_ty.?, visited, mode)) return false;
        }
        if (!try typeViewEqlInner(type_view, name_store, lhs_field.ty, rhs_field.ty, visited, mode)) return false;
    }
    return true;
}

fn tagSpanViewEql(
    type_view: anytype,
    name_store: *const names.NameStore,
    lhs_span: Span,
    rhs_span: Span,
    visited: *std.AutoHashMap(u64, void),
    mode: TypeMatchMode,
) std.mem.Allocator.Error!bool {
    const lhs = type_view.tagSpan(lhs_span);
    const rhs = type_view.tagSpan(rhs_span);
    if (lhs.len != rhs.len) return false;
    for (lhs, rhs) |lhs_tag, rhs_tag| {
        if (!std.mem.eql(u8, name_store.tagLabelText(lhs_tag.name), name_store.tagLabelText(rhs_tag.name))) return false;
        if (!try typeSpanViewEql(type_view, name_store, lhs_tag.payloads, rhs_tag.payloads, visited, mode)) return false;
    }
    return true;
}

/// Order-preserving pair key, for a match mode whose two sides mean different
/// things.
fn orderedTypePairKey(lhs: TypeId, rhs: TypeId) u64 {
    return (@as(u64, @intFromEnum(lhs)) << 32) | @as(u64, @intFromEnum(rhs));
}

fn typePairKey(lhs: TypeId, rhs: TypeId) u64 {
    const lhs_int = @intFromEnum(lhs);
    const rhs_int = @intFromEnum(rhs);
    const low = @min(lhs_int, rhs_int);
    const high = @max(lhs_int, rhs_int);
    return (@as(u64, low) << 32) | @as(u64, high);
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
            if (field.value_ty) |value_ty| {
                if (!self.typeRefInBounds(value_ty)) return .type_ref_out_of_bounds;
            }
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
            if (field.value_ty) |value_ty| {
                if (!self.typeRefInBounds(value_ty)) return .type_ref_out_of_bounds;
            }
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
    var visited = std.AutoHashMap(u64, void).init(allocator);
    defer visited.deinit();
    return try typeEqlAcrossStoresInner(name_store, lhs_view, lhs, rhs_view, rhs, &visited);
}

fn typeEqlAcrossStoresInner(
    name_store: *const names.NameStore,
    lhs_view: anytype,
    raw_lhs: TypeId,
    rhs_view: anytype,
    raw_rhs: TypeId,
    visited: *std.AutoHashMap(u64, void),
) std.mem.Allocator.Error!bool {
    const lhs_content = lhs_view.get(raw_lhs);
    if (lhs_content == .named and lhs_content.named.kind == .alias) {
        if (lhs_content.named.backing) |backing| {
            return try typeEqlAcrossStoresInner(name_store, lhs_view, backing.ty, rhs_view, raw_rhs, visited);
        }
    }

    const rhs_content = rhs_view.get(raw_rhs);
    if (rhs_content == .named and rhs_content.named.kind == .alias) {
        if (rhs_content.named.backing) |backing| {
            return try typeEqlAcrossStoresInner(name_store, lhs_view, raw_lhs, rhs_view, backing.ty, visited);
        }
    }

    const pair = directionalTypePair(raw_lhs, raw_rhs);
    const gop = try visited.getOrPut(pair);
    if (gop.found_existing) return true;

    if (std.meta.activeTag(lhs_content) != std.meta.activeTag(rhs_content)) return false;

    return switch (lhs_content) {
        .primitive => |lhs| lhs == rhs_content.primitive,
        .named => |lhs| try namedTypeEqlAcrossStores(name_store, lhs_view, lhs, rhs_view, rhs_content.named, visited),
        .record => |lhs| try fieldSpanEqlAcrossStores(name_store, lhs_view, lhs, rhs_view, rhs_content.record, visited),
        .tuple => |lhs| try typeSpanEqlAcrossStores(name_store, lhs_view, lhs, rhs_view, rhs_content.tuple, visited),
        .tag_union => |lhs| try tagSpanEqlAcrossStores(name_store, lhs_view, lhs, rhs_view, rhs_content.tag_union, visited),
        .list => |lhs| try typeEqlAcrossStoresInner(name_store, lhs_view, lhs, rhs_view, rhs_content.list, visited),
        .box => |lhs| try typeEqlAcrossStoresInner(name_store, lhs_view, lhs, rhs_view, rhs_content.box, visited),
        .func => |lhs_func| blk: {
            const rhs_func = rhs_content.func;
            if (!try typeSpanEqlAcrossStores(name_store, lhs_view, lhs_func.args, rhs_view, rhs_func.args, visited)) break :blk false;
            break :blk try typeEqlAcrossStoresInner(name_store, lhs_view, lhs_func.ret, rhs_view, rhs_func.ret, visited);
        },
        .erased => |lhs| std.mem.eql(u8, lhs.bytes[0..], rhs_content.erased.bytes[0..]),
        .zst => true,
    };
}

fn namedTypeEqlAcrossStores(
    name_store: *const names.NameStore,
    lhs_view: anytype,
    lhs: NamedContent,
    rhs_view: anytype,
    rhs: NamedContent,
    visited: *std.AutoHashMap(u64, void),
) std.mem.Allocator.Error!bool {
    if (lhs.kind != rhs.kind) return false;
    if (!std.mem.eql(u8, lhs.named_type.module.bytes[0..], rhs.named_type.module.bytes[0..])) return false;
    if (!std.mem.eql(u8, name_store.moduleIdentityBytes(lhs.def.module), name_store.moduleIdentityBytes(rhs.def.module))) return false;
    if (lhs.def.source_decl != rhs.def.source_decl) return false;
    if (lhs.def.source_decl == null and
        !std.mem.eql(u8, name_store.typeNameText(lhs.def.type_name), name_store.typeNameText(rhs.def.type_name)))
    {
        return false;
    }
    if (!optionalDigestEql(lhs.def.generated, rhs.def.generated)) return false;
    if (lhs.def.iterator_representation != rhs.def.iterator_representation) return false;
    if (lhs.def.iterator_kind != rhs.def.iterator_kind) return false;
    if (lhs.def.iterator_depth != rhs.def.iterator_depth) return false;
    if (!std.meta.eql(lhs.def.iterator_topology, rhs.def.iterator_topology)) return false;
    if (lhs.builtin_owner != rhs.builtin_owner) return false;
    if (!try typeSpanEqlAcrossStores(name_store, lhs_view, lhs.args, rhs_view, rhs.args, visited)) return false;

    if (lhs.kind == .alias) {
        const lhs_backing = lhs.backing orelse return rhs.backing == null;
        const rhs_backing = rhs.backing orelse return false;
        return try typeEqlAcrossStoresInner(name_store, lhs_view, lhs_backing.ty, rhs_view, rhs_backing.ty, visited);
    }

    if (specializationUsesBacking(lhs.backing) or specializationUsesBacking(rhs.backing)) {
        const lhs_backing = lhs.backing orelse return false;
        const rhs_backing = rhs.backing orelse return false;
        if (lhs_backing.use != rhs_backing.use or lhs_backing.authority != rhs_backing.authority) return false;
        return try typeEqlAcrossStoresInner(name_store, lhs_view, lhs_backing.ty, rhs_view, rhs_backing.ty, visited);
    }

    return true;
}

fn typeSpanEqlAcrossStores(
    name_store: *const names.NameStore,
    lhs_view: anytype,
    lhs_span: Span,
    rhs_view: anytype,
    rhs_span: Span,
    visited: *std.AutoHashMap(u64, void),
) std.mem.Allocator.Error!bool {
    const lhs = lhs_view.span(lhs_span);
    const rhs = rhs_view.span(rhs_span);
    if (lhs.len != rhs.len) return false;
    for (lhs, rhs) |lhs_ty, rhs_ty| {
        if (!try typeEqlAcrossStoresInner(name_store, lhs_view, lhs_ty, rhs_view, rhs_ty, visited)) return false;
    }
    return true;
}

fn fieldSpanEqlAcrossStores(
    name_store: *const names.NameStore,
    lhs_view: anytype,
    lhs_span: Span,
    rhs_view: anytype,
    rhs_span: Span,
    visited: *std.AutoHashMap(u64, void),
) std.mem.Allocator.Error!bool {
    const lhs = lhs_view.fieldSpan(lhs_span);
    const rhs = rhs_view.fieldSpan(rhs_span);
    if (lhs.len != rhs.len) return false;
    for (lhs, rhs) |lhs_field, rhs_field| {
        if (!std.mem.eql(u8, name_store.recordFieldLabelText(lhs_field.name), name_store.recordFieldLabelText(rhs_field.name))) return false;
        if (!fieldDefaultEql(name_store, lhs_field.default, rhs_field.default)) return false;
        if (lhs_field.kind_state != rhs_field.kind_state) return false;
        if ((lhs_field.value_ty == null) != (rhs_field.value_ty == null)) return false;
        if (lhs_field.value_ty) |lhs_value_ty| {
            if (!try typeEqlAcrossStoresInner(name_store, lhs_view, lhs_value_ty, rhs_view, rhs_field.value_ty.?, visited)) return false;
        }
        if (!try typeEqlAcrossStoresInner(name_store, lhs_view, lhs_field.ty, rhs_view, rhs_field.ty, visited)) return false;
    }
    return true;
}

fn tagSpanEqlAcrossStores(
    name_store: *const names.NameStore,
    lhs_view: anytype,
    lhs_span: Span,
    rhs_view: anytype,
    rhs_span: Span,
    visited: *std.AutoHashMap(u64, void),
) std.mem.Allocator.Error!bool {
    const lhs = lhs_view.tagSpan(lhs_span);
    const rhs = rhs_view.tagSpan(rhs_span);
    if (lhs.len != rhs.len) return false;
    for (lhs, rhs) |lhs_tag, rhs_tag| {
        if (!std.mem.eql(u8, name_store.tagLabelText(lhs_tag.name), name_store.tagLabelText(rhs_tag.name))) return false;
        if (!try typeSpanEqlAcrossStores(name_store, lhs_view, lhs_tag.payloads, rhs_view, rhs_tag.payloads, visited)) return false;
    }
    return true;
}

fn directionalTypePair(lhs: TypeId, rhs: TypeId) u64 {
    return (@as(u64, @intFromEnum(lhs)) << 32) | @as(u64, @intFromEnum(rhs));
}

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

/// Digest queries back deep comparison predicates whose call chains carry no
/// error path, so allocation failure while building digest traversal state
/// stops the build with a defined abort in every build mode rather than
/// threading `OutOfMemory` through every type comparison.
fn digestOutOfMemory() noreturn {
    std.debug.panic("out of memory while digesting a Monotype type", .{});
}

fn writeBytes(hasher: *std.crypto.hash.sha2.Sha256, bytes: []const u8) void {
    writeU32(hasher, @intCast(bytes.len));
    hasher.update(bytes);
}

fn specializationUsesBacking(backing: ?NamedBacking) bool {
    return if (backing) |present| present.authority == .generated_private else false;
}

fn writeU32(hasher: *std.crypto.hash.sha2.Sha256, value: u32) void {
    const little = std.mem.nativeToLittle(u32, value);
    hasher.update(std.mem.asBytes(&little));
}

// Digest sinks define `writeBytes`/`writeU32` methods, which shadow the
// file-scope hasher helpers inside their bodies; these aliases keep the
// helpers reachable there.
const hashBytes = writeBytes;
const hashU32 = writeU32;

fn sha256Of(bytes: []const u8) [32]u8 {
    var hasher = std.crypto.hash.sha2.Sha256.init(.{});
    hasher.update(bytes);
    return hasher.finalResult();
}

fn renderU32(gpa: std.mem.Allocator, out: *std.ArrayList(u8), value: u32) std.mem.Allocator.Error!void {
    const little = std.mem.nativeToLittle(u32, value);
    try out.appendSlice(gpa, std.mem.asBytes(&little));
}

fn renderBytes(gpa: std.mem.Allocator, out: *std.ArrayList(u8), bytes: []const u8) std.mem.Allocator.Error!void {
    try renderU32(gpa, out, @intCast(bytes.len));
    try out.appendSlice(gpa, bytes);
}

fn sinkOptionalU32(sink: anytype, value: ?u32) std.mem.Allocator.Error!void {
    if (value) |v| {
        try sink.writeBytes("some-u32");
        try sink.writeU32(v);
    } else {
        try sink.writeBytes("no-u32");
    }
}

fn sinkOptionalDigest(sink: anytype, value: ?names.TypeDigest) std.mem.Allocator.Error!void {
    if (value) |digest| {
        try sink.writeBytes("digest");
        try sink.writeBytes(&digest.bytes);
    } else {
        try sink.writeBytes("no-digest");
    }
}

fn sinkIteratorTopology(
    sink: anytype,
    name_store: *const names.NameStore,
    topology: ?IteratorTopology,
) std.mem.Allocator.Error!void {
    const value = topology orelse {
        try sink.writeBytes("no-iterator-topology");
        return;
    };
    try sink.writeBytes("iterator-topology");
    try sink.writeBytes(name_store.recordFieldLabelText(value.len_field));
    try sink.writeBytes(name_store.recordFieldLabelText(value.step_field));
    try sink.writeBytes(name_store.tagLabelText(value.known_tag));
    try sink.writeBytes(name_store.tagLabelText(value.unknown_tag));
    try sink.writeBytes(name_store.tagLabelText(value.done_tag));
    try sink.writeBytes(name_store.tagLabelText(value.one_tag));
    try sink.writeBytes(name_store.tagLabelText(value.skip_tag));
    try sink.writeBytes(name_store.recordFieldLabelText(value.item_field));
    try sink.writeBytes(name_store.recordFieldLabelText(value.rest_field));
}

/// A named backing is a stored type identity, never an interface, so its
/// child always digests in full mode regardless of the enclosing mode.
fn encodeNamedBacking(sink: anytype, backing: ?NamedBacking) std.mem.Allocator.Error!void {
    try sink.writeBytes("backing");
    if (backing) |named_backing| {
        try sink.writeBytes(@tagName(named_backing.use));
        try sink.writeBytes(@tagName(named_backing.authority));
        try sink.child(named_backing.ty, .full);
    } else {
        try sink.writeBytes("none");
    }
}

fn sinkFieldDefault(sink: anytype, name_store: *const names.NameStore, default: ?FieldDefault) std.mem.Allocator.Error!void {
    if (default) |field_default| {
        try sink.writeBytes("field-default");
        try sink.writeBytes(name_store.moduleIdentityBytes(field_default.module));
        try sink.writeU32(field_default.expr_node);
    } else {
        try sink.writeBytes("field-no-default");
    }
}

fn optionalDigestEql(lhs: ?names.TypeDigest, rhs: ?names.TypeDigest) bool {
    if (lhs) |lhs_digest| {
        const rhs_digest = rhs orelse return false;
        return std.mem.eql(u8, lhs_digest.bytes[0..], rhs_digest.bytes[0..]);
    }
    return rhs == null;
}

test "monotype type declarations are referenced" {
    std.testing.refAllDecls(@This());
}

test "monotype type store acyclic interning reuses child-first function nodes" {
    var name_store = names.NameStore.init(std.testing.allocator);
    defer name_store.deinit();

    var store = Store.init(std.testing.allocator);
    defer store.deinit();

    const unit = try store.internZst(&name_store);
    const first = try store.internFunc(&name_store, &.{unit}, unit);
    const second = try store.internFunc(&name_store, &.{unit}, unit);

    try std.testing.expectEqual(first, second);
    try std.testing.expectEqual(@as(usize, 2), store.view().types.len);
    try std.testing.expect(store.view().type_digests[@intFromEnum(first)] != null);
    try std.testing.expect(store.specializationDigestsView()[@intFromEnum(first)] != null);
}

test "monotype type store recursive transaction interns equal SCC positions" {
    var name_store = names.NameStore.init(std.testing.allocator);
    defer name_store.deinit();
    var store = Store.init(std.testing.allocator);
    defer store.deinit();

    const transaction = store.beginTransaction();
    const first = try transaction.reserve(&store);
    const second = try transaction.reserve(&store);
    transaction.fill(&store, first, .{ .list = second });
    transaction.fill(&store, second, .{ .list = first });

    var result = try store.commitTransaction(&name_store, transaction, first);
    defer result.deinit();
    try std.testing.expectEqual(result.remap[0], result.remap[1]);
    try std.testing.expectEqual(result.root, result.remap[0]);
    try std.testing.expectEqual(@as(usize, 1), store.view().types.len);
    try std.testing.expect(store.view().type_digests[@intFromEnum(result.root)] != null);
    try std.testing.expect(store.specializationDigestsView()[@intFromEnum(result.root)] != null);
}

test "monotype type store recursive transaction indexes every representative" {
    var name_store = names.NameStore.init(std.testing.allocator);
    defer name_store.deinit();
    var store = Store.init(std.testing.allocator);
    defer store.deinit();

    const transaction = store.beginTransaction();
    const root = try transaction.reserve(&store);
    const child = try transaction.reserve(&store);
    transaction.fill(&store, root, .{ .box = child });
    transaction.fill(&store, child, .{ .list = root });
    var result = try store.commitTransaction(&name_store, transaction, root);
    defer result.deinit();

    const reinterned_child = try store.internList(&name_store, result.root);
    try std.testing.expectEqual(result.remap[1], reinterned_child);
}

test "monotype type store recursive transaction all-hit restores every suffix pool" {
    var name_store = names.NameStore.init(std.testing.allocator);
    defer name_store.deinit();
    var store = Store.init(std.testing.allocator);
    defer store.deinit();

    const first_transaction = store.beginTransaction();
    const first = try first_transaction.reserve(&store);
    const second = try first_transaction.reserve(&store);
    first_transaction.fill(&store, first, .{ .tuple = try store.addSpan(&.{second}) });
    first_transaction.fill(&store, second, .{ .tuple = try store.addSpan(&.{first}) });
    var first_result = try store.commitTransaction(&name_store, first_transaction, first);
    defer first_result.deinit();
    const types_len = store.types.len();
    const spans_len = store.spans.len();

    const hit_transaction = store.beginTransaction();
    const hit_first = try hit_transaction.reserve(&store);
    const hit_second = try hit_transaction.reserve(&store);
    hit_transaction.fill(&store, hit_first, .{ .tuple = try store.addSpan(&.{hit_second}) });
    hit_transaction.fill(&store, hit_second, .{ .tuple = try store.addSpan(&.{hit_first}) });
    var hit_result = try store.commitTransaction(&name_store, hit_transaction, hit_first);
    defer hit_result.deinit();

    try std.testing.expectEqual(first_result.root, hit_result.root);
    try std.testing.expectEqual(types_len, store.types.len());
    try std.testing.expectEqual(spans_len, store.spans.len());
}

test "monotype type store recursive transaction partial hit rewrites references" {
    var name_store = names.NameStore.init(std.testing.allocator);
    defer name_store.deinit();
    var store = Store.init(std.testing.allocator);
    defer store.deinit();

    const committed = try store.internZst(&name_store);
    const transaction = store.beginTransaction();
    const duplicate = try transaction.reserve(&store);
    const root = try transaction.reserve(&store);
    transaction.fill(&store, duplicate, .zst);
    transaction.fill(&store, root, .{ .tuple = try store.addSpan(&.{duplicate}) });
    var result = try store.commitTransaction(&name_store, transaction, root);
    defer result.deinit();

    try std.testing.expectEqual(committed, result.remap[0]);
    const children = store.span(store.get(result.root).tuple);
    try std.testing.expectEqual(committed, GuardedList.at(children, 0));
    try std.testing.expect(store.verify(&name_store) == null);
}

test "monotype type store defers acyclic interning to the active transaction" {
    var name_store = names.NameStore.init(std.testing.allocator);
    defer name_store.deinit();
    var store = Store.init(std.testing.allocator);
    defer store.deinit();

    const committed_unit = try store.internZst(&name_store);
    const transaction = store.beginTransaction();
    const duplicate_unit = try transaction.reserve(&store);
    transaction.fill(&store, duplicate_unit, .zst);

    // This candidate's original id shifts when the duplicate before it
    // collapses. Indexing it before the transaction seals would leave its
    // digest bucket pointing at the truncated id.
    const speculative_list = try store.internList(&name_store, duplicate_unit);
    var result = try store.commitTransaction(&name_store, transaction, speculative_list);
    defer result.deinit();
    const committed_list = result.remapType(speculative_list);
    try std.testing.expect(committed_list != speculative_list);

    const types_len = store.types.len();
    const reinterned_list = try store.internList(&name_store, committed_unit);
    try std.testing.expectEqual(committed_list, reinterned_list);
    try std.testing.expectEqual(types_len, store.types.len());
    try std.testing.expect(store.verify(&name_store) == null);
}

test "monotype type store recursive transaction seals atomically under allocation failure" {
    // Sealing either indexes the whole suffix or leaves the store exactly as
    // it found it. The dangerous middle state is a committed-but-unindexed (or
    // truncated-but-indexed) suffix, so every failure point is swept and the
    // store is inspected for surviving ids, side-pool rows, and bucket entries.
    var name_store = names.NameStore.init(std.testing.allocator);
    defer name_store.deinit();

    const Scenario = struct {
        const Built = struct { transaction: Store.Transaction, root: TypeId };

        /// A cycle, a speculative duplicate of a durable node, and side-pool
        /// rows: enough to exercise classification, capture, construction, and
        /// indexing in one seal.
        fn build(store: *Store, unit: TypeId) std.mem.Allocator.Error!Built {
            const transaction = store.beginTransaction();
            errdefer transaction.abort(store);

            const first = try transaction.reserve(store);
            const second = try transaction.reserve(store);
            const duplicate = try transaction.reserve(store);
            transaction.fill(store, duplicate, .zst);
            transaction.fill(store, first, .{ .tuple = try store.addSpan(&.{ second, duplicate, unit }) });
            transaction.fill(store, second, .{ .box = first });
            return .{ .transaction = transaction, .root = first };
        }
    };

    var failing = std.testing.FailingAllocator.init(std.testing.allocator, .{});
    var store = Store.init(failing.allocator());
    defer store.deinit();

    const unit = try store.internZst(&name_store);
    const baseline_types = store.types.len();
    const baseline_spans = store.spans.len();
    const baseline_fields = store.fields.len();
    const baseline_tags = store.tags.len();
    const baseline_declared = store.declared_fields.len();

    var budget: usize = 0;
    var sealed: ?Store.TransactionResult = null;
    while (sealed == null) : (budget += 1) {
        try std.testing.expect(budget < 1024);
        const built = try Scenario.build(&store, unit);
        failing.fail_index = failing.alloc_index + budget;
        sealed = store.commitTransaction(&name_store, built.transaction, built.root) catch |err| blk: {
            failing.fail_index = std.math.maxInt(usize);
            failing.has_induced_failure = false;
            try std.testing.expectEqual(error.OutOfMemory, err);
            try std.testing.expectEqual(baseline_types, store.types.len());
            try std.testing.expectEqual(baseline_spans, store.spans.len());
            try std.testing.expectEqual(baseline_fields, store.fields.len());
            try std.testing.expectEqual(baseline_tags, store.tags.len());
            try std.testing.expectEqual(baseline_declared, store.declared_fields.len());
            var buckets = store.full_digest_interned.valueIterator();
            while (buckets.next()) |bucket| {
                for (bucket.items) |indexed| {
                    try std.testing.expect(@intFromEnum(indexed) < baseline_types);
                }
            }
            try std.testing.expect(store.verify(&name_store) == null);
            break :blk null;
        };
        failing.fail_index = std.math.maxInt(usize);
    }

    // The seal that finally ran to completion must be fully indexed, not just
    // committed: re-interning an equal type has to find the representative.
    var result = sealed.?;
    defer result.deinit();
    try std.testing.expect(store.verify(&name_store) == null);
    try std.testing.expectEqual(unit, result.remapType(unit));
    try std.testing.expectEqual(unit, result.remapType(@enumFromInt(baseline_types + 2)));

    const committed_types = store.types.len();
    const reinterned = try store.internBox(&name_store, result.root);
    try std.testing.expectEqual(result.remapType(@enumFromInt(baseline_types + 1)), reinterned);
    try std.testing.expectEqual(committed_types, store.types.len());
}

test "monotype type store recursive transaction preserves captured side-pool rows" {
    // Sealing constructs representatives from a capture taken before the suffix
    // is truncated, so every side pool a node can reach -- fields, tags, tag
    // payloads, named arguments -- has to come back with its rows intact and
    // its references rewritten to durable ids.
    var name_store = names.NameStore.init(std.testing.allocator);
    defer name_store.deinit();
    var store = Store.init(std.testing.allocator);
    defer store.deinit();

    const module_identity = try name_store.internModuleIdentity(&([_]u8{0x5A} ** 32));
    const type_name = try name_store.internTypeName("Tree");
    const next_field = try name_store.internRecordFieldLabel("next");
    const more_tag = try name_store.internTagLabel("More");

    const unit = try store.internZst(&name_store);

    const transaction = store.beginTransaction();
    const record = try transaction.reserve(&store);
    const variants = try transaction.reserve(&store);
    const named = try transaction.reserve(&store);
    transaction.fill(&store, record, .{ .record = try store.addRecordFields(&name_store, &.{
        .{ .name = next_field, .ty = variants, .default = null },
    }) });
    transaction.fill(&store, variants, .{ .tag_union = try store.addTagVariants(&name_store, &.{
        .{ .name = more_tag, .checked_name = more_tag, .payloads = try store.addSpan(&.{ named, unit }) },
    }) });
    transaction.fill(&store, named, .{ .named = .{
        .named_type = .{ .module = .{}, .ty = @enumFromInt(1) },
        .def = .{ .module = module_identity, .type_name = type_name },
        .kind = .nominal,
        .args = try store.addSpan(&.{unit}),
        .backing = .{ .ty = record, .use = .inspectable },
        .declared_order = Span.empty(),
    } });

    var result = try store.commitTransaction(&name_store, transaction, record);
    defer result.deinit();
    try std.testing.expect(store.verify(&name_store) == null);

    const fields = store.fieldSpan(store.get(result.root).record);
    try std.testing.expectEqual(@as(usize, 1), GuardedList.borrowLen(fields));
    try std.testing.expectEqual(next_field, GuardedList.at(fields, 0).name);
    const union_ty = GuardedList.at(fields, 0).ty;
    try std.testing.expectEqual(result.remapType(variants), union_ty);

    const tags_ = store.tagSpan(store.get(union_ty).tag_union);
    try std.testing.expectEqual(more_tag, GuardedList.at(tags_, 0).name);
    const payloads = store.span(GuardedList.at(tags_, 0).payloads);
    try std.testing.expectEqual(@as(usize, 2), GuardedList.borrowLen(payloads));
    const named_ty = GuardedList.at(payloads, 0);
    try std.testing.expectEqual(result.remapType(named), named_ty);
    try std.testing.expectEqual(unit, GuardedList.at(payloads, 1));

    const stored_named = store.get(named_ty).named;
    try std.testing.expectEqual(result.root, stored_named.backing.?.ty);
    const args = store.span(stored_named.args);
    try std.testing.expectEqual(unit, GuardedList.at(args, 0));

    // Sealing an equal knot again must land on the same interned nodes.
    const committed_types_len = store.types.len();
    const committed_spans_len = store.spans.len();
    const committed_fields_len = store.fields.len();
    const committed_tags_len = store.tags.len();
    const committed_declared_len = store.declared_fields.len();
    const second = store.beginTransaction();
    const second_record = try second.reserve(&store);
    const second_variants = try second.reserve(&store);
    const second_named = try second.reserve(&store);
    second.fill(&store, second_record, .{ .record = try store.addRecordFields(&name_store, &.{
        .{ .name = next_field, .ty = second_variants, .default = null },
    }) });
    second.fill(&store, second_variants, .{ .tag_union = try store.addTagVariants(&name_store, &.{
        .{ .name = more_tag, .checked_name = more_tag, .payloads = try store.addSpan(&.{ second_named, unit }) },
    }) });
    second.fill(&store, second_named, .{ .named = .{
        .named_type = .{ .module = .{}, .ty = @enumFromInt(1) },
        .def = .{ .module = module_identity, .type_name = type_name },
        .kind = .nominal,
        .args = try store.addSpan(&.{unit}),
        .backing = .{ .ty = second_record, .use = .inspectable },
        .declared_order = Span.empty(),
    } });

    var second_result = try store.commitTransaction(&name_store, second, second_record);
    defer second_result.deinit();
    try std.testing.expectEqual(result.root, second_result.root);
    try std.testing.expectEqual(committed_types_len, store.types.len());
    try std.testing.expectEqual(committed_spans_len, store.spans.len());
    try std.testing.expectEqual(committed_fields_len, store.fields.len());
    try std.testing.expectEqual(committed_tags_len, store.tags.len());
    try std.testing.expectEqual(committed_declared_len, store.declared_fields.len());
}

test "monotype type store aborted transaction leaves no speculative state" {
    var name_store = names.NameStore.init(std.testing.allocator);
    defer name_store.deinit();
    var store = Store.init(std.testing.allocator);
    defer store.deinit();

    const unit = try store.internZst(&name_store);
    const types_len = store.types.len();
    const spans_len = store.spans.len();

    const transaction = store.beginTransaction();
    const reserved = try transaction.reserve(&store);
    transaction.fill(&store, reserved, .{ .tuple = try store.addSpan(&.{unit}) });
    transaction.abort(&store);

    try std.testing.expectEqual(types_len, store.types.len());
    try std.testing.expectEqual(spans_len, store.spans.len());
    // Aborting twice is the shape `errdefer transaction.abort(store)` produces
    // when a failed commit already rolled back.
    transaction.abort(&store);
    try std.testing.expectEqual(types_len, store.types.len());
    try std.testing.expect(store.verify(&name_store) == null);
}

test "monotype type store restores keep durable iterator containment answers" {
    // `restore` only wipes retained containment answers when the mark saw an
    // unfinished slot; interning hits and transaction seals restore to marks
    // with none, so answers cached for durable types must survive both.
    var name_store = names.NameStore.init(std.testing.allocator);
    defer name_store.deinit();
    var store = Store.init(std.testing.allocator);
    defer store.deinit();

    const unit = try store.internZst(&name_store);
    const unit_index = @intFromEnum(unit);
    try std.testing.expect(!(try store.containsIteratorInterface(unit)));
    try std.testing.expectEqual(@as(?bool, false), store.iterator_interface_cache.unsafeRawItemsForView()[unit_index]);

    // An interning hit restores the store to its pre-candidate mark.
    _ = try store.internZst(&name_store);
    try std.testing.expectEqual(@as(?bool, false), store.iterator_interface_cache.unsafeRawItemsForView()[unit_index]);

    const transaction = store.beginTransaction();
    const recursive = try transaction.reserve(&store);
    transaction.fill(&store, recursive, .{ .list = recursive });
    var result = try store.commitTransaction(&name_store, transaction, recursive);
    defer result.deinit();
    try std.testing.expectEqual(@as(?bool, false), store.iterator_interface_cache.unsafeRawItemsForView()[unit_index]);

    const aborted = store.beginTransaction();
    _ = try aborted.reserve(&store);
    aborted.abort(&store);
    try std.testing.expectEqual(@as(?bool, false), store.iterator_interface_cache.unsafeRawItemsForView()[unit_index]);
}

test "monotype type store acyclic interning normalizes record and tag rows" {
    var name_store = names.NameStore.init(std.testing.allocator);
    defer name_store.deinit();

    const a_field = try name_store.internRecordFieldLabel("a");
    const b_field = try name_store.internRecordFieldLabel("b");
    const a_tag = try name_store.internTagLabel("A");
    const b_tag = try name_store.internTagLabel("B");

    var store = Store.init(std.testing.allocator);
    defer store.deinit();

    const unit = try store.internZst(&name_store);
    const first_record = try store.internRecord(&name_store, &.{
        .{ .name = b_field, .ty = unit, .default = null },
        .{ .name = a_field, .ty = unit, .default = null },
    });
    const second_record = try store.internRecord(&name_store, &.{
        .{ .name = a_field, .ty = unit, .default = null },
        .{ .name = b_field, .ty = unit, .default = null },
    });
    try std.testing.expectEqual(first_record, second_record);

    const first_tags = try store.internTagUnion(&name_store, &.{
        .{ .name = b_tag, .checked_name = b_tag, .payloads = &.{unit} },
        .{ .name = a_tag, .checked_name = a_tag, .payloads = &.{unit} },
    });
    const second_tags = try store.internTagUnion(&name_store, &.{
        .{ .name = a_tag, .checked_name = a_tag, .payloads = &.{unit} },
        .{ .name = b_tag, .checked_name = b_tag, .payloads = &.{unit} },
    });
    try std.testing.expectEqual(first_tags, second_tags);

    const record_fields = store.fieldSpan(store.get(first_record).record);
    try std.testing.expectEqual(a_field, GuardedList.at(record_fields, 0).name);
    try std.testing.expectEqual(b_field, GuardedList.at(record_fields, 1).name);
    const tag_fields = store.tagSpan(store.get(first_tags).tag_union);
    try std.testing.expectEqual(a_tag, GuardedList.at(tag_fields, 0).name);
    try std.testing.expectEqual(b_tag, GuardedList.at(tag_fields, 1).name);
}

test "monotype type store acyclic interning preserves tag payload order" {
    var name_store = names.NameStore.init(std.testing.allocator);
    defer name_store.deinit();

    const tag_name = try name_store.internTagLabel("Pair");

    var store = Store.init(std.testing.allocator);
    defer store.deinit();

    const first = try store.internPrimitive(&name_store, .i64);
    const second = try store.internPrimitive(&name_store, .str);
    const tag_ty = try store.internTagUnion(&name_store, &.{
        .{ .name = tag_name, .checked_name = tag_name, .payloads = &.{ first, second } },
    });

    const tags_ = store.tagSpan(store.get(tag_ty).tag_union);
    const stored_payloads = store.span(GuardedList.at(tags_, 0).payloads);
    try std.testing.expectEqual(first, GuardedList.at(stored_payloads, 0));
    try std.testing.expectEqual(second, GuardedList.at(stored_payloads, 1));
}

test "monotype type store acyclic interning keeps distinct backing-less aliases" {
    // Mirrors the standalone interner's equivalent coverage: backing-less
    // aliases are opaque named nodes, so differently named backing-less
    // aliases carry distinct digests and stay distinct store-level entries.
    var name_store = names.NameStore.init(std.testing.allocator);
    defer name_store.deinit();

    const module_identity = try name_store.internModuleIdentity(&([_]u8{0xCD} ** 32));
    const first_name = try name_store.internTypeName("First");
    const second_name = try name_store.internTypeName("Second");

    var store = Store.init(std.testing.allocator);
    defer store.deinit();

    const first = try store.internNamed(&name_store, .{
        .named_type = .{ .module = .{}, .ty = @enumFromInt(1) },
        .def = .{ .module = module_identity, .type_name = first_name },
        .kind = .alias,
        .backing = null,
    });
    const second = try store.internNamed(&name_store, .{
        .named_type = .{ .module = .{}, .ty = @enumFromInt(2) },
        .def = .{ .module = module_identity, .type_name = second_name },
        .kind = .alias,
        .backing = null,
    });
    const repeat_first = try store.internNamed(&name_store, .{
        .named_type = .{ .module = .{}, .ty = @enumFromInt(1) },
        .def = .{ .module = module_identity, .type_name = first_name },
        .kind = .alias,
        .backing = null,
    });

    const first_digest = store.typeDigest(&name_store, first);
    const second_digest = store.typeDigest(&name_store, second);
    try std.testing.expect(!std.mem.eql(u8, first_digest.bytes[0..], second_digest.bytes[0..]));
    try std.testing.expect(first != second);
    try std.testing.expectEqual(first, repeat_first);
    try std.testing.expectEqual(@as(usize, 2), store.view().types.len);
}

test "monotype named type digest includes generic arguments" {
    var name_store = names.NameStore.init(std.testing.allocator);
    defer name_store.deinit();

    const module_identity = try name_store.internModuleIdentity(&([_]u8{0xAB} ** 32));
    const type_name = try name_store.internTypeName("Box");

    var store = Store.init(std.testing.allocator);
    defer store.deinit();

    const i64_ty = try store.add(.{ .primitive = .i64 });
    const str = try store.add(.{ .primitive = .str });
    const i64_args = try store.addSpan(&.{i64_ty});
    const str_args = try store.addSpan(&.{str});
    const checked_ty: checked.CheckedTypeId = @enumFromInt(1);

    const named_i64 = try store.add(.{ .named = .{
        .named_type = .{ .module = .{}, .ty = checked_ty },
        .def = .{ .module = module_identity, .type_name = type_name },
        .kind = .nominal,
        .args = i64_args,
    } });
    const named_str = try store.add(.{ .named = .{
        .named_type = .{ .module = .{}, .ty = checked_ty },
        .def = .{ .module = module_identity, .type_name = type_name },
        .kind = .nominal,
        .args = str_args,
    } });

    const i64_digest = store.typeDigest(&name_store, named_i64);
    const str_digest = store.typeDigest(&name_store, named_str);
    try std.testing.expect(!std.mem.eql(u8, i64_digest.bytes[0..], str_digest.bytes[0..]));
}

test "monotype recursive nominal digest ignores how deep the knot is tied" {
    // A recursive nominal reached through independently lowered graphs can be
    // built either knotted (its recursive occurrence is the node itself) or
    // unrolled one step (its recursive occurrence is a separate, equal node).
    // Both denote the same type, so both must digest the same: `sameType` in
    // call-pattern specialization relies on the digest to prove a call and its
    // callee share a representation, and declines to inline when it differs.
    var name_store = names.NameStore.init(std.testing.allocator);
    defer name_store.deinit();

    const module_identity = try name_store.internModuleIdentity(&([_]u8{0xCD} ** 32));
    const type_name = try name_store.internTypeName("V");
    const tag_name = try name_store.internTagLabel("Node");

    var store = Store.init(std.testing.allocator);
    defer store.deinit();

    const u64_ty = try store.add(.{ .primitive = .u64 });
    const checked_ty: checked.CheckedTypeId = @enumFromInt(1);

    const Context = struct {
        store: *Store,
        name_store: *names.NameStore,
        module_identity: names.ModuleIdentityId,
        type_name: names.TypeNameId,
        tag_name: names.TagLabelId,
        checked_ty: checked.CheckedTypeId,
        arg: TypeId,

        /// `V(U64) := [Node(List(V(U64)))]`, with `recursive_occurrence` standing
        /// in for the nested `V(U64)`.
        fn nominal(ctx: @This(), recursive_occurrence: TypeId) std.mem.Allocator.Error!Content {
            const list_ty = try ctx.store.add(.{ .list = recursive_occurrence });
            const backing = try ctx.store.add(.{ .tag_union = try ctx.store.addTagVariants(ctx.name_store, &.{.{
                .name = ctx.tag_name,
                .checked_name = ctx.tag_name,
                .payloads = try ctx.store.addSpan(&.{list_ty}),
            }}) });
            return .{ .named = .{
                .named_type = .{ .module = .{}, .ty = ctx.checked_ty },
                .def = .{ .module = ctx.module_identity, .type_name = ctx.type_name },
                .kind = .nominal,
                .args = try ctx.store.addSpan(&.{ctx.arg}),
                .backing = .{ .ty = backing, .use = .inspectable },
            } };
        }

        fn fillKnotted(ctx: @This(), root: TypeId) std.mem.Allocator.Error!Content {
            return try ctx.nominal(root);
        }
    };

    const context = Context{
        .store = &store,
        .name_store = &name_store,
        .module_identity = module_identity,
        .type_name = type_name,
        .tag_name = tag_name,
        .checked_ty = checked_ty,
        .arg = u64_ty,
    };

    const knotted = try store.addRecursive(context, Context.fillKnotted);
    const unrolled = try store.add(try context.nominal(knotted));

    try std.testing.expect(knotted != unrolled);
    const knotted_digest = store.typeDigest(&name_store, knotted);
    const unrolled_digest = store.typeDigest(&name_store, unrolled);
    try std.testing.expectEqualSlices(u8, knotted_digest.bytes[0..], unrolled_digest.bytes[0..]);
}

test "monotype recursive nominal digest still separates different arguments" {
    // Unrolling insensitivity must key on the nominal's declaration *and*
    // arguments: a non-uniformly recursive occurrence at different arguments
    // is a different type and must not collapse into its parent.
    var name_store = names.NameStore.init(std.testing.allocator);
    defer name_store.deinit();

    const module_identity = try name_store.internModuleIdentity(&([_]u8{0xCE} ** 32));
    const type_name = try name_store.internTypeName("V");
    const tag_name = try name_store.internTagLabel("Node");

    var store = Store.init(std.testing.allocator);
    defer store.deinit();

    const u64_ty = try store.add(.{ .primitive = .u64 });
    const str_ty = try store.add(.{ .primitive = .str });
    const checked_ty: checked.CheckedTypeId = @enumFromInt(1);

    const inner_args = try store.addSpan(&.{str_ty});
    const inner_backing = try store.add(.{ .tag_union = try store.addTagVariants(&name_store, &.{.{
        .name = tag_name,
        .checked_name = tag_name,
        .payloads = try store.addSpan(&.{u64_ty}),
    }}) });
    const inner = try store.add(.{ .named = .{
        .named_type = .{ .module = .{}, .ty = checked_ty },
        .def = .{ .module = module_identity, .type_name = type_name },
        .kind = .nominal,
        .args = inner_args,
        .backing = .{ .ty = inner_backing, .use = .inspectable },
    } });

    const outer_args = try store.addSpan(&.{u64_ty});
    const outer_backing = try store.add(.{ .tag_union = try store.addTagVariants(&name_store, &.{.{
        .name = tag_name,
        .checked_name = tag_name,
        .payloads = try store.addSpan(&.{inner}),
    }}) });
    const outer = try store.add(.{ .named = .{
        .named_type = .{ .module = .{}, .ty = checked_ty },
        .def = .{ .module = module_identity, .type_name = type_name },
        .kind = .nominal,
        .args = outer_args,
        .backing = .{ .ty = outer_backing, .use = .inspectable },
    } });

    const inner_digest = store.typeDigest(&name_store, inner);
    const outer_digest = store.typeDigest(&name_store, outer);
    try std.testing.expect(!std.mem.eql(u8, inner_digest.bytes[0..], outer_digest.bytes[0..]));
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
    const fields = try store.addFields(&.{.{ .name = field_name, .ty = i64_ty, .default = null }});
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
    const nonempty_fields = try store.addFields(&.{.{ .name = @enumFromInt(1), .ty = unit, .default = null }});
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
        .{ .name = a_field, .ty = i64_ty, .default = null },
        .{ .name = b_field, .ty = i64_ty, .default = null },
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
            .{ .name = b_field, .ty = i64_ty, .default = null },
            .{ .name = a_field, .ty = i64_ty, .default = null },
        });
        _ = try store.add(.{ .record = unsorted });
        try std.testing.expectEqual(Store.VerifyError.record_fields_not_sorted, store.verify(&name_store).?);
    }

    {
        var store = Store.init(std.testing.allocator);
        defer store.deinit();

        const bad_fields = try store.addFields(&.{.{ .name = a_field, .ty = @enumFromInt(99), .default = null }});
        _ = try store.add(.{ .record = bad_fields });
        try std.testing.expectEqual(Store.VerifyError.type_ref_out_of_bounds, store.verify(&name_store).?);
    }
}

test "monotype digest terminates on recursive structural types" {
    var name_store = names.NameStore.init(std.testing.allocator);
    defer name_store.deinit();

    var store = Store.init(std.testing.allocator);
    defer store.deinit();

    const field_name = try name_store.internRecordFieldLabel("step");

    // A record whose field is a function returning the record itself.
    const rec_a = try store.reserveSlot();
    const fn_a = try store.add(.{ .func = .{ .args = Span.empty(), .ret = rec_a } });
    const fields_a = try store.addFields(&.{.{ .name = field_name, .ty = fn_a, .default = null }});
    store.fillReservedSlot(rec_a, .{ .record = fields_a });

    const first = store.typeDigest(&name_store, rec_a);
    const again = store.typeDigest(&name_store, rec_a);
    try std.testing.expect(std.mem.eql(u8, first.bytes[0..], again.bytes[0..]));

    // An isomorphic cycle at different ids digests identically: cycles are
    // encoded as back references by position, not by id.
    const rec_b = try store.reserveSlot();
    const fn_b = try store.add(.{ .func = .{ .args = Span.empty(), .ret = rec_b } });
    const fields_b = try store.addFields(&.{.{ .name = field_name, .ty = fn_b, .default = null }});
    store.fillReservedSlot(rec_b, .{ .record = fields_b });

    const other = store.typeDigest(&name_store, rec_b);
    try std.testing.expect(std.mem.eql(u8, first.bytes[0..], other.bytes[0..]));
}

test "monotype cached digest survives completion of an unrelated reserved slot" {
    var name_store = names.NameStore.init(std.testing.allocator);
    defer name_store.deinit();

    var store = Store.init(std.testing.allocator);
    defer store.deinit();

    const inner_field = try name_store.internRecordFieldLabel("inner");
    const outer_field = try name_store.internRecordFieldLabel("outer");

    const i64_ty = try store.add(.{ .primitive = .i64 });
    const inner_fields = try store.addFields(&.{.{ .name = inner_field, .ty = i64_ty, .default = null }});
    const inner = try store.add(.{ .record = inner_fields });

    var inner_stats: Store.DigestStats = .{};
    const inner_digest = store.typeDigestCached(&name_store, inner, &inner_stats);
    try std.testing.expectEqual(@as(u64, 0), inner_stats.cache_hits);
    try std.testing.expectEqual(@as(u64, 2), inner_stats.cache_misses);
    try std.testing.expectEqual(@as(u64, 2), inner_stats.nodes_visited);

    const outer_fields = try store.addFields(&.{.{ .name = outer_field, .ty = inner, .default = null }});
    const outer = try store.add(.{ .record = outer_fields });

    var outer_stats: Store.DigestStats = .{};
    _ = store.typeDigestCached(&name_store, outer, &outer_stats);
    try std.testing.expectEqual(@as(u64, 1), outer_stats.cache_hits);
    try std.testing.expectEqual(@as(u64, 1), outer_stats.cache_misses);
    try std.testing.expectEqual(@as(u64, 1), outer_stats.nodes_visited);

    const unrelated = try store.reserveSlot();
    store.fillReservedSlot(unrelated, .{ .record = Span.empty() });

    var after_fill_stats: Store.DigestStats = .{};
    const after_fill = store.typeDigestCached(&name_store, inner, &after_fill_stats);
    try std.testing.expect(std.mem.eql(u8, inner_digest.bytes[0..], after_fill.bytes[0..]));
    try std.testing.expectEqual(@as(u64, 1), after_fill_stats.cache_hits);
    try std.testing.expectEqual(@as(u64, 0), after_fill_stats.cache_misses);
    try std.testing.expectEqual(@as(u64, 0), after_fill_stats.nodes_visited);
}

test "monotype iterator containment cache is invalidated by rollback" {
    var name_store = names.NameStore.init(std.testing.allocator);
    defer name_store.deinit();

    var store = Store.init(std.testing.allocator);
    defer store.deinit();

    const survivor = try store.reserveSlot();
    const mark_ = store.mark();
    const transient = try store.add(.{ .primitive = .u64 });
    store.fillReservedSlot(survivor, .{ .box = transient });
    try std.testing.expect(!try store.containsIteratorInterface(survivor));

    store.restore(mark_);
    const module_identity = try name_store.internModuleIdentity(&([_]u8{0xAB} ** 32));
    const type_name = try name_store.internTypeName("Iter");
    const replacement = try store.add(.{ .named = .{
        .named_type = .{ .module = .{}, .ty = @enumFromInt(1) },
        .def = .{ .module = module_identity, .type_name = type_name },
        .kind = .nominal,
        .builtin_owner = .iter,
        .args = Span.empty(),
    } });
    try std.testing.expectEqual(transient, replacement);
    try std.testing.expect(try store.containsIteratorInterface(survivor));
}

test "monotype cached digest stays stable across multiple edges into one recursive group" {
    var name_store = names.NameStore.init(std.testing.allocator);
    defer name_store.deinit();

    var store = Store.init(std.testing.allocator);
    defer store.deinit();

    const first_field = try name_store.internRecordFieldLabel("first");
    const second_field = try name_store.internRecordFieldLabel("second");
    const recursive = try store.reserveSlot();
    const first_fn = try store.add(.{ .func = .{ .args = Span.empty(), .ret = recursive } });
    const second_fn = try store.add(.{ .func = .{ .args = Span.empty(), .ret = recursive } });
    const fields = try store.addFields(&.{
        .{ .name = first_field, .ty = first_fn, .default = null },
        .{ .name = second_field, .ty = second_fn, .default = null },
    });
    store.fillReservedSlot(recursive, .{ .record = fields });

    const first_full = store.typeDigestCached(&name_store, recursive, null);
    const second_full = store.typeDigestCached(&name_store, recursive, null);
    try std.testing.expect(std.mem.eql(u8, first_full.bytes[0..], second_full.bytes[0..]));

    const first_specialization = store.specializationDigestCached(&name_store, recursive, null);
    const second_specialization = store.specializationDigestCached(&name_store, recursive, null);
    try std.testing.expect(std.mem.eql(u8, first_specialization.bytes[0..], second_specialization.bytes[0..]));
}

test "monotype type equality accepts isomorphic recursive structural types" {
    var name_store = names.NameStore.init(std.testing.allocator);
    defer name_store.deinit();

    var store = Store.init(std.testing.allocator);
    defer store.deinit();

    const field_name = try name_store.internRecordFieldLabel("step");

    const rec_a = try store.reserveSlot();
    const fn_a = try store.add(.{ .func = .{ .args = Span.empty(), .ret = rec_a } });
    const fields_a = try store.addFields(&.{.{ .name = field_name, .ty = fn_a, .default = null }});
    store.fillReservedSlot(rec_a, .{ .record = fields_a });

    const rec_b = try store.reserveSlot();
    const fn_b = try store.add(.{ .func = .{ .args = Span.empty(), .ret = rec_b } });
    const fields_b = try store.addFields(&.{.{ .name = field_name, .ty = fn_b, .default = null }});
    store.fillReservedSlot(rec_b, .{ .record = fields_b });

    try std.testing.expect(try store.typeEql(&name_store, rec_a, rec_b));

    const str = try store.add(.{ .primitive = .str });
    const rec_c = try store.reserveSlot();
    const fields_c = try store.addFields(&.{.{ .name = field_name, .ty = str, .default = null }});
    store.fillReservedSlot(rec_c, .{ .record = fields_c });
    try std.testing.expect(!try store.typeEql(&name_store, rec_a, rec_c));
}

test "monotype digest keeps aliases opaque" {
    // Aliases survive into later IR as observable nodes, so their digests are
    // their own: a nickname is not the same stored identity as its backing,
    // and not the same as an equally named nominal either. Structural
    // equality still unwraps aliases; only the content identity is opaque.
    var name_store = names.NameStore.init(std.testing.allocator);
    defer name_store.deinit();

    var store = Store.init(std.testing.allocator);
    defer store.deinit();

    const module_identity = try name_store.internModuleIdentity(&([_]u8{0xAB} ** 32));
    const type_name = try name_store.internTypeName("Pretty");
    const checked_ty: checked.CheckedTypeId = @enumFromInt(1);

    const str = try store.add(.{ .primitive = .str });
    const aliased = try store.add(.{ .named = .{
        .named_type = .{ .module = .{}, .ty = checked_ty },
        .def = .{ .module = module_identity, .type_name = type_name },
        .kind = .alias,
        .args = Span.empty(),
        .backing = .{ .ty = str, .use = .inspectable },
    } });
    const nominal = try store.add(.{ .named = .{
        .named_type = .{ .module = .{}, .ty = checked_ty },
        .def = .{ .module = module_identity, .type_name = type_name },
        .kind = .nominal,
        .args = Span.empty(),
        .backing = .{ .ty = str, .use = .inspectable },
    } });

    const str_digest = store.typeDigest(&name_store, str);
    const alias_digest = store.typeDigest(&name_store, aliased);
    const nominal_digest = store.typeDigest(&name_store, nominal);
    try std.testing.expect(!std.mem.eql(u8, str_digest.bytes[0..], alias_digest.bytes[0..]));
    try std.testing.expect(!std.mem.eql(u8, str_digest.bytes[0..], nominal_digest.bytes[0..]));
    try std.testing.expect(!std.mem.eql(u8, alias_digest.bytes[0..], nominal_digest.bytes[0..]));

    // Alias-over-nominal is likewise distinct from the nominal it wraps.
    const alias_over_nominal = try store.add(.{ .named = .{
        .named_type = .{ .module = .{}, .ty = checked_ty },
        .def = .{ .module = module_identity, .type_name = type_name },
        .kind = .alias,
        .args = Span.empty(),
        .backing = .{ .ty = nominal, .use = .inspectable },
    } });
    const alias_over_nominal_digest = store.typeDigest(&name_store, alias_over_nominal);
    try std.testing.expect(!std.mem.eql(u8, alias_over_nominal_digest.bytes[0..], nominal_digest.bytes[0..]));
    try std.testing.expect(!std.mem.eql(u8, alias_over_nominal_digest.bytes[0..], alias_digest.bytes[0..]));

    // Structural equality still unwraps aliases; only content identity is
    // opaque, so alias-vs-backing is the deliberate one-way exception to
    // "structurally equal implies digest equal".
    try std.testing.expect(try store.typeEql(&name_store, str, aliased));
    try std.testing.expect(try store.typeEql(&name_store, nominal, alias_over_nominal));
}

test "monotype type equality treats aliases as their backing" {
    var name_store = names.NameStore.init(std.testing.allocator);
    defer name_store.deinit();

    var store = Store.init(std.testing.allocator);
    defer store.deinit();

    const module_identity = try name_store.internModuleIdentity(&([_]u8{0xAB} ** 32));
    const type_name = try name_store.internTypeName("Pretty");
    const checked_ty: checked.CheckedTypeId = @enumFromInt(1);

    const str = try store.add(.{ .primitive = .str });
    const aliased = try store.add(.{ .named = .{
        .named_type = .{ .module = .{}, .ty = checked_ty },
        .def = .{ .module = module_identity, .type_name = type_name },
        .kind = .alias,
        .args = Span.empty(),
        .backing = .{ .ty = str, .use = .inspectable },
    } });
    const nominal = try store.add(.{ .named = .{
        .named_type = .{ .module = .{}, .ty = checked_ty },
        .def = .{ .module = module_identity, .type_name = type_name },
        .kind = .nominal,
        .args = Span.empty(),
        .backing = .{ .ty = str, .use = .inspectable },
    } });

    try std.testing.expect(try store.typeEql(&name_store, str, aliased));
    try std.testing.expect(!try store.typeEql(&name_store, str, nominal));
    try std.testing.expectEqual(store.equalityDigest(&name_store, str), store.equalityDigest(&name_store, aliased));
    try std.testing.expect(!std.meta.eql(store.equalityDigest(&name_store, str), store.equalityDigest(&name_store, nominal)));
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
    const current_fields = try current.addFields(&.{.{ .name = field_name, .ty = current_unit, .default = null }});
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
    const loaded_fields = try loaded.addFields(&.{.{ .name = field_name, .ty = loaded_unit, .default = null }});
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

test "monotype type equality and digests separate aliases without backing" {
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
    try std.testing.expect(!std.mem.eql(u8, first_digest.bytes[0..], second_digest.bytes[0..]));
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

test "monotype specialization identity includes generated backing without builtin owner" {
    var name_store = names.NameStore.init(std.testing.allocator);
    defer name_store.deinit();

    var store = Store.init(std.testing.allocator);
    defer store.deinit();

    const module_identity = try name_store.internModuleIdentity(&([_]u8{0xAB} ** 32));
    const type_name = try name_store.internTypeName("GeneratedEvidence");
    const checked_ty: checked.CheckedTypeId = @enumFromInt(1);
    const i64_ty = try store.add(.{ .primitive = .i64 });
    const str_ty = try store.add(.{ .primitive = .str });

    const evidence_i64 = try store.add(.{ .named = .{
        .named_type = .{ .module = .{}, .ty = checked_ty },
        .def = .{ .module = module_identity, .type_name = type_name },
        .kind = .@"opaque",
        .args = Span.empty(),
        .backing = .{ .ty = i64_ty, .use = .runtime_layout_only, .authority = .generated_private },
    } });
    const evidence_str = try store.add(.{ .named = .{
        .named_type = .{ .module = .{}, .ty = checked_ty },
        .def = .{ .module = module_identity, .type_name = type_name },
        .kind = .@"opaque",
        .args = Span.empty(),
        .backing = .{ .ty = str_ty, .use = .runtime_layout_only, .authority = .generated_private },
    } });

    const i64_spec_digest = store.specializationDigest(&name_store, evidence_i64);
    const str_spec_digest = store.specializationDigest(&name_store, evidence_str);
    try std.testing.expect(!std.mem.eql(u8, i64_spec_digest.bytes[0..], str_spec_digest.bytes[0..]));
    try std.testing.expect(!try store.typeEql(&name_store, evidence_i64, evidence_str));
    try std.testing.expect(!try typeEqlAcrossStores(
        std.testing.allocator,
        &name_store,
        store.view(),
        evidence_i64,
        store.view(),
        evidence_str,
    ));
}

test "monotype named backing authority participates in durable identity" {
    var name_store = names.NameStore.init(std.testing.allocator);
    defer name_store.deinit();

    var store = Store.init(std.testing.allocator);
    defer store.deinit();

    const module_identity = try name_store.internModuleIdentity(&([_]u8{0xAC} ** 32));
    const type_name = try name_store.internTypeName("UnownedEvidence");
    const backing = try store.add(.{ .record = Span.empty() });
    const base = NamedContent{
        .named_type = .{ .module = .{}, .ty = @enumFromInt(1) },
        .def = .{ .module = module_identity, .type_name = type_name },
        .kind = .@"opaque",
        .args = Span.empty(),
        .backing = .{ .ty = backing, .use = .runtime_layout_only },
    };
    const public = try store.add(.{ .named = base });
    var private_content = base;
    private_content.backing.?.authority = .generated_private;
    const private = try store.add(.{ .named = private_content });

    const public_digest = store.specializationDigest(&name_store, public);
    const private_digest = store.specializationDigest(&name_store, private);
    try std.testing.expect(!std.mem.eql(u8, public_digest.bytes[0..], private_digest.bytes[0..]));
    try std.testing.expect(!try store.typeEql(&name_store, public, private));
    try std.testing.expect(!try typeEqlAcrossStores(
        std.testing.allocator,
        &name_store,
        store.view(),
        public,
        store.view(),
        private,
    ));
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
        .{ .name = field_a, .ty = i64_ty, .default = null },
        .{ .name = field_b, .ty = i64_ty, .default = null },
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

    // The interface digest intentionally omits declared field order: both
    // orders may share a specialization.
    const ab_spec = store.specializationDigest(&name_store, named_ab);
    const ba_spec = store.specializationDigest(&name_store, named_ba);
    try std.testing.expect(std.mem.eql(u8, ab_spec.bytes[0..], ba_spec.bytes[0..]));
}

test "monotype digest folds a recursive knot beyond depth 300 into its unrolling" {
    // A cycle of 350 box nodes and a self-box denote the same infinite type.
    // The deleted depth-256 slot-index digest would have made the long
    // cycle's digest depend on its ids; graph reduction collapses both to the
    // same one-node quotient.
    var name_store = names.NameStore.init(std.testing.allocator);
    defer name_store.deinit();

    var store = Store.init(std.testing.allocator);
    defer store.deinit();

    var chain: [350]TypeId = undefined;
    for (&chain) |*id| id.* = try store.reserveSlot();
    for (chain, 0..) |id, index| {
        store.fillReservedSlot(id, .{ .box = chain[(index + 1) % chain.len] });
    }
    const knot = try store.reserveSlot();
    store.fillReservedSlot(knot, .{ .box = knot });

    const chain_digest = store.typeDigest(&name_store, chain[0]);
    const knot_digest = store.typeDigest(&name_store, knot);
    try std.testing.expectEqualSlices(u8, chain_digest.bytes[0..], knot_digest.bytes[0..]);
    // Every position of the long cycle is equivalent to every other.
    const mid_digest = store.typeDigest(&name_store, chain[137]);
    try std.testing.expectEqualSlices(u8, chain_digest.bytes[0..], mid_digest.bytes[0..]);
}

test "monotype digest equates bisimilar recursive graphs of different sizes" {
    var name_store = names.NameStore.init(std.testing.allocator);
    defer name_store.deinit();

    var store = Store.init(std.testing.allocator);
    defer store.deinit();

    const x = try store.reserveSlot();
    const y = try store.reserveSlot();
    store.fillReservedSlot(x, .{ .list = y });
    store.fillReservedSlot(y, .{ .list = x });
    const z = try store.reserveSlot();
    store.fillReservedSlot(z, .{ .list = z });

    const x_digest = store.typeDigest(&name_store, x);
    const z_digest = store.typeDigest(&name_store, z);
    try std.testing.expectEqualSlices(u8, x_digest.bytes[0..], z_digest.bytes[0..]);

    // Equivalent positions inside one SCC receive equal digests.
    const y_digest = store.typeDigest(&name_store, y);
    try std.testing.expectEqualSlices(u8, x_digest.bytes[0..], y_digest.bytes[0..]);

    // A rolled-out prefix built after the knot's digest was cached folds to
    // the member digest through the store's unfolding index.
    const prefix = try store.add(.{ .list = z });
    const prefix_digest = store.typeDigest(&name_store, prefix);
    try std.testing.expectEqualSlices(u8, z_digest.bytes[0..], prefix_digest.bytes[0..]);
}

test "monotype digest ignores child cachedness inside a recursive group" {
    // k1 and k2 are equivalent positions of one knot, but k1 references a str
    // leaf whose digest was cached before the knot was digested while k2
    // references an uncached duplicate leaf. Whether a child arrived as a
    // cache hit must not influence the reduced partition: both members share
    // one digest, equal to an independently tied one-node knot over a third
    // duplicate leaf.
    var name_store = names.NameStore.init(std.testing.allocator);
    defer name_store.deinit();

    var store = Store.init(std.testing.allocator);
    defer store.deinit();

    const cached_str = try store.add(.{ .primitive = .str });
    _ = store.typeDigest(&name_store, cached_str);
    const fresh_str = try store.add(.{ .primitive = .str });

    const k1 = try store.reserveSlot();
    const k2 = try store.reserveSlot();
    store.fillReservedSlot(k1, .{ .tuple = try store.addSpan(&.{ k2, cached_str }) });
    store.fillReservedSlot(k2, .{ .tuple = try store.addSpan(&.{ k1, fresh_str }) });

    const k1_digest = store.typeDigest(&name_store, k1);
    const k2_digest = store.typeDigest(&name_store, k2);
    try std.testing.expectEqualSlices(u8, k1_digest.bytes[0..], k2_digest.bytes[0..]);

    const third_str = try store.add(.{ .primitive = .str });
    const solo = try store.reserveSlot();
    store.fillReservedSlot(solo, .{ .tuple = try store.addSpan(&.{ solo, third_str }) });
    const solo_digest = store.typeDigest(&name_store, solo);
    try std.testing.expectEqualSlices(u8, k1_digest.bytes[0..], solo_digest.bytes[0..]);
}

test "monotype digest keeps entangled equivalent knots conservatively distinct" {
    // b1/b2 tie a knot that also points into c, a separately tied knot of the
    // same infinite type. Per-SCC reduction cannot identify an in-SCC
    // position with an equivalent position outside its own SCC, so the
    // members digest apart from c even though all three are structurally
    // equal. This pins a KNOWN conservative incompleteness: equal digests
    // still imply structural equality (the direction interning relies on),
    // and a future whole-graph reduction may intentionally flip the
    // inequality assertions below.
    var name_store = names.NameStore.init(std.testing.allocator);
    defer name_store.deinit();

    var store = Store.init(std.testing.allocator);
    defer store.deinit();

    const c = try store.reserveSlot();
    store.fillReservedSlot(c, .{ .tuple = try store.addSpan(&.{ c, c }) });
    const b1 = try store.reserveSlot();
    const b2 = try store.reserveSlot();
    store.fillReservedSlot(b1, .{ .tuple = try store.addSpan(&.{ b2, b1 }) });
    store.fillReservedSlot(b2, .{ .tuple = try store.addSpan(&.{ b1, c }) });

    try std.testing.expect(try store.typeEql(&name_store, b1, c));
    try std.testing.expect(try store.typeEql(&name_store, b2, c));

    const c_digest = store.typeDigest(&name_store, c);
    const b1_digest = store.typeDigest(&name_store, b1);
    const b2_digest = store.typeDigest(&name_store, b2);
    try std.testing.expect(!std.mem.eql(u8, b1_digest.bytes[0..], c_digest.bytes[0..]));
    try std.testing.expect(!std.mem.eql(u8, b2_digest.bytes[0..], c_digest.bytes[0..]));

    // Digests stay stable on repeat queries.
    const b1_again = store.typeDigest(&name_store, b1);
    try std.testing.expectEqualSlices(u8, b1_digest.bytes[0..], b1_again.bytes[0..]);
}

test "monotype digest separates tag unions by checked name" {
    var name_store = names.NameStore.init(std.testing.allocator);
    defer name_store.deinit();

    var store = Store.init(std.testing.allocator);
    defer store.deinit();

    const runtime_name = try name_store.internTagLabel("Runtime");
    const first_checked = try name_store.internTagLabel("FirstChecked");
    const second_checked = try name_store.internTagLabel("SecondChecked");

    const first = try store.add(.{ .tag_union = try store.addTags(&.{
        .{ .name = runtime_name, .checked_name = first_checked, .payloads = Span.empty() },
    }) });
    const second = try store.add(.{ .tag_union = try store.addTags(&.{
        .{ .name = runtime_name, .checked_name = second_checked, .payloads = Span.empty() },
    }) });

    const first_digest = store.typeDigest(&name_store, first);
    const second_digest = store.typeDigest(&name_store, second);
    try std.testing.expect(!std.mem.eql(u8, first_digest.bytes[0..], second_digest.bytes[0..]));
    const first_spec = store.specializationDigest(&name_store, first);
    const second_spec = store.specializationDigest(&name_store, second);
    try std.testing.expect(!std.mem.eql(u8, first_spec.bytes[0..], second_spec.bytes[0..]));
}

test "monotype digest separates named types by checked type id" {
    // `named_type.ty` survives into `ConstStore` and is later used to
    // re-enter the checked store, so it is part of the identity.
    var name_store = names.NameStore.init(std.testing.allocator);
    defer name_store.deinit();

    var store = Store.init(std.testing.allocator);
    defer store.deinit();

    const module_identity = try name_store.internModuleIdentity(&([_]u8{0xAB} ** 32));
    const type_name = try name_store.internTypeName("Reentrant");

    const first = try store.add(.{ .named = .{
        .named_type = .{ .module = .{}, .ty = @enumFromInt(1) },
        .def = .{ .module = module_identity, .type_name = type_name },
        .kind = .nominal,
        .args = Span.empty(),
    } });
    const second = try store.add(.{ .named = .{
        .named_type = .{ .module = .{}, .ty = @enumFromInt(2) },
        .def = .{ .module = module_identity, .type_name = type_name },
        .kind = .nominal,
        .args = Span.empty(),
    } });

    const first_digest = store.typeDigest(&name_store, first);
    const second_digest = store.typeDigest(&name_store, second);
    try std.testing.expect(!std.mem.eql(u8, first_digest.bytes[0..], second_digest.bytes[0..]));
    const first_spec = store.specializationDigest(&name_store, first);
    const second_spec = store.specializationDigest(&name_store, second);
    try std.testing.expect(!std.mem.eql(u8, first_spec.bytes[0..], second_spec.bytes[0..]));
}

test "monotype full and interface digests use separate domains" {
    var name_store = names.NameStore.init(std.testing.allocator);
    defer name_store.deinit();

    var store = Store.init(std.testing.allocator);
    defer store.deinit();

    const unit = try store.add(.zst);
    const full = store.typeDigest(&name_store, unit);
    const interface = store.specializationDigest(&name_store, unit);
    try std.testing.expect(!std.mem.eql(u8, full.bytes[0..], interface.bytes[0..]));
}

test "monotype digests are independent of allocation order" {
    var name_store = names.NameStore.init(std.testing.allocator);
    defer name_store.deinit();

    var first = Store.init(std.testing.allocator);
    defer first.deinit();
    var second = Store.init(std.testing.allocator);
    defer second.deinit();

    // First store: knot filled forward, root tuple built directly.
    const first_str = try first.add(.{ .primitive = .str });
    const first_x = try first.reserveSlot();
    const first_y = try first.reserveSlot();
    first.fillReservedSlot(first_x, .{ .list = first_y });
    first.fillReservedSlot(first_y, .{ .list = first_x });
    const first_root = try first.add(.{ .tuple = try first.addSpan(&.{ first_x, first_str }) });

    // Second store: junk ids first, the knot filled in the opposite order,
    // and the equivalent cycle entered at the other position.
    _ = try second.add(.{ .primitive = .i64 });
    _ = try second.add(.zst);
    const second_y = try second.reserveSlot();
    const second_x = try second.reserveSlot();
    second.fillReservedSlot(second_y, .{ .list = second_x });
    second.fillReservedSlot(second_x, .{ .list = second_y });
    const second_str = try second.add(.{ .primitive = .str });
    const second_root = try second.add(.{ .tuple = try second.addSpan(&.{ second_x, second_str }) });

    const first_root_digest = first.typeDigest(&name_store, first_root);
    const second_root_digest = second.typeDigest(&name_store, second_root);
    try std.testing.expectEqualSlices(u8, first_root_digest.bytes[0..], second_root_digest.bytes[0..]);

    const first_spec = first.specializationDigest(&name_store, first_root);
    const second_spec = second.specializationDigest(&name_store, second_root);
    try std.testing.expectEqualSlices(u8, first_spec.bytes[0..], second_spec.bytes[0..]);
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
