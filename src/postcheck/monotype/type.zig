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
            .iterator_interface_cache = .empty,
            .iterator_interface_pending = .empty,
            .iterator_interface_visited = .empty,
            .iterator_interface_visit_epochs = .empty,
            .iterator_interface_visit_epoch = 0,
            .recursive_digest_unfoldings = std.AutoHashMap([32]u8, names.TypeDigest).init(allocator),
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
        for (self.constructing.unsafeRawItemsForView()) |unfinished| {
            if (unfinished) Common.invariant("cannot freeze Monotype types with an unfinished reserved slot");
        }
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
                        try self.iterator_interface_pending.append(self.allocator, GuardedList.at(record_fields, index).ty);
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
        self.iterator_interface_cache.restoreLen(mark_.iterator_interface_cache_len);
        // A surviving reserved slot may have been filled after the mark with
        // children that are now truncated and whose ids can be reused. Clear
        // every retained containment answer so those new children are walked.
        // Digest caches need no equivalent clearing only because no caller
        // digests a mid-transaction fill before its rollback decision; the
        // unfolding index is content-addressed and stays valid regardless.
        @memset(self.iterator_interface_cache.unsafeRawItemsMutForStore(), null);
        self.iterator_interface_visit_epochs.restoreLen(mark_.iterator_interface_visit_epochs_len);
        self.spans.restoreLen(mark_.spans_len);
        self.fields.restoreLen(mark_.fields_len);
        self.tags.restoreLen(mark_.tags_len);
        self.declared_fields.restoreLen(mark_.declared_fields_len);
    }

    pub fn ownerHead(self: *const Store, ty: TypeId) OwnerHead {
        return switch (self.get(ty)) {
            .primitive => |primitive| .{ .builtin = builtinOwner(primitive) },
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

/// Mutable builder for immutable Monotype type nodes.
///
/// The interner is child-first for acyclic types: callers provide
/// already-interned child `TypeId`s, and every successful call returns a
/// `TypeId` whose content is not mutated by the interner afterwards. Recursive
/// roots are sealed through `internRecursiveRoot`, which keeps the temporary
/// back-reference slots private until the root has immutable content and a
/// digest/equality bucket.
const InternerState = struct {
    allocator: std.mem.Allocator,
    name_store: *const names.NameStore,
    store: Store,
    by_digest: std.AutoHashMap(InternerLookupDigest, std.ArrayList(TypeId)),
};

/// Opaque builder handle for interning immutable Monotype type ids.
pub const Interner = opaque {
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

    pub fn init(allocator: std.mem.Allocator, name_store: *const names.NameStore) std.mem.Allocator.Error!*Interner {
        const state_ = try allocator.create(InternerState);
        state_.* = .{
            .allocator = allocator,
            .name_store = name_store,
            .store = Store.init(allocator),
            .by_digest = std.AutoHashMap(InternerLookupDigest, std.ArrayList(TypeId)).init(allocator),
        };
        return @ptrCast(state_);
    }

    pub fn deinit(self: *Interner) void {
        const state_ = self.state();
        var lists = state_.by_digest.valueIterator();
        while (lists.next()) |list| list.deinit(state_.allocator);
        state_.by_digest.deinit();
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
        const store_ = self.store();
        const mark_ = store_.mark();
        const ty = try store_.add(.{ .primitive = primitive });
        return try self.internCandidate(mark_, ty);
    }

    pub fn internZst(self: *Interner) std.mem.Allocator.Error!TypeId {
        const store_ = self.store();
        const mark_ = store_.mark();
        const ty = try store_.add(.zst);
        return try self.internCandidate(mark_, ty);
    }

    pub fn internList(self: *Interner, elem: TypeId) std.mem.Allocator.Error!TypeId {
        const store_ = self.store();
        const mark_ = store_.mark();
        const ty = try store_.add(.{ .list = elem });
        return try self.internCandidate(mark_, ty);
    }

    pub fn internBox(self: *Interner, elem: TypeId) std.mem.Allocator.Error!TypeId {
        const store_ = self.store();
        const mark_ = store_.mark();
        const ty = try store_.add(.{ .box = elem });
        return try self.internCandidate(mark_, ty);
    }

    pub fn internTuple(self: *Interner, items: []const TypeId) std.mem.Allocator.Error!TypeId {
        const store_ = self.store();
        const mark_ = store_.mark();
        const span_ = try store_.addSpan(items);
        const ty = try store_.add(.{ .tuple = span_ });
        return try self.internCandidate(mark_, ty);
    }

    pub fn internFunc(self: *Interner, args: []const TypeId, ret: TypeId) std.mem.Allocator.Error!TypeId {
        const store_ = self.store();
        const mark_ = store_.mark();
        const span_ = try store_.addSpan(args);
        const ty = try store_.add(.{ .func = .{ .args = span_, .ret = ret } });
        return try self.internCandidate(mark_, ty);
    }

    pub fn internRecord(self: *Interner, raw_fields: []const Field) std.mem.Allocator.Error!TypeId {
        const state_ = self.state();
        const mark_ = state_.store.mark();
        const span_ = try state_.store.addRecordFields(state_.name_store, raw_fields);
        const ty = try state_.store.add(.{ .record = span_ });
        return try self.internCandidate(mark_, ty);
    }

    pub const TagInput = struct {
        name: names.TagNameId,
        checked_name: names.TagNameId,
        payloads: []const TypeId,
    };

    pub fn internTagUnion(self: *Interner, raw_tags: []const TagInput) std.mem.Allocator.Error!TypeId {
        const state_ = self.state();
        const mark_ = state_.store.mark();
        errdefer state_.store.restore(mark_);

        const lowered = try state_.allocator.alloc(Tag, raw_tags.len);
        defer state_.allocator.free(lowered);
        for (raw_tags, 0..) |tag, index| {
            lowered[index] = .{
                .name = tag.name,
                .checked_name = tag.checked_name,
                .payloads = try state_.store.addSpan(tag.payloads),
            };
        }

        const span_ = try state_.store.addTagVariants(state_.name_store, lowered);
        const ty = try state_.store.add(.{ .tag_union = span_ });
        return try self.internCandidate(mark_, ty);
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

    pub fn internNamed(self: *Interner, named: NamedInput) std.mem.Allocator.Error!TypeId {
        const store_ = self.store();
        const mark_ = store_.mark();
        errdefer store_.restore(mark_);

        const args = try store_.addSpan(named.args);
        const declared_order = try store_.addDeclaredFields(named.declared_order);
        const content: NamedContent = .{
            .named_type = named.named_type,
            .def = named.def,
            .kind = named.kind,
            .builtin_owner = named.builtin_owner,
            .args = args,
            .backing = named.backing,
            .declared_order = declared_order,
        };
        const ty = try store_.add(.{ .named = content });
        return try self.internCandidate(mark_, ty);
    }

    pub fn internErased(self: *Interner, digest: names.TypeDigest) std.mem.Allocator.Error!TypeId {
        const store_ = self.store();
        const mark_ = store_.mark();
        const ty = try store_.add(.{ .erased = digest });
        return try self.internCandidate(mark_, ty);
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
        value_ty: ?RecursiveLink = null,
        kind_state: FieldKindState = .resolved,
        default: ?FieldDefault,
    };

    pub const RecursiveTag = struct {
        name: names.TagNameId,
        checked_name: names.TagNameId,
        payloads: []const RecursiveLink,
    };

    pub const RecursiveNamedBacking = struct {
        ty: RecursiveLink,
        use: BackingUse,
        authority: BackingAuthority = .checked_public,
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
    pub fn internRecursiveRoot(self: *Interner, content: RecursiveContent) std.mem.Allocator.Error!TypeId {
        return try self.internRecursiveGroupRoot(&.{content}, recursiveNodeId(0));
    }

    /// Intern one public root from a private recursive group. Group nodes may
    /// reference each other through `RecursiveLink.node`; only the selected root
    /// is returned to the caller, and it is returned only after every private
    /// node has been filled exactly once.
    pub fn internRecursiveGroupRoot(
        self: *Interner,
        contents: []const RecursiveContent,
        root_node: RecursiveNodeId,
    ) std.mem.Allocator.Error!TypeId {
        if (@intFromEnum(root_node) >= contents.len) {
            Common.invariant("Monotype recursive type group root is outside the group");
        }

        const store_ = self.store();
        const mark_ = store_.mark();
        errdefer store_.restore(mark_);

        const allocator = self.state().allocator;
        const ids = try allocator.alloc(TypeId, contents.len);
        defer allocator.free(ids);

        for (ids) |*id| {
            id.* = try store_.reserveSlot();
        }
        const root = ids[@intFromEnum(root_node)];
        for (contents, 0..) |content, index| {
            const lowered = try self.lowerRecursiveContent(ids, root, content);
            store_.fillReservedSlot(ids[index], lowered);
        }
        return try self.internCandidate(mark_, root);
    }

    fn lowerRecursiveLink(_: *Interner, ids: []const TypeId, root: TypeId, link: RecursiveLink) TypeId {
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
        self: *Interner,
        ids: []const TypeId,
        root: TypeId,
        links: []const RecursiveLink,
    ) std.mem.Allocator.Error!Span {
        if (links.len == 0) return .empty();
        const state_ = self.state();
        const lowered = try state_.allocator.alloc(TypeId, links.len);
        defer state_.allocator.free(lowered);
        for (links, 0..) |link, index| {
            lowered[index] = self.lowerRecursiveLink(ids, root, link);
        }
        return try state_.store.addSpan(lowered);
    }

    fn lowerRecursiveFields(
        self: *Interner,
        ids: []const TypeId,
        root: TypeId,
        fields: []const RecursiveField,
    ) std.mem.Allocator.Error!Span {
        if (fields.len == 0) return .empty();
        const state_ = self.state();
        const lowered = try state_.allocator.alloc(Field, fields.len);
        defer state_.allocator.free(lowered);
        for (fields, 0..) |field, index| {
            lowered[index] = .{
                .name = field.name,
                .ty = self.lowerRecursiveLink(ids, root, field.ty),
                .value_ty = if (field.value_ty) |value_ty| self.lowerRecursiveLink(ids, root, value_ty) else null,
                .kind_state = field.kind_state,
                .default = field.default,
            };
        }
        return try state_.store.addRecordFields(state_.name_store, lowered);
    }

    fn lowerRecursiveTags(
        self: *Interner,
        ids: []const TypeId,
        root: TypeId,
        tags_: []const RecursiveTag,
    ) std.mem.Allocator.Error!Span {
        if (tags_.len == 0) return .empty();
        const state_ = self.state();
        const lowered = try state_.allocator.alloc(Tag, tags_.len);
        defer state_.allocator.free(lowered);
        for (tags_, 0..) |tag, index| {
            lowered[index] = .{
                .name = tag.name,
                .checked_name = tag.checked_name,
                .payloads = try self.lowerRecursiveLinkSpan(ids, root, tag.payloads),
            };
        }
        return try state_.store.addTagVariants(state_.name_store, lowered);
    }

    fn lowerRecursiveNamed(
        self: *Interner,
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
                .authority = backing.authority,
            } else null,
            .declared_order = named.declared_order,
        };
    }

    fn lowerRecursiveContent(
        self: *Interner,
        ids: []const TypeId,
        root: TypeId,
        content: RecursiveContent,
    ) std.mem.Allocator.Error!Content {
        return switch (content) {
            .primitive => |primitive| .{ .primitive = primitive },
            .named => |named| .{ .named = try self.lowerRecursiveNamed(ids, root, named) },
            .record => |fields| .{ .record = try self.lowerRecursiveFields(ids, root, fields) },
            .tuple => |items| .{ .tuple = try self.lowerRecursiveLinkSpan(ids, root, items) },
            .tag_union => |tags_| .{ .tag_union = try self.lowerRecursiveTags(ids, root, tags_) },
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

    fn internCandidate(self: *Interner, mark_: Store.Mark, candidate: TypeId) std.mem.Allocator.Error!TypeId {
        const state_ = self.state();
        errdefer state_.store.restore(mark_);

        const digest = state_.store.typeDigestCached(state_.name_store, candidate, null);
        const key = InternerLookupDigest.from(digest);
        if (state_.by_digest.getPtr(key)) |bucket| {
            for (bucket.items) |existing| {
                if (try state_.store.typeEql(state_.name_store, existing, candidate)) {
                    state_.store.restore(mark_);
                    return existing;
                }
            }
            try bucket.append(state_.allocator, candidate);
            return candidate;
        }

        var bucket = std.ArrayList(TypeId).empty;
        errdefer bucket.deinit(state_.allocator);
        try bucket.append(state_.allocator, candidate);
        try state_.by_digest.put(key, bucket);
        return candidate;
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

fn builtinOwner(primitive: Primitive) static_dispatch.BuiltinOwner {
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
        .u8x16 => .u8x16,
        .i8x16 => .i8x16,
        .u16x8 => .u16x8,
        .i16x8 => .i16x8,
        .u32x4 => .u32x4,
        .i32x4 => .i32x4,
        .u64x2 => .u64x2,
        .i64x2 => .i64x2,
    };
}

test "monotype type declarations are referenced" {
    std.testing.refAllDecls(@This());
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
        .{ .name = b_field, .ty = unit, .default = null },
        .{ .name = a_field, .ty = unit, .default = null },
    });
    const second_record = try interner.internRecord(&.{
        .{ .name = a_field, .ty = unit, .default = null },
        .{ .name = b_field, .ty = unit, .default = null },
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

test "monotype backing-less aliases digest by their own identity" {
    // Backing-less aliases used to be the one theoretical digest collision:
    // every one of them digested as "alias-without-backing". Aliases are now
    // opaque named nodes, so differently named backing-less aliases carry
    // distinct digests and stay distinct interner entries.
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
    try std.testing.expect(!std.mem.eql(u8, first_digest.bytes[0..], second_digest.bytes[0..]));
    try std.testing.expect(first != second);
}

test "monotype type interner seals recursive root before exposing type id" {
    var name_store = names.NameStore.init(std.testing.allocator);
    defer name_store.deinit();

    const field_name = try name_store.internRecordFieldLabel("next");

    const interner = try Interner.init(std.testing.allocator, &name_store);
    defer interner.deinit();

    const root = try interner.internRecursiveRoot(.{ .record = &.{
        .{ .name = field_name, .ty = .root, .default = null },
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
        .{ .name = field_name, .ty = .root, .default = null },
    } });
    const second = try interner.internRecursiveRoot(.{ .record = &.{
        .{ .name = field_name, .ty = .root, .default = null },
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
            .{ .name = field_name, .ty = .{ .node = func_node }, .default = null },
        } },
        .{ .func = .{
            .args = &.{},
            .ret = .{ .node = record_node },
        } },
    }, record_node);
    const second = try interner.internRecursiveGroupRoot(&.{
        .{ .record = &.{
            .{ .name = field_name, .ty = .{ .node = func_node }, .default = null },
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
        .{ .name = next_name, .ty = .root, .default = null },
    } });
    const recursive_with_bool = try interner.internRecursiveRoot(.{ .record = &.{
        .{ .name = next_name, .ty = .root, .default = null },
        .{ .name = done_name, .ty = .{ .interned = bool_ty }, .default = null },
    } });

    try std.testing.expect(recursive_only != recursive_with_bool);
    try std.testing.expect(!try interner.typeEql(recursive_only, recursive_with_bool));
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
