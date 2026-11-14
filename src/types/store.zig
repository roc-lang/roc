//! The store of solved types
//! Contains both Slot & Descriptor stores

const std = @import("std");
const builtin = @import("builtin");
const tracy = @import("tracy");
const base = @import("base");
const collections = @import("collections");
const types = @import("types.zig");
const debug = @import("debug.zig");
const instantiate = @import("instantiate.zig");
const use_absorbers = @import("use_absorbers.zig");

const Allocator = std.mem.Allocator;

/// Compile-time switch selecting whether the savepoint trail can be
/// cross-checked against a full copy of the store.
///
/// - `.savepoint_only` (production): rollback trusts the savepoint's undo trail
///   alone. The copy, the cross-check assert, and the savepoint's copy field are
///   all compiled away—zero code, zero state.
/// - `.clone_crosscheck` (test builds): the full-copy cross-check is compiled
///   in, and a test can opt an individual savepoint into it via
///   `createSavepointVerifying`. Savepoints created the normal way still copy
///   nothing, so the suite runs the same savepoint-only path production uses.
const SavepointVerification = enum { savepoint_only, clone_crosscheck };
const savepoint_verification: SavepointVerification =
    if (builtin.is_test) .clone_crosscheck else .savepoint_only;

/// One journaled in-place write to a pre-existing slot (for trail rollback).
const SlotUndo = struct { idx: SlotStore.Idx, old: Slot };
/// One journaled in-place write to a pre-existing descriptor, holding the
/// stored row it replaced.
const DescUndo = struct { idx: DescStore.Idx, old: Desc.Row };
/// One journaled in-place write to a pre-existing equivalence-class root.
const RootMetaUndo = struct { idx: DescStore.Idx, old: RootMeta };
/// One journaled in-place write to a structural union rank.
const UnionRankUndo = struct { idx: SlotStore.Idx, old: u8 };

const Desc = types.Descriptor;
const Var = types.Var;
const Content = types.Content;
const Rank = types.Rank;
const Flex = types.Flex;
const Rigid = types.Rigid;
const RecordField = types.RecordField;
const TagUnion = types.TagUnion;
const Tag = types.Tag;
const VarSafeList = collections.ReservedList(Var, .dense);
const RecordFieldSafeMultiList = RecordField.SafeMultiList;
const TagSafeMultiList = Tag.SafeMultiList;
const Descriptor = types.Descriptor;
const TypeIdent = types.TypeIdent;
const Alias = types.Alias;
const FlatType = types.FlatType;
const NominalType = types.NominalType;
const NominalDecl = types.NominalDecl;
const StaticDispatchConstraint = types.StaticDispatchConstraint;
const InterpolationPartMetadata = types.InterpolationPartMetadata;
const InterpolationMetadata = StaticDispatchConstraint.InterpolationMetadata;
const NumeralInfo = types.NumeralInfo;
const SourceDecl = types.SourceDecl;
const DerivedMapPlan = types.DerivedMapPlan;
const NominalIdentity = types.NominalIdentity;
const AliasIdentity = types.AliasIdentity;

/// Metadata belonging to a live union-find storage root.
///
/// This is indexed by the descriptor named by the root slot. Descriptors that
/// became orphaned after a union retain stale metadata which is never observed.
/// Keeping it beside (rather than inside) Descriptor keeps structural solver
/// state out of checked type descriptors and stores its fields densely.
const RootMeta = struct {
    /// The checked representative returned to every type-store consumer.
    checked_var: Var,
};

const RootMetaSafeMultiList = collections.ReservedColumns(RootMeta, .dense);
const UnionRankSafeList = collections.ReservedList(u8, .dense);

/// A variable & its descriptor info
pub const ResolvedVarDesc = struct {
    /// The checked representative of this equivalence class. This need not be
    /// the private union-find storage root.
    var_: Var,
    /// Whether the queried variable is the checked representative.
    is_root: bool,
    desc_idx: DescStore.Idx,
    desc: Desc,
};

/// Two variables & descs
pub const ResolvedVarDescs = struct { a: ResolvedVarDesc, b: ResolvedVarDesc };

/// One entry in the store's sorted nominal-declaration lookup index: a
/// declaration key (origin module identity, source statement) mapped to the
/// declaration's stable index in `Store.nominal_decls`. The index list is kept
/// sorted by key so lookups are a binary search; declarations themselves are
/// append-only so `NominalDecl.Idx` values stay stable across registrations.
const NominalDeclIndexEntry = struct {
    origin_module: base.ModuleIdentity.Idx,
    statement: u32,
    decl: NominalDecl.Idx,

    const SafeList = collections.SafeList(@This());

    /// Total order over declaration keys: origin module identity index first,
    /// then statement.
    fn orderByKey(origin_module: base.ModuleIdentity.Idx, statement: u32, entry: @This()) std.math.Order {
        const lhs_origin = @backingInt(origin_module);
        const rhs_origin = @backingInt(entry.origin_module);
        if (lhs_origin != rhs_origin) return std.math.order(lhs_origin, rhs_origin);
        return std.math.order(statement, entry.statement);
    }
};

/// One class's entry in `Store.derived_map_plans`.
const DerivedMapPlanEntry = struct {
    class: DescStore.Idx,
    plan: DerivedMapPlan,

    const SafeList = collections.SafeList(@This());
};

/// Watched class descriptors. A write to one advances `generation` and drops
/// every watch, so each memo that recorded the generation it read under sees
/// the change, whichever owner registered the written class.
const ClassWatch = struct {
    watched: std.DynamicBitSetUnmanaged = .{},
    watched_list: std.ArrayListUnmanaged(u32) = .empty,
    generation: u64 = 0,
    /// Whether the watches belong to an earlier generation.
    stale: bool = false,

    fn deinit(self: *ClassWatch, gpa: Allocator) void {
        self.watched.deinit(gpa);
        self.watched_list.deinit(gpa);
    }

    fn noteWrite(self: *ClassWatch, desc_idx: DescStore.Idx) void {
        if (self.stale) return;
        const index: u32 = @backingInt(desc_idx);
        if (index < self.watched.bit_length and self.watched.isSet(index)) self.advance();
    }

    fn noteRollback(self: *ClassWatch) void {
        if (!self.stale and self.watched_list.items.len != 0) self.advance();
    }

    fn advance(self: *ClassWatch) void {
        self.generation += 1;
        self.stale = true;
    }
};

/// Descriptors a scan recorded as settled: the content reachable from each
/// one has been found to need no further work by that scan. Every descriptor
/// reachable from a settled one is settled too, so the set stays exact as long
/// as no settled descriptor's observable content changes; any such change, and
/// any rollback, empties the whole set.
const SettledDescs = struct {
    bits: std.DynamicBitSetUnmanaged = .{},
    list: std.ArrayListUnmanaged(u32) = .empty,

    fn deinit(self: *SettledDescs, gpa: Allocator) void {
        self.bits.deinit(gpa);
        self.list.deinit(gpa);
    }

    fn contains(self: *const SettledDescs, desc_idx: DescStore.Idx) bool {
        const index: u32 = @backingInt(desc_idx);
        return index < self.bits.bit_length and self.bits.isSet(index);
    }

    fn insert(self: *SettledDescs, gpa: Allocator, desc_idx: DescStore.Idx) Allocator.Error!void {
        const index: u32 = @backingInt(desc_idx);
        if (index >= self.bits.bit_length) try self.bits.resize(gpa, @max(index + 1, self.bits.bit_length * 2), false);
        if (self.bits.isSet(index)) return;
        try self.list.append(gpa, index);
        self.bits.set(index);
    }

    fn clear(self: *SettledDescs) void {
        for (self.list.items) |index| self.bits.unset(index);
        self.list.clearRetainingCapacity();
    }
};

/// Reperents either type data *or* a symlink to another type variable
pub const Slot = union(SlotKind) {
    root: DescStore.Idx,
    redirect: Var,
};

/// One static-dispatch relation the unifier folded into a retained same-name
/// relation. Both are raw constraint function vars; unification has already
/// made their callables equal.
pub const StaticDispatchRelationMerge = struct {
    dropped_fn_var: Var,
    retained_fn_var: Var,
};

/// How a store loaded from serialized bytes may be used.
pub const LoadedAccess = enum {
    /// A solved module's types, only ever read.
    sealed,
    /// A canonicalized module's types, which checking solves further.
    writable,
};

/// The store of all type variables and their descriptors.
///
/// Each type variable (`Var`) points to a Slot.
/// A Slot either redirects to a different slot or contains type `Content`.
///
/// Var maps to a SlotStore.Idx internally.
pub const Store = struct {
    const Self = @This();

    gpa: Allocator,

    /// Type variable storage
    slots: SlotStore,
    descs: DescStore,
    /// Checked class representatives, indexed in lockstep with `descs`.
    root_metas: RootMetaSafeMultiList,
    /// Structural union rank for every slot. Keeping it separate from class
    /// descriptors lets explicit error recovery re-root a class without
    /// coupling storage-parent selection to checked descriptor identity.
    union_ranks: UnionRankSafeList,

    /// Storage for compound type parts
    vars: VarSafeList,
    record_fields: RecordFieldSafeMultiList,
    tags: TagSafeMultiList,
    interpolation_parts: InterpolationPartMetadata.SafeList,
    numeral_infos: NumeralInfo.SafeList,
    /// Interpolation metadata for `from_interpolation` constraints, held out
    /// of line so a constraint stays a few words. Entries are never mutated
    /// after appending, so constraint copies share them.
    interpolation_metadata: InterpolationMetadata.MetadataList,
    static_dispatch_constraints: StaticDispatchConstraint.SafeList,

    /// The nominal declaration table: one entry per nominal declaration whose
    /// applications can appear in this store (local declarations plus every
    /// imported declaration copied in by `copy_import`). Append-only, so
    /// `NominalDecl.Idx` values are stable; keyed lookups go through the
    /// sorted `nominal_decl_index`.
    nominal_decls: NominalDecl.SafeList,
    /// Sorted (origin module identity, statement) -> declaration index. Kept
    /// sorted on insert; lookups binary-search.
    nominal_decl_index: NominalDeclIndexEntry.SafeList,
    /// False only while no entry of `nominal_decls` has ever been invalid.
    /// Serialized with the store, so a loaded store carries the value of the
    /// store it was written from.
    invalid_nominal_decl_written: bool = true,

    /// The payload each accepted derived `map`/`map!` relation selects, by the
    /// class of the relation's callable, sorted by class descriptor so lookups
    /// binary-search. The selection is a fact about that equivalence class: a
    /// merge keeps it, a member detached from the class keeps it, and a copy
    /// of a relation's callable carries it.
    derived_map_plans: DerivedMapPlanEntry.SafeList = .{},
    /// The sorted, distinct name hashes (`use_absorbers.nameHash`) of every
    /// optional or defaulted field a record of this store holds, filled when
    /// the store is sealed. A module checking against this store reads them
    /// as the fields a relation can widen a record with (design.md
    /// "Anonymous Structure Is Copied Per Use").
    widening_names: collections.SafeList(u64) = .{},

    /// The identity of every nominal type application and alias a descriptor
    /// of this store holds, one row per distinct identity: a stored nominal or
    /// alias descriptor names its row (`types.Descriptor.encode`). Append-only;
    /// a probe's rollback keeps the rows it interned, which stay valid
    /// identities whether or not a descriptor names them.
    nominal_identities: NominalIdentity.SafeList = .{},
    alias_identities: AliasIdentity.SafeList = .{},
    /// Which row holds each identity, covering the first
    /// `nominal_identities_indexed` rows (and likewise for aliases); the rest
    /// are indexed before the next lookup. Runtime-only: never serialized,
    /// cloned, or relocated.
    nominal_identity_index: std.AutoHashMapUnmanaged(NominalIdentity, u32) = .empty,
    nominal_identities_indexed: u32 = 0,
    alias_identity_index: std.AutoHashMapUnmanaged(AliasIdentity, u32) = .empty,
    alias_identities_indexed: u32 = 0,
    /// The rows most recently looked up, in front of the indexes above: a
    /// store's descriptors name few distinct identities, written over and
    /// over. Runtime-only, like the indexes.
    nominal_identity_memo: IdentityMemo(NominalIdentity) = .{},
    alias_identity_memo: IdentityMemo(AliasIdentity) = .{},

    /// Reusable worklist buffers for `instantiate.Instantiator`'s explicit
    /// graph-copy machine. Runtime-only scratch: never serialized, cloned, or
    /// relocated; capacity persists across instantiations until sealing.
    instantiate_scratch: instantiate.Scratch = .{},

    /// For each frozen class (see `DescriptorFlags.frozen`), keyed by its
    /// descriptor, the variable it was frozen at. A merge leaves a frozen
    /// class's descriptor in place but may hand its checked representative to
    /// another member; a write aimed at that member hands the class back to
    /// this anchor before detaching the member. Runtime-only: frozen classes
    /// are thawed before a store is serialized or cloned.
    frozen_anchors: collections.DenseMap(DescStore.Idx, Var),
    /// The members of the frozen class `detachOccurrence` is splitting.
    /// Runtime-only scratch, reused across detaches.
    detach_members_scratch: std.ArrayListUnmanaged(Var) = .empty,

    /// Classes callers' memos depend on. Runtime-only: never serialized,
    /// cloned, or relocated.
    class_watch: ClassWatch = .{},

    /// Descriptors a scan recorded as settled (see `SettledDescs`).
    /// Runtime-only: never serialized, cloned, or relocated.
    settled_descs: SettledDescs = .{},
    /// Set while `union_` relinks storage roots whose observable content it
    /// has already compared, so the relinking itself settles nothing away.
    relinking_compared_classes: bool = false,

    /// Advances whenever a write could make a variable-free type reach a
    /// variable or an error: a class merged into a leaf from concrete
    /// content, a poisoned occurrence, or a direct content rewrite or
    /// redirect. While it is unchanged, a type the solver found
    /// variable-free still is. Runtime-only: never serialized, cloned, or
    /// relocated.
    ground_epoch: u64 = 0,

    /// Undo trail for speculative unification. While a probe is active
    /// (`savepoint_active`), every in-place write to a slot, descriptor, checked
    /// representative, or structural rank that existed before the probe began
    /// is journaled as (index, old value); rollback replays the journal in
    /// reverse. Entries appended during the probe are undone by truncation, not
    /// journaled.
    /// Probes never nest (they bracket leaf-level unification), so this is a
    /// flag, not a depth.
    savepoint_active: bool = false,
    /// Set once checking has solved this store (see `seal`). Every write to a
    /// sealed store is a violated compiler invariant.
    sealed: bool = false,
    savepoint_baseline_slots: u32 = 0,
    savepoint_baseline_descs: u32 = 0,
    slot_trail: std.ArrayListUnmanaged(SlotUndo) = .empty,
    /// Advances whenever a slot write can change which class a var resolves
    /// to, so a caller that indexed resolved classes knows when to re-index.
    slot_generation: u64 = 0,
    desc_trail: std.ArrayListUnmanaged(DescUndo) = .empty,
    root_meta_trail: std.ArrayListUnmanaged(RootMetaUndo) = .empty,
    union_rank_trail: std.ArrayListUnmanaged(UnionRankUndo) = .empty,
    /// `derived_map_plans` as it stood when the open probe first changed it,
    /// valid while `derived_map_plans_journaled`; rollback restores it.
    derived_map_plans_undo: std.ArrayListUnmanaged(DerivedMapPlanEntry) = .empty,
    derived_map_plans_journaled: bool = false,

    /// Descriptors no slot names any more: a class merge keeps one side's
    /// descriptor and orphans the other's. The next class created takes the
    /// most recently freed one (and its `root_metas` entry) instead of
    /// appending, so the descriptor store does not grow with dead entries.
    /// Runtime-only: never serialized, cloned, or relocated.
    free_descs: std.ArrayListUnmanaged(DescStore.Idx) = .empty,
    /// How many entries at the end of `free_descs` the open probe has
    /// reissued. A probe takes without removing, so that rollback can hand
    /// them back by forgetting the count; commit removes them.
    probe_taken_descs: u32 = 0,
    /// Descriptors orphaned while a probe is open. Rollback restores the slots
    /// that named them, so they are freed only when the probe commits.
    pending_free_descs: std.ArrayListUnmanaged(DescStore.Idx) = .empty,

    /// Every static-dispatch relation the unifier folded into a retained
    /// same-name relation, by raw constraint function var. Checking publishes
    /// these so a folded relation's dispatch names the retained relation's
    /// target instantiation. Sealing releases this journal after checking has
    /// published the selected relations into ModuleEnv.
    static_dispatch_relation_merges: std.ArrayListUnmanaged(StaticDispatchRelationMerge) = .empty,
    /// Unused room reserved directly after a constraint range, keyed by the
    /// range's end and holding the room's end, so a receiver whose
    /// requirements grow one use at a time extends its own list in place
    /// rather than copying it (`appendStaticDispatchConstraintsAfter`). Room
    /// is neither reserved nor used while a savepoint is active, so a
    /// rollback never has room to forget.
    static_dispatch_constraint_room: std.AutoHashMapUnmanaged(u32, u32) = .empty,

    /// Relocate the self-contained type store and its explicit nominal lookup index.
    pub fn rebindSourceReferences(store: *Store, mapper: anytype) @TypeOf(mapper.*).Error!void {
        store.debugAssertUnsealed();
        // A nominal type's or alias's source statement lives in its identity
        // row; a defaulted field presence's default lives in its descriptor.
        for (store.nominal_identities.items()) |*identity| {
            if (identity.source.present) identity.source.statement = @intCast(try mapper.node(identity.origin_module, identity.source.statement));
        }
        for (store.alias_identities.items()) |*identity| {
            if (identity.source_decl.present) identity.source_decl.statement = @intCast(try mapper.node(identity.origin_module, identity.source_decl.statement));
        }
        store.forgetIdentityIndexes();
        for (0..store.descs.backing.len()) |i| {
            const desc_idx: DescStore.Idx = @fromBackingInt(@intCast(i));
            if (store.descs.getWord(desc_idx).kind != .field_presence) continue;
            var desc = store.descAt(desc_idx);
            try rebindSourceReferenceValue(&desc.content, mapper);
            store.descs.setRow(desc_idx, store.encodeDescWithoutIdentity(desc));
        }
        for (store.nominal_decls.items()) |*decl| try rebindSourceReferenceValue(decl, mapper);
        for (store.nominal_decl_index.items()) |*entry| entry.statement = try mapper.node(entry.origin_module, entry.statement);
        const Entry = @TypeOf(store.nominal_decl_index.items()[0]);
        std.mem.sort(Entry, store.nominal_decl_index.items(), {}, struct {
            fn lessThan(_: void, a: Entry, b: Entry) bool {
                return if (a.origin_module == b.origin_module) a.statement < b.statement else @backingInt(a.origin_module) < @backingInt(b.origin_module);
            }
        }.lessThan);
        for (0..store.record_fields.len()) |i| {
            var field = store.record_fields.get(@fromBackingInt(@intCast(i)));
            try rebindSourceReferenceValue(&field, mapper);
            store.record_fields.set(@fromBackingInt(@intCast(i)), field);
        }
    }

    /// Init the unification table with default capacity.
    pub fn init(gpa: Allocator) std.mem.Allocator.Error!Self {
        return try Self.initCapacity(gpa, 1024, 512);
    }

    /// Init the type store with capacity heuristics based on source file size.
    /// Larger source files typically need more type slots and variables.
    ///
    /// Heuristics based on typical Roc code patterns:
    /// - ~1 type slot per 50 bytes of source
    /// - ~1 child element (vars, tags, record fields) per 100 bytes
    pub fn initFromSourceLen(gpa: Allocator, source_len: usize) std.mem.Allocator.Error!Self {
        const root_capacity = @max(2048, @min(50_000, source_len / 50));
        const child_capacity = @max(512, @min(10_000, source_len / 100));
        return try Self.initCapacity(gpa, root_capacity, child_capacity);
    }

    /// Init the unification table
    pub fn initCapacity(gpa: Allocator, root_capacity: usize, child_capacity: usize) std.mem.Allocator.Error!Self {
        var result: Self = .{
            .gpa = gpa,
            .frozen_anchors = .init(gpa),

            // slots & descriptors
            .descs = .{ .backing = .{} },
            .slots = .{ .payloads = .{}, .kinds = .{} },
            .root_metas = .{},
            .union_ranks = .{},

            // everything else
            .vars = .{},
            .record_fields = .{},
            .tags = .{},
            .interpolation_parts = .{},
            .numeral_infos = .{},
            .interpolation_metadata = .{},
            .static_dispatch_constraints = .{},

            // nominal declaration table (modules typically declare few types)
            .nominal_decls = .{},
            .nominal_decl_index = .{},
            .invalid_nominal_decl_written = false,
        };
        errdefer result.deinit();
        result.descs = try DescStore.init(gpa, root_capacity);
        result.slots = try SlotStore.init(gpa, root_capacity);
        result.root_metas = try RootMetaSafeMultiList.initCapacity(gpa, root_capacity);
        result.union_ranks = try UnionRankSafeList.initCapacity(gpa, root_capacity);
        result.vars = try VarSafeList.initCapacity(gpa, child_capacity);
        result.record_fields = try RecordFieldSafeMultiList.initCapacity(gpa, child_capacity);
        result.tags = try TagSafeMultiList.initCapacity(gpa, child_capacity);
        result.interpolation_parts = try InterpolationPartMetadata.SafeList.initCapacity(gpa, child_capacity);
        result.numeral_infos = try NumeralInfo.SafeList.initCapacity(gpa, child_capacity);
        result.static_dispatch_constraints = try StaticDispatchConstraint.SafeList.initCapacity(gpa, child_capacity);
        result.nominal_decls = try NominalDecl.SafeList.initCapacity(gpa, 16);
        result.nominal_decl_index = try NominalDeclIndexEntry.SafeList.initCapacity(gpa, 16);
        return result;
    }

    /// Make this store read-only: checking has solved it, and every later
    /// stage only reads it. Sealing happens at a quiescent point, with no
    /// savepoint open and no class frozen. All checked outputs have been
    /// published; only checker-owned buffers are released at this boundary.
    pub fn seal(self: *Self) void {
        if (std.debug.runtime_safety) {
            if (self.sealed) base.invariant("a type store was sealed twice", .{});
            if (self.savepoint_active) base.invariant("a type store was sealed with a savepoint open", .{});
            if (self.frozen_anchors.count() != 0) base.invariant("a type store was sealed with a frozen class", .{});
        }
        self.sealGrowingColumns();
        self.releaseCheckerBuffers();
        self.sealed = true;
    }

    /// Forbid growth of every growing column.
    fn sealGrowingColumns(self: *Self) void {
        self.slots.payloads.seal();
        self.slots.kinds.seal();
        self.descs.backing.seal();
        self.root_metas.seal();
        self.union_ranks.seal();
        self.vars.seal();
        self.record_fields.seal();
        self.tags.seal();
    }

    /// Canonicalize the solved store (`canonicalizeForSeal`), then seal it.
    /// Checking calls this once, when it finishes a module.
    pub fn canonicalizeAndSeal(self: *Self, idents: *const base.Ident.Store) Allocator.Error!void {
        try self.canonicalizeForSeal(idents);
        try self.collectWideningNames(idents);
        self.seal();
    }

    /// Fill `widening_names` from the field table, which holds every field of
    /// every record of the store. A field of a record no longer reachable only
    /// adds a name, which can only make a structure look changeable.
    fn collectWideningNames(self: *Self, idents: *const base.Ident.Store) Allocator.Error!void {
        var hashes: std.ArrayListUnmanaged(u64) = .empty;
        defer hashes.deinit(self.gpa);
        for (0..self.record_fields.len()) |index| {
            const field = self.record_fields.get(@fromBackingInt(@intCast(index)));
            const may_widen = switch (field.presence.decode()) {
                .required => false,
                .unknown => |unknown| switch (self.resolveVar(unknown.presence).desc.content) {
                    .field_presence => |kind| kind != .required,
                    .flex, .rigid, .alias, .structure, .err => false,
                },
            };
            if (may_widen) try hashes.append(self.gpa, use_absorbers.nameHash(idents.getText(field.name)));
        }
        std.mem.sortUnstable(u64, hashes.items, {}, std.sort.asc(u64));
        var distinct: usize = 0;
        for (hashes.items) |hash| {
            if (distinct != 0 and hashes.items[distinct - 1] == hash) continue;
            hashes.items[distinct] = hash;
            distinct += 1;
        }
        self.widening_names.deinit(self.gpa);
        self.widening_names = try collections.SafeList(u64).initCapacity(self.gpa, distinct);
        for (hashes.items[0..distinct]) |hash| _ = try self.widening_names.append(self.gpa, hash);
    }

    /// The last write to a solved store. Every slot comes to name its
    /// class's storage root directly; every variable a descriptor, a type
    /// argument or field pool, or a nominal declaration holds becomes its
    /// class's checked representative; and every record and tag union row
    /// absorbs the rows its extension chain reaches, so a row's extension is
    /// never itself a row. A reader of the sealed store therefore receives
    /// representatives as children and reads each row in one piece. A
    /// static-dispatch constraint keeps its own function variable: checking
    /// recorded that variable at the dispatch site, so it identifies the
    /// constraint, and a reader resolves it (one slot read).
    fn canonicalizeForSeal(self: *Self, idents: *const base.Ident.Store) Allocator.Error!void {
        self.debugAssertUnsealed();
        if (std.debug.runtime_safety and self.savepoint_active) base.invariant("a type store was canonicalized with a savepoint open", .{});

        const slot_count: usize = @intCast(self.slots.len());
        var live_descs = try std.DynamicBitSetUnmanaged.initEmpty(self.gpa, self.descs.backing.len());
        defer live_descs.deinit(self.gpa);
        for (0..slot_count) |index| {
            const slot_idx: SlotStore.Idx = @fromBackingInt(@intCast(index));
            const storage = self.resolveStorageRoot(slotIdxToVar(slot_idx));
            if (storage.storage_var != slotIdxToVar(slot_idx)) self.slots.set(slot_idx, .{ .redirect = storage.storage_var });
            // No generalization pass is running, so no stored word is marked.
            std.debug.assert(!self.descs.getWord(storage.desc_idx).marked());
            live_descs.set(@backingInt(storage.desc_idx));
        }

        for (self.vars.items) |*var_| {
            if (isRangeLengthSlot(var_.*)) continue;
            var_.* = self.representativeVar(var_.*);
        }
        for (0..self.record_fields.len()) |index| {
            var field = self.record_fields.get(@fromBackingInt(@intCast(index)));
            field.presence.var_ = self.representativeVar(field.presence.var_);
            if (field.presence.presenceVar()) |presence| field.presence.presence_var = self.representativeVar(presence);
            self.record_fields.set(@fromBackingInt(@intCast(index)), field);
        }
        for (self.nominal_decls.items()) |*decl| decl.backing = self.representativeVar(decl.backing);

        var live = live_descs.iterator(.{});
        while (live.next()) |index| {
            const desc_idx: DescStore.Idx = @fromBackingInt(@intCast(index));
            var desc = self.descAt(desc_idx);
            switch (desc.content) {
                .flex, .rigid, .alias, .field_presence, .err => continue,
                .structure => |*flat| switch (flat.*) {
                    .tuple, .nominal_type, .empty_record, .empty_tag_union => continue,
                    .fn_pure, .fn_effectful, .fn_unbound => |*func| func.ret = self.representativeVar(func.ret),
                    .record => |*record| record.ext = self.representativeVar(record.ext),
                    .tag_union => |*tag_union| tag_union.ext = self.representativeVar(tag_union.ext),
                },
            }
            self.descs.setRow(desc_idx, self.encodeDescWithoutIdentity(desc));
        }

        var row_fields: std.ArrayListUnmanaged(RecordField) = .empty;
        defer row_fields.deinit(self.gpa);
        var row_tags: std.ArrayListUnmanaged(Tag) = .empty;
        defer row_tags.deinit(self.gpa);
        live = live_descs.iterator(.{});
        while (live.next()) |index| {
            const desc_idx: DescStore.Idx = @fromBackingInt(@intCast(index));
            var desc = self.descAt(desc_idx);
            const flat = switch (desc.content) {
                .structure => |*flat| flat,
                .flex, .rigid, .alias, .field_presence, .err => continue,
            };
            switch (flat.*) {
                .record => |*record| {
                    if (self.extensionRecord(record.ext) == null) continue;
                    row_fields.clearRetainingCapacity();
                    var segment = record.*;
                    var guard = debug.IterationGuard.init("canonicalizeForSeal.record");
                    while (true) {
                        guard.tick();
                        var fields = self.record_fields.iterRange(segment.fields);
                        while (fields.next()) |field| try row_fields.append(self.gpa, field);
                        segment = self.extensionRecord(segment.ext) orelse break;
                    }
                    std.mem.sort(RecordField, row_fields.items, idents, RecordField.sortByNameAsc);
                    record.* = .{ .fields = try self.record_fields.appendSlice(self.gpa, row_fields.items), .ext = segment.ext };
                },
                .tag_union => |*tag_union| {
                    if (self.extensionTagUnion(tag_union.ext) == null) continue;
                    row_tags.clearRetainingCapacity();
                    var segment = tag_union.*;
                    var guard = debug.IterationGuard.init("canonicalizeForSeal.tag_union");
                    while (true) {
                        guard.tick();
                        var tags = self.tags.iterRange(segment.tags);
                        while (tags.next()) |tag| try row_tags.append(self.gpa, tag);
                        segment = self.extensionTagUnion(segment.ext) orelse break;
                    }
                    std.mem.sort(Tag, row_tags.items, idents, Tag.sortByNameAsc);
                    tag_union.* = .{ .tags = try self.tags.appendSlice(self.gpa, row_tags.items), .ext = segment.ext };
                },
                .tuple, .nominal_type, .empty_record, .empty_tag_union, .fn_pure, .fn_effectful, .fn_unbound => continue,
            }
            self.descs.setRow(desc_idx, self.encodeDescWithoutIdentity(desc));
        }
    }

    /// The checked representative of `var_`'s class.
    fn representativeVar(self: *const Self, var_: Var) Var {
        const representative = self.resolveVar(var_).var_;
        if (std.debug.runtime_safety and @backingInt(representative) >= (@as(u32, 1) << 31)) {
            base.invariant("type variable {d} does not fit below the checked synthesized-type bit", .{@backingInt(representative)});
        }
        return representative;
    }

    /// The record `ext` resolves to, when it is one.
    fn extensionRecord(self: *const Self, ext: Var) ?types.Record {
        return switch (self.resolveVar(ext).desc.content) {
            .structure => |flat| switch (flat) {
                .record => |record| record,
                .tuple, .nominal_type, .empty_record, .empty_tag_union, .fn_pure, .fn_effectful, .fn_unbound, .tag_union => null,
            },
            .flex, .rigid, .alias, .field_presence, .err => null,
        };
    }

    /// The tag union `ext` resolves to, when it is one.
    fn extensionTagUnion(self: *const Self, ext: Var) ?TagUnion {
        return switch (self.resolveVar(ext).desc.content) {
            .structure => |flat| switch (flat) {
                .tag_union => |tag_union| tag_union,
                .tuple, .nominal_type, .empty_record, .empty_tag_union, .fn_pure, .fn_effectful, .fn_unbound, .record => null,
            },
            .flex, .rigid, .alias, .field_presence, .err => null,
        };
    }

    /// Whether checking has solved and sealed this store (see `seal`).
    pub fn isSealed(self: *const Self) bool {
        return self.sealed;
    }

    fn debugAssertUnsealed(self: *const Self) void {
        if (std.debug.runtime_safety and self.sealed) base.invariant("a sealed type store was written", .{});
    }

    /// Ensure that slots & descriptor arrays have at least the provided capacity
    pub fn ensureTotalCapacity(self: *Self, capacity: usize) Allocator.Error!void {
        self.debugAssertUnsealed();
        try self.descs.backing.ensureTotalCapacity(self.gpa, capacity);
        try self.slots.ensureTotalCapacity(self.gpa, capacity);
        try self.root_metas.ensureTotalCapacity(self.gpa, capacity);
        try self.union_ranks.ensureTotalCapacity(self.gpa, capacity);
    }

    /// Release buffers used only while solving. Sealing calls this after the
    /// final canonicalization and checked-output publication; destruction also
    /// calls it for an unfinished store. Reset every owner so destroying a
    /// sealed store is safe. Solved columns and their borrowed slices stay put.
    fn releaseCheckerBuffers(self: *Self) void {
        self.instantiate_scratch.deinit(self.gpa);
        self.instantiate_scratch = .{};
        self.frozen_anchors.deinit();
        self.frozen_anchors = .init(self.gpa);
        self.class_watch.deinit(self.gpa);
        self.class_watch = .{};
        self.settled_descs.deinit(self.gpa);
        self.settled_descs = .{};
        self.detach_members_scratch.deinit(self.gpa);
        self.detach_members_scratch = .empty;
        self.slot_trail.deinit(self.gpa);
        self.slot_trail = .empty;
        self.desc_trail.deinit(self.gpa);
        self.desc_trail = .empty;
        self.root_meta_trail.deinit(self.gpa);
        self.root_meta_trail = .empty;
        self.union_rank_trail.deinit(self.gpa);
        self.union_rank_trail = .empty;
        self.derived_map_plans_undo.deinit(self.gpa);
        self.derived_map_plans_undo = .empty;
        self.free_descs.deinit(self.gpa);
        self.free_descs = .empty;
        self.pending_free_descs.deinit(self.gpa);
        self.pending_free_descs = .empty;
        self.static_dispatch_relation_merges.deinit(self.gpa);
        self.static_dispatch_relation_merges = .empty;
        self.static_dispatch_constraint_room.deinit(self.gpa);
        self.static_dispatch_constraint_room = .empty;
        self.nominal_identity_index.deinit(self.gpa);
        self.nominal_identity_index = .empty;
        self.nominal_identities_indexed = 0;
        self.alias_identity_index.deinit(self.gpa);
        self.alias_identity_index = .empty;
        self.alias_identities_indexed = 0;
        self.nominal_identity_memo = .{};
        self.alias_identity_memo = .{};
    }

    /// Deinit the unification table
    pub fn deinit(self: *Self) void {
        self.releaseCheckerBuffers();
        // slots & descriptors
        self.descs.deinit(self.gpa);
        self.slots.deinit(self.gpa);
        self.root_metas.deinit(self.gpa);
        self.union_ranks.deinit(self.gpa);

        // everything else
        self.vars.deinit(self.gpa);
        self.record_fields.deinit(self.gpa);
        self.tags.deinit(self.gpa);
        self.interpolation_parts.deinit(self.gpa);
        self.numeral_infos.deinit(self.gpa);
        self.interpolation_metadata.deinit(self.gpa);
        self.static_dispatch_constraints.deinit(self.gpa);

        // nominal declaration table
        self.nominal_decls.deinit(self.gpa);
        self.nominal_decl_index.deinit(self.gpa);
        self.derived_map_plans.deinit(self.gpa);
        self.widening_names.deinit(self.gpa);
        self.nominal_identities.deinit(self.gpa);
        self.alias_identities.deinit(self.gpa);
    }

    /// Clone this store into fresh owned memory.
    pub fn clone(self: *const Self, gpa: Allocator) Allocator.Error!Self {
        return .{
            .gpa = gpa,
            .frozen_anchors = .init(gpa),
            .slots = try self.slots.clone(gpa),
            .descs = .{ .backing = try self.descs.backing.clone(gpa), .err_written = self.descs.err_written },
            .root_metas = try self.root_metas.clone(gpa),
            .union_ranks = try self.union_ranks.clone(gpa),
            .vars = try self.vars.clone(gpa),
            .record_fields = try self.record_fields.clone(gpa),
            .tags = try self.tags.clone(gpa),
            .interpolation_parts = try self.interpolation_parts.clone(gpa),
            .numeral_infos = try self.numeral_infos.clone(gpa),
            .interpolation_metadata = try self.interpolation_metadata.clone(gpa),
            .static_dispatch_constraints = try self.static_dispatch_constraints.clone(gpa),
            .nominal_decls = try self.nominal_decls.clone(gpa),
            .nominal_decl_index = try self.nominal_decl_index.clone(gpa),
            .invalid_nominal_decl_written = self.invalid_nominal_decl_written,
            .derived_map_plans = try self.derived_map_plans.clone(gpa),
            .widening_names = try self.widening_names.clone(gpa),
            .nominal_identities = try self.nominal_identities.clone(gpa),
            .alias_identities = try self.alias_identities.clone(gpa),
        };
    }

    /// False only when no variable in this store can reach the error state:
    /// no descriptor has ever held `.err` and no nominal declaration has ever
    /// been invalid (applications of invalid declarations are erroneous).
    pub fn mayContainErrorState(self: *const Self) bool {
        return self.descs.err_written or self.invalid_nominal_decl_written;
    }

    /// Return the number of type variables in the store.
    pub fn len(self: *const Self) u64 {
        return self.slots.len();
    }

    /// Return true when checking left any live type variable in the explicit
    /// error state. Descriptors not referenced by a current slot are rollback
    /// history and do not affect checked output.
    pub fn containsErrContent(self: *const Self) bool {
        for (self.slots.kinds.items, self.slots.payloads.items) |kind, payload| {
            switch (kind) {
                .root => {
                    if (self.descs.getWord(@fromBackingInt(payload)).kind == .err) return true;
                },
                .redirect => {},
            }
        }
        return false;
    }

    // savepoint (create/rollback) for unification //
    //
    // Probe whether two types could unify, then discard the result. The undo
    // trail records each in-place write to a pre-existing slot/descriptor;
    // rollback replays it in reverse and truncates everything appended during
    // the probe. Cost is O(entries the probe mutated), not O(store size).

    /// A handle returned by `createSavepoint`, passed back to
    /// `rollbackToSavepoint`. Captures the rollback-only state: trail position
    /// and the append-only list lengths to rewind to (and, under the
    /// clone cross-check, a full store copy to compare against). The
    /// slot/desc baselines are not here—they live on the store as
    /// `savepoint_baseline_*` because they are also the per-write journaling
    /// threshold.
    pub const Savepoint = struct {
        slot_trail_len: usize,
        desc_trail_len: usize,
        root_meta_trail_len: usize,
        union_rank_trail_len: usize,
        vars_len: usize,
        record_fields_len: usize,
        tags_len: usize,
        interpolation_parts_len: usize,
        numeral_infos_len: usize,
        interpolation_metadata_len: usize,
        static_dispatch_constraints_len: usize,
        static_dispatch_relation_merges_len: usize,
        verify_clone: SavepointVerifyClone = savepoint_verify_clone_init,
    };

    /// Full store copy kept only under the clone cross-check, to assert that the
    /// trail restored all union-find state to exactly its pre-savepoint values.
    const VerifyClone = struct {
        slots: []Slot,
        descs: DescStore.Rows,
        root_metas: RootMetaSafeMultiList,
        union_ranks: []u8,
        derived_map_plans: []DerivedMapPlanEntry,
        fn deinit(self: *VerifyClone, gpa: Allocator) void {
            gpa.free(self.slots);
            self.descs.deinit(gpa);
            self.root_metas.deinit(gpa);
            gpa.free(self.union_ranks);
            gpa.free(self.derived_map_plans);
        }
    };

    /// The `Savepoint.verify_clone` field type: a real optional when the clone
    /// cross-check is compiled in, and a zero-sized `void` otherwise so
    /// production savepoints carry no extra state.
    const SavepointVerifyClone = if (savepoint_verification == .clone_crosscheck) ?VerifyClone else void;
    const savepoint_verify_clone_init: SavepointVerifyClone = if (savepoint_verification == .clone_crosscheck) null else {};

    fn cloneForSavepointVerification(self: *Self) Allocator.Error!VerifyClone {
        const slots = try self.slots.toOwnedSlots(self.gpa);
        errdefer self.gpa.free(slots);

        var descs = try self.descs.backing.clone(self.gpa);
        errdefer descs.deinit(self.gpa);

        var root_metas = try self.root_metas.clone(self.gpa);
        errdefer root_metas.deinit(self.gpa);

        const union_ranks = try self.gpa.dupe(u8, self.union_ranks.items);
        errdefer self.gpa.free(union_ranks);
        const derived_map_plans = try self.gpa.dupe(DerivedMapPlanEntry, self.derived_map_plans.items());
        return .{
            .slots = slots,
            .descs = descs,
            .root_metas = root_metas,
            .union_ranks = union_ranks,
            .derived_map_plans = derived_map_plans,
        };
    }

    /// Open a savepoint over the type store. Pair with `rollbackToSavepoint`.
    /// Rollback relies solely on the undo trail; no store copy is taken, so this
    /// is the path production and the bulk of the test suite run.
    pub fn createSavepoint(self: *Self) Allocator.Error!Savepoint {
        return self.createSavepointImpl(false);
    }

    /// Test-only variant of `createSavepoint` that additionally copies the whole
    /// store, so the matching `rollbackToSavepoint` asserts the trail restored
    /// every piece of union-find state byte-for-byte—i.e. that the savepoint
    /// trail is behaviorally identical to fully copying the store and restoring
    /// the copy.
    /// Only available when the clone cross-check is compiled in (test builds).
    fn createSavepointVerifying(self: *Self) Allocator.Error!Savepoint {
        comptime std.debug.assert(savepoint_verification == .clone_crosscheck);
        return self.createSavepointImpl(true);
    }

    fn createSavepointImpl(self: *Self, comptime take_clone: bool) Allocator.Error!Savepoint {
        self.debugAssertUnsealed();
        const verify_clone: SavepointVerifyClone =
            if (savepoint_verification == .clone_crosscheck and take_clone) vc: {
                break :vc try self.cloneForSavepointVerification();
            } else savepoint_verify_clone_init;

        const savepoint = Savepoint{
            .slot_trail_len = self.slot_trail.items.len,
            .desc_trail_len = self.desc_trail.items.len,
            .root_meta_trail_len = self.root_meta_trail.items.len,
            .union_rank_trail_len = self.union_rank_trail.items.len,
            .vars_len = self.vars.items.len,
            .record_fields_len = self.record_fields.len(),
            .tags_len = self.tags.len(),
            .interpolation_parts_len = self.interpolation_parts.items().len,
            .numeral_infos_len = self.numeral_infos.items().len,
            .interpolation_metadata_len = self.interpolation_metadata.items().len,
            .static_dispatch_constraints_len = self.static_dispatch_constraints.items().len,
            .static_dispatch_relation_merges_len = self.static_dispatch_relation_merges.items.len,
            .verify_clone = verify_clone,
        };

        // Probes never nest; catch it loudly if that invariant is ever broken.
        std.debug.assert(!self.savepoint_active);
        std.debug.assert(self.pending_free_descs.items.len == 0);
        std.debug.assert(self.probe_taken_descs == 0);
        std.debug.assert(!self.derived_map_plans_journaled);
        self.savepoint_active = true;
        self.savepoint_baseline_slots = @intCast(self.slots.len());
        self.savepoint_baseline_descs = @intCast(self.descs.backing.len());

        return savepoint;
    }

    /// Close a savepoint KEEPING everything done since it was created—the
    /// counterpart to `rollbackToSavepoint` for a speculation that succeeded
    /// and is committed in place. The journaled undo entries are dead weight
    /// once nothing will replay them, so the trails shrink back to their
    /// savepoint lengths; the baselines deactivate so in-place writes stop
    /// journaling.
    pub fn commitSavepoint(self: *Self, savepoint: *Savepoint) void {
        self.debugAssertUnsealed();
        std.debug.assert(self.savepoint_active);
        self.desc_trail.shrinkRetainingCapacity(savepoint.desc_trail_len);
        self.slot_trail.shrinkRetainingCapacity(savepoint.slot_trail_len);
        self.root_meta_trail.shrinkRetainingCapacity(savepoint.root_meta_trail_len);
        self.union_rank_trail.shrinkRetainingCapacity(savepoint.union_rank_trail_len);
        self.derived_map_plans_journaled = false;
        self.savepoint_active = false;
        // The descriptors the probe reissued stay reissued, and its merges
        // are kept, so the descriptors they orphaned are dead for good.
        // `freeDesc` reserved their room on the free list.
        self.free_descs.items.len -= self.probe_taken_descs;
        self.probe_taken_descs = 0;
        for (self.pending_free_descs.items) |desc_idx| self.recycleDesc(desc_idx);
        self.pending_free_descs.clearRetainingCapacity();

        if (savepoint_verification == .clone_crosscheck) {
            if (savepoint.verify_clone) |*vclone| {
                vclone.deinit(self.gpa);
                savepoint.verify_clone = null;
            }
        }
    }

    /// Assert that no speculative solver transaction is open. Mismatch
    /// poisoning is permanent error recovery and must never run speculatively;
    /// callers that unify under a savepoint use the non-poisoning relation API.
    pub fn assertNoSavepointActive(self: *const Self) void {
        std.debug.assert(!self.savepoint_active);
    }

    /// Whether a speculative solver transaction is open, so writes made now
    /// may still be rolled back.
    pub fn savepointActive(self: *const Self) bool {
        return self.savepoint_active;
    }

    /// Undo everything done since `savepoint` was created.
    pub fn rollbackToSavepoint(self: *Self, savepoint: *Savepoint) void {
        self.debugAssertUnsealed();
        self.class_watch.noteRollback();
        self.settled_descs.clear();
        // Replay journaled in-place writes in reverse so each pre-existing entry
        // lands back on its original value.
        var di = self.desc_trail.items.len;
        while (di > savepoint.desc_trail_len) {
            di -= 1;
            const u = self.desc_trail.items[di];
            self.descs.setRow(u.idx, u.old);
        }
        self.desc_trail.shrinkRetainingCapacity(savepoint.desc_trail_len);

        var mi = self.root_meta_trail.items.len;
        while (mi > savepoint.root_meta_trail_len) {
            mi -= 1;
            const u = self.root_meta_trail.items[mi];
            self.root_metas.set(rootMetaIdx(u.idx), u.old);
        }
        self.root_meta_trail.shrinkRetainingCapacity(savepoint.root_meta_trail_len);

        var ri = self.union_rank_trail.items.len;
        while (ri > savepoint.union_rank_trail_len) {
            ri -= 1;
            const u = self.union_rank_trail.items[ri];
            self.union_ranks.set(unionRankIdx(u.idx), u.old);
        }
        self.union_rank_trail.shrinkRetainingCapacity(savepoint.union_rank_trail_len);

        var si = self.slot_trail.items.len;
        while (si > savepoint.slot_trail_len) {
            si -= 1;
            const u = self.slot_trail.items[si];
            self.slots.set(u.idx, u.old);
            self.slot_generation += 1;
        }
        self.slot_trail.shrinkRetainingCapacity(savepoint.slot_trail_len);

        // Drop everything appended during the probe. The slot/desc baselines are
        // the store fields (also the journaling threshold); the rest come from
        // the savepoint.
        self.slots.shrinkRetainingCapacity(self.savepoint_baseline_slots);
        self.descs.backing.shrinkRetainingCapacity(self.savepoint_baseline_descs);
        self.root_metas.shrinkRetainingCapacity(self.savepoint_baseline_descs);
        self.union_ranks.shrinkRetainingCapacity(self.savepoint_baseline_slots);
        self.vars.shrinkRetainingCapacity(savepoint.vars_len);
        self.record_fields.shrinkRetainingCapacity(savepoint.record_fields_len);
        self.tags.shrinkRetainingCapacity(savepoint.tags_len);
        self.interpolation_parts.shrinkRetainingCapacity(savepoint.interpolation_parts_len);
        self.numeral_infos.shrinkRetainingCapacity(savepoint.numeral_infos_len);
        self.interpolation_metadata.shrinkRetainingCapacity(savepoint.interpolation_metadata_len);
        self.static_dispatch_constraints.shrinkRetainingCapacity(savepoint.static_dispatch_constraints_len);
        self.static_dispatch_relation_merges.shrinkRetainingCapacity(savepoint.static_dispatch_relation_merges_len);
        if (self.derived_map_plans_journaled) {
            // A probe only inserts plan rows, so the journaled rows are no
            // longer than the current list.
            const restored = self.derived_map_plans_undo.items;
            self.derived_map_plans.shrinkRetainingCapacity(restored.len);
            @memcpy(self.derived_map_plans.items(), restored);
            self.derived_map_plans_journaled = false;
        }

        // The probe's merges are undone, so nothing they orphaned is dead,
        // and every descriptor the probe took from the free list is dead
        // again: it was never removed, only counted as taken.
        self.pending_free_descs.clearRetainingCapacity();
        self.probe_taken_descs = 0;

        // Back to not speculating; savepoint_baseline_* are dead until the next create.
        self.savepoint_active = false;

        if (savepoint_verification == .clone_crosscheck) {
            if (savepoint.verify_clone) |*vclone| {
                self.assertMatchesClone(vclone);
                vclone.deinit(self.gpa);
                savepoint.verify_clone = null;
            }
        }
    }

    /// Clone cross-check: assert the trail-restored store is byte-for-byte
    /// identical to the full copy taken at `createSavepointVerifying`.
    fn assertMatchesClone(self: *Self, vclone: *const VerifyClone) void {
        std.debug.assert(self.slots.len() == vclone.slots.len);
        for (vclone.slots, 0..) |b, i| std.debug.assert(std.meta.eql(self.slots.get(@fromBackingInt(@intCast(i))), b));

        std.debug.assert(self.descs.backing.len() == vclone.descs.len());
        var i: usize = 0;
        while (i < vclone.descs.len()) : (i += 1) {
            std.debug.assert(std.meta.eql(self.descs.backing.get(@fromBackingInt(@intCast(i))), vclone.descs.get(@fromBackingInt(@intCast(i)))));
        }

        std.debug.assert(self.root_metas.len() == vclone.root_metas.len());
        i = 0;
        while (i < vclone.root_metas.len()) : (i += 1) {
            std.debug.assert(std.meta.eql(self.root_metas.get(@fromBackingInt(@intCast(i))), vclone.root_metas.get(@fromBackingInt(@intCast(i)))));
        }

        std.debug.assert(std.mem.eql(u8, self.union_ranks.items, vclone.union_ranks));

        std.debug.assert(self.derived_map_plans.items().len == vclone.derived_map_plans.len);
        for (self.derived_map_plans.items(), vclone.derived_map_plans) |entry, cloned| {
            std.debug.assert(std.meta.eql(entry, cloned));
        }
    }

    /// In-place slot write. While a probe is active, journals the slot's previous
    /// value so rollback can restore it; a failed journal append is propagated
    /// rather than risk a type store the trail can no longer faithfully undo.
    fn setSlot(self: *Self, idx: SlotStore.Idx, val: Slot) Allocator.Error!void {
        self.debugAssertUnsealed();
        if (self.savepoint_active and @backingInt(idx) < self.savepoint_baseline_slots) {
            try self.slot_trail.append(self.gpa, .{ .idx = idx, .old = self.slots.get(idx) });
        }
        switch (self.slots.get(idx)) {
            .root => |desc_idx| {
                self.class_watch.noteWrite(desc_idx);
                // The slot's vars stop resolving to a settled descriptor.
                if (!self.relinking_compared_classes and self.settled_descs.contains(desc_idx) and
                    !std.meta.eql(val, Slot{ .root = desc_idx }))
                {
                    self.settled_descs.clear();
                }
            },
            .redirect => {},
        }
        switch (val) {
            .root => |desc_idx| self.class_watch.noteWrite(desc_idx),
            .redirect => {},
        }
        self.slots.set(idx, val);
        self.slot_generation += 1;
    }

    /// In-place descriptor write. See setSlot.
    inline fn setDesc(self: *Self, idx: DescStore.Idx, val: Desc) Allocator.Error!void {
        self.debugAssertUnfrozenWrite(idx);
        try self.setDescUnguarded(idx, val);
    }

    fn debugAssertUnfrozenWrite(self: *const Self, idx: DescStore.Idx) void {
        if (std.debug.runtime_safety and self.descs.getWord(idx).flags.frozen) {
            base.invariant("a frozen type class was written in place", .{});
        }
    }

    /// `setDesc` without the frozen-class guard, for freezing and thawing.
    inline fn setDescUnguarded(self: *Self, idx: DescStore.Idx, val: Desc) Allocator.Error!void {
        try self.setDescRowUnguarded(idx, try self.encodeDesc(val));
    }

    /// `setDesc` for a descriptor already encoded.
    fn setDescRow(self: *Self, idx: DescStore.Idx, row: Desc.Row) Allocator.Error!void {
        self.debugAssertUnfrozenWrite(idx);
        try self.setDescRowUnguarded(idx, row);
    }

    fn setDescRowUnguarded(self: *Self, idx: DescStore.Idx, row: Desc.Row) Allocator.Error!void {
        // A whole row replaces the word, so it must not replace one a
        // generalization pass has marked.
        std.debug.assert(!self.descs.getWord(idx).marked());
        try self.journalDescWrite(idx);
        if (self.settled_descs.contains(idx) and !DescStore.sameContent(self.descs.getRow(idx), row)) {
            self.settled_descs.clear();
        }
        self.descs.setRow(idx, row);
    }

    /// In-place write of a descriptor's word alone: its flags or rank, never
    /// its kind, so its content stays as it was. Journaled and watched like
    /// every descriptor write (see setSlot).
    inline fn setDescWord(self: *Self, idx: DescStore.Idx, word: Desc.Word) Allocator.Error!void {
        self.debugAssertUnfrozenWrite(idx);
        try self.setDescWordUnguarded(idx, word);
    }

    /// `setDescWord` without the frozen-class guard, for freezing and thawing.
    inline fn setDescWordUnguarded(self: *Self, idx: DescStore.Idx, word: Desc.Word) Allocator.Error!void {
        std.debug.assert(word.kind == self.descs.getWord(idx).kind and word.literal == self.descs.getWord(idx).literal);
        try self.journalDescWrite(idx);
        self.descs.setWord(idx, word);
    }

    /// Before an in-place write to descriptor `idx`: journal its row while a
    /// probe that predates it is open, and note the write for class watches.
    inline fn journalDescWrite(self: *Self, idx: DescStore.Idx) Allocator.Error!void {
        self.debugAssertUnsealed();
        if (self.savepoint_active and @backingInt(idx) < self.savepoint_baseline_descs) try self.journalDescRow(idx);
        self.class_watch.noteWrite(idx);
    }

    /// Journal descriptor `idx`'s row without its generalization marks: a
    /// rollback runs outside any generalization pass, where no descriptor is
    /// marked, so the row it restores is never marked either.
    fn journalDescRow(self: *Self, idx: DescStore.Idx) Allocator.Error!void {
        var old = self.descs.getRow(idx);
        old.word.generalizing = false;
        old.word.rank_adjusted = false;
        try self.desc_trail.append(self.gpa, .{ .idx = idx, .old = old });
    }

    /// Record the class of each of `vars` as settled. The caller guarantees
    /// that every class reachable from these classes is among them or already
    /// settled.
    pub fn markSettled(self: *Self, vars: []const Var) Allocator.Error!void {
        self.debugAssertUnsealed();
        for (vars) |var_| try self.settled_descs.insert(self.gpa, self.resolveVar(var_).desc_idx);
    }

    /// Whether a checking scan recorded `var_`'s class as settled (see
    /// `markSettled`). This scan hint is discarded when checking seals the store.
    pub fn isSettled(self: *const Self, var_: Var) bool {
        return self.settled_descs.contains(self.resolveVar(var_).desc_idx);
    }

    /// In-place equivalence-class root metadata write. See setSlot.
    fn setRootMeta(self: *Self, idx: DescStore.Idx, val: RootMeta) Allocator.Error!void {
        self.debugAssertUnsealed();
        if (self.savepoint_active and @backingInt(idx) < self.savepoint_baseline_descs) {
            try self.root_meta_trail.append(self.gpa, .{ .idx = idx, .old = self.getRootMeta(idx) });
        }
        self.class_watch.noteWrite(idx);
        self.root_metas.set(rootMetaIdx(idx), val);
    }

    fn setUnionRank(self: *Self, storage_var: Var, rank: u8) Allocator.Error!void {
        self.debugAssertUnsealed();
        const slot_idx = Self.varToSlotIdx(storage_var);
        if (self.savepoint_active and @backingInt(slot_idx) < self.savepoint_baseline_slots) {
            try self.union_rank_trail.append(self.gpa, .{ .idx = slot_idx, .old = self.getUnionRank(storage_var) });
        }
        self.union_ranks.set(unionRankIdx(slot_idx), rank);
    }

    fn getUnionRank(self: *const Self, storage_var: Var) u8 {
        return self.union_ranks.get(unionRankIdx(Self.varToSlotIdx(storage_var))).*;
    }

    /// Create one descriptor and its structural root metadata atomically with
    /// respect to allocation failure. The two stores always use identical
    /// indices. A descriptor a merge freed is reused before the stores grow.
    inline fn appendClass(self: *Self, desc: Desc, checked_var: Var) Allocator.Error!DescStore.Idx {
        return self.appendClassRow(try self.encodeDesc(desc), checked_var);
    }

    /// `appendClass` for a descriptor already encoded.
    fn appendClassRow(self: *Self, row: Desc.Row, checked_var: Var) Allocator.Error!DescStore.Idx {
        self.debugAssertUnsealed();
        if (self.takeFreeDesc()) |desc_idx| {
            // Inside a probe the freed descriptor predates it (frees made
            // during the probe are pending), so both writes journal and
            // rollback restores them.
            try self.setDescRowUnguarded(desc_idx, row);
            try self.setRootMeta(desc_idx, .{ .checked_var = checked_var });
            return desc_idx;
        }
        const next_len = @as(usize, self.descs.backing.len()) + 1;
        try self.descs.backing.ensureTotalCapacity(self.gpa, next_len);
        try self.root_metas.ensureTotalCapacity(self.gpa, next_len);

        const desc_idx = self.descs.appendRowAssumeCapacity(row);
        const meta_idx = self.root_metas.appendAssumeCapacity(.{
            .checked_var = checked_var,
        });
        std.debug.assert(@backingInt(meta_idx) == @backingInt(desc_idx));
        return desc_idx;
    }

    /// The most recently freed descriptor not yet reissued, if any. Outside
    /// a probe it leaves the free list; inside one it is only counted as
    /// taken, so rollback can hand it back.
    fn takeFreeDesc(self: *Self) ?DescStore.Idx {
        if (!self.savepoint_active) return self.free_descs.pop();
        const available = self.free_descs.items.len - self.probe_taken_descs;
        if (available == 0) return null;
        self.probe_taken_descs += 1;
        return self.free_descs.items[available - 1];
    }

    /// Free the descriptor `former_root` named until the write that orphaned
    /// it: no slot names it any more, so the next class created may take it.
    /// While a probe is open the free waits for the probe to commit, since
    /// rollback would restore `former_root`'s slot.
    fn freeDesc(self: *Self, desc_idx: DescStore.Idx, former_root: Var) Allocator.Error!void {
        self.debugAssertUnsealed();
        std.debug.assert(!std.meta.eql(self.slots.get(Self.varToSlotIdx(former_root)), Slot{ .root = desc_idx }));
        if (self.savepoint_active) {
            // Reserve the free-list room now so committing never fails.
            try self.free_descs.ensureUnusedCapacity(self.gpa, self.pending_free_descs.items.len + 1);
            try self.pending_free_descs.append(self.gpa, desc_idx);
            return;
        }
        try self.free_descs.ensureUnusedCapacity(self.gpa, 1);
        self.recycleDesc(desc_idx);
    }

    /// Put a dead descriptor on the free list, forgetting everything the
    /// store itself remembers about it by index: a settled mark, a watch,
    /// a frozen anchor, and a derived map plan. The caller has reserved the
    /// list's room.
    fn recycleDesc(self: *Self, desc_idx: DescStore.Idx) void {
        std.debug.assert(!self.savepoint_active);
        const index: u32 = @backingInt(desc_idx);
        if (index < self.settled_descs.bits.bit_length) self.settled_descs.bits.unset(index);
        if (index < self.class_watch.watched.bit_length) self.class_watch.watched.unset(index);
        if (self.descs.getWord(desc_idx).flags.frozen) _ = self.frozen_anchors.remove(desc_idx);
        const plan_slot = self.derivedMapPlanSlot(desc_idx);
        if (plan_slot.found) _ = self.derived_map_plans.orderedRemove(plan_slot.index);
        self.free_descs.appendAssumeCapacity(desc_idx);
    }

    // derived map plans //

    const DerivedMapPlanSlot = struct { index: usize, found: bool };

    /// Where `class`'s row in `derived_map_plans` is, or would be inserted.
    fn derivedMapPlanSlot(self: *const Self, class: DescStore.Idx) DerivedMapPlanSlot {
        const entries = self.derived_map_plans.items();
        const key: u32 = @backingInt(class);
        var lo: usize = 0;
        var hi: usize = entries.len;
        while (lo < hi) {
            const mid = lo + (hi - lo) / 2;
            const mid_key: u32 = @backingInt(entries[mid].class);
            if (key < mid_key) {
                hi = mid;
            } else if (key > mid_key) {
                lo = mid + 1;
            } else {
                return .{ .index = mid, .found = true };
            }
        }
        return .{ .index = lo, .found = false };
    }

    fn classDerivedMapPlan(self: *const Self, class: DescStore.Idx) ?DerivedMapPlan {
        const slot = self.derivedMapPlanSlot(class);
        if (!slot.found) return null;
        return self.derived_map_plans.items()[slot.index].plan;
    }

    /// Before the open probe first changes the plan rows, keep a copy for
    /// rollback.
    fn journalDerivedMapPlans(self: *Self) Allocator.Error!void {
        if (!self.savepoint_active or self.derived_map_plans_journaled) return;
        self.derived_map_plans_undo.clearRetainingCapacity();
        try self.derived_map_plans_undo.appendSlice(self.gpa, self.derived_map_plans.items());
        self.derived_map_plans_journaled = true;
    }

    fn putClassDerivedMapPlan(self: *Self, class: DescStore.Idx, plan: DerivedMapPlan) Allocator.Error!void {
        self.debugAssertUnsealed();
        const slot = self.derivedMapPlanSlot(class);
        if (slot.found and std.meta.eql(self.derived_map_plans.items()[slot.index].plan, plan)) return;
        try self.journalDerivedMapPlans();
        if (slot.found) {
            self.derived_map_plans.items()[slot.index].plan = plan;
        } else {
            try self.derived_map_plans.insert(self.gpa, slot.index, .{ .class = class, .plan = plan });
        }
    }

    fn removeClassDerivedMapPlan(self: *Self, class: DescStore.Idx) Allocator.Error!void {
        self.debugAssertUnsealed();
        const slot = self.derivedMapPlanSlot(class);
        if (!slot.found) return;
        try self.journalDerivedMapPlans();
        _ = self.derived_map_plans.orderedRemove(slot.index);
    }

    /// Give the class `to` the plan of the class `from`, when it has one: a
    /// member detached from a class stays a member of the relation it
    /// joined.
    fn inheritClassDerivedMapPlan(self: *Self, from: DescStore.Idx, to: DescStore.Idx) Allocator.Error!void {
        const plan = self.classDerivedMapPlan(from) orelse return;
        try self.putClassDerivedMapPlan(to, plan);
    }

    /// Two classes merge into `destination`, one of them: the merged class
    /// keeps `b`'s plan, or else `a`'s.
    fn mergeClassDerivedMapPlans(self: *Self, a: DescStore.Idx, b: DescStore.Idx, destination: DescStore.Idx) Allocator.Error!void {
        std.debug.assert(destination == a or destination == b);
        if (a == b) return;
        const merged = self.classDerivedMapPlan(b) orelse self.classDerivedMapPlan(a) orelse return;
        try self.removeClassDerivedMapPlan(if (destination == a) b else a);
        try self.putClassDerivedMapPlan(destination, merged);
    }

    /// The payload the accepted derived `map`/`map!` relation whose callable
    /// class holds `fn_var` selects, if checking accepted one.
    pub fn derivedMapPlan(self: *const Self, fn_var: Var) ?DerivedMapPlan {
        if (self.derived_map_plans.items().len == 0) return null;
        return self.classDerivedMapPlan(self.resolveVar(fn_var).desc_idx);
    }

    /// Record the payload a derived `map`/`map!` relation selects for the
    /// class of its callable `fn_var`.
    pub fn setDerivedMapPlan(self: *Self, fn_var: Var, plan: DerivedMapPlan) Allocator.Error!void {
        try self.putClassDerivedMapPlan(self.resolveVar(fn_var).desc_idx, plan);
    }

    fn getRootMeta(self: *const Self, desc_idx: DescStore.Idx) RootMeta {
        return self.root_metas.get(rootMetaIdx(desc_idx));
    }

    // stored descriptors //

    /// The descriptor stored at `desc_idx`.
    inline fn descAt(self: *const Self, desc_idx: DescStore.Idx) Desc {
        return Desc.decode(self.descs.getRow(desc_idx), self);
    }

    // per-kind readers //
    //
    // The content of descriptor `desc_idx` for a reader that already knows
    // its kind (`resolveVarWord`, `kindAt`): each decodes that kind's payload
    // alone and builds no `Content`.

    pub inline fn flexAt(self: *const Self, desc_idx: DescStore.Idx) Flex {
        return Desc.decodeFlex(self.descs.getRow(desc_idx));
    }

    pub inline fn rigidAt(self: *const Self, desc_idx: DescStore.Idx) Rigid {
        return Desc.decodeRigid(self.descs.getRow(desc_idx));
    }

    pub inline fn aliasAt(self: *const Self, desc_idx: DescStore.Idx) Alias {
        return Desc.decodeAlias(self.descs.getRow(desc_idx), self);
    }

    pub inline fn fieldPresenceAt(self: *const Self, desc_idx: DescStore.Idx) types.FieldPresence {
        return Desc.decodeFieldPresence(self.descs.getRow(desc_idx));
    }

    pub inline fn recordAt(self: *const Self, desc_idx: DescStore.Idx) types.Record {
        return Desc.decodeRecord(self.descs.getRow(desc_idx));
    }

    pub inline fn tupleAt(self: *const Self, desc_idx: DescStore.Idx) types.Tuple {
        return Desc.decodeTuple(self.descs.getRow(desc_idx), self);
    }

    pub inline fn nominalAt(self: *const Self, desc_idx: DescStore.Idx) NominalType {
        return Desc.decodeNominal(self.descs.getRow(desc_idx), self);
    }

    pub inline fn funcAt(self: *const Self, desc_idx: DescStore.Idx) types.Func {
        return Desc.decodeFunc(self.descs.getRow(desc_idx), self);
    }

    pub inline fn tagUnionAt(self: *const Self, desc_idx: DescStore.Idx) TagUnion {
        return Desc.decodeTagUnion(self.descs.getRow(desc_idx));
    }

    /// The identity row a nominal descriptor names: two descriptors naming
    /// the same row have the same identity.
    pub inline fn nominalIdentityRowAt(self: *const Self, desc_idx: DescStore.Idx) u32 {
        const row = self.descs.getRow(desc_idx);
        std.debug.assert(row.word.kind == .nominal_type);
        return row.payload[0];
    }

    /// `desc` as a stored row, interning the identity of a nominal type or
    /// alias.
    inline fn encodeDesc(self: *Self, desc: Desc) Allocator.Error!Desc.Row {
        const identity: u32 = switch (desc.content) {
            .alias => |alias| try self.internAliasIdentity(.of(alias)),
            .structure => |flat| switch (flat) {
                .nominal_type => |nominal| try self.internNominalIdentity(.of(nominal)),
                .record, .tuple, .fn_pure, .fn_effectful, .fn_unbound, .empty_record, .tag_union, .empty_tag_union => 0,
            },
            .flex, .rigid, .field_presence, .err => 0,
        };
        return desc.encode(identity, self);
    }

    /// `desc` as a stored row, for content that is neither a nominal type nor
    /// an alias and so names no identity.
    inline fn encodeDescWithoutIdentity(self: *const Self, desc: Desc) Desc.Row {
        const kind = types.ContentKind.of(desc.content);
        std.debug.assert(kind != .alias and kind != .nominal_type);
        return desc.encode(0, self);
    }

    /// The identity row a stored nominal descriptor names.
    pub inline fn nominalIdentity(self: *const Self, index: u32) NominalIdentity {
        return self.nominal_identities.items()[index];
    }

    /// The identity row a stored alias descriptor names.
    pub inline fn aliasIdentity(self: *const Self, index: u32) AliasIdentity {
        return self.alias_identities.items()[index];
    }

    inline fn internNominalIdentity(self: *Self, identity: NominalIdentity) Allocator.Error!u32 {
        if (self.nominal_identity_memo.get(identity)) |row| return row;
        const row = try internIdentity(NominalIdentity, self.gpa, &self.nominal_identities, &self.nominal_identity_index, &self.nominal_identities_indexed, identity);
        self.nominal_identity_memo.put(identity, row);
        return row;
    }

    inline fn internAliasIdentity(self: *Self, identity: AliasIdentity) Allocator.Error!u32 {
        if (self.alias_identity_memo.get(identity)) |row| return row;
        const row = try internIdentity(AliasIdentity, self.gpa, &self.alias_identities, &self.alias_identity_index, &self.alias_identities_indexed, identity);
        self.alias_identity_memo.put(identity, row);
        return row;
    }

    /// A direct-mapped memo of identity rows, keyed by the identity's name.
    /// Every hit compares the whole identity, so it returns exactly the row
    /// the index would.
    fn IdentityMemo(comptime Identity: type) type {
        return struct {
            entries: [entry_count]Entry = @splat(.{}),

            const entry_count = 64;
            const Entry = struct {
                identity: Identity = undefined,
                /// The row plus one; zero for an empty entry.
                row_plus_one: u32 = 0,
            };

            inline fn slot(identity: Identity) usize {
                return (@as(u32, @bitCast(identity.ident)) *% 0x9E3779B1) >> (32 - @ctz(@as(u32, entry_count)));
            }

            inline fn get(self: *const @This(), identity: Identity) ?u32 {
                const entry = &self.entries[slot(identity)];
                if (entry.row_plus_one == 0 or !std.mem.eql(u8, std.mem.asBytes(&entry.identity), std.mem.asBytes(&identity))) return null;
                return entry.row_plus_one - 1;
            }

            fn put(self: *@This(), identity: Identity, row: u32) void {
                self.entries[slot(identity)] = .{ .identity = identity, .row_plus_one = row + 1 };
            }
        };
    }

    /// The row holding `identity`, appending one when no row does.
    fn internIdentity(
        comptime Identity: type,
        gpa: Allocator,
        rows: *Identity.SafeList,
        index: *std.AutoHashMapUnmanaged(Identity, u32),
        indexed: *u32,
        identity: Identity,
    ) Allocator.Error!u32 {
        const all = rows.items();
        while (indexed.* < all.len) : (indexed.* += 1) {
            const entry = try index.getOrPut(gpa, all[indexed.*]);
            if (!entry.found_existing) entry.value_ptr.* = indexed.*;
        }
        const entry = try index.getOrPut(gpa, identity);
        if (entry.found_existing) return entry.value_ptr.*;
        errdefer _ = index.remove(identity);
        const row: u32 = @intCast(all.len);
        _ = try rows.append(gpa, identity);
        entry.value_ptr.* = row;
        indexed.* = row + 1;
        return row;
    }

    /// Forget which row holds each identity, after the rows changed in place;
    /// the next lookup indexes them again.
    fn forgetIdentityIndexes(self: *Self) void {
        self.nominal_identity_index.clearRetainingCapacity();
        self.nominal_identities_indexed = 0;
        self.alias_identity_index.clearRetainingCapacity();
        self.alias_identities_indexed = 0;
        self.nominal_identity_memo = .{};
        self.alias_identity_memo = .{};
    }

    // fresh variables //

    /// Create a new unbound, flexible type variable without a name
    /// Used in canonicalization when creating type slots
    pub fn fresh(self: *Self) std.mem.Allocator.Error!Var {
        const trace = tracy.traceNamed(@src(), "typesStore.fresh");
        defer trace.end();
        return try self.registerRow(self.encodeDescWithoutIdentity(.{
            .content = .{ .flex = Flex.init() },
            .rank = Rank.outermost,
        }));
    }

    /// Create a new variable with the provided desc
    /// Used in tests
    /// TODO: Can we remove this function? It hardcodes rank, which is fine for
    /// test but we can never use this in actual typechecking
    pub fn freshFromContent(self: *Self, content: Content) std.mem.Allocator.Error!Var {
        const trace = tracy.traceNamed(@src(), "typesStore.freshFromContent");
        defer trace.end();
        return try self.register(.{
            .content = content,
            .rank = Rank.outermost,
        });
    }

    /// Create a new variable with the given content and rank
    pub inline fn freshFromContentWithRank(self: *Self, content: Content, rank: Rank) std.mem.Allocator.Error!Var {
        return try self.registerRow(try self.encodeDesc(.{
            .content = content,
            .rank = rank,
        }));
    }

    /// Create a variable redirecting to the provided var
    /// Used in tests
    pub fn freshRedirect(self: *Self, var_: Var) std.mem.Allocator.Error!Var {
        self.debugAssertUnsealed();
        try self.slots.ensureUnusedCapacity(self.gpa, 1);
        try self.union_ranks.ensureUnusedCapacity(self.gpa, 1);
        const slot_idx = self.slots.appendAssumeCapacity(.{ .redirect = var_ });
        const rank_idx = self.union_ranks.appendAssumeCapacity(0);
        std.debug.assert(@backingInt(rank_idx) == @backingInt(slot_idx));
        return Self.slotIdxToVar(slot_idx);
    }

    /// Create a new variable with the given descriptor
    pub fn register(self: *Self, desc: Desc) std.mem.Allocator.Error!Var {
        return self.registerRow(try self.encodeDesc(desc));
    }

    /// `register` for a descriptor already encoded.
    fn registerRow(self: *Self, row: Desc.Row) std.mem.Allocator.Error!Var {
        self.debugAssertUnsealed();
        try self.slots.ensureUnusedCapacity(self.gpa, 1);
        try self.union_ranks.ensureUnusedCapacity(self.gpa, 1);
        const slot_idx: SlotStore.Idx = @fromBackingInt(@intCast(@as(u32, @intCast(self.slots.len()))));
        const checked_var = Self.slotIdxToVar(slot_idx);
        const desc_idx = try self.appendClassRow(row, checked_var);
        const inserted_slot_idx = self.slots.appendAssumeCapacity(.{ .root = desc_idx });
        const rank_idx = self.union_ranks.appendAssumeCapacity(0);
        std.debug.assert(inserted_slot_idx == slot_idx);
        std.debug.assert(@backingInt(rank_idx) == @backingInt(slot_idx));
        return Self.slotIdxToVar(slot_idx);
    }

    /// Create a new variable with the provided content assuming there is capacity
    pub fn appendFromContentAssumeCapacity(self: *Self, content: Content, rank: Rank) Var {
        self.debugAssertUnsealed();
        std.debug.assert(!self.savepoint_active);
        const slot_idx: SlotStore.Idx = @fromBackingInt(@intCast(@as(u32, @intCast(self.slots.len()))));
        const checked_var = Self.slotIdxToVar(slot_idx);
        const desc: Desc = .{ .content = content, .rank = rank };
        const desc_idx = if (self.free_descs.pop()) |freed| reused: {
            // Nothing watches or settles a freed descriptor, and no probe is
            // open, so the raw writes need no notes and no journal.
            self.descs.setRow(freed, self.encodeDescWithoutIdentity(desc));
            self.root_metas.set(rootMetaIdx(freed), .{ .checked_var = checked_var });
            break :reused freed;
        } else appended: {
            const appended = self.descs.appendRowAssumeCapacity(self.encodeDescWithoutIdentity(desc));
            const meta_idx = self.root_metas.appendAssumeCapacity(.{ .checked_var = checked_var });
            std.debug.assert(@backingInt(meta_idx) == @backingInt(appended));
            break :appended appended;
        };
        const inserted_slot_idx = self.slots.appendAssumeCapacity(.{ .root = desc_idx });
        const rank_idx = self.union_ranks.appendAssumeCapacity(0);
        std.debug.assert(inserted_slot_idx == slot_idx);
        std.debug.assert(@backingInt(rank_idx) == @backingInt(slot_idx));
        return Self.slotIdxToVar(slot_idx);
    }

    // setting variables //

    /// Freeze the class `var_` belongs to; `var_` becomes its anchor.
    pub fn freezeClass(self: *Self, var_: Var) Allocator.Error!void {
        self.debugAssertUnsealed();
        const storage = self.resolveStorageRoot(var_);
        std.debug.assert(!storage.desc.flags.frozen);
        std.debug.assert(storage.meta.checked_var == var_);
        var word = self.descs.getWord(storage.desc_idx);
        word.flags.frozen = true;
        try self.frozen_anchors.put(storage.desc_idx, var_);
        try self.setDescWordUnguarded(storage.desc_idx, word);
    }

    /// End every class's freeze; afterwards the store holds no frozen class.
    pub fn thawFrozenClasses(self: *Self) Allocator.Error!void {
        self.debugAssertUnsealed();
        var anchors = self.frozen_anchors.constIterator();
        while (anchors.next()) |entry| {
            const idx = entry.key_ptr.*;
            var word = self.descs.getWord(idx);
            word.flags.frozen = false;
            try self.setDescWordUnguarded(idx, word);
        }
        self.frozen_anchors.clearRetainingCapacity();
    }

    /// When `target_var` is a member of a frozen class, detach it into a class
    /// of its own holding the same type, so a write aimed at it changes it
    /// alone. A class whose checked representative is `target_var` is first
    /// handed back to its anchor. Returns false when `target_var` is the
    /// anchor itself, which no write may change.
    inline fn isolateFromFrozenClass(self: *Self, target_var: Var) Allocator.Error!bool {
        if (self.frozen_anchors.count() == 0) return true;
        return self.isolateFromFrozenClassSlow(target_var);
    }

    fn isolateFromFrozenClassSlow(self: *Self, target_var: Var) Allocator.Error!bool {
        var class = self.resolveStorageRoot(target_var);
        if (!class.desc.flags.frozen) return true;
        const anchor = self.frozen_anchors.get(class.desc_idx) orelse
            base.invariant("a frozen type class has no anchor", .{});
        if (target_var == anchor) return false;
        if (class.meta.checked_var == target_var) {
            try self.setRootMeta(class.desc_idx, .{ .checked_var = anchor });
            class.meta.checked_var = anchor;
        }
        try self.detachOccurrence(class, target_var);
        var desc = class.desc;
        desc.flags.frozen = false;
        const desc_idx = try self.appendClass(desc, target_var);
        try self.inheritClassDerivedMapPlan(class.desc_idx, desc_idx);
        try self.setSlot(Self.varToSlotIdx(target_var), .{ .root = desc_idx });
        return true;
    }

    /// `target_var` resolved for a write aimed at it, after
    /// `isolateWriteTarget` when its class is frozen.
    inline fn resolveWriteTarget(self: *Self, target_var: Var) Allocator.Error!ResolvedVarDesc {
        const resolved = self.resolveVar(target_var);
        if (!resolved.desc.flags.frozen) return resolved;
        return self.resolveIsolatedWriteTarget(target_var);
    }

    fn resolveIsolatedWriteTarget(self: *Self, target_var: Var) Allocator.Error!ResolvedVarDesc {
        try self.isolateWriteTarget(target_var);
        return self.resolveVar(target_var);
    }

    /// `isolateFromFrozenClass` for a write that must not reach an anchor.
    fn isolateWriteTarget(self: *Self, target_var: Var) Allocator.Error!void {
        if (!try self.isolateFromFrozenClass(target_var)) {
            base.invariant("a write was aimed at the anchor of a frozen type class", .{});
        }
    }

    /// Reset a variable's slot to an unbound flex at the given rank. If it was a
    /// redirect, its entire storage subtree is detached; the former storage
    /// root becomes the checked representative of the remainder. The retained
    /// structural rank is a valid upper bound for both fragments.
    ///
    /// IMPORTANT: Only sound when nothing live references the variable's
    /// previous class through this slot. Used by the annotated-scheme
    /// pre-pass to return annotation nodes to their pre-generation state
    /// (after the declared scheme was copied out of them) so the def's body
    /// check can generate the annotation again.
    pub fn resetVarToUnbound(self: *Self, target_var: Var, rank: Rank) Allocator.Error!void {
        self.debugAssertUnsealed();
        std.debug.assert(@backingInt(target_var) < self.len());
        try self.isolateWriteTarget(target_var);
        const storage = self.resolveStorageRoot(target_var);
        const desc_idx = try self.appendClass(.{
            .content = .{ .flex = Flex.init() },
            .rank = rank,
        }, target_var);
        if (target_var != storage.storage_var) {
            // `target_var` may be the old checked representative, or its
            // detached subtree may contain that representative. Keep the
            // remainder self-contained by selecting its storage root.
            try self.setRootMeta(storage.desc_idx, .{
                .checked_var = storage.storage_var,
            });
        }
        try self.setSlot(Self.varToSlotIdx(target_var), .{ .root = desc_idx });
        // A storage root takes its whole subtree to the fresh flex, so its
        // old descriptor is orphaned; a redirect leaves the remainder on it.
        if (target_var == storage.storage_var) try self.freeDesc(storage.desc_idx, storage.storage_var);
    }

    /// Set a type variable to the provided content
    ///
    /// IMPORTANT: When using this function during type checking, it's possible
    /// to loose `rank` information! You should prefer to use regular `unify`
    /// over this function, which correctly propagates rank, unless you already
    /// know the two vars are of  the same rank.
    pub inline fn dangerousSetVarDesc(self: *Self, target_var: Var, desc: Desc) Allocator.Error!void {
        self.debugAssertUnsealed();
        std.debug.assert(@backingInt(target_var) < self.len());
        const resolved = try self.resolveWriteTarget(target_var);
        self.noteContentRewrite(resolved.desc.content);
        try self.setDesc(resolved.desc_idx, desc);
    }

    /// Show `target_var`'s whole class with `content` until the active
    /// savepoint rolls back: a diagnostic's view of a class as it stood
    /// before a decision. The view is of the class itself, so a frozen class
    /// shows it too; the rollback restores the shared descriptor.
    pub fn setClassContentInSavepoint(self: *Self, target_var: Var, content: Content) Allocator.Error!void {
        self.debugAssertUnsealed();
        std.debug.assert(self.savepoint_active);
        const resolved = self.resolveVar(target_var);
        self.noteContentRewrite(resolved.desc.content);
        var desc = resolved.desc;
        desc.content = content;
        desc.flags.empty_tag_union_is_default = false;
        try self.setDescUnguarded(resolved.desc_idx, desc);
    }

    /// Whether the solver can still refine or poison what this content
    /// denotes: a variable, a field presence, or an error.
    fn kindIsLeaf(kind: types.ContentKind) bool {
        return switch (kind) {
            .flex, .rigid, .field_presence, .err => true,
            .alias, .record, .tuple, .nominal_type, .fn_pure, .fn_effectful, .fn_unbound, .empty_record, .tag_union, .empty_tag_union => false,
        };
    }

    fn contentIsLeaf(content: Content) bool {
        return switch (content) {
            .flex, .rigid, .field_presence, .err => true,
            .alias, .structure => false,
        };
    }

    /// Note that a direct write replaces content `old`. Rewriting concrete
    /// content can make a variable-free type reach a variable.
    fn noteContentRewrite(self: *Self, old: Content) void {
        if (!contentIsLeaf(old)) self.ground_epoch +%= 1;
    }

    /// Whether two descriptors agree on every flag an instantiated copy
    /// carries over.
    fn sameCopiedFlags(a: types.DescriptorFlags, b: types.DescriptorFlags) bool {
        return a.copied() == b.copied();
    }

    pub fn groundEpoch(self: *const Self) u64 {
        return self.ground_epoch;
    }

    /// Set a type variable to the provided content
    pub fn setVarContent(self: *Self, target_var: Var, content: Content) Allocator.Error!void {
        self.debugAssertUnsealed();
        std.debug.assert(@backingInt(target_var) < self.len());
        const resolved = try self.resolveWriteTarget(target_var);
        self.noteContentRewrite(resolved.desc.content);
        var desc = resolved.desc;
        desc.content = content;
        desc.flags.empty_tag_union_is_default = false;
        try self.setDesc(resolved.desc_idx, desc);
    }

    /// Close an otherwise-unresolved variable to the empty tag union while
    /// retaining the checker's authoritative defaulting decision.
    pub fn setVarToEmptyTagUnionDefault(self: *Self, target_var: Var) Allocator.Error!void {
        self.debugAssertUnsealed();
        std.debug.assert(@backingInt(target_var) < self.len());
        try self.isolateWriteTarget(target_var);
        const resolved = self.resolveVar(target_var);
        self.noteContentRewrite(resolved.desc.content);
        var desc = resolved.desc;
        desc.content = .{ .structure = .empty_tag_union };
        desc.flags.empty_tag_union_is_default = true;
        try self.setDesc(resolved.desc_idx, desc);
    }

    /// Record that checking rejected a static-dispatch obligation whose
    /// constraint function type is `target_var`'s equivalence class. This is
    /// evidence metadata: the class's content is left exactly as the unifier
    /// left it. Returns whether this call is what rejected the class, so a
    /// caller mirroring the marker into a durable record writes one entry per
    /// class rather than one per occurrence.
    pub fn markVarStaticDispatchRejected(self: *Self, target_var: Var) Allocator.Error!bool {
        self.debugAssertUnsealed();
        std.debug.assert(@backingInt(target_var) < self.len());
        try self.isolateWriteTarget(target_var);
        const desc_idx = self.resolveDescIdx(target_var);
        var word = self.descs.getWord(desc_idx);
        if (word.flags.static_dispatch_rejected) return false;
        word.flags.static_dispatch_rejected = true;
        try self.setDescWord(desc_idx, word);
        return true;
    }

    /// Record that a defaulting decision is about to choose `target_var`'s
    /// equivalence class. Provenance only: content and links are untouched.
    pub fn markVarDefaultDecided(self: *Self, target_var: Var) Allocator.Error!void {
        self.debugAssertUnsealed();
        std.debug.assert(@backingInt(target_var) < self.len());
        const desc_idx = self.resolveDescIdx(target_var);
        var word = self.descs.getWord(desc_idx);
        if (word.flags.default_decided) return;
        word.flags.default_decided = true;
        try self.setDescWord(desc_idx, word);
    }

    /// Withdraw a `markVarDefaultDecided` whose decision left the class
    /// undetermined: the default it anticipated never reached this class.
    pub fn clearVarDefaultDecided(self: *Self, target_var: Var) Allocator.Error!void {
        self.debugAssertUnsealed();
        std.debug.assert(@backingInt(target_var) < self.len());
        const desc_idx = self.resolveDescIdx(target_var);
        var word = self.descs.getWord(desc_idx);
        if (!word.flags.default_decided) return;
        word.flags.default_decided = false;
        try self.setDescWord(desc_idx, word);
    }

    /// Whether a defaulting decision chose `target_var`'s equivalence class.
    pub fn varDefaultDecided(self: *const Self, target_var: Var) bool {
        std.debug.assert(@backingInt(target_var) < self.len());
        return self.flagsOf(target_var).default_decided;
    }

    /// Whether checking rejected a static-dispatch obligation on `target_var`'s
    /// equivalence class.
    pub fn varStaticDispatchRejected(self: *const Self, target_var: Var) bool {
        std.debug.assert(@backingInt(target_var) < self.len());
        return self.flagsOf(target_var).static_dispatch_rejected;
    }

    /// Record that the vars in `[start, end)`, minted by opening a nominal
    /// declaration's backing as `opened`, are that backing's declared
    /// structure below its root. The root itself is related to a constructor
    /// operand by the nominal constructor backing relation, which owns that
    /// pair. A minted var whose class is rooted outside the range was linked
    /// to a var that predates the opening, such as a substituted arg, and
    /// keeps that var's provenance.
    pub fn markNominalBackingStructure(self: *Self, opened: Var, start: u32, end: u32) Allocator.Error!void {
        self.debugAssertUnsealed();
        std.debug.assert(start <= end and end <= self.len());
        const opened_root = self.resolveVar(opened).var_;
        var minted = start;
        while (minted < end) : (minted += 1) {
            const resolved = self.resolveVar(@fromBackingInt(@intCast(minted)));
            if (resolved.var_ == opened_root) continue;
            const root: u32 = @backingInt(resolved.var_);
            if (root < start or root >= end) continue;
            if (resolved.desc.flags.nominal_backing_structure) continue;
            self.ground_epoch +%= 1;
            var word = self.descs.getWord(resolved.desc_idx);
            word.flags.nominal_backing_structure = true;
            try self.setDescWord(resolved.desc_idx, word);
        }
    }

    /// Record definition-site annotation openness (design.md "Derived Parser
    /// Tag-Row Closure"). Provenance travels with the flex equivalence class.
    pub fn markAnnotationTagExt(self: *Self, target_var: Var) Allocator.Error!void {
        self.debugAssertUnsealed();
        try self.isolateWriteTarget(target_var);
        const desc_idx = self.resolveDescIdx(target_var);
        var word = self.descs.getWord(desc_idx);
        std.debug.assert(word.kind == .flex);
        word.flags.annotation_tag_ext = true;
        try self.setDescWord(desc_idx, word);
    }

    /// Mark a tag row a host-boundary annotation wrote (design.md "Row
    /// Joins"). The mark travels with the row's equivalence class and its
    /// copies.
    pub fn markHostFixedRow(self: *Self, target_var: Var) Allocator.Error!void {
        self.debugAssertUnsealed();
        const resolved = try self.resolveWriteTarget(target_var);
        var word = self.descs.getWord(resolved.desc_idx);
        word.flags.host_fixed_row = true;
        try self.setDescWord(resolved.desc_idx, word);
    }

    /// Bound an annotated definition's implicitly opened row (design.md
    /// "Polarity"). The bound travels with the row's equivalence class.
    pub fn markBoundedRowExt(self: *Self, target_var: Var) Allocator.Error!void {
        self.debugAssertUnsealed();
        const resolved = try self.resolveWriteTarget(target_var);
        var word = self.descs.getWord(resolved.desc_idx);
        word.flags.bounded_row_ext = true;
        try self.setDescWord(resolved.desc_idx, word);
    }

    /// End the bound on a row: a platform-required definition's rows are
    /// released where the platform relates them, and a bound an enclosing
    /// release cleared is applied again for the owners that still hold it.
    pub fn clearBoundedRowExt(self: *Self, target_var: Var) Allocator.Error!void {
        self.debugAssertUnsealed();
        const resolved = try self.resolveWriteTarget(target_var);
        var word = self.descs.getWord(resolved.desc_idx);
        word.flags.bounded_row_ext = false;
        try self.setDescWord(resolved.desc_idx, word);
    }

    /// The declared rule a `dangerousSetVarRedirect` call site bends the solved
    /// graph under. A redirect outside ordinary unification is indistinguishable
    /// at review time from a change to the language's typing rules, so every call
    /// site must name the rule it operates under, and every member here must be
    /// one of:
    ///
    ///   (i)  diagnostic recovery on an already-reported error—the redirect
    ///        cannot change which programs typecheck or which plans are output
    ///        for error-free programs; or
    ///   (ii) a language/pipeline rule declared in design.md—the member's doc
    ///        comment names the design.md section that declares it, and the rule
    ///        has tests pinning both its accepted and its rejected side.
    ///
    /// A new call site must either cite an existing member whose rule covers it
    /// or add a member (and the design.md declaration it cites) in the same
    /// change. "It makes a test pass" is not a rule.
    pub const RedirectRule = enum {
        /// (ii) design.md "Polarity" / Rewrite Inventory
        /// `closeTagRowsForDerivation`: a polarity marker rigid in tag-ext
        /// position (the alias-declaration-body deferral, which stands for
        /// exactly "flex or `[]`, per use site") reaches a derivation through
        /// a directly-used local alias declaration, where no instantiation
        /// ever resolves it. The derivation determines the row exactly, so
        /// the marker closes: it redirects to the empty tag union, the same
        /// outcome instantiation's `.close` behavior produces.
        derivation_marker_ext_closure,
        /// (ii) design.md "Row Union Normalization": a row part whose every
        /// label also occurs further along its extension chain denotes that
        /// extension once the repeated occurrences are related.
        row_union_normalization,
        /// Mechanism, design.md "Deferred requirement callables": a use's
        /// placeholder for a requirement callable its instantiation deferred
        /// redirects to the callable's copy, which that instantiation would
        /// have written in its place. The placeholder is related to nothing
        /// before it is linked.
        deferred_requirement_callable,
    };

    /// Set a type variable to redirect to the provided variables.
    /// During type-checking, you probably don't want to use this function.
    ///
    /// This is the primitive that mutates the solved graph outside ordinary
    /// unification. `rule` names the declared rule (see `RedirectRule`) the call
    /// site operates under; a call without one does not compile.
    ///
    /// IMPORTANT: When using this function during type checking, it's possible
    /// to loose `rank` information! You should prefer to use regular `unify`
    /// over this function, which correctly propagates rank, unless you already
    /// know the two vars are of the same rank.
    pub fn dangerousSetVarRedirect(self: *Self, comptime rule: RedirectRule, target_var: Var, redirect_to: Var) Allocator.Error!void {
        self.debugAssertUnsealed();
        std.debug.assert(@backingInt(target_var) < self.len());
        std.debug.assert(@backingInt(redirect_to) < self.len());
        try self.isolateWriteTarget(target_var);
        const target_storage = self.resolveStorageRoot(target_var);
        const redirect_storage = self.resolveStorageRoot(redirect_to);
        self.ground_epoch +%= 1;
        // Joining a class to itself is always an invalid invocation of a
        // solver-mutating rewrite, even if the two source vars differ.
        if (target_storage.storage_var == redirect_storage.storage_var) {
            base.invariant("self-redirect of equivalent vars {d} and {d} under rule {s}", .{
                @backingInt(target_var),
                @backingInt(redirect_to),
                @tagName(rule),
            });
        }
        if (std.debug.runtime_safety) {
            // Redirecting a root var into a transparent alias whose backing resolves
            // back to that same root creates a self-referential (infinite) alias.
            // Recursive transparent aliases are illegal, so this is always a bug;
            // catch it loudly rather than silently producing an INFINITE TYPE later.
            if (redirect_storage.desc.content == .alias) {
                const backing_root = self.resolveVar(self.getAliasBackingVar(redirect_storage.desc.content.alias)).var_;
                std.debug.assert(backing_root != target_storage.meta.checked_var);
            }
        }
        try self.linkStorageRoots(
            target_storage,
            redirect_storage,
            redirect_storage.desc_idx,
            redirect_storage.meta.checked_var,
        );
        try self.freeDesc(target_storage.desc_idx, target_storage.storage_var);
    }

    // make builtin types //

    // make content types //

    /// Make a tag union data type
    /// Does not insert content into the types store
    pub fn mkTagUnion(self: *Self, tags: []const Tag, ext_var: Var) std.mem.Allocator.Error!Content {
        const tags_range = try self.appendTags(tags);
        const tag_union = TagUnion{ .tags = tags_range, .ext = ext_var };
        return Content{ .structure = .{ .tag_union = tag_union } };
    }

    /// Make a tag data type
    /// Does not insert content into the types store
    pub fn mkTag(self: *Self, name: base.Ident.Idx, args: []const Var) std.mem.Allocator.Error!Tag {
        const args_range = try self.appendVars(args);
        return Tag{ .name = name, .args = args_range };
    }

    /// Make alias data type
    /// Does not insert content into the types store
    pub fn mkAlias(
        self: *Self,
        ident: TypeIdent,
        backing_var: Var,
        args: []const Var,
        origin_module: base.ModuleIdentity.Idx,
    ) std.mem.Allocator.Error!Content {
        return self.mkAliasWithSourceDecl(ident, backing_var, args, origin_module, null);
    }

    pub fn mkAliasWithSourceDecl(
        self: *Self,
        ident: TypeIdent,
        backing_var: Var,
        args: []const Var,
        origin_module: base.ModuleIdentity.Idx,
        source_decl: ?u32,
    ) std.mem.Allocator.Error!Content {
        return self.mkAliasWithSourceDeclAndBuiltinOrigin(
            ident,
            backing_var,
            args,
            @intCast(args.len),
            origin_module,
            source_decl,
            false,
        );
    }

    pub fn mkAliasWithSourceDeclAndBuiltinOrigin(
        self: *Self,
        ident: TypeIdent,
        backing_var: Var,
        args: []const Var,
        source_arg_count: u32,
        origin_module: base.ModuleIdentity.Idx,
        source_decl: ?u32,
        builtin_origin: bool,
    ) std.mem.Allocator.Error!Content {
        const packed_source_decl = try SourceDecl.fromOptionalWithBuiltinOriginChecked(source_decl, builtin_origin);
        return self.mkAliasWithPackedSourceDecl(ident, backing_var, args, source_arg_count, origin_module, packed_source_decl);
    }

    /// Make alias content whose first `source_arg_count` args are source
    /// arguments and the rest hidden implicit-row parameters. Appends
    /// `[backing, args...]` to the vars pool as the alias's vars range.
    pub fn mkAliasWithPackedSourceDecl(
        self: *Self,
        ident: TypeIdent,
        backing_var: Var,
        args: []const Var,
        source_arg_count: u32,
        origin_module: base.ModuleIdentity.Idx,
        source_decl: SourceDecl,
    ) std.mem.Allocator.Error!Content {
        self.debugAssertUnsealed();
        std.debug.assert(source_arg_count <= args.len);
        if (builtin.mode == .debug and self.sliceAliasesVars(args)) {
            base.invariant("mkAlias: argument slice aliases the var list it is appended to", .{});
        }
        try self.vars.ensureUnusedCapacity(self.gpa, args.len + 2);
        _ = self.vars.appendAssumeCapacity(rangeLengthSlot(@intCast(args.len + 1)));
        const backing_idx = self.vars.appendAssumeCapacity(backing_var);
        for (args) |arg| _ = self.vars.appendAssumeCapacity(arg);

        return Content{
            .alias = Alias{
                .ident = ident,
                .vars = .{ .nonempty = .{ .start = backing_idx, .count = @intCast(args.len + 1) } },
                .source_arg_count = source_arg_count,
                .origin_module = origin_module,
                .source_decl = source_decl,
            },
        };
    }

    /// Make a nominal type application: identity plus actual type args only.
    /// The backing type lives in the declaration table, not the application.
    /// Does not insert content into the types store
    pub fn mkNominal(
        self: *Self,
        ident: TypeIdent,
        args: []const Var,
        origin_module: base.ModuleIdentity.Idx,
        is_opaque: bool,
    ) std.mem.Allocator.Error!Content {
        return self.mkNominalWithSourceDecl(ident, args, origin_module, null, is_opaque);
    }

    pub fn mkNominalWithSourceDecl(
        self: *Self,
        ident: TypeIdent,
        args: []const Var,
        origin_module: base.ModuleIdentity.Idx,
        source_decl: ?u32,
        is_opaque: bool,
    ) std.mem.Allocator.Error!Content {
        return self.mkNominalWithSourceDeclAndBuiltinOrigin(
            ident,
            args,
            origin_module,
            source_decl,
            is_opaque,
            false,
        );
    }

    pub fn mkNominalWithSourceDeclAndBuiltinOrigin(
        self: *Self,
        ident: TypeIdent,
        args: []const Var,
        origin_module: base.ModuleIdentity.Idx,
        source_decl: ?u32,
        is_opaque: bool,
        builtin_origin: bool,
    ) std.mem.Allocator.Error!Content {
        const source = try NominalType.Source.initChecked(
            try SourceDecl.fromOptionalWithBuiltinOriginChecked(source_decl, builtin_origin),
            is_opaque,
            builtin_origin,
        );
        const args_range = try self.appendVars(args);

        return Content{ .structure = FlatType{
            .nominal_type = NominalType{
                .ident = ident,
                .args = args_range,
                .origin_module = origin_module,
                .source = source,
            },
        } };
    }

    // Make a function data type with unbound effectfulness
    // Does not insert content into the types store.
    pub fn mkFuncUnbound(self: *Self, args: []const Var, ret: Var) std.mem.Allocator.Error!Content {
        return self.mkFuncUnboundWithEffectDeps(args, ret, &.{});
    }

    /// Make a function data type whose effect is inferred from directed
    /// dependencies on other function types.
    pub fn mkFuncUnboundWithEffectDeps(
        self: *Self,
        args: []const Var,
        ret: Var,
        effect_deps: []const Var,
    ) std.mem.Allocator.Error!Content {
        const args_range = try self.appendVars(args);
        const effect_deps_range = try self.appendVars(effect_deps);
        return Content{ .structure = .{ .fn_unbound = .{
            .args = args_range,
            .ret = ret,
            .effect_deps = effect_deps_range,
        } } };
    }

    // Make a pure function data type (as opposed to an effectful or unbound function)
    // Does not insert content into the types store.
    pub fn mkFuncPure(self: *Self, args: []const Var, ret: Var) std.mem.Allocator.Error!Content {
        const args_range = try self.appendVars(args);
        return Content{ .structure = .{ .fn_pure = .{
            .args = args_range,
            .ret = ret,
        } } };
    }

    // Make an effectful function data type (as opposed to a pure or unbound function)
    // Does not insert content into the types store.
    pub fn mkFuncEffectful(self: *Self, args: []const Var, ret: Var) std.mem.Allocator.Error!Content {
        const args_range = try self.appendVars(args);
        return Content{ .structure = .{ .fn_effectful = .{
            .args = args_range,
            .ret = ret,
        } } };
    }

    // sub list setters //

    /// Append `s` to the vars pool as a range: its length goes in the slot
    /// just before it (`varRange`), so a stored descriptor keeps only the
    /// range's start. Every range of the pool is made here or by
    /// `mkAliasWithPackedSourceDecl`.
    pub fn appendVars(self: *Self, s: []const Var) std.mem.Allocator.Error!VarSafeList.Range {
        self.debugAssertUnsealed();
        const trace = tracy.traceNamed(@src(), "typesStore.appendVars");
        defer trace.end();
        // A source slice that lives inside this same list (a `sliceVars`
        // result) would dangle if the append reallocated the list, so callers
        // must copy such a slice out first. Debug builds check that here.
        if (builtin.mode == .debug and self.sliceAliasesVars(s)) {
            base.invariant("appendVars: source slice aliases the var list it is appended to", .{});
        }
        try self.vars.ensureUnusedCapacity(self.gpa, s.len + 1);
        _ = self.vars.appendAssumeCapacity(rangeLengthSlot(@intCast(s.len)));
        return self.vars.appendSliceAssumeCapacity(s);
    }

    /// The bit marking a vars-pool slot that holds the length of the range
    /// after it rather than a variable. Every variable is below it (the
    /// checked synthesized-type bit, see `representativeVar`), so a pass over
    /// the whole pool tells the two apart.
    const range_length_bit: u32 = @as(u32, 1) << 31;

    fn rangeLengthSlot(count: u32) Var {
        std.debug.assert(count < range_length_bit);
        return @fromBackingInt(count | range_length_bit);
    }

    fn isRangeLengthSlot(slot: Var) bool {
        return @backingInt(slot) & range_length_bit != 0;
    }

    /// What a stored descriptor keeps of the vars range `range`: its start,
    /// or the empty range's start for `Range.empty()`. Every other range of
    /// a stored descriptor came from `appendVars`, so its length is in the
    /// slot before its start.
    pub inline fn storedVarRange(self: *const Self, range: VarSafeList.Range) u32 {
        if (range.start == VarSafeList.Range.empty_start) {
            std.debug.assert(range.count == 0);
            return @backingInt(range.start);
        }
        const start: u32 = @backingInt(range.start);
        if (std.debug.runtime_safety) {
            if (start == 0 or self.vars.items[start - 1] != rangeLengthSlot(range.count)) {
                base.invariant("a stored descriptor holds vars range {d}+{d}, which appendVars did not make", .{ start, range.count });
            }
        }
        return start;
    }

    /// The vars range a stored descriptor keeps as `stored` (`storedVarRange`).
    pub inline fn varRange(self: *const Self, stored: u32) VarSafeList.Range {
        if (stored == @backingInt(VarSafeList.Range.empty_start)) return .empty();
        return .{ .start = @fromBackingInt(stored), .count = @backingInt(self.vars.items[stored - 1]) & ~range_length_bit };
    }

    fn sliceAliasesVars(self: *const Self, s: []const Var) bool {
        if (s.len == 0) return false;
        const items = self.vars.items;
        const items_start = @intFromPtr(items.ptr);
        const items_end = items_start + items.len * @sizeOf(Var);
        const source_start = @intFromPtr(s.ptr);
        return source_start >= items_start and source_start < items_end;
    }

    /// Append a slice of record fields to the backing list, returning the range
    pub fn appendRecordFields(self: *Self, slice: []const RecordField) std.mem.Allocator.Error!RecordFieldSafeMultiList.Range {
        self.debugAssertUnsealed();
        return try self.record_fields.appendSlice(self.gpa, slice);
    }

    /// Append a slice of tags to the backing list, returning the range
    pub fn appendTags(self: *Self, slice: []const Tag) std.mem.Allocator.Error!TagSafeMultiList.Range {
        self.debugAssertUnsealed();
        return try self.tags.appendSlice(self.gpa, slice);
    }

    /// Append interpolation part metadata to the backing list, returning the range
    pub fn appendInterpolationParts(self: *Self, slice: []const InterpolationPartMetadata) std.mem.Allocator.Error!InterpolationPartMetadata.SafeList.Range {
        self.debugAssertUnsealed();
        return try self.interpolation_parts.appendSlice(self.gpa, slice);
    }

    /// The range holding `prefix`'s constraints followed by `suffix`. When the
    /// room reserved after `prefix` fits `suffix`, `prefix` grows in place;
    /// otherwise both are copied to the end of the list, and a list of at
    /// least `min_constraint_room` constraints reserves as much room again
    /// after itself, so a list built one append at a time is copied a
    /// logarithmic number of times.
    pub fn appendStaticDispatchConstraintsAfter(
        self: *Self,
        prefix: StaticDispatchConstraint.SafeList.Range,
        suffix: []const StaticDispatchConstraint,
    ) std.mem.Allocator.Error!StaticDispatchConstraint.SafeList.Range {
        self.debugAssertUnsealed();
        const list = &self.static_dispatch_constraints;
        const start: u32 = @backingInt(prefix.start);
        const end: u32 = start + @as(u32, @intCast(prefix.count));
        if (!self.savepoint_active) {
            if (self.static_dispatch_constraint_room.get(end)) |room_end| {
                if (room_end - end >= suffix.len) {
                    std.debug.assert(room_end <= list.items().len);
                    @memcpy(list.items()[end..][0..suffix.len], suffix);
                    std.debug.assert(self.static_dispatch_constraint_room.remove(end));
                    const new_end: u32 = end + @as(u32, @intCast(suffix.len));
                    if (new_end < room_end) try self.static_dispatch_constraint_room.put(self.gpa, new_end, room_end);
                    return .{ .start = prefix.start, .count = @intCast(new_end - start) };
                }
            }
        }
        const total: usize = @as(usize, prefix.count) + suffix.len;
        const room = if (!self.savepoint_active and total >= min_constraint_room) total else 0;
        try list.ensureUnusedCapacity(self.gpa, total + room);
        if (room != 0) try self.static_dispatch_constraint_room.ensureUnusedCapacity(self.gpa, 1);
        const new_start: u32 = @intCast(list.items().len);
        _ = list.appendSliceAssumeCapacity(list.items()[start..end]);
        _ = list.appendSliceAssumeCapacity(suffix);
        if (room != 0) {
            // The room holds copies of the last constraint until a later
            // append overwrites them; no range covers them.
            const last = list.items()[list.items().len - 1];
            list.appendNTimesAssumeCapacity(last, room);
            const new_end: u32 = new_start + @as(u32, @intCast(total));
            self.static_dispatch_constraint_room.putAssumeCapacity(new_end, new_end + @as(u32, @intCast(room)));
        }
        return .{ .start = @fromBackingInt(new_start), .count = @intCast(total) };
    }

    /// The smallest constraint list `appendStaticDispatchConstraintsAfter`
    /// reserves room after.
    const min_constraint_room = 8;

    /// Append static dispatch constraints to the backing list, returning the range
    pub fn appendStaticDispatchConstraints(self: *Self, s: []const StaticDispatchConstraint) std.mem.Allocator.Error!StaticDispatchConstraint.SafeList.Range {
        self.debugAssertUnsealed();
        return try self.static_dispatch_constraints.appendSlice(self.gpa, s);
    }

    // sub list getters //

    /// Given a range, get a slice of vars from the backing array
    pub fn sliceVars(self: *const Self, range: VarSafeList.Range) []Var {
        return self.vars.sliceRange(range);
    }

    /// Get a var at a specific offset within a range.
    /// Use this for index-based iteration when unification may trigger reallocations.
    pub fn getVarAt(self: *const Self, range: VarSafeList.Range, offset: u32) Var {
        std.debug.assert(offset < range.count);
        const idx: VarSafeList.Idx = @fromBackingInt(@intCast(@backingInt(range.start) + offset));
        return self.vars.get(idx).*;
    }

    /// Given a range, get a slice of record fields from the backing array
    pub fn getRecordFieldsSlice(self: *const Self, range: RecordFieldSafeMultiList.Range) RecordFieldSafeMultiList.Slice {
        return self.record_fields.sliceRange(range);
    }

    /// Get a record field at a specific offset within a range.
    /// Use this for index-based iteration when checking can trigger reallocations.
    pub fn getRecordFieldAt(self: *const Self, range: RecordFieldSafeMultiList.Range, offset: u32) RecordField {
        std.debug.assert(offset < range.count);
        const idx: RecordFieldSafeMultiList.Idx = @fromBackingInt(@intCast(@backingInt(range.start) + offset));
        return self.record_fields.get(idx);
    }

    /// Given a range, get a iter of record fields from the backing array
    pub fn iterRecordFields(self: *const Self, range: RecordFieldSafeMultiList.Range) RecordFieldSafeMultiList.Iterator {
        return self.record_fields.iterRange(range);
    }

    /// Given a range, get a slice of tags from the backing array
    pub fn getTagsSlice(self: *const Self, range: TagSafeMultiList.Range) TagSafeMultiList.Slice {
        return self.tags.sliceRange(range);
    }

    /// Get a tag at a specific offset within a range.
    /// Use this for index-based iteration when checking can trigger reallocations.
    pub fn getTagAt(self: *const Self, range: TagSafeMultiList.Range, offset: u32) Tag {
        std.debug.assert(offset < range.count);
        const idx: TagSafeMultiList.Idx = @fromBackingInt(@intCast(@backingInt(range.start) + offset));
        return self.tags.get(idx);
    }

    /// Given a range, get a slice of interpolation part metadata from the backing array
    pub fn sliceInterpolationParts(self: *const Self, range: InterpolationPartMetadata.SafeList.Range) []InterpolationPartMetadata {
        return self.interpolation_parts.sliceRange(range);
    }

    /// Get an interpolation part at a specific offset within a range.
    /// Use this for index-based iteration when checking can trigger reallocations.
    pub fn getInterpolationPartAt(self: *const Self, range: InterpolationPartMetadata.SafeList.Range, offset: u32) InterpolationPartMetadata {
        std.debug.assert(offset < range.count);
        const idx: InterpolationPartMetadata.SafeList.Idx = @fromBackingInt(@intCast(@backingInt(range.start) + offset));
        return self.interpolation_parts.get(idx).*;
    }

    /// Pool interpolation metadata for a `from_interpolation` constraint.
    pub fn appendInterpolationMetadata(self: *Self, metadata: InterpolationMetadata) std.mem.Allocator.Error!InterpolationMetadata.OptIdx {
        self.debugAssertUnsealed();
        std.debug.assert(metadata.isPresent());
        return .from(try self.interpolation_metadata.append(self.gpa, metadata));
    }

    /// A constraint's interpolation metadata, or `.none` when it has none.
    pub fn getInterpolationMetadata(self: *const Self, idx: InterpolationMetadata.OptIdx) InterpolationMetadata {
        const pooled = idx.get() orelse return .none;
        return self.interpolation_metadata.get(pooled).*;
    }

    /// Record a numeric literal conversion's facts, for a constraint's
    /// `numeral`.
    pub fn appendNumeralInfo(self: *Self, info: NumeralInfo) std.mem.Allocator.Error!NumeralInfo.OptIdx {
        self.debugAssertUnsealed();
        return .from(try self.numeral_infos.append(self.gpa, info));
    }

    /// The facts of `constraint`'s numeric literal conversion, when its
    /// origin is one.
    pub fn constraintNumeralInfo(self: *const Self, constraint: StaticDispatchConstraint) ?NumeralInfo {
        const idx = constraint.numeral.get() orelse {
            std.debug.assert(constraint.origin.literalKind() != .numeral);
            return null;
        };
        std.debug.assert(constraint.origin.literalKind() == .numeral);
        return self.numeral_infos.get(idx).*;
    }

    /// Given a range, get a slice of vars from the backing array
    pub fn sliceStaticDispatchConstraints(self: *const Self, range: StaticDispatchConstraint.SafeList.Range) []StaticDispatchConstraint {
        return self.static_dispatch_constraints.sliceRange(range);
    }

    /// Get an iterator over static-dispatch constraints for the given range.
    /// Use this instead of sliceStaticDispatchConstraints when the iteration
    /// may append to the constraint store (e.g., instantiation/copy during a
    /// candidate probe)—a held slice would dangle on reallocation.
    pub fn iterStaticDispatchConstraints(self: *const Self, range: StaticDispatchConstraint.SafeList.Range) StaticDispatchConstraint.SafeList.Iterator {
        return self.static_dispatch_constraints.iterRange(range);
    }

    pub fn getStaticDispatchConstraintAt(self: *const Self, idx: usize) StaticDispatchConstraint {
        return self.static_dispatch_constraints.items()[idx];
    }

    // helpers - alias types //

    // Alias types contain a span of variables. In this span, the 1st element
    // is the backing variable, and the remainder are the arguments

    /// Get the backing var for this alias type
    pub fn getAliasBackingVar(self: *const Self, alias: Alias) Var {
        std.debug.assert(alias.vars.nonempty.count > 0);
        return self.vars.get(alias.vars.nonempty.start).*;
    }

    /// Source arguments only; hidden row parameters are not source arity.
    pub fn sliceAliasArgs(self: *const Self, alias: Alias) []Var {
        std.debug.assert(alias.vars.nonempty.count > 0);
        const slice = self.vars.sliceRange(alias.vars.nonempty);
        return slice[1..][0..alias.source_arg_count];
    }

    /// All alias parameters, including hidden implicit-row parameters.
    pub fn sliceAliasAllArgs(self: *const Self, alias: Alias) []Var {
        return self.vars.sliceRange(alias.vars.nonempty)[1..];
    }

    pub fn sliceAliasHiddenArgs(self: *const Self, alias: Alias) []Var {
        return self.sliceAliasAllArgs(alias)[alias.source_arg_count..];
    }

    /// Iterate source arguments only.
    pub fn iterAliasArgs(self: *const Self, alias: Alias) VarSafeList.Iterator {
        std.debug.assert(alias.vars.nonempty.count > 0);
        var span = alias.vars.nonempty;
        span.dropFirstElem();
        span.count = alias.source_arg_count;
        return self.vars.iterRange(span);
    }

    // helpers - nominal types //

    // A nominal application carries only its actual type arguments; backing
    // structure is resolved through the declaration table.

    /// Get the arg vars for this nominal type
    pub fn sliceNominalArgs(self: *const Self, nominal: NominalType) []Var {
        return self.vars.sliceRange(nominal.args);
    }

    /// Get the arg vars range for this nominal type.
    /// Returns a range (start index + count) which can be stored safely.
    /// Unlike sliceNominalArgs, this returns indices that remain valid even if
    /// the underlying storage is reallocated.
    pub fn getNominalArgsRange(nominal: NominalType) VarSafeList.Range {
        return nominal.args;
    }

    /// Get the an iterator arg vars for this nominal type
    pub fn iterNominalArgs(self: *const Self, nominal: NominalType) VarSafeList.Iterator {
        return self.vars.iterRange(nominal.args);
    }

    /// Whether this nominal application's declaration is known invalid
    /// (malformed backing or invalid recursion). Applications whose
    /// declaration cannot be resolved (no source declaration—possible only
    /// for hand-constructed types in tests) count as valid.
    pub fn nominalDeclIsInvalid(self: *const Self, nominal: NominalType) bool {
        const decl_idx = self.lookupNominalDecl(nominal) orelse return false;
        return !self.getNominalDecl(decl_idx).isValid();
    }

    // nominal declaration table //

    /// Register a nominal declaration, or update it if its key is already
    /// present (a declaration is re-registered when its body is generated
    /// after predeclaration). Returns the declaration's stable index.
    ///
    /// Must not run inside a unification savepoint: the declaration table is
    /// not journaled, so a rollback could not undo the registration.
    pub fn registerNominalDecl(self: *Self, decl: NominalDecl) Allocator.Error!NominalDecl.Idx {
        self.debugAssertUnsealed();
        std.debug.assert(!self.savepoint_active);
        std.debug.assert(decl.source.sourceDecl().present);

        const statement = decl.statement();
        const entries = self.nominal_decl_index.items();
        var lo: usize = 0;
        var hi: usize = entries.len;
        while (lo < hi) {
            const mid = lo + (hi - lo) / 2;
            switch (NominalDeclIndexEntry.orderByKey(decl.origin_module, statement, entries[mid])) {
                .lt => hi = mid,
                .gt => lo = mid + 1,
                .eq => {
                    const existing = entries[mid].decl;
                    self.noteNominalDeclWrite(decl);
                    self.nominal_decls.set(existing, decl);
                    return existing;
                },
            }
        }

        self.noteNominalDeclWrite(decl);
        const decl_idx = try self.nominal_decls.append(self.gpa, decl);
        try self.nominal_decl_index.insert(self.gpa, lo, .{
            .origin_module = decl.origin_module,
            .statement = statement,
            .decl = decl_idx,
        });
        return decl_idx;
    }

    /// Look up a nominal declaration by its key: the declaring module's
    /// env-local identity index plus the declaration statement in that module.
    pub fn lookupNominalDeclByKey(
        self: *const Self,
        origin_module: base.ModuleIdentity.Idx,
        statement: u32,
    ) ?NominalDecl.Idx {
        const entries = self.nominal_decl_index.items();
        var lo: usize = 0;
        var hi: usize = entries.len;
        while (lo < hi) {
            const mid = lo + (hi - lo) / 2;
            switch (NominalDeclIndexEntry.orderByKey(origin_module, statement, entries[mid])) {
                .lt => hi = mid,
                .gt => lo = mid + 1,
                .eq => return entries[mid].decl,
            }
        }
        return null;
    }

    /// Look up the declaration for a nominal application. Returns null only
    /// when the application carries no source declaration (possible for
    /// hand-constructed types in tests; checker-created applications always
    /// carry one).
    pub fn lookupNominalDecl(self: *const Self, nominal: NominalType) ?NominalDecl.Idx {
        const source_decl = nominal.sourceDecl();
        if (!source_decl.present) return null;
        return self.lookupNominalDeclByKey(nominal.origin_module, source_decl.statement);
    }

    /// The arguments a nominal application gives its declaration's declared
    /// formals: every argument but the trailing context arguments of a
    /// declaration in a function body (design.md "Body-Local Type Context"),
    /// which no source type and no printed type shows.
    pub fn nominalDeclaredArgs(self: *const Self, nominal: NominalType) []Var {
        const args = self.sliceNominalArgs(nominal);
        const idx = self.lookupNominalDecl(nominal) orelse return args;
        const context_len = self.getNominalDecl(idx).flags.context_len;
        std.debug.assert(context_len <= args.len);
        return args[0 .. args.len - context_len];
    }

    /// Get a nominal declaration by index.
    pub fn getNominalDecl(self: *const Self, idx: NominalDecl.Idx) NominalDecl {
        return self.nominal_decls.get(idx).*;
    }

    /// Overwrite a nominal declaration entry in place (used by copy_import to
    /// fill a reserved entry once its formals and backing have been copied).
    pub fn setNominalDecl(self: *Self, idx: NominalDecl.Idx, decl: NominalDecl) void {
        self.debugAssertUnsealed();
        std.debug.assert(!self.savepoint_active);
        self.noteNominalDeclWrite(decl);
        self.nominal_decls.set(idx, decl);
    }

    fn noteNominalDeclWrite(self: *Self, decl: NominalDecl) void {
        if (!decl.isValid()) self.invalid_nominal_decl_written = true;
    }

    /// Mark a nominal declaration invalid (malformed backing or invalid
    /// recursion). Applications of invalid declarations poison to err.
    pub fn markNominalDeclInvalid(self: *Self, idx: NominalDecl.Idx) void {
        self.debugAssertUnsealed();
        std.debug.assert(!self.savepoint_active);
        var decl = self.nominal_decls.get(idx).*;
        decl.flags.valid = false;
        self.invalid_nominal_decl_written = true;
        self.nominal_decls.set(idx, decl);
    }

    /// The number of registered nominal declarations.
    pub fn nominalDeclCount(self: *const Self) u64 {
        return self.nominal_decls.len();
    }

    // rank //

    /// Mark a class a generalization boundary found to be anonymous structure
    /// made in its own scope and left ungeneralized
    /// (`DescriptorFlags.per_use_structure`). A frozen class never changes,
    /// so no use can lift it and it stays shared.
    pub fn markPerUseStructure(self: *Self, desc_idx: DescStore.Idx) Allocator.Error!void {
        self.debugAssertUnsealed();
        var word = self.descs.getWord(desc_idx);
        if (word.flags.frozen or word.flags.per_use_structure) return;
        word.flags.per_use_structure = true;
        try self.setDescWord(desc_idx, word);
    }

    /// Set the rank for a descriptor. Writes the descriptor's word alone.
    pub inline fn setDescRank(self: *Self, desc_idx: DescStore.Idx, rank: Rank) Allocator.Error!void {
        self.debugAssertUnsealed();
        var word = self.descs.getWord(desc_idx);
        // A frozen class is ground, so no rank changes what it means.
        if (word.flags.frozen) return;
        word.rank = rank;
        try self.setDescWord(desc_idx, word);
    }

    // generalization marks //

    /// Set the `generalizing` mark of descriptor `desc_idx`
    /// (`types.Descriptor.Word`). A mark is a generalization pass's scratch,
    /// which the pass clears before it returns, so a mark write is neither
    /// journaled nor reported to class watches: no probe opens or rolls back
    /// while a pass runs, and a journaled row never carries a mark
    /// (`journalDescRow`).
    pub fn markGeneralizing(self: *Self, desc_idx: DescStore.Idx) void {
        self.debugAssertUnsealed();
        var word = self.descs.getWord(desc_idx);
        word.generalizing = true;
        self.descs.setWord(desc_idx, word);
    }

    /// Set the `rank_adjusted` mark of descriptor `desc_idx`; see
    /// `markGeneralizing`.
    pub fn markRankAdjusted(self: *Self, desc_idx: DescStore.Idx) void {
        self.debugAssertUnsealed();
        var word = self.descs.getWord(desc_idx);
        word.rank_adjusted = true;
        self.descs.setWord(desc_idx, word);
    }

    /// Clear both generalization marks of descriptor `desc_idx`; see
    /// `markGeneralizing`.
    pub fn clearGeneralizationMarks(self: *Self, desc_idx: DescStore.Idx) void {
        self.debugAssertUnsealed();
        var word = self.descs.getWord(desc_idx);
        word.generalizing = false;
        word.rank_adjusted = false;
        self.descs.setWord(desc_idx, word);
    }

    // word readers //

    /// A variable's class without its content: what `resolveVar` gives, with
    /// the descriptor's word in place of the decoded descriptor.
    pub const ResolvedVarWord = struct {
        var_: Var,
        is_root: bool,
        desc_idx: DescStore.Idx,
        word: Desc.Word,
    };

    /// `resolveVar`, decoding nothing but the descriptor's word.
    pub fn resolveVarWord(self: *const Self, var_: Var) ResolvedVarWord {
        const desc_idx = self.resolveDescIdx(var_);
        const checked_var = self.getRootMeta(desc_idx).checked_var;
        return .{ .var_ = checked_var, .is_root = var_ == checked_var, .desc_idx = desc_idx, .word = self.descs.getWord(desc_idx) };
    }

    /// The descriptor `var_`'s class resolves to, decoding nothing.
    pub fn resolveDescIdx(self: *const Self, var_: Var) DescStore.Idx {
        var slot_idx = Self.varToSlotIdx(var_);
        var guard = debug.IterationGuard.init("resolveDescIdx");
        while (true) {
            guard.tick();
            switch (self.slots.get(slot_idx)) {
                .redirect => |next| slot_idx = Self.varToSlotIdx(next),
                .root => |desc_idx| return desc_idx,
            }
        }
    }

    /// The kind of content `var_`'s class holds.
    pub fn kindOf(self: *const Self, var_: Var) types.ContentKind {
        return self.descs.getWord(self.resolveDescIdx(var_)).kind;
    }

    /// The word of descriptor `desc_idx`: its kind, flags and rank.
    pub inline fn wordAt(self: *const Self, desc_idx: DescStore.Idx) Desc.Word {
        return self.descs.getWord(desc_idx);
    }

    /// The kind of content descriptor `desc_idx` holds.
    pub fn kindAt(self: *const Self, desc_idx: DescStore.Idx) types.ContentKind {
        return self.descs.getWord(desc_idx).kind;
    }

    /// The rank of `var_`'s class.
    pub fn rankOf(self: *const Self, var_: Var) Rank {
        return self.descs.getWord(self.resolveDescIdx(var_)).rank;
    }

    /// The rank of descriptor `desc_idx`.
    pub fn rankAt(self: *const Self, desc_idx: DescStore.Idx) Rank {
        return self.descs.getWord(desc_idx).rank;
    }

    /// The flags of `var_`'s class.
    pub fn flagsOf(self: *const Self, var_: Var) types.DescriptorFlags {
        return self.descs.getWord(self.resolveDescIdx(var_)).flags;
    }

    /// The flags of descriptor `desc_idx`.
    pub fn flagsAt(self: *const Self, desc_idx: DescStore.Idx) types.DescriptorFlags {
        return self.descs.getWord(desc_idx).flags;
    }

    // resolvers //

    /// The storage root of one union-find tree. This identity is private to
    /// Store; checker consumers only observe RootMeta.checked_var.
    const ResolvedStorageRoot = struct {
        storage_var: Var,
        desc_idx: DescStore.Idx,
        desc: Desc,
        meta: RootMeta,
    };

    /// `ResolvedStorageRoot` without the descriptor: what the union-find walk
    /// finds, small enough to return in registers.
    const StorageRoot = struct {
        storage_var: Var,
        desc_idx: DescStore.Idx,
        meta: RootMeta,
    };

    /// The storage root of `initial_var`'s tree, decoding nothing.
    fn findStorageRoot(self: *const Self, initial_var: Var) StorageRoot {
        var redirected_slot_idx = Self.varToSlotIdx(initial_var);
        var redirected_slot: Slot = self.slots.get(redirected_slot_idx);
        var guard = debug.IterationGuard.init("findStorageRoot");

        while (true) {
            guard.tick();
            switch (redirected_slot) {
                .redirect => |next_redirect_var| {
                    redirected_slot_idx = Self.varToSlotIdx(next_redirect_var);
                    redirected_slot = self.slots.get(redirected_slot_idx);
                },
                .root => |desc_idx| {
                    return .{
                        .storage_var = Self.slotIdxToVar(redirected_slot_idx),
                        .desc_idx = desc_idx,
                        .meta = self.getRootMeta(desc_idx),
                    };
                },
            }
        }
    }

    /// The storage root of `initial_var`'s tree with its descriptor. The walk
    /// stays out of line and the decode is inlined into the caller, so the
    /// fields a caller never reads are never loaded.
    inline fn resolveStorageRoot(self: *const Self, initial_var: Var) ResolvedStorageRoot {
        return self.withDesc(self.findStorageRoot(initial_var));
    }

    inline fn withDesc(self: *const Self, root: StorageRoot) ResolvedStorageRoot {
        return .{
            .storage_var = root.storage_var,
            .desc_idx = root.desc_idx,
            .desc = self.descAt(root.desc_idx),
            .meta = root.meta,
        };
    }

    inline fn publicResolved(initial_var: Var, storage: ResolvedStorageRoot) ResolvedVarDesc {
        return .{
            .var_ = storage.meta.checked_var,
            .is_root = initial_var == storage.meta.checked_var,
            .desc_idx = storage.desc_idx,
            .desc = storage.desc,
        };
    }

    /// Given a type var, find its class descriptor and checked representative.
    ///
    /// Mutates storage redirects in place to compress the storage path. This
    /// never changes the checked representative returned to callers.
    pub inline fn resolveVarAndCompressPath(self: *Self, initial_var: Var) ResolvedVarDesc {
        return publicResolved(initial_var, self.withDesc(self.findStorageRootAndCompressPath(initial_var)));
    }

    /// `findStorageRoot`, compressing the storage path on the way.
    fn findStorageRootAndCompressPath(self: *Self, initial_var: Var) StorageRoot {
        self.debugAssertUnsealed();
        const storage = self.findStorageRoot(initial_var);

        // Compress the chain so future resolves are O(1). Skipped during a probe:
        // compression is a pure optimization (it never changes what a var
        // resolves to), so it would only be journaled and rolled back. Skipping
        // also keeps this resolver infallible (no journaling, no allocation).
        if (!self.savepoint_active and initial_var != storage.storage_var) {
            var compressed_slot_idx = Self.varToSlotIdx(initial_var);
            var compressed_slot: Slot = self.slots.get(compressed_slot_idx);
            var guard = debug.IterationGuard.init("resolveVarAndCompressPath");
            while (true) {
                guard.tick();
                switch (compressed_slot) {
                    .redirect => |next_redirect_var| {
                        // Raw set: not speculating here, so nothing to journal.
                        self.slots.set(compressed_slot_idx, Slot{ .redirect = storage.storage_var });
                        compressed_slot_idx = Self.varToSlotIdx(next_redirect_var);
                        compressed_slot = self.slots.get(compressed_slot_idx);
                    },
                    .root => break,
                }
            }
        }

        return storage;
    }

    /// What `resolveVar` gives for `checked_var`, the checked representative
    /// of descriptor `desc_idx`'s class, without walking to the root again.
    pub inline fn resolvedAtRoot(self: *const Self, checked_var: Var, desc_idx: DescStore.Idx) ResolvedVarDesc {
        std.debug.assert(self.getRootMeta(desc_idx).checked_var == checked_var);
        return .{ .var_ = checked_var, .is_root = true, .desc_idx = desc_idx, .desc = self.descAt(desc_idx) };
    }

    /// Given a type var, find its class descriptor and checked representative.
    pub inline fn resolveVar(self: *const Self, initial_var: Var) ResolvedVarDesc {
        return publicResolved(initial_var, self.resolveStorageRoot(initial_var));
    }

    /// Resolve `var_` through any chain of aliases to the content they name.
    pub inline fn resolveThroughAliases(self: *const Self, var_: Var) ResolvedVarDesc {
        const found = self.findThroughAliases(var_);
        return publicResolved(found.var_, self.withDesc(found.root));
    }

    /// The first variable on `var_`'s chain of alias backings whose class is
    /// not an alias, with its storage root, reading only descriptor words.
    fn findThroughAliases(self: *const Self, var_: Var) struct { var_: Var, root: StorageRoot } {
        var current = var_;
        var guard = debug.IterationGuard.init("resolveThroughAliases");
        while (true) {
            const root = self.findStorageRoot(current);
            if (self.descs.getWord(root.desc_idx).kind != .alias) return .{ .var_ = current, .root = root };
            guard.tick();
            current = self.aliasBackingVarAt(root.desc_idx);
        }
    }

    /// The backing var of the alias stored at `desc_idx`: the first of its
    /// vars (`getAliasBackingVar`).
    inline fn aliasBackingVarAt(self: *const Self, desc_idx: DescStore.Idx) Var {
        const row = self.descs.getRow(desc_idx);
        std.debug.assert(row.word.kind == .alias);
        return self.vars.items[row.payload[1]];
    }

    /// Whether `var_` resolves through aliases to a function structure.
    pub fn varResolvesToFunction(self: *const Self, var_: Var) bool {
        var current = var_;
        while (true) {
            const resolved = self.resolveVar(current);
            switch (resolved.desc.content) {
                .alias => |alias| current = self.getAliasBackingVar(alias),
                .structure => |flat| return switch (flat) {
                    .fn_pure, .fn_effectful, .fn_unbound => true,
                    .record,
                    .tuple,
                    .nominal_type,
                    .empty_record,
                    .tag_union,
                    .empty_tag_union,
                    => false,
                },
                // A presence variable never resolves to a function.
                .err, .flex, .rigid, .field_presence => return false,
            }
        }
    }

    // equivalence //

    /// The result of checking for equivalence
    pub const VarEquivResult = union(enum) { equiv, not_equiv: ResolvedVarDescs };

    /// Check if two variables are equivalent
    /// This will follow all redirects and compress the path
    ///
    /// If the vars are *not equivalent, then return the resolved vars & descs
    pub fn checkVarsEquiv(self: *Self, a_var: Var, b_var: Var) VarEquivResult {
        const a = self.resolveVarAndCompressPath(a_var);
        const b = self.resolveVarAndCompressPath(b_var);
        if (a.desc_idx == b.desc_idx) {
            return .equiv;
        } else {
            return .{ .not_equiv = .{ .a = a, .b = b } };
        }
    }

    // union //

    /// Merge two storage trees by structural union rank while adopting
    /// `destination_desc_idx` and `checked_var` for the resulting class.
    /// `a` and `b` are `StorageRoot`s or `ResolvedStorageRoot`s; only their
    /// storage vars and descriptor indices are read.
    fn linkStorageRoots(
        self: *Self,
        a: anytype,
        b: anytype,
        destination_desc_idx: DescStore.Idx,
        checked_var: Var,
    ) Allocator.Error!void {
        std.debug.assert(a.storage_var != b.storage_var);

        const a_rank = self.getUnionRank(a.storage_var);
        const b_rank = self.getUnionRank(b.storage_var);
        const parent_is_a = a_rank > b_rank;
        const ranks_tied = a_rank == b_rank;
        // Preserve the historical storage direction on ties. Besides making
        // small trees deterministic, this means a first merge still has a -> b
        // shape even though later unbalanced merges may retain a's storage root.
        const parent = if (parent_is_a) a else b;
        const child = if (parent_is_a) b else a;
        const combined_rank = if (ranks_tied)
            std.math.add(u8, self.getUnionRank(parent.storage_var), 1) catch unreachable // union-by-rank needs 2^r members for rank r, so a u32 var count keeps ranks at most 32
        else
            self.getUnionRank(parent.storage_var);

        if (self.derived_map_plans.items().len != 0) {
            try self.mergeClassDerivedMapPlans(a.desc_idx, b.desc_idx, destination_desc_idx);
        }
        try self.setRootMeta(destination_desc_idx, .{
            .checked_var = checked_var,
        });
        try self.setUnionRank(parent.storage_var, combined_rank);
        try self.setSlot(Self.varToSlotIdx(parent.storage_var), .{ .root = destination_desc_idx });
        try self.setSlot(Self.varToSlotIdx(child.storage_var), .{ .redirect = parent.storage_var });
    }

    /// Link the variables and update their class descriptor.
    ///
    /// The checked-identity merge direction remains load-bearing: `b` is always the
    /// surviving checked representative. Multiple parts of the unification
    /// algorithm depend on this specific order. Callers therefore control which
    /// variable survives by choosing operand order: a variable that must remain
    /// canonical (e.g. a shared expected-return var reused across branches and
    /// embedded in a function's annotated type) has to be passed as `b`.
    ///
    /// Storage-parent selection is independent and rank-balanced. Passing a
    /// variable as `b` does not require its slot to become the storage root.
    /// Alias spelling is not preserved by choosing an alias representative; source
    /// alias views stay separate from the concrete solved backing variable.
    pub fn union_(self: *Self, a_var: Var, b_var: Var, new_desc: Desc) Allocator.Error!void {
        self.debugAssertUnsealed();
        // Only the two classes' words are read: their kinds and flags.
        const a_data = self.findStorageRoot(a_var);
        const b_data = self.findStorageRoot(b_var);
        const a_word = self.descs.getWord(a_data.desc_idx);
        const b_word = self.descs.getWord(b_data.desc_idx);
        if (a_word.flags.frozen or b_word.flags.frozen) return self.unionWithFrozen(a_data, a_word, b_data, b_word, new_desc);

        var merged_desc = new_desc;
        merged_desc.flags.annotation_tag_ext = merged_desc.content == .flex and
            (a_word.flags.annotation_tag_ext or b_word.flags.annotation_tag_ext);
        merged_desc.flags.bounded_row_ext = a_word.flags.bounded_row_ext or b_word.flags.bounded_row_ext;
        merged_desc.flags.host_fixed_row = a_word.flags.host_fixed_row or b_word.flags.host_fixed_row;
        const merged_is_empty_tag_union = merged_desc.content == .structure and
            merged_desc.content.structure == .empty_tag_union;
        if (merged_is_empty_tag_union) {
            const a_is_explicit_empty = a_word.kind == .empty_tag_union and !a_word.flags.empty_tag_union_is_default;
            const b_is_explicit_empty = b_word.kind == .empty_tag_union and !b_word.flags.empty_tag_union_is_default;
            merged_desc.flags.empty_tag_union_is_default = !a_is_explicit_empty and !b_is_explicit_empty and
                (a_word.flags.empty_tag_union_is_default or b_word.flags.empty_tag_union_is_default);
        } else {
            merged_desc.flags.empty_tag_union_is_default = false;
        }
        // A rejected dispatch edge is a fact about the constraint callable's
        // equivalence class, so merging two classes rejects the result if
        // either side was rejected.
        merged_desc.flags.static_dispatch_rejected = a_word.flags.static_dispatch_rejected or
            b_word.flags.static_dispatch_rejected;
        // Declared backing structure stays declared: a class that merged with
        // an opened nominal backing component is that component.
        merged_desc.flags.nominal_backing_structure = a_word.flags.nominal_backing_structure or
            b_word.flags.nominal_backing_structure;
        // Structure copied per use stays so only while it is nothing else: a
        // class it merged with keeps its own sharing.
        merged_desc.flags.per_use_structure = a_word.flags.per_use_structure and
            b_word.flags.per_use_structure;
        // A defaulting decision about one side is a decision about the merged
        // class: the default it committed is the type both sides now share.
        merged_desc.flags.default_decided = a_word.flags.default_decided or
            b_word.flags.default_decided;
        if ((contentIsLeaf(merged_desc.content) and
            (!kindIsLeaf(a_word.kind) or !kindIsLeaf(b_word.kind))) or
            !sameCopiedFlags(merged_desc.flags, a_word.flags) or
            !sameCopiedFlags(merged_desc.flags, b_word.flags))
        {
            self.ground_epoch +%= 1;
        }

        if (a_data.storage_var == b_data.storage_var) {
            try self.setDesc(a_data.desc_idx, merged_desc);
            return;
        }

        // a's vars come to see the merged descriptor. When its content is
        // what a settled side already had, that side's settledness carries
        // over to the merged class; otherwise a settled side has changed.
        const settled_before = self.settled_descs.list.items.len;
        const a_settled = self.settled_descs.contains(a_data.desc_idx);
        const b_settled = self.settled_descs.contains(b_data.desc_idx);
        const merged_row = try self.encodeDesc(merged_desc);
        const a_kept = DescStore.sameContent(self.descs.getRow(a_data.desc_idx), merged_row);
        const b_kept = DescStore.sameContent(self.descs.getRow(b_data.desc_idx), merged_row);
        if (a_settled and !a_kept) self.settled_descs.clear();

        // The unifier computes merged content for b's descriptor destination.
        // Keep that destination even when balancing retains a's storage root.
        try self.setDescRow(b_data.desc_idx, merged_row);
        self.relinking_compared_classes = true;
        defer self.relinking_compared_classes = false;
        try self.linkStorageRoots(a_data, b_data, b_data.desc_idx, b_data.meta.checked_var);
        try self.freeDesc(a_data.desc_idx, a_data.storage_var);
        const nothing_changed = self.settled_descs.list.items.len == settled_before and settled_before != 0;
        if (nothing_changed and ((a_settled and a_kept) or (b_settled and b_kept))) {
            try self.settled_descs.insert(self.gpa, b_data.desc_idx);
        }
    }

    /// `union_` when either class is frozen. The merged class keeps the frozen
    /// descriptor, which already holds the type both sides agreed on, and `b`
    /// stays the checked representative as in every merge. Relating a frozen
    /// class to an error leaves the two classes apart: the error side is
    /// already poisoned, and the frozen side must not change.
    fn unionWithFrozen(self: *Self, a: StorageRoot, a_word: Desc.Word, b: StorageRoot, b_word: Desc.Word, new_desc: Desc) Allocator.Error!void {
        if (a.storage_var == b.storage_var) return;
        if (new_desc.content == .err) return;
        const frozen = if (b_word.flags.frozen) b else a;
        const other = if (b_word.flags.frozen) a else b;
        if (std.debug.runtime_safety) {
            const frozen_word = if (b_word.flags.frozen) b_word else a_word;
            const other_word = if (b_word.flags.frozen) a_word else b_word;
            if (other_word.flags.static_dispatch_rejected and !frozen_word.flags.static_dispatch_rejected) {
                base.invariant("a rejected dispatch callable was related to a frozen type class", .{});
            }
        }
        try self.linkStorageRoots(a, b, frozen.desc_idx, b.meta.checked_var);
        try self.freeDesc(other.desc_idx, other.storage_var);
    }

    /// Poison a failed unification at its two queried occurrences.
    ///
    /// Successful unification always merges whole equivalence classes. Error
    /// recovery is intentionally occurrence-directed: either operand can be a
    /// checked expression or pattern occurrence already connected to a shared
    /// binding, such as a lookup of a lambda parameter. If an operand is not
    /// its class's checked representative, poisoning that exact occurrence
    /// must not make the binding—or an incidental storage child of the
    /// occurrence—erroneous: the remaining class is re-rooted and flattened at
    /// its checked representative and keeps its content, and the occurrence
    /// joins the error class as a rank-zero singleton. An operand that is its
    /// class's checked representative owns the mismatch, so its whole class
    /// joins the error class.
    pub fn poisonOnMismatch(self: *Self, a_var: Var, b_var: Var) Allocator.Error!void {
        self.debugAssertUnsealed();
        self.ground_epoch +%= 1;
        // A frozen class never changes: an operand inside one is detached and
        // poisoned alone, and an anchor, which is a type's structure rather
        // than any occurrence, stays as it is.
        const a_open = try self.isolateFromFrozenClass(a_var);
        const b_open = try self.isolateFromFrozenClass(b_var);
        if (!a_open and !b_open) return;
        if (!a_open) return self.poisonOccurrence(b_var);
        if (!b_open) return self.poisonOccurrence(a_var);
        var a = self.resolveStorageRoot(a_var);
        var b = self.resolveStorageRoot(b_var);
        // The error class joins whatever derived map relation either
        // operand's class belonged to, as a merge of the two would.
        const err_plan = self.classDerivedMapPlan(b.desc_idx) orelse self.classDerivedMapPlan(a.desc_idx);
        // Poisoning replaces the content, not the rejection or defaulting
        // history: a class whose dispatch check was already rejected stays
        // rejected, and a class a default chose stays default-decided.
        const err_desc = Desc{
            .content = .err,
            .rank = Rank.generalized,
            .flags = .{
                .static_dispatch_rejected = a.desc.flags.static_dispatch_rejected or b.desc.flags.static_dispatch_rejected,
                .default_decided = a.desc.flags.default_decided or b.desc.flags.default_decided,
            },
        };

        if (a.storage_var == b.storage_var) {
            try self.setDesc(a.desc_idx, err_desc);
            return;
        }

        if (b_var == b.meta.checked_var) {
            try self.setDesc(b.desc_idx, err_desc);
        } else {
            try self.detachOccurrence(b, b_var);
            const err_desc_idx = try self.appendClass(err_desc, b_var);
            try self.setSlot(Self.varToSlotIdx(b_var), .{ .root = err_desc_idx });
            b = .{
                .storage_var = b_var,
                .desc_idx = err_desc_idx,
                .desc = err_desc,
                .meta = .{ .checked_var = b_var },
            };
        }

        if (a_var != a.meta.checked_var) {
            try self.detachOccurrence(a, a_var);
            try self.setSlot(Self.varToSlotIdx(a_var), .{ .root = b.desc_idx });
            a = .{
                .storage_var = a_var,
                .desc_idx = b.desc_idx,
                .desc = err_desc,
                .meta = .{ .checked_var = b.meta.checked_var },
            };
        }

        try self.linkStorageRoots(a, b, b.desc_idx, b.meta.checked_var);
        if (err_plan) |plan| try self.putClassDerivedMapPlan(b.desc_idx, plan);
        // Only a class that joined the error class whole loses its
        // descriptor; a detached occurrence already shares `b`'s.
        if (a.desc_idx != b.desc_idx) try self.freeDesc(a.desc_idx, a.storage_var);
    }

    /// Poison one occurrence the way `poisonOnMismatch` poisons each operand:
    /// an occurrence that only joined its class (a lookup of a binding) is
    /// detached and poisoned alone, and the class keeps its type.
    pub fn poisonOccurrence(self: *Self, var_: Var) Allocator.Error!void {
        self.debugAssertUnsealed();
        const class = self.resolveStorageRoot(var_);
        const err_desc = Desc{
            .content = .err,
            .rank = Rank.generalized,
            .flags = .{ .static_dispatch_rejected = class.desc.flags.static_dispatch_rejected },
        };
        if (var_ == class.meta.checked_var) return self.setDesc(class.desc_idx, err_desc);
        try self.detachOccurrence(class, var_);
        const err_desc_idx = try self.appendClass(err_desc, var_);
        try self.inheritClassDerivedMapPlan(class.desc_idx, err_desc_idx);
        try self.setSlot(Self.varToSlotIdx(var_), .{ .root = err_desc_idx });
    }

    /// Detach `occurrence`, a member of `class` other than its checked
    /// representative, from the class. The remaining members are re-rooted and
    /// flattened at the checked representative and keep the class descriptor.
    /// `occurrence` is left as a rank-zero storage root whose slot the caller
    /// assigns.
    fn detachOccurrence(self: *Self, class: ResolvedStorageRoot, occurrence: Var) Allocator.Error!void {
        std.debug.assert(!self.savepoint_active);
        const checked_var = class.meta.checked_var;
        std.debug.assert(occurrence != checked_var);

        const class_members = &self.detach_members_scratch;
        class_members.clearRetainingCapacity();
        var raw_var: u32 = 0;
        while (raw_var < self.len()) : (raw_var += 1) {
            const candidate: Var = @fromBackingInt(@intCast(raw_var));
            if (self.resolveStorageRoot(candidate).storage_var == class.storage_var) {
                try class_members.append(self.gpa, candidate);
            }
        }

        try self.setSlot(Self.varToSlotIdx(checked_var), .{ .root = class.desc_idx });
        const remaining_class_rank: u8 = if (class_members.items.len > 2) 1 else 0;
        try self.setUnionRank(checked_var, remaining_class_rank);
        for (class_members.items) |member| {
            if (member == checked_var or member == occurrence) continue;
            try self.setUnionRank(member, 0);
            try self.setSlot(Self.varToSlotIdx(member), .{ .redirect = checked_var });
        }

        // `occurrence` has no remaining storage children after the exact class
        // flatten above, so its singleton structural rank is zero.
        try self.setUnionRank(occurrence, 0);
    }

    // test helpers //

    /// Get the slot for the provided var
    /// Used in tests
    /// If you're reaching for this in non-test code, you probably want
    /// resolveVar or resolveVarAndCompressPath instead
    pub fn getSlot(self: *Self, var_: Var) Slot {
        return self.slots.get(Self.varToSlotIdx(var_));
    }

    /// Get the descriptor for the provided idx
    /// Used in tests
    pub inline fn getDesc(self: *Self, desc_idx: DescStore.Idx) Desc {
        return self.descAt(desc_idx);
    }

    const Error = error{VarNotRoot};

    /// Set a root var to be the specified content
    /// Used in tests
    pub fn setRootVarContent(self: *Self, var_: Var, content: Content) (error{VarNotRoot} || Allocator.Error)!void {
        self.debugAssertUnsealed();
        const slot = self.slots.get(Self.varToSlotIdx(var_));
        switch (slot) {
            .root => |desc_idx| {
                var desc = self.descAt(desc_idx);
                self.noteContentRewrite(desc.content);
                desc.content = content;
                try self.setDesc(desc_idx, desc);
            },
            .redirect => {
                return error.VarNotRoot;
            },
        }
    }

    // helpers //

    pub fn varToSlotIdx(var_: Var) SlotStore.Idx {
        return @fromBackingInt(@intCast(@backingInt(var_)));
    }

    fn rootMetaIdx(desc_idx: DescStore.Idx) RootMetaSafeMultiList.Idx {
        return @fromBackingInt(@intCast(@backingInt(desc_idx)));
    }

    fn unionRankIdx(slot_idx: SlotStore.Idx) UnionRankSafeList.Idx {
        return @fromBackingInt(@intCast(@backingInt(slot_idx)));
    }

    fn slotIdxToVar(slot_idx: SlotStore.Idx) Var {
        return @fromBackingInt(@intCast(@backingInt(slot_idx)));
    }

    // serialization //

    /// Serialized representation of types store
    /// Uses extern struct to guarantee consistent field layout across optimization levels.
    pub const Serialized = extern struct {
        gpa: [2]u64, // Reserve space for allocator (vtable ptr + context ptr), provided during deserialization
        slots: SlotStore.Serialized,
        descs: DescStore.Serialized,
        root_metas: RootMetaSafeMultiList.Serialized,
        union_ranks: UnionRankSafeList.Serialized,
        vars: VarSafeList.Serialized,
        record_fields: RecordFieldSafeMultiList.Serialized,
        tags: TagSafeMultiList.Serialized,
        interpolation_parts: InterpolationPartMetadata.SafeList.Serialized,
        numeral_infos: NumeralInfo.SafeList.Serialized,
        interpolation_metadata: InterpolationMetadata.MetadataList.Serialized,
        static_dispatch_constraints: StaticDispatchConstraint.SafeList.Serialized,
        nominal_decls: NominalDecl.SafeList.Serialized,
        nominal_decl_index: NominalDeclIndexEntry.SafeList.Serialized,
        derived_map_plans: DerivedMapPlanEntry.SafeList.Serialized,
        widening_names: collections.SafeList(u64).Serialized,
        nominal_identities: NominalIdentity.SafeList.Serialized,
        alias_identities: AliasIdentity.SafeList.Serialized,
        /// `Store.invalid_nominal_decl_written`, as 0 or 1.
        invalid_nominal_decl_written: u8,
        /// `DescStore.err_written` of `descs`, as 0 or 1.
        descs_err_written: u8,
        _padding: [6]u8 = @splat(0),

        /// Serialize a Store into this Serialized struct, appending data to the writer
        pub fn serialize(
            self: *Serialized,
            store: *const Store,
            allocator: Allocator,
            writer: *collections.CompactWriter,
        ) Allocator.Error!void {
            // Serialize each component
            try self.slots.serialize(&store.slots, allocator, writer);
            try self.descs.serialize(&store.descs, allocator, writer);
            try self.root_metas.serialize(&store.root_metas, allocator, writer);
            try self.union_ranks.serialize(&store.union_ranks, allocator, writer);
            try self.vars.serialize(&store.vars, allocator, writer);
            try self.record_fields.serialize(&store.record_fields, allocator, writer);
            try self.tags.serialize(&store.tags, allocator, writer);
            try self.interpolation_parts.serialize(&store.interpolation_parts, allocator, writer);
            try self.numeral_infos.serialize(&store.numeral_infos, allocator, writer);
            try self.interpolation_metadata.serialize(&store.interpolation_metadata, allocator, writer);
            try self.static_dispatch_constraints.serialize(&store.static_dispatch_constraints, allocator, writer);
            try self.nominal_decls.serialize(&store.nominal_decls, allocator, writer);
            try self.nominal_decl_index.serialize(&store.nominal_decl_index, allocator, writer);
            try self.derived_map_plans.serialize(&store.derived_map_plans, allocator, writer);
            try self.widening_names.serialize(&store.widening_names, allocator, writer);
            try self.nominal_identities.serialize(&store.nominal_identities, allocator, writer);
            try self.alias_identities.serialize(&store.alias_identities, allocator, writer);
            self.invalid_nominal_decl_written = @intFromBool(store.invalid_nominal_decl_written);
            self.descs_err_written = @intFromBool(store.descs.err_written);
            self._padding = @splat(0);

            // Set gpa to all zeros; the space needs to be here,
            // but the value will be set separately during deserialization.
            self.gpa = .{ 0, 0 };
        }

        /// A Store whose columns borrow the serialized columns at
        /// `base_addr`. A `.sealed` store holds a solved module's types, which
        /// are only ever read. A `.writable` store is solved further: it writes
        /// its columns in place, and a column copies itself into owned memory
        /// the first time it grows. `deinit` frees only those copies.
        pub fn deserializeInto(self: *const Serialized, base_addr: usize, gpa: Allocator, access: LoadedAccess) Store {
            var store = Store{
                .gpa = gpa,
                .sealed = access == .sealed,
                .frozen_anchors = .init(gpa),
                .slots = self.slots.deserializeInto(base_addr),
                .descs = self.descs.deserializeInto(base_addr, self.descs_err_written != 0),
                .root_metas = self.root_metas.deserializeInto(base_addr),
                .union_ranks = self.union_ranks.deserializeInto(base_addr),
                .vars = self.vars.deserializeInto(base_addr),
                .record_fields = self.record_fields.deserializeInto(base_addr),
                .tags = self.tags.deserializeInto(base_addr),
                .interpolation_parts = self.interpolation_parts.deserializeInto(base_addr),
                .numeral_infos = self.numeral_infos.deserializeInto(base_addr),
                .interpolation_metadata = self.interpolation_metadata.deserializeInto(base_addr),
                .static_dispatch_constraints = self.static_dispatch_constraints.deserializeInto(base_addr),
                .nominal_decls = self.nominal_decls.deserializeInto(base_addr),
                .nominal_decl_index = self.nominal_decl_index.deserializeInto(base_addr),
                .derived_map_plans = self.derived_map_plans.deserializeInto(base_addr),
                .widening_names = self.widening_names.deserializeInto(base_addr),
                .nominal_identities = self.nominal_identities.deserializeInto(base_addr),
                .alias_identities = self.alias_identities.deserializeInto(base_addr),
                .invalid_nominal_decl_written = self.invalid_nominal_decl_written != 0,
            };
            if (access == .sealed) store.sealGrowingColumns();
            return store;
        }
    };
};

/// Which `Slot` variant a slot holds.
const SlotKind = enum(u8) { root, redirect };

/// Represents a store of slots. A slot's variant and its payload (a
/// `DescStore.Idx` or a redirect `Var`) are parallel columns, so the payload
/// column holds no tag padding.
const SlotStore = struct {
    const Self = @This();

    payloads: collections.ReservedList(u32, .dense),
    kinds: collections.ReservedList(SlotKind, .dense),

    fn init(gpa: Allocator, capacity: usize) std.mem.Allocator.Error!Self {
        var payloads = try collections.ReservedList(u32, .dense).initCapacity(gpa, capacity);
        errdefer payloads.deinit(gpa);
        return .{
            .payloads = payloads,
            .kinds = try collections.ReservedList(SlotKind, .dense).initCapacity(gpa, capacity),
        };
    }

    fn deinit(self: *Self, gpa: Allocator) void {
        self.payloads.deinit(gpa);
        self.kinds.deinit(gpa);
    }

    fn clone(self: *const Self, gpa: Allocator) Allocator.Error!Self {
        var payloads = try self.payloads.clone(gpa);
        errdefer payloads.deinit(gpa);
        return .{ .payloads = payloads, .kinds = try self.kinds.clone(gpa) };
    }

    fn len(self: *const Self) u64 {
        return self.payloads.len();
    }

    fn ensureTotalCapacity(self: *Self, gpa: Allocator, capacity: usize) Allocator.Error!void {
        try self.payloads.ensureTotalCapacity(gpa, capacity);
        try self.kinds.ensureTotalCapacity(gpa, capacity);
    }

    fn ensureUnusedCapacity(self: *Self, gpa: Allocator, additional: usize) Allocator.Error!void {
        try self.payloads.ensureUnusedCapacity(gpa, additional);
        try self.kinds.ensureUnusedCapacity(gpa, additional);
    }

    fn shrinkRetainingCapacity(self: *Self, new_len: usize) void {
        self.payloads.shrinkRetainingCapacity(new_len);
        self.kinds.shrinkRetainingCapacity(new_len);
    }

    /// Serialized representation of SlotStore
    /// Uses extern struct to guarantee consistent field layout across optimization levels.
    pub const Serialized = extern struct {
        payloads: collections.ReservedList(u32, .dense).Serialized,
        kinds: collections.ReservedList(SlotKind, .dense).Serialized,

        /// Serialize a SlotStore into this Serialized struct, appending data to the writer
        pub fn serialize(
            self: *Serialized,
            slot_store: *const SlotStore,
            allocator: Allocator,
            writer: *collections.CompactWriter,
        ) Allocator.Error!void {
            try self.payloads.serialize(&slot_store.payloads, allocator, writer);
            try self.kinds.serialize(&slot_store.kinds, allocator, writer);
        }

        /// Deserialize into a SlotStore value (no in-place modification of cache buffer).
        /// The base parameter is the base address of the serialized buffer in memory.
        pub fn deserializeInto(self: *const Serialized, base_addr: usize) SlotStore {
            return SlotStore{
                .payloads = self.payloads.deserializeInto(base_addr),
                .kinds = self.kinds.deserializeInto(base_addr),
            };
        }
    };

    /// Insert a new slot into the store
    fn insert(self: *Self, gpa: Allocator, typ: Slot) std.mem.Allocator.Error!Idx {
        try self.ensureUnusedCapacity(gpa, 1);
        return self.appendAssumeCapacity(typ);
    }

    /// Insert a value into the store assuming there is capacity
    fn appendAssumeCapacity(self: *Self, typ: Slot) Idx {
        const safe_idx = self.payloads.appendAssumeCapacity(payloadOf(typ));
        _ = self.kinds.appendAssumeCapacity(typ);
        return @fromBackingInt(@intCast(@backingInt(safe_idx)));
    }

    /// Set a value in the store
    pub fn set(self: *Self, idx: Idx, val: Slot) void {
        const i: usize = @backingInt(idx);
        self.payloads.items[i] = payloadOf(val);
        self.kinds.items[i] = val;
    }

    /// Get a value from the store
    fn get(self: *const Self, idx: Idx) Slot {
        const i: usize = @backingInt(idx);
        const payload = self.payloads.items[i];
        return switch (self.kinds.items[i]) {
            .root => .{ .root = @fromBackingInt(payload) },
            .redirect => .{ .redirect = @fromBackingInt(payload) },
        };
    }

    /// Every slot, in order, as an owned slice.
    fn toOwnedSlots(self: *const Self, gpa: Allocator) Allocator.Error![]Slot {
        const slots = try gpa.alloc(Slot, self.payloads.items.len);
        for (slots, 0..) |*slot, i| slot.* = self.get(@fromBackingInt(@intCast(i)));
        return slots;
    }

    fn payloadOf(slot: Slot) u32 {
        return switch (slot) {
            .root => |desc_idx| @backingInt(desc_idx),
            .redirect => |var_| @backingInt(var_),
        };
    }

    /// A type-safe index into the store
    const Idx = enum(u32) {
        first = 0,
        _,
    };
};

/// Represents a store of descriptors
///
/// Indexes into the list are typesafe
const DescStore = struct {
    const Self = @This();
    /// One stored row per descriptor (`Descriptor.Row`): a column of words and
    /// a column of payloads.
    const Rows = collections.ReservedColumns(Desc.Row, .dense);
    const DescSafeMultiList = Rows;

    backing: DescSafeMultiList,
    /// False only while no descriptor in `backing` has ever held `.err`.
    /// Serialized with the type store, so a loaded store carries the value of
    /// the store it was written from.
    err_written: bool = true,

    /// Init & allocated memory
    fn init(gpa: Allocator, capacity: usize) std.mem.Allocator.Error!Self {
        return .{ .backing = try DescSafeMultiList.initCapacity(gpa, capacity), .err_written = false };
    }

    /// Whether two rows hold the same content, whatever their words' flags
    /// and ranks.
    fn sameContent(a: Desc.Row, b: Desc.Row) bool {
        return a.word.kind == b.word.kind and a.word.literal == b.word.literal and std.mem.eql(u32, &a.payload, &b.payload);
    }

    /// Deinit & free allocated memory
    pub fn deinit(self: *Self, gpa: Allocator) void {
        self.backing.deinit(gpa);
    }

    /// Serialized representation of DescStore
    /// Uses extern struct to guarantee consistent field layout across optimization levels.
    pub const Serialized = extern struct {
        backing: DescSafeMultiList.Serialized,

        /// Serialize a DescStore into this Serialized struct, appending data to the writer
        pub fn serialize(
            self: *Serialized,
            desc_store: *const DescStore,
            allocator: Allocator,
            writer: *collections.CompactWriter,
        ) Allocator.Error!void {
            try self.backing.serialize(&desc_store.backing, allocator, writer);
        }

        /// A store whose descriptors borrow the serialized columns at
        /// `base_addr`; `err_written` is stored beside it in the type store.
        pub fn deserializeInto(self: *const Serialized, base_addr: usize, err_written: bool) DescStore {
            return .{ .backing = self.backing.deserializeInto(base_addr), .err_written = err_written };
        }
    };

    /// Append a stored row, assuming there is capacity.
    fn appendRowAssumeCapacity(self: *Self, row: Desc.Row) Idx {
        if (row.word.kind == .err) self.err_written = true;
        const safe_idx = self.backing.appendAssumeCapacity(row);
        return @fromBackingInt(@intCast(@backingInt(safe_idx)));
    }

    /// The stored row of descriptor `idx`.
    inline fn getRow(self: *const Self, idx: Idx) Desc.Row {
        return self.backing.get(rowIdx(idx));
    }

    /// Overwrite the stored row of descriptor `idx`.
    fn setRow(self: *Self, idx: Idx, row: Desc.Row) void {
        if (row.word.kind == .err) self.err_written = true;
        self.backing.set(rowIdx(idx), row);
    }

    /// The word of descriptor `idx`: its kind, flags and rank.
    inline fn getWord(self: *const Self, idx: Idx) Desc.Word {
        return self.backing.fieldItem(.word, rowIdx(idx));
    }

    /// Overwrite the word of descriptor `idx`, keeping its payload.
    fn setWord(self: *Self, idx: Idx, word: Desc.Word) void {
        self.backing.items(.word)[@backingInt(idx)] = word;
    }

    inline fn rowIdx(idx: Idx) Rows.Idx {
        return @fromBackingInt(@intCast(@backingInt(idx)));
    }

    /// A type-safe index into the store
    /// This type is made public below
    const Idx = enum(u32) {
        first = 0,
        _,
    };
};

/// An index into the desc store
pub const DescStoreIdx = DescStore.Idx;

// path compression

test "resolveVarAndCompressPath - flattens redirect chain to flex" {
    const gpa = std.testing.allocator;

    var store = try Store.init(gpa);
    defer store.deinit();

    const c = try store.fresh();
    const b = try store.freshRedirect(c);
    const a = try store.freshRedirect(b);

    const result = store.resolveVarAndCompressPath(a);
    try std.testing.expectEqual(Content{ .flex = Flex.init() }, result.desc.content);
    try std.testing.expectEqual(c, result.var_);
    try std.testing.expectEqual(Slot{ .redirect = c }, store.getSlot(a));
    try std.testing.expectEqual(Slot{ .redirect = c }, store.getSlot(b));
}

test "canonicalizing for seal gives children representatives and flattens rows" {
    const gpa = std.testing.allocator;
    var idents = try base.Ident.Store.initCapacity(gpa, 4);
    defer idents.deinit(gpa);
    const a_name = try idents.insert(gpa, base.Ident.for_text("a"));
    const b_name = try idents.insert(gpa, base.Ident.for_text("b"));
    const ok_name = try idents.insert(gpa, base.Ident.for_text("Ok"));
    const err_name = try idents.insert(gpa, base.Ident.for_text("Err"));

    var store = try Store.init(gpa);
    defer store.deinit();
    const elem = try store.fresh();
    const elem_alias = try store.freshRedirect(elem);
    const tail = try store.fresh();
    const inner = try store.freshFromContent(.{ .structure = .{ .record = .{
        .fields = try store.appendRecordFields(&.{.{ .name = a_name, .presence = .required(elem_alias) }}),
        .ext = tail,
    } } });
    const inner_alias = try store.freshRedirect(inner);
    const outer = try store.freshFromContent(.{ .structure = .{ .record = .{
        .fields = try store.appendRecordFields(&.{.{ .name = b_name, .presence = .required(elem_alias) }}),
        .ext = inner_alias,
    } } });
    const tag_tail = try store.fresh();
    const inner_tags = try store.freshFromContent(.{ .structure = .{ .tag_union = .{
        .tags = try store.appendTags(&.{.{ .name = ok_name, .args = try store.appendVars(&.{elem_alias}) }}),
        .ext = tag_tail,
    } } });
    const outer_tags = try store.freshFromContent(.{ .structure = .{ .tag_union = .{
        .tags = try store.appendTags(&.{.{ .name = err_name, .args = Var.SafeList.Range.empty() }}),
        .ext = inner_tags,
    } } });

    // Sealing retires a real committed probe and a thawed freeze, while the
    // selected plan and nominal backing remain part of the solved store.
    const callable = try store.fresh();
    const joined = try store.fresh();
    const plan = testDerivedMapPlan(5, 1);
    try store.setDerivedMapPlan(callable, plan);
    var savepoint = try store.createSavepoint();
    try store.union_(callable, joined, .{ .content = .{ .flex = Flex.init() }, .rank = Rank.outermost });
    store.commitSavepoint(&savepoint);
    const checked_callable = store.resolveVar(callable).var_;
    try store.freezeClass(checked_callable);
    try store.thawFrozenClasses();
    const origin: base.ModuleIdentity.Idx = @fromBackingInt(1);
    const decl = try store.registerNominalDecl(try testNominalDecl(origin, 7, elem_alias));
    try store.markSettled(&.{elem});

    try store.canonicalizeAndSeal(&idents);
    try std.testing.expect(store.isSealed());
    try std.testing.expectEqual(checked_callable, store.resolveVar(callable).var_);
    try std.testing.expectEqual(plan, store.derivedMapPlan(callable).?);
    try std.testing.expectEqual(plan, store.derivedMapPlan(joined).?);
    try std.testing.expectEqual(decl, store.lookupNominalDeclByKey(origin, 7).?);
    try std.testing.expectEqual(elem, store.getNominalDecl(decl).backing);
    try std.testing.expectEqual(@as(usize, 0), store.desc_trail.capacity);
    try std.testing.expectEqual(@as(usize, 0), store.derived_map_plans_undo.capacity);
    try std.testing.expectEqual(@as(usize, 0), store.frozen_anchors.capacity());
    try std.testing.expect(!store.isSettled(elem));

    const record = store.resolveVar(outer).desc.content.structure.record;
    try std.testing.expectEqual(tail, record.ext);
    const fields = store.getRecordFieldsSlice(record.fields);
    try std.testing.expectEqual(@as(usize, 2), fields.len);
    try std.testing.expectEqual(a_name, fields.items(.name)[0]);
    try std.testing.expectEqual(b_name, fields.items(.name)[1]);
    for (fields.items(.presence)) |presence| try std.testing.expectEqual(elem, presence.var_);

    const tag_union = store.resolveVar(outer_tags).desc.content.structure.tag_union;
    try std.testing.expectEqual(tag_tail, tag_union.ext);
    const tags = store.getTagsSlice(tag_union.tags);
    try std.testing.expectEqual(@as(usize, 2), tags.len);
    try std.testing.expectEqual(err_name, tags.items(.name)[0]);
    try std.testing.expectEqual(ok_name, tags.items(.name)[1]);
    try std.testing.expectEqualSlices(Var, &.{elem}, store.sliceVars(tags.items(.args)[1]));
}

test "sealing releases checker-only buffers without allocating" {
    const Snapshot = struct {
        fn bytes(store: *Store) Allocator.Error![]u8 {
            const allocator = std.testing.allocator;
            var writer = collections.CompactWriter.init();
            defer writer.deinit(allocator);
            const serialized = try writer.appendAlloc(allocator, Store.Serialized);
            try serialized.serialize(store, allocator, &writer);
            const buffer = try allocator.alloc(u8, writer.total_bytes);
            _ = writer.writeToBuffer(buffer) catch unreachable; // Buffer has exactly the writer's required size.
            return buffer;
        }
    };
    var counter = std.testing.FailingAllocator.init(std.testing.allocator, .{});
    const gpa = counter.allocator();
    {
        var store = try Store.init(gpa);
        defer store.deinit();
        const value = try store.fresh();
        const alias = try store.freshRedirect(value);
        const solved_bytes = counter.allocated_bytes - counter.freed_bytes;
        const before = try Snapshot.bytes(&store);
        defer std.testing.allocator.free(before);

        // Reserve every checker buffer, including capacity in empty lists and
        // maps returned to the nested-instantiation pool. None of this storage
        // is needed to answer reads of the solved type graph.
        inline for (.{ "frames", "value_stack", "pending_tags", "pending_fields", "pending_parts", "reach_edges", "reach_stack", "marker_pending", "marker_binders" }) |field| {
            try @field(store.instantiate_scratch, field).ensureTotalCapacity(gpa, 8);
        }
        store.instantiate_scratch.reach_heads = .init(gpa);
        try store.instantiate_scratch.reach_heads.?.put(value, 0);
        store.instantiate_scratch.reach_state = .init(gpa);
        try store.instantiate_scratch.reach_state.?.put(value, true);
        store.instantiate_scratch.marker_visited = .init(gpa);
        {
            const pool = &store.instantiate_scratch.marker_visited.?;
            var visited = pool.acquire();
            defer pool.release(&visited);
            try visited.put(value, 1);
        }
        try store.freezeClass(value);
        try store.thawFrozenClasses();
        try store.markSettled(&.{value});
        try store.class_watch.watched.resize(gpa, 8, false);
        try store.class_watch.watched_list.ensureTotalCapacity(gpa, 8);
        inline for (.{ "detach_members_scratch", "slot_trail", "desc_trail", "root_meta_trail", "union_rank_trail", "derived_map_plans_undo", "free_descs", "pending_free_descs" }) |field| {
            try @field(store, field).ensureTotalCapacity(gpa, 8);
        }
        try store.static_dispatch_constraint_room.ensureTotalCapacity(gpa, 8);
        try store.static_dispatch_relation_merges.append(gpa, .{ .dropped_fn_var = alias, .retained_fn_var = value });
        try std.testing.expect(counter.allocated_bytes - counter.freed_bytes > solved_bytes);
        const allocations_before_seal = counter.allocated_bytes;

        store.seal();

        try std.testing.expectEqual(allocations_before_seal, counter.allocated_bytes);
        try std.testing.expectEqual(solved_bytes, counter.allocated_bytes - counter.freed_bytes);
        try std.testing.expectEqual(value, store.resolveVar(alias).var_);
        // Storage ownership changes at seal; persisted contents do not.
        const after = try Snapshot.bytes(&store);
        defer std.testing.allocator.free(after);
        try std.testing.expectEqualSlices(u8, before, after);
    }
    try std.testing.expectEqual(counter.allocated_bytes, counter.freed_bytes);
}

/// A sealed store holding `a`, a redirect to the flex `b`, for the sealing
/// tests. The caller deinits it.
fn sealedTestStore(a: *Var, b: *Var) Allocator.Error!Store {
    var store = try Store.init(std.testing.allocator);
    errdefer store.deinit();
    b.* = try store.fresh();
    a.* = try store.freshRedirect(b.*);
    store.seal();
    return store;
}

test "a sealed store answers every read" {
    var a: Var = undefined;
    var b: Var = undefined;
    var store = try sealedTestStore(&a, &b);
    defer store.deinit();

    try std.testing.expect(store.isSealed());
    try std.testing.expectEqual(b, store.resolveVar(a).var_);
    try std.testing.expectEqual(Content{ .flex = Flex.init() }, store.resolveVar(a).desc.content);
    try std.testing.expectEqual(Slot{ .redirect = b }, store.getSlot(a));
    try std.testing.expectEqual(@as(u64, 2), store.len());
    try std.testing.expect(!store.varDefaultDecided(a));
    try std.testing.expect(!store.isSettled(a));
    try std.testing.expect(store.derivedMapPlan(a) == null);
}

fn writeSealedDescriptor() Allocator.Error!void {
    var a: Var = undefined;
    var b: Var = undefined;
    var store = try sealedTestStore(&a, &b);
    defer store.deinit();
    try store.setVarContent(a, .err);
}

test "a descriptor write to a sealed store is a violated invariant" {
    try base.death_test.expectPanic("a sealed type store was written", writeSealedDescriptor, .{});
}

fn createVarInSealedStore() Allocator.Error!void {
    var a: Var = undefined;
    var b: Var = undefined;
    var store = try sealedTestStore(&a, &b);
    defer store.deinit();
    _ = try store.fresh();
}

test "creating a var in a sealed store is a violated invariant" {
    try base.death_test.expectPanic("a sealed type store was written", createVarInSealedStore, .{});
}

fn appendToSealedPool() Allocator.Error!void {
    var a: Var = undefined;
    var b: Var = undefined;
    var store = try sealedTestStore(&a, &b);
    defer store.deinit();
    _ = try store.appendVars(&.{ a, b });
}

test "appending to a sealed store's pools is a violated invariant" {
    try base.death_test.expectPanic("a sealed type store was written", appendToSealedPool, .{});
}

fn unifySealedClasses() Allocator.Error!void {
    var a: Var = undefined;
    var b: Var = undefined;
    var store = try sealedTestStore(&a, &b);
    defer store.deinit();
    try store.union_(a, a, .{ .content = .{ .flex = Flex.init() }, .rank = Rank.outermost });
}

test "merging classes of a sealed store is a violated invariant" {
    try base.death_test.expectPanic("a sealed type store was written", unifySealedClasses, .{});
}

fn openSavepointOnSealedStore() Allocator.Error!void {
    var a: Var = undefined;
    var b: Var = undefined;
    var store = try sealedTestStore(&a, &b);
    defer store.deinit();
    _ = try store.createSavepoint();
}

test "opening a savepoint on a sealed store is a violated invariant" {
    try base.death_test.expectPanic("a sealed type store was written", openSavepointOnSealedStore, .{});
}

fn compressSealedPath() Allocator.Error!void {
    var a: Var = undefined;
    var b: Var = undefined;
    var store = try sealedTestStore(&a, &b);
    defer store.deinit();
    _ = store.resolveVarAndCompressPath(a);
}

test "path compression on a sealed store is a violated invariant" {
    try base.death_test.expectPanic("a sealed type store was written", compressSealedPath, .{});
}

fn sealTwice() Allocator.Error!void {
    var a: Var = undefined;
    var b: Var = undefined;
    var store = try sealedTestStore(&a, &b);
    defer store.deinit();
    store.seal();
}

test "sealing a sealed store is a violated invariant" {
    try base.death_test.expectPanic("a type store was sealed twice", sealTwice, .{});
}

test "union rank keeps a long checked-representative chain storage-flat" {
    const gpa = std.testing.allocator;
    const statement_count = 15_000;

    var store = try Store.initCapacity(gpa, statement_count + 1, 1);
    defer store.deinit();

    const vars = try gpa.alloc(Var, statement_count + 1);
    defer gpa.free(vars);

    vars[0] = try store.fresh();
    for (1..vars.len) |i| {
        vars[i] = try store.fresh();
        try store.union_(vars[i - 1], vars[i], .{
            .content = .{ .flex = Flex.init() },
            .rank = Rank.outermost,
        });
    }

    const checked_var = vars[statement_count];
    const storage = store.resolveStorageRoot(vars[0]);
    try std.testing.expectEqual(@as(u8, 1), store.getUnionRank(storage.storage_var));
    try std.testing.expect(storage.storage_var != checked_var);
    try std.testing.expect(store.resolveVar(checked_var).is_root);
    try std.testing.expect(!store.resolveVar(storage.storage_var).is_root);

    for (vars) |var_| {
        const resolved = store.resolveVar(var_);
        try std.testing.expectEqual(checked_var, resolved.var_);

        var depth: usize = 0;
        var current = var_;
        while (true) {
            switch (store.getSlot(current)) {
                .root => break,
                .redirect => |parent| {
                    depth += 1;
                    current = parent;
                },
            }
        }
        try std.testing.expect(depth <= 1);
    }
}

test "declared redirects preserve destination checked identity and structural balance" {
    const gpa = std.testing.allocator;

    var store = try Store.init(gpa);
    defer store.deinit();

    const destination = try store.freshFromContent(.{ .structure = .empty_tag_union });
    const a = try store.fresh();
    const b = try store.fresh();
    try store.union_(a, b, .{ .content = try store.mkTagUnion(&.{}, destination), .rank = Rank.outermost });

    try store.dangerousSetVarRedirect(.row_union_normalization, b, destination);

    const storage = store.resolveStorageRoot(a);
    try std.testing.expectEqual(@as(u8, 1), store.getUnionRank(storage.storage_var));
    try std.testing.expectEqual(destination, store.resolveVar(a).var_);
    try std.testing.expectEqual(destination, store.resolveVar(b).var_);
    try std.testing.expectEqual(destination, store.resolveVar(destination).var_);
    try std.testing.expect(store.resolveVar(destination).is_root);
    try std.testing.expectEqual(Content{ .structure = .empty_tag_union }, storage.desc.content);
}

test "mismatch poisoning detaches an occurrence from its shared binding" {
    const gpa = std.testing.allocator;

    var store = try Store.init(gpa);
    defer store.deinit();

    const shared_binding = try store.fresh();
    const checked_occurrence = try store.freshRedirect(shared_binding);
    const incidental_storage_child = try store.freshRedirect(checked_occurrence);
    const mismatched_pattern = try store.fresh();

    try store.poisonOnMismatch(checked_occurrence, mismatched_pattern);

    const shared = store.resolveVar(shared_binding);
    try std.testing.expectEqual(shared_binding, shared.var_);
    try std.testing.expectEqual(Content{ .flex = Flex.init() }, shared.desc.content);
    try std.testing.expectEqual(shared_binding, store.resolveVar(incidental_storage_child).var_);
    try std.testing.expectEqual(Content{ .flex = Flex.init() }, store.resolveVar(incidental_storage_child).desc.content);

    const occurrence = store.resolveVar(checked_occurrence);
    try std.testing.expectEqual(mismatched_pattern, occurrence.var_);
    try std.testing.expectEqual(Content.err, occurrence.desc.content);
    try std.testing.expectEqual(mismatched_pattern, store.resolveVar(mismatched_pattern).var_);
    const error_storage = store.resolveStorageRoot(checked_occurrence);
    try std.testing.expectEqual(@as(u8, 1), store.getUnionRank(error_storage.storage_var));
}

test "mismatch poisoning detaches a second-operand occurrence from its shared binding" {
    const gpa = std.testing.allocator;

    var store = try Store.init(gpa);
    defer store.deinit();

    const expected_bool = try store.fresh();
    const shared_binding = try store.fresh();
    const lookup_occurrence = try store.freshRedirect(shared_binding);
    const incidental_storage_child = try store.freshRedirect(lookup_occurrence);

    try store.poisonOnMismatch(expected_bool, lookup_occurrence);

    const shared = store.resolveVar(shared_binding);
    try std.testing.expectEqual(shared_binding, shared.var_);
    try std.testing.expectEqual(Content{ .flex = Flex.init() }, shared.desc.content);
    try std.testing.expectEqual(shared_binding, store.resolveVar(incidental_storage_child).var_);
    try std.testing.expectEqual(Content{ .flex = Flex.init() }, store.resolveVar(incidental_storage_child).desc.content);

    for ([_]Var{ expected_bool, lookup_occurrence }) |var_| {
        const resolved = store.resolveVar(var_);
        try std.testing.expectEqual(lookup_occurrence, resolved.var_);
        try std.testing.expectEqual(Content.err, resolved.desc.content);
    }
}

test "mismatch poisoning detaches both operand occurrences from their shared bindings" {
    const gpa = std.testing.allocator;

    var store = try Store.init(gpa);
    defer store.deinit();

    const first_binding = try store.fresh();
    const first_occurrence = try store.freshRedirect(first_binding);
    const second_binding = try store.fresh();
    const second_occurrence = try store.freshRedirect(second_binding);

    try store.poisonOnMismatch(first_occurrence, second_occurrence);

    for ([_]Var{ first_binding, second_binding }) |binding| {
        const resolved = store.resolveVar(binding);
        try std.testing.expectEqual(binding, resolved.var_);
        try std.testing.expectEqual(Content{ .flex = Flex.init() }, resolved.desc.content);
    }
    for ([_]Var{ first_occurrence, second_occurrence }) |occurrence| {
        const resolved = store.resolveVar(occurrence);
        try std.testing.expectEqual(second_occurrence, resolved.var_);
        try std.testing.expectEqual(Content.err, resolved.desc.content);
    }
}

test "mismatch poisoning a non-storage-root checked representative poisons its whole class" {
    const gpa = std.testing.allocator;

    var store = try Store.init(gpa);
    defer store.deinit();

    const first = try store.fresh();
    const storage_root = try store.fresh();
    try store.union_(first, storage_root, .{ .content = .{ .flex = Flex.init() }, .rank = Rank.outermost });
    const checked_var = try store.fresh();
    try store.union_(storage_root, checked_var, .{ .content = .{ .flex = Flex.init() }, .rank = Rank.outermost });
    try std.testing.expect(store.resolveStorageRoot(first).storage_var != checked_var);

    const mismatch = try store.fresh();
    try store.poisonOnMismatch(checked_var, mismatch);

    for ([_]Var{ first, storage_root, checked_var, mismatch }) |var_| {
        const resolved = store.resolveVar(var_);
        try std.testing.expectEqual(mismatch, resolved.var_);
        try std.testing.expectEqual(Content.err, resolved.desc.content);
    }
}

/// A frozen empty record, and a use variable merged into its class that is
/// the class's checked representative, as a replayed edge leaves them.
fn frozenClassWithUse(store: *Store) Allocator.Error!struct { anchor: Var, use: Var } {
    const anchor = try store.freshFromContentWithRank(.{ .structure = .empty_record }, Rank.outermost);
    try store.freezeClass(anchor);
    const use = try store.fresh();
    try store.union_(anchor, use, .{ .content = .{ .structure = .empty_record }, .rank = Rank.outermost });
    return .{ .anchor = anchor, .use = use };
}

fn expectFrozenEmptyRecord(store: *const Store, var_: Var) error{ TestExpectedEqual, TestUnexpectedResult }!void {
    const resolved = store.resolveVar(var_);
    try std.testing.expectEqual(Content{ .structure = .empty_record }, resolved.desc.content);
    try std.testing.expect(resolved.desc.flags.frozen);
    try std.testing.expectEqual(Rank.outermost, resolved.desc.rank);
}

test "a merge with a frozen class keeps its descriptor and the second operand's identity" {
    const gpa = std.testing.allocator;
    var store = try Store.init(gpa);
    defer store.deinit();

    const class = try frozenClassWithUse(&store);
    const later = try store.fresh();
    try store.union_(later, class.use, .{ .content = .{ .flex = Flex.init() }, .rank = @fromBackingInt(3) });
    try expectFrozenEmptyRecord(&store, later);
    try std.testing.expectEqual(class.use, store.resolveVar(class.anchor).var_);
}

/// The descriptor index `var_`'s storage root names.
fn storageDescIdx(store: *const Store, var_: Var) DescStore.Idx {
    return store.resolveStorageRoot(var_).desc_idx;
}

/// Whether some slot names `desc_idx`.
fn descIsNamed(store: *const Store, desc_idx: DescStore.Idx) bool {
    for (store.slots.kinds.items, store.slots.payloads.items) |kind, payload| {
        switch (kind) {
            .root => if (payload == @backingInt(desc_idx)) return true,
            .redirect => {},
        }
    }
    return false;
}

test "a merge frees the losing descriptor and the next class takes it" {
    const gpa = std.testing.allocator;
    var store = try Store.init(gpa);
    defer store.deinit();

    const a = try store.fresh();
    const b = try store.fresh();
    const a_desc = storageDescIdx(&store, a);
    const b_desc = storageDescIdx(&store, b);
    const descs_before = store.descs.backing.len();
    try store.union_(a, b, .{ .content = .{ .flex = Flex.init() }, .rank = Rank.outermost });

    try std.testing.expectEqual(b_desc, store.resolveVar(a).desc_idx);
    try std.testing.expect(!descIsNamed(&store, a_desc));
    try std.testing.expectEqualSlices(DescStore.Idx, &.{a_desc}, store.free_descs.items);

    const c = try store.freshFromContent(.{ .structure = .empty_record });
    try std.testing.expectEqual(a_desc, store.resolveVar(c).desc_idx);
    try std.testing.expectEqual(c, store.resolveVar(c).var_);
    try std.testing.expectEqual(Content{ .structure = .empty_record }, store.resolveVar(c).desc.content);
    try std.testing.expectEqual(descs_before, store.descs.backing.len());
    try std.testing.expectEqual(@as(usize, 0), store.free_descs.items.len);
    try std.testing.expectEqual(Content{ .flex = Flex.init() }, store.resolveVar(a).desc.content);
}

test "the bulk fill takes freed descriptors before growing" {
    const gpa = std.testing.allocator;
    var store = try Store.init(gpa);
    defer store.deinit();

    const a = try store.fresh();
    const b = try store.fresh();
    const a_desc = storageDescIdx(&store, a);
    try store.union_(a, b, .{ .content = .{ .flex = Flex.init() }, .rank = Rank.outermost });
    const descs_before = store.descs.backing.len();

    try store.ensureTotalCapacity(@intCast(store.len() + 2));
    const c = store.appendFromContentAssumeCapacity(.{ .structure = .empty_record }, Rank.outermost);
    const d = store.appendFromContentAssumeCapacity(.{ .flex = Flex.init() }, Rank.outermost);
    try std.testing.expectEqual(a_desc, store.resolveVar(c).desc_idx);
    try std.testing.expectEqual(c, store.resolveVar(c).var_);
    try std.testing.expectEqual(Content{ .structure = .empty_record }, store.resolveVar(c).desc.content);
    try std.testing.expectEqual(descs_before + 1, store.descs.backing.len());
    try std.testing.expectEqual(d, store.resolveVar(d).var_);
}

test "a recycled descriptor is neither settled nor watched" {
    const gpa = std.testing.allocator;
    var store = try Store.init(gpa);
    defer store.deinit();

    const settled = try store.freshFromContent(.{ .structure = .empty_record });
    try store.markSettled(&.{settled});
    const settled_desc = storageDescIdx(&store, settled);
    const other = try store.freshFromContent(.{ .structure = .empty_record });
    // `settled`'s content is kept, so the merged class stays settled while
    // `settled`'s own descriptor dies.
    try store.union_(settled, other, .{ .content = .{ .structure = .empty_record }, .rank = Rank.outermost });
    try std.testing.expect(store.isSettled(other));
    try std.testing.expect(!store.settled_descs.contains(settled_desc));

    const reissued = try store.fresh();
    try std.testing.expectEqual(settled_desc, store.resolveVar(reissued).desc_idx);
    try std.testing.expect(!store.isSettled(reissued));
    const index: u32 = @backingInt(settled_desc);
    try std.testing.expect(!(index < store.class_watch.watched.bit_length and store.class_watch.watched.isSet(index)));
}

test "a merge of two frozen classes drops the dead descriptor's anchor" {
    const gpa = std.testing.allocator;
    var store = try Store.init(gpa);
    defer store.deinit();

    const first = try frozenClassWithUse(&store);
    const second = try frozenClassWithUse(&store);
    const first_desc = storageDescIdx(&store, first.anchor);
    const second_desc = storageDescIdx(&store, second.anchor);
    try std.testing.expectEqual(@as(usize, 2), store.frozen_anchors.count());

    try store.union_(first.use, second.use, .{ .content = .{ .structure = .empty_record }, .rank = Rank.outermost });
    try std.testing.expectEqual(second_desc, store.resolveVar(first.anchor).desc_idx);
    try std.testing.expect(!store.frozen_anchors.contains(first_desc));
    try std.testing.expectEqual(second.anchor, store.frozen_anchors.get(second_desc).?);
    try std.testing.expectEqual(first_desc, store.free_descs.items[store.free_descs.items.len - 1]);

    // The reissued descriptor is an ordinary class: not frozen, not anchored.
    const reissued = try store.fresh();
    try std.testing.expectEqual(first_desc, store.resolveVar(reissued).desc_idx);
    try std.testing.expect(!store.resolveVar(reissued).desc.flags.frozen);
    try store.setVarContent(reissued, .{ .structure = .empty_record });
    try std.testing.expectEqual(Content{ .structure = .empty_record }, store.resolveVar(reissued).desc.content);
    try expectFrozenEmptyRecord(&store, first.use);

    try store.thawFrozenClasses();
    try std.testing.expect(!store.resolveVar(first.use).desc.flags.frozen);
    try std.testing.expectEqual(Content{ .structure = .empty_record }, store.resolveVar(reissued).desc.content);
}

test "a mismatch that poisons a whole class frees its descriptor" {
    const gpa = std.testing.allocator;
    var store = try Store.init(gpa);
    defer store.deinit();

    const a = try store.freshFromContent(.{ .structure = .empty_record });
    const b = try store.freshFromContent(.{ .structure = .empty_tag_union });
    const a_desc = storageDescIdx(&store, a);
    try store.poisonOnMismatch(a, b);
    try std.testing.expect(store.resolveVar(a).desc.content == .err);
    try std.testing.expect(store.resolveVar(b).desc.content == .err);
    try std.testing.expect(!descIsNamed(&store, a_desc));
    try std.testing.expectEqualSlices(DescStore.Idx, &.{a_desc}, store.free_descs.items);

    // A detached occurrence keeps its binding's descriptor live.
    const binding = try store.freshFromContent(.{ .structure = .empty_record });
    const occurrence = try store.fresh();
    try store.union_(occurrence, binding, .{ .content = .{ .structure = .empty_record }, .rank = Rank.outermost });
    const binding_desc = store.resolveVar(binding).desc_idx;
    const other = try store.freshFromContent(.{ .structure = .empty_tag_union });
    const free_before = store.free_descs.items.len;
    try store.poisonOnMismatch(occurrence, other);
    try std.testing.expectEqual(binding_desc, store.resolveVar(binding).desc_idx);
    try std.testing.expectEqual(Content{ .structure = .empty_record }, store.resolveVar(binding).desc.content);
    try std.testing.expect(store.resolveVar(occurrence).desc.content == .err);
    try std.testing.expectEqual(free_before, store.free_descs.items.len);
}

test "resetting a storage root to unbound frees its old descriptor" {
    const gpa = std.testing.allocator;
    var store = try Store.init(gpa);
    defer store.deinit();

    const root = try store.freshFromContent(.{ .structure = .empty_record });
    const root_desc = storageDescIdx(&store, root);
    try store.resetVarToUnbound(root, Rank.outermost);
    try std.testing.expect(store.resolveVar(root).desc.content == .flex);
    try std.testing.expect(!descIsNamed(&store, root_desc));
    try std.testing.expectEqualSlices(DescStore.Idx, &.{root_desc}, store.free_descs.items);

    // A redirect leaves the remainder of its class on the old descriptor;
    // its fresh flex takes the freed one.
    const a = try store.fresh();
    const b = try store.freshFromContent(.{ .structure = .empty_record });
    try store.union_(a, b, .{ .content = .{ .structure = .empty_record }, .rank = Rank.outermost });
    const kept_desc = store.resolveVar(b).desc_idx;
    try std.testing.expectEqual(@as(usize, 1), store.free_descs.items.len);
    const freed = store.free_descs.items[0];
    const redirect = if (store.resolveStorageRoot(a).storage_var == a) b else a;
    try store.resetVarToUnbound(redirect, Rank.outermost);
    try std.testing.expect(store.resolveVar(redirect).desc.content == .flex);
    try std.testing.expectEqual(freed, store.resolveVar(redirect).desc_idx);
    try std.testing.expect(descIsNamed(&store, kept_desc));
    try std.testing.expectEqual(@as(usize, 0), store.free_descs.items.len);
}

test "a probe frees nothing until it commits and rollback restores the free list" {
    const gpa = std.testing.allocator;
    var store = try Store.init(gpa);
    defer store.deinit();

    // A class the probes merge away, and two descriptors freed before them.
    const d = try store.freshFromContent(.{ .structure = .empty_record });
    const d_desc = store.resolveVar(d).desc_idx;
    const a = try store.fresh();
    const b = try store.fresh();
    const c = try store.fresh();
    try store.union_(a, b, .{ .content = .{ .flex = Flex.init() }, .rank = Rank.outermost });
    try store.union_(b, c, .{ .content = .{ .flex = Flex.init() }, .rank = Rank.outermost });
    try std.testing.expectEqual(@as(usize, 2), store.free_descs.items.len);
    const free_before = try gpa.dupe(DescStore.Idx, store.free_descs.items);
    defer gpa.free(free_before);
    const descs_before = store.descs.backing.len();

    // A rolled-back probe: a fresh var takes a freed descriptor and the
    // merge inside orphans a pre-probe descriptor. Rollback puts the
    // descriptor and the free list back (the verifying savepoint also
    // cross-checks every descriptor against a full copy).
    {
        var savepoint = try store.createSavepointVerifying();
        const e = try store.fresh();
        try std.testing.expectEqual(free_before[1], store.resolveVar(e).desc_idx);
        try std.testing.expectEqual(@as(u32, 1), store.probe_taken_descs);
        try store.union_(d, a, .{ .content = .{ .flex = Flex.init() }, .rank = Rank.outermost });
        try std.testing.expectEqualSlices(DescStore.Idx, free_before, store.free_descs.items);
        try std.testing.expectEqualSlices(DescStore.Idx, &.{d_desc}, store.pending_free_descs.items);
        store.rollbackToSavepoint(&savepoint);
        try std.testing.expectEqualSlices(DescStore.Idx, free_before, store.free_descs.items);
        try std.testing.expectEqual(@as(u32, 0), store.probe_taken_descs);
        try std.testing.expectEqual(@as(usize, 0), store.pending_free_descs.items.len);
        try std.testing.expectEqual(d_desc, store.resolveVar(d).desc_idx);
        try std.testing.expectEqual(Content{ .structure = .empty_record }, store.resolveVar(d).desc.content);
        try std.testing.expectEqual(descs_before, store.descs.backing.len());
    }

    // A committed probe keeps its merge, so the orphan is freed at commit,
    // and the descriptor the probe took stays taken.
    {
        var savepoint = try store.createSavepoint();
        const e = try store.fresh();
        const e_desc = store.resolveVar(e).desc_idx;
        try std.testing.expectEqual(free_before[1], e_desc);
        try store.union_(d, a, .{ .content = .{ .flex = Flex.init() }, .rank = Rank.outermost });
        store.commitSavepoint(&savepoint);
        try std.testing.expectEqualSlices(DescStore.Idx, &.{ free_before[0], d_desc }, store.free_descs.items);
        try std.testing.expectEqual(@as(usize, 0), store.pending_free_descs.items.len);
        try std.testing.expectEqual(e_desc, store.resolveVar(e).desc_idx);
        try std.testing.expect(!descIsNamed(&store, d_desc));
        try std.testing.expectEqual(store.resolveVar(a).desc_idx, store.resolveVar(d).desc_idx);
        try std.testing.expectEqual(descs_before, store.descs.backing.len());
        for (store.free_descs.items) |freed| try std.testing.expect(!descIsNamed(&store, freed));
    }
}

test "poisoning a frozen class's checked representative detaches it alone" {
    const gpa = std.testing.allocator;
    var store = try Store.init(gpa);
    defer store.deinit();

    const class = try frozenClassWithUse(&store);
    const sibling = try store.freshRedirect(class.use);
    const mismatch = try store.fresh();
    try store.poisonOnMismatch(class.use, mismatch);

    try std.testing.expectEqual(Content.err, store.resolveVar(class.use).desc.content);
    try std.testing.expectEqual(Content.err, store.resolveVar(mismatch).desc.content);
    try expectFrozenEmptyRecord(&store, class.anchor);
    try expectFrozenEmptyRecord(&store, sibling);
    try std.testing.expectEqual(class.anchor, store.resolveVar(sibling).var_);
}

test "poisoning a frozen anchor leaves it as it is" {
    const gpa = std.testing.allocator;
    var store = try Store.init(gpa);
    defer store.deinit();

    const class = try frozenClassWithUse(&store);
    const mismatch = try store.fresh();
    try store.poisonOnMismatch(mismatch, class.anchor);

    try std.testing.expectEqual(Content.err, store.resolveVar(mismatch).desc.content);
    try expectFrozenEmptyRecord(&store, class.anchor);
    try expectFrozenEmptyRecord(&store, class.use);
}

test "a content write aimed at a frozen class's member changes only that member" {
    const gpa = std.testing.allocator;
    var store = try Store.init(gpa);
    defer store.deinit();

    const class = try frozenClassWithUse(&store);
    const member = try store.freshRedirect(class.use);
    try store.setVarContent(member, .err);

    try std.testing.expectEqual(Content.err, store.resolveVar(member).desc.content);
    try expectFrozenEmptyRecord(&store, class.anchor);
    try expectFrozenEmptyRecord(&store, class.use);
}

test "a row bound aimed at a frozen class's member changes only that member" {
    const gpa = std.testing.allocator;
    var store = try Store.init(gpa);
    defer store.deinit();

    const class = try frozenClassWithUse(&store);
    const member = try store.freshRedirect(class.use);
    try store.markBoundedRowExt(member);

    try std.testing.expect(store.resolveVar(member).desc.flags.bounded_row_ext);
    try expectFrozenEmptyRecord(&store, class.anchor);
    try expectFrozenEmptyRecord(&store, class.use);
    try std.testing.expect(!store.resolveVar(class.use).desc.flags.bounded_row_ext);

    const other = try store.freshRedirect(class.use);
    try store.clearBoundedRowExt(other);
    try std.testing.expect(!store.resolveVar(other).desc.flags.bounded_row_ext);
    try expectFrozenEmptyRecord(&store, class.anchor);
    try expectFrozenEmptyRecord(&store, class.use);
}

test "a frozen class keeps its rank and thawing ends the freeze" {
    const gpa = std.testing.allocator;
    var store = try Store.init(gpa);
    defer store.deinit();

    const class = try frozenClassWithUse(&store);
    try store.setDescRank(store.resolveVar(class.use).desc_idx, Rank.generalized);
    try expectFrozenEmptyRecord(&store, class.use);

    try store.thawFrozenClasses();
    try std.testing.expect(!store.resolveVar(class.anchor).desc.flags.frozen);
    try store.setDescRank(store.resolveVar(class.use).desc_idx, Rank.generalized);
    try std.testing.expectEqual(Rank.generalized, store.resolveVar(class.anchor).desc.rank);
}

test "dangerousSetVarRedirect requires a declared rule by signature" {
    // Zig has no negative-compile test harness, so the "an unreasoned call
    // does not build" guarantee is pinned by reflection: the signature must
    // take a `RedirectRule` before the two vars, and the enum must stay
    // exhaustive so only declared members can be passed. Removing the rule
    // parameter fails this test.
    const fn_info = @typeInfo(@TypeOf(Store.dangerousSetVarRedirect)).@"fn";
    try std.testing.expectEqual(4, fn_info.param_types.len);
    try std.testing.expectEqual(Store.RedirectRule, fn_info.param_types[1].?);
    comptime std.debug.assert(@typeInfo(Store.RedirectRule).@"enum".mode == .exhaustive);
}

test "settled classes survive content-preserving merges and clear on any observable change" {
    const gpa = std.testing.allocator;
    var store = try Store.init(gpa);
    defer store.deinit();

    // A flex merged into a settled class keeps the settled content, and the
    // merged class stays settled.
    const settled = try store.freshFromContent(.{ .structure = .empty_record });
    try store.markSettled(&.{settled});
    const joining = try store.fresh();
    try store.union_(joining, settled, .{ .content = .{ .structure = .empty_record }, .rank = Rank.outermost });
    try std.testing.expect(store.isSettled(settled));
    try std.testing.expect(store.isSettled(joining));

    // Changing a settled class's content clears every settled class.
    const other = try store.freshFromContent(.{ .structure = .empty_record });
    try store.markSettled(&.{other});
    try store.setVarContent(settled, .err);
    try std.testing.expect(!store.isSettled(other));
    try std.testing.expect(!store.isSettled(settled));

    // A settled flex whose vars come to see other content has changed.
    const settled_flex = try store.fresh();
    const witness = try store.freshFromContent(.{ .structure = .empty_record });
    try store.markSettled(&.{ settled_flex, witness });
    const structure = try store.freshFromContent(.{ .structure = .empty_record });
    try store.union_(settled_flex, structure, .{ .content = .{ .structure = .empty_record }, .rank = Rank.outermost });
    try std.testing.expect(!store.isSettled(witness));

    // A rollback clears every settled class.
    try store.markSettled(&.{witness});
    var savepoint = try store.createSavepoint();
    store.rollbackToSavepoint(&savepoint);
    try std.testing.expect(!store.isSettled(witness));
}

test "savepoint clone cross-check is compiled in for test builds" {
    try std.testing.expect(savepoint_verification == .clone_crosscheck);
}

test "savepoint trail is byte-for-byte identical to a full store copy+rollback" {
    const gpa = std.testing.allocator;

    // A few independent runs with different mutation mixes.
    var run: usize = 0;
    while (run < 4) : (run += 1) {
        var store = try Store.init(gpa);
        defer store.deinit();

        // Pre-savepoint content: a handful of vars, some redirected/unioned.
        const a = try store.fresh();
        const b = try store.fresh();
        _ = try store.freshRedirect(b);
        try store.union_(a, b, .{ .content = .err, .rank = Rank.generalized });

        // Independent oracle: keep our own copy of the pre-savepoint slots/descs
        // to compare against after rollback, alongside the verifying savepoint's
        // internal cross-check.
        const before_slots = try store.slots.toOwnedSlots(gpa);
        defer gpa.free(before_slots);
        var before_descs = try store.descs.backing.clone(gpa);
        defer before_descs.deinit(gpa);
        var before_root_metas = try store.root_metas.clone(gpa);
        defer before_root_metas.deinit(gpa);
        const before_union_ranks = try gpa.dupe(u8, store.union_ranks.items);
        defer gpa.free(before_union_ranks);
        const before_vars_len = store.vars.items.len;

        // Verifying savepoint: copies the whole store up front; rollback asserts
        // the trail restored it byte-for-byte (same behavior as restoring a copy).
        var sp = try store.createSavepointVerifying();

        // Mutations a probe might do, varied per run. These exercise: appends
        // (fresh/register), in-place writes to pre-existing entries (union_,
        // setVarContent, setDescRank), the same entry written twice (reverse
        // replay), and the compression path (a no-op while a savepoint is open).
        const fresh1 = try store.fresh();
        const fresh2 = try store.register(.{ .content = .{ .flex = Flex.init() }, .rank = Rank.outermost });
        try store.union_(fresh1, fresh2, .{ .content = .err, .rank = Rank.outermost });
        try store.setVarContent(a, .{ .flex = Flex.init() });
        try store.setVarContent(a, .err);
        if (run % 2 == 0) try store.setDescRank(store.resolveVar(b).desc_idx, Rank.outermost);
        _ = store.resolveVarAndCompressPath(a);

        store.rollbackToSavepoint(&sp);

        // The store must be byte-identical to its pre-savepoint state.
        try std.testing.expect(!store.savepoint_active);
        try std.testing.expectEqual(@as(u64, before_slots.len), store.slots.len());
        for (before_slots, 0..) |x, slot_index| {
            try std.testing.expect(std.meta.eql(x, store.slots.get(@fromBackingInt(@intCast(slot_index)))));
        }
        try std.testing.expectEqual(before_descs.len(), store.descs.backing.len());
        var i: usize = 0;
        while (i < before_descs.len()) : (i += 1) {
            try std.testing.expect(std.meta.eql(before_descs.get(@fromBackingInt(@intCast(i))), store.descs.backing.get(@fromBackingInt(@intCast(i)))));
        }
        try std.testing.expectEqual(before_root_metas.len(), store.root_metas.len());
        i = 0;
        while (i < before_root_metas.len()) : (i += 1) {
            try std.testing.expect(std.meta.eql(before_root_metas.get(@fromBackingInt(@intCast(i))), store.root_metas.get(@fromBackingInt(@intCast(i)))));
        }
        try std.testing.expectEqualSlices(u8, before_union_ranks, store.union_ranks.items);
        try std.testing.expectEqual(before_vars_len, store.vars.items.len);
    }
}

test "createSavepointVerifying cross-checks a probe-unify against a full copy" {
    const gpa = std.testing.allocator;

    var store = try Store.init(gpa);
    defer store.deinit();

    // A small typed environment a real probe would unify against.
    const a = try store.fresh();
    const b = try store.fresh();
    try store.union_(a, b, .{ .content = .err, .rank = Rank.generalized });

    // A probe brackets a trial unification it always discards. The verifying
    // savepoint copies the store up front; on rollback its internal cross-check
    // asserts the trail put the store back byte-for-byte—exactly as if we had
    // restored the full copy.
    var sp = try store.createSavepointVerifying();
    const c = try store.fresh();
    try store.union_(a, c, .{ .content = .{ .flex = Flex.init() }, .rank = Rank.outermost });
    try store.setVarContent(b, .err);
    _ = store.resolveVarAndCompressPath(a);
    store.rollbackToSavepoint(&sp);

    try std.testing.expect(!store.savepoint_active);
}

test "Store empty CompactWriter roundtrip" {
    const gpa = std.testing.allocator;
    const io = std.testing.io;
    const CompactWriter = collections.CompactWriter;

    // Create an empty Store
    var original = try Store.init(gpa);
    defer original.deinit();

    // Create a temp file
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    const file = try tmp_dir.dir.createFile(io, "test_empty_store.dat", .{ .read = true });
    defer file.close(io);

    // Serialize using CompactWriter
    var writer = CompactWriter.init();
    defer writer.deinit(gpa);

    const serialized = try writer.appendAlloc(gpa, Store.Serialized);
    try serialized.serialize(&original, gpa, &writer);

    // Write to file
    try writer.writeGather(file, io);

    // Read back
    const file_size = writer.total_bytes;
    const buffer = try gpa.alignedAlloc(u8, std.mem.Alignment.@"16", @intCast(file_size));
    defer gpa.free(buffer);

    _ = try file.readPositionalAll(io, buffer, 0);

    // Deserialize
    const deserialized_serialized: *const Store.Serialized = @ptrCast(@alignCast(buffer.ptr));
    var deserialized_value = deserialized_serialized.deserializeInto(@intFromPtr(buffer.ptr), gpa, .sealed);
    const deserialized = &deserialized_value;

    // Verify empty
    try std.testing.expectEqual(@as(usize, 0), deserialized.len());
}

test "Store basic CompactWriter roundtrip" {
    const gpa = std.testing.allocator;
    const io = std.testing.io;
    const CompactWriter = collections.CompactWriter;

    // Create original Store and add some types
    var original = try Store.init(gpa);
    defer original.deinit();

    // Create some type variables
    const flex = try original.fresh();
    const rigid = try original.freshFromContent(Content{ .rigid = Rigid.init(@bitCast(@as(u32, 42))) });

    // Create a redirect
    const redirect_var = try original.freshRedirect(flex);

    // Verify original values
    const flex_resolved = original.resolveVar(flex);
    try std.testing.expectEqual(Content{ .flex = Flex.init() }, flex_resolved.desc.content);

    const rigid_resolved = original.resolveVar(rigid);
    try std.testing.expectEqual(Content{ .rigid = Rigid.init(@bitCast(@as(u32, 42))) }, rigid_resolved.desc.content);

    const redirect_resolved = original.resolveVar(redirect_var);
    try std.testing.expectEqual(flex_resolved.desc_idx, redirect_resolved.desc_idx);

    // Create a temp file
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    const file = try tmp_dir.dir.createFile(io, "test_basic_store.dat", .{ .read = true });
    defer file.close(io);

    // Serialize using CompactWriter
    var writer = CompactWriter.init();
    defer writer.deinit(gpa);

    const serialized = try writer.appendAlloc(gpa, Store.Serialized);
    try serialized.serialize(&original, gpa, &writer);

    // Write to file
    try writer.writeGather(file, io);

    // Read back
    const file_size = writer.total_bytes;
    const buffer = try gpa.alignedAlloc(u8, std.mem.Alignment.@"16", @intCast(file_size));
    defer gpa.free(buffer);

    _ = try file.readPositionalAll(io, buffer, 0);

    // Deserialize
    const deserialized_serialized: *const Store.Serialized = @ptrCast(@alignCast(buffer.ptr));
    var deserialized_value = deserialized_serialized.deserializeInto(@intFromPtr(buffer.ptr), gpa, .sealed);
    const deserialized = &deserialized_value;

    // Verify the types are accessible
    try std.testing.expectEqual(@as(usize, 3), deserialized.len());

    const deser_flex_resolved = deserialized.resolveVar(flex);
    try std.testing.expectEqual(Content{ .flex = Flex.init() }, deser_flex_resolved.desc.content);

    const deser_rigid_resolved = deserialized.resolveVar(rigid);
    try std.testing.expectEqual(Content{ .rigid = Rigid.init(@bitCast(@as(u32, 42))) }, deser_rigid_resolved.desc.content);

    const deser_redirect_resolved = deserialized.resolveVar(redirect_var);
    try std.testing.expectEqual(deser_flex_resolved.desc_idx, deser_redirect_resolved.desc_idx);
}

fn testNominalDecl(origin_module: base.ModuleIdentity.Idx, statement: u32, backing: Var) error{OutOfMemory}!NominalDecl {
    return NominalDecl{
        .ident = .{ .ident_idx = @bitCast(@as(u32, 1)) },
        .origin_module = origin_module,
        .source = try NominalType.Source.initChecked(
            try SourceDecl.fromStatementChecked(statement),
            false,
            false,
        ),
        .formals = Var.SafeList.Range.empty(),
        .backing = backing,
        .flags = .{ .valid = true },
    };
}

test "nominal declaration table: register, lookup, upsert" {
    const gpa = std.testing.allocator;

    var store = try Store.init(gpa);
    defer store.deinit();

    const backing_a = try store.fresh();
    const backing_b = try store.fresh();
    const backing_c = try store.fresh();

    const origin_0: base.ModuleIdentity.Idx = @fromBackingInt(@intCast(1));
    const origin_1: base.ModuleIdentity.Idx = @fromBackingInt(@intCast(2));

    // Register out of key order to exercise sorted insertion.
    const idx_b = try store.registerNominalDecl(try testNominalDecl(origin_1, 5, backing_b));
    const idx_a = try store.registerNominalDecl(try testNominalDecl(origin_0, 9, backing_a));
    const idx_c = try store.registerNominalDecl(try testNominalDecl(origin_1, 2, backing_c));

    try std.testing.expectEqual(@as(u64, 3), store.nominalDeclCount());
    try std.testing.expectEqual(idx_a, store.lookupNominalDeclByKey(origin_0, 9).?);
    try std.testing.expectEqual(idx_b, store.lookupNominalDeclByKey(origin_1, 5).?);
    try std.testing.expectEqual(idx_c, store.lookupNominalDeclByKey(origin_1, 2).?);
    try std.testing.expectEqual(@as(?NominalDecl.Idx, null), store.lookupNominalDeclByKey(origin_0, 5));
    try std.testing.expectEqual(@as(?NominalDecl.Idx, null), store.lookupNominalDeclByKey(origin_1, 9));

    try std.testing.expectEqual(backing_a, store.getNominalDecl(idx_a).backing);

    // Re-registering the same key updates in place and keeps the index stable.
    var updated = try testNominalDecl(origin_0, 9, backing_c);
    const formal = try store.fresh();
    updated.formals = try store.appendVars(&.{formal});
    const idx_a_again = try store.registerNominalDecl(updated);
    try std.testing.expectEqual(idx_a, idx_a_again);
    try std.testing.expectEqual(@as(u64, 3), store.nominalDeclCount());
    try std.testing.expectEqual(backing_c, store.getNominalDecl(idx_a).backing);
    try std.testing.expectEqual(@as(u32, 1), store.getNominalDecl(idx_a).formals.count);

    // Validity flips in place.
    try std.testing.expect(store.getNominalDecl(idx_b).isValid());
    store.markNominalDeclInvalid(idx_b);
    try std.testing.expect(!store.getNominalDecl(idx_b).isValid());

    // Lookup through a nominal application resolves by (origin, statement).
    const app_content = try store.mkNominalWithSourceDecl(
        .{ .ident_idx = @bitCast(@as(u32, 1)) },
        &.{},
        origin_1,
        5,
        false,
    );
    const app = app_content.structure.nominal_type;
    try std.testing.expectEqual(idx_b, store.lookupNominalDecl(app).?);
}

test "nominal declaration table: CompactWriter roundtrip" {
    const gpa = std.testing.allocator;
    const io = std.testing.io;
    const CompactWriter = collections.CompactWriter;

    var original = try Store.init(gpa);
    defer original.deinit();

    const formal = try original.freshFromContent(Content{ .rigid = Rigid.init(@bitCast(@as(u32, 7))) });
    const backing = try original.freshFromContent(Content{ .structure = .empty_record });

    const origin: base.ModuleIdentity.Idx = @fromBackingInt(@intCast(3));
    var decl = try testNominalDecl(origin, 11, backing);
    decl.formals = try original.appendVars(&.{formal});
    _ = try original.registerNominalDecl(decl);

    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    const file = try tmp_dir.dir.createFile(io, "test_nominal_decls.dat", .{ .read = true });
    defer file.close(io);

    var writer = CompactWriter.init();
    defer writer.deinit(gpa);

    try (try writer.appendAlloc(gpa, Store.Serialized)).serialize(&original, gpa, &writer);
    try writer.writeGather(file, io);

    const file_size = writer.total_bytes;
    const buffer = try gpa.alignedAlloc(u8, std.mem.Alignment.@"16", @intCast(file_size));
    defer gpa.free(buffer);

    _ = try file.readPositionalAll(io, buffer, 0);

    const deserialized_serialized: *const Store.Serialized = @ptrCast(@alignCast(buffer.ptr));
    var deserialized_value = deserialized_serialized.deserializeInto(@intFromPtr(buffer.ptr), gpa, .sealed);
    const deserialized = &deserialized_value;

    try std.testing.expectEqual(@as(u64, 1), deserialized.nominalDeclCount());
    const deser_idx = deserialized.lookupNominalDeclByKey(origin, 11).?;
    const deser_decl = deserialized.getNominalDecl(deser_idx);
    try std.testing.expectEqual(backing, deser_decl.backing);
    try std.testing.expect(deser_decl.isValid());
    const deser_formals = deserialized.sliceVars(deser_decl.formals);
    try std.testing.expectEqual(@as(usize, 1), deser_formals.len);
    try std.testing.expectEqual(formal, deser_formals[0]);
}

fn testDerivedMapPlan(tag: u32, payload_index: u32) DerivedMapPlan {
    return .{ .tag_name = @bitCast(tag), .payload_index = payload_index };
}

test "a derived map plan follows its callable's class through merges and detaches" {
    const gpa = std.testing.allocator;
    var store = try Store.init(gpa);
    defer store.deinit();
    const flex_desc: Desc = .{ .content = .{ .flex = Flex.init() }, .rank = Rank.outermost };
    const plan = testDerivedMapPlan(5, 1);

    const callable = try store.fresh();
    const unrelated = try store.fresh();
    try store.setDerivedMapPlan(callable, plan);
    try std.testing.expectEqual(plan, store.derivedMapPlan(callable).?);
    try std.testing.expectEqual(@as(?DerivedMapPlan, null), store.derivedMapPlan(unrelated));

    // The merged class keeps the plan whichever side's descriptor survives.
    const joined_first = try store.fresh();
    try store.union_(joined_first, callable, flex_desc);
    const joined_second = try store.fresh();
    try store.union_(callable, joined_second, flex_desc);
    for ([_]Var{ callable, joined_first, joined_second }) |member| {
        try std.testing.expectEqual(plan, store.derivedMapPlan(member).?);
    }
    try std.testing.expectEqual(@as(usize, 1), store.derived_map_plans.items().len);

    // A merge of two planned classes keeps the second operand's plan.
    const other_plan = testDerivedMapPlan(9, 0);
    const other_callable = try store.fresh();
    try store.setDerivedMapPlan(other_callable, other_plan);
    try store.union_(other_callable, unrelated, flex_desc);
    try store.union_(callable, other_callable, flex_desc);
    try std.testing.expectEqual(other_plan, store.derivedMapPlan(joined_first).?);
    try std.testing.expectEqual(@as(usize, 1), store.derived_map_plans.items().len);

    // An occurrence poisoned alone stays in the relation it joined.
    try std.testing.expect(store.resolveVar(joined_first).var_ != joined_first);
    try store.poisonOccurrence(joined_first);
    try std.testing.expectEqual(Content.err, store.resolveVar(joined_first).desc.content);
    try std.testing.expectEqual(other_plan, store.derivedMapPlan(joined_first).?);
    try std.testing.expectEqual(other_plan, store.derivedMapPlan(callable).?);
    try std.testing.expectEqual(@as(usize, 2), store.derived_map_plans.items().len);
}

test "a rolled-back probe restores the derived map plans and a freed class forgets its plan" {
    const gpa = std.testing.allocator;
    var store = try Store.init(gpa);
    defer store.deinit();
    const flex_desc: Desc = .{ .content = .{ .flex = Flex.init() }, .rank = Rank.outermost };
    const plan = testDerivedMapPlan(3, 2);

    const callable = try store.fresh();
    const other = try store.fresh();
    try store.setDerivedMapPlan(callable, plan);
    {
        var savepoint = try store.createSavepointVerifying();
        try store.union_(callable, other, flex_desc);
        const fresh_callable = try store.fresh();
        try store.setDerivedMapPlan(fresh_callable, testDerivedMapPlan(4, 0));
        try std.testing.expectEqual(plan, store.derivedMapPlan(other).?);
        store.rollbackToSavepoint(&savepoint);
    }
    try std.testing.expectEqual(plan, store.derivedMapPlan(callable).?);
    try std.testing.expectEqual(@as(?DerivedMapPlan, null), store.derivedMapPlan(other));
    try std.testing.expectEqual(@as(usize, 1), store.derived_map_plans.items().len);

    // Resetting the class's storage root frees its descriptor, and the class
    // that takes the descriptor next has no plan.
    try store.resetVarToUnbound(callable, Rank.outermost);
    try std.testing.expectEqual(@as(?DerivedMapPlan, null), store.derivedMapPlan(callable));
    try std.testing.expectEqual(@as(usize, 0), store.derived_map_plans.items().len);
    const reuser = try store.fresh();
    try std.testing.expectEqual(@as(?DerivedMapPlan, null), store.derivedMapPlan(reuser));
}

test "derived map plans: CompactWriter roundtrip" {
    const gpa = std.testing.allocator;
    const io = std.testing.io;
    const CompactWriter = collections.CompactWriter;

    var original = try Store.init(gpa);
    defer original.deinit();
    const plain = try original.fresh();
    const callable = try original.fresh();
    const plan = testDerivedMapPlan(12, 3);
    try original.setDerivedMapPlan(callable, plan);

    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();
    const file = try tmp_dir.dir.createFile(io, "test_derived_map_plans.dat", .{ .read = true });
    defer file.close(io);

    var writer = CompactWriter.init();
    defer writer.deinit(gpa);
    try (try writer.appendAlloc(gpa, Store.Serialized)).serialize(&original, gpa, &writer);
    try writer.writeGather(file, io);

    const buffer = try gpa.alignedAlloc(u8, std.mem.Alignment.@"16", @intCast(writer.total_bytes));
    defer gpa.free(buffer);
    _ = try file.readPositionalAll(io, buffer, 0);

    const serialized: *const Store.Serialized = @ptrCast(@alignCast(buffer.ptr));
    const deserialized = serialized.deserializeInto(@intFromPtr(buffer.ptr), gpa, .sealed);
    try std.testing.expectEqual(plan, deserialized.derivedMapPlan(callable).?);
    try std.testing.expectEqual(@as(?DerivedMapPlan, null), deserialized.derivedMapPlan(plain));
}

test "Store comprehensive CompactWriter roundtrip" {
    const gpa = std.testing.allocator;
    const io = std.testing.io;
    const CompactWriter = collections.CompactWriter;
    var idents = try base.Ident.Store.initCapacity(gpa, 10);
    defer idents.deinit(gpa);

    var original = try Store.init(gpa);
    defer original.deinit();

    // Create various types
    const flex = try original.fresh();
    const str_var = try original.freshFromContent(Content{ .structure = .empty_record });
    const list_elem = try original.fresh();
    const list_ident_idx = base.Ident.Idx{ .attributes = .{ .effectful = false, .ignored = false, .reserved = false }, .idx = 999 };
    const builtin_module_idx = base.ModuleIdentity.Idx.NONE;
    const list_content = try original.mkNominal(
        .{ .ident_idx = list_ident_idx },
        &[_]Var{list_elem},
        builtin_module_idx,
        false,
    );
    const list_var = try original.freshFromContent(list_content);

    // Create a function type
    const arg1 = try original.fresh();
    const arg2 = try original.fresh();
    const ret = try original.fresh();
    const func_content = try original.mkFuncPure(&[_]Var{ arg1, arg2 }, ret);
    const func_var = try original.freshFromContent(func_content);

    // Create a record type: one field with a known-present type, one field
    // whose presence is still undetermined (both axes are variables).
    const field1_var = try original.fresh();
    const field2_var = try original.fresh();
    const field2_presence = try original.fresh();
    const record_fields = try original.appendRecordFields(&[_]RecordField{
        .{ .name = base.Ident.Idx{ .attributes = .{ .effectful = false, .ignored = false, .reserved = false }, .idx = 100 }, .presence = .required(field1_var) },
        .{ .name = base.Ident.Idx{ .attributes = .{ .effectful = false, .ignored = false, .reserved = false }, .idx = 200 }, .presence = .unknown(field2_presence, field2_var) },
    });
    const record_ext = try original.fresh();
    const record_content = Content{ .structure = .{ .record = .{ .fields = record_fields, .ext = record_ext } } };
    const record_var = try original.freshFromContent(record_content);

    // Create a tag union
    const tag1 = try original.mkTag(base.Ident.Idx{ .attributes = .{ .effectful = false, .ignored = false, .reserved = false }, .idx = 300 }, &[_]Var{flex});
    const tag2 = try original.mkTag(base.Ident.Idx{ .attributes = .{ .effectful = false, .ignored = false, .reserved = false }, .idx = 400 }, &[_]Var{ arg1, arg2 });
    const tag_union_ext = try original.fresh();
    const tag_union_content = try original.mkTagUnion(&[_]Tag{ tag1, tag2 }, tag_union_ext);
    const tag_union_var = try original.freshFromContent(tag_union_content);

    const alias_content = try original.mkAliasWithSourceDeclAndBuiltinOrigin(
        .{ .ident_idx = list_ident_idx },
        tag_union_var,
        &.{ flex, tag_union_ext },
        1,
        builtin_module_idx,
        null,
        false,
    );
    const alias_var = try original.freshFromContent(alias_content);

    // Create a temp file
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    const file = try tmp_dir.dir.createFile(io, "test_comprehensive_store.dat", .{ .read = true });
    defer file.close(io);

    // Serialize
    var writer = CompactWriter{
        .iovecs = .empty,
        .total_bytes = 0,
        .allocated_memory = .empty,
    };
    defer writer.deinit(gpa);

    const serialized = try writer.appendAlloc(gpa, Store.Serialized);
    try serialized.serialize(&original, gpa, &writer);

    // Sealing releases checker state without changing any serialized solved
    // data. Compare fresh serialization plans on each side of the boundary.
    const before_seal = try gpa.alloc(u8, writer.total_bytes);
    defer gpa.free(before_seal);
    _ = try writer.writeToBuffer(before_seal);
    try original.freezeClass(flex);
    try original.thawFrozenClasses();
    try original.markSettled(&.{flex});
    original.seal();
    var sealed_writer = CompactWriter.init();
    defer sealed_writer.deinit(gpa);
    try (try sealed_writer.appendAlloc(gpa, Store.Serialized)).serialize(&original, gpa, &sealed_writer);
    const after_seal = try gpa.alloc(u8, sealed_writer.total_bytes);
    defer gpa.free(after_seal);
    _ = try sealed_writer.writeToBuffer(after_seal);
    try std.testing.expectEqualSlices(u8, before_seal, after_seal);

    // Write to file
    try writer.writeGather(file, io);

    // Read back
    const file_size = writer.total_bytes;
    const buffer = try gpa.alignedAlloc(u8, std.mem.Alignment.@"16", @intCast(file_size));
    defer gpa.free(buffer);

    _ = try file.readPositionalAll(io, buffer, 0);

    // Deserialize - the serialized Store is at the beginning of the buffer
    const deserialized_serialized: *const Store.Serialized = @ptrCast(@alignCast(buffer.ptr));
    var deserialized_value = deserialized_serialized.deserializeInto(@intFromPtr(buffer.ptr), gpa, .sealed);
    const deserialized = &deserialized_value;

    // Verify all types
    const deser_str = deserialized.resolveVar(str_var);
    try std.testing.expectEqual(Content{ .structure = .empty_record }, deser_str.desc.content);

    const deser_list = deserialized.resolveVar(list_var);
    // List is a nominal type
    try std.testing.expect(deser_list.desc.content.structure == .nominal_type);
    const deser_nominal = deser_list.desc.content.structure.nominal_type;
    const deser_list_args = deserialized.sliceNominalArgs(deser_nominal);
    try std.testing.expectEqual(list_elem, deser_list_args[0]);

    const deser_func = deserialized.resolveVar(func_var);
    try std.testing.expect(deser_func.desc.content.structure == .fn_pure);
    const func = deser_func.desc.content.structure.fn_pure;
    const args = deserialized.sliceVars(func.args);
    try std.testing.expectEqual(@as(usize, 2), args.len);
    try std.testing.expectEqual(arg1, args[0]);
    try std.testing.expectEqual(arg2, args[1]);
    try std.testing.expectEqual(ret, func.ret);

    const deser_record = deserialized.resolveVar(record_var);
    try std.testing.expect(deser_record.desc.content.structure == .record);
    const record = deser_record.desc.content.structure.record;
    const fields_slice = deserialized.getRecordFieldsSlice(record.fields);
    try std.testing.expectEqual(@as(usize, 2), fields_slice.len);
    try std.testing.expectEqual(@as(u29, 100), fields_slice.items(.name)[0].idx);
    try std.testing.expectEqual(@as(u29, 200), fields_slice.items(.name)[1].idx);
    try std.testing.expectEqual(field1_var, fields_slice.items(.presence)[0].typeVar());
    try std.testing.expectEqual(null, fields_slice.items(.presence)[0].presenceVar());
    try std.testing.expectEqual(field2_var, fields_slice.items(.presence)[1].typeVar());
    try std.testing.expectEqual(field2_presence, fields_slice.items(.presence)[1].presenceVar());
    try std.testing.expectEqual(record_ext, record.ext);

    const deser_tag_union = deserialized.resolveVar(tag_union_var);
    try std.testing.expect(deser_tag_union.desc.content.structure == .tag_union);
    const tag_union = deser_tag_union.desc.content.structure.tag_union;
    const tags_slice = deserialized.getTagsSlice(tag_union.tags);
    try std.testing.expectEqual(@as(usize, 2), tags_slice.len);
    try std.testing.expectEqual(@as(u29, 300), tags_slice.items(.name)[0].idx);
    try std.testing.expectEqual(@as(u29, 400), tags_slice.items(.name)[1].idx);

    const tag1_args = deserialized.sliceVars(tags_slice.items(.args)[0]);
    try std.testing.expectEqual(@as(usize, 1), tag1_args.len);
    try std.testing.expectEqual(flex, tag1_args[0]);

    const tag2_args = deserialized.sliceVars(tags_slice.items(.args)[1]);
    try std.testing.expectEqual(@as(usize, 2), tag2_args.len);
    try std.testing.expectEqual(arg1, tag2_args[0]);
    try std.testing.expectEqual(arg2, tag2_args[1]);

    try std.testing.expectEqual(tag_union_ext, tag_union.ext);
    const alias = deserialized.resolveVar(alias_var).desc.content.alias;
    try std.testing.expectEqual(@as(u32, 1), alias.source_arg_count);
    try std.testing.expectEqualSlices(Var, &.{flex}, deserialized.sliceAliasArgs(alias));
    try std.testing.expectEqualSlices(Var, &.{tag_union_ext}, deserialized.sliceAliasHiddenArgs(alias));
    const backing = deserialized.resolveVar(deserialized.getAliasBackingVar(alias)).desc.content.structure.tag_union;
    try std.testing.expectEqual(backing.ext, deserialized.sliceAliasHiddenArgs(alias)[0]);
}

test "SlotStore.Serialized roundtrip" {
    const gpa = std.testing.allocator;
    const io = std.testing.io;
    const CompactWriter = collections.CompactWriter;

    // Use a real Store to get real Var and DescStore.Idx values
    var store = try Store.init(gpa);
    defer store.deinit();

    // Create real type variables - fresh() creates a flex var with a root slot
    const var_a = try store.fresh();
    const var_b = try store.fresh();
    const var_c = try store.fresh();

    // Get the DescStore.Idx from the root slots
    const desc_idx_a = store.getSlot(var_a).root;
    const desc_idx_c = store.getSlot(var_c).root;

    // Create a separate SlotStore for serialization testing
    var slot_store = try SlotStore.init(gpa, 4);
    defer slot_store.deinit(gpa);

    // Add slots and capture returned indices
    const slot_a = try slot_store.insert(gpa, .{ .root = desc_idx_a });
    const slot_b = try slot_store.insert(gpa, .{ .redirect = var_b });
    const slot_c = try slot_store.insert(gpa, .{ .root = desc_idx_c });

    // Create temp file
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();
    const file = try tmp_dir.dir.createFile(io, "test_slot_store_serialized.dat", .{ .read = true });
    defer file.close(io);

    // Serialize using SlotStore.Serialized with arena allocator
    var arena = collections.SingleThreadArena.init(gpa);
    defer arena.deinit();
    const arena_allocator = arena.allocator();

    var writer = CompactWriter.init();
    defer writer.deinit(arena_allocator);

    const serialized_ptr = try writer.appendAlloc(arena_allocator, SlotStore.Serialized);
    try serialized_ptr.serialize(&slot_store, arena_allocator, &writer);

    // Write to file
    try writer.writeGather(file, io);

    // Read back
    const file_size = writer.total_bytes;
    const buffer = try gpa.alignedAlloc(u8, std.mem.Alignment.@"16", @intCast(file_size));
    defer gpa.free(buffer);
    _ = try file.readPositionalAll(io, buffer, 0);

    // Deserialize - find the Serialized struct at the beginning of the buffer
    const deser_ptr = @as(*SlotStore.Serialized, @ptrCast(@alignCast(buffer.ptr)));
    const deserialized = deser_ptr.deserializeInto(@intFromPtr(buffer.ptr));

    // Verify using captured indices
    try std.testing.expectEqual(@as(u64, 3), deserialized.len());
    try std.testing.expectEqual(Slot{ .root = desc_idx_a }, deserialized.get(slot_a));
    try std.testing.expectEqual(Slot{ .redirect = var_b }, deserialized.get(slot_b));
    try std.testing.expectEqual(Slot{ .root = desc_idx_c }, deserialized.get(slot_c));
}

test "DescStore.Serialized roundtrip" {
    const gpa = std.testing.allocator;
    const io = std.testing.io;
    const CompactWriter = collections.CompactWriter;

    var desc_store = try DescStore.init(gpa, 4);
    defer desc_store.deinit(gpa);

    // Add some descriptors and capture returned indices
    const desc1 = Descriptor{
        .content = Content{ .flex = Flex.init() },
        .rank = Rank.generalized,
    };
    const desc2 = Descriptor{
        .content = Content{ .structure = .empty_record },
        .rank = Rank.outermost,
    };

    var store = try Store.init(gpa);
    defer store.deinit();
    const row1 = store.encodeDescWithoutIdentity(desc1);
    const row2 = store.encodeDescWithoutIdentity(desc2);
    try desc_store.backing.ensureUnusedCapacity(gpa, 2);
    const desc_idx_1 = desc_store.appendRowAssumeCapacity(row1);
    const desc_idx_2 = desc_store.appendRowAssumeCapacity(row2);

    // Create temp file
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();
    const file = try tmp_dir.dir.createFile(io, "test_desc_store_serialized.dat", .{ .read = true });
    defer file.close(io);

    // Serialize using DescStore.Serialized with arena allocator
    var arena = collections.SingleThreadArena.init(gpa);
    defer arena.deinit();
    const arena_allocator = arena.allocator();

    var writer = CompactWriter{
        .iovecs = .empty,
        .total_bytes = 0,
        .allocated_memory = .empty,
    };
    defer writer.deinit(arena_allocator);

    const serialized_ptr = try writer.appendAlloc(arena_allocator, DescStore.Serialized);
    try serialized_ptr.serialize(&desc_store, arena_allocator, &writer);

    // Write to file
    try writer.writeGather(file, io);

    // Read back
    const file_size = writer.total_bytes;
    const buffer = try gpa.alignedAlloc(u8, std.mem.Alignment.@"16", @intCast(file_size));
    defer gpa.free(buffer);
    _ = try file.readPositionalAll(io, buffer, 0);

    // Deserialize - find the Serialized struct at the beginning of the buffer
    const deser_ptr = @as(*DescStore.Serialized, @ptrCast(@alignCast(buffer.ptr)));
    const deserialized = deser_ptr.deserializeInto(@intFromPtr(buffer.ptr), false);

    // Verify using captured indices
    try std.testing.expectEqual(@as(usize, 2), deserialized.backing.len());
    try std.testing.expectEqual(row1, deserialized.getRow(desc_idx_1));
    try std.testing.expectEqual(row2, deserialized.getRow(desc_idx_2));
}

test "Store.Serialized roundtrip" {
    const gpa = std.testing.allocator;
    const io = std.testing.io;
    const CompactWriter = collections.CompactWriter;

    var store = try Store.init(gpa);
    defer store.deinit();

    // Create some type variables
    const flex = try store.fresh();
    const str_var = try store.freshFromContent(Content{ .structure = .empty_record });
    const redirect_var = try store.freshRedirect(flex);
    const field_presence = try store.fresh();
    const record_fields = try store.appendRecordFields(&.{
        .{
            .name = .{ .attributes = .{ .effectful = false, .ignored = false, .reserved = false }, .idx = 100 },
            .presence = .required(flex),
        },
        .{
            .name = .{ .attributes = .{ .effectful = false, .ignored = false, .reserved = false }, .idx = 200 },
            .presence = .unknown(field_presence, str_var),
        },
    });
    const class_a = try store.fresh();
    const class_b = try store.fresh();
    const class_checked = try store.fresh();
    try store.union_(class_a, class_b, .{ .content = .err, .rank = Rank.outermost });
    try store.union_(class_b, class_checked, .{ .content = .err, .rank = Rank.outermost });

    // Create temp file
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();
    const file = try tmp_dir.dir.createFile(io, "test_store_serialized.dat", .{ .read = true });
    defer file.close(io);

    // Serialize using Store.Serialized
    var writer = CompactWriter{
        .iovecs = .empty,
        .total_bytes = 0,
        .allocated_memory = .empty,
    };
    defer writer.deinit(gpa);

    const serialized_ptr = try writer.appendAlloc(gpa, Store.Serialized);
    try serialized_ptr.serialize(&store, gpa, &writer);

    // Write to file
    try writer.writeGather(file, io);

    // Read back
    const file_size = writer.total_bytes;
    const buffer = try gpa.alignedAlloc(u8, std.mem.Alignment.@"16", @intCast(file_size));
    defer gpa.free(buffer);
    _ = try file.readPositionalAll(io, buffer, 0);

    // Deserialize - Store.Serialized is at the beginning of the buffer
    const deser_ptr = @as(*Store.Serialized, @ptrCast(@alignCast(buffer.ptr)));
    const deserialized = deser_ptr.deserializeInto(@intFromPtr(buffer.ptr), gpa, .sealed);

    // Verify the store was deserialized correctly (flex, str_var, redirect,
    // the undetermined field's presence var, and the three union-rank class
    // vars).
    try std.testing.expectEqual(@as(usize, 7), deserialized.len());

    const flex_resolved = deserialized.resolveVar(flex);
    try std.testing.expectEqual(Content{ .flex = Flex.init() }, flex_resolved.desc.content);

    const str_resolved = deserialized.resolveVar(str_var);
    try std.testing.expectEqual(Content{ .structure = .empty_record }, str_resolved.desc.content);

    const redirect_resolved = deserialized.resolveVar(redirect_var);
    try std.testing.expectEqual(flex_resolved.desc_idx, redirect_resolved.desc_idx);

    const class_resolved = deserialized.resolveVar(class_a);
    try std.testing.expectEqual(class_checked, class_resolved.var_);
    try std.testing.expectEqual(class_checked, deserialized.resolveVar(class_b).var_);
    try std.testing.expect(deserialized.resolveVar(class_checked).is_root);
    const class_storage = deserialized.resolveStorageRoot(class_a);
    try std.testing.expectEqual(@as(u8, 1), deserialized.getUnionRank(class_storage.storage_var));
    try std.testing.expect(class_storage.storage_var != class_checked);

    const deserialized_fields = deserialized.getRecordFieldsSlice(record_fields);
    try std.testing.expectEqual(@as(usize, 2), deserialized_fields.len);
    try std.testing.expectEqual(null, deserialized_fields.items(.presence)[0].presenceVar());
    try std.testing.expectEqual(field_presence, deserialized_fields.items(.presence)[1].presenceVar());

    // A writable load solves further: a new variable grows the borrowed
    // columns by copying them, and every loaded variable still resolves.
    var writable = deser_ptr.deserializeInto(@intFromPtr(buffer.ptr), gpa, .writable);
    defer writable.deinit();
    try std.testing.expect(!writable.isSealed());
    const added = try writable.fresh();
    try std.testing.expectEqual(@as(usize, 8), writable.len());
    try std.testing.expect(writable.resolveVar(added).is_root);
    try std.testing.expectEqual(Content{ .structure = .empty_record }, writable.resolveVar(str_var).desc.content);

    const writable_fields = writable.getRecordFieldsSlice(record_fields);
    try std.testing.expectEqual(@as(usize, 2), writable_fields.len);
    try std.testing.expectEqual(null, writable_fields.items(.presence)[0].presenceVar());
    try std.testing.expectEqual(field_presence, writable_fields.items(.presence)[1].presenceVar());
}

test "Store.Serialized bytes are a function of the stored types" {
    // The types store is copied verbatim into every checked module cache entry, so
    // its bytes must depend on the stored types alone, including the `start` of
    // every empty range a content holds (a pure function's `effect_deps`, an
    // unconstrained flex's or rigid's `constraints`).
    const CompactWriter = collections.CompactWriter;

    // Fills every allocation with `poison` and never grows one in place, so each
    // store below is built over memory that holds a different stale pattern.
    const PoisonAllocator = struct {
        child: Allocator,
        poison: u8,

        fn allocator(self: *@This()) Allocator {
            return .{ .ptr = self, .vtable = &.{
                .alloc = alloc,
                .resize = Allocator.noResize,
                .remap = Allocator.noRemap,
                .free = free,
            } };
        }

        fn alloc(ctx: *anyopaque, len: usize, alignment: std.mem.Alignment, ret_addr: usize) ?[*]u8 {
            const self: *@This() = @ptrCast(@alignCast(ctx));
            const ptr = self.child.rawAlloc(len, alignment, ret_addr) orelse return null;
            @memset(ptr[0..len], self.poison);
            return ptr;
        }

        fn free(ctx: *anyopaque, memory: []u8, alignment: std.mem.Alignment, ret_addr: usize) void {
            const self: *@This() = @ptrCast(@alignCast(ctx));
            self.child.rawFree(memory, alignment, ret_addr);
        }
    };

    const Build = struct {
        fn serialized(poison: u8) (Allocator.Error || error{BufferTooSmall})![]align(CompactWriter.SERIALIZATION_ALIGNMENT.toByteUnits()) u8 {
            const gpa = std.testing.allocator;
            var poisoned: PoisonAllocator = .{ .child = gpa, .poison = poison };
            var store = try Store.init(poisoned.allocator());
            defer store.deinit();

            const flex = try store.freshFromContent(.{ .flex = Flex.init() });
            const rigid = try store.freshFromContent(.{ .rigid = Rigid.init(.{
                .attributes = .{ .effectful = false, .ignored = false, .reserved = false },
                .idx = 7,
            }) });
            _ = try store.freshFromContent(try store.mkFuncPure(&.{ flex, rigid }, flex));
            _ = try store.freshFromContent(try store.mkFuncEffectful(&.{}, rigid));

            var writer = CompactWriter.init();
            defer writer.deinit(gpa);
            const serialized_ptr = try writer.appendAlloc(gpa, Store.Serialized);
            try serialized_ptr.serialize(&store, gpa, &writer);
            const buffer = try gpa.alignedAlloc(u8, CompactWriter.SERIALIZATION_ALIGNMENT, writer.total_bytes);
            errdefer gpa.free(buffer);
            _ = try writer.writeToBuffer(buffer);
            return buffer;
        }
    };

    const over_as = try Build.serialized(0xAA);
    defer std.testing.allocator.free(over_as);
    const over_fives = try Build.serialized(0x55);
    defer std.testing.allocator.free(over_fives);

    try std.testing.expectEqualSlices(u8, over_as, over_fives);
    // No stale pattern survives, including the debug-build fill of an
    // `undefined` field.
    try std.testing.expect(std.mem.find(u8, over_as, &.{ 0xAA, 0xAA, 0xAA, 0xAA }) == null);
    try std.testing.expect(std.mem.find(u8, over_fives, &.{ 0x55, 0x55, 0x55, 0x55 }) == null);
}

test "Store multiple instances CompactWriter roundtrip" {
    const gpa = std.testing.allocator;
    const io = std.testing.io;
    const CompactWriter = collections.CompactWriter;

    // Create multiple stores
    var store1 = try Store.init(gpa);
    defer store1.deinit();

    var store2 = try Store.init(gpa);
    defer store2.deinit();

    var store3 = try Store.init(gpa);
    defer store3.deinit();

    // Populate differently
    const var1_1 = try store1.fresh();
    const var1_2 = try store1.freshFromContent(Content{ .structure = .empty_record });
    const redirect1_1 = try store1.freshRedirect(var1_1);
    try std.testing.expectEqual(Slot{ .redirect = var1_1 }, store1.getSlot(redirect1_1));

    const var2_1 = try store2.fresh();
    const var2_2 = try store2.fresh();
    const func_content = try store2.mkFuncEffectful(&[_]Var{var2_1}, var2_2);
    const func_var = try store2.freshFromContent(func_content);
    try std.testing.expect(store2.resolveVar(func_var).desc.content.unwrapFunc() != null);

    // store3 left empty

    // Create a temp file
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    const file = try tmp_dir.dir.createFile(io, "test_multiple_stores.dat", .{ .read = true });
    defer file.close(io);

    // Serialize all three
    var writer = CompactWriter{
        .iovecs = .empty,
        .total_bytes = 0,
        .allocated_memory = .empty,
    };
    defer writer.deinit(gpa);

    try writer.padToAlignment(gpa, @alignOf(Store.Serialized));
    const offset1 = writer.total_bytes; // Store1 starts at current position
    const serialized1 = try writer.appendAlloc(gpa, Store.Serialized);
    try serialized1.serialize(&store1, gpa, &writer);

    try writer.padToAlignment(gpa, @alignOf(Store.Serialized));
    const offset2 = writer.total_bytes; // Store2 starts at current position
    const serialized2 = try writer.appendAlloc(gpa, Store.Serialized);
    try serialized2.serialize(&store2, gpa, &writer);

    try writer.padToAlignment(gpa, @alignOf(Store.Serialized));
    const offset3 = writer.total_bytes; // Store3 starts at current position
    const serialized3 = try writer.appendAlloc(gpa, Store.Serialized);
    try serialized3.serialize(&store3, gpa, &writer);

    // Write to file
    try writer.writeGather(file, io);

    // Read back
    const file_size = writer.total_bytes;
    const buffer = try gpa.alignedAlloc(u8, std.mem.Alignment.@"16", @intCast(file_size));
    defer gpa.free(buffer);

    _ = try file.readPositionalAll(io, buffer, 0);

    // Deserialize all three
    const deserialized1_serialized: *const Store.Serialized = @ptrCast(@alignCast(buffer.ptr + offset1));
    var deserialized1_value = deserialized1_serialized.deserializeInto(@intFromPtr(buffer.ptr), gpa, .sealed);
    const deserialized1 = &deserialized1_value;

    const deserialized2_serialized: *const Store.Serialized = @ptrCast(@alignCast(buffer.ptr + offset2));
    var deserialized2_value = deserialized2_serialized.deserializeInto(@intFromPtr(buffer.ptr), gpa, .sealed);
    const deserialized2 = &deserialized2_value;

    const deserialized3_serialized: *const Store.Serialized = @ptrCast(@alignCast(buffer.ptr + offset3));
    var deserialized3_value = deserialized3_serialized.deserializeInto(@intFromPtr(buffer.ptr), gpa, .sealed);
    const deserialized3 = &deserialized3_value;

    // Verify store 1
    try std.testing.expectEqual(@as(usize, 3), deserialized1.len());
    const deser1_var2 = deserialized1.resolveVar(var1_2);
    try std.testing.expectEqual(Content{ .structure = .empty_record }, deser1_var2.desc.content);

    // Verify store 2
    try std.testing.expectEqual(@as(usize, 3), deserialized2.len());

    // Verify store 3 (empty)
    try std.testing.expectEqual(@as(usize, 0), deserialized3.len());
}

test "source declaration overflow is rejected before mutating type store" {
    const gpa = std.testing.allocator;

    var store = try Store.initCapacity(gpa, 1, 1);
    defer store.deinit();

    const before_slots = store.len();
    const before_descs = store.descs.backing.len();
    const before_vars = store.vars.len();
    const unread_backing_var: Var = undefined; // source declaration validation returns before reading this value

    try std.testing.expectError(
        error.OutOfMemory,
        store.mkAliasWithSourceDecl(
            .{ .ident_idx = base.Ident.Idx.NONE },
            unread_backing_var,
            &.{},
            base.ModuleIdentity.Idx.NONE,
            SourceDecl.max_statement + 1,
        ),
    );
    try std.testing.expectEqual(before_slots, store.len());
    try std.testing.expectEqual(before_descs, store.descs.backing.len());
    try std.testing.expectEqual(before_vars, store.vars.len());

    try std.testing.expectError(
        error.OutOfMemory,
        store.mkNominalWithSourceDecl(
            .{ .ident_idx = base.Ident.Idx.NONE },
            &.{},
            base.ModuleIdentity.Idx.NONE,
            NominalType.Source.max_statement + 1,
            false,
        ),
    );
    try std.testing.expectEqual(before_slots, store.len());
    try std.testing.expectEqual(before_descs, store.descs.backing.len());
    try std.testing.expectEqual(before_vars, store.vars.len());
}

test "Store annotation tag provenance follows flex equivalence classes" {
    for ([_]bool{ false, true }) |reverse| {
        var store = try Store.init(std.testing.allocator);
        defer store.deinit();
        const annotation_ext = try store.fresh();
        const inferred_ext = try store.fresh();
        try store.markAnnotationTagExt(annotation_ext);
        const a = if (reverse) annotation_ext else inferred_ext;
        const b = if (reverse) inferred_ext else annotation_ext;
        try store.union_(a, b, .{ .content = .{ .flex = Flex.init() }, .rank = .outermost });
        try std.testing.expect(store.resolveVar(inferred_ext).desc.flags.annotation_tag_ext);
        try std.testing.expect(store.resolveVar(annotation_ext).desc.flags.annotation_tag_ext);

        const empty = try store.freshFromContentWithRank(.{ .structure = .empty_tag_union }, .outermost);
        try store.union_(inferred_ext, empty, .{ .content = .{ .structure = .empty_tag_union }, .rank = .outermost });
        try std.testing.expect(!store.resolveVar(annotation_ext).desc.flags.annotation_tag_ext);
    }
}

test "mayContainErrorState tracks error descriptors and invalid nominal declarations" {
    const gpa = std.testing.allocator;

    var store = try Store.init(gpa);
    defer store.deinit();
    const backing = try store.fresh();
    const decl_idx = try store.registerNominalDecl(try testNominalDecl(@fromBackingInt(@intCast(1)), 3, backing));
    try std.testing.expect(!store.mayContainErrorState());

    store.markNominalDeclInvalid(decl_idx);
    try std.testing.expect(store.mayContainErrorState());

    var errs = try Store.init(gpa);
    defer errs.deinit();
    const b = try errs.fresh();
    try std.testing.expect(!errs.mayContainErrorState());
    try errs.setVarContent(b, .err);
    try std.testing.expect(errs.mayContainErrorState());
    try errs.setVarContent(b, .{ .flex = Flex.init() });
    try std.testing.expect(errs.mayContainErrorState());

    var copy = try errs.clone(gpa);
    defer copy.deinit();
    try std.testing.expect(copy.mayContainErrorState());
}

/// Translate source locators without changing a solved type relationship.
pub fn rebindSourceReferenceValue(value: anytype, mapper: anytype) @TypeOf(mapper.*).Error!void {
    const T = @typeInfo(@TypeOf(value)).pointer.child;
    if (T == Alias) {
        if (value.source_decl.present) value.source_decl.statement = @intCast(try mapper.node(value.origin_module, value.source_decl.statement));
    } else if (T == NominalType or T == NominalDecl) {
        if (value.source.present) value.source.statement = @intCast(try mapper.node(value.origin_module, value.source.statement));
    } else if (T == types.DefaultId) {
        value.expr_node = try mapper.node(value.origin_module, value.expr_node);
    } else switch (@typeInfo(T)) {
        .@"struct" => inline for (@typeInfo(T).@"struct".field_names) |field| try rebindSourceReferenceValue(&@field(value, field), mapper),
        .@"union" => inline for (@typeInfo(T).@"union".field_names) |field| {
            if (value.* == @field(std.meta.Tag(T), field)) try rebindSourceReferenceValue(&@field(value, field), mapper);
        },
        .optional => if (value.*) |*present| try rebindSourceReferenceValue(present, mapper),
        .array => for (value) |*item| try rebindSourceReferenceValue(item, mapper),
        .bool, .int, .float, .@"enum", .void => {},
        .type,
        .noreturn,
        .pointer,
        .error_union,
        .error_set,
        .@"fn",
        .@"opaque",
        .frame,
        .@"anyframe",
        .vector,
        .enum_literal,
        .comptime_int,
        .comptime_float,
        .undefined,
        .null,
        .spirv,
        => @compileError("source-reference storage needs explicit relocation for " ++ @typeName(T)),
    }
}

test "the written flags travel with the serialized store" {
    const gpa = std.testing.allocator;
    const CompactWriter = collections.CompactWriter;

    for ([_]bool{ false, true }) |write_err| {
        var store = try Store.init(gpa);
        defer store.deinit();
        _ = try store.fresh();
        if (write_err) _ = try store.freshFromContent(.err);
        try std.testing.expectEqual(write_err, store.descs.err_written);
        try std.testing.expect(!store.invalid_nominal_decl_written);

        var writer = CompactWriter.init();
        defer writer.deinit(gpa);
        const serialized = try writer.appendAlloc(gpa, Store.Serialized);
        try serialized.serialize(&store, gpa, &writer);
        const buffer = try gpa.alignedAlloc(u8, std.mem.Alignment.@"16", writer.total_bytes);
        defer gpa.free(buffer);
        _ = try writer.writeToBuffer(buffer);

        const loaded_ptr: *const Store.Serialized = @ptrCast(@alignCast(buffer.ptr));
        var loaded = loaded_ptr.deserializeInto(@intFromPtr(buffer.ptr), gpa, .sealed);
        defer loaded.deinit();
        try std.testing.expectEqual(write_err, loaded.descs.err_written);
        try std.testing.expect(!loaded.invalid_nominal_decl_written);
        try std.testing.expectEqual(write_err, loaded.mayContainErrorState());

        var writable = loaded_ptr.deserializeInto(@intFromPtr(buffer.ptr), gpa, .writable);
        defer writable.deinit();
        try std.testing.expectEqual(write_err, writable.mayContainErrorState());
    }
}

fn exerciseStoreConstructionFailure(gpa: Allocator) (Allocator.Error || error{TestExpectedEqual})!void {
    var store = try Store.initCapacity(gpa, 4, 4);
    defer store.deinit();
    const first = try store.fresh();
    for (0..24) |_| _ = try store.freshRedirect(first);
    try std.testing.expectEqual(@as(u64, 25), store.len());
}

test "reserved type columns preserve ownership on construction and growth failure" {
    try std.testing.checkAllAllocationFailures(std.testing.allocator, exerciseStoreConstructionFailure, .{});
}
