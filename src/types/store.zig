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

const Allocator = std.mem.Allocator;

/// Which family an in-place write to a type-store entry belongs to. The
/// unifier's writes (`unifierUnion` and the other `unifier*` functions, and
/// the store internals they reach) test, with one comparison against the
/// open probe's baseline, whether the entry predates the probe, and journal its
/// old value when it does. Outside a probe the baselines are 0, so the test
/// never holds. Every other write is plain: it tests and journals nothing,
/// and debug builds assert it never reaches an entry that predates an open
/// probe. Only the unifier overwrites such an entry: a compiler invariant.
const WriteFamily = enum { plain, unifier };

/// One entry of the undo log (`Store.undo_log`): an in-place write the
/// unifier made, while a probe was open, to an entry that predates the probe,
/// holding what the write replaced. Discarding the probe replays the log
/// strictly backwards.
const Undo = union(enum) {
    slot: struct { idx: SlotStore.Idx, old: Slot },
    /// The stored row a descriptor held, without generalization marks.
    desc: struct { idx: DescStore.Idx, old: Desc.Row },
    root_meta: struct { idx: DescStore.Idx, old: RootMeta },
    union_rank: struct { idx: SlotStore.Idx, old: u8 },
    /// A row inserted into `derived_map_plans` at this index.
    plan_inserted: u32,
    /// The row removed from `derived_map_plans` at `index`.
    plan_removed: struct { index: u32, entry: DerivedMapPlanEntry },
    /// The plan the row at `index` of `derived_map_plans` held.
    plan_replaced: struct { index: u32, old: DerivedMapPlan },
    /// A row inserted into `tag_tail_closings` at this index.
    closing_inserted: u32,
    /// The row removed from `tag_tail_closings` at `index`.
    closing_removed: struct { index: u32, entry: TagTailClosingEntry },
    /// The closing the row at `index` of `tag_tail_closings` held.
    closing_replaced: struct { index: u32, old: TagTailClosing },
};

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
const TagTailClosing = types.TagTailClosing;
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

    /// A declaration key as one integer, ordered as `orderByKey` orders
    /// keys.
    fn sortKey(origin_module: base.ModuleIdentity.Idx, statement: u32) u64 {
        return (@as(u64, @backingInt(origin_module)) << 32) | statement;
    }

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

/// One class's entry in `Store.tag_tail_closings`.
const TagTailClosingEntry = struct {
    class: DescStore.Idx,
    closing: TagTailClosing,
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

    /// What a merge of two classes needs to know about this set before it
    /// writes anything (see `Store.unionRows`).
    const Merge = struct {
        before: usize,
        a_settled: bool,
        b_settled: bool,
    };

    /// Note a merge that will give `a`'s and `b`'s vars content `a_kept` or
    /// `b_kept` says is each side's own. A settled side whose content
    /// changes clears the set.
    inline fn beginMerge(self: *SettledDescs, a: DescStore.Idx, b: DescStore.Idx, a_kept: bool) Merge {
        if (self.list.items.len == 0) return .{ .before = 0, .a_settled = false, .b_settled = false };
        const merge: Merge = .{ .before = self.list.items.len, .a_settled = self.contains(a), .b_settled = self.contains(b) };
        if (merge.a_settled and !a_kept) {
            self.clear();
            return .{ .before = 0, .a_settled = merge.a_settled, .b_settled = merge.b_settled };
        }
        return merge;
    }

    /// After the merge: when nothing cleared the set and a settled side kept
    /// its content, the merged class `merged` is settled.
    inline fn finishMerge(self: *SettledDescs, gpa: Allocator, merge: Merge, merged: DescStore.Idx, a_kept: bool, b_kept: bool) Allocator.Error!void {
        const nothing_changed = self.list.items.len == merge.before and merge.before != 0;
        if (nothing_changed and ((merge.a_settled and a_kept) or (merge.b_settled and b_kept))) {
            try self.insert(gpa, merged);
        }
    }
};

/// Which entries of one column of a store survive sealing, and where each
/// lands when the column closes its gaps: survivors keep their order, so an
/// entry's new index is the number of survivors before it (`rankOf`), read
/// as the running count kept per mask word plus a population count within
/// the word. The bit set holds one spare entry past the column, so a range
/// that ends at the column's end, or an empty range that starts there,
/// ranks like any other.
const SealSurvivors = struct {
    bits: std.DynamicBitSetUnmanaged = .{},
    /// For each mask word of `bits`, the survivors in the words before it.
    before: []u32 = &.{},
    /// The surviving entries in all, once `finish` has counted them.
    count: u32 = 0,

    const MaskInt = std.DynamicBitSetUnmanaged.MaskInt;
    const mask_bits = @bitSizeOf(MaskInt);

    /// No survivor yet, over a column of `len` entries.
    fn init(gpa: Allocator, len: usize) Allocator.Error!SealSurvivors {
        return .{ .bits = try std.DynamicBitSetUnmanaged.initEmpty(gpa, len + 1) };
    }

    fn deinit(self: *SealSurvivors, gpa: Allocator) void {
        self.bits.deinit(gpa);
        gpa.free(self.before);
        self.* = .{};
    }

    /// The column grew to `len` entries while survivors were being marked,
    /// and every entry it gained survives.
    fn growSurviving(self: *SealSurvivors, gpa: Allocator, len: usize) Allocator.Error!void {
        const old_len = self.bits.bit_length - 1;
        if (len == old_len) return;
        try self.bits.resize(gpa, len + 1, false);
        self.bits.setRangeValue(.{ .start = old_len, .end = len }, true);
    }

    inline fn keep(self: *SealSurvivors, index: usize) void {
        self.bits.set(index);
    }

    inline fn keepRange(self: *SealSurvivors, start: usize, count: usize) void {
        if (count == 0) return;
        self.bits.setRangeValue(.{ .start = start, .end = start + count }, true);
    }

    inline fn survives(self: *const SealSurvivors, index: usize) bool {
        return self.bits.isSet(index);
    }

    /// Count the survivors before each mask word; marking is over.
    fn finish(self: *SealSurvivors, gpa: Allocator) Allocator.Error!void {
        const words = (self.bits.bit_length + mask_bits - 1) / mask_bits;
        self.before = try gpa.alloc(u32, words);
        var running: u32 = 0;
        for (self.before, self.bits.masks[0..words]) |*before, mask| {
            before.* = running;
            running += @popCount(mask);
        }
        self.count = running;
    }

    /// The number of survivors before `index`: a survivor's index once the
    /// column has closed its gaps, and where an empty range at `index` goes.
    inline fn rankOf(self: *const SealSurvivors, index: usize) u32 {
        const word = index / mask_bits;
        const below: MaskInt = (@as(MaskInt, 1) << @intCast(index % mask_bits)) -% 1;
        return self.before[word] + @popCount(self.bits.masks[word] & below);
    }

    /// The index survivor `index` moves to.
    inline fn rankOfSurvivor(self: *const SealSurvivors, index: usize) u32 {
        if (std.debug.runtime_safety and !self.survives(index)) {
            base.invariant("sealing renumbered entry {d}, which does not survive", .{index});
        }
        return self.rankOf(index);
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

/// Two row parts that each store their own entry for one tag a match pattern
/// asked for (design.md "Pattern-Only Tags"). The unifier rebuilt the row
/// `root` resolves to holding a copy of the marked tag `name`, and `part`
/// keeps its own entry outside that row's extension chain.
pub const PatternOnlyTagCopy = struct {
    part: Var,
    root: Var,
    name: base.Ident.Idx,
};

/// An empty tag union a relation can no longer lift: written by an
/// annotation, or committed after inference closed it. The empty tag union
/// an opening of a nominal declaration's backing minted is the declaration's
/// own row and decides nothing about the anonymous row it closes.
fn committedEmptyTagUnion(word: types.Descriptor.Word) bool {
    const flags = word.flags();
    return word.kind == .empty_tag_union and !flags.uncommitted_tag_tail and !flags.nominal_backing_structure;
}

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

    /// In builds with runtime safety, the identifiers this store's tag names
    /// index while a checker writes to it, so every tag-union descriptor
    /// written can be asserted to hold its own tags in name order.
    debug_idents: if (std.debug.runtime_safety) ?*const base.Ident.Store else void = if (std.debug.runtime_safety) null else {},

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
    /// Every tag-union descriptor's own range is in name order
    /// (`debugAssertTagsInNameOrder`).
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

    /// The identity of every nominal type application and alias a descriptor
    /// of this store holds, one row per distinct identity: a stored nominal or
    /// alias descriptor names its row (`types.Descriptor.encode`). Append-only;
    /// a discarded probe keeps the rows it interned, which stay valid
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
    /// Openings of nominal declaration backings recorded for replay
    /// (`instantiate.BackingOpenings`). Runtime-only: never serialized,
    /// cloned, or relocated; released when the store is sealed.
    backing_openings: instantiate.BackingOpenings = .{},

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
    /// Descriptors a scan found to reach no function type (see
    /// `SettledDescs` and `markFunctionFree`), kept exactly like
    /// `settled_descs`. Runtime-only: never serialized, cloned, or relocated.
    function_free_descs: SettledDescs = .{},
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

    /// Set while a probe is open (`beginProbe`): the unifier's writes journal
    /// every in-place write to a slot, descriptor, checked representative,
    /// or structural rank that existed before the probe began as (index, old
    /// value), and every other write may only reach entries the probe
    /// appended. Discarding the probe replays the journal in reverse and
    /// truncates what was appended. Probes never nest, so this is a flag, not
    /// a depth. No write's journaling test reads it: that test compares
    /// against the baselines below.
    probe_open: bool = false,
    /// Set while the unifier runs (`enterUnifier`), in debug builds only, so
    /// a unifier write while a probe is open can assert it comes from the
    /// unifier.
    unifier_running: UnifierRunning = if (std.debug.runtime_safety) false else {},
    /// `free_descs` while a probe is open: the probe sets the free list aside,
    /// so every class created during the probe is appended (and discarding the
    /// probe truncates it), and puts it back when it ends.
    probe_free_descs: std.ArrayListUnmanaged(DescStore.Idx) = .empty,
    /// Set once checking has solved this store (see `seal`). Every write to a
    /// sealed store is a violated compiler invariant.
    sealed: bool = false,
    /// The slot and descriptor counts when the open probe began, and 0 when no
    /// probe is open: an entry below its baseline predates the probe, which a
    /// unifier write tests with one comparison (`journalsWrite`).
    probe_baseline_slots: u32 = 0,
    probe_baseline_descs: u32 = 0,
    /// The open probe's undo log (`Undo`), in write order; empty while no probe
    /// is open, since probes never nest and ending one empties it.
    undo_log: std.ArrayListUnmanaged(Undo) = .empty,
    /// Advances whenever a slot write can change which class a var resolves
    /// to, so a caller that indexed resolved classes knows when to re-index.
    slot_generation: u64 = 0,
    /// The checking step that closed each uncommitted tag tail class a match
    /// closed (`TagTailClosing`), sorted by class descriptor so lookups
    /// binary-search. Like the tail's mark, a merge keeps it, a member
    /// detached from the class keeps it, and a copy of the tail carries it.
    /// Only a type mismatch's report reads it. Runtime-only: never
    /// serialized or cloned; sealing releases it.
    tag_tail_closings: std.ArrayListUnmanaged(TagTailClosingEntry) = .empty,

    /// Descriptors no slot names any more: a class merge keeps one side's
    /// descriptor and orphans the other's. The next class created takes the
    /// most recently freed one (and its `root_metas` entry) instead of
    /// appending, so the descriptor store does not grow with dead entries.
    /// Runtime-only: never serialized, cloned, or relocated.
    free_descs: std.ArrayListUnmanaged(DescStore.Idx) = .empty,
    /// Descriptors orphaned while a probe is open. Discarding the probe restores
    /// the slots that named them, so they are freed only when it is kept.
    pending_free_descs: std.ArrayListUnmanaged(DescStore.Idx) = .empty,

    /// Every static-dispatch relation the unifier folded into a retained
    /// same-name relation, by raw constraint function var. Checking publishes
    /// these so a folded relation's dispatch names the retained relation's
    /// target instantiation. Sealing releases this journal after checking has
    /// published the selected relations into ModuleEnv.
    static_dispatch_relation_merges: std.ArrayListUnmanaged(StaticDispatchRelationMerge) = .empty,
    /// Every copy the unifier made of a pattern-only tag whose source part
    /// outlives the relation (design.md "Pattern-Only Tags"). The
    /// unmatchable-pattern judgment reads these to carry a producer one copy
    /// met to the others. Sealing releases this journal.
    pattern_only_tag_copies: std.ArrayListUnmanaged(PatternOnlyTagCopy) = .empty,
    /// Unused room reserved directly after a constraint range, keyed by the
    /// range's end and holding the room's end, so a receiver whose
    /// requirements grow one use at a time extends its own list in place
    /// rather than copying it (`appendStaticDispatchConstraintsAfter`). Room
    /// is neither reserved nor used while a probe is open, so discarding a
    /// probe never has room to forget.
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
            .slots = .{ .words = .{} },
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
    /// probe open and no class frozen. All checked outputs have been
    /// published; only checker-owned buffers are released at this boundary.
    pub fn seal(self: *Self) Allocator.Error!void {
        if (std.debug.runtime_safety) {
            if (self.sealed) base.invariant("a type store was sealed twice", .{});
            if (self.probe_open) base.invariant("a type store was sealed with a probe open", .{});
            if (self.frozen_anchors.count() != 0) base.invariant("a type store was sealed with a frozen class", .{});
        }
        self.releaseCheckerBuffers();
        try self.sealGrowingColumns();
        self.sealed = true;
    }

    /// Forbid growth of every growing column and release every column's
    /// unused capacity.
    fn sealGrowingColumns(self: *Self) Allocator.Error!void {
        try self.slots.words.seal(self.gpa);
        try self.descs.backing.seal(self.gpa);
        try self.root_metas.seal(self.gpa);
        try self.union_ranks.seal(self.gpa);
        try self.vars.seal(self.gpa);
        try self.record_fields.seal(self.gpa);
        try self.tags.seal(self.gpa);
        inline for (.{
            &self.interpolation_parts,
            &self.numeral_infos,
            &self.interpolation_metadata,
            &self.static_dispatch_constraints,
            &self.nominal_decls,
            &self.nominal_decl_index,
            &self.derived_map_plans,
            &self.nominal_identities,
            &self.alias_identities,
        }) |list| try list.releaseUnusedCapacity(self.gpa);
    }

    /// Forbid growth of the columns of a store loaded over serialized bytes.
    fn sealLoadedColumns(self: *Self) void {
        self.slots.words.sealView();
        self.descs.backing.sealView();
        self.root_metas.sealView();
        self.union_ranks.sealView();
        self.vars.sealView();
        self.record_fields.sealView();
        self.tags.sealView();
    }

    /// Canonicalize the solved store (`canonicalizeForSeal`), flattening
    /// every row, then close its gaps and seal it (`sealCanonicalizing`).
    pub fn canonicalizeAndSeal(self: *Self, idents: *const base.Ident.Store, fixed_vars: u64, holders: anytype) Allocator.Error!void {
        try self.sealCanonicalizing(idents, {}, fixed_vars, holders);
    }

    /// Canonicalize the solved store (`canonicalizeForSeal`), flattening the
    /// rows `roots` reaches, then close its gaps and seal it
    /// (`sealCanonicalizing`). Checking calls this once, when it finishes a
    /// module. `roots.forEach(visitor)` names every variable a reader of the
    /// sealed store may start from, through `visitor.visit(Var)`,
    /// `visitor.visitRequired(?Var, comptime []const u8)` and
    /// `visitor.visitLeading(u32)`, the last naming every variable below a
    /// count. The store itself adds its nominal declarations' backings.
    pub fn canonicalizeAndSealReached(self: *Self, idents: *const base.Ident.Store, roots: anytype, fixed_vars: u64, holders: anytype) Allocator.Error!void {
        try self.sealCanonicalizing(idents, roots, fixed_vars, holders);
    }

    /// Canonicalize the solved store, close the gaps its dead entries leave
    /// (`closeSealGaps`), renumber everything `holders` holds to match, then
    /// seal it.
    ///
    /// Variables below `fixed_vars` keep their numbers: canonicalization made
    /// them, and a CIR node's type variable is the node's own number.
    /// `holders` is the owner of every variable and static-dispatch
    /// constraint index recorded outside the store before sealing: its
    /// `visitSealHolders(visitor)` hands each one to the visitor, once to
    /// keep it alive and once to renumber it (`SealHolderVisitor`).
    fn sealCanonicalizing(self: *Self, idents: *const base.Ident.Store, roots: anytype, fixed_vars: u64, holders: anytype) Allocator.Error!void {
        self.debugAssertUnsealed();
        if (std.debug.runtime_safety and self.probe_open) base.invariant("a type store was canonicalized with a probe open", .{});
        // Canonicalization renumbers classes, and no report reads a closing
        // once checking has finished.
        self.tag_tail_closings.clearRetainingCapacity();
        var columns = try SealColumns.init(self, fixed_vars);
        defer columns.deinit(self.gpa);
        holders.visitSealHolders(SealHolderVisitor(.keep){ .columns = &columns });
        try self.canonicalizeForSeal(idents, roots, &columns);
        self.keepConstraintRows(&columns);
        try columns.finish(self);
        self.closeSealGaps(&columns);
        holders.visitSealHolders(SealHolderVisitor(.renumber){ .columns = &columns });
        try self.seal();
    }

    /// The last rewrite of a solved store before its gaps close. Every class
    /// comes to be stored at its checked representative, and every other slot
    /// names that representative directly; every variable a descriptor, a
    /// type argument or field pool, or a nominal declaration holds becomes
    /// its class's checked representative; and every record and tag union row
    /// `roots` reaches absorbs the rows its extension chain reaches, so such
    /// a row's extension is never itself a row. A reader of the sealed store
    /// therefore receives representatives as children and reads each row it
    /// can reach in one piece. A row no root reaches keeps its chain: nothing
    /// reads it. With no roots (`{}`), every row is flattened. A
    /// static-dispatch constraint keeps its own function variable: checking
    /// recorded that variable at the dispatch site, so it identifies the
    /// constraint, and a reader resolves it (one slot read). Every live
    /// class and its representative survive, and each class keeps the pool
    /// and identity rows its canonical descriptor names (`SealColumns`).
    fn canonicalizeForSeal(self: *Self, idents: *const base.Ident.Store, roots: anytype, columns: *SealColumns) Allocator.Error!void {
        // One pass over the slots. A slot not yet visited still reaches its
        // class's old storage root, which holds the descriptor until it is
        // visited itself, and a visited slot reaches the representative,
        // which holds it from the first visit of any member on.
        const slot_count = self.slots.len();
        for (0..slot_count) |index| {
            const slot_idx: SlotStore.Idx = @fromBackingInt(@intCast(index));
            const storage = self.findStorageRoot(slotIdxToVar(slot_idx));
            const representative = representativeOf(storage);
            // No generalization pass is running, so no stored word is marked.
            std.debug.assert(!self.descs.getWord(storage.desc_idx).marked());
            self.slots.set(varToSlotIdx(representative), .{ .root = storage.desc_idx });
            if (representative != slotIdxToVar(slot_idx)) self.slots.set(slot_idx, .{ .redirect = representative });
            columns.descs.keep(@backingInt(storage.desc_idx));
        }

        for (self.nominal_decls.items()) |*decl| {
            decl.backing = self.representativeVar(decl.backing);
            columns.var_pool.keepRange(@backingInt(decl.formals.start), decl.formals.count);
        }

        // One pass over the live descriptors, in index order, reading each
        // one's kind. Only a function, a record and a tag union hold a
        // variable inline (a return or an extension), so only those are read
        // past their word. Resolving a row's extension also tells whether it
        // is itself a row: such a row has a chain to absorb. Each descriptor
        // keeps its representative.
        var chained: std.ArrayListUnmanaged(DescStore.Idx) = .empty;
        defer chained.deinit(self.gpa);
        var live = columns.descs.bits.iterator(.{});
        while (live.next()) |index| {
            const desc_idx: DescStore.Idx = @fromBackingInt(@intCast(index));
            columns.vars.keep(@backingInt(self.getRootMeta(desc_idx).checked_var));
            switch (self.descs.getWord(desc_idx).kind) {
                .flex, .rigid, .alias, .field_presence, .err, .tuple, .nominal_type, .empty_record, .empty_tag_union => {},
                .fn_pure, .fn_effectful, .fn_unbound => {
                    const ret: Var = @fromBackingInt(self.descs.getPayloadWord(desc_idx, Desc.fn_ret_payload_word));
                    self.setInlineVarForSeal(desc_idx, Desc.fn_ret_payload_word, self.representativeVar(ret));
                },
                .record, .tag_union => |kind| {
                    const stored: Var = @fromBackingInt(self.descs.getPayloadWord(desc_idx, Desc.row_ext_payload_word));
                    const ext = self.findStorageRoot(stored);
                    self.setInlineVarForSeal(desc_idx, Desc.row_ext_payload_word, representativeOf(ext));
                    if (self.descs.getWord(ext.desc_idx).kind == kind) try chained.append(self.gpa, desc_idx);
                },
            }
        }
        if (chained.items.len != 0) try self.flattenRowsForSeal(idents, roots, chained.items);

        // Once every row is final, each live descriptor keeps the pool rows
        // and identity row it names.
        live = columns.descs.bits.iterator(.{});
        while (live.next()) |index| columns.keepRowsOf(self, @fromBackingInt(@intCast(index)));
    }

    /// Flatten the rows of `chained`, each a record or tag union whose
    /// extension is itself a row: every one of them with no `roots`, and
    /// those `roots` reaches otherwise.
    fn flattenRowsForSeal(self: *Self, idents: *const base.Ident.Store, roots: anytype, chained: []const DescStore.Idx) Allocator.Error!void {
        var flattener = RowFlattener{ .store = self, .idents = idents };
        defer flattener.deinit();
        if (@TypeOf(roots) == void) {
            // A chain's segments are read as stored: one flattened already
            // has absorbed its own chain, and one not yet reached has not.
            for (chained) |desc_idx| try flattener.flatten(desc_idx);
            return;
        }
        var reached = try std.DynamicBitSetUnmanaged.initEmpty(self.gpa, self.descs.backing.len());
        defer reached.deinit(self.gpa);
        var tracer = SealTracer{ .flattener = &flattener, .reached = &reached };
        defer tracer.pending.deinit(self.gpa);
        for (self.nominal_decls.items()) |decl| try tracer.visit(decl.backing);
        try roots.forEach(&tracer);
        while (tracer.pending.pop()) |desc_idx| try tracer.expand(desc_idx);
    }

    /// Keep every static-dispatch constraint row a holder can still reach,
    /// and what each one names. A live flex or rigid keeps its own rows
    /// (`SealColumns.keepRowsOf`), and an explicit constraint index keeps its
    /// row. A reader also finds a constraint by the function variable checking
    /// recorded at its dispatch site, searching the whole pool, so a row whose
    /// function variable a holder names survives too, whichever class holds
    /// it. A surviving constraint keeps its function variable, its numeral
    /// facts and its interpolation metadata, and that metadata keeps its item
    /// variable and its parts with their variables.
    fn keepConstraintRows(self: *const Self, columns: *SealColumns) void {
        const constraints = self.static_dispatch_constraints.items();
        for (constraints, 0..) |constraint, index| {
            if (!columns.constraints.survives(index) and columns.holder_vars.isSet(@backingInt(constraint.fn_var))) columns.constraints.keep(index);
        }
        var kept = columns.constraints.bits.iterator(.{});
        while (kept.next()) |index| {
            const constraint = constraints[index];
            columns.vars.keep(@backingInt(constraint.fn_var));
            if (constraint.numeral.get()) |numeral| columns.numeral_infos.keep(@backingInt(numeral));
            const metadata_idx = constraint.interpolation.get() orelse continue;
            if (columns.interpolation_metadata.survives(@backingInt(metadata_idx))) continue;
            columns.interpolation_metadata.keep(@backingInt(metadata_idx));
            const metadata = self.interpolation_metadata.get(metadata_idx).*;
            columns.vars.keep(@backingInt(metadata.item_var));
            const parts_start: usize = @backingInt(metadata.interpolated_parts.start);
            columns.interpolation_parts.keepRange(parts_start, metadata.interpolated_parts.count);
            for (self.interpolation_parts.items()[parts_start..][0..metadata.interpolated_parts.count]) |part| {
                columns.vars.keep(@backingInt(part.var_));
            }
        }
    }

    /// Close the gaps the dead entries of every column `columns` covers
    /// leave, sliding each survivor down to its rank (`SealSurvivors`).
    /// Variables in the pools are rewritten to their classes' representatives
    /// as they move, and every variable and index a moved entry holds is
    /// renumbered with it. Each column is walked once: the pools first, while
    /// slots and descriptors still resolve variables at their old indices,
    /// then the slots, while descriptors are still at theirs.
    fn closeSealGaps(self: *Self, columns: *const SealColumns) void {
        const vars = &columns.vars;
        {
            const items = self.vars.items;
            var next: usize = 0;
            var kept = columns.var_pool.bits.iterator(.{});
            while (kept.next()) |index| : (next += 1) {
                const entry = items[index];
                items[next] = if (isRangeLengthSlot(entry)) entry else columns.renumberVar(self.representativeVar(entry));
            }
            self.vars.shrinkRetainingCapacity(next);
        }
        {
            var next: u32 = 0;
            var kept = columns.record_fields.bits.iterator(.{});
            while (kept.next()) |index| : (next += 1) {
                var field = self.record_fields.get(@fromBackingInt(@intCast(index)));
                field.presence.var_ = columns.renumberVar(self.representativeVar(field.presence.var_));
                if (field.presence.presenceVar()) |presence| field.presence.presence_var = .some(columns.renumberVar(self.representativeVar(presence)));
                self.record_fields.set(@fromBackingInt(next), field);
            }
            self.record_fields.shrinkRetainingCapacity(next);
        }
        {
            var next: u32 = 0;
            var kept = columns.tags.bits.iterator(.{});
            while (kept.next()) |index| : (next += 1) {
                var tag = self.tags.get(@fromBackingInt(@intCast(index)));
                tag.args.start = @fromBackingInt(columns.var_pool.rankOf(@backingInt(tag.args.start)));
                self.tags.set(@fromBackingInt(next), tag);
            }
            self.tags.shrinkRetainingCapacity(next);
        }
        {
            const items = self.static_dispatch_constraints.items();
            var next: usize = 0;
            var kept = columns.constraints.bits.iterator(.{});
            while (kept.next()) |index| : (next += 1) {
                var constraint = items[index];
                constraint.fn_var = columns.renumberVar(constraint.fn_var);
                if (constraint.numeral.get()) |numeral| constraint.numeral = .some(@fromBackingInt(columns.numeral_infos.rankOfSurvivor(@backingInt(numeral))));
                if (constraint.interpolation.get()) |metadata| constraint.interpolation = .some(@fromBackingInt(columns.interpolation_metadata.rankOfSurvivor(@backingInt(metadata))));
                items[next] = constraint;
            }
            self.static_dispatch_constraints.shrinkRetainingCapacity(next);
        }
        slideSurvivors(NumeralInfo, self.numeral_infos.items(), &columns.numeral_infos);
        self.numeral_infos.shrinkRetainingCapacity(columns.numeral_infos.count);
        {
            const items = self.interpolation_metadata.items();
            var next: usize = 0;
            var kept = columns.interpolation_metadata.bits.iterator(.{});
            while (kept.next()) |index| : (next += 1) {
                var metadata = items[index];
                metadata.item_var = columns.renumberVar(metadata.item_var);
                metadata.interpolated_parts.start = @fromBackingInt(columns.interpolation_parts.rankOf(@backingInt(metadata.interpolated_parts.start)));
                items[next] = metadata;
            }
            self.interpolation_metadata.shrinkRetainingCapacity(next);
        }
        {
            const items = self.interpolation_parts.items();
            var next: usize = 0;
            var kept = columns.interpolation_parts.bits.iterator(.{});
            while (kept.next()) |index| : (next += 1) {
                var part = items[index];
                part.var_ = columns.renumberVar(part.var_);
                items[next] = part;
            }
            self.interpolation_parts.shrinkRetainingCapacity(next);
        }
        for (self.nominal_decls.items()) |*decl| {
            decl.backing = columns.renumberVar(decl.backing);
            decl.formals.start = @fromBackingInt(columns.var_pool.rankOf(@backingInt(decl.formals.start)));
        }
        slideSurvivors(NominalIdentity, self.nominal_identities.items(), &columns.nominal_identities);
        self.nominal_identities.shrinkRetainingCapacity(columns.nominal_identities.count);
        slideSurvivors(AliasIdentity, self.alias_identities.items(), &columns.alias_identities);
        self.alias_identities.shrinkRetainingCapacity(columns.alias_identities.count);

        {
            // Every slot names its descriptor at the representative or
            // redirects to the representative (`canonicalizeForSeal`), so a
            // slot is renumbered without reading any other slot, and a slot
            // only ever moves down onto one already read.
            var next: u32 = 0;
            var kept = vars.bits.iterator(.{});
            while (kept.next()) |index| : (next += 1) {
                const slot_idx: SlotStore.Idx = @fromBackingInt(@intCast(index));
                const renumbered: Slot = switch (self.slots.get(slot_idx)) {
                    .root => |desc_idx| .{ .root = @fromBackingInt(columns.descs.rankOfSurvivor(@backingInt(desc_idx))) },
                    .redirect => |representative| .{ .redirect = columns.renumberVar(representative) },
                };
                self.slots.set(@fromBackingInt(next), renumbered);
                self.union_ranks.items[next] = self.union_ranks.items[index];
            }
            self.slots.shrinkRetainingCapacity(next);
            self.union_ranks.shrinkRetainingCapacity(next);
        }
        {
            var next: u32 = 0;
            var kept = columns.descs.bits.iterator(.{});
            while (kept.next()) |index| : (next += 1) {
                const old: DescStore.Idx = @fromBackingInt(@intCast(index));
                const new: DescStore.Idx = @fromBackingInt(next);
                var row = self.descs.getRow(old);
                columns.renumberRow(&row);
                self.descs.setRow(new, row);
                self.root_metas.set(rootMetaIdx(new), .{ .checked_var = columns.renumberVar(self.getRootMeta(old).checked_var) });
            }
            self.descs.backing.shrinkRetainingCapacity(next);
            self.root_metas.shrinkRetainingCapacity(next);
        }
        {
            // A plan belongs to a class, so a dead class's plan dies with it,
            // and renumbering keeps the entries sorted by class.
            const entries = self.derived_map_plans.items();
            var next: usize = 0;
            for (entries) |entry| {
                const class: u32 = @backingInt(entry.class);
                if (!columns.descs.survives(class)) continue;
                entries[next] = .{ .class = @fromBackingInt(columns.descs.rankOf(class)), .plan = entry.plan };
                next += 1;
            }
            self.derived_map_plans.shrinkRetainingCapacity(next);
        }

        if (std.debug.runtime_safety) self.debugAssertSealedLayout();
    }

    /// After the gaps close: each class is stored at its representative,
    /// every other slot names a representative, and every stored row is its
    /// descriptor's encoding, so every variable and range it holds is in
    /// range.
    fn debugAssertSealedLayout(self: *const Self) void {
        const slot_count: u32 = @intCast(self.slots.len());
        for (self.slots.words.items, 0..) |word, index| {
            switch (SlotStore.decode(word)) {
                .root => |desc_idx| if (@backingInt(desc_idx) >= self.descs.backing.len() or @backingInt(self.getRootMeta(desc_idx).checked_var) != index) {
                    base.invariant("sealed slot {d} stores a class it does not represent", .{index});
                },
                .redirect => |target| if (@backingInt(target) >= slot_count or SlotStore.decode(self.slots.words.items[@backingInt(target)]) != .root) {
                    base.invariant("sealed slot {d} names {d}, which is not a representative", .{ index, @backingInt(target) });
                },
            }
        }
        for (0..self.descs.backing.len()) |index| {
            const desc_idx: DescStore.Idx = @fromBackingInt(@intCast(index));
            const row = self.descs.getRow(desc_idx);
            const identity: u32 = switch (row.word.kind) {
                .alias, .nominal_type => row.payload[0],
                .flex, .rigid, .field_presence, .err, .record, .tuple, .fn_pure, .fn_effectful, .fn_unbound, .empty_record, .tag_union, .empty_tag_union => 0,
            };
            if (!std.meta.eql(row, self.descAt(desc_idx).encode(identity, self))) {
                base.invariant("sealing left descriptor {d}'s stored row unlike its descriptor's encoding", .{index});
            }
            switch (row.word.kind) {
                .fn_pure, .fn_effectful, .fn_unbound => if (row.payload[Desc.fn_ret_payload_word] >= slot_count) base.invariant("sealed descriptor {d} returns a variable out of range", .{index}),
                .record, .tag_union => if (row.payload[Desc.row_ext_payload_word] >= slot_count) base.invariant("sealed descriptor {d} extends a variable out of range", .{index}),
                .flex, .rigid, .alias, .field_presence, .err, .tuple, .nominal_type, .empty_record, .empty_tag_union => {},
            }
        }
        for (self.vars.items) |entry| {
            if (!isRangeLengthSlot(entry) and @backingInt(entry) >= slot_count) base.invariant("a sealed vars pool entry is out of range", .{});
        }
        for (self.static_dispatch_constraints.items()) |constraint| {
            if (@backingInt(constraint.fn_var) >= slot_count) base.invariant("a sealed constraint's function variable is out of range", .{});
        }
    }

    /// Slide the survivors of a column of plain rows down to their ranks.
    fn slideSurvivors(comptime T: type, items: []T, survivors: *const SealSurvivors) void {
        var next: usize = 0;
        var kept = survivors.bits.iterator(.{});
        while (kept.next()) |index| : (next += 1) items[next] = items[index];
    }

    /// What a seal-holder visitor does with each variable or constraint index
    /// it is handed (`canonicalizeAndSeal`).
    pub const SealHolderPhase = enum {
        /// Keep it alive through sealing.
        keep,
        /// Rewrite it to its number once the gaps have closed.
        renumber,
    };

    /// The visitor a seal holder's `visitSealHolders` hands each variable and
    /// static-dispatch constraint index it holds outside the store. A holder
    /// visits exactly the same entries in both phases.
    pub fn SealHolderVisitor(comptime phase: SealHolderPhase) type {
        return struct {
            columns: *SealColumns,

            /// A variable held as its raw number.
            pub fn rawVar(self: @This(), var_: *u32) void {
                switch (phase) {
                    .keep => self.columns.keepHolderVar(var_.*),
                    .renumber => var_.* = self.columns.vars.rankOfSurvivor(var_.*),
                }
            }

            /// A variable.
            pub fn typeVar(self: @This(), var_: *Var) void {
                var raw: u32 = @backingInt(var_.*);
                self.rawVar(&raw);
                var_.* = @fromBackingInt(raw);
            }

            /// A variable held as an optional id.
            pub fn optionalVar(self: @This(), var_: *collections.OptionalId(Var)) void {
                var present = var_.get() orelse return;
                self.typeVar(&present);
                var_.* = .some(present);
            }

            /// A variable held as an optional raw number.
            pub fn optionalRawVar(self: @This(), var_: *collections.OptionalU32) void {
                var present = var_.get() orelse return;
                self.rawVar(&present);
                var_.* = .of(present);
            }

            /// The index of a row of the store's static-dispatch constraints.
            pub fn constraintIndex(self: @This(), index: *u32) void {
                switch (phase) {
                    .keep => self.columns.constraints.keep(index.*),
                    .renumber => index.* = self.columns.constraints.rankOfSurvivor(index.*),
                }
            }
        };
    }

    /// A holder of nothing, for a store no data outside it names.
    pub const NoSealHolders = struct {
        pub fn visitSealHolders(_: NoSealHolders, _: anytype) void {}
    };

    /// The survivors of every column sealing closes the gaps of, and how each
    /// descriptor's rows are kept and renumbered.
    pub const SealColumns = struct {
        /// Slots: every variable below the fixed count, every class's
        /// representative, and every variable a holder or a surviving
        /// constraint names.
        vars: SealSurvivors,
        /// The variables holders outside the store name.
        holder_vars: std.DynamicBitSetUnmanaged,
        descs: SealSurvivors,
        var_pool: SealSurvivors,
        record_fields: SealSurvivors,
        tags: SealSurvivors,
        constraints: SealSurvivors,
        numeral_infos: SealSurvivors,
        interpolation_metadata: SealSurvivors,
        interpolation_parts: SealSurvivors,
        nominal_identities: SealSurvivors,
        alias_identities: SealSurvivors,
        /// The record field and tag rows before sealing appended flattened
        /// rows, every one of which survives.
        record_fields_before: u32,
        tags_before: u32,

        fn init(store: *const Self, fixed_vars: u64) Allocator.Error!SealColumns {
            const gpa = store.gpa;
            const slot_count = store.slots.len();
            std.debug.assert(fixed_vars <= slot_count);
            var columns: SealColumns = .{
                .vars = .{},
                .holder_vars = .{},
                .descs = .{},
                .var_pool = .{},
                .record_fields = .{},
                .tags = .{},
                .constraints = .{},
                .numeral_infos = .{},
                .interpolation_metadata = .{},
                .interpolation_parts = .{},
                .nominal_identities = .{},
                .alias_identities = .{},
                .record_fields_before = store.record_fields.len(),
                .tags_before = store.tags.len(),
            };
            errdefer columns.deinit(gpa);
            columns.vars = try .init(gpa, slot_count);
            columns.vars.keepRange(0, @intCast(fixed_vars));
            columns.holder_vars = try .initEmpty(gpa, slot_count);
            columns.descs = try .init(gpa, store.descs.backing.len());
            columns.var_pool = try .init(gpa, store.vars.items.len);
            columns.record_fields = try .init(gpa, columns.record_fields_before);
            columns.tags = try .init(gpa, columns.tags_before);
            columns.constraints = try .init(gpa, store.static_dispatch_constraints.items().len);
            columns.numeral_infos = try .init(gpa, store.numeral_infos.items().len);
            columns.interpolation_metadata = try .init(gpa, store.interpolation_metadata.items().len);
            columns.interpolation_parts = try .init(gpa, store.interpolation_parts.items().len);
            columns.nominal_identities = try .init(gpa, store.nominal_identities.items().len);
            columns.alias_identities = try .init(gpa, store.alias_identities.items().len);
            return columns;
        }

        fn deinit(self: *SealColumns, gpa: Allocator) void {
            self.vars.deinit(gpa);
            self.holder_vars.deinit(gpa);
            self.descs.deinit(gpa);
            self.var_pool.deinit(gpa);
            self.record_fields.deinit(gpa);
            self.tags.deinit(gpa);
            self.constraints.deinit(gpa);
            self.numeral_infos.deinit(gpa);
            self.interpolation_metadata.deinit(gpa);
            self.interpolation_parts.deinit(gpa);
            self.nominal_identities.deinit(gpa);
            self.alias_identities.deinit(gpa);
        }

        /// A holder outside the store names `raw`, which survives as itself.
        fn keepHolderVar(self: *SealColumns, raw: u32) void {
            if (std.debug.runtime_safety and raw >= self.holder_vars.bit_length) {
                base.invariant("a holder outside the type store names variable {d}, past the store's {d}", .{ raw, self.holder_vars.bit_length });
            }
            self.holder_vars.set(raw);
            self.vars.keep(raw);
        }

        /// Marking is over: the flattened rows survive, and every column
        /// counts its survivors.
        fn finish(self: *SealColumns, store: *const Self) Allocator.Error!void {
            const gpa = store.gpa;
            try self.record_fields.growSurviving(gpa, store.record_fields.len());
            try self.tags.growSurviving(gpa, store.tags.len());
            inline for (.{ "vars", "descs", "var_pool", "record_fields", "tags", "constraints", "numeral_infos", "interpolation_metadata", "interpolation_parts", "nominal_identities", "alias_identities" }) |name| {
                try @field(self, name).finish(gpa);
            }
        }

        /// The number surviving variable `var_` has once the slots close
        /// their gaps.
        fn renumberVar(self: *const SealColumns, var_: Var) Var {
            return @fromBackingInt(self.vars.rankOfSurvivor(@backingInt(var_)));
        }

        /// Keep the vars range a stored descriptor keeps as `stored`
        /// (`varRange`), with the length slot before it.
        fn keepStoredVarRange(self: *SealColumns, store: *const Self, stored: u32) void {
            if (stored == stored_empty_var_range) return;
            const count = @backingInt(store.vars.items[stored - 1]) & ~range_length_bit;
            self.var_pool.keepRange(stored - 1, count + 1);
        }

        /// Keep every pool row and identity row the canonical descriptor
        /// `desc_idx` names. A record or tag union whose row sealing
        /// flattened names rows appended after marking began, which survive
        /// without marking.
        fn keepRowsOf(self: *SealColumns, store: *const Self, desc_idx: DescStore.Idx) void {
            const row = store.descs.getRow(desc_idx);
            const p = row.payload;
            switch (row.word.kind) {
                .field_presence, .err, .empty_record, .empty_tag_union => {},
                .flex, .rigid => {
                    const constraints = Desc.variableConstraints(row);
                    self.constraints.keepRange(@backingInt(constraints.start), constraints.count);
                },
                .alias => {
                    self.alias_identities.keep(p[0]);
                    self.keepStoredVarRange(store, p[1]);
                },
                .nominal_type => {
                    self.nominal_identities.keep(p[0]);
                    self.keepStoredVarRange(store, p[1]);
                },
                .tuple => self.keepStoredVarRange(store, p[0]),
                .fn_pure, .fn_effectful, .fn_unbound => {
                    self.keepStoredVarRange(store, p[0]);
                    self.keepStoredVarRange(store, p[2]);
                },
                .record => {
                    const fields = Desc.decodeRecord(row).fields;
                    if (@backingInt(fields.start) < self.record_fields_before) self.record_fields.keepRange(@backingInt(fields.start), fields.count);
                },
                .tag_union => {
                    const tags = Desc.decodeTagUnion(row).tags;
                    for (store.tags.sliceRange(tags).items(.args)) |args| self.var_pool.keepRange(@backingInt(args.start), args.count);
                    if (@backingInt(tags.start) < self.tags_before) self.tags.keepRange(@backingInt(tags.start), tags.count);
                },
            }
        }

        /// What a stored descriptor keeps of a vars range `stored`
        /// (`storedVarRange`) once the vars pool has closed its gaps.
        fn renumberStoredVarRange(self: *const SealColumns, stored: u32) u32 {
            if (stored == stored_empty_var_range) return stored_empty_var_range;
            return self.var_pool.rankOfSurvivor(stored - 1) + 1;
        }

        /// Renumber every variable, pool index and identity index a stored
        /// descriptor holds.
        fn renumberRow(self: *const SealColumns, row: *Desc.Row) void {
            const p = &row.payload;
            switch (row.word.kind) {
                .field_presence, .err, .empty_record, .empty_tag_union => {},
                .flex, .rigid => p[1] = self.constraints.rankOf(p[1]),
                .alias => {
                    p[0] = self.alias_identities.rankOfSurvivor(p[0]);
                    p[1] = self.renumberStoredVarRange(p[1]);
                },
                .nominal_type => {
                    p[0] = self.nominal_identities.rankOfSurvivor(p[0]);
                    p[1] = self.renumberStoredVarRange(p[1]);
                },
                .tuple => p[0] = self.renumberStoredVarRange(p[0]),
                .fn_pure, .fn_effectful, .fn_unbound => {
                    p[0] = self.renumberStoredVarRange(p[0]);
                    p[Desc.fn_ret_payload_word] = self.vars.rankOfSurvivor(p[Desc.fn_ret_payload_word]);
                    p[2] = self.renumberStoredVarRange(p[2]);
                },
                .record => {
                    p[0] = self.record_fields.rankOf(p[0]);
                    p[Desc.row_ext_payload_word] = self.vars.rankOfSurvivor(p[Desc.row_ext_payload_word]);
                },
                .tag_union => {
                    p[0] = self.tags.rankOf(p[0]);
                    p[Desc.row_ext_payload_word] = self.vars.rankOfSurvivor(p[Desc.row_ext_payload_word]);
                },
            }
        }
    };

    /// Flattens the chained rows of a store being canonicalized.
    const RowFlattener = struct {
        store: *Self,
        idents: *const base.Ident.Store,
        fields: std.ArrayListUnmanaged(RecordField) = .empty,
        tags: std.ArrayListUnmanaged(Tag) = .empty,

        fn deinit(self: *RowFlattener) void {
            self.fields.deinit(self.store.gpa);
            self.tags.deinit(self.store.gpa);
        }

        /// Make the row `desc_idx` absorb the rows its extension chain
        /// reaches. A row whose extension is not a row is left as it is.
        fn flatten(self: *RowFlattener, desc_idx: DescStore.Idx) Allocator.Error!void {
            const store = self.store;
            switch (store.descs.getWord(desc_idx).kind) {
                .record => {
                    var segment = store.recordAt(desc_idx);
                    if (store.extensionRecord(segment.ext) == null) return;
                    self.fields.clearRetainingCapacity();
                    var guard = debug.IterationGuard.init("canonicalizeForSeal.record");
                    while (true) {
                        guard.tick();
                        var fields = store.record_fields.iterRange(segment.fields);
                        while (fields.next()) |field| try self.fields.append(store.gpa, field);
                        segment = store.extensionRecord(segment.ext) orelse break;
                    }
                    std.mem.sort(RecordField, self.fields.items, self.idents, RecordField.sortByNameAsc);
                    var desc = store.descAt(desc_idx);
                    desc.content = .{ .structure = .{ .record = .{
                        .fields = try store.record_fields.appendSlice(store.gpa, self.fields.items),
                        .ext = segment.ext,
                    } } };
                    store.descs.setRow(desc_idx, store.encodeDescWithoutIdentity(desc));
                },
                .tag_union => {
                    var segment = store.tagUnionAt(desc_idx);
                    if (store.extensionTagUnion(segment.ext) == null) return;
                    self.tags.clearRetainingCapacity();
                    var guard = debug.IterationGuard.init("canonicalizeForSeal.tag_union");
                    while (true) {
                        guard.tick();
                        var tags = store.tags.iterRange(segment.tags);
                        while (tags.next()) |tag| try self.tags.append(store.gpa, tag);
                        segment = store.extensionTagUnion(segment.ext) orelse break;
                    }
                    std.mem.sort(Tag, self.tags.items, self.idents, Tag.sortByNameAsc);
                    var desc = store.descAt(desc_idx);
                    desc.content = .{ .structure = .{ .tag_union = .{
                        .tags = try store.tags.appendSlice(store.gpa, self.tags.items),
                        .ext = segment.ext,
                    } } };
                    store.descs.setRow(desc_idx, store.encodeDescWithoutIdentity(desc));
                },
                .flex, .rigid, .alias, .field_presence, .err, .tuple, .nominal_type, .empty_record, .empty_tag_union, .fn_pure, .fn_effectful, .fn_unbound => unreachable, // callers flatten only record and tag-union descriptors
            }
        }
    };

    /// Walks every descriptor a sealed store's readers can reach from its
    /// roots, flattening each row it reaches before reading the row's
    /// children.
    const SealTracer = struct {
        flattener: *RowFlattener,
        reached: *std.DynamicBitSetUnmanaged,
        pending: std.ArrayListUnmanaged(DescStore.Idx) = .empty,

        pub fn visit(self: *SealTracer, var_: Var) Allocator.Error!void {
            const store = self.flattener.store;
            const desc_idx = store.resolveDescIdx(var_);
            if (self.reached.isSet(@backingInt(desc_idx))) return;
            self.reached.set(@backingInt(desc_idx));
            switch (store.descs.getWord(desc_idx).kind) {
                // These hold no variable.
                .field_presence, .err, .empty_record, .empty_tag_union => {},
                .flex, .rigid, .alias, .tuple, .nominal_type, .fn_pure, .fn_effectful, .fn_unbound, .record, .tag_union => try self.pending.append(store.gpa, desc_idx),
            }
        }

        pub fn visitRequired(self: *SealTracer, var_: ?Var, comptime _: []const u8) Allocator.Error!void {
            if (var_) |required| try self.visit(required);
        }

        /// Visit every variable below `count`.
        pub fn visitLeading(self: *SealTracer, count: u32) Allocator.Error!void {
            for (0..count) |raw| try self.visit(@fromBackingInt(@intCast(raw)));
        }

        fn expand(self: *SealTracer, desc_idx: DescStore.Idx) Allocator.Error!void {
            const store = self.flattener.store;
            switch (store.descs.getWord(desc_idx).kind) {
                .field_presence, .err, .empty_record, .empty_tag_union => unreachable, // `visit` queues only descriptors that hold a variable
                .flex, .rigid => for (store.sliceStaticDispatchConstraints(store.variableConstraintsAt(desc_idx))) |constraint| {
                    try self.visit(constraint.fn_var);
                },
                .alias => {
                    const alias = store.aliasAt(desc_idx);
                    try self.visit(store.getAliasBackingVar(alias));
                    for (store.sliceAliasArgs(alias)) |arg| try self.visit(arg);
                },
                .tuple => for (store.sliceVars(store.tupleAt(desc_idx).elems)) |elem| try self.visit(elem),
                .nominal_type => for (store.sliceNominalArgs(store.nominalAt(desc_idx))) |arg| try self.visit(arg),
                .fn_pure, .fn_effectful, .fn_unbound => {
                    const func = store.funcAt(desc_idx);
                    for (store.sliceVars(func.args)) |arg| try self.visit(arg);
                    try self.visit(func.ret);
                },
                .record => {
                    try self.flattener.flatten(desc_idx);
                    const record = store.recordAt(desc_idx);
                    var fields = store.record_fields.iterRange(record.fields);
                    while (fields.next()) |field| {
                        try self.visit(field.presence.typeVar());
                        if (field.presence.presenceVar()) |presence| try self.visit(presence);
                    }
                    try self.visit(record.ext);
                },
                .tag_union => {
                    try self.flattener.flatten(desc_idx);
                    const tag_union = store.tagUnionAt(desc_idx);
                    var tags = store.tags.iterRange(tag_union.tags);
                    while (tags.next()) |tag| {
                        for (store.sliceVars(tag.args)) |arg| try self.visit(arg);
                    }
                    try self.visit(tag_union.ext);
                },
            }
        }
    };

    /// Replace the variable a function or row descriptor holds inline in
    /// `payload_word`, leaving the rest of its stored row as it is: the row
    /// is then what encoding the descriptor with that variable gives.
    fn setInlineVarForSeal(self: *Self, desc_idx: DescStore.Idx, comptime payload_word: usize, var_: Var) void {
        self.descs.setPayloadWord(desc_idx, payload_word, @backingInt(var_));
        if (std.debug.runtime_safety) {
            const reencoded = self.encodeDescWithoutIdentity(self.descAt(desc_idx));
            if (!std.meta.eql(self.descs.getRow(desc_idx), reencoded)) {
                base.invariant("a sealed descriptor's stored row is not its descriptor's encoding", .{});
            }
        }
    }

    /// The checked representative of the class `root` stores.
    fn representativeOf(root: StorageRoot) Var {
        const representative = root.meta.checked_var;
        if (std.debug.runtime_safety and @backingInt(representative) >= (@as(u32, 1) << 31)) {
            base.invariant("type variable {d} does not fit below the checked synthesized-type bit", .{@backingInt(representative)});
        }
        return representative;
    }

    /// The checked representative of `var_`'s class.
    fn representativeVar(self: *const Self, var_: Var) Var {
        return representativeOf(self.findStorageRoot(var_));
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
        self.backing_openings.deinit(self.gpa);
        self.backing_openings = .{};
        self.frozen_anchors.deinit();
        self.frozen_anchors = .init(self.gpa);
        self.class_watch.deinit(self.gpa);
        self.class_watch = .{};
        self.settled_descs.deinit(self.gpa);
        self.settled_descs = .{};
        self.function_free_descs.deinit(self.gpa);
        self.function_free_descs = .{};
        self.detach_members_scratch.deinit(self.gpa);
        self.detach_members_scratch = .empty;
        self.undo_log.deinit(self.gpa);
        self.undo_log = .empty;
        self.tag_tail_closings.deinit(self.gpa);
        self.tag_tail_closings = .empty;
        self.free_descs.deinit(self.gpa);
        self.free_descs = .empty;
        self.pending_free_descs.deinit(self.gpa);
        self.pending_free_descs = .empty;
        self.probe_free_descs.deinit(self.gpa);
        self.probe_free_descs = .empty;
        self.static_dispatch_relation_merges.deinit(self.gpa);
        self.static_dispatch_relation_merges = .empty;
        self.pattern_only_tag_copies.deinit(self.gpa);
        self.pattern_only_tag_copies = .empty;
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
    pub fn len(self: *const Self) usize {
        return self.slots.len();
    }

    /// Return true when checking left any live type variable in the explicit
    /// error state. Descriptors not referenced by a current slot are rollback
    /// history and do not affect checked output.
    pub fn containsErrContent(self: *const Self) bool {
        for (self.slots.words.items) |word| {
            switch (SlotStore.decode(word)) {
                .root => |desc_idx| {
                    if (self.descs.getWord(desc_idx).kind == .err) return true;
                },
                .redirect => {},
            }
        }
        return false;
    }

    // probe (see `WriteFamily`) //

    /// The lengths of the append-only lists the unifier grows, which
    /// discarding a probe rewinds to. The slot and descriptor counts live on
    /// the store as `probe_baseline_*`, because they are also the unifier
    /// writes' journaling test, and the undo log needs no length: it is
    /// empty whenever a probe begins.
    pub const ProbeMark = struct {
        vars_len: usize,
        record_fields_len: usize,
        tags_len: usize,
        interpolation_parts_len: usize,
        numeral_infos_len: usize,
        interpolation_metadata_len: usize,
        static_dispatch_constraints_len: usize,
        static_dispatch_relation_merges_len: usize,
        pattern_only_tag_copies_len: usize,
    };

    /// Full store copy a test compares the store against (`TestCopy`).
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

    fn cloneForProbeVerification(self: *Self) Allocator.Error!VerifyClone {
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

    /// The list lengths a probe restores.
    fn markForRestore(self: *const Self) ProbeMark {
        std.debug.assert(self.pending_free_descs.items.len == 0);
        std.debug.assert(self.undo_log.items.len == 0);
        return .{
            .vars_len = self.vars.items.len,
            .record_fields_len = self.record_fields.len(),
            .tags_len = self.tags.len(),
            .interpolation_parts_len = self.interpolation_parts.items().len,
            .numeral_infos_len = self.numeral_infos.items().len,
            .interpolation_metadata_len = self.interpolation_metadata.items().len,
            .static_dispatch_constraints_len = self.static_dispatch_constraints.items().len,
            .static_dispatch_relation_merges_len = self.static_dispatch_relation_merges.items.len,
            .pattern_only_tag_copies_len = self.pattern_only_tag_copies.items.len,
        };
    }

    /// Assert that no probe is open. Mismatch poisoning is permanent error
    /// recovery and must never run speculatively; a probe relates through the
    /// non-poisoning relation API.
    pub fn assertNoProbeOpen(self: *const Self) void {
        std.debug.assert(!self.probe_open);
    }

    /// Restore every entry journaled since `mark` and drop everything
    /// appended since.
    fn restoreTo(self: *Self, mark: *const ProbeMark) void {
        self.class_watch.noteRollback();
        self.settled_descs.clear();
        self.function_free_descs.clear();
        // Rows for classes appended since the probe began sort after every
        // older class, and their writes are not journaled: drop them first,
        // so the rows of older classes stand at the indices their journaled
        // writes name.
        var plans_len = self.derived_map_plans.items().len;
        while (plans_len > 0 and @backingInt(self.derived_map_plans.items()[plans_len - 1].class) >= self.probe_baseline_descs) plans_len -= 1;
        self.derived_map_plans.shrinkRetainingCapacity(plans_len);
        var closings_len = self.tag_tail_closings.items.len;
        while (closings_len > 0 and @backingInt(self.tag_tail_closings.items[closings_len - 1].class) >= self.probe_baseline_descs) closings_len -= 1;
        self.tag_tail_closings.shrinkRetainingCapacity(closings_len);

        // Replay the log strictly backwards, so each pre-existing entry lands
        // back on its original value. A removed row goes back into room the
        // list had when the row was removed.
        var index = self.undo_log.items.len;
        while (index > 0) {
            index -= 1;
            switch (self.undo_log.items[index]) {
                .slot => |u| {
                    self.slots.set(u.idx, u.old);
                    self.slot_generation += 1;
                },
                .desc => |u| self.descs.setRow(u.idx, u.old),
                .root_meta => |u| self.root_metas.set(rootMetaIdx(u.idx), u.old),
                .union_rank => |u| self.union_ranks.set(unionRankIdx(u.idx), u.old),
                .plan_inserted => |at| _ = self.derived_map_plans.orderedRemove(at),
                .plan_removed => |u| self.derived_map_plans.insertAssumeCapacity(u.index, u.entry),
                .plan_replaced => |u| self.derived_map_plans.items()[u.index].plan = u.old,
                .closing_inserted => |at| _ = self.tag_tail_closings.orderedRemove(at),
                .closing_removed => |u| self.tag_tail_closings.insertAssumeCapacity(u.index, u.entry),
                .closing_replaced => |u| self.tag_tail_closings.items[u.index].closing = u.old,
            }
        }
        self.undo_log.clearRetainingCapacity();

        // Drop everything appended since the mark. The slot/desc baselines are
        // the store fields (also the journaling threshold); the rest come from
        // the mark.
        self.slots.shrinkRetainingCapacity(self.probe_baseline_slots);
        self.descs.backing.shrinkRetainingCapacity(self.probe_baseline_descs);
        self.root_metas.shrinkRetainingCapacity(self.probe_baseline_descs);
        self.union_ranks.shrinkRetainingCapacity(self.probe_baseline_slots);
        self.vars.shrinkRetainingCapacity(mark.vars_len);
        self.record_fields.shrinkRetainingCapacity(mark.record_fields_len);
        self.tags.shrinkRetainingCapacity(mark.tags_len);
        self.interpolation_parts.shrinkRetainingCapacity(mark.interpolation_parts_len);
        self.numeral_infos.shrinkRetainingCapacity(mark.numeral_infos_len);
        self.interpolation_metadata.shrinkRetainingCapacity(mark.interpolation_metadata_len);
        self.static_dispatch_constraints.shrinkRetainingCapacity(mark.static_dispatch_constraints_len);
        self.static_dispatch_relation_merges.shrinkRetainingCapacity(mark.static_dispatch_relation_merges_len);
        self.pattern_only_tag_copies.shrinkRetainingCapacity(mark.pattern_only_tag_copies_len);

        // The merges are undone, so nothing they orphaned is dead.
        self.pending_free_descs.clearRetainingCapacity();
    }

    // probe (see `WriteFamily`) //

    /// Open a probe: from here until `endProbeYes` or `endProbeNo`, the
    /// unifier's writes journal each in-place write to an entry that predates
    /// the probe, and any other code may only append and write to entries
    /// appended since. The free list is set aside, so every class created
    /// meanwhile is appended. Probes never nest.
    pub fn beginProbe(self: *Self) ProbeMark {
        self.debugAssertUnsealed();
        std.debug.assert(!self.probe_open);
        std.debug.assert(self.probe_free_descs.items.len == 0);
        const mark = self.markForRestore();
        self.probe_baseline_slots = @intCast(self.slots.len());
        self.probe_baseline_descs = @intCast(self.descs.backing.len());
        std.mem.swap(std.ArrayListUnmanaged(DescStore.Idx), &self.free_descs, &self.probe_free_descs);
        self.probe_open = true;
        return mark;
    }

    /// Close the open probe keeping everything it wrote: the descriptors its
    /// merges orphaned are dead for good, and go on the free list.
    pub fn endProbeYes(self: *Self) Allocator.Error!void {
        self.debugAssertUnsealed();
        std.debug.assert(self.probe_open);
        self.undo_log.clearRetainingCapacity();
        self.probe_open = false;
        self.probe_baseline_slots = 0;
        self.probe_baseline_descs = 0;
        self.restoreFreeList();
        try self.free_descs.ensureUnusedCapacity(self.gpa, self.pending_free_descs.items.len);
        for (self.pending_free_descs.items) |desc_idx| self.recycleDesc(desc_idx);
        self.pending_free_descs.clearRetainingCapacity();
    }

    /// Close the open probe discarding everything since `beginProbe`: every
    /// journaled entry is restored and everything appended is dropped, so
    /// the store is as it was.
    pub fn endProbeNo(self: *Self, mark: *const ProbeMark) void {
        self.debugAssertUnsealed();
        std.debug.assert(self.probe_open);
        self.restoreTo(mark);
        self.probe_open = false;
        self.probe_baseline_slots = 0;
        self.probe_baseline_descs = 0;
        self.restoreFreeList();
    }

    /// Put back the free list a probe set aside. Nothing is freed during a
    /// probe except into `pending_free_descs`, so the set-aside list is the
    /// whole of it.
    fn restoreFreeList(self: *Self) void {
        std.debug.assert(self.free_descs.items.len == 0);
        std.mem.swap(std.ArrayListUnmanaged(DescStore.Idx), &self.free_descs, &self.probe_free_descs);
    }

    /// Whether a probe is open.
    pub fn probeOpen(self: *const Self) bool {
        return self.probe_open;
    }

    /// What `enterUnifier` hands `leaveUnifier`: whether the unifier was
    /// already running. Debug builds only.
    pub const UnifierRunning = if (std.debug.runtime_safety) bool else void;

    /// Note that the unifier runs from here until `leaveUnifier`, which a
    /// unifier write asserts while a probe is open. Debug builds only.
    pub inline fn enterUnifier(self: *Self) UnifierRunning {
        if (!std.debug.runtime_safety) return {};
        const was = self.unifier_running;
        self.unifier_running = true;
        return was;
    }

    pub inline fn leaveUnifier(self: *Self, was: UnifierRunning) void {
        if (!std.debug.runtime_safety) return;
        self.unifier_running = was;
    }

    /// What a failed test expectation returns.
    pub const TestExpectError = error{ TestExpectedEqual, TestUnexpectedResult };

    /// A copy of everything a discarded probe restores, for a test to compare
    /// the store against (`expectRestoredTo`). Test builds only.
    pub const TestCopy = struct {
        clone: VerifyClone,
        mark: ProbeMark,
        free_descs: []DescStore.Idx,
        tag_tail_closings: []TagTailClosingEntry,

        pub fn deinit(self: *TestCopy, gpa: Allocator) void {
            self.clone.deinit(gpa);
            gpa.free(self.free_descs);
            gpa.free(self.tag_tail_closings);
        }
    };

    pub fn testCopy(self: *Self) Allocator.Error!TestCopy {
        comptime std.debug.assert(builtin.is_test);
        var full = try self.cloneForProbeVerification();
        errdefer full.deinit(self.gpa);
        const free = try self.gpa.dupe(DescStore.Idx, self.free_descs.items);
        errdefer self.gpa.free(free);
        return .{
            .clone = full,
            .mark = self.markForRestore(),
            .free_descs = free,
            .tag_tail_closings = try self.gpa.dupe(TagTailClosingEntry, self.tag_tail_closings.items),
        };
    }

    /// Whether the store holds exactly what `copy` holds: every slot,
    /// descriptor, root, union rank, list length, free descriptor and side
    /// list.
    pub fn expectRestoredTo(self: *Self, copy: *const TestCopy) TestExpectError!void {
        comptime std.debug.assert(builtin.is_test);
        self.assertMatchesClone(&copy.clone);
        const now = self.markForRestore();
        try std.testing.expectEqual(copy.mark.vars_len, now.vars_len);
        try std.testing.expectEqual(copy.mark.record_fields_len, now.record_fields_len);
        try std.testing.expectEqual(copy.mark.tags_len, now.tags_len);
        try std.testing.expectEqual(copy.mark.interpolation_parts_len, now.interpolation_parts_len);
        try std.testing.expectEqual(copy.mark.numeral_infos_len, now.numeral_infos_len);
        try std.testing.expectEqual(copy.mark.interpolation_metadata_len, now.interpolation_metadata_len);
        try std.testing.expectEqual(copy.mark.static_dispatch_constraints_len, now.static_dispatch_constraints_len);
        try std.testing.expectEqual(copy.mark.static_dispatch_relation_merges_len, now.static_dispatch_relation_merges_len);
        try std.testing.expectEqual(copy.mark.pattern_only_tag_copies_len, now.pattern_only_tag_copies_len);
        try std.testing.expectEqualSlices(DescStore.Idx, copy.free_descs, self.free_descs.items);
        try std.testing.expectEqualSlices(TagTailClosingEntry, copy.tag_tail_closings, self.tag_tail_closings.items);
        try std.testing.expectEqual(@as(usize, 0), self.pending_free_descs.items.len);
    }

    /// Whether every variable of `self` and `other` resolves to the same
    /// checked representative holding the same descriptor, over the same
    /// variable, field and tag lists: the two hold the same solved types,
    /// however their storage paths and descriptor indices differ.
    pub fn expectSameSolvedTypes(self: *const Self, other: *const Self) TestExpectError!void {
        comptime std.debug.assert(builtin.is_test);
        try std.testing.expectEqual(self.len(), other.len());
        try std.testing.expectEqualSlices(Var, self.vars.items, other.vars.items);
        try std.testing.expectEqual(self.record_fields.len(), other.record_fields.len());
        try std.testing.expectEqual(self.tags.len(), other.tags.len());
        for (0..self.len()) |index| {
            const var_: Var = @fromBackingInt(@intCast(index));
            const mine = self.resolveVar(var_);
            const theirs = other.resolveVar(var_);
            try std.testing.expectEqual(mine.var_, theirs.var_);
            try std.testing.expect(std.meta.eql(mine.desc, theirs.desc));
        }
    }

    /// Whether `var_` existed when the open probe began.
    pub fn predatesProbe(self: *const Self, var_: Var) bool {
        std.debug.assert(self.probe_open);
        return @backingInt(var_) < self.probe_baseline_slots;
    }

    /// Clone cross-check: assert the trail-restored store is byte-for-byte
    /// identical to the full copy `vclone`.
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

    /// Whether a write of `family` to the entry at `index` of a list whose
    /// open-probe baseline is `baseline` must journal the entry's old value.
    /// A unifier write journals every entry that predates the open probe, with
    /// one comparison: outside a probe the baseline is 0, so nothing is. A
    /// plain write journals nothing, and never reaches an entry that predates
    /// an open probe: only the unifier overwrites one.
    inline fn journalsWrite(self: *const Self, comptime family: WriteFamily, index: u32, baseline: u32) bool {
        switch (family) {
            .unifier => {
                if (std.debug.runtime_safety and self.probe_open and !self.unifier_running) {
                    base.invariant("a unifier write ran outside the unifier while a probe was open", .{});
                }
                return index < baseline;
            },
            .plain => {
                if (std.debug.runtime_safety and self.probe_open and index < baseline) {
                    base.invariant("a plain write reached a type-store entry an open probe may restore", .{});
                }
                return false;
            },
        }
    }

    /// In-place slot write. A unifier write to a slot that predates the open
    /// probe journals its previous value so discarding the probe can restore
    /// it; a failed journal append is propagated rather than risk a type
    /// store the log can no longer faithfully undo.
    fn setSlot(self: *Self, comptime family: WriteFamily, idx: SlotStore.Idx, val: Slot) Allocator.Error!void {
        self.debugAssertUnsealed();
        if (self.journalsWrite(family, @backingInt(idx), self.probe_baseline_slots)) {
            try self.undo_log.append(self.gpa, .{ .slot = .{ .idx = idx, .old = self.slots.get(idx) } });
        }
        switch (self.slots.get(idx)) {
            .root => |desc_idx| {
                self.class_watch.noteWrite(desc_idx);
                // The slot's vars stop resolving to a settled descriptor.
                if (!self.relinking_compared_classes and !std.meta.eql(val, Slot{ .root = desc_idx })) {
                    if (self.settled_descs.contains(desc_idx)) self.settled_descs.clear();
                    if (self.function_free_descs.contains(desc_idx)) self.function_free_descs.clear();
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
    inline fn setDesc(self: *Self, comptime family: WriteFamily, idx: DescStore.Idx, val: Desc) Allocator.Error!void {
        self.debugAssertUnfrozenWrite(idx);
        try self.setDescUnguarded(family, idx, val);
    }

    fn debugAssertUnfrozenWrite(self: *const Self, idx: DescStore.Idx) void {
        if (std.debug.runtime_safety and self.descs.getWord(idx).flags().frozen) {
            base.invariant("a frozen type class was written in place", .{});
        }
    }

    /// `setDesc` without the frozen-class guard, for freezing and thawing.
    inline fn setDescUnguarded(self: *Self, comptime family: WriteFamily, idx: DescStore.Idx, val: Desc) Allocator.Error!void {
        try self.setDescRowUnguarded(family, idx, try self.encodeDesc(val));
    }

    /// `setDesc` for a descriptor already encoded.
    fn setDescRow(self: *Self, comptime family: WriteFamily, idx: DescStore.Idx, row: Desc.Row) Allocator.Error!void {
        self.debugAssertUnfrozenWrite(idx);
        try self.setDescRowUnguarded(family, idx, row);
    }

    fn setDescRowUnguarded(self: *Self, comptime family: WriteFamily, idx: DescStore.Idx, row: Desc.Row) Allocator.Error!void {
        // A whole row replaces the word, so it must not replace one a
        // generalization pass has marked.
        std.debug.assert(!self.descs.getWord(idx).marked());
        try self.journalDescWrite(family, idx);
        if ((self.settled_descs.contains(idx) or self.function_free_descs.contains(idx)) and
            !DescStore.sameContent(self.descs.getRow(idx), row))
        {
            if (self.settled_descs.contains(idx)) self.settled_descs.clear();
            if (self.function_free_descs.contains(idx)) self.function_free_descs.clear();
        }
        self.descs.setRow(idx, row);
    }

    /// In-place write of a descriptor's word alone: its flags or rank, never
    /// its kind, so its content stays as it was. Journaled and watched like
    /// every descriptor write (see setSlot).
    inline fn setDescWord(self: *Self, comptime family: WriteFamily, idx: DescStore.Idx, word: Desc.Word) Allocator.Error!void {
        self.debugAssertUnfrozenWrite(idx);
        try self.setDescWordUnguarded(family, idx, word);
    }

    /// `setDescWord` without the frozen-class guard, for freezing and thawing.
    inline fn setDescWordUnguarded(self: *Self, comptime family: WriteFamily, idx: DescStore.Idx, word: Desc.Word) Allocator.Error!void {
        std.debug.assert(word.kind == self.descs.getWord(idx).kind and word.literal() == self.descs.getWord(idx).literal());
        try self.journalDescWrite(family, idx);
        self.descs.setWord(idx, word);
    }

    /// Before an in-place write to descriptor `idx`: journal its row when a
    /// unifier write reaches a descriptor that predates the open probe, and
    /// note the write for class watches.
    inline fn journalDescWrite(self: *Self, comptime family: WriteFamily, idx: DescStore.Idx) Allocator.Error!void {
        self.debugAssertUnsealed();
        if (self.journalsWrite(family, @backingInt(idx), self.probe_baseline_descs)) try self.journalDescRow(idx);
        self.class_watch.noteWrite(idx);
    }

    /// Journal descriptor `idx`'s row without its generalization marks: a
    /// rollback runs outside any generalization pass, where no descriptor is
    /// marked, so the row it restores is never marked either.
    fn journalDescRow(self: *Self, idx: DescStore.Idx) Allocator.Error!void {
        var old = self.descs.getRow(idx);
        old.word.generalizing = false;
        old.word.rank_adjusted = false;
        try self.undo_log.append(self.gpa, .{ .desc = .{ .idx = idx, .old = old } });
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

    /// Record the class of each of `vars` as reaching no function type. The
    /// caller guarantees that every class reachable from these classes is
    /// among them or already recorded.
    pub fn markFunctionFree(self: *Self, vars: []const Var) Allocator.Error!void {
        self.debugAssertUnsealed();
        for (vars) |var_| try self.function_free_descs.insert(self.gpa, self.resolveVar(var_).desc_idx);
    }

    /// Whether a checking scan recorded `var_`'s class as reaching no
    /// function type (see `markFunctionFree`). Exact while recorded: any
    /// change to a recorded class's observable content forgets every record.
    pub fn isFunctionFree(self: *const Self, var_: Var) bool {
        return self.function_free_descs.contains(self.resolveVar(var_).desc_idx);
    }

    /// In-place equivalence-class root metadata write. See setSlot.
    fn setRootMeta(self: *Self, comptime family: WriteFamily, idx: DescStore.Idx, val: RootMeta) Allocator.Error!void {
        self.debugAssertUnsealed();
        if (self.journalsWrite(family, @backingInt(idx), self.probe_baseline_descs)) {
            try self.undo_log.append(self.gpa, .{ .root_meta = .{ .idx = idx, .old = self.getRootMeta(idx) } });
        }
        self.class_watch.noteWrite(idx);
        self.root_metas.set(rootMetaIdx(idx), val);
    }

    fn setUnionRank(self: *Self, comptime family: WriteFamily, storage_var: Var, rank: u8) Allocator.Error!void {
        self.debugAssertUnsealed();
        const slot_idx = Self.varToSlotIdx(storage_var);
        if (self.journalsWrite(family, @backingInt(slot_idx), self.probe_baseline_slots)) {
            try self.undo_log.append(self.gpa, .{ .union_rank = .{ .idx = slot_idx, .old = self.getUnionRank(storage_var) } });
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
            // An open probe sets the free list aside, so a reissued
            // descriptor is never one a probe may restore.
            try self.setDescRowUnguarded(.plain, desc_idx, row);
            try self.setRootMeta(.plain, desc_idx, .{ .checked_var = checked_var });
            return desc_idx;
        }
        const next_len = @as(usize, self.descs.backing.len()) + 1;
        try self.descs.backing.ensureTotalCapacity(self.gpa, next_len);
        try self.root_metas.ensureTotalCapacity(self.gpa, next_len);
        return self.appendNewClassRowAssumeCapacity(row, checked_var);
    }

    /// `appendClassRow` into descriptor and root metadata room reserved
    /// earlier.
    inline fn appendClassRowAssumeCapacity(self: *Self, row: Desc.Row, checked_var: Var) Allocator.Error!DescStore.Idx {
        if (self.takeFreeDesc()) |desc_idx| {
            try self.setDescRowUnguarded(.plain, desc_idx, row);
            try self.setRootMeta(.plain, desc_idx, .{ .checked_var = checked_var });
            return desc_idx;
        }
        return self.appendNewClassRowAssumeCapacity(row, checked_var);
    }

    inline fn appendNewClassRowAssumeCapacity(self: *Self, row: Desc.Row, checked_var: Var) DescStore.Idx {
        const desc_idx = self.descs.appendRowAssumeCapacity(row);
        const meta_idx = self.root_metas.appendAssumeCapacity(.{
            .checked_var = checked_var,
        });
        std.debug.assert(@backingInt(meta_idx) == @backingInt(desc_idx));
        return desc_idx;
    }

    /// The most recently freed descriptor not yet reissued, if any. An open
    /// probe has set the free list aside, so it reissues none.
    fn takeFreeDesc(self: *Self) ?DescStore.Idx {
        return self.free_descs.pop();
    }

    /// Free the descriptor `former_root` named until the write that orphaned
    /// it: no slot names it any more, so the next class created may take it.
    /// A free while a probe is open waits for the probe to be kept, since
    /// discarding it would restore `former_root`'s slot.
    fn freeDesc(self: *Self, comptime family: WriteFamily, desc_idx: DescStore.Idx, former_root: Var) Allocator.Error!void {
        self.debugAssertUnsealed();
        std.debug.assert(!std.meta.eql(self.slots.get(Self.varToSlotIdx(former_root)), Slot{ .root = desc_idx }));
        switch (family) {
            // Discarding the probe restores the slots that named it, so the
            // free waits for the probe to be kept.
            .unifier => if (self.probe_open) {
                try self.pending_free_descs.append(self.gpa, desc_idx);
                return;
            },
            .plain => std.debug.assert(!self.probe_open),
        }
        try self.free_descs.ensureUnusedCapacity(self.gpa, 1);
        self.recycleDesc(desc_idx);
    }

    /// Put a dead descriptor on the free list, forgetting everything the
    /// store itself remembers about it by index: a settled mark, a watch,
    /// a frozen anchor, a derived map plan, and a tag tail closing. The caller has reserved the
    /// list's room.
    fn recycleDesc(self: *Self, desc_idx: DescStore.Idx) void {
        std.debug.assert(!self.probe_open);
        const index: u32 = @backingInt(desc_idx);
        if (index < self.settled_descs.bits.bit_length) self.settled_descs.bits.unset(index);
        if (index < self.function_free_descs.bits.bit_length) self.function_free_descs.bits.unset(index);
        if (index < self.class_watch.watched.bit_length) self.class_watch.watched.unset(index);
        if (self.descs.getWord(desc_idx).flags().frozen) _ = self.frozen_anchors.remove(desc_idx);
        const plan_slot = self.derivedMapPlanSlot(desc_idx);
        if (plan_slot.found) _ = self.derived_map_plans.orderedRemove(plan_slot.index);
        const closing_slot = self.tagTailClosingSlot(desc_idx);
        if (closing_slot.found) _ = self.tag_tail_closings.orderedRemove(closing_slot.index);
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

    /// A write of `family` to the plan row of `class` journals it (in the
    /// undo log) when the class predates the open probe.
    fn putClassDerivedMapPlan(self: *Self, comptime family: WriteFamily, class: DescStore.Idx, plan: DerivedMapPlan) Allocator.Error!void {
        self.debugAssertUnsealed();
        const slot = self.derivedMapPlanSlot(class);
        if (slot.found and std.meta.eql(self.derived_map_plans.items()[slot.index].plan, plan)) return;
        const journals = self.journalsWrite(family, @backingInt(class), self.probe_baseline_descs);
        if (journals) try self.undo_log.ensureUnusedCapacity(self.gpa, 1);
        const index: u32 = @intCast(slot.index);
        if (slot.found) {
            if (journals) self.undo_log.appendAssumeCapacity(.{ .plan_replaced = .{ .index = index, .old = self.derived_map_plans.items()[slot.index].plan } });
            self.derived_map_plans.items()[slot.index].plan = plan;
        } else {
            try self.derived_map_plans.insert(self.gpa, slot.index, .{ .class = class, .plan = plan });
            if (journals) self.undo_log.appendAssumeCapacity(.{ .plan_inserted = index });
        }
    }

    fn removeClassDerivedMapPlan(self: *Self, comptime family: WriteFamily, class: DescStore.Idx) Allocator.Error!void {
        self.debugAssertUnsealed();
        const slot = self.derivedMapPlanSlot(class);
        if (!slot.found) return;
        if (self.journalsWrite(family, @backingInt(class), self.probe_baseline_descs)) {
            try self.undo_log.append(self.gpa, .{ .plan_removed = .{ .index = @intCast(slot.index), .entry = self.derived_map_plans.items()[slot.index] } });
        }
        _ = self.derived_map_plans.orderedRemove(slot.index);
    }

    /// Give the class `to` the plan of the class `from`, when it has one: a
    /// member detached from a class stays a member of the relation it
    /// joined.
    fn inheritClassDerivedMapPlan(self: *Self, from: DescStore.Idx, to: DescStore.Idx) Allocator.Error!void {
        const plan = self.classDerivedMapPlan(from) orelse return;
        try self.putClassDerivedMapPlan(.plain, to, plan);
    }

    /// Two classes merge into `destination`, one of them: the merged class
    /// keeps `b`'s plan, or else `a`'s.
    fn mergeClassDerivedMapPlans(self: *Self, comptime family: WriteFamily, a: DescStore.Idx, b: DescStore.Idx, destination: DescStore.Idx) Allocator.Error!void {
        std.debug.assert(destination == a or destination == b);
        if (a == b) return;
        const merged = self.classDerivedMapPlan(b) orelse self.classDerivedMapPlan(a) orelse return;
        try self.removeClassDerivedMapPlan(family, if (destination == a) b else a);
        try self.putClassDerivedMapPlan(family, destination, merged);
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
        try self.putClassDerivedMapPlan(.plain, self.resolveVar(fn_var).desc_idx, plan);
    }

    // tag tail closings //

    const TagTailClosingSlot = struct { index: usize, found: bool };

    /// Where `class`'s row in `tag_tail_closings` is, or would be inserted.
    fn tagTailClosingSlot(self: *const Self, class: DescStore.Idx) TagTailClosingSlot {
        const entries = self.tag_tail_closings.items;
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

    fn classTagTailClosing(self: *const Self, class: DescStore.Idx) ?TagTailClosing {
        const slot = self.tagTailClosingSlot(class);
        if (!slot.found) return null;
        return self.tag_tail_closings.items[slot.index].closing;
    }

    /// A write of `family` to the closing row of `class` journals it (in
    /// the undo log) when the class predates the open probe.
    fn putClassTagTailClosing(self: *Self, comptime family: WriteFamily, class: DescStore.Idx, closing: TagTailClosing) Allocator.Error!void {
        self.debugAssertUnsealed();
        const slot = self.tagTailClosingSlot(class);
        if (slot.found and self.tag_tail_closings.items[slot.index].closing == closing) return;
        const journals = self.journalsWrite(family, @backingInt(class), self.probe_baseline_descs);
        if (journals) try self.undo_log.ensureUnusedCapacity(self.gpa, 1);
        const index: u32 = @intCast(slot.index);
        if (slot.found) {
            if (journals) self.undo_log.appendAssumeCapacity(.{ .closing_replaced = .{ .index = index, .old = self.tag_tail_closings.items[slot.index].closing } });
            self.tag_tail_closings.items[slot.index].closing = closing;
        } else {
            try self.tag_tail_closings.insert(self.gpa, slot.index, .{ .class = class, .closing = closing });
            if (journals) self.undo_log.appendAssumeCapacity(.{ .closing_inserted = index });
        }
    }

    fn removeClassTagTailClosing(self: *Self, comptime family: WriteFamily, class: DescStore.Idx) Allocator.Error!void {
        self.debugAssertUnsealed();
        const slot = self.tagTailClosingSlot(class);
        if (!slot.found) return;
        if (self.journalsWrite(family, @backingInt(class), self.probe_baseline_descs)) {
            try self.undo_log.append(self.gpa, .{ .closing_removed = .{ .index = @intCast(slot.index), .entry = self.tag_tail_closings.items[slot.index] } });
        }
        _ = self.tag_tail_closings.orderedRemove(slot.index);
    }

    /// Give the class `to` the closing of the class `from`, when it has one.
    fn inheritClassTagTailClosing(self: *Self, from: DescStore.Idx, to: DescStore.Idx) Allocator.Error!void {
        const closing = self.classTagTailClosing(from) orelse return;
        try self.putClassTagTailClosing(.plain, to, closing);
    }

    /// Two classes merge into `destination`, one of them: the merged class
    /// keeps `b`'s closing, or else `a`'s.
    fn mergeClassTagTailClosings(self: *Self, comptime family: WriteFamily, a: DescStore.Idx, b: DescStore.Idx, destination: DescStore.Idx) Allocator.Error!void {
        std.debug.assert(destination == a or destination == b);
        if (a == b) return;
        const merged = self.classTagTailClosing(b) orelse self.classTagTailClosing(a) orelse return;
        try self.removeClassTagTailClosing(family, if (destination == a) b else a);
        try self.putClassTagTailClosing(family, destination, merged);
    }

    /// The checking step that closed the uncommitted tag tail `tail_var` is,
    /// when a match closed it and it is still uncommitted: a merge with a
    /// committed empty tag union takes the mark away, and with it the
    /// match's say over the row.
    pub fn tagTailClosing(self: *const Self, tail_var: Var) ?TagTailClosing {
        if (self.tag_tail_closings.items.len == 0) return null;
        const resolved = self.resolveVar(tail_var);
        if (!resolved.desc.flags.uncommitted_tag_tail) return null;
        return self.classTagTailClosing(resolved.desc_idx);
    }

    /// Record which checking step closed the uncommitted tag tail class of
    /// `tail_var`.
    pub fn setTagTailClosing(self: *Self, tail_var: Var, closing: TagTailClosing) Allocator.Error!void {
        const resolved = self.resolveVar(tail_var);
        std.debug.assert(resolved.desc.flags.uncommitted_tag_tail);
        try self.putClassTagTailClosing(.plain, resolved.desc_idx, closing);
    }

    /// A copy of the uncommitted tag tail `source` carries its closing.
    pub fn carryTagTailClosing(self: *Self, source: Var, copy: Var) Allocator.Error!void {
        if (self.tag_tail_closings.items.len == 0) return;
        const closing = self.classTagTailClosing(self.resolveVar(source).desc_idx) orelse return;
        try self.putClassTagTailClosing(.plain, self.resolveVar(copy).desc_idx, closing);
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

    /// The constraints of descriptor `desc_idx` when it is a flex or a
    /// rigid; none for any other kind.
    pub inline fn variableConstraintsAt(self: *const Self, desc_idx: DescStore.Idx) StaticDispatchConstraint.SafeList.Range {
        return Desc.variableConstraints(self.descs.getRow(desc_idx));
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

    /// The stored row of descriptor `desc_idx`: its word and its content's
    /// payload, undecoded.
    pub inline fn rowAt(self: *const Self, desc_idx: DescStore.Idx) Desc.Row {
        return self.descs.getRow(desc_idx);
    }

    /// The identity row a nominal descriptor names: two descriptors naming
    /// the same row have the same identity.
    pub inline fn nominalIdentityRowAt(self: *const Self, desc_idx: DescStore.Idx) u32 {
        const row = self.descs.getRow(desc_idx);
        std.debug.assert(row.word.kind == .nominal_type);
        return row.payload[0];
    }

    /// The identity row an alias descriptor names (`nominalIdentityRowAt`).
    pub inline fn aliasIdentityRowAt(self: *const Self, desc_idx: DescStore.Idx) u32 {
        const row = self.descs.getRow(desc_idx);
        std.debug.assert(row.word.kind == .alias);
        return row.payload[0];
    }

    /// A tag-union descriptor's own range holds its tags in name order. A
    /// label may repeat only where a copy flattens a row whose extension chain
    /// repeats it (design.md "Row Union Normalization"), so equal neighbors
    /// are in order. Checked where a descriptor is encoded, while
    /// `debug_idents` is known.
    pub fn debugAssertTagsInNameOrder(self: *const Self, range: TagSafeMultiList.Range) void {
        if (!std.debug.runtime_safety) return;
        const idents = self.debug_idents orelse return;
        if (range.count < 2) return;
        const names = self.tags.sliceRange(range).items(.name);
        for (names[0 .. names.len - 1], names[1..]) |prev, next| {
            if (base.Ident.textOrder(idents.getText(prev), idents.getText(next)) == .gt) {
                base.invariant("a tag union's own tags are not in name order: {s} before {s}", .{ idents.getText(prev), idents.getText(next) });
            }
        }
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
                /// The row, absent for an empty entry.
                row: collections.OptionalU32 = .none,
            };

            inline fn slot(identity: Identity) usize {
                return (@as(u32, @bitCast(identity.ident)) *% 0x9E3779B1) >> (32 - @ctz(@as(u32, entry_count)));
            }

            inline fn get(self: *const @This(), identity: Identity) ?u32 {
                const entry = &self.entries[slot(identity)];
                const row = entry.row.get() orelse return null;
                if (!std.mem.eql(u8, std.mem.asBytes(&entry.identity), std.mem.asBytes(&identity))) return null;
                return row;
            }

            fn put(self: *@This(), identity: Identity, row: u32) void {
                self.entries[slot(identity)] = .{ .identity = identity, .row = .some(row) };
            }
        };
    }

    /// The row holding `identity`, appending one when no row does. No two
    /// rows hold the same identity, so a descriptor copied within the store
    /// may keep its source's row (`fillFreshPlaceholderWithIdentity`).
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
            std.debug.assert(!entry.found_existing);
            entry.value_ptr.* = indexed.*;
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

    /// Create one new variable per row of `rows`, in order, each holding its
    /// row, a descriptor already encoded for this store, as `register` does
    /// for each in turn (`instantiate.BackingOpenings` replays the
    /// descriptors an opening of a nominal declaration's backing minted).
    pub fn registerEncodedRows(self: *Self, rows: []const Desc.Row) std.mem.Allocator.Error!void {
        self.debugAssertUnsealed();
        try self.slots.ensureUnusedCapacity(self.gpa, rows.len);
        try self.union_ranks.ensureUnusedCapacity(self.gpa, rows.len);
        const descs_len = @as(usize, self.descs.backing.len()) + rows.len;
        try self.descs.backing.ensureTotalCapacity(self.gpa, descs_len);
        try self.root_metas.ensureTotalCapacity(self.gpa, descs_len);
        for (rows) |row| {
            const slot_idx: SlotStore.Idx = @fromBackingInt(@intCast(@as(u32, @intCast(self.slots.len()))));
            const desc_idx = try self.appendClassRowAssumeCapacity(row, Self.slotIdxToVar(slot_idx));
            const inserted_slot_idx = self.slots.appendAssumeCapacity(.{ .root = desc_idx });
            const rank_idx = self.union_ranks.appendAssumeCapacity(0);
            std.debug.assert(inserted_slot_idx == slot_idx);
            std.debug.assert(@backingInt(rank_idx) == @backingInt(slot_idx));
        }
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
        std.debug.assert(!self.probe_open);
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
        word.setFlag(.frozen, true);
        try self.frozen_anchors.put(storage.desc_idx, var_);
        try self.setDescWordUnguarded(.plain, storage.desc_idx, word);
    }

    /// End every class's freeze; afterwards the store holds no frozen class.
    pub fn thawFrozenClasses(self: *Self) Allocator.Error!void {
        self.debugAssertUnsealed();
        var anchors = self.frozen_anchors.constIterator();
        while (anchors.next()) |entry| {
            const idx = entry.key_ptr.*;
            var word = self.descs.getWord(idx);
            word.setFlag(.frozen, false);
            try self.setDescWordUnguarded(.plain, idx, word);
        }
        self.frozen_anchors.clearRetainingCapacity();
    }

    /// When `target_var` is a member of a frozen class, detach it into a class
    /// of its own holding the same type, so a write aimed at it changes it
    /// alone. A class whose checked representative is `target_var` is first
    /// handed back to its anchor. Returns false when `target_var` is the
    /// anchor itself, which no write may change.
    inline fn isolateFromFrozenClass(self: *Self, comptime family: WriteFamily, target_var: Var) Allocator.Error!bool {
        if (self.frozen_anchors.count() == 0) return true;
        return self.isolateFromFrozenClassSlow(family, target_var);
    }

    fn isolateFromFrozenClassSlow(self: *Self, comptime family: WriteFamily, target_var: Var) Allocator.Error!bool {
        var class = self.resolveStorageRoot(target_var);
        if (!class.desc.flags.frozen) return true;
        if (family == .unifier and self.probe_open) unreachable; // no relation a probe runs reaches a frozen class
        const anchor = self.frozen_anchors.get(class.desc_idx) orelse
            base.invariant("a frozen type class has no anchor", .{});
        if (target_var == anchor) return false;
        if (class.meta.checked_var == target_var) {
            try self.setRootMeta(family, class.desc_idx, .{ .checked_var = anchor });
            class.meta.checked_var = anchor;
        }
        try self.detachOccurrence(class, target_var);
        var desc = class.desc;
        desc.flags.frozen = false;
        const desc_idx = try self.appendClass(desc, target_var);
        try self.inheritClassDerivedMapPlan(class.desc_idx, desc_idx);
        try self.inheritClassTagTailClosing(class.desc_idx, desc_idx);
        try self.setSlot(family, Self.varToSlotIdx(target_var), .{ .root = desc_idx });
        return true;
    }

    /// `target_var` resolved for a write aimed at it, after
    /// `isolateWriteTarget` when its class is frozen.
    inline fn resolveWriteTarget(self: *Self, comptime family: WriteFamily, target_var: Var) Allocator.Error!ResolvedVarDesc {
        const resolved = self.resolveVar(target_var);
        if (!resolved.desc.flags.frozen) return resolved;
        return self.resolveIsolatedWriteTarget(family, target_var);
    }

    fn resolveIsolatedWriteTarget(self: *Self, comptime family: WriteFamily, target_var: Var) Allocator.Error!ResolvedVarDesc {
        try self.isolateWriteTarget(family, target_var);
        return self.resolveVar(target_var);
    }

    /// `isolateFromFrozenClass` for a write that must not reach an anchor.
    fn isolateWriteTarget(self: *Self, comptime family: WriteFamily, target_var: Var) Allocator.Error!void {
        if (!try self.isolateFromFrozenClass(family, target_var)) {
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
        try self.isolateWriteTarget(.plain, target_var);
        const storage = self.resolveStorageRoot(target_var);
        const desc_idx = try self.appendClass(.{
            .content = .{ .flex = Flex.init() },
            .rank = rank,
        }, target_var);
        if (target_var != storage.storage_var) {
            // `target_var` may be the old checked representative, or its
            // detached subtree may contain that representative. Keep the
            // remainder self-contained by selecting its storage root.
            try self.setRootMeta(.plain, storage.desc_idx, .{
                .checked_var = storage.storage_var,
            });
        }
        try self.setSlot(.plain, Self.varToSlotIdx(target_var), .{ .root = desc_idx });
        // A storage root takes its whole subtree to the fresh flex, so its
        // old descriptor is orphaned; a redirect leaves the remainder on it.
        if (target_var == storage.storage_var) try self.freeDesc(.plain, storage.desc_idx, storage.storage_var);
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
        const resolved = try self.resolveWriteTarget(.plain, target_var);
        self.noteContentRewrite(resolved.desc.content);
        try self.setDesc(.plain, resolved.desc_idx, desc);
    }

    /// Give `fresh_var` its descriptor: a variable this store minted as an
    /// unbound flex placeholder that nothing has unified, written once the
    /// content it stands for is complete. Construction, not a rewrite of the
    /// solved graph: the placeholder is its own unfrozen root, so the write
    /// reaches exactly its descriptor, as `dangerousSetVarDesc` would.
    pub inline fn fillFreshPlaceholder(self: *Self, fresh_var: Var, desc: Desc) Allocator.Error!void {
        self.debugAssertUnsealed();
        const desc_idx = self.resolveDescIdx(fresh_var);
        if (std.debug.runtime_safety) {
            const word = self.descs.getWord(desc_idx);
            std.debug.assert(word.kind == .flex and !word.flags().frozen);
            std.debug.assert(self.getRootMeta(desc_idx).checked_var == fresh_var);
        }
        try self.setDesc(.plain, desc_idx, desc);
    }

    /// `fillFreshPlaceholder` for a nominal type or alias whose identity is
    /// row `identity` of this store, as the descriptor it was copied from
    /// names it. Identity rows are unique (`internIdentity`), so `identity`
    /// is the row interning `desc`'s identity finds.
    pub inline fn fillFreshPlaceholderWithIdentity(self: *Self, fresh_var: Var, desc: Desc, identity: u32) Allocator.Error!void {
        self.debugAssertUnsealed();
        const desc_idx = self.resolveDescIdx(fresh_var);
        if (std.debug.runtime_safety) {
            const word = self.descs.getWord(desc_idx);
            std.debug.assert(word.kind == .flex and !word.flags().frozen);
            std.debug.assert(self.getRootMeta(desc_idx).checked_var == fresh_var);
            switch (desc.content) {
                .alias => |alias| std.debug.assert(std.meta.eql(self.aliasIdentity(identity), AliasIdentity.of(alias))),
                .structure => |flat| switch (flat) {
                    .nominal_type => |nominal| std.debug.assert(std.meta.eql(self.nominalIdentity(identity), NominalIdentity.of(nominal))),
                    .record, .tuple, .fn_pure, .fn_effectful, .fn_unbound, .empty_record, .tag_union, .empty_tag_union => unreachable, // only a nominal type or alias names an identity
                },
                .flex, .rigid, .field_presence, .err => unreachable, // only a nominal type or alias names an identity
            }
        }
        try self.setDescRow(.plain, desc_idx, desc.encode(identity, self));
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

    /// Advance the ground epoch by `count` writes (`instantiate.BackingOpenings`
    /// replays the advance an opening of a nominal declaration's backing made).
    pub fn advanceGroundEpoch(self: *Self, count: u64) void {
        self.debugAssertUnsealed();
        self.ground_epoch +%= count;
    }

    /// Set a type variable to the provided content
    pub fn setVarContent(self: *Self, target_var: Var, content: Content) Allocator.Error!void {
        return self.setVarContentWith(.plain, target_var, content);
    }

    /// `setVarContent` written by the unifier (see `WriteFamily`).
    pub fn unifierSetVarContent(self: *Self, target_var: Var, content: Content) Allocator.Error!void {
        return self.setVarContentWith(.unifier, target_var, content);
    }

    fn setVarContentWith(self: *Self, comptime family: WriteFamily, target_var: Var, content: Content) Allocator.Error!void {
        self.debugAssertUnsealed();
        std.debug.assert(@backingInt(target_var) < self.len());
        const resolved = try self.resolveWriteTarget(family, target_var);
        self.noteContentRewrite(resolved.desc.content);
        var desc = resolved.desc;
        desc.content = content;
        desc.flags = desc.flags.forKind(types.ContentKind.of(content));
        try self.setDesc(family, resolved.desc_idx, desc);
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
        try self.isolateWriteTarget(.plain, target_var);
        const desc_idx = self.resolveDescIdx(target_var);
        var word = self.descs.getWord(desc_idx);
        if (word.flags().static_dispatch_rejected) return false;
        word.setFlag(.static_dispatch_rejected, true);
        try self.setDescWord(.plain, desc_idx, word);
        return true;
    }

    /// Record that a defaulting decision is about to choose `target_var`'s
    /// equivalence class. Provenance only: content and links are untouched.
    pub fn markVarDefaultDecided(self: *Self, target_var: Var) Allocator.Error!void {
        self.debugAssertUnsealed();
        std.debug.assert(@backingInt(target_var) < self.len());
        const desc_idx = self.resolveDescIdx(target_var);
        var word = self.descs.getWord(desc_idx);
        if (word.flags().default_decided) return;
        word.setFlag(.default_decided, true);
        try self.setDescWord(.plain, desc_idx, word);
    }

    /// Withdraw a `markVarDefaultDecided` whose decision left the class
    /// undetermined: the default it anticipated never reached this class.
    pub fn clearVarDefaultDecided(self: *Self, target_var: Var) Allocator.Error!void {
        self.debugAssertUnsealed();
        std.debug.assert(@backingInt(target_var) < self.len());
        const desc_idx = self.resolveDescIdx(target_var);
        var word = self.descs.getWord(desc_idx);
        if (!word.flags().default_decided) return;
        word.setFlag(.default_decided, false);
        try self.setDescWord(.plain, desc_idx, word);
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
            word.setFlag(.nominal_backing_structure, true);
            try self.setDescWord(.plain, resolved.desc_idx, word);
        }
    }

    /// Record definition-site annotation openness (design.md "Derived Parser
    /// Tag-Row Closure"). Provenance travels with the flex equivalence class.
    pub fn markAnnotationTagExt(self: *Self, target_var: Var) Allocator.Error!void {
        self.debugAssertUnsealed();
        try self.isolateWriteTarget(.plain, target_var);
        const desc_idx = self.resolveDescIdx(target_var);
        var word = self.descs.getWord(desc_idx);
        std.debug.assert(word.kind == .flex);
        word.setFlag(.annotation_tag_ext, true);
        try self.setDescWord(.plain, desc_idx, word);
    }

    /// Mark a tag row a host-boundary annotation wrote (design.md "Row
    /// Joins"). The mark travels with the row's equivalence class and its
    /// copies.
    pub fn markHostFixedRow(self: *Self, target_var: Var) Allocator.Error!void {
        self.debugAssertUnsealed();
        const resolved = try self.resolveWriteTarget(.plain, target_var);
        var word = self.descs.getWord(resolved.desc_idx);
        word.setFlag(.host_fixed_row, true);
        try self.setDescWord(.plain, resolved.desc_idx, word);
    }

    /// Make a flex class an uncommitted record tail (design.md "Uncommitted
    /// Record Tails"), minted by a record pattern when `pattern` holds. The
    /// mark travels with the class and its copies.
    pub fn markUncommittedRecordTail(self: *Self, target_var: Var, pattern: bool) Allocator.Error!void {
        self.debugAssertUnsealed();
        const resolved = try self.resolveWriteTarget(.plain, target_var);
        std.debug.assert(resolved.desc.content == .flex);
        var desc = resolved.desc;
        desc.flags.uncommitted_record_tail = true;
        desc.flags.record_pattern_tail = pattern;
        try self.setDesc(.plain, resolved.desc_idx, desc);
    }

    /// Record that an empty record or tag union is the commitment of an
    /// uncommitted tail its binding compared or hashed (design.md "Methods On
    /// Open Rows").
    pub fn markComparedPlain(self: *Self, target_var: Var) Allocator.Error!void {
        self.debugAssertUnsealed();
        const resolved = try self.resolveWriteTarget(.plain, target_var);
        var desc = resolved.desc;
        desc.flags.compared_plain = true;
        try self.setDesc(.plain, resolved.desc_idx, desc);
    }

    /// Make a fresh empty tag union an uncommitted tag tail (design.md
    /// "Uncommitted Tag Tails").
    pub fn markUncommittedTagTail(self: *Self, target_var: Var) Allocator.Error!void {
        self.debugAssertUnsealed();
        const resolved = try self.resolveWriteTarget(.plain, target_var);
        std.debug.assert(resolved.desc.content == .structure and resolved.desc.content.structure == .empty_tag_union);
        var desc = resolved.desc;
        desc.flags.uncommitted_tag_tail = true;
        try self.setDesc(.plain, resolved.desc_idx, desc);
    }

    /// Commit an uncommitted tag tail (`DescriptorFlags.uncommitted_tag_tail`):
    /// the empty tag union it is stays, and no longer lifts or generalizes
    /// (design.md "Uncommitted Tag Tails").
    pub fn commitUncommittedTagTail(self: *Self, target_var: Var) Allocator.Error!void {
        self.debugAssertUnsealed();
        const resolved = try self.resolveWriteTarget(.plain, target_var);
        std.debug.assert(resolved.desc.content == .structure and resolved.desc.content.structure == .empty_tag_union);
        var desc = resolved.desc;
        desc.flags.uncommitted_tag_tail = false;
        desc.rank = .outermost;
        try self.setDesc(.plain, resolved.desc_idx, desc);
    }

    /// Mark a tag union an annotation or type declaration wrote (design.md
    /// "Uncommitted Record Tails"). The mark travels with the class and its
    /// copies.
    pub fn markAnnotationTagUnion(self: *Self, target_var: Var) Allocator.Error!void {
        self.debugAssertUnsealed();
        const resolved = try self.resolveWriteTarget(.plain, target_var);
        var desc = resolved.desc;
        desc.flags.annotation_tag_union = true;
        try self.setDesc(.plain, resolved.desc_idx, desc);
    }

    /// Bound an annotated definition's implicitly opened row (design.md
    /// "Polarity"). The bound travels with the row's equivalence class.
    pub fn markBoundedRowExt(self: *Self, target_var: Var) Allocator.Error!void {
        return self.markBoundedRowExtWith(.plain, target_var);
    }

    /// `markBoundedRowExt` written by the unifier (see `WriteFamily`).
    pub fn unifierMarkBoundedRowExt(self: *Self, target_var: Var) Allocator.Error!void {
        return self.markBoundedRowExtWith(.unifier, target_var);
    }

    fn markBoundedRowExtWith(self: *Self, comptime family: WriteFamily, target_var: Var) Allocator.Error!void {
        self.debugAssertUnsealed();
        const resolved = try self.resolveWriteTarget(family, target_var);
        var word = self.descs.getWord(resolved.desc_idx);
        word.setFlag(.bounded_row_ext, true);
        try self.setDescWord(family, resolved.desc_idx, word);
    }

    /// Record the kind of a variable used as an extension. Aliases are
    /// transparent; a known non-row is rejected by the annotation checker.
    pub fn requireTagRow(self: *Self, target_var: Var) Allocator.Error!void {
        return self.requireTagRowWith(.plain, target_var);
    }

    /// `requireTagRow` written by the unifier (see `WriteFamily`).
    pub fn unifierRequireTagRow(self: *Self, target_var: Var) Allocator.Error!void {
        return self.requireTagRowWith(.unifier, target_var);
    }

    fn requireTagRowWith(self: *Self, comptime family: WriteFamily, target_var: Var) Allocator.Error!void {
        var current = target_var;
        var remaining = self.len();
        while (remaining > 0) : (remaining -= 1) {
            const resolved = self.resolveVarWord(current);
            const flags = resolved.word.flags();
            if (flags.requires_tag_row) return;
            switch (resolved.word.kind) {
                .alias => current = self.aliasBackingVarAt(resolved.desc_idx),
                .rigid, .flex, .tag_union => {
                    if (flags.uncommitted_record_tail or flags.frozen) return;
                    var word = resolved.word;
                    word.setFlag(.requires_tag_row, true);
                    try self.setDescWord(family, resolved.desc_idx, word);
                    return;
                },
                .field_presence, .err, .record, .tuple, .nominal_type, .fn_pure, .fn_effectful, .fn_unbound, .empty_record, .empty_tag_union => return,
            }
        }
    }

    /// End the bound on a row: a platform-required definition's rows are
    /// released where the platform relates them, and a bound an enclosing
    /// release cleared is applied again for the owners that still hold it.
    pub fn clearBoundedRowExt(self: *Self, target_var: Var) Allocator.Error!void {
        self.debugAssertUnsealed();
        const resolved = try self.resolveWriteTarget(.plain, target_var);
        var word = self.descs.getWord(resolved.desc_idx);
        word.setFlag(.bounded_row_ext, false);
        try self.setDescWord(.plain, resolved.desc_idx, word);
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
        return self.dangerousSetVarRedirectWith(.plain, rule, target_var, redirect_to);
    }

    /// `dangerousSetVarRedirect` written by the unifier (see `WriteFamily`).
    pub fn unifierDangerousSetVarRedirect(self: *Self, comptime rule: RedirectRule, target_var: Var, redirect_to: Var) Allocator.Error!void {
        return self.dangerousSetVarRedirectWith(.unifier, rule, target_var, redirect_to);
    }

    fn dangerousSetVarRedirectWith(self: *Self, comptime family: WriteFamily, comptime rule: RedirectRule, target_var: Var, redirect_to: Var) Allocator.Error!void {
        self.debugAssertUnsealed();
        std.debug.assert(@backingInt(target_var) < self.len());
        std.debug.assert(@backingInt(redirect_to) < self.len());
        try self.isolateWriteTarget(family, target_var);
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
            family,
            target_storage,
            redirect_storage,
            redirect_storage.desc_idx,
            redirect_storage.meta.checked_var,
        );
        try self.freeDesc(family, target_storage.desc_idx, target_storage.storage_var);
    }

    // make builtin types //

    // make content types //

    /// Make a tag union data type
    /// Does not insert content into the types store
    pub fn mkTagUnion(self: *Self, tags: []const Tag, ext_var: Var) std.mem.Allocator.Error!Content {
        try self.requireTagRow(ext_var);
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
    /// `mkAliasWithPackedSourceDecl`, or copied from ranges they made by
    /// `appendVarsPoolEntries`. An empty `s` appends nothing and is
    /// `Range.empty()`, which a descriptor stores as
    /// `stored_empty_var_range`.
    pub fn appendVars(self: *Self, s: []const Var) std.mem.Allocator.Error!VarSafeList.Range {
        self.debugAssertUnsealed();
        if (s.len == 0) return .empty();
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

    /// Whether a vars-pool entry is the length slot `appendVars` writes
    /// before a range rather than a variable.
    pub fn isVarsPoolLengthSlot(entry: Var) bool {
        return isRangeLengthSlot(entry);
    }

    /// Append `entries` to the vars pool as they are, returning the index of
    /// the first. `entries` is a run of whole ranges as `appendVars` wrote
    /// them, each led by its length slot (`instantiate.BackingOpenings`
    /// replays the run an opening of a nominal declaration's backing
    /// appended).
    pub fn appendVarsPoolEntries(self: *Self, entries: []const Var) std.mem.Allocator.Error!u32 {
        self.debugAssertUnsealed();
        return @backingInt((try self.vars.appendSlice(self.gpa, entries)).start);
    }

    /// The number of entries in the vars pool.
    pub fn varsPoolLen(self: *const Self) u32 {
        return @intCast(self.vars.items.len);
    }

    /// What a stored descriptor keeps of `Range.empty()`. Every range
    /// `appendVars` appends starts after its length slot, so no such range
    /// starts at zero.
    pub const stored_empty_var_range: u32 = 0;

    /// What a stored descriptor keeps of the vars range `range`: its start,
    /// or `stored_empty_var_range` for `Range.empty()`. Every other range of
    /// a stored descriptor came from `appendVars`, so its length is in the
    /// slot before its start.
    pub inline fn storedVarRange(self: *const Self, range: VarSafeList.Range) u32 {
        if (@backingInt(range.start) == stored_empty_var_range) {
            std.debug.assert(range.count == 0);
            return stored_empty_var_range;
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
        if (stored == stored_empty_var_range) return .empty();
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
        return self.appendStaticDispatchConstraintsAfterWith(.plain, prefix, suffix);
    }

    /// `appendStaticDispatchConstraintsAfter` written by the unifier (see
    /// `WriteFamily`). An append while a probe is open neither uses nor
    /// reserves room, so discarding the probe, which truncates the list, leaves
    /// no room behind.
    pub fn unifierAppendStaticDispatchConstraintsAfter(
        self: *Self,
        prefix: StaticDispatchConstraint.SafeList.Range,
        suffix: []const StaticDispatchConstraint,
    ) std.mem.Allocator.Error!StaticDispatchConstraint.SafeList.Range {
        return self.appendStaticDispatchConstraintsAfterWith(.unifier, prefix, suffix);
    }

    fn appendStaticDispatchConstraintsAfterWith(
        self: *Self,
        comptime family: WriteFamily,
        prefix: StaticDispatchConstraint.SafeList.Range,
        suffix: []const StaticDispatchConstraint,
    ) std.mem.Allocator.Error!StaticDispatchConstraint.SafeList.Range {
        self.debugAssertUnsealed();
        if (family == .plain) std.debug.assert(!self.probe_open);
        const reserves_room = !self.probe_open;
        const list = &self.static_dispatch_constraints;
        const start: u32 = @backingInt(prefix.start);
        const end: u32 = start + @as(u32, @intCast(prefix.count));
        if (reserves_room) {
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
        const room = if (reserves_room and total >= min_constraint_room) total else 0;
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
        return .some(try self.interpolation_metadata.append(self.gpa, metadata));
    }

    /// The interpolation metadata of a constraint that has some.
    pub fn getInterpolationMetadata(self: *const Self, idx: InterpolationMetadata.OptIdx) InterpolationMetadata {
        return self.interpolation_metadata.get(idx.get().?).*;
    }

    /// Record a numeric literal conversion's facts, for a constraint's
    /// `numeral`.
    pub fn appendNumeralInfo(self: *Self, info: NumeralInfo) std.mem.Allocator.Error!NumeralInfo.OptIdx {
        self.debugAssertUnsealed();
        return .some(try self.numeral_infos.append(self.gpa, info));
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
    /// Must not run inside a probe: the declaration table is not journaled,
    /// so discarding the probe could not undo the registration.
    pub fn registerNominalDecl(self: *Self, decl: NominalDecl) Allocator.Error!NominalDecl.Idx {
        self.debugAssertUnsealed();
        std.debug.assert(!self.probe_open);
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
        if (entries.len == 0) return null;
        const key = NominalDeclIndexEntry.sortKey(origin_module, statement);
        // The last entry whose key is not past `key` is always among the
        // `span` entries from `first`. Each step selects a half instead of
        // branching on the comparison, whose outcome is unpredictable; the
        // number of steps depends on the entry count alone.
        var first: usize = 0;
        var span: usize = entries.len;
        while (span > 1) {
            const half = span / 2;
            const upper = entries[first + half];
            first = if (NominalDeclIndexEntry.sortKey(upper.origin_module, upper.statement) <= key) first + half else first;
            span -= half;
        }
        const found = entries[first];
        return if (NominalDeclIndexEntry.sortKey(found.origin_module, found.statement) == key) found.decl else null;
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
        std.debug.assert(!self.probe_open);
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
        std.debug.assert(!self.probe_open);
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

    /// Set the rank for a descriptor. Writes the descriptor's word alone.
    pub inline fn setDescRank(self: *Self, desc_idx: DescStore.Idx, rank: Rank) Allocator.Error!void {
        return self.setDescRankWith(.plain, desc_idx, rank);
    }

    /// `setDescRank` written by the unifier (see `WriteFamily`).
    pub inline fn unifierSetDescRank(self: *Self, desc_idx: DescStore.Idx, rank: Rank) Allocator.Error!void {
        return self.setDescRankWith(.unifier, desc_idx, rank);
    }

    inline fn setDescRankWith(self: *Self, comptime family: WriteFamily, desc_idx: DescStore.Idx, rank: Rank) Allocator.Error!void {
        self.debugAssertUnsealed();
        var word = self.descs.getWord(desc_idx);
        // A frozen class is ground, so no rank changes what it means.
        if (word.flags().frozen) return;
        word.rank = rank;
        try self.setDescWord(family, desc_idx, word);
    }

    // generalization marks //

    /// Set the `generalizing` mark of descriptor `desc_idx`
    /// (`types.Descriptor.Word`) when `condition` holds, and leave it as it
    /// is otherwise; the word is written either way, so nothing branches on
    /// `condition`. A mark is a generalization pass's scratch,
    /// which the pass clears before it returns, so a mark write is neither
    /// journaled nor reported to class watches: no probe opens or is discarded
    /// while a pass runs, and a journaled row never carries a mark
    /// (`journalDescRow`).
    pub fn markGeneralizingIf(self: *Self, desc_idx: DescStore.Idx, condition: bool) void {
        self.debugAssertUnsealed();
        var word = self.descs.getWord(desc_idx);
        word.generalizing = word.generalizing or condition;
        self.descs.setWord(desc_idx, word);
    }

    /// Set the `rank_adjusted` mark of descriptor `desc_idx`; see
    /// `markGeneralizingIf`.
    pub fn markRankAdjusted(self: *Self, desc_idx: DescStore.Idx) void {
        self.debugAssertUnsealed();
        var word = self.descs.getWord(desc_idx);
        word.rank_adjusted = true;
        self.descs.setWord(desc_idx, word);
    }

    /// Clear both generalization marks of descriptor `desc_idx`; see
    /// `markGeneralizingIf`.
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

    /// Whether descriptors `a` and `b` store the same content: the same kind
    /// with the same payload.
    pub fn sameStoredContent(self: *const Self, a: DescStore.Idx, b: DescStore.Idx) bool {
        return DescStore.sameContent(self.descs.getRow(a), self.descs.getRow(b));
    }

    /// `resolveVarWord`, compressing the storage path on the way.
    pub fn resolveVarWordAndCompressPath(self: *Self, var_: Var) ResolvedVarWord {
        return self.resolveVarWordAndCompressPathWith(.plain, var_);
    }

    /// `resolveVarWordAndCompressPath` for the unifier (see `WriteFamily`),
    /// which compresses no path while a probe is open.
    pub fn unifierResolveVarWord(self: *Self, var_: Var) ResolvedVarWord {
        return self.resolveVarWordAndCompressPathWith(.unifier, var_);
    }

    fn resolveVarWordAndCompressPathWith(self: *Self, comptime family: WriteFamily, var_: Var) ResolvedVarWord {
        const root = self.findStorageRootAndCompressPath(family, var_);
        return .{
            .var_ = root.meta.checked_var,
            .is_root = var_ == root.meta.checked_var,
            .desc_idx = root.desc_idx,
            .word = self.descs.getWord(root.desc_idx),
        };
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
        return self.descs.getWord(self.resolveDescIdx(var_)).flags();
    }

    /// The flags of descriptor `desc_idx`.
    pub fn flagsAt(self: *const Self, desc_idx: DescStore.Idx) types.DescriptorFlags {
        return self.descs.getWord(desc_idx).flags();
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
        return publicResolved(initial_var, self.withDesc(self.findStorageRootAndCompressPath(.plain, initial_var)));
    }

    /// `findStorageRoot`, compressing the storage path on the way.
    fn findStorageRootAndCompressPath(self: *Self, comptime family: WriteFamily, initial_var: Var) StorageRoot {
        self.debugAssertUnsealed();
        const storage = self.findStorageRoot(initial_var);

        // Compress the chain so future resolves are O(1), but not while a
        // probe is open, which a unifier resolve tells by the slot baseline
        // being nonzero: compression is a pure optimization (it never changes
        // what a var resolves to), so it would only be journaled and
        // discarded, and a compressed slot could come to name a variable the
        // probe appended. Skipping also keeps this resolver infallible (no
        // journaling, no allocation).
        const compresses = switch (family) {
            .plain => true,
            .unifier => self.probe_baseline_slots == 0,
        };
        if (compresses and initial_var != storage.storage_var) {
            if (family == .plain) std.debug.assert(!self.probe_open);
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
    pub inline fn aliasBackingVarAt(self: *const Self, desc_idx: DescStore.Idx) Var {
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
        comptime family: WriteFamily,
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
            try self.mergeClassDerivedMapPlans(family, a.desc_idx, b.desc_idx, destination_desc_idx);
        }
        if (self.tag_tail_closings.items.len != 0) {
            try self.mergeClassTagTailClosings(family, a.desc_idx, b.desc_idx, destination_desc_idx);
        }
        try self.setRootMeta(family, destination_desc_idx, .{
            .checked_var = checked_var,
        });
        try self.setUnionRank(family, parent.storage_var, combined_rank);
        try self.setSlot(family, Self.varToSlotIdx(parent.storage_var), .{ .root = destination_desc_idx });
        try self.setSlot(family, Self.varToSlotIdx(child.storage_var), .{ .redirect = parent.storage_var });
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
        return self.unionWith(.plain, a_var, b_var, new_desc);
    }

    /// `union_` written by the unifier (see `WriteFamily`).
    pub fn unifierUnion(self: *Self, a_var: Var, b_var: Var, new_desc: Desc) Allocator.Error!void {
        return self.unionWith(.unifier, a_var, b_var, new_desc);
    }

    fn unionWith(self: *Self, comptime family: WriteFamily, a_var: Var, b_var: Var, new_desc: Desc) Allocator.Error!void {
        self.debugAssertUnsealed();
        // Only the two classes' words are read: their kinds and flags.
        const a_data = self.findStorageRoot(a_var);
        const b_data = self.findStorageRoot(b_var);
        const a_word = self.descs.getWord(a_data.desc_idx);
        const b_word = self.descs.getWord(b_data.desc_idx);
        if (a_word.flags().frozen or b_word.flags().frozen) {
            return self.unionWithFrozen(family, a_data, a_word, b_data, b_word, new_desc.content == .err);
        }

        var merged_desc = new_desc;
        merged_desc.flags = mergedFlags(new_desc.flags, types.ContentKind.of(new_desc.content), a_word, b_word);
        return self.unionRows(family, a_data, a_word, b_data, b_word, try self.encodeDesc(merged_desc));
    }

    /// One of the two classes a merge relates.
    pub const MergeSide = enum { a, b };

    /// `union_` for a merge whose content is the content one of the two
    /// classes already stores: the merged class holds `kept`'s stored content
    /// as it stands, at `rank`, with the flags `union_` gives a merge whose
    /// descriptor states none. Neither class's content is decoded.
    pub fn unionKeepingContent(self: *Self, a_var: Var, b_var: Var, kept: MergeSide, rank: Rank) Allocator.Error!void {
        return self.unionKeepingContentWith(.plain, a_var, b_var, kept, rank);
    }

    /// `unionKeepingContent` written by the unifier (see `WriteFamily`).
    pub fn unifierUnionKeepingContent(self: *Self, a_var: Var, b_var: Var, kept: MergeSide, rank: Rank) Allocator.Error!void {
        return self.unionKeepingContentWith(.unifier, a_var, b_var, kept, rank);
    }

    fn unionKeepingContentWith(self: *Self, comptime family: WriteFamily, a_var: Var, b_var: Var, kept: MergeSide, rank: Rank) Allocator.Error!void {
        self.debugAssertUnsealed();
        const a_data = self.findStorageRoot(a_var);
        const b_data = self.findStorageRoot(b_var);
        const content = self.descs.getRow(switch (kept) {
            .a => a_data.desc_idx,
            .b => b_data.desc_idx,
        });
        return self.unionRootsStoringContent(family, a_data, b_data, content, rank);
    }

    /// `union_` for a merge whose content is content a class of this store
    /// held, as it was stored (`rowAt`): the merged class holds `content`'s
    /// kind and payload, at `rank`, with the flags `union_` gives a merge
    /// whose descriptor states none. `content`'s own flags and rank are not
    /// read, and nothing is decoded or encoded.
    pub fn unionStoringContent(self: *Self, a_var: Var, b_var: Var, content: Desc.Row, rank: Rank) Allocator.Error!void {
        return self.unionStoringContentWith(.plain, a_var, b_var, content, rank);
    }

    /// `unionStoringContent` written by the unifier (see `WriteFamily`).
    pub fn unifierUnionStoringContent(self: *Self, a_var: Var, b_var: Var, content: Desc.Row, rank: Rank) Allocator.Error!void {
        return self.unionStoringContentWith(.unifier, a_var, b_var, content, rank);
    }

    fn unionStoringContentWith(self: *Self, comptime family: WriteFamily, a_var: Var, b_var: Var, content: Desc.Row, rank: Rank) Allocator.Error!void {
        self.debugAssertUnsealed();
        return self.unionRootsStoringContent(family, self.findStorageRoot(a_var), self.findStorageRoot(b_var), content, rank);
    }

    fn unionRootsStoringContent(self: *Self, comptime family: WriteFamily, a_data: StorageRoot, b_data: StorageRoot, content: Desc.Row, rank: Rank) Allocator.Error!void {
        const a_word = self.descs.getWord(a_data.desc_idx);
        const b_word = self.descs.getWord(b_data.desc_idx);
        if (a_word.flags().frozen or b_word.flags().frozen) {
            return self.unionWithFrozen(family, a_data, a_word, b_data, b_word, content.word.kind == .err);
        }

        self.debugAssertStoredEncoding(content);
        // No generalization pass is running, so the stored word is unmarked.
        std.debug.assert(!content.word.marked());
        var merged_row = content;
        merged_row.word.setFlags(mergedFlags(.{}, content.word.kind, a_word, b_word));
        merged_row.word.rank = rank;
        return self.unionRows(family, a_data, a_word, b_data, b_word, merged_row);
    }

    /// A stored row is its content's encoding: the row `union_` would encode
    /// from the decoded content, naming the one row that holds its identity.
    fn debugAssertStoredEncoding(self: *const Self, row: Desc.Row) void {
        if (!std.debug.runtime_safety) return;
        const desc = Desc.decode(row, self);
        const identity: u32 = switch (desc.content) {
            .alias => |alias| self.alias_identity_index.get(.of(alias)) orelse row.payload[0],
            .structure => |flat| switch (flat) {
                .nominal_type => |nominal| self.nominal_identity_index.get(.of(nominal)) orelse row.payload[0],
                .record, .tuple, .fn_pure, .fn_effectful, .fn_unbound, .empty_record, .tag_union, .empty_tag_union => 0,
            },
            .flex, .rigid, .field_presence, .err => 0,
        };
        if (!DescStore.sameContent(desc.encode(identity, self), row)) {
            base.invariant("a stored descriptor row is not the encoding of its content", .{});
        }
    }

    /// The flags of the class two classes merge into, given the flags the
    /// merged descriptor states and the kind of content it holds.
    fn mergedFlags(
        stated: types.DescriptorFlags,
        merged_kind: types.ContentKind,
        a_word: Desc.Word,
        b_word: Desc.Word,
    ) types.DescriptorFlags {
        var flags = stated;
        const a_flags = a_word.flags();
        const b_flags = b_word.flags();
        flags.annotation_tag_ext = merged_kind == .flex and
            (a_flags.annotation_tag_ext or b_flags.annotation_tag_ext);
        flags.bounded_row_ext = a_flags.bounded_row_ext or b_flags.bounded_row_ext;
        flags.requires_tag_row = a_flags.requires_tag_row or b_flags.requires_tag_row;
        flags.host_fixed_row = a_flags.host_fixed_row or b_flags.host_fixed_row;
        // A rejected dispatch edge is a fact about the constraint callable's
        // equivalence class, so merging two classes rejects the result if
        // either side was rejected.
        flags.static_dispatch_rejected = a_flags.static_dispatch_rejected or
            b_flags.static_dispatch_rejected;
        // Declared backing structure stays declared: a class that merged with
        // an opened nominal backing component is that component.
        flags.nominal_backing_structure = a_flags.nominal_backing_structure or
            b_flags.nominal_backing_structure;
        // An uncommitted record tail, and the mark of a copy of one, describe
        // a flex class; a class that became anything else is no tail.
        flags.uncommitted_record_tail = merged_kind == .flex and
            (a_flags.uncommitted_record_tail or b_flags.uncommitted_record_tail);
        flags.uncommitted_copy = merged_kind == .flex and
            (a_flags.uncommitted_copy or b_flags.uncommitted_copy);
        // A record a pattern takes apart stays one only while no tail a
        // construction minted joins its class.
        const a_constructed = a_flags.uncommitted_record_tail and !a_flags.record_pattern_tail;
        const b_constructed = b_flags.uncommitted_record_tail and !b_flags.record_pattern_tail;
        // A row committed because its binding compared it as a plain one
        // stays one.
        flags.compared_plain = a_flags.compared_plain or b_flags.compared_plain;
        flags.record_pattern_tail = merged_kind == .flex and
            (a_flags.record_pattern_tail or b_flags.record_pattern_tail) and
            !a_constructed and !b_constructed;
        // Annotated structure stays annotated: a tag union that merged with
        // one an annotation wrote is that annotation's union.
        flags.annotation_tag_union = a_flags.annotation_tag_union or
            b_flags.annotation_tag_union;
        // An inference-closed empty tag union stays uncommitted unless it
        // meets one an annotation or a commitment closed.
        flags.uncommitted_tag_tail = merged_kind == .empty_tag_union and
            (a_flags.uncommitted_tag_tail or b_flags.uncommitted_tag_tail) and
            !committedEmptyTagUnion(a_word) and !committedEmptyTagUnion(b_word);
        // A defaulting decision about one side is a decision about the merged
        // class: the default it committed is the type both sides now share.
        flags.default_decided = a_flags.default_decided or
            b_flags.default_decided;
        return flags;
    }

    /// Merge two unfrozen classes into one that holds `merged_row`.
    fn unionRows(
        self: *Self,
        comptime family: WriteFamily,
        a_data: StorageRoot,
        a_word: Desc.Word,
        b_data: StorageRoot,
        b_word: Desc.Word,
        merged_row: Desc.Row,
    ) Allocator.Error!void {
        if ((kindIsLeaf(merged_row.word.kind) and
            (!kindIsLeaf(a_word.kind) or !kindIsLeaf(b_word.kind))) or
            !sameCopiedFlags(merged_row.word.flags(), a_word.flags()) or
            !sameCopiedFlags(merged_row.word.flags(), b_word.flags()))
        {
            self.ground_epoch +%= 1;
        }

        if (a_data.storage_var == b_data.storage_var) {
            try self.setDescRow(family, a_data.desc_idx, merged_row);
            return;
        }

        // a's vars come to see the merged descriptor. When its content is
        // what a settled side already had, that side's settledness carries
        // over to the merged class; otherwise a settled side has changed.
        const a_kept = DescStore.sameContent(self.descs.getRow(a_data.desc_idx), merged_row);
        const b_kept = DescStore.sameContent(self.descs.getRow(b_data.desc_idx), merged_row);
        const settled_merge = self.settled_descs.beginMerge(a_data.desc_idx, b_data.desc_idx, a_kept);
        const function_free_merge = self.function_free_descs.beginMerge(a_data.desc_idx, b_data.desc_idx, a_kept);

        // The unifier computes merged content for b's descriptor destination.
        // Keep that destination even when balancing retains a's storage root.
        try self.setDescRow(family, b_data.desc_idx, merged_row);
        self.relinking_compared_classes = true;
        defer self.relinking_compared_classes = false;
        try self.linkStorageRoots(family, a_data, b_data, b_data.desc_idx, b_data.meta.checked_var);
        try self.freeDesc(family, a_data.desc_idx, a_data.storage_var);
        try self.settled_descs.finishMerge(self.gpa, settled_merge, b_data.desc_idx, a_kept, b_kept);
        try self.function_free_descs.finishMerge(self.gpa, function_free_merge, b_data.desc_idx, a_kept, b_kept);
    }

    /// `union_` when either class is frozen. The merged class keeps the frozen
    /// descriptor, which already holds the type both sides agreed on, and `b`
    /// stays the checked representative as in every merge. Relating a frozen
    /// class to an error leaves the two classes apart: the error side is
    /// already poisoned, and the frozen side must not change.
    fn unionWithFrozen(
        self: *Self,
        comptime family: WriteFamily,
        a: StorageRoot,
        a_word: Desc.Word,
        b: StorageRoot,
        b_word: Desc.Word,
        merged_is_err: bool,
    ) Allocator.Error!void {
        if (a.storage_var == b.storage_var) return;
        if (merged_is_err) return;
        const frozen = if (b_word.flags().frozen) b else a;
        const other = if (b_word.flags().frozen) a else b;
        if (std.debug.runtime_safety) {
            const frozen_word = if (b_word.flags().frozen) b_word else a_word;
            const other_word = if (b_word.flags().frozen) a_word else b_word;
            if (other_word.flags().static_dispatch_rejected and !frozen_word.flags().static_dispatch_rejected) {
                base.invariant("a rejected dispatch callable was related to a frozen type class", .{});
            }
        }
        try self.linkStorageRoots(family, a, b, frozen.desc_idx, b.meta.checked_var);
        try self.freeDesc(family, other.desc_idx, other.storage_var);
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
        const a_open = try self.isolateFromFrozenClass(.plain, a_var);
        const b_open = try self.isolateFromFrozenClass(.plain, b_var);
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
            try self.setDesc(.plain, a.desc_idx, err_desc);
            return;
        }

        if (b_var == b.meta.checked_var) {
            try self.setDesc(.plain, b.desc_idx, err_desc);
        } else {
            try self.detachOccurrence(b, b_var);
            const err_desc_idx = try self.appendClass(err_desc, b_var);
            try self.setSlot(.plain, Self.varToSlotIdx(b_var), .{ .root = err_desc_idx });
            b = .{
                .storage_var = b_var,
                .desc_idx = err_desc_idx,
                .desc = err_desc,
                .meta = .{ .checked_var = b_var },
            };
        }

        if (a_var != a.meta.checked_var) {
            try self.detachOccurrence(a, a_var);
            try self.setSlot(.plain, Self.varToSlotIdx(a_var), .{ .root = b.desc_idx });
            a = .{
                .storage_var = a_var,
                .desc_idx = b.desc_idx,
                .desc = err_desc,
                .meta = .{ .checked_var = b.meta.checked_var },
            };
        }

        try self.linkStorageRoots(.plain, a, b, b.desc_idx, b.meta.checked_var);
        if (err_plan) |plan| try self.putClassDerivedMapPlan(.plain, b.desc_idx, plan);
        // Only a class that joined the error class whole loses its
        // descriptor; a detached occurrence already shares `b`'s.
        if (a.desc_idx != b.desc_idx) try self.freeDesc(.plain, a.desc_idx, a.storage_var);
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
        if (var_ == class.meta.checked_var) return self.setDesc(.plain, class.desc_idx, err_desc);
        try self.detachOccurrence(class, var_);
        const err_desc_idx = try self.appendClass(err_desc, var_);
        try self.inheritClassDerivedMapPlan(class.desc_idx, err_desc_idx);
        try self.setSlot(.plain, Self.varToSlotIdx(var_), .{ .root = err_desc_idx });
    }

    /// Detach `occurrence`, a member of `class` other than its checked
    /// representative, from the class. The remaining members are re-rooted and
    /// flattened at the checked representative and keep the class descriptor.
    /// `occurrence` is left as a rank-zero storage root whose slot the caller
    /// assigns.
    fn detachOccurrence(self: *Self, class: ResolvedStorageRoot, occurrence: Var) Allocator.Error!void {
        std.debug.assert(!self.probe_open);
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

        try self.setSlot(.plain, Self.varToSlotIdx(checked_var), .{ .root = class.desc_idx });
        const remaining_class_rank: u8 = if (class_members.items.len > 2) 1 else 0;
        try self.setUnionRank(.plain, checked_var, remaining_class_rank);
        for (class_members.items) |member| {
            if (member == checked_var or member == occurrence) continue;
            try self.setUnionRank(.plain, member, 0);
            try self.setSlot(.plain, Self.varToSlotIdx(member), .{ .redirect = checked_var });
        }

        // `occurrence` has no remaining storage children after the exact class
        // flatten above, so its singleton structural rank is zero.
        try self.setUnionRank(.plain, occurrence, 0);
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
                desc.flags = desc.flags.forKind(types.ContentKind.of(content));
                try self.setDesc(.plain, desc_idx, desc);
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
                .nominal_identities = self.nominal_identities.deserializeInto(base_addr),
                .alias_identities = self.alias_identities.deserializeInto(base_addr),
                .invalid_nominal_decl_written = self.invalid_nominal_decl_written != 0,
            };
            if (access == .sealed) store.sealLoadedColumns();
            return store;
        }
    };
};

/// Which `Slot` variant a slot holds.
const SlotKind = enum(u8) { root, redirect };

/// Represents a store of slots. Each slot is one word: its payload (a
/// `DescStore.Idx` or a redirect `Var`) with the top bit set for a redirect.
/// Every variable and descriptor index is below that bit (the checked
/// synthesized-type bit, see `representativeVar`).
const SlotStore = struct {
    const Self = @This();

    words: collections.ReservedList(u32, .dense),

    /// The bit marking a redirect.
    const redirect_bit: u32 = @as(u32, 1) << 31;

    fn init(gpa: Allocator, capacity: usize) std.mem.Allocator.Error!Self {
        return .{ .words = try collections.ReservedList(u32, .dense).initCapacity(gpa, capacity) };
    }

    fn deinit(self: *Self, gpa: Allocator) void {
        self.words.deinit(gpa);
    }

    fn clone(self: *const Self, gpa: Allocator) Allocator.Error!Self {
        return .{ .words = try self.words.clone(gpa) };
    }

    fn len(self: *const Self) usize {
        return self.words.items.len;
    }

    fn ensureTotalCapacity(self: *Self, gpa: Allocator, capacity: usize) Allocator.Error!void {
        try self.words.ensureTotalCapacity(gpa, capacity);
    }

    fn ensureUnusedCapacity(self: *Self, gpa: Allocator, additional: usize) Allocator.Error!void {
        try self.words.ensureUnusedCapacity(gpa, additional);
    }

    fn shrinkRetainingCapacity(self: *Self, new_len: usize) void {
        self.words.shrinkRetainingCapacity(new_len);
    }

    /// Serialized representation of SlotStore
    /// Uses extern struct to guarantee consistent field layout across optimization levels.
    pub const Serialized = extern struct {
        words: collections.ReservedList(u32, .dense).Serialized,

        /// Serialize a SlotStore into this Serialized struct, appending data to the writer
        pub fn serialize(
            self: *Serialized,
            slot_store: *const SlotStore,
            allocator: Allocator,
            writer: *collections.CompactWriter,
        ) Allocator.Error!void {
            try self.words.serialize(&slot_store.words, allocator, writer);
        }

        /// Deserialize into a SlotStore value (no in-place modification of cache buffer).
        /// The base parameter is the base address of the serialized buffer in memory.
        pub fn deserializeInto(self: *const Serialized, base_addr: usize) SlotStore {
            return SlotStore{ .words = self.words.deserializeInto(base_addr) };
        }
    };

    /// Insert a new slot into the store
    fn insert(self: *Self, gpa: Allocator, typ: Slot) std.mem.Allocator.Error!Idx {
        try self.ensureUnusedCapacity(gpa, 1);
        return self.appendAssumeCapacity(typ);
    }

    /// Insert a value into the store assuming there is capacity
    fn appendAssumeCapacity(self: *Self, typ: Slot) Idx {
        const safe_idx = self.words.appendAssumeCapacity(encode(typ));
        return @fromBackingInt(@intCast(@backingInt(safe_idx)));
    }

    /// Set a value in the store
    pub fn set(self: *Self, idx: Idx, val: Slot) void {
        self.words.items[@backingInt(idx)] = encode(val);
    }

    /// Get a value from the store
    fn get(self: *const Self, idx: Idx) Slot {
        return decode(self.words.items[@backingInt(idx)]);
    }

    /// Every slot, in order, as an owned slice.
    fn toOwnedSlots(self: *const Self, gpa: Allocator) Allocator.Error![]Slot {
        const slots = try gpa.alloc(Slot, self.words.items.len);
        for (slots, self.words.items) |*slot, word| slot.* = decode(word);
        return slots;
    }

    fn encode(slot: Slot) u32 {
        return switch (slot) {
            .root => |desc_idx| root: {
                std.debug.assert(@backingInt(desc_idx) & redirect_bit == 0);
                break :root @backingInt(desc_idx);
            },
            .redirect => |var_| redirect: {
                std.debug.assert(@backingInt(var_) & redirect_bit == 0);
                break :redirect @backingInt(var_) | redirect_bit;
            },
        };
    }

    fn decode(word: u32) Slot {
        if (word & redirect_bit != 0) return .{ .redirect = @fromBackingInt(word & ~redirect_bit) };
        return .{ .root = @fromBackingInt(word) };
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
        return a.word.kind == b.word.kind and a.word.literal() == b.word.literal() and std.mem.eql(u32, &a.payload, &b.payload);
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

    /// One word of descriptor `idx`'s payload.
    inline fn getPayloadWord(self: *const Self, idx: Idx, comptime word: usize) u32 {
        return self.backing.items(.payload)[@backingInt(idx)][word];
    }

    /// Overwrite one word of descriptor `idx`'s payload, keeping its word
    /// and the rest of its payload.
    fn setPayloadWord(self: *Self, idx: Idx, comptime word: usize, value: u32) void {
        self.backing.items(.payload)[@backingInt(idx)][word] = value;
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

test "an empty vars range appends nothing and a non-empty one appends its length slot and vars" {
    const gpa = std.testing.allocator;
    var store = try Store.init(gpa);
    defer store.deinit();
    const ret = try store.fresh();
    const arg = try store.fresh();

    const before = store.varsPoolLen();
    const empty = try store.appendVars(&.{});
    try std.testing.expectEqual(VarSafeList.Range.empty(), empty);
    try std.testing.expectEqual(before, store.varsPoolLen());
    try std.testing.expectEqual(Store.stored_empty_var_range, store.storedVarRange(empty));

    const no_args = try store.freshFromContent(try store.mkFuncPure(&.{}, ret));
    try std.testing.expectEqual(before, store.varsPoolLen());
    try std.testing.expectEqual(@as(usize, 0), store.sliceVars(store.resolveVar(no_args).desc.content.structure.fn_pure.args).len);

    const one_arg = try store.freshFromContent(try store.mkFuncPure(&.{arg}, ret));
    try std.testing.expectEqual(before + 2, store.varsPoolLen());
    try std.testing.expectEqualSlices(Var, &.{arg}, store.sliceVars(store.resolveVar(one_arg).desc.content.structure.fn_pure.args));
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

    // Sealing retires a kept probe and a thawed freeze, while the selected plan
    // and nominal backing remain part of the solved store.
    const callable = try store.fresh();
    const joined = try store.fresh();
    const plan = testDerivedMapPlan(5, 1);
    try store.setDerivedMapPlan(callable, plan);
    _ = store.beginProbe();
    const unifier_entry = store.enterUnifier();
    defer store.leaveUnifier(unifier_entry);
    try store.unifierUnion(callable, joined, .{ .content = .{ .flex = Flex.init() }, .rank = Rank.outermost });
    try store.endProbeYes();
    const checked_callable = store.resolveVar(callable).var_;
    try store.freezeClass(checked_callable);
    try store.thawFrozenClasses();
    const origin: base.ModuleIdentity.Idx = @fromBackingInt(1);
    const decl = try store.registerNominalDecl(try testNominalDecl(origin, 7, elem_alias));
    try store.markSettled(&.{elem});

    try store.canonicalizeAndSeal(&idents, store.len(), Store.NoSealHolders{});
    try std.testing.expect(store.isSealed());
    try std.testing.expectEqual(checked_callable, store.resolveVar(callable).var_);
    try std.testing.expectEqual(plan, store.derivedMapPlan(callable).?);
    try std.testing.expectEqual(plan, store.derivedMapPlan(joined).?);
    try std.testing.expectEqual(decl, store.lookupNominalDeclByKey(origin, 7).?);
    try std.testing.expectEqual(elem, store.getNominalDecl(decl).backing);
    try std.testing.expectEqual(@as(usize, 0), store.undo_log.capacity);
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

test "sealing closes the gaps dead descriptors and pool rows leave" {
    const gpa = std.testing.allocator;
    var idents = try base.Ident.Store.initCapacity(gpa, 2);
    defer idents.deinit(gpa);
    const ok_name = try idents.insert(gpa, base.Ident.for_text("Ok"));

    var store = try Store.init(gpa);
    defer store.deinit();
    const arg = try store.fresh();
    // A tuple whose class a merge orphans: its descriptor and vars range die.
    const dead_tuple = try store.freshFromContent(.{ .structure = .{ .tuple = .{ .elems = try store.appendVars(&.{ arg, arg }) } } });
    const elem = try store.fresh();
    const func = try store.freshFromContent(try store.mkFuncPure(&.{elem}, arg));
    const tags = try store.freshFromContent(.{ .structure = .{ .tag_union = .{
        .tags = try store.appendTags(&.{try store.mkTag(ok_name, &.{elem})}),
        .ext = try store.fresh(),
    } } });
    try store.union_(dead_tuple, func, store.resolveVar(func).desc);
    const live_classes = 5;
    try std.testing.expect(store.descs.backing.len() > live_classes);

    try store.canonicalizeAndSeal(&idents, store.len(), Store.NoSealHolders{});
    try std.testing.expectEqual(@as(u32, live_classes), store.descs.backing.len());
    try std.testing.expectEqual(@as(u32, live_classes), store.root_metas.len());
    // The function's argument range with the length slot a descriptor reads,
    // and the tag's argument range, whose tag holds its length itself.
    try std.testing.expectEqual(@as(usize, 3), store.vars.items.len);
    const resolved = store.resolveVar(dead_tuple).desc.content.structure.fn_pure;
    try std.testing.expectEqualSlices(Var, &.{elem}, store.sliceVars(resolved.args));
    try std.testing.expectEqual(arg, resolved.ret);
    const tag = store.getTagsSlice(store.resolveVar(tags).desc.content.structure.tag_union.tags).get(0);
    try std.testing.expectEqualSlices(Var, &.{elem}, store.sliceVars(tag.args));
}

/// Variables and a constraint index recorded outside a store, as checking
/// records them, for the renumbering tests.
const TestSealHolders = struct {
    raw: u32,
    typed: Var,
    constraint_index: u32,

    pub fn visitSealHolders(self: *TestSealHolders, visitor: anytype) void {
        visitor.rawVar(&self.raw);
        visitor.typeVar(&self.typed);
        visitor.constraintIndex(&self.constraint_index);
    }
};

test "sealing renumbers the variables above the canonical ones and what holders name" {
    const gpa = std.testing.allocator;
    var idents = try base.Ident.Store.initCapacity(gpa, 2);
    defer idents.deinit(gpa);
    const method = try idents.insert(gpa, base.Ident.for_text("m"));

    var store = try Store.init(gpa);
    defer store.deinit();
    const node = try store.fresh();
    const canonical_vars = store.len();
    // A merged-away member a holder names survives as a member of its class.
    const unnamed = try store.fresh();
    const held = try store.fresh();
    const elem = try store.fresh();
    try store.union_(unnamed, elem, .{ .content = .{ .flex = Flex.init() }, .rank = Rank.outermost });
    try store.union_(held, elem, .{ .content = .{ .flex = Flex.init() }, .rank = Rank.outermost });
    // A redirect to the node that nothing names.
    _ = try store.freshRedirect(node);
    // A constraint keeps its own function variable, a merged-away member.
    const fn_var = try store.fresh();
    const fn_class = try store.fresh();
    try store.union_(fn_var, fn_class, .{ .content = .{ .flex = Flex.init() }, .rank = Rank.outermost });
    const constraints = try store.appendStaticDispatchConstraints(&.{.{ .fn_name = method, .fn_var = fn_var, .origin = .method_call }});
    const receiver = try store.freshFromContent(.{ .flex = Flex.init().withConstraints(constraints) });
    const list = try store.freshFromContent(.{ .structure = .{ .tuple = .{ .elems = try store.appendVars(&.{ elem, receiver }) } } });
    try store.union_(node, list, store.resolveVar(list).desc);

    const elem_rep = store.resolveVar(elem).var_;
    var holders = TestSealHolders{ .raw = @backingInt(held), .typed = elem_rep, .constraint_index = @backingInt(constraints.start) };
    try store.canonicalizeAndSeal(&idents, canonical_vars, &holders);

    const held_var: Var = @fromBackingInt(holders.raw);
    try std.testing.expectEqual(holders.typed, store.resolveVar(held_var).var_);
    const tuple = store.resolveVar(node).desc.content.structure.tuple;
    const elems = store.sliceVars(tuple.elems);
    try std.testing.expectEqual(holders.typed, elems[0]);
    const receiver_constraints = store.resolveVar(elems[1]).desc.content.flex.constraints;
    try std.testing.expectEqual(holders.constraint_index, @backingInt(receiver_constraints.start));
    const constraint = store.getStaticDispatchConstraintAt(holders.constraint_index);
    try std.testing.expectEqual(method, constraint.fn_name);
    // Nothing names the redirect to the node, so it dies with the gaps.
    try std.testing.expect(store.len() <= 8);
    for (0..store.len()) |index| {
        switch (store.getSlot(@fromBackingInt(@intCast(index)))) {
            .root => |desc_idx| try std.testing.expectEqual(@as(u32, @intCast(index)), @backingInt(store.getRootMeta(desc_idx).checked_var)),
            .redirect => |target| try std.testing.expect(store.getSlot(target) == .root),
        }
    }
}

test "sealing drops dead descriptors and renumbers the slots and selections naming survivors" {
    const gpa = std.testing.allocator;
    var idents = try base.Ident.Store.initCapacity(gpa, 1);
    defer idents.deinit(gpa);
    var store = try Store.init(gpa);
    defer store.deinit();

    const rigid: Content = .{ .rigid = Rigid.init(@bitCast(@as(u32, 7))) };
    const a = try store.fresh();
    const b = try store.freshFromContent(rigid);
    const c = try store.fresh();
    const tuple = try store.freshFromContent(.{ .structure = .{ .tuple = .{ .elems = try store.appendVars(&.{ a, c }) } } });
    const d = try store.fresh();
    const plan = testDerivedMapPlan(3, 0);
    try store.setDerivedMapPlan(d, plan);
    // Each merge orphans the first side's descriptor: a's and c's.
    try store.union_(a, b, .{ .content = rigid, .rank = Rank.outermost });
    try store.union_(c, d, .{ .content = .{ .flex = Flex.init() }, .rank = Rank.outermost });
    const descs_before = store.descs.backing.len();

    try store.canonicalizeAndSeal(&idents, store.len(), Store.NoSealHolders{});
    try std.testing.expectEqual(descs_before - 2, store.descs.backing.len());
    try std.testing.expectEqual(descs_before - 2, store.root_metas.len());
    for (store.slots.words.items) |word| switch (SlotStore.decode(word)) {
        .root => |desc_idx| try std.testing.expect(@backingInt(desc_idx) < store.descs.backing.len()),
        .redirect => {},
    };
    try std.testing.expectEqual(rigid, store.resolveVar(a).desc.content);
    try std.testing.expectEqual(rigid, store.resolveVar(b).desc.content);
    try std.testing.expectEqual(store.resolveVar(a).var_, store.resolveVar(b).var_);
    try std.testing.expectEqual(Content{ .flex = Flex.init() }, store.resolveVar(c).desc.content);
    try std.testing.expectEqual(plan, store.derivedMapPlan(c).?);
    try std.testing.expectEqual(plan, store.derivedMapPlan(d).?);
    const elems = store.resolveVar(tuple).desc.content.structure.tuple.elems;
    try std.testing.expectEqualSlices(Var, &.{ store.resolveVar(a).var_, store.resolveVar(c).var_ }, store.sliceVars(elems));
}

test "sealing drops unreached vars ranges and keeps shared ones shared" {
    const gpa = std.testing.allocator;
    var idents = try base.Ident.Store.initCapacity(gpa, 1);
    defer idents.deinit(gpa);
    var store = try Store.init(gpa);
    defer store.deinit();

    const x = try store.fresh();
    const y = try store.fresh();
    _ = try store.appendVars(&.{ x, y, x });
    const shared = try store.appendVars(&.{ x, y });
    const first = try store.freshFromContent(.{ .structure = .{ .tuple = .{ .elems = shared } } });
    const second = try store.freshFromContent(.{ .structure = .{ .tuple = .{ .elems = shared } } });
    _ = try store.appendVars(&.{y});
    const func = try store.freshFromContent(try store.mkFuncUnboundWithEffectDeps(&.{x}, y, &.{ y, x }));
    const vars_before = store.varsPoolLen();

    try store.canonicalizeAndSeal(&idents, store.len(), Store.NoSealHolders{});
    // Dropped: the first range (four entries) and `.{y}` (two).
    try std.testing.expectEqual(vars_before - 6, store.varsPoolLen());
    const first_elems = store.resolveVar(first).desc.content.structure.tuple.elems;
    const second_elems = store.resolveVar(second).desc.content.structure.tuple.elems;
    try std.testing.expectEqual(first_elems, second_elems);
    try std.testing.expectEqual(@as(u32, 1), @backingInt(first_elems.start));
    try std.testing.expectEqualSlices(Var, &.{ x, y }, store.sliceVars(first_elems));
    const func_content = store.resolveVar(func).desc.content.structure.fn_unbound;
    try std.testing.expectEqualSlices(Var, &.{x}, store.sliceVars(func_content.args));
    try std.testing.expectEqualSlices(Var, &.{ y, x }, store.sliceVars(func_content.effect_deps));
    try std.testing.expectEqual(y, func_content.ret);
}

test "sealing drops unreached tags and moves reached tags' payload ranges" {
    const gpa = std.testing.allocator;
    var idents = try base.Ident.Store.initCapacity(gpa, 2);
    defer idents.deinit(gpa);
    const ok_name = try idents.insert(gpa, base.Ident.for_text("Ok"));
    const err_name = try idents.insert(gpa, base.Ident.for_text("Err"));
    var store = try Store.init(gpa);
    defer store.deinit();

    const x = try store.fresh();
    const y = try store.fresh();
    _ = try store.appendTags(&.{.{ .name = ok_name, .args = try store.appendVars(&.{x}) }});
    const err_args = try store.appendVars(&.{ x, y });
    const ok_args = try store.appendVars(&.{y});
    const tail = try store.fresh();
    const tag_union = try store.freshFromContent(.{ .structure = .{ .tag_union = .{
        .tags = try store.appendTags(&.{
            .{ .name = err_name, .args = err_args },
            .{ .name = ok_name, .args = ok_args },
            .{ .name = ok_name, .args = Var.SafeList.Range.empty() },
        }),
        .ext = tail,
    } } });

    try store.canonicalizeAndSeal(&idents, store.len(), Store.NoSealHolders{});
    try std.testing.expectEqual(@as(u32, 3), store.tags.len());
    try std.testing.expectEqual(@as(u32, 3), store.varsPoolLen());
    const tags = store.getTagsSlice(store.resolveVar(tag_union).desc.content.structure.tag_union.tags);
    try std.testing.expectEqual(@as(usize, 3), tags.len);
    try std.testing.expectEqualSlices(base.Ident.Idx, &.{ err_name, ok_name, ok_name }, tags.items(.name));
    try std.testing.expectEqualSlices(Var, &.{ x, y }, store.sliceVars(tags.items(.args)[0]));
    try std.testing.expectEqualSlices(Var, &.{y}, store.sliceVars(tags.items(.args)[1]));
    try std.testing.expectEqual(@as(u32, 0), tags.items(.args)[2].count);
}

test "sealing keeps absorbed rows and drops the segments only they reached" {
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
    const outer = try store.freshFromContent(.{ .structure = .{ .record = .{
        .fields = try store.appendRecordFields(&.{.{ .name = b_name, .presence = .required(elem_alias) }}),
        .ext = inner,
    } } });
    const inner_tags = try store.freshFromContent(.{ .structure = .{ .tag_union = .{
        .tags = try store.appendTags(&.{.{ .name = ok_name, .args = try store.appendVars(&.{elem_alias}) }}),
        .ext = tail,
    } } });
    const outer_tags = try store.freshFromContent(.{ .structure = .{ .tag_union = .{
        .tags = try store.appendTags(&.{.{ .name = err_name, .args = try store.appendVars(&.{ elem_alias, elem }) }}),
        .ext = inner_tags,
    } } });

    try store.canonicalizeAndSeal(&idents, store.len(), Store.NoSealHolders{});
    // The inner rows stay; the outer rows' own segments are replaced by
    // their absorbed rows.
    try std.testing.expectEqual(@as(u32, 3), store.record_fields.len());
    try std.testing.expectEqual(@as(u32, 3), store.tags.len());
    try std.testing.expectEqual(@as(u32, 3), store.varsPoolLen());

    const inner_record = store.resolveVar(inner).desc.content.structure.record;
    try std.testing.expectEqualSlices(base.Ident.Idx, &.{a_name}, store.getRecordFieldsSlice(inner_record.fields).items(.name));
    const outer_record = store.resolveVar(outer).desc.content.structure.record;
    try std.testing.expectEqual(tail, outer_record.ext);
    const outer_fields = store.getRecordFieldsSlice(outer_record.fields);
    try std.testing.expectEqualSlices(base.Ident.Idx, &.{ a_name, b_name }, outer_fields.items(.name));
    for (outer_fields.items(.presence)) |presence| try std.testing.expectEqual(elem, presence.var_);

    const inner_union = store.resolveVar(inner_tags).desc.content.structure.tag_union;
    try std.testing.expectEqualSlices(Var, &.{elem}, store.sliceVars(store.getTagsSlice(inner_union.tags).items(.args)[0]));
    const outer_union = store.resolveVar(outer_tags).desc.content.structure.tag_union;
    try std.testing.expectEqual(tail, outer_union.ext);
    const outer_tag_rows = store.getTagsSlice(outer_union.tags);
    try std.testing.expectEqualSlices(base.Ident.Idx, &.{ err_name, ok_name }, outer_tag_rows.items(.name));
    try std.testing.expectEqualSlices(Var, &.{ elem, elem }, store.sliceVars(outer_tag_rows.items(.args)[0]));
    try std.testing.expectEqualSlices(Var, &.{elem}, store.sliceVars(outer_tag_rows.items(.args)[1]));
}

test "sealing keeps nominal declaration formals and moves them" {
    const gpa = std.testing.allocator;
    var idents = try base.Ident.Store.initCapacity(gpa, 1);
    defer idents.deinit(gpa);
    var store = try Store.init(gpa);
    defer store.deinit();

    const x = try store.fresh();
    const backing = try store.fresh();
    const backing_alias = try store.freshRedirect(backing);
    _ = try store.appendVars(&.{ x, x });
    const formal = try store.fresh();
    const formal_alias = try store.freshRedirect(formal);
    var decl = try testNominalDecl(@fromBackingInt(1), 3, backing_alias);
    decl.formals = try store.appendVars(&.{formal_alias});
    const decl_idx = try store.registerNominalDecl(decl);

    try store.canonicalizeAndSeal(&idents, store.len(), Store.NoSealHolders{});
    try std.testing.expectEqual(@as(u32, 1), store.varsPoolLen());
    const sealed = store.getNominalDecl(decl_idx);
    try std.testing.expectEqual(backing, sealed.backing);
    try std.testing.expectEqual(@as(u32, 0), @backingInt(sealed.formals.start));
    try std.testing.expectEqualSlices(Var, &.{formal}, store.sliceVars(sealed.formals));
}

test "sealing a store with no dead descriptor or row keeps each in place" {
    const gpa = std.testing.allocator;
    var idents = try base.Ident.Store.initCapacity(gpa, 2);
    defer idents.deinit(gpa);
    const a_name = try idents.insert(gpa, base.Ident.for_text("a"));
    const ok_name = try idents.insert(gpa, base.Ident.for_text("Ok"));
    var store = try Store.init(gpa);
    defer store.deinit();

    const x = try store.fresh();
    const x_alias = try store.freshRedirect(x);
    const tail = try store.fresh();
    const tuple = try store.freshFromContent(.{ .structure = .{ .tuple = .{ .elems = try store.appendVars(&.{ x_alias, tail }) } } });
    const record = try store.freshFromContent(.{ .structure = .{ .record = .{
        .fields = try store.appendRecordFields(&.{.{ .name = a_name, .presence = .required(x_alias) }}),
        .ext = tail,
    } } });
    const tag_union = try store.freshFromContent(.{ .structure = .{ .tag_union = .{
        .tags = try store.appendTags(&.{.{ .name = ok_name, .args = try store.appendVars(&.{x_alias}) }}),
        .ext = tail,
    } } });
    const plan = testDerivedMapPlan(1, 0);
    try store.setDerivedMapPlan(tail, plan);
    const descs_before = store.descs.backing.len();
    const vars_before = store.varsPoolLen();
    const slots_before = try gpa.dupe(u32, store.slots.words.items);
    defer gpa.free(slots_before);

    try store.canonicalizeAndSeal(&idents, store.len(), Store.NoSealHolders{});
    try std.testing.expectEqual(descs_before, store.descs.backing.len());
    // Only the length slot before the tag's payload range dies: a tag holds
    // its range's length itself, and only a descriptor's stored range reads
    // the slot.
    try std.testing.expectEqual(vars_before - 1, store.varsPoolLen());
    try std.testing.expectEqual(@as(u32, 1), store.record_fields.len());
    try std.testing.expectEqual(@as(u32, 1), store.tags.len());
    try std.testing.expectEqualSlices(u32, slots_before, store.slots.words.items);
    try std.testing.expectEqual(plan, store.derivedMapPlan(tail).?);
    try std.testing.expectEqualSlices(Var, &.{ x, tail }, store.sliceVars(store.resolveVar(tuple).desc.content.structure.tuple.elems));
    const fields = store.getRecordFieldsSlice(store.resolveVar(record).desc.content.structure.record.fields);
    try std.testing.expectEqual(x, fields.items(.presence)[0].var_);
    const tags = store.getTagsSlice(store.resolveVar(tag_union).desc.content.structure.tag_union.tags);
    try std.testing.expectEqualSlices(Var, &.{x}, store.sliceVars(tags.items(.args)[0]));
}

test "sealing releases checker-only buffers and unused column capacity" {
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
        inline for (.{ "rows", "pool", "fields", "tags", "pinned", "replay_pool", "replay_fields", "replay_tags", "replay_rows", "args" }) |field| {
            try @field(store.backing_openings, field).ensureTotalCapacity(gpa, 8);
        }
        try store.backing_openings.recordings.ensureTotalCapacity(gpa, 8);
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
        inline for (.{ "detach_members_scratch", "undo_log", "free_descs", "pending_free_descs" }) |field| {
            try @field(store, field).ensureTotalCapacity(gpa, 8);
        }
        try store.static_dispatch_constraint_room.ensureTotalCapacity(gpa, 8);
        try store.static_dispatch_relation_merges.append(gpa, .{ .dropped_fn_var = alias, .retained_fn_var = value });
        try std.testing.expect(counter.allocated_bytes - counter.freed_bytes > solved_bytes);
        try store.seal();

        try std.testing.expect(counter.allocated_bytes - counter.freed_bytes < solved_bytes);
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
    try store.seal();
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

fn openAskOnSealedStore() Allocator.Error!void {
    var a: Var = undefined;
    var b: Var = undefined;
    var store = try sealedTestStore(&a, &b);
    defer store.deinit();
    _ = store.beginProbe();
}

test "opening a probe on a sealed store is a violated invariant" {
    try base.death_test.expectPanic("a sealed type store was written", openAskOnSealedStore, .{});
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
    try store.seal();
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
    for (store.slots.words.items) |word| {
        switch (SlotStore.decode(word)) {
            .root => |named| if (named == desc_idx) return true,
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

test "a probe frees nothing until it is kept and reissues no freed descriptor" {
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

    // A discarded probe: a fresh var is appended rather than reissued, and the
    // merge inside orphans a pre-probe descriptor. Discarding puts the
    // descriptor and the free list back, byte for byte.
    {
        var copy = try store.testCopy();
        defer copy.deinit(gpa);
        const mark = store.beginProbe();
        const e = try store.fresh();
        try std.testing.expectEqual(descs_before, @backingInt(store.resolveVar(e).desc_idx));
        const unifier_entry = store.enterUnifier();
        defer store.leaveUnifier(unifier_entry);
        try store.unifierUnion(d, a, .{ .content = .{ .flex = Flex.init() }, .rank = Rank.outermost });
        try std.testing.expectEqualSlices(DescStore.Idx, &.{d_desc}, store.pending_free_descs.items);
        store.endProbeNo(&mark);
        try store.expectRestoredTo(&copy);
        try std.testing.expectEqual(d_desc, store.resolveVar(d).desc_idx);
        try std.testing.expectEqual(Content{ .structure = .empty_record }, store.resolveVar(d).desc.content);
    }

    // A kept probe keeps its merge, so the orphan is freed when it is kept.
    {
        _ = store.beginProbe();
        const e = try store.fresh();
        const e_desc = store.resolveVar(e).desc_idx;
        const unifier_entry = store.enterUnifier();
        defer store.leaveUnifier(unifier_entry);
        try store.unifierUnion(d, a, .{ .content = .{ .flex = Flex.init() }, .rank = Rank.outermost });
        try store.endProbeYes();
        try std.testing.expectEqualSlices(DescStore.Idx, &.{ free_before[0], free_before[1], d_desc }, store.free_descs.items);
        try std.testing.expectEqual(@as(usize, 0), store.pending_free_descs.items.len);
        try std.testing.expectEqual(e_desc, store.resolveVar(e).desc_idx);
        try std.testing.expect(!descIsNamed(&store, d_desc));
        try std.testing.expectEqual(store.resolveVar(a).desc_idx, store.resolveVar(d).desc_idx);
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

    // A discarded probe clears every settled class.
    try store.markSettled(&.{witness});
    const mark = store.beginProbe();
    store.endProbeNo(&mark);
    try std.testing.expect(!store.isSettled(witness));
}

test "function-free records are kept apart from settled ones and forgotten on any observable change" {
    const gpa = std.testing.allocator;
    var store = try Store.init(gpa);
    defer store.deinit();

    // A flex merged into a recorded class keeps its content, and the merged
    // class stays recorded. The two kinds of record are independent.
    const recorded = try store.freshFromContent(.{ .structure = .empty_record });
    try store.markFunctionFree(&.{recorded});
    try std.testing.expect(!store.isSettled(recorded));
    const joining = try store.fresh();
    try store.union_(joining, recorded, .{ .content = .{ .structure = .empty_record }, .rank = Rank.outermost });
    try std.testing.expect(store.isFunctionFree(recorded));
    try std.testing.expect(store.isFunctionFree(joining));

    // A recorded flex that comes to see other content forgets every record,
    // and leaves the settled classes alone.
    const recorded_flex = try store.fresh();
    const settled = try store.freshFromContent(.{ .structure = .empty_record });
    try store.markSettled(&.{settled});
    try store.markFunctionFree(&.{recorded_flex});
    const structure = try store.freshFromContent(.{ .structure = .empty_record });
    try store.union_(recorded_flex, structure, .{ .content = .{ .structure = .empty_record }, .rank = Rank.outermost });
    try std.testing.expect(!store.isFunctionFree(recorded));
    try std.testing.expect(store.isSettled(settled));

    // A discarded probe forgets every record.
    try store.markFunctionFree(&.{settled});
    const mark = store.beginProbe();
    store.endProbeNo(&mark);
    try std.testing.expect(!store.isFunctionFree(settled));
}

test "a discarded probe leaves the store byte-for-byte as it was" {
    const gpa = std.testing.allocator;

    // A few independent runs with different mutation mixes.
    var run: usize = 0;
    while (run < 4) : (run += 1) {
        var store = try Store.init(gpa);
        defer store.deinit();

        // Content before the probe: a handful of vars, some redirected/unioned.
        const a = try store.fresh();
        const b = try store.fresh();
        _ = try store.freshRedirect(b);
        try store.union_(a, b, .{ .content = .err, .rank = Rank.generalized });

        var copy = try store.testCopy();
        defer copy.deinit(gpa);
        const mark = store.beginProbe();

        // Writes a relation a probe runs might make, varied per run: appends
        // (fresh/register), unifier writes to entries that predate the probe
        // (union, content, rank), the same entry written twice (reverse
        // replay), and a unifier resolve (which compresses nothing).
        const fresh1 = try store.fresh();
        const fresh2 = try store.register(.{ .content = .{ .flex = Flex.init() }, .rank = Rank.outermost });
        const unifier_entry = store.enterUnifier();
        defer store.leaveUnifier(unifier_entry);
        try store.unifierUnion(fresh1, fresh2, .{ .content = .err, .rank = Rank.outermost });
        try store.unifierSetVarContent(a, .{ .flex = Flex.init() });
        try store.unifierSetVarContent(a, .err);
        if (run % 2 == 0) try store.unifierSetDescRank(store.resolveVar(b).desc_idx, Rank.outermost);
        _ = store.unifierResolveVarWord(a);

        store.endProbeNo(&mark);
        try std.testing.expect(!store.probe_open);
        try store.expectRestoredTo(&copy);
    }
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

test "a discarded probe restores the derived map plans and a freed class forgets its plan" {
    const gpa = std.testing.allocator;
    var store = try Store.init(gpa);
    defer store.deinit();
    const flex_desc: Desc = .{ .content = .{ .flex = Flex.init() }, .rank = Rank.outermost };
    const plan = testDerivedMapPlan(3, 2);

    const callable = try store.fresh();
    const other = try store.fresh();
    try store.setDerivedMapPlan(callable, plan);
    {
        var copy = try store.testCopy();
        defer copy.deinit(gpa);
        const mark = store.beginProbe();
        const unifier_entry = store.enterUnifier();
        defer store.leaveUnifier(unifier_entry);
        try store.unifierUnion(callable, other, flex_desc);
        const fresh_callable = try store.fresh();
        try store.setDerivedMapPlan(fresh_callable, testDerivedMapPlan(4, 0));
        try std.testing.expectEqual(plan, store.derivedMapPlan(other).?);
        store.endProbeNo(&mark);
        try store.expectRestoredTo(&copy);
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
    const builtin_module_idx = @as(base.ModuleIdentity.Idx, @fromBackingInt(1));
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

    // Sealing releases checker state and moves columns into storage that
    // holds exactly their entries, without changing any serialized solved
    // data. A serialization plan borrows the columns it was made from, so each
    // side of the boundary is serialized afresh and only the sealed plan is
    // written out below.
    const before_seal = try gpa.alloc(u8, writer.total_bytes);
    defer gpa.free(before_seal);
    _ = try writer.writeToBuffer(before_seal);
    try original.freezeClass(flex);
    try original.thawFrozenClasses();
    try original.markSettled(&.{flex});
    try original.seal();
    var sealed_writer = CompactWriter.init();
    defer sealed_writer.deinit(gpa);
    try (try sealed_writer.appendAlloc(gpa, Store.Serialized)).serialize(&original, gpa, &sealed_writer);
    const after_seal = try gpa.alloc(u8, sealed_writer.total_bytes);
    defer gpa.free(after_seal);
    _ = try sealed_writer.writeToBuffer(after_seal);
    try std.testing.expectEqualSlices(u8, before_seal, after_seal);

    // Write to file
    try sealed_writer.writeGather(file, io);

    // Read back
    const file_size = sealed_writer.total_bytes;
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
            @as(base.ModuleIdentity.Idx, @fromBackingInt(1)),
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
            @as(base.ModuleIdentity.Idx, @fromBackingInt(1)),
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
