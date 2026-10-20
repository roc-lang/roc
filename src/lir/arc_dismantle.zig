//! Field takes from dying aggregates.
//!
//! A payload read pays a retain whenever its result must be owned, because
//! the container keeps its stored unit. When the container itself is about
//! to die, that retain is the difference between mutating in place and
//! copying: the read result carries count 2 into the mutation's runtime
//! uniqueness check. This analysis finds containers whose whole life is
//! being read field-by-field and then dying, and marks their consuming
//! reads as takes: the read consumes the container's stored unit for that
//! field, and the container is dismantled instead of released whole.
//!
//! Like precise lifetimes, take solving is order-sensitive, so it runs in
//! the ARC stage against the solved binding modes rather than inside the
//! mode fixpoint. It is deliberately demand-driven: a local that cannot
//! benefit—wrong layout shape, borrowed, or non-operand whole uses—
//! contributes nothing beyond its visit in one linear statement scan, and
//! per-candidate tables exist only for locals that pass the layout gate.
//! The rules are specified in design.md's "Field Takes From Dying
//! Aggregates".

const std = @import("std");
const base = @import("base");
const collections = @import("collections");
const core = @import("lir_core");
const layout_mod = @import("layout");
const arc_sig = @import("arc_sig.zig");
const arc_solve = @import("arc_solve.zig");
const body_clone = @import("body_clone.zig");
const TaskExecutor = base.post_check_task_executor;

const LIR = core.LIR;
const LirStore = core.LirStore;
const GuardedList = collections.GuardedList;
const Allocator = std.mem.Allocator;

/// Allocation errors returned while solving field takes.
pub const Error = std.mem.Allocator.Error;

const no_index: u32 = std.math.maxInt(u32);

/// Compact description of an aggregate projection whose result may carry a
/// stored ownership unit out of its source: the projection form and its
/// semantic field or tag indices. It has no padding, so its bytes hash
/// directly.
pub const Projection = extern struct {
    kind: Kind,
    /// The field index of a `.field` projection, or the variant index of a tag
    /// payload projection.
    first: u32,
    /// The payload index within the variant of a `.tag_payload` projection.
    second: u32,

    pub const Kind = enum(u32) {
        none,
        field,
        tag_payload,
        tag_payload_struct,
    };

    pub const none: Projection = .{ .kind = .none, .first = 0, .second = 0 };

    pub fn eql(a: Projection, b: Projection) bool {
        return a.kind == b.kind and a.first == b.first and a.second == b.second;
    }

    pub fn isNone(self: Projection) bool {
        return self.kind == .none;
    }
};

/// Encodes the ownership-relevant shape of a reference projection, or null
/// when the operation does not select aggregate storage.
pub fn encodeProjection(op: LIR.RefOp) ?Projection {
    return switch (op.kind) {
        .field => .{ .kind = .field, .first = op.index, .second = 0 },
        .tag_payload => .{ .kind = .tag_payload, .first = op.variant_index, .second = op.index },
        .tag_payload_struct => .{ .kind = .tag_payload_struct, .first = op.variant_index, .second = 0 },
        .local, .discriminant, .list_reinterpret, .nominal => null,
    };
}

/// Cycle-safe check for whether a layout may hold descriptor-driven dynamic
/// (`erased_box`) content. Recursive tag unions reference themselves through
/// their layout indices, so the walk tracks visited indices; `visited` and
/// `stack` are caller-owned scratch reused across queries.
pub fn layoutMayContainBoxyDynamic(
    allocator: Allocator,
    layouts: *const layout_mod.Store,
    layout_idx: layout_mod.Idx,
    visited: *std.AutoHashMap(layout_mod.Idx, void),
    stack: *std.ArrayList(layout_mod.Idx),
) Error!bool {
    visited.clearRetainingCapacity();
    stack.clearRetainingCapacity();
    try stack.append(allocator, layout_idx);
    while (stack.pop()) |idx| {
        if ((try visited.getOrPut(idx)).found_existing) continue;
        const layout_val = layouts.getLayout(idx);
        switch (layout_val.tag) {
            .erased_box => return true,
            .box, .list => try stack.append(allocator, layout_val.getIdx()),
            .list_of_zst, .box_of_zst, .zst, .scalar, .erased_callable, .ptr => {},
            .struct_ => {
                const info = layouts.getStructInfo(layout_val);
                for (0..info.fields.len) |index| {
                    try stack.append(allocator, info.fields.get(@intCast(index)).layout);
                }
            },
            .tag_union => {
                const info = layouts.getTagUnionInfo(layout_val);
                for (0..info.variants.len) |index| {
                    try stack.append(allocator, info.variants.get(@intCast(index)).payload_layout);
                }
            },
            .closure => try stack.append(allocator, layout_val.getClosure().captures_layout_idx),
        }
    }
    return false;
}

fn structFieldBySemanticIndex(layouts: *const layout_mod.Store, struct_layout: layout_mod.Layout, field_idx: u32) ?layout_mod.StructField {
    const info = layouts.getStructInfo(struct_layout);
    for (0..info.fields.len) |index| {
        const field = info.fields.get(@intCast(index));
        if (field.index == field_idx) return field;
    }
    return null;
}

fn structHasOneRcField(layouts: *const layout_mod.Store, struct_layout: layout_mod.Layout, field_idx: u32) bool {
    const info = layouts.getStructInfo(struct_layout);
    var only_field: ?u32 = null;
    for (0..info.fields.len) |index| {
        const field = info.fields.get(@intCast(index));
        if (!layouts.layoutContainsRefcounted(layouts.getLayout(field.layout))) continue;
        if (only_field != null) return false;
        only_field = field.index;
    }
    return only_field != null and only_field.? == field_idx;
}

/// Whether this projection covers every refcounted byte owned by the source's
/// active aggregate shape. Moving such a projection transfers the source's
/// one ownership unit without a retain or a residual release. For a tag union,
/// the payload read itself proves which variant is active on that path; other
/// variants remain ordinary whole releases on their own paths.
pub fn projectionOwnsAllRc(
    store: *const LirStore,
    layouts: *const layout_mod.Store,
    source: LIR.LocalId,
    target: LIR.LocalId,
    projection: Projection,
) bool {
    const source_layout_idx = store.getLocal(source).layout_idx;
    const target_layout_idx = store.getLocal(target).layout_idx;
    const source_layout = layouts.getLayout(source_layout_idx);
    switch (projection.kind) {
        .field => {
            if (source_layout.tag != .struct_) return false;
            const field_idx = projection.first;
            const field = structFieldBySemanticIndex(layouts, source_layout, field_idx) orelse return false;
            return field.layout == target_layout_idx and
                layouts.layoutContainsRefcounted(layouts.getLayout(field.layout)) and
                structHasOneRcField(layouts, source_layout, field_idx);
        },
        .tag_payload, .tag_payload_struct => {
            if (source_layout.tag != .tag_union) return false;
            const info = layouts.getTagUnionInfo(source_layout);
            const variant_index = projection.first;
            if (variant_index >= info.variants.len) return false;
            const payload_layout_idx = info.variants.get(variant_index).payload_layout;
            const payload_layout = layouts.getLayout(payload_layout_idx);
            if (projection.kind == .tag_payload_struct) {
                return payload_layout_idx == target_layout_idx and
                    layouts.layoutContainsRefcounted(payload_layout);
            }
            const payload_idx = projection.second;
            if (payload_layout.tag == .struct_) {
                const field = structFieldBySemanticIndex(layouts, payload_layout, payload_idx) orelse return false;
                return field.layout == target_layout_idx and
                    layouts.layoutContainsRefcounted(layouts.getLayout(field.layout)) and
                    structHasOneRcField(layouts, payload_layout, payload_idx);
            }
            return payload_idx == 0 and payload_layout_idx == target_layout_idx and
                layouts.layoutContainsRefcounted(payload_layout);
        },
        .none => return false,
    }
}

/// The payload view a tag-union container dismantles through: its fields are
/// the view struct's, and its residual release reads the discriminant fresh
/// and releases the variant's residual fields through that same view. One
/// view means one claim chain: a second view of the union would compete with
/// the first for the union's single ownership-complete claim.
pub const PayloadView = struct {
    view: LIR.LocalId,
    tag_discriminant: u32,
};

/// A contiguous range of a struct's ownership slots (see
/// `layout.Store.ownershipSlotCount`).
pub const SlotRange = struct {
    first: u32,
    count: u32,

    pub fn end(self: SlotRange) u32 {
        return self.first + self.count;
    }

    pub fn contains(self: SlotRange, slot: u32) bool {
        return slot >= self.first and slot < self.end();
    }
};

/// Committed field-place domain for one dismantlable container. Its places
/// are pieces: the maximal ranges of its ownership slots between the bounds
/// of the ranges its reads address, each holding refcounted slots. Mask bit
/// `i` is `pieces[i]`, so every read takes or observes whole pieces, and
/// slots no read separates share one piece. Residual ownership is path
/// state in ARC, not a global property of this descriptor.
pub const Container = struct {
    /// The struct layout whose ownership slots the pieces cover: the
    /// container's own, or its payload view's for a tag union.
    layout_idx: layout_mod.Idx,
    pieces: []const SlotRange,
    full_mask: u64,
    /// Set for a tag-union container, whose fields belong to this view.
    payload_view: ?PayloadView = null,

    /// The mask bit of the piece holding `slot`, if one does.
    pub fn pieceOf(self: Container, slot: u32) ?u6 {
        var low: usize = 0;
        var high: usize = self.pieces.len;
        while (low < high) {
            const mid = low + (high - low) / 2;
            if (self.pieces[mid].end() <= slot) low = mid + 1 else high = mid;
        }
        if (low == self.pieces.len or !self.pieces[low].contains(slot)) return null;
        return @intCast(low);
    }
};

/// Exact resource-place transfer performed by one field read: the ownership
/// slots of `root` the read's result carries away.
pub const Take = struct {
    root: LIR.LocalId,
    field_mask: u64,
};

/// A borrowed struct read out of a dismantlable container. It holds no unit:
/// its field reads are reads of the container's slots starting at `base`,
/// and its representation names slots the container may already have given
/// away, which ARC records on its definition as a residual shell.
pub const View = struct {
    root: LIR.LocalId,
    base: u32,
    /// The innermost view, this one or one enclosing it, whose definition
    /// takes its slots out of its container. In an emission where that take
    /// applies, the holder owns those slots: this view is then a view of the
    /// holder's own container from `holder_base`, or no view at all when it
    /// is the holder or the holder does not dismantle.
    holder: ?LIR.LocalId = null,
    holder_base: u32 = 0,
    /// This local is the holder itself or a borrowed alias of it.
    is_holder: bool = false,
};

/// One direct-call argument whose owned unit came from an exact aggregate
/// field place. The callee's declared outcome convention may return that
/// unit to this place on selected result edges.
pub const CallArg = struct {
    stmt: LIR.CFStmtId,
    position: u8,
};

/// Exact aggregate field place and result refinement that can restore it.
pub const FieldRestitutionArg = struct {
    place: Take,
    projection: LIR.CFStmtId,
    refinement: LIR.CFStmtId,
};

/// Per-procedure dismantling decisions consumed while emitting ARC operations.
pub const Dismantles = struct {
    arena: base.SingleThreadArena,
    /// `assign_ref .field` statements whose reads consume the container's
    /// stored unit; emission skips their retain.
    takes: collections.DenseMap(LIR.CFStmtId, Take),
    /// Dismantlable containers and their committed field-place domains.
    containers: collections.DenseMap(LIR.LocalId, Container),
    /// Takes on containers that are proc parameters solved borrowed: valid
    /// only in emissions where the demand vector overrides the parameter to
    /// owned (mode-specialized variants). The value is the parameter local,
    /// so emission can check the override for the current variant.
    owned_only_takes: collections.DenseMap(LIR.CFStmtId, Take),
    /// Containers behind `owned_only_takes`, keyed by the container local:
    /// the parameter itself, or a projection binding whose takes that
    /// parameter's owned override activates.
    owned_only_containers: collections.DenseMap(LIR.LocalId, Container),
    /// Parameter positions whose owned variant activates an exact field take,
    /// indexed directly by source procedure id.
    owned_only_param_benefits: []arc_sig.ParamMask,
    /// Binding targets produced by unconditional field takes. The target
    /// carries the stored field unit even when its base binding was solved
    /// borrowed.
    take_bindings: []bool,
    /// For a binding solved borrowed whose field read becomes a take in an
    /// owned-parameter variant, the exact borrowed parameter root authorizing
    /// that binding-mode override; `no_index` means no override.
    owned_only_binding_roots: []u32,
    /// Exact field-place destinations for outcome-conditioned argument
    /// restitution. Entries exist only when the projection binding has no
    /// operand use other than this call and the call result has the closed,
    /// explicit discriminant refinement accepted by this pass.
    field_restitution_args: std.AutoHashMapUnmanaged(CallArg, FieldRestitutionArg),
    /// Borrowed projection locals that denote the complete refcounted payload
    /// of another local. Moving one of these locals moves the root's unit
    /// directly when that unit is present; otherwise the ordinary retain is
    /// preserved.
    projection_units: collections.DenseMap(LIR.LocalId, LIR.LocalId),
    /// Complete projection reads whose owned target can receive the root's
    /// unit directly. The solve-time transfer still checks path liveness and
    /// keeps the ordinary retain when the root must survive.
    complete_takes: collections.DenseMap(LIR.CFStmtId, LIR.LocalId),
    /// Later complete projections rewritten to explicit aliases of the
    /// dominating projected container that owns their representation.
    projection_aliases: collections.DenseMap(LIR.LocalId, LIR.LocalId),
    /// Views into committed containers, keyed by the view binding: borrowed
    /// struct field reads and borrowed pure aliases of them.
    views: collections.DenseMap(LIR.LocalId, View),

    pub fn deinit(self: *Dismantles) void {
        const gpa = self.arena.child_allocator;
        self.takes.deinit();
        self.containers.deinit();
        self.owned_only_takes.deinit();
        self.owned_only_containers.deinit();
        self.field_restitution_args.deinit(gpa);
        self.projection_units.deinit();
        self.complete_takes.deinit();
        self.projection_aliases.deinit();
        self.views.deinit();
        self.arena.deinit();
    }

    pub fn takeAt(self: *const Dismantles, stmt: LIR.CFStmtId) ?Take {
        return self.takes.get(stmt);
    }

    pub fn ownedOnlyTake(self: *const Dismantles, stmt: LIR.CFStmtId) ?Take {
        return self.owned_only_takes.get(stmt);
    }

    pub fn containerOf(self: *const Dismantles, local: LIR.LocalId) ?Container {
        return self.containers.get(local);
    }

    pub fn ownedOnlyContainerOf(self: *const Dismantles, local: LIR.LocalId) ?Container {
        return self.owned_only_containers.get(local);
    }

    pub fn ownedOnlyParamBenefits(self: *const Dismantles, proc: LIR.LirProcSpecId) arc_sig.ParamMask {
        const index = @backingInt(proc);
        if (index >= self.owned_only_param_benefits.len) {
            dismantleInvariant("ARC owned-only benefit lookup exceeded the analyzed source-procedure table");
        }
        return self.owned_only_param_benefits[index];
    }

    pub fn ownedOnlyBindingRoot(self: *const Dismantles, local: LIR.LocalId) ?LIR.LocalId {
        const index = @backingInt(local);
        if (index >= self.owned_only_binding_roots.len) {
            dismantleInvariant("ARC owned-only binding lookup exceeded the analyzed local table");
        }
        const root = self.owned_only_binding_roots[index];
        return if (root == no_index) null else @fromBackingInt(@intCast(root));
    }

    pub fn isTakeBinding(self: *const Dismantles, local: LIR.LocalId) bool {
        const index = @backingInt(local);
        if (index >= self.take_bindings.len) {
            dismantleInvariant("ARC take-binding lookup exceeded the analyzed local table");
        }
        return self.take_bindings[index];
    }

    pub fn fieldRestitutionArg(self: *const Dismantles, stmt: LIR.CFStmtId, position: usize) ?FieldRestitutionArg {
        if (position > std.math.maxInt(u8)) return null;
        return self.field_restitution_args.get(.{ .stmt = stmt, .position = @intCast(position) });
    }

    pub fn projectionUnitOf(self: *const Dismantles, local: LIR.LocalId) ?LIR.LocalId {
        return self.projection_units.get(local);
    }

    pub fn completeTakeRoot(self: *const Dismantles, stmt: LIR.CFStmtId) ?LIR.LocalId {
        return self.complete_takes.get(stmt);
    }

    pub fn projectionAliasRoot(self: *const Dismantles, local: LIR.LocalId) ?LIR.LocalId {
        return self.projection_aliases.get(local);
    }

    pub fn viewOf(self: *const Dismantles, local: LIR.LocalId) ?View {
        return self.views.get(local);
    }
};

fn dismantleInvariant(comptime message: []const u8) noreturn {
    if (@import("builtin").mode == .debug) base.invariant(message, .{});
    unreachable;
}

const State = enum(u8) {
    unknown,
    ineligible,
    candidate,
    /// Borrowed pure same-value alias of a candidate; reads through it
    /// attribute to the root container.
    transparent_alias,
    /// Borrowed struct read out of a candidate, or a borrowed pure alias of
    /// one; reads through it attribute to the root container's slots from
    /// the view's base.
    view,
};

const Read = struct {
    stmt: LIR.CFStmtId,
    target: LIR.LocalId,
    /// The root container's ownership slots the read's result holds.
    slots: SlotRange,
    /// The read defines a view: its result reads the container's slots in
    /// place instead of holding them.
    view: bool = false,
    /// The view the read's source belongs to, `no_index` for a read of the
    /// container itself or of a transparent alias of it.
    through: u32 = no_index,
};

/// A complete field read of a root that dominance canonicalized to an
/// earlier read of the same root and layout. Whether it is a field read of
/// the root or a pure alias of that representative is decided by the
/// representative's plan, so the root defers its classification.
const EquivalencedRead = struct {
    read: Read,
    representative: LIR.LocalId,
};

const MentionEdge = struct {
    stmt: u32,
    next: u32,
};

const LocalEdge = struct {
    local: u32,
    next: u32,
};

/// A tag union whose stored units may be taken through one payload view. Any
/// occurrence of the root other than its definition, a discriminant read, a
/// borrowed pure alias, or the view itself disqualifies it.
const UnionRoot = struct {
    def_stmt: LIR.CFStmtId = @fromBackingInt(@intCast(no_index)),
    def_count: u32 = 0,
    disqualified: bool = false,
    /// The single payload view, `no_index` before one is seen and
    /// `ambiguous_view` once a second view or variant appears.
    view: u32 = no_index,
    variant_index: u32 = 0,
    tag_discriminant: u32 = 0,
};

const ambiguous_view: u32 = no_index - 1;

/// Depth-first ordering state for solving projection representatives
/// before the roots whose deferred reads they settle.
const OrderMark = enum(u8) {
    unvisited,
    visiting,
    ordered,
};

const Candidate = struct {
    def_stmt: LIR.CFStmtId = @fromBackingInt(@intCast(no_index)),
    def_count: u32 = 0,
    join_starts: std.ArrayList(LIR.CFStmtId) = .empty,
    disqualified: bool = false,
    reads: std.ArrayList(Read) = .empty,
    /// Complete reads canonicalized to a dominating representative. Once the
    /// representative commits a plan they are materialized as its aliases and
    /// belong to its occurrence set; otherwise they rejoin `reads` before
    /// this container's flow runs.
    equivalenced_reads: std.ArrayList(EquivalencedRead) = .empty,
    /// Statements consuming or observing the container as one value—moved
    /// into an aggregate, passed to a call, returned, or join-carried. Takes
    /// stay valid as long as no whole use can run after a take, which the
    /// dataflow checks exactly like a borrow of every field at once.
    whole_uses: std.ArrayList(LIR.CFStmtId) = .empty,
    /// Lengths of `reads` and `whole_uses` as classification left them.
    /// Solving appends the occurrences that depend on other candidates'
    /// plans and on owned demand, and truncates back first, so a recursive
    /// call-graph component can solve its candidates again.
    own_read_count: u32 = 0,
    own_whole_use_count: u32 = 0,
};

const Analysis = struct {
    gpa: Allocator,
    store: *const LirStore,
    layouts: *const layout_mod.Store,
    solution: *const arc_solve.Solution,
    /// Exact ARC emission-resource classification produced before solving.
    /// Dismantling may only claim units that the emitter can release.
    rc_local: []const bool,
    state: []State,
    /// Root container local per transparent alias or view, `no_index`
    /// otherwise.
    alias_root: []u32,
    /// First root slot of each view.
    view_base: []u32,
    /// Targets of refcounted reads through a view. Each is a read of the
    /// view's root in its own right, so the view's borrow forest skips it.
    read_through_view: []bool,
    /// The view each view local belongs to: the view a field read defined,
    /// for that read's target and every borrowed alias of it; `no_index`
    /// for any other local.
    view_head: []u32,
    /// For each view, the view its defining read went through, `no_index`
    /// when that read named the container or a transparent alias of it.
    view_parent: []u32,
    /// Each view as a container of its own, keyed by the view. A view whose
    /// definition takes its slots out of its container holds them, so its
    /// reads, every read through views nested in it, and its whole uses are
    /// the occurrences of that container, numbered from the view's base.
    view_candidates: std.AutoHashMapUnmanaged(u32, Candidate),
    /// Tag-union root per ownership-complete borrowed payload view,
    /// `no_index` otherwise. Such a view is the struct candidate through
    /// which the union dismantles; its unit is the root's.
    view_root: []u32,
    /// Tag-union locals that may dismantle through a payload view, keyed by
    /// the root local.
    union_roots: std.AutoHashMapUnmanaged(u32, UnionRoot),
    candidates: std.AutoHashMapUnmanaged(u32, Candidate),
    /// Proc parameters. A parameter solved borrowed may still qualify as an
    /// owned-only candidate: mode-specialized variants re-emit it owned.
    is_param: []const bool,
    /// Root ownership unit for a borrowed struct reached through an explicit
    /// chain of ownership-complete projections, `no_index` otherwise.
    projected_root: []const u32,
    /// Canonical projected struct binding for repeated complete reads of the
    /// same root and committed layout.
    projected_container: []const u32,
    /// Join cells with explicit initialization edges. Each edge supplies a
    /// fresh intact container independently of the other incoming values.
    explicit_init_join: []const bool,
    /// Solved-borrowed bindings whose value reaches an explicitly owned
    /// direct-call or low-level operand. Field-take variants override exactly
    /// these bindings to owned instead of manufacturing a retain at that
    /// ownership boundary.
    owned_demand: []bool,
    /// Same-value aliases propagate an owned demand back to their source.
    demand_aliases: std.ArrayList(struct { source: LIR.LocalId, target: LIR.LocalId }) = .empty,
    /// `demand_aliases` indexed by target: per-local linked lists of sources.
    demand_source_heads: []u32 = &.{},
    demand_source_edges: std.ArrayList(LocalEdge) = .empty,
    demand_stack: std.ArrayList(u32) = .empty,
    /// Every operand mention of a solved-borrowed refcounted local, as a
    /// per-local linked list into `mention_edges`. A binding borrowed through
    /// a candidate's field read observes that field at each of its mentions,
    /// so the take dataflow rejects takes such a mention could follow.
    mention_heads: []u32,
    mention_edges: std.ArrayList(MentionEdge) = .empty,
    /// Per-layout answers of `layoutMayContainBoxyDynamic` for field layouts
    /// the gate has asked about, and the scratch that answers them.
    boxy_dynamic_layouts: collections.DenseMap(layout_mod.Idx, bool),
    layout_visited: std.AutoHashMap(layout_mod.Idx, void),
    layout_stack: std.ArrayList(layout_mod.Idx) = .empty,

    fn deinit(self: *Analysis) void {
        var it = self.candidates.valueIterator();
        while (it.next()) |candidate| {
            candidate.reads.deinit(self.gpa);
            candidate.equivalenced_reads.deinit(self.gpa);
            candidate.join_starts.deinit(self.gpa);
            candidate.whole_uses.deinit(self.gpa);
        }
        self.candidates.deinit(self.gpa);
        var view_it = self.view_candidates.valueIterator();
        while (view_it.next()) |candidate| {
            candidate.reads.deinit(self.gpa);
            candidate.equivalenced_reads.deinit(self.gpa);
            candidate.join_starts.deinit(self.gpa);
            candidate.whole_uses.deinit(self.gpa);
        }
        self.view_candidates.deinit(self.gpa);
        self.union_roots.deinit(self.gpa);
        self.gpa.free(self.view_root);
        self.gpa.free(self.alias_root);
        self.gpa.free(self.view_base);
        self.gpa.free(self.read_through_view);
        self.gpa.free(self.view_head);
        self.gpa.free(self.view_parent);
        self.gpa.free(self.mention_heads);
        self.mention_edges.deinit(self.gpa);
        self.gpa.free(self.state);
        self.gpa.free(self.owned_demand);
        self.demand_aliases.deinit(self.gpa);
        self.gpa.free(self.demand_source_heads);
        self.demand_source_edges.deinit(self.gpa);
        self.demand_stack.deinit(self.gpa);
        self.boxy_dynamic_layouts.deinit();
        self.layout_visited.deinit();
        self.layout_stack.deinit(self.gpa);
    }

    fn demandOwned(self: *Analysis, local: LIR.LocalId) void {
        self.owned_demand[@backingInt(local)] = true;
    }

    fn noteDemandAlias(self: *Analysis, source: LIR.LocalId, target: LIR.LocalId) Error!void {
        try self.demand_aliases.append(self.gpa, .{ .source = source, .target = target });
    }

    /// Index the same-value alias edges by target and close the demands
    /// recorded during classification over them. Later demands close through
    /// `demandOwnedClosed`, so each edge is crossed at most once overall.
    fn closeOwnedDemand(self: *Analysis) Error!void {
        self.demand_source_heads = try self.gpa.alloc(u32, self.owned_demand.len);
        @memset(self.demand_source_heads, no_index);
        try self.demand_source_edges.ensureTotalCapacity(self.gpa, self.demand_aliases.items.len);
        for (self.demand_aliases.items) |edge| {
            const target_index = @backingInt(edge.target);
            self.demand_source_edges.appendAssumeCapacity(.{
                .local = @backingInt(edge.source),
                .next = self.demand_source_heads[target_index],
            });
            self.demand_source_heads[target_index] = @intCast(self.demand_source_edges.items.len - 1);
        }
        for (self.owned_demand, 0..) |demanded, index| {
            if (demanded) try self.demand_stack.append(self.gpa, @intCast(index));
        }
        try self.drainDemand();
    }

    /// Demand `local` owned after classification, closing the demand over
    /// its alias sources. Returns whether `local` was newly demanded.
    fn demandOwnedClosed(self: *Analysis, local: LIR.LocalId) Error!bool {
        const index = @backingInt(local);
        if (self.owned_demand[index]) return false;
        self.owned_demand[index] = true;
        try self.demand_stack.append(self.gpa, index);
        try self.drainDemand();
        return true;
    }

    fn drainDemand(self: *Analysis) Error!void {
        while (self.demand_stack.pop()) |target| {
            var edge_index = self.demand_source_heads[target];
            while (edge_index != no_index) {
                const edge = self.demand_source_edges.items[edge_index];
                if (!self.owned_demand[edge.local]) {
                    self.owned_demand[edge.local] = true;
                    try self.demand_stack.append(self.gpa, edge.local);
                }
                edge_index = edge.next;
            }
        }
    }

    /// Whether the local's layout and binding shape could ever benefit from
    /// dismantling. Cheap: layout answers are memoized per field layout, and the
    /// full per-candidate work only happens for locals that pass.
    fn passesGate(self: *Analysis, local: LIR.LocalId) Error!bool {
        const local_index = @backingInt(local);
        if (local_index >= self.rc_local.len) dismantleInvariant("ARC dismantle resource table did not cover local");
        if (!self.rc_local[local_index]) return false;
        const local_layout = self.layouts.getLayout(self.store.getLocal(local).layout_idx);
        if (local_layout.tag != .struct_) return false;
        if (self.solution.isBorrowed(local) and !self.is_param[local_index] and self.projected_root[local_index] == no_index and self.view_root[local_index] == no_index) return false;
        if (self.solution.isJoinParam(local) and !self.explicit_init_join[local_index]) return false;
        if (self.solution.maybeUninitializedCondition(local) != null) return false;

        const info = self.layouts.getStructInfo(local_layout);
        var any_rc = false;
        for (0..info.fields.len) |i| {
            const field = info.fields.get(@intCast(i));
            if (self.layouts.layoutContainsRefcounted(self.layouts.getLayout(field.layout))) {
                any_rc = true;
                // A residual field is released through its layout's RC
                // helper. Descriptor-driven (`erased_box`) content has no
                // layout helper; its container is released whole instead.
                if (try self.fieldLayoutIsBoxyDynamic(field.layout)) return false;
            }
        }
        return any_rc;
    }

    fn fieldLayoutIsBoxyDynamic(self: *Analysis, layout_idx: layout_mod.Idx) Error!bool {
        if (self.boxy_dynamic_layouts.get(layout_idx)) |known| return known;
        const answer = try layoutMayContainBoxyDynamic(self.gpa, self.layouts, layout_idx, &self.layout_visited, &self.layout_stack);
        try self.boxy_dynamic_layouts.put(layout_idx, answer);
        return answer;
    }

    fn entryOf(self: *Analysis, local: LIR.LocalId) Error!?*Candidate {
        const index = @backingInt(local);
        switch (self.state[index]) {
            .ineligible, .transparent_alias, .view => return null,
            .candidate => return self.candidates.getPtr(index).?,
            .unknown => {
                if (!try self.passesGate(local)) {
                    self.state[index] = .ineligible;
                    return null;
                }
                self.state[index] = .candidate;
                const slot = try self.candidates.getOrPut(self.gpa, index);
                slot.value_ptr.* = .{};
                return slot.value_ptr;
            },
        }
    }

    /// The container a source local stands for: itself, or its alias root.
    fn resolveRoot(self: *Analysis, local: LIR.LocalId) LIR.LocalId {
        const index = @backingInt(local);
        if (self.projected_container[index] != no_index and self.projected_container[index] != index) {
            return @fromBackingInt(@intCast(self.projected_container[index]));
        }
        if (self.state[index] == .transparent_alias or self.state[index] == .view) {
            return @fromBackingInt(@intCast(self.alias_root[index]));
        }
        return local;
    }

    fn isView(self: *const Analysis, local: LIR.LocalId) bool {
        return self.state[@backingInt(local)] == .view;
    }

    fn disqualify(self: *Analysis, local: LIR.LocalId) void {
        const root = self.resolveRoot(local);
        const index = @backingInt(root);
        if (self.state[index] == .candidate) {
            if (self.candidates.getPtr(index)) |candidate| candidate.disqualified = true;
        }
        self.state[index] = .ineligible;
    }

    /// Records an operand mention of a solved-borrowed refcounted local.
    fn noteMention(self: *Analysis, stmt: LIR.CFStmtId, local: LIR.LocalId) Error!void {
        const index = @backingInt(local);
        if (!self.rc_local[index] or !self.solution.isBorrowed(local)) return;
        try self.mention_edges.append(self.gpa, .{
            .stmt = @backingInt(stmt),
            .next = self.mention_heads[index],
        });
        self.mention_heads[index] = @intCast(self.mention_edges.items.len - 1);
    }

    /// Any occurrence that is not a field read: the local (or the container
    /// it aliases) cannot dismantle.
    fn useWhole(self: *Analysis, stmt: LIR.CFStmtId, local: LIR.LocalId) Error!void {
        try self.noteMention(stmt, local);
        try self.touchUnion(local);
        // A view's other uses are borrow observations of its slots, which
        // its borrow forest replays against the root's takes. The view
        // itself cannot dismantle.
        if (self.isView(local)) {
            self.viewCandidateOf(local).disqualified = true;
            return;
        }
        self.disqualify(local);
    }

    /// The container a view local stands for once its view holds its slots.
    fn viewCandidateOf(self: *Analysis, local: LIR.LocalId) *Candidate {
        return self.view_candidates.getPtr(self.view_head[@backingInt(local)]) orelse
            dismantleInvariant("ARC dismantle view had no container entry");
    }

    /// A whole-value use of the container as an operand—moved into an
    /// aggregate or a call, returned, or join-carried. The container stays
    /// eligible; the dataflow rejects takes that could run before it.
    fn useWholeAt(self: *Analysis, local: LIR.LocalId, stmt: LIR.CFStmtId) Error!void {
        try self.noteMention(stmt, local);
        try self.touchUnion(local);
        if (self.isView(local)) {
            try self.viewCandidateOf(local).whole_uses.append(self.gpa, stmt);
            return;
        }
        const root = self.resolveRoot(local);
        const candidate = (try self.entryOf(root)) orelse return;
        try candidate.whole_uses.append(self.gpa, stmt);
    }

    /// The tag-union root a local's ownership unit belongs to, when that root
    /// could dismantle through a payload view: refcounted, owned outright, and
    /// neither a join parameter nor conditionally initialized.
    fn unionRootOf(self: *Analysis, local: LIR.LocalId) ?u32 {
        const unit = self.solution.unitLocalOf(local);
        const unit_index = @backingInt(unit);
        if (unit_index >= self.rc_local.len or !self.rc_local[unit_index]) return null;
        if (self.layouts.getLayout(self.store.getLocal(unit).layout_idx).tag != .tag_union) return null;
        if (self.solution.isBorrowed(unit) or self.solution.isJoinParam(unit)) return null;
        if (self.solution.maybeUninitializedCondition(unit) != null) return null;
        return unit_index;
    }

    fn unionEntryOf(self: *Analysis, root_index: u32) Error!*UnionRoot {
        const slot = try self.union_roots.getOrPut(self.gpa, root_index);
        if (!slot.found_existing) slot.value_ptr.* = .{};
        return slot.value_ptr;
    }

    /// A whole use of a tag union (or of an alias sharing its unit) ends its
    /// chance to dismantle.
    fn touchUnion(self: *Analysis, local: LIR.LocalId) Error!void {
        const root_index = self.unionRootOf(local) orelse return;
        const entry = try self.unionEntryOf(root_index);
        entry.disqualified = true;
    }

    /// A borrowed payload view of a union root that owns every refcounted
    /// byte of the union's active variant. The view is the struct candidate
    /// the union dismantles through; a second view or variant leaves the
    /// union whole-released.
    fn notePayloadView(self: *Analysis, target: LIR.LocalId, op: anytype) Error!bool {
        if (!self.solution.isBorrowed(target)) return false;
        const root_index = self.unionRootOf(op.source) orelse return false;
        const projection = encodeProjection(op).?;
        if (!projectionOwnsAllRc(self.store, self.layouts, op.source, target, projection)) return false;
        const entry = try self.unionEntryOf(root_index);
        if (entry.view == no_index) {
            entry.view = @backingInt(target);
            entry.variant_index = op.variant_index;
            entry.tag_discriminant = op.tag_discriminant;
        } else {
            entry.view = ambiguous_view;
        }
        self.view_root[@backingInt(target)] = root_index;
        return true;
    }

    /// A definition of `local` by `stmt`: one value-producing assignment,
    /// or an explicit initialization edge of a join cell.
    fn noteDef(self: *Analysis, local: LIR.LocalId, stmt: LIR.CFStmtId) Error!void {
        const index = @backingInt(local);
        if (self.projected_container[index] != no_index and self.projected_container[index] != index) return;
        if (self.state[index] == .transparent_alias or self.state[index] == .view) {
            // A second definition of an alias re-points it; the root can no
            // longer attribute its reads.
            self.disqualify(local);
            self.state[index] = .ineligible;
            return;
        }
        if (self.unionRootOf(local)) |root_index| {
            if (root_index == index) {
                const entry = try self.unionEntryOf(root_index);
                entry.def_count += 1;
                entry.def_stmt = stmt;
            }
        }
        const candidate = (try self.entryOf(local)) orelse return;
        if (self.solution.isJoinParam(local)) {
            const definition = self.store.getCFStmtImmediate(stmt);
            if (definition.* != .set_local or definition.set_local.mode != .initialize_join_param) {
                candidate.disqualified = true;
                return;
            }
            try candidate.join_starts.append(self.gpa, definition.set_local.next);
        }
        candidate.def_count += 1;
        if (candidate.def_count > 1 and !self.solution.isJoinParam(local)) {
            candidate.disqualified = true;
        } else {
            candidate.def_stmt = stmt;
        }
    }

    /// The root container's slots a field read of `source` addresses.
    fn readSlots(self: *const Analysis, source: LIR.LocalId, field_idx: u32, target: LIR.LocalId) SlotRange {
        const view_base: u32 = if (self.isView(source)) self.view_base[@backingInt(source)] else 0;
        return .{
            .first = view_base + self.layouts.ownershipSlotBase(self.store.getLocal(source).layout_idx, field_idx),
            .count = self.layouts.ownershipSlotCount(self.store.getLocal(target).layout_idx),
        };
    }

    /// Records a field read of `source`, and returns whether it defined a
    /// view, whose definition is then already recorded.
    fn noteFieldRead(self: *Analysis, stmt: LIR.CFStmtId, source: LIR.LocalId, field_idx: u32, target: LIR.LocalId) Error!bool {
        const target_index = @backingInt(target);
        const through_view = self.isView(source);
        const through: u32 = if (through_view) self.view_head[@backingInt(source)] else no_index;
        // A refcounted field read through a borrowed source observes the
        // source's stored unit at the read itself when the target retains.
        // A read through a view is a read of the view's root in its own
        // right instead.
        if (self.rc_local[target_index]) {
            if (through_view) {
                self.read_through_view[target_index] = true;
            } else {
                try self.noteMention(stmt, source);
            }
        }
        const root = self.resolveRoot(source);
        const representative_index = self.projected_container[target_index];
        if (representative_index != no_index and representative_index != target_index) {
            // The root settles this read against the representative's plan,
            // so no view it went through can hold it as its own read.
            var enclosing = through;
            while (enclosing != no_index) : (enclosing = self.view_parent[enclosing]) {
                (self.view_candidates.getPtr(enclosing) orelse dismantleInvariant("ARC dismantle view had no container entry")).disqualified = true;
            }
            try self.noteEquivalencedRead(stmt, root, self.readSlots(source, field_idx, target), target, @fromBackingInt(@intCast(representative_index)));
            return false;
        }
        const candidate = (try self.entryOf(root)) orelse return false;
        const slots = self.readSlots(source, field_idx, target);
        const view = self.state[target_index] == .unknown and
            self.rc_local[target_index] and
            self.solution.isBorrowed(target) and
            !self.is_param[target_index] and
            self.layouts.getLayout(self.store.getLocal(target).layout_idx).tag == .struct_ and
            self.projected_root[target_index] == no_index and
            self.view_root[target_index] == no_index;
        if (view) {
            self.state[target_index] = .view;
            self.alias_root[target_index] = @backingInt(root);
            self.view_base[target_index] = slots.first;
            self.view_head[target_index] = target_index;
            self.view_parent[target_index] = through;
            try self.view_candidates.put(self.gpa, target_index, .{ .def_stmt = stmt, .def_count = 1 });
        }
        try candidate.reads.append(self.gpa, .{
            .stmt = stmt,
            .target = target,
            .slots = slots,
            .view = view,
            .through = through,
        });
        // Every view the read went through reads the same slots, from its
        // own base, as the container it becomes when it holds them.
        var enclosing = through;
        while (enclosing != no_index) : (enclosing = self.view_parent[enclosing]) {
            const enclosing_candidate = self.view_candidates.getPtr(enclosing) orelse
                dismantleInvariant("ARC dismantle view had no container entry");
            try enclosing_candidate.reads.append(self.gpa, .{
                .stmt = stmt,
                .target = target,
                .slots = .{ .first = slots.first - self.view_base[enclosing], .count = slots.count },
                .view = view,
                .through = through,
            });
        }
        return view;
    }

    /// A complete read canonicalized to a dominating representative. If the
    /// representative commits a plan, the read is materialized as a pure
    /// alias of it and is scanned as that alias now: a target emitted owned
    /// is a whole use of the representative (an owned-demanded borrowed
    /// target joins it once demand is closed), and a borrowed target reads
    /// through it.
    /// The root defers the read until the representative's plan is known;
    /// the representative's plan never depends on the root's, so solving
    /// representatives first settles every deferred read exactly once. The
    /// read's mention stays on the root: the representative's defining read
    /// dominates this one, so the root's flow already observes the field
    /// before it, and a mention of the representative here would poison the
    /// root's own take when the read rejoins its field reads.
    fn noteEquivalencedRead(self: *Analysis, stmt: LIR.CFStmtId, root: LIR.LocalId, slots: SlotRange, target: LIR.LocalId, representative: LIR.LocalId) Error!void {
        if (!self.solution.isBorrowed(target)) {
            if (try self.entryOf(representative)) |candidate| try candidate.whole_uses.append(self.gpa, stmt);
        }
        const root_candidate = (try self.entryOf(root)) orelse return;
        try root_candidate.equivalenced_reads.append(self.gpa, .{
            .read = .{ .stmt = stmt, .target = target, .slots = slots },
            .representative = representative,
        });
    }

    fn noteAliasDef(self: *Analysis, stmt: LIR.CFStmtId, target: LIR.LocalId, source: LIR.LocalId) Error!void {
        const target_index = @backingInt(target);
        if (self.state[target_index] == .transparent_alias or self.state[target_index] == .view) {
            // Redefinition of an existing alias: neither its old nor its new
            // root can attribute reads through it.
            self.disqualify(target);
            self.disqualify(source);
            self.state[target_index] = .ineligible;
            return;
        }
        if (self.isView(source)) {
            const source_index = @backingInt(source);
            if (self.solution.isBorrowed(target) and self.state[target_index] == .unknown) {
                // A borrowed alias of a view is the same view.
                self.state[target_index] = .view;
                self.alias_root[target_index] = self.alias_root[source_index];
                self.view_base[target_index] = self.view_base[source_index];
                self.view_head[target_index] = self.view_head[source_index];
                return;
            }
            // An owned binding retains the view's slots here, observing
            // them, and holds that retained unit as its own.
            try self.noteMention(stmt, source);
            try self.touchUnion(source);
            try self.viewCandidateOf(source).whole_uses.append(self.gpa, stmt);
            if (self.solution.isBorrowed(target)) {
                self.disqualify(target);
            } else {
                try self.noteDef(target, stmt);
            }
            return;
        }
        const root = self.resolveRoot(source);
        // A borrowed pure alias shares its source's unit. Its liveness leader
        // is the container for an owned struct but the union root for a
        // payload view, so the unit chain is the same-value test that covers
        // both.
        const explicit_projected_alias = self.projected_container[target_index] == @backingInt(root);
        const transparent = self.solution.isBorrowed(target) and
            (explicit_projected_alias or self.solution.leaderOf(target) == root or self.solution.unitLocalOf(target) == root) and
            ((try self.entryOf(root)) != null);
        if (transparent) {
            // The alias target itself can never be a container.
            if (self.state[target_index] == .candidate) {
                if (self.candidates.getPtr(target_index)) |candidate| candidate.disqualified = true;
            }
            self.state[target_index] = .transparent_alias;
            self.alias_root[target_index] = @backingInt(root);
        } else {
            // An owned same-value binding is a path-local whole use. It can
            // move an intact container on this edge, while takes on mutually
            // exclusive edges keep their exact residual states. A borrowed
            // alias sharing a tag union's unit carries its variant knowledge
            // and is no use of the union.
            const union_alias = self.solution.isBorrowed(target) and
                self.unionRootOf(source) != null and
                self.unionRootOf(source) == self.unionRootOf(target);
            if (!union_alias) try self.touchUnion(source);
            if (try self.entryOf(root)) |candidate| try candidate.whole_uses.append(self.gpa, stmt);
            if (self.solution.isBorrowed(target)) {
                self.disqualify(target);
            } else {
                // The owned binding holds its own unit, received by this
                // statement whether it moved the source's unit or retained
                // a new one, so it is a container defined here.
                try self.noteDef(target, stmt);
            }
        }
    }
};

/// Candidate-local backward may-observation solution. Each procedure's
/// statement graph is reversed once, as the field walk follows it; every
/// candidate then walks back only from its own reads, and each field bit
/// crosses each edge at most once. Edge masks end precisely the old field
/// lifetimes restored on that edge.
const FutureFields = struct {
    const Node = struct {
        stmt: LIR.CFStmtId,
        observed: u64 = 0,
        pending: u64 = 0,
    };

    /// One edge of the field walk, stored at its target.
    const Predecessor = struct {
        target: LIR.CFStmtId,
        source: LIR.CFStmtId,
        /// The switch branch index for `.branch`, the rebound local for `.rebind`.
        label: u32,
        kind: Kind,

        const Kind = enum(u8) {
            plain,
            branch,
            default_branch,
            /// A `set_local` edge, which a candidate rebinding that local ends.
            rebind,
        };
    };

    /// One procedure's reversed field-walk graph, as spans of the shared pools.
    const ProcGraph = struct {
        predecessors_start: u32,
        predecessors_len: u32,
    };

    indices: collections.DenseMap(LIR.CFStmtId, u32),
    nodes: std.ArrayList(Node) = .empty,
    work: std.ArrayList(u32) = .empty,
    /// Every built procedure's edges, grouped by procedure and sorted by
    /// target within each group.
    predecessors: std.ArrayList(Predecessor) = .empty,
    seen: collections.DenseMap(LIR.CFStmtId, void),
    stack: std.ArrayList(LIR.CFStmtId) = .empty,

    fn init(gpa: Allocator) FutureFields {
        return .{
            .indices = collections.DenseMap(LIR.CFStmtId, u32).init(gpa),
            .seen = collections.DenseMap(LIR.CFStmtId, void).init(gpa),
        };
    }

    fn deinit(self: *FutureFields, gpa: Allocator) void {
        self.indices.deinit();
        self.nodes.deinit(gpa);
        self.work.deinit(gpa);
        self.predecessors.deinit(gpa);
        self.seen.deinit();
        self.stack.deinit(gpa);
    }

    fn addPredecessor(self: *FutureFields, gpa: Allocator, source: LIR.CFStmtId, target: LIR.CFStmtId, kind: Predecessor.Kind, label: u32) Error!void {
        try self.predecessors.append(gpa, .{ .target = target, .source = source, .label = label, .kind = kind });
    }

    fn predecessorTargetLessThan(_: void, a: Predecessor, b: Predecessor) bool {
        return @backingInt(a.target) < @backingInt(b.target);
    }

    /// Reverse the field walk over every statement reachable from `roots`.
    /// Join bodies are entered through the jumps that name them, and a switch
    /// continuation is not an edge of the walk; both are still visited so
    /// that every statement a read can reach has its incoming edges.
    fn buildProcGraph(
        self: *FutureFields,
        gpa: Allocator,
        store: *const LirStore,
        roots: []const LIR.CFStmtId,
        joins: *const collections.DenseMap(u32, LIR.CFStmtId),
    ) Error!ProcGraph {
        const predecessors_start = self.predecessors.items.len;
        self.seen.clearRetainingCapacity();
        self.stack.clearRetainingCapacity();
        try self.stack.appendSlice(gpa, roots);
        while (self.stack.pop()) |cursor| {
            if ((try self.seen.getOrPut(cursor)).found_existing) continue;
            const reached = self.stack.items.len;
            try body_clone.appendSuccessors(store, &self.stack, cursor, gpa);
            switch (store.getCFStmtImmediate(cursor).*) {
                .set_local => |stmt| try self.addPredecessor(gpa, cursor, stmt.next, .rebind, @backingInt(stmt.target)),
                .join => |stmt| try self.addPredecessor(gpa, cursor, stmt.remainder, .plain, 0),
                .jump => |stmt| {
                    if (joins.get(@backingInt(stmt.target))) |body| try self.addPredecessor(gpa, cursor, body, .plain, 0);
                },
                .switch_stmt => |stmt| {
                    const branches = store.getCFSwitchBranches(stmt.branches);
                    for (0..GuardedList.borrowLen(branches)) |branch_index| {
                        try self.addPredecessor(gpa, cursor, GuardedList.at(branches, branch_index).body, .branch, @intCast(branch_index));
                    }
                    try self.addPredecessor(gpa, cursor, stmt.default_branch, .default_branch, 0);
                },
                .init_uninitialized,
                .assign_ref,
                .assign_literal,
                .assign_call,
                .assign_call_erased,
                .assign_packed_erased_fn,
                .assign_boxy_desc_ref,
                .assign_boxy_dict_ref,
                .assign_boxy_box,
                .assign_boxy_record_update,
                .assign_boxy_unbox,
                .assign_boxy_adapt,
                .assign_boxy_inspect,
                .assign_boxy_eq,
                .assign_boxy_hash,
                .assign_boxy_tag,
                .assign_boxy_tag_payload,
                .boxy_tag_match,
                .assign_call_dict,
                .assign_low_level,
                .assign_list,
                .assign_struct,
                .assign_tag,
                .store_struct,
                .store_tag,
                .debug,
                .expect,
                .expect_err,
                .runtime_error,
                .comptime_exhaustiveness_failed,
                .comptime_branch_taken,
                .incref,
                .decref,
                .decref_if_initialized,
                .free,
                .switch_initialized_payload,
                .str_match,
                .str_match_set,
                .ret,
                .crash,
                => for (self.stack.items[reached..]) |next| try self.addPredecessor(gpa, cursor, next, .plain, 0),
            }
        }
        const group = self.predecessors.items[predecessors_start..];
        std.sort.pdq(Predecessor, group, {}, predecessorTargetLessThan);
        return .{
            .predecessors_start = @intCast(predecessors_start),
            .predecessors_len = @intCast(group.len),
        };
    }

    fn predecessorsOf(self: *const FutureFields, graph: ProcGraph, stmt: LIR.CFStmtId) []const Predecessor {
        const group = self.predecessors.items[graph.predecessors_start..][0..graph.predecessors_len];
        const target = @backingInt(stmt);
        var low: usize = 0;
        var high: usize = group.len;
        while (low < high) {
            const mid = low + (high - low) / 2;
            if (@backingInt(group[mid].target) < target) low = mid + 1 else high = mid;
        }
        var end = low;
        while (end < group.len and group[end].target == stmt) end += 1;
        return group[low..end];
    }

    fn clear(self: *FutureFields) void {
        self.indices.clearRetainingCapacity();
        self.nodes.clearRetainingCapacity();
        self.work.clearRetainingCapacity();
    }

    fn node(self: *FutureFields, gpa: Allocator, stmt: LIR.CFStmtId) Error!u32 {
        if (self.indices.get(stmt)) |index| return index;
        const index: u32 = @intCast(self.nodes.items.len);
        try self.nodes.ensureUnusedCapacity(gpa, 1);
        try self.indices.put(stmt, index);
        self.nodes.appendAssumeCapacity(.{ .stmt = stmt });
        return index;
    }

    fn observe(self: *FutureFields, gpa: Allocator, stmt: LIR.CFStmtId, bits: u64) Error!void {
        if (bits == 0) return;
        const index = try self.node(gpa, stmt);
        const target = &self.nodes.items[index];
        const added = bits & ~target.observed;
        if (added == 0) return;
        if (target.pending == 0) try self.work.append(gpa, index);
        target.observed |= added;
        target.pending |= added;
    }

    fn observed(self: *const FutureFields, stmt: LIR.CFStmtId) u64 {
        const index = self.indices.get(stmt) orelse return 0;
        return self.nodes.items[index].observed;
    }

    fn compute(
        self: *FutureFields,
        gpa: Allocator,
        store: *const LirStore,
        solution: *const arc_solve.Solution,
        local: LIR.LocalId,
        graph: ProcGraph,
        reads: *const collections.DenseMap(LIR.CFStmtId, ReadKind),
        receipts: []const FieldRestitution,
        redefinition: ?LIR.CFStmtId,
    ) Error!void {
        self.clear();
        // Past the container's own single definition, reached through a loop
        // back edge, every read observes the next iteration's value, so that
        // definition observes nothing and no observation crosses it.
        var read_it = reads.constIterator();
        while (read_it.next()) |read| {
            if (redefinition) |def_stmt| if (read.key_ptr.* == def_stmt) continue;
            try self.observe(gpa, read.key_ptr.*, read.value_ptr.bit);
        }
        while (self.work.pop()) |index| {
            const bits = self.nodes.items[index].pending;
            self.nodes.items[index].pending = 0;
            for (self.predecessorsOf(graph, self.nodes.items[index].stmt)) |edge| {
                if (redefinition) |def_stmt| if (edge.source == def_stmt) continue;
                const preserved: u64 = switch (edge.kind) {
                    .plain => ~@as(u64, 0),
                    .rebind => if (edge.label == @backingInt(local)) 0 else ~@as(u64, 0),
                    .branch => if (receipts.len == 0) ~@as(u64, 0) else blk: {
                        const branches = store.getCFSwitchBranches(store.getCFStmtImmediate(edge.source).switch_stmt.branches);
                        break :blk ~restoredFieldMaskForBranch(solution, receipts, edge.source, GuardedList.at(branches, edge.label).value);
                    },
                    .default_branch => if (receipts.len == 0) ~@as(u64, 0) else ~restoredFieldMaskForDefault(solution, store, receipts, edge.source),
                };
                try self.observe(gpa, edge.source, bits & preserved);
            }
        }
    }
};

/// Independent test oracle: the original per-read traversal.
/// Proves whether a stored field is observed again in this definition. Every
/// reachable statement is visited once; explicit reinitialization and outcome
/// restitution end the old field lifetime on their respective edges.
fn fieldObservedAfter(
    gpa: Allocator,
    store: *const LirStore,
    solution: *const arc_solve.Solution,
    local: LIR.LocalId,
    bit: u64,
    start: LIR.CFStmtId,
    reads: *const collections.DenseMap(LIR.CFStmtId, ReadKind),
    joins: *const collections.DenseMap(u32, LIR.CFStmtId),
    receipts: []const FieldRestitution,
    redefinition: ?LIR.CFStmtId,
    visits: *usize,
) Error!bool {
    var seen = collections.DenseMap(LIR.CFStmtId, void).init(gpa);
    defer seen.deinit();
    var work = std.ArrayList(LIR.CFStmtId).empty;
    defer work.deinit(gpa);
    try work.append(gpa, start);
    while (work.pop()) |cursor| {
        if ((try seen.getOrPut(cursor)).found_existing) continue;
        visits.* += 1;
        // Past the container's own single definition, reached through a
        // loop back edge, every read observes the next iteration's value.
        if (redefinition) |def_stmt| if (cursor == def_stmt) continue;
        if (reads.get(cursor)) |read| {
            if (read.bit & bit != 0) return true;
        }
        switch (store.getCFStmtImmediate(cursor).*) {
            .set_local => |stmt| {
                if (stmt.target != local) try work.append(gpa, stmt.next);
            },
            .join => |stmt| try work.append(gpa, stmt.remainder),
            .jump => |stmt| {
                if (joins.get(@backingInt(stmt.target))) |body| try work.append(gpa, body);
            },
            .switch_stmt => |stmt| {
                const branches = store.getCFSwitchBranches(stmt.branches);
                for (0..GuardedList.borrowLen(branches)) |index| {
                    const branch = GuardedList.at(branches, index);
                    if (restoredFieldMaskForBranch(solution, receipts, cursor, branch.value) & bit == 0) {
                        try work.append(gpa, branch.body);
                    }
                }
                if (restoredFieldMaskForDefault(solution, store, receipts, cursor) & bit == 0) {
                    try work.append(gpa, stmt.default_branch);
                }
            },
            .init_uninitialized,
            .assign_ref,
            .assign_literal,
            .assign_call,
            .assign_call_erased,
            .assign_packed_erased_fn,
            .assign_boxy_desc_ref,
            .assign_boxy_dict_ref,
            .assign_boxy_box,
            .assign_boxy_record_update,
            .assign_boxy_unbox,
            .assign_boxy_adapt,
            .assign_boxy_inspect,
            .assign_boxy_eq,
            .assign_boxy_hash,
            .assign_boxy_tag,
            .assign_boxy_tag_payload,
            .boxy_tag_match,
            .assign_call_dict,
            .assign_low_level,
            .assign_list,
            .assign_struct,
            .assign_tag,
            .store_struct,
            .store_tag,
            .debug,
            .expect,
            .expect_err,
            .runtime_error,
            .comptime_exhaustiveness_failed,
            .comptime_branch_taken,
            .incref,
            .decref,
            .decref_if_initialized,
            .free,
            .switch_initialized_payload,
            .str_match,
            .str_match_set,
            .ret,
            .crash,
            => try body_clone.appendSuccessors(store, &work, cursor, gpa),
        }
    }
    return false;
}

/// Index one procedure's structural CFG. Jump targets use procedure-local
/// identities, so a field-use walk must never resolve them through another
/// procedure's join declaration. `stmts` is every statement reachable from the
/// procedure's body.
fn collectProcJoinBodies(
    store: *const LirStore,
    stmts: []const LIR.CFStmtId,
    joins: *collections.DenseMap(u32, LIR.CFStmtId),
) Error!void {
    for (stmts) |stmt_id| {
        const stmt = store.getCFStmtImmediate(stmt_id);
        if (stmt.* != .join) continue;
        const entry = try joins.getOrPut(@backingInt(stmt.join.id));
        if (entry.found_existing) {
            if (entry.value_ptr.* != stmt.join.body) dismantleInvariant("procedure contains two bodies for one join identity");
        } else {
            entry.value_ptr.* = stmt.join.body;
        }
    }
}

test "future field uses resolve joins in their procedure" {
    const gpa = std.testing.allocator;
    var store = LirStore.init(gpa);
    defer store.deinit();
    const container = try store.addLocal(.{ .layout_idx = .str });
    const field = try store.addLocal(.{ .layout_idx = .str });
    var join_ids = body_clone.JoinParamIndex.init(gpa);
    defer join_ids.deinit();
    const join_id = join_ids.freshJoinPoint();
    const exit = try store.addCFStmt(.{ .ret = .{ .value = field } }, .test_fixture);
    const back_edge = try store.addCFStmt(.{ .jump = .{ .target = join_id } }, .test_fixture);
    const read = try store.addCFStmt(.{ .assign_ref = .{
        .target = field,
        .op = .field(container, 0),
        .next = back_edge,
    } }, .test_fixture);
    const loop_proc = try store.addCFStmt(.{ .join = .{
        .id = join_id,
        .params = .empty(),
        .body = read,
        .remainder = back_edge,
    } }, .test_fixture);
    const other_proc = try store.addCFStmt(.{ .join = .{
        .id = join_id,
        .params = .empty(),
        .body = exit,
        .remainder = back_edge,
    } }, .test_fixture);
    var loop_joins = collections.DenseMap(u32, LIR.CFStmtId).init(gpa);
    defer loop_joins.deinit();
    var other_joins = collections.DenseMap(u32, LIR.CFStmtId).init(gpa);
    defer other_joins.deinit();
    var proc_stmts = std.ArrayList(LIR.CFStmtId).empty;
    defer proc_stmts.deinit(gpa);
    try arc_solve.collectProcStatements(gpa, &store, loop_proc, &proc_stmts);
    try collectProcJoinBodies(&store, proc_stmts.items, &loop_joins);
    try arc_solve.collectProcStatements(gpa, &store, other_proc, &proc_stmts);
    try collectProcJoinBodies(&store, proc_stmts.items, &other_joins);
    try std.testing.expectEqual(read, loop_joins.get(0).?);
    try std.testing.expectEqual(exit, other_joins.get(0).?);

    var reads = collections.DenseMap(LIR.CFStmtId, ReadKind).init(gpa);
    defer reads.deinit();
    try reads.put(read, .{ .bit = 1, .consuming = true });
    const solution: arc_solve.Solution = undefined;
    var future = FutureFields.init(gpa);
    defer future.deinit(gpa);
    const loop_graph = try future.buildProcGraph(gpa, &store, &.{loop_proc}, &loop_joins);
    const other_graph = try future.buildProcGraph(gpa, &store, &.{other_proc}, &other_joins);
    try future.compute(gpa, &store, &solution, container, loop_graph, &reads, &.{}, null);
    try std.testing.expect(future.observed(back_edge) & 1 != 0);
    try future.compute(gpa, &store, &solution, container, other_graph, &reads, &.{}, null);
    try std.testing.expect(future.observed(back_edge) & 1 == 0);
}

test "future field observations agree with per-read traversal across joins rebinds and outcomes" {
    const gpa = std.testing.allocator;
    var store = LirStore.init(gpa);
    defer store.deinit();
    const local = try store.addLocal(.{ .layout_idx = .str });
    const other = try store.addLocal(.{ .layout_idx = .str });
    const exit = try store.addCFStmt(.{ .ret = .{ .value = other } }, .test_fixture);
    const boundary = try store.addCFStmt(.runtime_error, .test_fixture);
    // Reserve the two procedure-local join identities before building their
    // bodies, which contain forward references to the enclosing joins.
    var join_ids: [2]LIR.JoinPointId = undefined;
    for (&join_ids, 0..) |*id, index| id.* = @fromBackingInt(@intCast(index));
    const outer_id = join_ids[0];
    const nested_id = join_ids[1];
    const jump = try store.addCFStmt(.{ .jump = .{ .target = outer_id } }, .test_fixture);
    const rebind = try store.addCFStmt(.{ .set_local = .{
        .target = local,
        .value = other,
        .mode = .initialize_join_param,
        .next = jump,
    } }, .test_fixture);
    const read = try store.addCFStmt(.{ .assign_ref = .{
        .target = other,
        .op = .field(local, 0),
        .next = rebind,
    } }, .test_fixture);
    const branch = try store.addCFStmt(.{ .switch_stmt = .{
        .cond = other,
        .branches = try store.addCFSwitchBranches(&.{
            .{ .value = 0, .body = read },
            .{ .value = 1, .body = boundary },
        }),
        .default_branch = read,
    } }, .test_fixture);
    const nested_jump = try store.addCFStmt(.{ .jump = .{ .target = nested_id } }, .test_fixture);
    const nested = try store.addCFStmt(.{ .join = .{
        .id = nested_id,
        .params = .empty(),
        .body = branch,
        .remainder = nested_jump,
    } }, .test_fixture);
    const outer = try store.addCFStmt(.{ .join = .{
        .id = outer_id,
        .params = .empty(),
        .body = nested,
        .remainder = jump,
    } }, .test_fixture);
    var joins = collections.DenseMap(u32, LIR.CFStmtId).init(gpa);
    defer joins.deinit();
    try joins.put(@backingInt(outer_id), nested);
    try joins.put(@backingInt(nested_id), branch);
    var reads = collections.DenseMap(LIR.CFStmtId, ReadKind).init(gpa);
    defer reads.deinit();
    try reads.put(read, .{ .bit = 1, .consuming = false });
    // Models the already-expanded borrowed descendant/whole-value mentions.
    try reads.put(rebind, .{ .bit = 2, .consuming = false });
    const starts = [_]LIR.CFStmtId{ exit, boundary, jump, rebind, read, branch, nested_jump, nested, outer };
    var probes: [starts.len]LIR.CFStmtId = undefined;
    for (starts, &probes) |start, *probe| {
        probe.* = try store.addCFStmt(.{ .assign_ref = .{
            .target = other,
            .op = .field(local, 2),
            .next = start,
        } }, .test_fixture);
        try reads.put(probe.*, .{ .bit = 4, .consuming = true });
    }
    // These queries consume only join membership and the outcome signature
    // table; keep unrelated solver state undefined so accidental access fails.
    var solution: arc_solve.Solution = undefined;
    solution.sigs = &.{};
    var leaders = [_]u32{ 0, 1 };
    solution.leader = &leaders;
    solution.join_param = try std.bit_set.DynamicBitSetUnmanaged.initEmpty(gpa, 2);
    defer solution.join_param.deinit(gpa);
    var outcomes = [_]arc_sig.Outcome{
        .{ .discriminant = 0, .restituted_params = 0 },
        .{ .discriminant = 1, .restituted_params = 0 },
        .{ .discriminant = 2, .restituted_params = 0 },
        .{ .discriminant = 3, .restituted_params = 0 },
    };
    solution.outcomes = &outcomes;
    const receipts = [_]FieldRestitution{.{
        .call_arg = .{ .stmt = read, .position = 0 },
        .projection = read,
        .switch_stmt = branch,
        .field_mask = 1,
        .outcomes = .{ .start = 0, .len = 4 },
    }};
    var future = FutureFields.init(gpa);
    defer future.deinit(gpa);
    for (0..128) |configuration| {
        for (&outcomes, 0..) |*outcome, index| outcome.restituted_params = @intCast((configuration >> @intCast(index)) & 1);
        if (configuration & 16 != 0) solution.join_param.set(0) else solution.join_param.unset(0);
        store.getCFStmtPtr(rebind).set_local.target = if (configuration & 32 != 0) local else other;
        try joins.put(0, if (configuration & 64 != 0) nested else exit);
        const graph = try future.buildProcGraph(gpa, &store, &probes, &joins);
        try future.compute(gpa, &store, &solution, local, graph, &reads, &receipts, null);
        for (starts) |start| {
            for ([_]u64{ 1, 2, 4 }) |bit| {
                var visits: usize = 0;
                const expected = try fieldObservedAfter(gpa, &store, &solution, local, bit, start, &reads, &joins, &receipts, null, &visits);
                try std.testing.expectEqual(expected, future.observed(start) & bit != 0);
            }
        }
    }
}

test "future field observations share one reversed CFG across many consuming reads" {
    const gpa = std.testing.allocator;
    var store = LirStore.init(gpa);
    defer store.deinit();
    const local = try store.addLocal(.{ .layout_idx = .str });
    var start = try store.addCFStmt(.{ .ret = .{ .value = local } }, .test_fixture);
    const exit = start;
    // Mutually exclusive consuming reads share a long continuation observing
    // another field: the old DFS cannot exit early for any of their queries.
    for (0..255) |_| {
        start = try store.addCFStmt(.{ .assign_ref = .{
            .target = local,
            .op = .local(local),
            .next = start,
        } }, .test_fixture);
    }
    var reads = collections.DenseMap(LIR.CFStmtId, ReadKind).init(gpa);
    defer reads.deinit();
    try reads.put(exit, .{ .bit = 2, .consuming = false });
    var probes = std.ArrayList(LIR.CFStmtId).empty;
    defer probes.deinit(gpa);
    var joins = collections.DenseMap(u32, LIR.CFStmtId).init(gpa);
    var solution: arc_solve.Solution = undefined;
    solution.leader = &.{};
    solution.join_param = .{};
    solution.sigs = &.{};
    solution.outcomes = &.{};
    var future = FutureFields.init(gpa);
    defer future.deinit(gpa);
    for ([_]usize{ 1, 16, 256 }) |count| {
        while (reads.count() <= count) {
            const probe = try store.addCFStmt(.{ .assign_ref = .{
                .target = local,
                .op = .field(local, 0),
                .next = start,
            } }, .test_fixture);
            try reads.put(probe, .{ .bit = 1, .consuming = true });
            try probes.append(gpa, probe);
        }
        var old_visits: usize = 0;
        for (0..count) |_| {
            try std.testing.expect(!try fieldObservedAfter(gpa, &store, &solution, local, 1, start, &reads, &joins, &.{}, null, &old_visits));
        }
        const graph = try future.buildProcGraph(gpa, &store, probes.items, &joins);
        try future.compute(gpa, &store, &solution, local, graph, &reads, &.{}, null);
        try std.testing.expectEqual(@as(usize, 256) * count, old_visits);
        // The shared continuation and each read are solved once.
        try std.testing.expectEqual(@as(usize, 256) + count, future.nodes.items.len);
        try std.testing.expectEqual(@as(usize, 255) + count, graph.predecessors_len);
        try std.testing.expectEqual(@as(u64, 2), future.observed(start));
    }
}

/// Per-field take dataflow state at one point in a candidate's region. The
/// may/must pair rejects double takes and post-take observations. Divergent
/// exits are allowed: ARC carries their exact residual masks and normalizes
/// them explicitly at the merge.
const FlowState = struct {
    may: u64,
    must: u64,

    fn meet(a: FlowState, b: FlowState) FlowState {
        return .{ .may = a.may | b.may, .must = a.must & b.must };
    }

    fn eql(a: FlowState, b: FlowState) bool {
        return a.may == b.may and a.must == b.must;
    }
};

/// How one statement reads the candidate under dataflow: which field's bit
/// it touches and whether it consumes (owned result) or borrows.
const ReadKind = struct {
    bit: u64,
    consuming: bool,
    visited: bool = false,
};

const FieldRestitution = struct {
    call_arg: CallArg,
    projection: LIR.CFStmtId,
    switch_stmt: LIR.CFStmtId,
    field_mask: u64,
    outcomes: arc_sig.OutcomeSpan,
};

fn restoredFieldMaskForBranch(
    solution: *const arc_solve.Solution,
    restorations: []const FieldRestitution,
    switch_stmt: LIR.CFStmtId,
    value: u64,
) u64 {
    var restored: u64 = 0;
    for (restorations) |receipt| {
        if (receipt.switch_stmt != switch_stmt) continue;
        const position_bit = arc_sig.paramBit(receipt.call_arg.position).?;
        const outcomes = solution.sigTable().outcomesOf(.{ .outcomes = receipt.outcomes });
        const mask = arc_sig.outcomeMaskForValue(outcomes, value) orelse continue;
        if ((mask & position_bit) != 0) restored |= receipt.field_mask;
    }
    return restored;
}

fn restoredFieldMaskForDefault(
    solution: *const arc_solve.Solution,
    store: *const LirStore,
    restorations: []const FieldRestitution,
    switch_stmt: LIR.CFStmtId,
) u64 {
    const branches = store.getCFSwitchBranches(store.getCFStmtImmediate(switch_stmt).switch_stmt.branches);
    var restored: u64 = 0;
    for (restorations) |receipt| {
        if (receipt.switch_stmt != switch_stmt) continue;
        const position_bit = arc_sig.paramBit(receipt.call_arg.position).?;
        const outcomes = solution.sigTable().outcomesOf(.{ .outcomes = receipt.outcomes });
        const mask = arc_sig.defaultOutcomeMask(outcomes, branches) orelse continue;
        if ((mask & position_bit) != 0) restored |= receipt.field_mask;
    }
    return restored;
}

/// Exact dominance of the statement graph, computed once for all projection
/// queries. A synthetic entry reaches every procedure body. Reverse-postorder
/// immediate-dominator iteration handles shared successors and cycles; tree
/// intervals then answer each query in constant time.
const StatementDominance = struct {
    enter: []u32,
    leave: []u32,

    const Edge = struct { from: u32, to: u32 };
    const Frame = struct { node: u32, edge: u32 };

    fn initGraph(gpa: Allocator, count: usize, entry: u32, edges: []const Edge) Error!StatementDominance {
        const outgoing = try gpa.alloc(u32, count);
        defer gpa.free(outgoing);
        const incoming = try gpa.alloc(u32, count);
        defer gpa.free(incoming);
        const next_out = try gpa.alloc(u32, edges.len);
        defer gpa.free(next_out);
        const next_in = try gpa.alloc(u32, edges.len);
        defer gpa.free(next_in);
        @memset(outgoing, no_index);
        @memset(incoming, no_index);
        for (edges, 0..) |edge, index| {
            next_out[index] = outgoing[edge.from];
            outgoing[edge.from] = @intCast(index);
            next_in[index] = incoming[edge.to];
            incoming[edge.to] = @intCast(index);
        }
        const order = try gpa.alloc(u32, count);
        defer gpa.free(order);
        @memset(order, no_index);
        var postorder = std.ArrayList(u32).empty;
        defer postorder.deinit(gpa);
        var frames = std.ArrayList(Frame).empty;
        defer frames.deinit(gpa);
        order[entry] = 0;
        try frames.append(gpa, .{ .node = entry, .edge = outgoing[entry] });
        while (frames.items.len != 0) {
            const frame = &frames.items[frames.items.len - 1];
            if (frame.edge == no_index) {
                try postorder.append(gpa, frame.node);
                _ = frames.pop();
            } else {
                const edge_index = frame.edge;
                frame.edge = next_out[edge_index];
                const child = edges[edge_index].to;
                if (order[child] != no_index) continue;
                order[child] = 0;
                try frames.append(gpa, .{ .node = child, .edge = outgoing[child] });
            }
        }
        std.mem.reverse(u32, postorder.items);
        for (postorder.items, 0..) |node, index| order[node] = @intCast(index);
        const idom = try gpa.alloc(u32, count);
        defer gpa.free(idom);
        @memset(idom, no_index);
        idom[entry] = entry;
        var changed = true;
        while (changed) {
            changed = false;
            for (postorder.items[1..]) |node| {
                var parent: u32 = no_index;
                var edge_index = incoming[node];
                while (edge_index != no_index) : (edge_index = next_in[edge_index]) {
                    const predecessor = edges[edge_index].from;
                    if (idom[predecessor] == no_index) continue;
                    if (parent == no_index) {
                        parent = predecessor;
                    } else {
                        var other = predecessor;
                        while (parent != other) {
                            if (order[parent] > order[other]) {
                                parent = idom[parent];
                            } else {
                                other = idom[other];
                            }
                        }
                    }
                }
                std.debug.assert(parent != no_index);
                if (idom[node] != parent) {
                    idom[node] = parent;
                    changed = true;
                }
            }
        }
        // Reuse the node columns as first-child and next-sibling links for
        // the solved dominator tree. Unreachable nodes retain no interval.
        @memset(outgoing, no_index);
        @memset(incoming, no_index);
        for (postorder.items[1..]) |node| {
            incoming[node] = outgoing[idom[node]];
            outgoing[idom[node]] = node;
        }
        const enter = try gpa.alloc(u32, count);
        errdefer gpa.free(enter);
        const leave = try gpa.alloc(u32, count);
        errdefer gpa.free(leave);
        @memset(enter, no_index);
        @memset(leave, no_index);
        var clock: u32 = 0;
        enter[entry] = clock;
        clock += 1;
        try frames.append(gpa, .{ .node = entry, .edge = outgoing[entry] });
        while (frames.items.len != 0) {
            const frame = &frames.items[frames.items.len - 1];
            if (frame.edge == no_index) {
                leave[frame.node] = clock;
                _ = frames.pop();
            } else {
                const child = frame.edge;
                frame.edge = incoming[child];
                enter[child] = clock;
                clock += 1;
                try frames.append(gpa, .{ .node = child, .edge = outgoing[child] });
            }
        }
        return .{ .enter = enter, .leave = leave };
    }

    fn deinit(self: *StatementDominance, gpa: Allocator) void {
        gpa.free(self.enter);
        gpa.free(self.leave);
    }

    fn dominates(self: StatementDominance, dominator: LIR.CFStmtId, target: LIR.CFStmtId) bool {
        return self.dominatesNode(@backingInt(dominator), @backingInt(target));
    }

    fn dominatesNode(self: StatementDominance, a: u32, b: u32) bool {
        std.debug.assert(self.enter[a] != no_index and self.enter[b] != no_index);
        return self.enter[a] <= self.enter[b] and self.enter[b] < self.leave[a];
    }
};

/// Statement dominance from a synthetic entry for every procedure body, solved
/// separately for each component of procedures that reach a common statement,
/// on its first query. Every path from the entry to a statement starts at the
/// body of a procedure reaching that statement and stays among statements that
/// procedure reaches, all of which lie in the statement's component. A
/// statement therefore dominates only statements of its own component, and the
/// component's graph answers every query exactly as the whole store's would.
const ComponentDominance = struct {
    store: *const LirStore,
    solution: *const arc_solve.Solution,
    /// Union-find over procedures; a statement reached by two procedures
    /// unites them.
    proc_parent: []u32,
    /// First procedure (in procedure order) reaching each statement.
    stmt_first_proc: []const u32,
    /// Index of each statement within its solved component's graph.
    dense: []u32 = &.{},
    /// Procedures grouped by component root, built with the first solve.
    component_offsets: []u32 = &.{},
    component_procs: []u32 = &.{},
    solved: std.AutoHashMapUnmanaged(u32, StatementDominance) = .empty,

    fn init(gpa: Allocator, store: *const LirStore, solution: *const arc_solve.Solution, stmt_first_proc: []const u32) Error!ComponentDominance {
        const proc_parent = try gpa.alloc(u32, store.procSpecCount());
        for (proc_parent, 0..) |*parent, index| parent.* = @intCast(index);
        return .{ .store = store, .solution = solution, .proc_parent = proc_parent, .stmt_first_proc = stmt_first_proc };
    }

    fn deinit(self: *ComponentDominance, gpa: Allocator) void {
        var it = self.solved.valueIterator();
        while (it.next()) |dominance| dominance.deinit(gpa);
        self.solved.deinit(gpa);
        gpa.free(self.component_procs);
        gpa.free(self.component_offsets);
        gpa.free(self.dense);
        gpa.free(self.proc_parent);
    }

    fn root(self: *ComponentDominance, proc: u32) u32 {
        var current = proc;
        while (self.proc_parent[current] != current) {
            self.proc_parent[current] = self.proc_parent[self.proc_parent[current]];
            current = self.proc_parent[current];
        }
        return current;
    }

    fn unite(self: *ComponentDominance, left: u32, right: u32) void {
        const a = self.root(left);
        const b = self.root(right);
        self.proc_parent[@max(a, b)] = @min(a, b);
    }

    fn dominates(self: *ComponentDominance, gpa: Allocator, dominator: LIR.CFStmtId, target: LIR.CFStmtId) Error!bool {
        const component = self.root(self.stmt_first_proc[@backingInt(target)]);
        if (self.root(self.stmt_first_proc[@backingInt(dominator)]) != component) return false;
        const slot = try self.solved.getOrPut(gpa, component);
        if (!slot.found_existing) {
            errdefer _ = self.solved.remove(component);
            slot.value_ptr.* = try self.solve(gpa, component);
        }
        return slot.value_ptr.dominatesNode(self.dense[@backingInt(dominator)], self.dense[@backingInt(target)]);
    }

    fn solve(self: *ComponentDominance, gpa: Allocator, component: u32) Error!StatementDominance {
        const store = self.store;
        if (self.component_offsets.len == 0) try self.groupProcs(gpa);
        var stmts = std.ArrayList(LIR.CFStmtId).empty;
        defer stmts.deinit(gpa);
        const procs = self.component_procs[self.component_offsets[component]..self.component_offsets[component + 1]];
        for (procs) |proc| {
            for (self.solution.procStatements(store, @fromBackingInt(@intCast(proc)))) |stmt_id| {
                const dense = &self.dense[@backingInt(stmt_id)];
                if (dense.* != no_index) continue;
                dense.* = @intCast(stmts.items.len);
                try stmts.append(gpa, stmt_id);
            }
        }
        const entry: u32 = @intCast(stmts.items.len);
        var edges = std.ArrayList(StatementDominance.Edge).empty;
        defer edges.deinit(gpa);
        var successors = std.ArrayList(LIR.CFStmtId).empty;
        defer successors.deinit(gpa);
        for (stmts.items, 0..) |stmt_id, index| {
            successors.clearRetainingCapacity();
            try body_clone.appendSuccessors(store, &successors, stmt_id, gpa);
            for (successors.items) |next| try edges.append(gpa, .{ .from = @intCast(index), .to = self.dense[@backingInt(next)] });
        }
        for (procs) |proc| {
            const body = store.getProcSpec(@fromBackingInt(@intCast(proc))).body orelse continue;
            try edges.append(gpa, .{ .from = entry, .to = self.dense[@backingInt(body)] });
        }
        return StatementDominance.initGraph(gpa, stmts.items.len + 1, entry, edges.items);
    }

    fn groupProcs(self: *ComponentDominance, gpa: Allocator) Error!void {
        const proc_count = self.proc_parent.len;
        const offsets = try gpa.alloc(u32, proc_count + 1);
        errdefer gpa.free(offsets);
        @memset(offsets, 0);
        for (0..proc_count) |proc| offsets[self.root(@intCast(proc)) + 1] += 1;
        for (1..offsets.len) |index| offsets[index] += offsets[index - 1];
        const procs = try gpa.alloc(u32, proc_count);
        errdefer gpa.free(procs);
        const cursors = try gpa.dupe(u32, offsets[0..proc_count]);
        defer gpa.free(cursors);
        for (0..proc_count) |proc| {
            const component = self.root(@intCast(proc));
            procs[cursors[component]] = @intCast(proc);
            cursors[component] += 1;
        }
        const dense = try gpa.alloc(u32, self.store.cfStmtCount());
        @memset(dense, no_index);
        self.component_offsets = offsets;
        self.component_procs = procs;
        self.dense = dense;
    }
};

test "component dominance agrees with whole-store dominance across shared procedure statements" {
    const gpa = std.testing.allocator;
    var store = LirStore.init(gpa);
    defer store.deinit();
    var layouts = try layout_mod.Store.init(gpa, .u64);
    defer layouts.deinit();
    const value = try store.addLocal(.{ .layout_idx = .i64 });
    const frame = try store.addLocalSpan(&.{value});
    const literal = struct {
        fn add(s: *LirStore, target: LIR.LocalId, next: LIR.CFStmtId) Error!LIR.CFStmtId {
            return s.addCFStmt(.{ .assign_literal = .{
                .target = target,
                .value = .{ .i64_literal = .{ .value = 1, .layout_idx = .i64 } },
                .next = next,
            } }, .test_fixture);
        }
    }.add;
    // Procedures 0 and 1 share a tail; procedure 2 shares nothing. An
    // unreachable statement points into the shared tail.
    const tail = try store.addCFStmt(.{ .ret = .{ .value = value } }, .test_fixture);
    const shared = try literal(&store, value, tail);
    const left = try literal(&store, value, shared);
    const right = try literal(&store, value, shared);
    const branch = try store.addCFStmt(.{ .switch_stmt = .{
        .cond = value,
        .branches = try store.addCFSwitchBranches(&.{.{ .value = 0, .body = left }}),
        .default_branch = right,
        .continuation = null,
    } }, .test_fixture);
    const bodies = [_]LIR.CFStmtId{
        try literal(&store, value, branch),
        try literal(&store, value, shared),
        try literal(&store, value, try store.addCFStmt(.{ .ret = .{ .value = value } }, .test_fixture)),
    };
    _ = try literal(&store, value, left);
    for (bodies) |body| {
        _ = try store.addProcSpec(.{
            .identity = LIR.ProcIdentity.forTest(@intCast(store.procSpecCount())),
            .name = store.freshSyntheticSymbol(),
            .args = .empty(),
            .frame_locals = frame,
            .body = body,
            .ret_layout = .i64,
        }, .none);
    }
    const rc = try gpa.alloc(bool, store.localCount());
    defer gpa.free(rc);
    @memset(rc, false);
    var solution = try arc_solve.solve(gpa, &store, &layouts, rc, &.{}, &.{}, true);
    defer solution.deinit();

    const stmt_first_proc = try gpa.alloc(u32, store.cfStmtCount());
    defer gpa.free(stmt_first_proc);
    @memset(stmt_first_proc, no_index);
    var components = try ComponentDominance.init(gpa, &store, &solution, stmt_first_proc);
    defer components.deinit(gpa);
    var edges = std.ArrayList(StatementDominance.Edge).empty;
    defer edges.deinit(gpa);
    var successors = std.ArrayList(LIR.CFStmtId).empty;
    defer successors.deinit(gpa);
    for (0..store.procSpecCount()) |proc_index| {
        const proc: u32 = @intCast(proc_index);
        for (solution.procStatements(&store, @fromBackingInt(proc))) |stmt_id| {
            const index = @backingInt(stmt_id);
            if (stmt_first_proc[index] != no_index) {
                components.unite(stmt_first_proc[index], proc);
                continue;
            }
            stmt_first_proc[index] = proc;
            successors.clearRetainingCapacity();
            try body_clone.appendSuccessors(&store, &successors, stmt_id, gpa);
            for (successors.items) |next| try edges.append(gpa, .{ .from = index, .to = @backingInt(next) });
        }
    }
    const entry: u32 = @intCast(store.cfStmtCount());
    for (bodies) |body| try edges.append(gpa, .{ .from = entry, .to = @backingInt(body) });
    var whole = try StatementDominance.initGraph(gpa, store.cfStmtCount() + 1, entry, edges.items);
    defer whole.deinit(gpa);
    try std.testing.expect(components.root(1) == components.root(0));
    try std.testing.expect(components.root(2) != components.root(0));
    var comparisons: usize = 0;
    for (0..store.cfStmtCount()) |a| {
        if (stmt_first_proc[a] == no_index) continue;
        for (0..store.cfStmtCount()) |b| {
            if (stmt_first_proc[b] == no_index) continue;
            const dominator: LIR.CFStmtId = @fromBackingInt(@intCast(a));
            const target: LIR.CFStmtId = @fromBackingInt(@intCast(b));
            try std.testing.expectEqual(whole.dominates(dominator, target), try components.dominates(gpa, dominator, target));
            comparisons += 1;
        }
    }
    try std.testing.expectEqual(@as(usize, 81), comparisons);
    try std.testing.expect(!try components.dominates(gpa, bodies[0], shared));
    try std.testing.expect(try components.dominates(gpa, branch, left));
}

test "statement dominance agrees with deleted-node reachability on every four-node graph" {
    const gpa = std.testing.allocator;
    // All directed edges between distinct nodes: includes diamonds, cycles,
    // irreducible loops, multiple entry successors, and unreachable regions.
    for (0..4096) |mask| {
        var edge_buffer: [12]StatementDominance.Edge = undefined;
        var edge_count: usize = 0;
        var bit: u4 = 0;
        for (0..4) |from| {
            for (0..4) |to| {
                if (from == to) continue;
                if (mask & (@as(usize, 1) << bit) != 0) {
                    edge_buffer[edge_count] = .{ .from = @intCast(from), .to = @intCast(to) };
                    edge_count += 1;
                }
                bit += 1;
            }
        }
        const edges = edge_buffer[0..edge_count];
        var dominance = try StatementDominance.initGraph(gpa, 4, 0, edges);
        defer dominance.deinit(gpa);
        for (0..4) |removed| {
            if (dominance.enter[removed] == no_index) continue;
            var reachable: [4]bool = .{ removed != 0, false, false, false };
            var changed = true;
            while (changed) {
                changed = false;
                for (edges) |edge| {
                    if (edge.to != removed and reachable[edge.from] and !reachable[edge.to]) {
                        reachable[edge.to] = true;
                        changed = true;
                    }
                }
            }
            for (0..4) |target| {
                if (dominance.enter[target] == no_index) continue;
                try std.testing.expectEqual(!reachable[target], dominance.dominates(
                    @fromBackingInt(@intCast(@as(u32, @intCast(removed)))),
                    @fromBackingInt(@intCast(@as(u32, @intCast(target)))),
                ));
            }
        }
    }
}

/// Classify every occurrence of every local in one reachable statement. The
/// switch is exhaustive so a new statement form fails to compile rather than
/// silently escaping classification.
fn classifyStatement(
    analysis: *Analysis,
    store: *const LirStore,
    solution: *const arc_solve.Solution,
    operand_read_counts: []u32,
    direct_call_stmt_by_arg: []u32,
    direct_call_position_by_arg: []u8,
    current: LIR.CFStmtId,
) Error!void {
    const current_stmt = store.getCFStmtImmediate(current);
    body_clone.countStmtReads(store, operand_read_counts, current_stmt.*);
    switch (current_stmt.*) {
        .init_uninitialized => |stmt| {
            try analysis.useWhole(current, stmt.target);
        },
        .assign_ref => |stmt| {
            switch (stmt.op.kind) {
                .field => {
                    if (!try analysis.noteFieldRead(current, stmt.op.source, stmt.op.index, stmt.target)) {
                        try analysis.noteDef(stmt.target, current);
                    }
                },
                .local => {
                    try analysis.noteDemandAlias(stmt.op.source, stmt.target);
                    if (stmt.target == stmt.op.source) {
                        try analysis.useWhole(current, stmt.op.source);
                    } else {
                        try analysis.noteAliasDef(current, stmt.target, stmt.op.source);
                    }
                },
                .discriminant => {
                    // The tag word is disjoint from every stored unit, so
                    // reading it is no use of a tag union.
                    analysis.disqualify(stmt.op.source);
                    try analysis.noteDef(stmt.target, current);
                },
                .tag_payload => {
                    try analysis.useWhole(current, stmt.op.source);
                    try analysis.noteDef(stmt.target, current);
                },
                .tag_payload_struct => {
                    if (!try analysis.notePayloadView(stmt.target, stmt.op)) try analysis.useWhole(current, stmt.op.source);
                    try analysis.noteDef(stmt.target, current);
                },
                inline .list_reinterpret, .nominal => {
                    try analysis.useWhole(current, stmt.op.source);
                    analysis.disqualify(stmt.target);
                },
            }
        },
        .assign_literal => |stmt| {
            try analysis.noteDef(stmt.target, current);
        },
        .assign_call => |stmt| {
            const args = store.getLocalSpan(stmt.args);
            const callee_sig = solution.sigOf(stmt.proc);
            for (0..GuardedList.borrowLen(args)) |i| {
                const arg = GuardedList.at(args, i);
                if (i < arc_sig.tracked_param_count) {
                    direct_call_stmt_by_arg[@backingInt(arg)] = @backingInt(current);
                    direct_call_position_by_arg[@backingInt(arg)] = @intCast(i);
                }
                try analysis.useWholeAt(arg, current);
                if (callee_sig.paramMode(i) == .owned) analysis.demandOwned(arg);
            }
            try analysis.noteDef(stmt.target, current);
        },
        .assign_call_erased => |inline_stmt| {
            const stmt = store.stmtRow(inline_stmt.data);
            try analysis.useWhole(current, stmt.closure);
            if (stmt.reuse_source) |reuse_source| try analysis.useWhole(current, reuse_source);
            const args = store.getLocalSpan(stmt.args);
            for (0..GuardedList.borrowLen(args)) |i| try analysis.useWholeAt(GuardedList.at(args, i), current);
            try analysis.noteDef(inline_stmt.target, current);
        },
        .assign_packed_erased_fn => |inline_stmt| {
            const stmt = store.stmtRow(inline_stmt.data);
            if (stmt.capture) |capture| try analysis.useWhole(current, capture);
            if (stmt.reuse) |reuse| try analysis.useWhole(current, reuse);
            try analysis.noteDef(inline_stmt.target, current);
        },
        .assign_boxy_desc_ref => |inline_stmt| {
            const stmt = store.stmtRow(inline_stmt.data);
            if (stmt.desc.localOrNull()) |local| try analysis.useWhole(current, local);
            if (stmt.tag_residual_for) |desc| if (desc.localOrNull()) |local| try analysis.useWhole(current, local);
            const captures = store.getLocalSpan(stmt.captures);
            for (0..GuardedList.borrowLen(captures)) |i| try analysis.useWhole(current, GuardedList.at(captures, i));
            try analysis.noteDef(inline_stmt.target, current);
            analysis.disqualify(inline_stmt.target);
        },
        .assign_boxy_dict_ref => |stmt| {
            if (stmt.dict.localOrNull()) |local| try analysis.useWhole(current, local);
            const captures = store.getLocalSpan(stmt.captures);
            for (0..GuardedList.borrowLen(captures)) |i| try analysis.useWhole(current, GuardedList.at(captures, i));
            try analysis.noteDef(stmt.target, current);
            analysis.disqualify(stmt.target);
        },
        .assign_boxy_box => |inline_stmt| {
            const stmt = store.stmtRow(inline_stmt.data);
            try analysis.useWhole(current, stmt.payload);
            try analysis.noteDef(inline_stmt.target, current);
            analysis.disqualify(inline_stmt.target);
        },
        .assign_boxy_record_update => |inline_stmt| {
            const stmt = store.stmtRow(inline_stmt.data);
            try analysis.useWhole(current, stmt.base);
            try analysis.useWhole(current, stmt.fields);
            try analysis.noteDef(inline_stmt.target, current);
            analysis.disqualify(inline_stmt.target);
        },
        inline .assign_boxy_unbox, .assign_boxy_adapt, .assign_boxy_inspect => |stmt_stmt| {
            const stmt = store.stmtRow(stmt_stmt.data);
            try analysis.useWhole(current, stmt.source);
            try analysis.noteDef(stmt_stmt.target, current);
            analysis.disqualify(stmt_stmt.target);
        },
        .assign_boxy_eq => |inline_stmt| {
            const stmt = store.stmtRow(inline_stmt.data);
            try analysis.useWhole(current, stmt.lhs);
            try analysis.useWhole(current, stmt.rhs);
            try analysis.noteDef(inline_stmt.target, current);
            analysis.disqualify(inline_stmt.target);
        },
        .assign_boxy_hash => |inline_stmt| {
            const stmt = store.stmtRow(inline_stmt.data);
            try analysis.useWhole(current, stmt.value);
            try analysis.useWhole(current, stmt.hasher);
            try analysis.noteDef(inline_stmt.target, current);
            analysis.disqualify(inline_stmt.target);
        },
        .assign_boxy_tag => |inline_stmt| {
            const stmt = store.stmtRow(inline_stmt.data);
            if (stmt.payload) |payload| try analysis.useWhole(current, payload);
            try analysis.noteDef(inline_stmt.target, current);
            analysis.disqualify(inline_stmt.target);
        },
        .assign_boxy_tag_payload => |inline_stmt| {
            const stmt = store.stmtRow(inline_stmt.data);
            try analysis.useWhole(current, stmt.source);
            try analysis.noteDef(inline_stmt.target, current);
            analysis.disqualify(inline_stmt.target);
            if (stmt.target_desc) |target_desc| {
                try analysis.noteDef(target_desc, current);
                analysis.disqualify(target_desc);
            }
        },
        .assign_call_dict => |inline_stmt| {
            const stmt = store.stmtRow(inline_stmt.data);
            if (stmt.dict.localOrNull()) |local| try analysis.useWhole(current, local);
            const args = store.getLocalSpan(stmt.args);
            for (0..GuardedList.borrowLen(args)) |i| try analysis.useWhole(current, GuardedList.at(args, i));
            const arg_descs = store.getLocalSpan(stmt.arg_descs);
            for (0..GuardedList.borrowLen(arg_descs)) |i| try analysis.useWhole(current, GuardedList.at(arg_descs, i));
            const hidden_args = store.getLocalSpan(stmt.hidden_args);
            for (0..GuardedList.borrowLen(hidden_args)) |i| try analysis.useWhole(current, GuardedList.at(hidden_args, i));
            try analysis.noteDef(inline_stmt.target, current);
            analysis.disqualify(inline_stmt.target);
        },
        .assign_low_level => |stmt| {
            const args = store.getLocalSpan(stmt.args);
            const demand_mask = store.stmtRow(stmt.rc_effect).consume_args | store.stmtRow(stmt.rc_effect).retain_args;
            for (0..GuardedList.borrowLen(args)) |i| {
                const arg = GuardedList.at(args, i);
                try analysis.useWholeAt(arg, current);
                if (i < 64 and (demand_mask & (@as(u64, 1) << @intCast(i))) != 0) analysis.demandOwned(arg);
            }
            try analysis.noteDef(stmt.target, current);
        },
        .assign_list => |stmt| {
            const elems = store.getLocalSpan(stmt.elems);
            for (0..GuardedList.borrowLen(elems)) |i| try analysis.useWholeAt(GuardedList.at(elems, i), current);
            try analysis.noteDef(stmt.target, current);
        },
        .assign_struct => |stmt| {
            const fields = store.getLocalSpan(stmt.fields);
            for (0..GuardedList.borrowLen(fields)) |i| try analysis.useWholeAt(GuardedList.at(fields, i), current);
            try analysis.noteDef(stmt.target, current);
        },
        .assign_tag => |stmt| {
            if (stmt.payload) |payload| try analysis.useWholeAt(payload, current);
            try analysis.noteDef(stmt.target, current);
        },
        .store_struct => |stmt| {
            try analysis.useWhole(current, stmt.dest);
            const fields = store.getLocalSpan(stmt.fields);
            for (0..GuardedList.borrowLen(fields)) |i| try analysis.useWhole(current, GuardedList.at(fields, i));
        },
        .store_tag => |stmt| {
            try analysis.useWhole(current, stmt.dest);
            if (stmt.payload) |payload| try analysis.useWhole(current, payload);
        },
        .set_local => |stmt| {
            try analysis.useWholeAt(stmt.value, current);
            if (stmt.mode == .initialize_join_param and analysis.explicit_init_join[@backingInt(stmt.target)]) {
                try analysis.noteDef(stmt.target, current);
            } else {
                try analysis.useWhole(current, stmt.target);
            }
        },
        .debug => |stmt| {
            try analysis.useWhole(current, stmt.message);
        },
        .expect => |stmt| {
            try analysis.useWhole(current, stmt.condition);
        },
        .expect_err => |stmt| try analysis.useWhole(current, stmt.message),
        .runtime_error, .comptime_exhaustiveness_failed, .jump => {},
        .comptime_branch_taken => {},
        // The input contract is RC-free LIR; if RC statements ever appear
        // here, classifying their operands as whole uses stays sound.
        inline .incref, .decref, .free => |stmt| {
            try analysis.useWhole(current, stmt.value);
        },
        .decref_if_initialized => |stmt| {
            try analysis.useWhole(current, stmt.cond);
            try analysis.useWhole(current, stmt.value);
        },
        .switch_stmt => |stmt| {
            try analysis.useWhole(current, stmt.cond);
        },
        .switch_initialized_payload => |stmt| {
            try analysis.useWhole(current, stmt.cond);
            try analysis.useWhole(current, stmt.payload);
        },
        .str_match => |stmt_stmt| {
            const stmt = store.stmtRow(stmt_stmt.data);
            try analysis.useWhole(current, stmt_stmt.source);
            const steps = store.getStrMatchSteps(stmt.steps);
            for (0..GuardedList.borrowLen(steps)) |i| {
                switch (GuardedList.at(steps, i).capture) {
                    .discard => {},
                    .view => |view_local| try analysis.useWhole(current, view_local),
                }
            }
        },
        .str_match_set => |stmt| {
            try analysis.useWhole(current, stmt.source);
            const arms = store.getStrMatchArms(stmt.arms);
            for (0..GuardedList.borrowLen(arms)) |arm_index| {
                const arm = GuardedList.at(arms, arm_index);
                const steps = store.getStrMatchSteps(arm.steps);
                for (0..GuardedList.borrowLen(steps)) |i| {
                    switch (GuardedList.at(steps, i).capture) {
                        .discard => {},
                        .view => |view_local| try analysis.useWhole(current, view_local),
                    }
                }
            }
        },
        .boxy_tag_match => |stmt| {
            try analysis.useWhole(current, stmt.source);
        },
        // Join parameters are excluded by the gate; the condition locals
        // are scalar presence words.
        .join => {},
        .ret => |stmt| try analysis.useWholeAt(stmt.value, current),
        .crash => |stmt| if (stmt.msg.localId()) |message| try analysis.useWholeAt(message, current),
    }
}

/// A reachable reference read that can carry its source's ownership root: a
/// borrowed same-layout alias or an ownership-complete projection.
const ProjectionEdge = struct {
    source: LIR.LocalId,
    target: LIR.LocalId,
    stmt: LIR.CFStmtId,
    is_projection: bool,
};

fn projectionEdgeOf(
    store: *const LirStore,
    layouts: *const layout_mod.Store,
    solution: *const arc_solve.Solution,
    stmt_id: LIR.CFStmtId,
    assign: anytype,
) ?ProjectionEdge {
    const source: LIR.LocalId, const is_projection: bool = switch (assign.op.kind) {
        .local => blk: {
            if (!solution.isBorrowed(assign.target)) return null;
            if (store.getLocal(assign.op.source).layout_idx != store.getLocal(assign.target).layout_idx) return null;
            break :blk .{ assign.op.source, false };
        },
        .field, .tag_payload, .tag_payload_struct => blk: {
            const projection = encodeProjection(assign.op).?;
            const local = switch (assign.op.kind) {
                inline .field, .tag_payload, .tag_payload_struct => assign.op.source,
                .local, .discriminant, .list_reinterpret, .nominal => unreachable,
            };
            if (!projectionOwnsAllRc(store, layouts, local, assign.target, projection)) return null;
            break :blk .{ local, true };
        },
        .discriminant, .list_reinterpret, .nominal => return null,
    };
    return .{ .source = source, .target = assign.target, .stmt = stmt_id, .is_projection = is_projection };
}

const PlaceEdgeKind = union(enum) {
    alias,
    projection: u32,
};

/// One edge of the ownership-place graph: a borrowed alias sharing its
/// source's unit, or an ownership-complete projection of its source.
const PlaceLink = struct {
    source: u32,
    target: u32,
    kind: PlaceEdgeKind,
};

fn placeLinkOf(
    store: *const LirStore,
    layouts: *const layout_mod.Store,
    solution: *const arc_solve.Solution,
    stmt_index: u32,
    assign: anytype,
) ?PlaceLink {
    const source: LIR.LocalId, const kind: PlaceEdgeKind = switch (assign.op.kind) {
        .local => blk: {
            if (!solution.isBorrowed(assign.target)) return null;
            if (solution.unitLocalOf(assign.target) != solution.unitLocalOf(assign.op.source)) return null;
            break :blk .{ assign.op.source, .alias };
        },
        .field, .tag_payload, .tag_payload_struct => blk: {
            const projection = encodeProjection(assign.op).?;
            const projection_source = switch (assign.op.kind) {
                inline .field, .tag_payload, .tag_payload_struct => assign.op.source,
                .local, .discriminant, .list_reinterpret, .nominal => unreachable,
            };
            if (!projectionOwnsAllRc(store, layouts, projection_source, assign.target, projection)) return null;
            break :blk .{ projection_source, .{ .projection = stmt_index } };
        },
        .discriminant, .list_reinterpret, .nominal => return null,
    };
    return .{ .source = @backingInt(source), .target = @backingInt(assign.target), .kind = kind };
}

/// A reachable reference read whose place link is read after projection
/// aliases are materialized. Only a read carrying a projection edge can be
/// rewritten into an alias, so every other read keeps the link computed here.
const PlaceRead = struct {
    stmt: u32,
    link: ?PlaceLink,
};

/// Facts of the reachable statements in one statement-id range. Each fact
/// depends only on its own statement, the store, and the solution, so ranges
/// are independent executor tasks. Concatenating ranges in order lists every
/// fact in ascending statement order, exactly as one whole-store scan would.
const StatementFacts = struct {
    store: *const LirStore,
    layouts: *const layout_mod.Store,
    solution: *const arc_solve.Solution,
    reachable: *const std.bit_set.DynamicBitSetUnmanaged,
    start: u32,
    end: u32,
    allocator: Allocator,
    /// Join cells written by an `initialize_join_param` statement.
    join_init_targets: std.ArrayList(LIR.LocalId) = .empty,
    projection_edges: std.ArrayList(ProjectionEdge) = .empty,
    place_reads: std.ArrayList(PlaceRead) = .empty,
    calls: std.ArrayList(LIR.CFStmtId) = .empty,
    failed: bool = false,
    completed: bool = false,

    fn deinit(self: *StatementFacts) void {
        self.join_init_targets.deinit(self.allocator);
        self.projection_edges.deinit(self.allocator);
        self.place_reads.deinit(self.allocator);
        self.calls.deinit(self.allocator);
    }

    fn collect(self: *StatementFacts, allocator: Allocator) Error!void {
        self.allocator = allocator;
        var index = self.start;
        while (index < self.end) : (index += 1) {
            if (!self.reachable.isSet(index)) continue;
            const stmt_id: LIR.CFStmtId = @fromBackingInt(@intCast(index));
            switch (self.store.getCFStmtImmediate(stmt_id).*) {
                .set_local => |stmt| if (stmt.mode == .initialize_join_param) {
                    try self.join_init_targets.append(allocator, stmt.target);
                },
                .assign_ref => |stmt| {
                    const projection = projectionEdgeOf(self.store, self.layouts, self.solution, stmt_id, stmt);
                    if (projection) |edge| try self.projection_edges.append(allocator, edge);
                    const link = placeLinkOf(self.store, self.layouts, self.solution, index, stmt);
                    if (projection != null or link != null) try self.place_reads.append(allocator, .{ .stmt = index, .link = link });
                },
                .assign_call => try self.calls.append(allocator, stmt_id),
                .init_uninitialized,
                .assign_literal,
                .assign_call_erased,
                .assign_packed_erased_fn,
                .assign_boxy_desc_ref,
                .assign_boxy_dict_ref,
                .assign_boxy_box,
                .assign_boxy_record_update,
                .assign_boxy_unbox,
                .assign_boxy_adapt,
                .assign_boxy_inspect,
                .assign_boxy_eq,
                .assign_boxy_hash,
                .assign_boxy_tag,
                .assign_boxy_tag_payload,
                .boxy_tag_match,
                .assign_call_dict,
                .assign_low_level,
                .assign_list,
                .assign_struct,
                .assign_tag,
                .store_struct,
                .store_tag,
                .debug,
                .expect,
                .expect_err,
                .runtime_error,
                .comptime_exhaustiveness_failed,
                .comptime_branch_taken,
                .incref,
                .decref,
                .decref_if_initialized,
                .free,
                .switch_stmt,
                .switch_initialized_payload,
                .str_match,
                .str_match_set,
                .join,
                .jump,
                .ret,
                .crash,
                => {},
            }
        }
    }

    fn run(opaque_context: *anyopaque, worker: TaskExecutor.Worker) ?*anyopaque {
        const self: *StatementFacts = @ptrCast(@alignCast(opaque_context));
        self.collect(worker.allocator) catch {
            self.failed = true;
        };
        return self;
    }
};

/// Statement ids per fact range. A fixed width makes the task set a function
/// of the store alone, independent of how many lanes run it.
const statement_facts_range_size = 8192;

/// Executor tasks of one `compute` call.
pub const TaskMetrics = struct {
    submitted: u64 = 0,
    committed: u64 = 0,
};

/// How `compute` schedules its statement-range tasks.
pub const ComputeOptions = struct {
    executor: ?*const TaskExecutor.Executor = null,
    metrics: ?*TaskMetrics = null,
};

/// Collect every range's facts, on the executor's lanes when one is given.
/// Submission failures stop admission, not draining: every accepted range
/// finishes before its owner can be released.
fn collectStatementFacts(
    gpa: Allocator,
    ranges: []StatementFacts,
    options: ComputeOptions,
) Error!void {
    const executor = options.executor orelse {
        for (ranges) |*range| try range.collect(gpa);
        return;
    };
    var session = executor.begin();
    var submitted: usize = 0;
    var received: usize = 0;
    var failed = false;
    while (received < submitted or (!failed and submitted < ranges.len)) {
        while (!failed and submitted < ranges.len and session.canSubmit()) {
            session.submit(.{ .id = submitted, .context = &ranges[submitted], .run = StatementFacts.run }) catch {
                failed = true;
                break;
            };
            submitted += 1;
            if (options.metrics) |metrics| metrics.submitted += 1;
        }
        if (received < submitted) {
            const completion = session.receive();
            if (completion.id >= submitted or completion.worker_id >= executor.worker_count) {
                dismantleInvariant("ARC dismantle received invalid task identity");
            }
            const range = &ranges[completion.id];
            if (range.completed or completion.value != @as(*anyopaque, @ptrCast(range))) {
                dismantleInvariant("ARC dismantle received invalid task completion");
            }
            range.completed = true;
            received += 1;
            if (range.failed) {
                failed = true;
            } else if (options.metrics) |metrics| {
                metrics.committed += 1;
            }
        }
    }
    session.end();
    if (failed) return error.OutOfMemory;
}

/// Items grouped by call-graph component, each group in the items' original
/// relative order.
const ComponentBuckets = struct {
    starts: []u32,
    items: []u32,

    fn build(allocator: Allocator, component_count: usize, keys: []const u32, values: []const u32) Error!ComponentBuckets {
        const starts = try allocator.alloc(u32, component_count + 1);
        @memset(starts, 0);
        for (keys) |key| starts[key + 1] += 1;
        for (1..starts.len) |index| starts[index] += starts[index - 1];
        const items = try allocator.alloc(u32, keys.len);
        const fill = try allocator.dupe(u32, starts[0..component_count]);
        for (keys, values) |key, value| {
            items[fill[key]] = value;
            fill[key] += 1;
        }
        return .{ .starts = starts, .items = items };
    }

    fn of(self: ComponentBuckets, component: usize) []const u32 {
        return self.items[self.starts[component]..self.starts[component + 1]];
    }
};

/// Whether a read whose source belongs to view `through` goes through a view
/// in `held` on its way out to `stop`, the view whose container is being
/// solved (`no_index` for any other container).
fn readsThroughHeldView(analysis: *const Analysis, held: *const std.AutoHashMapUnmanaged(u32, void), through: u32, stop: u32) bool {
    var view = through;
    while (view != no_index and view != stop) : (view = analysis.view_parent[view]) {
        if (held.contains(view)) return true;
    }
    return false;
}

/// A reachable statement naming the candidate, which places it in its
/// procedure's call-graph component, or null for a candidate with no
/// occurrence (which never commits a plan).
fn candidateStatement(candidate: *const Candidate) ?LIR.CFStmtId {
    if (candidate.reads.items.len != 0) return candidate.reads.items[0].stmt;
    if (candidate.equivalenced_reads.items.len != 0) return candidate.equivalenced_reads.items[0].read.stmt;
    if (candidate.whole_uses.items.len != 0) return candidate.whole_uses.items[0];
    if (candidate.def_count != 0) return candidate.def_stmt;
    if (candidate.join_starts.items.len != 0) return candidate.join_starts.items[0];
    return null;
}

/// The place link a reference read has once projection aliases are
/// materialized: a projection whose representative committed a plan links as
/// an alias of that representative, and every other read keeps its own link.
fn materializedPlaceLink(
    store: *const LirStore,
    layouts: *const layout_mod.Store,
    solution: *const arc_solve.Solution,
    result: *const Dismantles,
    projected_container: []const u32,
    projected_stmt: []const u32,
    read: PlaceRead,
) ?PlaceLink {
    const assign = store.getCFStmtImmediate(@fromBackingInt(read.stmt)).assign_ref;
    const target_index = @backingInt(assign.target);
    const representative = projected_container[target_index];
    if (representative == no_index or representative == target_index or projected_stmt[target_index] != read.stmt) return read.link;
    const representative_local: LIR.LocalId = @fromBackingInt(representative);
    if (!result.containers.contains(representative_local) and
        !result.owned_only_containers.contains(representative_local)) return read.link;
    return placeLinkOf(store, layouts, solution, read.stmt, .{ .target = assign.target, .op = LIR.RefOp.local(representative_local) });
}

/// Solve takes for every reachable statement in the store.
pub fn compute(
    gpa: Allocator,
    store: *LirStore,
    layouts: *const layout_mod.Store,
    rc_local: []const bool,
    solution: *const arc_solve.Solution,
    options: ComputeOptions,
) Error!Dismantles {
    if (rc_local.len != store.localCount()) {
        dismantleInvariant("ARC dismantle resource table did not cover every local");
    }
    // Proc parameters are defined by the proc entry rather than a statement;
    // remember each parameter's body so its spine has a start. A local that
    // parameterizes more than one proc spec never dismantles.
    var param_bodies = collections.DenseMap(LIR.LocalId, ?LIR.CFStmtId).init(gpa);
    defer param_bodies.deinit();
    const is_param = try gpa.alloc(bool, store.localCount());
    defer gpa.free(is_param);
    @memset(is_param, false);
    const operand_read_counts = try gpa.alloc(u32, store.localCount());
    defer gpa.free(operand_read_counts);
    @memset(operand_read_counts, 0);
    const direct_call_stmt_by_arg = try gpa.alloc(u32, store.localCount());
    defer gpa.free(direct_call_stmt_by_arg);
    @memset(direct_call_stmt_by_arg, no_index);
    const direct_call_position_by_arg = try gpa.alloc(u8, store.localCount());
    defer gpa.free(direct_call_position_by_arg);
    @memset(direct_call_position_by_arg, std.math.maxInt(u8));
    for (0..store.procSpecCount()) |proc_index| {
        const proc = store.getProcSpec(@fromBackingInt(@intCast(@as(u32, @intCast(proc_index)))));
        const body = proc.body orelse continue;
        const params = store.getLocalSpan(proc.args);
        for (0..GuardedList.borrowLen(params)) |position| {
            const param = GuardedList.at(params, position);
            is_param[@backingInt(param)] = true;
            const slot = try param_bodies.getOrPut(param);
            if (slot.found_existing) {
                slot.value_ptr.* = null;
            } else {
                slot.value_ptr.* = body;
            }
        }
    }

    // Establish reachability before classifying dismantle candidates. Complete
    // projection roots must be known at the candidate gate: a borrowed payload
    // struct can carry its parent's exact unit in an owned emission and is then
    // a real dismantlable container, rather than an independently borrowed
    // value with no unit to spend. The uniqueness solve already listed every
    // statement reachable from each procedure. Join identities belong to a
    // procedure, so the same lists name each statement's procedure. The
    // candidate's defining CFG statement identifies that procedure explicitly,
    // including dismantle temporaries that are deliberately absent from a
    // specialization frame.
    var reachable = try std.bit_set.DynamicBitSetUnmanaged.initEmpty(gpa, store.cfStmtCount());
    defer reachable.deinit(gpa);
    const stmt_proc = try gpa.alloc(u32, store.cfStmtCount());
    defer gpa.free(stmt_proc);
    @memset(stmt_proc, no_index);
    // Statements reached by more than one procedure have no single owner.
    var shared_stmts = try std.bit_set.DynamicBitSetUnmanaged.initEmpty(gpa, store.cfStmtCount());
    defer shared_stmts.deinit(gpa);
    var dominance = try ComponentDominance.init(gpa, store, solution, stmt_proc);
    defer dominance.deinit(gpa);
    for (0..store.procSpecCount()) |proc_index| {
        const proc: u32 = @intCast(proc_index);
        for (solution.procStatements(store, @fromBackingInt(proc))) |stmt_id| {
            const stmt_index = @backingInt(stmt_id);
            reachable.set(stmt_index);
            const owner = stmt_proc[stmt_index];
            if (owner == no_index) {
                stmt_proc[stmt_index] = proc;
            } else if (owner != proc) {
                shared_stmts.set(stmt_index);
                dominance.unite(owner, proc);
            }
        }
    }

    // Every per-statement fact the whole-store passes below consume, gathered
    // in disjoint statement ranges off the coordinator.
    const range_count = @max(1, std.math.divCeil(usize, store.cfStmtCount(), statement_facts_range_size) catch unreachable);
    const fact_ranges = try gpa.alloc(StatementFacts, range_count);
    defer gpa.free(fact_ranges);
    for (fact_ranges, 0..) |*range, index| {
        range.* = .{
            .store = store,
            .layouts = layouts,
            .solution = solution,
            .reachable = &reachable,
            .start = @intCast(@min(index * statement_facts_range_size, store.cfStmtCount())),
            .end = @intCast(@min((index + 1) * statement_facts_range_size, store.cfStmtCount())),
            .allocator = gpa,
        };
    }
    defer for (fact_ranges) |*range| range.deinit();
    try collectStatementFacts(gpa, fact_ranges, options);

    const explicit_init_join = try gpa.alloc(bool, store.localCount());
    defer gpa.free(explicit_init_join);
    @memset(explicit_init_join, false);
    for (fact_ranges) |range| {
        for (range.join_init_targets.items) |target| explicit_init_join[@backingInt(target)] = true;
    }

    const projected_root = try gpa.alloc(u32, store.localCount());
    defer gpa.free(projected_root);
    @memset(projected_root, no_index);
    var projection_edge_count: usize = 0;
    for (fact_ranges) |range| projection_edge_count += range.projection_edges.items.len;
    var projection_edges = try std.ArrayList(ProjectionEdge).initCapacity(gpa, projection_edge_count);
    defer projection_edges.deinit(gpa);
    for (fact_ranges) |range| projection_edges.appendSliceAssumeCapacity(range.projection_edges.items);
    var projection_changed = true;
    while (projection_changed) {
        projection_changed = false;
        for (projection_edges.items) |edge| {
            const source_index = @backingInt(edge.source);
            const source_unit = solution.unitLocalOf(edge.source);
            const source_root = if (projected_root[source_index] != no_index)
                projected_root[source_index]
            else if (edge.is_projection and (!solution.isBorrowed(source_unit) or is_param[@backingInt(source_unit)] or solution.isJoinParam(source_unit)))
                @backingInt(source_unit)
            else
                no_index;
            if (source_root == no_index) continue;
            const target_index = @backingInt(edge.target);
            if (projected_root[target_index] == no_index) {
                projected_root[target_index] = source_root;
                projection_changed = true;
            } else if (projected_root[target_index] != source_root and projected_root[target_index] != no_index - 1) {
                projected_root[target_index] = no_index - 1;
                projection_changed = true;
            }
        }
    }
    for (projected_root) |*root| {
        if (root.* == no_index - 1) root.* = no_index;
    }
    const projected_container = try gpa.alloc(u32, store.localCount());
    defer gpa.free(projected_container);
    @memset(projected_container, no_index);
    const projected_stmt = try gpa.alloc(u32, store.localCount());
    defer gpa.free(projected_stmt);
    @memset(projected_stmt, no_index);
    for (projection_edges.items) |edge| projected_stmt[@backingInt(edge.target)] = @backingInt(edge.stmt);
    // Compare only projections of the same explicit ownership root, rather
    // than scanning every local in the module for each projected container.
    const root_heads = try gpa.alloc(u32, store.localCount());
    defer gpa.free(root_heads);
    @memset(root_heads, no_index);
    const root_next = try gpa.alloc(u32, store.localCount());
    defer gpa.free(root_next);
    @memset(root_next, no_index);
    for (projected_root, 0..) |root, index| {
        if (root == no_index or projected_stmt[index] == no_index) continue;
        root_next[index] = root_heads[root];
        root_heads[root] = @intCast(index);
    }
    for (projected_root, 0..) |root, local_index| {
        if (root == no_index) continue;
        const local: LIR.LocalId = @fromBackingInt(@intCast(@as(u32, @intCast(local_index))));
        const local_layout = layouts.getLayout(store.getLocal(local).layout_idx);
        if (local_layout.tag != .struct_) continue;
        const target_stmt_index = projected_stmt[local_index];
        if (target_stmt_index == no_index) continue;
        var representative: u32 = @intCast(local_index);
        var other_index = root_heads[root];
        while (other_index != no_index) : (other_index = root_next[other_index]) {
            // A projection dominates itself and the representative it starts as.
            if (other_index == local_index) continue;
            const other: LIR.LocalId = @fromBackingInt(@intCast(@as(u32, @intCast(other_index))));
            if (store.getLocal(other).layout_idx != store.getLocal(local).layout_idx) continue;
            const other_stmt: LIR.CFStmtId = @fromBackingInt(@intCast(projected_stmt[other_index]));
            const target_stmt: LIR.CFStmtId = @fromBackingInt(@intCast(target_stmt_index));
            if (!try dominance.dominates(gpa, other_stmt, target_stmt)) continue;
            const representative_stmt: LIR.CFStmtId = @fromBackingInt(@intCast(projected_stmt[representative]));
            if (try dominance.dominates(gpa, other_stmt, representative_stmt)) {
                representative = @intCast(other_index);
            }
        }
        projected_container[local_index] = representative;
    }
    var analysis = Analysis{
        .gpa = gpa,
        .store = store,
        .layouts = layouts,
        .solution = solution,
        .rc_local = rc_local,
        .state = try gpa.alloc(State, store.localCount()),
        .alias_root = try gpa.alloc(u32, store.localCount()),
        .view_base = try gpa.alloc(u32, store.localCount()),
        .read_through_view = try gpa.alloc(bool, store.localCount()),
        .view_head = try gpa.alloc(u32, store.localCount()),
        .view_parent = try gpa.alloc(u32, store.localCount()),
        .view_candidates = .empty,
        .view_root = try gpa.alloc(u32, store.localCount()),
        .union_roots = .empty,
        .candidates = .empty,
        .is_param = is_param,
        .projected_root = projected_root,
        .projected_container = projected_container,
        .explicit_init_join = explicit_init_join,
        .owned_demand = try gpa.alloc(bool, store.localCount()),
        .mention_heads = try gpa.alloc(u32, store.localCount()),
        .boxy_dynamic_layouts = collections.DenseMap(layout_mod.Idx, bool).init(gpa),
        .layout_visited = std.AutoHashMap(layout_mod.Idx, void).init(gpa),
    };
    defer analysis.deinit();
    @memset(analysis.state, .unknown);
    @memset(analysis.alias_root, no_index);
    @memset(analysis.view_base, 0);
    @memset(analysis.read_through_view, false);
    @memset(analysis.view_head, no_index);
    @memset(analysis.view_parent, no_index);
    @memset(analysis.view_root, no_index);
    @memset(analysis.owned_demand, false);
    @memset(analysis.mention_heads, no_index);

    // One linear scan over every reachable statement, classifying each
    // occurrence of each local. The order is one depth-first walk from every
    // procedure body, last procedure first, which classifies a statement
    // shared by several procedures on its first visit: each procedure's
    // statement list, last procedure first, skipping statements already
    // classified.
    var visited = try std.bit_set.DynamicBitSetUnmanaged.initEmpty(gpa, store.cfStmtCount());
    defer visited.deinit(gpa);
    var proc_cursor = store.procSpecCount();
    while (proc_cursor > 0) {
        proc_cursor -= 1;
        const proc_id: LIR.LirProcSpecId = @fromBackingInt(@intCast(@as(u32, @intCast(proc_cursor))));
        for (solution.procStatements(store, proc_id)) |current| {
            const stmt_index = @backingInt(current);
            if (visited.isSet(stmt_index)) continue;
            visited.set(stmt_index);
            try classifyStatement(&analysis, store, solution, operand_read_counts, direct_call_stmt_by_arg, direct_call_position_by_arg, current);
        }
    }

    try analysis.closeOwnedDemand();
    var own_counts_it = analysis.candidates.valueIterator();
    while (own_counts_it.next()) |candidate| {
        candidate.own_read_count = @intCast(candidate.reads.items.len);
        candidate.own_whole_use_count = @intCast(candidate.whole_uses.items.len);
    }

    // Second phase: verify the surviving candidates' read shapes and spines,
    // and build the output.
    var result = Dismantles{
        .arena = base.SingleThreadArena.init(gpa),
        .takes = .init(gpa),
        .containers = .init(gpa),
        .owned_only_takes = .init(gpa),
        .owned_only_containers = .init(gpa),
        .owned_only_param_benefits = &.{},
        .take_bindings = &.{},
        .owned_only_binding_roots = &.{},
        .field_restitution_args = .empty,
        .projection_units = .init(gpa),
        .complete_takes = .init(gpa),
        .projection_aliases = .init(gpa),
        .views = .init(gpa),
    };
    errdefer result.deinit();
    result.owned_only_param_benefits = try result.arena.allocator().alloc(arc_sig.ParamMask, store.procSpecCount());
    @memset(result.owned_only_param_benefits, 0);
    result.take_bindings = try result.arena.allocator().alloc(bool, store.localCount());
    @memset(result.take_bindings, false);
    result.owned_only_binding_roots = try result.arena.allocator().alloc(u32, store.localCount());
    @memset(result.owned_only_binding_roots, no_index);

    var read_kinds = collections.DenseMap(LIR.CFStmtId, ReadKind).init(gpa);
    defer read_kinds.deinit();
    const proc_joins = try gpa.alloc(collections.DenseMap(u32, LIR.CFStmtId), store.procSpecCount());
    defer {
        for (proc_joins) |*joins| joins.deinit();
        gpa.free(proc_joins);
    }
    for (proc_joins) |*joins| joins.* = .init(gpa);
    const joins_ready = try gpa.alloc(bool, store.procSpecCount());
    defer gpa.free(joins_ready);
    @memset(joins_ready, false);
    const proc_graphs = try gpa.alloc(FutureFields.ProcGraph, store.procSpecCount());
    defer gpa.free(proc_graphs);
    // The one successor of every statement the take flow walk passes straight
    // through, or `no_index` for a statement it must decode. Each candidate's
    // walk reads this column instead of loading every statement it passes.
    const straight_next = try gpa.alloc(u32, store.cfStmtCount());
    defer gpa.free(straight_next);
    for (straight_next, 0..) |*next, stmt_index| {
        next.* = switch (store.getCFStmtImmediate(@fromBackingInt(@intCast(stmt_index))).*) {
            inline .init_uninitialized, .assign_ref, .assign_literal, .assign_call, .assign_call_erased, .assign_packed_erased_fn, .assign_boxy_desc_ref, .assign_boxy_dict_ref, .assign_boxy_box, .assign_boxy_record_update, .assign_boxy_unbox, .assign_boxy_adapt, .assign_boxy_inspect, .assign_boxy_eq, .assign_boxy_hash, .assign_boxy_tag, .assign_boxy_tag_payload, .assign_call_dict, .assign_low_level, .assign_list, .assign_struct, .assign_tag, .store_struct, .store_tag, .debug, .expect, .comptime_branch_taken, .incref, .decref, .decref_if_initialized, .free => |stmt| @backingInt(stmt.next),
            .set_local,
            .join,
            .switch_stmt,
            .jump,
            .switch_initialized_payload,
            .str_match,
            .boxy_tag_match,
            .str_match_set,
            .ret,
            .crash,
            .expect_err,
            .runtime_error,
            .comptime_exhaustiveness_failed,
            => no_index,
        };
    }

    var future_fields = FutureFields.init(gpa);
    defer future_fields.deinit(gpa);
    var body_states = collections.DenseMap(LIR.CFStmtId, FlowState).init(gpa);
    defer body_states.deinit();
    const FlowFrame = struct { cursor: LIR.CFStmtId, state: FlowState };
    var flow_frames = std.ArrayList(FlowFrame).empty;
    defer flow_frames.deinit(gpa);
    var field_restitutions = std.ArrayList(FieldRestitution).empty;
    defer field_restitutions.deinit(gpa);

    // The solved borrow forest, edges reversed: every binding borrowed
    // through a candidate's field read observes that field at each of its
    // mentions, and so does every binding borrowed onward from it.
    const BorrowChildEdge = struct { local: u32, next: u32 };
    var borrow_child_edges = std.ArrayList(BorrowChildEdge).empty;
    defer borrow_child_edges.deinit(gpa);
    const borrow_child_heads = try gpa.alloc(u32, store.localCount());
    defer gpa.free(borrow_child_heads);
    @memset(borrow_child_heads, no_index);
    for (0..store.localCount()) |index| {
        const source = solution.borrowSourceOf(@fromBackingInt(@intCast(@as(u32, @intCast(index))))) orelse continue;
        const source_index = @backingInt(source);
        try borrow_child_edges.append(gpa, .{ .local = @intCast(index), .next = borrow_child_heads[source_index] });
        borrow_child_heads[source_index] = @intCast(borrow_child_edges.items.len - 1);
    }
    var borrow_stack = std.ArrayList(u32).empty;
    defer borrow_stack.deinit(gpa);
    var rc_slots = std.ArrayList(layout_mod.Store.RcSlot).empty;
    defer rc_slots.deinit(gpa);
    var bounds = std.ArrayList(u32).empty;
    defer bounds.deinit(gpa);
    var pieces = std.ArrayList(SlotRange).empty;
    defer pieces.deinit(gpa);
    var read_masks = std.ArrayList(u64).empty;
    defer read_masks.deinit(gpa);
    // The containers of one component's solve: its candidates, then each view
    // whose definition took its slots, solved right after its container.
    const SolveEntry = struct { local: u32, view: bool };
    var solve_queue = std.ArrayList(SolveEntry).empty;
    defer solve_queue.deinit(gpa);
    // Per container: reads solved as owned-demanded takes whose binding is
    // borrowed, and those that turned out not to be takes. A borrowed binding
    // that holds no unit reads the container's slots at every later mention.
    var deferred_takes = std.ArrayList(u32).empty;
    defer deferred_takes.deinit(gpa);
    var declined_takes = std.AutoHashMapUnmanaged(u32, void).empty;
    defer declined_takes.deinit(gpa);
    var held_views = std.AutoHashMapUnmanaged(u32, void).empty;
    defer held_views.deinit(gpa);

    // A root's deferred reads are settled by their representatives' plans,
    // and a representative's layout is a proper part of its root's layout,
    // so the dependency graph is acyclic. Solve representatives before roots.
    var candidate_order = std.ArrayList(u32).empty;
    defer candidate_order.deinit(gpa);
    try candidate_order.ensureTotalCapacity(gpa, analysis.candidates.count());
    const order_marks = try gpa.alloc(OrderMark, store.localCount());
    defer gpa.free(order_marks);
    @memset(order_marks, .unvisited);
    var order_stack = std.ArrayList(u32).empty;
    defer order_stack.deinit(gpa);
    var order_it = analysis.candidates.keyIterator();
    while (order_it.next()) |key| {
        if (order_marks[key.*] != .unvisited) continue;
        try order_stack.append(gpa, key.*);
        while (order_stack.items.len != 0) {
            const index = order_stack.items[order_stack.items.len - 1];
            switch (order_marks[index]) {
                .unvisited => {
                    order_marks[index] = .visiting;
                    for (analysis.candidates.getPtr(index).?.equivalenced_reads.items) |deferred| {
                        const representative_index = @backingInt(deferred.representative);
                        switch (order_marks[representative_index]) {
                            .unvisited => if (analysis.candidates.contains(representative_index)) {
                                try order_stack.append(gpa, representative_index);
                            },
                            .visiting => dismantleInvariant("ARC dismantle projection representatives formed a cycle"),
                            .ordered => {},
                        }
                    }
                },
                .visiting => {
                    order_marks[index] = .ordered;
                    candidate_order.appendAssumeCapacity(index);
                    _ = order_stack.pop();
                },
                .ordered => _ = order_stack.pop(),
            }
        }
    }

    // A borrowed parameter whose owned variant takes a field is a mandatory
    // variant for every caller passing it a dying value, so a call argument
    // at such a position is an owned operand exactly like one at an owned
    // position. Its demand must be known before the caller's takes are
    // solved, so candidates are solved one call-graph component at a time,
    // callees first, and each component publishes its parameter benefits
    // before any caller is solved. Procedures sharing statements form one
    // graph node, because their shared statements have one owner here.
    const proc_count = store.procSpecCount();
    var scc_arena = std.heap.ArenaAllocator.init(gpa);
    defer scc_arena.deinit();
    const scc_allocator = scc_arena.allocator();
    const group_of_proc = try scc_allocator.alloc(u32, proc_count);
    for (group_of_proc, 0..) |*group, proc_index| group.* = dominance.root(@intCast(proc_index));
    var call_stmts = std.ArrayList(u32).empty;
    for (fact_ranges) |range| {
        for (range.calls.items) |call_stmt| try call_stmts.append(scc_allocator, @backingInt(call_stmt));
    }
    const succ_starts = try scc_allocator.alloc(u32, proc_count + 1);
    @memset(succ_starts, 0);
    for (call_stmts.items) |call_index| succ_starts[group_of_proc[stmt_proc[call_index]] + 1] += 1;
    for (1..proc_count + 1) |index| succ_starts[index] += succ_starts[index - 1];
    const succs = try scc_allocator.alloc(u32, call_stmts.items.len);
    const succ_fill = try scc_allocator.dupe(u32, succ_starts[0..proc_count]);
    for (call_stmts.items) |call_index| {
        const caller = group_of_proc[stmt_proc[call_index]];
        const callee = store.getCFStmtImmediate(@fromBackingInt(call_index)).assign_call.proc;
        succs[succ_fill[caller]] = group_of_proc[@backingInt(callee)];
        succ_fill[caller] += 1;
    }
    const preds = try collections.csr.transpose(scc_allocator, succ_starts, succs);
    const components = try collections.scc.compute(scc_allocator, succ_starts, succs, preds.starts, preds.targets);
    const component_count = components.count();
    const component_of_proc = try scc_allocator.alloc(u32, proc_count);
    for (component_of_proc, group_of_proc) |*component, group| component.* = components.component_of[group];

    var candidate_keys = std.ArrayList(u32).empty;
    var candidate_values = std.ArrayList(u32).empty;
    for (candidate_order.items) |candidate_index| {
        const stmt = candidateStatement(analysis.candidates.getPtr(candidate_index).?) orelse continue;
        try candidate_keys.append(scc_allocator, component_of_proc[stmt_proc[@backingInt(stmt)]]);
        try candidate_values.append(scc_allocator, candidate_index);
    }
    for (candidate_order.items) |candidate_index| {
        const candidate = analysis.candidates.getPtr(candidate_index).?;
        const stmt = candidateStatement(candidate) orelse continue;
        for (candidate.equivalenced_reads.items) |deferred| {
            const representative = analysis.candidates.getPtr(@backingInt(deferred.representative)) orelse continue;
            const representative_stmt = candidateStatement(representative) orelse continue;
            if (component_of_proc[stmt_proc[@backingInt(representative_stmt)]] != component_of_proc[stmt_proc[@backingInt(stmt)]]) {
                dismantleInvariant("ARC dismantle projection representative left its root's call-graph component");
            }
        }
    }
    const candidate_buckets = try ComponentBuckets.build(scc_allocator, component_count, candidate_keys.items, candidate_values.items);
    const call_keys = try scc_allocator.alloc(u32, call_stmts.items.len);
    for (call_keys, call_stmts.items) |*key, call_index| key.* = component_of_proc[stmt_proc[call_index]];
    const call_buckets = try ComponentBuckets.build(scc_allocator, component_count, call_keys, call_stmts.items);
    var all_place_reads = std.ArrayList(PlaceRead).empty;
    for (fact_ranges) |range| try all_place_reads.appendSlice(scc_allocator, range.place_reads.items);
    const place_read_keys = try scc_allocator.alloc(u32, all_place_reads.items.len);
    const place_read_values = try scc_allocator.alloc(u32, all_place_reads.items.len);
    for (all_place_reads.items, place_read_keys, place_read_values, 0..) |read, *key, *value, index| {
        key.* = component_of_proc[stmt_proc[read.stmt]];
        value.* = @intCast(index);
    }
    const place_read_buckets = try ComponentBuckets.build(scc_allocator, component_count, place_read_keys, place_read_values);
    const proc_values = try scc_allocator.alloc(u32, proc_count);
    for (proc_values, 0..) |*value, index| value.* = @intCast(index);
    const proc_buckets = try ComponentBuckets.build(scc_allocator, component_count, component_of_proc, proc_values);

    const ambiguous_index = no_index - 1;
    const ParamInfo = struct { proc: u32, position: u16 };
    var param_info = std.AutoHashMapUnmanaged(u32, ParamInfo).empty;
    defer param_info.deinit(gpa);
    for (0..proc_count) |proc_index| {
        const proc_id: LIR.LirProcSpecId = @fromBackingInt(@intCast(@as(u32, @intCast(proc_index))));
        const params = store.getLocalSpan(store.getProcSpecImmediate(proc_id).args);
        for (0..GuardedList.borrowLen(params)) |position| {
            const param = GuardedList.at(params, position);
            const param_index = @backingInt(param);
            if (position >= arc_sig.tracked_param_count) continue;
            const param_slot = try param_info.getOrPut(gpa, param_index);
            if (param_slot.found_existing) {
                param_slot.value_ptr.* = .{ .proc = ambiguous_index, .position = 0 };
                continue;
            }
            param_slot.value_ptr.* = .{ .proc = @intCast(proc_index), .position = @intCast(position) };
        }
    }

    // Every committed write of the component being solved, so a recursive
    // component whose own benefits add demand can retract and solve again.
    const UndoEntry = union(enum) {
        take: LIR.CFStmtId,
        owned_only_take: LIR.CFStmtId,
        complete_take: LIR.CFStmtId,
        take_binding: LIR.LocalId,
        binding_root: LIR.LocalId,
        restitution: CallArg,
        container: LIR.LocalId,
        owned_only_container: LIR.LocalId,
    };
    var undo = std.ArrayList(UndoEntry).empty;
    defer undo.deinit(gpa);

    // `root` is the place's unit-carrying origin, which parameter benefits
    // trace through every link. `owner` is the nearest binding on the
    // place's chain that holds a unit of its own: the root, or a take
    // binding or owned-only binding between the root and this local, which
    // holds the field unit its take consumed. `projected` is relative to
    // `owner`.
    const PlaceOrigin = struct {
        root: u32,
        owner: u32,
        projected: bool,
    };
    const PlaceEdge = struct {
        source: u32,
        target: u32,
        kind: PlaceEdgeKind,
        next: u32,
    };
    var places = std.AutoHashMapUnmanaged(u32, PlaceOrigin).empty;
    defer places.deinit(gpa);
    var component_place_keys = std.ArrayList(u32).empty;
    defer component_place_keys.deinit(gpa);
    var place_edges = std.ArrayList(PlaceEdge).empty;
    defer place_edges.deinit(gpa);
    // Per-component edge lists, headed by flat per-key tables whose touched
    // entries alone are reset between components.
    const place_heads = try scc_allocator.alloc(u32, store.localCount());
    @memset(place_heads, no_index);
    var place_sources = std.ArrayList(u32).empty;
    defer place_sources.deinit(gpa);
    var place_work = std.ArrayList(u32).empty;
    defer place_work.deinit(gpa);
    const BenefitEdge = struct {
        source_key: u32,
        next: u32,
    };
    var benefit_edges = std.ArrayList(BenefitEdge).empty;
    defer benefit_edges.deinit(gpa);
    const benefit_heads = try scc_allocator.alloc(u32, proc_count * arc_sig.tracked_param_count);
    @memset(benefit_heads, no_index);
    var benefit_targets = std.ArrayList(u32).empty;
    defer benefit_targets.deinit(gpa);
    var benefit_work = std.ArrayList(u32).empty;
    defer benefit_work.deinit(gpa);

    var component = component_count;
    while (component > 0) {
        component -= 1;
        const scc_candidates = candidate_buckets.of(component);
        const scc_calls = call_buckets.of(component);
        const scc_place_reads = place_read_buckets.of(component);
        const scc_procs = proc_buckets.of(component);
        var recursive = false;
        for (scc_calls) |call_index| {
            const callee = store.getCFStmtImmediate(@fromBackingInt(call_index)).assign_call.proc;
            if (component_of_proc[@backingInt(callee)] == component) recursive = true;
        }
        undo.clearRetainingCapacity();
        var solved = false;
        while (true) {
            // A call argument at a borrowed position whose owned variant
            // takes a field is an owned operand, like one at an owned
            // position.
            var demand_changed = false;
            for (scc_calls) |call_index| {
                const call = store.getCFStmtImmediate(@fromBackingInt(call_index)).assign_call;
                if (solution.isPinnedProc(call.proc)) continue;
                const benefits = result.owned_only_param_benefits[@backingInt(call.proc)];
                if (benefits == 0) continue;
                const callee_sig = solution.sigOf(call.proc);
                const args = store.getLocalSpan(call.args);
                for (0..@min(GuardedList.borrowLen(args), arc_sig.tracked_param_count)) |position| {
                    const bit = arc_sig.paramBit(position).?;
                    if ((benefits & bit) == 0 or callee_sig.paramMode(position) != .borrowed) continue;
                    const arg = GuardedList.at(args, position);
                    if (!rc_local[@backingInt(arg)]) continue;
                    if (try analysis.demandOwnedClosed(arg)) demand_changed = true;
                }
            }
            if (solved) {
                // The component's own benefits demanded nothing new: its
                // last solve is final.
                if (!demand_changed) break;
                var undo_index = undo.items.len;
                while (undo_index > 0) {
                    undo_index -= 1;
                    switch (undo.items[undo_index]) {
                        .take => |stmt| _ = result.takes.remove(stmt),
                        .owned_only_take => |stmt| _ = result.owned_only_takes.remove(stmt),
                        .complete_take => |stmt| _ = result.complete_takes.remove(stmt),
                        .take_binding => |local| result.take_bindings[@backingInt(local)] = false,
                        .binding_root => |local| result.owned_only_binding_roots[@backingInt(local)] = no_index,
                        .restitution => |call_arg| _ = result.field_restitution_args.remove(call_arg),
                        .container => |local| _ = result.containers.remove(local),
                        .owned_only_container => |local| _ = result.owned_only_containers.remove(local),
                    }
                }
                undo.clearRetainingCapacity();
                for (component_place_keys.items) |key| _ = places.remove(key);
                for (scc_procs) |proc_index| result.owned_only_param_benefits[proc_index] = 0;
            }
            solved = true;

            // A deferred read whose borrowed target is re-emitted owned
            // retains through its representative exactly like an owned
            // target, so it is the same whole use of that representative.
            for (scc_candidates) |candidate_index| {
                const candidate = analysis.candidates.getPtr(candidate_index).?;
                candidate.whole_uses.shrinkRetainingCapacity(candidate.own_whole_use_count);
            }
            for (scc_candidates) |candidate_index| {
                const candidate = analysis.candidates.getPtr(candidate_index).?;
                for (candidate.equivalenced_reads.items) |deferred| {
                    const target_index = @backingInt(deferred.read.target);
                    if (!solution.isBorrowed(deferred.read.target) or !analysis.owned_demand[target_index]) continue;
                    const representative = analysis.candidates.getPtr(@backingInt(deferred.representative)) orelse continue;
                    try representative.whole_uses.append(gpa, deferred.read.stmt);
                }
            }
            solve_queue.clearRetainingCapacity();
            for (scc_candidates) |candidate_index| try solve_queue.append(gpa, .{ .local = candidate_index, .view = false });
            var queue_index: usize = 0;
            candidates: while (queue_index < solve_queue.items.len) : (queue_index += 1) {
                const entry = solve_queue.items[queue_index];
                const candidate_index = entry.local;
                const local: LIR.LocalId = @fromBackingInt(@intCast(candidate_index));
                const candidate = if (entry.view) analysis.view_candidates.getPtr(candidate_index).? else analysis.candidates.getPtr(candidate_index).?;
                if (candidate.disqualified) continue;
                // Every read of a view's container went through the view, so only
                // views nested inside it can hold a read's slots.
                const reads_stop: u32 = if (entry.view) candidate_index else no_index;
                if (!entry.view) {
                    // A deferred read whose representative committed a plan is that
                    // representative's alias; every other one is an ordinary field read
                    // of this container.
                    candidate.reads.shrinkRetainingCapacity(candidate.own_read_count);
                    for (candidate.equivalenced_reads.items) |deferred| {
                        if (result.containers.contains(deferred.representative) or
                            result.owned_only_containers.contains(deferred.representative)) continue;
                        try candidate.reads.append(gpa, deferred.read);
                    }
                }
                if (candidate.reads.items.len == 0) continue;

                // A payload view dismantles its tag union: the view holds no unit of
                // its own, but an ownership-complete view of a root that dies whole,
                // is defined once, and is otherwise only aliased or discriminated
                // spends the root's unit through its fields.
                const union_root: ?*UnionRoot = if (!entry.view and analysis.view_root[@backingInt(local)] != no_index) blk: {
                    const root_entry = analysis.union_roots.getPtr(analysis.view_root[@backingInt(local)]) orelse continue :candidates;
                    if (root_entry.disqualified or root_entry.def_count != 1 or root_entry.view != @backingInt(local)) continue :candidates;
                    break :blk root_entry;
                } else null;

                // An ownership-complete projected struct can receive its root's exact
                // unit in an owned emission, and an owned same-value binding holds the
                // unit its definition gave it. A borrowed reference-defined container
                // holds no unit of its own to dismantle.
                const spine_start: LIR.CFStmtId = if (candidate.join_starts.items.len != 0)
                    candidate.join_starts.items[0]
                else if (candidate.def_count == 1)
                    switch (store.getCFStmtImmediate(candidate.def_stmt).*) {
                        inline .assign_literal, .assign_call, .assign_call_erased, .assign_packed_erased_fn, .assign_low_level, .assign_list, .assign_struct, .assign_tag => |stmt| stmt.next,
                        .assign_ref => |stmt| if (entry.view or
                            union_root != null or
                            analysis.projected_root[@backingInt(local)] != no_index or
                            (stmt.op.kind == .local and !solution.isBorrowed(local))) stmt.next else continue :candidates,
                        .set_local => continue :candidates,
                        .init_uninitialized,
                        .assign_boxy_desc_ref,
                        .assign_boxy_dict_ref,
                        .assign_boxy_box,
                        .assign_boxy_record_update,
                        .assign_boxy_unbox,
                        .assign_boxy_adapt,
                        .assign_boxy_inspect,
                        .assign_boxy_eq,
                        .assign_boxy_hash,
                        .assign_boxy_tag,
                        .assign_boxy_tag_payload,
                        .boxy_tag_match,
                        .assign_call_dict,
                        .store_struct,
                        .store_tag,
                        .debug,
                        .expect,
                        .expect_err,
                        .runtime_error,
                        .comptime_exhaustiveness_failed,
                        .comptime_branch_taken,
                        .incref,
                        .decref,
                        .decref_if_initialized,
                        .free,
                        .switch_stmt,
                        .switch_initialized_payload,
                        .str_match,
                        .str_match_set,
                        .join,
                        .jump,
                        .ret,
                        .crash,
                        => continue :candidates,
                    }
                else if (candidate.def_count == 0)
                    ((param_bodies.get(local) orelse continue :candidates) orelse continue :candidates)
                else
                    continue :candidates;

                // A container released through its Boxy descriptor has fields whose
                // release needs that descriptor too, so they cannot be released one
                // at a time from their layouts.
                if (store.getLocal(local).boxy_desc != null) continue :candidates;

                // Only refcounted fields carry stored units worth taking; a
                // container with no refcounted take keeps its ordinary whole
                // release.
                const local_layout_idx = store.getLocal(local).layout_idx;
                rc_slots.clearRetainingCapacity();
                try layouts.appendRcSlots(gpa, local_layout_idx, 0, &rc_slots);
                // Cut the slots at every read's bounds, and keep the pieces
                // holding stored units: one mask bit each.
                bounds.clearRetainingCapacity();
                try bounds.append(gpa, 0);
                try bounds.append(gpa, layouts.ownershipSlotCount(local_layout_idx));
                for (candidate.reads.items) |read| {
                    try bounds.append(gpa, read.slots.first);
                    try bounds.append(gpa, read.slots.end());
                }
                std.sort.pdq(u32, bounds.items, {}, std.sort.asc(u32));
                pieces.clearRetainingCapacity();
                var next_rc: usize = 0;
                for (bounds.items[0 .. bounds.items.len - 1], bounds.items[1..]) |low, high| {
                    if (low == high) continue;
                    while (next_rc < rc_slots.items.len and rc_slots.items[next_rc].slot < low) next_rc += 1;
                    if (next_rc == rc_slots.items.len or rc_slots.items[next_rc].slot >= high) continue;
                    try pieces.append(gpa, .{ .first = low, .count = high - low });
                }
                if (pieces.items.len > 64) continue :candidates;
                const rc_mask: u64 = if (pieces.items.len == 64) ~@as(u64, 0) else (@as(u64, 1) << @intCast(pieces.items.len)) - 1;
                read_masks.clearRetainingCapacity();
                for (candidate.reads.items) |read| {
                    var mask: u64 = 0;
                    for (pieces.items, 0..) |piece, index| {
                        if (piece.first >= read.slots.first and piece.end() <= read.slots.end()) mask |= @as(u64, 1) << @intCast(index);
                    }
                    try read_masks.append(gpa, mask);
                }

                if (candidate.reads.items.len == 0) dismantleInvariant("field-take candidate had no field projection");
                const first_read = @backingInt(candidate.reads.items[0].stmt);
                const owner = stmt_proc[first_read];
                if (owner == no_index or shared_stmts.isSet(first_read)) continue;
                if (!joins_ready[owner]) {
                    joins_ready[owner] = true;
                    try collectProcJoinBodies(store, solution.procStatements(store, @fromBackingInt(@intCast(owner))), &proc_joins[owner]);
                    const proc_body = store.getProcSpec(@fromBackingInt(@intCast(owner))).body.?;
                    proc_graphs[owner] = try future_fields.buildProcGraph(gpa, store, &.{proc_body}, &proc_joins[owner]);
                }
                const join_bodies = &proc_joins[owner];

                // Verify each field by a forward dataflow from the container's
                // definition over the control-flow graph: a consuming read is a take
                // only if the field cannot have been taken yet at that point, a
                // borrow of a taken field must run before every take that could
                // reach it. Divergent residuals remain exact per path in ARC. Loops
                // poison themselves unless an explicit reinitialization or outcome
                // receipt supplies the field again before the next take.
                declined_takes.clearRetainingCapacity();
                const taken_mask: u64 = attempt: while (true) {
                    // A view whose value reaches an owned operand is solved as the take
                    // of its whole range, and while it is, it holds its slots: reads
                    // through it are its own, solved once it is committed. A view the
                    // solve declines is a borrowed view again, whose reads are this
                    // container's.
                    held_views.clearRetainingCapacity();
                    for (candidate.reads.items) |read| {
                        if (!read.view or !solution.isBorrowed(read.target)) continue;
                        const target_index = @backingInt(read.target);
                        if (!analysis.owned_demand[target_index] or declined_takes.contains(target_index)) continue;
                        try held_views.put(gpa, target_index, {});
                    }
                    deferred_takes.clearRetainingCapacity();
                    read_kinds.clearRetainingCapacity();
                    field_restitutions.clearRetainingCapacity();
                    for (candidate.reads.items, read_masks.items, 0..) |read, bit, read_index| {
                        if (bit == 0) continue;
                        if (readsThroughHeldView(&analysis, &held_views, read.through, reads_stop)) continue;
                        const target_index = @backingInt(read.target);
                        const demanded = solution.isBorrowed(read.target) and analysis.owned_demand[target_index];
                        const consuming = !solution.isBorrowed(read.target) or (demanded and !declined_takes.contains(target_index));
                        if (demanded and consuming) try deferred_takes.append(gpa, @intCast(read_index));
                        // A borrowed view's definition copies only the
                        // representation; its reads and other uses observe the
                        // slots themselves.
                        if (read.view and !consuming) continue;
                        try read_kinds.put(read.stmt, .{
                            .bit = bit,
                            .consuming = consuming,
                        });

                        // Initial closed field-place restitution capability. The field
                        // binding must have exactly one operand read, and that read must
                        // be one represented owned position of a direct call whose
                        // result is refined by the explicit alias/discriminant/switch
                        // continuation. This is a total syntactic/metadata boundary:
                        // unsupported continuations simply keep the ordinary schedule.
                        if (!consuming or operand_read_counts[@backingInt(read.target)] != 1) continue;
                        const call_stmt_index = direct_call_stmt_by_arg[@backingInt(read.target)];
                        const position = direct_call_position_by_arg[@backingInt(read.target)];
                        if (call_stmt_index == no_index or position == std.math.maxInt(u8)) continue;
                        const call_stmt_id: LIR.CFStmtId = @fromBackingInt(@intCast(call_stmt_index));
                        const call_stmt = store.getCFStmtImmediate(call_stmt_id);
                        if (call_stmt.* != .assign_call) continue;
                        const args = store.getLocalSpan(call_stmt.assign_call.args);
                        if (position >= GuardedList.borrowLen(args) or GuardedList.at(args, position) != read.target) continue;
                        const callee_sig = solution.sigOf(call_stmt.assign_call.proc);
                        if (callee_sig.paramMode(position) != .owned) continue;
                        const outcomes = solution.availableOutcomeSpanOf(call_stmt.assign_call.proc);
                        const refinement = (try arc_solve.findOutcomeRefinement(gpa, store, call_stmt.assign_call.target, call_stmt.assign_call.next, outcomes)) orelse continue;
                        const position_bit = arc_sig.paramBit(position).?;
                        var any_restitution = false;
                        for (solution.sigTable().outcomesOf(.{ .outcomes = outcomes })) |outcome| {
                            any_restitution = any_restitution or (outcome.restituted_params & position_bit) != 0;
                        }
                        if (!any_restitution) continue;
                        try field_restitutions.append(gpa, .{
                            .call_arg = .{ .stmt = call_stmt_id, .position = position },
                            .projection = read.stmt,
                            .switch_stmt = refinement,
                            .field_mask = bit,
                            .outcomes = outcomes,
                        });
                    }
                    // A whole use behaves like a borrow of every field at once: no take
                    // may run before it on any path, so the value it moves or observes
                    // is the intact container.
                    for (candidate.whole_uses.items) |stmt| {
                        const slot = try read_kinds.getOrPut(stmt);
                        if (slot.found_existing) {
                            slot.value_ptr.bit = ~@as(u64, 0);
                            slot.value_ptr.consuming = false;
                        } else {
                            slot.value_ptr.* = .{ .bit = ~@as(u64, 0), .consuming = false };
                        }
                    }
                    // A borrowed field read binds a name that reads the field's stored
                    // unit at every later mention, without holding a unit of its own, and
                    // every binding borrowed onward from it does the same. Each such
                    // mention is a borrow observation of that field: a take of the field
                    // may not run before any of them on any path. An owned-demanded read
                    // is such a binding exactly when it is not a take.
                    var forced_poison: u64 = 0;
                    for (candidate.reads.items, read_masks.items) |read, bit| {
                        if (bit == 0) continue;
                        if (readsThroughHeldView(&analysis, &held_views, read.through, reads_stop)) continue;
                        const target_index = @backingInt(read.target);
                        if (!solution.isBorrowed(read.target)) continue;
                        if (analysis.owned_demand[target_index] and !declined_takes.contains(target_index)) continue;
                        borrow_stack.clearRetainingCapacity();
                        try borrow_stack.append(gpa, @backingInt(read.target));
                        var borrow_steps: usize = 0;
                        while (borrow_stack.pop()) |borrower| {
                            borrow_steps += 1;
                            if (borrow_steps > store.localCount()) dismantleInvariant("ARC dismantle borrow forest contained a cycle");
                            var mention = analysis.mention_heads[borrower];
                            while (mention != no_index) {
                                const edge = analysis.mention_edges.items[mention];
                                const slot = try read_kinds.getOrPut(@fromBackingInt(@intCast(edge.stmt)));
                                if (!slot.found_existing) {
                                    slot.value_ptr.* = .{ .bit = bit, .consuming = false };
                                } else if (slot.value_ptr.consuming) {
                                    forced_poison |= bit;
                                } else {
                                    slot.value_ptr.bit |= bit;
                                }
                                mention = edge.next;
                            }
                            var child = borrow_child_heads[borrower];
                            while (child != no_index) {
                                const child_edge = borrow_child_edges.items[child];
                                // A read through a view is a read of this
                                // container with its own slots and observations.
                                if (!analysis.read_through_view[child_edge.local]) {
                                    try borrow_stack.append(gpa, child_edge.local);
                                }
                                child = child_edge.next;
                            }
                        }
                    }

                    var candidate_mask: u64 = 0;
                    for (candidate.reads.items) |read| {
                        const kind = read_kinds.get(read.stmt) orelse continue;
                        if (kind.consuming) candidate_mask |= kind.bit;
                    }
                    candidate_mask &= rc_mask;
                    const solved_mask: u64 = solve: {
                        if (candidate_mask == 0) break :solve 0;
                        var poison: u64 = 0;
                        // A borrowed payload view supplies no unit of its own. Only producing
                        // its union root again restores fresh fields across a loop back edge;
                        // recreating the view still observes the same stored units.
                        const fresh_def = if (union_root) |root| root.def_stmt else candidate.def_stmt;
                        const redefinition: ?LIR.CFStmtId = if (candidate.join_starts.items.len == 0) fresh_def else null;
                        try future_fields.compute(gpa, store, solution, local, proc_graphs[owner], &read_kinds, field_restitutions.items, redefinition);
                        for (candidate.reads.items) |read| {
                            const kind = read_kinds.getPtr(read.stmt) orelse continue;
                            if (!kind.consuming) continue;
                            const definition = store.getCFStmtImmediate(read.stmt).assign_ref;
                            if (future_fields.observed(definition.next) & kind.bit != 0) {
                                kind.consuming = false;
                            }
                        }
                        candidate_mask = 0;
                        for (candidate.reads.items) |read| {
                            const kind = read_kinds.get(read.stmt) orelse continue;
                            if (kind.consuming) candidate_mask |= kind.bit;
                        }
                        if (candidate_mask == 0) break :solve 0;
                        body_states.clearRetainingCapacity();
                        flow_frames.clearRetainingCapacity();
                        try flow_frames.append(gpa, .{ .cursor = spine_start, .state = .{ .may = 0, .must = 0 } });
                        for (candidate.join_starts.items, 0..) |start, index| {
                            if (index == 0) continue;
                            try flow_frames.append(gpa, .{ .cursor = start, .state = .{ .may = 0, .must = 0 } });
                        }
                        var steps: usize = 0;
                        // Each statement is re-walked at most once per lattice step of its
                        // reaching state; 2 bits per tracked field bound the lattice height.
                        const step_limit = (store.cfStmtCount() + 1) * (2 * 64 + 1);
                        flow: while (flow_frames.pop()) |frame| {
                            var cursor = frame.cursor;
                            var state = frame.state;
                            chain: while (true) {
                                steps += 1;
                                if (steps > step_limit) {
                                    poison = ~@as(u64, 0);
                                    break :flow;
                                }
                                // Reaching the container's single value-producing definition
                                // again, through a loop back edge, starts the next
                                // iteration's fresh value with every field intact; the
                                // previous value is dead past its redefinition exactly as it
                                // is past an explicit join-cell write.
                                if (redefinition) |def_stmt| if (cursor == def_stmt) {
                                    state = .{ .may = 0, .must = 0 };
                                };
                                if (read_kinds.getPtr(cursor)) |kind| {
                                    kind.visited = true;
                                    if (kind.consuming) {
                                        // A take where the field may already be gone would
                                        // double-consume its unit on that path.
                                        poison |= state.may & kind.bit;
                                        state.may |= kind.bit;
                                        state.must |= kind.bit;
                                    } else {
                                        // A borrow after a possible take would observe the
                                        // taker's mutation instead of the original field.
                                        poison |= state.may & kind.bit;
                                    }
                                }
                                const straight = straight_next[@backingInt(cursor)];
                                if (straight != no_index) {
                                    cursor = @fromBackingInt(straight);
                                    continue :chain;
                                }
                                switch (store.getCFStmtImmediate(cursor).*) {
                                    inline .init_uninitialized, .assign_ref, .assign_literal, .assign_call, .assign_call_erased, .assign_packed_erased_fn, .assign_boxy_desc_ref, .assign_boxy_dict_ref, .assign_boxy_box, .assign_boxy_record_update, .assign_boxy_unbox, .assign_boxy_adapt, .assign_boxy_inspect, .assign_boxy_eq, .assign_boxy_hash, .assign_boxy_tag, .assign_boxy_tag_payload, .assign_call_dict, .assign_low_level, .assign_list, .assign_struct, .assign_tag, .store_struct, .store_tag, .debug, .expect, .comptime_branch_taken, .incref, .decref, .decref_if_initialized, .free => |stmt| cursor = stmt.next,
                                    .set_local => |stmt| {
                                        // The value operand above still observes the old
                                        // definition. Only the explicit write starts a fresh
                                        // intact field domain for the next loop iteration.
                                        if (stmt.target == local) state = .{ .may = 0, .must = 0 };
                                        cursor = stmt.next;
                                    },
                                    .join => |stmt| {
                                        cursor = stmt.remainder;
                                    },
                                    .switch_stmt => |stmt| {
                                        const heads = store.getCFSwitchBranches(stmt.branches);
                                        for (0..GuardedList.borrowLen(heads)) |i| {
                                            const branch = GuardedList.at(heads, i);
                                            const restored = restoredFieldMaskForBranch(solution, field_restitutions.items, cursor, branch.value);
                                            try flow_frames.append(gpa, .{
                                                .cursor = branch.body,
                                                .state = .{ .may = state.may & ~restored, .must = state.must & ~restored },
                                            });
                                        }
                                        const default_restored = restoredFieldMaskForDefault(solution, store, field_restitutions.items, cursor);
                                        state.may &= ~default_restored;
                                        state.must &= ~default_restored;
                                        cursor = stmt.default_branch;
                                    },
                                    .jump => |stmt| {
                                        // Control continues at the join's body; meet this
                                        // path's state into it and re-walk on change. A jump
                                        // to a join declared before the definition leaves
                                        // the candidate's region—a loop back edge or an
                                        // enclosing early exit—so it ends this path like a
                                        // return would. Reads living past it are never
                                        // visited, which keeps their fields residual.
                                        const body = join_bodies.get(@backingInt(stmt.target)) orelse break :chain;
                                        const slot = try body_states.getOrPut(body);
                                        if (slot.found_existing) {
                                            const merged = FlowState.meet(slot.value_ptr.*, state);
                                            if (FlowState.eql(merged, slot.value_ptr.*)) break :chain;
                                            slot.value_ptr.* = merged;
                                            try flow_frames.append(gpa, .{ .cursor = body, .state = merged });
                                        } else {
                                            slot.value_ptr.* = state;
                                            try flow_frames.append(gpa, .{ .cursor = body, .state = state });
                                        }
                                        break :chain;
                                    },
                                    .switch_initialized_payload => |stmt| {
                                        try flow_frames.append(gpa, .{ .cursor = stmt.initialized_branch, .state = state });
                                        cursor = stmt.uninitialized_branch;
                                    },
                                    inline .str_match, .boxy_tag_match => |stmt| {
                                        try flow_frames.append(gpa, .{ .cursor = stmt.on_match, .state = state });
                                        cursor = stmt.on_miss;
                                    },
                                    .str_match_set => |stmt| {
                                        const arms = store.getStrMatchArms(stmt.arms);
                                        for (0..GuardedList.borrowLen(arms)) |i| {
                                            try flow_frames.append(gpa, .{ .cursor = GuardedList.at(arms, i).on_match, .state = state });
                                        }
                                        cursor = stmt.on_miss;
                                    },
                                    .ret, .crash, .expect_err, .runtime_error, .comptime_exhaustiveness_failed => {
                                        break :chain;
                                    },
                                }
                            }
                        }

                        // A read the flow never reached sits outside the verified region;
                        // its field keeps ordinary retains and residual release.
                        var kinds_it = read_kinds.valueIterator();
                        while (kinds_it.next()) |kind| {
                            if (!kind.visited) poison |= kind.bit;
                        }

                        poison |= forced_poison;
                        break :solve candidate_mask & ~poison;
                    };
                    // An owned-demanded read whose binding stays borrowed is no take,
                    // so its later mentions observe its slots: solve again with them.
                    var declined_any = false;
                    for (deferred_takes.items) |read_index| {
                        const read = candidate.reads.items[read_index];
                        const bit = read_masks.items[read_index];
                        const kind = read_kinds.get(read.stmt) orelse dismantleInvariant("ARC dismantle lost a deferred take's read");
                        if (kind.consuming and (solved_mask & bit) == bit) continue;
                        try declined_takes.put(gpa, @backingInt(read.target), {});
                        declined_any = true;
                    }
                    if (!declined_any) break :attempt solved_mask;
                };
                if (taken_mask == 0) continue;

                // Accepted. The committed field-place domain is the layout's
                // refcounted slots. ARC owns the path-sensitive residual mask
                // and release normalization.

                // A parameter solved borrowed dismantles only in emissions whose
                // demand vector overrides it to owned; everything else applies to
                // every emission of its proc.
                const projection_root_index = analysis.projected_root[@backingInt(local)];
                const projected = projection_root_index != no_index and union_root == null;
                const activation_root: LIR.LocalId = if (projected) @fromBackingInt(@intCast(projection_root_index)) else local;
                // A view holds its slots exactly in the emissions its own take
                // applies to.
                const owned_only = if (entry.view)
                    !result.take_bindings[candidate_index]
                else
                    solution.isBorrowed(activation_root) and union_root == null;
                if (projected) {
                    // The projection read itself moves the outer unit into this
                    // container in precisely those emissions where the root is owned.
                    // Publishing the binding edge makes the ordinary ARC transfer and
                    // certifier machinery carry that unit; no retain is suppressed
                    // without the explicit complete-projection receipt.
                    try result.complete_takes.put(candidate.def_stmt, activation_root);
                    try undo.append(gpa, .{ .complete_take = candidate.def_stmt });
                    if (owned_only) {
                        result.owned_only_binding_roots[@backingInt(local)] = @backingInt(activation_root);
                        try undo.append(gpa, .{ .binding_root = local });
                    } else {
                        result.take_bindings[@backingInt(local)] = true;
                        try undo.append(gpa, .{ .take_binding = local });
                    }
                }
                const stored_pieces = try result.arena.allocator().dupe(SlotRange, pieces.items);
                for (candidate.reads.items, read_masks.items) |read, bit| {
                    if (bit == 0 or (taken_mask & bit) != bit) continue;
                    if (!(read_kinds.get(read.stmt) orelse continue).consuming) continue;
                    if (solution.isBorrowed(read.target) and !analysis.owned_demand[@backingInt(read.target)]) continue;
                    const take = Take{ .root = local, .field_mask = bit };
                    if (owned_only) {
                        try result.owned_only_takes.put(read.stmt, take);
                        try undo.append(gpa, .{ .owned_only_take = read.stmt });
                        // A target solved owned holds its own unit in every emission;
                        // the take only spares its retain. Only a target solved
                        // borrowed needs its binding overridden to owned, and a
                        // borrowed binding has exactly one defining read, so exactly
                        // one parameter root authorizes the override.
                        if (!solution.isBorrowed(read.target)) continue;
                        const target_index = @backingInt(read.target);
                        const prior = result.owned_only_binding_roots[target_index];
                        if (prior != no_index and prior != @backingInt(activation_root)) {
                            dismantleInvariant("ARC owned-only field binding had conflicting parameter roots");
                        }
                        result.owned_only_binding_roots[target_index] = @backingInt(activation_root);
                        try undo.append(gpa, .{ .binding_root = read.target });
                    } else {
                        try result.takes.put(read.stmt, take);
                        result.take_bindings[@backingInt(read.target)] = true;
                        try undo.append(gpa, .{ .take = read.stmt });
                        try undo.append(gpa, .{ .take_binding = read.target });
                    }
                }
                // A view whose definition took its slots is a container of its
                // own, solved next.
                for (candidate.reads.items, read_masks.items) |read, bit| {
                    if (!read.view or bit == 0 or (taken_mask & bit) != bit) continue;
                    if (!(read_kinds.get(read.stmt) orelse continue).consuming) continue;
                    if (!solution.isBorrowed(read.target)) continue;
                    try solve_queue.append(gpa, .{ .local = @backingInt(read.target), .view = true });
                }
                for (field_restitutions.items) |receipt| {
                    if ((receipt.field_mask & taken_mask) == 0) continue;
                    if (!(read_kinds.get(receipt.projection) orelse continue).consuming) continue;
                    const slot = try result.field_restitution_args.getOrPut(gpa, receipt.call_arg);
                    if (slot.found_existing) {
                        if (slot.value_ptr.place.root != local or
                            slot.value_ptr.place.field_mask != receipt.field_mask or
                            slot.value_ptr.projection != receipt.projection or
                            slot.value_ptr.refinement != receipt.switch_stmt)
                        {
                            dismantleInvariant("ARC field restitution call argument had conflicting committed places");
                        }
                    } else {
                        slot.value_ptr.* = .{
                            .place = .{ .root = local, .field_mask = receipt.field_mask },
                            .projection = receipt.projection,
                            .refinement = receipt.switch_stmt,
                        };
                        try undo.append(gpa, .{ .restitution = receipt.call_arg });
                    }
                }
                if (union_root) |view_root_entry| {
                    const root_local: LIR.LocalId = @fromBackingInt(@intCast(analysis.view_root[@backingInt(local)]));
                    try result.containers.put(root_local, .{
                        .layout_idx = local_layout_idx,
                        .pieces = stored_pieces,
                        .full_mask = rc_mask,
                        .payload_view = .{
                            .view = local,
                            .tag_discriminant = view_root_entry.tag_discriminant,
                        },
                    });
                    try undo.append(gpa, .{ .container = root_local });
                } else if (owned_only) {
                    try result.owned_only_containers.put(local, .{ .layout_idx = local_layout_idx, .pieces = stored_pieces, .full_mask = rc_mask });
                    try undo.append(gpa, .{ .owned_only_container = local });
                } else {
                    try result.containers.put(local, .{ .layout_idx = local_layout_idx, .pieces = stored_pieces, .full_mask = rc_mask });
                    try undo.append(gpa, .{ .container = local });
                }
            }

            // Publish the component's parameter benefits. Variant admission
            // consumes them exactly, without rescanning bodies or
            // reconstructing parameter identity from statements.
            for (scc_procs) |proc_index| {
                const proc_id: LIR.LirProcSpecId = @fromBackingInt(proc_index);
                const params = store.getLocalSpan(store.getProcSpecImmediate(proc_id).args);
                for (0..GuardedList.borrowLen(params)) |position| {
                    const bit = arc_sig.paramBit(position) orelse break;
                    const param = GuardedList.at(params, position);
                    if (result.owned_only_containers.contains(param)) {
                        result.owned_only_param_benefits[proc_index] |= bit;
                    }
                }
            }

            // Projected dismantles activate from the parameter that owns
            // their root, not from the borrowed projection binding itself.
            for (scc_candidates) |candidate_index| {
                const local: LIR.LocalId = @fromBackingInt(candidate_index);
                if (!result.owned_only_containers.contains(local)) continue;
                const root_index = result.owned_only_binding_roots[candidate_index];
                if (root_index == no_index) continue;
                const source_info = param_info.get(root_index) orelse continue;
                if (source_info.proc == ambiguous_index) continue;
                if (solution.isPinnedProc(@fromBackingInt(source_info.proc))) continue;
                if (component_of_proc[source_info.proc] != component) {
                    dismantleInvariant("ARC dismantle parameter benefit left its call-graph component");
                }
                result.owned_only_param_benefits[source_info.proc] |= arc_sig.paramBit(source_info.position).?;
            }

            // A complete aggregate projection denotes the same ownership
            // place as its root: it can transfer that root's unit without
            // manufacturing a second one. Discover the component's places to
            // a fixpoint over complete projections and transparent aliases.
            // A projection that materializes as an alias of its committed
            // representative links as that alias.
            place_edges.clearRetainingCapacity();
            for (place_sources.items) |source| place_heads[source] = no_index;
            place_sources.clearRetainingCapacity();
            component_place_keys.clearRetainingCapacity();
            for (scc_place_reads) |read_index| {
                const read = all_place_reads.items[read_index];
                const link = materializedPlaceLink(store, layouts, solution, &result, projected_container, projected_stmt, read) orelse continue;
                const previous_head = place_heads[link.source];
                if (previous_head == no_index) try place_sources.append(gpa, link.source);
                try place_edges.append(gpa, .{
                    .source = link.source,
                    .target = link.target,
                    .kind = link.kind,
                    .next = previous_head,
                });
                place_heads[link.source] = @intCast(place_edges.items.len - 1);
            }

            // Seed only locals that can actually carry a unit: owned
            // bindings, proc or join parameters (which can be owned in an
            // emission), and borrowed pure aliases whose `unitLocalOf`
            // resolves to one of those roots. A borrowed payload local is
            // therefore reached only through its explicit complete-projection
            // edge.
            place_work.clearRetainingCapacity();
            for (place_sources.items) |source_index| {
                const source: LIR.LocalId = @fromBackingInt(source_index);
                const root = solution.unitLocalOf(source);
                const root_index = @backingInt(root);
                const root_can_own = !solution.isBorrowed(root) or
                    is_param[root_index] or
                    solution.isJoinParam(root);
                if (!root_can_own) continue;
                try places.put(gpa, source_index, .{ .root = root_index, .owner = root_index, .projected = false });
                try component_place_keys.append(gpa, source_index);
                try place_work.append(gpa, source_index);
            }

            while (place_work.pop()) |source_index| {
                const source_origin = places.get(source_index) orelse continue;
                if (source_origin.root == ambiguous_index) continue;
                var edge_index = place_heads[source_index];
                while (edge_index != no_index) {
                    const edge = place_edges.items[edge_index];
                    const target: LIR.LocalId = @fromBackingInt(edge.target);
                    // A join parameter is a cell with one definition per
                    // incoming edge, not an SSA value. Its edge-specific
                    // transfer is handled by join solving; one global place
                    // origin would incorrectly apply one edge's source to
                    // every arrival.
                    if (solution.isJoinParam(target)) {
                        edge_index = edge.next;
                        continue;
                    }
                    if (edge.kind == .projection and !solution.isBorrowed(target)) {
                        edge_index = edge.next;
                        continue;
                    }
                    // A take binding (owned-only ones included, in the
                    // emissions their take applies to) holds the field unit
                    // its take consumed, so it starts an ownership place of
                    // its own; the root's unit no longer includes that field.
                    const target_holds_unit = result.take_bindings[edge.target] or
                        result.owned_only_binding_roots[edge.target] != no_index;
                    const arrival: PlaceOrigin = if (target_holds_unit)
                        .{ .root = source_origin.root, .owner = edge.target, .projected = false }
                    else
                        .{
                            .root = source_origin.root,
                            .owner = source_origin.owner,
                            .projected = source_origin.projected or edge.kind == .projection,
                        };
                    const slot = try places.getOrPut(gpa, edge.target);
                    if (!slot.found_existing) {
                        slot.value_ptr.* = arrival;
                        try component_place_keys.append(gpa, edge.target);
                        try place_work.append(gpa, edge.target);
                    } else if (slot.value_ptr.root != ambiguous_index and slot.value_ptr.root != arrival.root) {
                        slot.value_ptr.* = .{ .root = ambiguous_index, .owner = ambiguous_index, .projected = false };
                        try place_work.append(gpa, edge.target);
                    } else if (slot.value_ptr.root == arrival.root) {
                        if (slot.value_ptr.owner != ambiguous_index and slot.value_ptr.owner != arrival.owner) {
                            slot.value_ptr.owner = ambiguous_index;
                            slot.value_ptr.projected = false;
                            try place_work.append(gpa, edge.target);
                        } else if (slot.value_ptr.owner == arrival.owner and arrival.projected and !slot.value_ptr.projected) {
                            slot.value_ptr.projected = true;
                            try place_work.append(gpa, edge.target);
                        }
                    }
                    edge_index = edge.next;
                }
            }

            // A complete projection can move its root's exact unit in an
            // owned parameter emission. Publish that mechanical capability to
            // variant admission just like an ordinary field dismantle; the
            // path solver remains responsible for proving whether the move is
            // legal at this particular read (including outcome-conditioned
            // restitution).
            for (place_edges.items) |edge| {
                if (edge.kind != .projection) continue;
                if (solution.isBorrowed(@fromBackingInt(edge.target))) continue;
                const origin = places.get(edge.source) orelse continue;
                if (origin.root == ambiguous_index) continue;
                const source_info = param_info.get(origin.root) orelse continue;
                if (source_info.proc == ambiguous_index) continue;
                if (solution.isPinnedProc(@fromBackingInt(source_info.proc))) continue;
                if (component_of_proc[source_info.proc] != component) {
                    dismantleInvariant("ARC dismantle parameter benefit left its call-graph component");
                }
                result.owned_only_param_benefits[source_info.proc] |= arc_sig.paramBit(source_info.position).?;
            }

            // A leaf procedure's ordinary field dismantle makes its parameter
            // beneficial. Direct calls propagate that benefit through
            // complete places and plain forwarding, admitting owned parameter
            // variants all the way up a derived-encoder wrapper chain. Every
            // callee outside the component is already final.
            benefit_edges.clearRetainingCapacity();
            for (benefit_targets.items) |target_key| benefit_heads[target_key] = no_index;
            benefit_targets.clearRetainingCapacity();
            benefit_work.clearRetainingCapacity();
            for (scc_calls) |call_index| {
                const call = store.getCFStmtImmediate(@fromBackingInt(call_index)).assign_call;
                const args = store.getLocalSpan(call.args);
                for (0..@min(GuardedList.borrowLen(args), arc_sig.tracked_param_count)) |position| {
                    const arg = GuardedList.at(args, position);
                    const arg_index = @backingInt(arg);
                    const place_origin = places.get(arg_index);
                    const root_index: u32 = if (place_origin) |origin| origin.root else @backingInt(solution.unitLocalOf(arg));
                    if (root_index == no_index or root_index == ambiguous_index) continue;
                    const source_info = param_info.get(root_index) orelse continue;
                    if (source_info.proc == ambiguous_index) continue;
                    if (solution.isPinnedProc(@fromBackingInt(source_info.proc))) continue;
                    if (component_of_proc[source_info.proc] != component) {
                        dismantleInvariant("ARC dismantle parameter benefit left its call-graph component");
                    }
                    const target_key: u32 = @intCast(@backingInt(call.proc) * arc_sig.tracked_param_count + position);
                    const previous_head = benefit_heads[target_key];
                    try benefit_edges.append(gpa, .{
                        .source_key = @intCast(source_info.proc * arc_sig.tracked_param_count + source_info.position),
                        .next = previous_head,
                    });
                    benefit_heads[target_key] = @intCast(benefit_edges.items.len - 1);
                    if (previous_head == no_index) try benefit_targets.append(gpa, target_key);
                    // A final benefit outside the component seeds the
                    // propagation once; the component's own bits seed below.
                    if (previous_head == no_index and component_of_proc[@backingInt(call.proc)] != component and
                        (result.owned_only_param_benefits[@backingInt(call.proc)] & arc_sig.paramBit(position).?) != 0)
                    {
                        try benefit_work.append(gpa, target_key);
                    }
                }
            }
            // Propagate each newly beneficial parameter exactly once. A
            // wrapper chain is therefore linear in its calls rather than one
            // full call-site scan per wrapper depth.
            for (scc_procs) |proc_index| {
                const mask = result.owned_only_param_benefits[proc_index];
                for (0..arc_sig.tracked_param_count) |position| {
                    if ((mask & arc_sig.paramBit(position).?) == 0) continue;
                    try benefit_work.append(gpa, @intCast(proc_index * arc_sig.tracked_param_count + position));
                }
            }
            while (benefit_work.pop()) |target_key| {
                var edge_index = benefit_heads[target_key];
                while (edge_index != no_index) {
                    const edge = benefit_edges.items[edge_index];
                    const source_proc_index = edge.source_key / arc_sig.tracked_param_count;
                    const source_bit = arc_sig.paramBit(edge.source_key % arc_sig.tracked_param_count).?;
                    if ((result.owned_only_param_benefits[source_proc_index] & source_bit) == 0) {
                        result.owned_only_param_benefits[source_proc_index] |= source_bit;
                        try benefit_work.append(gpa, edge.source_key);
                    }
                    edge_index = edge.next;
                }
            }

            if (!recursive) break;
        }

        // Borrowed complete projections keep their place's owner as their
        // unit key. A take binding owns its place and is never a projection
        // of it, so it keeps its own unit. Owned projection targets instead
        // receive a solve-time move opportunity at the read itself; if
        // liveness says the owner survives, ARC retains exactly as before
        // and the target owns the retained unit independently.
        for (component_place_keys.items) |key| {
            const origin = places.get(key).?;
            if (origin.root == ambiguous_index or origin.owner == ambiguous_index or !origin.projected) continue;
            const local: LIR.LocalId = @fromBackingInt(key);
            if (!solution.isBorrowed(local) or solution.isJoinParam(local)) continue;
            try result.projection_units.put(local, @fromBackingInt(origin.owner));
        }
        for (place_edges.items) |edge| {
            if (edge.kind != .projection) continue;
            const target: LIR.LocalId = @fromBackingInt(edge.target);
            // Unlike a borrowed place origin, this move is attached to one
            // exact incoming read. An owned join cell can therefore receive
            // the unit on this edge without conflating its other definitions.
            if (solution.isBorrowed(target)) continue;
            const origin = places.get(edge.source) orelse continue;
            if (origin.root == ambiguous_index or origin.owner == ambiguous_index) continue;
            try result.complete_takes.put(@fromBackingInt(edge.kind.projection), @fromBackingInt(origin.owner));
        }
    }
    // Materialize only equivalences whose representative actually acquired a
    // committed dismantle plan. Lookup-only projections retain their original
    // LIR and ownership schedule.
    for (projected_container, 0..) |representative, local_index| {
        if (representative == no_index or representative == local_index) continue;
        const representative_local: LIR.LocalId = @fromBackingInt(@intCast(representative));
        if (!result.containers.contains(representative_local) and
            !result.owned_only_containers.contains(representative_local)) continue;
        // The deferred reads of this alias were settled against this exact
        // plan, which only a candidate solved in dependency order can hold.
        if (order_marks[representative] != .ordered) {
            dismantleInvariant("ARC dismantle materialized an alias of a representative that was never solved");
        }
        const stmt_index = projected_stmt[local_index];
        if (stmt_index == no_index) continue;
        const stmt = store.getCFStmtPtr(@fromBackingInt(@intCast(stmt_index)));
        if (stmt.* != .assign_ref) dismantleInvariant("projected alias definition stopped being a reference read");
        stmt.assign_ref.op = .local(representative_local);
        try result.projection_aliases.put(@fromBackingInt(@intCast(@as(u32, @intCast(local_index)))), representative_local);
    }
    // Publish the views of committed containers. A tag union's container is
    // keyed by the union, whose payload view numbers the slots.
    for (analysis.state, 0..) |state, local_index| {
        if (state != .view) continue;
        const root_index = analysis.alias_root[local_index];
        const union_index = analysis.view_root[root_index];
        const key: LIR.LocalId = @fromBackingInt(@intCast(if (union_index != no_index) union_index else root_index));
        if (!result.containers.contains(key) and !result.owned_only_containers.contains(key)) continue;
        // Takes nested in a view's take apply in exactly its emissions, so
        // the innermost view whose definition is a take decides them all.
        var holder = analysis.view_head[local_index];
        while (holder != no_index and !result.take_bindings[holder] and result.owned_only_binding_roots[holder] == no_index) {
            holder = analysis.view_parent[holder];
        }
        // A view whose definition takes its slots in every emission holds
        // them itself.
        if (holder == analysis.view_head[local_index] and result.take_bindings[holder]) continue;
        try result.views.put(@fromBackingInt(@intCast(@as(u32, @intCast(local_index)))), .{
            .root = key,
            .base = analysis.view_base[local_index],
            .holder = if (holder == no_index) null else @fromBackingInt(@intCast(holder)),
            .holder_base = if (holder == no_index) 0 else analysis.view_base[holder],
            .is_holder = holder == analysis.view_head[local_index],
        });
    }
    return result;
}
