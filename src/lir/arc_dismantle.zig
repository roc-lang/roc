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
const collections = @import("collections");
const core = @import("lir_core");
const layout_mod = @import("layout");
const arc_sig = @import("arc_sig.zig");
const arc_solve = @import("arc_solve.zig");
const arc_takes = @import("arc_takes.zig");

const LIR = core.LIR;
const LirStore = core.LirStore;
const GuardedList = collections.GuardedList;
const Allocator = std.mem.Allocator;

/// Allocation errors returned while solving field takes.
pub const Error = std.mem.Allocator.Error;

const no_index: u32 = std.math.maxInt(u32);

/// One refcounted field a dismantled container still owns at its death
/// point: emission reads it into a temporary and releases the temporary.
pub const ResidualField = struct {
    /// Original field index, as `assign_ref .field` addresses it.
    field_idx: u32,
    layout_idx: layout_mod.Idx,
};

/// Residual ownership plan for one taken variant of a dismantled tag union.
/// At the death point, an arm keyed by the variant's discriminant releases
/// exactly the refcounted payload fields its takes left behind.
pub const VariantPlan = struct {
    variant_index: u16,
    tag_discriminant: u16,
    /// Whether the payload is addressed through a struct view. A direct
    /// payload is released as one value read straight off the union.
    payload_is_struct: bool,
    payload_layout: layout_mod.Idx,
    residual: []const ResidualField,
};

/// Residual ownership plan for one container local selected for dismantling.
pub const Container = struct {
    residual: []const ResidualField,
    /// Tag-union dismantling: the death point switches on the runtime
    /// discriminant, each planned variant releases its own residual, and
    /// every unplanned variant keeps the whole release. Empty for struct
    /// containers.
    variant_plans: []const VariantPlan = &.{},
    /// Layout for the discriminant scratch local of a tag-union dismantle,
    /// taken from an observed discriminant read of the container.
    disc_layout: ?layout_mod.Idx = null,
};

/// Per-procedure dismantling decisions consumed while emitting ARC operations.
pub const Dismantles = struct {
    arena: std.heap.ArenaAllocator,
    /// `assign_ref .field` statements whose reads consume the container's
    /// stored unit; emission skips their retain.
    takes: std.AutoHashMapUnmanaged(LIR.CFStmtId, void),
    /// Dismantled containers. At the container's death point emission
    /// releases the residual fields instead of the whole value.
    containers: std.AutoHashMapUnmanaged(LIR.LocalId, Container),
    /// Takes on containers that are proc parameters solved borrowed: valid
    /// only in emissions where the demand vector overrides the parameter to
    /// owned (mode-specialized variants). The value is the parameter local,
    /// so emission can check the override for the current variant.
    owned_only_takes: std.AutoHashMapUnmanaged(LIR.CFStmtId, LIR.LocalId),
    /// Containers behind `owned_only_takes`, keyed by the parameter local.
    owned_only_containers: std.AutoHashMapUnmanaged(LIR.LocalId, Container),
    /// Parameter positions whose owned variant activates an exact field take,
    /// indexed directly by source procedure id.
    owned_only_param_benefits: []arc_sig.ParamMask,

    pub fn deinit(self: *Dismantles) void {
        const gpa = self.arena.child_allocator;
        self.takes.deinit(gpa);
        self.containers.deinit(gpa);
        self.owned_only_takes.deinit(gpa);
        self.owned_only_containers.deinit(gpa);
        self.arena.deinit();
    }

    pub fn isTake(self: *const Dismantles, stmt: LIR.CFStmtId) bool {
        return self.takes.contains(stmt);
    }

    pub fn ownedOnlyTakeParam(self: *const Dismantles, stmt: LIR.CFStmtId) ?LIR.LocalId {
        return self.owned_only_takes.get(stmt);
    }

    pub fn containerOf(self: *const Dismantles, local: LIR.LocalId) ?Container {
        return self.containers.get(local);
    }

    pub fn ownedOnlyContainerOf(self: *const Dismantles, local: LIR.LocalId) ?Container {
        return self.owned_only_containers.get(local);
    }

    pub fn ownedOnlyParamBenefits(self: *const Dismantles, proc: LIR.LirProcSpecId) arc_sig.ParamMask {
        const index = @intFromEnum(proc);
        if (index >= self.owned_only_param_benefits.len) {
            dismantleInvariant("ARC owned-only benefit lookup exceeded the analyzed source-procedure table");
        }
        return self.owned_only_param_benefits[index];
    }
};

fn dismantleInvariant(comptime message: []const u8) noreturn {
    if (@import("builtin").mode == .Debug) std.debug.panic(message, .{});
    unreachable;
}

const State = enum(u8) {
    unknown,
    ineligible,
    candidate,
    /// Borrowed pure same-value alias of a candidate; reads through it
    /// attribute to the root container.
    transparent_alias,
    /// Borrowed struct view of one variant's payload of a tag-union
    /// candidate (or a pure alias of such a view); field reads through it
    /// attribute to the root container under that variant.
    payload_view,
};

/// Variant marker for reads of struct containers, which have no variant.
const no_variant: u16 = std.math.maxInt(u16);

const Read = struct {
    stmt: LIR.CFStmtId,
    target: LIR.LocalId,
    field_idx: u32,
    /// Variant the read addresses for tag-union containers; `no_variant`
    /// for struct containers.
    variant_index: u16,
    tag_discriminant: u16,
    consuming: bool,
};

const Candidate = struct {
    def_stmt: LIR.CFStmtId = @enumFromInt(no_index),
    def_count: u32 = 0,
    disqualified: bool = false,
    reads: std.ArrayList(Read) = .empty,
    /// Statements consuming or observing the container as one value—moved
    /// into an aggregate, passed to a call, returned, or join-carried. Takes
    /// stay valid as long as no whole use can run after a take, which the
    /// dataflow checks exactly like a borrow of every field at once.
    whole_uses: std.ArrayList(LIR.CFStmtId) = .empty,
    /// Locals holding this container's discriminant, each defined exactly
    /// once by that read. A switch on one of these partitions the paths by
    /// variant, which is what lets takes on one variant coexist with exits
    /// that never see that variant.
    disc_targets: std.ArrayList(LIR.LocalId) = .empty,
};

const Analysis = struct {
    gpa: Allocator,
    store: *const LirStore,
    layouts: *const layout_mod.Store,
    solution: *const arc_solve.Solution,
    state: []State,
    /// Root container local per transparent alias or payload view,
    /// `no_index` otherwise.
    alias_root: []u32,
    /// Variant addressed per payload view, parallel to `alias_root`.
    view_variant: []u16,
    /// Discriminant value per payload view, parallel to `alias_root`.
    view_disc: []u16,
    /// Definition count per local, saturating. Discriminant targets are
    /// trusted for switch partitioning only when defined exactly once.
    def_counts: []u8,
    candidates: std.AutoHashMapUnmanaged(u32, Candidate),
    /// Proc parameters. A parameter solved borrowed may still qualify as an
    /// owned-only candidate: mode-specialized variants re-emit it owned.
    is_param: []const bool,

    fn deinit(self: *Analysis) void {
        var it = self.candidates.valueIterator();
        while (it.next()) |candidate| {
            candidate.reads.deinit(self.gpa);
            candidate.whole_uses.deinit(self.gpa);
            candidate.disc_targets.deinit(self.gpa);
        }
        self.candidates.deinit(self.gpa);
        self.gpa.free(self.def_counts);
        self.gpa.free(self.view_disc);
        self.gpa.free(self.view_variant);
        self.gpa.free(self.alias_root);
        self.gpa.free(self.state);
    }

    /// Whether the local's layout and binding shape could ever benefit from
    /// dismantling. Cheap, no allocation; the full per-candidate work only
    /// happens for locals that pass.
    fn passesGate(self: *Analysis, local: LIR.LocalId) bool {
        const local_layout = self.layouts.getLayout(self.store.getLocal(local).layout_idx);
        if (local_layout.tag != .struct_ and local_layout.tag != .tag_union) return false;
        if (self.solution.isBorrowed(local) and !self.is_param[@intFromEnum(local)]) return false;
        if (self.solution.isJoinParam(local)) return false;
        if (self.solution.maybeUninitializedCondition(local) != null) return false;

        if (local_layout.tag == .tag_union) {
            const encoding = arc_takes.unionClaimEncoding(self.layouts, local_layout) orelse return false;
            var it = encoding.iterate();
            return it.next() != null;
        }

        const info = self.layouts.getStructInfo(local_layout);
        var any_rc = false;
        for (0..info.fields.len) |i| {
            const field = info.fields.get(@intCast(i));
            if (field.index >= 64) return false;
            if (self.layouts.layoutContainsRefcounted(self.layouts.getLayout(field.layout))) {
                any_rc = true;
            }
        }
        return any_rc;
    }

    fn entryOf(self: *Analysis, local: LIR.LocalId) Error!?*Candidate {
        const index = @intFromEnum(local);
        switch (self.state[index]) {
            .ineligible, .transparent_alias, .payload_view => return null,
            .candidate => return self.candidates.getPtr(index).?,
            .unknown => {
                if (!self.passesGate(local)) {
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
        const index = @intFromEnum(local);
        if (self.state[index] == .transparent_alias or self.state[index] == .payload_view) {
            return @enumFromInt(self.alias_root[index]);
        }
        return local;
    }

    fn disqualify(self: *Analysis, local: LIR.LocalId) void {
        const root = self.resolveRoot(local);
        const index = @intFromEnum(root);
        if (self.state[index] == .candidate) {
            if (self.candidates.getPtr(index)) |candidate| candidate.disqualified = true;
        }
        self.state[index] = .ineligible;
    }

    /// Any occurrence that is not a field read: the local (or the container
    /// it aliases) cannot dismantle.
    fn useWhole(self: *Analysis, local: LIR.LocalId) void {
        self.disqualify(local);
    }

    /// A whole-value use of the container as an operand—moved into an
    /// aggregate or a call, returned, or join-carried. The container stays
    /// eligible; the dataflow rejects takes that could run before it.
    fn useWholeAt(self: *Analysis, local: LIR.LocalId, stmt: LIR.CFStmtId) Error!void {
        const root = self.resolveRoot(local);
        const candidate = (try self.entryOf(root)) orelse return;
        try candidate.whole_uses.append(self.gpa, stmt);
    }

    /// A definition of `local` by `stmt`. Candidates must be bound exactly
    /// once by a value-producing assignment.
    fn noteDef(self: *Analysis, local: LIR.LocalId, stmt: LIR.CFStmtId) Error!void {
        const index = @intFromEnum(local);
        self.def_counts[index] +|= 1;
        if (self.state[index] == .transparent_alias or self.state[index] == .payload_view) {
            // A second definition of an alias re-points it; the root can no
            // longer attribute its reads.
            self.disqualify(local);
            self.state[index] = .ineligible;
            return;
        }
        const candidate = (try self.entryOf(local)) orelse return;
        candidate.def_count += 1;
        if (candidate.def_count > 1) {
            candidate.disqualified = true;
        } else {
            candidate.def_stmt = stmt;
        }
    }

    /// A definition that never produces a candidate binding but still counts
    /// toward the local's definition total.
    fn noteNonCandidateDef(self: *Analysis, local: LIR.LocalId) void {
        self.def_counts[@intFromEnum(local)] +|= 1;
    }

    fn noteFieldRead(self: *Analysis, stmt: LIR.CFStmtId, source: LIR.LocalId, field_idx: u32, target: LIR.LocalId) Error!void {
        const source_index = @intFromEnum(source);
        const through_view = self.state[source_index] == .payload_view;
        const root = self.resolveRoot(source);
        const candidate = (try self.entryOf(root)) orelse return;
        const root_layout = self.layouts.getLayout(self.store.getLocal(root).layout_idx);
        if ((root_layout.tag == .tag_union) != through_view) {
            // A direct field read of a tag union, or a view of a struct:
            // neither addresses the container's field space.
            self.disqualify(root);
            return;
        }
        try candidate.reads.append(self.gpa, .{
            .stmt = stmt,
            .target = target,
            .field_idx = field_idx,
            .variant_index = if (through_view) self.view_variant[source_index] else no_variant,
            .tag_discriminant = if (through_view) self.view_disc[source_index] else no_variant,
            .consuming = !self.solution.isBorrowed(target),
        });
    }

    /// A read of the container's discriminant. The tag word is disjoint from
    /// every stored payload unit, so the read itself needs no ordering with
    /// takes; the target is remembered so switches on it can partition the
    /// dataflow by variant.
    fn noteDiscriminantRead(self: *Analysis, source: LIR.LocalId, target: LIR.LocalId) Error!void {
        const source_index = @intFromEnum(source);
        if (self.state[source_index] == .payload_view) {
            self.disqualify(source);
            return;
        }
        const root = self.resolveRoot(source);
        const candidate = (try self.entryOf(root)) orelse return;
        try candidate.disc_targets.append(self.gpa, target);
    }

    /// A `tag_payload_struct` read of one variant's payload. A struct
    /// payload read borrowed against the container becomes a payload view;
    /// a non-struct payload read is the variant's single field. Handles the
    /// target's definition bookkeeping itself: a view target is an alias
    /// binding, not a candidate definition.
    fn notePayloadStructRead(
        self: *Analysis,
        stmt: LIR.CFStmtId,
        source: LIR.LocalId,
        variant_index: u16,
        tag_discriminant: u16,
        target: LIR.LocalId,
    ) Error!void {
        const source_index = @intFromEnum(source);
        if (self.state[source_index] == .payload_view) {
            self.disqualify(source);
            try self.noteDef(target, stmt);
            return;
        }
        const root = self.resolveRoot(source);
        const candidate = (try self.entryOf(root)) orelse {
            self.disqualify(source);
            try self.noteDef(target, stmt);
            return;
        };
        const root_layout = self.layouts.getLayout(self.store.getLocal(root).layout_idx);
        if (root_layout.tag != .tag_union) {
            self.disqualify(root);
            try self.noteDef(target, stmt);
            return;
        }
        const info = self.layouts.getTagUnionInfo(root_layout);
        if (variant_index >= info.variants.len) {
            self.disqualify(root);
            try self.noteDef(target, stmt);
            return;
        }
        const payload_layout = self.layouts.getLayout(info.variants.get(variant_index).payload_layout);
        if (payload_layout.tag == .struct_) {
            const target_index = @intFromEnum(target);
            self.def_counts[target_index] +|= 1;
            if (self.state[target_index] == .transparent_alias or self.state[target_index] == .payload_view) {
                // Re-pointing an existing alias: neither root can attribute
                // reads through it any longer.
                self.disqualify(target);
                self.disqualify(root);
                self.state[target_index] = .ineligible;
                return;
            }
            const transparent = self.solution.isBorrowed(target) and
                self.solution.leaderOf(target) == root;
            if (!transparent) {
                self.disqualify(root);
                self.disqualify(target);
                return;
            }
            if (self.state[target_index] == .candidate) {
                if (self.candidates.getPtr(target_index)) |view_candidate| view_candidate.disqualified = true;
            }
            self.state[target_index] = .payload_view;
            self.alias_root[target_index] = @intFromEnum(root);
            self.view_variant[target_index] = variant_index;
            self.view_disc[target_index] = tag_discriminant;
            return;
        }
        // The payload is the variant's single field, addressed as field 0.
        try candidate.reads.append(self.gpa, .{
            .stmt = stmt,
            .target = target,
            .field_idx = 0,
            .variant_index = variant_index,
            .tag_discriminant = tag_discriminant,
            .consuming = !self.solution.isBorrowed(target),
        });
        try self.noteDef(target, stmt);
    }

    fn noteAliasDef(self: *Analysis, target: LIR.LocalId, source: LIR.LocalId) Error!void {
        const target_index = @intFromEnum(target);
        self.def_counts[target_index] +|= 1;
        if (self.state[target_index] == .transparent_alias or self.state[target_index] == .payload_view) {
            // Redefinition of an existing alias: neither its old nor its new
            // root can attribute reads through it.
            self.disqualify(target);
            self.disqualify(source);
            self.state[target_index] = .ineligible;
            return;
        }
        const source_index = @intFromEnum(source);
        const source_is_view = self.state[source_index] == .payload_view;
        const root = self.resolveRoot(source);
        const transparent = self.solution.isBorrowed(target) and
            self.solution.leaderOf(target) == root and
            ((try self.entryOf(root)) != null);
        if (transparent) {
            // The alias target itself can never be a container.
            if (self.state[target_index] == .candidate) {
                if (self.candidates.getPtr(target_index)) |candidate| candidate.disqualified = true;
            }
            if (source_is_view) {
                self.state[target_index] = .payload_view;
                self.view_variant[target_index] = self.view_variant[source_index];
                self.view_disc[target_index] = self.view_disc[source_index];
            } else {
                self.state[target_index] = .transparent_alias;
            }
            self.alias_root[target_index] = @intFromEnum(root);
        } else {
            // An owned alias duplicates the value; the container keeps a
            // second observer.
            self.useWhole(source);
            self.disqualify(target);
        }
    }
};

/// Per-field take dataflow state at one point in a candidate's region:
/// which fields may have been taken on some path reaching this point, and
/// which must have been taken on every such path. A take is valid only where
/// the field cannot have been taken yet, a borrow only where it cannot have
/// been taken yet, and every exit must agree (`may == must`) so the residual
/// release is the same on all paths.
const FlowState = struct {
    may: u64,
    must: u64,
    /// Variants (by index) this path cannot carry at runtime, refined by
    /// switches on the container's own discriminant. An exit reached with a
    /// variant excluded owes nothing for that variant's takes: control can
    /// only arrive here when the container holds some other variant, whose
    /// residual release the death point dispatches separately.
    excluded: u64,

    fn meet(a: FlowState, b: FlowState) FlowState {
        return .{ .may = a.may | b.may, .must = a.must & b.must, .excluded = a.excluded & b.excluded };
    }

    fn eql(a: FlowState, b: FlowState) bool {
        return a.may == b.may and a.must == b.must and a.excluded == b.excluded;
    }
};

/// How one statement reads the candidate under dataflow: which field's bit
/// it touches, which variant it addresses (`no_variant` for struct fields
/// and whole uses), and whether it consumes (owned result) or borrows.
const ReadKind = struct {
    bit: u64,
    variant_index: u16,
    consuming: bool,
    visited: bool = false,
};

/// The take dataflow's record of one region exit: the fields whose takes
/// ran on every path reaching it, and the variants the path had excluded.
const ExitRecord = struct {
    must: u64,
    excluded: u64,
};

/// One (variant, discriminant) pair addressed by a union candidate's reads.
const VariantCase = struct {
    variant_index: u16,
    tag_discriminant: u16,
};

/// Solve takes for every reachable statement in the store.
pub fn compute(
    gpa: Allocator,
    store: *const LirStore,
    layouts: *const layout_mod.Store,
    solution: *const arc_solve.Solution,
) Error!Dismantles {
    // Proc parameters are defined by the proc entry rather than a statement;
    // remember each parameter's body so its spine has a start. A local that
    // parameterizes more than one proc spec never dismantles.
    var param_bodies = std.AutoHashMapUnmanaged(LIR.LocalId, ?LIR.CFStmtId).empty;
    defer param_bodies.deinit(gpa);
    const is_param = try gpa.alloc(bool, store.localCount());
    defer gpa.free(is_param);
    @memset(is_param, false);
    for (0..store.procSpecCount()) |proc_index| {
        const proc = store.getProcSpec(@enumFromInt(@as(u32, @intCast(proc_index))));
        const body = proc.body orelse continue;
        const params = store.getLocalSpan(proc.args);
        for (0..GuardedList.borrowLen(params)) |position| {
            const param = GuardedList.at(params, position);
            is_param[@intFromEnum(param)] = true;
            const slot = try param_bodies.getOrPut(gpa, param);
            if (slot.found_existing) {
                slot.value_ptr.* = null;
            } else {
                slot.value_ptr.* = body;
            }
        }
    }

    var analysis = Analysis{
        .gpa = gpa,
        .store = store,
        .layouts = layouts,
        .solution = solution,
        .state = try gpa.alloc(State, store.localCount()),
        .alias_root = try gpa.alloc(u32, store.localCount()),
        .view_variant = try gpa.alloc(u16, store.localCount()),
        .view_disc = try gpa.alloc(u16, store.localCount()),
        .def_counts = try gpa.alloc(u8, store.localCount()),
        .candidates = .empty,
        .is_param = is_param,
    };
    defer analysis.deinit();
    @memset(analysis.state, .unknown);
    @memset(analysis.alias_root, no_index);
    @memset(analysis.view_variant, no_variant);
    @memset(analysis.view_disc, no_variant);
    @memset(analysis.def_counts, 0);

    // One linear scan over every reachable statement, classifying each
    // occurrence of each local. The switch is exhaustive so a new statement
    // form fails to compile rather than silently escaping classification.
    var visited = try std.bit_set.DynamicBitSetUnmanaged.initEmpty(gpa, store.cfStmtCount());
    defer visited.deinit(gpa);
    var stack = std.ArrayList(LIR.CFStmtId).empty;
    defer stack.deinit(gpa);
    for (0..store.procSpecCount()) |proc_index| {
        const proc = store.getProcSpec(@enumFromInt(@as(u32, @intCast(proc_index))));
        if (proc.body) |body| try stack.append(gpa, body);
    }

    while (stack.pop()) |current| {
        const stmt_index = @intFromEnum(current);
        if (visited.isSet(stmt_index)) continue;
        visited.set(stmt_index);
        switch (store.getCFStmt(current)) {
            .init_uninitialized => |stmt| {
                analysis.noteNonCandidateDef(stmt.target);
                analysis.useWhole(stmt.target);
                try stack.append(gpa, stmt.next);
            },
            .assign_ref => |stmt| {
                switch (stmt.op) {
                    .field => |op| {
                        try analysis.noteFieldRead(current, op.source, op.field_idx, stmt.target);
                        try analysis.noteDef(stmt.target, current);
                    },
                    .local => |source| {
                        if (stmt.target == source) {
                            analysis.useWhole(source);
                        } else {
                            try analysis.noteAliasDef(stmt.target, source);
                        }
                    },
                    .discriminant => |op| {
                        try analysis.noteDiscriminantRead(op.source, stmt.target);
                        try analysis.noteDef(stmt.target, current);
                    },
                    .tag_payload => |op| {
                        analysis.useWhole(op.source);
                        try analysis.noteDef(stmt.target, current);
                    },
                    .tag_payload_struct => |op| {
                        try analysis.notePayloadStructRead(current, op.source, op.variant_index, op.tag_discriminant, stmt.target);
                    },
                    .list_reinterpret => |op| {
                        analysis.useWhole(op.backing_ref);
                        analysis.noteNonCandidateDef(stmt.target);
                        analysis.disqualify(stmt.target);
                    },
                    .nominal => |op| {
                        analysis.useWhole(op.backing_ref);
                        analysis.noteNonCandidateDef(stmt.target);
                        analysis.disqualify(stmt.target);
                    },
                }
                try stack.append(gpa, stmt.next);
            },
            .assign_literal => |stmt| {
                try analysis.noteDef(stmt.target, current);
                try stack.append(gpa, stmt.next);
            },
            .assign_call => |stmt| {
                const args = store.getLocalSpan(stmt.args);
                for (0..GuardedList.borrowLen(args)) |i| try analysis.useWholeAt(GuardedList.at(args, i), current);
                try analysis.noteDef(stmt.target, current);
                try stack.append(gpa, stmt.next);
            },
            .assign_call_erased => |stmt| {
                analysis.useWhole(stmt.closure);
                if (stmt.reuse_source) |reuse_source| analysis.useWhole(reuse_source);
                const args = store.getLocalSpan(stmt.args);
                for (0..GuardedList.borrowLen(args)) |i| try analysis.useWholeAt(GuardedList.at(args, i), current);
                try analysis.noteDef(stmt.target, current);
                try stack.append(gpa, stmt.next);
            },
            .assign_packed_erased_fn => |stmt| {
                if (stmt.capture) |capture| analysis.useWhole(capture);
                if (stmt.reuse) |reuse| analysis.useWhole(reuse);
                try analysis.noteDef(stmt.target, current);
                try stack.append(gpa, stmt.next);
            },
            .assign_low_level => |stmt| {
                const args = store.getLocalSpan(stmt.args);
                for (0..GuardedList.borrowLen(args)) |i| try analysis.useWholeAt(GuardedList.at(args, i), current);
                try analysis.noteDef(stmt.target, current);
                try stack.append(gpa, stmt.next);
            },
            .assign_list => |stmt| {
                const elems = store.getLocalSpan(stmt.elems);
                for (0..GuardedList.borrowLen(elems)) |i| try analysis.useWholeAt(GuardedList.at(elems, i), current);
                try analysis.noteDef(stmt.target, current);
                try stack.append(gpa, stmt.next);
            },
            .assign_struct => |stmt| {
                const fields = store.getLocalSpan(stmt.fields);
                for (0..GuardedList.borrowLen(fields)) |i| try analysis.useWholeAt(GuardedList.at(fields, i), current);
                try analysis.noteDef(stmt.target, current);
                try stack.append(gpa, stmt.next);
            },
            .assign_tag => |stmt| {
                if (stmt.payload) |payload| try analysis.useWholeAt(payload, current);
                try analysis.noteDef(stmt.target, current);
                try stack.append(gpa, stmt.next);
            },
            .store_struct => |stmt| {
                analysis.useWhole(stmt.dest);
                const fields = store.getLocalSpan(stmt.fields);
                for (0..GuardedList.borrowLen(fields)) |i| analysis.useWhole(GuardedList.at(fields, i));
                try stack.append(gpa, stmt.next);
            },
            .store_tag => |stmt| {
                analysis.useWhole(stmt.dest);
                if (stmt.payload) |payload| analysis.useWhole(payload);
                try stack.append(gpa, stmt.next);
            },
            .set_local => |stmt| {
                try analysis.useWholeAt(stmt.value, current);
                analysis.noteNonCandidateDef(stmt.target);
                analysis.useWhole(stmt.target);
                try stack.append(gpa, stmt.next);
            },
            .debug => |stmt| {
                analysis.useWhole(stmt.message);
                try stack.append(gpa, stmt.next);
            },
            .expect => |stmt| {
                analysis.useWhole(stmt.condition);
                try stack.append(gpa, stmt.next);
            },
            .expect_err => |stmt| analysis.useWhole(stmt.message),
            .runtime_error => {},
            .comptime_exhaustiveness_failed => {},
            .comptime_branch_taken => |stmt| try stack.append(gpa, stmt.next),
            // The input contract is RC-free LIR; if RC statements ever appear
            // here, classifying their operands as whole uses stays sound.
            .incref => |stmt| {
                analysis.useWhole(stmt.value);
                try stack.append(gpa, stmt.next);
            },
            .decref => |stmt| {
                analysis.useWhole(stmt.value);
                try stack.append(gpa, stmt.next);
            },
            .decref_if_initialized => |stmt| {
                analysis.useWhole(stmt.cond);
                analysis.useWhole(stmt.value);
                try stack.append(gpa, stmt.next);
            },
            .free => |stmt| {
                analysis.useWhole(stmt.value);
                try stack.append(gpa, stmt.next);
            },
            .switch_stmt => |stmt| {
                analysis.useWhole(stmt.cond);
                const branches = store.getCFSwitchBranches(stmt.branches);
                for (0..GuardedList.borrowLen(branches)) |i| {
                    try stack.append(gpa, GuardedList.at(branches, i).body);
                }
                try stack.append(gpa, stmt.default_branch);
                if (stmt.continuation) |continuation| try stack.append(gpa, continuation);
            },
            .switch_initialized_payload => |stmt| {
                analysis.useWhole(stmt.cond);
                analysis.useWhole(stmt.payload);
                try stack.append(gpa, stmt.initialized_branch);
                try stack.append(gpa, stmt.uninitialized_branch);
            },
            .str_match => |stmt| {
                analysis.useWhole(stmt.source);
                const steps = store.getStrMatchSteps(stmt.steps);
                for (0..GuardedList.borrowLen(steps)) |i| {
                    switch (GuardedList.at(steps, i).capture) {
                        .discard => {},
                        .view => |view_local| analysis.useWhole(view_local),
                    }
                }
                try stack.append(gpa, stmt.on_match);
                try stack.append(gpa, stmt.on_miss);
            },
            .str_match_set => |stmt| {
                analysis.useWhole(stmt.source);
                const arms = store.getStrMatchArms(stmt.arms);
                for (0..GuardedList.borrowLen(arms)) |arm_index| {
                    const arm = GuardedList.at(arms, arm_index);
                    const steps = store.getStrMatchSteps(arm.steps);
                    for (0..GuardedList.borrowLen(steps)) |i| {
                        switch (GuardedList.at(steps, i).capture) {
                            .discard => {},
                            .view => |view_local| analysis.useWhole(view_local),
                        }
                    }
                    try stack.append(gpa, arm.on_match);
                }
                try stack.append(gpa, stmt.on_miss);
            },
            .loop_continue, .loop_break => {},
            .join => |stmt| {
                // Join parameters are excluded by the gate; the condition
                // locals are scalar presence words.
                try stack.append(gpa, stmt.body);
                try stack.append(gpa, stmt.remainder);
            },
            .jump => {},
            .ret => |stmt| try analysis.useWholeAt(stmt.value, current),
            .crash => |stmt| if (stmt.msg.localId()) |message| try analysis.useWholeAt(message, current),
        }
    }

    // Second phase: verify the surviving candidates' read shapes and spines,
    // and build the output.
    var result = Dismantles{
        .arena = std.heap.ArenaAllocator.init(gpa),
        .takes = .empty,
        .containers = .empty,
        .owned_only_takes = .empty,
        .owned_only_containers = .empty,
        .owned_only_param_benefits = &.{},
    };
    errdefer result.deinit();
    result.owned_only_param_benefits = try result.arena.allocator().alloc(arc_sig.ParamMask, store.procSpecCount());
    @memset(result.owned_only_param_benefits, 0);

    var read_kinds = std.AutoHashMapUnmanaged(LIR.CFStmtId, ReadKind).empty;
    defer read_kinds.deinit(gpa);
    var join_bodies = std.AutoHashMapUnmanaged(u32, LIR.CFStmtId).empty;
    defer join_bodies.deinit(gpa);
    var body_states = std.AutoHashMapUnmanaged(LIR.CFStmtId, FlowState).empty;
    defer body_states.deinit(gpa);
    const FlowFrame = struct { cursor: LIR.CFStmtId, state: FlowState };
    var flow_frames = std.ArrayList(FlowFrame).empty;
    defer flow_frames.deinit(gpa);
    var exit_musts = std.ArrayList(ExitRecord).empty;
    defer exit_musts.deinit(gpa);

    var it = analysis.candidates.iterator();
    candidates: while (it.next()) |entry| {
        const local: LIR.LocalId = @enumFromInt(entry.key_ptr.*);
        const candidate = entry.value_ptr;
        if (candidate.disqualified) continue;
        if (candidate.reads.items.len == 0) continue;

        // Payload-read definitions (`assign_ref`) are excluded: a container
        // that is itself a taken or claimable payload never holds its own
        // certifier unit, so its dismantle's claims would have nothing to
        // spend. Its whole release stays, itself claiming the outer field
        // when the outer container dismantles.
        const spine_start: LIR.CFStmtId = if (candidate.def_count == 1)
            switch (store.getCFStmt(candidate.def_stmt)) {
                inline .assign_literal, .assign_call, .assign_call_erased, .assign_packed_erased_fn, .assign_low_level, .assign_list, .assign_struct, .assign_tag => |stmt| stmt.next,
                .init_uninitialized,
                .assign_ref,
                .store_struct,
                .store_tag,
                .set_local,
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
                .loop_continue,
                .loop_break,
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

        // Reads keep their field indexes within a mask's reach or the
        // container cannot dismantle at all.
        for (candidate.reads.items) |read| {
            if (read.field_idx >= 64) continue :candidates;
        }

        // Only refcounted fields carry stored units worth taking; a
        // container with no refcounted take keeps its ordinary whole
        // release. A tag union's fields live behind its variants, so its
        // bits use the shared (variant, field) claim encoding and its reads
        // must address variants inside the exclusion mask's reach.
        const local_layout = layouts.getLayout(store.getLocal(local).layout_idx);
        const is_union = local_layout.tag == .tag_union;
        const encoding: ?arc_takes.UnionClaimEncoding = if (is_union)
            (arc_takes.unionClaimEncoding(layouts, local_layout) orelse continue :candidates)
        else
            null;
        var rc_mask: u64 = 0;
        if (encoding) |enc| {
            var enc_it = enc.iterate();
            while (enc_it.next()) |field| rc_mask |= @as(u64, 1) << field.bit;
        } else {
            const info = layouts.getStructInfo(local_layout);
            for (0..info.fields.len) |i| {
                const field = info.fields.get(@intCast(i));
                if (layouts.layoutContainsRefcounted(layouts.getLayout(field.layout))) {
                    rc_mask |= @as(u64, 1) << @intCast(field.index);
                }
            }
        }

        // The bit each read touches. A union read of a non-refcounted field
        // has no bit and needs no ordering; a struct read keys directly by
        // field index.
        var variant_cases = std.ArrayList(VariantCase).empty;
        defer variant_cases.deinit(gpa);
        for (candidate.reads.items) |read| {
            if (is_union) {
                if (read.variant_index == no_variant or read.variant_index >= 64) continue :candidates;
                var known = false;
                for (variant_cases.items) |case| {
                    if (case.variant_index == read.variant_index) {
                        if (case.tag_discriminant != read.tag_discriminant) continue :candidates;
                        known = true;
                        break;
                    }
                }
                if (!known) {
                    try variant_cases.append(gpa, .{
                        .variant_index = read.variant_index,
                        .tag_discriminant = read.tag_discriminant,
                    });
                }
            } else if (read.variant_index != no_variant) {
                continue :candidates;
            }
        }

        // Verify each field by a forward dataflow from the container's
        // definition over the control-flow graph: a consuming read is a take
        // only if the field cannot have been taken yet at that point, a
        // borrow of a taken field must run before every take that could
        // reach it, and every exit the flow reaches must agree on the taken
        // set so the residual release is the same on all paths. Loops poison
        // themselves: a take inside one reaches itself as possibly-taken.
        read_kinds.clearRetainingCapacity();
        for (candidate.reads.items) |read| {
            const bit: u64 = if (encoding) |enc|
                (if (enc.bitFor(read.variant_index, @intCast(read.field_idx))) |b| @as(u64, 1) << b else continue)
            else blk: {
                const b = @as(u64, 1) << @intCast(read.field_idx);
                if (rc_mask & b == 0) continue;
                break :blk b;
            };
            try read_kinds.put(gpa, read.stmt, .{
                .bit = bit,
                .variant_index = read.variant_index,
                .consuming = read.consuming,
            });
        }
        // A whole use behaves like a borrow of every field at once: no take
        // may run before it on any path, so the value it moves or observes
        // is the intact container.
        for (candidate.whole_uses.items) |stmt| {
            const slot = try read_kinds.getOrPut(gpa, stmt);
            slot.value_ptr.* = .{ .bit = ~@as(u64, 0), .variant_index = no_variant, .consuming = false };
        }

        var candidate_mask: u64 = 0;
        for (candidate.reads.items) |read| {
            if (!read.consuming) continue;
            if (encoding) |enc| {
                if (enc.bitFor(read.variant_index, @intCast(read.field_idx))) |b| {
                    candidate_mask |= @as(u64, 1) << b;
                }
            } else {
                candidate_mask |= @as(u64, 1) << @intCast(read.field_idx);
            }
        }
        candidate_mask &= rc_mask;
        if (candidate_mask == 0) continue;

        // Emitting a union dismantle needs a discriminant scratch layout;
        // one comes from any single-definition discriminant read of the
        // container. Reads whose target is rebound cannot name the
        // discriminant reliably and are skipped for switch partitioning too.
        var disc_layout: ?layout_mod.Idx = null;
        if (is_union) {
            for (candidate.disc_targets.items) |disc_target| {
                if (analysis.def_counts[@intFromEnum(disc_target)] != 1) continue;
                disc_layout = store.getLocal(disc_target).layout_idx;
                break;
            }
            if (disc_layout == null) continue :candidates;
        }

        var poison: u64 = 0;
        exit_musts.clearRetainingCapacity();
        join_bodies.clearRetainingCapacity();
        body_states.clearRetainingCapacity();
        flow_frames.clearRetainingCapacity();
        try flow_frames.append(gpa, .{ .cursor = spine_start, .state = .{ .may = 0, .must = 0, .excluded = 0 } });
        var steps: usize = 0;
        // Each statement is re-walked at most once per lattice step of its
        // reaching state; 3 bits per tracked field bound the lattice height.
        const step_limit = (store.cfStmtCount() + 1) * (3 * 64 + 1);
        flow: while (flow_frames.pop()) |frame| {
            var cursor = frame.cursor;
            var state = frame.state;
            chain: while (true) {
                steps += 1;
                if (steps > step_limit) {
                    poison = ~@as(u64, 0);
                    break :flow;
                }
                if (read_kinds.getPtr(cursor)) |kind| {
                    kind.visited = true;
                    if (kind.variant_index != no_variant and
                        (state.excluded >> @intCast(kind.variant_index)) & 1 != 0)
                    {
                        // A read of a variant the path has excluded is
                        // statically walkable but never runs here; keeping
                        // its field residual sidesteps the contradiction.
                        poison |= kind.bit;
                    } else if (kind.consuming) {
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
                switch (store.getCFStmt(cursor)) {
                    inline .init_uninitialized, .assign_ref, .assign_literal, .assign_call, .assign_call_erased, .assign_packed_erased_fn, .assign_low_level, .assign_list, .assign_struct, .assign_tag, .store_struct, .store_tag, .set_local, .debug, .expect, .comptime_branch_taken, .incref, .decref, .decref_if_initialized, .free => |stmt| cursor = stmt.next,
                    .join => |stmt| {
                        if (is_union) {
                            // Tag-union takes stay region-local: a join
                            // declared inside the region means reads or
                            // deaths can sit in its body, where the variant
                            // knowledge that justifies the residual dispatch
                            // no longer accompanies every walk of the
                            // emitted code. Such containers keep their whole
                            // release; cross-join takes are future work.
                            poison = ~@as(u64, 0);
                            break :flow;
                        }
                        try join_bodies.put(gpa, @intFromEnum(stmt.id), stmt.body);
                        cursor = stmt.remainder;
                    },
                    .switch_stmt => |stmt| {
                        // A switch on the container's own discriminant
                        // partitions the paths by variant: each arm excludes
                        // every read variant whose discriminant is some
                        // other case, and the default excludes every read
                        // variant whose discriminant is a listed case.
                        var partitions = false;
                        if (is_union) {
                            for (candidate.disc_targets.items) |disc_target| {
                                if (disc_target != stmt.cond) continue;
                                if (analysis.def_counts[@intFromEnum(disc_target)] != 1) continue;
                                partitions = true;
                                break;
                            }
                        }
                        const heads = store.getCFSwitchBranches(stmt.branches);
                        for (0..GuardedList.borrowLen(heads)) |i| {
                            const head = GuardedList.at(heads, i);
                            var branch_state = state;
                            if (partitions) {
                                for (variant_cases.items) |case| {
                                    if (case.tag_discriminant != head.value) {
                                        branch_state.excluded |= @as(u64, 1) << @intCast(case.variant_index);
                                    }
                                }
                            }
                            try flow_frames.append(gpa, .{ .cursor = head.body, .state = branch_state });
                        }
                        if (partitions) {
                            for (variant_cases.items) |case| {
                                for (0..GuardedList.borrowLen(heads)) |i| {
                                    if (GuardedList.at(heads, i).value == case.tag_discriminant) {
                                        state.excluded |= @as(u64, 1) << @intCast(case.variant_index);
                                        break;
                                    }
                                }
                            }
                        }
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
                        const body = join_bodies.get(@intFromEnum(stmt.target)) orelse {
                            poison |= state.may & ~state.must;
                            try exit_musts.append(gpa, .{ .must = state.must, .excluded = state.excluded });
                            break :chain;
                        };
                        const slot = try body_states.getOrPut(gpa, body);
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
                    .str_match => |stmt| {
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
                    // Every exit the flow reaches must agree, so the death
                    // point's residual is the same however it was reached.
                    .ret, .crash, .expect_err, .runtime_error, .comptime_exhaustiveness_failed, .loop_continue, .loop_break => {
                        poison |= state.may & ~state.must;
                        try exit_musts.append(gpa, .{ .must = state.must, .excluded = state.excluded });
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

        // One static residual serves every death point, so a field is taken
        // only if every exit the flow reached agrees its take ran: an exit a
        // taken field's take did not dominate would be under-released. An
        // exit that excluded the field's variant owes nothing: the death
        // point's variant dispatch never releases that variant's residual on
        // paths that cannot carry it. Bits only ever leave the set, so this
        // converges.
        var taken_mask: u64 = candidate_mask & ~poison;
        while (taken_mask != 0) {
            var missing: u64 = 0;
            for (exit_musts.items) |exit| {
                var owed = taken_mask;
                if (encoding) |enc| {
                    for (variant_cases.items) |case| {
                        if ((exit.excluded >> @intCast(case.variant_index)) & 1 != 0) {
                            owed &= ~enc.variantMask(case.variant_index);
                        }
                    }
                }
                missing |= owed & ~exit.must;
            }
            if (missing == 0) break;
            taken_mask &= ~missing;
        }
        if (taken_mask == 0) continue;

        // Accepted. Record the takes and the residual: every refcounted
        // field that was not taken is released at the death point. A tag
        // union's residual is per taken variant; the variants its reads
        // never addressed keep the whole release behind the death point's
        // variant dispatch.
        var residual = std.ArrayList(ResidualField).empty;
        defer residual.deinit(gpa);
        var variant_plans = std.ArrayList(VariantPlan).empty;
        defer variant_plans.deinit(gpa);
        if (encoding) |enc| {
            const info = layouts.getTagUnionInfo(local_layout);
            for (variant_cases.items) |case| {
                if (taken_mask & enc.variantMask(case.variant_index) == 0) continue;
                residual.clearRetainingCapacity();
                var enc_it = enc.iterate();
                while (enc_it.next()) |field| {
                    if (field.variant != case.variant_index) continue;
                    if (taken_mask & (@as(u64, 1) << field.bit) != 0) continue;
                    try residual.append(gpa, .{ .field_idx = field.field_idx, .layout_idx = field.field_layout });
                }
                const payload_idx = info.variants.get(case.variant_index).payload_layout;
                try variant_plans.append(gpa, .{
                    .variant_index = case.variant_index,
                    .tag_discriminant = case.tag_discriminant,
                    .payload_is_struct = layouts.getLayout(payload_idx).tag == .struct_,
                    .payload_layout = payload_idx,
                    .residual = try result.arena.allocator().dupe(ResidualField, residual.items),
                });
            }
            residual.clearRetainingCapacity();
        } else {
            const info = layouts.getStructInfo(local_layout);
            for (0..info.fields.len) |i| {
                const field = info.fields.get(@intCast(i));
                const bit = @as(u64, 1) << @intCast(field.index);
                if (taken_mask & bit != 0) continue;
                if (!layouts.layoutContainsRefcounted(layouts.getLayout(field.layout))) continue;
                try residual.append(gpa, .{ .field_idx = field.index, .layout_idx = field.layout });
            }
        }

        // A parameter solved borrowed dismantles only in emissions whose
        // demand vector overrides it to owned; everything else applies to
        // every emission of its proc.
        const owned_only = solution.isBorrowed(local);
        const stored_residual = try result.arena.allocator().dupe(ResidualField, residual.items);
        for (candidate.reads.items) |read| {
            const bit: u64 = if (encoding) |enc|
                (if (enc.bitFor(read.variant_index, @intCast(read.field_idx))) |b| @as(u64, 1) << b else continue)
            else
                @as(u64, 1) << @intCast(read.field_idx);
            if (taken_mask & bit == 0) continue;
            // Borrowing reads of a taken field stay plain borrows; only the
            // consuming reads take the stored unit.
            if (!read.consuming) continue;
            if (owned_only) {
                try result.owned_only_takes.put(gpa, read.stmt, local);
            } else {
                try result.takes.put(gpa, read.stmt, {});
            }
        }
        const stored_container = Container{
            .residual = stored_residual,
            .variant_plans = try result.arena.allocator().dupe(VariantPlan, variant_plans.items),
            .disc_layout = disc_layout,
        };
        if (owned_only) {
            try result.owned_only_containers.put(gpa, local, stored_container);
        } else {
            try result.containers.put(gpa, local, stored_container);
        }
    }

    // Variant admission consumes the exact owned-only benefit without
    // rescanning bodies or reconstructing parameter identity from statements.
    for (0..store.procSpecCount()) |proc_index| {
        const proc_id: LIR.LirProcSpecId = @enumFromInt(@as(u32, @intCast(proc_index)));
        const params = store.getLocalSpan(store.getProcSpec(proc_id).args);
        for (0..GuardedList.borrowLen(params)) |position| {
            const bit = arc_sig.paramBit(position) orelse break;
            const param = GuardedList.at(params, position);
            if (result.owned_only_containers.contains(param)) {
                result.owned_only_param_benefits[proc_index] |= bit;
            }
        }
    }

    return result;
}
