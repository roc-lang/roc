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

const LIR = core.LIR;
const LirStore = core.LirStore;
const GuardedList = collections.GuardedList;
const Allocator = std.mem.Allocator;

/// Allocation errors returned while solving field takes.
pub const Error = std.mem.Allocator.Error;

const no_index: u32 = std.math.maxInt(u32);

/// Compact description of an aggregate projection whose result may carry a
/// stored ownership unit out of its source. The high two bits identify the
/// projection form; the remaining bits hold the semantic field or tag indices.
pub const no_projection: u64 = std.math.maxInt(u64);
const projection_kind_shift = 62;
const projection_data_mask = (@as(u64, 1) << projection_kind_shift) - 1;
const ProjectionKind = enum(u2) {
    field,
    tag_payload,
    tag_payload_struct,
    reserved,
};

/// Encodes the ownership-relevant shape of a reference projection, or null
/// when the operation does not select aggregate storage.
pub fn encodeProjection(op: LIR.RefOp) ?u64 {
    return switch (op) {
        .field => |field| @as(u64, field.field_idx),
        .tag_payload => |payload| (@as(u64, @intFromEnum(ProjectionKind.tag_payload)) << projection_kind_shift) |
            (@as(u64, payload.variant_index) << 16) |
            @as(u64, payload.payload_idx),
        .tag_payload_struct => |payload| (@as(u64, @intFromEnum(ProjectionKind.tag_payload_struct)) << projection_kind_shift) |
            @as(u64, payload.variant_index),
        .local, .discriminant, .list_reinterpret, .nominal => null,
    };
}

fn projectionKind(projection: u64) ProjectionKind {
    return @enumFromInt(@as(u2, @intCast(projection >> projection_kind_shift)));
}

fn projectionField(projection: u64) u16 {
    return @intCast(projection & projection_data_mask);
}

fn projectionVariant(projection: u64) u16 {
    return switch (projectionKind(projection)) {
        .tag_payload => @intCast((projection >> 16) & 0xffff),
        .tag_payload_struct => @intCast(projection & 0xffff),
        .field, .reserved => 0,
    };
}

fn projectionPayload(projection: u64) u16 {
    return @intCast(projection & 0xffff);
}

fn structFieldBySemanticIndex(layouts: *const layout_mod.Store, struct_layout: layout_mod.Layout, field_idx: u16) ?layout_mod.StructField {
    const info = layouts.getStructInfo(struct_layout);
    for (0..info.fields.len) |index| {
        const field = info.fields.get(@intCast(index));
        if (field.index == field_idx) return field;
    }
    return null;
}

fn structHasOneRcField(layouts: *const layout_mod.Store, struct_layout: layout_mod.Layout, field_idx: u16) bool {
    const info = layouts.getStructInfo(struct_layout);
    var only_field: ?u16 = null;
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
    projection: u64,
) bool {
    if (projection == no_projection) return false;
    const source_layout_idx = store.getLocal(source).layout_idx;
    const target_layout_idx = store.getLocal(target).layout_idx;
    const source_layout = layouts.getLayout(source_layout_idx);
    switch (projectionKind(projection)) {
        .field => {
            if (source_layout.tag != .struct_) return false;
            const field_idx = projectionField(projection);
            const field = structFieldBySemanticIndex(layouts, source_layout, field_idx) orelse return false;
            return field.layout == target_layout_idx and
                layouts.layoutContainsRefcounted(layouts.getLayout(field.layout)) and
                structHasOneRcField(layouts, source_layout, field_idx);
        },
        .tag_payload, .tag_payload_struct => {
            if (source_layout.tag != .tag_union) return false;
            const info = layouts.getTagUnionInfo(source_layout);
            const variant_index = projectionVariant(projection);
            if (variant_index >= info.variants.len) return false;
            const payload_layout_idx = info.variants.get(variant_index).payload_layout;
            const payload_layout = layouts.getLayout(payload_layout_idx);
            if (projectionKind(projection) == .tag_payload_struct) {
                return payload_layout_idx == target_layout_idx and
                    layouts.layoutContainsRefcounted(payload_layout);
            }
            const payload_idx = projectionPayload(projection);
            if (payload_layout.tag == .struct_) {
                const field = structFieldBySemanticIndex(layouts, payload_layout, payload_idx) orelse return false;
                return field.layout == target_layout_idx and
                    layouts.layoutContainsRefcounted(layouts.getLayout(field.layout)) and
                    structHasOneRcField(layouts, payload_layout, payload_idx);
            }
            return payload_idx == 0 and payload_layout_idx == target_layout_idx and
                layouts.layoutContainsRefcounted(payload_layout);
        },
        .reserved => return false,
    }
}

/// One refcounted field a dismantled container still owns at its death
/// point: emission reads it into a temporary and releases the temporary.
pub const ResidualField = struct {
    /// Original field index, as `assign_ref .field` addresses it.
    field_idx: u32,
    layout_idx: layout_mod.Idx,
};

/// Residual ownership plan for one container local selected for dismantling.
pub const Container = struct {
    residual: []const ResidualField,
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
    /// Borrowed projection locals that denote the complete refcounted payload
    /// of another local. Moving one of these locals moves the root's unit
    /// directly when that unit is present; otherwise the ordinary retain is
    /// preserved.
    projection_units: std.AutoHashMapUnmanaged(LIR.LocalId, LIR.LocalId),
    /// Complete projection reads whose owned target can receive the root's
    /// unit directly. The solve-time transfer still checks path liveness and
    /// keeps the ordinary retain when the root must survive.
    complete_takes: std.AutoHashMapUnmanaged(LIR.CFStmtId, LIR.LocalId),

    pub fn deinit(self: *Dismantles) void {
        const gpa = self.arena.child_allocator;
        self.takes.deinit(gpa);
        self.containers.deinit(gpa);
        self.owned_only_takes.deinit(gpa);
        self.owned_only_containers.deinit(gpa);
        self.projection_units.deinit(gpa);
        self.complete_takes.deinit(gpa);
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

    pub fn projectionUnitOf(self: *const Dismantles, local: LIR.LocalId) ?LIR.LocalId {
        return self.projection_units.get(local);
    }

    pub fn completeTakeRoot(self: *const Dismantles, stmt: LIR.CFStmtId) ?LIR.LocalId {
        return self.complete_takes.get(stmt);
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
};

const Read = struct {
    stmt: LIR.CFStmtId,
    target: LIR.LocalId,
    field_idx: u32,
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
};

const Analysis = struct {
    gpa: Allocator,
    store: *const LirStore,
    layouts: *const layout_mod.Store,
    solution: *const arc_solve.Solution,
    state: []State,
    /// Root container local per transparent alias, `no_index` otherwise.
    alias_root: []u32,
    candidates: std.AutoHashMapUnmanaged(u32, Candidate),
    /// Proc parameters. A parameter solved borrowed may still qualify as an
    /// owned-only candidate: mode-specialized variants re-emit it owned.
    is_param: []const bool,

    fn deinit(self: *Analysis) void {
        var it = self.candidates.valueIterator();
        while (it.next()) |candidate| {
            candidate.reads.deinit(self.gpa);
            candidate.whole_uses.deinit(self.gpa);
        }
        self.candidates.deinit(self.gpa);
        self.gpa.free(self.alias_root);
        self.gpa.free(self.state);
    }

    /// Whether the local's layout and binding shape could ever benefit from
    /// dismantling. Cheap, no allocation; the full per-candidate work only
    /// happens for locals that pass.
    fn passesGate(self: *Analysis, local: LIR.LocalId) bool {
        const local_layout = self.layouts.getLayout(self.store.getLocal(local).layout_idx);
        if (local_layout.tag != .struct_) return false;
        if (self.solution.isBorrowed(local) and !self.is_param[@intFromEnum(local)]) return false;
        if (self.solution.isJoinParam(local)) return false;
        if (self.solution.maybeUninitializedCondition(local) != null) return false;

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
            .ineligible, .transparent_alias => return null,
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
        if (self.state[index] == .transparent_alias) {
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
        if (self.state[index] == .transparent_alias) {
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

    fn noteFieldRead(self: *Analysis, stmt: LIR.CFStmtId, source: LIR.LocalId, field_idx: u32, target: LIR.LocalId) Error!void {
        const root = self.resolveRoot(source);
        const candidate = (try self.entryOf(root)) orelse return;
        try candidate.reads.append(self.gpa, .{
            .stmt = stmt,
            .target = target,
            .field_idx = field_idx,
            .consuming = !self.solution.isBorrowed(target),
        });
    }

    fn noteAliasDef(self: *Analysis, target: LIR.LocalId, source: LIR.LocalId) Error!void {
        const target_index = @intFromEnum(target);
        if (self.state[target_index] == .transparent_alias) {
            // Redefinition of an existing alias: neither its old nor its new
            // root can attribute reads through it.
            self.disqualify(target);
            self.disqualify(source);
            self.state[target_index] = .ineligible;
            return;
        }
        const root = self.resolveRoot(source);
        const transparent = self.solution.isBorrowed(target) and
            self.solution.leaderOf(target) == root and
            ((try self.entryOf(root)) != null);
        if (transparent) {
            // The alias target itself can never be a container.
            if (self.state[target_index] == .candidate) {
                if (self.candidates.getPtr(target_index)) |candidate| candidate.disqualified = true;
            }
            self.state[target_index] = .transparent_alias;
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
        .candidates = .empty,
        .is_param = is_param,
    };
    defer analysis.deinit();
    @memset(analysis.state, .unknown);
    @memset(analysis.alias_root, no_index);

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
                        analysis.useWhole(op.source);
                        try analysis.noteDef(stmt.target, current);
                    },
                    .tag_payload => |op| {
                        analysis.useWhole(op.source);
                        try analysis.noteDef(stmt.target, current);
                    },
                    .tag_payload_struct => |op| {
                        analysis.useWhole(op.source);
                        try analysis.noteDef(stmt.target, current);
                    },
                    .list_reinterpret => |op| {
                        analysis.useWhole(op.backing_ref);
                        analysis.disqualify(stmt.target);
                    },
                    .nominal => |op| {
                        analysis.useWhole(op.backing_ref);
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
            .assign_boxy_desc_ref => |stmt| {
                if (stmt.desc.localOrNull()) |local| analysis.useWhole(local);
                if (stmt.tag_residual_for) |desc| if (desc.localOrNull()) |local| analysis.useWhole(local);
                const captures = store.getLocalSpan(stmt.captures);
                for (0..GuardedList.borrowLen(captures)) |i| analysis.useWhole(GuardedList.at(captures, i));
                try analysis.noteDef(stmt.target, current);
                analysis.disqualify(stmt.target);
                try stack.append(gpa, stmt.next);
            },
            .assign_boxy_dict_ref => |stmt| {
                if (stmt.dict.localOrNull()) |local| analysis.useWhole(local);
                try analysis.noteDef(stmt.target, current);
                analysis.disqualify(stmt.target);
                try stack.append(gpa, stmt.next);
            },
            .assign_boxy_box => |stmt| {
                analysis.useWhole(stmt.payload);
                try analysis.noteDef(stmt.target, current);
                analysis.disqualify(stmt.target);
                try stack.append(gpa, stmt.next);
            },
            .assign_boxy_reuse_box => |stmt| {
                analysis.useWhole(stmt.source);
                try analysis.noteDef(stmt.target, current);
                analysis.disqualify(stmt.target);
                try stack.append(gpa, stmt.next);
            },
            .assign_boxy_unbox => |stmt| {
                analysis.useWhole(stmt.source);
                try analysis.noteDef(stmt.target, current);
                analysis.disqualify(stmt.target);
                try stack.append(gpa, stmt.next);
            },
            .assign_boxy_adapt => |stmt| {
                analysis.useWhole(stmt.source);
                try analysis.noteDef(stmt.target, current);
                analysis.disqualify(stmt.target);
                try stack.append(gpa, stmt.next);
            },
            .assign_boxy_inspect => |stmt| {
                analysis.useWhole(stmt.source);
                try analysis.noteDef(stmt.target, current);
                analysis.disqualify(stmt.target);
                try stack.append(gpa, stmt.next);
            },
            .assign_boxy_eq => |stmt| {
                analysis.useWhole(stmt.lhs);
                analysis.useWhole(stmt.rhs);
                try analysis.noteDef(stmt.target, current);
                analysis.disqualify(stmt.target);
                try stack.append(gpa, stmt.next);
            },
            .assign_boxy_tag => |stmt| {
                if (stmt.payload) |payload| analysis.useWhole(payload);
                try analysis.noteDef(stmt.target, current);
                analysis.disqualify(stmt.target);
                try stack.append(gpa, stmt.next);
            },
            .assign_boxy_tag_payload => |stmt| {
                analysis.useWhole(stmt.source);
                try analysis.noteDef(stmt.target, current);
                analysis.disqualify(stmt.target);
                if (stmt.target_desc) |target_desc| {
                    try analysis.noteDef(target_desc, current);
                    analysis.disqualify(target_desc);
                }
                try stack.append(gpa, stmt.next);
            },
            .assign_call_dict => |stmt| {
                if (stmt.dict.localOrNull()) |local| analysis.useWhole(local);
                const args = store.getLocalSpan(stmt.args);
                for (0..GuardedList.borrowLen(args)) |i| analysis.useWhole(GuardedList.at(args, i));
                const arg_descs = store.getLocalSpan(stmt.arg_descs);
                for (0..GuardedList.borrowLen(arg_descs)) |i| analysis.useWhole(GuardedList.at(arg_descs, i));
                const hidden_args = store.getLocalSpan(stmt.hidden_args);
                for (0..GuardedList.borrowLen(hidden_args)) |i| analysis.useWhole(GuardedList.at(hidden_args, i));
                try analysis.noteDef(stmt.target, current);
                analysis.disqualify(stmt.target);
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
            .boxy_tag_match => |stmt| {
                analysis.useWhole(stmt.source);
                try stack.append(gpa, stmt.on_match);
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
        .projection_units = .empty,
        .complete_takes = .empty,
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
    var exit_musts = std.ArrayList(u64).empty;
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
                .assign_boxy_desc_ref,
                .assign_boxy_dict_ref,
                .assign_boxy_box,
                .assign_boxy_reuse_box,
                .assign_boxy_unbox,
                .assign_boxy_adapt,
                .assign_boxy_inspect,
                .assign_boxy_eq,
                .assign_boxy_tag,
                .assign_boxy_tag_payload,
                .boxy_tag_match,
                .assign_call_dict,
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
        // release.
        const local_layout = layouts.getLayout(store.getLocal(local).layout_idx);
        const info = layouts.getStructInfo(local_layout);
        var rc_mask: u64 = 0;
        for (0..info.fields.len) |i| {
            const field = info.fields.get(@intCast(i));
            if (layouts.layoutContainsRefcounted(layouts.getLayout(field.layout))) {
                rc_mask |= @as(u64, 1) << @intCast(field.index);
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
            const bit = @as(u64, 1) << @intCast(read.field_idx);
            if (rc_mask & bit == 0) continue;
            try read_kinds.put(gpa, read.stmt, .{
                .bit = bit,
                .consuming = read.consuming,
            });
        }
        // A whole use behaves like a borrow of every field at once: no take
        // may run before it on any path, so the value it moves or observes
        // is the intact container.
        for (candidate.whole_uses.items) |stmt| {
            const slot = try read_kinds.getOrPut(gpa, stmt);
            if (slot.found_existing) {
                slot.value_ptr.bit = ~@as(u64, 0);
                slot.value_ptr.consuming = false;
            } else {
                slot.value_ptr.* = .{ .bit = ~@as(u64, 0), .consuming = false };
            }
        }

        var candidate_mask: u64 = 0;
        for (candidate.reads.items) |read| {
            if (read.consuming) candidate_mask |= @as(u64, 1) << @intCast(read.field_idx);
        }
        candidate_mask &= rc_mask;
        if (candidate_mask == 0) continue;

        var poison: u64 = 0;
        exit_musts.clearRetainingCapacity();
        join_bodies.clearRetainingCapacity();
        body_states.clearRetainingCapacity();
        flow_frames.clearRetainingCapacity();
        try flow_frames.append(gpa, .{ .cursor = spine_start, .state = .{ .may = 0, .must = 0 } });
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
                switch (store.getCFStmt(cursor)) {
                    inline .init_uninitialized, .assign_ref, .assign_literal, .assign_call, .assign_call_erased, .assign_packed_erased_fn, .assign_boxy_desc_ref, .assign_boxy_dict_ref, .assign_boxy_box, .assign_boxy_reuse_box, .assign_boxy_unbox, .assign_boxy_adapt, .assign_boxy_inspect, .assign_boxy_eq, .assign_boxy_tag, .assign_boxy_tag_payload, .assign_call_dict, .assign_low_level, .assign_list, .assign_struct, .assign_tag, .store_struct, .store_tag, .set_local, .debug, .expect, .comptime_branch_taken, .incref, .decref, .decref_if_initialized, .free => |stmt| cursor = stmt.next,
                    .join => |stmt| {
                        try join_bodies.put(gpa, @intFromEnum(stmt.id), stmt.body);
                        cursor = stmt.remainder;
                    },
                    .switch_stmt => |stmt| {
                        const heads = store.getCFSwitchBranches(stmt.branches);
                        for (0..GuardedList.borrowLen(heads)) |i| {
                            try flow_frames.append(gpa, .{ .cursor = GuardedList.at(heads, i).body, .state = state });
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
                            try exit_musts.append(gpa, state.must);
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
                    .boxy_tag_match => |stmt| {
                        try flow_frames.append(gpa, .{ .cursor = stmt.on_match, .state = state });
                        cursor = stmt.on_miss;
                    },
                    // Every exit the flow reaches must agree, so the death
                    // point's residual is the same however it was reached.
                    .ret, .crash, .expect_err, .runtime_error, .comptime_exhaustiveness_failed, .loop_continue, .loop_break => {
                        poison |= state.may & ~state.must;
                        try exit_musts.append(gpa, state.must);
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
        // taken field's take did not dominate would be under-released. Bits
        // only ever leave the set, so this converges.
        var taken_mask: u64 = candidate_mask & ~poison;
        while (taken_mask != 0) {
            var missing: u64 = 0;
            for (exit_musts.items) |must| missing |= taken_mask & ~must;
            if (missing == 0) break;
            taken_mask &= ~missing;
        }
        if (taken_mask == 0) continue;

        // Accepted. Record the takes and the residual: every refcounted
        // field that was not taken is released at the death point.
        var residual = std.ArrayList(ResidualField).empty;
        defer residual.deinit(gpa);
        for (0..info.fields.len) |i| {
            const field = info.fields.get(@intCast(i));
            const bit = @as(u64, 1) << @intCast(field.index);
            if (taken_mask & bit != 0) continue;
            if (!layouts.layoutContainsRefcounted(layouts.getLayout(field.layout))) continue;
            try residual.append(gpa, .{ .field_idx = field.index, .layout_idx = field.layout });
        }

        // A parameter solved borrowed dismantles only in emissions whose
        // demand vector overrides it to owned; everything else applies to
        // every emission of its proc.
        const owned_only = solution.isBorrowed(local);
        const stored_residual = try result.arena.allocator().dupe(ResidualField, residual.items);
        for (candidate.reads.items) |read| {
            const bit = @as(u64, 1) << @intCast(read.field_idx);
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
        if (owned_only) {
            try result.owned_only_containers.put(gpa, local, .{ .residual = stored_residual });
        } else {
            try result.containers.put(gpa, local, .{ .residual = stored_residual });
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

    // A complete aggregate projection denotes the same ownership place as its
    // root: it can transfer that root's unit without manufacturing a second
    // one. Discover those places to a fixpoint over complete projections and
    // transparent aliases. The graph is sparse, so its memory is proportional
    // to relevant reads rather than the total local count.
    const ambiguous_index = no_index - 1;
    const ParamInfo = struct { proc: u32, position: u16 };
    var param_info = std.AutoHashMapUnmanaged(u32, ParamInfo).empty;
    defer param_info.deinit(gpa);

    for (0..store.procSpecCount()) |proc_index| {
        const proc_id: LIR.LirProcSpecId = @enumFromInt(@as(u32, @intCast(proc_index)));
        const params = store.getLocalSpan(store.getProcSpec(proc_id).args);
        for (0..GuardedList.borrowLen(params)) |position| {
            const param = GuardedList.at(params, position);
            const param_index = @intFromEnum(param);
            if (position >= arc_sig.tracked_param_count) continue;
            const param_slot = try param_info.getOrPut(gpa, param_index);
            if (param_slot.found_existing) {
                param_slot.value_ptr.* = .{ .proc = ambiguous_index, .position = 0 };
                continue;
            }
            param_slot.value_ptr.* = .{ .proc = @intCast(proc_index), .position = @intCast(position) };
        }
    }

    const PlaceOrigin = struct {
        root: u32,
        projected: bool,
    };
    const PlaceEdgeKind = union(enum) {
        alias,
        projection: u32,
    };
    const PlaceEdge = struct {
        source: u32,
        target: u32,
        kind: PlaceEdgeKind,
        next: u32,
    };
    var place_edges = std.ArrayList(PlaceEdge).empty;
    defer place_edges.deinit(gpa);
    var place_heads = std.AutoHashMapUnmanaged(u32, u32).empty;
    defer place_heads.deinit(gpa);
    for (0..store.cfStmtCount()) |stmt_index| {
        if (!visited.isSet(stmt_index)) continue;
        const stmt = store.getCFStmt(@enumFromInt(@as(u32, @intCast(stmt_index))));
        if (stmt != .assign_ref) continue;
        const assign = stmt.assign_ref;
        const source: LIR.LocalId, const kind: PlaceEdgeKind = switch (assign.op) {
            .local => |local| blk: {
                if (!solution.isBorrowed(assign.target)) continue;
                if (solution.unitLocalOf(assign.target) != solution.unitLocalOf(local)) continue;
                break :blk .{ local, .alias };
            },
            .field, .tag_payload, .tag_payload_struct => blk: {
                const projection = encodeProjection(assign.op).?;
                const projection_source = switch (assign.op) {
                    .field => |op| op.source,
                    .tag_payload => |op| op.source,
                    .tag_payload_struct => |op| op.source,
                    .local,
                    .discriminant,
                    .list_reinterpret,
                    .nominal,
                    => unreachable,
                };
                if (!projectionOwnsAllRc(store, layouts, projection_source, assign.target, projection)) continue;
                break :blk .{ projection_source, .{ .projection = @intCast(stmt_index) } };
            },
            .discriminant, .list_reinterpret, .nominal => continue,
        };
        const source_index = @intFromEnum(source);
        const previous_head = place_heads.get(source_index) orelse no_index;
        try place_edges.append(gpa, .{
            .source = source_index,
            .target = @intFromEnum(assign.target),
            .kind = kind,
            .next = previous_head,
        });
        try place_heads.put(gpa, source_index, @intCast(place_edges.items.len - 1));
    }

    var places = std.AutoHashMapUnmanaged(u32, PlaceOrigin).empty;
    defer places.deinit(gpa);
    var place_work = std.ArrayList(u32).empty;
    defer place_work.deinit(gpa);

    // Seed only locals that can actually carry a unit: owned bindings, proc
    // or join parameters (which can be owned in an emission), and borrowed
    // pure aliases whose `unitLocalOf` resolves to one of those roots. A
    // borrowed payload local is therefore reached only through its explicit
    // complete-projection edge.
    var source_it = place_heads.keyIterator();
    while (source_it.next()) |source_ptr| {
        const source: LIR.LocalId = @enumFromInt(source_ptr.*);
        const root = solution.unitLocalOf(source);
        const root_index = @intFromEnum(root);
        const root_can_own = !solution.isBorrowed(root) or
            is_param[root_index] or
            solution.isJoinParam(root);
        if (!root_can_own) continue;
        try places.put(gpa, source_ptr.*, .{ .root = root_index, .projected = false });
        try place_work.append(gpa, source_ptr.*);
    }

    while (place_work.pop()) |source_index| {
        const source_origin = places.get(source_index) orelse continue;
        if (source_origin.root == ambiguous_index) continue;
        var edge_index = place_heads.get(source_index) orelse no_index;
        while (edge_index != no_index) {
            const edge = place_edges.items[edge_index];
            const projected = source_origin.projected or edge.kind == .projection;
            const target: LIR.LocalId = @enumFromInt(edge.target);
            // A join parameter is a cell with one definition per incoming
            // edge, not an SSA value. Its edge-specific transfer is handled
            // by join solving; one global place origin would incorrectly
            // apply one edge's source to every arrival.
            if (solution.isJoinParam(target)) {
                edge_index = edge.next;
                continue;
            }
            if (edge.kind == .projection and !solution.isBorrowed(target)) {
                edge_index = edge.next;
                continue;
            }
            const slot = try places.getOrPut(gpa, edge.target);
            if (!slot.found_existing) {
                slot.value_ptr.* = .{ .root = source_origin.root, .projected = projected };
                try place_work.append(gpa, edge.target);
            } else if (slot.value_ptr.root != ambiguous_index and slot.value_ptr.root != source_origin.root) {
                slot.value_ptr.* = .{ .root = ambiguous_index, .projected = false };
                try place_work.append(gpa, edge.target);
            } else if (slot.value_ptr.root == source_origin.root and projected and !slot.value_ptr.projected) {
                slot.value_ptr.projected = true;
                try place_work.append(gpa, edge.target);
            }
            edge_index = edge.next;
        }
    }

    // Borrowed complete projections keep the root as their unit key. Owned
    // projection targets instead receive a solve-time move opportunity at the
    // read itself; if liveness says the root survives, ARC retains exactly as
    // before and the target owns the retained unit independently.
    var places_it = places.iterator();
    while (places_it.next()) |entry| {
        const origin = entry.value_ptr.*;
        if (origin.root == ambiguous_index or !origin.projected) continue;
        const local: LIR.LocalId = @enumFromInt(entry.key_ptr.*);
        if (!solution.isBorrowed(local) or solution.isJoinParam(local)) continue;
        try result.projection_units.put(gpa, local, @enumFromInt(origin.root));
    }
    for (place_edges.items) |edge| {
        if (edge.kind != .projection) continue;
        const target: LIR.LocalId = @enumFromInt(edge.target);
        // Unlike a borrowed place origin, this move is attached to one exact
        // incoming read. An owned join cell can therefore receive the unit on
        // this edge without conflating its other definitions.
        if (solution.isBorrowed(target)) continue;
        const origin = places.get(edge.source) orelse continue;
        if (origin.root == ambiguous_index) continue;
        try result.complete_takes.put(gpa, @enumFromInt(edge.kind.projection), @enumFromInt(origin.root));
    }

    // A leaf procedure's ordinary field dismantle makes its parameter
    // beneficial. Direct calls propagate that benefit through complete places
    // and plain forwarding to a fixpoint, admitting owned parameter variants
    // all the way up a derived-encoder wrapper chain.
    const BenefitEdge = struct {
        source_key: u32,
        next: u32,
    };
    var benefit_edges = std.ArrayList(BenefitEdge).empty;
    defer benefit_edges.deinit(gpa);
    var benefit_heads = std.AutoHashMapUnmanaged(u32, u32).empty;
    defer benefit_heads.deinit(gpa);
    for (0..store.cfStmtCount()) |stmt_index| {
        if (!visited.isSet(stmt_index)) continue;
        const stmt = store.getCFStmt(@enumFromInt(@as(u32, @intCast(stmt_index))));
        if (stmt != .assign_call) continue;
        const call = stmt.assign_call;
        const args = store.getLocalSpan(call.args);
        for (0..GuardedList.borrowLen(args)) |position| {
            if (position >= arc_sig.tracked_param_count) continue;
            const arg = GuardedList.at(args, position);
            const arg_index = @intFromEnum(arg);
            const place_origin = places.get(arg_index);
            const root_index: u32 = if (place_origin) |origin| origin.root else @intFromEnum(solution.unitLocalOf(arg));
            if (root_index == no_index or root_index == ambiguous_index) continue;
            const source_info = param_info.get(root_index) orelse continue;
            if (source_info.proc == ambiguous_index) continue;
            const source_proc: LIR.LirProcSpecId = @enumFromInt(source_info.proc);
            if (solution.isPinnedProc(source_proc)) continue;
            const target_key: u32 = @intCast(@intFromEnum(call.proc) * arc_sig.tracked_param_count + position);
            const previous_head = benefit_heads.get(target_key) orelse no_index;
            try benefit_edges.append(gpa, .{
                .source_key = @intCast(source_info.proc * arc_sig.tracked_param_count + source_info.position),
                .next = previous_head,
            });
            try benefit_heads.put(gpa, target_key, @intCast(benefit_edges.items.len - 1));
        }
    }

    // Propagate each newly beneficial parameter exactly once. A wrapper chain
    // is therefore linear in its calls rather than one full call-site scan per
    // wrapper depth.
    var benefit_work = std.ArrayList(u32).empty;
    defer benefit_work.deinit(gpa);
    for (result.owned_only_param_benefits, 0..) |mask, proc_index| {
        for (0..arc_sig.tracked_param_count) |position| {
            const bit = arc_sig.paramBit(position).?;
            if ((mask & bit) == 0) continue;
            try benefit_work.append(gpa, @intCast(proc_index * arc_sig.tracked_param_count + position));
        }
    }
    while (benefit_work.pop()) |target_key| {
        var edge_index = benefit_heads.get(target_key) orelse no_index;
        while (edge_index != no_index) {
            const edge = benefit_edges.items[edge_index];
            const source_proc_index = edge.source_key / arc_sig.tracked_param_count;
            const source_position = edge.source_key % arc_sig.tracked_param_count;
            const source_bit = arc_sig.paramBit(source_position).?;
            if ((result.owned_only_param_benefits[source_proc_index] & source_bit) == 0) {
                result.owned_only_param_benefits[source_proc_index] |= source_bit;
                try benefit_work.append(gpa, edge.source_key);
            }
            edge_index = edge.next;
        }
    }

    return result;
}
