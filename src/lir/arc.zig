//! ARC insertion for LIR: borrow inference plus RC statement emission.
//!
//! This pass is the only non-builtin stage that may synthesize explicit
//! `incref`, `decref`, and `free` statements. It first solves binding modes
//! and proc ownership signatures (`arc_solve`), then fills stable plans for
//! each structured ownership context and materializes their explicit RC
//! decisions: borrowed bindings emit nothing, owned final occurrences move,
//! and lifetime-ending releases land right after the last use of a binding's
//! borrow group. Optimized builds also emit
//! mode-specialized proc variants for call sites that can move arguments into
//! positions the solved signature borrows, or that prove a dying argument
//! statically unique so the variant elides the runtime uniqueness checks
//! that parameter reaches. Debug builds re-check the output
//! with the borrow certifier (`arc_certify`). Backends consume explicit RC
//! statements without doing reference-counting analysis.

const std = @import("std");
const builtin = @import("builtin");
const collections = @import("collections");
const Allocator = std.mem.Allocator;
const core = @import("lir_core");
const layout_mod = @import("layout");
const arc_sig = @import("arc_sig.zig");
const arc_solve = @import("arc_solve.zig");
const arc_certify = @import("arc_certify.zig");
const arc_dismantle = @import("arc_dismantle.zig");
const debug_print = @import("debug_print.zig");

const LIR = core.LIR;
const LirStore = core.LirStore;
const GuardedList = collections.GuardedList;

pub const ResourceError = std.mem.Allocator.Error;

/// Debug-only count of join-summary solver work items, for scaling tests
/// and profiling. Not updated in release builds.
pub var solver_iterations: u64 = 0;

/// Options for ARC insertion.
pub const InsertOptions = struct {
    /// Root procs whose ownership signature is pinned all-owned by ABI.
    roots: []const LIR.LirProcSpecId = &.{},
    /// Emit mode-specialized proc variants for call sites that demand more
    /// ownership than a callee's solved signature provides. Optimized builds
    /// enable this; dev builds and compile-time evaluation use the solved
    /// single variant per proc.
    specialize: bool = false,
    /// Select consuming Box.unbox when its lender is dead. Compiled backends
    /// enable this; the value-model interpreter keeps an explicit borrow.
    consume_dead_boxes: bool = true,
};

const no_proc_local_index = std.math.maxInt(u32);
const no_stmt_node_index = std.math.maxInt(u32);
const no_arc_bit = std.math.maxInt(u32);
const arc_bit_needed = no_arc_bit - 1;

/// Exact per-procedure ARC bit domain. `frame_locals` is the complete,
/// producer-authored local inventory maintained by direct LIR lowering and
/// every pre-ARC transform. ARC consumes it directly: ownership sets contain
/// only refcounted resources and their solved representatives, and liveness
/// rows add only the proc's solved group and borrowed-result bits.
const ProcArcDomain = struct {
    global_local_index: []u32,
    frame_locals: []const LIR.LocalId,
    resource_bit_index: []const u32,
    resource_locals: []const LIR.LocalId,
    /// Full committed field-place domain for dismantlable aggregate
    /// resources; zero for ordinary whole-value resources.
    resource_full_masks: []u64,
    refcounted_locals: []const LIR.LocalId,
    group_bit_index: []const u32,
    group_leaders: []const LIR.LocalId,
    value_use_bit_index: []const u32,
    value_use_locals: []const LIR.LocalId,

    fn init(
        allocator: Allocator,
        store: *const LirStore,
        solution: *const arc_solve.Solution,
        local_contains_refcounted: []const bool,
        global_local_index: []u32,
        frame_span: LIR.LocalSpan,
    ) ResourceError!ProcArcDomain {
        const frame = store.getLocalSpan(frame_span);
        const frame_len = GuardedList.borrowLen(frame);
        if (frame_len >= arc_bit_needed) arcInvariant("ARC proc-local domain exceeds its dense index representation");

        const frame_locals = try allocator.alloc(LIR.LocalId, frame_len);
        const resource_bit_index = try allocator.alloc(u32, frame_len);
        @memset(resource_bit_index, no_arc_bit);
        const resource_locals_buffer = try allocator.alloc(LIR.LocalId, frame_len);
        const refcounted_locals_buffer = try allocator.alloc(LIR.LocalId, frame_len);
        const group_bit_index = try allocator.alloc(u32, frame_len);
        @memset(group_bit_index, no_arc_bit);
        const group_leaders_buffer = try allocator.alloc(LIR.LocalId, frame_len);
        const value_use_bit_index = try allocator.alloc(u32, frame_len);
        @memset(value_use_bit_index, no_arc_bit);
        const value_use_locals_buffer = try allocator.alloc(LIR.LocalId, frame_len);

        var mapped_len: usize = 0;
        errdefer for (frame_locals[0..mapped_len]) |local| {
            global_local_index[@intFromEnum(local)] = no_proc_local_index;
        };
        for (0..frame_len) |frame_index| {
            const local = GuardedList.at(frame, frame_index);
            const local_index = @intFromEnum(local);
            if (local_index >= global_local_index.len) arcInvariant("ARC frame-local inventory names an unknown local");
            if (frame_index > 0 and @intFromEnum(frame_locals[frame_index - 1]) >= local_index) {
                arcInvariant("ARC frame-local inventory is not unique and sorted");
            }
            if (global_local_index[local_index] != no_proc_local_index) {
                arcInvariant("ARC frame-local inventory overlaps an active proc domain");
            }
            frame_locals[frame_index] = local;
            global_local_index[local_index] = @intCast(frame_index);
            mapped_len += 1;
        }

        var refcounted_count: usize = 0;
        for (frame_locals) |local| {
            const local_index = @intFromEnum(local);
            if (local_index >= local_contains_refcounted.len) arcInvariant("ARC refcounted-local table did not cover frame local");
            if (!local_contains_refcounted[local_index]) continue;
            refcounted_locals_buffer[refcounted_count] = local;
            refcounted_count += 1;

            const resource_locals = [_]LIR.LocalId{
                local,
                solution.leaderOf(local),
                solution.unitLocalOf(local),
            };
            for (resource_locals) |resource_local| {
                const resource_frame_index = requiredFrameIndex(global_local_index, resource_local);
                resource_bit_index[resource_frame_index] = arc_bit_needed;
            }
        }

        var resource_count: usize = 0;
        for (resource_bit_index, 0..) |*bit_index, frame_index| {
            if (bit_index.* != arc_bit_needed) continue;
            bit_index.* = @intCast(resource_count);
            resource_locals_buffer[resource_count] = frame_locals[frame_index];
            resource_count += 1;
        }
        const resource_full_masks = try allocator.alloc(u64, resource_count);
        @memset(resource_full_masks, 0);

        // A solved borrow group may have members in several proc specs when
        // ownership-neutral bodies share locals. Liveness rows are proc-local,
        // so record exactly the members named by this frame rather than
        // requiring every module-wide member to belong to it.
        const group_member_counts = try allocator.alloc(u32, frame_len);
        @memset(group_member_counts, 0);
        for (frame_locals) |local| {
            const leader_frame_index = requiredFrameIndex(global_local_index, solution.leaderOf(local));
            group_member_counts[leader_frame_index] += 1;
        }

        var group_count: usize = 0;
        for (frame_locals, 0..) |local, frame_index| {
            const leader = solution.leaderOf(local);
            _ = requiredFrameIndex(global_local_index, leader);
            if (leader != local) continue;
            if (group_member_counts[frame_index] <= 1) continue;
            group_bit_index[frame_index] = @intCast(group_count);
            group_leaders_buffer[group_count] = leader;
            group_count += 1;
        }

        var value_use_count: usize = 0;
        for (frame_locals, 0..) |local, frame_index| {
            if (!solution.isBorrowedCallResult(local)) continue;
            value_use_bit_index[frame_index] = @intCast(value_use_count);
            value_use_locals_buffer[value_use_count] = local;
            value_use_count += 1;
        }

        return .{
            .global_local_index = global_local_index,
            .frame_locals = frame_locals,
            .resource_bit_index = resource_bit_index,
            .resource_locals = resource_locals_buffer[0..resource_count],
            .resource_full_masks = resource_full_masks,
            .refcounted_locals = refcounted_locals_buffer[0..refcounted_count],
            .group_bit_index = group_bit_index,
            .group_leaders = group_leaders_buffer[0..group_count],
            .value_use_bit_index = value_use_bit_index,
            .value_use_locals = value_use_locals_buffer[0..value_use_count],
        };
    }

    fn requiredFrameIndex(global_local_index: []const u32, local: LIR.LocalId) usize {
        const local_index = @intFromEnum(local);
        if (local_index >= global_local_index.len) arcInvariant("ARC proc domain queried an unknown local");
        const frame_index = global_local_index[local_index];
        if (frame_index == no_proc_local_index) arcInvariant("ARC proc domain is missing a required frame local");
        return frame_index;
    }

    fn frameIndexOf(self: *const ProcArcDomain, local: LIR.LocalId) usize {
        return requiredFrameIndex(self.global_local_index, local);
    }

    fn resourceBitOf(self: *const ProcArcDomain, local: LIR.LocalId) ?usize {
        const bit_index = self.resource_bit_index[self.frameIndexOf(local)];
        if (bit_index == no_arc_bit) return null;
        return bit_index;
    }

    fn requiredResourceBitOf(self: *const ProcArcDomain, local: LIR.LocalId) usize {
        return self.resourceBitOf(local) orelse arcInvariant("ARC ownership set queried a non-resource local");
    }

    fn resourceLocalAt(self: *const ProcArcDomain, bit_index: usize) LIR.LocalId {
        if (bit_index >= self.resource_locals.len) arcInvariant("ARC ownership bit exceeded its proc domain");
        return self.resource_locals[bit_index];
    }

    fn installResidualDomains(self: *ProcArcDomain, solution: *const arc_solve.Solution, dismantles: *const arc_dismantle.Dismantles) void {
        var normal = dismantles.containers.iterator();
        while (normal.next()) |entry| self.installResidualDomain(solution, entry.key_ptr.*, entry.value_ptr.full_mask);
        var owned_only = dismantles.owned_only_containers.iterator();
        while (owned_only.next()) |entry| self.installResidualDomain(solution, entry.key_ptr.*, entry.value_ptr.full_mask);
    }

    fn installResidualDomain(self: *ProcArcDomain, solution: *const arc_solve.Solution, local: LIR.LocalId, full_mask: u64) void {
        const local_index = @intFromEnum(local);
        if (local_index >= self.global_local_index.len) return;
        if (self.global_local_index[local_index] == no_proc_local_index) return;
        const unit = solution.unitLocalOf(local);
        const bit = self.resourceBitOf(unit) orelse arcInvariant("ARC residual aggregate has no ownership resource");
        const prior = self.resource_full_masks[bit];
        if (prior != 0 and prior != full_mask) arcInvariant("ARC ownership resource has conflicting committed field domains");
        self.resource_full_masks[bit] = full_mask;
    }

    fn fullResidualMaskAt(self: *const ProcArcDomain, bit_index: usize) u64 {
        if (bit_index >= self.resource_full_masks.len) arcInvariant("ARC residual mask exceeded its proc domain");
        return self.resource_full_masks[bit_index];
    }

    fn groupBitOf(self: *const ProcArcDomain, leader: LIR.LocalId) ?usize {
        const bit_index = self.group_bit_index[self.frameIndexOf(leader)];
        if (bit_index == no_arc_bit) return null;
        return self.resource_locals.len + bit_index;
    }

    fn valueUseBitOf(self: *const ProcArcDomain, local: LIR.LocalId) ?usize {
        const bit_index = self.value_use_bit_index[self.frameIndexOf(local)];
        if (bit_index == no_arc_bit) return null;
        return self.resource_locals.len + self.group_leaders.len + bit_index;
    }

    fn livenessBitLen(self: *const ProcArcDomain) usize {
        return self.resource_locals.len + self.group_leaders.len + self.value_use_locals.len;
    }

    fn clearGlobalIndices(self: *ProcArcDomain) void {
        for (self.frame_locals, 0..) |local, expected_index| {
            const local_index = @intFromEnum(local);
            if (self.global_local_index[local_index] != expected_index) {
                arcInvariant("ARC proc domain index changed while active");
            }
            self.global_local_index[local_index] = no_proc_local_index;
        }
    }
};

/// Public `insert` function.
pub fn insert(store: *LirStore, layouts: *const layout_mod.Store, options: InsertOptions) ResourceError!void {
    var inserter = Inserter{
        .store = store,
        .layouts = layouts,
        .options = options,
    };
    const boxy_rc_descs = try computeBoxyRcDescs(store);
    defer store.allocator.free(boxy_rc_descs);

    const local_contains_refcounted = try computeLocalContainsRefcounted(store.allocator, store, layouts, boxy_rc_descs);
    defer store.allocator.free(local_contains_refcounted);
    inserter.local_contains_refcounted = local_contains_refcounted;
    inserter.boxy_rc_descs = boxy_rc_descs;

    const borrow_anchor_refcounted = try computeBorrowAnchorRefcounted(store.allocator, store, layouts, local_contains_refcounted);
    defer store.allocator.free(borrow_anchor_refcounted);

    var solution = try arc_solve.solve(
        store.allocator,
        store,
        layouts,
        borrow_anchor_refcounted,
        boxy_rc_descs,
        options.roots,
        options.consume_dead_boxes,
    );
    defer solution.deinit();
    inserter.solution = &solution;

    var dismantles = try arc_dismantle.compute(
        store.allocator,
        store,
        layouts,
        local_contains_refcounted,
        &solution,
    );
    defer dismantles.deinit();
    inserter.dismantles = &dismantles;

    // Domains are active one proc at a time. This reusable exact map makes
    // global LocalId -> proc-dense index lookup O(1) without allocating and
    // clearing a module-wide table for every proc.
    const proc_local_index = try store.allocator.alloc(u32, store.localCount());
    defer store.allocator.free(proc_local_index);
    @memset(proc_local_index, no_proc_local_index);

    // Liveness graphs are built serially. Reuse one dense statement-to-node
    // table for every build and clear exactly the statements each graph
    // touched, avoiding hash lookup in the dataflow inner loop.
    const stmt_node_indices = try store.allocator.alloc(u32, store.cfStmtCount());
    defer store.allocator.free(stmt_node_indices);
    @memset(stmt_node_indices, no_stmt_node_index);
    inserter.stmt_node_indices = stmt_node_indices;

    const base_proc_count = store.procSpecCount();
    var liveness_arena = std.heap.ArenaAllocator.init(store.allocator);
    defer liveness_arena.deinit();
    inserter.liveness_allocator = liveness_arena.allocator();
    const liveness_graphs = try store.allocator.alloc(?Inserter.ReadBeforeRebindGraph, base_proc_count);
    defer store.allocator.free(liveness_graphs);
    @memset(liveness_graphs, null);
    inserter.liveness_graphs = liveness_graphs;

    // Original (ownership-neutral) bodies stay valid after each proc's base
    // emission because materialization clones statements; specialized variants
    // re-emit from these.
    var original_bodies = try store.allocator.alloc(?LIR.CFStmtId, base_proc_count);
    defer store.allocator.free(original_bodies);
    var original_frame_locals = try store.allocator.alloc(LIR.LocalSpan, base_proc_count);
    defer store.allocator.free(original_frame_locals);
    for (0..base_proc_count) |proc_index| {
        const proc = store.getProcSpec(@enumFromInt(@as(u32, @intCast(proc_index))));
        original_bodies[proc_index] = proc.body;
        original_frame_locals[proc_index] = proc.frame_locals;
    }

    var variants = VariantTable{
        .map = std.AutoHashMap(VariantSelector, LIR.LirProcSpecId).init(store.allocator),
        .sigs = .empty,
        .queue = .empty,
        .enabled = options.specialize,
        .original_bodies = original_bodies,
        .original_frame_locals = original_frame_locals,
    };
    defer {
        variants.map.deinit();
        variants.sigs.deinit(store.allocator);
        variants.queue.deinit(store.allocator);
    }
    inserter.variants = &variants;

    var emit_index: usize = 0;
    while (true) {
        var emit_proc: LIR.LirProcSpecId = undefined;
        var source_proc: LIR.LirProcSpecId = undefined;
        var emit_sig: arc_sig.RcSig = undefined;
        if (emit_index < base_proc_count) {
            emit_proc = @enumFromInt(@as(u32, @intCast(emit_index)));
            source_proc = emit_proc;
            emit_sig = solution.sigOf(emit_proc);
            emit_index += 1;
        } else if (variants.queue.items.len > 0) {
            const queued = variants.queue.pop().?;
            emit_proc = queued.variant;
            source_proc = queued.source;
            emit_sig = queued.sig;
        } else {
            break;
        }

        const emit_proc_spec = store.getProcSpecPtr(emit_proc);
        emit_proc_spec.rc_borrowed_params = emit_sig.borrowed_params;
        emit_proc_spec.rc_ret_borrowed = emit_sig.ret_mode == .borrowed;
        emit_proc_spec.rc_ret_lenders = emit_sig.ret_lenders;

        const body = original_bodies[@intFromEnum(source_proc)] orelse continue;
        var domain_arena = std.heap.ArenaAllocator.init(store.allocator);
        defer domain_arena.deinit();
        var domain = try ProcArcDomain.init(
            domain_arena.allocator(),
            store,
            &solution,
            local_contains_refcounted,
            proc_local_index,
            original_frame_locals[@intFromEnum(source_proc)],
        );
        domain.installResidualDomains(&solution, &dismantles);
        defer domain.clearGlobalIndices();
        inserter.current_domain = &domain;
        defer inserter.current_domain = null;

        const emit_args = store.getProcSpec(emit_proc).args;
        inserter.current_proc = emit_proc;
        inserter.current_sig = emit_sig;
        inserter.current_proc_body = body;
        inserter.current_source_proc = source_proc;

        // The ownership-neutral body and emitted LIR outlive this iteration;
        // all solver and materialization state does not. A single per-emission arena
        // gives those exact temporary structures one lifetime and removes
        // thousands of individually paired allocations and frees.
        var emission_arena = std.heap.ArenaAllocator.init(store.allocator);
        defer emission_arena.deinit();
        inserter.emission_allocator = emission_arena.allocator();
        inserter.solve_allocator = inserter.emission_allocator;
        var death_scratch = std.ArrayList(ReleaseDecision).empty;
        var transfer_position_scratch = std.ArrayList(u32).empty;
        var retain_arg_scratch = std.ArrayList(LIR.LocalId).empty;
        var dismantle_temps = std.ArrayList(LIR.LocalId).empty;
        inserter.death_scratch = &death_scratch;
        inserter.transfer_position_scratch = &transfer_position_scratch;
        inserter.retain_arg_scratch = &retain_arg_scratch;
        inserter.dismantle_temps = &dismantle_temps;
        var arc_plans = ArcPlans{};
        inserter.arc_plans = &arc_plans;
        var restitution_switches: std.AutoHashMapUnmanaged(LIR.CFStmtId, RestitutionSwitch) = .empty;
        inserter.restitution_switches = &restitution_switches;
        inserter.next_loop_keep_id = 1;
        var loop_liveness_caches = std.ArrayList(LoopLivenessCache).empty;
        try loop_liveness_caches.append(inserter.emission_allocator, .{});
        inserter.loop_liveness_caches = &loop_liveness_caches;

        var owned_binding_override = try OwnedSet.init(inserter.emission_allocator, &domain);
        defer owned_binding_override.deinit();
        inserter.owned_binding_override = &owned_binding_override;

        var unique_param_override = try OwnedSet.init(inserter.emission_allocator, &domain);
        defer unique_param_override.deinit();
        inserter.unique_param_override = &unique_param_override;

        // Variant parameter positions demanded owned override the solved
        // borrowed binding for this emission only, and positions the demand
        // vector proves unique seed the body's born-unique view.
        const solved_sig = solution.sigOf(source_proc);
        const emit_params_for_overrides = store.getLocalSpan(emit_args);
        for (0..GuardedList.borrowLen(emit_params_for_overrides)) |position| {
            const param = GuardedList.at(emit_params_for_overrides, position);
            const bit = arc_sig.paramBit(position) orelse break;
            if (solved_sig.paramMode(position) == .borrowed and emit_sig.paramMode(position) == .owned) {
                owned_binding_override.set(param);
            }
            if ((emit_sig.unique_params & bit) != 0) {
                unique_param_override.set(param);
            }
        }
        for (domain.frame_locals) |local| {
            if (dismantles.isTakeBinding(local)) owned_binding_override.set(local);
        }
        // A field read solved borrowed in the unconditional base signature
        // becomes an owned binding exactly when the explicit dismantle plan
        // says it takes a field from a parameter overridden to owned. This is
        // part of the mandatory field-take variant, not optional inlining or
        // specialization.
        for (domain.frame_locals) |local| {
            const root = dismantles.ownedOnlyBindingRoot(local) orelse continue;
            if (owned_binding_override.contains(root)) owned_binding_override.set(local);
        }

        const join_bodies = solution.joinBodiesOf(source_proc);
        inserter.join_bodies = join_bodies;
        defer inserter.join_bodies = &.{};
        const final_joins = try inserter.emission_allocator.alloc(?LIR.JoinPoint, join_bodies.len);
        @memset(final_joins, null);
        inserter.final_joins = final_joins;
        defer inserter.final_joins = &.{};
        var owned = try OwnedSet.init(inserter.emission_allocator, &domain);
        defer owned.deinit();
        const emit_params_for_owned = store.getLocalSpan(emit_args);
        for (0..GuardedList.borrowLen(emit_params_for_owned)) |position| {
            const param = GuardedList.at(emit_params_for_owned, position);
            if (emit_sig.paramMode(position) == .owned) {
                if (inserter.localContainsRefcounted(param)) owned.set(param);
            }
        }

        const join_summaries = try inserter.emission_allocator.alloc(?*JoinSummary, join_bodies.len);
        @memset(join_summaries, null);
        const switch_summaries = try inserter.emission_allocator.alloc(?*SwitchSummary, solution.switchCountOf(source_proc));
        @memset(switch_summaries, null);
        inserter.join_summaries = join_summaries;
        inserter.switch_summaries = switch_summaries;
        defer {
            inserter.join_summaries = &.{};
            inserter.switch_summaries = &.{};
        }
        try inserter.solveProcSummaries(body, &owned);

        const rewritten_body = try inserter.materializeArcPlans();
        const join_points = try inserter.finishFinalJoinPoints();
        store.setProcSpecBodyAndJoinPoints(emit_proc, rewritten_body, join_points);

        // Dismantle temporaries are fresh locals referenced by the emitted
        // body; the frame must own them. They were appended to the store in
        // ascending id order above every existing local, so concatenating
        // keeps the frame span sorted.
        if (dismantle_temps.items.len > 0) {
            const emit_spec = store.getProcSpec(emit_proc);
            const old_frame = store.getLocalSpan(emit_spec.frame_locals);
            const old_len = GuardedList.borrowLen(old_frame);
            var frame = try std.ArrayList(LIR.LocalId).initCapacity(store.allocator, old_len + dismantle_temps.items.len);
            defer frame.deinit(store.allocator);
            for (0..old_len) |i| frame.appendAssumeCapacity(GuardedList.at(old_frame, i));
            frame.appendSliceAssumeCapacity(dismantle_temps.items);
            const frame_locals = try store.addLocalSpan(frame.items);
            store.getProcSpecPtr(emit_proc).frame_locals = frame_locals;
        }
    }

    if (builtin.mode == .Debug) {
        const all_sigs = try store.allocator.alloc(arc_sig.RcSig, store.procSpecCount());
        defer store.allocator.free(all_sigs);
        for (all_sigs, 0..) |*sig, proc_index| {
            sig.* = if (proc_index < solution.sigs.len)
                solution.sigs[proc_index]
            else
                variants.sigs.items[proc_index - solution.sigs.len];
        }
        const certified_boxy_rc_descs = try computeBoxyRcDescs(store);
        defer store.allocator.free(certified_boxy_rc_descs);
        try arc_certify.certifyStoreOrPanic(store.allocator, store, layouts, certified_boxy_rc_descs, .{
            .sigs = all_sigs,
            .outcomes = solution.outcomes,
        }, options.roots);
    }
}

fn computeBoxyRcDescs(store: *const LirStore) ResourceError![]?LIR.BoxyDescRef {
    const local_count = store.localCount();
    const descs = try store.allocator.alloc(?LIR.BoxyDescRef, local_count);
    for (0..local_count) |index| {
        const local_id: LIR.LocalId = @enumFromInt(@as(u32, @intCast(index)));
        descs[index] = store.getLocal(local_id).boxy_desc;
    }
    return descs;
}

fn boxyDescForLocal(descs: []const ?LIR.BoxyDescRef, local: LIR.LocalId) ?LIR.BoxyDescRef {
    const index = @intFromEnum(local);
    if (index >= descs.len) return null;
    return descs[index];
}

fn computeLocalContainsRefcounted(
    allocator: Allocator,
    store: *const LirStore,
    layouts: *const layout_mod.Store,
    boxy_rc_descs: []const ?LIR.BoxyDescRef,
) ResourceError![]bool {
    const local_count = store.localCount();
    if (boxy_rc_descs.len != local_count) arcInvariant("ARC Boxy descriptor table did not cover every local");
    const contains = try allocator.alloc(bool, local_count);
    errdefer allocator.free(contains);
    for (0..local_count) |index| {
        const local_id: LIR.LocalId = @enumFromInt(@as(u32, @intCast(index)));
        const local = store.getLocal(local_id);
        contains[index] = layouts.layoutContainsRefcounted(layouts.getLayout(local.layout_idx));
    }
    // An `erased_capture_load` whose aggregate contains descriptor-driven
    // fields is an explicit borrowed view into the executing callable's capture
    // allocation. The view has no aggregate descriptor of its own, so it cannot
    // use a layout-driven concrete helper. Keep it out of emission;
    // `computeBorrowAnchorRefcounted` adds it back to the solver domain so its
    // projected fields remain tied to the callable.
    var visited = std.AutoHashMap(layout_mod.Idx, void).init(allocator);
    defer visited.deinit();
    var stack = std.ArrayList(layout_mod.Idx).empty;
    defer stack.deinit(allocator);
    for (0..store.cfStmtCount()) |stmt_index| {
        const stmt_id: LIR.CFStmtId = @enumFromInt(@as(u32, @intCast(stmt_index)));
        const stmt = store.getCFStmt(stmt_id);
        if (stmt == .assign_low_level and stmt.assign_low_level.op == .erased_capture_load) {
            const target = stmt.assign_low_level.target;
            const target_layout = store.getLocal(target).layout_idx;
            if (try layoutMayContainBoxyDynamic(allocator, layouts, target_layout, &visited, &stack)) {
                contains[@intFromEnum(target)] = false;
            }
        }
    }

    var changed = true;
    while (changed) {
        changed = false;
        for (0..store.cfStmtCount()) |stmt_index| {
            const stmt_id: LIR.CFStmtId = @enumFromInt(@as(u32, @intCast(stmt_index)));
            const stmt = store.getCFStmt(stmt_id);
            if (stmt == .assign_ref) {
                const assign = stmt.assign_ref;
                switch (assign.op) {
                    .local => |source| changed = markLocalRcIfSourceRc(contains, assign.target, source) or changed,
                    .nominal => |op| changed = markLocalRcIfSourceRc(contains, assign.target, op.backing_ref) or changed,
                    .list_reinterpret => |op| changed = markLocalRcIfSourceRc(contains, assign.target, op.backing_ref) or changed,
                    .field,
                    .tag_payload,
                    .tag_payload_struct,
                    .discriminant,
                    => {},
                }
            } else if (stmt == .assign_list) {
                const assign = stmt.assign_list;
                changed = markLocalRcIfSpanContainsRc(store, contains, assign.target, assign.elems) or changed;
            } else if (stmt == .assign_struct) {
                const assign = stmt.assign_struct;
                changed = markLocalRcIfSpanContainsRc(store, contains, assign.target, assign.fields) or changed;
            } else if (stmt == .assign_tag) {
                const assign = stmt.assign_tag;
                if (assign.payload) |payload| {
                    changed = markLocalRcIfSourceRc(contains, assign.target, payload) or changed;
                }
                if (assign.target_desc != null) {
                    changed = markLocalRc(contains, assign.target) or changed;
                }
            } else if (stmt == .assign_boxy_box) {
                changed = markLocalRc(contains, stmt.assign_boxy_box.target) or changed;
            } else if (stmt == .assign_boxy_reuse_box) {
                changed = markLocalRc(contains, stmt.assign_boxy_reuse_box.target) or changed;
            } else if (stmt == .assign_boxy_tag) {
                changed = markLocalRc(contains, stmt.assign_boxy_tag.target) or changed;
            }
        }
    }

    return contains;
}

/// Borrow-anchor refcounted set for the ARC solver. Extends the emission-time
/// refcounted set with payload-read projections (`.field`, `.tag_payload`,
/// `.tag_payload_struct`) whose result carries descriptor-driven dynamic
/// (`erased_box`) content borrowed out of a refcounted source. Such a
/// projection is an alias into its source's allocation whose extracted boxes
/// stay live past the projection, so the source's release must land after the
/// projection's last use. An erased capture load similarly produces a view of
/// the capture storage owned by the pinned callable frame. These intermediate
/// views own no RC unit: their dynamic payloads are refcounted by descriptor,
/// which the layout-only refcount check cannot see, so the views carry no Boxy
/// descriptor of their own. Marking them refcounted for the solver alone lets
/// projections join an explicit liveness group. Emission keeps consulting the
/// narrower `local_contains_refcounted`, so a solver-only anchor is never
/// forced to carry an RC helper it lacks.
fn computeBorrowAnchorRefcounted(
    allocator: Allocator,
    store: *const LirStore,
    layouts: *const layout_mod.Store,
    local_contains_refcounted: []const bool,
) ResourceError![]bool {
    const anchor = try allocator.dupe(bool, local_contains_refcounted);
    errdefer allocator.free(anchor);

    var visited = std.AutoHashMap(layout_mod.Idx, void).init(allocator);
    defer visited.deinit();
    var stack = std.ArrayList(layout_mod.Idx).empty;
    defer stack.deinit(allocator);

    var changed = true;
    while (changed) {
        changed = false;
        for (0..store.cfStmtCount()) |stmt_index| {
            const stmt_id: LIR.CFStmtId = @enumFromInt(@as(u32, @intCast(stmt_index)));
            const stmt = store.getCFStmt(stmt_id);
            if (stmt == .assign_low_level) {
                const assign = stmt.assign_low_level;
                if (assign.op != .erased_capture_load) continue;
                const target_layout = store.getLocal(assign.target).layout_idx;
                if (!try layoutMayContainBoxyDynamic(allocator, layouts, target_layout, &visited, &stack)) continue;
                changed = markLocalRc(anchor, assign.target) or changed;
            } else if (stmt == .assign_ref) {
                const assign = stmt.assign_ref;
                if (assign.op != .field and assign.op != .tag_payload and assign.op != .tag_payload_struct) continue;
                const source_index = @intFromEnum(refOpSource(assign.op));
                if (source_index >= anchor.len or !anchor[source_index]) continue;
                const target_layout = store.getLocal(assign.target).layout_idx;
                if (!try layoutMayContainBoxyDynamic(allocator, layouts, target_layout, &visited, &stack)) continue;
                changed = markLocalRc(anchor, assign.target) or changed;
            }
        }
    }
    return anchor;
}

/// Cycle-safe check for whether a layout may hold descriptor-driven dynamic
/// (`box_of_zst`) content. Recursive tag unions reference themselves through
/// their layout indices, so the walk tracks visited indices; `visited` and
/// `stack` are caller-owned scratch reused across queries.
fn layoutMayContainBoxyDynamic(
    allocator: Allocator,
    layouts: *const layout_mod.Store,
    layout_idx: layout_mod.Idx,
    visited: *std.AutoHashMap(layout_mod.Idx, void),
    stack: *std.ArrayList(layout_mod.Idx),
) ResourceError!bool {
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

fn markLocalRc(contains: []bool, local: LIR.LocalId) bool {
    const index = @intFromEnum(local);
    if (index >= contains.len or contains[index]) return false;
    contains[index] = true;
    return true;
}

fn markLocalRcIfSourceRc(contains: []bool, target: LIR.LocalId, source: LIR.LocalId) bool {
    const source_index = @intFromEnum(source);
    if (source_index >= contains.len or !contains[source_index]) return false;
    return markLocalRc(contains, target);
}

fn markLocalRcIfSpanContainsRc(store: *const LirStore, contains: []bool, target: LIR.LocalId, span: LIR.LocalSpan) bool {
    const locals = store.getLocalSpan(span);
    for (0..GuardedList.borrowLen(locals)) |span_index| {
        const local = GuardedList.at(locals, span_index);
        const local_index = @intFromEnum(local);
        if (local_index < contains.len and contains[local_index]) return markLocalRc(contains, target);
    }
    return false;
}

const VariantSelector = struct {
    source: LIR.LirProcSpecId,
    borrowed_params: arc_sig.ParamMask,
    ret_mode: arc_sig.Mode,
    /// Parameter positions the demand vector seeds born-unique.
    unique_params: arc_sig.ParamMask,
    outcomes: arc_sig.OutcomeSpan,
};

const QueuedVariant = struct {
    variant: LIR.LirProcSpecId,
    source: LIR.LirProcSpecId,
    sig: arc_sig.RcSig,
};

/// Mode-specialized proc variants keyed by demanded ownership vector.
const VariantTable = struct {
    map: std.AutoHashMap(VariantSelector, LIR.LirProcSpecId),
    /// Signature per variant, indexed by (variant id - base proc count).
    sigs: std.ArrayList(arc_sig.RcSig),
    queue: std.ArrayList(QueuedVariant),
    enabled: bool,
    /// Ownership-neutral bodies of the base procs, for variant re-emission.
    original_bodies: []const ?LIR.CFStmtId,
    /// Pre-ARC frame inventories of the base procs. ARC appends generated
    /// dismantling temporaries to each emitted proc's frame; variants must
    /// solve from the immutable producer-authored inventory and append only
    /// the temporaries generated by their own emission.
    original_frame_locals: []const LIR.LocalSpan,
};

/// Explicit loop-liveness input. The identity is allocated with the join
/// summary and travels with the keep-set; no consumer reconstructs it from a
/// pointer address.
const LoopKeep = struct {
    set: *const OwnedSet,
    id: u32,
};

const LoopLivenessCache = struct {
    /// Dense liveness-node rows for one compact loop identity. Null rows are
    /// outside the forward closure of that join's two explicit regions.
    rows: []?ExactBitSet = &.{},
    active: ?std.bit_set.DynamicBitSetUnmanaged = null,
    keep_reads: ?ExactBitSet = null,
    /// The join regions interpreted under this loop identity. Solving their
    /// forward closure up front makes every later query a direct node lookup,
    /// including sibling switch arms that the first query did not traverse.
    region_roots: [2]LIR.CFStmtId = undefined,
    initialized: bool = false,
    consumed_keep_bits: bool = false,
    dirty: bool = false,
};

/// One exact finite-lattice bit set. Procedure-local ARC domains commonly fit
/// in one machine word; those sets stay inline and wider producer-authored
/// domains allocate exact words. Ownership and liveness use the same
/// representation so snapshots preserve identical operations and ordering.
const ExactBitSet = struct {
    bit_len: usize,
    storage: union(enum) {
        inline_word: usize,
        allocated: std.bit_set.DynamicBitSetUnmanaged,
    },

    fn initEmpty(allocator: Allocator, bit_len: usize) Allocator.Error!ExactBitSet {
        if (bit_len <= @bitSizeOf(usize)) {
            return .{ .bit_len = bit_len, .storage = .{ .inline_word = 0 } };
        }
        return .{
            .bit_len = bit_len,
            .storage = .{ .allocated = try std.bit_set.DynamicBitSetUnmanaged.initEmpty(allocator, bit_len) },
        };
    }

    fn deinit(self: *ExactBitSet, allocator: Allocator) void {
        switch (self.storage) {
            .inline_word => {},
            .allocated => |*bits| bits.deinit(allocator),
        }
        self.* = undefined;
    }

    fn clone(self: *const ExactBitSet, allocator: Allocator) Allocator.Error!ExactBitSet {
        return switch (self.storage) {
            .inline_word => |bits| .{ .bit_len = self.bit_len, .storage = .{ .inline_word = bits } },
            .allocated => |bits| .{ .bit_len = self.bit_len, .storage = .{ .allocated = try bits.clone(allocator) } },
        };
    }

    fn set(self: *ExactBitSet, bit: usize) void {
        std.debug.assert(bit < self.bit_len);
        switch (self.storage) {
            .inline_word => |*bits| bits.* |= @as(usize, 1) << @intCast(bit),
            .allocated => |*bits| bits.set(bit),
        }
    }

    fn unset(self: *ExactBitSet, bit: usize) void {
        std.debug.assert(bit < self.bit_len);
        switch (self.storage) {
            .inline_word => |*bits| bits.* &= ~(@as(usize, 1) << @intCast(bit)),
            .allocated => |*bits| bits.unset(bit),
        }
    }

    fn isSet(self: *const ExactBitSet, bit: usize) bool {
        std.debug.assert(bit < self.bit_len);
        return switch (self.storage) {
            .inline_word => |bits| bits & (@as(usize, 1) << @intCast(bit)) != 0,
            .allocated => |bits| bits.isSet(bit),
        };
    }

    fn unsetAll(self: *ExactBitSet) void {
        switch (self.storage) {
            .inline_word => |*bits| bits.* = 0,
            .allocated => |*bits| bits.unsetAll(),
        }
    }

    fn setUnion(self: *ExactBitSet, other: ExactBitSet) void {
        std.debug.assert(self.bit_len == other.bit_len);
        switch (self.storage) {
            .inline_word => |*bits| bits.* |= other.storage.inline_word,
            .allocated => |*bits| bits.setUnion(other.storage.allocated),
        }
    }

    fn setIntersection(self: *ExactBitSet, other: ExactBitSet) void {
        std.debug.assert(self.bit_len == other.bit_len);
        switch (self.storage) {
            .inline_word => |*bits| bits.* &= other.storage.inline_word,
            .allocated => |*bits| bits.setIntersection(other.storage.allocated),
        }
    }

    fn count(self: *const ExactBitSet) usize {
        return switch (self.storage) {
            .inline_word => |bits| @popCount(bits),
            .allocated => |bits| bits.count(),
        };
    }

    fn Iterator(comptime options: std.bit_set.IteratorOptions) type {
        if (options.kind != .set) @compileError("ARC exact sets iterate only set bits");
        return union(enum) {
            inline_word: usize,
            allocated: std.bit_set.DynamicBitSetUnmanaged.Iterator(options),

            fn next(self: *@This()) ?usize {
                return switch (self.*) {
                    .inline_word => |*remaining| {
                        if (remaining.* == 0) return null;
                        const bit = switch (options.direction) {
                            .forward => @ctz(remaining.*),
                            .reverse => @bitSizeOf(usize) - 1 - @clz(remaining.*),
                        };
                        remaining.* &= ~(@as(usize, 1) << @intCast(bit));
                        return bit;
                    },
                    .allocated => |*iter| iter.next(),
                };
            }
        };
    }

    fn iterator(self: *const ExactBitSet, comptime options: std.bit_set.IteratorOptions) Iterator(options) {
        return switch (self.storage) {
            .inline_word => |bits| .{ .inline_word = bits },
            .allocated => |*bits| .{ .allocated = bits.iterator(options) },
        };
    }

    fn eql(self: *const ExactBitSet, other: ExactBitSet) bool {
        if (self.bit_len != other.bit_len) return false;
        return switch (self.storage) {
            .inline_word => |bits| bits == other.storage.inline_word,
            .allocated => |bits| bits.eql(other.storage.allocated),
        };
    }
};

// Join-summary solver
//
// One structured abstract interpretation per proc emission computes each
// join's entry keep-set, body keep-set, and body reachability, plus each
// continuation-carrying switch's merged branch exit state. The same
// dependency visits update stable ArcPlan slots; there is no second ownership
// walk after convergence.
//
// Domain and soundness: the abstract state is a must-owned unit set—a bit
// is present at a program point only when every path represented by that
// point still carries the ownership unit. Statement transfers are the shared
// `transferFor*` layer, whose state effects are monotone in the input set
// (every condition on the owned set tests a single unit bit, so smaller
// inputs produce smaller outputs) and distribute over intersection.
// Merges therefore commute with transfers, so one canonical state per
// region equals the intersection of the per-path states reaching it.
//
// Fixpoint and termination: every accumulator—a join's entry state, the
// per-jump-site states feeding a join's body keep, and a switch's merged
// continuation entry—only ever shrinks (set intersection) or receives the
// fixed additions the equations prescribe (join params placed after the
// body-use filter). A region is re-walked only when an accumulator it
// depends on shrinks, each accumulator is a finite bitset that can shrink at
// most once per bit, and segment walks cannot cycle because join bodies are
// entered only through their seeded keep-set and remainders only through
// their join statement. The walk count is therefore bounded by the bit
// budget of the accumulators, and each walk is linear in region size.
//
// The equations that update solver-owned ArcPlan slots:
// - entry_state(J): intersection of the must-owned states reaching J's join
//   statement.
// - entry_keep(J): entry_state filtered to units whose liveness group is
//   read from the remainder, unioned with `body_keep & entry_state`
//   (a unit rebound in the remainder before every jump but read in the body
//   escapes the remainder filter yet must survive the join entry).
// - body_keep(J): intersection of the states at every eligible jump to J
//   (a jump is eligible unless it sits inside J's own body region—loop
//   back edges conform at emission by releasing down to the keep), filtered
//   to units read in the shared body, then join params placed owned and
//   maybe-initialized params placed conditionally, exactly like the
//   emission-side keep construction. Seeded from above (all body-read units
//   plus params) so descent stays monotone. Params are placed only when the
//   back edges also rebind them (`back_edge_params`): a param a back edge
//   leaves alone re-enters the body holding the value the previous iteration
//   already released, so treating it as freshly owned would release it once
//   per iteration.
// - body_reachable(J): whether any eligible jump contributed.
// - switch common: intersection of branch exit states that reach the
//   continuation statement without crossing a join frame.

const JoinSummary = struct {
    index: u32,
    id: LIR.JoinPointId,
    start: LIR.CFStmtId,
    params: LIR.LocalSpan,
    maybe_uninitialized_params: LIR.LocalSpan,
    remainder: LIR.CFStmtId,
    body: LIR.CFStmtId,
    /// Must-owned state at the join statement, intersected over arrivals.
    entry_state: OwnedSet,
    entry_keep: OwnedSet,
    /// Pointer-stable: loop liveness boundary facts and structured plan
    /// contexts reference this set in place.
    body_keep: OwnedSet,
    /// Monotone intersection of the latest state from every eligible jump
    /// site. When one site shrinks, intersecting only that new state produces
    /// the exact new all-site meet.
    jump_common: OwnedSet,
    /// Monotone meet of join-param ownership over the back edges (jumps that
    /// sit inside this join's own body). A param that a back edge does not
    /// re-initialize arrives at the body carrying the same value the previous
    /// iteration already released, so it must not be placed owned in
    /// `body_keep`; see `placeSolveJoinParamsInto`.
    back_edge_params: OwnedSet,
    back_edge_seen: bool = false,
    body_keep_seeded: bool = false,
    body_reachable: bool = false,
    loop_keep_id: u32,
    remainder_plan: u32,
    body_plan: u32,
    arrival_plans: std.ArrayList(u32) = .empty,
    jump_plans: std.ArrayList(u32) = .empty,
    /// Context the join statement was first reached in; remainder and body
    /// segments derive theirs from it.
    origin_ctx: SolveContext,
    /// Must-owned state per eligible jump statement targeting this join.
    jump_states: []?OwnedSet,
    process_queued: bool = false,
    body_walk_queued: bool = false,
};

const SwitchSummary = struct {
    index: u32,
    start: LIR.CFStmtId,
    continuation: LIR.CFStmtId,
    /// Intersection of branch exit states reaching the continuation; only
    /// meaningful once `reached` is set.
    common: OwnedSet,
    reached: bool = false,
    resume_ctx: SolveContext,
    continuation_plan: u32,
    exit_plans: std.ArrayList(u32) = .empty,
    /// Plans inside a join frame that structurally encounter this
    /// continuation before any ordinary branch makes it reachable. They are
    /// initially completed as independent paths; the first contributing
    /// branch converts them to shared-continuation stops using their exact
    /// saved state.
    latent_stop_plans: std.ArrayList(u32) = .empty,
    control_plans: std.ArrayList(u32) = .empty,
    resume_queued: bool = false,
};

/// Units a direct call conditionally returns to the caller, partitioned by
/// the exact result-discriminant switch that refines them.
const RestoredResource = union(enum) {
    unit: LIR.LocalId,
    field: arc_dismantle.Take,
};

const RestitutionSwitch = struct {
    branch_resources: []std.ArrayList(RestoredResource),
    default_resources: std.ArrayList(RestoredResource) = .empty,
    /// The converged resource carrier for each call argument position. One
    /// position can denote a whole unit or one residual field, never both.
    position_resources: [arc_sig.tracked_param_count]?RestoredResource = .{null} ** arc_sig.tracked_param_count,
    restored_positions: arc_sig.ParamMask = 0,
};

const OutcomeRestitution = struct {
    refinement: OutcomeRefinement,
    sig: arc_sig.RcSig,
    position: usize,
    resource: RestoredResource,
};

const OutcomeRefinement = struct {
    stmt: LIR.CFStmtId,
};

/// Stop entry for switch-continuation collection. `summary` is set for the
/// scope whose branch exits feed the switch merge and null once a join frame
/// intervenes: a join starts a separate ownership frame, so its interior
/// reaching an enclosing continuation contributes nothing.
const SolveStop = struct {
    stmt: LIR.CFStmtId,
    summary: *SwitchSummary,
    contributes: bool,
    parent: ?*const SolveStop,
};

/// Join bodies the current segment is nested inside; jumps targeting one of
/// these are loop back edges and do not feed that join's body keep.
const SolveBodyScope = struct {
    join_index: u32,
    parent: ?*const SolveBodyScope,
};

const SolveContext = struct {
    loop_keep: ?LoopKeep = null,
    stops: ?*const SolveStop = null,
    body_scope: ?*const SolveBodyScope = null,
};

const SolveSegment = struct {
    cursor: LIR.CFStmtId,
    owned: OwnedSet,
    ctx: SolveContext,
    plan_index: u32,
    plan_version: u32,
};

const SolveTask = union(enum) {
    segment: *SolveSegment,
    join_process: u32,
    body_walk: u32,
    switch_resume: u32,
};

fn cloneOwnedSetWith(allocator: Allocator, source: *const OwnedSet) ResourceError!OwnedSet {
    const bits = try source.bits.clone(allocator);
    const residual_masks = try allocator.dupe(u64, source.residual_masks);
    return .{ .allocator = allocator, .domain = source.domain, .bits = bits, .residual_masks = residual_masks };
}

fn assignOwnedSet(target: *OwnedSet, source: *const OwnedSet) void {
    target.requireSameDomain(source);
    target.bits.unsetAll();
    target.bits.setUnion(source.bits);
    @memcpy(target.residual_masks, source.residual_masks);
}

/// Intersects `target` with `other` in place, reporting whether any bit
/// dropped. Intersection never adds bits, so a changed population count is
/// exactly a changed set.
fn intersectOwnedSetChanged(target: *OwnedSet, other: *const OwnedSet) bool {
    target.requireSameDomain(other);
    var changed = false;
    var before = target.bits.iterator(.{});
    while (before.next()) |bit| {
        if (!other.bits.isSet(bit) or (target.residual_masks[bit] & ~other.residual_masks[bit]) != 0) {
            changed = true;
            break;
        }
    }
    target.intersect(other);
    return changed;
}

/// Sentinel outside the compact plan-index domain.
const no_plan: u32 = std.math.maxInt(u32);

/// Concrete release form selected during planning. Materialization does not
/// consult solved maybe-initialized metadata to decide which LIR statement to
/// emit.
const ReleaseDecision = union(enum) {
    initialized: LIR.LocalId,
    /// Release exactly these committed aggregate field places. A zero mask
    /// settles a representation shell without emitting RC statements.
    residual: struct {
        value: LIR.LocalId,
        field_mask: u64,
    },
    maybe_initialized: struct {
        value: LIR.LocalId,
        condition: LIR.LocalId,
        condition_mask: u64,
    },
};

/// One solver-owned linear decision. The small lists retain their capacity
/// across fixed-point revisits; only the converged contents are consumed by
/// materialization.
const LowLevelSelection = struct {
    op: LIR.LowLevel,
    rc_effect: LIR.LowLevel.RcEffect,
};

const ArcPlanStep = struct {
    initialized: bool = false,
    stmt: LIR.CFStmtId = undefined,
    pre_release: ?ReleaseDecision = null,
    pre_release_extra: std.ArrayList(ReleaseDecision) = .empty,
    pre_retain: std.ArrayList(LIR.LocalId) = .empty,
    retain_assign_ref_target: bool = true,
    /// Absent committed field places on a same-layout representation-shell
    /// alias, in the dismantle container's compact field-mask domain.
    residual_shell_absent_mask: u64 = 0,
    residual_shell_all_rc_fields_absent: bool = false,
    retain_set_target: bool = true,
    preserve_consumed_args: u64 = 0,
    transfer_mask: u64 = 0,
    transfer_positions: std.ArrayList(u32) = .empty,
    transfer_single: bool = false,
    reuse_unique: bool = false,
    skip_result_retain: bool = false,
    unique_args: u64 = 0,
    low_level_selection: ?LowLevelSelection = null,
    retain_call_result: bool = false,
    call_callee: ?LIR.LirProcSpecId = null,
    call_demanded: arc_sig.RcSig = arc_sig.RcSig.all_owned,
    call_target_override: ?LIR.LirProcSpecId = null,
    post_release: std.ArrayList(ReleaseDecision) = .empty,

    fn reset(self: *ArcPlanStep, stmt: LIR.CFStmtId) void {
        self.initialized = true;
        self.stmt = stmt;
        self.pre_release = null;
        self.pre_release_extra.clearRetainingCapacity();
        self.pre_retain.clearRetainingCapacity();
        self.retain_assign_ref_target = true;
        self.residual_shell_absent_mask = 0;
        self.residual_shell_all_rc_fields_absent = false;
        self.retain_set_target = true;
        self.preserve_consumed_args = 0;
        self.transfer_mask = 0;
        self.transfer_positions.clearRetainingCapacity();
        self.transfer_single = false;
        self.reuse_unique = false;
        self.skip_result_retain = false;
        self.unique_args = 0;
        self.low_level_selection = null;
        self.retain_call_result = false;
        self.call_callee = null;
        self.call_demanded = arc_sig.RcSig.all_owned;
        self.call_target_override = null;
        self.post_release.clearRetainingCapacity();
    }
};

const ArcPlanTerminal = union(enum) {
    none,
    /// A switch branch reached its shared continuation. `target_plan` is a
    /// structured plan identity, not a statement-id lookup.
    stop: struct {
        switch_index: u32,
        target_plan: u32,
        releases: std.ArrayList(ReleaseDecision),
    },
    join: struct {
        stmt: LIR.CFStmtId,
        join_index: u32,
        remainder_plan: u32,
        body_plan: u32,
        body_reachable: bool,
        releases: std.ArrayList(ReleaseDecision),
    },
    switch_stmt: struct {
        stmt: LIR.CFStmtId,
        branch_plans: []u32,
        default_plan: u32,
        continuation_plan: u32,
        continuation_reachable: bool,
    },
    initialized_payload_switch: struct {
        stmt: LIR.CFStmtId,
        initialized_plan: u32,
        uninitialized_plan: u32,
    },
    str_match: struct {
        stmt: LIR.CFStmtId,
        match_plan: u32,
        miss_plan: u32,
        capture_retain_count: u16,
    },
    boxy_tag_match: struct {
        stmt: LIR.CFStmtId,
        match_plan: u32,
        miss_plan: u32,
    },
    str_match_set: struct {
        stmt: LIR.CFStmtId,
        match_plans: []u32,
        miss_plan: u32,
        capture_retain_counts: []u16,
    },
    jump: struct {
        stmt: LIR.CFStmtId,
        join_index: u32,
        releases: std.ArrayList(ReleaseDecision),
    },
    terminal: struct {
        stmt: LIR.CFStmtId,
        releases: std.ArrayList(ReleaseDecision),
        retain_value: ?LIR.LocalId,
    },
};

/// A stable structured-path/context slot. The solver overwrites its steps and
/// terminal whenever the exact dependency that supplies its entry state
/// changes. `previous_terminal` preserves child plan identities across that
/// overwrite; those identities are what distinguish the same statement under
/// different structured ownership contexts.
const ArcPlan = struct {
    start: LIR.CFStmtId,
    steps: std.ArrayList(ArcPlanStep) = .empty,
    step_count: u32 = 0,
    terminal: ArcPlanTerminal = .none,
    previous_terminal: ArcPlanTerminal = .none,
};

const ArcPlanMetadata = struct {
    /// Incremented whenever a dependency schedules a replacement visit.
    /// Older queued visits are ignored before they can overwrite the slot.
    version: u32 = 0,
    scheduled: bool = false,
    terminal_state: ?OwnedSet = null,
    stop_switch_index: u32 = no_plan,
    latent_stop_switch_index: u32 = no_plan,
    latent_stop_state: ?OwnedSet = null,
    control_switch_index: u32 = no_plan,
    arrival_join_index: u32 = no_plan,
    jump_join_index: u32 = no_plan,
};

const ArcPlans = struct {
    plans: std.ArrayList(ArcPlan) = .empty,
    metadata: std.ArrayList(ArcPlanMetadata) = .empty,
    root: u32 = no_plan,
};

const Inserter = struct {
    store: *LirStore,
    layouts: *const layout_mod.Store,
    options: InsertOptions,
    local_contains_refcounted: []const bool = &.{},
    boxy_rc_descs: []const ?LIR.BoxyDescRef = &.{},
    solution: *const arc_solve.Solution = undefined,
    /// Field takes solved against the ownership-neutral bodies; consulted by
    /// statement id, so base and variant emissions share one solve.
    dismantles: *const arc_dismantle.Dismantles = undefined,
    /// Temporaries synthesized while dismantling containers in the proc
    /// currently being emitted; appended to its frame locals afterward.
    dismantle_temps: *std.ArrayList(LIR.LocalId) = undefined,
    /// Mode-specialized variant table (shared across the emission worklist).
    variants: *VariantTable = undefined,
    /// Parameter locals whose borrowed solved binding is overridden to owned
    /// for the variant currently being emitted.
    owned_binding_override: *OwnedSet = undefined,
    /// Parameter locals the current variant's demand vector seeds as born
    /// unique; consumed by `uniqueArgsMask` through `isLocalUniqueHere`.
    unique_param_override: *OwnedSet = undefined,
    /// Exact resource and liveness bit domain of the proc currently emitted.
    /// It is built directly from that proc's explicit `frame_locals` span.
    current_domain: ?*const ProcArcDomain = null,
    /// Ownership signature of the proc currently being materialized.
    current_sig: arc_sig.RcSig = arc_sig.RcSig.all_owned,
    /// Direct cache per compact loop identity; each entry is indexed by the
    /// immutable liveness graph's compact node number.
    loop_liveness_caches: *std.ArrayList(LoopLivenessCache) = undefined,
    /// Reusable dense original-statement id -> active liveness-node index.
    stmt_node_indices: []u32 = &.{},
    /// Immutable ownership-neutral liveness graph per source proc. Variants
    /// share their source graph and its keep-free fixed point.
    liveness_graphs: []?ReadBeforeRebindGraph = &.{},
    liveness_allocator: Allocator = undefined,
    /// Source proc whose immutable graph currently occupies the reusable
    /// dense statement-to-node table.
    active_liveness_source: ?LIR.LirProcSpecId = null,
    current_source_proc: LIR.LirProcSpecId = undefined,
    next_loop_keep_id: u32 = 1,
    // Set to the proc being rewritten before any diagnostic or helper reads it.
    current_proc: LIR.LirProcSpecId = undefined,
    current_proc_body: LIR.CFStmtId = undefined,
    current_rewrite_stmt: ?LIR.CFStmtId = null,
    join_bodies: []const arc_solve.JoinBody = &.{},
    /// Join metadata accumulated when each final join is materialized,
    /// avoiding a second discovery walk over the finished graph.
    final_joins: []?LIR.JoinPoint = &.{},
    /// Per-emission join summaries computed alongside ArcPlan decisions.
    join_summaries: []?*JoinSummary = &.{},
    /// Per-emission switch-continuation summaries from the same solve.
    switch_summaries: []?*SwitchSummary = &.{},
    /// Arena backing one emission's solver structures.
    solve_allocator: Allocator = undefined,
    /// Arena backing all non-output state for the current proc emission.
    emission_allocator: Allocator = undefined,
    arc_plans: *ArcPlans = undefined,
    /// Exact call-result refinements registered while solving this emission.
    /// Keys are ownership-neutral switch statement ids.
    restitution_switches: *std.AutoHashMapUnmanaged(LIR.CFStmtId, RestitutionSwitch) = undefined,
    materialized_plans: []?LIR.CFStmtId = &.{},
    materialized_joins: []?LIR.CFStmtId = &.{},
    /// Reused planning buffers. Their contents are ephemeral; every slice
    /// retained by an ArcPlan is copied into the proc arena first.
    death_scratch: *std.ArrayList(ReleaseDecision) = undefined,
    transfer_position_scratch: *std.ArrayList(u32) = undefined,
    retain_arg_scratch: *std.ArrayList(LIR.LocalId) = undefined,

    const CallArgOwnership = struct {
        retain_args: []const LIR.LocalId = &.{},
        /// Ownership vector the call site demands; differs from the callee's
        /// solved signature only when borrowed positions upgrade to moves.
        demanded: arc_sig.RcSig = arc_sig.RcSig.all_owned,
    };

    const MaterializeTask = union(enum) {
        path: MaterializePathTask,
        control: *MaterializeControlTask,
        join: *MaterializeJoinTask,
        stop: *MaterializeStopTask,
    };

    const MaterializePathTask = struct {
        plan_index: u32,
        result: *LIR.CFStmtId,
    };

    const MaterializeControlTask = struct {
        plan_index: u32,
        child_results: []LIR.CFStmtId,
        result: *LIR.CFStmtId,
    };

    const MaterializeJoinTask = struct {
        plan_index: u32,
        remainder: LIR.CFStmtId = undefined,
        body: LIR.CFStmtId = undefined,
        result: *LIR.CFStmtId,
    };

    const MaterializeStopTask = struct {
        plan_index: u32,
        target: LIR.CFStmtId = undefined,
        result: *LIR.CFStmtId,
    };

    fn materializeArcPlans(self: *Inserter) ResourceError!LIR.CFStmtId {
        if (self.arc_plans.root == no_plan) arcInvariant("ARC materialized plans without a root");
        const materialized = try self.emission_allocator.alloc(?LIR.CFStmtId, self.arc_plans.plans.items.len);
        @memset(materialized, null);
        self.materialized_plans = materialized;
        const joins = try self.emission_allocator.alloc(?LIR.CFStmtId, self.join_bodies.len);
        @memset(joins, null);
        self.materialized_joins = joins;

        var result: LIR.CFStmtId = undefined;
        var tasks = std.ArrayList(MaterializeTask).empty;
        try tasks.append(self.emission_allocator, .{ .path = .{
            .plan_index = self.arc_plans.root,
            .result = &result,
        } });
        while (tasks.pop()) |task| {
            switch (task) {
                .path => |path| try self.processMaterializePath(&tasks, path),
                .control => |control| try self.finishMaterializeControl(control),
                .join => |join| try self.finishMaterializeJoin(join),
                .stop => |stop| try self.finishMaterializeStop(stop),
            }
        }
        return result;
    }

    fn pushMaterializePath(self: *Inserter, tasks: *std.ArrayList(MaterializeTask), plan_index: u32, result: *LIR.CFStmtId) ResourceError!void {
        if (plan_index == no_plan) arcInvariant("ARC materializer received the no-plan sentinel");
        try tasks.append(self.emission_allocator, .{ .path = .{ .plan_index = plan_index, .result = result } });
    }

    fn completeMaterializedPath(self: *Inserter, plan_index: u32, tail: LIR.CFStmtId, result: *LIR.CFStmtId) ResourceError!void {
        const head = try self.materializeArcPlanSteps(plan_index, tail);
        if (plan_index >= self.materialized_plans.len) arcInvariant("ARC materialized path index exceeded its cache");
        if (self.materialized_plans[plan_index]) |existing| {
            if (existing != head) arcInvariant("ARC materialized one structured path inconsistently");
        } else {
            self.materialized_plans[plan_index] = head;
        }
        result.* = head;
    }

    fn processMaterializePath(self: *Inserter, tasks: *std.ArrayList(MaterializeTask), path: MaterializePathTask) ResourceError!void {
        if (path.plan_index >= self.materialized_plans.len) arcInvariant("ARC materializer referenced an unknown plan");
        if (self.materialized_plans[path.plan_index]) |cached| {
            path.result.* = cached;
            return;
        }
        if (!self.planMetadata(path.plan_index).scheduled) arcInvariant("ARC materializer reached an unscheduled plan");
        const plan = self.arcPlan(path.plan_index);
        switch (plan.terminal) {
            .none => arcInvariant("ARC materializer reached an incomplete plan"),
            .terminal => |terminal| {
                var tail = terminal.stmt;
                tail = try self.materializeTerminalReleases(terminal.releases.items, tail);
                if (terminal.retain_value) |value| tail = try self.retainLocalIfRc(value, tail);
                try self.completeMaterializedPath(path.plan_index, tail, path.result);
            },
            .jump => |jump| {
                const source = self.store.getCFStmt(jump.stmt).jump;
                var tail = try self.addCFStmtAtSource(jump.stmt, .{ .jump = .{ .target = source.target } });
                tail = try self.materializeTerminalReleases(jump.releases.items, tail);
                try self.completeMaterializedPath(path.plan_index, tail, path.result);
            },
            .stop => |stop| {
                if (self.materialized_plans[stop.target_plan]) |target| {
                    const tail = try self.materializeTerminalReleases(stop.releases.items, target);
                    try self.completeMaterializedPath(path.plan_index, tail, path.result);
                    return;
                }
                const state = try self.emission_allocator.create(MaterializeStopTask);
                state.* = .{ .plan_index = path.plan_index, .result = path.result };
                try tasks.append(self.emission_allocator, .{ .stop = state });
                try self.pushMaterializePath(tasks, stop.target_plan, &state.target);
            },
            .join => |join| {
                if (join.join_index >= self.materialized_joins.len) arcInvariant("ARC materializer join index exceeded its cache");
                if (self.materialized_joins[join.join_index]) |cached_join| {
                    const tail = try self.materializeTerminalReleases(join.releases.items, cached_join);
                    try self.completeMaterializedPath(path.plan_index, tail, path.result);
                    return;
                }
                const state = try self.emission_allocator.create(MaterializeJoinTask);
                state.* = .{ .plan_index = path.plan_index, .result = path.result };
                try tasks.append(self.emission_allocator, .{ .join = state });
                if (join.body_reachable) try self.pushMaterializePath(tasks, join.body_plan, &state.body);
                try self.pushMaterializePath(tasks, join.remainder_plan, &state.remainder);
            },
            .switch_stmt => |switch_plan| {
                const source = self.store.getCFStmt(switch_plan.stmt).switch_stmt;
                const branch_count = switch_plan.branch_plans.len;
                const child_count = branch_count + 1 + @intFromBool(switch_plan.continuation_reachable);
                const state = try self.emission_allocator.create(MaterializeControlTask);
                state.* = .{
                    .plan_index = path.plan_index,
                    .child_results = try self.emission_allocator.alloc(LIR.CFStmtId, child_count),
                    .result = path.result,
                };
                try tasks.append(self.emission_allocator, .{ .control = state });
                for (switch_plan.branch_plans, 0..) |child, index| try self.pushMaterializePath(tasks, child, &state.child_results[index]);
                try self.pushMaterializePath(tasks, switch_plan.default_plan, &state.child_results[branch_count]);
                if (switch_plan.continuation_reachable) {
                    if (source.continuation == null) arcInvariant("ARC continuation plan belonged to a switch without a continuation");
                    try self.pushMaterializePath(tasks, switch_plan.continuation_plan, &state.child_results[branch_count + 1]);
                }
            },
            .initialized_payload_switch => |switch_plan| {
                const state = try self.emission_allocator.create(MaterializeControlTask);
                state.* = .{
                    .plan_index = path.plan_index,
                    .child_results = try self.emission_allocator.alloc(LIR.CFStmtId, 2),
                    .result = path.result,
                };
                try tasks.append(self.emission_allocator, .{ .control = state });
                try self.pushMaterializePath(tasks, switch_plan.uninitialized_plan, &state.child_results[1]);
                try self.pushMaterializePath(tasks, switch_plan.initialized_plan, &state.child_results[0]);
            },
            .str_match => |str_plan| {
                const state = try self.emission_allocator.create(MaterializeControlTask);
                state.* = .{
                    .plan_index = path.plan_index,
                    .child_results = try self.emission_allocator.alloc(LIR.CFStmtId, 2),
                    .result = path.result,
                };
                try tasks.append(self.emission_allocator, .{ .control = state });
                try self.pushMaterializePath(tasks, str_plan.miss_plan, &state.child_results[1]);
                try self.pushMaterializePath(tasks, str_plan.match_plan, &state.child_results[0]);
            },
            .boxy_tag_match => |tag_plan| {
                const state = try self.emission_allocator.create(MaterializeControlTask);
                state.* = .{
                    .plan_index = path.plan_index,
                    .child_results = try self.emission_allocator.alloc(LIR.CFStmtId, 2),
                    .result = path.result,
                };
                try tasks.append(self.emission_allocator, .{ .control = state });
                try self.pushMaterializePath(tasks, tag_plan.miss_plan, &state.child_results[1]);
                try self.pushMaterializePath(tasks, tag_plan.match_plan, &state.child_results[0]);
            },
            .str_match_set => |str_plan| {
                const state = try self.emission_allocator.create(MaterializeControlTask);
                state.* = .{
                    .plan_index = path.plan_index,
                    .child_results = try self.emission_allocator.alloc(LIR.CFStmtId, str_plan.match_plans.len + 1),
                    .result = path.result,
                };
                try tasks.append(self.emission_allocator, .{ .control = state });
                try self.pushMaterializePath(tasks, str_plan.miss_plan, &state.child_results[str_plan.match_plans.len]);
                for (str_plan.match_plans, 0..) |child, index| try self.pushMaterializePath(tasks, child, &state.child_results[index]);
            },
        }
    }

    fn finishMaterializeStop(self: *Inserter, state: *MaterializeStopTask) ResourceError!void {
        const terminal = self.arcPlan(state.plan_index).terminal.stop;
        const tail = try self.materializeTerminalReleases(terminal.releases.items, state.target);
        try self.completeMaterializedPath(state.plan_index, tail, state.result);
    }

    fn finishMaterializeJoin(self: *Inserter, state: *MaterializeJoinTask) ResourceError!void {
        const terminal = self.arcPlan(state.plan_index).terminal.join;
        const source = self.store.getCFStmt(terminal.stmt).join;
        var tail: LIR.CFStmtId = undefined;
        if (terminal.body_reachable) {
            tail = try self.addCFStmtAtSource(terminal.stmt, .{ .join = .{
                .id = source.id,
                .params = source.params,
                .maybe_uninitialized_params = source.maybe_uninitialized_params,
                .maybe_uninitialized_conditions = source.maybe_uninitialized_conditions,
                .maybe_uninitialized_condition_masks = source.maybe_uninitialized_condition_masks,
                .body = state.body,
                .remainder = state.remainder,
            } });
            try self.recordFinalJoin(terminal.join_index, .{
                .id = source.id,
                .params = source.params,
                .body = state.body,
            });
            self.materialized_joins[terminal.join_index] = tail;
        } else {
            tail = state.remainder;
        }
        tail = try self.materializeTerminalReleases(terminal.releases.items, tail);
        try self.completeMaterializedPath(state.plan_index, tail, state.result);
    }

    fn finishMaterializeControl(self: *Inserter, state: *MaterializeControlTask) ResourceError!void {
        const plan = self.arcPlan(state.plan_index);
        var tail: LIR.CFStmtId = undefined;
        switch (plan.terminal) {
            .switch_stmt => |switch_plan| {
                const source = self.store.getCFStmt(switch_plan.stmt).switch_stmt;
                const source_branches = self.store.getCFSwitchBranches(source.branches);
                if (source_branches.len != switch_plan.branch_plans.len) arcInvariant("ARC materializer switch branch count changed");
                const branches = try self.emission_allocator.alloc(LIR.CFSwitchBranch, source_branches.len);
                for (0..source_branches.len) |index| {
                    const source_branch = GuardedList.at(source_branches, index);
                    branches[index] = .{ .value = source_branch.value, .body = state.child_results[index] };
                }
                const continuation = if (switch_plan.continuation_reachable)
                    state.child_results[source_branches.len + 1]
                else
                    source.continuation;
                tail = try self.addCFStmtAtSource(switch_plan.stmt, .{ .switch_stmt = .{
                    .cond = source.cond,
                    .branches = try self.store.addCFSwitchBranches(branches),
                    .default_branch = state.child_results[source_branches.len],
                    .default_is_cold = source.default_is_cold,
                    .continuation = continuation,
                } });
            },
            .initialized_payload_switch => |switch_plan| {
                const source = self.store.getCFStmt(switch_plan.stmt).switch_initialized_payload;
                tail = try self.addCFStmtAtSource(switch_plan.stmt, .{ .switch_initialized_payload = .{
                    .cond = source.cond,
                    .cond_mask = source.cond_mask,
                    .payload = source.payload,
                    .uninitialized_is_cold = source.uninitialized_is_cold,
                    .initialized_branch = state.child_results[0],
                    .uninitialized_branch = state.child_results[1],
                } });
            },
            .str_match => |str_plan| {
                const source = self.store.getCFStmt(str_plan.stmt).str_match;
                const on_match = try self.retainLocalIfRcCount(source.source, str_plan.capture_retain_count, state.child_results[0]);
                tail = try self.addCFStmtAtSource(str_plan.stmt, .{ .str_match = .{
                    .source = source.source,
                    .prefix = source.prefix,
                    .steps = source.steps,
                    .end = source.end,
                    .on_match = on_match,
                    .on_miss = state.child_results[1],
                } });
            },
            .boxy_tag_match => |tag_plan| {
                const source = self.store.getCFStmt(tag_plan.stmt).boxy_tag_match;
                tail = try self.addCFStmtAtSource(tag_plan.stmt, .{ .boxy_tag_match = .{
                    .source = source.source,
                    .source_desc = source.source_desc,
                    .tag_name = source.tag_name,
                    .on_match = state.child_results[0],
                    .on_miss = state.child_results[1],
                } });
            },
            .str_match_set => |str_plan| {
                const source = self.store.getCFStmt(str_plan.stmt).str_match_set;
                const source_arms = self.store.getStrMatchArms(source.arms);
                if (source_arms.len != str_plan.match_plans.len) arcInvariant("ARC materializer string arm count changed");
                const arms = try self.emission_allocator.alloc(LIR.StrMatchArm, source_arms.len);
                for (0..source_arms.len) |index| {
                    const source_arm = GuardedList.at(source_arms, index);
                    arms[index] = .{
                        .prefix = source_arm.prefix,
                        .steps = source_arm.steps,
                        .end = source_arm.end,
                        .on_match = try self.retainLocalIfRcCount(source.source, str_plan.capture_retain_counts[index], state.child_results[index]),
                    };
                }
                tail = try self.addCFStmtAtSource(str_plan.stmt, .{ .str_match_set = .{
                    .source = source.source,
                    .arms = try self.store.addStrMatchArms(arms),
                    .on_miss = state.child_results[source_arms.len],
                } });
            },
            .none, .stop, .join, .jump, .terminal => arcInvariant("ARC control materializer received a non-control plan"),
        }
        try self.completeMaterializedPath(state.plan_index, tail, state.result);
    }

    fn materializeReleaseDecision(self: *Inserter, release: ReleaseDecision, next: LIR.CFStmtId) ResourceError!LIR.CFStmtId {
        return switch (release) {
            .initialized => |local| try self.releaseLocalIfRc(local, next),
            .residual => |residual| try self.dismantleContainer(
                residual.value,
                self.dismantleFor(residual.value) orelse arcInvariant("ARC residual release named a container without a committed field domain"),
                residual.field_mask,
                next,
            ),
            .maybe_initialized => |conditional| try self.releaseMaybeInitializedLocal(
                conditional.condition,
                conditional.condition_mask,
                conditional.value,
                next,
            ),
        };
    }

    fn materializeTerminalReleases(self: *Inserter, releases: []const ReleaseDecision, tail: LIR.CFStmtId) ResourceError!LIR.CFStmtId {
        var next = tail;
        for (releases) |release| next = try self.materializeReleaseDecision(release, next);
        return next;
    }

    fn addCFStmtAtSource(self: *Inserter, source_stmt: LIR.CFStmtId, stmt: LIR.CFStmt) ResourceError!LIR.CFStmtId {
        const saved_loc = self.store.current_loc;
        defer self.store.current_loc = saved_loc;
        const saved_region = self.store.current_region;
        defer self.store.current_region = saved_region;
        const saved_inline_scope = self.store.current_inline_scope;
        defer self.store.current_inline_scope = saved_inline_scope;
        self.store.current_loc = self.store.stmtLoc(source_stmt);
        self.store.current_region = self.store.stmtRegion(source_stmt);
        self.store.current_inline_scope = self.store.stmtInlineScope(source_stmt);
        return try self.store.addCFStmt(stmt);
    }

    fn materializeArcPlanSteps(self: *Inserter, plan_index: u32, tail: LIR.CFStmtId) ResourceError!LIR.CFStmtId {
        const plan = self.arcPlan(plan_index);
        var next = tail;
        var index: usize = plan.step_count;
        while (index > 0) {
            index -= 1;
            next = try self.materializeArcPlanStep(&plan.steps.items[index], next);
        }
        return next;
    }

    fn materializeResidualShellAbsentFields(
        self: *Inserter,
        assign: @FieldType(LIR.CFStmt, "assign_ref"),
        absent_mask: u64,
        all_rc_fields_absent: bool,
    ) ResourceError!LIR.U32Span {
        if (absent_mask == 0 and !all_rc_fields_absent) return .empty();
        const source = switch (assign.op) {
            .local => |local| local,
            .discriminant,
            .field,
            .tag_payload,
            .tag_payload_struct,
            .list_reinterpret,
            .nominal,
            => arcInvariant("ARC attached representation-shell fields to a non-local alias"),
        };
        if (self.store.getLocal(source).layout_idx != self.store.getLocal(assign.target).layout_idx) {
            arcInvariant("ARC attached representation-shell fields to a layout-changing alias");
        }
        if (all_rc_fields_absent) {
            if (absent_mask != 0) arcInvariant("ARC representation-shell alias mixed full and residual absent-field domains");
            const source_layout = self.layouts.getLayout(self.store.getLocal(source).layout_idx);
            if (source_layout.tag != .struct_) arcInvariant("ARC full representation shell did not have struct layout");
            const info = self.layouts.getStructInfo(source_layout);
            var count: usize = 0;
            for (0..info.fields.len) |index| {
                const field = info.fields.get(@intCast(index));
                if (!self.layouts.layoutContainsRefcounted(self.layouts.getLayout(field.layout))) continue;
                count += 1;
            }
            const semantic_fields = try self.emission_allocator.alloc(u32, count);
            var next_field: usize = 0;
            for (0..info.fields.len) |index| {
                const field = info.fields.get(@intCast(index));
                if (!self.layouts.layoutContainsRefcounted(self.layouts.getLayout(field.layout))) continue;
                semantic_fields[next_field] = field.index;
                next_field += 1;
            }
            return try self.store.addU32Span(semantic_fields);
        }

        const container = self.dismantleFor(self.unitOf(source)) orelse
            arcInvariant("ARC residual representation-shell alias has no committed field domain");
        if ((absent_mask & ~container.full_mask) != 0) {
            arcInvariant("ARC representation-shell alias exceeded its committed field domain");
        }

        var semantic_fields: [64]u32 = undefined;
        var count: usize = 0;
        for (container.fields, 0..) |field, index| {
            const field_mask = @as(u64, 1) << @intCast(index);
            if ((absent_mask & field_mask) == 0) continue;
            semantic_fields[count] = field.field_idx;
            count += 1;
        }
        return try self.store.addU32Span(semantic_fields[0..count]);
    }

    fn materializeArcPlanStep(self: *Inserter, step: *const ArcPlanStep, tail: LIR.CFStmtId) ResourceError!LIR.CFStmtId {
        const stmt = self.store.getCFStmt(step.stmt);
        const saved_rewrite_stmt = self.current_rewrite_stmt;
        defer self.current_rewrite_stmt = saved_rewrite_stmt;
        self.current_rewrite_stmt = step.stmt;
        const saved_loc = self.store.current_loc;
        defer self.store.current_loc = saved_loc;
        const saved_region = self.store.current_region;
        defer self.store.current_region = saved_region;
        const saved_inline_scope = self.store.current_inline_scope;
        defer self.store.current_inline_scope = saved_inline_scope;
        self.store.current_loc = self.store.stmtLoc(step.stmt);
        self.store.current_region = self.store.stmtRegion(step.stmt);
        self.store.current_inline_scope = self.store.stmtInlineScope(step.stmt);

        var next = tail;
        var release_index = step.post_release.items.len;
        while (release_index > 0) {
            release_index -= 1;
            next = try self.materializeReleaseDecision(step.post_release.items[release_index], next);
        }

        var cloned: LIR.CFStmtId = switch (stmt) {
            .assign_ref => |assign| blk: {
                if (step.retain_assign_ref_target) next = try self.retainLocalIfRc(assign.target, next);
                break :blk try self.store.addCFStmt(.{ .assign_ref = .{
                    .target = assign.target,
                    .op = assign.op,
                    .residual_shell_absent_fields = try self.materializeResidualShellAbsentFields(
                        assign,
                        step.residual_shell_absent_mask,
                        step.residual_shell_all_rc_fields_absent,
                    ),
                    .next = next,
                } });
            },
            .assign_literal => |assign| try self.store.addCFStmt(.{ .assign_literal = .{
                .target = assign.target,
                .value = assign.value,
                .next = next,
            } }),
            .init_uninitialized => |uninit| try self.store.addCFStmt(.{ .init_uninitialized = .{
                .target = uninit.target,
                .next = next,
            } }),
            .assign_call => |assign| blk: {
                if (step.retain_call_result) next = try self.retainLocalIfRc(assign.target, next);
                break :blk try self.store.addCFStmt(.{ .assign_call = .{
                    .target = assign.target,
                    .proc = step.call_target_override orelse assign.proc,
                    .args = assign.args,
                    .result_desc = assign.result_desc,
                    .out_desc = assign.out_desc,
                    .is_cold = assign.is_cold,
                    .next = next,
                } });
            },
            .assign_call_erased => |assign| blk: {
                if (!assign.reuse_closure) next = try self.releaseLocalIfRc(assign.closure, next);
                break :blk try self.store.addCFStmt(.{ .assign_call_erased = .{
                    .target = assign.target,
                    .closure = assign.closure,
                    .args = assign.args,
                    .arg_layouts = assign.arg_layouts,
                    .arg_descs = assign.arg_descs,
                    .arg_desc_keys = assign.arg_desc_keys,
                    .result_desc = assign.result_desc,
                    .out_desc = assign.out_desc,
                    .arg_plan = assign.arg_plan,
                    .reuse_closure = assign.reuse_closure,
                    .reuse_source = assign.reuse_source,
                    .next = next,
                } });
            },
            .assign_packed_erased_fn => |assign| blk: {
                if (assign.capture) |capture| {
                    if (!step.transfer_single) next = try self.retainLocalIfRc(capture, next);
                }
                break :blk try self.store.addCFStmt(.{ .assign_packed_erased_fn = .{
                    .target = assign.target,
                    .proc = assign.proc,
                    .capture = assign.capture,
                    .capture_layout = assign.capture_layout,
                    .on_drop = assign.on_drop,
                    .result_desc = assign.result_desc,
                    .reuse = assign.reuse,
                    .reuse_unique = step.reuse_unique,
                    .next = next,
                } });
            },
            .assign_boxy_desc_ref => |assign| try self.store.addCFStmt(.{ .assign_boxy_desc_ref = .{
                .target = assign.target,
                .desc = assign.desc,
                .nested_index = assign.nested_index,
                .box_payload_layout = assign.box_payload_layout,
                .tag_payload = assign.tag_payload,
                .tag_ext = assign.tag_ext,
                .tag_residual_for = assign.tag_residual_for,
                .captures = assign.captures,
                .next = next,
            } }),
            .assign_boxy_dict_ref => |assign| try self.store.addCFStmt(.{ .assign_boxy_dict_ref = .{
                .target = assign.target,
                .dict = assign.dict,
                .next = next,
            } }),
            .assign_boxy_box => |assign| try self.store.addCFStmt(.{ .assign_boxy_box = .{
                .target = assign.target,
                .payload = assign.payload,
                .payload_layout = assign.payload_layout,
                .source_desc = assign.source_desc,
                .payload_desc = assign.payload_desc,
                .payload_mode = assign.payload_mode,
                .next = next,
            } }),
            .assign_boxy_reuse_box => |assign| try self.store.addCFStmt(.{ .assign_boxy_reuse_box = .{
                .target = assign.target,
                .source = assign.source,
                .desc = assign.desc,
                .next = next,
            } }),
            .assign_boxy_unbox => |assign| blk: {
                if (step.retain_assign_ref_target) next = try self.retainLocalIfRc(assign.target, next);
                break :blk try self.store.addCFStmt(.{ .assign_boxy_unbox = .{
                    .target = assign.target,
                    .source = assign.source,
                    .source_desc = assign.source_desc,
                    .target_desc = assign.target_desc,
                    .target_layout = assign.target_layout,
                    .source_mode = assign.source_mode,
                    .next = next,
                } });
            },
            .assign_boxy_adapt => |assign| try self.store.addCFStmt(.{ .assign_boxy_adapt = .{
                .target = assign.target,
                .source = assign.source,
                .adapter = assign.adapter,
                .source_desc = assign.source_desc,
                .target_desc = assign.target_desc,
                .source_mode = assign.source_mode,
                .next = next,
            } }),
            .assign_boxy_inspect => |assign| try self.store.addCFStmt(.{ .assign_boxy_inspect = .{
                .target = assign.target,
                .source = assign.source,
                .source_desc = assign.source_desc,
                .source_mode = assign.source_mode,
                .next = next,
            } }),
            .assign_boxy_eq => |assign| blk: {
                if (assign.source_mode == .move) {
                    next = try self.retainLocalIfRc(assign.rhs, next);
                    next = try self.retainLocalIfRc(assign.lhs, next);
                }
                break :blk try self.store.addCFStmt(.{ .assign_boxy_eq = .{
                    .target = assign.target,
                    .lhs = assign.lhs,
                    .rhs = assign.rhs,
                    .source_desc = assign.source_desc,
                    .source_mode = assign.source_mode,
                    .next = next,
                } });
            },
            .assign_boxy_tag => |assign| try self.store.addCFStmt(.{ .assign_boxy_tag = .{
                .target = assign.target,
                .target_desc = assign.target_desc,
                .tag_name = assign.tag_name,
                .payload = assign.payload,
                .payload_layout = assign.payload_layout,
                .payload_desc = assign.payload_desc,
                .payload_mode = assign.payload_mode,
                .next = next,
            } }),
            .assign_boxy_tag_payload => |assign| blk: {
                if (step.retain_assign_ref_target) next = try self.retainLocalIfRc(assign.target, next);
                break :blk try self.store.addCFStmt(.{ .assign_boxy_tag_payload = .{
                    .target = assign.target,
                    .target_desc = assign.target_desc,
                    .source = assign.source,
                    .source_desc = assign.source_desc,
                    .tag_name = assign.tag_name,
                    .payload_index = assign.payload_index,
                    .source_mode = assign.source_mode,
                    .next = next,
                } });
            },
            .assign_call_dict => |assign| blk: {
                next = try self.retainSpanExcept(assign.args, step.transfer_mask, next);
                break :blk try self.store.addCFStmt(.{ .assign_call_dict = .{
                    .target = assign.target,
                    .dict = assign.dict,
                    .method = assign.method,
                    .method_slot = assign.method_slot,
                    .args = assign.args,
                    .arg_descs = assign.arg_descs,
                    .hidden_args = assign.hidden_args,
                    .result_desc = assign.result_desc,
                    .is_cold = assign.is_cold,
                    .next = next,
                } });
            },
            .assign_low_level => |assign| blk: {
                var op = if (step.low_level_selection) |selection| selection.op else assign.op;
                var rc_effect = if (step.low_level_selection) |selection| selection.rc_effect else assign.rc_effect;
                if (rc_effect.retain_args != 0) {
                    next = try self.retainMaskedArgs(assign.args, rc_effect.retain_args & ~step.transfer_mask, next);
                }
                if (rc_effect.retain_result and !step.skip_result_retain) next = try self.retainLocalIfRc(assign.target, next);
                if (op == .box_unbox) {
                    // Normalize ownership transfer into operations whose RC
                    // meaning is already explicit in LIR. The payload retain
                    // and outer release cancel on a unique box; on a shared
                    // box they create the result's independent child units.
                    const args = self.store.getLocalSpan(assign.args);
                    const boxed = GuardedList.at(args, 0);
                    next = try self.releaseLocalIfRc(boxed, next);
                    next = try self.retainLocalIfRc(assign.target, next);
                    op = .box_unbox_borrowed;
                    rc_effect = op.rcEffect();
                }
                break :blk try self.store.addCFStmt(.{ .assign_low_level = .{
                    .target = assign.target,
                    .op = op,
                    .rc_effect = rc_effect,
                    .unique_args = step.unique_args,
                    .args = assign.args,
                    .interchangeable = assign.interchangeable,
                    .next = next,
                } });
            },
            .assign_list => |assign| blk: {
                next = try self.retainSpanExceptPositions(assign.elems, step.transfer_positions.items, next);
                break :blk try self.store.addCFStmt(.{ .assign_list = .{
                    .target = assign.target,
                    .elems = assign.elems,
                    .next = next,
                } });
            },
            .assign_struct => |assign| blk: {
                next = try self.retainSpanExceptPositions(assign.fields, step.transfer_positions.items, next);
                break :blk try self.store.addCFStmt(.{ .assign_struct = .{
                    .target = assign.target,
                    .fields = assign.fields,
                    .contents_desc = assign.contents_desc,
                    .next = next,
                } });
            },
            .assign_tag => |assign| blk: {
                if (assign.payload) |payload| {
                    if (!step.transfer_single) next = try self.retainLocalIfRc(payload, next);
                }
                break :blk try self.store.addCFStmt(.{ .assign_tag = .{
                    .target = assign.target,
                    .target_desc = assign.target_desc,
                    .variant_index = assign.variant_index,
                    .discriminant = assign.discriminant,
                    .payload = assign.payload,
                    .next = next,
                } });
            },
            .store_struct => |assign| blk: {
                next = try self.retainSpanExceptPositions(assign.fields, step.transfer_positions.items, next);
                break :blk try self.store.addCFStmt(.{ .store_struct = .{
                    .dest = assign.dest,
                    .struct_layout = assign.struct_layout,
                    .fields = assign.fields,
                    .next = next,
                } });
            },
            .store_tag => |assign| blk: {
                if (assign.payload) |payload| {
                    if (!step.transfer_single) next = try self.retainLocalIfRc(payload, next);
                }
                break :blk try self.store.addCFStmt(.{ .store_tag = .{
                    .dest = assign.dest,
                    .tag_layout = assign.tag_layout,
                    .variant_index = assign.variant_index,
                    .discriminant = assign.discriminant,
                    .payload = assign.payload,
                    .next = next,
                } });
            },
            .set_local => |assign| blk: {
                if (assign.target != assign.value and step.retain_set_target) next = try self.retainLocalIfRc(assign.target, next);
                break :blk try self.store.addCFStmt(.{ .set_local = .{
                    .target = assign.target,
                    .value = assign.value,
                    .mode = assign.mode,
                    .next = next,
                } });
            },
            .debug => |debug_stmt| try self.store.addCFStmt(.{ .debug = .{
                .message = debug_stmt.message,
                .next = next,
            } }),
            .expect => |expect_stmt| try self.store.addCFStmt(.{ .expect = .{
                .condition = expect_stmt.condition,
                .next = next,
            } }),
            .decref_if_initialized => |rc| try self.store.addCFStmt(.{ .decref_if_initialized = .{
                .cond = rc.cond,
                .cond_mask = rc.cond_mask,
                .value = rc.value,
                .rc = rc.rc,
                .atomicity = self.rcAtomicity(rc.value),
                .next = next,
            } }),
            .comptime_branch_taken => |marker| try self.store.addCFStmt(.{ .comptime_branch_taken = .{
                .site = marker.site,
                .branch_index = marker.branch_index,
                .next = next,
            } }),
            .expect_err,
            .runtime_error,
            .comptime_exhaustiveness_failed,
            .incref,
            .decref,
            .free,
            .switch_stmt,
            .switch_initialized_payload,
            .str_match,
            .str_match_set,
            .boxy_tag_match,
            .loop_continue,
            .loop_break,
            .join,
            .jump,
            .ret,
            .crash,
            => arcInvariant("ARC solved linear plan contained a non-linear statement"),
        };

        if (step.pre_release) |release| cloned = try self.materializeReleaseDecision(release, cloned);
        var pre_release_index = step.pre_release_extra.items.len;
        while (pre_release_index > 0) {
            pre_release_index -= 1;
            cloned = try self.materializeReleaseDecision(step.pre_release_extra.items[pre_release_index], cloned);
        }
        if (step.preserve_consumed_args != 0) {
            const assign = stmt.assign_low_level;
            cloned = try self.retainMaskedArgs(assign.args, step.preserve_consumed_args, cloned);
        }
        if (step.pre_retain.items.len != 0) cloned = try self.retainArgs(step.pre_retain.items, cloned);
        return cloned;
    }

    /// Runs the join-summary solver for one proc emission: a structured
    /// abstract interpretation of the ownership-neutral body that fills the
    /// per-emission join and switch summary tables. See the solver comment
    /// block above `JoinSummary` for the domain, equations, monotonicity,
    /// and termination argument.
    fn solveProcSummaries(self: *Inserter, body: LIR.CFStmtId, entry_owned: *const OwnedSet) ResourceError!void {
        var tasks = std.ArrayList(SolveTask).empty;
        defer tasks.deinit(self.solve_allocator);
        const root_plan = try self.newArcPlan(body);
        self.arc_plans.root = root_plan;
        try self.pushSolveSegment(&tasks, body, entry_owned, .{}, root_plan);
        while (true) {
            while (tasks.pop()) |task| {
                if (builtin.mode == .Debug) solver_iterations += 1;
                switch (task) {
                    .segment => |segment| try self.processSolveSegment(&tasks, segment),
                    .join_process => |join_index| try self.processSolveJoin(&tasks, join_index),
                    .body_walk => |join_index| try self.processSolveBodyWalk(&tasks, join_index),
                    .switch_resume => |switch_index| try self.processSolveSwitchResume(&tasks, switch_index),
                }
            }
            // Jump reachability is structural, so once the tasks drain, a
            // join with no contributions has an unreachable body for good.
            // Its keep then settles to the params-only set the emission uses
            // for unreachable bodies; the shrink can ripple through
            // loop-keyed liveness, so drain again until nothing adjusts.
            var adjusted = false;
            for (self.join_summaries) |maybe_summary| {
                const summary = maybe_summary orelse continue;
                if (summary.body_reachable) continue;
                var params_only = try OwnedSet.init(self.solve_allocator, self.domain());
                self.placeSolveJoinParamsInto(summary, &params_only);
                if (params_only.eql(&summary.body_keep)) continue;
                assignOwnedSet(&summary.body_keep, &params_only);
                try self.refreshJumpPlans(summary);
                const purged = try self.purgeLoopKeepLiveness(summary.loop_keep_id);
                const entry_changed = try self.recomputeSolveEntryKeep(summary);
                try self.refreshJoinArrivalPlans(summary);
                if (purged or entry_changed) {
                    try self.scheduleSolveJoinProcess(&tasks, summary);
                    adjusted = true;
                }
            }
            if (!adjusted) break;
        }
        try self.finalizeArcPlans();
    }

    fn finalizeArcPlans(self: *Inserter) ResourceError!void {
        for (self.arc_plans.plans.items, 0..) |*plan, plan_index| {
            if (!self.arc_plans.metadata.items[plan_index].scheduled) continue;
            if (plan.terminal == .none) arcInvariant("ARC fixed point left a scheduled structured plan without a terminal decision");
            if (plan.step_count > plan.steps.items.len) arcInvariant("ARC fixed point left an invalid plan step count");
            for (plan.steps.items[0..plan.step_count]) |*step| {
                if (step.call_callee) |callee| {
                    step.call_target_override = try self.variantForCall(callee, step.call_demanded);
                }
            }
        }
    }

    fn processSolveSegment(self: *Inserter, tasks: *std.ArrayList(SolveTask), segment: *SolveSegment) ResourceError!void {
        if (!self.beginArcPlanUpdate(segment)) return;
        while (true) {
            var stop_entry = segment.ctx.stops;
            while (stop_entry) |entry| : (stop_entry = entry.parent) {
                if (entry.stmt != segment.cursor) continue;
                if (!entry.contributes and !entry.summary.reached) {
                    try self.registerLatentSwitchStopPlan(entry.summary, segment.plan_index, &segment.owned);
                    continue;
                }
                try self.updateSwitchStopPlan(entry.summary, segment.plan_index, &segment.owned);
                if (entry.contributes) try self.contributeSolveSwitchExit(tasks, entry.summary, &segment.owned);
                return;
            }

            const stmt = self.store.getCFStmt(segment.cursor);
            switch (stmt) {
                .assign_ref => |assign| {
                    const step = try self.nextArcPlanStep(segment.plan_index, segment.cursor);
                    if (assign.op == .local and
                        self.store.getLocal(assign.op.local).layout_idx == self.store.getLocal(assign.target).layout_idx and
                        self.layouts.getLayout(self.store.getLocal(assign.op.local).layout_idx).tag == .struct_ and
                        self.localContainsRefcounted(assign.op.local))
                    {
                        const source_unit = self.unitOf(assign.op.local);
                        if (self.dismantleFor(source_unit)) |container| {
                            if (segment.owned.contains(source_unit)) {
                                step.residual_shell_absent_mask = container.full_mask & ~segment.owned.residualMask(source_unit);
                            } else {
                                // A dismantlable unit that is no longer in
                                // the path state leaves only its inline
                                // representation. Every committed RC field
                                // is absent from that shell.
                                step.residual_shell_absent_mask = container.full_mask;
                            }
                        } else if (!segment.owned.contains(source_unit) and !self.isBindingBorrowed(source_unit)) {
                            // Complete-root transfer or whole release leaves
                            // a representation-only struct even when no
                            // partial-field container domain was needed.
                            step.residual_shell_all_rc_fields_absent = true;
                        }
                    }
                    var transfer = AliasBindTransfer{ .retain_target = true, .release_old_target = false };
                    const complete_take_root = self.dismantles.completeTakeRoot(segment.cursor);
                    var complete_moved_root = false;
                    if (self.isBindingBorrowed(assign.target)) {
                        transfer.retain_target = false;
                    } else if (complete_take_root) |root| {
                        transfer = try self.transferForCompleteProjectionBind(
                            &segment.owned,
                            assign.target,
                            root,
                            assign.next,
                        );
                        complete_moved_root = !transfer.retain_target;
                    } else {
                        switch (assign.op) {
                            .local => |source| {
                                if (assign.target != source) {
                                    transfer = try self.transferForAliasBind(&segment.owned, assign.target, source, assign.next, segment.ctx.loop_keep);
                                } else {
                                    transfer.retain_target = false;
                                }
                            },
                            .discriminant,
                            .field,
                            .tag_payload,
                            .tag_payload_struct,
                            .list_reinterpret,
                            .nominal,
                            => transfer.release_old_target = self.transferForFreshBind(&segment.owned, assign.target),
                        }
                    }
                    // A partial field take consumes the container's stored
                    // field unit: the target still binds owned, but no
                    // retain is paid. Complete projections made their
                    // path-sensitive move decision above.
                    if (self.takeForEmission(segment.cursor)) |take| {
                        if (complete_moved_root) {
                            // The complete projection moved the root unit, so
                            // there is no residual shell to update.
                        } else {
                            const take_root = self.unitOf(take.root);
                            const field_available = segment.owned.contains(take_root) and
                                (segment.owned.residualMask(take_root) & take.field_mask) == take.field_mask;
                            if (complete_take_root == null or field_available) {
                                // When a later use keeps the aggregate root
                                // alive, an independently committed field take
                                // moves only this stored unit and leaves the
                                // exact residual shell behind.
                                transfer.retain_target = false;
                                segment.owned.takeResidualField(take_root, take.field_mask);
                            }
                        }
                    }
                    step.pre_release = if (transfer.release_old_target) self.releaseDecision(assign.target) else null;
                    step.retain_assign_ref_target = transfer.retain_target;
                    const singles = [_]LIR.LocalId{ refOpSource(assign.op), assign.target };
                    try self.finishArcPlanStepDeaths(step, &segment.owned, &singles, null, assign.next, segment.ctx.loop_keep);
                    segment.cursor = assign.next;
                },
                .init_uninitialized => |uninit| {
                    const step = try self.nextArcPlanStep(segment.plan_index, segment.cursor);
                    step.pre_release = if (self.transferForInit(&segment.owned, uninit.target)) self.releaseDecision(uninit.target) else null;
                    segment.cursor = uninit.next;
                },
                .assign_literal => |assign| {
                    const step = try self.nextArcPlanStep(segment.plan_index, segment.cursor);
                    step.pre_release = if (self.transferForFreshBind(&segment.owned, assign.target)) self.releaseDecision(assign.target) else null;
                    const singles = [_]LIR.LocalId{assign.target};
                    try self.finishArcPlanStepDeaths(step, &segment.owned, &singles, null, assign.next, segment.ctx.loop_keep);
                    segment.cursor = assign.next;
                },
                .assign_call => |assign| {
                    const step = try self.nextArcPlanStep(segment.plan_index, segment.cursor);
                    const unique_demand = self.variants.enabled and !self.solution.isPinnedProc(assign.proc);
                    const transfer = try self.transferForCall(&segment.owned, segment.cursor, assign.proc, self.solution.sigOf(assign.proc), unique_demand, assign.args, assign.next, assign.target, null, segment.ctx.loop_keep);
                    step.pre_release = if (transfer.release_old_target) self.releaseDecision(assign.target) else null;
                    try step.pre_retain.appendSlice(self.solve_allocator, transfer.args.retain_args);
                    step.retain_call_result = transfer.retain_call_result;
                    step.call_callee = assign.proc;
                    step.call_demanded = transfer.args.demanded;
                    self.death_scratch.clearRetainingCapacity();
                    try self.noteCallResultDeathIfUnused(&segment.owned, assign.target, transfer.args.demanded.ret_mode, assign.next, segment.ctx.loop_keep, self.death_scratch);
                    try self.postStmtDeaths(&segment.owned, &.{}, assign.args, assign.next, segment.ctx.loop_keep, self.death_scratch);
                    try self.copyDeathScratchToStep(step);
                    segment.cursor = assign.next;
                },
                .assign_call_erased => |assign| {
                    const step = try self.nextArcPlanStep(segment.plan_index, segment.cursor);
                    const transfer = try self.transferForCall(&segment.owned, null, null, arc_sig.RcSig.all_owned, false, assign.args, assign.next, assign.target, assign.closure, segment.ctx.loop_keep);
                    var preserve_reuse_source = false;
                    if (assign.reuse_closure) {
                        if (assign.closure == assign.target) arcInvariant("owned erased call cannot consume and rebind the same local");
                        const reuse_source = assign.reuse_source orelse arcInvariant("owned erased call lacked its explicit reuse ownership source");
                        preserve_reuse_source = try self.groupUsedInPath(assign.next, reuse_source, segment.ctx.loop_keep);
                        if (!preserve_reuse_source) _ = self.takeUnit(&segment.owned, reuse_source);
                    }
                    step.pre_release = if (transfer.release_old_target) self.releaseDecision(assign.target) else null;
                    try step.pre_retain.appendSlice(self.solve_allocator, transfer.args.retain_args);
                    if (!assign.reuse_closure) try step.pre_retain.append(self.solve_allocator, assign.closure);
                    if (preserve_reuse_source) try step.pre_retain.append(self.solve_allocator, assign.reuse_source.?);
                    self.death_scratch.clearRetainingCapacity();
                    try self.noteCallResultDeathIfUnused(&segment.owned, assign.target, .owned, assign.next, segment.ctx.loop_keep, self.death_scratch);
                    const singles = [_]LIR.LocalId{ assign.closure, assign.reuse_source orelse assign.closure };
                    try self.postStmtDeaths(&segment.owned, &singles, assign.args, assign.next, segment.ctx.loop_keep, self.death_scratch);
                    try self.copyDeathScratchToStep(step);
                    segment.cursor = assign.next;
                },
                .assign_packed_erased_fn => |assign| {
                    const step = try self.nextArcPlanStep(segment.plan_index, segment.cursor);
                    const transfer = try self.transferForPackedErased(&segment.owned, assign, segment.ctx.loop_keep);
                    step.pre_release = if (transfer.release_old_target) self.releaseDecision(assign.target) else null;
                    step.transfer_single = transfer.transfer_single;
                    step.reuse_unique = transfer.reuse_unique;
                    if (transfer.preserve_reuse) try step.pre_retain.append(self.solve_allocator, assign.reuse.?);
                    const singles = [_]LIR.LocalId{ assign.capture orelse assign.target, assign.reuse orelse assign.target, assign.target };
                    try self.finishArcPlanStepDeaths(step, &segment.owned, &singles, null, assign.next, segment.ctx.loop_keep);
                    segment.cursor = assign.next;
                },
                .assign_boxy_desc_ref => |assign| {
                    const step = try self.nextArcPlanStep(segment.plan_index, segment.cursor);
                    try self.planValuesInvalidatedByDescriptorUpdate(
                        assign.target,
                        &segment.owned,
                        assign.next,
                        segment.ctx.loop_keep,
                        &step.pre_release_extra,
                    );
                    step.pre_release = if (self.transferForFreshBind(&segment.owned, assign.target)) self.releaseDecision(assign.target) else null;
                    self.death_scratch.clearRetainingCapacity();
                    const desc_local = assign.desc.localOrNull() orelse assign.target;
                    const residual_local = if (assign.tag_residual_for) |desc| desc.localOrNull() orelse assign.target else assign.target;
                    const singles = [_]LIR.LocalId{ desc_local, residual_local, assign.target };
                    try self.postStmtDeaths(&segment.owned, &singles, assign.captures, assign.next, segment.ctx.loop_keep, self.death_scratch);
                    try self.copyDeathScratchToStep(step);
                    segment.cursor = assign.next;
                },
                .assign_boxy_dict_ref => |assign| {
                    const step = try self.nextArcPlanStep(segment.plan_index, segment.cursor);
                    step.pre_release = if (self.transferForFreshBind(&segment.owned, assign.target)) self.releaseDecision(assign.target) else null;
                    const dict_local = assign.dict.localOrNull() orelse assign.target;
                    const singles = [_]LIR.LocalId{ dict_local, assign.target };
                    try self.finishArcPlanStepDeaths(step, &segment.owned, &singles, null, assign.next, segment.ctx.loop_keep);
                    segment.cursor = assign.next;
                },
                .assign_boxy_box => |assign| {
                    const step = try self.nextArcPlanStep(segment.plan_index, segment.cursor);
                    const transfer = if (assign.payload_mode == .move)
                        try self.transferForSingle(&segment.owned, assign.payload, assign.target, assign.next, segment.ctx.loop_keep)
                    else
                        SingleTransfer{
                            .transfer_single = false,
                            .release_old_target = self.transferForFreshBind(&segment.owned, assign.target),
                        };
                    step.pre_release = if (transfer.release_old_target) self.releaseDecision(assign.target) else null;
                    step.transfer_single = transfer.transfer_single;
                    if (assign.payload_mode == .move and !transfer.transfer_single) {
                        try step.pre_retain.append(self.solve_allocator, assign.payload);
                    }
                    const singles = [_]LIR.LocalId{ assign.payload, assign.target };
                    try self.finishArcPlanStepDeaths(step, &segment.owned, &singles, null, assign.next, segment.ctx.loop_keep);
                    segment.cursor = assign.next;
                },
                .assign_boxy_reuse_box => |assign| {
                    const step = try self.nextArcPlanStep(segment.plan_index, segment.cursor);
                    const transfer = try self.transferForSingle(&segment.owned, assign.source, assign.target, assign.next, segment.ctx.loop_keep);
                    step.pre_release = if (transfer.release_old_target) self.releaseDecision(assign.target) else null;
                    step.transfer_single = transfer.transfer_single;
                    if (!transfer.transfer_single) try step.pre_retain.append(self.solve_allocator, assign.source);
                    const singles = [_]LIR.LocalId{ assign.source, assign.target };
                    try self.finishArcPlanStepDeaths(step, &segment.owned, &singles, null, assign.next, segment.ctx.loop_keep);
                    segment.cursor = assign.next;
                },
                .assign_boxy_unbox => |assign| {
                    const step = try self.nextArcPlanStep(segment.plan_index, segment.cursor);
                    const transfer = if (assign.source_mode == .move)
                        try self.transferForSingle(&segment.owned, assign.source, assign.target, assign.next, segment.ctx.loop_keep)
                    else
                        SingleTransfer{
                            .transfer_single = false,
                            .release_old_target = self.transferForFreshBind(&segment.owned, assign.target),
                        };
                    step.pre_release = if (transfer.release_old_target) self.releaseDecision(assign.target) else null;
                    step.transfer_single = transfer.transfer_single;
                    if (assign.source_mode == .move and !transfer.transfer_single) {
                        try step.pre_retain.append(self.solve_allocator, assign.source);
                    }
                    step.retain_assign_ref_target = assign.source_mode == .borrow and !self.isBindingBorrowed(assign.target);
                    const singles = [_]LIR.LocalId{ assign.source, assign.target };
                    try self.finishArcPlanStepDeaths(step, &segment.owned, &singles, null, assign.next, segment.ctx.loop_keep);
                    segment.cursor = assign.next;
                },
                .assign_boxy_adapt => |assign| {
                    const step = try self.nextArcPlanStep(segment.plan_index, segment.cursor);
                    const transfer = if (assign.source_mode == .move)
                        try self.transferForSingle(&segment.owned, assign.source, assign.target, assign.next, segment.ctx.loop_keep)
                    else
                        SingleTransfer{
                            .transfer_single = false,
                            .release_old_target = self.transferForFreshBind(&segment.owned, assign.target),
                        };
                    step.pre_release = if (transfer.release_old_target) self.releaseDecision(assign.target) else null;
                    step.transfer_single = transfer.transfer_single;
                    if (assign.source_mode == .move and !transfer.transfer_single) {
                        try step.pre_retain.append(self.solve_allocator, assign.source);
                    }
                    const singles = [_]LIR.LocalId{ assign.source, assign.target };
                    try self.finishArcPlanStepDeaths(step, &segment.owned, &singles, null, assign.next, segment.ctx.loop_keep);
                    segment.cursor = assign.next;
                },
                .assign_boxy_inspect => |assign| {
                    const step = try self.nextArcPlanStep(segment.plan_index, segment.cursor);
                    const transfer = if (assign.source_mode == .move)
                        try self.transferForSingle(&segment.owned, assign.source, assign.target, assign.next, segment.ctx.loop_keep)
                    else
                        SingleTransfer{
                            .transfer_single = false,
                            .release_old_target = self.transferForFreshBind(&segment.owned, assign.target),
                        };
                    step.pre_release = if (transfer.release_old_target) self.releaseDecision(assign.target) else null;
                    step.transfer_single = transfer.transfer_single;
                    if (assign.source_mode == .move and !transfer.transfer_single) {
                        try step.pre_retain.append(self.solve_allocator, assign.source);
                    }
                    const singles = [_]LIR.LocalId{ assign.source, assign.target };
                    try self.finishArcPlanStepDeaths(step, &segment.owned, &singles, null, assign.next, segment.ctx.loop_keep);
                    segment.cursor = assign.next;
                },
                .assign_boxy_eq => |assign| {
                    const step = try self.nextArcPlanStep(segment.plan_index, segment.cursor);
                    if (assign.source_mode == .move) {
                        _ = try self.singleTransfer(assign.lhs, assign.next, assign.target, &segment.owned, segment.ctx.loop_keep);
                        _ = try self.singleTransfer(assign.rhs, assign.next, assign.target, &segment.owned, segment.ctx.loop_keep);
                    }
                    step.pre_release = if (self.transferForFreshBind(&segment.owned, assign.target)) self.releaseDecision(assign.target) else null;
                    const singles = [_]LIR.LocalId{ assign.lhs, assign.rhs, assign.target };
                    try self.finishArcPlanStepDeaths(step, &segment.owned, &singles, null, assign.next, segment.ctx.loop_keep);
                    segment.cursor = assign.next;
                },
                .assign_boxy_tag => |assign| {
                    const step = try self.nextArcPlanStep(segment.plan_index, segment.cursor);
                    const transfer = if (assign.payload_mode == .move)
                        try self.transferForSingle(&segment.owned, assign.payload, assign.target, assign.next, segment.ctx.loop_keep)
                    else
                        SingleTransfer{
                            .transfer_single = false,
                            .release_old_target = self.transferForFreshBind(&segment.owned, assign.target),
                        };
                    step.pre_release = if (transfer.release_old_target) self.releaseDecision(assign.target) else null;
                    step.transfer_single = transfer.transfer_single;
                    if (assign.payload_mode == .move and !transfer.transfer_single) {
                        if (assign.payload) |payload| try step.pre_retain.append(self.solve_allocator, payload);
                    }
                    const singles = [_]LIR.LocalId{ assign.payload orelse assign.target, assign.target };
                    try self.finishArcPlanStepDeaths(step, &segment.owned, &singles, null, assign.next, segment.ctx.loop_keep);
                    segment.cursor = assign.next;
                },
                .assign_boxy_tag_payload => |assign| {
                    const step = try self.nextArcPlanStep(segment.plan_index, segment.cursor);
                    const transfer = if (assign.source_mode == .move)
                        try self.transferForSingle(&segment.owned, assign.source, assign.target, assign.next, segment.ctx.loop_keep)
                    else
                        SingleTransfer{
                            .transfer_single = false,
                            .release_old_target = self.transferForFreshBind(&segment.owned, assign.target),
                        };
                    step.pre_release = if (transfer.release_old_target) self.releaseDecision(assign.target) else null;
                    step.transfer_single = transfer.transfer_single;
                    if (assign.source_mode == .move and !transfer.transfer_single) {
                        try step.pre_retain.append(self.solve_allocator, assign.source);
                    }
                    step.retain_assign_ref_target = assign.source_mode == .borrow and !self.isBindingBorrowed(assign.target);
                    const singles = [_]LIR.LocalId{ assign.source, assign.target };
                    try self.finishArcPlanStepDeaths(step, &segment.owned, &singles, null, assign.next, segment.ctx.loop_keep);
                    segment.cursor = assign.next;
                },
                .boxy_tag_match => |tag_match| {
                    const terminal = try self.prepareBoxyTagMatchPlan(segment.plan_index, segment.cursor, tag_match.on_match, tag_match.on_miss);
                    const child_plans = terminal.boxy_tag_match;
                    try self.pushSolveSegment(tasks, tag_match.on_match, &segment.owned, segment.ctx, child_plans.match_plan);
                    try self.pushSolveSegment(tasks, tag_match.on_miss, &segment.owned, segment.ctx, child_plans.miss_plan);
                    return;
                },
                .assign_call_dict => |assign| {
                    const step = try self.nextArcPlanStep(segment.plan_index, segment.cursor);
                    step.transfer_mask = try self.spanTransferMask(assign.args, ~@as(u64, 0), assign.next, assign.target, &segment.owned, segment.ctx.loop_keep, .no);
                    step.pre_release = if (self.transferForFreshBind(&segment.owned, assign.target)) self.releaseDecision(assign.target) else null;
                    self.death_scratch.clearRetainingCapacity();
                    const singles = [_]LIR.LocalId{assign.target};
                    try self.postStmtDeaths(&segment.owned, &singles, assign.args, assign.next, segment.ctx.loop_keep, self.death_scratch);
                    try self.postStmtDeaths(&segment.owned, &.{}, assign.arg_descs, assign.next, segment.ctx.loop_keep, self.death_scratch);
                    try self.postStmtDeaths(&segment.owned, &.{}, assign.hidden_args, assign.next, segment.ctx.loop_keep, self.death_scratch);
                    try self.copyDeathScratchToStep(step);
                    segment.cursor = assign.next;
                },
                .assign_low_level => |assign| {
                    const step = try self.nextArcPlanStep(segment.plan_index, segment.cursor);
                    const borrowed_variant = assign.op.arcBorrowedResultVariant();
                    var use_borrowed = borrowed_variant != null and self.isBindingBorrowed(assign.target);
                    if (borrowed_variant != null and assign.op == .box_unbox) {
                        const args = self.store.getLocalSpan(assign.args);
                        const boxed = GuardedList.at(args, 0);
                        use_borrowed = !self.options.consume_dead_boxes or
                            !self.ownsUnit(&segment.owned, boxed) or
                            try self.groupUsedInPathExcept(assign.next, boxed, assign.target, segment.ctx.loop_keep);
                    }
                    const selected_op = if (use_borrowed) borrowed_variant.? else assign.op;
                    const selected_effect = if (borrowed_variant != null) selected_op.rcEffect() else assign.rc_effect;
                    const transfer = try self.transferForLowLevel(
                        &segment.owned,
                        assign.args,
                        selected_effect,
                        assign.target,
                        assign.next,
                        segment.ctx.loop_keep,
                        true,
                        borrowed_variant != null and !use_borrowed,
                    );
                    step.pre_release = if (transfer.release_old_target) self.releaseDecision(assign.target) else null;
                    step.preserve_consumed_args = transfer.preserve_consumed_args;
                    step.transfer_mask = transfer.transfer_mask;
                    step.skip_result_retain = self.isBindingBorrowed(assign.target);
                    step.unique_args = transfer.unique_args;
                    if (borrowed_variant != null) {
                        step.low_level_selection = .{ .op = selected_op, .rc_effect = selected_effect };
                    }
                    const singles = [_]LIR.LocalId{assign.target};
                    try self.finishArcPlanStepDeaths(step, &segment.owned, &singles, assign.args, assign.next, segment.ctx.loop_keep);
                    segment.cursor = assign.next;
                },
                .assign_list => |assign| {
                    const step = try self.nextArcPlanStep(segment.plan_index, segment.cursor);
                    self.transfer_position_scratch.clearRetainingCapacity();
                    const transfer = try self.transferForAggregate(&segment.owned, assign.elems, assign.target, assign.next, segment.ctx.loop_keep, self.transfer_position_scratch);
                    step.pre_release = if (transfer.release_old_target) self.releaseDecision(assign.target) else null;
                    try step.transfer_positions.appendSlice(self.solve_allocator, self.transfer_position_scratch.items);
                    const singles = [_]LIR.LocalId{assign.target};
                    try self.finishArcPlanStepDeaths(step, &segment.owned, &singles, assign.elems, assign.next, segment.ctx.loop_keep);
                    segment.cursor = assign.next;
                },
                .assign_struct => |assign| {
                    const step = try self.nextArcPlanStep(segment.plan_index, segment.cursor);
                    self.transfer_position_scratch.clearRetainingCapacity();
                    const transfer = try self.transferForAggregate(&segment.owned, assign.fields, assign.target, assign.next, segment.ctx.loop_keep, self.transfer_position_scratch);
                    step.pre_release = if (transfer.release_old_target) self.releaseDecision(assign.target) else null;
                    try step.transfer_positions.appendSlice(self.solve_allocator, self.transfer_position_scratch.items);
                    const singles = [_]LIR.LocalId{assign.target};
                    try self.finishArcPlanStepDeaths(step, &segment.owned, &singles, assign.fields, assign.next, segment.ctx.loop_keep);
                    segment.cursor = assign.next;
                },
                .assign_tag => |assign| {
                    const step = try self.nextArcPlanStep(segment.plan_index, segment.cursor);
                    const transfer = try self.transferForSingle(&segment.owned, assign.payload, assign.target, assign.next, segment.ctx.loop_keep);
                    step.pre_release = if (transfer.release_old_target) self.releaseDecision(assign.target) else null;
                    step.transfer_single = transfer.transfer_single;
                    const singles = [_]LIR.LocalId{ assign.payload orelse assign.target, assign.target };
                    try self.finishArcPlanStepDeaths(step, &segment.owned, &singles, null, assign.next, segment.ctx.loop_keep);
                    segment.cursor = assign.next;
                },
                .store_struct => |assign| {
                    const step = try self.nextArcPlanStep(segment.plan_index, segment.cursor);
                    self.transfer_position_scratch.clearRetainingCapacity();
                    try self.spanTransferPositions(assign.fields, assign.next, assign.dest, &segment.owned, segment.ctx.loop_keep, self.transfer_position_scratch);
                    try step.transfer_positions.appendSlice(self.solve_allocator, self.transfer_position_scratch.items);
                    try self.finishArcPlanStepDeaths(step, &segment.owned, &.{}, assign.fields, assign.next, segment.ctx.loop_keep);
                    segment.cursor = assign.next;
                },
                .store_tag => |assign| {
                    const step = try self.nextArcPlanStep(segment.plan_index, segment.cursor);
                    if (assign.payload) |payload| {
                        step.transfer_single = try self.singleTransfer(payload, assign.next, assign.dest, &segment.owned, segment.ctx.loop_keep);
                    }
                    const singles = [_]LIR.LocalId{assign.payload orelse assign.dest};
                    try self.finishArcPlanStepDeaths(step, &segment.owned, &singles, null, assign.next, segment.ctx.loop_keep);
                    segment.cursor = assign.next;
                },
                .set_local => |assign| {
                    const step = try self.nextArcPlanStep(segment.plan_index, segment.cursor);
                    const transfer = try self.transferForSetLocal(&segment.owned, assign.target, assign.value, assign.mode, assign.next, segment.ctx.loop_keep);
                    step.pre_release = if (transfer.release_old_target) self.releaseDecision(assign.target) else null;
                    step.retain_set_target = transfer.retain_target;
                    const singles = [_]LIR.LocalId{ assign.value, assign.target };
                    try self.finishArcPlanStepDeaths(step, &segment.owned, &singles, null, assign.next, segment.ctx.loop_keep);
                    segment.cursor = assign.next;
                },
                .debug => |debug_stmt| {
                    const step = try self.nextArcPlanStep(segment.plan_index, segment.cursor);
                    const singles = [_]LIR.LocalId{debug_stmt.message};
                    try self.finishArcPlanStepDeaths(step, &segment.owned, &singles, null, debug_stmt.next, segment.ctx.loop_keep);
                    segment.cursor = debug_stmt.next;
                },
                .expect => |expect_stmt| {
                    const step = try self.nextArcPlanStep(segment.plan_index, segment.cursor);
                    const singles = [_]LIR.LocalId{expect_stmt.condition};
                    try self.finishArcPlanStepDeaths(step, &segment.owned, &singles, null, expect_stmt.next, segment.ctx.loop_keep);
                    segment.cursor = expect_stmt.next;
                },
                .decref_if_initialized => |rc| {
                    _ = try self.nextArcPlanStep(segment.plan_index, segment.cursor);
                    self.noteEmittedRelease(&segment.owned, rc.value);
                    segment.cursor = rc.next;
                },
                .incref, .decref, .free => arcInvariant("ARC summary solver received already-reference-counted LIR"),
                .switch_stmt => |switch_stmt| {
                    const branches = self.store.getCFSwitchBranches(switch_stmt.branches);
                    if (switch_stmt.continuation) |continuation| {
                        const switch_index = self.solution.switchIndexOfStmt(segment.cursor);
                        if (switch_index >= self.switch_summaries.len) arcInvariant("ARC solver switch index exceeded its lifted table");
                        if (self.switch_summaries[switch_index] == null) {
                            const switch_summary = try self.solve_allocator.create(SwitchSummary);
                            const continuation_plan = try self.newArcPlan(continuation);
                            switch_summary.* = .{
                                .index = switch_index,
                                .start = segment.cursor,
                                .continuation = continuation,
                                .common = try OwnedSet.init(self.solve_allocator, self.domain()),
                                .resume_ctx = segment.ctx,
                                .continuation_plan = continuation_plan,
                            };
                            self.switch_summaries[switch_index] = switch_summary;
                        }
                        const switch_summary = self.switch_summaries[switch_index].?;
                        const terminal = try self.prepareSwitchPlan(
                            segment.plan_index,
                            segment.cursor,
                            branches,
                            switch_stmt.default_branch,
                            switch_summary.continuation_plan,
                            switch_summary.reached,
                        );
                        try self.registerSwitchControlPlan(switch_summary, segment.plan_index);
                        const child_plans = terminal.switch_stmt;
                        if (!switch_summary.resume_queued) {
                            switch_summary.resume_queued = true;
                            try tasks.append(self.solve_allocator, .{ .switch_resume = switch_index });
                        }
                        const stop = try self.solve_allocator.create(SolveStop);
                        stop.* = .{
                            .stmt = continuation,
                            .summary = switch_summary,
                            .contributes = true,
                            .parent = segment.ctx.stops,
                        };
                        var branch_ctx = segment.ctx;
                        branch_ctx.stops = stop;
                        const restoration = self.restitution_switches.get(segment.cursor);
                        for (0..GuardedList.borrowLen(branches)) |branch_index| {
                            const branch = GuardedList.at(branches, branch_index);
                            if (restoration) |restored| {
                                var branch_owned = try cloneOwnedSetWith(self.solve_allocator, &segment.owned);
                                restoreOutcomeResources(&branch_owned, restored.branch_resources[branch_index].items);
                                try self.pushSolveSegment(tasks, branch.body, &branch_owned, branch_ctx, child_plans.branch_plans[branch_index]);
                            } else {
                                try self.pushSolveSegment(tasks, branch.body, &segment.owned, branch_ctx, child_plans.branch_plans[branch_index]);
                            }
                        }
                        if (restoration) |restored| {
                            var default_owned = try cloneOwnedSetWith(self.solve_allocator, &segment.owned);
                            restoreOutcomeResources(&default_owned, restored.default_resources.items);
                            try self.pushSolveSegment(tasks, switch_stmt.default_branch, &default_owned, branch_ctx, child_plans.default_plan);
                        } else {
                            try self.pushSolveSegment(tasks, switch_stmt.default_branch, &segment.owned, branch_ctx, child_plans.default_plan);
                        }
                        return;
                    }

                    const terminal = try self.prepareSwitchPlan(segment.plan_index, segment.cursor, branches, switch_stmt.default_branch, no_plan, false);
                    const child_plans = terminal.switch_stmt;
                    const restoration = self.restitution_switches.get(segment.cursor);
                    for (0..GuardedList.borrowLen(branches)) |branch_index| {
                        const branch = GuardedList.at(branches, branch_index);
                        if (restoration) |restored| {
                            var branch_owned = try cloneOwnedSetWith(self.solve_allocator, &segment.owned);
                            restoreOutcomeResources(&branch_owned, restored.branch_resources[branch_index].items);
                            try self.pushSolveSegment(tasks, branch.body, &branch_owned, segment.ctx, child_plans.branch_plans[branch_index]);
                        } else {
                            try self.pushSolveSegment(tasks, branch.body, &segment.owned, segment.ctx, child_plans.branch_plans[branch_index]);
                        }
                    }
                    if (restoration) |restored| {
                        var default_owned = try cloneOwnedSetWith(self.solve_allocator, &segment.owned);
                        restoreOutcomeResources(&default_owned, restored.default_resources.items);
                        try self.pushSolveSegment(tasks, switch_stmt.default_branch, &default_owned, segment.ctx, child_plans.default_plan);
                    } else {
                        try self.pushSolveSegment(tasks, switch_stmt.default_branch, &segment.owned, segment.ctx, child_plans.default_plan);
                    }
                    return;
                },
                .switch_initialized_payload => |switch_stmt| {
                    const terminal = try self.prepareInitializedPayloadSwitchPlan(
                        segment.plan_index,
                        segment.cursor,
                        switch_stmt.initialized_branch,
                        switch_stmt.uninitialized_branch,
                    );
                    const child_plans = terminal.initialized_payload_switch;
                    var initialized_owned = try cloneOwnedSetWith(self.solve_allocator, &segment.owned);
                    self.placeUnit(&initialized_owned, switch_stmt.payload);

                    var uninitialized_owned = try cloneOwnedSetWith(self.solve_allocator, &segment.owned);
                    // The branch proves the cell uninitialized: the payload
                    // binding ends with nothing to release.
                    _ = self.transferForInit(&uninitialized_owned, switch_stmt.payload);

                    try self.pushSolveSegment(tasks, switch_stmt.initialized_branch, &initialized_owned, segment.ctx, child_plans.initialized_plan);
                    try self.pushSolveSegment(tasks, switch_stmt.uninitialized_branch, &uninitialized_owned, segment.ctx, child_plans.uninitialized_plan);
                    return;
                },
                .str_match => |str_match| {
                    const terminal = try self.prepareStrMatchPlan(segment.plan_index, segment.cursor, str_match.on_match, str_match.on_miss);
                    const child_plans = terminal.str_match;
                    var match_owned = try cloneOwnedSetWith(self.solve_allocator, &segment.owned);
                    const steps = self.store.getStrMatchSteps(str_match.steps);
                    for (0..GuardedList.borrowLen(steps)) |step_index| {
                        const step = GuardedList.at(steps, step_index);
                        switch (step.capture) {
                            .discard => {},
                            .view => |local| self.placeUnit(&match_owned, local),
                        }
                    }
                    try self.pushSolveSegment(tasks, str_match.on_match, &match_owned, segment.ctx, child_plans.match_plan);
                    try self.pushSolveSegment(tasks, str_match.on_miss, &segment.owned, segment.ctx, child_plans.miss_plan);
                    return;
                },
                .str_match_set => |str_match_set| {
                    const arms = self.store.getStrMatchArms(str_match_set.arms);
                    const terminal = try self.prepareStrMatchSetPlan(segment.plan_index, segment.cursor, arms, str_match_set.on_miss);
                    const child_plans = terminal.str_match_set;
                    for (0..GuardedList.borrowLen(arms)) |arm_index| {
                        const arm = GuardedList.at(arms, arm_index);
                        var match_owned = try cloneOwnedSetWith(self.solve_allocator, &segment.owned);
                        const steps = self.store.getStrMatchSteps(arm.steps);
                        for (0..GuardedList.borrowLen(steps)) |step_index| {
                            const step = GuardedList.at(steps, step_index);
                            switch (step.capture) {
                                .discard => {},
                                .view => |local| self.placeUnit(&match_owned, local),
                            }
                        }
                        try self.pushSolveSegment(tasks, arm.on_match, &match_owned, segment.ctx, child_plans.match_plans[arm_index]);
                    }
                    try self.pushSolveSegment(tasks, str_match_set.on_miss, &segment.owned, segment.ctx, child_plans.miss_plan);
                    return;
                },
                .join => |join_stmt| {
                    try self.solveArriveAtJoin(tasks, segment, join_stmt);
                    return;
                },
                .jump => {
                    try self.solveJumpContribution(tasks, segment);
                    return;
                },
                .runtime_error, .comptime_exhaustiveness_failed => {
                    try self.setArcPlanTerminal(segment.plan_index, segment.cursor, &segment.owned, null, null);
                    return;
                },
                .crash => |crash_stmt| {
                    const retain_value = if (crash_stmt.msg.localId()) |message|
                        if (self.consumeAtTerminal(&segment.owned, message)) null else message
                    else
                        null;
                    try self.setArcPlanTerminal(segment.plan_index, segment.cursor, &segment.owned, null, retain_value);
                    return;
                },
                .loop_continue, .loop_break => {
                    if (segment.ctx.loop_keep) |keep| {
                        try self.setArcPlanTerminal(segment.plan_index, segment.cursor, &segment.owned, keep.set, null);
                    } else {
                        try self.setArcPlanTerminal(segment.plan_index, segment.cursor, &segment.owned, &segment.owned, null);
                    }
                    return;
                },
                .ret => |ret_stmt| {
                    var retain_value: ?LIR.LocalId = null;
                    if (self.current_sig.ret_mode == .borrowed) {
                        // No ownership unit transfers to the caller.
                    } else if (!self.consumeAtTerminal(&segment.owned, ret_stmt.value)) {
                        retain_value = ret_stmt.value;
                    }
                    const restitution_mask = if (self.current_sig.outcomes.isEmpty())
                        0
                    else
                        self.solution.restitutionParamsAt(segment.cursor);
                    if (restitution_mask == 0) {
                        try self.setArcPlanTerminal(segment.plan_index, segment.cursor, &segment.owned, null, retain_value);
                    } else {
                        var keep = try OwnedSet.init(self.solve_allocator, self.domain());
                        self.addRestitutionKeep(segment.cursor, &segment.owned, &keep);
                        try self.setArcPlanTerminal(segment.plan_index, segment.cursor, &segment.owned, &keep, retain_value);
                    }
                    return;
                },
                .expect_err => |expect_err_stmt| {
                    const retain_value = if (self.consumeAtTerminal(&segment.owned, expect_err_stmt.message))
                        null
                    else
                        expect_err_stmt.message;
                    try self.setArcPlanTerminal(segment.plan_index, segment.cursor, &segment.owned, null, retain_value);
                    return;
                },
                .comptime_branch_taken => |marker| {
                    _ = try self.nextArcPlanStep(segment.plan_index, segment.cursor);
                    segment.cursor = marker.next;
                },
            }
        }
    }

    fn solveSummaryOf(self: *Inserter, join_index: u32) *JoinSummary {
        if (join_index >= self.join_summaries.len) arcInvariant("ARC solver join index exceeded its lifted table");
        return self.join_summaries[join_index] orelse arcInvariant("ARC solver referenced a join before its summary was initialized");
    }

    fn newArcPlan(self: *Inserter, start: LIR.CFStmtId) ResourceError!u32 {
        if (self.arc_plans.plans.items.len != self.arc_plans.metadata.items.len) {
            arcInvariant("ARC plan and metadata tables lost index alignment");
        }
        const index = self.arc_plans.plans.items.len;
        if (index >= no_plan) arcInvariant("ARC plan index representation exhausted");
        try self.arc_plans.plans.append(self.solve_allocator, .{ .start = start });
        try self.arc_plans.metadata.append(self.solve_allocator, .{});
        return @intCast(index);
    }

    fn arcPlan(self: *Inserter, plan_index: u32) *ArcPlan {
        if (plan_index >= self.arc_plans.plans.items.len) arcInvariant("ARC referenced an unknown structured plan");
        return &self.arc_plans.plans.items[plan_index];
    }

    fn planMetadata(self: *Inserter, plan_index: u32) *ArcPlanMetadata {
        if (plan_index >= self.arc_plans.metadata.items.len) arcInvariant("ARC referenced metadata for an unknown structured plan");
        return &self.arc_plans.metadata.items[plan_index];
    }

    fn beginArcPlanUpdate(self: *Inserter, segment: *const SolveSegment) bool {
        const metadata = self.planMetadata(segment.plan_index);
        if (segment.plan_version != metadata.version) return false;
        const plan = self.arcPlan(segment.plan_index);
        if (plan.start != segment.cursor) arcInvariant("ARC structured plan was scheduled from a different start statement");
        plan.step_count = 0;
        plan.previous_terminal = plan.terminal;
        plan.terminal = .none;
        return true;
    }

    fn nextArcPlanStep(self: *Inserter, plan_index: u32, stmt: LIR.CFStmtId) ResourceError!*ArcPlanStep {
        const plan = self.arcPlan(plan_index);
        const step_index = plan.step_count;
        plan.step_count += 1;
        if (step_index == plan.steps.items.len) {
            try plan.steps.append(self.solve_allocator, .{});
        } else if (step_index > plan.steps.items.len) {
            arcInvariant("ARC plan step cursor skipped a stable slot");
        }
        const step = &plan.steps.items[step_index];
        if (step.initialized and step.stmt != stmt) {
            arcInvariant("ARC structured plan changed statement shape across fixed-point visits");
        }
        step.reset(stmt);
        return step;
    }

    fn copyDeathScratchToStep(self: *Inserter, step: *ArcPlanStep) ResourceError!void {
        try step.post_release.appendSlice(self.solve_allocator, self.death_scratch.items);
    }

    fn finishArcPlanStepDeaths(
        self: *Inserter,
        step: *ArcPlanStep,
        owned: *OwnedSet,
        singles: []const LIR.LocalId,
        span: ?LIR.LocalSpan,
        next: LIR.CFStmtId,
        loop_keep: ?LoopKeep,
    ) ResourceError!void {
        self.death_scratch.clearRetainingCapacity();
        try self.postStmtDeaths(owned, singles, span, next, loop_keep, self.death_scratch);
        try self.copyDeathScratchToStep(step);
    }

    fn setPlanTerminalState(self: *Inserter, plan_index: u32, owned: *const OwnedSet) ResourceError!void {
        const metadata = self.planMetadata(plan_index);
        if (metadata.terminal_state) |*state| {
            assignOwnedSet(state, owned);
        } else {
            metadata.terminal_state = try cloneOwnedSetWith(self.solve_allocator, owned);
        }
    }

    fn releaseDecision(self: *const Inserter, local: LIR.LocalId) ReleaseDecision {
        if (self.solution.maybeUninitializedCondition(local)) |condition| {
            return .{ .maybe_initialized = .{
                .value = local,
                .condition = condition.local,
                .condition_mask = condition.mask,
            } };
        }
        return .{ .initialized = local };
    }

    fn releaseDecisionFrom(self: *const Inserter, owned: *const OwnedSet, local: LIR.LocalId) ReleaseDecision {
        if (owned.fullResidualMask(local) != 0) {
            return .{ .residual = .{ .value = local, .field_mask = owned.residualMask(local) } };
        }
        return self.releaseDecision(local);
    }

    fn collectReleaseDifferenceInto(self: *Inserter, releases: *std.ArrayList(ReleaseDecision), owned: *const OwnedSet, keep: *const OwnedSet) ResourceError!void {
        owned.requireSameDomain(keep);
        releases.clearRetainingCapacity();
        var iter = owned.bits.iterator(.{ .direction = .reverse });
        while (iter.next()) |bit| {
            const local = owned.domain.resourceLocalAt(bit);
            if (!keep.bits.isSet(bit)) {
                try releases.append(self.solve_allocator, self.releaseDecisionFrom(owned, local));
                continue;
            }
            const residual_difference = owned.residual_masks[bit] & ~keep.residual_masks[bit];
            if (residual_difference != 0) {
                try releases.append(self.solve_allocator, .{ .residual = .{
                    .value = local,
                    .field_mask = residual_difference,
                } });
            }
        }
    }

    fn collectReleaseAllInto(self: *Inserter, releases: *std.ArrayList(ReleaseDecision), owned: *const OwnedSet) ResourceError!void {
        releases.clearRetainingCapacity();
        var iter = owned.bits.iterator(.{ .direction = .reverse });
        while (iter.next()) |bit| {
            const local = owned.domain.resourceLocalAt(bit);
            try releases.append(self.solve_allocator, self.releaseDecisionFrom(owned, local));
        }
    }

    fn addRestitutionKeep(
        self: *Inserter,
        stmt: LIR.CFStmtId,
        owned: *const OwnedSet,
        keep: *OwnedSet,
    ) void {
        if (self.current_sig.outcomes.isEmpty()) return;
        const mask = self.solution.restitutionParamsAt(stmt);
        if (mask == 0) return;
        const params = self.store.getLocalSpan(self.store.getProcSpec(self.current_proc).args);
        for (0..GuardedList.borrowLen(params)) |position| {
            const bit = arc_sig.paramBit(position) orelse break;
            if ((mask & bit) == 0) continue;
            if (self.current_sig.paramMode(position) != .owned) {
                arcInvariant("ARC restitution named a non-owned emitted parameter");
            }
            const param = GuardedList.at(params, position);
            if (!owned.contains(param)) {
                arcInvariant("ARC restitution boundary did not carry its exact entry parameter unit");
            }
            keep.copyResourceFrom(owned, param);
        }
    }

    fn restoreOutcomeResources(owned: *OwnedSet, resources: []const RestoredResource) void {
        for (resources) |resource| switch (resource) {
            .unit => |unit| {
                if (owned.contains(unit)) arcInvariant("ARC outcome restoration duplicated a live ownership unit");
                owned.set(unit);
            },
            .field => |field| owned.restoreResidualField(field.root, field.field_mask),
        };
    }

    fn noteRestitutionBoundaryReads(
        self: *Inserter,
        reads: *ExactBitSet,
        stmt: LIR.CFStmtId,
    ) void {
        if (self.current_sig.outcomes.isEmpty()) return;
        const mask = self.solution.restitutionParamsAt(stmt);
        if (mask == 0) return;
        const params = self.store.getLocalSpan(self.store.getProcSpec(self.current_proc).args);
        for (0..GuardedList.borrowLen(params)) |position| {
            const bit = arc_sig.paramBit(position) orelse break;
            if ((mask & bit) == 0) continue;
            self.noteLivenessUseLocal(reads, GuardedList.at(params, position));
        }
    }

    fn collectJumpReleaseDifferenceInto(
        self: *Inserter,
        releases: *std.ArrayList(ReleaseDecision),
        stmt: LIR.CFStmtId,
        owned: *const OwnedSet,
        body_keep: *const OwnedSet,
    ) ResourceError!void {
        var keep = try cloneOwnedSetWith(self.solve_allocator, body_keep);
        self.addRestitutionKeep(stmt, owned, &keep);
        try self.collectReleaseDifferenceInto(releases, owned, &keep);
    }

    fn updateJoinArrivalPlan(self: *Inserter, summary: *JoinSummary, plan_index: u32, stmt: LIR.CFStmtId, owned: *const OwnedSet) ResourceError!void {
        try self.setPlanTerminalState(plan_index, owned);
        const metadata = self.planMetadata(plan_index);
        if (metadata.arrival_join_index == no_plan) {
            metadata.arrival_join_index = summary.index;
            try summary.arrival_plans.append(self.solve_allocator, plan_index);
        } else if (metadata.arrival_join_index != summary.index) {
            arcInvariant("ARC structured plan arrived at two different joins");
        }
        const plan = self.arcPlan(plan_index);
        var releases: std.ArrayList(ReleaseDecision) = .empty;
        switch (plan.previous_terminal) {
            .join => |previous| {
                if (previous.stmt != stmt or previous.join_index != summary.index) {
                    arcInvariant("ARC join plan changed structural identity");
                }
                releases = previous.releases;
            },
            .none,
            .stop,
            .switch_stmt,
            .initialized_payload_switch,
            .str_match,
            .boxy_tag_match,
            .str_match_set,
            .jump,
            .terminal,
            => {},
        }
        try self.collectReleaseDifferenceInto(&releases, owned, &summary.entry_keep);
        plan.terminal = .{ .join = .{
            .stmt = stmt,
            .join_index = summary.index,
            .remainder_plan = summary.remainder_plan,
            .body_plan = summary.body_plan,
            .body_reachable = summary.body_reachable,
            .releases = releases,
        } };
    }

    fn refreshJoinArrivalPlans(self: *Inserter, summary: *JoinSummary) ResourceError!void {
        for (summary.arrival_plans.items) |plan_index| {
            const metadata = self.planMetadata(plan_index);
            const owned = if (metadata.terminal_state) |*state| state else arcInvariant("ARC join arrival plan lacked its exact state");
            const plan = self.arcPlan(plan_index);
            const terminal = switch (plan.terminal) {
                .join => |*join| join,
                .none,
                .stop,
                .switch_stmt,
                .initialized_payload_switch,
                .str_match,
                .boxy_tag_match,
                .str_match_set,
                .jump,
                .terminal,
                => arcInvariant("ARC registered join arrival plan had another terminal"),
            };
            terminal.body_reachable = summary.body_reachable;
            try self.collectReleaseDifferenceInto(&terminal.releases, owned, &summary.entry_keep);
        }
    }

    fn updateJumpPlan(self: *Inserter, summary: *JoinSummary, plan_index: u32, stmt: LIR.CFStmtId, owned: *const OwnedSet) ResourceError!void {
        try self.setPlanTerminalState(plan_index, owned);
        const metadata = self.planMetadata(plan_index);
        if (metadata.jump_join_index == no_plan) {
            metadata.jump_join_index = summary.index;
            try summary.jump_plans.append(self.solve_allocator, plan_index);
        } else if (metadata.jump_join_index != summary.index) {
            arcInvariant("ARC structured plan jumped to two different joins");
        }
        const plan = self.arcPlan(plan_index);
        var releases: std.ArrayList(ReleaseDecision) = .empty;
        switch (plan.previous_terminal) {
            .jump => |previous| {
                if (previous.stmt != stmt or previous.join_index != summary.index) arcInvariant("ARC jump plan changed structural identity");
                releases = previous.releases;
            },
            .none,
            .stop,
            .join,
            .switch_stmt,
            .initialized_payload_switch,
            .str_match,
            .boxy_tag_match,
            .str_match_set,
            .terminal,
            => {},
        }
        try self.collectJumpReleaseDifferenceInto(&releases, stmt, owned, &summary.body_keep);
        plan.terminal = .{ .jump = .{
            .stmt = stmt,
            .join_index = summary.index,
            .releases = releases,
        } };
    }

    fn refreshJumpPlans(self: *Inserter, summary: *JoinSummary) ResourceError!void {
        for (summary.jump_plans.items) |plan_index| {
            const metadata = self.planMetadata(plan_index);
            const owned = if (metadata.terminal_state) |*state| state else arcInvariant("ARC jump plan lacked its exact state");
            const plan = self.arcPlan(plan_index);
            const terminal = switch (plan.terminal) {
                .jump => |*jump| jump,
                .none,
                .stop,
                .join,
                .switch_stmt,
                .initialized_payload_switch,
                .str_match,
                .boxy_tag_match,
                .str_match_set,
                .terminal,
                => arcInvariant("ARC registered jump plan had another terminal"),
            };
            try self.collectJumpReleaseDifferenceInto(&terminal.releases, terminal.stmt, owned, &summary.body_keep);
        }
    }

    fn updateSwitchStopPlan(self: *Inserter, summary: *SwitchSummary, plan_index: u32, owned: *const OwnedSet) ResourceError!void {
        try self.setPlanTerminalState(plan_index, owned);
        const metadata = self.planMetadata(plan_index);
        if (metadata.stop_switch_index == no_plan) {
            metadata.stop_switch_index = summary.index;
            try summary.exit_plans.append(self.solve_allocator, plan_index);
        } else if (metadata.stop_switch_index != summary.index) {
            arcInvariant("ARC structured plan stopped at two different switch continuations");
        }
        const plan = self.arcPlan(plan_index);
        var releases: std.ArrayList(ReleaseDecision) = .empty;
        switch (plan.previous_terminal) {
            .stop => |previous| {
                if (previous.switch_index != summary.index or previous.target_plan != summary.continuation_plan) {
                    arcInvariant("ARC switch-stop plan changed structural identity");
                }
                releases = previous.releases;
            },
            .none,
            .join,
            .switch_stmt,
            .initialized_payload_switch,
            .str_match,
            .boxy_tag_match,
            .str_match_set,
            .jump,
            .terminal,
            => {},
        }
        try self.collectReleaseDifferenceInto(&releases, owned, &summary.common);
        plan.terminal = .{ .stop = .{
            .switch_index = summary.index,
            .target_plan = summary.continuation_plan,
            .releases = releases,
        } };
    }

    fn registerLatentSwitchStopPlan(
        self: *Inserter,
        summary: *SwitchSummary,
        plan_index: u32,
        owned: *const OwnedSet,
    ) ResourceError!void {
        const metadata = self.planMetadata(plan_index);
        if (metadata.latent_stop_switch_index == no_plan) {
            metadata.latent_stop_switch_index = summary.index;
            try summary.latent_stop_plans.append(self.solve_allocator, plan_index);
        } else if (metadata.latent_stop_switch_index != summary.index) {
            arcInvariant("ARC structured plan was latent at two different switch continuations");
        }
        if (metadata.latent_stop_state) |*state| {
            assignOwnedSet(state, owned);
        } else {
            metadata.latent_stop_state = try cloneOwnedSetWith(self.solve_allocator, owned);
        }
    }

    fn activateLatentSwitchStopPlans(self: *Inserter, summary: *SwitchSummary) ResourceError!void {
        for (summary.latent_stop_plans.items) |plan_index| {
            const metadata = self.planMetadata(plan_index);
            const owned = if (metadata.latent_stop_state) |*state|
                state
            else
                arcInvariant("ARC latent switch-stop plan lacked its exact state");
            try self.updateSwitchStopPlan(summary, plan_index, owned);
        }
    }

    fn refreshSwitchStopPlans(self: *Inserter, summary: *SwitchSummary) ResourceError!void {
        for (summary.exit_plans.items) |plan_index| {
            const metadata = self.planMetadata(plan_index);
            const owned = if (metadata.terminal_state) |*state| state else arcInvariant("ARC switch-stop plan lacked its exact state");
            const plan = self.arcPlan(plan_index);
            const terminal = switch (plan.terminal) {
                .stop => |*stop| stop,
                .none,
                .join,
                .switch_stmt,
                .initialized_payload_switch,
                .str_match,
                .boxy_tag_match,
                .str_match_set,
                .jump,
                .terminal,
                => arcInvariant("ARC registered switch-stop plan had another terminal"),
            };
            try self.collectReleaseDifferenceInto(&terminal.releases, owned, &summary.common);
        }
    }

    fn registerSwitchControlPlan(self: *Inserter, summary: *SwitchSummary, plan_index: u32) ResourceError!void {
        const metadata = self.planMetadata(plan_index);
        if (metadata.control_switch_index == no_plan) {
            metadata.control_switch_index = summary.index;
            try summary.control_plans.append(self.solve_allocator, plan_index);
        } else if (metadata.control_switch_index != summary.index) {
            arcInvariant("ARC structured plan contained two continuation switches");
        }
    }

    fn refreshSwitchControlPlans(self: *Inserter, summary: *SwitchSummary) void {
        for (summary.control_plans.items) |plan_index| {
            const plan = self.arcPlan(plan_index);
            switch (plan.terminal) {
                .switch_stmt => |*switch_plan| switch_plan.continuation_reachable = summary.reached,
                .none,
                .stop,
                .join,
                .initialized_payload_switch,
                .str_match,
                .boxy_tag_match,
                .str_match_set,
                .jump,
                .terminal,
                => arcInvariant("ARC registered switch control plan had another terminal"),
            }
        }
    }

    fn setArcPlanTerminal(
        self: *Inserter,
        plan_index: u32,
        stmt: LIR.CFStmtId,
        owned: *const OwnedSet,
        keep: ?*const OwnedSet,
        retain_value: ?LIR.LocalId,
    ) ResourceError!void {
        const plan = self.arcPlan(plan_index);
        var releases: std.ArrayList(ReleaseDecision) = .empty;
        switch (plan.previous_terminal) {
            .terminal => |previous| {
                if (previous.stmt != stmt) arcInvariant("ARC terminal plan changed structural identity");
                releases = previous.releases;
            },
            .none,
            .stop,
            .join,
            .switch_stmt,
            .initialized_payload_switch,
            .str_match,
            .boxy_tag_match,
            .str_match_set,
            .jump,
            => {},
        }
        if (keep) |kept| {
            try self.collectReleaseDifferenceInto(&releases, owned, kept);
        } else {
            try self.collectReleaseAllInto(&releases, owned);
        }
        plan.terminal = .{ .terminal = .{
            .stmt = stmt,
            .releases = releases,
            .retain_value = retain_value,
        } };
    }

    fn prepareSwitchPlan(
        self: *Inserter,
        plan_index: u32,
        stmt: LIR.CFStmtId,
        branch_starts: anytype,
        default_start: LIR.CFStmtId,
        continuation_plan: u32,
        continuation_reachable: bool,
    ) ResourceError!*ArcPlanTerminal {
        const previous_terminal = self.arcPlan(plan_index).previous_terminal;
        var branch_plans: []u32 = undefined;
        var default_plan: u32 = undefined;
        switch (previous_terminal) {
            .switch_stmt => |previous| {
                if (previous.stmt != stmt or previous.branch_plans.len != branch_starts.len) {
                    arcInvariant("ARC switch plan changed structural shape across fixed-point visits");
                }
                branch_plans = previous.branch_plans;
                default_plan = previous.default_plan;
            },
            .none,
            .stop,
            .join,
            .initialized_payload_switch,
            .str_match,
            .boxy_tag_match,
            .str_match_set,
            .jump,
            .terminal,
            => {
                branch_plans = try self.solve_allocator.alloc(u32, branch_starts.len);
                for (0..branch_starts.len) |index| {
                    branch_plans[index] = try self.newArcPlan(GuardedList.at(branch_starts, index).body);
                }
                default_plan = try self.newArcPlan(default_start);
            },
        }
        for (branch_plans, 0..) |child, index| {
            if (self.arcPlan(child).start != GuardedList.at(branch_starts, index).body) {
                arcInvariant("ARC switch arm plan changed its structured start");
            }
        }
        if (self.arcPlan(default_plan).start != default_start) arcInvariant("ARC switch default plan changed its structured start");
        self.arcPlan(plan_index).terminal = .{ .switch_stmt = .{
            .stmt = stmt,
            .branch_plans = branch_plans,
            .default_plan = default_plan,
            .continuation_plan = continuation_plan,
            .continuation_reachable = continuation_reachable,
        } };
        return &self.arcPlan(plan_index).terminal;
    }

    fn prepareInitializedPayloadSwitchPlan(
        self: *Inserter,
        plan_index: u32,
        stmt: LIR.CFStmtId,
        initialized_start: LIR.CFStmtId,
        uninitialized_start: LIR.CFStmtId,
    ) ResourceError!*ArcPlanTerminal {
        const previous_terminal = self.arcPlan(plan_index).previous_terminal;
        const children = switch (previous_terminal) {
            .initialized_payload_switch => |previous| blk: {
                if (previous.stmt != stmt) arcInvariant("ARC initialized-payload plan changed structural shape");
                break :blk .{ previous.initialized_plan, previous.uninitialized_plan };
            },
            .none,
            .stop,
            .join,
            .switch_stmt,
            .str_match,
            .boxy_tag_match,
            .str_match_set,
            .jump,
            .terminal,
            => .{ try self.newArcPlan(initialized_start), try self.newArcPlan(uninitialized_start) },
        };
        if (self.arcPlan(children[0]).start != initialized_start or self.arcPlan(children[1]).start != uninitialized_start) {
            arcInvariant("ARC initialized-payload child plan changed its structured start");
        }
        self.arcPlan(plan_index).terminal = .{ .initialized_payload_switch = .{
            .stmt = stmt,
            .initialized_plan = children[0],
            .uninitialized_plan = children[1],
        } };
        return &self.arcPlan(plan_index).terminal;
    }

    fn prepareStrMatchPlan(self: *Inserter, plan_index: u32, stmt: LIR.CFStmtId, match_start: LIR.CFStmtId, miss_start: LIR.CFStmtId) ResourceError!*ArcPlanTerminal {
        const previous_terminal = self.arcPlan(plan_index).previous_terminal;
        const children = switch (previous_terminal) {
            .str_match => |previous| blk: {
                if (previous.stmt != stmt) arcInvariant("ARC string-match plan changed structural shape");
                break :blk .{ previous.match_plan, previous.miss_plan };
            },
            .none,
            .stop,
            .join,
            .switch_stmt,
            .initialized_payload_switch,
            .boxy_tag_match,
            .str_match_set,
            .jump,
            .terminal,
            => .{ try self.newArcPlan(match_start), try self.newArcPlan(miss_start) },
        };
        if (self.arcPlan(children[0]).start != match_start or self.arcPlan(children[1]).start != miss_start) {
            arcInvariant("ARC string-match child plan changed its structured start");
        }
        const capture_retain_count = self.strMatchCaptureRetainCount(self.store.getCFStmt(stmt).str_match.steps);
        self.arcPlan(plan_index).terminal = .{ .str_match = .{
            .stmt = stmt,
            .match_plan = children[0],
            .miss_plan = children[1],
            .capture_retain_count = capture_retain_count,
        } };
        return &self.arcPlan(plan_index).terminal;
    }

    fn prepareBoxyTagMatchPlan(self: *Inserter, plan_index: u32, stmt: LIR.CFStmtId, match_start: LIR.CFStmtId, miss_start: LIR.CFStmtId) ResourceError!*ArcPlanTerminal {
        const previous_terminal = self.arcPlan(plan_index).previous_terminal;
        const children = if (previous_terminal == .boxy_tag_match) blk: {
            const previous = previous_terminal.boxy_tag_match;
            if (previous.stmt != stmt) arcInvariant("ARC Boxy tag-match plan changed structural shape");
            break :blk .{ previous.match_plan, previous.miss_plan };
        } else .{ try self.newArcPlan(match_start), try self.newArcPlan(miss_start) };
        if (self.arcPlan(children[0]).start != match_start or self.arcPlan(children[1]).start != miss_start) {
            arcInvariant("ARC Boxy tag-match child plan changed its structured start");
        }
        self.arcPlan(plan_index).terminal = .{ .boxy_tag_match = .{
            .stmt = stmt,
            .match_plan = children[0],
            .miss_plan = children[1],
        } };
        return &self.arcPlan(plan_index).terminal;
    }

    fn prepareStrMatchSetPlan(self: *Inserter, plan_index: u32, stmt: LIR.CFStmtId, arms: anytype, miss_start: LIR.CFStmtId) ResourceError!*ArcPlanTerminal {
        const previous_terminal = self.arcPlan(plan_index).previous_terminal;
        var match_plans: []u32 = undefined;
        var miss_plan: u32 = undefined;
        var retain_counts: []u16 = undefined;
        switch (previous_terminal) {
            .str_match_set => |previous| {
                if (previous.stmt != stmt or previous.match_plans.len != arms.len) {
                    arcInvariant("ARC string-match-set plan changed structural shape");
                }
                match_plans = previous.match_plans;
                miss_plan = previous.miss_plan;
                retain_counts = previous.capture_retain_counts;
            },
            .none,
            .stop,
            .join,
            .switch_stmt,
            .initialized_payload_switch,
            .str_match,
            .boxy_tag_match,
            .jump,
            .terminal,
            => {
                match_plans = try self.solve_allocator.alloc(u32, arms.len);
                retain_counts = try self.solve_allocator.alloc(u16, arms.len);
                for (0..arms.len) |index| match_plans[index] = try self.newArcPlan(GuardedList.at(arms, index).on_match);
                miss_plan = try self.newArcPlan(miss_start);
            },
        }
        for (match_plans, 0..) |child, index| {
            if (self.arcPlan(child).start != GuardedList.at(arms, index).on_match) arcInvariant("ARC string-match-set arm plan changed its start");
        }
        if (self.arcPlan(miss_plan).start != miss_start) arcInvariant("ARC string-match-set miss plan changed its start");
        for (0..arms.len) |index| retain_counts[index] = self.strMatchCaptureRetainCount(GuardedList.at(arms, index).steps);
        self.arcPlan(plan_index).terminal = .{ .str_match_set = .{
            .stmt = stmt,
            .match_plans = match_plans,
            .miss_plan = miss_plan,
            .capture_retain_counts = retain_counts,
        } };
        return &self.arcPlan(plan_index).terminal;
    }

    fn pushSolveSegment(
        self: *Inserter,
        tasks: *std.ArrayList(SolveTask),
        start: LIR.CFStmtId,
        owned: *const OwnedSet,
        ctx: SolveContext,
        plan_index: u32,
    ) ResourceError!void {
        const metadata = self.planMetadata(plan_index);
        metadata.scheduled = true;
        metadata.version += 1;
        if (metadata.version == 0) arcInvariant("ARC plan version representation exhausted");
        const segment = try self.solve_allocator.create(SolveSegment);
        segment.* = .{
            .cursor = start,
            .owned = try cloneOwnedSetWith(self.solve_allocator, owned),
            .ctx = ctx,
            .plan_index = plan_index,
            .plan_version = metadata.version,
        };
        try tasks.append(self.solve_allocator, .{ .segment = segment });
    }

    fn scheduleSolveJoinProcess(self: *Inserter, tasks: *std.ArrayList(SolveTask), summary: *JoinSummary) ResourceError!void {
        if (summary.process_queued) return;
        summary.process_queued = true;
        try tasks.append(self.solve_allocator, .{ .join_process = summary.index });
    }

    fn scheduleSolveBodyWalk(self: *Inserter, tasks: *std.ArrayList(SolveTask), summary: *JoinSummary) ResourceError!void {
        if (summary.body_walk_queued) return;
        summary.body_walk_queued = true;
        try tasks.append(self.solve_allocator, .{ .body_walk = summary.index });
    }

    /// Rebuilds a stop chain with contributions disabled: segments inside a
    /// join frame that reach an enclosing switch continuation end silently.
    fn stripStopContributions(self: *Inserter, stops: ?*const SolveStop) ResourceError!?*const SolveStop {
        const entry = stops orelse return null;
        const parent = try self.stripStopContributions(entry.parent);
        if (!entry.contributes and parent == entry.parent) return entry;
        const node = try self.solve_allocator.create(SolveStop);
        node.* = .{
            .stmt = entry.stmt,
            .summary = entry.summary,
            .contributes = false,
            .parent = parent,
        };
        return node;
    }

    fn registerLoopKeep(
        self: *Inserter,
        loop_keep_id: u32,
        body: LIR.CFStmtId,
        remainder: LIR.CFStmtId,
    ) ResourceError!void {
        if (loop_keep_id != self.loop_liveness_caches.items.len) {
            arcInvariant("ARC loop identities were not registered sequentially");
        }
        try self.loop_liveness_caches.append(self.emission_allocator, .{
            .region_roots = .{ body, remainder },
        });
    }

    /// Marks exactly one loop identity for boundary-delta propagation after
    /// its keep-set shrank. Rows depend on the keep-set only through loop-edge
    /// reads, so a build that consumed no keep bits remains valid.
    fn purgeLoopKeepLiveness(self: *Inserter, loop_keep_id: u32) ResourceError!bool {
        if (loop_keep_id >= self.loop_liveness_caches.items.len) {
            arcInvariant("ARC purged an unknown loop identity");
        }
        const cache = &self.loop_liveness_caches.items[loop_keep_id];
        if (!cache.consumed_keep_bits) return false;
        cache.dirty = true;
        return true;
    }

    /// True when the local's liveness group is read according to the given
    /// table row: the group bit for multi-member groups, the raw bit
    /// otherwise.
    fn groupUsedFromTable(
        self: *Inserter,
        reads: *const ExactBitSet,
        local: LIR.LocalId,
    ) bool {
        if (self.groupBitOf(local)) |bit| return reads.isSet(bit);
        const bit = self.rawLivenessBitOf(local) orelse return false;
        return reads.isSet(bit);
    }

    /// Seeds a join's body keep from above: every refcounted unit whose
    /// group is read in the body, plus the join params. Always a superset of
    /// the final keep, so the fixpoint descends monotonically.
    fn seedSolveBodyKeep(self: *Inserter, summary: *JoinSummary) ResourceError!void {
        const reads = try self.computeReadsBeforeRebind(summary.body, null, 0);
        for (self.domain().refcounted_locals) |local| {
            if (self.groupUsedFromTable(reads, local)) summary.body_keep.set(local);
        }
        self.placeSolveJoinParamsInto(summary, &summary.body_keep);
    }

    /// Places a join's params owned, skipping any the back edges leave
    /// loop-invariant.
    ///
    /// A join param is only freshly owned in the body when every arrival
    /// hands it a new unit. An entry edge always does, but a back edge that
    /// does not re-initialize the param leaves the previous iteration's value
    /// in place. Placing such a param owned makes the body release it once
    /// per iteration, so the second iteration releases an already-dead value.
    /// The back-edge meet is a shrinking accumulator seeded from the full
    /// param set and narrowed by the lattice's exact resource intersection,
    /// so filtering by it keeps this placement monotone and carries residual
    /// field masks through.
    fn placeSolveJoinParamsInto(self: *Inserter, summary: *const JoinSummary, keep: *OwnedSet) void {
        if (!summary.back_edge_seen) {
            self.placeAllSolveJoinParamsInto(summary, keep);
            return;
        }
        const params = self.store.getLocalSpan(summary.params);
        for (0..GuardedList.borrowLen(params)) |index| {
            placeSurvivingParam(keep, &summary.back_edge_params, GuardedList.at(params, index));
        }
        const maybe_params = self.store.getLocalSpan(summary.maybe_uninitialized_params);
        for (0..GuardedList.borrowLen(maybe_params)) |index| {
            placeSurvivingParam(keep, &summary.back_edge_params, GuardedList.at(maybe_params, index));
        }
    }

    /// Copies exactly what the back-edge meet still proves for one param:
    /// presence plus its surviving residual field mask. Placing full
    /// ownership here would re-authorize releasing aggregate fields a back
    /// edge already took, so the mask travels with the bit.
    fn placeSurvivingParam(keep: *OwnedSet, meet: *const OwnedSet, local: LIR.LocalId) void {
        if (!meet.contains(local)) return;
        keep.setWithResidual(local, meet.residualMask(local));
    }

    /// Unfiltered param placement: the seed for the back-edge meet.
    fn placeAllSolveJoinParamsInto(self: *Inserter, summary: *const JoinSummary, keep: *OwnedSet) void {
        const params = self.store.getLocalSpan(summary.params);
        for (0..GuardedList.borrowLen(params)) |index| {
            self.placeUnit(keep, GuardedList.at(params, index));
        }
        const maybe_params = self.store.getLocalSpan(summary.maybe_uninitialized_params);
        for (0..GuardedList.borrowLen(maybe_params)) |index| {
            self.placeConditionalUnit(keep, GuardedList.at(maybe_params, index));
        }
    }

    /// Folds one back edge's state into `back_edge_params`. Returns whether
    /// the meet shrank, which invalidates the body keep placed from it.
    fn absorbBackEdgeParams(
        self: *Inserter,
        summary: *JoinSummary,
        owned: *const OwnedSet,
    ) bool {
        var changed = false;
        if (!summary.back_edge_seen) {
            summary.back_edge_seen = true;
            self.placeAllSolveJoinParamsInto(summary, &summary.back_edge_params);
            changed = true;
        }
        // The same exact resource meet the rest of the lattice uses: a param
        // survives only with the residual field places every back edge still
        // proves, not merely its presence bit.
        if (intersectOwnedSetChanged(&summary.back_edge_params, owned)) changed = true;
        return changed;
    }

    const BodyKeepUpdate = struct {
        changed: bool,
        /// Loop-keyed liveness for this join changed; every ownership region
        /// interpreted under its keep must re-walk after the exact row delta.
        purged: bool,
    };

    /// Recomputes a join's body keep from its jump-site states: intersect,
    /// filter to units read in the body, then place params. The stored keep
    /// can only shrink.
    fn recomputeSolveBodyKeep(self: *Inserter, summary: *JoinSummary) ResourceError!BodyKeepUpdate {
        if (!summary.body_reachable) return .{ .changed = false, .purged = false };
        var merged = try OwnedSet.init(self.solve_allocator, self.domain());
        assignOwnedSet(&merged, &summary.jump_common);
        const reads = try self.computeReadsBeforeRebind(summary.body, null, 0);
        var owned_iter = merged.bits.iterator(.{});
        while (owned_iter.next()) |index| {
            const local = merged.domain.resourceLocalAt(index);
            if (!self.groupUsedFromTable(reads, local)) merged.unset(local);
        }
        self.placeSolveJoinParamsInto(summary, &merged);
        if (merged.eql(&summary.body_keep)) return .{ .changed = false, .purged = false };
        assignOwnedSet(&summary.body_keep, &merged);
        try self.refreshJumpPlans(summary);
        const purged = try self.purgeLoopKeepLiveness(summary.loop_keep_id);
        return .{ .changed = true, .purged = purged };
    }

    /// Recomputes entry_keep = (entry_state filtered to units read from the
    /// remainder) | (body_keep & entry_state). Returns whether it changed.
    fn recomputeSolveEntryKeep(self: *Inserter, summary: *JoinSummary) ResourceError!bool {
        var keep = try OwnedSet.init(self.solve_allocator, self.domain());
        const remainder_reads = try self.computeReadsBeforeRebind(summary.remainder, null, 0);
        var entry_iter = summary.entry_state.bits.iterator(.{});
        while (entry_iter.next()) |index| {
            const local = summary.entry_state.domain.resourceLocalAt(index);
            if (self.groupUsedFromTable(remainder_reads, local) or summary.body_keep.contains(local)) {
                keep.copyResourceFrom(&summary.entry_state, local);
            }
        }
        if (keep.eql(&summary.entry_keep)) return false;
        assignOwnedSet(&summary.entry_keep, &keep);
        return true;
    }

    /// Context for walking one of a join's regions: the join's body keep is
    /// the loop keep-set, and inherited switch stops still bound the walk
    /// but collect no contributions across the join frame.
    fn solveRegionCtx(
        self: *Inserter,
        summary: *JoinSummary,
        body_scope: ?*const SolveBodyScope,
    ) ResourceError!SolveContext {
        return .{
            .loop_keep = .{ .set = &summary.body_keep, .id = summary.loop_keep_id },
            .stops = try self.stripStopContributions(summary.origin_ctx.stops),
            .body_scope = body_scope,
        };
    }

    fn processSolveJoin(self: *Inserter, tasks: *std.ArrayList(SolveTask), join_index: u32) ResourceError!void {
        const summary = self.solveSummaryOf(join_index);
        summary.process_queued = false;
        if (!summary.body_keep_seeded) {
            summary.body_keep_seeded = true;
            try self.seedSolveBodyKeep(summary);
        }

        _ = try self.recomputeSolveEntryKeep(summary);
        try self.refreshJoinArrivalPlans(summary);

        const remainder_ctx = try self.solveRegionCtx(summary, summary.origin_ctx.body_scope);
        try self.pushSolveSegment(tasks, summary.remainder, &summary.entry_keep, remainder_ctx, summary.remainder_plan);
        if (summary.body_reachable) try self.scheduleSolveBodyWalk(tasks, summary);
    }

    fn processSolveBodyWalk(self: *Inserter, tasks: *std.ArrayList(SolveTask), join_index: u32) ResourceError!void {
        const summary = self.solveSummaryOf(join_index);
        summary.body_walk_queued = false;
        if (!summary.body_reachable) return;
        const scope = try self.solve_allocator.create(SolveBodyScope);
        scope.* = .{ .join_index = summary.index, .parent = summary.origin_ctx.body_scope };
        const body_ctx = try self.solveRegionCtx(summary, scope);
        try self.pushSolveSegment(tasks, summary.body, &summary.body_keep, body_ctx, summary.body_plan);
    }

    fn processSolveSwitchResume(self: *Inserter, tasks: *std.ArrayList(SolveTask), switch_index: u32) ResourceError!void {
        if (switch_index >= self.switch_summaries.len) arcInvariant("ARC solver resumed a switch beyond its lifted table");
        const summary = self.switch_summaries[switch_index] orelse arcInvariant("ARC solver resumed an uninitialized switch");
        summary.resume_queued = false;
        if (!summary.reached) return;
        try self.pushSolveSegment(tasks, summary.continuation, &summary.common, summary.resume_ctx, summary.continuation_plan);
    }

    /// A branch segment reached its switch's continuation: fold its exit
    /// state into the merged entry and (re)schedule the continuation walk.
    fn contributeSolveSwitchExit(
        self: *Inserter,
        tasks: *std.ArrayList(SolveTask),
        summary: *SwitchSummary,
        owned: *const OwnedSet,
    ) ResourceError!void {
        var changed = false;
        if (!summary.reached) {
            summary.reached = true;
            self.refreshSwitchControlPlans(summary);
            assignOwnedSet(&summary.common, owned);
            try self.activateLatentSwitchStopPlans(summary);
            changed = true;
        } else {
            changed = intersectOwnedSetChanged(&summary.common, owned);
        }
        if (changed and !summary.resume_queued) {
            try self.refreshSwitchStopPlans(summary);
            summary.resume_queued = true;
            try tasks.append(self.solve_allocator, .{ .switch_resume = summary.index });
        } else if (changed) {
            try self.refreshSwitchStopPlans(summary);
        }
    }

    /// A segment arrived at a join statement: intersect the entry state and
    /// (re)schedule the join's keep computation and remainder walk.
    fn solveArriveAtJoin(
        self: *Inserter,
        tasks: *std.ArrayList(SolveTask),
        segment: *SolveSegment,
        join_stmt: anytype,
    ) ResourceError!void {
        const join_index = self.solution.joinIndexOfStmt(segment.cursor);
        if (join_index >= self.join_summaries.len) arcInvariant("ARC solver join index exceeded its lifted table");
        if (self.join_summaries[join_index] == null) {
            const summary = try self.solve_allocator.create(JoinSummary);
            const join_fact = self.join_bodies[join_index];
            const jump_states = try self.solve_allocator.alloc(?OwnedSet, join_fact.jump_count);
            @memset(jump_states, null);
            const remainder_plan = try self.newArcPlan(join_stmt.remainder);
            const body_plan = try self.newArcPlan(join_stmt.body);
            summary.* = .{
                .index = join_index,
                .id = join_stmt.id,
                .start = segment.cursor,
                .params = join_stmt.params,
                .maybe_uninitialized_params = join_stmt.maybe_uninitialized_params,
                .remainder = join_stmt.remainder,
                .body = join_stmt.body,
                .entry_state = try cloneOwnedSetWith(self.solve_allocator, &segment.owned),
                .entry_keep = try OwnedSet.init(self.solve_allocator, self.domain()),
                .body_keep = try OwnedSet.init(self.solve_allocator, self.domain()),
                .jump_common = try OwnedSet.init(self.solve_allocator, self.domain()),
                .back_edge_params = try OwnedSet.init(self.solve_allocator, self.domain()),
                .loop_keep_id = self.next_loop_keep_id,
                .remainder_plan = remainder_plan,
                .body_plan = body_plan,
                .origin_ctx = segment.ctx,
                .jump_states = jump_states,
            };
            try self.registerLoopKeep(summary.loop_keep_id, summary.body, summary.remainder);
            self.next_loop_keep_id += 1;
            self.join_summaries[join_index] = summary;
            try self.updateJoinArrivalPlan(summary, segment.plan_index, segment.cursor, &segment.owned);
            try self.scheduleSolveJoinProcess(tasks, summary);
            return;
        }

        const summary = self.join_summaries[join_index].?;
        if (summary.body != join_stmt.body or summary.remainder != join_stmt.remainder or
            !localSpanEql(summary.params, join_stmt.params) or
            !localSpanEql(summary.maybe_uninitialized_params, join_stmt.maybe_uninitialized_params))
        {
            arcInvariant("ARC solver saw one join id with conflicting metadata");
        }
        try self.updateJoinArrivalPlan(summary, segment.plan_index, segment.cursor, &segment.owned);
        if (intersectOwnedSetChanged(&summary.entry_state, &segment.owned)) {
            try self.scheduleSolveJoinProcess(tasks, summary);
        }
    }

    /// A segment reached a jump: contribute its state to the target join's
    /// body keep unless the jump is a back edge inside that join's own body.
    fn solveJumpContribution(
        self: *Inserter,
        tasks: *std.ArrayList(SolveTask),
        segment: *SolveSegment,
    ) ResourceError!void {
        const target_index = self.solution.jumpTargetJoinIndexOf(segment.cursor);
        const summary = self.solveSummaryOf(target_index);
        try self.updateJumpPlan(summary, segment.plan_index, segment.cursor, &segment.owned);
        var scope = segment.ctx.body_scope;
        while (scope) |entry| {
            if (entry.join_index == target_index) {
                // A back edge does not feed the body keep's general
                // intersection—it conforms at emission by releasing down to
                // the keep—but it does constrain which params the body may
                // treat as freshly owned, because a param it leaves alone
                // still holds the value the body already released.
                if (self.absorbBackEdgeParams(summary, &segment.owned)) {
                    try self.applySolveBodyKeepUpdate(tasks, summary);
                }
                return;
            }
            scope = entry.parent;
        }
        const site_index = self.solution.jumpSiteIndexOf(segment.cursor);
        if (site_index >= summary.jump_states.len) arcInvariant("ARC jump-site index exceeded its lifted join table");
        var changed = false;
        if (summary.jump_states[site_index] == null) {
            summary.jump_states[site_index] = try cloneOwnedSetWith(self.solve_allocator, &segment.owned);
            changed = true;
        } else {
            changed = intersectOwnedSetChanged(&summary.jump_states[site_index].?, &segment.owned);
        }
        const site = &summary.jump_states[site_index].?;
        const first_reach = !summary.body_reachable;
        if (!changed and !first_reach) return;
        const common_changed = if (first_reach) blk: {
            summary.body_reachable = true;
            try self.refreshJoinArrivalPlans(summary);
            assignOwnedSet(&summary.jump_common, site);
            break :blk true;
        } else intersectOwnedSetChanged(&summary.jump_common, site);
        if (!common_changed) return;
        try self.applySolveBodyKeepUpdate(tasks, summary);
        if (first_reach) try self.scheduleSolveBodyWalk(tasks, summary);
    }

    /// Recomputes a join's body keep and schedules whatever the shrink
    /// invalidated.
    fn applySolveBodyKeepUpdate(
        self: *Inserter,
        tasks: *std.ArrayList(SolveTask),
        summary: *JoinSummary,
    ) ResourceError!void {
        const update = try self.recomputeSolveBodyKeep(summary);
        if (update.purged) {
            // Liveness rows under this join's keep changed, so states
            // everywhere in its regions can shift: full re-process.
            try self.scheduleSolveJoinProcess(tasks, summary);
            return;
        }
        if (update.changed) {
            // Liveness rows are unaffected, so the remainder walk would
            // reproduce its states unless the entry keep itself moved; the
            // body still re-walks from its smaller seed.
            if (try self.recomputeSolveEntryKeep(summary)) {
                try self.scheduleSolveJoinProcess(tasks, summary);
            } else {
                try self.scheduleSolveBodyWalk(tasks, summary);
            }
        }
    }

    fn isBindingBorrowed(self: *const Inserter, local: LIR.LocalId) bool {
        if (!self.solution.isBorrowed(local)) return false;
        return !self.owned_binding_override.contains(local);
    }

    // Ownership-transfer keying layer
    //
    // Every ownership-transfer decision in this pass routes through this
    // section, so the alias-to-unit keying rules live in exactly one place
    // (issue 9703 was a transfer that tested and cleared the `OwnedSet` by
    // an alias's own local id while the solver had put the unit on the
    // alias's source local). The layer has two levels:
    //
    // Keying primitives—the only code allowed to touch `OwnedSet` bits at
    // a transfer site:
    // - `unitOf` is the single alias-to-unit resolution: a borrowed pure
    //   same-value alias moves its source's unit; everything else moves its
    //   own.
    // - `ownsUnit` / `takeUnit` test / test-and-clear a unit by its unit
    //   local.
    // - `placeUnit` places a fresh unit on the *name* being bound; borrowed
    //   bindings never carry a unit. `placeConditionalUnit` and
    //   `placeCallResultUnit` are its two mode-specific variants.
    // - `takeRebindTarget` ends the previous binding of a name that is
    //   being rebound. Rebinding kills only that name's own binding—a
    //   rebound borrowed alias must not release its source's unit—so this
    //   one is keyed by the raw local id on purpose.
    // - `noteEmittedRelease` replays already-emitted release statements into
    //   the state during boundary re-walks; emitted RC statements name unit
    //   locals directly, so raw keying is exact there.
    //
    // Per-instruction transfer functions (`transferFor*`,
    // `consumeAtTerminal`)—one per ownership-moving instruction kind.
    // Each advances the abstract ownership state exactly once and returns
    // the concrete decision written into the stable ArcPlan slot by
    // `processSolveSegment`. Adding an ownership-moving LIR instruction
    // means adding one transfer function here and one dumb materialization
    // case that follows its completed decision.

    /// The single alias-to-unit resolution used by every transfer site.
    fn unitOf(self: *const Inserter, local: LIR.LocalId) LIR.LocalId {
        if (self.dismantles.projectionUnitOf(local)) |root| return root;
        return self.solution.unitLocalOf(local);
    }

    fn ownsUnit(self: *const Inserter, owned: *const OwnedSet, local: LIR.LocalId) bool {
        const unit = self.unitOf(local);
        if (!owned.contains(unit)) return false;
        const full = owned.fullResidualMask(unit);
        return full == 0 or owned.residualMask(unit) == full;
    }

    /// Test-and-clear by unit key; returns whether a unit moved.
    fn takeUnit(self: *const Inserter, owned: *OwnedSet, local: LIR.LocalId) bool {
        const unit = self.unitOf(local);
        if (!owned.contains(unit)) return false;
        owned.unset(unit);
        return true;
    }

    fn unsetOwnedUnit(self: *const Inserter, owned: *OwnedSet, local: LIR.LocalId) void {
        owned.unset(self.unitOf(local));
    }

    /// Places a fresh ownership unit on the name being bound. Borrowed
    /// bindings carry no unit: the lender's liveness group keeps the value
    /// alive across every use.
    fn placeUnit(self: *Inserter, owned: *OwnedSet, local: LIR.LocalId) void {
        if (!self.localContainsRefcounted(local)) return;
        if (self.isBindingBorrowed(local)) return;
        owned.set(local);
    }

    /// Places a conditionally-present unit (maybe-initialized join payload
    /// cells). The overwrite of such a cell is still the lifetime end of the
    /// previous payload, so the unit is placed even on a solved-borrowed
    /// binding.
    fn placeConditionalUnit(self: *Inserter, owned: *OwnedSet, local: LIR.LocalId) void {
        if (!self.localContainsRefcounted(local)) return;
        owned.set(local);
    }

    /// Places the unit a call result carries: an owned return always hands
    /// the caller a unit, and a borrowed return still needs one here unless
    /// the result binding itself is borrowed.
    fn placeCallResultUnit(
        self: *Inserter,
        owned: *OwnedSet,
        local: LIR.LocalId,
        ret_mode: arc_sig.Mode,
    ) void {
        if (!self.localContainsRefcounted(local)) return;
        if (ret_mode == .owned) {
            owned.set(local);
        } else if (!self.isBindingBorrowed(local)) {
            owned.set(local);
        }
    }

    /// Ends the previous binding of a name being rebound, returning whether
    /// a release must be emitted before the rebind. Keyed by the raw local
    /// id on purpose: rebinding a name kills only that name's own binding,
    /// never the unit of a value the name merely borrowed (audited for
    /// issue 9703's keying class—see the layer comment above).
    fn takeRebindTarget(_: *const Inserter, owned: *OwnedSet, target: LIR.LocalId) bool {
        if (!owned.contains(target)) return false;
        owned.unset(target);
        return true;
    }

    /// Replays an already-emitted decref/free into the state during a
    /// boundary re-walk. Emitted RC statements name unit locals directly.
    fn noteEmittedRelease(_: *const Inserter, owned: *OwnedSet, value: LIR.LocalId) void {
        owned.unset(value);
    }

    const AliasBindTransfer = struct {
        /// Retain the target right after the bind; false when the source's
        /// unit moved into the target or the binding is borrowed.
        retain_target: bool,
        release_old_target: bool,
    };

    /// `assign_ref` with a pure same-value `local` op, and `set_local`:
    /// binding one name to another name's value. A dying source moves its
    /// unit instead of paying a retain/release pair.
    fn transferForAliasBind(
        self: *Inserter,
        owned: *OwnedSet,
        target: LIR.LocalId,
        source: LIR.LocalId,
        next: LIR.CFStmtId,
        loop_keep: ?LoopKeep,
    ) ResourceError!AliasBindTransfer {
        const move_value = try self.canMoveAliasBindValue(owned, source, target, next, loop_keep);
        const release_old_target = self.takeRebindTarget(owned, target);
        if (move_value) _ = self.takeUnit(owned, source);
        self.placeUnit(owned, target);
        return .{ .retain_target = !move_value, .release_old_target = release_old_target };
    }

    /// A binding whose value is freshly produced by the statement itself
    /// (literals, payload reads): the new binding owns one unit; the old
    /// binding, if any, dies here.
    fn transferForFreshBind(self: *Inserter, owned: *OwnedSet, target: LIR.LocalId) bool {
        const release_old_target = self.takeRebindTarget(owned, target);
        self.placeUnit(owned, target);
        return release_old_target;
    }

    /// An owned read of a complete aggregate projection. If the root's unit
    /// is present and its liveness group has no later use on this path, move
    /// that unit into the target. Otherwise preserve the root and retain the
    /// projected value exactly like an ordinary fresh bind.
    fn transferForCompleteProjectionBind(
        self: *Inserter,
        owned: *OwnedSet,
        target: LIR.LocalId,
        root: LIR.LocalId,
        next: LIR.CFStmtId,
    ) ResourceError!AliasBindTransfer {
        const release_old_target = self.takeRebindTarget(owned, target);
        const unit = self.unitOf(root);
        // The path query follows loop edges itself and stops at a rebind of
        // this exact place definition. A loop keep-set cannot make that
        // distinction: it intentionally merges the old and next-iteration
        // bindings under the same LocalId.
        const root_used = try self.ownershipPlaceUsedInPath(next, unit);
        const has_unit = owned.contains(unit);
        const restitution = if (root_used)
            try self.completeProjectionRestitution(target, unit, next)
        else
            null;
        const move_root = has_unit and (!root_used or restitution != null);
        if (restitution) |claim| try self.setOutcomeRestoration(claim, move_root);
        if (move_root) owned.unset(unit);
        self.placeUnit(owned, target);
        return .{
            .retain_target = !move_root,
            .release_old_target = release_old_target,
        };
    }

    fn aliasesContain(aliases: []const LIR.LocalId, local: LIR.LocalId) bool {
        for (aliases) |alias| if (alias == local) return true;
        return false;
    }

    fn spanContainsAlias(self: *const Inserter, span: LIR.LocalSpan, aliases: []const LIR.LocalId) bool {
        const locals = self.store.getLocalSpan(span);
        for (0..GuardedList.borrowLen(locals)) |index| {
            if (aliasesContain(aliases, GuardedList.at(locals, index))) return true;
        }
        return false;
    }

    /// Exact initial ownership-place composition rule: between a complete
    /// projection and its checked direct call, only same-container aliases,
    /// non-RC field reads, and statements that do not mention the container
    /// may intervene. The call's explicit outcome switch must then guard every
    /// later use of the root with restitution.
    fn completeProjectionRestitution(
        self: *Inserter,
        projection: LIR.LocalId,
        root: LIR.LocalId,
        next: LIR.CFStmtId,
    ) ResourceError!?OutcomeRestitution {
        var aliases = std.ArrayList(LIR.LocalId).empty;
        try aliases.append(self.emission_allocator, root);
        var cursor = next;
        while (true) {
            const stmt = self.store.getCFStmt(cursor);
            if (stmt == .assign_ref) {
                const assign = stmt.assign_ref;
                switch (assign.op) {
                    .local => |source| {
                        if (aliasesContain(aliases.items, source)) {
                            if (!aliasesContain(aliases.items, assign.target)) {
                                try aliases.append(self.emission_allocator, assign.target);
                            }
                        }
                    },
                    .field => |field| {
                        if (aliasesContain(aliases.items, field.source) and self.localContainsRefcounted(assign.target)) {
                            return null;
                        }
                    },
                    .discriminant => |op| if (aliasesContain(aliases.items, op.source)) return null,
                    .tag_payload => |op| if (aliasesContain(aliases.items, op.source)) return null,
                    .tag_payload_struct => |op| if (aliasesContain(aliases.items, op.source)) return null,
                    .list_reinterpret => |op| if (aliasesContain(aliases.items, op.backing_ref)) return null,
                    .nominal => |op| if (aliasesContain(aliases.items, op.backing_ref)) return null,
                }
                cursor = assign.next;
            } else if (stmt == .assign_literal) {
                cursor = stmt.assign_literal.next;
            } else if (stmt == .assign_low_level) {
                if (self.spanContainsAlias(stmt.assign_low_level.args, aliases.items)) return null;
                cursor = stmt.assign_low_level.next;
            } else if (stmt == .assign_call) {
                const args = self.store.getLocalSpan(stmt.assign_call.args);
                var projection_position: ?usize = null;
                for (0..GuardedList.borrowLen(args)) |position| {
                    const arg = GuardedList.at(args, position);
                    if (arg == projection) {
                        if (projection_position != null) return null;
                        projection_position = position;
                    } else if (aliasesContain(aliases.items, arg)) {
                        return null;
                    }
                }
                if (projection_position) |position| {
                    var sig = self.solution.sigOf(stmt.assign_call.proc);
                    sig.outcomes = self.solution.availableOutcomeSpanOf(stmt.assign_call.proc);
                    if (!self.outcomeArgumentsHaveDistinctPlaces(stmt.assign_call.args, sig)) return null;
                    return try self.outcomeRestitutionGuard(
                        root,
                        .{ .unit = root },
                        position,
                        stmt.assign_call.target,
                        stmt.assign_call.next,
                        sig,
                    );
                }
                cursor = stmt.assign_call.next;
            } else {
                return null;
            }
        }
    }

    /// `init_uninitialized`: the target's previous binding dies and nothing
    /// replaces it.
    fn transferForInit(self: *Inserter, owned: *OwnedSet, target: LIR.LocalId) bool {
        return self.takeRebindTarget(owned, target);
    }

    /// `set_local`: like an alias bind, but the write mode decides whether
    /// the target's previous binding dies (join-result initialization writes
    /// into a cell that holds no previous value).
    fn transferForSetLocal(
        self: *Inserter,
        owned: *OwnedSet,
        target: LIR.LocalId,
        value: LIR.LocalId,
        mode: LIR.SetLocalWriteMode,
        next: LIR.CFStmtId,
        loop_keep: ?LoopKeep,
    ) ResourceError!AliasBindTransfer {
        if (target == value) return .{ .retain_target = false, .release_old_target = false };
        const move_value = try self.canMoveAliasBindValue(owned, value, target, next, loop_keep);
        const release_old_target = switch (mode) {
            .replace_existing, .initialize_join_param => self.takeRebindTarget(owned, target),
            .initialize_join_result => false,
        };
        if (move_value) _ = self.takeUnit(owned, value);
        self.placeUnit(owned, target);
        return .{ .retain_target = !move_value, .release_old_target = release_old_target };
    }

    const CallTransfer = struct {
        args: CallArgOwnership,
        release_old_target: bool,
        /// The callee returns a borrow of its arguments but this binding
        /// needs its own unit: pay one retain right after the call.
        retain_call_result: bool,
    };

    /// Direct and erased calls: owned argument positions consume the
    /// caller's units (dying arguments move, surviving ones pay a retain),
    /// and the result binding receives the returned unit. `extra_use` is the
    /// erased call's closure operand, which keeps the target's old binding
    /// alive through the call like an argument does.
    fn transferForCall(
        self: *Inserter,
        owned: *OwnedSet,
        call_stmt: ?LIR.CFStmtId,
        callee: ?LIR.LirProcSpecId,
        callee_sig: arc_sig.RcSig,
        unique_demand: bool,
        args: LIR.LocalSpan,
        next: LIR.CFStmtId,
        target: LIR.LocalId,
        extra_use: ?LIR.LocalId,
        loop_keep: ?LoopKeep,
    ) ResourceError!CallTransfer {
        const arg_ownership = try self.callArgOwnership(call_stmt, callee, owned, callee_sig, unique_demand, args, next, target, loop_keep);
        const target_feeds_call = self.spanUsesLocal(args, target) or
            (extra_use != null and extra_use.? == target);
        const release_old_target = if (target_feeds_call)
            false
        else
            self.takeRebindTarget(owned, target);
        self.placeCallResultUnit(owned, target, arg_ownership.demanded.ret_mode);
        const retain_call_result = arg_ownership.demanded.ret_mode == .borrowed and
            self.localContainsRefcounted(target) and
            !self.isBindingBorrowed(target);
        return .{
            .args = arg_ownership,
            .release_old_target = release_old_target,
            .retain_call_result = retain_call_result,
        };
    }

    const AggregateTransfer = struct {
        release_old_target: bool,
    };

    /// List and struct construction: every refcounted operand occurrence
    /// moves one unit into the aggregate; dying operands transfer their own
    /// unit, surviving ones pay a retain (emitted as the trailing increfs).
    fn transferForAggregate(
        self: *Inserter,
        owned: *OwnedSet,
        operands: LIR.LocalSpan,
        target: LIR.LocalId,
        next: LIR.CFStmtId,
        loop_keep: ?LoopKeep,
        transfer_positions: ?*std.ArrayList(u32),
    ) ResourceError!AggregateTransfer {
        try self.spanTransferPositions(operands, next, target, owned, loop_keep, transfer_positions);
        const release_old_target = self.takeRebindTarget(owned, target);
        self.placeUnit(owned, target);
        return .{ .release_old_target = release_old_target };
    }

    const SingleTransfer = struct {
        /// Move the tag payload or packed capture instead of retaining it.
        transfer_single: bool,
        release_old_target: bool,
    };

    /// Tag construction and packed erased-fn capture: the single operand
    /// variant of `transferForAggregate`.
    fn transferForSingle(
        self: *Inserter,
        owned: *OwnedSet,
        payload: ?LIR.LocalId,
        target: LIR.LocalId,
        next: LIR.CFStmtId,
        loop_keep: ?LoopKeep,
    ) ResourceError!SingleTransfer {
        var transfer_single = false;
        if (payload) |operand| {
            transfer_single = try self.singleTransfer(operand, next, target, owned, loop_keep);
        }
        const release_old_target = self.takeRebindTarget(owned, target);
        self.placeUnit(owned, target);
        return .{ .transfer_single = transfer_single, .release_old_target = release_old_target };
    }

    const PackedErasedTransfer = struct {
        transfer_single: bool,
        preserve_reuse: bool,
        reuse_unique: bool,
        release_old_target: bool,
    };

    fn transferForPackedErased(
        self: *Inserter,
        owned: *OwnedSet,
        assign: @FieldType(LIR.CFStmt, "assign_packed_erased_fn"),
        loop_keep: ?LoopKeep,
    ) ResourceError!PackedErasedTransfer {
        var preserve_reuse = false;
        var reuse_unique = false;
        if (assign.reuse) |reuse| {
            if (reuse == assign.target) arcInvariant("erased callable repack cannot reuse its result binding");
            preserve_reuse = try self.groupUsedInPath(assign.next, reuse, loop_keep);
            reuse_unique = !preserve_reuse and self.ownsUnit(owned, reuse) and self.isLocalUniqueHere(reuse);
            if (assign.capture) |capture| {
                if (self.solution.leaderOf(capture) == self.solution.leaderOf(reuse)) reuse_unique = false;
            }
            if (!preserve_reuse) _ = self.takeUnit(owned, reuse);
        }

        const transfer = try self.transferForSingle(owned, assign.capture, assign.target, assign.next, loop_keep);
        return .{
            .transfer_single = transfer.transfer_single,
            .preserve_reuse = preserve_reuse,
            .reuse_unique = reuse_unique,
            .release_old_target = transfer.release_old_target,
        };
    }

    const LowLevelTransfer = struct {
        /// Consumed positions that pay a retain before the op, either because
        /// their group survives or because an owned variant needs a unit from
        /// an argument whose solved binding is borrowed.
        preserve_consumed_args: u64,
        /// Retained positions whose group dies here: their unit moves into
        /// the result instead of paying the trailing retain.
        transfer_mask: u64,
        /// Runtime uniqueness checks this statement proved redundant, by
        /// argument position.
        unique_args: u64,
        release_old_target: bool,
    };

    /// Low-level ops: `RcEffect` masks say which positions the op consumes
    /// or retains; this decides which of those transfers move existing units
    /// and which pay retains. Borrowed-result variants may also retain a
    /// borrowed input to supply the unit required by the selected owned op.
    /// `want_unique` gates the uniqueness-claim scan, whose result only affects
    /// materialized statements.
    fn transferForLowLevel(
        self: *Inserter,
        owned: *OwnedSet,
        args: LIR.LocalSpan,
        rc_effect: LIR.LowLevel.RcEffect,
        target: LIR.LocalId,
        next: LIR.CFStmtId,
        loop_keep: ?LoopKeep,
        want_unique: bool,
        supply_missing_consumed_args: bool,
    ) ResourceError!LowLevelTransfer {
        if ((rc_effect.result_aliases_consumed_args & ~rc_effect.consume_args) != 0) {
            arcInvariant("ARC low-level result-token metadata referenced a non-consumed argument");
        }
        var preserve_consumed_args = try self.preserveConsumedArgMask(
            args,
            rc_effect.consume_args,
            rc_effect.result_aliases_consumed_args,
            next,
            target,
            loop_keep,
        );
        if (supply_missing_consumed_args) {
            const locals = self.store.getLocalSpan(args);
            for (0..@min(GuardedList.borrowLen(locals), 64)) |position| {
                const bit = argMaskBit(position);
                if ((rc_effect.consume_args & bit) == 0) continue;
                if (!self.ownsUnit(owned, GuardedList.at(locals, position))) {
                    // The solved signature borrowed this argument. Retaining it
                    // here supplies the unit consumed by the owned operation.
                    preserve_consumed_args |= bit;
                }
            }
        }
        const unique_args = if (want_unique)
            self.uniqueArgsMask(args, rc_effect, target, preserve_consumed_args, owned)
        else
            0;
        const target_consumed = self.maskedArgsContainLocal(args, rc_effect.consume_args, target);
        var release_old_target = false;
        if (target_consumed) {
            _ = self.takeUnit(owned, target);
        } else {
            release_old_target = self.takeRebindTarget(owned, target);
        }
        if (rc_effect.consume_args != 0) {
            self.unsetMaskedArgsExcept(owned, args, rc_effect.consume_args & ~preserve_consumed_args, target);
        }
        var transfer_mask: u64 = 0;
        if (rc_effect.retain_args != 0) {
            transfer_mask = try self.spanTransferMask(args, rc_effect.retain_args, next, target, owned, loop_keep, .yes);
        }
        self.placeUnit(owned, target);
        return .{
            .preserve_consumed_args = preserve_consumed_args,
            .transfer_mask = transfer_mask,
            .unique_args = unique_args,
            .release_old_target = release_old_target,
        };
    }

    /// Terminal consumption (`ret` with an owned return mode, `expect_err`):
    /// returns true when the local's own unit moves out with the terminal,
    /// so no retain is needed.
    fn consumeAtTerminal(self: *Inserter, owned: *OwnedSet, local: LIR.LocalId) bool {
        if (!self.ownsUnit(owned, local)) return false;
        _ = self.takeUnit(owned, local);
        return true;
    }

    fn planValuesInvalidatedByDescriptorUpdate(
        self: *Inserter,
        desc_local: LIR.LocalId,
        owned: *OwnedSet,
        next: LIR.CFStmtId,
        loop_keep: ?LoopKeep,
        releases: *std.ArrayList(ReleaseDecision),
    ) ResourceError!void {
        for (owned.domain.frame_locals) |local| {
            if (!owned.contains(local)) continue;
            if (!self.localUsesDescriptorLocal(local, desc_local)) continue;
            if (try self.valueUsedInPath(next, local, loop_keep)) continue;
            if (!self.takeRebindTarget(owned, local)) arcInvariant("ARC descriptor invalidation lost an owned local");
            try releases.append(self.solve_allocator, self.releaseDecision(local));
        }
    }

    fn localUsesDescriptorLocal(self: *const Inserter, local: LIR.LocalId, desc_local: LIR.LocalId) bool {
        const desc = boxyDescForLocal(self.boxy_rc_descs, local) orelse return false;
        return if (desc.localOrNull()) |local_desc| local_desc == desc_local else false;
    }

    /// Computes which low-level argument positions in `span` (restricted to
    /// `position_mask`) can move their ownership unit into the value being
    /// constructed: the operand is owned and its liveness group has no use
    /// after this statement. Transferred operands leave the owned set.
    /// Whether a transferred operand's unit lands inside the statement's
    /// result (`.yes`, for aggregate and low-level constructions) or is handed
    /// off to a callee that owns it (`.no`, for dict calls).
    const TargetHoldsUnit = enum { yes, no };

    fn spanTransferMask(
        self: *Inserter,
        span: LIR.LocalSpan,
        position_mask: u64,
        next: LIR.CFStmtId,
        target: LIR.LocalId,
        owned: *OwnedSet,
        loop_keep: ?LoopKeep,
        target_holds_unit: TargetHoldsUnit,
    ) ResourceError!u64 {
        var transfer: u64 = 0;
        // Aggregate and low-level transfers move the operand's unit into the
        // result, so a non-refcounted result has nothing to carry it and the
        // operand must be retained instead. A dict call instead hands the unit
        // to the callee, which owns it regardless of the result's layout, so
        // that transfer stays valid even when the result is not refcounted.
        if (target_holds_unit == .yes and !self.localContainsRefcounted(target)) return 0;
        const locals = self.store.getLocalSpan(span);
        for (0..GuardedList.borrowLen(locals)) |i| {
            const local = GuardedList.at(locals, i);
            if (i >= 64) break;
            const bit = argMaskBit(i);
            if ((position_mask & bit) == 0) continue;
            if (local == target) continue;
            if (!self.localContainsRefcounted(local)) continue;
            if (!self.ownsUnit(owned, local)) continue;
            if (try self.groupUsedInPath(next, local, loop_keep)) continue;
            self.unsetOwnedUnit(owned, local);
            transfer |= bit;
        }
        return transfer;
    }

    /// Aggregate variant of `spanTransferMask`. Aggregate spans are not
    /// low-level argument vectors and can be wider than a u64 mask, so the
    /// ArcPlan records transferred positions in stable per-step storage while
    /// the analysis path passes null and keeps only the ownership-state
    /// transition.
    fn spanTransferPositions(
        self: *Inserter,
        span: LIR.LocalSpan,
        next: LIR.CFStmtId,
        target: LIR.LocalId,
        owned: *OwnedSet,
        loop_keep: ?LoopKeep,
        transferred_positions: ?*std.ArrayList(u32),
    ) ResourceError!void {
        if (!self.localContainsRefcounted(target)) return;
        const locals = self.store.getLocalSpan(span);
        for (0..GuardedList.borrowLen(locals)) |i| {
            const local = GuardedList.at(locals, i);
            if (local == target) continue;
            if (!self.localContainsRefcounted(local)) continue;
            if (!self.ownsUnit(owned, local)) continue;
            if (try self.groupUsedInPath(next, local, loop_keep)) continue;
            self.unsetOwnedUnit(owned, local);
            if (transferred_positions) |positions| {
                try positions.append(self.emission_allocator, @intCast(i));
            }
        }
    }

    /// Single-operand variant of `spanTransferMask` for tag payloads and
    /// packed captures.
    fn singleTransfer(
        self: *Inserter,
        local: LIR.LocalId,
        next: LIR.CFStmtId,
        target: LIR.LocalId,
        owned: *OwnedSet,
        loop_keep: ?LoopKeep,
    ) ResourceError!bool {
        if (local == target) return false;
        if (!self.localContainsRefcounted(target)) return false;
        if (!self.localContainsRefcounted(local)) return false;
        if (!self.ownsUnit(owned, local)) return false;
        if (try self.groupUsedInPath(next, local, loop_keep)) return false;
        self.unsetOwnedUnit(owned, local);
        return true;
    }

    /// Releases every owned operand whose liveness group has no use after
    /// this statement, returning the list for emission right after the
    /// statement. When `collected` is null the deaths only leave the owned
    /// set (analysis mirror).
    fn postStmtDeaths(
        self: *Inserter,
        owned: *OwnedSet,
        singles: []const LIR.LocalId,
        span: ?LIR.LocalSpan,
        next: LIR.CFStmtId,
        loop_keep: ?LoopKeep,
        collected: ?*std.ArrayList(ReleaseDecision),
    ) ResourceError!void {
        for (singles) |local| {
            try self.noteDeathIfUnused(owned, local, next, loop_keep, collected);
        }
        if (span) |operand_span| {
            const locals = self.store.getLocalSpan(operand_span);
            for (0..GuardedList.borrowLen(locals)) |index| {
                const local = GuardedList.at(locals, index);
                try self.noteDeathIfUnused(owned, local, next, loop_keep, collected);
            }
        }
    }

    fn canMoveAliasBindValue(
        self: *Inserter,
        owned: *const OwnedSet,
        value: LIR.LocalId,
        target: LIR.LocalId,
        next: LIR.CFStmtId,
        loop_keep: ?LoopKeep,
    ) ResourceError!bool {
        if (!self.ownsUnit(owned, value)) return false;
        if (!self.localContainsRefcounted(value)) return false;
        return !(try self.groupUsedInPathExcept(next, value, target, loop_keep));
    }

    fn noteDeathIfUnused(
        self: *Inserter,
        owned: *OwnedSet,
        local: LIR.LocalId,
        next: LIR.CFStmtId,
        loop_keep: ?LoopKeep,
        collected: ?*std.ArrayList(ReleaseDecision),
    ) ResourceError!void {
        // A borrowed operand's lifetime event belongs to its owning leader:
        // the leader dies when no group member has a later use.
        const owner = self.solution.leaderOf(local);
        if (!owned.contains(owner)) {
            return;
        }
        // Join parameters carry their unit into the join body, whose release
        // statements are not visible to use scans.
        if (self.solution.isJoinParam(owner) or self.solution.isJoinParam(local)) {
            return;
        }
        const used = try self.groupUsedInPath(next, owner, loop_keep);
        if (used) {
            return;
        }
        if (collected) |list| {
            try list.append(self.emission_allocator, self.releaseDecisionFrom(owned, owner));
        }
        owned.unset(owner);
    }

    fn noteCallResultDeathIfUnused(
        self: *Inserter,
        owned: *OwnedSet,
        local: LIR.LocalId,
        ret_mode: arc_sig.Mode,
        next: LIR.CFStmtId,
        loop_keep: ?LoopKeep,
        collected: ?*std.ArrayList(ReleaseDecision),
    ) ResourceError!void {
        if (ret_mode == .owned and self.solution.isBorrowed(local)) {
            try self.noteOwnedLocalDeathIfUnused(owned, local, next, loop_keep, collected);
        } else {
            try self.noteDeathIfUnused(owned, local, next, loop_keep, collected);
        }
    }

    fn noteOwnedLocalDeathIfUnused(
        self: *Inserter,
        owned: *OwnedSet,
        local: LIR.LocalId,
        next: LIR.CFStmtId,
        loop_keep: ?LoopKeep,
        collected: ?*std.ArrayList(ReleaseDecision),
    ) ResourceError!void {
        if (!owned.contains(local)) return;
        if (self.solution.isJoinParam(local)) return;
        if (try self.valueUsedInPath(next, local, loop_keep)) return;
        if (collected) |list| {
            try list.append(self.emission_allocator, self.releaseDecisionFrom(owned, local));
        }
        owned.unset(local);
    }

    fn retainMaskedArgs(self: *Inserter, span: LIR.LocalSpan, mask: u64, next: LIR.CFStmtId) ResourceError!LIR.CFStmtId {
        var current = next;
        const locals = self.store.getLocalSpan(span);
        var i = locals.len;
        while (i > 0) {
            i -= 1;
            if (i >= 64) continue;
            if ((mask & argMaskBit(i)) != 0) {
                current = try self.retainLocalIfRc(GuardedList.at(locals, i), current);
            }
        }
        return current;
    }

    fn retainSpanExcept(self: *Inserter, span: LIR.LocalSpan, skip_mask: u64, next: LIR.CFStmtId) ResourceError!LIR.CFStmtId {
        var current = next;
        const locals = self.store.getLocalSpan(span);
        var i = locals.len;
        while (i > 0) {
            i -= 1;
            if (i < 64 and (skip_mask & argMaskBit(i)) != 0) continue;
            current = try self.retainLocalIfRc(GuardedList.at(locals, i), current);
        }
        return current;
    }

    fn retainArgs(self: *Inserter, args: []const LIR.LocalId, next: LIR.CFStmtId) ResourceError!LIR.CFStmtId {
        var current = next;
        var i = args.len;
        while (i > 0) {
            i -= 1;
            current = try self.retainLocalIfRc(args[i], current);
        }
        return current;
    }

    fn preserveConsumedArgMask(
        self: *Inserter,
        span: LIR.LocalSpan,
        mask: u64,
        result_aliases_mask: u64,
        next: LIR.CFStmtId,
        target: LIR.LocalId,
        loop_keep: ?LoopKeep,
    ) ResourceError!u64 {
        if (mask == 0) return 0;
        var preserve: u64 = 0;
        const locals = self.store.getLocalSpan(span);
        for (0..GuardedList.borrowLen(locals)) |i| {
            const local = GuardedList.at(locals, i);
            if (i >= 64) break;
            const bit = argMaskBit(i);
            if ((mask & bit) == 0) continue;
            if (local == target) continue;
            const used_after = if ((result_aliases_mask & bit) != 0)
                try self.groupUsedInPathExcept(next, local, target, loop_keep)
            else
                try self.groupUsedInPath(next, local, loop_keep);
            if (used_after) {
                preserve |= bit;
            }
        }
        return preserve;
    }

    /// Runtime uniqueness checks proven redundant at this low-level
    /// statement, by argument position: the argument's value is unique in
    /// the current emission view (born with count 1—by a fresh
    /// allocation, a direct call to a unique-returning callee, or a variant
    /// parameter seed—and never given another holder), its single
    /// ownership unit moves into this op (owned here, and not in the
    /// preserve mask, whose positions pay a retain before the op that holds
    /// the count above 1), and no borrow of it is live at the op. Any doubt
    /// leaves a bit zero; the runtime check is always sound.
    fn uniqueArgsMask(
        self: *Inserter,
        span: LIR.LocalSpan,
        rc_effect: LIR.LowLevel.RcEffect,
        target: LIR.LocalId,
        preserve_consumed_args: u64,
        owned: *const OwnedSet,
    ) u64 {
        const check_mask = rc_effect.may_runtime_uniqueness_check_args;
        if (check_mask == 0) return 0;
        var unique: u64 = 0;
        const locals = self.store.getLocalSpan(span);
        for (0..GuardedList.borrowLen(locals)) |i| {
            const local = GuardedList.at(locals, i);
            if (i >= 64) break;
            const bit = argMaskBit(i);
            if ((check_mask & bit) == 0) continue;
            if (local == target) continue;
            if (!self.localContainsRefcounted(local)) continue;
            if (!self.isLocalUniqueHere(local)) continue;
            if ((rc_effect.consume_args & bit) == 0) continue;
            if ((preserve_consumed_args & bit) != 0) continue;
            if (!owned.contains(local)) continue;
            // The preserve scan proved the argument's borrow group dead
            // after this statement; a group member appearing as another
            // operand of this same statement is still live at the op.
            if (self.groupSharesOtherOperand(locals, i, local)) continue;
            unique |= bit;
        }
        return unique;
    }

    /// True when the local's value is statically unique in the current
    /// emission view: solved unique, or a parameter the variant being
    /// emitted seeds born-unique and whose body never adds another holder.
    fn isLocalUniqueHere(self: *const Inserter, local: LIR.LocalId) bool {
        if (self.solution.isUnique(local)) return true;
        if (!self.unique_param_override.contains(local)) return false;
        return !self.solution.isUniqueDestroyed(local);
    }

    /// True when another operand of the same statement belongs to this
    /// argument's borrow group, so the group is still live at the statement
    /// even though no later statement uses it.
    fn groupSharesOtherOperand(
        self: *const Inserter,
        locals: anytype,
        position: usize,
        local: LIR.LocalId,
    ) bool {
        const leader = self.solution.leaderOf(local);
        for (0..GuardedList.borrowLen(locals)) |j| {
            if (j == position) continue;
            const other = GuardedList.at(locals, j);
            if (other == local) return true;
            if (self.solution.leaderOf(other) == leader) return true;
        }
        return false;
    }

    fn procParamCanUseUniqueSeed(
        self: *const Inserter,
        proc_id: LIR.LirProcSpecId,
        position: usize,
    ) bool {
        const bit = arc_sig.paramBit(position) orelse return false;
        return (self.solution.uniqueSeedMaskOf(proc_id) & bit) != 0;
    }

    fn findOutcomeRefinement(
        self: *Inserter,
        target: LIR.LocalId,
        next: LIR.CFStmtId,
        sig: arc_sig.RcSig,
    ) ResourceError!?OutcomeRefinement {
        if (sig.outcomes.isEmpty()) return null;
        var aliases = std.ArrayList(LIR.LocalId).empty;
        try aliases.append(self.emission_allocator, target);
        var discriminant: ?LIR.LocalId = null;
        var cursor = next;
        while (true) {
            const stmt = self.store.getCFStmt(cursor);
            if (stmt == .assign_ref) {
                const assign = stmt.assign_ref;
                switch (assign.op) {
                    .local => |source| {
                        var source_is_result = false;
                        for (aliases.items) |alias| {
                            if (alias == source) {
                                source_is_result = true;
                                break;
                            }
                        }
                        if (!source_is_result) return null;
                        var already = false;
                        for (aliases.items) |alias| already = already or alias == assign.target;
                        if (!already) try aliases.append(self.emission_allocator, assign.target);
                        cursor = assign.next;
                    },
                    .discriminant => |op| {
                        var source_is_result = false;
                        for (aliases.items) |alias| {
                            if (alias == op.source) {
                                source_is_result = true;
                                break;
                            }
                        }
                        if (!source_is_result or discriminant != null) return null;
                        discriminant = assign.target;
                        cursor = assign.next;
                    },
                    .field, .tag_payload, .tag_payload_struct, .list_reinterpret, .nominal => return null,
                }
            } else if (stmt == .switch_stmt) {
                if (discriminant == null or stmt.switch_stmt.cond != discriminant.?) return null;
                return .{ .stmt = cursor };
            } else {
                return null;
            }
        }
    }

    fn outcomeMaskForValue(outcomes: []const arc_sig.Outcome, value: u64) ?arc_sig.ParamMask {
        if (value > std.math.maxInt(u16)) return null;
        for (outcomes) |outcome| {
            if (outcome.discriminant == @as(u16, @intCast(value))) return outcome.restituted_params;
        }
        return null;
    }

    fn outcomeRestitutableMask(self: *const Inserter, sig: arc_sig.RcSig) arc_sig.ParamMask {
        var mask: arc_sig.ParamMask = 0;
        for (self.solution.sigTable().outcomesOf(sig)) |outcome| {
            mask |= outcome.restituted_params;
        }
        return mask;
    }

    /// Representation-transparent producer root used only to reject duplicate
    /// restitutable call arguments in the initial closed capability. Emission's
    /// must-owned set has one bit per ownership place, so two positions rooted
    /// at the same source value cannot be restored independently yet.
    fn outcomeArgumentOrigin(self: *const Inserter, local: LIR.LocalId) LIR.LocalId {
        var cursor = @intFromEnum(local);
        var steps: usize = 0;
        while (cursor < self.solution.alias_source.len and
            self.solution.alias_source[cursor] != no_arc_bit)
        {
            cursor = self.solution.alias_source[cursor];
            steps += 1;
            if (steps > self.solution.alias_source.len) {
                arcInvariant("ARC outcome argument alias chain contained a cycle");
            }
        }
        return @enumFromInt(cursor);
    }

    fn outcomeArgumentsHaveDistinctPlaces(
        self: *const Inserter,
        span: LIR.LocalSpan,
        sig: arc_sig.RcSig,
    ) bool {
        const restitution = self.outcomeRestitutableMask(sig);
        if (restitution == 0) return false;
        const args = self.store.getLocalSpan(span);
        var origins: [arc_sig.tracked_param_count]LIR.LocalId = undefined;
        var origin_count: usize = 0;
        for (0..@min(GuardedList.borrowLen(args), arc_sig.tracked_param_count)) |position| {
            const bit = arc_sig.paramBit(position).?;
            if ((restitution & bit) == 0) continue;
            if (sig.paramMode(position) != .owned) return false;
            const arg = GuardedList.at(args, position);
            if (!self.localContainsRefcounted(arg)) return false;
            const origin = self.outcomeArgumentOrigin(arg);
            for (origins[0..origin_count]) |existing| {
                if (existing == origin) return false;
            }
            origins[origin_count] = origin;
            origin_count += 1;
        }
        return true;
    }

    fn defaultOutcomeMask(
        outcomes: []const arc_sig.Outcome,
        branches: anytype,
    ) ?arc_sig.ParamMask {
        var mask: arc_sig.ParamMask = std.math.maxInt(arc_sig.ParamMask);
        var any = false;
        for (outcomes) |outcome| {
            var explicit = false;
            for (0..GuardedList.borrowLen(branches)) |index| {
                if (GuardedList.at(branches, index).value == outcome.discriminant) {
                    explicit = true;
                    break;
                }
            }
            if (explicit) continue;
            any = true;
            mask &= outcome.restituted_params;
        }
        return if (any) mask else null;
    }

    fn restoredResourceEql(a: RestoredResource, b: RestoredResource) bool {
        return switch (a) {
            .unit => |unit| b == .unit and b.unit == unit,
            .field => |field| b == .field and b.field.root == field.root and b.field.field_mask == field.field_mask,
        };
    }

    fn appendUniqueRestoredResource(
        self: *Inserter,
        resources: *std.ArrayList(RestoredResource),
        resource: RestoredResource,
    ) ResourceError!void {
        for (resources.items) |existing| if (restoredResourceEql(existing, resource)) return;
        try resources.append(self.solve_allocator, resource);
    }

    fn removeRestoredResource(resources: *std.ArrayList(RestoredResource), resource: RestoredResource) void {
        var index: usize = 0;
        while (index < resources.items.len) {
            if (restoredResourceEql(resources.items[index], resource)) {
                _ = resources.orderedRemove(index);
            } else {
                index += 1;
            }
        }
    }

    fn clearOutcomeRestoration(
        self: *Inserter,
        refinement: OutcomeRefinement,
        position: usize,
    ) void {
        const bit = arc_sig.paramBit(position) orelse return;
        const entry = self.restitution_switches.getPtr(refinement.stmt) orelse return;
        const old = entry.position_resources[position] orelse {
            entry.restored_positions &= ~bit;
            return;
        };
        entry.position_resources[position] = null;
        entry.restored_positions &= ~bit;

        // Distinct-place admission normally makes this scan find nothing.
        // Keeping it explicit makes replacement total even for a future
        // extension that lets two positions carry separate receipts for one
        // representation-transparent value.
        for (entry.position_resources) |maybe_resource| {
            if (maybe_resource) |resource| {
                if (restoredResourceEql(resource, old)) return;
            }
        }
        for (entry.branch_resources) |*resources| removeRestoredResource(resources, old);
        removeRestoredResource(&entry.default_resources, old);
    }

    fn registerOutcomeRestoration(
        self: *Inserter,
        refinement: OutcomeRefinement,
        sig: arc_sig.RcSig,
        position: usize,
        resource: RestoredResource,
    ) ResourceError!void {
        const bit = arc_sig.paramBit(position) orelse return;
        const switch_stmt = self.store.getCFStmt(refinement.stmt).switch_stmt;
        const branches = self.store.getCFSwitchBranches(switch_stmt.branches);
        const entry = try self.restitution_switches.getOrPut(self.solve_allocator, refinement.stmt);
        if (!entry.found_existing) {
            const branch_resources = try self.solve_allocator.alloc(std.ArrayList(RestoredResource), GuardedList.borrowLen(branches));
            @memset(branch_resources, .empty);
            entry.value_ptr.* = .{ .branch_resources = branch_resources };
        } else if (entry.value_ptr.branch_resources.len != GuardedList.borrowLen(branches)) {
            arcInvariant("ARC restitution switch changed branch arity");
        }
        if (entry.value_ptr.position_resources[position]) |old| {
            if (restoredResourceEql(old, resource)) {
                entry.value_ptr.restored_positions |= bit;
                return;
            }
            self.clearOutcomeRestoration(refinement, position);
        }
        entry.value_ptr.position_resources[position] = resource;
        const outcomes = self.solution.sigTable().outcomesOf(sig);
        for (0..GuardedList.borrowLen(branches)) |index| {
            const mask = outcomeMaskForValue(outcomes, GuardedList.at(branches, index).value) orelse continue;
            if ((mask & bit) != 0) try self.appendUniqueRestoredResource(&entry.value_ptr.branch_resources[index], resource);
        }
        if (defaultOutcomeMask(outcomes, branches)) |mask| {
            if ((mask & bit) != 0) try self.appendUniqueRestoredResource(&entry.value_ptr.default_resources, resource);
        }
        entry.value_ptr.restored_positions |= bit;
    }

    fn setOutcomeRestoration(self: *Inserter, claim: OutcomeRestitution, active: bool) ResourceError!void {
        if (active) {
            try self.registerOutcomeRestoration(claim.refinement, claim.sig, claim.position, claim.resource);
        } else {
            self.clearOutcomeRestoration(claim.refinement, claim.position);
        }
    }

    fn isAliasOfOwnershipPlace(self: *const Inserter, local: LIR.LocalId, root: LIR.LocalId) bool {
        var cursor = @intFromEnum(local);
        var steps: usize = 0;
        while (true) {
            if (cursor == @intFromEnum(root)) return true;
            if (cursor >= self.solution.alias_source.len) return false;
            const source = self.solution.alias_source[cursor];
            if (source == no_arc_bit) return false;
            cursor = source;
            steps += 1;
            if (steps > self.solution.alias_source.len) arcInvariant("ARC ownership-place alias chain contained a cycle");
        }
    }

    fn spanUsesOwnershipPlace(self: *const Inserter, span: LIR.LocalSpan, root: LIR.LocalId) bool {
        const locals = self.store.getLocalSpan(span);
        for (0..GuardedList.borrowLen(locals)) |index| {
            if (self.isAliasOfOwnershipPlace(GuardedList.at(locals, index), root)) return true;
        }
        return false;
    }

    /// Exact use query for one complete ownership place. Pure container
    /// aliases and non-RC field/discriminant reads do not use its stored unit;
    /// whole-value operands and RC-bearing projections do.
    fn ownershipPlaceUsedInPath(
        self: *Inserter,
        start: LIR.CFStmtId,
        root: LIR.LocalId,
    ) ResourceError!bool {
        var seen = try std.bit_set.DynamicBitSetUnmanaged.initEmpty(self.emission_allocator, self.store.cfStmtCount());
        var stack = std.ArrayList(LIR.CFStmtId).empty;
        try stack.append(self.emission_allocator, start);
        while (stack.pop()) |current| {
            const stmt_index = @intFromEnum(current);
            if (seen.isSet(stmt_index)) continue;
            seen.set(stmt_index);
            switch (self.store.getCFStmt(current)) {
                .assign_ref => |assign| {
                    switch (assign.op) {
                        .local => {},
                        .discriminant => {},
                        .field => |op| if (self.isAliasOfOwnershipPlace(op.source, root) and self.localContainsRefcounted(assign.target)) return true,
                        .tag_payload => |op| if (self.isAliasOfOwnershipPlace(op.source, root) and self.localContainsRefcounted(assign.target)) return true,
                        .tag_payload_struct => |op| if (self.isAliasOfOwnershipPlace(op.source, root) and self.localContainsRefcounted(assign.target)) return true,
                        .list_reinterpret => {},
                        .nominal => {},
                    }
                    try stack.append(self.emission_allocator, assign.next);
                },
                .assign_literal => |assign| try stack.append(self.emission_allocator, assign.next),
                .init_uninitialized => |assign| try stack.append(self.emission_allocator, assign.next),
                .assign_call => |assign| {
                    if (self.spanUsesOwnershipPlace(assign.args, root)) return true;
                    try stack.append(self.emission_allocator, assign.next);
                },
                .assign_call_erased => |assign| {
                    if (self.isAliasOfOwnershipPlace(assign.closure, root) or
                        (assign.reuse_source != null and self.isAliasOfOwnershipPlace(assign.reuse_source.?, root)) or
                        self.spanUsesOwnershipPlace(assign.args, root)) return true;
                    try stack.append(self.emission_allocator, assign.next);
                },
                .assign_packed_erased_fn => |assign| {
                    if ((assign.capture != null and self.isAliasOfOwnershipPlace(assign.capture.?, root)) or
                        (assign.reuse != null and self.isAliasOfOwnershipPlace(assign.reuse.?, root))) return true;
                    try stack.append(self.emission_allocator, assign.next);
                },
                .assign_boxy_desc_ref => |assign| try stack.append(self.emission_allocator, assign.next),
                .assign_boxy_dict_ref => |assign| try stack.append(self.emission_allocator, assign.next),
                .assign_boxy_box => |assign| {
                    if (self.isAliasOfOwnershipPlace(assign.payload, root)) return true;
                    try stack.append(self.emission_allocator, assign.next);
                },
                .assign_boxy_reuse_box => |assign| {
                    if (self.isAliasOfOwnershipPlace(assign.source, root)) return true;
                    try stack.append(self.emission_allocator, assign.next);
                },
                .assign_boxy_unbox => |assign| {
                    if (self.isAliasOfOwnershipPlace(assign.source, root)) return true;
                    try stack.append(self.emission_allocator, assign.next);
                },
                .assign_boxy_adapt => |assign| {
                    if (self.isAliasOfOwnershipPlace(assign.source, root)) return true;
                    try stack.append(self.emission_allocator, assign.next);
                },
                .assign_boxy_inspect => |assign| {
                    if (self.isAliasOfOwnershipPlace(assign.source, root)) return true;
                    try stack.append(self.emission_allocator, assign.next);
                },
                .assign_boxy_eq => |assign| {
                    if (self.isAliasOfOwnershipPlace(assign.lhs, root) or self.isAliasOfOwnershipPlace(assign.rhs, root)) return true;
                    try stack.append(self.emission_allocator, assign.next);
                },
                .assign_boxy_tag => |assign| {
                    if (assign.payload != null and self.isAliasOfOwnershipPlace(assign.payload.?, root)) return true;
                    try stack.append(self.emission_allocator, assign.next);
                },
                .assign_boxy_tag_payload => |assign| {
                    if (self.isAliasOfOwnershipPlace(assign.source, root)) return true;
                    try stack.append(self.emission_allocator, assign.next);
                },
                .assign_call_dict => |assign| {
                    if (self.spanUsesOwnershipPlace(assign.args, root) or self.spanUsesOwnershipPlace(assign.hidden_args, root)) return true;
                    try stack.append(self.emission_allocator, assign.next);
                },
                .assign_low_level => |assign| {
                    if (self.spanUsesOwnershipPlace(assign.args, root)) return true;
                    try stack.append(self.emission_allocator, assign.next);
                },
                .assign_list => |assign| {
                    if (self.spanUsesOwnershipPlace(assign.elems, root)) return true;
                    try stack.append(self.emission_allocator, assign.next);
                },
                .assign_struct => |assign| {
                    if (self.spanUsesOwnershipPlace(assign.fields, root)) return true;
                    try stack.append(self.emission_allocator, assign.next);
                },
                .assign_tag => |assign| {
                    if (assign.payload != null and self.isAliasOfOwnershipPlace(assign.payload.?, root)) return true;
                    try stack.append(self.emission_allocator, assign.next);
                },
                .store_struct => |assign| {
                    if (self.isAliasOfOwnershipPlace(assign.dest, root) or self.spanUsesOwnershipPlace(assign.fields, root)) return true;
                    try stack.append(self.emission_allocator, assign.next);
                },
                .store_tag => |assign| {
                    if (self.isAliasOfOwnershipPlace(assign.dest, root) or
                        (assign.payload != null and self.isAliasOfOwnershipPlace(assign.payload.?, root))) return true;
                    try stack.append(self.emission_allocator, assign.next);
                },
                .set_local => |assign| {
                    if (self.isAliasOfOwnershipPlace(assign.value, root)) return true;
                    // Rebinding the place ends this definition. Uses reached
                    // through the following jump belong to the newly written
                    // join value, not to the value whose projection is being
                    // considered here.
                    if (assign.target == root) continue;
                    try stack.append(self.emission_allocator, assign.next);
                },
                .debug => |stmt| {
                    if (self.isAliasOfOwnershipPlace(stmt.message, root)) return true;
                    try stack.append(self.emission_allocator, stmt.next);
                },
                .expect => |stmt| {
                    if (self.isAliasOfOwnershipPlace(stmt.condition, root)) return true;
                    try stack.append(self.emission_allocator, stmt.next);
                },
                .comptime_branch_taken => |stmt| try stack.append(self.emission_allocator, stmt.next),
                .incref => |stmt| {
                    if (self.isAliasOfOwnershipPlace(stmt.value, root)) return true;
                    try stack.append(self.emission_allocator, stmt.next);
                },
                .decref, .decref_if_initialized, .free => return true,
                .switch_stmt => |stmt| {
                    if (self.isAliasOfOwnershipPlace(stmt.cond, root)) return true;
                    if (stmt.continuation) |continuation| try stack.append(self.emission_allocator, continuation);
                    const branches = self.store.getCFSwitchBranches(stmt.branches);
                    for (0..GuardedList.borrowLen(branches)) |index| try stack.append(self.emission_allocator, GuardedList.at(branches, index).body);
                    try stack.append(self.emission_allocator, stmt.default_branch);
                },
                .switch_initialized_payload => |stmt| {
                    if (self.isAliasOfOwnershipPlace(stmt.cond, root) or self.isAliasOfOwnershipPlace(stmt.payload, root)) return true;
                    try stack.append(self.emission_allocator, stmt.initialized_branch);
                    try stack.append(self.emission_allocator, stmt.uninitialized_branch);
                },
                .str_match, .str_match_set, .boxy_tag_match => return true,
                .join => |stmt| try stack.append(self.emission_allocator, stmt.remainder),
                .jump => {
                    const join_index = self.solution.jumpTargetJoinIndexOf(current);
                    const joins = self.solution.joinBodiesOf(self.current_source_proc);
                    if (join_index >= joins.len) arcInvariant("ARC ownership-place use query exceeded its join table");
                    try stack.append(self.emission_allocator, joins[join_index].body);
                },
                .ret => |stmt| if (self.isAliasOfOwnershipPlace(stmt.value, root)) return true,
                .crash, .expect_err => return true,
                // An implicit loop boundary hands the kept value to either
                // the next iteration or the code after the loop. A root
                // rebind encountered earlier stopped this path before it
                // could get here.
                .loop_continue, .loop_break => return true,
                .runtime_error, .comptime_exhaustiveness_failed => {},
            }
        }
        return false;
    }

    fn outcomeRestitutionGuard(
        self: *Inserter,
        local: LIR.LocalId,
        resource: RestoredResource,
        position: usize,
        target: LIR.LocalId,
        next: LIR.CFStmtId,
        sig: arc_sig.RcSig,
    ) ResourceError!?OutcomeRestitution {
        const bit = arc_sig.paramBit(position) orelse return null;
        const refinement = try self.findOutcomeRefinement(target, next, sig) orelse return null;
        const switch_stmt = self.store.getCFStmt(refinement.stmt).switch_stmt;
        const branches = self.store.getCFSwitchBranches(switch_stmt.branches);
        const outcomes = self.solution.sigTable().outcomesOf(sig);
        var has_restored_outcome = false;
        for (0..GuardedList.borrowLen(branches)) |index| {
            const branch = GuardedList.at(branches, index);
            const mask = outcomeMaskForValue(outcomes, branch.value) orelse continue;
            if ((mask & bit) != 0) {
                has_restored_outcome = true;
            } else if (try self.ownershipPlaceUsedInPath(branch.body, local)) {
                return null;
            }
        }
        if (defaultOutcomeMask(outcomes, branches)) |mask| {
            if ((mask & bit) != 0) {
                has_restored_outcome = true;
            } else if (try self.ownershipPlaceUsedInPath(switch_stmt.default_branch, local)) {
                return null;
            }
        }
        if (!has_restored_outcome) return null;
        return .{
            .refinement = refinement,
            .sig = sig,
            .position = position,
            .resource = resource,
        };
    }

    fn callArgOwnership(
        self: *Inserter,
        call_stmt: ?LIR.CFStmtId,
        callee: ?LIR.LirProcSpecId,
        owned: *OwnedSet,
        callee_sig: arc_sig.RcSig,
        unique_demand: bool,
        span: LIR.LocalSpan,
        next: LIR.CFStmtId,
        target: LIR.LocalId,
        loop_keep: ?LoopKeep,
    ) ResourceError!CallArgOwnership {
        self.retain_arg_scratch.clearRetainingCapacity();
        var demanded = callee_sig;
        const locals = self.store.getLocalSpan(span);
        var outcome_sig = callee_sig;
        if (callee) |direct| outcome_sig.outcomes = self.solution.availableOutcomeSpanOf(direct);
        const outcome_places_distinct = !outcome_sig.outcomes.isEmpty() and
            self.outcomeArgumentsHaveDistinctPlaces(span, outcome_sig);
        const outcome_mask = self.outcomeRestitutableMask(outcome_sig);
        const outcome_refinement = if (outcome_places_distinct)
            try self.findOutcomeRefinement(target, next, outcome_sig)
        else
            null;

        // One outcome span is an atomic calling convention. Preflight every
        // position named by any row before changing the call-entry ownership
        // state. Selecting the span for one argument also makes the callee
        // return every other named argument on its restituting outcomes, even
        // when the caller only needs to release that returned unit.
        var outcome_required = false;
        var outcome_admissible = outcome_refinement != null and outcome_mask != 0;
        var unchecked_outcome_positions = outcome_mask;
        if (outcome_refinement) |refinement| {
            for (0..arc_sig.tracked_param_count) |position| {
                self.clearOutcomeRestoration(refinement, position);
            }
        }
        for (0..@min(GuardedList.borrowLen(locals), arc_sig.tracked_param_count)) |position| {
            const bit = arc_sig.paramBit(position).?;
            if ((outcome_mask & bit) == 0) continue;
            unchecked_outcome_positions &= ~bit;
            const local = GuardedList.at(locals, position);
            if (!self.localContainsRefcounted(local) or callee_sig.paramMode(position) != .owned) {
                outcome_admissible = false;
                continue;
            }
            const owner = self.unitOf(local);
            const field_receipt = if (call_stmt) |stmt_id|
                self.fieldRestitutionForEmission(stmt_id, position)
            else
                null;
            const used_after_call = field_receipt != null or
                (local != target and try self.groupUsedInPath(next, local, loop_keep));
            const restored_resource: RestoredResource = if (field_receipt) |receipt|
                if (owned.contains(receipt.place.root))
                    .{ .field = receipt.place }
                else
                    .{ .unit = owner }
            else
                .{ .unit = owner };
            const restitution: ?OutcomeRestitution = if (field_receipt) |receipt| blk: {
                if (outcome_refinement == null or receipt.refinement != outcome_refinement.?.stmt) break :blk null;
                break :blk .{
                    .refinement = outcome_refinement.?,
                    .sig = outcome_sig,
                    .position = position,
                    .resource = restored_resource,
                };
            } else try self.outcomeRestitutionGuard(
                local,
                restored_resource,
                position,
                target,
                next,
                outcome_sig,
            );
            const projected_alias_conflict = self.dismantles.projectionUnitOf(local) != null and
                self.groupSharesOtherOperand(locals, position, local);
            const can_transfer = owned.contains(owner) and
                (!used_after_call or restitution != null) and
                !projected_alias_conflict;
            if (used_after_call and restitution != null) outcome_required = true;
            if (restitution == null or !can_transfer) outcome_admissible = false;
        }
        outcome_admissible = outcome_admissible and outcome_required and unchecked_outcome_positions == 0;
        if (outcome_admissible) demanded.outcomes = outcome_sig.outcomes;

        for (0..GuardedList.borrowLen(locals)) |position| {
            const local = GuardedList.at(locals, position);
            if (!self.localContainsRefcounted(local)) continue;
            if (callee_sig.paramMode(position) == .borrowed) {
                // Borrowed positions keep the caller's ownership untouched.
                // With specialization enabled, a final-use argument only
                // moves into an owned-demanding variant when that changes
                // runtime work inside the callee. Merely moving the caller's
                // post-call release into a clone preserves the same RC work
                // while growing live code.
                const bit = arc_sig.paramBit(position) orelse continue;
                const enables_field_take = if (callee) |direct|
                    (self.dismantles.ownedOnlyParamBenefits(direct) & bit) != 0
                else
                    false;
                // Field-take variants are a correctness-preserving ownership
                // schedule, not optional optimization work: without
                // them a complete payload move is forced to manufacture a
                // second unit and defeats runtime uniqueness. General return
                // and born-unique variants remain opt-in.
                if (!self.variants.enabled and !enables_field_take) continue;
                const used_after_call = local != target and try self.groupUsedInPath(next, local, loop_keep);
                const owner = self.unitOf(local);
                const projected_alias_conflict = self.dismantles.projectionUnitOf(local) != null and
                    self.groupSharesOtherOperand(locals, position, local);
                const can_transfer = owned.contains(owner) and !used_after_call and !projected_alias_conflict;
                if (!can_transfer) continue;
                const return_borrows_param = callee_sig.ret_mode == .borrowed and (callee_sig.ret_lenders & bit) != 0;
                const seed_can_reach_check = if (callee) |direct| self.procParamCanUseUniqueSeed(direct, position) else false;
                const seeds_unique_param = unique_demand and seed_can_reach_check and self.isLocalUniqueHere(local) and
                    !self.groupSharesOtherOperand(locals, position, local);
                if (!return_borrows_param and !seeds_unique_param and !enables_field_take) continue;
                demanded.borrowed_params &= ~bit;
                if (return_borrows_param) {
                    demanded.ret_mode = .owned;
                    demanded.ret_lenders = 0;
                }
                if (seeds_unique_param) {
                    demanded.unique_params |= bit;
                }
                owned.unset(owner);
                continue;
            }

            const owner = self.unitOf(local);
            const field_receipt = if (call_stmt) |stmt_id|
                self.fieldRestitutionForEmission(stmt_id, position)
            else
                null;
            const used_after_call = field_receipt != null or
                (local != target and try self.groupUsedInPath(next, local, loop_keep));
            const restored_resource: RestoredResource = if (field_receipt) |receipt| blk: {
                // A live residual shell can receive its field back. Once the
                // root resource has ended, the projection binding is the
                // returned unit's only exact carrier and must be settled as
                // an ordinary standalone unit on the restituting edge.
                break :blk if (owned.contains(receipt.place.root))
                    .{ .field = receipt.place }
                else
                    .{ .unit = owner };
            } else .{ .unit = owner };
            const restitution: ?OutcomeRestitution = if (!outcome_admissible)
                null
            else if (field_receipt) |receipt| blk: {
                break :blk .{
                    .refinement = .{ .stmt = receipt.refinement },
                    .sig = outcome_sig,
                    .position = position,
                    .resource = restored_resource,
                };
            } else if (callee != null and outcome_places_distinct and
                (outcome_mask & (arc_sig.paramBit(position) orelse 0)) != 0)
                try self.outcomeRestitutionGuard(
                    local,
                    restored_resource,
                    position,
                    target,
                    next,
                    outcome_sig,
                )
            else
                null;
            const projected_alias_conflict = self.dismantles.projectionUnitOf(local) != null and
                self.groupSharesOtherOperand(locals, position, local);
            const can_transfer = owned.contains(owner) and (!used_after_call or restitution != null) and !projected_alias_conflict;
            if (restitution) |claim| try self.setOutcomeRestoration(claim, can_transfer);

            if (can_transfer) {
                // A dying argument moving into an owned position that is
                // statically unique with no borrow live at the call demands
                // a variant whose parameter is seeded born-unique, so
                // checked ops it reaches in the body go check-free.
                const seed_can_reach_check = if (position < arc_sig.tracked_param_count) blk: {
                    const direct = callee orelse break :blk false;
                    break :blk self.procParamCanUseUniqueSeed(direct, position);
                } else false;
                if (unique_demand and seed_can_reach_check and self.isLocalUniqueHere(local) and
                    !self.groupSharesOtherOperand(locals, position, local))
                {
                    demanded.unique_params |= arc_sig.paramBit(position).?;
                }
                owned.unset(owner);
            } else {
                try self.retain_arg_scratch.append(self.emission_allocator, local);
            }
        }

        return .{
            .retain_args = self.retain_arg_scratch.items,
            .demanded = demanded,
        };
    }

    /// Resolves the proc a call site targets: the original callee when the
    /// demanded vector matches its solved signature, or a mode-specialized
    /// variant emitted for the demanded vector.
    fn variantForCall(
        self: *Inserter,
        callee: LIR.LirProcSpecId,
        demanded: arc_sig.RcSig,
    ) ResourceError!?LIR.LirProcSpecId {
        const solved = self.solution.sigOf(callee);
        if (demanded.borrowed_params == solved.borrowed_params and
            demanded.ret_mode == solved.ret_mode and
            demanded.unique_params == 0 and
            demanded.outcomes.isEmpty())
        {
            return null;
        }
        const selector = VariantSelector{
            .source = callee,
            .borrowed_params = demanded.borrowed_params,
            .ret_mode = demanded.ret_mode,
            .unique_params = demanded.unique_params,
            .outcomes = demanded.outcomes,
        };
        const entry = try self.variants.map.getOrPut(selector);
        if (entry.found_existing) return entry.value_ptr.*;

        const source_spec = self.store.getProcSpec(callee);
        const variant = try self.store.addProcSpec(.{
            .name = self.store.freshSyntheticSymbol(),
            .args = source_spec.args,
            .erased_reuse_arg = source_spec.erased_reuse_arg,
            .erased_call_args = source_spec.erased_call_args,
            .frame_locals = self.variants.original_frame_locals[@intFromEnum(callee)],
            .body = self.variants.original_bodies[@intFromEnum(callee)],
            .ret_layout = source_spec.ret_layout,
            .ret_desc = source_spec.ret_desc,
            .runtime_ret_desc = source_spec.runtime_ret_desc,
            .erased_arg_desc_offsets = source_spec.erased_arg_desc_offsets,
            .erased_arg_layouts = source_spec.erased_arg_layouts,
            .erased_arg_desc_params = source_spec.erased_arg_desc_params,
            .erased_capture_arg = source_spec.erased_capture_arg,
            .abi = source_spec.abi,
            .boxy_runtime_entry = source_spec.boxy_runtime_entry,
            .is_static_initializer = source_spec.is_static_initializer,
            .hosted = source_spec.hosted,
            .tail_transform = source_spec.tail_transform,
            .stack_probe = source_spec.stack_probe,
        });
        try self.store.copyProcDebugInfo(variant, callee);
        entry.value_ptr.* = variant;
        try self.variants.sigs.append(self.store.allocator, demanded);
        try self.variants.queue.append(self.store.allocator, .{
            .variant = variant,
            .source = callee,
            .sig = demanded,
        });
        return variant;
    }

    fn maskedArgsContainLocal(self: *Inserter, span: LIR.LocalSpan, mask: u64, needle: LIR.LocalId) bool {
        if (mask == 0) return false;
        const locals = self.store.getLocalSpan(span);
        for (0..GuardedList.borrowLen(locals)) |i| {
            const local = GuardedList.at(locals, i);
            if (i >= 64) break;
            if ((mask & argMaskBit(i)) != 0 and local == needle) return true;
        }
        return false;
    }

    fn unsetMaskedArgsExcept(
        self: *Inserter,
        owned: *OwnedSet,
        span: LIR.LocalSpan,
        mask: u64,
        except: LIR.LocalId,
    ) void {
        if (mask == 0) return;
        const locals = self.store.getLocalSpan(span);
        for (0..GuardedList.borrowLen(locals)) |i| {
            const local = GuardedList.at(locals, i);
            if (i >= 64) break;
            if ((mask & argMaskBit(i)) != 0 and local != except) {
                self.unsetOwnedUnit(owned, local);
            }
        }
    }

    fn recordFinalJoin(self: *Inserter, join_index: u32, join_point: LIR.JoinPoint) ResourceError!void {
        if (join_index >= self.final_joins.len) arcInvariant("ARC emitted a join beyond its lifted final-join table");
        if (self.final_joins[join_index]) |existing| {
            if (joinPointEql(existing, join_point)) return;
            arcInvariant("ARC final join-point output saw one join id with different data");
        }
        self.final_joins[join_index] = join_point;
    }

    fn finishFinalJoinPoints(self: *Inserter) ResourceError!LIR.JoinPointSpan {
        var count: usize = 0;
        for (self.final_joins) |join| count += @intFromBool(join != null);
        if (count == 0) return LIR.JoinPointSpan.empty();
        const sorted = try self.emission_allocator.alloc(LIR.JoinPoint, count);
        var next: usize = 0;
        for (self.final_joins) |maybe_join| {
            const join = maybe_join orelse continue;
            sorted[next] = join;
            next += 1;
        }
        std.mem.sort(LIR.JoinPoint, sorted, {}, joinPointLessThan);
        return try self.store.addJoinPointSpan(sorted);
    }

    /// Kill applied to one successor edge only: value-use bits of string
    /// match captures die on the match edge (the capture is rebound there),
    /// while the miss edge still exposes earlier bindings' uses.
    const ReadBeforeRebindEdgeKill = struct {
        successor_offset: u32,
        bit: u32,
    };

    const ReadBeforeRebindNode = struct {
        stmt: LIR.CFStmtId,
        reads: ExactBitSet,
        exposed: ExactBitSet,
        successor_start: usize,
        successor_len: u32,
        def: ?LIR.LocalId,
        edge_kills: []const ReadBeforeRebindEdgeKill = &.{},
    };

    const ReadBeforeRebindGraph = struct {
        allocator: Allocator,
        nodes: std.ArrayList(ReadBeforeRebindNode),
        /// Node indices, resolved exactly once while each edge is appended.
        successors: std.ArrayList(u32),
        predecessor_starts: []const usize = &.{},
        predecessors: []const usize = &.{},
        loop_edges: []const usize = &.{},
        /// Compact node bits whose forward paths can reach a loop boundary.
        reaches_loop_edge: std.bit_set.DynamicBitSetUnmanaged = .{},

        fn init(allocator: Allocator) ReadBeforeRebindGraph {
            return .{
                .allocator = allocator,
                .nodes = .empty,
                .successors = .empty,
            };
        }
    };

    fn ensureReadBeforeRebindNode(
        self: *Inserter,
        graph: *ReadBeforeRebindGraph,
        work: *std.ArrayList(u32),
        stmt: LIR.CFStmtId,
    ) ResourceError!u32 {
        const stmt_index = @intFromEnum(stmt);
        if (stmt_index >= self.stmt_node_indices.len) {
            arcInvariant("ARC liveness reached a generated statement");
        }
        if (self.stmt_node_indices[stmt_index] != no_stmt_node_index) {
            return self.stmt_node_indices[stmt_index];
        }

        var reads = try ExactBitSet.initEmpty(graph.allocator, self.domain().livenessBitLen());
        errdefer reads.deinit(graph.allocator);
        var exposed = try ExactBitSet.initEmpty(graph.allocator, self.domain().livenessBitLen());
        errdefer exposed.deinit(graph.allocator);

        const index = graph.nodes.items.len;
        if (index >= no_stmt_node_index) arcInvariant("ARC liveness graph exceeded its node index representation");

        try graph.nodes.append(graph.allocator, .{
            .stmt = stmt,
            .reads = reads,
            .exposed = exposed,
            .successor_start = 0,
            .successor_len = 0,
            .def = null,
        });
        self.stmt_node_indices[stmt_index] = @intCast(index);
        try work.append(graph.allocator, @intCast(index));
        return @intCast(index);
    }

    fn appendReadBeforeRebindSuccessor(
        self: *Inserter,
        graph: *ReadBeforeRebindGraph,
        work: *std.ArrayList(u32),
        node_index: usize,
        successor: LIR.CFStmtId,
    ) ResourceError!void {
        const successor_node = try self.ensureReadBeforeRebindNode(graph, work, successor);
        const successor_index = graph.successors.items.len;
        if (graph.nodes.items[node_index].successor_len == 0) {
            graph.nodes.items[node_index].successor_start = successor_index;
        }
        try graph.successors.append(graph.allocator, successor_node);
        graph.nodes.items[node_index].successor_len += 1;
    }

    /// Raw liveness-bit position for a refcounted resource local or one of
    /// its explicit solved ownership-unit / borrow-group representatives.
    fn rawLivenessBitOf(self: *const Inserter, local: LIR.LocalId) ?usize {
        return self.domain().resourceBitOf(local);
    }

    fn noteReadBeforeRebindLocal(self: *const Inserter, reads: *ExactBitSet, local: LIR.LocalId) void {
        if (self.rawLivenessBitOf(local)) |bit| reads.set(bit);
    }

    /// Group-bit position of a local's multi-member borrow group, if any.
    fn groupBitOf(self: *const Inserter, local: LIR.LocalId) ?usize {
        const leader = self.solution.leaderOf(local);
        return self.domain().groupBitOf(leader);
    }

    /// Value-use bit position of a borrowed call-result local, if any.
    fn valueUseBitOf(self: *const Inserter, local: LIR.LocalId) ?usize {
        return self.domain().valueUseBitOf(local);
    }

    /// Records a value use: the raw read-before-rebind bit, the local's
    /// group bit, and its value-use bit. Reference-count statements record
    /// only the raw bit through `noteReadBeforeRebindLocal`: they must not
    /// extend group or call-result liveness.
    fn noteLivenessUseLocal(self: *const Inserter, reads: *ExactBitSet, local: LIR.LocalId) void {
        if (self.rawLivenessBitOf(local)) |bit| reads.set(bit);
        if (self.groupBitOf(local)) |bit| reads.set(bit);
        if (self.valueUseBitOf(local)) |bit| reads.set(bit);
    }

    fn noteLivenessUseSpan(self: *const Inserter, reads: *ExactBitSet, span: LIR.LocalSpan) void {
        const locals = self.store.getLocalSpan(span);
        for (0..GuardedList.borrowLen(locals)) |index| {
            const local = GuardedList.at(locals, index);
            self.noteLivenessUseLocal(reads, local);
        }
    }

    fn noteLivenessUseRefOp(self: *const Inserter, reads: *ExactBitSet, op: LIR.RefOp) void {
        self.noteLivenessUseLocal(reads, refOpSource(op));
    }

    /// Records the loop keep-set as reads at a `loop_continue`/`loop_break`:
    /// kept units, groups with any kept member, and kept call-result locals
    /// all stay live across the loop edge.
    fn noteLivenessLoopKeep(self: *const Inserter, reads: *ExactBitSet, keep: *const OwnedSet) void {
        // Every kept unit reads as a value use, which also sets its group
        // and value-use bits, so the loop edge keeps a group alive whenever
        // any member is kept.
        var keep_iter = keep.bits.iterator(.{});
        while (keep_iter.next()) |kept| {
            self.noteLivenessUseLocal(reads, keep.domain.resourceLocalAt(kept));
        }
    }

    fn setReadBeforeRebindDef(
        graph: *ReadBeforeRebindGraph,
        node_index: usize,
        local: LIR.LocalId,
    ) void {
        graph.nodes.items[node_index].def = local;
    }

    /// Records value-use kills for one string-match arm: a capture view is
    /// rebound on that arm's match edge, so the previous binding's value-use
    /// liveness does not flow back through it.
    fn attachStrMatchEdgeKills(
        self: *Inserter,
        graph: *ReadBeforeRebindGraph,
        node_index: usize,
        steps_span: LIR.StrMatchStepSpan,
        successor_offset: u32,
    ) ResourceError!void {
        var kills = std.ArrayList(ReadBeforeRebindEdgeKill).empty;
        const steps = self.store.getStrMatchSteps(steps_span);
        for (0..GuardedList.borrowLen(steps)) |step_index| {
            const step = GuardedList.at(steps, step_index);
            switch (step.capture) {
                .discard => {},
                .view => |local| if (self.valueUseBitOf(local)) |bit| {
                    try kills.append(graph.allocator, .{
                        .successor_offset = successor_offset,
                        .bit = @intCast(bit),
                    });
                },
            }
        }
        if (kills.items.len == 0) return;
        const existing = graph.nodes.items[node_index].edge_kills;
        const merged = try graph.allocator.alloc(ReadBeforeRebindEdgeKill, existing.len + kills.items.len);
        @memcpy(merged[0..existing.len], existing);
        @memcpy(merged[existing.len..], kills.items);
        graph.nodes.items[node_index].edge_kills = merged;
    }

    /// Makes the reusable dense statement lookup name exactly one immutable
    /// source graph. Switching source procs clears only nodes touched by the
    /// previous graph and installs only nodes in the next; persisted rows stay
    /// in their graph's arena and variants of one source never remap.
    fn activateCurrentLivenessGraph(self: *Inserter) void {
        if (self.active_liveness_source != null and self.active_liveness_source.? == self.current_source_proc) return;
        if (self.active_liveness_source) |previous_source| {
            const previous_index = @intFromEnum(previous_source);
            if (previous_index >= self.liveness_graphs.len) arcInvariant("ARC active liveness source exceeded its graph table");
            if (self.liveness_graphs[previous_index]) |*previous| {
                for (previous.nodes.items) |node| self.stmt_node_indices[@intFromEnum(node.stmt)] = no_stmt_node_index;
            }
        }
        const source_index = @intFromEnum(self.current_source_proc);
        if (source_index >= self.liveness_graphs.len) arcInvariant("ARC liveness source proc exceeded its graph table");
        if (self.liveness_graphs[source_index]) |*graph| {
            for (graph.nodes.items, 0..) |node, node_index| {
                if (node_index >= no_stmt_node_index) arcInvariant("ARC liveness graph exceeded its node index representation");
                const stmt_index = @intFromEnum(node.stmt);
                if (self.stmt_node_indices[stmt_index] != no_stmt_node_index) {
                    arcInvariant("ARC liveness graph activation overlapped a previous source graph");
                }
                self.stmt_node_indices[stmt_index] = @intCast(node_index);
            }
        }
        self.active_liveness_source = self.current_source_proc;
    }

    fn computeReadsBeforeRebind(
        self: *Inserter,
        start: LIR.CFStmtId,
        loop_keep: ?LoopKeep,
        loop_keep_id: u32,
    ) ResourceError!*const ExactBitSet {
        self.activateCurrentLivenessGraph();
        if (loop_keep_id == 0) {
            const stmt_index = @intFromEnum(start);
            if (stmt_index >= self.stmt_node_indices.len) arcInvariant("ARC liveness queried a generated statement");
            const source_index = @intFromEnum(self.current_source_proc);
            if (self.liveness_graphs[source_index]) |*graph| {
                const node_index = self.stmt_node_indices[stmt_index];
                if (node_index == no_stmt_node_index or node_index >= graph.nodes.items.len) {
                    arcInvariant("ARC keep-free liveness start was outside its source graph");
                }
                return &graph.nodes.items[node_index].exposed;
            }
        } else {
            if (loop_keep_id >= self.loop_liveness_caches.items.len) {
                arcInvariant("ARC liveness queried an unknown loop identity");
            }
            const cache = &self.loop_liveness_caches.items[loop_keep_id];
            if (cache.initialized) {
                if (cache.dirty) {
                    const keep = loop_keep orelse arcInvariant("ARC dirty loop cache was refreshed without its boundary facts");
                    try self.refreshLoopReadsBeforeRebind(cache, keep);
                }
                const stmt_index = @intFromEnum(start);
                if (stmt_index >= self.stmt_node_indices.len) arcInvariant("ARC loop liveness queried a generated statement");
                const node_index = self.stmt_node_indices[stmt_index];
                if (node_index == no_stmt_node_index or node_index >= cache.rows.len) {
                    arcInvariant("ARC loop liveness start was outside its source graph");
                }
                return if (cache.rows[node_index]) |*cached|
                    cached
                else
                    arcInvariant("ARC loop cache omitted a queried active node");
            }
        }

        if (loop_keep) |keep| {
            return self.computeLoopReadsBeforeRebind(start, keep, loop_keep_id);
        }

        const source_index = @intFromEnum(self.current_source_proc);
        if (source_index >= self.liveness_graphs.len) arcInvariant("ARC liveness source proc exceeded its graph table");
        const graph_slot = &self.liveness_graphs[source_index];
        if (graph_slot.* != null) arcInvariant("ARC keep-free liveness graph existed without its requested row");
        graph_slot.* = ReadBeforeRebindGraph.init(self.liveness_allocator);
        var graph = graph_slot.*.?;
        const graph_allocator = graph.allocator;
        var work = std.ArrayList(u32).empty;
        // Loop-edge nodes found by a keep-free build seed the backward
        // reachability sweep that decides which statements ever need
        // loop-keyed rows.
        var loop_edge_nodes = std.ArrayList(usize).empty;

        // Without a loop keep-set, start from the whole proc so one dataflow
        // run answers most repeated local/group liveness questions. With a
        // loop keep-set, the keep-set semantics belong to one loop body, so
        // root the graph at the queried statement instead of applying that
        // loop's exits to unrelated loops elsewhere in the proc.
        _ = try self.ensureReadBeforeRebindNode(&graph, &work, self.current_proc_body);
        _ = try self.ensureReadBeforeRebindNode(&graph, &work, start);

        while (work.pop()) |node_index_u32| {
            const node_index: usize = node_index_u32;
            const stmt = graph.nodes.items[node_index].stmt;

            switch (self.store.getCFStmt(stmt)) {
                .assign_ref => |assign| {
                    self.noteLivenessUseRefOp(&graph.nodes.items[node_index].reads, assign.op);
                    setReadBeforeRebindDef(&graph, node_index, assign.target);
                    try self.appendReadBeforeRebindSuccessor(&graph, &work, node_index, assign.next);
                },
                .assign_literal => |assign| {
                    setReadBeforeRebindDef(&graph, node_index, assign.target);
                    try self.appendReadBeforeRebindSuccessor(&graph, &work, node_index, assign.next);
                },
                .init_uninitialized => |init| {
                    setReadBeforeRebindDef(&graph, node_index, init.target);
                    try self.appendReadBeforeRebindSuccessor(&graph, &work, node_index, init.next);
                },
                .assign_call => |assign| {
                    self.noteLivenessUseSpan(&graph.nodes.items[node_index].reads, assign.args);
                    if (assign.result_desc) |result_desc| {
                        if (result_desc.localOrNull()) |local| self.noteLivenessUseLocal(&graph.nodes.items[node_index].reads, local);
                    }
                    setReadBeforeRebindDef(&graph, node_index, assign.target);
                    try self.appendReadBeforeRebindSuccessor(&graph, &work, node_index, assign.next);
                },
                .assign_call_erased => |assign| {
                    self.noteLivenessUseLocal(&graph.nodes.items[node_index].reads, assign.closure);
                    if (assign.reuse_source) |reuse_source| self.noteLivenessUseLocal(&graph.nodes.items[node_index].reads, reuse_source);
                    self.noteLivenessUseSpan(&graph.nodes.items[node_index].reads, assign.args);
                    self.noteLivenessUseSpan(&graph.nodes.items[node_index].reads, assign.arg_descs);
                    if (assign.result_desc) |result_desc| {
                        if (result_desc.localOrNull()) |local| self.noteLivenessUseLocal(&graph.nodes.items[node_index].reads, local);
                    }
                    setReadBeforeRebindDef(&graph, node_index, assign.target);
                    if (assign.out_desc) |out_desc| setReadBeforeRebindDef(&graph, node_index, out_desc);
                    try self.appendReadBeforeRebindSuccessor(&graph, &work, node_index, assign.next);
                },
                .assign_packed_erased_fn => |assign| {
                    if (assign.capture) |capture| self.noteLivenessUseLocal(&graph.nodes.items[node_index].reads, capture);
                    if (assign.reuse) |reuse| self.noteLivenessUseLocal(&graph.nodes.items[node_index].reads, reuse);
                    setReadBeforeRebindDef(&graph, node_index, assign.target);
                    try self.appendReadBeforeRebindSuccessor(&graph, &work, node_index, assign.next);
                },
                .assign_boxy_desc_ref => |assign| {
                    if (assign.desc.localOrNull()) |local| self.noteLivenessUseLocal(&graph.nodes.items[node_index].reads, local);
                    if (assign.tag_residual_for) |desc| if (desc.localOrNull()) |local| self.noteLivenessUseLocal(&graph.nodes.items[node_index].reads, local);
                    self.noteLivenessUseSpan(&graph.nodes.items[node_index].reads, assign.captures);
                    setReadBeforeRebindDef(&graph, node_index, assign.target);
                    try self.appendReadBeforeRebindSuccessor(&graph, &work, node_index, assign.next);
                },
                .assign_boxy_dict_ref => |assign| {
                    if (assign.dict.localOrNull()) |local| self.noteLivenessUseLocal(&graph.nodes.items[node_index].reads, local);
                    setReadBeforeRebindDef(&graph, node_index, assign.target);
                    try self.appendReadBeforeRebindSuccessor(&graph, &work, node_index, assign.next);
                },
                .assign_boxy_box => |assign| {
                    self.noteLivenessUseLocal(&graph.nodes.items[node_index].reads, assign.payload);
                    if (assign.source_desc) |desc| if (desc.localOrNull()) |local| self.noteLivenessUseLocal(&graph.nodes.items[node_index].reads, local);
                    if (assign.payload_desc) |desc| if (desc.localOrNull()) |local| self.noteLivenessUseLocal(&graph.nodes.items[node_index].reads, local);
                    setReadBeforeRebindDef(&graph, node_index, assign.target);
                    try self.appendReadBeforeRebindSuccessor(&graph, &work, node_index, assign.next);
                },
                .assign_boxy_reuse_box => |assign| {
                    self.noteLivenessUseLocal(&graph.nodes.items[node_index].reads, assign.source);
                    if (assign.desc.localOrNull()) |local| self.noteLivenessUseLocal(&graph.nodes.items[node_index].reads, local);
                    setReadBeforeRebindDef(&graph, node_index, assign.target);
                    try self.appendReadBeforeRebindSuccessor(&graph, &work, node_index, assign.next);
                },
                .assign_boxy_unbox => |assign| {
                    self.noteLivenessUseLocal(&graph.nodes.items[node_index].reads, assign.source);
                    if (assign.source_desc.localOrNull()) |local| self.noteLivenessUseLocal(&graph.nodes.items[node_index].reads, local);
                    if (assign.target_desc) |desc| if (desc.localOrNull()) |local| self.noteLivenessUseLocal(&graph.nodes.items[node_index].reads, local);
                    setReadBeforeRebindDef(&graph, node_index, assign.target);
                    try self.appendReadBeforeRebindSuccessor(&graph, &work, node_index, assign.next);
                },
                .assign_boxy_adapt => |assign| {
                    self.noteLivenessUseLocal(&graph.nodes.items[node_index].reads, assign.source);
                    if (assign.source_desc) |desc| if (desc.localOrNull()) |local| self.noteLivenessUseLocal(&graph.nodes.items[node_index].reads, local);
                    if (assign.target_desc) |desc| if (desc.localOrNull()) |local| self.noteLivenessUseLocal(&graph.nodes.items[node_index].reads, local);
                    setReadBeforeRebindDef(&graph, node_index, assign.target);
                    try self.appendReadBeforeRebindSuccessor(&graph, &work, node_index, assign.next);
                },
                .assign_boxy_inspect => |assign| {
                    self.noteLivenessUseLocal(&graph.nodes.items[node_index].reads, assign.source);
                    if (assign.source_desc.localOrNull()) |local| self.noteLivenessUseLocal(&graph.nodes.items[node_index].reads, local);
                    setReadBeforeRebindDef(&graph, node_index, assign.target);
                    try self.appendReadBeforeRebindSuccessor(&graph, &work, node_index, assign.next);
                },
                .assign_boxy_eq => |assign| {
                    self.noteLivenessUseLocal(&graph.nodes.items[node_index].reads, assign.lhs);
                    self.noteLivenessUseLocal(&graph.nodes.items[node_index].reads, assign.rhs);
                    if (assign.source_desc.localOrNull()) |local| self.noteLivenessUseLocal(&graph.nodes.items[node_index].reads, local);
                    setReadBeforeRebindDef(&graph, node_index, assign.target);
                    try self.appendReadBeforeRebindSuccessor(&graph, &work, node_index, assign.next);
                },
                .assign_boxy_tag => |assign| {
                    if (assign.target_desc.localOrNull()) |local| self.noteLivenessUseLocal(&graph.nodes.items[node_index].reads, local);
                    if (assign.payload) |payload| self.noteLivenessUseLocal(&graph.nodes.items[node_index].reads, payload);
                    if (assign.payload_desc) |desc| if (desc.localOrNull()) |local| self.noteLivenessUseLocal(&graph.nodes.items[node_index].reads, local);
                    setReadBeforeRebindDef(&graph, node_index, assign.target);
                    try self.appendReadBeforeRebindSuccessor(&graph, &work, node_index, assign.next);
                },
                .assign_boxy_tag_payload => |assign| {
                    self.noteLivenessUseLocal(&graph.nodes.items[node_index].reads, assign.source);
                    if (assign.source_desc.localOrNull()) |local| self.noteLivenessUseLocal(&graph.nodes.items[node_index].reads, local);
                    setReadBeforeRebindDef(&graph, node_index, assign.target);
                    if (assign.target_desc) |target_desc| setReadBeforeRebindDef(&graph, node_index, target_desc);
                    try self.appendReadBeforeRebindSuccessor(&graph, &work, node_index, assign.next);
                },
                .assign_call_dict => |assign| {
                    if (assign.dict.localOrNull()) |local| self.noteLivenessUseLocal(&graph.nodes.items[node_index].reads, local);
                    if (assign.result_desc) |desc| if (desc.localOrNull()) |local| self.noteLivenessUseLocal(&graph.nodes.items[node_index].reads, local);
                    self.noteLivenessUseSpan(&graph.nodes.items[node_index].reads, assign.args);
                    self.noteLivenessUseSpan(&graph.nodes.items[node_index].reads, assign.arg_descs);
                    self.noteLivenessUseSpan(&graph.nodes.items[node_index].reads, assign.hidden_args);
                    setReadBeforeRebindDef(&graph, node_index, assign.target);
                    try self.appendReadBeforeRebindSuccessor(&graph, &work, node_index, assign.next);
                },
                .assign_low_level => |assign| {
                    self.noteLivenessUseSpan(&graph.nodes.items[node_index].reads, assign.args);
                    setReadBeforeRebindDef(&graph, node_index, assign.target);
                    try self.appendReadBeforeRebindSuccessor(&graph, &work, node_index, assign.next);
                },
                .assign_list => |assign| {
                    self.noteLivenessUseSpan(&graph.nodes.items[node_index].reads, assign.elems);
                    setReadBeforeRebindDef(&graph, node_index, assign.target);
                    try self.appendReadBeforeRebindSuccessor(&graph, &work, node_index, assign.next);
                },
                .assign_struct => |assign| {
                    self.noteLivenessUseSpan(&graph.nodes.items[node_index].reads, assign.fields);
                    setReadBeforeRebindDef(&graph, node_index, assign.target);
                    try self.appendReadBeforeRebindSuccessor(&graph, &work, node_index, assign.next);
                },
                .assign_tag => |assign| {
                    if (assign.target_desc) |target_desc| {
                        if (target_desc.localOrNull()) |local| self.noteLivenessUseLocal(&graph.nodes.items[node_index].reads, local);
                    }
                    if (assign.payload) |payload| self.noteLivenessUseLocal(&graph.nodes.items[node_index].reads, payload);
                    setReadBeforeRebindDef(&graph, node_index, assign.target);
                    try self.appendReadBeforeRebindSuccessor(&graph, &work, node_index, assign.next);
                },
                .store_struct => |assign| {
                    self.noteLivenessUseLocal(&graph.nodes.items[node_index].reads, assign.dest);
                    self.noteLivenessUseSpan(&graph.nodes.items[node_index].reads, assign.fields);
                    try self.appendReadBeforeRebindSuccessor(&graph, &work, node_index, assign.next);
                },
                .store_tag => |assign| {
                    self.noteLivenessUseLocal(&graph.nodes.items[node_index].reads, assign.dest);
                    if (assign.payload) |payload| self.noteLivenessUseLocal(&graph.nodes.items[node_index].reads, payload);
                    try self.appendReadBeforeRebindSuccessor(&graph, &work, node_index, assign.next);
                },
                .set_local => |assign| {
                    self.noteLivenessUseLocal(&graph.nodes.items[node_index].reads, assign.value);
                    setReadBeforeRebindDef(&graph, node_index, assign.target);
                    try self.appendReadBeforeRebindSuccessor(&graph, &work, node_index, assign.next);
                },
                .debug => |debug_stmt| {
                    self.noteLivenessUseLocal(&graph.nodes.items[node_index].reads, debug_stmt.message);
                    try self.appendReadBeforeRebindSuccessor(&graph, &work, node_index, debug_stmt.next);
                },
                .expect => |expect_stmt| {
                    self.noteLivenessUseLocal(&graph.nodes.items[node_index].reads, expect_stmt.condition);
                    try self.appendReadBeforeRebindSuccessor(&graph, &work, node_index, expect_stmt.next);
                },
                .expect_err => |expect_err_stmt| {
                    self.noteLivenessUseLocal(&graph.nodes.items[node_index].reads, expect_err_stmt.message);
                },
                .incref => |rc| {
                    self.noteReadBeforeRebindLocal(&graph.nodes.items[node_index].reads, rc.value);
                    try self.appendReadBeforeRebindSuccessor(&graph, &work, node_index, rc.next);
                },
                .decref => |rc| {
                    self.noteReadBeforeRebindLocal(&graph.nodes.items[node_index].reads, rc.value);
                    try self.appendReadBeforeRebindSuccessor(&graph, &work, node_index, rc.next);
                },
                .decref_if_initialized => |rc| {
                    self.noteLivenessUseLocal(&graph.nodes.items[node_index].reads, rc.cond);
                    self.noteLivenessUseLocal(&graph.nodes.items[node_index].reads, rc.value);
                    try self.appendReadBeforeRebindSuccessor(&graph, &work, node_index, rc.next);
                },
                .free => |rc| {
                    self.noteReadBeforeRebindLocal(&graph.nodes.items[node_index].reads, rc.value);
                    try self.appendReadBeforeRebindSuccessor(&graph, &work, node_index, rc.next);
                },
                .switch_stmt => |switch_stmt| {
                    self.noteLivenessUseLocal(&graph.nodes.items[node_index].reads, switch_stmt.cond);
                    if (switch_stmt.continuation) |continuation| {
                        try self.appendReadBeforeRebindSuccessor(&graph, &work, node_index, continuation);
                    }
                    try self.appendReadBeforeRebindSuccessor(&graph, &work, node_index, switch_stmt.default_branch);
                    const branches = self.store.getCFSwitchBranches(switch_stmt.branches);
                    for (0..GuardedList.borrowLen(branches)) |branch_index| {
                        const branch = GuardedList.at(branches, branch_index);
                        try self.appendReadBeforeRebindSuccessor(&graph, &work, node_index, branch.body);
                    }
                },
                .switch_initialized_payload => |switch_stmt| {
                    self.noteLivenessUseLocal(&graph.nodes.items[node_index].reads, switch_stmt.cond);
                    self.noteLivenessUseLocal(&graph.nodes.items[node_index].reads, switch_stmt.payload);
                    try self.appendReadBeforeRebindSuccessor(&graph, &work, node_index, switch_stmt.initialized_branch);
                    try self.appendReadBeforeRebindSuccessor(&graph, &work, node_index, switch_stmt.uninitialized_branch);
                },
                .str_match => |str_match| {
                    self.noteLivenessUseLocal(&graph.nodes.items[node_index].reads, str_match.source);
                    try self.appendReadBeforeRebindSuccessor(&graph, &work, node_index, str_match.on_match);
                    try self.appendReadBeforeRebindSuccessor(&graph, &work, node_index, str_match.on_miss);
                    try self.attachStrMatchEdgeKills(&graph, node_index, str_match.steps, 0);
                },
                .str_match_set => |str_match_set| {
                    self.noteLivenessUseLocal(&graph.nodes.items[node_index].reads, str_match_set.source);
                    const arms = self.store.getStrMatchArms(str_match_set.arms);
                    for (0..GuardedList.borrowLen(arms)) |arm_index| {
                        const arm = GuardedList.at(arms, arm_index);
                        try self.appendReadBeforeRebindSuccessor(&graph, &work, node_index, arm.on_match);
                    }
                    try self.appendReadBeforeRebindSuccessor(&graph, &work, node_index, str_match_set.on_miss);
                    for (0..GuardedList.borrowLen(arms)) |arm_index| {
                        const arm = GuardedList.at(arms, arm_index);
                        try self.attachStrMatchEdgeKills(&graph, node_index, arm.steps, @intCast(arm_index));
                    }
                },
                .boxy_tag_match => |tag_match| {
                    self.noteLivenessUseLocal(&graph.nodes.items[node_index].reads, tag_match.source);
                    if (tag_match.source_desc.localOrNull()) |local| self.noteLivenessUseLocal(&graph.nodes.items[node_index].reads, local);
                    try self.appendReadBeforeRebindSuccessor(&graph, &work, node_index, tag_match.on_match);
                    try self.appendReadBeforeRebindSuccessor(&graph, &work, node_index, tag_match.on_miss);
                },
                .join => |join_stmt| {
                    // Entering a join statement itself continues with the
                    // remainder. The body is not a normal successor; it only
                    // runs through `.jump`, whose transfer semantics are
                    // modeled by entering the collected body below. Still add
                    // the body as an independent root so direct queries for
                    // `groupUsedInPath(join.body, ...)` are cached by this run.
                    _ = try self.ensureReadBeforeRebindNode(&graph, &work, join_stmt.body);
                    try self.appendReadBeforeRebindSuccessor(&graph, &work, node_index, join_stmt.remainder);
                },
                .jump => {
                    self.noteRestitutionBoundaryReads(&graph.nodes.items[node_index].reads, graph.nodes.items[node_index].stmt);
                    const join_index = self.solution.jumpTargetJoinIndexOf(graph.nodes.items[node_index].stmt);
                    if (join_index >= self.join_bodies.len) arcInvariant("ARC liveness jump index exceeded its lifted join table");
                    const target_body = self.join_bodies[join_index].body;
                    try self.appendReadBeforeRebindSuccessor(&graph, &work, node_index, target_body);
                },
                .ret => |ret_stmt| {
                    self.noteLivenessUseLocal(&graph.nodes.items[node_index].reads, ret_stmt.value);
                    self.noteRestitutionBoundaryReads(&graph.nodes.items[node_index].reads, graph.nodes.items[node_index].stmt);
                },
                .crash => |crash_stmt| if (crash_stmt.msg.localId()) |message| {
                    self.noteLivenessUseLocal(&graph.nodes.items[node_index].reads, message);
                },
                .loop_continue,
                .loop_break,
                => try loop_edge_nodes.append(graph_allocator, node_index),
                .runtime_error,
                .comptime_exhaustiveness_failed,
                => {},
                .comptime_branch_taken => |marker| {
                    try self.appendReadBeforeRebindSuccessor(&graph, &work, node_index, marker.next);
                },
            }
        }

        const node_count = graph.nodes.items.len;
        var pred_counts = try graph_allocator.alloc(usize, node_count);
        @memset(pred_counts, 0);
        for (graph.nodes.items) |node| {
            const successor_start = node.successor_start;
            const successor_end = successor_start + @as(usize, node.successor_len);
            for (graph.successors.items[successor_start..successor_end]) |successor| {
                const successor_index: usize = successor;
                pred_counts[successor_index] += 1;
            }
        }

        var pred_starts = try graph_allocator.alloc(usize, node_count + 1);
        pred_starts[0] = 0;
        for (pred_counts, 0..) |count, index| {
            pred_starts[index + 1] = pred_starts[index] + count;
        }
        var pred_writes = try graph_allocator.dupe(usize, pred_starts[0..node_count]);
        const predecessors = try graph_allocator.alloc(usize, pred_starts[node_count]);
        for (graph.nodes.items, 0..) |node, predecessor_index| {
            const successor_start = node.successor_start;
            const successor_end = successor_start + @as(usize, node.successor_len);
            for (graph.successors.items[successor_start..successor_end]) |successor| {
                const successor_index: usize = successor;
                const write_index = pred_writes[successor_index];
                predecessors[write_index] = predecessor_index;
                pred_writes[successor_index] += 1;
            }
        }
        graph.predecessor_starts = pred_starts;
        graph.predecessors = predecessors;
        graph.loop_edges = try graph_allocator.dupe(usize, loop_edge_nodes.items);
        graph.reaches_loop_edge = try std.bit_set.DynamicBitSetUnmanaged.initEmpty(graph_allocator, node_count);

        if (loop_edge_nodes.items.len != 0) {
            var reached = try std.bit_set.DynamicBitSetUnmanaged.initEmpty(graph_allocator, node_count);
            for (loop_edge_nodes.items) |edge_node| reached.set(edge_node);
            while (loop_edge_nodes.pop()) |reach_index| {
                graph.reaches_loop_edge.set(reach_index);
                const pred_start = pred_starts[reach_index];
                const pred_end = pred_starts[reach_index + 1];
                for (predecessors[pred_start..pred_end]) |predecessor_index| {
                    if (reached.isSet(predecessor_index)) continue;
                    reached.set(predecessor_index);
                    try loop_edge_nodes.append(graph_allocator, predecessor_index);
                }
            }
        }

        try self.solveKeepFreeLiveness(&graph);
        graph_slot.* = graph;
        const start_node = self.stmt_node_indices[@intFromEnum(start)];
        if (start_node == no_stmt_node_index or start_node >= graph_slot.*.?.nodes.items.len) {
            arcInvariant("ARC keep-free liveness cache did not include requested start");
        }
        return &graph_slot.*.?.nodes.items[start_node].exposed;
    }

    fn solveKeepFreeLiveness(self: *Inserter, graph: *ReadBeforeRebindGraph) ResourceError!void {
        const allocator = graph.allocator;
        const node_count = graph.nodes.items.len;
        const Frame = struct { node: usize, next_successor: usize };
        var seen = try std.bit_set.DynamicBitSetUnmanaged.initEmpty(allocator, node_count);
        var frames = std.ArrayList(Frame).empty;
        var finish_order = std.ArrayList(usize).empty;
        for (0..node_count) |start| {
            if (seen.isSet(start)) continue;
            seen.set(start);
            try frames.append(allocator, .{ .node = start, .next_successor = 0 });
            while (frames.items.len != 0) {
                const frame = &frames.items[frames.items.len - 1];
                const node = graph.nodes.items[frame.node];
                if (frame.next_successor < node.successor_len) {
                    const successor = graph.successors.items[node.successor_start + frame.next_successor];
                    frame.next_successor += 1;
                    if (!seen.isSet(successor)) {
                        seen.set(successor);
                        try frames.append(allocator, .{ .node = successor, .next_successor = 0 });
                    }
                    continue;
                }
                try finish_order.append(allocator, frame.node);
                _ = frames.pop();
            }
        }

        const no_scc = std.math.maxInt(u32);
        const scc_of = try allocator.alloc(u32, node_count);
        @memset(scc_of, no_scc);
        var scc_nodes = std.ArrayList(usize).empty;
        var scc_offsets = std.ArrayList(usize).empty;
        var reverse_work = std.ArrayList(usize).empty;
        var order_index = finish_order.items.len;
        while (order_index > 0) {
            order_index -= 1;
            const start = finish_order.items[order_index];
            if (scc_of[start] != no_scc) continue;
            const scc_id: u32 = @intCast(scc_offsets.items.len);
            try scc_offsets.append(allocator, scc_nodes.items.len);
            scc_of[start] = scc_id;
            try reverse_work.append(allocator, start);
            while (reverse_work.pop()) |node_index| {
                try scc_nodes.append(allocator, node_index);
                const pred_start = graph.predecessor_starts[node_index];
                const pred_end = graph.predecessor_starts[node_index + 1];
                for (graph.predecessors[pred_start..pred_end]) |predecessor| {
                    if (scc_of[predecessor] != no_scc) continue;
                    scc_of[predecessor] = scc_id;
                    try reverse_work.append(allocator, predecessor);
                }
            }
        }
        try scc_offsets.append(allocator, scc_nodes.items.len);

        var scratch = try ExactBitSet.initEmpty(allocator, self.domain().livenessBitLen());
        var edge_scratch = try ExactBitSet.initEmpty(allocator, self.domain().livenessBitLen());
        var in_work = try std.bit_set.DynamicBitSetUnmanaged.initEmpty(allocator, node_count);
        var node_work = std.ArrayList(usize).empty;

        var scc_cursor = scc_offsets.items.len - 1;
        while (scc_cursor > 0) {
            scc_cursor -= 1;
            const members = scc_nodes.items[scc_offsets.items[scc_cursor]..scc_offsets.items[scc_cursor + 1]];
            var cyclic = members.len > 1;
            if (!cyclic) {
                const node = graph.nodes.items[members[0]];
                const successor_end = node.successor_start + @as(usize, node.successor_len);
                for (graph.successors.items[node.successor_start..successor_end]) |successor| {
                    if (successor == members[0]) {
                        cyclic = true;
                        break;
                    }
                }
            }
            if (!cyclic) {
                _ = self.recomputeLivenessNode(graph, members[0], &scratch, &edge_scratch);
                continue;
            }
            for (members) |node_index| {
                if (in_work.isSet(node_index)) continue;
                in_work.set(node_index);
                try node_work.append(allocator, node_index);
            }
            while (node_work.pop()) |node_index| {
                in_work.unset(node_index);
                if (!self.recomputeLivenessNode(graph, node_index, &scratch, &edge_scratch)) continue;
                const pred_start = graph.predecessor_starts[node_index];
                const pred_end = graph.predecessor_starts[node_index + 1];
                for (graph.predecessors[pred_start..pred_end]) |predecessor| {
                    if (scc_of[predecessor] != scc_cursor or in_work.isSet(predecessor)) continue;
                    in_work.set(predecessor);
                    try node_work.append(allocator, predecessor);
                }
            }
        }
    }

    fn recomputeLivenessNode(
        self: *Inserter,
        graph: *ReadBeforeRebindGraph,
        node_index: usize,
        scratch: *ExactBitSet,
        edge_scratch: *ExactBitSet,
    ) bool {
        const node = &graph.nodes.items[node_index];
        scratch.unsetAll();
        const successor_end = node.successor_start + @as(usize, node.successor_len);
        for (graph.successors.items[node.successor_start..successor_end], 0..) |successor, successor_offset| {
            var edge_killed = false;
            for (node.edge_kills) |kill| {
                if (kill.successor_offset != successor_offset) continue;
                if (!edge_killed) {
                    edge_scratch.unsetAll();
                    edge_scratch.setUnion(graph.nodes.items[successor].exposed);
                    edge_killed = true;
                }
                edge_scratch.unset(kill.bit);
            }
            scratch.setUnion(if (edge_killed) edge_scratch.* else graph.nodes.items[successor].exposed);
        }
        if (node.def) |local| {
            if (self.rawLivenessBitOf(local)) |bit| scratch.unset(bit);
            if (self.groupBitOf(local)) |bit| scratch.unset(bit);
            if (self.valueUseBitOf(local)) |bit| scratch.unset(bit);
        }
        scratch.setUnion(node.reads);
        if (node.exposed.eql(scratch.*)) return false;
        node.exposed.unsetAll();
        node.exposed.setUnion(scratch.*);
        return true;
    }

    fn computeLoopReadsBeforeRebind(
        self: *Inserter,
        start: LIR.CFStmtId,
        keep: LoopKeep,
        loop_keep_id: u32,
    ) ResourceError!*const ExactBitSet {
        if (loop_keep_id == 0 or loop_keep_id >= self.loop_liveness_caches.items.len) {
            arcInvariant("ARC loop liveness referenced an unknown loop identity");
        }
        const cache = &self.loop_liveness_caches.items[loop_keep_id];
        if (cache.initialized) arcInvariant("ARC recomputed an initialized direct loop cache");
        const source_index = @intFromEnum(self.current_source_proc);
        if (source_index >= self.liveness_graphs.len) arcInvariant("ARC loop liveness source proc exceeded its graph table");
        const graph = if (self.liveness_graphs[source_index]) |*entry|
            entry
        else
            arcInvariant("ARC loop liveness ran before keep-free graph construction");
        const start_stmt_index = @intFromEnum(start);
        if (start_stmt_index >= self.stmt_node_indices.len) arcInvariant("ARC loop liveness queried a generated statement");
        const start_node = self.stmt_node_indices[start_stmt_index];
        if (start_node == no_stmt_node_index or start_node >= graph.nodes.items.len) {
            arcInvariant("ARC loop liveness start was outside its source graph");
        }

        const allocator = self.emission_allocator;
        const node_count = graph.nodes.items.len;
        var active = try std.bit_set.DynamicBitSetUnmanaged.initEmpty(allocator, node_count);
        var walk = std.ArrayList(usize).empty;
        for (cache.region_roots) |root| {
            const root_stmt_index = @intFromEnum(root);
            if (root_stmt_index >= self.stmt_node_indices.len) arcInvariant("ARC loop region root was a generated statement");
            const root_node = self.stmt_node_indices[root_stmt_index];
            if (root_node == no_stmt_node_index or root_node >= node_count) {
                arcInvariant("ARC loop region root was outside its source graph");
            }
            if (active.isSet(root_node)) continue;
            active.set(root_node);
            try walk.append(allocator, root_node);
        }
        while (walk.pop()) |node_index| {
            const node = graph.nodes.items[node_index];
            const successor_end = node.successor_start + @as(usize, node.successor_len);
            for (graph.successors.items[node.successor_start..successor_end]) |successor| {
                if (active.isSet(successor)) continue;
                active.set(successor);
                try walk.append(allocator, successor);
            }
        }
        if (!active.isSet(start_node)) arcInvariant("ARC loop liveness query was outside its explicit join regions");

        var keep_reads = try ExactBitSet.initEmpty(allocator, self.domain().livenessBitLen());
        self.noteLivenessLoopKeep(&keep_reads, keep.set);
        cache.consumed_keep_bits = keep_reads.count() != 0;

        const rows = try allocator.alloc(?ExactBitSet, node_count);
        @memset(rows, null);
        var in_work = try std.bit_set.DynamicBitSetUnmanaged.initEmpty(allocator, node_count);
        var node_work = std.ArrayList(usize).empty;
        for (graph.loop_edges) |loop_node| {
            if (!active.isSet(loop_node)) continue;
            var row = try graph.nodes.items[loop_node].exposed.clone(allocator);
            row.setUnion(keep_reads);
            rows[loop_node] = row;
            const pred_start = graph.predecessor_starts[loop_node];
            const pred_end = graph.predecessor_starts[loop_node + 1];
            for (graph.predecessors[pred_start..pred_end]) |predecessor| {
                if (!active.isSet(predecessor) or in_work.isSet(predecessor)) continue;
                in_work.set(predecessor);
                try node_work.append(allocator, predecessor);
            }
        }

        var scratch = try ExactBitSet.initEmpty(allocator, self.domain().livenessBitLen());
        var edge_scratch = try ExactBitSet.initEmpty(allocator, self.domain().livenessBitLen());
        while (node_work.pop()) |node_index| {
            in_work.unset(node_index);
            const node = graph.nodes.items[node_index];
            scratch.unsetAll();
            const successor_end = node.successor_start + @as(usize, node.successor_len);
            for (graph.successors.items[node.successor_start..successor_end], 0..) |successor, successor_offset| {
                const successor_row = if (rows[successor]) |*row| row else &graph.nodes.items[successor].exposed;
                var edge_killed = false;
                for (node.edge_kills) |kill| {
                    if (kill.successor_offset != successor_offset) continue;
                    if (!edge_killed) {
                        edge_scratch.unsetAll();
                        edge_scratch.setUnion(successor_row.*);
                        edge_killed = true;
                    }
                    edge_scratch.unset(kill.bit);
                }
                scratch.setUnion(if (edge_killed) edge_scratch else successor_row.*);
            }
            if (node.def) |local| {
                if (self.rawLivenessBitOf(local)) |bit| scratch.unset(bit);
                if (self.groupBitOf(local)) |bit| scratch.unset(bit);
                if (self.valueUseBitOf(local)) |bit| scratch.unset(bit);
            }
            scratch.setUnion(node.reads);
            const previous = if (rows[node_index]) |*row| row else &node.exposed;
            if (previous.eql(scratch)) continue;
            if (rows[node_index]) |*row| {
                row.unsetAll();
                row.setUnion(scratch);
            } else {
                rows[node_index] = try scratch.clone(allocator);
            }
            const pred_start = graph.predecessor_starts[node_index];
            const pred_end = graph.predecessor_starts[node_index + 1];
            for (graph.predecessors[pred_start..pred_end]) |predecessor| {
                if (!active.isSet(predecessor) or in_work.isSet(predecessor)) continue;
                in_work.set(predecessor);
                try node_work.append(allocator, predecessor);
            }
        }

        var active_iter = active.iterator(.{});
        while (active_iter.next()) |node_index| {
            if (rows[node_index] == null) {
                rows[node_index] = try graph.nodes.items[node_index].exposed.clone(allocator);
            }
        }
        cache.rows = rows;
        cache.active = active;
        cache.keep_reads = keep_reads;
        cache.initialized = true;
        cache.dirty = false;
        if (builtin.mode == .Debug) try self.certifyLoopReadsBeforeRebind(cache);
        return if (cache.rows[start_node]) |*row|
            row
        else
            arcInvariant("ARC loop liveness cache did not include its requested start");
    }

    /// Applies a changed loop keep-set as an exact boundary delta to the
    /// retained active subgraph. Keep-sets only shrink, so only loop-edge
    /// rows that actually lose bits seed the reverse worklist; propagation
    /// then visits precisely predecessors whose solved row changes.
    fn refreshLoopReadsBeforeRebind(self: *Inserter, cache: *LoopLivenessCache, keep: LoopKeep) ResourceError!void {
        if (!cache.initialized or !cache.dirty) arcInvariant("ARC refreshed a loop cache outside its dirty initialized state");
        const source_index = @intFromEnum(self.current_source_proc);
        if (source_index >= self.liveness_graphs.len) arcInvariant("ARC loop refresh source proc exceeded its graph table");
        const graph = if (self.liveness_graphs[source_index]) |*entry|
            entry
        else
            arcInvariant("ARC loop refresh ran without an immutable liveness graph");
        const active = if (cache.active) |*bits| bits else arcInvariant("ARC initialized loop cache lacked its active subgraph");
        const old_keep_reads = if (cache.keep_reads) |*bits| bits else arcInvariant("ARC initialized loop cache lacked its boundary row");
        if (cache.rows.len != graph.nodes.items.len) arcInvariant("ARC loop cache row domain changed after graph construction");

        const allocator = self.emission_allocator;
        var new_keep_reads = try ExactBitSet.initEmpty(allocator, self.domain().livenessBitLen());
        self.noteLivenessLoopKeep(&new_keep_reads, keep.set);
        var new_iter = new_keep_reads.iterator(.{});
        while (new_iter.next()) |bit| {
            if (!old_keep_reads.isSet(bit)) arcInvariant("ARC loop boundary facts grew after their monotone keep-set shrank");
        }
        if (old_keep_reads.eql(new_keep_reads)) {
            cache.dirty = false;
            cache.consumed_keep_bits = new_keep_reads.count() != 0;
            if (builtin.mode == .Debug) try self.certifyLoopReadsBeforeRebind(cache);
            return;
        }

        var scratch = try ExactBitSet.initEmpty(allocator, self.domain().livenessBitLen());
        var edge_scratch = try ExactBitSet.initEmpty(allocator, self.domain().livenessBitLen());
        var in_work = try std.bit_set.DynamicBitSetUnmanaged.initEmpty(allocator, graph.nodes.items.len);
        var node_work = std.ArrayList(usize).empty;

        for (graph.loop_edges) |loop_node| {
            if (!active.isSet(loop_node)) continue;
            scratch.unsetAll();
            scratch.setUnion(graph.nodes.items[loop_node].exposed);
            scratch.setUnion(new_keep_reads);
            const row = if (cache.rows[loop_node]) |*existing| existing else arcInvariant("ARC active loop edge lacked its cached row");
            if (row.eql(scratch)) continue;
            row.unsetAll();
            row.setUnion(scratch);
            const pred_start = graph.predecessor_starts[loop_node];
            const pred_end = graph.predecessor_starts[loop_node + 1];
            for (graph.predecessors[pred_start..pred_end]) |predecessor| {
                if (!active.isSet(predecessor) or in_work.isSet(predecessor)) continue;
                in_work.set(predecessor);
                try node_work.append(allocator, predecessor);
            }
        }

        while (node_work.pop()) |node_index| {
            in_work.unset(node_index);
            const node = graph.nodes.items[node_index];
            scratch.unsetAll();
            const successor_end = node.successor_start + @as(usize, node.successor_len);
            for (graph.successors.items[node.successor_start..successor_end], 0..) |successor, successor_offset| {
                const successor_row = if (cache.rows[successor]) |*row| row else arcInvariant("ARC active loop predecessor reached an uncached successor");
                var edge_killed = false;
                for (node.edge_kills) |kill| {
                    if (kill.successor_offset != successor_offset) continue;
                    if (!edge_killed) {
                        edge_scratch.unsetAll();
                        edge_scratch.setUnion(successor_row.*);
                        edge_killed = true;
                    }
                    edge_scratch.unset(kill.bit);
                }
                scratch.setUnion(if (edge_killed) edge_scratch else successor_row.*);
            }
            if (node.def) |local| {
                if (self.rawLivenessBitOf(local)) |bit| scratch.unset(bit);
                if (self.groupBitOf(local)) |bit| scratch.unset(bit);
                if (self.valueUseBitOf(local)) |bit| scratch.unset(bit);
            }
            scratch.setUnion(node.reads);
            const row = if (cache.rows[node_index]) |*existing| existing else arcInvariant("ARC active loop node lacked its cached row");
            if (row.eql(scratch)) continue;
            row.unsetAll();
            row.setUnion(scratch);
            const pred_start = graph.predecessor_starts[node_index];
            const pred_end = graph.predecessor_starts[node_index + 1];
            for (graph.predecessors[pred_start..pred_end]) |predecessor| {
                if (!active.isSet(predecessor) or in_work.isSet(predecessor)) continue;
                in_work.set(predecessor);
                try node_work.append(allocator, predecessor);
            }
        }

        old_keep_reads.unsetAll();
        old_keep_reads.setUnion(new_keep_reads);
        cache.consumed_keep_bits = new_keep_reads.count() != 0;
        cache.dirty = false;
        if (builtin.mode == .Debug) try self.certifyLoopReadsBeforeRebind(cache);
    }

    /// Debug-only independent least-fixed-point recomputation for one loop
    /// cache. The production update descends from the previous solution via
    /// the changed boundary; this oracle instead starts at the immutable
    /// keep-free solution and grows all active rows, so agreement certifies
    /// both directions of the exact-delta algorithm.
    fn certifyLoopReadsBeforeRebind(self: *Inserter, cache: *const LoopLivenessCache) ResourceError!void {
        if (builtin.mode != .Debug) return;
        const source_index = @intFromEnum(self.current_source_proc);
        const graph = if (self.liveness_graphs[source_index]) |*entry|
            entry
        else
            arcInvariant("ARC loop-liveness certifier lacked its immutable graph");
        const active = if (cache.active) |*bits|
            bits
        else
            arcInvariant("ARC loop-liveness certifier lacked its active subgraph");
        const keep_reads = if (cache.keep_reads) |*bits|
            bits
        else
            arcInvariant("ARC loop-liveness certifier lacked its boundary facts");
        if (cache.rows.len != graph.nodes.items.len) arcInvariant("ARC loop-liveness certifier saw a row-domain mismatch");

        const allocator = self.emission_allocator;
        const expected = try allocator.alloc(?ExactBitSet, graph.nodes.items.len);
        @memset(expected, null);
        var active_iter = active.iterator(.{});
        while (active_iter.next()) |node_index| {
            expected[node_index] = try graph.nodes.items[node_index].exposed.clone(allocator);
        }
        var is_loop_edge = try std.bit_set.DynamicBitSetUnmanaged.initEmpty(allocator, graph.nodes.items.len);
        for (graph.loop_edges) |loop_node| {
            if (!active.isSet(loop_node)) continue;
            is_loop_edge.set(loop_node);
            expected[loop_node].?.setUnion(keep_reads.*);
        }

        var in_work = try active.clone(allocator);
        var work = std.ArrayList(usize).empty;
        active_iter = active.iterator(.{});
        while (active_iter.next()) |node_index| try work.append(allocator, node_index);
        var scratch = try ExactBitSet.initEmpty(allocator, self.domain().livenessBitLen());
        var edge_scratch = try ExactBitSet.initEmpty(allocator, self.domain().livenessBitLen());
        while (work.pop()) |node_index| {
            in_work.unset(node_index);
            const node = graph.nodes.items[node_index];
            scratch.unsetAll();
            if (is_loop_edge.isSet(node_index)) {
                scratch.setUnion(node.exposed);
                scratch.setUnion(keep_reads.*);
            } else {
                const successor_end = node.successor_start + @as(usize, node.successor_len);
                for (graph.successors.items[node.successor_start..successor_end], 0..) |successor, successor_offset| {
                    const successor_row = if (expected[successor]) |*row|
                        row
                    else
                        arcInvariant("ARC loop-liveness certifier reached an inactive successor");
                    var edge_killed = false;
                    for (node.edge_kills) |kill| {
                        if (kill.successor_offset != successor_offset) continue;
                        if (!edge_killed) {
                            edge_scratch.unsetAll();
                            edge_scratch.setUnion(successor_row.*);
                            edge_killed = true;
                        }
                        edge_scratch.unset(kill.bit);
                    }
                    scratch.setUnion(if (edge_killed) edge_scratch else successor_row.*);
                }
                if (node.def) |local| {
                    if (self.rawLivenessBitOf(local)) |bit| scratch.unset(bit);
                    if (self.groupBitOf(local)) |bit| scratch.unset(bit);
                    if (self.valueUseBitOf(local)) |bit| scratch.unset(bit);
                }
                scratch.setUnion(node.reads);
            }
            const row = if (expected[node_index]) |*entry|
                entry
            else
                arcInvariant("ARC loop-liveness certifier omitted an active row");
            if (row.eql(scratch)) continue;
            row.unsetAll();
            row.setUnion(scratch);
            const pred_start = graph.predecessor_starts[node_index];
            const pred_end = graph.predecessor_starts[node_index + 1];
            for (graph.predecessors[pred_start..pred_end]) |predecessor| {
                if (!active.isSet(predecessor) or in_work.isSet(predecessor)) continue;
                in_work.set(predecessor);
                try work.append(allocator, predecessor);
            }
        }

        active_iter = active.iterator(.{});
        while (active_iter.next()) |node_index| {
            const actual = if (cache.rows[node_index]) |*row|
                row
            else
                arcInvariant("ARC loop-liveness cache omitted an active row");
            const independently_solved = expected[node_index].?;
            if (!actual.eql(independently_solved)) arcInvariant("ARC loop liveness delta disagreed with an independent fixed point");
        }
    }

    /// Value liveness for one raw local (no group extension). Borrowed call
    /// results have a dedicated value-use bit; other resources use their raw
    /// ownership bit.
    fn valueUsedInPath(
        self: *Inserter,
        start: LIR.CFStmtId,
        needle: LIR.LocalId,
        loop_keep: ?LoopKeep,
    ) ResourceError!bool {
        const bit = self.valueUseBitOf(needle) orelse self.rawLivenessBitOf(needle) orelse
            arcInvariant("ARC value-use query for a local without a value-use bit");
        const reads = try self.livenessRow(start, loop_keep);
        return reads.isSet(bit);
    }

    fn spanUsesLocal(self: *Inserter, span: LIR.LocalSpan, needle: LIR.LocalId) bool {
        const locals = self.store.getLocalSpan(span);
        for (0..GuardedList.borrowLen(locals)) |index| {
            const local = GuardedList.at(locals, index);
            if (local == needle) return true;
        }
        return false;
    }

    /// Liveness for one owned local extended over its borrow group: the
    /// local's value must stay live while the local itself or any borrow
    /// anchored on it is still used.
    /// Liveness row for a query start under an optional loop keep-set. A
    /// loop-keyed row differs from the keep-free row only when a loop edge
    /// is reachable from the start, so everything else shares the keep-free
    /// row and the cache stays linear in proc size.
    fn livenessRow(
        self: *Inserter,
        start: LIR.CFStmtId,
        loop_keep: ?LoopKeep,
    ) ResourceError!*const ExactBitSet {
        const keep = loop_keep orelse return self.computeReadsBeforeRebind(start, null, 0);
        if (keep.id == 0) arcInvariant("ARC loop keep-set used the keep-free identity");
        const keep_free = try self.computeReadsBeforeRebind(start, null, 0);
        const source_index = @intFromEnum(self.current_source_proc);
        const graph = if (self.liveness_graphs[source_index]) |*entry|
            entry
        else
            arcInvariant("ARC loop liveness query lacked its immutable source graph");
        const stmt_index = @intFromEnum(start);
        if (stmt_index >= self.stmt_node_indices.len) arcInvariant("ARC loop liveness queried a generated statement");
        const node_index = self.stmt_node_indices[stmt_index];
        if (node_index == no_stmt_node_index or node_index >= graph.nodes.items.len) {
            arcInvariant("ARC loop liveness query was outside its source graph");
        }
        if (!graph.reaches_loop_edge.isSet(node_index)) return keep_free;
        return self.computeReadsBeforeRebind(start, keep, keep.id);
    }

    fn groupUsedInPath(
        self: *Inserter,
        start: LIR.CFStmtId,
        local: LIR.LocalId,
        loop_keep: ?LoopKeep,
    ) ResourceError!bool {
        const reads = try self.livenessRow(start, loop_keep);
        return self.groupUsedFromTable(reads, local);
    }

    fn groupUsedInPathExcept(
        self: *Inserter,
        start: LIR.CFStmtId,
        local: LIR.LocalId,
        except: LIR.LocalId,
        loop_keep: ?LoopKeep,
    ) ResourceError!bool {
        const reads = try self.livenessRow(start, loop_keep);
        const leader = self.solution.leaderOf(local);
        // Resource locals include concrete RC values plus the solver-authored
        // ownership-unit and borrow-group representatives. Those synthetic
        // anchors have liveness bits even though they need no concrete RC
        // helper of their own, and must participate in lender-death checks.
        for (self.domain().resource_locals) |member_local| {
            if (self.solution.leaderOf(member_local) != leader) continue;
            if (member_local == except) continue;
            const bit = self.rawLivenessBitOf(member_local) orelse
                arcInvariant("ARC refcounted borrow-group member missing its raw liveness bit");
            if (reads.isSet(bit)) return true;
        }
        return false;
    }

    fn retainSpanExceptPositions(
        self: *Inserter,
        span: LIR.LocalSpan,
        skip_positions: []const u32,
        next: LIR.CFStmtId,
    ) ResourceError!LIR.CFStmtId {
        var current = next;
        const locals = self.store.getLocalSpan(span);
        var skip_index = skip_positions.len;
        var i = locals.len;
        while (i > 0) {
            i -= 1;
            if (skip_index > 0 and @as(usize, skip_positions[skip_index - 1]) == i) {
                skip_index -= 1;
                continue;
            }
            current = try self.retainLocalIfRc(GuardedList.at(locals, i), current);
        }
        return current;
    }

    fn retainLocalIfRc(self: *Inserter, local: LIR.LocalId, next: LIR.CFStmtId) ResourceError!LIR.CFStmtId {
        return try self.retainLocalIfRcCount(local, 1, next);
    }

    fn retainLocalIfRcCount(self: *Inserter, local: LIR.LocalId, count: u16, next: LIR.CFStmtId) ResourceError!LIR.CFStmtId {
        if (count == 0) return next;
        if (!self.localContainsRefcounted(local)) return next;
        const rc = self.rcHelperForLocal(.incref, local);
        const atomicity = self.rcAtomicity(local);
        return try addCanonicalRetain(self.store, local, rc, atomicity, count, next);
    }

    fn strMatchCaptureRetainCount(self: *const Inserter, steps: LIR.StrMatchStepSpan) u16 {
        var count: u16 = 0;
        const step_borrow = self.store.getStrMatchSteps(steps);
        for (0..GuardedList.borrowLen(step_borrow)) |step_index| {
            const step = GuardedList.at(step_borrow, step_index);
            switch (step.capture) {
                .discard => {},
                .view => |local| {
                    if (self.localContainsRefcounted(local) and !self.isBindingBorrowed(local)) {
                        count +|= 1;
                    }
                },
            }
        }
        return count;
    }

    /// Whether this statement's field read is a take in the current
    /// emission. Takes on containers solved borrowed apply only when the
    /// variant demand vector overrides the parameter to owned.
    fn takeForEmission(self: *const Inserter, stmt: LIR.CFStmtId) ?arc_dismantle.Take {
        if (self.dismantles.takeAt(stmt)) |take| return take;
        if (self.dismantles.ownedOnlyTake(stmt)) |take| {
            if (self.owned_binding_override.contains(take.root)) return take;
        }
        return null;
    }

    fn fieldRestitutionForEmission(self: *const Inserter, stmt: LIR.CFStmtId, position: usize) ?arc_dismantle.FieldRestitutionArg {
        const receipt = self.dismantles.fieldRestitutionArg(stmt, position) orelse return null;
        // An ownership-complete projection transfers the root unit. Treating
        // that same read as a partial field take would publish two receipts
        // for one call position and restore the same committed place twice.
        if (self.dismantles.completeTakeRoot(receipt.projection) != null) return null;
        if (self.dismantles.ownedOnlyContainerOf(receipt.place.root) != null and
            !self.owned_binding_override.contains(receipt.place.root)) return null;
        return receipt;
    }

    /// The dismantle plan for this local's release in the current emission,
    /// if it has one.
    fn dismantleFor(self: *const Inserter, local: LIR.LocalId) ?arc_dismantle.Container {
        if (self.dismantles.containerOf(local)) |container| return container;
        if (self.dismantles.ownedOnlyContainerOf(local)) |container| {
            if (self.owned_binding_override.contains(local)) return container;
        }
        return null;
    }

    fn releaseLocalIfRc(self: *Inserter, local: LIR.LocalId, next: LIR.CFStmtId) ResourceError!LIR.CFStmtId {
        if (!self.localContainsRefcounted(local)) return next;
        if (self.dismantleFor(local)) |container| {
            return try self.dismantleContainer(local, container, container.full_mask, next);
        }
        const rc = self.rcHelperForLocal(.decref, local);
        return try self.store.addCFStmt(.{ .decref = .{
            .value = local,
            .rc = rc,
            .atomicity = self.rcAtomicity(local),
            .next = next,
        } });
    }

    /// Release a dismantled container: its taken fields' units were consumed
    /// by their take reads, so only the residual refcounted fields are read
    /// into temporaries and released. The temporaries and the container's
    /// solved arrays never meet: field layouts drive the helpers directly,
    /// and the container's atomicity covers its stored payloads exactly as
    /// the whole-struct helper would have.
    fn dismantleContainer(self: *Inserter, local: LIR.LocalId, container: arc_dismantle.Container, residual_mask: u64, next: LIR.CFStmtId) ResourceError!LIR.CFStmtId {
        if ((residual_mask & ~container.full_mask) != 0) arcInvariant("ARC residual release exceeded its committed aggregate field domain");
        const atomicity = self.rcAtomicity(local);
        var tail = next;
        var index = container.fields.len;
        while (index > 0) {
            index -= 1;
            const field = container.fields[index];
            const field_mask = @as(u64, 1) << @intCast(field.field_idx);
            if ((residual_mask & field_mask) == 0) continue;
            const rc = self.rcHelperForLayout(.decref, field.layout_idx);
            if (self.layouts.rcHelperPlan(rc) == .noop) {
                arcInvariant("ARC dismantle selected a noop RC helper for a refcounted residual field");
            }
            const temp = try self.store.addLocal(.{ .layout_idx = field.layout_idx });
            try self.dismantle_temps.append(self.emission_allocator, temp);
            tail = try self.store.addCFStmt(.{ .decref = .{
                .value = temp,
                .rc = LIR.RcHelper.fromConcrete(rc),
                .atomicity = atomicity,
                .next = tail,
            } });
            tail = try self.store.addCFStmt(.{ .assign_ref = .{
                .target = temp,
                .op = .{ .field = .{
                    .source = local,
                    .field_idx = @intCast(field.field_idx),
                } },
                .next = tail,
            } });
        }
        return tail;
    }

    fn releaseMaybeInitializedLocal(self: *Inserter, condition: LIR.LocalId, condition_mask: u64, local: LIR.LocalId, next: LIR.CFStmtId) ResourceError!LIR.CFStmtId {
        if (!self.localContainsRefcounted(local)) return next;
        const rc = self.rcHelperForLocal(.decref, local);
        return try self.store.addCFStmt(.{ .decref_if_initialized = .{
            .cond = condition,
            .cond_mask = condition_mask,
            .value = local,
            .rc = rc,
            .atomicity = self.rcAtomicity(local),
            .next = next,
        } });
    }

    /// Count-update mode for RC statements on this local: plain loads and
    /// stores when the visibility analysis proves no host thread can ever
    /// touch the local's allocation, atomic otherwise.
    fn rcAtomicity(self: *const Inserter, local: LIR.LocalId) LIR.RcAtomicity {
        return if (self.solution.isVisible(local)) .atomic else .single_thread;
    }

    fn rcHelperForLocal(self: *const Inserter, op: layout_mod.RcOp, local: LIR.LocalId) LIR.RcHelper {
        const local_index = @intFromEnum(local);
        if (local_index < self.boxy_rc_descs.len) {
            if (self.boxy_rc_descs[local_index]) |desc| {
                return .{ .boxy = desc };
            }
        }

        const local_layout = self.store.getLocal(local).layout_idx;
        const helper = self.rcHelperForLayout(op, local_layout);
        if (self.layouts.rcHelperPlan(helper) == .noop) {
            if (comptime builtin.mode == .Debug and builtin.target.os.tag == .freestanding) {
                @panic("ARC attempted to emit a noop RC helper for a refcounted local");
            } else if (comptime builtin.mode == .Debug) {
                var buffer: std.Io.Writer.Allocating = .init(self.store.allocator);
                defer buffer.deinit();
                debug_print.writeProc(self.store.allocator, self.store, self.layouts, self.current_proc, &buffer.writer) catch {};
                std.debug.print("\n{s}\n", .{buffer.written()});
                const ref_source: ?LIR.LocalId = if (self.current_rewrite_stmt) |stmt_id| blk: {
                    const stmt = self.store.getCFStmt(stmt_id);
                    break :blk if (stmt == .assign_ref) refOpSource(stmt.assign_ref.op) else null;
                } else null;
                const ref_source_layout: ?layout_mod.Idx = if (ref_source) |source| self.store.getLocal(source).layout_idx else null;
                const ref_source_desc: ?LIR.BoxyDescRef = if (ref_source) |source| boxyDescForLocal(self.boxy_rc_descs, source) else null;
                std.debug.panic("ARC attempted to emit a noop RC helper for refcounted local {d} layout={d} layout_data={any} desc={?} proc={d} stmt={?d} ref_source={?d} ref_source_layout={?d} ref_source_layout_data={any} ref_source_desc={?} stmt_data={any}", .{
                    @intFromEnum(local),
                    @intFromEnum(local_layout),
                    self.layouts.getLayout(local_layout),
                    boxyDescForLocal(self.boxy_rc_descs, local),
                    @intFromEnum(self.current_proc),
                    if (self.current_rewrite_stmt) |stmt_id| @intFromEnum(stmt_id) else null,
                    if (ref_source) |source| @intFromEnum(source) else null,
                    if (ref_source_layout) |source_layout| @intFromEnum(source_layout) else null,
                    if (ref_source_layout) |source_layout| self.layouts.getLayout(source_layout) else null,
                    ref_source_desc,
                    if (self.current_rewrite_stmt) |stmt_id| self.store.getCFStmt(stmt_id) else null,
                });
            }
            unreachable;
        }
        return LIR.RcHelper.fromConcrete(helper);
    }

    fn rcHelperForLayout(self: *const Inserter, op: layout_mod.RcOp, layout_idx: layout_mod.Idx) layout_mod.RcHelper {
        const layout_val = self.layouts.getLayout(layout_idx);
        if (layout_val.tag == .closure) {
            return self.rcHelperForLayout(nestedDropOp(op), layout_val.getClosure().captures_layout_idx);
        }
        return .{ .op = op, .layout_idx = layout_idx };
    }

    fn layoutMayContainBoxyDynamic(self: *const Inserter, layout_idx: layout_mod.Idx) bool {
        const layout_val = self.layouts.getLayout(layout_idx);
        return switch (layout_val.tag) {
            .box_of_zst => true,
            .box => self.layoutMayContainBoxyDynamic(layout_val.getIdx()),
            .list => self.layoutMayContainBoxyDynamic(layout_val.getIdx()),
            .list_of_zst => false,
            .struct_ => blk: {
                const info = self.layouts.getStructInfo(layout_val);
                for (0..info.fields.len) |index| {
                    const field = info.fields.get(@intCast(index));
                    if (self.layoutMayContainBoxyDynamic(field.layout)) break :blk true;
                }
                break :blk false;
            },
            .tag_union => blk: {
                const info = self.layouts.getTagUnionInfo(layout_val);
                for (0..info.variants.len) |index| {
                    const payload_layout = info.variants.get(@intCast(index)).payload_layout;
                    if (self.layoutMayContainBoxyDynamic(payload_layout)) break :blk true;
                }
                break :blk false;
            },
            .closure => self.layoutMayContainBoxyDynamic(layout_val.getClosure().captures_layout_idx),
            .zst, .scalar, .erased_callable, .ptr => false,
        };
    }

    fn nestedDropOp(op: layout_mod.RcOp) layout_mod.RcOp {
        return switch (op) {
            .incref => .incref,
            .decref, .free => .decref,
        };
    }

    fn localContainsRefcounted(self: *const Inserter, local: LIR.LocalId) bool {
        const index = @intFromEnum(local);
        if (index >= self.local_contains_refcounted.len) arcInvariant("ARC local refcounted cache did not cover local");
        return self.local_contains_refcounted[index];
    }

    fn domain(self: *const Inserter) *const ProcArcDomain {
        return self.current_domain orelse arcInvariant("ARC operation ran without a proc-local domain");
    }
};

fn joinPointLessThan(_: void, a: LIR.JoinPoint, b: LIR.JoinPoint) bool {
    return @intFromEnum(a.id) < @intFromEnum(b.id);
}

fn joinPointEql(a: LIR.JoinPoint, b: LIR.JoinPoint) bool {
    return a.id == b.id and a.body == b.body and localSpanEql(a.params, b.params);
}

fn localSpanEql(a: LIR.LocalSpan, b: LIR.LocalSpan) bool {
    return a.start == b.start and a.len == b.len;
}

const OwnedSet = struct {
    allocator: std.mem.Allocator,
    domain: *const ProcArcDomain,
    bits: ExactBitSet,
    /// Exact committed aggregate field places still stored in each resource.
    /// Ordinary resources have a zero full domain and ignore this row.
    residual_masks: []u64,

    fn init(allocator: std.mem.Allocator, domain: *const ProcArcDomain) ResourceError!OwnedSet {
        const bits = try ExactBitSet.initEmpty(allocator, domain.resource_locals.len);
        const residual_masks = try allocator.alloc(u64, domain.resource_locals.len);
        @memset(residual_masks, 0);
        return .{ .allocator = allocator, .domain = domain, .bits = bits, .residual_masks = residual_masks };
    }

    fn deinit(self: *OwnedSet) void {
        self.bits.deinit(self.allocator);
        self.allocator.free(self.residual_masks);
    }

    fn clone(self: *const OwnedSet) ResourceError!OwnedSet {
        const bits = try self.bits.clone(self.allocator);
        const residual_masks = try self.allocator.dupe(u64, self.residual_masks);
        return .{ .allocator = self.allocator, .domain = self.domain, .bits = bits, .residual_masks = residual_masks };
    }

    fn set(self: *OwnedSet, local: LIR.LocalId) void {
        const bit = self.domain.requiredResourceBitOf(local);
        self.bits.set(bit);
        self.residual_masks[bit] = self.domain.fullResidualMaskAt(bit);
    }

    fn unset(self: *OwnedSet, local: LIR.LocalId) void {
        // Scalars are outside the ownership lattice, so removing one is the
        // same empty-set operation the former zeroed global bit performed.
        if (self.domain.resourceBitOf(local)) |bit| {
            self.bits.unset(bit);
            self.residual_masks[bit] = 0;
        }
    }

    fn contains(self: *const OwnedSet, local: LIR.LocalId) bool {
        const bit = self.domain.resourceBitOf(local) orelse return false;
        return self.bits.isSet(bit);
    }

    fn eql(self: *const OwnedSet, other: *const OwnedSet) bool {
        self.requireSameDomain(other);
        return self.bits.eql(other.bits) and std.mem.eql(u64, self.residual_masks, other.residual_masks);
    }

    fn intersect(self: *OwnedSet, other: *const OwnedSet) void {
        self.requireSameDomain(other);
        self.bits.setIntersection(other.bits);
        for (self.residual_masks, 0..) |*mask, bit| {
            if (!self.bits.isSet(bit)) {
                mask.* = 0;
            } else {
                mask.* &= other.residual_masks[bit];
            }
        }
    }

    /// Places `local` carrying exactly `mask` of its committed field places,
    /// rather than the full ownership `set` grants. Used to copy a resource
    /// out of a meet that may have narrowed it.
    fn setWithResidual(self: *OwnedSet, local: LIR.LocalId, mask: u64) void {
        const bit = self.domain.requiredResourceBitOf(local);
        if ((mask & ~self.domain.fullResidualMaskAt(bit)) != 0) {
            arcInvariant("ARC residual placement exceeded the committed field places");
        }
        self.bits.set(bit);
        self.residual_masks[bit] = mask;
    }

    fn residualMask(self: *const OwnedSet, local: LIR.LocalId) u64 {
        const bit = self.domain.resourceBitOf(local) orelse return 0;
        return self.residual_masks[bit];
    }

    fn fullResidualMask(self: *const OwnedSet, local: LIR.LocalId) u64 {
        const bit = self.domain.resourceBitOf(local) orelse return 0;
        return self.domain.fullResidualMaskAt(bit);
    }

    fn takeResidualField(self: *OwnedSet, local: LIR.LocalId, field_mask: u64) void {
        const bit = self.domain.requiredResourceBitOf(local);
        if (!self.bits.isSet(bit)) arcInvariant("ARC field take reached an absent aggregate resource");
        if ((self.residual_masks[bit] & field_mask) != field_mask) arcInvariant("ARC field take consumed an absent committed field place");
        self.residual_masks[bit] &= ~field_mask;
    }

    fn restoreResidualField(self: *OwnedSet, local: LIR.LocalId, field_mask: u64) void {
        const bit = self.domain.requiredResourceBitOf(local);
        if (!self.bits.isSet(bit)) arcInvariant("ARC field restitution reached an absent aggregate resource");
        const full = self.domain.fullResidualMaskAt(bit);
        if ((field_mask & ~full) != 0) arcInvariant("ARC field restitution exceeded its committed aggregate field domain");
        if ((self.residual_masks[bit] & field_mask) != 0) arcInvariant("ARC field restitution duplicated a live committed field place");
        self.residual_masks[bit] |= field_mask;
    }

    fn copyResourceFrom(self: *OwnedSet, source: *const OwnedSet, local: LIR.LocalId) void {
        self.requireSameDomain(source);
        const bit = self.domain.requiredResourceBitOf(local);
        if (!source.bits.isSet(bit)) arcInvariant("ARC copied an absent ownership resource");
        self.bits.set(bit);
        self.residual_masks[bit] = source.residual_masks[bit];
    }

    fn requireSameDomain(self: *const OwnedSet, other: *const OwnedSet) void {
        if (self.domain != other.domain) arcInvariant("ARC combined ownership sets from different proc domains");
    }
};

fn refOpSource(op: LIR.RefOp) LIR.LocalId {
    return switch (op) {
        .local => |local| local,
        .discriminant => |ref| ref.source,
        .field => |ref| ref.source,
        .tag_payload => |ref| ref.source,
        .tag_payload_struct => |ref| ref.source,
        .list_reinterpret => |ref| ref.backing_ref,
        .nominal => |ref| ref.backing_ref,
    };
}

fn addCanonicalRetain(
    store: *LirStore,
    local: LIR.LocalId,
    rc: LIR.RcHelper,
    atomicity: LIR.RcAtomicity,
    count: u16,
    next: LIR.CFStmtId,
) ResourceError!LIR.CFStmtId {
    std.debug.assert(count > 0);
    var canonical_count = count;
    var canonical_next = next;
    const next_stmt = store.getCFStmt(next);
    if (next_stmt == .decref) {
        const release = next_stmt.decref;
        if (rcRetainReleasePair(.{
            .value = local,
            .rc = rc,
            .atomicity = atomicity,
        }, release)) {
            if (count == 1) return release.next;
            canonical_count = count - 1;
            canonical_next = release.next;
        }
    }
    return try store.addCFStmt(.{ .incref = .{
        .value = local,
        .rc = rc,
        .count = canonical_count,
        .atomicity = atomicity,
        .next = canonical_next,
    } });
}

fn rcRetainReleasePair(retain: anytype, release: anytype) bool {
    if (retain.value != release.value or retain.atomicity != release.atomicity) return false;
    const retain_helper = retain.rc.concreteOrNull() orelse return false;
    const release_helper = release.rc.concreteOrNull() orelse return false;
    return retain_helper.op == .incref and
        release_helper.op == .decref and
        retain_helper.layout_idx == release_helper.layout_idx;
}

fn argMaskBit(index: usize) u64 {
    if (index >= 64) arcInvariant("ARC low-level runtime mutation argument mask exceeded 64 args");
    return @as(u64, 1) << @as(u6, @intCast(index));
}

fn arcInvariant(comptime message: []const u8) noreturn {
    if (@import("builtin").mode == .Debug) std.debug.panic(message, .{});
    unreachable;
}

/// Convert an intentional fixture-table position while preserving enum inference.
fn fixtureTableIndex(comptime index: u32) u32 {
    return index;
}

test "exact ARC sets preserve operations at the inline boundary" {
    const word_bits = @bitSizeOf(usize);
    for ([_]usize{ word_bits, word_bits + 1 }) |bit_len| {
        var left = try ExactBitSet.initEmpty(testing.allocator, bit_len);
        defer left.deinit(testing.allocator);
        var right = try ExactBitSet.initEmpty(testing.allocator, bit_len);
        defer right.deinit(testing.allocator);

        left.set(0);
        left.set(bit_len - 1);
        right.set(bit_len - 1);
        left.setIntersection(right);
        try testing.expectEqual(@as(usize, 1), left.count());
        try testing.expect(left.isSet(bit_len - 1));

        var cloned = try left.clone(testing.allocator);
        defer cloned.deinit(testing.allocator);
        try testing.expect(cloned.eql(left));
        var iter = cloned.iterator(.{ .direction = .reverse });
        try testing.expectEqual(bit_len - 1, iter.next().?);
        try testing.expectEqual(@as(?usize, null), iter.next());
    }
}

test "arc insertion boundary exists" {
    std.testing.refAllDecls(@This());
}

test "RC elision removes adjacent retain release pairs" {
    var f = try ArcTest.init(testing.allocator);
    defer f.deinit();
    const value = try f.local(.str);
    const ret = try f.ret(value);
    const release = try f.store.addCFStmt(.{ .decref = .{
        .value = value,
        .rc = LIR.RcHelper.fromConcrete(.{ .op = .decref, .layout_idx = .str }),
        .next = ret,
    } });
    const retain = try addCanonicalRetain(
        &f.store,
        value,
        LIR.RcHelper.fromConcrete(.{ .op = .incref, .layout_idx = .str }),
        .atomic,
        1,
        release,
    );

    try testing.expectEqual(ret, retain);
}

test "RC elision lowers adjacent multi retain count" {
    var f = try ArcTest.init(testing.allocator);
    defer f.deinit();
    const value = try f.local(.str);
    const ret = try f.ret(value);
    const release = try f.store.addCFStmt(.{ .decref = .{
        .value = value,
        .rc = LIR.RcHelper.fromConcrete(.{ .op = .decref, .layout_idx = .str }),
        .next = ret,
    } });
    const retain = try addCanonicalRetain(
        &f.store,
        value,
        LIR.RcHelper.fromConcrete(.{ .op = .incref, .layout_idx = .str }),
        .atomic,
        3,
        release,
    );
    const stmt = f.store.getCFStmt(retain).incref;
    try testing.expectEqual(@as(u16, 2), stmt.count);
    try testing.expectEqual(ret, stmt.next);
}

const testing = std.testing;

test "ARC stale plan visit cannot overwrite a newer slot version" {
    var plans = ArcPlans{};
    defer plans.plans.deinit(testing.allocator);
    defer plans.metadata.deinit(testing.allocator);
    // The stale version returns before either stored statement id is read.
    const start: LIR.CFStmtId = undefined;
    try plans.plans.append(testing.allocator, .{ .start = start, .step_count = 7 });
    try plans.metadata.append(testing.allocator, .{ .version = 2, .scheduled = true });

    var inserter: Inserter = undefined;
    inserter.arc_plans = &plans;
    const stale = SolveSegment{
        .cursor = start,
        .owned = undefined,
        .ctx = undefined,
        .plan_index = 0,
        .plan_version = 1,
    };

    try testing.expect(!inserter.beginArcPlanUpdate(&stale));
    try testing.expectEqual(@as(u32, 7), plans.plans.items[0].step_count);
}

const ArcTest = struct {
    allocator: std.mem.Allocator,
    store: LirStore,
    layouts: layout_mod.Store,
    list_str: layout_mod.Idx,
    list_i64: layout_mod.Idx,
    box_str: layout_mod.Idx,
    pair_str: layout_mod.Idx,
    pair_list: layout_mod.Idx,
    tag_str: layout_mod.Idx,
    next_join_point: u32 = 0,

    fn init(allocator: std.mem.Allocator) Allocator.Error!ArcTest {
        var layouts = try layout_mod.Store.init(allocator, .u64);
        errdefer layouts.deinit();

        const list_str = try layouts.insertList(.str);
        const list_i64 = try layouts.insertList(.i64);
        const box_str = try layouts.insertBox(.str);
        const pair_list = try layouts.putStructFields(&[_]layout_mod.StructField{
            .{ .index = 0, .layout = list_i64 },
            .{ .index = 1, .layout = list_i64 },
        });
        const pair_str = try layouts.putStructFields(&[_]layout_mod.StructField{
            .{ .index = 0, .layout = .str },
            .{ .index = 1, .layout = .str },
        });
        const tag_str = try layouts.putTagUnion(&[_]layout_mod.Idx{
            try layouts.ensureZstLayout(),
            .str,
        });

        return .{
            .allocator = allocator,
            .store = LirStore.init(allocator),
            .layouts = layouts,
            .list_str = list_str,
            .list_i64 = list_i64,
            .pair_list = pair_list,
            .box_str = box_str,
            .pair_str = pair_str,
            .tag_str = tag_str,
        };
    }

    fn deinit(self: *ArcTest) void {
        self.store.deinit();
        self.layouts.deinit();
    }

    fn local(self: *ArcTest, layout_idx: layout_mod.Idx) Allocator.Error!LIR.LocalId {
        return try self.store.addLocal(.{ .layout_idx = layout_idx });
    }

    fn freshJoinPointId(self: *ArcTest) LIR.JoinPointId {
        const id: LIR.JoinPointId = @enumFromInt(self.next_join_point);
        self.next_join_point += 1;
        return id;
    }

    fn span(self: *ArcTest, locals: []const LIR.LocalId) Allocator.Error!LIR.LocalSpan {
        return try self.store.addLocalSpan(locals);
    }

    fn addProc(self: *ArcTest, args: []const LIR.LocalId, body: LIR.CFStmtId, ret_layout: layout_mod.Idx) Allocator.Error!LIR.LirProcSpecId {
        // Fixtures build the body before registering its proc. Supplying all
        // locals allocated so far keeps the inventory explicitly complete;
        // production lowering supplies the exact per-proc subset.
        const frame_locals = try self.allocator.alloc(LIR.LocalId, self.store.localCount());
        defer self.allocator.free(frame_locals);
        for (frame_locals, 0..) |*frame_local, index| frame_local.* = @enumFromInt(@as(u32, @intCast(index)));
        return try self.store.addProcSpec(.{
            .name = self.store.freshSyntheticSymbol(),
            .args = try self.span(args),
            .body = body,
            .frame_locals = try self.span(frame_locals),
            .ret_layout = ret_layout,
        });
    }

    fn addBodylessProc(self: *ArcTest, ret_layout: layout_mod.Idx) Allocator.Error!LIR.LirProcSpecId {
        return try self.store.addProcSpec(.{
            .name = self.store.freshSyntheticSymbol(),
            .args = LIR.LocalSpan.empty(),
            .body = null,
            .ret_layout = ret_layout,
        });
    }

    fn addHostedProc(self: *ArcTest, args: []const LIR.LocalId, ret_layout: layout_mod.Idx) Allocator.Error!LIR.LirProcSpecId {
        return try self.store.addProcSpec(.{
            .name = self.store.freshSyntheticSymbol(),
            .args = try self.span(args),
            .body = null,
            .ret_layout = ret_layout,
            .hosted = .{
                .symbol = try self.store.insertString("roc_test_hosted"),
                .dispatch_index = 0,
            },
        });
    }

    fn ret(self: *ArcTest, value: LIR.LocalId) Allocator.Error!LIR.CFStmtId {
        return try self.store.addCFStmt(.{ .ret = .{ .value = value } });
    }

    fn crash(self: *ArcTest, message: []const u8) Allocator.Error!LIR.CFStmtId {
        return try self.store.addCFStmt(.{ .crash = .{ .msg = .{ .literal = try self.store.insertString(message) } } });
    }

    fn assignI64(self: *ArcTest, target: LIR.LocalId, value: i64, next: LIR.CFStmtId) Allocator.Error!LIR.CFStmtId {
        return try self.store.addCFStmt(.{ .assign_literal = .{
            .target = target,
            .value = .{ .i64_literal = .{ .value = value, .layout_idx = .i64 } },
            .next = next,
        } });
    }

    fn assignStr(self: *ArcTest, target: LIR.LocalId, text: []const u8, next: LIR.CFStmtId) Allocator.Error!LIR.CFStmtId {
        return try self.store.addCFStmt(.{ .assign_literal = .{
            .target = target,
            .value = .{ .str_literal = try self.store.insertStringView(text, 0, @intCast(text.len)) },
            .next = next,
        } });
    }

    fn assignList(self: *ArcTest, target: LIR.LocalId, elems: []const LIR.LocalId, next: LIR.CFStmtId) Allocator.Error!LIR.CFStmtId {
        return try self.store.addCFStmt(.{ .assign_list = .{
            .target = target,
            .elems = try self.span(elems),
            .next = next,
        } });
    }

    fn assignStruct(self: *ArcTest, target: LIR.LocalId, fields: []const LIR.LocalId, next: LIR.CFStmtId) Allocator.Error!LIR.CFStmtId {
        return try self.store.addCFStmt(.{ .assign_struct = .{
            .target = target,
            .fields = try self.span(fields),
            .next = next,
        } });
    }

    fn assignTag(self: *ArcTest, target: LIR.LocalId, discriminant: u16, payload: ?LIR.LocalId, next: LIR.CFStmtId) Allocator.Error!LIR.CFStmtId {
        return try self.store.addCFStmt(.{ .assign_tag = .{
            .target = target,
            .variant_index = discriminant,
            .discriminant = discriminant,
            .payload = payload,
            .next = next,
        } });
    }

    fn assignRefLocal(self: *ArcTest, target: LIR.LocalId, source: LIR.LocalId, next: LIR.CFStmtId) Allocator.Error!LIR.CFStmtId {
        return try self.store.addCFStmt(.{ .assign_ref = .{
            .target = target,
            .op = .{ .local = source },
            .next = next,
        } });
    }

    fn assignRefReinterpret(self: *ArcTest, target: LIR.LocalId, backing: LIR.LocalId, next: LIR.CFStmtId) Allocator.Error!LIR.CFStmtId {
        return try self.store.addCFStmt(.{ .assign_ref = .{
            .target = target,
            .op = .{ .list_reinterpret = .{ .backing_ref = backing } },
            .next = next,
        } });
    }

    fn assignRefField(self: *ArcTest, target: LIR.LocalId, source: LIR.LocalId, field_idx: u16, next: LIR.CFStmtId) Allocator.Error!LIR.CFStmtId {
        return try self.store.addCFStmt(.{ .assign_ref = .{
            .target = target,
            .op = .{ .field = .{ .source = source, .field_idx = field_idx } },
            .next = next,
        } });
    }

    fn assignDiscriminant(self: *ArcTest, target: LIR.LocalId, source: LIR.LocalId, next: LIR.CFStmtId) Allocator.Error!LIR.CFStmtId {
        return try self.store.addCFStmt(.{ .assign_ref = .{
            .target = target,
            .op = .{ .discriminant = .{ .source = source } },
            .next = next,
        } });
    }

    fn assignTagPayload(self: *ArcTest, target: LIR.LocalId, source: LIR.LocalId, next: LIR.CFStmtId) Allocator.Error!LIR.CFStmtId {
        return try self.store.addCFStmt(.{ .assign_ref = .{
            .target = target,
            .op = .{ .tag_payload = .{ .source = source, .payload_idx = 0, .variant_index = 1, .tag_discriminant = 1 } },
            .next = next,
        } });
    }

    fn assignCall(self: *ArcTest, target: LIR.LocalId, args: []const LIR.LocalId, next: LIR.CFStmtId) Allocator.Error!LIR.CFStmtId {
        return try self.store.addCFStmt(.{ .assign_call = .{
            .target = target,
            .proc = try self.addBodylessProc(self.store.getLocal(target).layout_idx),
            .args = try self.span(args),
            .next = next,
        } });
    }

    fn assignHostedCall(self: *ArcTest, target: LIR.LocalId, args: []const LIR.LocalId, next: LIR.CFStmtId) Allocator.Error!LIR.CFStmtId {
        return try self.store.addCFStmt(.{ .assign_call = .{
            .target = target,
            .proc = try self.addHostedProc(args, self.store.getLocal(target).layout_idx),
            .args = try self.span(args),
            .next = next,
        } });
    }

    fn assignLowLevel(self: *ArcTest, target: LIR.LocalId, args: []const LIR.LocalId, rc_effect: LIR.LowLevel.RcEffect, next: LIR.CFStmtId) Allocator.Error!LIR.CFStmtId {
        return try self.store.addCFStmt(.{ .assign_low_level = .{
            .target = target,
            .op = .list_append_unsafe,
            .rc_effect = rc_effect,
            .args = try self.span(args),
            .next = next,
        } });
    }

    fn setLocal(self: *ArcTest, target: LIR.LocalId, value: LIR.LocalId, mode: LIR.SetLocalWriteMode, next: LIR.CFStmtId) Allocator.Error!LIR.CFStmtId {
        return try self.store.addCFStmt(.{ .set_local = .{
            .target = target,
            .value = value,
            .mode = mode,
            .next = next,
        } });
    }

    fn expectStmt(self: *ArcTest, condition: LIR.LocalId, next: LIR.CFStmtId) Allocator.Error!LIR.CFStmtId {
        return try self.store.addCFStmt(.{ .expect = .{
            .condition = condition,
            .next = next,
        } });
    }

    fn switchStmt(
        self: *ArcTest,
        cond: LIR.LocalId,
        branch_body: LIR.CFStmtId,
        default_branch: LIR.CFStmtId,
        continuation: ?LIR.CFStmtId,
    ) Allocator.Error!LIR.CFStmtId {
        return try self.switchStmtWithDefaultCold(cond, branch_body, default_branch, false, continuation);
    }

    fn switchStmtWithDefaultCold(
        self: *ArcTest,
        cond: LIR.LocalId,
        branch_body: LIR.CFStmtId,
        default_branch: LIR.CFStmtId,
        default_is_cold: bool,
        continuation: ?LIR.CFStmtId,
    ) Allocator.Error!LIR.CFStmtId {
        const branches = try self.store.addCFSwitchBranches(&[_]LIR.CFSwitchBranch{
            .{ .value = 1, .body = branch_body },
        });
        return try self.store.addCFStmt(.{ .switch_stmt = .{
            .cond = cond,
            .branches = branches,
            .default_branch = default_branch,
            .default_is_cold = default_is_cold,
            .continuation = continuation,
        } });
    }

    fn strMatchTailCapture(
        self: *ArcTest,
        source: LIR.LocalId,
        capture: LIR.LocalId,
        prefix: []const u8,
        on_match: LIR.CFStmtId,
        on_miss: LIR.CFStmtId,
    ) Allocator.Error!LIR.CFStmtId {
        const steps = try self.store.addStrMatchSteps(&[_]LIR.StrMatchStep{
            .{
                .capture = .{ .view = capture },
                .delimiter = try self.store.insertStringView("", 0, 0),
            },
        });
        return try self.store.addCFStmt(.{ .str_match = .{
            .source = source,
            .prefix = try self.store.insertStringView(prefix, 0, @intCast(prefix.len)),
            .steps = steps,
            .end = .tail,
            .on_match = on_match,
            .on_miss = on_miss,
        } });
    }

    fn run(self: *ArcTest) Allocator.Error!void {
        try insert(&self.store, &self.layouts, .{});
    }

    /// Follows linear `next` links from `start` and returns the first
    /// switch_stmt payload encountered.
    fn walkToSwitch(self: *const ArcTest, start: LIR.CFStmtId) @FieldType(LIR.CFStmt, "switch_stmt") {
        var cursor = start;
        var remaining: usize = self.store.cfStmtCount() + 1;
        while (remaining > 0) : (remaining -= 1) {
            switch (self.store.getCFStmt(cursor)) {
                .switch_stmt => |s| return s,
                .incref => |rc| cursor = rc.next,
                .decref => |rc| cursor = rc.next,
                .decref_if_initialized => |rc| cursor = rc.next,
                .free => |rc| cursor = rc.next,
                .assign_ref => |assign| cursor = assign.next,
                .assign_literal => |assign| cursor = assign.next,
                .init_uninitialized => |uninit| cursor = uninit.next,
                .assign_call => |assign| cursor = assign.next,
                .assign_call_erased => |assign| cursor = assign.next,
                .assign_packed_erased_fn => |assign| cursor = assign.next,
                inline .assign_boxy_desc_ref,
                .assign_boxy_dict_ref,
                .assign_boxy_box,
                .assign_boxy_reuse_box,
                .assign_boxy_unbox,
                .assign_boxy_adapt,
                .assign_boxy_inspect,
                .assign_boxy_eq,
                .assign_boxy_tag,
                .assign_boxy_tag_payload,
                .assign_call_dict,
                => |assign| cursor = assign.next,
                .assign_low_level => |assign| cursor = assign.next,
                .assign_list => |assign| cursor = assign.next,
                .assign_struct => |assign| cursor = assign.next,
                .assign_tag => |assign| cursor = assign.next,
                .store_struct => |assign| cursor = assign.next,
                .store_tag => |assign| cursor = assign.next,
                .set_local => |assign| cursor = assign.next,
                .debug => |debug_stmt| cursor = debug_stmt.next,
                .expect => |expect_stmt| cursor = expect_stmt.next,
                .comptime_branch_taken => |marker| cursor = marker.next,
                .expect_err,
                .runtime_error,
                .comptime_exhaustiveness_failed,
                .switch_initialized_payload,
                .str_match,
                .str_match_set,
                .boxy_tag_match,
                .loop_continue,
                .loop_break,
                .join,
                .jump,
                .ret,
                .crash,
                => arcInvariant("ARC test fixture expected a switch_stmt on the linear path"),
            }
        }
        arcInvariant("ARC test fixture cycled while walking to a switch_stmt");
    }

    fn procBody(self: *const ArcTest) LIR.CFStmtId {
        for (0..self.store.procSpecCount()) |proc_index| {
            const proc = self.store.getProcSpec(@enumFromInt(@as(u32, @intCast(proc_index))));
            if (proc.body) |body| return body;
        }
        arcInvariant("ARC test fixture has no procedure body");
    }

    fn joinBody(self: *const ArcTest, join_id: LIR.JoinPointId) LIR.CFStmtId {
        var found: ?LIR.CFStmtId = null;
        for (0..self.store.cfStmtCount()) |stmt_index| {
            const stmt = self.store.getCFStmt(@enumFromInt(@as(u32, @intCast(stmt_index))));
            if (stmt == .join) {
                if (stmt.join.id == join_id) found = stmt.join.body;
            }
        }
        return found orelse arcInvariant("ARC test fixture has no matching join body");
    }

    fn countRc(self: *const ArcTest, local_id: LIR.LocalId, kind: RcKind) usize {
        var count: usize = 0;
        for (0..self.store.cfStmtCount()) |stmt_index| {
            const stmt = self.store.getCFStmt(@enumFromInt(@as(u32, @intCast(stmt_index))));
            if (stmt == .incref and kind == .incref and stmt.incref.value == local_id) count += 1;
            if (stmt == .decref and kind == .decref and stmt.decref.value == local_id) count += 1;
            if (stmt == .decref_if_initialized and kind == .decref and stmt.decref_if_initialized.value == local_id) count += 1;
            if (stmt == .free and kind == .free and stmt.free.value == local_id) count += 1;
        }
        return count;
    }

    const ExpectError = error{ TestExpectedEqual, TestUnexpectedResult };

    fn expectRcAtomicity(self: *const ArcTest, local_id: LIR.LocalId, expected: LIR.RcAtomicity) ExpectError!void {
        var seen: usize = 0;
        for (0..self.store.cfStmtCount()) |stmt_index| {
            const stmt = self.store.getCFStmt(@enumFromInt(@as(u32, @intCast(stmt_index))));
            const found: LIR.RcAtomicity = if (stmt == .incref and stmt.incref.value == local_id)
                stmt.incref.atomicity
            else if (stmt == .decref and stmt.decref.value == local_id)
                stmt.decref.atomicity
            else if (stmt == .decref_if_initialized and stmt.decref_if_initialized.value == local_id)
                stmt.decref_if_initialized.atomicity
            else if (stmt == .free and stmt.free.value == local_id)
                stmt.free.atomicity
            else
                continue;
            seen += 1;
            try testing.expectEqual(expected, found);
        }
        try testing.expect(seen > 0);
    }

    fn uniqueArgsFor(self: *const ArcTest, target: LIR.LocalId) u64 {
        var mask: u64 = 0;
        for (0..self.store.cfStmtCount()) |stmt_index| {
            const stmt = self.store.getCFStmt(@enumFromInt(@as(u32, @intCast(stmt_index))));
            if (stmt == .assign_low_level and stmt.assign_low_level.target == target) {
                mask |= stmt.assign_low_level.unique_args;
            }
        }
        return mask;
    }

    fn reachableLowLevelAssign(self: *const ArcTest, target: LIR.LocalId) @FieldType(LIR.CFStmt, "assign_low_level") {
        var cursor = self.procBody();
        var remaining: usize = self.store.cfStmtCount() + 1;
        while (remaining > 0) : (remaining -= 1) {
            switch (self.store.getCFStmt(cursor)) {
                .assign_low_level => |assign| {
                    if (assign.target == target) return assign;
                    cursor = assign.next;
                },
                .incref => |rc| cursor = rc.next,
                .decref => |rc| cursor = rc.next,
                .decref_if_initialized => |rc| cursor = rc.next,
                .free => |rc| cursor = rc.next,
                .assign_ref => |assign| cursor = assign.next,
                .assign_literal => |assign| cursor = assign.next,
                .init_uninitialized => |uninit| cursor = uninit.next,
                .assign_call => |assign| cursor = assign.next,
                .assign_call_erased => |assign| cursor = assign.next,
                .assign_packed_erased_fn => |assign| cursor = assign.next,
                inline .assign_boxy_desc_ref,
                .assign_boxy_dict_ref,
                .assign_boxy_box,
                .assign_boxy_reuse_box,
                .assign_boxy_unbox,
                .assign_boxy_adapt,
                .assign_boxy_inspect,
                .assign_boxy_eq,
                .assign_boxy_tag,
                .assign_boxy_tag_payload,
                .assign_call_dict,
                => |assign| cursor = assign.next,
                .assign_list => |assign| cursor = assign.next,
                .assign_struct => |assign| cursor = assign.next,
                .assign_tag => |assign| cursor = assign.next,
                .store_struct => |assign| cursor = assign.next,
                .store_tag => |assign| cursor = assign.next,
                .set_local => |assign| cursor = assign.next,
                .debug => |debug_stmt| cursor = debug_stmt.next,
                .expect => |expect_stmt| cursor = expect_stmt.next,
                .comptime_branch_taken => |marker| cursor = marker.next,
                .expect_err,
                .runtime_error,
                .comptime_exhaustiveness_failed,
                .switch_stmt,
                .switch_initialized_payload,
                .str_match,
                .str_match_set,
                .boxy_tag_match,
                .loop_continue,
                .loop_break,
                .join,
                .jump,
                .ret,
                .crash,
                => arcInvariant("ARC test fixture expected a low-level op on a linear path"),
            }
        }
        arcInvariant("ARC test fixture cycled while walking to a low-level op");
    }

    /// Like `uniqueArgsFor`, but restricted to statements reachable from one
    /// proc's body, so a variant and its base proc (which share locals) can
    /// be asserted separately.
    fn uniqueArgsInProc(self: *const ArcTest, proc_id: LIR.LirProcSpecId, target: LIR.LocalId) Allocator.Error!u64 {
        var mask: u64 = 0;
        var visited = collections.DenseMap(LIR.CFStmtId, void).init(self.allocator);
        defer visited.deinit();
        var stack = std.ArrayList(LIR.CFStmtId).empty;
        defer stack.deinit(self.allocator);
        const body = self.store.getProcSpec(proc_id).body orelse return 0;
        try stack.append(self.allocator, body);
        while (stack.pop()) |current| {
            if (visited.contains(current)) continue;
            try visited.put(current, {});
            switch (self.store.getCFStmt(current)) {
                .assign_low_level => |assign| {
                    if (assign.target == target) mask |= assign.unique_args;
                    try stack.append(self.allocator, assign.next);
                },
                .switch_stmt => |s| {
                    const branches = self.store.getCFSwitchBranches(s.branches);
                    for (0..GuardedList.borrowLen(branches)) |branch_index| {
                        const branch = GuardedList.at(branches, branch_index);
                        try stack.append(self.allocator, branch.body);
                    }
                    try stack.append(self.allocator, s.default_branch);
                    if (s.continuation) |continuation| {
                        try stack.append(self.allocator, continuation);
                    }
                },
                .switch_initialized_payload => |s| {
                    try stack.append(self.allocator, s.initialized_branch);
                    try stack.append(self.allocator, s.uninitialized_branch);
                },
                .str_match => |s| {
                    try stack.append(self.allocator, s.on_match);
                    try stack.append(self.allocator, s.on_miss);
                },
                .str_match_set => |s| {
                    const arms = self.store.getStrMatchArms(s.arms);
                    for (0..GuardedList.borrowLen(arms)) |arm_index| {
                        const arm = GuardedList.at(arms, arm_index);
                        try stack.append(self.allocator, arm.on_match);
                    }
                    try stack.append(self.allocator, s.on_miss);
                },
                .boxy_tag_match => |s| {
                    try stack.append(self.allocator, s.on_match);
                    try stack.append(self.allocator, s.on_miss);
                },
                .join => |j| {
                    try stack.append(self.allocator, j.body);
                    try stack.append(self.allocator, j.remainder);
                },
                inline .assign_ref, .assign_literal, .init_uninitialized, .assign_call, .assign_call_erased, .assign_packed_erased_fn, .assign_boxy_desc_ref, .assign_boxy_dict_ref, .assign_boxy_box, .assign_boxy_reuse_box, .assign_boxy_unbox, .assign_boxy_adapt, .assign_boxy_inspect, .assign_boxy_eq, .assign_boxy_tag, .assign_boxy_tag_payload, .assign_call_dict, .assign_list, .assign_struct, .assign_tag, .store_struct, .store_tag, .set_local, .debug, .expect, .comptime_branch_taken, .incref, .decref, .decref_if_initialized, .free => |s| {
                    try stack.append(self.allocator, s.next);
                },
                .ret, .jump, .crash, .expect_err, .runtime_error, .comptime_exhaustiveness_failed, .loop_continue, .loop_break => {},
            }
        }
        return mask;
    }

    fn countAllRc(self: *const ArcTest) usize {
        var count: usize = 0;
        for (0..self.store.cfStmtCount()) |stmt_index| {
            const stmt = self.store.getCFStmt(@enumFromInt(@as(u32, @intCast(stmt_index))));
            if (stmt == .incref or stmt == .decref or stmt == .decref_if_initialized or stmt == .free) count += 1;
        }
        return count;
    }

    fn expectRc(self: *const ArcTest, local_id: LIR.LocalId, increfs: usize, decrefs: usize, frees: usize) ExpectError!void {
        try testing.expectEqual(increfs, self.countRc(local_id, .incref));
        try testing.expectEqual(decrefs, self.countRc(local_id, .decref));
        try testing.expectEqual(frees, self.countRc(local_id, .free));
    }

    fn expectReachableRcBefore(self: *const ArcTest, start: LIR.CFStmtId, kind: RcKind, local_id: LIR.LocalId, before: RcStopKind) error{ ExpectedRcBeforeStop, NonLinearPath, CyclicPath }!void {
        var cursor = start;
        var remaining: usize = self.store.cfStmtCount() + 1;
        while (remaining > 0) : (remaining -= 1) {
            const stmt = self.store.getCFStmt(cursor);
            switch (stmt) {
                .incref => |rc| {
                    if (kind == .incref and rc.value == local_id) return;
                    cursor = rc.next;
                },
                .decref => |rc| {
                    if (kind == .decref and rc.value == local_id) return;
                    cursor = rc.next;
                },
                .decref_if_initialized => |rc| {
                    if (kind == .decref and rc.value == local_id) return;
                    cursor = rc.next;
                },
                .free => |rc| {
                    if (kind == .free and rc.value == local_id) return;
                    cursor = rc.next;
                },
                .assign_ref => |assign| cursor = assign.next,
                .assign_literal => |assign| cursor = assign.next,
                .init_uninitialized => |uninit| cursor = uninit.next,
                .assign_call => |assign| cursor = assign.next,
                .assign_call_erased => |assign| cursor = assign.next,
                .assign_packed_erased_fn => |assign| cursor = assign.next,
                .assign_boxy_desc_ref => |assign| cursor = assign.next,
                .assign_boxy_dict_ref => |assign| cursor = assign.next,
                .assign_boxy_box => |assign| cursor = assign.next,
                .assign_boxy_reuse_box => |assign| cursor = assign.next,
                .assign_boxy_unbox => |assign| cursor = assign.next,
                .assign_boxy_adapt => |assign| cursor = assign.next,
                .assign_boxy_inspect => |assign| cursor = assign.next,
                .assign_boxy_eq => |assign| cursor = assign.next,
                .assign_boxy_tag => |assign| cursor = assign.next,
                .assign_boxy_tag_payload => |assign| cursor = assign.next,
                .assign_call_dict => |assign| cursor = assign.next,
                .assign_low_level => |assign| cursor = assign.next,
                .assign_list => |assign| cursor = assign.next,
                .assign_struct => |assign| cursor = assign.next,
                .assign_tag => |assign| cursor = assign.next,
                .store_struct => |assign| cursor = assign.next,
                .store_tag => |assign| cursor = assign.next,
                .set_local => |assign| cursor = assign.next,
                .debug => |debug_stmt| cursor = debug_stmt.next,
                .expect => |expect_stmt| cursor = expect_stmt.next,
                .comptime_branch_taken => |marker| cursor = marker.next,
                .ret => {
                    if (before == .ret) return error.ExpectedRcBeforeStop;
                    return;
                },
                .crash => {
                    if (before == .crash) return error.ExpectedRcBeforeStop;
                    return;
                },
                .expect_err, .runtime_error, .comptime_exhaustiveness_failed, .switch_stmt, .switch_initialized_payload, .str_match, .str_match_set, .boxy_tag_match, .loop_continue, .loop_break, .join, .jump => return error.NonLinearPath,
            }
        }
        return error.CyclicPath;
    }

    fn expectReachableConditionalDecrefBeforeSet(
        self: *const ArcTest,
        start: LIR.CFStmtId,
        value: LIR.LocalId,
        cond: LIR.LocalId,
        cond_mask: u64,
        set_target: LIR.LocalId,
    ) error{ ExpectedConditionalDecref, SetBeforeConditionalDecref, NonLinearPath, CyclicPath }!void {
        var cursor = start;
        var remaining: usize = self.store.cfStmtCount() + 1;
        while (remaining > 0) : (remaining -= 1) {
            const stmt = self.store.getCFStmt(cursor);
            switch (stmt) {
                .decref_if_initialized => |rc| {
                    if (rc.value == value and rc.cond == cond and rc.cond_mask == cond_mask) return;
                    cursor = rc.next;
                },
                .set_local => |assign| {
                    if (assign.target == set_target) return error.SetBeforeConditionalDecref;
                    cursor = assign.next;
                },
                .incref => |rc| cursor = rc.next,
                .decref => |rc| cursor = rc.next,
                .free => |rc| cursor = rc.next,
                .assign_ref => |assign| cursor = assign.next,
                .assign_literal => |assign| cursor = assign.next,
                .init_uninitialized => |uninit| cursor = uninit.next,
                .assign_call => |assign| cursor = assign.next,
                .assign_call_erased => |assign| cursor = assign.next,
                .assign_packed_erased_fn => |assign| cursor = assign.next,
                .assign_boxy_desc_ref => |assign| cursor = assign.next,
                .assign_boxy_dict_ref => |assign| cursor = assign.next,
                .assign_boxy_box => |assign| cursor = assign.next,
                .assign_boxy_reuse_box => |assign| cursor = assign.next,
                .assign_boxy_unbox => |assign| cursor = assign.next,
                .assign_boxy_adapt => |assign| cursor = assign.next,
                .assign_boxy_inspect => |assign| cursor = assign.next,
                .assign_boxy_eq => |assign| cursor = assign.next,
                .assign_boxy_tag => |assign| cursor = assign.next,
                .assign_boxy_tag_payload => |assign| cursor = assign.next,
                .assign_call_dict => |assign| cursor = assign.next,
                .assign_low_level => |assign| cursor = assign.next,
                .assign_list => |assign| cursor = assign.next,
                .assign_struct => |assign| cursor = assign.next,
                .assign_tag => |assign| cursor = assign.next,
                .store_struct => |assign| cursor = assign.next,
                .store_tag => |assign| cursor = assign.next,
                .debug => |debug_stmt| cursor = debug_stmt.next,
                .expect => |expect_stmt| cursor = expect_stmt.next,
                .comptime_branch_taken => |marker| cursor = marker.next,
                .ret, .jump, .crash, .expect_err, .runtime_error, .comptime_exhaustiveness_failed, .loop_continue, .loop_break => return error.ExpectedConditionalDecref,
                .switch_stmt, .switch_initialized_payload, .str_match, .str_match_set, .boxy_tag_match, .join => return error.NonLinearPath,
            }
        }
        return error.CyclicPath;
    }

    fn expectReachableDecrefBeforeSet(
        self: *const ArcTest,
        start: LIR.CFStmtId,
        value: LIR.LocalId,
        set_target: LIR.LocalId,
    ) error{ ExpectedDecref, SetBeforeDecref, NonLinearPath, CyclicPath }!void {
        var cursor = start;
        var remaining = self.store.cfStmtCount() + 1;
        while (remaining > 0) : (remaining -= 1) {
            const stmt = self.store.getCFStmt(cursor);
            switch (stmt) {
                .decref => |rc| {
                    if (rc.value == value) return;
                    cursor = rc.next;
                },
                .set_local => |assign| {
                    if (assign.target == set_target) return error.SetBeforeDecref;
                    cursor = assign.next;
                },
                .incref => |rc| cursor = rc.next,
                .decref_if_initialized => |rc| cursor = rc.next,
                .free => |rc| cursor = rc.next,
                .assign_ref => |assign| cursor = assign.next,
                .assign_literal => |assign| cursor = assign.next,
                .init_uninitialized => |uninit| cursor = uninit.next,
                .assign_call => |assign| cursor = assign.next,
                .assign_call_erased => |assign| cursor = assign.next,
                .assign_packed_erased_fn => |assign| cursor = assign.next,
                .assign_boxy_desc_ref => |assign| cursor = assign.next,
                .assign_boxy_dict_ref => |assign| cursor = assign.next,
                .assign_boxy_box => |assign| cursor = assign.next,
                .assign_boxy_reuse_box => |assign| cursor = assign.next,
                .assign_boxy_unbox => |assign| cursor = assign.next,
                .assign_boxy_adapt => |assign| cursor = assign.next,
                .assign_boxy_inspect => |assign| cursor = assign.next,
                .assign_boxy_eq => |assign| cursor = assign.next,
                .assign_boxy_tag => |assign| cursor = assign.next,
                .assign_boxy_tag_payload => |assign| cursor = assign.next,
                .assign_call_dict => |assign| cursor = assign.next,
                .assign_low_level => |assign| cursor = assign.next,
                .assign_list => |assign| cursor = assign.next,
                .assign_struct => |assign| cursor = assign.next,
                .assign_tag => |assign| cursor = assign.next,
                .store_struct => |assign| cursor = assign.next,
                .store_tag => |assign| cursor = assign.next,
                .debug => |debug_stmt| cursor = debug_stmt.next,
                .expect => |expect_stmt| cursor = expect_stmt.next,
                .comptime_branch_taken => |marker| cursor = marker.next,
                .ret, .crash, .expect_err, .runtime_error, .comptime_exhaustiveness_failed => return error.ExpectedDecref,
                .switch_stmt, .switch_initialized_payload, .str_match, .str_match_set, .boxy_tag_match, .loop_continue, .loop_break, .join, .jump => return error.NonLinearPath,
            }
        }
        return error.CyclicPath;
    }
};

test "ARC uses erased capture views as solver-only Boxy borrow anchors" {
    var f = try ArcTest.init(testing.allocator);
    defer f.deinit();

    const erased_box = try f.layouts.insertLayout(layout_mod.Layout.erasedBox());
    const capture_layout = try f.layouts.putStructFields(&[_]layout_mod.StructField{
        .{ .index = 0, .layout = erased_box },
        .{ .index = 1, .layout = .opaque_ptr },
    });
    const capture_ptr = try f.local(.opaque_ptr);
    const capture_view = try f.local(capture_layout);
    const captured_value = try f.local(erased_box);
    f.store.setLocalBoxyDesc(captured_value, .{ .static = @enumFromInt(fixtureTableIndex(0)) });

    const result = try f.local(.i64);
    const ret = try f.ret(result);
    const result_stmt = try f.assignI64(result, 0, ret);
    const field_read = try f.assignRefField(captured_value, capture_view, 0, result_stmt);
    _ = try f.store.addCFStmt(.{ .assign_low_level = .{
        .target = capture_view,
        .op = .erased_capture_load,
        .rc_effect = LIR.LowLevel.erased_capture_load.rcEffect(),
        .args = try f.span(&.{capture_ptr}),
        .next = field_read,
    } });

    const boxy_descs = try computeBoxyRcDescs(&f.store);
    defer f.allocator.free(boxy_descs);
    const local_contains_refcounted = try computeLocalContainsRefcounted(
        f.allocator,
        &f.store,
        &f.layouts,
        boxy_descs,
    );
    defer f.allocator.free(local_contains_refcounted);
    const borrow_anchors = try computeBorrowAnchorRefcounted(
        f.allocator,
        &f.store,
        &f.layouts,
        local_contains_refcounted,
    );
    defer f.allocator.free(borrow_anchors);

    try testing.expect(!local_contains_refcounted[@intFromEnum(capture_view)]);
    try testing.expect(local_contains_refcounted[@intFromEnum(captured_value)]);
    try testing.expect(borrow_anchors[@intFromEnum(capture_view)]);
    try testing.expect(borrow_anchors[@intFromEnum(captured_value)]);
}

test "ARC preserves erased callable repack reuse" {
    var f = try ArcTest.init(testing.allocator);
    defer f.deinit();

    const erased_callable = try f.layouts.insertErasedCallable();
    const capture = try f.local(.str);
    const old_callable = try f.local(erased_callable);
    const new_callable = try f.local(erased_callable);
    const callee_arg = try f.local(.u64);

    const callback = try f.store.addProcSpec(.{
        .name = f.store.freshSyntheticSymbol(),
        .args = try f.span(&.{callee_arg}),
        .frame_locals = try f.span(&.{callee_arg}),
        .body = null,
        .ret_layout = erased_callable,
    });

    const ret = try f.ret(new_callable);
    const result_desc: LIR.BoxyDescRef = .{ .static = @enumFromInt(fixtureTableIndex(2)) };
    const new_pack = try f.store.addCFStmt(.{ .assign_packed_erased_fn = .{
        .target = new_callable,
        .proc = callback,
        .capture = capture,
        .capture_layout = .str,
        .on_drop = .none,
        .result_desc = result_desc,
        .reuse = old_callable,
        .next = ret,
    } });
    const old_pack = try f.store.addCFStmt(.{ .assign_packed_erased_fn = .{
        .target = old_callable,
        .proc = callback,
        .capture = capture,
        .capture_layout = .str,
        .on_drop = .none,
        .next = new_pack,
    } });
    const caller = try f.addProc(&.{capture}, old_pack, erased_callable);

    try f.run();

    var cursor = f.store.getProcSpec(caller).body orelse return error.MissingCallerBody;
    var found = false;
    var remaining = f.store.cfStmtCount() + 1;
    while (remaining > 0) : (remaining -= 1) {
        switch (f.store.getCFStmt(cursor)) {
            .assign_packed_erased_fn => |assign| {
                if (assign.target == new_callable) {
                    try testing.expect(cursor != new_pack);
                    try testing.expectEqual(old_callable, assign.reuse.?);
                    try testing.expect(assign.reuse_unique);
                    try testing.expectEqual(result_desc, assign.result_desc.?);
                    found = true;
                    break;
                }
                cursor = assign.next;
            },
            .incref => |rc| cursor = rc.next,
            .decref => |rc| cursor = rc.next,
            .decref_if_initialized => |rc| cursor = rc.next,
            .free => |rc| cursor = rc.next,
            .ret => break,
            .init_uninitialized,
            .assign_ref,
            .assign_literal,
            .assign_call,
            .assign_call_erased,
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
            .assign_low_level,
            .assign_list,
            .assign_struct,
            .assign_tag,
            .store_struct,
            .store_tag,
            .set_local,
            .debug,
            .expect,
            .expect_err,
            .runtime_error,
            .comptime_exhaustiveness_failed,
            .comptime_branch_taken,
            .switch_stmt,
            .switch_initialized_payload,
            .str_match,
            .str_match_set,
            .loop_continue,
            .loop_break,
            .join,
            .jump,
            .crash,
            => return error.UnexpectedStatement,
        }
    }
    try testing.expect(found);
}

test "ARC preserves erased call ABI metadata" {
    var f = try ArcTest.init(testing.allocator);
    defer f.deinit();

    const erased_callable = try f.layouts.insertErasedCallable();
    const closure = try f.local(erased_callable);
    const arg = try f.local(.u64);
    const arg_desc = try f.local(.opaque_ptr);
    const result = try f.local(.u64);
    const out_desc = try f.local(.opaque_ptr);
    const args = try f.span(&.{arg});
    const arg_descs = try f.span(&.{arg_desc});
    const arg_layouts: LIR.BoxySpan = .{ .start = 4, .len = 1 };
    const arg_desc_keys: LIR.BoxySpan = .{ .start = 7, .len = 1 };
    const result_desc: LIR.BoxyDescRef = .{ .static = @enumFromInt(fixtureTableIndex(3)) };
    const arg_plan = try f.store.internErasedCallArgsPlan(&f.layouts, &.{.u64});

    const ret = try f.ret(result);
    const call = try f.store.addCFStmt(.{ .assign_call_erased = .{
        .target = result,
        .closure = closure,
        .args = args,
        .arg_layouts = arg_layouts,
        .arg_descs = arg_descs,
        .arg_desc_keys = arg_desc_keys,
        .result_desc = result_desc,
        .out_desc = out_desc,
        .arg_plan = arg_plan,
        .next = ret,
    } });
    const caller = try f.addProc(&.{ closure, arg, arg_desc }, call, .u64);

    try f.run();

    var cursor = f.store.getProcSpec(caller).body.?;
    var remaining = f.store.cfStmtCount() + 1;
    while (remaining > 0) : (remaining -= 1) {
        const stmt = f.store.getCFStmt(cursor);
        if (stmt == .assign_call_erased) {
            const rewritten = stmt.assign_call_erased;
            try testing.expectEqual(args, rewritten.args);
            try testing.expectEqual(arg_layouts, rewritten.arg_layouts);
            try testing.expectEqual(arg_descs, rewritten.arg_descs);
            try testing.expectEqual(arg_desc_keys, rewritten.arg_desc_keys);
            try testing.expectEqual(result_desc, rewritten.result_desc.?);
            try testing.expectEqual(out_desc, rewritten.out_desc.?);
            try testing.expectEqual(arg_plan, rewritten.arg_plan);
            return;
        } else if (stmt == .incref) {
            cursor = stmt.incref.next;
        } else if (stmt == .decref) {
            cursor = stmt.decref.next;
        } else if (stmt == .decref_if_initialized) {
            cursor = stmt.decref_if_initialized.next;
        } else if (stmt == .free) {
            cursor = stmt.free.next;
        } else {
            return error.UnexpectedStatement;
        }
    }
    return error.MissingErasedCall;
}

test "ARC runtime-checks erased callable repack from an ordinary parameter" {
    var f = try ArcTest.init(testing.allocator);
    defer f.deinit();

    const erased_callable = try f.layouts.insertErasedCallable();
    const old_callable = try f.local(erased_callable);
    const new_callable = try f.local(erased_callable);
    const capture = try f.local(.u64);
    const callee_arg = try f.local(.u64);

    const callback = try f.store.addProcSpec(.{
        .name = f.store.freshSyntheticSymbol(),
        .args = try f.span(&.{callee_arg}),
        .frame_locals = try f.span(&.{callee_arg}),
        .body = null,
        .ret_layout = erased_callable,
    });

    const ret = try f.ret(new_callable);
    const pack = try f.store.addCFStmt(.{ .assign_packed_erased_fn = .{
        .target = new_callable,
        .proc = callback,
        .capture = capture,
        .capture_layout = .u64,
        .on_drop = .none,
        .reuse = old_callable,
        .next = ret,
    } });
    const caller = try f.addProc(&.{ old_callable, capture }, pack, erased_callable);

    try f.run();

    var cursor = f.store.getProcSpec(caller).body orelse return error.MissingCallerBody;
    var remaining = f.store.cfStmtCount() + 1;
    while (remaining > 0) : (remaining -= 1) {
        switch (f.store.getCFStmt(cursor)) {
            .assign_packed_erased_fn => |assign| {
                try testing.expectEqual(old_callable, assign.reuse.?);
                try testing.expect(!assign.reuse_unique);
                return;
            },
            .incref => |rc| cursor = rc.next,
            .decref => |rc| cursor = rc.next,
            .decref_if_initialized => |rc| cursor = rc.next,
            .free => |rc| cursor = rc.next,
            .init_uninitialized,
            .assign_ref,
            .assign_literal,
            .assign_call,
            .assign_call_erased,
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
            .assign_low_level,
            .assign_list,
            .assign_struct,
            .assign_tag,
            .store_struct,
            .store_tag,
            .set_local,
            .debug,
            .expect,
            .expect_err,
            .runtime_error,
            .comptime_exhaustiveness_failed,
            .comptime_branch_taken,
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
            => return error.UnexpectedStatement,
        }
    }
    return error.MissingPackedErasedFn;
}

test "ARC transfers erased call ownership from an explicit outer source" {
    var f = try ArcTest.init(testing.allocator);
    defer f.deinit();

    const erased_callable = try f.layouts.insertErasedCallable();
    const owned_callable = try f.local(erased_callable);
    const extracted_callable = try f.local(erased_callable);
    const next_callable = try f.local(erased_callable);
    const arg_plan = try f.store.internErasedCallArgsPlan(&f.layouts, &.{});

    const ret = try f.ret(next_callable);
    const call = try f.store.addCFStmt(.{ .assign_call_erased = .{
        .target = next_callable,
        .closure = extracted_callable,
        .args = LIR.LocalSpan.empty(),
        .arg_plan = arg_plan,
        .reuse_closure = true,
        .reuse_source = owned_callable,
        .next = ret,
    } });
    const body = try f.assignRefLocal(extracted_callable, owned_callable, call);
    const caller = try f.addProc(&.{owned_callable}, body, erased_callable);

    try f.run();

    try testing.expectEqual(@as(usize, 0), f.countRc(owned_callable, .incref));
    try testing.expectEqual(@as(usize, 0), f.countRc(owned_callable, .decref));
    try testing.expectEqual(@as(usize, 0), f.countRc(extracted_callable, .incref));
    try testing.expectEqual(@as(usize, 0), f.countRc(extracted_callable, .decref));

    var cursor = f.store.getProcSpec(caller).body orelse return error.MissingCallerBody;
    var remaining = f.store.cfStmtCount() + 1;
    while (remaining > 0) : (remaining -= 1) {
        switch (f.store.getCFStmt(cursor)) {
            .assign_call_erased => |assign| {
                try testing.expect(assign.reuse_closure);
                try testing.expectEqual(owned_callable, assign.reuse_source.?);
                return;
            },
            inline .assign_ref, .incref, .decref, .decref_if_initialized, .free => |stmt| cursor = stmt.next,
            .init_uninitialized,
            .assign_literal,
            .assign_call,
            .assign_packed_erased_fn,
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
            .assign_low_level,
            .assign_list,
            .assign_struct,
            .assign_tag,
            .store_struct,
            .store_tag,
            .set_local,
            .debug,
            .expect,
            .expect_err,
            .runtime_error,
            .comptime_exhaustiveness_failed,
            .comptime_branch_taken,
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
            => return error.UnexpectedStatement,
        }
    }
    return error.MissingErasedCall;
}

test "ARC retains an erased call reuse source that is read after the call" {
    var f = try ArcTest.init(testing.allocator);
    defer f.deinit();

    const erased_callable = try f.layouts.insertErasedCallable();
    const owned_callable = try f.local(erased_callable);
    const extracted_callable = try f.local(erased_callable);
    const next_callable = try f.local(erased_callable);
    const arg_plan = try f.store.internErasedCallArgsPlan(&f.layouts, &.{});

    const ret = try f.ret(next_callable);
    const later_use = try f.expectStmt(owned_callable, ret);
    const call = try f.store.addCFStmt(.{ .assign_call_erased = .{
        .target = next_callable,
        .closure = extracted_callable,
        .args = LIR.LocalSpan.empty(),
        .arg_plan = arg_plan,
        .reuse_closure = true,
        .reuse_source = owned_callable,
        .next = later_use,
    } });
    const body = try f.assignRefLocal(extracted_callable, owned_callable, call);
    const caller = try f.addProc(&.{owned_callable}, body, erased_callable);

    try f.run();

    try testing.expectEqual(@as(usize, 1), f.countRc(owned_callable, .incref));
    try testing.expectEqual(@as(usize, 1), f.countRc(owned_callable, .decref));
    try testing.expectEqual(@as(usize, 0), f.countRc(extracted_callable, .incref));
    try testing.expectEqual(@as(usize, 0), f.countRc(extracted_callable, .decref));

    var saw_retain = false;
    var saw_call = false;
    var saw_later_use = false;
    var cursor = f.store.getProcSpec(caller).body orelse return error.MissingCallerBody;
    var remaining = f.store.cfStmtCount() + 1;
    while (remaining > 0) : (remaining -= 1) {
        switch (f.store.getCFStmt(cursor)) {
            .assign_ref => |assign| cursor = assign.next,
            .incref => |rc| {
                if (rc.value == owned_callable) {
                    try testing.expect(!saw_call);
                    saw_retain = true;
                }
                cursor = rc.next;
            },
            .assign_call_erased => |assign| {
                try testing.expect(saw_retain);
                try testing.expect(assign.reuse_closure);
                try testing.expectEqual(owned_callable, assign.reuse_source.?);
                saw_call = true;
                cursor = assign.next;
            },
            .expect => |expect_stmt| {
                if (expect_stmt.condition == owned_callable) {
                    try testing.expect(saw_call);
                    saw_later_use = true;
                }
                cursor = expect_stmt.next;
            },
            .decref => |rc| {
                if (rc.value == owned_callable) try testing.expect(saw_later_use);
                cursor = rc.next;
            },
            .decref_if_initialized => |rc| {
                if (rc.value == owned_callable) try testing.expect(saw_later_use);
                cursor = rc.next;
            },
            .free => |rc| cursor = rc.next,
            .ret => break,
            .init_uninitialized,
            .assign_literal,
            .assign_call,
            .assign_packed_erased_fn,
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
            .assign_low_level,
            .assign_list,
            .assign_struct,
            .assign_tag,
            .store_struct,
            .store_tag,
            .set_local,
            .debug,
            .expect_err,
            .runtime_error,
            .comptime_exhaustiveness_failed,
            .comptime_branch_taken,
            .switch_stmt,
            .switch_initialized_payload,
            .str_match,
            .str_match_set,
            .loop_continue,
            .loop_break,
            .join,
            .jump,
            .crash,
            => return error.UnexpectedStatement,
        }
    }
    try testing.expect(saw_retain);
    try testing.expect(saw_call);
    try testing.expect(saw_later_use);
}

test "ARC retains an erased callable whose repack input is used later" {
    var f = try ArcTest.init(testing.allocator);
    defer f.deinit();

    const erased_callable = try f.layouts.insertErasedCallable();
    const old_callable = try f.local(erased_callable);
    const new_callable = try f.local(erased_callable);
    const capture = try f.local(.u64);
    const callee_arg = try f.local(.u64);

    const callback = try f.store.addProcSpec(.{
        .name = f.store.freshSyntheticSymbol(),
        .args = try f.span(&.{callee_arg}),
        .frame_locals = try f.span(&.{callee_arg}),
        .body = null,
        .ret_layout = erased_callable,
    });

    const ret = try f.ret(new_callable);
    const later_use = try f.expectStmt(old_callable, ret);
    const pack = try f.store.addCFStmt(.{ .assign_packed_erased_fn = .{
        .target = new_callable,
        .proc = callback,
        .capture = capture,
        .capture_layout = .u64,
        .on_drop = .none,
        .reuse = old_callable,
        .next = later_use,
    } });
    const caller = try f.addProc(&.{ old_callable, capture }, pack, erased_callable);

    try f.run();

    try testing.expectEqual(@as(usize, 1), f.countRc(old_callable, .incref));
    var cursor = f.store.getProcSpec(caller).body orelse return error.MissingCallerBody;
    var remaining = f.store.cfStmtCount() + 1;
    while (remaining > 0) : (remaining -= 1) {
        switch (f.store.getCFStmt(cursor)) {
            .assign_packed_erased_fn => |assign| {
                try testing.expectEqual(old_callable, assign.reuse.?);
                try testing.expect(!assign.reuse_unique);
                return;
            },
            .incref => |rc| cursor = rc.next,
            .decref => |rc| cursor = rc.next,
            .decref_if_initialized => |rc| cursor = rc.next,
            .free => |rc| cursor = rc.next,
            .init_uninitialized,
            .assign_ref,
            .assign_literal,
            .assign_call,
            .assign_call_erased,
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
            .assign_low_level,
            .assign_list,
            .assign_struct,
            .assign_tag,
            .store_struct,
            .store_tag,
            .set_local,
            .debug,
            .expect,
            .expect_err,
            .runtime_error,
            .comptime_exhaustiveness_failed,
            .comptime_branch_taken,
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
            => return error.UnexpectedStatement,
        }
    }
    return error.MissingPackedErasedFn;
}

const RcKind = enum { incref, decref, free };
const RcStopKind = enum { ret, crash };

fn setupUnusedBinding(layout_idx: layout_mod.Idx) Allocator.Error!struct { fixture: ArcTest, value: LIR.LocalId } {
    var f = try ArcTest.init(testing.allocator);
    errdefer f.deinit();
    const value = try f.local(layout_idx);
    const result = try f.local(.i64);
    const ret = try f.ret(result);
    const body = if (layout_idx == .str)
        try f.assignStr(value, "tmp", ret)
    else
        try f.assignList(value, &.{}, ret);
    _ = try f.addProc(&.{}, body, .i64);
    try f.run();
    return .{ .fixture = f, .value = value };
}

fn setupSwitchUse(use_branch: bool, use_default: bool, use_twice_in_branch: bool, use_after: bool) Allocator.Error!struct { fixture: ArcTest, value: LIR.LocalId, branch_local: LIR.LocalId, default_local: LIR.LocalId } {
    var f = try ArcTest.init(testing.allocator);
    errdefer f.deinit();
    const value = try f.local(.str);
    const branch_local = try f.local(.i64);
    const default_local = try f.local(.i64);
    const cond = try f.local(.i64);
    const result = try f.local(.i64);

    const continuation_ret = try f.ret(if (use_after) value else result);
    const branch_tail = if (use_after) continuation_ret else try f.assignI64(result, 11, continuation_ret);
    const default_tail = if (use_after) continuation_ret else try f.assignI64(result, 22, continuation_ret);
    const branch_body = if (use_branch)
        try f.assignCall(branch_local, if (use_twice_in_branch) &.{ value, value } else &.{value}, branch_tail)
    else
        branch_tail;
    const default_body = if (use_default)
        try f.assignCall(default_local, &.{value}, default_tail)
    else
        default_tail;
    const switch_stmt = try f.switchStmt(cond, branch_body, default_body, continuation_ret);
    const cond_assign = try f.assignI64(cond, 1, switch_stmt);
    const body = try f.assignStr(value, "branch", cond_assign);
    _ = try f.addProc(&.{}, body, if (use_after) .str else .i64);
    try f.run();
    return .{ .fixture = f, .value = value, .branch_local = branch_local, .default_local = default_local };
}

fn setupMutation(reuse_after: bool) Allocator.Error!struct { fixture: ArcTest, old_value: LIR.LocalId, new_value: LIR.LocalId, target: LIR.LocalId } {
    var f = try ArcTest.init(testing.allocator);
    errdefer f.deinit();
    const target = try f.local(f.list_str);
    const new_value = try f.local(f.list_str);
    const old_value = target;
    const final_value = if (reuse_after) target else try f.local(.i64);
    const ret = try f.ret(final_value);
    const reassign = try f.setLocal(target, new_value, .replace_existing, ret);
    const new_assign = try f.assignList(new_value, &.{}, reassign);
    const body = try f.assignList(target, &.{}, new_assign);
    _ = try f.addProc(&.{}, body, if (reuse_after) f.list_str else .i64);
    try f.run();
    return .{ .fixture = f, .old_value = old_value, .new_value = new_value, .target = target };
}

test "ARC proc domain excludes scalar and other-proc locals" {
    var f = try ArcTest.init(testing.allocator);
    defer f.deinit();

    const resource = try f.local(.str);
    const scalar = try f.local(.i64);
    const other_proc_resource = try f.local(.str);

    const resource_ret = try f.ret(resource);
    const resource_body = try f.assignStr(resource, "resource", resource_ret);
    const resource_proc = try f.store.addProcSpec(.{
        .name = f.store.freshSyntheticSymbol(),
        .args = LIR.LocalSpan.empty(),
        .body = resource_body,
        .frame_locals = try f.span(&.{ resource, scalar }),
        .ret_layout = .str,
    });
    const other_ret = try f.ret(other_proc_resource);
    const other_body = try f.assignStr(other_proc_resource, "other", other_ret);
    const other_proc = try f.store.addProcSpec(.{
        .name = f.store.freshSyntheticSymbol(),
        .args = LIR.LocalSpan.empty(),
        .body = other_body,
        .frame_locals = try f.span(&.{other_proc_resource}),
        .ret_layout = .str,
    });

    const local_contains_refcounted = [_]bool{ true, false, true };
    var solution = try arc_solve.solve(testing.allocator, &f.store, &f.layouts, &local_contains_refcounted, &.{}, &.{}, true);
    defer solution.deinit();
    var global_local_index = [_]u32{ no_proc_local_index, no_proc_local_index, no_proc_local_index };
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();

    {
        var domain = try ProcArcDomain.init(
            arena.allocator(),
            &f.store,
            &solution,
            &local_contains_refcounted,
            &global_local_index,
            f.store.getProcSpec(resource_proc).frame_locals,
        );
        defer domain.clearGlobalIndices();
        try testing.expectEqual(@as(usize, 2), domain.frame_locals.len);
        try testing.expectEqual(@as(usize, 1), domain.resource_locals.len);
        try testing.expectEqual(@as(usize, 1), domain.refcounted_locals.len);
        try testing.expectEqual(@as(usize, 1), domain.livenessBitLen());
        try testing.expectEqual(resource, domain.resourceLocalAt(0));

        var owned = try OwnedSet.init(testing.allocator, &domain);
        defer owned.deinit();
        try testing.expect(!owned.contains(scalar));
        owned.unset(scalar);
        owned.set(resource);
        try testing.expect(owned.contains(resource));
    }

    var other_domain = try ProcArcDomain.init(
        arena.allocator(),
        &f.store,
        &solution,
        &local_contains_refcounted,
        &global_local_index,
        f.store.getProcSpec(other_proc).frame_locals,
    );
    defer other_domain.clearGlobalIndices();
    try testing.expectEqual(@as(usize, 1), other_domain.frame_locals.len);
    try testing.expectEqual(@as(usize, 1), other_domain.resource_locals.len);
    try testing.expectEqual(other_proc_resource, other_domain.resourceLocalAt(0));
}

test "ARC proc domain filters module-wide borrow groups to its frame" {
    var f = try ArcTest.init(testing.allocator);
    defer f.deinit();

    const leader = try f.local(.str);
    const local_alias = try f.local(.str);
    const local_ret = try f.ret(leader);
    const use_alias = try f.expectStmt(local_alias, local_ret);
    const alias_bind = try f.assignRefLocal(local_alias, leader, use_alias);
    const local_body = try f.assignStr(leader, "local", alias_bind);
    const local_proc = try f.store.addProcSpec(.{
        .name = f.store.freshSyntheticSymbol(),
        .args = LIR.LocalSpan.empty(),
        .body = local_body,
        .frame_locals = try f.span(&.{ leader, local_alias }),
        .ret_layout = .str,
    });

    const external_member = try f.local(.str);
    const external_ret = try f.ret(external_member);
    const external_body = try f.assignStr(external_member, "external", external_ret);
    _ = try f.store.addProcSpec(.{
        .name = f.store.freshSyntheticSymbol(),
        .args = LIR.LocalSpan.empty(),
        .body = external_body,
        .frame_locals = try f.span(&.{external_member}),
        .ret_layout = .str,
    });

    const local_contains_refcounted = [_]bool{ true, true, true };
    var solution = try arc_solve.solve(testing.allocator, &f.store, &f.layouts, &local_contains_refcounted, &.{}, &.{}, true);
    defer solution.deinit();
    try testing.expectEqual(leader, solution.leaderOf(local_alias));

    // Model another proc spec sharing this solved leader. The module-wide
    // group has three members, but this proc's liveness domain must contain
    // only the leader and alias named by its explicit frame.
    solution.borrowed.set(@intFromEnum(external_member));
    solution.leader[@intFromEnum(external_member)] = @intFromEnum(leader);
    solution.alias_source[@intFromEnum(external_member)] = @intFromEnum(leader);
    try testing.expectEqual(leader, solution.leaderOf(external_member));

    var global_local_index = [_]u32{ no_proc_local_index, no_proc_local_index, no_proc_local_index };
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    var domain = try ProcArcDomain.init(
        arena.allocator(),
        &f.store,
        &solution,
        &local_contains_refcounted,
        &global_local_index,
        f.store.getProcSpec(local_proc).frame_locals,
    );
    defer domain.clearGlobalIndices();

    try testing.expectEqual(@as(usize, 2), domain.frame_locals.len);
    try testing.expectEqual(@as(usize, 2), domain.resource_locals.len);
    try testing.expectEqual(@as(usize, 1), domain.group_leaders.len);
    try testing.expectEqual(@as(usize, 3), domain.livenessBitLen());
}

test "ARC lender-death query sees a live solver-only borrow anchor" {
    var f = try ArcTest.init(testing.allocator);
    defer f.deinit();

    // The solver knows `anchor` participates in ARC because it is a borrow
    // anchor, while emission knows only `concrete` has a concrete RC helper.
    // This is the same split used by descriptor-driven projections: the
    // anchor has a real liveness bit but must never receive an incref/decref.
    const anchor = try f.local(.str);
    const concrete = try f.local(.str);
    const result = try f.local(.i64);
    const ret = try f.ret(result);
    const assign_result = try f.assignI64(result, 0, ret);
    const use_anchor = try f.expectStmt(anchor, assign_result);
    const body = try f.assignRefLocal(concrete, anchor, use_anchor);
    const proc = try f.addProc(&.{anchor}, body, .i64);

    const solver_rc = [_]bool{ true, true, false };
    var solution = try arc_solve.solve(testing.allocator, &f.store, &f.layouts, &solver_rc, &.{}, &.{proc}, true);
    defer solution.deinit();
    try testing.expectEqual(anchor, solution.leaderOf(concrete));

    // Only the borrowed projection has a concrete RC representation. The
    // lender is a solver-authored anchor in the exact resource domain.
    const concrete_rc = [_]bool{ false, true, false };
    var global_local_index = [_]u32{ no_proc_local_index, no_proc_local_index, no_proc_local_index };
    var domain_arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer domain_arena.deinit();
    var domain = try ProcArcDomain.init(
        domain_arena.allocator(),
        &f.store,
        &solution,
        &concrete_rc,
        &global_local_index,
        f.store.getProcSpec(proc).frame_locals,
    );
    defer domain.clearGlobalIndices();
    try testing.expectEqual(@as(usize, 2), domain.resource_locals.len);
    try testing.expectEqual(@as(usize, 1), domain.refcounted_locals.len);

    const stmt_node_indices = try testing.allocator.alloc(u32, f.store.cfStmtCount());
    defer testing.allocator.free(stmt_node_indices);
    @memset(stmt_node_indices, no_stmt_node_index);
    var liveness_arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer liveness_arena.deinit();
    var liveness_graphs = [_]?Inserter.ReadBeforeRebindGraph{null};
    var loop_liveness_caches = std.ArrayList(LoopLivenessCache).empty;
    defer loop_liveness_caches.deinit(testing.allocator);
    try loop_liveness_caches.append(testing.allocator, .{});

    var inserter = Inserter{
        .store = &f.store,
        .layouts = &f.layouts,
        .options = .{},
        .solution = &solution,
        .current_domain = &domain,
        .current_source_proc = proc,
        .current_proc_body = body,
        .stmt_node_indices = stmt_node_indices,
        .liveness_graphs = &liveness_graphs,
        .liveness_allocator = liveness_arena.allocator(),
        .loop_liveness_caches = &loop_liveness_caches,
    };

    // Excluding the concrete projection leaves only the live solver anchor.
    // Scanning `refcounted_locals` here incorrectly reports the lender dead;
    // scanning the exact `resource_locals` domain reports it live.
    try testing.expect(try inserter.groupUsedInPathExcept(use_anchor, concrete, concrete, null));
}

test "RC pass-through: non-refcounted i64 block unchanged" {
    var f = try ArcTest.init(testing.allocator);
    defer f.deinit();
    const value = try f.local(.i64);
    const ret = try f.ret(value);
    const body = try f.assignI64(value, 42, ret);
    _ = try f.addProc(&.{}, body, .i64);
    try f.run();
    try testing.expectEqual(@as(usize, 0), f.countAllRc());
}

test "RC: string binding used twice gets incref" {
    var f = try ArcTest.init(testing.allocator);
    defer f.deinit();
    const value = try f.local(.str);
    const pair = try f.local(f.pair_str);
    const ret = try f.ret(pair);
    const struct_stmt = try f.assignStruct(pair, &.{ value, value }, ret);
    const body = try f.assignStr(value, "shared", struct_stmt);
    _ = try f.addProc(&.{}, body, f.pair_str);
    try f.run();
    // One struct slot moves the binding's unit; the second pays the only
    // retain. The pair moves out on return.
    try f.expectRc(value, 1, 0, 0);
    try f.expectRc(pair, 0, 0, 0);
}

test "RC: unused string binding gets decref" {
    var scenario = try setupUnusedBinding(.str);
    defer scenario.fixture.deinit();
    try scenario.fixture.expectRc(scenario.value, 0, 1, 0);
}

test "RC: unused list binding gets decref" {
    var f = try ArcTest.init(testing.allocator);
    defer f.deinit();
    const value = try f.local(f.list_str);
    const result = try f.local(.i64);
    const ret = try f.ret(result);
    const body = try f.assignList(value, &.{}, ret);
    _ = try f.addProc(&.{}, body, .i64);
    try f.run();
    try f.expectRc(value, 0, 1, 0);
}

test "RC borrowed string expression releases original temporary binding" {
    var f = try ArcTest.init(testing.allocator);
    defer f.deinit();
    const original = try f.local(.str);
    const alias = try f.local(.str);
    const ret = try f.ret(original);
    const alias_stmt = try f.assignRefLocal(alias, original, ret);
    const body = try f.assignStr(original, "borrow-name-kept-for-audit", alias_stmt);
    _ = try f.addProc(&.{}, body, .str);
    try f.run();
    // The alias borrows the original and the original moves out on return.
    try f.expectRc(original, 0, 0, 0);
    try f.expectRc(alias, 0, 0, 0);
}

test "RC explicit retained list element keeps outer binding cleanup" {
    var f = try ArcTest.init(testing.allocator);
    defer f.deinit();
    const list = try f.local(f.list_str);
    const elem = try f.local(.str);
    const ret = try f.ret(elem);
    const elem_ref = try f.assignRefField(elem, list, 0, ret);
    const body = try f.assignList(list, &.{}, elem_ref);
    _ = try f.addProc(&.{}, body, .str);
    try f.run();
    try f.expectRc(list, 0, 1, 0);
    try testing.expect(f.countRc(elem, .incref) >= 1);
}

test "RC if result matched later tail-cleans matched binding" {
    var f = try ArcTest.init(testing.allocator);
    defer f.deinit();
    const cond = try f.local(.i64);
    const branch_value = try f.local(.str);
    const default_value = try f.local(.str);
    const result = try f.local(.str);
    const ret = try f.ret(result);
    const branch_set = try f.setLocal(result, branch_value, .initialize_join_result, ret);
    const default_set = try f.setLocal(result, default_value, .initialize_join_result, ret);
    const switch_stmt = try f.switchStmt(cond, branch_set, default_set, ret);
    const default_assign = try f.assignStr(default_value, "default", switch_stmt);
    const branch_assign = try f.assignStr(branch_value, "branch", default_assign);
    const body = try f.assignI64(cond, 1, branch_assign);
    _ = try f.addProc(&.{}, body, .str);
    try f.run();
    // Each branch moves its value into the result and releases the other
    // branch's value; the result moves out on return.
    try testing.expectEqual(@as(usize, 0), f.countRc(result, .incref) + f.countRc(result, .decref));
    try testing.expectEqual(@as(usize, 1), f.countRc(branch_value, .decref));
    try testing.expectEqual(@as(usize, 1), f.countRc(default_value, .decref));
}

test "RC identity call result matched later tail-cleans matched binding" {
    var f = try ArcTest.init(testing.allocator);
    defer f.deinit();
    const input = try f.local(.str);
    const result = try f.local(.str);
    const ret = try f.ret(result);
    const call = try f.assignCall(result, &.{input}, ret);
    const body = try f.assignStr(input, "identity", call);
    _ = try f.addProc(&.{}, body, .str);
    try f.run();
    // The input moves into the call and the result moves out on return.
    try testing.expectEqual(@as(usize, 0), f.countAllRc());
}

test "RC repeated identity call tail-cleans the unused second result" {
    var f = try ArcTest.init(testing.allocator);
    defer f.deinit();
    const input = try f.local(.str);
    const first = try f.local(.str);
    const second = try f.local(.str);
    const ret = try f.ret(first);
    const second_call = try f.assignCall(second, &.{first}, ret);
    const first_call = try f.assignCall(first, &.{input}, second_call);
    const body = try f.assignStr(input, "identity", first_call);
    _ = try f.addProc(&.{}, body, .str);
    try f.run();
    try f.expectRc(second, 0, 1, 0);
}

test "RC mutable list binding tail-cleans borrowed final use" {
    var scenario = try setupMutation(true);
    defer scenario.fixture.deinit();
    try testing.expect(scenario.fixture.countRc(scenario.target, .decref) >= 1);
}

test "RC branch-aware: symbol used in both match branches—no incref at binding" {
    var scenario = try setupSwitchUse(true, true, false, false);
    defer scenario.fixture.deinit();
    try scenario.fixture.expectRc(scenario.value, 0, 0, 0);
}

test "RC branch-aware: symbol used in one match branch only—decref in unused branch" {
    var scenario = try setupSwitchUse(true, false, false, false);
    defer scenario.fixture.deinit();
    try scenario.fixture.expectRc(scenario.value, 0, 1, 0);
}

test "RC branch-aware: symbol used twice in one branch—incref in that branch, decref in other" {
    var scenario = try setupSwitchUse(true, false, true, false);
    defer scenario.fixture.deinit();
    try scenario.fixture.expectRc(scenario.value, 1, 1, 0);
}

test "RC branch-aware: symbol used outside and inside branches" {
    var scenario = try setupSwitchUse(true, true, false, true);
    defer scenario.fixture.deinit();
    // Each branch retains the value for its call; the value then moves out
    // on return.
    try testing.expect(scenario.fixture.countRc(scenario.value, .incref) >= 1);
    try testing.expectEqual(@as(usize, 0), scenario.fixture.countRc(scenario.value, .decref));
}

test "RC proc body: returning refcounted param does not tail-decref it" {
    var f = try ArcTest.init(testing.allocator);
    defer f.deinit();
    const param = try f.local(.str);
    const ret = try f.ret(param);
    _ = try f.addProc(&.{param}, ret, .str);
    try f.run();
    // The parameter solves borrowed and the return borrows it: no RC
    // statements at all.
    try f.expectRc(param, 0, 0, 0);
}

test "RC shared neutral proc body is rewritten separately for each proc" {
    var f = try ArcTest.init(testing.allocator);
    defer f.deinit();
    const value = try f.local(.str);
    const ret = try f.ret(value);
    const shared_body = try f.assignStr(value, "shared-proc-body", ret);
    const first = try f.addProc(&.{}, shared_body, .str);
    const second = try f.addProc(&.{}, shared_body, .str);

    try f.run();

    const first_body = f.store.getProcSpec(first).body orelse return error.MissingProcBody;
    const second_body = f.store.getProcSpec(second).body orelse return error.MissingProcBody;
    try testing.expect(first_body != shared_body);
    try testing.expect(second_body != shared_body);
    try testing.expect(first_body != second_body);
}

test "RC shared neutral join body keeps compact indices per proc" {
    var f = try ArcTest.init(testing.allocator);
    defer f.deinit();
    const value = try f.local(.str);
    const state = try f.local(.str);
    const join_id = f.freshJoinPointId();

    const ret = try f.ret(state);
    const jump = try f.store.addCFStmt(.{ .jump = .{ .target = join_id } });
    const initialize = try f.setLocal(state, value, .initialize_join_param, jump);
    const remainder = try f.assignStr(value, "shared-join", initialize);
    const shared_body = try f.store.addCFStmt(.{ .join = .{
        .id = join_id,
        .params = try f.span(&.{state}),
        .body = ret,
        .remainder = remainder,
    } });
    const first = try f.addProc(&.{}, shared_body, .str);
    const second = try f.addProc(&.{}, shared_body, .str);

    try f.run();

    const first_body = f.store.getProcSpec(first).body orelse return error.MissingProcBody;
    const second_body = f.store.getProcSpec(second).body orelse return error.MissingProcBody;
    try testing.expect(first_body != shared_body);
    try testing.expect(second_body != shared_body);
    try testing.expect(first_body != second_body);
}

test "RC shared neutral branch tail is rewritten separately for each branch" {
    var f = try ArcTest.init(testing.allocator);
    defer f.deinit();
    const cond = try f.local(.i64);
    const value = try f.local(.str);
    const result = try f.local(.str);
    const ret = try f.ret(result);
    const shared_tail = try f.assignRefLocal(result, value, ret);
    const switch_stmt = try f.switchStmt(cond, shared_tail, shared_tail, null);
    const cond_assign = try f.assignI64(cond, 1, switch_stmt);
    const body = try f.assignStr(value, "shared-branch-tail", cond_assign);
    _ = try f.addProc(&.{}, body, .str);

    try f.run();

    const rewritten = f.walkToSwitch(f.procBody());
    const branch = GuardedList.at(f.store.getCFSwitchBranches(rewritten.branches), 0);
    try testing.expect(branch.body != rewritten.default_branch);
}

test "RC proc_call caller: consumed refcounted arg is not tail-decref'd by caller" {
    var f = try ArcTest.init(testing.allocator);
    defer f.deinit();
    const arg = try f.local(.str);
    const result = try f.local(.i64);
    const ret = try f.ret(result);
    const call = try f.assignCall(result, &.{arg}, ret);
    const body = try f.assignStr(arg, "consume", call);
    _ = try f.addProc(&.{}, body, .i64);
    try f.run();
    try f.expectRc(arg, 0, 0, 0);
}

test "RC proc_call caller: consumed list arg is not tail-decref'd by caller" {
    var f = try ArcTest.init(testing.allocator);
    defer f.deinit();
    const arg = try f.local(f.list_str);
    const result = try f.local(.i64);
    const ret = try f.ret(result);
    const call = try f.assignCall(result, &.{arg}, ret);
    const body = try f.assignList(arg, &.{}, call);
    _ = try f.addProc(&.{}, body, .i64);
    try f.run();
    try f.expectRc(arg, 0, 0, 0);
}

test "RC hosted call transfers unused refcounted arg to host" {
    var f = try ArcTest.init(testing.allocator);
    defer f.deinit();
    const arg = try f.local(.str);
    const result = try f.local(.i64);
    const ret = try f.ret(result);
    const call = try f.assignHostedCall(result, &.{arg}, ret);
    const body = try f.assignStr(arg, "transferred to host", call);
    _ = try f.addProc(&.{}, body, .i64);
    try f.run();
    try f.expectRc(arg, 0, 0, 0);
}

test "RC shadowed list decl only cleans latest generation at block tail" {
    var scenario = try setupMutation(false);
    defer scenario.fixture.deinit();
    try scenario.fixture.expectRc(scenario.target, 0, 2, 0);
    try scenario.fixture.expectRc(scenario.new_value, 0, 0, 0);
}

test "RC match guard: symbol used only in guard gets proper RC ops" {
    var f = try ArcTest.init(testing.allocator);
    defer f.deinit();
    const guard = try f.local(.str);
    const result = try f.local(.i64);
    const ret = try f.ret(result);
    const guard_use = try f.expectStmt(guard, ret);
    const body = try f.assignStr(guard, "guard", guard_use);
    _ = try f.addProc(&.{}, body, .i64);
    try f.run();
    try f.expectRc(guard, 0, 1, 0);
}

test "RC match guard+body: symbol used in both guard and body gets proper RC ops" {
    var f = try ArcTest.init(testing.allocator);
    defer f.deinit();
    const value = try f.local(.str);
    const result = try f.local(.i64);
    const ret = try f.ret(value);
    const call = try f.assignCall(result, &.{value}, ret);
    const guard_use = try f.expectStmt(value, call);
    const body = try f.assignStr(value, "guard-body", guard_use);
    _ = try f.addProc(&.{}, body, .str);
    try f.run();
    // Retained once for the call while still live, then moved on return.
    try f.expectRc(value, 1, 0, 0);
}

test "RC if_then_else: then-only value is decref'd inside the else branch" {
    var f = try ArcTest.init(testing.allocator);
    defer f.deinit();
    const cond = try f.local(.bool);
    const value = try f.local(.str);
    const then_result = try f.local(.i64);
    const else_result = try f.local(.i64);
    const result = try f.local(.i64);

    // if cond { result = call(value) } else { result = 0 }; return result
    const ret = try f.ret(result);
    const then_set = try f.setLocal(result, then_result, .initialize_join_result, ret);
    const then_body = try f.assignCall(then_result, &.{value}, then_set);
    const else_set = try f.setLocal(result, else_result, .initialize_join_result, ret);
    const else_body = try f.assignI64(else_result, 0, else_set);
    const if_stmt = try f.switchStmt(cond, then_body, else_body, ret);
    const body = try f.assignStr(value, "then-only", if_stmt);
    _ = try f.addProc(&.{cond}, body, .i64);
    try f.run();

    // The then branch consumes the value's unit in the call, so the binding
    // needs no incref; the else branch owns the single release.
    try f.expectRc(value, 0, 1, 0);
    const if_after = f.walkToSwitch(f.procBody());
    try f.expectReachableRcBefore(if_after.default_branch, .decref, value, .ret);
    const then_after = GuardedList.at(f.store.getCFSwitchBranches(if_after.branches), 0).body;
    try testing.expectError(
        error.ExpectedRcBeforeStop,
        f.expectReachableRcBefore(then_after, .decref, value, .ret),
    );
}

test "RC if_then_else: condition preserves live list owner for branch body" {
    var f = try ArcTest.init(testing.allocator);
    defer f.deinit();
    const list = try f.local(f.list_str);
    const cond = try f.local(.i64);
    const branch_result = try f.local(.i64);
    const default_result = try f.local(.i64);
    const ret = try f.ret(branch_result);
    const branch_body = try f.assignCall(branch_result, &.{list}, ret);
    const default_body = try f.assignCall(default_result, &.{list}, ret);
    const switch_stmt = try f.switchStmt(cond, branch_body, default_body, ret);
    const cond_assign = try f.assignI64(cond, 1, switch_stmt);
    const body = try f.assignList(list, &.{}, cond_assign);
    _ = try f.addProc(&.{}, body, .i64);
    try f.run();
    try f.expectRc(list, 0, 0, 0);
}

test "RC nested match: symbol used in inner and outer match branches" {
    var f = try ArcTest.init(testing.allocator);
    defer f.deinit();
    const value = try f.local(.str);
    const cond_outer = try f.local(.i64);
    const cond_inner = try f.local(.i64);
    const outer_result = try f.local(.i64);
    const inner_result = try f.local(.i64);
    const ret = try f.ret(outer_result);
    const inner_branch = try f.assignCall(inner_result, &.{value}, ret);
    const inner_default = try f.assignCall(inner_result, &.{value}, ret);
    const inner_switch = try f.switchStmt(cond_inner, inner_branch, inner_default, ret);
    const outer_default = try f.assignCall(outer_result, &.{value}, ret);
    const outer_switch = try f.switchStmt(cond_outer, inner_switch, outer_default, ret);
    const inner_cond_assign = try f.assignI64(cond_inner, 1, outer_switch);
    const outer_cond_assign = try f.assignI64(cond_outer, 1, inner_cond_assign);
    const body = try f.assignStr(value, "nested", outer_cond_assign);
    _ = try f.addProc(&.{}, body, .i64);
    try f.run();
    try f.expectRc(value, 0, 0, 0);
}

test "RC nested continuation preserves outer stop when inner branch breaks outward" {
    var f = try ArcTest.init(testing.allocator);
    defer f.deinit();
    const acc = try f.local(f.list_i64);
    const outer_cond = try f.local(.i64);
    const inner_cond = try f.local(.i64);
    const ret = try f.ret(acc);
    const inner_continuation = try f.store.addCFStmt(.runtime_error);
    const inner_switch = try f.switchStmt(inner_cond, inner_continuation, ret, inner_continuation);
    const outer_switch = try f.switchStmt(outer_cond, inner_switch, ret, ret);
    const body = try f.assignList(acc, &.{}, outer_switch);
    _ = try f.addProc(&.{}, body, f.list_i64);
    try f.run();
    // Returning paths move the accumulator out; the impossible path
    // releases it.
    try testing.expectEqual(@as(usize, 0), f.countRc(acc, .incref));
    try testing.expect(f.countRc(acc, .decref) >= 1);
}

test "RC match rest prelude tail-cleans outer scrutinee binding" {
    var f = try ArcTest.init(testing.allocator);
    defer f.deinit();
    const scrutinee = try f.local(f.list_str);
    const rest = try f.local(f.list_str);
    const ret = try f.ret(scrutinee);
    const rest_ref = try f.assignRefLocal(rest, scrutinee, ret);
    const body = try f.assignList(scrutinee, &.{}, rest_ref);
    _ = try f.addProc(&.{}, body, f.list_str);
    try f.run();
    // The rest alias borrows the scrutinee, which then moves out on return.
    try f.expectRc(scrutinee, 0, 0, 0);
    try f.expectRc(rest, 0, 0, 0);
}

test "RC nested list-pattern match tail-cleans rest binding" {
    var f = try ArcTest.init(testing.allocator);
    defer f.deinit();
    const rest = try f.local(f.list_str);
    const result = try f.local(.i64);
    const ret = try f.ret(result);
    const body = try f.assignList(rest, &.{}, ret);
    _ = try f.addProc(&.{}, body, .i64);
    try f.run();
    try f.expectRc(rest, 0, 1, 0);
}

test "RC combined match rest prelude with nested list pattern cleans both owners" {
    var f = try ArcTest.init(testing.allocator);
    defer f.deinit();
    const outer = try f.local(f.list_str);
    const rest = try f.local(f.list_str);
    const result = try f.local(.i64);
    const ret = try f.ret(result);
    const rest_assign = try f.assignList(rest, &.{}, ret);
    const body = try f.assignList(outer, &.{}, rest_assign);
    _ = try f.addProc(&.{}, body, .i64);
    try f.run();
    try f.expectRc(outer, 0, 1, 0);
    try f.expectRc(rest, 0, 1, 0);
}

test "RC tag-pattern match tail-cleans outer scrutinee binding with refcounted payload" {
    var f = try ArcTest.init(testing.allocator);
    defer f.deinit();
    const payload = try f.local(.str);
    const tag_value = try f.local(f.tag_str);
    const result = try f.local(.i64);
    const ret = try f.ret(result);
    const tag_assign = try f.assignTag(tag_value, 1, payload, ret);
    const body = try f.assignStr(payload, "payload", tag_assign);
    _ = try f.addProc(&.{}, body, .i64);
    try f.run();
    try f.expectRc(tag_value, 0, 1, 0);
}

test "RC discriminant_switch: complete payload moves scrutinee unit on payload path" {
    var f = try ArcTest.init(testing.allocator);
    defer f.deinit();
    const payload = try f.local(.str);
    const tag_value = try f.local(f.tag_str);
    const disc = try f.local(.u8);
    const extracted = try f.local(.str);
    const branch_result = try f.local(.i64);
    const result = try f.local(.i64);

    // match tag_value { Tag1(extracted) => call(extracted), _ => 0 }
    const ret = try f.ret(result);
    const branch_set = try f.setLocal(result, branch_result, .initialize_join_result, ret);
    const branch_call = try f.assignCall(branch_result, &.{extracted}, branch_set);
    const branch_body = try f.assignTagPayload(extracted, tag_value, branch_call);
    const default_body = try f.assignI64(result, 0, ret);
    const switch_stmt = try f.switchStmt(disc, branch_body, default_body, ret);
    const disc_read = try f.assignDiscriminant(disc, tag_value, switch_stmt);
    const tag_assign = try f.assignTag(tag_value, 1, payload, disc_read);
    const body = try f.assignStr(payload, "payload", tag_assign);
    _ = try f.addProc(&.{}, body, .i64);
    try f.run();

    // The payload's unit moves into the tag. The proven payload path moves
    // that same unit through the complete payload projection and into the
    // call; only the no-payload path releases the scrutinee whole.
    try f.expectRc(payload, 0, 0, 0);
    try testing.expectEqual(@as(usize, 0), f.countRc(tag_value, .incref));
    try testing.expectEqual(@as(usize, 1), f.countRc(tag_value, .decref));
    const switch_after = f.walkToSwitch(f.procBody());
    try f.expectReachableRcBefore(switch_after.default_branch, .decref, tag_value, .ret);
    try testing.expectEqual(@as(usize, 0), f.countRc(extracted, .incref));
}

test "RC discriminant_switch: body-bound symbols don't get per-branch RC ops" {
    var f = try ArcTest.init(testing.allocator);
    defer f.deinit();
    const cond = try f.local(.i64);
    const branch_value = try f.local(.str);
    const default_value = try f.local(.str);
    const result = try f.local(.i64);
    const ret = try f.ret(result);
    const branch_body = try f.assignStr(branch_value, "branch-local", ret);
    const default_body = try f.assignStr(default_value, "default-local", ret);
    const switch_stmt = try f.switchStmt(cond, branch_body, default_body, null);
    const body = try f.assignI64(cond, 1, switch_stmt);
    _ = try f.addProc(&.{}, body, .i64);
    try f.run();
    try f.expectRc(branch_value, 0, 1, 0);
    try f.expectRc(default_value, 0, 1, 0);
}

test "RC tag_payload_access: complete payload moves parent unit into return" {
    var f = try ArcTest.init(testing.allocator);
    defer f.deinit();
    const payload = try f.local(.str);
    const tag_value = try f.local(f.tag_str);
    const extracted = try f.local(.str);
    const ret = try f.ret(extracted);
    const extract = try f.assignTagPayload(extracted, tag_value, ret);
    const tag_assign = try f.assignTag(tag_value, 1, payload, extract);
    const body = try f.assignStr(payload, "extract", tag_assign);
    _ = try f.addProc(&.{}, body, .str);
    try f.run();
    try f.expectRc(tag_value, 0, 0, 0);
    try testing.expectEqual(@as(usize, 0), f.countRc(extracted, .incref));
}

test "RC tag_payload_access: complete payload retains when parent remains live" {
    var f = try ArcTest.init(testing.allocator);
    defer f.deinit();
    const payload = try f.local(.str);
    const tag_value = try f.local(f.tag_str);
    const extracted = try f.local(.str);
    const call_result = try f.local(.i64);
    const result = try f.local(.i64);

    const ret = try f.ret(result);
    const result_assign = try f.assignI64(result, 1, ret);
    const use_parent = try f.expectStmt(tag_value, result_assign);
    const consume_payload = try f.assignCall(call_result, &.{extracted}, use_parent);
    const extract = try f.assignTagPayload(extracted, tag_value, consume_payload);
    const tag_assign = try f.assignTag(tag_value, 1, payload, extract);
    const body = try f.assignStr(payload, "still-live", tag_assign);
    _ = try f.addProc(&.{}, body, .i64);
    try f.run();

    try f.expectRc(tag_value, 0, 1, 0);
    try testing.expect(f.countRc(extracted, .incref) >= 1);
}

test "RC complete payload moves while parent representation has a later scalar field read" {
    var f = try ArcTest.init(testing.allocator);
    defer f.deinit();
    const record_layout = try f.layouts.putStructFields(&[_]layout_mod.StructField{
        .{ .index = 0, .layout = f.tag_str },
        .{ .index = 1, .layout = .i64 },
    });
    const payload = try f.local(.str);
    const tag_value = try f.local(f.tag_str);
    const scalar_seed = try f.local(.i64);
    const record = try f.local(record_layout);
    const tag_field = try f.local(f.tag_str);
    const extracted = try f.local(.str);
    const call_result = try f.local(.i64);
    const scalar_result = try f.local(.i64);

    const ret = try f.ret(scalar_result);
    const read_scalar = try f.assignRefField(scalar_result, record, 1, ret);
    const consume_payload = try f.assignCall(call_result, &.{extracted}, read_scalar);
    const extract = try f.assignTagPayload(extracted, tag_field, consume_payload);
    const read_tag = try f.assignRefField(tag_field, record, 0, extract);
    const make_record = try f.assignStruct(record, &.{ tag_value, scalar_seed }, read_tag);
    const make_tag = try f.assignTag(tag_value, 1, payload, make_record);
    const make_payload = try f.assignStr(payload, "payload", make_tag);
    const body = try f.assignI64(scalar_seed, 7, make_payload);
    _ = try f.addProc(&.{}, body, .i64);
    try f.run();

    // The scalar field read needs only the record's inline representation,
    // not its stored RC unit. The complete payload can therefore move without
    // manufacturing another unit, while the certifier keeps the shell read
    // distinct from any later RC-bearing use.
    try testing.expectEqual(@as(usize, 0), f.countRc(extracted, .incref));
}

test "RC early_return emits correct number of decrefs for multi-use symbol" {
    var f = try ArcTest.init(testing.allocator);
    defer f.deinit();
    const value = try f.local(.str);
    const result = try f.local(.i64);
    const early = try f.ret(value);
    const use_twice = try f.assignCall(result, &.{ value, value }, early);
    const body = try f.assignStr(value, "early", use_twice);
    _ = try f.addProc(&.{}, body, .str);
    try f.run();
    // Two retains for the doubly-consuming call, then moved on return.
    try f.expectRc(value, 2, 0, 0);
}

test "RC early_return inside branch retains for branch uses and leaves cleanup to the fallthrough" {
    var f = try ArcTest.init(testing.allocator);
    defer f.deinit();
    const cond = try f.local(.i64);
    const value = try f.local(.str);
    const branch_result = try f.local(.i64);
    const other = try f.local(.str);

    // if cond == 1 { _ = call(value, value); return value }
    // other = "other"; return other
    const early_ret = try f.ret(value);
    const branch_body = try f.assignCall(branch_result, &.{ value, value }, early_ret);
    const cont_ret = try f.ret(other);
    const continuation = try f.assignStr(other, "other", cont_ret);
    const switch_stmt = try f.switchStmt(cond, branch_body, continuation, continuation);
    const cond_assign = try f.assignI64(cond, 1, switch_stmt);
    const body = try f.assignStr(value, "early", cond_assign);
    _ = try f.addProc(&.{}, body, .str);
    try f.run();

    // The early-returning branch consumes the value three times (two call
    // args plus the return), paying two retains; its own path never releases.
    // The fallthrough path never uses the value and owns the single release.
    try f.expectRc(value, 2, 1, 0);
    try f.expectRc(other, 0, 0, 0);
    const switch_after = f.walkToSwitch(f.procBody());
    const branch_after = GuardedList.at(f.store.getCFSwitchBranches(switch_after.branches), 0).body;
    try f.expectReachableRcBefore(branch_after, .incref, value, .ret);
    try testing.expectError(
        error.ExpectedRcBeforeStop,
        f.expectReachableRcBefore(branch_after, .decref, value, .ret),
    );
    try f.expectReachableRcBefore(switch_after.default_branch, .decref, value, .ret);
}

test "RC early_return nested in call arguments gets cleanup decrefs" {
    var f = try ArcTest.init(testing.allocator);
    defer f.deinit();
    const value = try f.local(.str);
    const result = try f.local(f.box_str);
    const crash = try f.crash("nested early return");
    const use_once = try f.assignLowLevel(result, &.{value}, LIR.LowLevel.RcEffect.allocatesRetainingArgs(1), crash);
    const body = try f.assignStr(value, "nested", use_once);
    _ = try f.addProc(&.{}, body, .i64);
    try f.run();
    // The value's unit moves into the box at its final use; the box is
    // released before the crash.
    try f.expectRc(value, 0, 0, 0);
    try f.expectReachableRcBefore(f.procBody(), .decref, result, .crash);
}

test "RC join param move excludes old source from loop body ownership" {
    var f = try ArcTest.init(testing.allocator);
    defer f.deinit();
    const source = try f.local(f.list_i64);
    const state = try f.local(f.list_i64);
    const result = try f.local(.i64);
    const join_id = f.freshJoinPointId();

    const ret = try f.ret(result);
    const body = try f.assignI64(result, 1, ret);
    const jump = try f.store.addCFStmt(.{ .jump = .{ .target = join_id } });
    const initialize_state = try f.setLocal(state, source, .initialize_join_param, jump);
    const remainder = try f.assignList(source, &.{}, initialize_state);
    const join = try f.store.addCFStmt(.{ .join = .{
        .id = join_id,
        .params = try f.span(&.{state}),
        .body = body,
        .remainder = remainder,
    } });

    _ = try f.addProc(&.{}, join, .i64);
    try f.run();
    try f.expectRc(source, 0, 0, 0);
    try f.expectRc(state, 0, 1, 0);
}

test "RC switch continuation analysis stops at join ownership boundary" {
    var f = try ArcTest.init(testing.allocator);
    defer f.deinit();
    const cond = try f.local(.i64);
    const source = try f.local(f.list_i64);
    const state = try f.local(f.list_i64);
    const join_id = f.freshJoinPointId();

    const ret = try f.ret(state);
    const jump = try f.store.addCFStmt(.{ .jump = .{ .target = join_id } });
    const initialize_state = try f.setLocal(state, source, .initialize_join_param, jump);
    const remainder = try f.assignList(source, &.{}, initialize_state);
    const join = try f.store.addCFStmt(.{ .join = .{
        .id = join_id,
        .params = try f.span(&.{state}),
        .body = ret,
        .remainder = remainder,
    } });
    const switch_stmt = try f.switchStmt(cond, join, try f.store.addCFStmt(.runtime_error), ret);
    const body = try f.assignI64(cond, 1, switch_stmt);

    _ = try f.addProc(&.{}, body, f.list_i64);
    try f.run();
    try f.expectRc(source, 0, 0, 0);
}

test "RC switch preserves cold default metadata" {
    var f = try ArcTest.init(testing.allocator);
    defer f.deinit();
    const cond = try f.local(.i64);
    const result = try f.local(.i64);

    const ret = try f.ret(result);
    const branch_body = try f.assignI64(result, 11, ret);
    const default_body = try f.assignI64(result, 22, ret);
    const switch_stmt = try f.switchStmtWithDefaultCold(cond, branch_body, default_body, true, null);
    const body = try f.assignI64(cond, 1, switch_stmt);

    _ = try f.addProc(&.{}, body, .i64);
    try f.run();

    const rewritten_body = f.store.getCFStmt(f.procBody());
    if (rewritten_body != .assign_literal) arcInvariant("ARC cold-default switch test body shape changed");
    const rewritten_next = f.store.getCFStmt(rewritten_body.assign_literal.next);
    if (rewritten_next != .switch_stmt) arcInvariant("ARC cold-default switch test body shape changed");
    const rewritten_switch = rewritten_next.switch_stmt;
    try testing.expect(rewritten_switch.default_is_cold);
}

test "RC direct call preserves cold metadata" {
    var f = try ArcTest.init(testing.allocator);
    defer f.deinit();
    const result = try f.local(.i64);
    const ret = try f.ret(result);
    const call = try f.store.addCFStmt(.{ .assign_call = .{
        .target = result,
        .proc = try f.addBodylessProc(.i64),
        .args = try f.span(&.{}),
        .is_cold = true,
        .next = ret,
    } });

    _ = try f.addProc(&.{}, call, .i64);
    try f.run();

    const rewritten_body = f.store.getCFStmt(f.procBody());
    if (rewritten_body != .assign_call) arcInvariant("ARC cold-call test body shape changed");
    const rewritten_call = rewritten_body.assign_call;
    try testing.expect(rewritten_call.is_cold);
}

test "RC join remainder starts from join entry ownership" {
    var f = try ArcTest.init(testing.allocator);
    defer f.deinit();
    const pair_layout = try f.layouts.putStructFields(&[_]layout_mod.StructField{
        .{ .index = 0, .layout = f.list_i64 },
    });
    const source = try f.local(f.list_i64);
    const pair = try f.local(pair_layout);
    const extracted = try f.local(f.list_i64);
    const result = try f.local(f.list_i64);
    const elem = try f.local(.i64);
    const appended = try f.local(f.list_i64);
    const join_id = f.freshJoinPointId();

    const ret = try f.ret(result);
    const jump = try f.store.addCFStmt(.{ .jump = .{ .target = join_id } });
    const initialize_result = try f.setLocal(result, appended, .initialize_join_param, jump);
    const append = try f.assignLowLevel(appended, &.{ extracted, elem }, LIR.LowLevel.RcEffect.consumesArgsReturningConsumedArgsRetainingArgs(1, 0), initialize_result);
    const join = try f.store.addCFStmt(.{ .join = .{
        .id = join_id,
        .params = try f.span(&.{result}),
        .body = ret,
        .remainder = append,
    } });
    const extract = try f.assignRefField(extracted, pair, 0, join);
    const make_pair = try f.assignStruct(pair, &.{source}, extract);
    const body = try f.assignList(source, &.{}, make_pair);

    _ = try f.addProc(&.{}, body, f.list_i64);
    try f.run();
    // The extraction is a field take of the dying pair: the read consumes
    // the pair's stored unit for its only refcounted field, so no whole
    // release of the pair remains.
    try f.expectRc(pair, 0, 0, 0);
    try f.expectRc(extracted, 0, 0, 0);
}

test "RC join body keeps local born in remainder" {
    var f = try ArcTest.init(testing.allocator);
    defer f.deinit();
    const carried = try f.local(.str);
    const join_id = f.freshJoinPointId();

    const ret = try f.ret(carried);
    const jump = try f.store.addCFStmt(.{ .jump = .{ .target = join_id } });
    const remainder = try f.assignStr(carried, "carried", jump);
    const join = try f.store.addCFStmt(.{ .join = .{
        .id = join_id,
        .params = LIR.LocalSpan.empty(),
        .body = ret,
        .remainder = remainder,
    } });

    _ = try f.addProc(&.{}, join, .str);
    try f.run();
    try f.expectRc(carried, 0, 0, 0);
}

test "RC join body keeps remainder local through nested join jump" {
    var f = try ArcTest.init(testing.allocator);
    defer f.deinit();
    const carried = try f.local(.str);
    const outer_join_id = f.freshJoinPointId();
    const inner_join_id = f.freshJoinPointId();

    const ret = try f.ret(carried);
    const outer_jump = try f.store.addCFStmt(.{ .jump = .{ .target = outer_join_id } });
    const inner_jump = try f.store.addCFStmt(.{ .jump = .{ .target = inner_join_id } });
    const inner_join = try f.store.addCFStmt(.{ .join = .{
        .id = inner_join_id,
        .params = LIR.LocalSpan.empty(),
        .body = outer_jump,
        .remainder = inner_jump,
    } });
    const outer_remainder = try f.assignStr(carried, "nested-carried", inner_join);
    const outer_join = try f.store.addCFStmt(.{ .join = .{
        .id = outer_join_id,
        .params = LIR.LocalSpan.empty(),
        .body = ret,
        .remainder = outer_remainder,
    } });

    _ = try f.addProc(&.{}, outer_join, .str);
    try f.run();
    try f.expectRc(carried, 0, 0, 0);
}

test "RC unreachable join body does not cache nested join ownership" {
    var f = try ArcTest.init(testing.allocator);
    defer f.deinit();
    const carried = try f.local(.str);
    const result = try f.local(.str);
    const dead_join_id = f.freshJoinPointId();
    const nested_join_id = f.freshJoinPointId();

    const ret = try f.ret(result);
    const nested_jump = try f.store.addCFStmt(.{ .jump = .{ .target = nested_join_id } });
    const set_result = try f.setLocal(result, carried, .initialize_join_param, nested_jump);
    const nested_join = try f.store.addCFStmt(.{ .join = .{
        .id = nested_join_id,
        .params = try f.span(&.{result}),
        .body = ret,
        .remainder = set_result,
    } });
    const dead_join = try f.store.addCFStmt(.{ .join = .{
        .id = dead_join_id,
        .params = LIR.LocalSpan.empty(),
        .body = nested_join,
        .remainder = nested_join,
    } });
    const body = try f.assignStr(carried, "cached-carried", dead_join);

    _ = try f.addProc(&.{}, body, .str);
    try f.run();
    try f.expectRc(carried, 0, 0, 0);
}

test "RC join body keep excludes units not owned at every jump" {
    var f = try ArcTest.init(testing.allocator);
    defer f.deinit();
    const cond = try f.local(.i64);
    const elem = try f.local(.str);
    const list = try f.local(f.list_str);
    const sink = try f.local(f.list_str);
    const out = try f.local(.str);
    const join_id = f.freshJoinPointId();

    const ret = try f.ret(out);
    const body = try f.assignStr(out, "done", ret);
    const consuming_jump = try f.store.addCFStmt(.{ .jump = .{ .target = join_id } });
    const consuming_branch = try f.assignCall(sink, &.{list}, consuming_jump);
    const direct_jump = try f.store.addCFStmt(.{ .jump = .{ .target = join_id } });
    const switch_stmt = try f.switchStmt(cond, consuming_branch, direct_jump, null);
    const join = try f.store.addCFStmt(.{ .join = .{
        .id = join_id,
        .params = LIR.LocalSpan.empty(),
        .body = body,
        .remainder = switch_stmt,
    } });
    const assign_list = try f.assignList(list, &.{elem}, join);
    const assign_elem = try f.assignStr(elem, "x", assign_list);
    const start = try f.assignI64(cond, 1, assign_elem);

    _ = try f.addProc(&.{}, start, .str);
    try f.run();
    // The body keep is the intersection of the two jump states: the
    // consuming branch moves the list into the call, so the direct jump
    // cannot carry it into the shared body and releases it instead.
    try f.expectRc(list, 0, 1, 0);
    try f.expectRc(elem, 0, 0, 0);
}

test "RC nested join body keep intersects divergent jumps across frames" {
    var f = try ArcTest.init(testing.allocator);
    defer f.deinit();
    const cond = try f.local(.i64);
    const elem = try f.local(.str);
    const list = try f.local(f.list_str);
    const sink = try f.local(f.list_str);
    const out = try f.local(.str);
    const outer_id = f.freshJoinPointId();
    const inner_id = f.freshJoinPointId();

    const ret = try f.ret(out);
    const outer_body = try f.assignStr(out, "done", ret);
    const outer_jump = try f.store.addCFStmt(.{ .jump = .{ .target = outer_id } });
    const consuming_jump = try f.store.addCFStmt(.{ .jump = .{ .target = inner_id } });
    const consuming_branch = try f.assignCall(sink, &.{list}, consuming_jump);
    const direct_jump = try f.store.addCFStmt(.{ .jump = .{ .target = inner_id } });
    const switch_stmt = try f.switchStmt(cond, consuming_branch, direct_jump, null);
    const inner_join = try f.store.addCFStmt(.{ .join = .{
        .id = inner_id,
        .params = LIR.LocalSpan.empty(),
        .body = outer_jump,
        .remainder = switch_stmt,
    } });
    const outer_join = try f.store.addCFStmt(.{ .join = .{
        .id = outer_id,
        .params = LIR.LocalSpan.empty(),
        .body = outer_body,
        .remainder = inner_join,
    } });
    const assign_list = try f.assignList(list, &.{elem}, outer_join);
    const assign_elem = try f.assignStr(elem, "x", assign_list);
    const start = try f.assignI64(cond, 1, assign_elem);

    _ = try f.addProc(&.{}, start, .str);
    try f.run();
    // The inner join's shared body is a jump into the outer frame. Its keep
    // is the intersection of the two divergent jump states, so the branch
    // that still owns the list releases it before entering the shared body.
    try f.expectRc(list, 0, 1, 0);
    try f.expectRc(elem, 0, 0, 0);
}

test "RC borrow group member used in a join body keeps the lender across the jump" {
    var f = try ArcTest.init(testing.allocator);
    defer f.deinit();
    const first = try f.local(.str);
    const second = try f.local(.str);
    const pair = try f.local(f.pair_str);
    const elem = try f.local(.str);
    const result = try f.local(.i64);
    const join_id = f.freshJoinPointId();

    const ret = try f.ret(result);
    const call = try f.assignCall(result, &.{elem}, ret);
    const jump = try f.store.addCFStmt(.{ .jump = .{ .target = join_id } });
    const join = try f.store.addCFStmt(.{ .join = .{
        .id = join_id,
        .params = LIR.LocalSpan.empty(),
        .body = call,
        .remainder = jump,
    } });
    const elem_read = try f.assignRefField(elem, pair, 0, join);
    const assign_pair = try f.assignStruct(pair, &.{ first, second }, elem_read);
    const assign_second = try f.assignStr(second, "b", assign_pair);
    const start = try f.assignStr(first, "a", assign_second);

    _ = try f.addProc(&.{}, start, .i64);
    try f.run();
    // The field read is used only inside the join body, so the pair's
    // liveness group must carry its unit through the jump: the jump releases
    // nothing and the pair dies after the read's last use in the body. The
    // read is a field take, so the pair's death dismantles it—one residual
    // release of the untaken field on a fresh temporary—instead of
    // releasing the pair whole.
    try f.expectRc(pair, 0, 0, 0);
    try testing.expectEqual(@as(usize, 1), f.countAllRc());
}

test "RC switch continuation merge releases branch-divergent owner at the boundary" {
    var f = try ArcTest.init(testing.allocator);
    defer f.deinit();
    const cond = try f.local(.i64);
    const diverged = try f.local(.str);
    const consumed = try f.local(.str);
    const out = try f.local(.i64);
    const filler = try f.local(.i64);

    const ret = try f.ret(out);
    const branch = try f.assignCall(consumed, &.{diverged}, ret);
    const default_branch = try f.assignI64(filler, 7, ret);
    const switch_stmt = try f.switchStmt(cond, branch, default_branch, ret);
    const assign_out = try f.assignI64(out, 3, switch_stmt);
    const assign_diverged = try f.assignStr(diverged, "diverge", assign_out);
    const start = try f.assignI64(cond, 1, assign_diverged);

    _ = try f.addProc(&.{}, start, .i64);
    try f.run();
    // Both branches fall through to the continuation. The call branch moves
    // the string's unit into the callee, so the merged continuation entry
    // excludes it and the default branch releases it at the boundary.
    try f.expectRc(diverged, 0, 1, 0);
}

test "RC complete field projection preserves a root at implicit loop boundaries" {
    for ([_]LIR.CFStmt{ .loop_continue, .loop_break }) |terminal| {
        var f = try ArcTest.init(testing.allocator);
        defer f.deinit();
        const record_layout = try f.layouts.putStructFields(&[_]layout_mod.StructField{
            .{ .index = 0, .layout = f.list_i64 },
        });
        const field = try f.local(f.list_i64);
        const record = try f.local(record_layout);
        const state = try f.local(record_layout);
        const extracted = try f.local(f.list_i64);
        const appended = try f.local(f.list_i64);
        const elem = try f.local(.i64);

        const join_id = f.freshJoinPointId();
        const boundary = try f.store.addCFStmt(terminal);
        const consume = try f.assignLowLevel(
            appended,
            &.{ extracted, elem },
            LIR.LowLevel.RcEffect.consumesArgsReturningConsumedArgsRetainingArgs(1, 0),
            boundary,
        );
        const take = try f.assignRefField(extracted, state, 0, consume);
        const entry = try f.store.addCFStmt(.{ .jump = .{ .target = join_id } });
        const initialize_state = try f.setLocal(state, record, .initialize_join_param, entry);
        const assign_elem = try f.assignI64(elem, 1, initialize_state);
        const assign_record = try f.assignStruct(record, &.{field}, assign_elem);
        const remainder = try f.assignList(field, &.{}, assign_record);
        const body = try f.store.addCFStmt(.{ .join = .{
            .id = join_id,
            .params = try f.span(&.{state}),
            .body = take,
            .remainder = remainder,
        } });

        _ = try f.addProc(&.{}, body, .i64);
        try f.run();

        // Both loop terminals hand the kept root to an enclosing iteration
        // engine. The complete field read must retain its target instead of
        // moving that unit away.
        try f.expectRc(extracted, 1, 0, 0);
    }
}

test "RC complete field projection moves a join binding replaced before the back edge" {
    var f = try ArcTest.init(testing.allocator);
    defer f.deinit();
    const record_layout = try f.layouts.putStructFields(&[_]layout_mod.StructField{
        .{ .index = 0, .layout = f.list_i64 },
    });
    const initial_list = try f.local(f.list_i64);
    const initial_record = try f.local(record_layout);
    const state = try f.local(record_layout);
    const extracted = try f.local(f.list_i64);
    const updated = try f.local(f.list_i64);
    const next_record = try f.local(record_layout);
    const elem = try f.local(.i64);

    const join_id = f.freshJoinPointId();
    const jump = try f.store.addCFStmt(.{ .jump = .{ .target = join_id } });
    const set_next = try f.setLocal(state, next_record, .initialize_join_param, jump);
    const build_next = try f.assignStruct(next_record, &.{updated}, set_next);
    const consume = try f.assignLowLevel(
        updated,
        &.{ extracted, elem },
        LIR.LowLevel.RcEffect.consumesArgsReturningConsumedArgsRetainingArgs(1, 0),
        build_next,
    );
    const take = try f.assignRefField(extracted, state, 0, consume);
    const enter = try f.store.addCFStmt(.{ .jump = .{ .target = join_id } });
    const initialize_state = try f.setLocal(state, initial_record, .initialize_join_param, enter);
    const assign_elem = try f.assignI64(elem, 1, initialize_state);
    const assign_record = try f.assignStruct(initial_record, &.{initial_list}, assign_elem);
    const remainder = try f.assignList(initial_list, &.{}, assign_record);
    const body = try f.store.addCFStmt(.{ .join = .{
        .id = join_id,
        .params = try f.span(&.{state}),
        .body = take,
        .remainder = remainder,
    } });

    _ = try f.addProc(&.{}, body, .i64);
    try f.run();

    // The state cell is explicitly rebound before the back edge. The next
    // iteration's state is a new definition, so the current record's sole RC
    // field moves into `extracted` without an incref.
    try f.expectRc(extracted, 0, 0, 0);
}

test "RC divergent field takes normalize exact residual places on each switch edge" {
    var f = try ArcTest.init(testing.allocator);
    defer f.deinit();
    const cond = try f.local(.i64);
    const first = try f.local(.str);
    const second = try f.local(.str);
    const pair = try f.local(f.pair_str);
    const first_read = try f.local(.str);
    const second_read = try f.local(.str);
    const first_sink = try f.local(.i64);
    const second_sink = try f.local(.i64);
    const result = try f.local(.i64);

    const ret = try f.ret(result);
    const first_call = try f.assignCall(first_sink, &.{first_read}, ret);
    const first_branch = try f.assignRefField(first_read, pair, 0, first_call);
    const second_call = try f.assignCall(second_sink, &.{second_read}, ret);
    const second_branch = try f.assignRefField(second_read, pair, 1, second_call);
    const choose = try f.switchStmt(cond, first_branch, second_branch, ret);
    const assign_result = try f.assignI64(result, 7, choose);
    const assign_pair = try f.assignStruct(pair, &.{ first, second }, assign_result);
    const assign_second = try f.assignStr(second, "second", assign_pair);
    const assign_first = try f.assignStr(first, "first", assign_second);
    const body = try f.assignI64(cond, 1, assign_first);

    _ = try f.addProc(&.{}, body, .i64);
    try f.run();

    // Each arm moves a different stored string into its call. Its edge then
    // releases exactly the opposite residual field before the common
    // continuation; neither projection needs a retain and no path releases
    // the partially dismantled pair as a whole.
    try f.expectRc(first_read, 0, 0, 0);
    try f.expectRc(second_read, 0, 0, 0);
    try f.expectRc(pair, 0, 0, 0);
    try testing.expectEqual(@as(usize, 2), f.countAllRc());
}

fn chainedJoinSolveWork(join_count: usize) Allocator.Error!u64 {
    var f = try ArcTest.init(testing.allocator);
    defer f.deinit();
    const carried = try f.local(.str);

    var current = try f.ret(carried);
    for (0..join_count) |_| {
        const id = f.freshJoinPointId();
        const jump = try f.store.addCFStmt(.{ .jump = .{ .target = id } });
        current = try f.store.addCFStmt(.{ .join = .{
            .id = id,
            .params = LIR.LocalSpan.empty(),
            .body = current,
            .remainder = jump,
        } });
    }
    const start = try f.assignStr(carried, "chained", current);

    _ = try f.addProc(&.{}, start, .str);
    // The delta over the process-global counter is meaningful because the
    // test runner executes tests in one thread; nothing else runs `insert`
    // between the two reads.
    const before = solver_iterations;
    try f.run();
    return solver_iterations - before;
}

test "RC join summary solver work grows linearly with chained joins" {
    if (builtin.mode != .Debug) return;
    const small = try chainedJoinSolveWork(8);
    const large = try chainedJoinSolveWork(16);
    // Doubling the join count must stay near double the solver work;
    // per-join region re-walks would grow it quadratically.
    try testing.expect(large <= small * 3);
}

test "RC join loop jump releases body-only list but keeps carried state" {
    var f = try ArcTest.init(testing.allocator);
    defer f.deinit();
    const source = try f.local(f.list_i64);
    const state = try f.local(f.list_i64);
    const scratch = try f.local(f.list_i64);
    const next_state = try f.local(f.list_i64);
    const join_id = f.freshJoinPointId();

    const body_jump = try f.store.addCFStmt(.{ .jump = .{ .target = join_id } });
    const set_next_state = try f.setLocal(state, next_state, .initialize_join_param, body_jump);
    const next_state_assign = try f.store.addCFStmt(.{ .assign_low_level = .{
        .target = next_state,
        .op = .list_reverse,
        .rc_effect = LIR.LowLevel.RcEffect.runtimeUniqueness(1),
        .args = try f.span(&.{state}),
        .next = set_next_state,
    } });
    const body = try f.assignList(scratch, &.{}, next_state_assign);

    const initial_jump = try f.store.addCFStmt(.{ .jump = .{ .target = join_id } });
    const initialize_state = try f.setLocal(state, source, .initialize_join_param, initial_jump);
    const remainder = try f.assignList(source, &.{}, initialize_state);
    const join = try f.store.addCFStmt(.{ .join = .{
        .id = join_id,
        .params = try f.span(&.{state}),
        .body = body,
        .remainder = remainder,
    } });

    _ = try f.addProc(&.{}, join, .i64);
    try f.run();
    try f.expectRc(scratch, 0, 1, 0);
    // The old state is consumed by the op that produces the next state.
    try f.expectRc(state, 0, 0, 0);
}

test "RC join loop retains an aggregate param projection across the back edge" {
    // A loop body projects field 0 of a carried pair into a call, and the back
    // edge never rebinds the pair. ARC retains the projection rather than
    // moving it: the take site sits inside the body, so it is itself a later
    // use of field 0 on the next iteration, and the pair is live across the
    // back edge. The edge therefore carries the pair's full residual mask, and
    // the pair is released exactly once, never while partially dismantled.
    //
    // This pins that retain-not-move outcome. It does NOT cover the partial
    // residual mask path in `placeSurvivingParam`/`setWithResidual`: no LIR
    // built through this fixture reaches a back edge holding a strict subset
    // of an aggregate's committed field places, because any projection whose
    // aggregate survives the edge must be retained. The exact resource meet in
    // `absorbBackEdgeParams` is what keeps that path correct if it is ever
    // reachable; it is not relied on for the behaviour asserted below.
    var f = try ArcTest.init(testing.allocator);
    defer f.deinit();
    const first = try f.local(.str);
    const second = try f.local(.str);
    const source = try f.local(f.pair_str);
    const pair = try f.local(f.pair_str);
    const flag = try f.local(.i64);
    const first_flag = try f.local(.i64);
    const cleared_flag = try f.local(.i64);
    const taken = try f.local(.str);
    const sink = try f.local(.i64);
    const result = try f.local(.i64);
    const join_id = f.freshJoinPointId();

    // Back edge: clears the flag and jumps without rebinding `pair`.
    const back_jump = try f.store.addCFStmt(.{ .jump = .{ .target = join_id } });
    const set_flag = try f.setLocal(flag, cleared_flag, .initialize_join_param, back_jump);
    const clear_flag = try f.assignI64(cleared_flag, 0, set_flag);
    const consume = try f.assignCall(sink, &.{taken}, clear_flag);
    const take_field = try f.assignRefField(taken, pair, 0, consume);

    // Exit edge.
    const exit = try f.ret(result);
    const assign_result = try f.assignI64(result, 7, exit);

    const body = try f.switchStmt(flag, take_field, assign_result, null);

    const entry_jump = try f.store.addCFStmt(.{ .jump = .{ .target = join_id } });
    const init_flag = try f.setLocal(flag, first_flag, .initialize_join_param, entry_jump);
    const init_pair = try f.setLocal(pair, source, .initialize_join_param, init_flag);
    const assign_flag = try f.assignI64(first_flag, 1, init_pair);
    const assign_pair = try f.assignStruct(source, &.{ first, second }, assign_flag);
    const assign_second = try f.assignStr(second, "second", assign_pair);
    const remainder = try f.assignStr(first, "first", assign_second);

    const join = try f.store.addCFStmt(.{ .join = .{
        .id = join_id,
        .params = try f.span(&.{ pair, flag }),
        .body = body,
        .remainder = remainder,
    } });

    _ = try f.addProc(&.{}, join, .i64);
    try f.run();

    try f.expectRc(taken, 1, 0, 0);
    try f.expectRc(pair, 0, 1, 0);
    try testing.expectEqual(@as(usize, 2), f.countAllRc());
}

test "RC join loop exit releases body-only list and preserves returned state" {
    var f = try ArcTest.init(testing.allocator);
    defer f.deinit();
    const source = try f.local(f.list_i64);
    const state = try f.local(f.list_i64);
    const scratch = try f.local(f.list_i64);
    const join_id = f.freshJoinPointId();

    const ret = try f.ret(state);
    const body = try f.assignList(scratch, &.{}, ret);

    const initial_jump = try f.store.addCFStmt(.{ .jump = .{ .target = join_id } });
    const initialize_state = try f.setLocal(state, source, .initialize_join_param, initial_jump);
    const remainder = try f.assignList(source, &.{}, initialize_state);
    const join = try f.store.addCFStmt(.{ .join = .{
        .id = join_id,
        .params = try f.span(&.{state}),
        .body = body,
        .remainder = remainder,
    } });

    _ = try f.addProc(&.{}, join, f.list_i64);
    try f.run();
    try f.expectRc(scratch, 0, 1, 0);
    // The carried state moves out on return.
    try f.expectRc(state, 0, 0, 0);
}

test "RC maybe-initialized join payload releases conditionally on loop exit" {
    var f = try ArcTest.init(testing.allocator);
    defer f.deinit();
    const selector = try f.local(.i64);
    const present = try f.local(.i64);
    const payload = try f.local(.str);
    const result = try f.local(.i64);
    const join_id = f.freshJoinPointId();

    const ret = try f.ret(result);
    const body = try f.assignI64(result, 1, ret);

    const present_jump = try f.store.addCFStmt(.{ .jump = .{ .target = join_id } });
    const present_payload = try f.assignStr(payload, "present", present_jump);
    const present_cond = try f.assignI64(present, 1, present_payload);

    const absent_jump = try f.store.addCFStmt(.{ .jump = .{ .target = join_id } });
    const absent_cond = try f.assignI64(present, 0, absent_jump);

    const switch_stmt = try f.switchStmt(selector, present_cond, absent_cond, null);
    const remainder = try f.assignI64(selector, 1, switch_stmt);
    const join = try f.store.addCFStmt(.{ .join = .{
        .id = join_id,
        .params = try f.span(&.{ payload, present }),
        .maybe_uninitialized_params = try f.span(&.{payload}),
        .maybe_uninitialized_conditions = try f.span(&.{present}),
        .maybe_uninitialized_condition_masks = try f.store.addU64Span(&.{1}),
        .body = body,
        .remainder = remainder,
    } });

    _ = try f.addProc(&.{}, join, .i64);
    try f.run();
    try f.expectRc(payload, 0, 1, 0);
    try f.expectReachableConditionalDecrefBeforeSet(f.joinBody(join_id), payload, present, 1, present);
}

test "RC maybe-initialized join payload overwrite tests old presence before setting new presence" {
    var f = try ArcTest.init(testing.allocator);
    defer f.deinit();
    const present = try f.local(.i64);
    const next_present = try f.local(.i64);
    const payload = try f.local(.str);
    const next_payload = try f.local(.str);
    const join_id = f.freshJoinPointId();

    const body_jump = try f.store.addCFStmt(.{ .jump = .{ .target = join_id } });
    const set_present = try f.setLocal(present, next_present, .initialize_join_param, body_jump);
    const set_payload = try f.setLocal(payload, next_payload, .initialize_join_param, set_present);
    const assign_payload = try f.assignStr(next_payload, "next", set_payload);
    const body = try f.assignI64(next_present, 1, assign_payload);

    const initial_jump = try f.store.addCFStmt(.{ .jump = .{ .target = join_id } });
    const remainder = try f.assignI64(present, 0, initial_jump);
    const join = try f.store.addCFStmt(.{ .join = .{
        .id = join_id,
        .params = try f.span(&.{ payload, present }),
        .maybe_uninitialized_params = try f.span(&.{payload}),
        .maybe_uninitialized_conditions = try f.span(&.{present}),
        .maybe_uninitialized_condition_masks = try f.store.addU64Span(&.{1}),
        .body = body,
        .remainder = remainder,
    } });

    _ = try f.addProc(&.{}, join, .i64);
    try f.run();
    try f.expectReachableConditionalDecrefBeforeSet(f.joinBody(join_id), payload, present, 1, present);
}

test "RC iterator join borrowed element used twice gets increfs and no decref" {
    var f = try ArcTest.init(testing.allocator);
    defer f.deinit();
    const pair = try f.local(f.pair_str);
    const elem = try f.local(.str);
    const result = try f.local(.i64);
    const join_id = f.freshJoinPointId();

    const ret = try f.ret(result);
    const body = try f.assignCall(result, &.{ elem, elem }, ret);
    const elem_read = try f.assignRefField(elem, pair, 0, body);
    const jump = try f.store.addCFStmt(.{ .jump = .{ .target = join_id } });
    const join = try f.store.addCFStmt(.{ .join = .{
        .id = join_id,
        .params = LIR.LocalSpan.empty(),
        .body = elem_read,
        .remainder = jump,
    } });

    _ = try f.addProc(&.{pair}, join, .i64);
    try f.run();
    // The pair parameter stays borrowed; the consumed element pays one
    // retain at the read and one for the second call slot, and never needs
    // a release.
    try f.expectRc(pair, 0, 0, 0);
    try f.expectRc(elem, 2, 0, 0);
}

test "RC iterator join unused borrowed element has no RC statements" {
    var f = try ArcTest.init(testing.allocator);
    defer f.deinit();
    const elem = try f.local(.str);
    const result = try f.local(.i64);
    const join_id = f.freshJoinPointId();

    const ret = try f.ret(result);
    const body = try f.assignI64(result, 1, ret);
    const jump = try f.store.addCFStmt(.{ .jump = .{ .target = join_id } });
    const join = try f.store.addCFStmt(.{ .join = .{
        .id = join_id,
        .params = LIR.LocalSpan.empty(),
        .body = body,
        .remainder = jump,
    } });

    _ = try f.addProc(&.{}, join, .i64);
    try f.run();
    try f.expectRc(elem, 0, 0, 0);
}

test "RC alias of a loop join parameter moves into the next join" {
    var f = try ArcTest.init(testing.allocator);
    defer f.deinit();
    const source = try f.local(f.list_i64);
    const state = try f.local(f.list_i64);
    const carried = try f.local(f.list_i64);
    const alias = try f.local(f.list_i64);
    const next = try f.local(f.list_i64);
    const loop_id = f.freshJoinPointId();
    const step_id = f.freshJoinPointId();

    // Loop join A(state) whose body advances the state and enters join
    // B(carried); B's body aliases its parameter and re-initializes A's.
    const back_jump = try f.store.addCFStmt(.{ .jump = .{ .target = loop_id } });
    const reinitialize_state = try f.setLocal(state, alias, .initialize_join_param, back_jump);
    const step_body = try f.assignRefLocal(alias, carried, reinitialize_state);

    const step_jump = try f.store.addCFStmt(.{ .jump = .{ .target = step_id } });
    const initialize_carried = try f.setLocal(carried, next, .initialize_join_param, step_jump);
    const advance = try f.store.addCFStmt(.{ .assign_low_level = .{
        .target = next,
        .op = .list_reverse,
        .rc_effect = LIR.LowLevel.RcEffect.runtimeUniqueness(1),
        .args = try f.span(&.{state}),
        .next = initialize_carried,
    } });
    const step_join = try f.store.addCFStmt(.{ .join = .{
        .id = step_id,
        .params = try f.span(&.{carried}),
        .body = step_body,
        .remainder = advance,
    } });

    const initial_jump = try f.store.addCFStmt(.{ .jump = .{ .target = loop_id } });
    const initialize_state = try f.setLocal(state, source, .initialize_join_param, initial_jump);
    const remainder = try f.assignList(source, &.{}, initialize_state);
    const loop_join = try f.store.addCFStmt(.{ .join = .{
        .id = loop_id,
        .params = try f.span(&.{state}),
        .body = step_join,
        .remainder = remainder,
    } });

    _ = try f.addProc(&.{}, loop_join, .i64);
    try f.run();
    // One unit circulates: state moves into the advance op, its result moves
    // into B's parameter, and B's body moves it back into A's parameter
    // through the alias. No retain or release belongs anywhere on the cycle.
    try testing.expectEqual(@as(usize, 0), f.countAllRc());
}

test "RC atomicity: confined values update counts single-threaded" {
    var f = try ArcTest.init(testing.allocator);
    defer f.deinit();
    const list = try f.local(f.list_i64);
    const pair = try f.local(f.pair_list);
    const result = try f.local(.i64);

    // list = []; pair = {list, list}; result = 1; ret result
    const ret = try f.ret(result);
    const result_assign = try f.assignI64(result, 1, ret);
    const pair_assign = try f.assignStruct(pair, &.{ list, list }, result_assign);
    const body = try f.assignList(list, &.{}, pair_assign);
    _ = try f.addProc(&.{}, body, .i64);

    try f.run();
    // No proc is a root and nothing reaches a host boundary, so every count
    // update may use plain loads and stores.
    try f.expectRcAtomicity(list, .single_thread);
    try f.expectRcAtomicity(pair, .single_thread);
}

test "RC atomicity: root-returned values keep atomic counts" {
    var f = try ArcTest.init(testing.allocator);
    defer f.deinit();
    const list = try f.local(f.list_i64);
    const pair = try f.local(f.pair_list);

    // list = []; pair = {list, list}; ret pair—the pair reaches the host
    // through the root return, and the list is reachable from the pair.
    const ret = try f.ret(pair);
    const pair_assign = try f.assignStruct(pair, &.{ list, list }, ret);
    const body = try f.assignList(list, &.{}, pair_assign);
    const proc = try f.addProc(&.{}, body, f.pair_list);

    try insert(&f.store, &f.layouts, .{ .roots = &.{proc} });
    try f.expectRcAtomicity(list, .atomic);
}

test "RC atomicity: bodyless callee arguments keep atomic counts" {
    var f = try ArcTest.init(testing.allocator);
    defer f.deinit();
    const list = try f.local(f.list_i64);
    const alias = try f.local(f.list_i64);
    const call_result = try f.local(.i64);
    const result = try f.local(.i64);

    const hosted = try f.store.addProcSpec(.{
        .name = f.store.freshSyntheticSymbol(),
        .args = try f.span(&.{}),
        .body = null,
        .ret_layout = .i64,
    });

    // list = []; alias = list; call hosted(list); expect(alias); ret 1—
    // the call's argument crosses a boundary the solver cannot see into.
    const ret = try f.ret(result);
    const result_assign = try f.assignI64(result, 1, ret);
    const use_alias = try f.expectStmt(alias, result_assign);
    const call = try f.store.addCFStmt(.{ .assign_call = .{
        .target = call_result,
        .proc = hosted,
        .args = try f.span(&.{list}),
        .next = use_alias,
    } });
    const alias_assign = try f.assignRefLocal(alias, list, call);
    const body = try f.assignList(list, &.{}, alias_assign);
    _ = try f.addProc(&.{}, body, .i64);

    try f.run();
    try f.expectRcAtomicity(list, .atomic);
}

test "uniqueness: freshly built list consumed by a checked op elides the check" {
    var f = try ArcTest.init(testing.allocator);
    defer f.deinit();
    const list = try f.local(f.list_i64);
    const elem = try f.local(.i64);
    const appended = try f.local(f.list_i64);
    const result = try f.local(.i64);

    // elem = 5; list = []; appended = checked_op(list, elem); result = 1
    const ret = try f.ret(result);
    const result_assign = try f.assignI64(result, 1, ret);
    const append = try f.assignLowLevel(appended, &.{ list, elem }, LIR.LowLevel.RcEffect.runtimeUniqueness(1), result_assign);
    const list_assign = try f.assignList(list, &.{}, append);
    const body = try f.assignI64(elem, 5, list_assign);
    _ = try f.addProc(&.{}, body, .i64);

    try f.run();
    // The list is born unique and its single unit moves into the op, so the
    // op's runtime count check on argument 0 is redundant.
    try testing.expectEqual(@as(u64, 1), f.uniqueArgsFor(appended));
}

test "uniqueness: slice-producing checked op result keeps later check" {
    var f = try ArcTest.init(testing.allocator);
    defer f.deinit();
    const list = try f.local(f.list_i64);
    const sliced = try f.local(f.list_i64);
    const trimmed = try f.local(f.list_i64);
    const result = try f.local(.i64);

    // elem = []; sliced = sublist_like(elem); trimmed = checked_op(sliced)
    const ret = try f.ret(result);
    const result_assign = try f.assignI64(result, 1, ret);
    const trim = try f.assignLowLevel(trimmed, &.{sliced}, LIR.LowLevel.RcEffect.runtimeUniqueness(1), result_assign);
    const slice = try f.assignLowLevel(sliced, &.{list}, LIR.LowLevel.RcEffect.runtimeUniquenessMaybeSharedResult(1), trim);
    const body = try f.assignList(list, &.{}, slice);
    _ = try f.addProc(&.{}, body, .i64);

    try f.run();
    // Slice-producing list builtins can return a seamless slice of a shared
    // allocation, so their result must not seed born-unique analysis.
    try testing.expectEqual(@as(u64, 0), f.uniqueArgsFor(trimmed));
}

test "uniqueness: list held by a struct keeps its runtime check" {
    var f = try ArcTest.init(testing.allocator);
    defer f.deinit();
    const list = try f.local(f.list_i64);
    const pair = try f.local(f.pair_list);
    const elem = try f.local(.i64);
    const appended = try f.local(f.list_i64);
    const result = try f.local(.i64);

    // elem = 5; list = []; pair = {list, list}; appended = checked_op(list, elem)
    const ret = try f.ret(result);
    const result_assign = try f.assignI64(result, 1, ret);
    const append = try f.assignLowLevel(appended, &.{ list, elem }, LIR.LowLevel.RcEffect.runtimeUniqueness(1), result_assign);
    const pair_assign = try f.assignStruct(pair, &.{ list, list }, append);
    const list_assign = try f.assignList(list, &.{}, pair_assign);
    const body = try f.assignI64(elem, 5, list_assign);
    _ = try f.addProc(&.{}, body, .i64);

    try f.run();
    // The struct holds the list's allocation, so its count is above 1 at
    // the op and the runtime check stays.
    try testing.expectEqual(@as(u64, 0), f.uniqueArgsFor(appended));
}

test "uniqueness: list consumed by two checked ops keeps both checks" {
    var f = try ArcTest.init(testing.allocator);
    defer f.deinit();
    const list = try f.local(f.list_i64);
    const elem = try f.local(.i64);
    const first = try f.local(f.list_i64);
    const second = try f.local(f.list_i64);
    const result = try f.local(.i64);

    // elem = 5; list = []; first = checked_op(list, elem); second = checked_op(list, elem)
    const ret = try f.ret(result);
    const result_assign = try f.assignI64(result, 1, ret);
    const second_append = try f.assignLowLevel(second, &.{ list, elem }, LIR.LowLevel.RcEffect.runtimeUniqueness(1), result_assign);
    const first_append = try f.assignLowLevel(first, &.{ list, elem }, LIR.LowLevel.RcEffect.runtimeUniqueness(1), second_append);
    const list_assign = try f.assignList(list, &.{}, first_append);
    const body = try f.assignI64(elem, 5, list_assign);
    _ = try f.addProc(&.{}, body, .i64);

    try f.run();
    // Two consuming uses: the first holds the list live past the op (a
    // retain pays for the second use), the second consumes a value whose
    // count was held above 1.
    try testing.expectEqual(@as(u64, 0), f.uniqueArgsFor(first));
    try testing.expectEqual(@as(u64, 0), f.uniqueArgsFor(second));
}

test "uniqueness: parameter consumed by a checked op keeps its check" {
    var f = try ArcTest.init(testing.allocator);
    defer f.deinit();
    const param = try f.local(f.list_i64);
    const elem = try f.local(.i64);
    const appended = try f.local(f.list_i64);

    // appended = checked_op(param, elem); ret appended—the caller may
    // still hold the argument, so the parameter is never born unique.
    const ret = try f.ret(appended);
    const append = try f.assignLowLevel(appended, &.{ param, elem }, LIR.LowLevel.RcEffect.runtimeUniqueness(1), ret);
    const body = try f.assignI64(elem, 5, append);
    _ = try f.addProc(&.{param}, body, f.list_i64);

    try f.run();
    try testing.expectEqual(@as(u64, 0), f.uniqueArgsFor(appended));
}

test "uniqueness: append result consumed by a checked op elides the check" {
    var f = try ArcTest.init(testing.allocator);
    defer f.deinit();
    const list = try f.local(f.list_i64);
    const elem = try f.local(.i64);
    const first = try f.local(f.list_i64);
    const second = try f.local(f.list_i64);
    const result = try f.local(.i64);

    // elem = 5; list = []; first = append_unsafe(list, elem);
    // second = checked_op(first, elem)—the append's RcEffect marks its
    // result unique, so the chained op's check is redundant.
    const ret = try f.ret(result);
    const result_assign = try f.assignI64(result, 1, ret);
    const second_append = try f.assignLowLevel(second, &.{ first, elem }, LIR.LowLevel.RcEffect.runtimeUniqueness(1), result_assign);
    const first_append = try f.assignLowLevel(first, &.{ list, elem }, LIR.LowLevel.RcEffect.consumesArgsReturningConsumedArgsRetainingArgs(1, 2), second_append);
    const list_assign = try f.assignList(list, &.{}, first_append);
    const body = try f.assignI64(elem, 5, list_assign);
    _ = try f.addProc(&.{}, body, .i64);

    try f.run();
    try testing.expectEqual(@as(u64, 1), f.uniqueArgsFor(second));
}

test "uniqueness: call result of a fresh-list callee elides the check" {
    var f = try ArcTest.init(testing.allocator);
    defer f.deinit();

    // Callee builds and returns a fresh list, so its return solves unique.
    const fresh = try f.local(f.list_i64);
    const callee_ret = try f.ret(fresh);
    const callee_body = try f.assignList(fresh, &.{}, callee_ret);
    const callee = try f.addProc(&.{}, callee_body, f.list_i64);

    // Caller runs a checked op on the call result.
    const list = try f.local(f.list_i64);
    const elem = try f.local(.i64);
    const appended = try f.local(f.list_i64);
    const result = try f.local(.i64);
    const ret = try f.ret(result);
    const result_assign = try f.assignI64(result, 1, ret);
    const append = try f.assignLowLevel(appended, &.{ list, elem }, LIR.LowLevel.RcEffect.runtimeUniqueness(1), result_assign);
    const elem_assign = try f.assignI64(elem, 5, append);
    const call = try f.store.addCFStmt(.{ .assign_call = .{
        .target = list,
        .proc = callee,
        .args = try f.span(&.{}),
        .next = elem_assign,
    } });
    _ = try f.addProc(&.{}, call, .i64);

    try f.run();
    // The callee's return is born unique and the result's single unit moves
    // into the op, so the runtime check on argument 0 is redundant.
    try testing.expectEqual(@as(u64, 1), f.uniqueArgsFor(appended));
}

test "uniqueness: multiply-defined unique call result keeps the check" {
    var f = try ArcTest.init(testing.allocator);
    defer f.deinit();

    const fresh = try f.local(f.list_i64);
    const callee_ret = try f.ret(fresh);
    const callee_body = try f.assignList(fresh, &.{}, callee_ret);
    const callee = try f.addProc(&.{}, callee_body, f.list_i64);

    // Either branch calls the same unique-returning callee, but the shared
    // result local has two definitions. The flow-insensitive proof must not
    // pick one runtime birth and claim that local is statically unique.
    const cond = try f.local(.bool);
    const list = try f.local(f.list_i64);
    const elem = try f.local(.i64);
    const appended = try f.local(f.list_i64);
    const result = try f.local(.i64);
    const ret = try f.ret(result);
    const result_assign = try f.assignI64(result, 1, ret);
    const append = try f.assignLowLevel(appended, &.{ list, elem }, LIR.LowLevel.RcEffect.runtimeUniqueness(1), result_assign);
    const continuation = try f.assignI64(elem, 5, append);
    const then_call = try f.store.addCFStmt(.{ .assign_call = .{
        .target = list,
        .proc = callee,
        .args = try f.span(&.{}),
        .next = continuation,
    } });
    const else_call = try f.store.addCFStmt(.{ .assign_call = .{
        .target = list,
        .proc = callee,
        .args = try f.span(&.{}),
        .next = continuation,
    } });
    const body = try f.switchStmt(cond, then_call, else_call, continuation);
    _ = try f.addProc(&.{cond}, body, .i64);

    try f.run();
    try testing.expectEqual(@as(u64, 0), f.uniqueArgsFor(appended));
}

test "uniqueness: pass-through callee result keeps the caller's check" {
    var f = try ArcTest.init(testing.allocator);
    defer f.deinit();

    // Callee returns its own parameter: the caller may hold other handles
    // to the value, so the return never solves unique.
    const param = try f.local(f.list_i64);
    const callee_ret = try f.ret(param);
    const callee = try f.addProc(&.{param}, callee_ret, f.list_i64);

    const list = try f.local(f.list_i64);
    const got = try f.local(f.list_i64);
    const elem = try f.local(.i64);
    const appended = try f.local(f.list_i64);
    const result = try f.local(.i64);
    const ret = try f.ret(result);
    const result_assign = try f.assignI64(result, 1, ret);
    const append = try f.assignLowLevel(appended, &.{ got, elem }, LIR.LowLevel.RcEffect.runtimeUniqueness(1), result_assign);
    const elem_assign = try f.assignI64(elem, 5, append);
    const call = try f.store.addCFStmt(.{ .assign_call = .{
        .target = got,
        .proc = callee,
        .args = try f.span(&.{list}),
        .next = elem_assign,
    } });
    const body = try f.assignList(list, &.{}, call);
    _ = try f.addProc(&.{}, body, .i64);

    try f.run();
    try testing.expectEqual(@as(u64, 0), f.uniqueArgsFor(appended));
}

test "uniqueness: root callee result keeps the caller's check" {
    var f = try ArcTest.init(testing.allocator);
    defer f.deinit();

    // Same shape as the fresh-list callee above, but the callee is a root:
    // pinned signatures never claim a unique return.
    const fresh = try f.local(f.list_i64);
    const callee_ret = try f.ret(fresh);
    const callee_body = try f.assignList(fresh, &.{}, callee_ret);
    const callee = try f.addProc(&.{}, callee_body, f.list_i64);

    const list = try f.local(f.list_i64);
    const elem = try f.local(.i64);
    const appended = try f.local(f.list_i64);
    const result = try f.local(.i64);
    const ret = try f.ret(result);
    const result_assign = try f.assignI64(result, 1, ret);
    const append = try f.assignLowLevel(appended, &.{ list, elem }, LIR.LowLevel.RcEffect.runtimeUniqueness(1), result_assign);
    const elem_assign = try f.assignI64(elem, 5, append);
    const call = try f.store.addCFStmt(.{ .assign_call = .{
        .target = list,
        .proc = callee,
        .args = try f.span(&.{}),
        .next = elem_assign,
    } });
    _ = try f.addProc(&.{}, call, .i64);

    try insert(&f.store, &f.layouts, .{ .roots = &.{callee} });
    try testing.expectEqual(@as(u64, 0), f.uniqueArgsFor(appended));
}

test "uniqueness: specialized variant elides the check on a unique dying argument" {
    var f = try ArcTest.init(testing.allocator);
    defer f.deinit();

    // Callee runs a checked op on its parameter; the parameter solves owned
    // and the base body keeps the runtime check.
    const param = try f.local(f.list_i64);
    const elem = try f.local(.i64);
    const appended = try f.local(f.list_i64);
    const callee_ret = try f.ret(appended);
    const append = try f.assignLowLevel(appended, &.{ param, elem }, LIR.LowLevel.RcEffect.runtimeUniqueness(1), callee_ret);
    const callee_body = try f.assignI64(elem, 5, append);
    const callee = try f.addProc(&.{param}, callee_body, f.list_i64);

    // Caller passes a dying fresh list.
    const list = try f.local(f.list_i64);
    const got = try f.local(f.list_i64);
    const caller_ret = try f.ret(got);
    const call = try f.store.addCFStmt(.{ .assign_call = .{
        .target = got,
        .proc = callee,
        .args = try f.span(&.{list}),
        .next = caller_ret,
    } });
    const caller_body = try f.assignList(list, &.{}, call);
    _ = try f.addProc(&.{}, caller_body, f.list_i64);

    const base_proc_count = f.store.procSpecCount();
    try insert(&f.store, &f.layouts, .{ .specialize = true });

    // One unique-seeded variant exists; its op runs check-free while the
    // base proc keeps the runtime check.
    try testing.expectEqual(base_proc_count + 1, f.store.procSpecCount());
    try testing.expectEqual(@as(u64, 0), try f.uniqueArgsInProc(callee, appended));
    const variant: LIR.LirProcSpecId = @enumFromInt(@as(u32, @intCast(base_proc_count)));
    try testing.expectEqual(@as(u64, 1), try f.uniqueArgsInProc(variant, appended));
}

test "uniqueness: specialized body clones do not poison local births" {
    var f = try ArcTest.init(testing.allocator);
    defer f.deinit();

    // Both the base and specialized callee bodies bind `first`. The first
    // checked op always returns a unique outer list, so the second op can be
    // check-free in each body independently even though the cloned bodies
    // deliberately share their source LocalIds.
    const param = try f.local(f.list_i64);
    const first = try f.local(f.list_i64);
    const second = try f.local(f.list_i64);
    const callee_ret = try f.ret(second);
    const second_op = try f.assignLowLevel(second, &.{first}, LIR.LowLevel.RcEffect.runtimeUniqueness(1), callee_ret);
    const first_op = try f.assignLowLevel(first, &.{param}, LIR.LowLevel.RcEffect.runtimeUniqueness(1), second_op);
    const callee = try f.addProc(&.{param}, first_op, f.list_i64);

    // A dying fresh argument requests the unique-parameter specialization.
    const list = try f.local(f.list_i64);
    const got = try f.local(f.list_i64);
    const caller_ret = try f.ret(got);
    const call = try f.store.addCFStmt(.{ .assign_call = .{
        .target = got,
        .proc = callee,
        .args = try f.span(&.{list}),
        .next = caller_ret,
    } });
    const caller_body = try f.assignList(list, &.{}, call);
    _ = try f.addProc(&.{}, caller_body, f.list_i64);

    const base_proc_count = f.store.procSpecCount();
    try insert(&f.store, &f.layouts, .{ .specialize = true });

    const variant: LIR.LirProcSpecId = @enumFromInt(@as(u32, @intCast(base_proc_count)));
    try testing.expectEqual(@as(u64, 0), try f.uniqueArgsInProc(callee, first));
    try testing.expectEqual(@as(u64, 1), try f.uniqueArgsInProc(callee, second));
    try testing.expectEqual(@as(u64, 1), try f.uniqueArgsInProc(variant, first));
    try testing.expectEqual(@as(u64, 1), try f.uniqueArgsInProc(variant, second));
}

test "uniqueness: without specialization the dying unique argument keeps the callee's check" {
    var f = try ArcTest.init(testing.allocator);
    defer f.deinit();

    const param = try f.local(f.list_i64);
    const elem = try f.local(.i64);
    const appended = try f.local(f.list_i64);
    const callee_ret = try f.ret(appended);
    const append = try f.assignLowLevel(appended, &.{ param, elem }, LIR.LowLevel.RcEffect.runtimeUniqueness(1), callee_ret);
    const callee_body = try f.assignI64(elem, 5, append);
    const callee = try f.addProc(&.{param}, callee_body, f.list_i64);

    const list = try f.local(f.list_i64);
    const got = try f.local(f.list_i64);
    const caller_ret = try f.ret(got);
    const call = try f.store.addCFStmt(.{ .assign_call = .{
        .target = got,
        .proc = callee,
        .args = try f.span(&.{list}),
        .next = caller_ret,
    } });
    const caller_body = try f.assignList(list, &.{}, call);
    _ = try f.addProc(&.{}, caller_body, f.list_i64);

    const base_proc_count = f.store.procSpecCount();
    try f.run();

    // Single-variant emission never sees unique parameters: no variant is
    // cloned and the callee keeps its runtime check.
    try testing.expectEqual(base_proc_count, f.store.procSpecCount());
    try testing.expectEqual(@as(u64, 0), f.uniqueArgsFor(appended));
}

test "uniqueness: pure alias of a fresh list elides the check" {
    var f = try ArcTest.init(testing.allocator);
    defer f.deinit();
    const list = try f.local(f.list_i64);
    const alias = try f.local(f.list_i64);
    const elem = try f.local(.i64);
    const appended = try f.local(f.list_i64);
    const result = try f.local(.i64);

    // elem = 5; list = []; alias = list; appended = checked_op(alias, elem)
    const ret = try f.ret(result);
    const result_assign = try f.assignI64(result, 1, ret);
    const append = try f.assignLowLevel(appended, &.{ alias, elem }, LIR.LowLevel.RcEffect.runtimeUniqueness(1), result_assign);
    const alias_assign = try f.assignRefLocal(alias, list, append);
    const list_assign = try f.assignList(list, &.{}, alias_assign);
    const body = try f.assignI64(elem, 5, list_assign);
    _ = try f.addProc(&.{}, body, .i64);

    try f.run();
    // The alias is the fresh list's single consuming use, so the list's
    // unit moves through the chain into the op and the runtime check on
    // argument 0 is redundant.
    try testing.expectEqual(@as(u64, 1), f.uniqueArgsFor(appended));
}

test "uniqueness: alias whose source is read elsewhere keeps the check" {
    var f = try ArcTest.init(testing.allocator);
    defer f.deinit();
    const list = try f.local(f.list_i64);
    const alias = try f.local(f.list_i64);
    const elem = try f.local(.i64);
    const appended = try f.local(f.list_i64);
    const result = try f.local(.i64);

    // elem = 5; list = []; alias = list; appended = checked_op(alias, elem);
    // expect(list)—the original is read besides the alias, so the alias
    // must keep its own unit and the count exceeds 1 at the op.
    const ret = try f.ret(result);
    const result_assign = try f.assignI64(result, 1, ret);
    const use_list = try f.expectStmt(list, result_assign);
    const append = try f.assignLowLevel(appended, &.{ alias, elem }, LIR.LowLevel.RcEffect.runtimeUniqueness(1), use_list);
    const alias_assign = try f.assignRefLocal(alias, list, append);
    const list_assign = try f.assignList(list, &.{}, alias_assign);
    const body = try f.assignI64(elem, 5, list_assign);
    _ = try f.addProc(&.{}, body, .i64);

    try f.run();
    try testing.expectEqual(@as(u64, 0), f.uniqueArgsFor(appended));
}

test "uniqueness: alias chain of two inherits the fresh birth" {
    var f = try ArcTest.init(testing.allocator);
    defer f.deinit();
    const list = try f.local(f.list_i64);
    const first_alias = try f.local(f.list_i64);
    const second_alias = try f.local(f.list_i64);
    const elem = try f.local(.i64);
    const appended = try f.local(f.list_i64);
    const result = try f.local(.i64);

    // elem = 5; list = []; first = list; second = first;
    // appended = checked_op(second, elem)
    const ret = try f.ret(result);
    const result_assign = try f.assignI64(result, 1, ret);
    const append = try f.assignLowLevel(appended, &.{ second_alias, elem }, LIR.LowLevel.RcEffect.runtimeUniqueness(1), result_assign);
    const second_assign = try f.assignRefLocal(second_alias, first_alias, append);
    const first_assign = try f.assignRefLocal(first_alias, list, second_assign);
    const list_assign = try f.assignList(list, &.{}, first_assign);
    const body = try f.assignI64(elem, 5, list_assign);
    _ = try f.addProc(&.{}, body, .i64);

    try f.run();
    try testing.expectEqual(@as(u64, 1), f.uniqueArgsFor(appended));
}

test "uniqueness: list reinterpret alias inherits the fresh birth" {
    var f = try ArcTest.init(testing.allocator);
    defer f.deinit();
    const list = try f.local(f.list_i64);
    const cast = try f.local(f.list_i64);
    const elem = try f.local(.i64);
    const appended = try f.local(f.list_i64);
    const result = try f.local(.i64);

    // elem = 5; list = []; cast = reinterpret(list);
    // appended = checked_op(cast, elem)
    const ret = try f.ret(result);
    const result_assign = try f.assignI64(result, 1, ret);
    const append = try f.assignLowLevel(appended, &.{ cast, elem }, LIR.LowLevel.RcEffect.runtimeUniqueness(1), result_assign);
    const cast_assign = try f.assignRefReinterpret(cast, list, append);
    const list_assign = try f.assignList(list, &.{}, cast_assign);
    const body = try f.assignI64(elem, 5, list_assign);
    _ = try f.addProc(&.{}, body, .i64);

    try f.run();
    try testing.expectEqual(@as(u64, 1), f.uniqueArgsFor(appended));
}

test "uniqueness: callee returning a fresh list through an alias solves a unique return" {
    var f = try ArcTest.init(testing.allocator);
    defer f.deinit();

    // Callee builds a fresh list and returns it through a pure alias, so
    // its return solves unique.
    const fresh = try f.local(f.list_i64);
    const out = try f.local(f.list_i64);
    const callee_ret = try f.ret(out);
    const out_assign = try f.assignRefLocal(out, fresh, callee_ret);
    const callee_body = try f.assignList(fresh, &.{}, out_assign);
    const callee = try f.addProc(&.{}, callee_body, f.list_i64);

    // Caller runs a checked op on the call result.
    const list = try f.local(f.list_i64);
    const elem = try f.local(.i64);
    const appended = try f.local(f.list_i64);
    const result = try f.local(.i64);
    const ret = try f.ret(result);
    const result_assign = try f.assignI64(result, 1, ret);
    const append = try f.assignLowLevel(appended, &.{ list, elem }, LIR.LowLevel.RcEffect.runtimeUniqueness(1), result_assign);
    const elem_assign = try f.assignI64(elem, 5, append);
    const call = try f.store.addCFStmt(.{ .assign_call = .{
        .target = list,
        .proc = callee,
        .args = try f.span(&.{}),
        .next = elem_assign,
    } });
    _ = try f.addProc(&.{}, call, .i64);

    try f.run();
    try testing.expectEqual(@as(u64, 1), f.uniqueArgsFor(appended));
}

test "RC mutable iterator accumulator replace cleans old state" {
    var f = try ArcTest.init(testing.allocator);
    defer f.deinit();
    const source = try f.local(f.list_i64);
    const acc = try f.local(f.list_i64);
    const next_acc = try f.local(f.list_i64);
    const join_id = f.freshJoinPointId();

    const ret = try f.ret(acc);
    const replace_acc = try f.setLocal(acc, next_acc, .replace_existing, ret);
    const body = try f.assignList(next_acc, &.{}, replace_acc);

    const initial_jump = try f.store.addCFStmt(.{ .jump = .{ .target = join_id } });
    const initialize_acc = try f.setLocal(acc, source, .initialize_join_param, initial_jump);
    const remainder = try f.assignList(source, &.{}, initialize_acc);
    const join = try f.store.addCFStmt(.{ .join = .{
        .id = join_id,
        .params = try f.span(&.{acc}),
        .body = body,
        .remainder = remainder,
    } });

    _ = try f.addProc(&.{}, join, f.list_i64);
    try f.run();
    try testing.expect(f.countRc(acc, .decref) >= 1);
}

test "dev lowering: list rest pattern emits two list decrefs" {
    var f = try ArcTest.init(testing.allocator);
    defer f.deinit();
    const scrutinee = try f.local(f.list_str);
    const rest = try f.local(f.list_str);
    const result = try f.local(.i64);
    const ret = try f.ret(result);
    const rest_assign = try f.assignList(rest, &.{}, ret);
    const body = try f.assignList(scrutinee, &.{}, rest_assign);
    _ = try f.addProc(&.{}, body, .i64);
    try f.run();
    try f.expectRc(scrutinee, 0, 1, 0);
    try f.expectRc(rest, 0, 1, 0);
}

test "dev lowering: mutable loop append decrefs mutable result binding once" {
    var f = try ArcTest.init(testing.allocator);
    defer f.deinit();
    const acc = try f.local(f.list_i64);
    const appended = try f.local(f.list_i64);
    const elem = try f.local(.i64);
    const ret = try f.ret(appended);
    const append = try f.assignLowLevel(appended, &.{ acc, elem }, LIR.LowLevel.RcEffect.consumesArgsReturningConsumedArgsRetainingArgs(1, 0), ret);
    const body = try f.assignList(acc, &.{}, append);
    _ = try f.addProc(&.{}, body, f.list_i64);
    try f.run();
    try f.expectRc(acc, 0, 0, 0);
    // The appended result moves out on return.
    try f.expectRc(appended, 0, 0, 0);
}

test "dev lowering: mutable list reassignment releases only the replaced value" {
    var scenario = try setupMutation(true);
    defer scenario.fixture.deinit();
    // The replaced value is released at the write; the new value moves out
    // on return.
    try testing.expectEqual(@as(usize, 1), scenario.fixture.countRc(scenario.target, .decref));
}

fn expectDecrefBeforeStmt(f: *const ArcTest, start: LIR.CFStmtId, local: LIR.LocalId, comptime stop_tag: std.meta.Tag(LIR.CFStmt)) error{ DecrefNotBeforeStop, NonLinearPath, CyclicPath }!void {
    var cursor = start;
    var remaining: usize = f.store.cfStmtCount() + 1;
    while (remaining > 0) : (remaining -= 1) {
        const stmt = f.store.getCFStmt(cursor);
        if (stmt == stop_tag) return error.DecrefNotBeforeStop;
        switch (stmt) {
            .decref => |rc| {
                if (rc.value == local) return;
                cursor = rc.next;
            },
            .decref_if_initialized => |rc| {
                if (rc.value == local) return;
                cursor = rc.next;
            },
            .incref => |rc| cursor = rc.next,
            .free => |rc| cursor = rc.next,
            .assign_ref => |a| cursor = a.next,
            .assign_literal => |a| cursor = a.next,
            .init_uninitialized => |a| cursor = a.next,
            .assign_call => |a| cursor = a.next,
            .assign_call_erased => |a| cursor = a.next,
            .assign_packed_erased_fn => |a| cursor = a.next,
            .assign_boxy_desc_ref => |a| cursor = a.next,
            .assign_boxy_dict_ref => |a| cursor = a.next,
            .assign_boxy_box => |a| cursor = a.next,
            .assign_boxy_reuse_box => |a| cursor = a.next,
            .assign_boxy_unbox => |a| cursor = a.next,
            .assign_boxy_adapt => |a| cursor = a.next,
            .assign_boxy_inspect => |a| cursor = a.next,
            .assign_boxy_eq => |a| cursor = a.next,
            .assign_boxy_tag => |a| cursor = a.next,
            .assign_boxy_tag_payload => |a| cursor = a.next,
            .assign_call_dict => |a| cursor = a.next,
            .assign_low_level => |a| cursor = a.next,
            .assign_list => |a| cursor = a.next,
            .assign_struct => |a| cursor = a.next,
            .assign_tag => |a| cursor = a.next,
            .set_local => |a| cursor = a.next,
            .debug => |a| cursor = a.next,
            .expect => |a| cursor = a.next,
            .store_struct,
            .store_tag,
            .expect_err,
            .runtime_error,
            .comptime_exhaustiveness_failed,
            .comptime_branch_taken,
            .switch_stmt,
            .switch_initialized_payload,
            .str_match,
            .str_match_set,
            .boxy_tag_match,
            .loop_continue,
            .loop_break,
            .join,
            .jump,
            .ret,
            .crash,
            => return error.NonLinearPath,
        }
    }
    return error.CyclicPath;
}

test "RC borrow: read-only payload read emits no RC statements for the borrow" {
    var f = try ArcTest.init(testing.allocator);
    defer f.deinit();
    const pair = try f.local(f.pair_str);
    const field = try f.local(.str);
    const a = try f.local(.str);
    const b = try f.local(.str);
    const result = try f.local(.i64);
    const ret = try f.ret(result);
    const result_assign = try f.assignI64(result, 1, ret);
    const use_field = try f.expectStmt(field, result_assign);
    const field_read = try f.assignRefField(field, pair, 0, use_field);
    const pair_assign = try f.assignStruct(pair, &.{ a, b }, field_read);
    const assign_b = try f.assignStr(b, "b", pair_assign);
    const body = try f.assignStr(a, "a", assign_b);
    _ = try f.addProc(&.{}, body, .i64);
    try f.run();
    // The field borrow needs no RC statements; the pair is released after
    // the borrow's last use; a and b move into the pair.
    try f.expectRc(field, 0, 0, 0);
    try f.expectRc(pair, 0, 1, 0);
    try f.expectRc(a, 0, 0, 0);
    try f.expectRc(b, 0, 0, 0);
}

test "RC borrow: payload read consumed by a call stays owned" {
    var f = try ArcTest.init(testing.allocator);
    defer f.deinit();
    const pair = try f.local(f.pair_str);
    const field = try f.local(.str);
    const a = try f.local(.str);
    const b = try f.local(.str);
    const call_result = try f.local(.i64);
    const ret = try f.ret(call_result);
    const call = try f.assignCall(call_result, &.{field}, ret);
    const field_read = try f.assignRefField(field, pair, 0, call);
    const pair_assign = try f.assignStruct(pair, &.{ a, b }, field_read);
    const assign_b = try f.assignStr(b, "b", pair_assign);
    const body = try f.assignStr(a, "a", assign_b);
    _ = try f.addProc(&.{}, body, .i64);
    try f.run();
    // The consumed read is a field take of the dying pair: it moves the
    // pair's stored unit into the call with no retain, and the pair's death
    // releases only the untaken field through a fresh temporary.
    try f.expectRc(field, 0, 0, 0);
    try f.expectRc(pair, 0, 0, 0);
    try testing.expectEqual(@as(usize, 1), f.countAllRc());
}

test "RC borrow: alias of an owned local emits no RC statements" {
    var f = try ArcTest.init(testing.allocator);
    defer f.deinit();
    const original = try f.local(.str);
    const alias = try f.local(.str);
    const ret = try f.ret(original);
    const use_alias = try f.expectStmt(alias, ret);
    const alias_stmt = try f.assignRefLocal(alias, original, use_alias);
    const body = try f.assignStr(original, "shared", alias_stmt);
    _ = try f.addProc(&.{}, body, .str);
    try f.run();
    try f.expectRc(alias, 0, 0, 0);
    // The original moves out on return.
    try f.expectRc(original, 0, 0, 0);
}

test "RC move on return leaves no RC statements" {
    var f = try ArcTest.init(testing.allocator);
    defer f.deinit();
    const value = try f.local(.str);
    const ret = try f.ret(value);
    const body = try f.assignStr(value, "moved", ret);
    _ = try f.addProc(&.{}, body, .str);
    try f.run();
    try testing.expectEqual(@as(usize, 0), f.countAllRc());
}

test "RC move into aggregate at final use" {
    var f = try ArcTest.init(testing.allocator);
    defer f.deinit();
    const a = try f.local(.str);
    const b = try f.local(.str);
    const pair = try f.local(f.pair_str);
    const ret = try f.ret(pair);
    const pair_assign = try f.assignStruct(pair, &.{ a, b }, ret);
    const assign_b = try f.assignStr(b, "b", pair_assign);
    const body = try f.assignStr(a, "a", assign_b);
    _ = try f.addProc(&.{}, body, f.pair_str);
    try f.run();
    // Both operands move into the pair; the pair moves out on return.
    try testing.expectEqual(@as(usize, 0), f.countAllRc());
}

test "RC move into wide aggregate transfers operands past mask width" {
    var f = try ArcTest.init(testing.allocator);
    defer f.deinit();
    const row_count = 70;
    var rows: [row_count]LIR.LocalId = undefined;
    for (&rows) |*row| {
        row.* = try f.local(f.list_i64);
    }
    const matrix_layout = try f.layouts.insertList(f.list_i64);
    const matrix = try f.local(matrix_layout);

    const ret = try f.ret(matrix);
    var next = try f.assignList(matrix, &rows, ret);
    var i = rows.len;
    while (i > 0) {
        i -= 1;
        next = try f.assignList(rows[i], &.{}, next);
    }

    _ = try f.addProc(&.{}, next, matrix_layout);
    try f.run();
    // Every row list moves into the outer list, including operands beyond the
    // 64-bit low-level argument mask width. The outer list then moves out.
    try f.expectRc(rows[0], 0, 0, 0);
    try f.expectRc(rows[63], 0, 0, 0);
    try f.expectRc(rows[64], 0, 0, 0);
    try f.expectRc(rows[69], 0, 0, 0);
    try f.expectRc(matrix, 0, 0, 0);
}

test "RC early drop places the release right after the last use" {
    var f = try ArcTest.init(testing.allocator);
    defer f.deinit();
    const value = try f.local(.str);
    const result = try f.local(.i64);
    const ret = try f.ret(result);
    const late_call = try f.assignCall(result, &.{}, ret);
    const use_value = try f.expectStmt(value, late_call);
    const body = try f.assignStr(value, "early", use_value);
    _ = try f.addProc(&.{}, body, .i64);
    try f.run();
    try f.expectRc(value, 0, 1, 0);
    // The release lands before the unrelated call, not at the return.
    try expectDecrefBeforeStmt(&f, f.procBody(), value, .assign_call);
}

test "RC borrow keeps the lender alive past a consuming use of the lender" {
    var f = try ArcTest.init(testing.allocator);
    defer f.deinit();
    const pair = try f.local(f.pair_str);
    const field = try f.local(.str);
    const a = try f.local(.str);
    const b = try f.local(.str);
    const call_result = try f.local(.i64);
    const result = try f.local(.i64);
    const ret = try f.ret(result);
    const result_assign = try f.assignI64(result, 7, ret);
    const use_field = try f.expectStmt(field, result_assign);
    const consuming_call = try f.assignCall(call_result, &.{pair}, use_field);
    const field_read = try f.assignRefField(field, pair, 0, consuming_call);
    const pair_assign = try f.assignStruct(pair, &.{ a, b }, field_read);
    const assign_b = try f.assignStr(b, "b", pair_assign);
    const body = try f.assignStr(a, "a", assign_b);
    _ = try f.addProc(&.{}, body, .i64);
    try f.run();
    // The pair is consumed by the call while its borrow is still live, so the
    // call argument pays a retain and the pair's own unit is released after
    // the borrow's last use.
    try f.expectRc(pair, 1, 1, 0);
    try f.expectRc(field, 0, 0, 0);
}

test "RC borrow: reassigned lender forces the read to stay owned" {
    var f = try ArcTest.init(testing.allocator);
    defer f.deinit();
    const pair = try f.local(f.pair_str);
    const field = try f.local(.str);
    const a = try f.local(.str);
    const b = try f.local(.str);
    const result = try f.local(.i64);
    const ret = try f.ret(result);
    const result_assign = try f.assignI64(result, 1, ret);
    const use_field = try f.expectStmt(field, result_assign);
    // Rebind the pair between the read and the use of the read.
    const pair_rebind = try f.assignStruct(pair, &.{ a, b }, use_field);
    const field_read = try f.assignRefField(field, pair, 0, pair_rebind);
    const pair_assign = try f.assignStruct(pair, &.{ a, b }, field_read);
    const incref_b2 = try f.assignStr(b, "b", pair_assign);
    const body = try f.assignStr(a, "a", incref_b2);
    _ = try f.addProc(&.{}, body, .i64);
    try f.run();
    // The lender is bound twice, so the read cannot borrow.
    try testing.expect(f.countRc(field, .incref) >= 1);
}

test "RC borrow: list element read via low-level borrows the list" {
    var f = try ArcTest.init(testing.allocator);
    defer f.deinit();
    const list = try f.local(f.list_str);
    const index = try f.local(.i64);
    const elem = try f.local(.str);
    const result = try f.local(.i64);
    const ret = try f.ret(result);
    const result_assign = try f.assignI64(result, 1, ret);
    const use_elem = try f.expectStmt(elem, result_assign);
    const get = try f.store.addCFStmt(.{ .assign_low_level = .{
        .target = elem,
        .op = .list_get_unsafe,
        .rc_effect = LIR.LowLevel.RcEffect.retainsResultBorrowingArgs(1),
        .args = try f.span(&.{ list, index }),
        .next = use_elem,
    } });
    const index_assign = try f.assignI64(index, 0, get);
    const body = try f.assignList(list, &.{}, index_assign);
    _ = try f.addProc(&.{}, body, .i64);
    try f.run();
    try f.expectRc(elem, 0, 0, 0);
    try f.expectRc(list, 0, 1, 0);
}

test "RC borrow: read-only sublist materializes a borrowed view" {
    var f = try ArcTest.init(testing.allocator);
    defer f.deinit();
    const list = try f.local(f.list_i64);
    const range = try f.local(.i64);
    const slice = try f.local(f.list_i64);
    const result = try f.local(.i64);
    const ret = try f.ret(result);
    const result_assign = try f.assignI64(result, 1, ret);
    const use_slice = try f.expectStmt(slice, result_assign);
    const sublist = try f.store.addCFStmt(.{ .assign_low_level = .{
        .target = slice,
        .op = .list_sublist,
        .rc_effect = LIR.LowLevel.list_sublist.rcEffect(),
        .args = try f.span(&.{ list, range }),
        .next = use_slice,
    } });
    const range_assign = try f.assignI64(range, 0, sublist);
    const body = try f.assignList(list, &.{}, range_assign);
    _ = try f.addProc(&.{}, body, .i64);

    try f.run();

    const emitted = f.reachableLowLevelAssign(slice);
    try testing.expectEqual(LIR.LowLevel.list_sublist_borrowed, emitted.op);
    try testing.expect(std.meta.eql(emitted.op.rcEffect(), emitted.rc_effect));
    try f.expectRc(slice, 0, 0, 0);
    try f.expectRc(list, 0, 1, 0);
}

test "RC Box.unbox normalizes consuming ownership to explicit RC statements" {
    var f = try ArcTest.init(testing.allocator);
    defer f.deinit();
    const boxed = try f.local(f.box_str);
    const source = try f.local(.str);
    const payload = try f.local(.str);
    const call_result = try f.local(.i64);
    const result = try f.local(.i64);
    const ret = try f.ret(result);
    const assign_result = try f.assignI64(result, 1, ret);
    const consume_payload = try f.assignCall(call_result, &.{payload}, assign_result);
    const unbox = try f.store.addCFStmt(.{ .assign_low_level = .{
        .target = payload,
        .op = .box_unbox,
        .rc_effect = LIR.LowLevel.box_unbox.rcEffect(),
        .args = try f.span(&.{boxed}),
        .next = consume_payload,
    } });
    const make_box = try f.store.addCFStmt(.{ .assign_low_level = .{
        .target = boxed,
        .op = .box_box,
        .rc_effect = LIR.LowLevel.box_box.rcEffect(),
        .args = try f.span(&.{source}),
        .next = unbox,
    } });
    const body = try f.assignStr(source, "owned", make_box);
    _ = try f.addProc(&.{}, body, .i64);
    try f.run();

    const emitted = f.reachableLowLevelAssign(payload);
    try testing.expectEqual(LIR.LowLevel.box_unbox_borrowed, emitted.op);
    try testing.expect(std.meta.eql(emitted.op.rcEffect(), emitted.rc_effect));
    try f.expectRc(boxed, 0, 1, 0);
    try f.expectRc(payload, 1, 0, 0);
}

test "RC Box.unbox keeps the payload borrowed while the box survives" {
    var f = try ArcTest.init(testing.allocator);
    defer f.deinit();
    const boxed = try f.local(f.box_str);
    const payload = try f.local(.str);
    const result = try f.local(.i64);
    const ret = try f.ret(result);
    const assign_result = try f.assignI64(result, 1, ret);
    const use_box = try f.expectStmt(boxed, assign_result);
    const use_payload = try f.expectStmt(payload, use_box);
    const unbox = try f.store.addCFStmt(.{ .assign_low_level = .{
        .target = payload,
        .op = .box_unbox,
        .rc_effect = LIR.LowLevel.box_unbox.rcEffect(),
        .args = try f.span(&.{boxed}),
        .next = use_payload,
    } });
    _ = try f.addProc(&.{boxed}, unbox, .i64);
    try f.run();

    const emitted = f.reachableLowLevelAssign(payload);
    try testing.expectEqual(LIR.LowLevel.box_unbox_borrowed, emitted.op);
    try testing.expect(std.meta.eql(emitted.op.rcEffect(), emitted.rc_effect));
    try f.expectRc(payload, 1, 1, 0);
    try f.expectRc(boxed, 0, 1, 0);
}

test "RC borrow: owned sublist from borrowed parameter retains one input unit" {
    var f = try ArcTest.init(testing.allocator);
    defer f.deinit();
    const list = try f.local(f.list_i64);
    const range = try f.local(.i64);
    const slice = try f.local(f.list_i64);
    const call_result = try f.local(.i64);
    const result = try f.local(.i64);
    const ret = try f.ret(result);
    const result_assign = try f.assignI64(result, 1, ret);
    const consume_slice = try f.assignCall(call_result, &.{slice}, result_assign);
    const sublist = try f.store.addCFStmt(.{ .assign_low_level = .{
        .target = slice,
        .op = .list_sublist,
        .rc_effect = LIR.LowLevel.list_sublist.rcEffect(),
        .args = try f.span(&.{ list, range }),
        .next = consume_slice,
    } });
    const body = try f.assignI64(range, 0, sublist);
    _ = try f.addProc(&.{list}, body, .i64);

    try f.run();

    const emitted = f.reachableLowLevelAssign(slice);
    try testing.expectEqual(LIR.LowLevel.list_sublist, emitted.op);
    try testing.expect(std.meta.eql(emitted.op.rcEffect(), emitted.rc_effect));
    try f.expectRc(list, 1, 0, 0);
    try f.expectRc(slice, 0, 0, 0);
}

test "RC borrow: owned sublist transfers an owned input unit" {
    var f = try ArcTest.init(testing.allocator);
    defer f.deinit();
    const list = try f.local(f.list_i64);
    const range = try f.local(.i64);
    const slice = try f.local(f.list_i64);
    const call_result = try f.local(.i64);
    const result = try f.local(.i64);
    const ret = try f.ret(result);
    const result_assign = try f.assignI64(result, 1, ret);
    const consume_slice = try f.assignCall(call_result, &.{slice}, result_assign);
    const sublist = try f.store.addCFStmt(.{ .assign_low_level = .{
        .target = slice,
        .op = .list_sublist,
        .rc_effect = LIR.LowLevel.list_sublist.rcEffect(),
        .args = try f.span(&.{ list, range }),
        .next = consume_slice,
    } });
    const range_assign = try f.assignI64(range, 0, sublist);
    const body = try f.assignList(list, &.{}, range_assign);
    _ = try f.addProc(&.{}, body, .i64);

    try f.run();

    const emitted = f.reachableLowLevelAssign(slice);
    try testing.expectEqual(LIR.LowLevel.list_sublist, emitted.op);
    try testing.expect(std.meta.eql(emitted.op.rcEffect(), emitted.rc_effect));
    try f.expectRc(list, 0, 0, 0);
    try f.expectRc(slice, 0, 0, 0);
}

test "RC borrow: string match view capture used read-only does not retain source" {
    var f = try ArcTest.init(testing.allocator);
    defer f.deinit();
    const source = try f.local(.str);
    const capture = try f.local(.str);
    const result = try f.local(.i64);
    const ret = try f.ret(result);
    const result_assign = try f.assignI64(result, 1, ret);
    const use_capture = try f.expectStmt(capture, result_assign);
    const miss = try f.crash("miss");
    const str_match = try f.strMatchTailCapture(source, capture, "pre", use_capture, miss);
    const body = try f.assignStr(source, "prefix", str_match);
    _ = try f.addProc(&.{}, body, .i64);

    try f.run();

    try testing.expectEqual(@as(usize, 0), f.countRc(source, .incref));
    try f.expectRc(capture, 0, 0, 0);
}

test "RC borrow: string match view capture consumed by call retains source" {
    var f = try ArcTest.init(testing.allocator);
    defer f.deinit();
    const source = try f.local(.str);
    const capture = try f.local(.str);
    const result = try f.local(.i64);
    const ret = try f.ret(result);
    const call = try f.assignCall(result, &.{capture}, ret);
    const miss = try f.crash("miss");
    const str_match = try f.strMatchTailCapture(source, capture, "pre", call, miss);
    const body = try f.assignStr(source, "prefix", str_match);
    _ = try f.addProc(&.{}, body, .i64);

    try f.run();

    try testing.expectEqual(@as(usize, 1), f.countRc(source, .incref));
    try f.expectRc(capture, 0, 0, 0);
}

test "RC borrow: string match view capture returned retains the view" {
    var f = try ArcTest.init(testing.allocator);
    defer f.deinit();
    const source = try f.local(.str);
    const capture = try f.local(.str);
    const match_ret = try f.ret(capture);
    const miss_ret = try f.ret(source);
    const str_match = try f.strMatchTailCapture(source, capture, "pre", match_ret, miss_ret);
    const body = try f.assignStr(source, "prefix", str_match);
    _ = try f.addProc(&.{}, body, .str);

    try f.run();

    try testing.expectEqual(@as(usize, 0), f.countRc(source, .incref));
    try f.expectRc(capture, 1, 0, 0);
}

test "RC outcome restitution preserves List Str on failure and seeds the success mutation unique" {
    var f = try ArcTest.init(testing.allocator);
    defer f.deinit();

    const try_list = try f.layouts.putTagUnion(&[_]layout_mod.Idx{
        try f.layouts.ensureZstLayout(),
        f.list_str,
    });

    // A generic checked mutation shape: success consumes the list into the
    // result, while failure returns no payload and leaves the exact entry
    // list untouched. List(Str) ensures the nested element RC plan is part of
    // both ARC emission and certification, not merely a flat-list control.
    const param = try f.local(f.list_str);
    const choose_success = try f.local(.i64);
    const changed = try f.local(f.list_str);
    const callee_result = try f.local(try_list);
    const success_ret = try f.ret(callee_result);
    const success_tag = try f.assignTag(callee_result, 1, changed, success_ret);
    const mutate = try f.store.addCFStmt(.{ .assign_low_level = .{
        .target = changed,
        .op = .list_reverse,
        .rc_effect = LIR.LowLevel.RcEffect.runtimeUniqueness(1),
        .args = try f.span(&.{param}),
        .next = success_tag,
    } });
    const failure_ret = try f.ret(callee_result);
    const failure_tag = try f.assignTag(callee_result, 0, null, failure_ret);
    const callee_body = try f.switchStmt(choose_success, mutate, failure_tag, null);
    const callee = try f.addProc(&.{ param, choose_success }, callee_body, try_list);

    const item = try f.local(.str);
    const input = try f.local(f.list_str);
    const caller_choose = try f.local(.i64);
    const call_result = try f.local(try_list);
    const discriminant = try f.local(.u8);
    const success_value = try f.local(.i64);
    const failure_value = try f.local(.i64);
    const caller_success_ret = try f.ret(success_value);
    const success_body = try f.assignI64(success_value, 1, caller_success_ret);
    const caller_failure_ret = try f.ret(failure_value);
    const failure_done = try f.assignI64(failure_value, 0, caller_failure_ret);
    const failure_body = try f.expectStmt(input, failure_done);
    const refine = try f.switchStmt(discriminant, success_body, failure_body, null);
    const read_discriminant = try f.assignDiscriminant(discriminant, call_result, refine);
    const call = try f.store.addCFStmt(.{ .assign_call = .{
        .target = call_result,
        .proc = callee,
        .args = try f.span(&.{ input, caller_choose }),
        .next = read_discriminant,
    } });
    const choose = try f.assignI64(caller_choose, 1, call);
    const make_input = try f.assignList(input, &.{item}, choose);
    const caller_body = try f.assignStr(item, "nested", make_input);
    const caller = try f.addProc(&.{}, caller_body, .i64);

    const base_proc_count = f.store.procSpecCount();
    try insert(&f.store, &f.layouts, .{ .specialize = true });

    try testing.expectEqual(base_proc_count + 1, f.store.procSpecCount());
    const outcome_variant: LIR.LirProcSpecId = @enumFromInt(@as(u32, @intCast(base_proc_count)));
    try testing.expectEqual(@as(u64, 0), try f.uniqueArgsInProc(callee, changed));
    try testing.expectEqual(@as(u64, 1), try f.uniqueArgsInProc(outcome_variant, changed));

    var cursor = f.store.getProcSpec(caller).body orelse return error.MissingCallerBody;
    var remaining = f.store.cfStmtCount() + 1;
    while (remaining > 0) : (remaining -= 1) {
        const stmt = f.store.getCFStmt(cursor);
        if (stmt == .assign_call and stmt.assign_call.target == call_result) {
            try testing.expectEqual(outcome_variant, stmt.assign_call.proc);
            break;
        }
        cursor = switch (stmt) {
            inline .assign_ref, .assign_literal, .assign_list, .assign_struct, .assign_tag, .set_local, .debug, .expect, .incref, .decref, .decref_if_initialized, .free, .comptime_branch_taken => |linear| linear.next,
            .init_uninitialized,
            .assign_call,
            .assign_call_erased,
            .assign_packed_erased_fn,
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
            .assign_low_level,
            .store_struct,
            .store_tag,
            .expect_err,
            .runtime_error,
            .comptime_exhaustiveness_failed,
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
            => return error.MissingOutcomeCall,
        };
    }
    try testing.expect(remaining > 0);
    try testing.expectEqual(@as(usize, 0), f.countRc(input, .incref));
}

test "RC outcome restitution releases every returned argument before a nested join" {
    var f = try ArcTest.init(testing.allocator);
    defer f.deinit();

    const outcome_layout = try f.layouts.putTagUnion(&[_]layout_mod.Idx{
        try f.layouts.ensureZstLayout(),
        f.list_i64,
    });

    // Failure returns both input ownership units. Success consumes both, but
    // only the first changed list appears in the source result.
    const first_param = try f.local(f.list_i64);
    const choose_success = try f.local(.i64);
    const second_param = try f.local(f.list_i64);
    const changed_first = try f.local(f.list_i64);
    const changed_second = try f.local(f.list_i64);
    const callee_result = try f.local(outcome_layout);
    const success_ret = try f.ret(callee_result);
    const success_tag = try f.assignTag(callee_result, 1, changed_first, success_ret);
    const consume_first = try f.store.addCFStmt(.{ .assign_low_level = .{
        .target = changed_first,
        .op = .list_reverse,
        .rc_effect = LIR.LowLevel.RcEffect.runtimeUniqueness(1),
        .args = try f.span(&.{first_param}),
        .next = success_tag,
    } });
    const consume_second = try f.store.addCFStmt(.{ .assign_low_level = .{
        .target = changed_second,
        .op = .list_reverse,
        .rc_effect = LIR.LowLevel.RcEffect.runtimeUniqueness(1),
        .args = try f.span(&.{second_param}),
        .next = consume_first,
    } });
    const failure_ret = try f.ret(callee_result);
    const failure_tag = try f.assignTag(callee_result, 0, null, failure_ret);
    const callee_body = try f.switchStmt(choose_success, consume_second, failure_tag, null);
    const callee = try f.addProc(&.{ first_param, choose_success, second_param }, callee_body, outcome_layout);

    const first_input = try f.local(f.list_i64);
    const second_input = try f.local(f.list_i64);
    const caller_choose = try f.local(.i64);
    const call_result = try f.local(outcome_layout);
    const discriminant = try f.local(.u8);
    const output = try f.local(.i64);
    const join_id = f.freshJoinPointId();
    const joined_ret = try f.ret(output);
    const success_jump = try f.store.addCFStmt(.{ .jump = .{ .target = join_id } });
    const success_body = try f.assignI64(output, 1, success_jump);
    const failure_jump = try f.store.addCFStmt(.{ .jump = .{ .target = join_id } });
    const failure_value = try f.assignI64(output, 0, failure_jump);
    const failure_body = try f.expectStmt(first_input, failure_value);
    const refine = try f.switchStmt(discriminant, success_body, failure_body, null);
    const read_discriminant = try f.assignDiscriminant(discriminant, call_result, refine);
    const call = try f.store.addCFStmt(.{ .assign_call = .{
        .target = call_result,
        .proc = callee,
        .args = try f.span(&.{ first_input, caller_choose, second_input }),
        .next = read_discriminant,
    } });
    const caller_body = try f.store.addCFStmt(.{ .join = .{
        .id = join_id,
        .params = LIR.LocalSpan.empty(),
        .body = joined_ret,
        .remainder = call,
    } });
    const caller = try f.addProc(&.{ first_input, caller_choose, second_input }, caller_body, .i64);

    // A second caller uses the second input on the non-restituting success
    // edge. The complete span is therefore inadmissible even though failure
    // could restore the first input; this call must keep the base convention.
    const rejected_first = try f.local(f.list_i64);
    const rejected_second = try f.local(f.list_i64);
    const rejected_choose = try f.local(.i64);
    const rejected_result = try f.local(outcome_layout);
    const rejected_discriminant = try f.local(.u8);
    const rejected_output = try f.local(.i64);
    const rejected_join_id = f.freshJoinPointId();
    const rejected_ret = try f.ret(rejected_output);
    const rejected_success_jump = try f.store.addCFStmt(.{ .jump = .{ .target = rejected_join_id } });
    const rejected_success_value = try f.assignI64(rejected_output, 1, rejected_success_jump);
    const rejected_success = try f.expectStmt(rejected_second, rejected_success_value);
    const rejected_failure_jump = try f.store.addCFStmt(.{ .jump = .{ .target = rejected_join_id } });
    const rejected_failure_value = try f.assignI64(rejected_output, 0, rejected_failure_jump);
    const rejected_failure = try f.expectStmt(rejected_first, rejected_failure_value);
    const rejected_refine = try f.switchStmt(rejected_discriminant, rejected_success, rejected_failure, null);
    const rejected_read = try f.assignDiscriminant(rejected_discriminant, rejected_result, rejected_refine);
    const rejected_call = try f.store.addCFStmt(.{ .assign_call = .{
        .target = rejected_result,
        .proc = callee,
        .args = try f.span(&.{ rejected_first, rejected_choose, rejected_second }),
        .next = rejected_read,
    } });
    const rejected_body = try f.store.addCFStmt(.{ .join = .{
        .id = rejected_join_id,
        .params = LIR.LocalSpan.empty(),
        .body = rejected_ret,
        .remainder = rejected_call,
    } });
    const rejected_caller = try f.addProc(&.{ rejected_first, rejected_choose, rejected_second }, rejected_body, .i64);

    const base_proc_count = f.store.procSpecCount();
    try insert(&f.store, &f.layouts, .{ .specialize = true });

    try testing.expectEqual(base_proc_count + 1, f.store.procSpecCount());
    try testing.expectEqual(@as(usize, 0), f.countRc(first_input, .incref));
    try testing.expectEqual(@as(usize, 0), f.countRc(second_input, .incref));
    try testing.expectEqual(@as(usize, 1), f.countRc(second_input, .decref));
    try testing.expectEqual(@as(usize, 1), f.countRc(rejected_first, .incref));
    try testing.expectEqual(@as(usize, 1), f.countRc(rejected_second, .incref));

    var cursor = f.store.getProcSpec(caller).body orelse return error.MissingCallerBody;
    var remaining = f.store.cfStmtCount() + 1;
    while (remaining > 0) : (remaining -= 1) {
        const stmt = f.store.getCFStmt(cursor);
        if (stmt == .assign_call and stmt.assign_call.target == call_result) {
            try testing.expect(stmt.assign_call.proc != callee);
            break;
        }
        cursor = switch (stmt) {
            .join => |join| join.remainder,
            inline .assign_ref, .assign_literal, .assign_list, .assign_struct, .assign_tag, .set_local, .debug, .expect, .incref, .decref, .decref_if_initialized, .free, .comptime_branch_taken => |linear| linear.next,
            .init_uninitialized,
            .assign_call,
            .assign_call_erased,
            .assign_packed_erased_fn,
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
            .assign_low_level,
            .store_struct,
            .store_tag,
            .expect_err,
            .runtime_error,
            .comptime_exhaustiveness_failed,
            .switch_stmt,
            .switch_initialized_payload,
            .str_match,
            .str_match_set,
            .loop_continue,
            .loop_break,
            .jump,
            .ret,
            .crash,
            => return error.MissingOutcomeCall,
        };
    }
    try testing.expect(remaining > 0);

    cursor = f.store.getProcSpec(rejected_caller).body orelse return error.MissingCallerBody;
    remaining = f.store.cfStmtCount() + 1;
    while (remaining > 0) : (remaining -= 1) {
        const stmt = f.store.getCFStmt(cursor);
        if (stmt == .assign_call and stmt.assign_call.target == rejected_result) {
            try testing.expectEqual(callee, stmt.assign_call.proc);
            break;
        }
        cursor = switch (stmt) {
            .join => |join| join.remainder,
            inline .assign_ref, .assign_literal, .assign_list, .assign_struct, .assign_tag, .set_local, .debug, .expect, .incref, .decref, .decref_if_initialized, .free, .comptime_branch_taken => |linear| linear.next,
            .init_uninitialized,
            .assign_call,
            .assign_call_erased,
            .assign_packed_erased_fn,
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
            .assign_low_level,
            .store_struct,
            .store_tag,
            .expect_err,
            .runtime_error,
            .comptime_exhaustiveness_failed,
            .switch_stmt,
            .switch_initialized_payload,
            .str_match,
            .str_match_set,
            .loop_continue,
            .loop_break,
            .jump,
            .ret,
            .crash,
            => return error.MissingOutcomeCall,
        };
    }
    try testing.expect(remaining > 0);
}

test "ARC outcome capability rejects a stale discriminant after return-local rebind" {
    var f = try ArcTest.init(testing.allocator);
    defer f.deinit();

    const outcome_layout = try f.layouts.putTagUnion(&[_]layout_mod.Idx{
        try f.layouts.ensureZstLayout(),
        f.list_i64,
    });
    const param = try f.local(f.list_i64);
    const choose = try f.local(.i64);
    const consumed = try f.local(f.list_i64);
    const result = try f.local(outcome_layout);
    const replacement = try f.local(outcome_layout);
    const ret = try f.ret(result);

    // The failure path first stamps result as discriminant 0, then replaces
    // that return local with a different same-layout value. The earlier tag
    // assignment is not a witness for the value actually returned.
    const rebind = try f.assignRefLocal(result, replacement, ret);
    const stale_tag = try f.assignTag(result, 0, null, rebind);
    const success_tag = try f.assignTag(result, 1, consumed, ret);
    const consume = try f.store.addCFStmt(.{ .assign_low_level = .{
        .target = consumed,
        .op = .list_reverse,
        .rc_effect = LIR.LowLevel.RcEffect.runtimeUniqueness(1),
        .args = try f.span(&.{param}),
        .next = success_tag,
    } });
    const body = try f.switchStmt(choose, consume, stale_tag, null);
    const proc = try f.addProc(&.{ param, choose, replacement }, body, outcome_layout);

    const rc_local = [_]bool{ true, false, true, true, true };
    var solution = try arc_solve.solve(
        testing.allocator,
        &f.store,
        &f.layouts,
        &rc_local,
        &.{},
        &.{},
        true,
    );
    defer solution.deinit();
    try testing.expect(solution.availableOutcomeSpanOf(proc).isEmpty());
}

test "ARC outcome capability rejects stale witnesses across set-local and join rebinding" {
    var f = try ArcTest.init(testing.allocator);
    defer f.deinit();
    const outcome_layout = try f.layouts.putTagUnion(&[_]layout_mod.Idx{
        try f.layouts.ensureZstLayout(),
        f.list_i64,
    });

    const set_param = try f.local(f.list_i64);
    const set_choose = try f.local(.i64);
    const set_consumed = try f.local(f.list_i64);
    const set_result = try f.local(outcome_layout);
    const set_replacement = try f.local(outcome_layout);
    const set_ret = try f.ret(set_result);
    const set_rebind = try f.setLocal(set_result, set_replacement, .replace_existing, set_ret);
    const set_stale_tag = try f.assignTag(set_result, 0, null, set_rebind);
    const set_success_tag = try f.assignTag(set_result, 1, set_consumed, set_ret);
    const set_consume = try f.store.addCFStmt(.{ .assign_low_level = .{
        .target = set_consumed,
        .op = .list_reverse,
        .rc_effect = LIR.LowLevel.RcEffect.runtimeUniqueness(1),
        .args = try f.span(&.{set_param}),
        .next = set_success_tag,
    } });
    const set_body = try f.switchStmt(set_choose, set_consume, set_stale_tag, null);
    const set_proc = try f.addProc(&.{ set_param, set_choose, set_replacement }, set_body, outcome_layout);

    const join_param = try f.local(f.list_i64);
    const join_choose = try f.local(.i64);
    const join_consumed = try f.local(f.list_i64);
    const join_result = try f.local(outcome_layout);
    const join_replacement = try f.local(outcome_layout);
    const join_id = f.freshJoinPointId();
    const join_ret = try f.ret(join_result);
    const join_rebind = try f.assignRefLocal(join_result, join_replacement, join_ret);
    const shared_jump = try f.store.addCFStmt(.{ .jump = .{ .target = join_id } });
    const join_failure_tag = try f.assignTag(join_result, 0, null, shared_jump);
    const join_success_tag = try f.assignTag(join_result, 1, join_consumed, shared_jump);
    const join_consume = try f.store.addCFStmt(.{ .assign_low_level = .{
        .target = join_consumed,
        .op = .list_reverse,
        .rc_effect = LIR.LowLevel.RcEffect.runtimeUniqueness(1),
        .args = try f.span(&.{join_param}),
        .next = join_success_tag,
    } });
    const join_switch = try f.switchStmt(join_choose, join_consume, join_failure_tag, null);
    const join_body = try f.store.addCFStmt(.{ .join = .{
        .id = join_id,
        .params = LIR.LocalSpan.empty(),
        .body = join_rebind,
        .remainder = join_switch,
    } });
    const join_proc = try f.addProc(&.{ join_param, join_choose, join_replacement }, join_body, outcome_layout);

    const rc_local = try testing.allocator.alloc(bool, f.store.localCount());
    defer testing.allocator.free(rc_local);
    for (rc_local, 0..) |*contains_rc, index| {
        const local: LIR.LocalId = @enumFromInt(@as(u32, @intCast(index)));
        contains_rc.* = f.layouts.layoutContainsRefcounted(f.layouts.getLayout(f.store.getLocal(local).layout_idx));
    }
    var solution = try arc_solve.solve(
        testing.allocator,
        &f.store,
        &f.layouts,
        rc_local,
        &.{},
        &.{},
        true,
    );
    defer solution.deinit();
    try testing.expect(solution.availableOutcomeSpanOf(set_proc).isEmpty());
    try testing.expect(solution.availableOutcomeSpanOf(join_proc).isEmpty());
}

const OutcomeMutationCallee = struct {
    proc: LIR.LirProcSpecId,
    changed: LIR.LocalId,
};

fn addOutcomeMutationCallee(
    f: *ArcTest,
    list_layout: layout_mod.Idx,
    outcome_layout: layout_mod.Idx,
) Allocator.Error!OutcomeMutationCallee {
    const param = try f.local(list_layout);
    const choose = try f.local(.i64);
    const changed = try f.local(list_layout);
    const result = try f.local(outcome_layout);
    const success_ret = try f.ret(result);
    const success_tag = try f.assignTag(result, 1, changed, success_ret);
    const mutate = try f.store.addCFStmt(.{ .assign_low_level = .{
        .target = changed,
        .op = .list_reverse,
        .rc_effect = LIR.LowLevel.RcEffect.runtimeUniqueness(1),
        .args = try f.span(&.{param}),
        .next = success_tag,
    } });
    const failure_ret = try f.ret(result);
    const failure_tag = try f.assignTag(result, 0, null, failure_ret);
    const body = try f.switchStmt(choose, mutate, failure_tag, null);
    return .{ .proc = try f.addProc(&.{ param, choose }, body, outcome_layout), .changed = changed };
}

test "RC outcome restitution refuses transfer when success still uses the old input" {
    var f = try ArcTest.init(testing.allocator);
    defer f.deinit();
    const outcome_layout = try f.layouts.putTagUnion(&[_]layout_mod.Idx{
        try f.layouts.ensureZstLayout(),
        f.list_i64,
    });
    const callee = try addOutcomeMutationCallee(&f, f.list_i64, outcome_layout);

    const input = try f.local(f.list_i64);
    const choose = try f.local(.i64);
    const call_result = try f.local(outcome_layout);
    const discriminant = try f.local(.u8);
    const success_value = try f.local(.i64);
    const failure_value = try f.local(.i64);
    const success_ret = try f.ret(success_value);
    const success_done = try f.assignI64(success_value, 1, success_ret);
    const success_body = try f.expectStmt(input, success_done);
    const failure_ret = try f.ret(failure_value);
    const failure_done = try f.assignI64(failure_value, 0, failure_ret);
    const failure_body = try f.expectStmt(input, failure_done);
    const refine = try f.switchStmt(discriminant, success_body, failure_body, null);
    const read_discriminant = try f.assignDiscriminant(discriminant, call_result, refine);
    const call = try f.store.addCFStmt(.{ .assign_call = .{
        .target = call_result,
        .proc = callee.proc,
        .args = try f.span(&.{ input, choose }),
        .next = read_discriminant,
    } });
    const choose_assign = try f.assignI64(choose, 1, call);
    const caller_body = try f.assignList(input, &.{}, choose_assign);
    _ = try f.addProc(&.{}, caller_body, .i64);

    const base_proc_count = f.store.procSpecCount();
    try insert(&f.store, &f.layouts, .{ .specialize = true });

    // The success outcome consumed the argument, so using the old input on
    // that arm cannot be justified by failure-only restitution. The caller
    // retains a second unit and the mutation keeps its runtime COW check.
    try testing.expectEqual(base_proc_count, f.store.procSpecCount());
    try testing.expectEqual(@as(usize, 1), f.countRc(input, .incref));
    try testing.expectEqual(@as(u64, 0), try f.uniqueArgsInProc(callee.proc, callee.changed));
}

test "RC unrefined outcome uses the unconditional base ownership schedule" {
    var f = try ArcTest.init(testing.allocator);
    defer f.deinit();
    const outcome_layout = try f.layouts.putTagUnion(&[_]layout_mod.Idx{
        try f.layouts.ensureZstLayout(),
        f.list_i64,
    });
    const callee = try addOutcomeMutationCallee(&f, f.list_i64, outcome_layout);

    const input = try f.local(f.list_i64);
    const choose = try f.local(.i64);
    const call_result = try f.local(outcome_layout);
    const done = try f.local(.i64);
    const ret = try f.ret(done);
    const done_assign = try f.assignI64(done, 1, ret);
    const use_input = try f.expectStmt(input, done_assign);
    const use_unmatched_result = try f.expectStmt(call_result, use_input);
    const call = try f.store.addCFStmt(.{ .assign_call = .{
        .target = call_result,
        .proc = callee.proc,
        .args = try f.span(&.{ input, choose }),
        .next = use_unmatched_result,
    } });
    const choose_assign = try f.assignI64(choose, 1, call);
    const body = try f.assignList(input, &.{}, choose_assign);
    _ = try f.addProc(&.{}, body, .i64);

    const base_proc_count = f.store.procSpecCount();
    try insert(&f.store, &f.layouts, .{ .specialize = true });

    try testing.expectEqual(base_proc_count, f.store.procSpecCount());
    try testing.expectEqual(@as(usize, 1), f.countRc(input, .incref));
    try testing.expectEqual(@as(u64, 0), try f.uniqueArgsInProc(callee.proc, callee.changed));
}

test "RC outcome restitution rejects duplicate and alias-equivalent argument places" {
    var f = try ArcTest.init(testing.allocator);
    defer f.deinit();
    const outcome_layout = try f.layouts.putTagUnion(&[_]layout_mod.Idx{
        try f.layouts.ensureZstLayout(),
        f.list_i64,
    });

    const first_param = try f.local(f.list_i64);
    const second_param = try f.local(f.list_i64);
    const choose = try f.local(.i64);
    const first_changed = try f.local(f.list_i64);
    const second_changed = try f.local(f.list_i64);
    const result = try f.local(outcome_layout);
    const success_ret = try f.ret(result);
    const success_tag = try f.assignTag(result, 1, first_changed, success_ret);
    const mutate_second = try f.store.addCFStmt(.{ .assign_low_level = .{
        .target = second_changed,
        .op = .list_reverse,
        .rc_effect = LIR.LowLevel.RcEffect.runtimeUniqueness(1),
        .args = try f.span(&.{second_param}),
        .next = success_tag,
    } });
    const mutate_first = try f.store.addCFStmt(.{ .assign_low_level = .{
        .target = first_changed,
        .op = .list_reverse,
        .rc_effect = LIR.LowLevel.RcEffect.runtimeUniqueness(1),
        .args = try f.span(&.{first_param}),
        .next = mutate_second,
    } });
    const failure_ret = try f.ret(result);
    const failure_tag = try f.assignTag(result, 0, null, failure_ret);
    const callee_body = try f.switchStmt(choose, mutate_first, failure_tag, null);
    const callee = try f.addProc(&.{ first_param, second_param, choose }, callee_body, outcome_layout);

    const same_input = try f.local(f.list_i64);
    const same_choose = try f.local(.i64);
    const same_result = try f.local(outcome_layout);
    const same_disc = try f.local(.u8);
    const same_done = try f.local(.i64);
    const same_success_ret = try f.ret(same_done);
    const same_success = try f.assignI64(same_done, 1, same_success_ret);
    const same_failure_ret = try f.ret(same_done);
    const same_failure_done = try f.assignI64(same_done, 0, same_failure_ret);
    const same_failure = try f.expectStmt(same_input, same_failure_done);
    const same_refine = try f.switchStmt(same_disc, same_success, same_failure, null);
    const same_read = try f.assignDiscriminant(same_disc, same_result, same_refine);
    const same_call = try f.store.addCFStmt(.{ .assign_call = .{
        .target = same_result,
        .proc = callee,
        .args = try f.span(&.{ same_input, same_input, same_choose }),
        .next = same_read,
    } });
    const same_choose_assign = try f.assignI64(same_choose, 1, same_call);
    const same_body = try f.assignList(same_input, &.{}, same_choose_assign);
    _ = try f.addProc(&.{}, same_body, .i64);

    const alias_input = try f.local(f.list_i64);
    const alias = try f.local(f.list_i64);
    const alias_choose = try f.local(.i64);
    const alias_result = try f.local(outcome_layout);
    const alias_disc = try f.local(.u8);
    const alias_done = try f.local(.i64);
    const alias_success_ret = try f.ret(alias_done);
    const alias_success = try f.assignI64(alias_done, 1, alias_success_ret);
    const alias_failure_ret = try f.ret(alias_done);
    const alias_failure_done = try f.assignI64(alias_done, 0, alias_failure_ret);
    const alias_failure = try f.expectStmt(alias_input, alias_failure_done);
    const alias_refine = try f.switchStmt(alias_disc, alias_success, alias_failure, null);
    const alias_read = try f.assignDiscriminant(alias_disc, alias_result, alias_refine);
    const alias_call = try f.store.addCFStmt(.{ .assign_call = .{
        .target = alias_result,
        .proc = callee,
        .args = try f.span(&.{ alias_input, alias, alias_choose }),
        .next = alias_read,
    } });
    const alias_choose_assign = try f.assignI64(alias_choose, 1, alias_call);
    const bind_alias = try f.assignRefLocal(alias, alias_input, alias_choose_assign);
    const alias_body = try f.assignList(alias_input, &.{}, bind_alias);
    _ = try f.addProc(&.{}, alias_body, .i64);

    const base_proc_count = f.store.procSpecCount();
    try insert(&f.store, &f.layouts, .{ .specialize = true });

    try testing.expectEqual(base_proc_count, f.store.procSpecCount());
    try testing.expectEqual(@as(u64, 0), try f.uniqueArgsInProc(callee, first_changed));
    try testing.expectEqual(@as(u64, 0), try f.uniqueArgsInProc(callee, second_changed));
    try testing.expect(f.countRc(same_input, .incref) >= 2);
    try testing.expect(f.countRc(alias_input, .incref) >= 1);
}

test "RC outcome restitution intersects every outcome represented by the default arm" {
    var f = try ArcTest.init(testing.allocator);
    defer f.deinit();
    const outcome_layout = try f.layouts.putTagUnion(&[_]layout_mod.Idx{
        try f.layouts.ensureZstLayout(),
        f.list_i64,
        try f.layouts.ensureZstLayout(),
    });

    const param = try f.local(f.list_i64);
    const is_success = try f.local(.i64);
    const is_second_failure = try f.local(.i64);
    const changed = try f.local(f.list_i64);
    const result = try f.local(outcome_layout);
    const success_ret = try f.ret(result);
    const success_tag = try f.assignTag(result, 1, changed, success_ret);
    const mutate = try f.store.addCFStmt(.{ .assign_low_level = .{
        .target = changed,
        .op = .list_reverse,
        .rc_effect = LIR.LowLevel.RcEffect.runtimeUniqueness(1),
        .args = try f.span(&.{param}),
        .next = success_tag,
    } });
    const first_failure_ret = try f.ret(result);
    const first_failure = try f.assignTag(result, 0, null, first_failure_ret);
    const second_failure_ret = try f.ret(result);
    const second_failure = try f.assignTag(result, 2, null, second_failure_ret);
    const choose_failure = try f.switchStmt(is_second_failure, second_failure, first_failure, null);
    const callee_body = try f.switchStmt(is_success, mutate, choose_failure, null);
    const callee = try f.addProc(&.{ param, is_success, is_second_failure }, callee_body, outcome_layout);

    const input = try f.local(f.list_i64);
    const caller_success = try f.local(.i64);
    const caller_second_failure = try f.local(.i64);
    const call_result = try f.local(outcome_layout);
    const discriminant = try f.local(.u8);
    const done = try f.local(.i64);
    const explicit_success_ret = try f.ret(done);
    const explicit_success = try f.assignI64(done, 1, explicit_success_ret);
    const default_ret = try f.ret(done);
    const default_done = try f.assignI64(done, 0, default_ret);
    const default_failure = try f.expectStmt(input, default_done);
    const refine = try f.switchStmt(discriminant, explicit_success, default_failure, null);
    const read_discriminant = try f.assignDiscriminant(discriminant, call_result, refine);
    const call = try f.store.addCFStmt(.{ .assign_call = .{
        .target = call_result,
        .proc = callee,
        .args = try f.span(&.{ input, caller_success, caller_second_failure }),
        .next = read_discriminant,
    } });
    const second_choice = try f.assignI64(caller_second_failure, 0, call);
    const first_choice = try f.assignI64(caller_success, 1, second_choice);
    const body = try f.assignList(input, &.{}, first_choice);
    _ = try f.addProc(&.{}, body, .i64);

    const base_proc_count = f.store.procSpecCount();
    try insert(&f.store, &f.layouts, .{ .specialize = true });

    try testing.expectEqual(base_proc_count + 1, f.store.procSpecCount());
    const variant: LIR.LirProcSpecId = @enumFromInt(@as(u32, @intCast(base_proc_count)));
    try testing.expectEqual(@as(u64, 0), try f.uniqueArgsInProc(callee, changed));
    try testing.expectEqual(@as(u64, 1), try f.uniqueArgsInProc(variant, changed));
    try testing.expectEqual(@as(usize, 0), f.countRc(input, .incref));
}

test "RC outcome restitution solves sixteen independent conditional arguments polynomially" {
    if (builtin.mode != .Debug) return;
    var f = try ArcTest.init(testing.allocator);
    defer f.deinit();

    const outcome_layout = try f.layouts.putTagUnion(&[_]layout_mod.Idx{
        try f.layouts.ensureZstLayout(),
        f.list_i64,
    });
    const count = arc_sig.tracked_param_count;
    var params: [count]LIR.LocalId = undefined;
    var conditions: [count]LIR.LocalId = undefined;
    var sinks: [count]LIR.LocalId = undefined;
    for (0..count) |index| {
        params[index] = try f.local(f.list_i64);
        conditions[index] = try f.local(.i64);
        sinks[index] = try f.local(.i64);
    }
    const result = try f.local(outcome_layout);
    const ret = try f.ret(result);
    var next = try f.assignTag(result, 0, null, ret);
    var index: usize = count;
    while (index > 0) {
        index -= 1;
        const consume = try f.assignCall(sinks[index], &.{params[index]}, next);
        next = try f.switchStmt(conditions[index], consume, next, next);
    }
    var args: [count * 2]LIR.LocalId = undefined;
    for (0..count) |arg_index| {
        args[arg_index] = params[arg_index];
        args[count + arg_index] = conditions[arg_index];
    }
    _ = try f.addProc(&args, next, outcome_layout);

    const before = arc_solve.outcome_solver_iterations;
    try f.run();
    const work = arc_solve.outcome_solver_iterations - before;

    // Each parameter has only present/spent rows at each statement. The old
    // full-mask walk materialized all 2^16 subsets at the final continuation.
    try testing.expect(work <= count * f.store.cfStmtCount() * 2);
}

test "RC outcome restitution follows an exact result through a terminal join" {
    var f = try ArcTest.init(testing.allocator);
    defer f.deinit();
    const outcome_layout = try f.layouts.putTagUnion(&[_]layout_mod.Idx{
        try f.layouts.ensureZstLayout(),
        f.list_i64,
    });

    const param = try f.local(f.list_i64);
    const choose = try f.local(.i64);
    const changed = try f.local(f.list_i64);
    const result = try f.local(outcome_layout);
    const join_id = f.freshJoinPointId();
    const terminal_ret = try f.ret(result);
    const jump_success = try f.store.addCFStmt(.{ .jump = .{ .target = join_id } });
    const success_tag = try f.assignTag(result, 1, changed, jump_success);
    const mutate = try f.store.addCFStmt(.{ .assign_low_level = .{
        .target = changed,
        .op = .list_reverse,
        .rc_effect = LIR.LowLevel.RcEffect.runtimeUniqueness(1),
        .args = try f.span(&.{param}),
        .next = success_tag,
    } });
    const jump_failure = try f.store.addCFStmt(.{ .jump = .{ .target = join_id } });
    const failure_tag = try f.assignTag(result, 0, null, jump_failure);
    const choose_outcome = try f.switchStmt(choose, mutate, failure_tag, null);
    const callee_body = try f.store.addCFStmt(.{ .join = .{
        .id = join_id,
        .params = LIR.LocalSpan.empty(),
        .body = terminal_ret,
        .remainder = choose_outcome,
    } });
    const callee = try f.addProc(&.{ param, choose }, callee_body, outcome_layout);

    const input = try f.local(f.list_i64);
    const caller_choose = try f.local(.i64);
    const call_result = try f.local(outcome_layout);
    const discriminant = try f.local(.u8);
    const success_value = try f.local(.i64);
    const failure_value = try f.local(.i64);
    const success_ret = try f.ret(success_value);
    const success_done = try f.assignI64(success_value, 1, success_ret);
    const failure_ret = try f.ret(failure_value);
    const failure_done = try f.assignI64(failure_value, 0, failure_ret);
    const failure_use = try f.expectStmt(input, failure_done);
    const refine = try f.switchStmt(discriminant, success_done, failure_use, null);
    const read_discriminant = try f.assignDiscriminant(discriminant, call_result, refine);
    const call = try f.store.addCFStmt(.{ .assign_call = .{
        .target = call_result,
        .proc = callee,
        .args = try f.span(&.{ input, caller_choose }),
        .next = read_discriminant,
    } });
    const choose_assign = try f.assignI64(caller_choose, 1, call);
    const body = try f.assignList(input, &.{}, choose_assign);
    _ = try f.addProc(&.{}, body, .i64);

    const base_proc_count = f.store.procSpecCount();
    try insert(&f.store, &f.layouts, .{ .specialize = true });

    try testing.expectEqual(base_proc_count + 1, f.store.procSpecCount());
    const variant: LIR.LirProcSpecId = @enumFromInt(@as(u32, @intCast(base_proc_count)));
    try testing.expectEqual(@as(u64, 1), try f.uniqueArgsInProc(variant, changed));
    try testing.expectEqual(@as(usize, 0), f.countRc(input, .incref));
}

test "RC specialization: borrowed final argument does not clone for release-only moves" {
    var f = try ArcTest.init(testing.allocator);
    defer f.deinit();

    // Callee reads its parameter and returns an integer; its parameter
    // solves borrowed.
    const param = try f.local(.str);
    const callee_result = try f.local(.i64);
    const callee_ret = try f.ret(callee_result);
    const callee_result_assign = try f.assignI64(callee_result, 1, callee_ret);
    const callee_body = try f.expectStmt(param, callee_result_assign);
    const callee = try f.addProc(&.{param}, callee_body, .i64);

    // Caller passes an owned value whose lifetime ends at the call.
    const value = try f.local(.str);
    const result = try f.local(.i64);
    const ret = try f.ret(result);
    const call = try f.store.addCFStmt(.{ .assign_call = .{
        .target = result,
        .proc = callee,
        .args = try f.span(&.{value}),
        .next = ret,
    } });
    const body = try f.assignStr(value, "arg", call);
    _ = try f.addProc(&.{}, body, .i64);

    const base_proc_count = f.store.procSpecCount();
    try insert(&f.store, &f.layouts, .{ .specialize = true });

    // Moving this argument into a variant would only relocate the release
    // from caller to callee. Keep the borrowed signature and avoid cloning
    // live code for no runtime RC reduction.
    try testing.expectEqual(base_proc_count, f.store.procSpecCount());
    try f.expectRc(value, 0, 1, 0);
    try f.expectRc(param, 0, 0, 0);
}

test "RC signature: position 16 uses the all-owned tail" {
    var f = try ArcTest.init(testing.allocator);
    defer f.deinit();

    var params: [17]LIR.LocalId = undefined;
    for (&params) |*param| param.* = try f.local(.str);
    const result = try f.local(.i64);
    const ret = try f.ret(result);
    const body = try f.assignI64(result, 1, ret);
    _ = try f.addProc(&params, body, .i64);

    try f.run();

    // Position 15 participates in inference and remains borrowed. Position
    // 16 is outside RcSig's represented prefix and follows all-owned ARC.
    try f.expectRc(params[15], 0, 0, 0);
    try f.expectRc(params[16], 0, 1, 0);
}

test "RC specialization: owned-only field take demands an owned variant" {
    var f = try ArcTest.init(testing.allocator);
    defer f.deinit();

    // Repro for https://github.com/roc-lang/roc/issues/10635: the callee's
    // aggregate parameter solves borrowed, but an owned variant can take its
    // nested list field instead of retaining it before the uniqueness check.
    const param = try f.local(f.pair_list);
    const field = try f.local(f.list_i64);
    const elem = try f.local(.i64);
    const appended = try f.local(f.list_i64);
    const callee_result = try f.local(.i64);
    const callee_ret = try f.ret(callee_result);
    const result_assign = try f.assignI64(callee_result, 1, callee_ret);
    const append = try f.assignLowLevel(appended, &.{ field, elem }, LIR.LowLevel.RcEffect.runtimeUniqueness(1), result_assign);
    const elem_assign = try f.assignI64(elem, 5, append);
    const field_read = try f.assignRefField(field, param, 0, elem_assign);
    const callee = try f.addProc(&.{param}, field_read, .i64);

    const first = try f.local(f.list_i64);
    const second = try f.local(f.list_i64);
    const pair = try f.local(f.pair_list);
    const caller_result = try f.local(.i64);
    const caller_ret = try f.ret(caller_result);
    const call = try f.store.addCFStmt(.{ .assign_call = .{
        .target = caller_result,
        .proc = callee,
        .args = try f.span(&.{pair}),
        .next = caller_ret,
    } });
    const pair_assign = try f.assignStruct(pair, &.{ first, second }, call);
    const second_assign = try f.assignList(second, &.{}, pair_assign);
    const caller_body = try f.assignList(first, &.{}, second_assign);
    _ = try f.addProc(&.{}, caller_body, .i64);

    const base_proc_count = f.store.procSpecCount();
    try insert(&f.store, &f.layouts, .{ .specialize = true });

    // The caller moves the dying pair into an owned variant. The base proc
    // retains the borrowed field once; the variant takes it without another
    // retain, and dismantles only the pair's residual field.
    try testing.expectEqual(base_proc_count + 1, f.store.procSpecCount());
    try f.expectRc(pair, 0, 0, 0);
    try testing.expectEqual(@as(usize, 1), f.countRc(field, .incref));
}

test "RC field take restores the exact aggregate field on checked failure without optional specialization" {
    var f = try ArcTest.init(testing.allocator);
    defer f.deinit();

    const outcome_layout = try f.layouts.putTagUnion(&[_]layout_mod.Idx{
        try f.layouts.ensureZstLayout(),
        f.list_i64,
    });

    // Callee convention: success consumes the list into the result; failure
    // returns the unchanged entry ownership unit by outcome convention.
    const callee_list = try f.local(f.list_i64);
    const callee_choose = try f.local(.i64);
    const callee_changed = try f.local(f.list_i64);
    const callee_result = try f.local(outcome_layout);
    const callee_success_ret = try f.ret(callee_result);
    const callee_success_tag = try f.assignTag(callee_result, 1, callee_changed, callee_success_ret);
    const callee_mutate = try f.store.addCFStmt(.{ .assign_low_level = .{
        .target = callee_changed,
        .op = .list_reverse,
        .rc_effect = LIR.LowLevel.RcEffect.runtimeUniqueness(1),
        .args = try f.span(&.{callee_list}),
        .next = callee_success_tag,
    } });
    const callee_failure_ret = try f.ret(callee_result);
    const callee_failure_tag = try f.assignTag(callee_result, 0, null, callee_failure_ret);
    const callee_body = try f.switchStmt(callee_choose, callee_mutate, callee_failure_tag, null);
    const callee = try f.addProc(&.{ callee_list, callee_choose }, callee_body, outcome_layout);

    // Caller takes pair.field0 into the checked call. Its failure arm rereads
    // that same committed place and consumes it; field1 remains residual on
    // both arms. The first projection has exactly one operand read (the call),
    // so Dismantles can publish an exact call-argument field receipt.
    const first = try f.local(f.list_i64);
    const second = try f.local(f.list_i64);
    const pair = try f.local(f.pair_list);
    const first_projection = try f.local(f.list_i64);
    const caller_choose = try f.local(.i64);
    const call_result = try f.local(outcome_layout);
    const discriminant = try f.local(.u8);
    const failure_projection = try f.local(f.list_i64);
    const failure_changed = try f.local(f.list_i64);
    const answer = try f.local(.i64);
    const caller_ret = try f.ret(answer);
    const success = try f.assignI64(answer, 1, caller_ret);
    const failure_done = try f.assignI64(answer, 0, caller_ret);
    const failure_consume = try f.store.addCFStmt(.{ .assign_low_level = .{
        .target = failure_changed,
        .op = .list_reverse,
        .rc_effect = LIR.LowLevel.RcEffect.runtimeUniqueness(1),
        .args = try f.span(&.{failure_projection}),
        .next = failure_done,
    } });
    const failure_read = try f.assignRefField(failure_projection, pair, 0, failure_consume);
    const refine = try f.switchStmt(discriminant, success, failure_read, null);
    const read_discriminant = try f.assignDiscriminant(discriminant, call_result, refine);
    const call = try f.store.addCFStmt(.{ .assign_call = .{
        .target = call_result,
        .proc = callee,
        .args = try f.span(&.{ first_projection, caller_choose }),
        .next = read_discriminant,
    } });
    const choose = try f.assignI64(caller_choose, 1, call);
    const first_read = try f.assignRefField(first_projection, pair, 0, choose);
    const make_pair = try f.assignStruct(pair, &.{ first, second }, first_read);
    const make_second = try f.assignList(second, &.{}, make_pair);
    const caller_body = try f.assignList(first, &.{}, make_second);
    const caller = try f.addProc(&.{}, caller_body, .i64);

    const base_proc_count = f.store.procSpecCount();
    try insert(&f.store, &f.layouts, .{ .specialize = false });

    // The exact outcome variant is mandatory in dev/no-inline mode. Both
    // field reads are moves, so neither manufactures a list unit.
    try testing.expectEqual(base_proc_count + 1, f.store.procSpecCount());
    try testing.expectEqual(@as(usize, 0), f.countRc(first_projection, .incref));
    try testing.expectEqual(@as(usize, 0), f.countRc(failure_projection, .incref));
    try testing.expectEqual(@as(usize, 0), f.countRc(pair, .decref));

    var cursor = f.store.getProcSpec(caller).body orelse return error.MissingCallerBody;
    var remaining = f.store.cfStmtCount() + 1;
    while (remaining > 0) : (remaining -= 1) {
        const stmt = f.store.getCFStmt(cursor);
        if (stmt == .assign_call and stmt.assign_call.target == call_result) {
            try testing.expect(@intFromEnum(stmt.assign_call.proc) >= base_proc_count);
            break;
        }
        cursor = switch (stmt) {
            inline .assign_ref, .assign_literal, .assign_list, .assign_struct, .assign_tag, .set_local, .debug, .expect, .incref, .decref, .decref_if_initialized, .free, .comptime_branch_taken => |linear| linear.next,
            .init_uninitialized,
            .assign_call,
            .assign_call_erased,
            .assign_packed_erased_fn,
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
            .assign_low_level,
            .store_struct,
            .store_tag,
            .expect_err,
            .runtime_error,
            .comptime_exhaustiveness_failed,
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
            => return error.MissingOutcomeCall,
        };
    }
    try testing.expect(remaining > 0);
}

test "RC field-take demand crosses complete tag payload and wrapper calls" {
    var f = try ArcTest.init(testing.allocator);
    defer f.deinit();

    const tag_pair = try f.layouts.putTagUnion(&[_]layout_mod.Idx{
        try f.layouts.ensureZstLayout(),
        f.pair_list,
    });

    // The leaf can dismantle an owned pair: field 0 moves through the checked
    // op while field 1 is released residually. Its base parameter stays
    // borrowed, so only the owned field-take variant omits the field retain.
    const leaf_param = try f.local(f.pair_list);
    const leaf_field = try f.local(f.list_i64);
    const reversed = try f.local(f.list_i64);
    const leaf_result = try f.local(.i64);
    const leaf_ret = try f.ret(leaf_result);
    const leaf_result_assign = try f.assignI64(leaf_result, 1, leaf_ret);
    const reverse = try f.store.addCFStmt(.{ .assign_low_level = .{
        .target = reversed,
        .op = .list_reverse,
        .rc_effect = LIR.LowLevel.RcEffect.runtimeUniqueness(1),
        .args = try f.span(&.{leaf_field}),
        .next = leaf_result_assign,
    } });
    const leaf_read = try f.assignRefField(leaf_field, leaf_param, 0, reverse);
    const leaf = try f.addProc(&.{leaf_param}, leaf_read, .i64);

    // The wrapper only borrows its tag in the base signature. On the payload
    // path, however, the payload is the active variant's complete RC
    // ownership, so an owned wrapper can move that unit into the leaf's
    // field-take variant without scalarizing the tag or retaining the pair.
    const wrapper_param = try f.local(tag_pair);
    const disc = try f.local(.u8);
    const payload = try f.local(f.pair_list);
    const payload_alias = try f.local(f.pair_list);
    const branch_result = try f.local(.i64);
    const default_result = try f.local(.i64);
    const branch_ret = try f.ret(branch_result);
    const branch_call = try f.store.addCFStmt(.{ .assign_call = .{
        .target = branch_result,
        .proc = leaf,
        .args = try f.span(&.{payload_alias}),
        .next = branch_ret,
    } });
    const alias = try f.assignRefLocal(payload_alias, payload, branch_call);
    const payload_read = try f.store.addCFStmt(.{ .assign_ref = .{
        .target = payload,
        .op = .{ .tag_payload_struct = .{
            .source = wrapper_param,
            .variant_index = 1,
            .tag_discriminant = 1,
        } },
        .next = alias,
    } });
    const default_ret = try f.ret(default_result);
    const default_body = try f.assignI64(default_result, 0, default_ret);
    const switch_stmt = try f.switchStmt(disc, payload_read, default_body, null);
    const wrapper_body = try f.assignDiscriminant(disc, wrapper_param, switch_stmt);
    const wrapper = try f.addProc(&.{wrapper_param}, wrapper_body, .i64);

    const first = try f.local(f.list_i64);
    const second = try f.local(f.list_i64);
    const pair = try f.local(f.pair_list);
    const tag = try f.local(tag_pair);
    const caller_result = try f.local(.i64);
    const caller_ret = try f.ret(caller_result);
    const wrapper_call = try f.store.addCFStmt(.{ .assign_call = .{
        .target = caller_result,
        .proc = wrapper,
        .args = try f.span(&.{tag}),
        .next = caller_ret,
    } });
    const tag_assign = try f.assignTag(tag, 1, pair, wrapper_call);
    const pair_assign = try f.assignStruct(pair, &.{ first, second }, tag_assign);
    const second_assign = try f.assignList(second, &.{}, pair_assign);
    const caller_body = try f.assignList(first, &.{}, second_assign);
    _ = try f.addProc(&.{}, caller_body, .i64);

    const base_proc_count = f.store.procSpecCount();
    // General mode specialization is disabled, as it is in dev builds.
    // Exact field-take schedules still materialize because otherwise the
    // generated program must manufacture a second ownership unit.
    try f.run();

    try testing.expectEqual(base_proc_count + 2, f.store.procSpecCount());
    try testing.expectEqual(@as(usize, 1), f.countRc(leaf_field, .incref));
    try f.expectRc(tag, 0, 0, 0);
}

test "RC specialization: caller body survives variant proc append" {
    var f = try ArcTest.init(testing.allocator);
    defer f.deinit();

    // Callee returns its string parameter at position 1, so that parameter
    // and return solve borrowed.
    const callee_flag = try f.local(.i64);
    const callee_param = try f.local(.str);
    const callee_ret = try f.ret(callee_param);
    const callee = try f.addProc(&.{ callee_flag, callee_param }, callee_ret, .str);
    f.store.getProcSpecPtr(callee).runtime_ret_desc = callee_flag;

    // Caller builds an owned string and passes it as its final use. The
    // variant turns the borrowed return into an owned return. It is appended
    // while this caller is being materialized, so the caller body must be written
    // back only after reacquiring its proc-spec pointer.
    const source = try f.local(.str);
    const flag = try f.local(.i64);
    const result = try f.local(.str);
    const done = try f.local(.i64);
    const caller_ret = try f.ret(done);
    const done_assign = try f.assignI64(done, 1, caller_ret);
    const call = try f.store.addCFStmt(.{ .assign_call = .{
        .target = result,
        .proc = callee,
        .args = try f.span(&.{ flag, source }),
        .next = done_assign,
    } });
    const flag_assign = try f.assignI64(flag, 0, call);
    const caller_body = try f.assignStr(source, "arg", flag_assign);
    const caller = try f.addProc(&.{}, caller_body, .i64);

    const base_proc_count = f.store.procSpecCount();
    f.store.proc_specs.shrinkAndFree(f.allocator, f.store.procSpecCount());
    try insert(&f.store, &f.layouts, .{ .specialize = true });

    try testing.expectEqual(base_proc_count + 1, f.store.procSpecCount());
    const variant: LIR.LirProcSpecId = @enumFromInt(@as(u32, @intCast(base_proc_count)));
    try testing.expectEqual(callee_flag, f.store.getProcSpec(variant).runtime_ret_desc.?);

    var cursor = f.store.getProcSpec(caller).body orelse return error.MissingCallerBody;
    var remaining = f.store.cfStmtCount() + 1;
    while (remaining > 0) : (remaining -= 1) {
        switch (f.store.getCFStmt(cursor)) {
            .assign_call => |assign| {
                if (assign.target == result) {
                    try testing.expectEqual(variant, assign.proc);
                    return;
                }
                cursor = assign.next;
            },
            inline .assign_ref, .assign_literal, .assign_list, .assign_struct, .assign_tag, .set_local, .debug, .expect, .incref, .decref, .free, .comptime_branch_taken => |stmt| {
                cursor = stmt.next;
            },
            .init_uninitialized,
            .assign_call_erased,
            .assign_packed_erased_fn,
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
            .assign_low_level,
            .store_struct,
            .store_tag,
            .expect_err,
            .runtime_error,
            .comptime_exhaustiveness_failed,
            .decref_if_initialized,
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
            => return error.ExpectedSpecializedCall,
        }
    }
    return error.ExpectedSpecializedCall;
}

test "RC specialization: a variant's frame excludes the source's base dismantle temporaries" {
    var f = try ArcTest.init(testing.allocator);
    defer f.deinit();

    // Repro for https://github.com/roc-lang/roc/issues/10787: the callee's base
    // emission dismantles a pair it owns, which appends a residual-field
    // temporary to the callee's frame inventory. A specialized variant of that
    // same callee is emitted afterwards and solves from the callee's
    // producer-authored frame, so each emission's frame owns exactly the
    // temporaries that emission generated.
    const param = try f.local(.str);
    const first = try f.local(f.list_i64);
    const second = try f.local(f.list_i64);
    const pair = try f.local(f.pair_list);
    const field = try f.local(f.list_i64);
    const elem = try f.local(.i64);
    const appended = try f.local(f.list_i64);
    const callee_ret = try f.ret(param);
    const append = try f.assignLowLevel(appended, &.{ field, elem }, LIR.LowLevel.RcEffect.runtimeUniqueness(1), callee_ret);
    const elem_assign = try f.assignI64(elem, 5, append);
    const field_read = try f.assignRefField(field, pair, 0, elem_assign);
    const pair_assign = try f.assignStruct(pair, &.{ first, second }, field_read);
    const second_assign = try f.assignList(second, &.{}, pair_assign);
    const callee_body = try f.assignList(first, &.{}, second_assign);
    const callee = try f.addProc(&.{param}, callee_body, .str);

    // The caller's string dies at the call, which demands an owned return the
    // callee's borrowed signature does not provide, so a variant is emitted.
    const value = try f.local(.str);
    const result = try f.local(.str);
    const done = try f.local(.i64);
    const caller_ret = try f.ret(done);
    const done_assign = try f.assignI64(done, 1, caller_ret);
    const call = try f.store.addCFStmt(.{ .assign_call = .{
        .target = result,
        .proc = callee,
        .args = try f.span(&.{value}),
        .next = done_assign,
    } });
    const caller_body = try f.assignStr(value, "arg", call);
    _ = try f.addProc(&.{}, caller_body, .i64);

    const base_proc_count = f.store.procSpecCount();
    const producer_local_count = f.store.localCount();
    try insert(&f.store, &f.layouts, .{ .specialize = true });

    try testing.expectEqual(base_proc_count + 1, f.store.procSpecCount());
    const variant: LIR.LirProcSpecId = @enumFromInt(@as(u32, @intCast(base_proc_count)));

    const base_frame = f.store.getLocalSpan(f.store.getProcSpec(callee).frame_locals);
    const variant_frame = f.store.getLocalSpan(f.store.getProcSpec(variant).frame_locals);
    var base_generated: usize = 0;
    var variant_generated: usize = 0;
    var shared_generated: usize = 0;
    for (0..GuardedList.borrowLen(base_frame)) |base_index| {
        const base_local = GuardedList.at(base_frame, base_index);
        if (@intFromEnum(base_local) < producer_local_count) continue;
        base_generated += 1;
        for (0..GuardedList.borrowLen(variant_frame)) |variant_index| {
            if (GuardedList.at(variant_frame, variant_index) == base_local) shared_generated += 1;
        }
    }
    for (0..GuardedList.borrowLen(variant_frame)) |variant_index| {
        if (@intFromEnum(GuardedList.at(variant_frame, variant_index)) >= producer_local_count) variant_generated += 1;
    }

    // Both emissions dismantle the pair, so both frames grow, and no generated
    // temporary belongs to a frame other than the one that generated it.
    try testing.expect(base_generated > 0);
    try testing.expect(variant_generated > 0);
    try testing.expectEqual(@as(usize, 0), shared_generated);
}

test "RC without specialization: owned final argument drops after the call" {
    var f = try ArcTest.init(testing.allocator);
    defer f.deinit();

    const param = try f.local(.str);
    const callee_result = try f.local(.i64);
    const callee_ret = try f.ret(callee_result);
    const callee_result_assign = try f.assignI64(callee_result, 1, callee_ret);
    const callee_body = try f.expectStmt(param, callee_result_assign);
    const callee = try f.addProc(&.{param}, callee_body, .i64);

    const value = try f.local(.str);
    const result = try f.local(.i64);
    const ret = try f.ret(result);
    const call = try f.store.addCFStmt(.{ .assign_call = .{
        .target = result,
        .proc = callee,
        .args = try f.span(&.{value}),
        .next = ret,
    } });
    const body = try f.assignStr(value, "arg", call);
    _ = try f.addProc(&.{}, body, .i64);

    const base_proc_count = f.store.procSpecCount();
    try f.run();

    // The single-variant build keeps the borrowed signature: the caller
    // retains ownership across the call and releases right after it.
    try testing.expectEqual(base_proc_count, f.store.procSpecCount());
    try f.expectRc(value, 0, 1, 0);
    try f.expectRc(param, 0, 0, 0);
}

test "RC specialization: identical demand vectors share one variant" {
    var f = try ArcTest.init(testing.allocator);
    defer f.deinit();

    const param = try f.local(.str);
    const callee_ret = try f.ret(param);
    const callee_body = try f.expectStmt(param, callee_ret);
    const callee = try f.addProc(&.{param}, callee_body, .str);

    const value_a = try f.local(.str);
    const value_b = try f.local(.str);
    const result_a = try f.local(.str);
    const result_b = try f.local(.str);
    const done = try f.local(.i64);
    const ret = try f.ret(done);
    const done_assign = try f.assignI64(done, 1, ret);
    const call_b = try f.store.addCFStmt(.{ .assign_call = .{
        .target = result_b,
        .proc = callee,
        .args = try f.span(&.{value_b}),
        .next = done_assign,
    } });
    const assign_b = try f.assignStr(value_b, "b", call_b);
    const call_a = try f.store.addCFStmt(.{ .assign_call = .{
        .target = result_a,
        .proc = callee,
        .args = try f.span(&.{value_a}),
        .next = assign_b,
    } });
    const body = try f.assignStr(value_a, "a", call_a);
    _ = try f.addProc(&.{}, body, .i64);

    const base_proc_count = f.store.procSpecCount();
    try insert(&f.store, &f.layouts, .{ .specialize = true });

    try testing.expectEqual(base_proc_count + 1, f.store.procSpecCount());
    try f.expectRc(value_a, 0, 0, 0);
    try f.expectRc(value_b, 0, 0, 0);
}

test "RC interprocedural: borrowed parameter passed through emits no RC statements" {
    var f = try ArcTest.init(testing.allocator);
    defer f.deinit();

    // Inner callee reads its parameter.
    const inner_param = try f.local(.str);
    const inner_result = try f.local(.i64);
    const inner_ret = try f.ret(inner_result);
    const inner_result_assign = try f.assignI64(inner_result, 1, inner_ret);
    const inner_body = try f.expectStmt(inner_param, inner_result_assign);
    const inner = try f.addProc(&.{inner_param}, inner_body, .i64);

    // Outer callee forwards its parameter to the inner one.
    const outer_param = try f.local(.str);
    const outer_result = try f.local(.i64);
    const outer_ret = try f.ret(outer_result);
    const outer_call = try f.store.addCFStmt(.{ .assign_call = .{
        .target = outer_result,
        .proc = inner,
        .args = try f.span(&.{outer_param}),
        .next = outer_ret,
    } });
    _ = try f.addProc(&.{outer_param}, outer_call, .i64);

    try f.run();
    // Both parameters solve borrowed: the chain of reads emits nothing.
    try f.expectRc(inner_param, 0, 0, 0);
    try f.expectRc(outer_param, 0, 0, 0);
}

test "RC interprocedural: borrowed return borrows the argument in the caller" {
    var f = try ArcTest.init(testing.allocator);
    defer f.deinit();

    // Identity proc: borrowed parameter, borrowed return.
    const id_param = try f.local(.str);
    const id_ret = try f.ret(id_param);
    const identity = try f.addProc(&.{id_param}, id_ret, .str);

    // Caller uses the identity result read-only.
    const value = try f.local(.str);
    const alias = try f.local(.str);
    const result = try f.local(.i64);
    const ret = try f.ret(result);
    const result_assign = try f.assignI64(result, 1, ret);
    const use_alias = try f.expectStmt(alias, result_assign);
    const call = try f.store.addCFStmt(.{ .assign_call = .{
        .target = alias,
        .proc = identity,
        .args = try f.span(&.{value}),
        .next = use_alias,
    } });
    const body = try f.assignStr(value, "borrowed-through", call);
    _ = try f.addProc(&.{}, body, .i64);

    try f.run();
    // The identity proc emits nothing; the caller borrows the result and
    // releases the original after the borrow's last use.
    try f.expectRc(id_param, 0, 0, 0);
    try f.expectRc(alias, 0, 0, 0);
    try f.expectRc(value, 0, 1, 0);
}
test "RC borrow survives the lender moving into an aggregate" {
    var f = try ArcTest.init(testing.allocator);
    defer f.deinit();
    const inner = try f.local(.str);
    const tagged = try f.local(f.tag_str);
    const payload = try f.local(.str);
    const alias = try f.local(.str);
    const other = try f.local(.str);
    const pair = try f.local(f.pair_str);
    const call_result = try f.local(.i64);
    const result = try f.local(.i64);

    // inner = "x"; tagged = tag(inner); payload = tagged.payload;
    // alias = payload; other = "y"; pair = {payload, other};
    // call(pair); expect(alias); result = 1; ret result
    const ret = try f.ret(result);
    const result_assign = try f.assignI64(result, 1, ret);
    const use_alias = try f.expectStmt(alias, result_assign);
    const consume_pair = try f.assignCall(call_result, &.{pair}, use_alias);
    const pair_assign = try f.assignStruct(pair, &.{ payload, other }, consume_pair);
    const other_assign = try f.assignStr(other, "y", pair_assign);
    const alias_assign = try f.assignRefLocal(alias, payload, other_assign);
    const payload_read = try f.assignTagPayload(payload, tagged, alias_assign);
    const tag_assign = try f.assignTag(tagged, 1, inner, payload_read);
    const body = try f.assignStr(inner, "x", tag_assign);
    _ = try f.addProc(&.{}, body, .i64);
    try f.run();
    // The payload's retain at the read or store must keep the alias's chain
    // live across the consuming call; the certifier validates whichever
    // placement emission chooses.
    try testing.expect(f.countRc(payload, .incref) >= 1);
}

test "RC alias chain into a consuming call moves the unit through" {
    var f = try ArcTest.init(testing.allocator);
    defer f.deinit();
    const value = try f.local(.str);
    const alias_a = try f.local(.str);
    const alias_b = try f.local(.str);
    const result = try f.local(.i64);
    const ret = try f.ret(result);
    const call = try f.assignCall(result, &.{alias_b}, ret);
    const alias_b_assign = try f.assignRefLocal(alias_b, alias_a, call);
    const alias_a_assign = try f.assignRefLocal(alias_a, value, alias_b_assign);
    const body = try f.assignStr(value, "through", alias_a_assign);
    _ = try f.addProc(&.{}, body, .i64);
    try f.run();
    // The demand on the consumed alias propagates to the chain's owner, so
    // the single unit moves link by link into the call.
    try testing.expectEqual(@as(usize, 0), f.countAllRc());
}

test "RC alias exclusion maps high local ids through dense liveness bits" {
    var f = try ArcTest.init(testing.allocator);
    defer f.deinit();
    for (0..16) |_| _ = try f.local(.i64);
    const value = try f.local(.str);
    const alias = try f.local(.str);
    const result = try f.local(.i64);
    const ret = try f.ret(result);
    const call = try f.assignCall(result, &.{alias}, ret);
    const alias_assign = try f.assignRefLocal(alias, value, call);
    const body = try f.assignStr(value, "dense", alias_assign);
    _ = try f.addProc(&.{}, body, .i64);
    try f.run();
    try testing.expectEqual(@as(usize, 0), f.countAllRc());
}

test "RC alias into aggregate moves the leader unit" {
    var f = try ArcTest.init(testing.allocator);
    defer f.deinit();
    const value = try f.local(.str);
    const alias = try f.local(.str);
    const pair = try f.local(f.pair_str);
    const ret = try f.ret(pair);
    const pair_assign = try f.assignStruct(pair, &.{alias}, ret);
    const alias_assign = try f.assignRefLocal(alias, value, pair_assign);
    const body = try f.assignStr(value, "through", alias_assign);
    _ = try f.addProc(&.{}, body, f.pair_str);
    try f.run();
    try testing.expectEqual(@as(usize, 0), f.countAllRc());
}

test "RC alias into set_local moves the leader unit" {
    var f = try ArcTest.init(testing.allocator);
    defer f.deinit();
    const value = try f.local(.str);
    const alias = try f.local(.str);
    const result = try f.local(.str);
    const ret = try f.ret(result);
    const set_result = try f.setLocal(result, alias, .initialize_join_result, ret);
    const alias_assign = try f.assignRefLocal(alias, value, set_result);
    const body = try f.assignStr(value, "through-set-local", alias_assign);
    _ = try f.addProc(&.{}, body, .str);
    try f.run();
    try testing.expectEqual(@as(usize, 0), f.countAllRc());
}

test "RC releases descriptor-backed old set_local value before immutable replacement" {
    var f = try ArcTest.init(testing.allocator);
    defer f.deinit();

    const desc = try f.local(.opaque_ptr);
    const current = try f.local(.str);
    const replacement = try f.local(.str);
    f.store.setLocalBoxyDesc(current, .{ .local = desc });

    const ret = try f.ret(current);
    const set_current = try f.setLocal(current, replacement, .replace_existing, ret);
    const assign_replacement = try f.assignStr(replacement, "new", set_current);
    const assign_current = try f.assignStr(current, "old", assign_replacement);
    const init_desc = try f.store.addCFStmt(.{ .assign_boxy_desc_ref = .{
        .target = desc,
        .desc = .{ .static = @enumFromInt(fixtureTableIndex(0)) },
        .next = assign_current,
    } });
    _ = try f.addProc(&.{}, init_desc, .str);

    try f.run();
    try f.expectReachableDecrefBeforeSet(f.procBody(), current, current);
}

test "RC descriptor updates scan only the current proc frame" {
    var f = try ArcTest.init(testing.allocator);
    defer f.deinit();

    const unrelated = try f.local(.str);
    const desc = try f.local(.opaque_ptr);
    const current = try f.local(.str);
    const result = try f.local(.i64);
    f.store.setLocalBoxyDesc(current, .{ .local = desc });

    const unrelated_ret = try f.ret(unrelated);
    const unrelated_body = try f.assignStr(unrelated, "other proc", unrelated_ret);
    _ = try f.store.addProcSpec(.{
        .name = f.store.freshSyntheticSymbol(),
        .args = LIR.LocalSpan.empty(),
        .body = unrelated_body,
        .frame_locals = try f.span(&.{unrelated}),
        .ret_layout = .str,
    });

    const ret = try f.ret(result);
    const assign_result = try f.assignI64(result, 1, ret);
    const update_desc = try f.store.addCFStmt(.{ .assign_boxy_desc_ref = .{
        .target = desc,
        .desc = .{ .static = @enumFromInt(fixtureTableIndex(0)) },
        .next = assign_result,
    } });
    const assign_current = try f.assignStr(current, "old", update_desc);
    const init_desc = try f.store.addCFStmt(.{ .assign_boxy_desc_ref = .{
        .target = desc,
        .desc = .{ .static = @enumFromInt(fixtureTableIndex(0)) },
        .next = assign_current,
    } });
    _ = try f.store.addProcSpec(.{
        .name = f.store.freshSyntheticSymbol(),
        .args = LIR.LocalSpan.empty(),
        .body = init_desc,
        .frame_locals = try f.span(&.{ desc, current, result }),
        .ret_layout = .i64,
    });

    try f.run();
    try f.expectRc(current, 0, 1, 0);
}

test "RC descriptor snapshot owns an alias across source descriptor reuse" {
    var f = try ArcTest.init(testing.allocator);
    defer f.deinit();

    const source_desc = try f.local(.opaque_ptr);
    const alias_desc = try f.local(.opaque_ptr);
    const source = try f.local(.str);
    const alias = try f.local(.str);
    const result = try f.local(.i64);
    f.store.setLocalBoxyDesc(source, .{ .local = source_desc });
    f.store.setLocalBoxyDesc(alias, .{ .local = alias_desc });

    const ret = try f.ret(result);
    const assign_result = try f.assignI64(result, 1, ret);
    const use_alias = try f.expectStmt(alias, assign_result);
    const reuse_source_desc = try f.store.addCFStmt(.{ .assign_boxy_desc_ref = .{
        .target = source_desc,
        .desc = .{ .static = @enumFromInt(fixtureTableIndex(1)) },
        .next = use_alias,
    } });
    const assign_alias = try f.assignRefLocal(alias, source, reuse_source_desc);
    const snapshot_desc = try f.setLocal(alias_desc, source_desc, .initialize_join_result, assign_alias);
    const assign_source = try f.assignStr(source, "old", snapshot_desc);
    const init_source_desc = try f.store.addCFStmt(.{ .assign_boxy_desc_ref = .{
        .target = source_desc,
        .desc = .{ .static = @enumFromInt(fixtureTableIndex(0)) },
        .next = assign_source,
    } });
    _ = try f.addProc(&.{}, init_source_desc, .i64);

    try f.run();
    try f.expectRc(source, 0, 0, 0);
    try f.expectRc(alias, 0, 1, 0);
}

test "RC preserves a surviving source before a consuming boxy adapter" {
    var f = try ArcTest.init(testing.allocator);
    defer f.deinit();

    const source = try f.local(f.list_i64);
    const adapted = try f.local(f.list_i64);
    const result = try f.local(.i64);
    const desc = LIR.BoxyDescRef{ .static = @enumFromInt(fixtureTableIndex(0)) };
    f.store.setLocalBoxyDesc(source, desc);
    f.store.setLocalBoxyDesc(adapted, desc);

    const ret = try f.ret(result);
    const assign_result = try f.assignI64(result, 1, ret);
    const use_source = try f.expectStmt(source, assign_result);
    const adapt = try f.store.addCFStmt(.{ .assign_boxy_adapt = .{
        .target = adapted,
        .source = source,
        .adapter = @enumFromInt(fixtureTableIndex(0)),
        .source_desc = desc,
        .target_desc = desc,
        .source_mode = .move,
        .next = use_source,
    } });
    const body = try f.assignList(source, &.{}, adapt);
    _ = try f.addProc(&.{}, body, .i64);

    // Certification verifies that the retain precedes the consuming move.
    try f.run();
    try f.expectRc(source, 1, 1, 0);
    try f.expectRc(adapted, 0, 1, 0);
}

test "RC does not treat a descriptor-bearing scalar dictionary result as refcounted" {
    var f = try ArcTest.init(testing.allocator);
    defer f.deinit();

    const desc = LIR.BoxyDescRef{ .static = @enumFromInt(fixtureTableIndex(0)) };
    const result = try f.local(.u32);
    const adapted = try f.local(.u32);
    f.store.setLocalBoxyDesc(result, desc);
    f.store.setLocalBoxyDesc(adapted, desc);

    const ret = try f.ret(result);
    const adapt = try f.store.addCFStmt(.{ .assign_boxy_adapt = .{
        .target = adapted,
        .source = result,
        .adapter = @enumFromInt(fixtureTableIndex(0)),
        .source_desc = desc,
        .target_desc = desc,
        .source_mode = .move,
        .next = ret,
    } });
    const body = try f.store.addCFStmt(.{ .assign_call_dict = .{
        .target = result,
        .dict = .{ .static = @enumFromInt(fixtureTableIndex(0)) },
        .method = @enumFromInt(fixtureTableIndex(0)),
        .method_slot = 0,
        .args = .empty(),
        .result_desc = desc,
        .next = adapt,
    } });
    _ = try f.addProc(&.{}, body, .u32);

    try f.run();
    try testing.expectEqual(@as(usize, 0), f.countAllRc());
}

test "RC alias passed as a dying call argument moves the leader unit" {
    // Issue 9703's keying class, through the call-argument transfer path:
    // the pure alias is borrowed, so its ownership unit lives on the source
    // local. Passing the alias as a dying owned call argument must move the
    // *source's* unit (no retain before the call, no release after), keyed
    // through unitOf—testing or clearing the OwnedSet by the alias's own
    // id would leak the source's unit. The debug certifier re-checks the
    // emitted schedule during `run`.
    var f = try ArcTest.init(testing.allocator);
    defer f.deinit();
    const value = try f.local(.str);
    const alias = try f.local(.str);
    const result = try f.local(.i64);
    const ret = try f.ret(result);
    const call = try f.assignCall(result, &.{alias}, ret);
    const alias_assign = try f.assignRefLocal(alias, value, call);
    const body = try f.assignStr(value, "through-call-arg", alias_assign);
    _ = try f.addProc(&.{}, body, .i64);
    try f.run();
    try testing.expectEqual(@as(usize, 0), f.countAllRc());
}

test "RC alias of a parameter consumed in the body solves the parameter owned" {
    var f = try ArcTest.init(testing.allocator);
    defer f.deinit();

    // Wrapper proc: alias the parameter, consume the alias.
    const param = try f.local(f.list_str);
    const alias = try f.local(f.list_str);
    const elem = try f.local(.str);
    const appended = try f.local(f.list_str);
    const wrapper_ret = try f.ret(appended);
    const append = try f.store.addCFStmt(.{ .assign_low_level = .{
        .target = appended,
        .op = .list_append_unsafe,
        .rc_effect = LIR.LowLevel.RcEffect.consumesArgsReturningConsumedArgsRetainingArgs(1, 2),
        .args = try f.span(&.{ alias, elem }),
        .next = wrapper_ret,
    } });
    const alias_assign = try f.assignRefLocal(alias, param, append);
    const elem_assign = try f.assignStr(elem, "x", alias_assign);
    const wrapper = try f.addProc(&.{param}, elem_assign, f.list_str);

    // Caller passes a dying list.
    const list = try f.local(f.list_str);
    const call_result = try f.local(f.list_str);
    const caller_ret = try f.ret(call_result);
    const call = try f.store.addCFStmt(.{ .assign_call = .{
        .target = call_result,
        .proc = wrapper,
        .args = try f.span(&.{list}),
        .next = caller_ret,
    } });
    const caller_body = try f.assignList(list, &.{}, call);
    _ = try f.addProc(&.{}, caller_body, f.list_str);

    try f.run();
    // The alias's consumption demands the parameter, so the parameter is
    // owned, the caller's argument moves in, the alias moves the parameter's
    // unit into the append, and the result moves out: no RC statements on
    // the list anywhere.
    try f.expectRc(param, 0, 0, 0);
    try f.expectRc(alias, 0, 0, 0);
    try f.expectRc(list, 0, 0, 0);
    try f.expectRc(appended, 0, 0, 0);
    try f.expectRc(call_result, 0, 0, 0);
}
