//! ARC insertion for LIR: borrow inference plus RC statement emission.
//!
//! This pass is the only non-builtin stage that may synthesize explicit
//! `incref`, `decref`, and `free` statements. It first solves binding modes
//! and proc ownership signatures (`arc_solve`), then walks each proc once and
//! emits RC statements from the solution: borrowed bindings emit nothing,
//! owned final occurrences move, and lifetime-ending releases land right
//! after the last use of a binding's borrow group. Optimized builds also emit
//! mode-specialized proc variants for call sites that can move arguments into
//! positions the solved signature borrows, or that prove a dying argument
//! statically unique so the variant elides the runtime uniqueness checks
//! that parameter reaches. Debug builds re-check the output
//! with the borrow certifier (`arc_certify`). Backends consume explicit RC
//! statements without doing reference-counting analysis.

const std = @import("std");
const builtin = @import("builtin");
const Allocator = std.mem.Allocator;
const core = @import("lir_core");
const layout_mod = @import("layout");
const arc_sig = @import("arc_sig.zig");
const arc_solve = @import("arc_solve.zig");
const arc_certify = @import("arc_certify.zig");

const LIR = core.LIR;
const LirStore = core.LirStore;

pub const ResourceError = std.mem.Allocator.Error;

/// Options for ARC insertion.
pub const InsertOptions = struct {
    /// Root procs whose ownership signature is pinned all-owned by ABI.
    roots: []const LIR.LirProcSpecId = &.{},
    /// Emit mode-specialized proc variants for call sites that demand more
    /// ownership than a callee's solved signature provides. Optimized builds
    /// enable this; dev builds and compile-time evaluation use the solved
    /// single variant per proc.
    specialize: bool = false,
};

/// Public `insert` function.
pub fn insert(store: *LirStore, layouts: *const layout_mod.Store, options: InsertOptions) ResourceError!void {
    var inserter = Inserter{
        .store = store,
        .layouts = layouts,
    };
    var local_contains_refcounted = try store.allocator.alloc(bool, store.locals.items.len);
    defer store.allocator.free(local_contains_refcounted);
    for (store.locals.items, 0..) |local, index| {
        local_contains_refcounted[index] = layouts.layoutContainsRefcounted(layouts.getLayout(local.layout_idx));
    }
    inserter.local_contains_refcounted = local_contains_refcounted;

    var solution = try arc_solve.solve(store.allocator, store, local_contains_refcounted, options.roots);
    defer solution.deinit();
    inserter.solution = &solution;

    var scan_needles = try OwnedSet.init(store.allocator, store.locals.items.len);
    defer scan_needles.deinit();
    inserter.scan_needles = &scan_needles;
    var scan_visited = std.AutoHashMap(LIR.CFStmtId, void).init(store.allocator);
    defer scan_visited.deinit();
    inserter.scan_visited = &scan_visited;
    var scan_stack = std.ArrayList(LIR.CFStmtId).empty;
    defer scan_stack.deinit(store.allocator);
    inserter.scan_stack = &scan_stack;
    var read_cache_arena = std.heap.ArenaAllocator.init(store.allocator);
    defer read_cache_arena.deinit();
    inserter.read_cache_arena = &read_cache_arena;
    inserter.read_cache_allocator = read_cache_arena.allocator();
    var reads_before_rebind_cache = std.AutoHashMap(ReadBeforeRebindKey, std.bit_set.DynamicBitSetUnmanaged).init(store.allocator);
    defer reads_before_rebind_cache.deinit();
    inserter.reads_before_rebind_cache = &reads_before_rebind_cache;
    var active_loop_keep_ids = std.AutoHashMap(usize, u32).init(store.allocator);
    defer active_loop_keep_ids.deinit();
    inserter.active_loop_keep_ids = &active_loop_keep_ids;

    // Original (ownership-neutral) bodies stay valid after each proc's base
    // emission because rewriting clones statements; specialized variants
    // re-emit from these.
    const base_proc_count = store.proc_specs.items.len;
    var original_bodies = try store.allocator.alloc(?LIR.CFStmtId, base_proc_count);
    defer store.allocator.free(original_bodies);
    for (store.proc_specs.items, 0..) |proc, proc_index| {
        original_bodies[proc_index] = proc.body;
    }

    var variants = VariantTable{
        .map = std.AutoHashMap(VariantSelector, LIR.LirProcSpecId).init(store.allocator),
        .sigs = .empty,
        .queue = .empty,
        .enabled = options.specialize,
        .original_bodies = original_bodies,
        .unique_seed_masks = try store.allocator.alloc(?u64, base_proc_count),
    };
    @memset(variants.unique_seed_masks, null);
    defer {
        store.allocator.free(variants.unique_seed_masks);
        variants.map.deinit();
        variants.sigs.deinit(store.allocator);
        variants.queue.deinit(store.allocator);
    }
    inserter.variants = &variants;

    var owned_param_override = try OwnedSet.init(store.allocator, store.locals.items.len);
    defer owned_param_override.deinit();
    inserter.owned_param_override = &owned_param_override;

    var unique_param_override = try OwnedSet.init(store.allocator, store.locals.items.len);
    defer unique_param_override.deinit();
    inserter.unique_param_override = &unique_param_override;

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

        const body = original_bodies[@intFromEnum(source_proc)] orelse continue;
        const emit_args = store.getProcSpec(emit_proc).args;
        inserter.current_sig = emit_sig;
        inserter.current_proc_body = body;
        inserter.clearReadsBeforeRebindCache();

        // Variant parameter positions demanded owned override the solved
        // borrowed binding for this emission only, and positions the demand
        // vector proves unique seed the body's born-unique view.
        const solved_sig = solution.sigOf(source_proc);
        var override_locals_buffer: [64]LIR.LocalId = undefined;
        var override_count: usize = 0;
        var unique_locals_buffer: [64]LIR.LocalId = undefined;
        var unique_count: usize = 0;
        for (store.getLocalSpan(emit_args), 0..) |param, position| {
            if (position >= 64) break;
            if (solved_sig.paramMode(position) == .borrowed and emit_sig.paramMode(position) == .owned) {
                owned_param_override.set(param);
                override_locals_buffer[override_count] = param;
                override_count += 1;
            }
            if ((emit_sig.unique_params >> @as(u6, @intCast(position))) & 1 != 0) {
                unique_param_override.set(param);
                unique_locals_buffer[unique_count] = param;
                unique_count += 1;
            }
        }
        defer for (override_locals_buffer[0..override_count]) |param| {
            owned_param_override.unset(param);
        };
        defer for (unique_locals_buffer[0..unique_count]) |param| {
            unique_param_override.unset(param);
        };

        var join_bodies = JoinBodyMap.init(store.allocator);
        defer join_bodies.deinit();
        var join_visit = std.AutoHashMap(LIR.CFStmtId, void).init(store.allocator);
        defer join_visit.deinit();
        try inserter.collectJoinBodies(body, &join_bodies, &join_visit);
        inserter.join_bodies = &join_bodies;
        defer inserter.join_bodies = null;
        var rewritten_joins = RewrittenJoinMap.init(store.allocator);
        defer {
            var iter = rewritten_joins.valueIterator();
            while (iter.next()) |entry| entry.keep.deinit();
            rewritten_joins.deinit();
        }
        inserter.rewritten_joins = &rewritten_joins;
        defer inserter.rewritten_joins = null;
        var join_body_memo = JoinBodyMemo.init(store.allocator);
        defer inserter.deinitJoinBodyMemo(&join_body_memo);
        inserter.join_body_memo = &join_body_memo;
        defer inserter.join_body_memo = null;
        var owned = try OwnedSet.init(store.allocator, store.locals.items.len);
        defer owned.deinit();
        for (store.getLocalSpan(emit_args), 0..) |param, position| {
            if (emit_sig.paramMode(position) == .owned) {
                if (inserter.localContainsRefcounted(param)) owned.set(param);
            }
        }
        const rewritten_body = try inserter.rewritePath(body, &owned, .{});
        // `rewritePath` may append mode-specialized proc variants via
        // `addProcSpec`, which can reallocate `store.proc_specs` and invalidate
        // the `proc` pointer captured above. Re-fetch the slot before writing
        // the rewritten body and join points so they land in the live backing
        // storage rather than a freed buffer.
        const proc_after = store.getProcSpecPtr(emit_proc);
        proc_after.body = rewritten_body;
        try inserter.writeProcJoinPoints(proc_after);
    }

    if (builtin.mode == .Debug) {
        const all_sigs = try store.allocator.alloc(arc_sig.RcSig, store.proc_specs.items.len);
        defer store.allocator.free(all_sigs);
        for (all_sigs, 0..) |*sig, proc_index| {
            sig.* = if (proc_index < solution.sigs.len)
                solution.sigs[proc_index]
            else
                variants.sigs.items[proc_index - solution.sigs.len];
        }
        try arc_certify.certifyStoreOrPanic(store.allocator, store, layouts, .{ .sigs = all_sigs }, options.roots);
    }
}

const VariantSelector = struct {
    source: LIR.LirProcSpecId,
    borrowed_params: u64,
    ret_mode: arc_sig.Mode,
    /// Parameter positions the demand vector seeds born-unique.
    unique_params: u64,
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
    /// Lazily-computed parameter positions whose unique seed can eliminate a
    /// runtime uniqueness check in the original body.
    unique_seed_masks: []?u64,
};

const RewriteOptions = struct {
    boundaries: []const RewriteBoundary = &.{},
    loop_keep: ?*const OwnedSet = null,
    loop_keep_id: u32 = 0,
    join_keeps: []const JoinKeep = &.{},
};

const RewriteBoundary = struct {
    stop: LIR.CFStmtId,
    replacement: LIR.CFStmtId,
    keep: *const OwnedSet,
};

const JoinKeep = struct {
    target: LIR.JoinPointId,
    keep: *const OwnedSet,
};

const ReadBeforeRebindKey = struct {
    start: LIR.CFStmtId,
    loop_keep_id: u32,
};

const AnalysisScopedJoin = struct {
    body: LIR.CFStmtId,
    remainder: LIR.CFStmtId,
    params: LIR.LocalSpan,
    maybe_uninitialized_params: LIR.LocalSpan,
    entry_owned: OwnedSet,
    keep: ?OwnedSet = null,
};

const AnalysisScopedJoinMap = std.AutoHashMap(LIR.JoinPointId, AnalysisScopedJoin);

const AnalysisSeenEntry = struct {
    owned: OwnedSet,
};

const AnalysisSeenId = struct {
    stmt: LIR.CFStmtId,
    owned_digest: u64,
};

const AnalysisSeenBucket = struct {
    entries: std.ArrayList(AnalysisSeenEntry),
};

const AnalysisSeen = std.AutoHashMap(AnalysisSeenId, AnalysisSeenBucket);

const JoinBodyMemoId = struct {
    join: LIR.JoinPointId,
    owned_digest: u64,
};

const JoinBodyMemoEntry = struct {
    entry_owned: OwnedSet,
    params: LIR.LocalSpan,
    maybe_uninitialized_params: LIR.LocalSpan,
    remainder: LIR.CFStmtId,
    body: LIR.CFStmtId,
    keep: OwnedSet,
    body_reachable: bool,
};

const JoinBodyMemoBucket = struct {
    entries: std.ArrayList(JoinBodyMemoEntry),
};

const JoinBodyMemo = std.AutoHashMap(JoinBodyMemoId, JoinBodyMemoBucket);

const AnalysisStop = union(enum) {
    /// Stop at an explicit shared continuation statement.
    stmt: LIR.CFStmtId,
    /// Stop at jumps targeting one join, recording the jump-entry ownership.
    jump_to: LIR.JoinPointId,
};

fn rewriteBoundaryForStop(boundaries: []const RewriteBoundary, cursor: LIR.CFStmtId) ?RewriteBoundary {
    var i = boundaries.len;
    while (i > 0) {
        i -= 1;
        if (boundaries[i].stop == cursor) return boundaries[i];
    }
    return null;
}

fn replacementReached(boundaries: []const RewriteBoundary, cursor: LIR.CFStmtId) bool {
    var i = boundaries.len;
    while (i > 0) {
        i -= 1;
        if (boundaries[i].replacement == cursor) return true;
    }
    return false;
}

fn appendRewriteBoundary(
    allocator: std.mem.Allocator,
    boundaries: []const RewriteBoundary,
    boundary: RewriteBoundary,
) ResourceError![]RewriteBoundary {
    const nested = try allocator.alloc(RewriteBoundary, boundaries.len + 1);
    @memcpy(nested[0..boundaries.len], boundaries);
    nested[boundaries.len] = boundary;
    return nested;
}

fn appendJoinKeep(
    allocator: std.mem.Allocator,
    join_keeps: []const JoinKeep,
    join_keep: JoinKeep,
) ResourceError![]JoinKeep {
    const nested = try allocator.alloc(JoinKeep, join_keeps.len + 1);
    @memcpy(nested[0..join_keeps.len], join_keeps);
    nested[join_keeps.len] = join_keep;
    return nested;
}

fn keepForJoin(join_keeps: []const JoinKeep, target: LIR.JoinPointId) ?*const OwnedSet {
    var i = join_keeps.len;
    while (i > 0) {
        i -= 1;
        if (join_keeps[i].target == target) return join_keeps[i].keep;
    }
    return null;
}

const LinearRewriteFrame = struct {
    stmt: LIR.CFStmtId,
    head: LIR.CFStmtId,
    retain_assign_ref_target: bool = true,
    retain_set_target: bool = true,
    /// Span-indexed operand positions whose ownership unit moves into the
    /// constructed value instead of being retained.
    transfer_mask: u64 = 0,
    /// Move the tag payload or packed capture instead of retaining it.
    transfer_single: bool = false,
    /// Skip the low-level retain_result incref: the result binding is
    /// borrowed and emits no RC statements.
    skip_result_retain: bool = false,
    /// Runtime uniqueness checks this low-level statement proved redundant,
    /// by argument position.
    unique_args: u64 = 0,
    /// Retain the call result right after the call: the callee returns a
    /// borrow of its arguments but this binding needs its own unit.
    retain_call_result: bool = false,
    /// Mode-specialized variant this call site targets instead of the
    /// original callee.
    call_target_override: ?LIR.LirProcSpecId = null,
    /// Locals whose lifetime ends at this statement; released right after it.
    /// Owned by the frame and freed when the frame is patched or destroyed.
    post_release: []const LIR.LocalId = &.{},
};

const Inserter = struct {
    store: *LirStore,
    layouts: *const layout_mod.Store,
    local_contains_refcounted: []const bool = &.{},
    solution: *const arc_solve.Solution = undefined,
    /// Mode-specialized variant table (shared across the emission worklist).
    variants: *VariantTable = undefined,
    /// Parameter locals whose borrowed solved binding is overridden to owned
    /// for the variant currently being emitted.
    owned_param_override: *OwnedSet = undefined,
    /// Parameter locals the current variant's demand vector seeds as born
    /// unique; consumed by `uniqueArgsMask` through `isLocalUniqueHere`.
    unique_param_override: *OwnedSet = undefined,
    /// Ownership signature of the proc currently being rewritten.
    current_sig: arc_sig.RcSig = arc_sig.RcSig.all_owned,
    /// Scratch needle set reused by liveness-group scans.
    scan_needles: *OwnedSet = undefined,
    /// Scratch control-flow visited set reused by liveness-group scans.
    scan_visited: *std.AutoHashMap(LIR.CFStmtId, void) = undefined,
    /// Scratch control-flow stack reused by liveness-group scans.
    scan_stack: *std.ArrayList(LIR.CFStmtId) = undefined,
    /// Per-proc cache for "which locals may be read before they are rebound
    /// from this statement?" ARC asks that question many times while deciding
    /// where ownership units die. Generated record parsers create many
    /// field-slot locals and join paths, so walking the graph once per local
    /// becomes quadratic. This cache stores the same information with the
    /// statement transfer rules used by the old scan: reads happen before a
    /// statement's target rebinding kills the previous value.
    reads_before_rebind_cache: *std.AutoHashMap(ReadBeforeRebindKey, std.bit_set.DynamicBitSetUnmanaged) = undefined,
    read_cache_arena: *std.heap.ArenaAllocator = undefined,
    read_cache_allocator: Allocator = undefined,
    /// Live mapping from an OwnedSet address used as a loop keep-set to its
    /// explicit cache identity. IDs are never reused within one proc emission,
    /// and the map entry is removed before the keep-set storage is destroyed.
    active_loop_keep_ids: *std.AutoHashMap(usize, u32) = undefined,
    next_loop_keep_id: u32 = 1,
    current_proc_body: LIR.CFStmtId = undefined,
    join_bodies: ?*const JoinBodyMap = null,
    rewritten_joins: ?*RewrittenJoinMap = null,
    join_body_memo: ?*JoinBodyMemo = null,

    const CallArgOwnership = struct {
        retain_args: std.ArrayList(LIR.LocalId) = .empty,
        transfer_args: std.ArrayList(LIR.LocalId) = .empty,
        /// Ownership vector the call site demands; differs from the callee's
        /// solved signature only when borrowed positions upgrade to moves.
        demanded: arc_sig.RcSig = arc_sig.RcSig.all_owned,

        fn deinit(self: *CallArgOwnership, allocator: std.mem.Allocator) void {
            self.retain_args.deinit(allocator);
            self.transfer_args.deinit(allocator);
        }
    };

    const RewriteTask = union(enum) {
        path: *RewritePathTask,
        join: *RewriteJoinTask,
        switch_no_continuation: *RewriteSwitchNoContinuationTask,
        switch_after_continuation: *RewriteSwitchContinuationTask,
        switch_finish_continuation: *RewriteSwitchContinuationTask,
        initialized_payload_switch: *RewriteInitializedPayloadSwitchTask,
        str_match: *RewriteStrMatchTask,
        str_match_set: *RewriteStrMatchSetTask,
    };

    const RewritePathTask = struct {
        cursor: LIR.CFStmtId,
        owned: OwnedSet,
        options: RewriteOptions,
        frames: std.ArrayList(LinearRewriteFrame),
        result: *LIR.CFStmtId,
    };

    const RewriteJoinTask = struct {
        start: LIR.CFStmtId,
        id: LIR.JoinPointId,
        params: LIR.LocalSpan,
        maybe_uninitialized_params: LIR.LocalSpan = .empty(),
        maybe_uninitialized_conditions: LIR.LocalSpan = .empty(),
        maybe_uninitialized_condition_masks: LIR.U64Span = .empty(),
        body: LIR.CFStmtId = undefined,
        remainder: LIR.CFStmtId = undefined,
        incoming_owned: OwnedSet,
        entry_keep: OwnedSet,
        body_keep: OwnedSet,
        body_reachable: bool,
        loop_keep_id: u32,
        join_keeps: []JoinKeep = &.{},
        frames: std.ArrayList(LinearRewriteFrame),
        result: *LIR.CFStmtId,
    };

    const RewriteSwitchNoContinuationTask = struct {
        start: LIR.CFStmtId,
        cond: LIR.LocalId,
        branches: LIR.CFSwitchBranchSpan,
        branch_results: []LIR.CFStmtId,
        default_branch: LIR.CFStmtId = undefined,
        default_is_cold: bool,
        continuation: ?LIR.CFStmtId,
        frames: std.ArrayList(LinearRewriteFrame),
        result: *LIR.CFStmtId,
    };

    const RewriteSwitchContinuationTask = struct {
        start: LIR.CFStmtId,
        cond: LIR.LocalId,
        branches: LIR.CFSwitchBranchSpan,
        original_continuation: LIR.CFStmtId,
        continuation: LIR.CFStmtId = undefined,
        branch_results: []LIR.CFStmtId,
        default_branch: LIR.CFStmtId = undefined,
        default_is_cold: bool,
        entry_owned: OwnedSet,
        common: OwnedSet,
        parent_options: RewriteOptions,
        nested_boundaries: []RewriteBoundary = &.{},
        frames: std.ArrayList(LinearRewriteFrame),
        result: *LIR.CFStmtId,
    };

    const RewriteInitializedPayloadSwitchTask = struct {
        cond: LIR.LocalId,
        cond_mask: u64,
        payload: LIR.LocalId,
        uninitialized_is_cold: bool,
        initialized_branch: LIR.CFStmtId = undefined,
        uninitialized_branch: LIR.CFStmtId = undefined,
        frames: std.ArrayList(LinearRewriteFrame),
        result: *LIR.CFStmtId,
    };

    const RewriteStrMatchTask = struct {
        start: LIR.CFStmtId,
        source: LIR.LocalId,
        prefix: LIR.StrLiteral,
        steps: LIR.StrMatchStepSpan,
        end: LIR.StrPatternEnd,
        on_match: LIR.CFStmtId = undefined,
        on_miss: LIR.CFStmtId = undefined,
        frames: std.ArrayList(LinearRewriteFrame),
        result: *LIR.CFStmtId,
    };

    const RewriteStrMatchSetTask = struct {
        start: LIR.CFStmtId,
        source: LIR.LocalId,
        arms: LIR.StrMatchArmSpan,
        on_matches: []LIR.CFStmtId,
        on_miss: LIR.CFStmtId = undefined,
        frames: std.ArrayList(LinearRewriteFrame),
        result: *LIR.CFStmtId,
    };

    const AnalysisTask = union(enum) {
        path: *AnalysisPathTask,
        resume_switch_continuation: *AnalysisSwitchContinuationTask,
    };

    const AnalysisPathTask = struct {
        cursor: LIR.CFStmtId,
        stop: AnalysisStop,
        owned: OwnedSet,
        exits: *std.ArrayList(OwnedSet),
        loop_keep: ?*const OwnedSet,
        scoped_joins: ?*AnalysisScopedJoinMap,
        seen: ?*AnalysisSeen,
    };

    const AnalysisSwitchContinuationTask = struct {
        continuation: LIR.CFStmtId,
        stop: AnalysisStop,
        switch_exits: std.ArrayList(OwnedSet),
        parent_exits: *std.ArrayList(OwnedSet),
        loop_keep: ?*const OwnedSet,
        scoped_joins: ?*AnalysisScopedJoinMap,
        seen: ?*AnalysisSeen,
    };

    fn rewritePath(self: *Inserter, start: LIR.CFStmtId, owned: *OwnedSet, options: RewriteOptions) ResourceError!LIR.CFStmtId {
        var result: LIR.CFStmtId = undefined;
        var tasks = std.ArrayList(RewriteTask).empty;
        defer {
            while (tasks.pop()) |task| self.destroyRewriteTask(task);
            tasks.deinit(self.store.allocator);
        }

        try self.pushRewritePath(&tasks, start, owned, options, &result);
        while (tasks.pop()) |task| {
            switch (task) {
                .path => |path| try self.processRewritePath(&tasks, path),
                .join => |join| try self.finishRewriteJoin(join),
                .switch_no_continuation => |switch_| try self.finishRewriteSwitchNoContinuation(switch_),
                .switch_after_continuation => |switch_| try self.processRewriteSwitchAfterContinuation(&tasks, switch_),
                .switch_finish_continuation => |switch_| try self.finishRewriteSwitchContinuation(switch_),
                .initialized_payload_switch => |switch_| try self.finishRewriteInitializedPayloadSwitch(switch_),
                .str_match => |str_match| try self.finishRewriteStrMatch(str_match),
                .str_match_set => |str_match_set| try self.finishRewriteStrMatchSet(str_match_set),
            }
        }
        return result;
    }

    fn pushRewritePath(
        self: *Inserter,
        tasks: *std.ArrayList(RewriteTask),
        start: LIR.CFStmtId,
        owned: *const OwnedSet,
        options: RewriteOptions,
        result: *LIR.CFStmtId,
    ) ResourceError!void {
        const task = try self.store.allocator.create(RewritePathTask);
        errdefer self.store.allocator.destroy(task);
        task.* = .{
            .cursor = start,
            .owned = try owned.clone(),
            .options = options,
            .frames = .empty,
            .result = result,
        };
        errdefer task.owned.deinit();
        try tasks.append(self.store.allocator, .{ .path = task });
    }

    fn takeRewriteFrames(path: *RewritePathTask) std.ArrayList(LinearRewriteFrame) {
        const frames = path.frames;
        path.frames = .empty;
        return frames;
    }

    fn processRewritePath(self: *Inserter, tasks: *std.ArrayList(RewriteTask), path: *RewritePathTask) ResourceError!void {
        errdefer self.destroyRewritePath(path);

        while (true) {
            if (rewriteBoundaryForStop(path.options.boundaries, path.cursor)) |boundary| {
                const tail = try self.releaseDifference(&path.owned, boundary.keep, boundary.replacement);
                path.result.* = try self.finishLinearRewrite(&path.frames, tail);
                self.destroyRewritePath(path);
                return;
            }
            if (replacementReached(path.options.boundaries, path.cursor)) {
                path.result.* = try self.finishLinearRewrite(&path.frames, path.cursor);
                self.destroyRewritePath(path);
                return;
            }

            const stmt = self.store.getCFStmt(path.cursor);
            var current_start = path.cursor;
            switch (stmt) {
                .assign_ref => |assign| {
                    var retain_assign_ref_target = true;
                    const target_borrowed = self.isBindingBorrowed(assign.target);
                    if (target_borrowed) {
                        // Borrowed bindings carry no ownership unit and emit
                        // no RC statements; the lender's liveness group keeps
                        // the value alive across every use.
                        retain_assign_ref_target = false;
                    } else switch (assign.op) {
                        .local => |source| {
                            if (assign.target != source) {
                                const move_value = try self.canMoveSetLocalValue(&path.owned, source, assign.next, path.options.loop_keep);
                                current_start = try self.releaseOldTargetIfNeeded(assign.target, &path.owned, current_start);
                                if (move_value) {
                                    self.unsetOwnedUnit(&path.owned, source);
                                    retain_assign_ref_target = false;
                                }
                                self.addOwnedIfRc(&path.owned, assign.target);
                            } else {
                                retain_assign_ref_target = false;
                            }
                        },
                        else => {
                            current_start = try self.releaseOldTargetIfNeeded(assign.target, &path.owned, current_start);
                            self.addOwnedIfRc(&path.owned, assign.target);
                        },
                    }
                    var deaths: std.ArrayList(LIR.LocalId) = .empty;
                    errdefer deaths.deinit(self.store.allocator);
                    const singles = [_]LIR.LocalId{ refOpSource(assign.op), assign.target };
                    try self.postStmtDeaths(&path.owned, &singles, null, assign.next, path.options.loop_keep, &deaths);
                    try path.frames.append(self.store.allocator, .{
                        .stmt = path.cursor,
                        .head = current_start,
                        .retain_assign_ref_target = retain_assign_ref_target,
                        .post_release = try self.takePostReleases(&deaths),
                    });
                    path.cursor = assign.next;
                },
                .assign_literal => |assign| {
                    current_start = try self.releaseOldTargetIfNeeded(assign.target, &path.owned, current_start);
                    self.addOwnedIfRc(&path.owned, assign.target);
                    var deaths: std.ArrayList(LIR.LocalId) = .empty;
                    errdefer deaths.deinit(self.store.allocator);
                    const singles = [_]LIR.LocalId{assign.target};
                    try self.postStmtDeaths(&path.owned, &singles, null, assign.next, path.options.loop_keep, &deaths);
                    try path.frames.append(self.store.allocator, .{
                        .stmt = path.cursor,
                        .head = current_start,
                        .post_release = try self.takePostReleases(&deaths),
                    });
                    path.cursor = assign.next;
                },
                .init_uninitialized => |uninit| {
                    current_start = try self.releaseOldTargetIfNeeded(uninit.target, &path.owned, current_start);
                    try path.frames.append(self.store.allocator, .{ .stmt = path.cursor, .head = current_start });
                    path.cursor = uninit.next;
                },
                .assign_call => |assign| {
                    const callee_sig = self.solution.sigOf(assign.proc);
                    // Unique demands clone variants, so they exist only when
                    // specialization is on, and never for pinned callees,
                    // whose vectors are ABI contracts.
                    const unique_demand = self.variants.enabled and !self.solution.isPinnedProc(assign.proc);
                    var arg_ownership = try self.callArgOwnership(assign.proc, &path.owned, callee_sig, unique_demand, assign.args, assign.next, assign.target, path.options.loop_keep);
                    defer arg_ownership.deinit(self.store.allocator);
                    const call_target = try self.variantForCall(assign.proc, arg_ownership.demanded);
                    if (!self.spanUsesLocal(assign.args, assign.target)) {
                        current_start = try self.releaseOldTargetIfNeeded(assign.target, &path.owned, current_start);
                    }
                    self.unsetArgs(&path.owned, arg_ownership.transfer_args.items);
                    self.addCallResultOwnedIfRc(&path.owned, assign.target, arg_ownership.demanded.ret_mode);
                    current_start = try self.retainArgs(arg_ownership.retain_args.items, current_start);
                    // A borrowed-return result that must be owned here pays
                    // one retain right after the call.
                    const retain_call_result = arg_ownership.demanded.ret_mode == .borrowed and
                        self.localContainsRefcounted(assign.target) and
                        !self.isBindingBorrowed(assign.target);
                    var deaths: std.ArrayList(LIR.LocalId) = .empty;
                    errdefer deaths.deinit(self.store.allocator);
                    try self.noteCallResultDeathIfUnused(&path.owned, assign.target, arg_ownership.demanded.ret_mode, assign.next, path.options.loop_keep, &deaths);
                    try self.postStmtDeaths(&path.owned, &.{}, assign.args, assign.next, path.options.loop_keep, &deaths);
                    try path.frames.append(self.store.allocator, .{
                        .stmt = path.cursor,
                        .head = current_start,
                        .retain_call_result = retain_call_result,
                        .call_target_override = call_target,
                        .post_release = try self.takePostReleases(&deaths),
                    });
                    path.cursor = assign.next;
                },
                .assign_call_erased => |assign| {
                    var arg_ownership = try self.callArgOwnership(null, &path.owned, arc_sig.RcSig.all_owned, false, assign.args, assign.next, assign.target, path.options.loop_keep);
                    defer arg_ownership.deinit(self.store.allocator);
                    if (!self.spanUsesLocal(assign.args, assign.target) and assign.closure != assign.target) {
                        current_start = try self.releaseOldTargetIfNeeded(assign.target, &path.owned, current_start);
                    }
                    self.unsetArgs(&path.owned, arg_ownership.transfer_args.items);
                    self.addCallResultOwnedIfRc(&path.owned, assign.target, .owned);
                    current_start = try self.retainLocalIfRc(assign.closure, current_start);
                    current_start = try self.retainArgs(arg_ownership.retain_args.items, current_start);
                    var deaths: std.ArrayList(LIR.LocalId) = .empty;
                    errdefer deaths.deinit(self.store.allocator);
                    try self.noteCallResultDeathIfUnused(&path.owned, assign.target, .owned, assign.next, path.options.loop_keep, &deaths);
                    const singles = [_]LIR.LocalId{assign.closure};
                    try self.postStmtDeaths(&path.owned, &singles, assign.args, assign.next, path.options.loop_keep, &deaths);
                    try path.frames.append(self.store.allocator, .{
                        .stmt = path.cursor,
                        .head = current_start,
                        .post_release = try self.takePostReleases(&deaths),
                    });
                    path.cursor = assign.next;
                },
                .assign_packed_erased_fn => |assign| {
                    var transfer_single = false;
                    if (assign.capture) |capture| {
                        transfer_single = try self.singleTransfer(capture, assign.next, assign.target, &path.owned, path.options.loop_keep);
                    }
                    current_start = try self.releaseOldTargetIfNeeded(assign.target, &path.owned, current_start);
                    self.addOwnedIfRc(&path.owned, assign.target);
                    var deaths: std.ArrayList(LIR.LocalId) = .empty;
                    errdefer deaths.deinit(self.store.allocator);
                    const singles = [_]LIR.LocalId{ assign.capture orelse assign.target, assign.target };
                    try self.postStmtDeaths(&path.owned, &singles, null, assign.next, path.options.loop_keep, &deaths);
                    try path.frames.append(self.store.allocator, .{
                        .stmt = path.cursor,
                        .head = current_start,
                        .transfer_single = transfer_single,
                        .post_release = try self.takePostReleases(&deaths),
                    });
                    path.cursor = assign.next;
                },
                .assign_low_level => |assign| {
                    if ((assign.rc_effect.result_aliases_consumed_args & ~assign.rc_effect.consume_args) != 0) {
                        arcInvariant("ARC low-level result-token metadata referenced a non-consumed argument");
                    }
                    const preserve_consumed_args = try self.preserveConsumedArgMask(
                        assign.args,
                        assign.rc_effect.consume_args,
                        assign.next,
                        assign.target,
                        path.options.loop_keep,
                    );
                    const unique_args = self.uniqueArgsMask(
                        assign.args,
                        assign.rc_effect,
                        assign.target,
                        preserve_consumed_args,
                        &path.owned,
                    );
                    const target_consumed = self.maskedArgsContainLocal(assign.args, assign.rc_effect.consume_args, assign.target);
                    if (target_consumed) {
                        self.unsetOwnedUnit(&path.owned, assign.target);
                    } else {
                        current_start = try self.releaseOldTargetIfNeeded(assign.target, &path.owned, current_start);
                    }
                    if (assign.rc_effect.consume_args != 0) {
                        self.unsetMaskedArgsExcept(&path.owned, assign.args, assign.rc_effect.consume_args & ~preserve_consumed_args, assign.target);
                    }
                    // Retained positions whose group dies here move their
                    // unit into the result instead of paying a retain.
                    var transfer_mask: u64 = 0;
                    if (assign.rc_effect.retain_args != 0) {
                        transfer_mask = try self.spanTransferMask(assign.args, assign.rc_effect.retain_args, assign.next, assign.target, &path.owned, path.options.loop_keep);
                    }
                    self.addOwnedIfRc(&path.owned, assign.target);
                    if (assign.rc_effect.consume_args != 0) {
                        current_start = try self.retainMaskedArgs(assign.args, preserve_consumed_args, current_start);
                    }
                    var deaths: std.ArrayList(LIR.LocalId) = .empty;
                    errdefer deaths.deinit(self.store.allocator);
                    const singles = [_]LIR.LocalId{assign.target};
                    try self.postStmtDeaths(&path.owned, &singles, assign.args, assign.next, path.options.loop_keep, &deaths);
                    try path.frames.append(self.store.allocator, .{
                        .stmt = path.cursor,
                        .head = current_start,
                        .transfer_mask = transfer_mask,
                        .skip_result_retain = self.isBindingBorrowed(assign.target),
                        .unique_args = unique_args,
                        .post_release = try self.takePostReleases(&deaths),
                    });
                    path.cursor = assign.next;
                },
                .assign_list => |assign| {
                    const transfer_mask = try self.spanTransferMask(assign.elems, ~@as(u64, 0), assign.next, assign.target, &path.owned, path.options.loop_keep);
                    current_start = try self.releaseOldTargetIfNeeded(assign.target, &path.owned, current_start);
                    self.addOwnedIfRc(&path.owned, assign.target);
                    var deaths: std.ArrayList(LIR.LocalId) = .empty;
                    errdefer deaths.deinit(self.store.allocator);
                    const singles = [_]LIR.LocalId{assign.target};
                    try self.postStmtDeaths(&path.owned, &singles, assign.elems, assign.next, path.options.loop_keep, &deaths);
                    try path.frames.append(self.store.allocator, .{
                        .stmt = path.cursor,
                        .head = current_start,
                        .transfer_mask = transfer_mask,
                        .post_release = try self.takePostReleases(&deaths),
                    });
                    path.cursor = assign.next;
                },
                .assign_struct => |assign| {
                    const transfer_mask = try self.spanTransferMask(assign.fields, ~@as(u64, 0), assign.next, assign.target, &path.owned, path.options.loop_keep);
                    current_start = try self.releaseOldTargetIfNeeded(assign.target, &path.owned, current_start);
                    self.addOwnedIfRc(&path.owned, assign.target);
                    var deaths: std.ArrayList(LIR.LocalId) = .empty;
                    errdefer deaths.deinit(self.store.allocator);
                    const singles = [_]LIR.LocalId{assign.target};
                    try self.postStmtDeaths(&path.owned, &singles, assign.fields, assign.next, path.options.loop_keep, &deaths);
                    try path.frames.append(self.store.allocator, .{
                        .stmt = path.cursor,
                        .head = current_start,
                        .transfer_mask = transfer_mask,
                        .post_release = try self.takePostReleases(&deaths),
                    });
                    path.cursor = assign.next;
                },
                .assign_tag => |assign| {
                    var transfer_single = false;
                    if (assign.payload) |payload| {
                        transfer_single = try self.singleTransfer(payload, assign.next, assign.target, &path.owned, path.options.loop_keep);
                    }
                    current_start = try self.releaseOldTargetIfNeeded(assign.target, &path.owned, current_start);
                    self.addOwnedIfRc(&path.owned, assign.target);
                    var deaths: std.ArrayList(LIR.LocalId) = .empty;
                    errdefer deaths.deinit(self.store.allocator);
                    const singles = [_]LIR.LocalId{ assign.payload orelse assign.target, assign.target };
                    try self.postStmtDeaths(&path.owned, &singles, null, assign.next, path.options.loop_keep, &deaths);
                    try path.frames.append(self.store.allocator, .{
                        .stmt = path.cursor,
                        .head = current_start,
                        .transfer_single = transfer_single,
                        .post_release = try self.takePostReleases(&deaths),
                    });
                    path.cursor = assign.next;
                },
                .set_local => |assign| {
                    var retain_set_target = assign.target != assign.value;
                    if (assign.target != assign.value) {
                        const move_value = try self.canMoveSetLocalValue(&path.owned, assign.value, assign.next, path.options.loop_keep);
                        switch (assign.mode) {
                            .replace_existing, .initialize_join_param => current_start = try self.releaseOldTargetIfNeeded(assign.target, &path.owned, current_start),
                            .initialize_join_result => {},
                        }
                        if (move_value) {
                            self.unsetOwnedUnit(&path.owned, assign.value);
                            retain_set_target = false;
                        }
                        self.addOwnedIfRc(&path.owned, assign.target);
                    }
                    var deaths: std.ArrayList(LIR.LocalId) = .empty;
                    errdefer deaths.deinit(self.store.allocator);
                    const singles = [_]LIR.LocalId{ assign.value, assign.target };
                    try self.postStmtDeaths(&path.owned, &singles, null, assign.next, path.options.loop_keep, &deaths);
                    try path.frames.append(self.store.allocator, .{
                        .stmt = path.cursor,
                        .head = current_start,
                        .retain_set_target = retain_set_target,
                        .post_release = try self.takePostReleases(&deaths),
                    });
                    path.cursor = assign.next;
                },
                .debug => |debug_stmt| {
                    var deaths: std.ArrayList(LIR.LocalId) = .empty;
                    errdefer deaths.deinit(self.store.allocator);
                    const singles = [_]LIR.LocalId{debug_stmt.message};
                    try self.postStmtDeaths(&path.owned, &singles, null, debug_stmt.next, path.options.loop_keep, &deaths);
                    try path.frames.append(self.store.allocator, .{
                        .stmt = path.cursor,
                        .head = current_start,
                        .post_release = try self.takePostReleases(&deaths),
                    });
                    path.cursor = debug_stmt.next;
                },
                .expect => |expect_stmt| {
                    var deaths: std.ArrayList(LIR.LocalId) = .empty;
                    errdefer deaths.deinit(self.store.allocator);
                    const singles = [_]LIR.LocalId{expect_stmt.condition};
                    try self.postStmtDeaths(&path.owned, &singles, null, expect_stmt.next, path.options.loop_keep, &deaths);
                    try path.frames.append(self.store.allocator, .{
                        .stmt = path.cursor,
                        .head = current_start,
                        .post_release = try self.takePostReleases(&deaths),
                    });
                    path.cursor = expect_stmt.next;
                },
                .incref => |rc| {
                    if (path.options.boundaries.len == 0) arcInvariant("ARC insertion received already-reference-counted LIR");
                    self.addOwnedIfRc(&path.owned, rc.value);
                    try path.frames.append(self.store.allocator, .{ .stmt = path.cursor, .head = current_start });
                    path.cursor = rc.next;
                },
                .decref => |rc| {
                    if (path.options.boundaries.len == 0) arcInvariant("ARC insertion received already-reference-counted LIR");
                    path.owned.unset(rc.value);
                    try path.frames.append(self.store.allocator, .{ .stmt = path.cursor, .head = current_start });
                    path.cursor = rc.next;
                },
                .decref_if_initialized => |rc| {
                    if (path.options.boundaries.len == 0) arcInvariant("ARC insertion received already-reference-counted LIR");
                    path.owned.unset(rc.value);
                    try path.frames.append(self.store.allocator, .{ .stmt = path.cursor, .head = current_start });
                    path.cursor = rc.next;
                },
                .free => |rc| {
                    if (path.options.boundaries.len == 0) arcInvariant("ARC insertion received already-reference-counted LIR");
                    path.owned.unset(rc.value);
                    try path.frames.append(self.store.allocator, .{ .stmt = path.cursor, .head = current_start });
                    path.cursor = rc.next;
                },
                .switch_stmt => |switch_stmt| {
                    try self.scheduleRewriteSwitch(tasks, path, path.cursor, switch_stmt);
                    self.destroyRewritePath(path);
                    return;
                },
                .switch_initialized_payload => |switch_stmt| {
                    try self.scheduleRewriteInitializedPayloadSwitch(tasks, path, switch_stmt);
                    self.destroyRewritePath(path);
                    return;
                },
                .str_match => |str_match| {
                    try self.scheduleRewriteStrMatch(tasks, path, path.cursor, str_match);
                    self.destroyRewritePath(path);
                    return;
                },
                .str_match_set => |str_match_set| {
                    try self.scheduleRewriteStrMatchSet(tasks, path, path.cursor, str_match_set);
                    self.destroyRewritePath(path);
                    return;
                },
                .join => |join_stmt| {
                    try self.scheduleRewriteJoin(tasks, path, path.cursor, join_stmt);
                    self.destroyRewritePath(path);
                    return;
                },
                .runtime_error, .comptime_exhaustiveness_failed => {
                    const tail = try self.releaseAll(&path.owned, path.cursor);
                    path.result.* = try self.finishLinearRewrite(&path.frames, tail);
                    self.destroyRewritePath(path);
                    return;
                },
                .comptime_branch_taken => |marker| {
                    try path.frames.append(self.store.allocator, .{ .stmt = path.cursor, .head = current_start });
                    path.cursor = marker.next;
                },
                .loop_continue => {
                    const tail = if (path.options.loop_keep) |keep| try self.releaseDifference(&path.owned, keep, path.cursor) else path.cursor;
                    path.result.* = try self.finishLinearRewrite(&path.frames, tail);
                    self.destroyRewritePath(path);
                    return;
                },
                .loop_break => {
                    const tail = if (path.options.loop_keep) |keep| try self.releaseDifference(&path.owned, keep, path.cursor) else path.cursor;
                    path.result.* = try self.finishLinearRewrite(&path.frames, tail);
                    self.destroyRewritePath(path);
                    return;
                },
                .jump => |jump_stmt| {
                    const keep = keepForJoin(path.options.join_keeps, jump_stmt.target) orelse
                        arcInvariant("ARC jump reached a join without an active ownership target");
                    const jump = try self.store.addCFStmt(.{ .jump = .{ .target = jump_stmt.target } });
                    const tail = try self.releaseDifference(&path.owned, keep, jump);
                    path.result.* = try self.finishLinearRewrite(&path.frames, tail);
                    self.destroyRewritePath(path);
                    return;
                },
                .ret => |ret_stmt| {
                    var tail = path.cursor;
                    if (self.current_sig.ret_mode == .borrowed) {
                        // The caller borrows the result from its own
                        // arguments; no unit transfers.
                        tail = try self.releaseAll(&path.owned, tail);
                    } else if (self.ownsUnit(&path.owned, ret_stmt.value)) {
                        // Move on return: the binding's own unit transfers to
                        // the caller; no retain/release pair is needed.
                        self.unsetOwnedUnit(&path.owned, ret_stmt.value);
                        tail = try self.releaseAll(&path.owned, tail);
                    } else {
                        tail = try self.releaseAll(&path.owned, tail);
                        tail = try self.retainLocalIfRc(ret_stmt.value, tail);
                    }
                    path.result.* = try self.finishLinearRewrite(&path.frames, tail);
                    self.destroyRewritePath(path);
                    return;
                },
                .crash => {
                    const tail = try self.releaseAll(&path.owned, path.cursor);
                    path.result.* = try self.finishLinearRewrite(&path.frames, tail);
                    self.destroyRewritePath(path);
                    return;
                },
                .expect_err => |expect_err_stmt| {
                    // Terminal: the message's ownership unit transfers to the
                    // failure report, so it is not released here.
                    var tail = path.cursor;
                    if (self.ownsUnit(&path.owned, expect_err_stmt.message)) {
                        self.unsetOwnedUnit(&path.owned, expect_err_stmt.message);
                        tail = try self.releaseAll(&path.owned, tail);
                    } else {
                        tail = try self.releaseAll(&path.owned, tail);
                        tail = try self.retainLocalIfRc(expect_err_stmt.message, tail);
                    }
                    path.result.* = try self.finishLinearRewrite(&path.frames, tail);
                    self.destroyRewritePath(path);
                    return;
                },
            }
        }
    }

    fn finishLinearRewrite(
        self: *Inserter,
        frames: *std.ArrayList(LinearRewriteFrame),
        tail_start: LIR.CFStmtId,
    ) ResourceError!LIR.CFStmtId {
        var next = tail_start;
        while (frames.pop()) |frame| {
            next = try self.patchLinearFrame(frame, next);
        }
        return next;
    }

    fn patchLinearFrame(
        self: *Inserter,
        frame: LinearRewriteFrame,
        tail_start: LIR.CFStmtId,
    ) ResourceError!LIR.CFStmtId {
        const stmt = self.store.getCFStmt(frame.stmt);
        const saved_loc = self.store.current_loc;
        defer self.store.current_loc = saved_loc;
        const saved_region = self.store.current_region;
        defer self.store.current_region = saved_region;
        self.store.current_loc = self.store.stmtLoc(frame.stmt);
        self.store.current_region = self.store.stmtRegion(frame.stmt);
        var next = tail_start;
        var cloned: LIR.CFStmtId = undefined;

        // Lifetime-ending releases come right after the statement and its
        // own retains.
        {
            var i = frame.post_release.len;
            while (i > 0) {
                i -= 1;
                next = try self.releaseLocalIfRc(frame.post_release[i], next);
            }
            if (frame.post_release.len != 0) {
                self.store.allocator.free(frame.post_release);
            }
        }
        switch (stmt) {
            .assign_ref => |assign| {
                if (frame.retain_assign_ref_target) {
                    next = try self.retainLocalIfRc(assign.target, next);
                }
                cloned = try self.store.addCFStmt(.{ .assign_ref = .{
                    .target = assign.target,
                    .op = assign.op,
                    .next = next,
                } });
            },
            .assign_literal => |assign| {
                cloned = try self.store.addCFStmt(.{ .assign_literal = .{
                    .target = assign.target,
                    .value = assign.value,
                    .next = next,
                } });
            },
            .init_uninitialized => |uninit| {
                cloned = try self.store.addCFStmt(.{ .init_uninitialized = .{
                    .target = uninit.target,
                    .next = next,
                } });
            },
            .assign_call => |assign| {
                if (frame.retain_call_result) {
                    next = try self.retainLocalIfRc(assign.target, next);
                }
                cloned = try self.store.addCFStmt(.{ .assign_call = .{
                    .target = assign.target,
                    .proc = frame.call_target_override orelse assign.proc,
                    .args = assign.args,
                    .is_cold = assign.is_cold,
                    .next = next,
                } });
            },
            .assign_call_erased => |assign| {
                next = try self.releaseLocalIfRc(assign.closure, next);
                cloned = try self.store.addCFStmt(.{ .assign_call_erased = .{
                    .target = assign.target,
                    .closure = assign.closure,
                    .args = assign.args,
                    .next = next,
                } });
            },
            .assign_packed_erased_fn => |assign| {
                if (assign.capture) |capture| {
                    if (!frame.transfer_single) {
                        next = try self.retainLocalIfRc(capture, next);
                    }
                }
                cloned = try self.store.addCFStmt(.{ .assign_packed_erased_fn = .{
                    .target = assign.target,
                    .proc = assign.proc,
                    .capture = assign.capture,
                    .capture_layout = assign.capture_layout,
                    .on_drop = assign.on_drop,
                    .next = next,
                } });
            },
            .assign_low_level => |assign| {
                if (assign.rc_effect.retain_args != 0) {
                    next = try self.retainMaskedArgs(assign.args, assign.rc_effect.retain_args & ~frame.transfer_mask, next);
                }
                if (assign.rc_effect.retain_result and !frame.skip_result_retain) {
                    next = try self.retainLocalIfRc(assign.target, next);
                }
                cloned = try self.store.addCFStmt(.{ .assign_low_level = .{
                    .target = assign.target,
                    .op = assign.op,
                    .rc_effect = assign.rc_effect,
                    .unique_args = frame.unique_args,
                    .args = assign.args,
                    .interchangeable = assign.interchangeable,
                    .next = next,
                } });
            },
            .assign_list => |assign| {
                next = try self.retainSpanExcept(assign.elems, frame.transfer_mask, next);
                cloned = try self.store.addCFStmt(.{ .assign_list = .{
                    .target = assign.target,
                    .elems = assign.elems,
                    .next = next,
                } });
            },
            .assign_struct => |assign| {
                next = try self.retainSpanExcept(assign.fields, frame.transfer_mask, next);
                cloned = try self.store.addCFStmt(.{ .assign_struct = .{
                    .target = assign.target,
                    .fields = assign.fields,
                    .next = next,
                } });
            },
            .assign_tag => |assign| {
                if (assign.payload) |payload| {
                    if (!frame.transfer_single) {
                        next = try self.retainLocalIfRc(payload, next);
                    }
                }
                cloned = try self.store.addCFStmt(.{ .assign_tag = .{
                    .target = assign.target,
                    .variant_index = assign.variant_index,
                    .discriminant = assign.discriminant,
                    .payload = assign.payload,
                    .next = next,
                } });
            },
            .set_local => |assign| {
                if (assign.target != assign.value and frame.retain_set_target) {
                    next = try self.retainLocalIfRc(assign.target, next);
                }
                cloned = try self.store.addCFStmt(.{ .set_local = .{
                    .target = assign.target,
                    .value = assign.value,
                    .mode = assign.mode,
                    .next = next,
                } });
            },
            .debug => |debug_stmt| {
                cloned = try self.store.addCFStmt(.{ .debug = .{
                    .message = debug_stmt.message,
                    .next = next,
                } });
            },
            .expect => |expect_stmt| {
                cloned = try self.store.addCFStmt(.{ .expect = .{
                    .condition = expect_stmt.condition,
                    .next = next,
                } });
            },
            .switch_initialized_payload => arcInvariant("ARC linear rewrite frame contained initialized-payload switch"),
            .comptime_branch_taken => |marker| {
                cloned = try self.store.addCFStmt(.{ .comptime_branch_taken = .{
                    .site = marker.site,
                    .branch_index = marker.branch_index,
                    .next = next,
                } });
            },
            .incref => |rc| {
                cloned = try self.store.addCFStmt(.{ .incref = .{
                    .value = rc.value,
                    .rc = rc.rc,
                    .count = rc.count,
                    .atomicity = self.rcAtomicity(rc.value),
                    .next = next,
                } });
            },
            .decref => |rc| {
                cloned = try self.store.addCFStmt(.{ .decref = .{
                    .value = rc.value,
                    .rc = rc.rc,
                    .atomicity = self.rcAtomicity(rc.value),
                    .next = next,
                } });
            },
            .decref_if_initialized => |rc| {
                cloned = try self.store.addCFStmt(.{ .decref_if_initialized = .{
                    .cond = rc.cond,
                    .cond_mask = rc.cond_mask,
                    .value = rc.value,
                    .rc = rc.rc,
                    .atomicity = self.rcAtomicity(rc.value),
                    .next = next,
                } });
            },
            .free => |rc| {
                cloned = try self.store.addCFStmt(.{ .free = .{
                    .value = rc.value,
                    .rc = rc.rc,
                    .atomicity = self.rcAtomicity(rc.value),
                    .next = next,
                } });
            },
            .runtime_error,
            .comptime_exhaustiveness_failed,
            .switch_stmt,
            .str_match,
            .str_match_set,
            .loop_continue,
            .loop_break,
            .join,
            .jump,
            .ret,
            .crash,
            .expect_err,
            => arcInvariant("ARC linear rewrite attempted to patch a non-linear statement"),
        }
        return self.attachPrefix(frame.head, frame.stmt, cloned);
    }

    fn attachPrefix(self: *Inserter, head: LIR.CFStmtId, old_tail: LIR.CFStmtId, new_tail: LIR.CFStmtId) LIR.CFStmtId {
        if (head == old_tail) return new_tail;

        var cursor = head;
        while (cursor != old_tail) {
            const stmt = self.store.getCFStmtPtr(cursor);
            switch (stmt.*) {
                .incref => |*rc| {
                    if (rc.next == old_tail) {
                        rc.next = new_tail;
                        return head;
                    }
                    cursor = rc.next;
                },
                .decref => |*rc| {
                    if (rc.next == old_tail) {
                        rc.next = new_tail;
                        return head;
                    }
                    cursor = rc.next;
                },
                .decref_if_initialized => |*rc| {
                    if (rc.next == old_tail) {
                        rc.next = new_tail;
                        return head;
                    }
                    cursor = rc.next;
                },
                .free => |*rc| {
                    if (rc.next == old_tail) {
                        rc.next = new_tail;
                        return head;
                    }
                    cursor = rc.next;
                },
                .switch_initialized_payload => |*switch_stmt| {
                    switch_stmt.initialized_branch = self.attachPrefix(switch_stmt.initialized_branch, old_tail, new_tail);
                    switch_stmt.uninitialized_branch = self.attachPrefix(switch_stmt.uninitialized_branch, old_tail, new_tail);
                    return head;
                },
                else => arcInvariant("ARC linear rewrite prefix contained a non-RC statement"),
            }
        }

        arcInvariant("ARC linear rewrite prefix did not reach the cloned statement");
    }

    fn scheduleRewriteJoin(
        self: *Inserter,
        tasks: *std.ArrayList(RewriteTask),
        path: *RewritePathTask,
        start: LIR.CFStmtId,
        join_stmt: anytype,
    ) ResourceError!void {
        if (self.rewritten_joins) |rewritten_joins| {
            if (rewritten_joins.get(start)) |rewritten| {
                const tail = try self.releaseDifference(&path.owned, &rewritten.keep, rewritten.stmt);
                path.result.* = try self.finishLinearRewrite(&path.frames, tail);
                return;
            }
        }

        const state = try self.store.allocator.create(RewriteJoinTask);
        var queued = false;
        errdefer if (!queued) self.store.allocator.destroy(state);
        var body_reachable = false;
        state.* = .{
            .start = start,
            .id = join_stmt.id,
            .params = join_stmt.params,
            .maybe_uninitialized_params = join_stmt.maybe_uninitialized_params,
            .maybe_uninitialized_conditions = join_stmt.maybe_uninitialized_conditions,
            .maybe_uninitialized_condition_masks = join_stmt.maybe_uninitialized_condition_masks,
            .incoming_owned = try path.owned.clone(),
            .entry_keep = try self.joinEntryOwnedSet(&path.owned, join_stmt.remainder),
            .body_keep = try self.joinBodyOwnedSet(
                &path.owned,
                join_stmt.id,
                join_stmt.params,
                join_stmt.maybe_uninitialized_params,
                join_stmt.remainder,
                join_stmt.body,
                &body_reachable,
            ),
            .body_reachable = body_reachable,
            .loop_keep_id = self.next_loop_keep_id,
            .frames = takeRewriteFrames(path),
            .result = path.result,
        };
        self.next_loop_keep_id += 1;
        var carried_body_keep = try state.body_keep.clone();
        defer carried_body_keep.deinit();
        carried_body_keep.intersect(&state.incoming_owned);
        state.entry_keep.unionWith(&carried_body_keep);
        errdefer if (!queued) state.incoming_owned.deinit();
        errdefer if (!queued) state.entry_keep.deinit();
        errdefer if (!queued) state.body_keep.deinit();
        errdefer if (!queued) state.frames.deinit(self.store.allocator);
        var loop_keep_registered = false;
        errdefer if (!queued and loop_keep_registered) {
            _ = self.active_loop_keep_ids.remove(@intFromPtr(&state.body_keep));
        };
        try self.active_loop_keep_ids.put(@intFromPtr(&state.body_keep), state.loop_keep_id);
        loop_keep_registered = true;
        state.join_keeps = try appendJoinKeep(self.store.allocator, path.options.join_keeps, .{
            .target = join_stmt.id,
            .keep = &state.body_keep,
        });
        errdefer if (!queued) self.store.allocator.free(state.join_keeps);

        try tasks.append(self.store.allocator, .{ .join = state });
        queued = true;
        const join_options = RewriteOptions{
            .boundaries = path.options.boundaries,
            .loop_keep = &state.body_keep,
            .loop_keep_id = state.loop_keep_id,
            .join_keeps = state.join_keeps,
        };
        try self.pushRewritePath(tasks, join_stmt.remainder, &state.entry_keep, .{
            .boundaries = join_options.boundaries,
            .loop_keep = join_options.loop_keep,
            .loop_keep_id = join_options.loop_keep_id,
            .join_keeps = join_options.join_keeps,
        }, &state.remainder);
        if (state.body_reachable) {
            try self.pushRewritePath(tasks, join_stmt.body, &state.body_keep, join_options, &state.body);
        }
    }

    fn finishRewriteJoin(self: *Inserter, state: *RewriteJoinTask) ResourceError!void {
        errdefer self.destroyRewriteJoin(state);
        if (!state.body_reachable) {
            const tail = try self.releaseDifference(&state.incoming_owned, &state.entry_keep, state.remainder);
            state.result.* = try self.finishLinearRewrite(&state.frames, tail);
            self.destroyRewriteJoin(state);
            return;
        }
        const join = try self.store.addCFStmt(.{ .join = .{
            .id = state.id,
            .params = state.params,
            .maybe_uninitialized_params = state.maybe_uninitialized_params,
            .maybe_uninitialized_conditions = state.maybe_uninitialized_conditions,
            .maybe_uninitialized_condition_masks = state.maybe_uninitialized_condition_masks,
            .body = state.body,
            .remainder = state.remainder,
        } });
        if (self.rewritten_joins) |rewritten_joins| {
            try rewritten_joins.put(state.start, .{
                .keep = try state.entry_keep.clone(),
                .stmt = join,
            });
        }
        const tail = try self.releaseDifference(&state.incoming_owned, &state.entry_keep, join);
        state.result.* = try self.finishLinearRewrite(&state.frames, tail);
        self.destroyRewriteJoin(state);
    }

    fn scheduleRewriteSwitch(
        self: *Inserter,
        tasks: *std.ArrayList(RewriteTask),
        path: *RewritePathTask,
        start: LIR.CFStmtId,
        switch_stmt: anytype,
    ) ResourceError!void {
        if (switch_stmt.continuation) |continuation| {
            var exit_states = std.ArrayList(OwnedSet).empty;
            defer {
                for (exit_states.items) |*state| state.deinit();
                exit_states.deinit(self.store.allocator);
            }

            for (self.store.getCFSwitchBranches(switch_stmt.branches)) |branch| {
                try self.analyzeUntil(branch.body, &path.owned, continuation, &exit_states, path.options.loop_keep);
            }
            try self.analyzeUntil(switch_stmt.default_branch, &path.owned, continuation, &exit_states, path.options.loop_keep);

            if (exit_states.items.len == 0) {
                try self.scheduleRewriteSwitchNoContinuation(tasks, path, start, switch_stmt.cond, switch_stmt.branches, switch_stmt.default_branch, switch_stmt.default_is_cold, switch_stmt.continuation);
                return;
            }

            var common = try exit_states.items[0].clone();
            defer common.deinit();
            for (exit_states.items[1..]) |*state| common.intersect(state);
            try self.scheduleRewriteSwitchContinuation(tasks, path, start, switch_stmt.cond, switch_stmt.branches, switch_stmt.default_is_cold, continuation, &common);
            return;
        }

        try self.scheduleRewriteSwitchNoContinuation(tasks, path, start, switch_stmt.cond, switch_stmt.branches, switch_stmt.default_branch, switch_stmt.default_is_cold, null);
    }

    fn scheduleRewriteInitializedPayloadSwitch(
        self: *Inserter,
        tasks: *std.ArrayList(RewriteTask),
        path: *RewritePathTask,
        switch_stmt: anytype,
    ) ResourceError!void {
        const state = try self.store.allocator.create(RewriteInitializedPayloadSwitchTask);
        var queued = false;
        errdefer if (!queued) self.store.allocator.destroy(state);
        state.* = .{
            .cond = switch_stmt.cond,
            .cond_mask = switch_stmt.cond_mask,
            .payload = switch_stmt.payload,
            .uninitialized_is_cold = switch_stmt.uninitialized_is_cold,
            .frames = takeRewriteFrames(path),
            .result = path.result,
        };
        errdefer if (!queued) state.frames.deinit(self.store.allocator);

        try tasks.append(self.store.allocator, .{ .initialized_payload_switch = state });
        queued = true;

        var initialized_owned = try path.owned.clone();
        defer initialized_owned.deinit();
        self.addOwnedIfRc(&initialized_owned, switch_stmt.payload);

        var uninitialized_owned = try path.owned.clone();
        defer uninitialized_owned.deinit();
        uninitialized_owned.unset(switch_stmt.payload);

        try self.pushRewritePath(tasks, switch_stmt.initialized_branch, &initialized_owned, path.options, &state.initialized_branch);
        try self.pushRewritePath(tasks, switch_stmt.uninitialized_branch, &uninitialized_owned, path.options, &state.uninitialized_branch);
    }

    fn scheduleRewriteSwitchNoContinuation(
        self: *Inserter,
        tasks: *std.ArrayList(RewriteTask),
        path: *RewritePathTask,
        start: LIR.CFStmtId,
        cond: LIR.LocalId,
        branches_span: LIR.CFSwitchBranchSpan,
        default_branch: LIR.CFStmtId,
        default_is_cold: bool,
        continuation: ?LIR.CFStmtId,
    ) ResourceError!void {
        const branches = self.store.getCFSwitchBranches(branches_span);
        const state = try self.store.allocator.create(RewriteSwitchNoContinuationTask);
        var queued = false;
        errdefer if (!queued) self.store.allocator.destroy(state);
        const branch_results = try self.store.allocator.alloc(LIR.CFStmtId, branches.len);
        errdefer if (!queued) self.store.allocator.free(branch_results);
        state.* = .{
            .start = start,
            .cond = cond,
            .branches = branches_span,
            .branch_results = branch_results,
            .default_is_cold = default_is_cold,
            .continuation = continuation,
            .frames = takeRewriteFrames(path),
            .result = path.result,
        };
        errdefer if (!queued) state.frames.deinit(self.store.allocator);

        try tasks.append(self.store.allocator, .{ .switch_no_continuation = state });
        queued = true;
        try self.pushRewritePath(tasks, default_branch, &path.owned, path.options, &state.default_branch);
        for (branches, 0..) |branch, index| {
            try self.pushRewritePath(tasks, branch.body, &path.owned, path.options, &state.branch_results[index]);
        }
    }

    fn finishRewriteSwitchNoContinuation(self: *Inserter, state: *RewriteSwitchNoContinuationTask) ResourceError!void {
        errdefer self.destroyRewriteSwitchNoContinuation(state);
        const branches = self.store.getCFSwitchBranches(state.branches);
        if (branches.len != state.branch_results.len) arcInvariant("ARC switch branch result count changed during rewrite");
        const rewritten_branches = try self.store.allocator.alloc(LIR.CFSwitchBranch, branches.len);
        defer self.store.allocator.free(rewritten_branches);
        for (branches, state.branch_results, 0..) |branch, rewritten, i| {
            rewritten_branches[i] = .{
                .value = branch.value,
                .body = rewritten,
            };
        }
        const switch_stmt = try self.store.addCFStmt(.{ .switch_stmt = .{
            .cond = state.cond,
            .branches = try self.store.addCFSwitchBranches(rewritten_branches),
            .default_branch = state.default_branch,
            .default_is_cold = state.default_is_cold,
            .continuation = state.continuation,
        } });
        state.result.* = try self.finishLinearRewrite(&state.frames, switch_stmt);
        self.destroyRewriteSwitchNoContinuation(state);
    }

    fn finishRewriteInitializedPayloadSwitch(self: *Inserter, state: *RewriteInitializedPayloadSwitchTask) ResourceError!void {
        errdefer self.destroyRewriteInitializedPayloadSwitch(state);
        const switch_stmt = try self.store.addCFStmt(.{ .switch_initialized_payload = .{
            .cond = state.cond,
            .cond_mask = state.cond_mask,
            .payload = state.payload,
            .uninitialized_is_cold = state.uninitialized_is_cold,
            .initialized_branch = state.initialized_branch,
            .uninitialized_branch = state.uninitialized_branch,
        } });
        state.result.* = try self.finishLinearRewrite(&state.frames, switch_stmt);
        self.destroyRewriteInitializedPayloadSwitch(state);
    }

    fn scheduleRewriteSwitchContinuation(
        self: *Inserter,
        tasks: *std.ArrayList(RewriteTask),
        path: *RewritePathTask,
        start: LIR.CFStmtId,
        cond: LIR.LocalId,
        branches_span: LIR.CFSwitchBranchSpan,
        default_is_cold: bool,
        continuation: LIR.CFStmtId,
        common: *const OwnedSet,
    ) ResourceError!void {
        const branches = self.store.getCFSwitchBranches(branches_span);
        const state = try self.store.allocator.create(RewriteSwitchContinuationTask);
        var queued = false;
        errdefer if (!queued) self.store.allocator.destroy(state);
        const branch_results = try self.store.allocator.alloc(LIR.CFStmtId, branches.len);
        errdefer if (!queued) self.store.allocator.free(branch_results);
        state.* = .{
            .start = start,
            .cond = cond,
            .branches = branches_span,
            .original_continuation = continuation,
            .branch_results = branch_results,
            .default_is_cold = default_is_cold,
            .entry_owned = try path.owned.clone(),
            .common = try common.clone(),
            .parent_options = path.options,
            .frames = takeRewriteFrames(path),
            .result = path.result,
        };
        errdefer if (!queued) state.entry_owned.deinit();
        errdefer if (!queued) state.common.deinit();
        errdefer if (!queued) state.frames.deinit(self.store.allocator);

        try tasks.append(self.store.allocator, .{ .switch_after_continuation = state });
        queued = true;
        try self.pushRewritePath(tasks, continuation, &state.common, path.options, &state.continuation);
    }

    fn processRewriteSwitchAfterContinuation(
        self: *Inserter,
        tasks: *std.ArrayList(RewriteTask),
        state: *RewriteSwitchContinuationTask,
    ) ResourceError!void {
        var handed_off = false;
        errdefer if (!handed_off) self.destroyRewriteSwitchContinuation(state);

        state.nested_boundaries = try appendRewriteBoundary(self.store.allocator, state.parent_options.boundaries, .{
            .stop = state.original_continuation,
            .replacement = state.continuation,
            .keep = &state.common,
        });
        const nested_options = RewriteOptions{
            .boundaries = state.nested_boundaries,
            .loop_keep = state.parent_options.loop_keep,
            .loop_keep_id = state.parent_options.loop_keep_id,
            .join_keeps = state.parent_options.join_keeps,
        };

        try tasks.append(self.store.allocator, .{ .switch_finish_continuation = state });
        handed_off = true;
        const switch_stmt = self.store.getCFStmt(state.start).switch_stmt;
        try self.pushRewritePath(tasks, switch_stmt.default_branch, &state.entry_owned, nested_options, &state.default_branch);
        const branches = self.store.getCFSwitchBranches(switch_stmt.branches);
        if (branches.len != state.branch_results.len) arcInvariant("ARC switch branch result count changed during continuation rewrite");
        for (branches, 0..) |branch, index| {
            try self.pushRewritePath(tasks, branch.body, &state.entry_owned, nested_options, &state.branch_results[index]);
        }
    }

    fn finishRewriteSwitchContinuation(self: *Inserter, state: *RewriteSwitchContinuationTask) ResourceError!void {
        errdefer self.destroyRewriteSwitchContinuation(state);
        const branches = self.store.getCFSwitchBranches(state.branches);
        if (branches.len != state.branch_results.len) arcInvariant("ARC switch branch result count changed during rewrite");
        const rewritten_branches = try self.store.allocator.alloc(LIR.CFSwitchBranch, branches.len);
        defer self.store.allocator.free(rewritten_branches);
        for (branches, state.branch_results, 0..) |branch, rewritten, i| {
            rewritten_branches[i] = .{
                .value = branch.value,
                .body = rewritten,
            };
        }
        const switch_stmt = try self.store.addCFStmt(.{ .switch_stmt = .{
            .cond = state.cond,
            .branches = try self.store.addCFSwitchBranches(rewritten_branches),
            .default_branch = state.default_branch,
            .default_is_cold = state.default_is_cold,
            .continuation = state.continuation,
        } });
        state.result.* = try self.finishLinearRewrite(&state.frames, switch_stmt);
        self.destroyRewriteSwitchContinuation(state);
    }

    fn scheduleRewriteStrMatch(
        self: *Inserter,
        tasks: *std.ArrayList(RewriteTask),
        path: *RewritePathTask,
        start: LIR.CFStmtId,
        str_match: anytype,
    ) ResourceError!void {
        const state = try self.store.allocator.create(RewriteStrMatchTask);
        var queued = false;
        errdefer if (!queued) self.store.allocator.destroy(state);
        state.* = .{
            .start = start,
            .source = str_match.source,
            .prefix = str_match.prefix,
            .steps = str_match.steps,
            .end = str_match.end,
            .frames = takeRewriteFrames(path),
            .result = path.result,
        };
        errdefer if (!queued) state.frames.deinit(self.store.allocator);

        try tasks.append(self.store.allocator, .{ .str_match = state });
        queued = true;

        var match_owned = try path.owned.clone();
        defer match_owned.deinit();
        for (self.store.getStrMatchSteps(str_match.steps)) |step| {
            switch (step.capture) {
                .discard => {},
                .view => |local| self.addOwnedIfRc(&match_owned, local),
            }
        }

        try self.pushRewritePath(tasks, str_match.on_miss, &path.owned, path.options, &state.on_miss);
        try self.pushRewritePath(tasks, str_match.on_match, &match_owned, path.options, &state.on_match);
    }

    fn finishRewriteStrMatch(self: *Inserter, state: *RewriteStrMatchTask) ResourceError!void {
        errdefer self.destroyRewriteStrMatch(state);
        const on_match = try self.retainStrMatchSourceForCaptures(state.source, state.steps, state.on_match);
        const str_match = try self.store.addCFStmt(.{ .str_match = .{
            .source = state.source,
            .prefix = state.prefix,
            .steps = state.steps,
            .end = state.end,
            .on_match = on_match,
            .on_miss = state.on_miss,
        } });
        state.result.* = try self.finishLinearRewrite(&state.frames, str_match);
        self.destroyRewriteStrMatch(state);
    }

    fn scheduleRewriteStrMatchSet(
        self: *Inserter,
        tasks: *std.ArrayList(RewriteTask),
        path: *RewritePathTask,
        start: LIR.CFStmtId,
        str_match_set: anytype,
    ) ResourceError!void {
        const arms = self.store.getStrMatchArms(str_match_set.arms);
        const state = try self.store.allocator.create(RewriteStrMatchSetTask);
        var queued = false;
        errdefer if (!queued) self.store.allocator.destroy(state);
        const on_matches = try self.store.allocator.alloc(LIR.CFStmtId, arms.len);
        errdefer if (!queued) self.store.allocator.free(on_matches);

        state.* = .{
            .start = start,
            .source = str_match_set.source,
            .arms = str_match_set.arms,
            .on_matches = on_matches,
            .frames = takeRewriteFrames(path),
            .result = path.result,
        };
        errdefer if (!queued) state.frames.deinit(self.store.allocator);

        try tasks.append(self.store.allocator, .{ .str_match_set = state });
        queued = true;

        try self.pushRewritePath(tasks, str_match_set.on_miss, &path.owned, path.options, &state.on_miss);
        for (arms, 0..) |arm, arm_index| {
            var match_owned = try path.owned.clone();
            defer match_owned.deinit();
            for (self.store.getStrMatchSteps(arm.steps)) |step| {
                switch (step.capture) {
                    .discard => {},
                    .view => |local| self.addOwnedIfRc(&match_owned, local),
                }
            }
            try self.pushRewritePath(tasks, arm.on_match, &match_owned, path.options, &state.on_matches[arm_index]);
        }
    }

    fn finishRewriteStrMatchSet(self: *Inserter, state: *RewriteStrMatchSetTask) ResourceError!void {
        errdefer self.destroyRewriteStrMatchSet(state);
        const arms = self.store.getStrMatchArms(state.arms);
        if (arms.len != state.on_matches.len) arcInvariant("ARC string-match-set arm count changed during rewrite");

        const rewritten_arms = try self.store.allocator.alloc(LIR.StrMatchArm, arms.len);
        defer self.store.allocator.free(rewritten_arms);
        for (arms, state.on_matches, 0..) |arm, rewritten_on_match, index| {
            rewritten_arms[index] = .{
                .prefix = arm.prefix,
                .steps = arm.steps,
                .end = arm.end,
                .on_match = try self.retainStrMatchSourceForCaptures(state.source, arm.steps, rewritten_on_match),
            };
        }

        const str_match_set = try self.store.addCFStmt(.{ .str_match_set = .{
            .source = state.source,
            .arms = try self.store.addStrMatchArms(rewritten_arms),
            .on_miss = state.on_miss,
        } });
        state.result.* = try self.finishLinearRewrite(&state.frames, str_match_set);
        self.destroyRewriteStrMatchSet(state);
    }

    fn analyzeUntil(
        self: *Inserter,
        start: LIR.CFStmtId,
        owned: *const OwnedSet,
        stop: LIR.CFStmtId,
        exits: *std.ArrayList(OwnedSet),
        loop_keep: ?*const OwnedSet,
    ) ResourceError!void {
        var tasks = std.ArrayList(AnalysisTask).empty;
        defer {
            while (tasks.pop()) |task| self.destroyAnalysisTask(task);
            tasks.deinit(self.store.allocator);
        }

        try self.pushAnalysisPath(&tasks, start, .{ .stmt = stop }, owned, exits, loop_keep, null, null);
        while (tasks.pop()) |task| {
            switch (task) {
                .path => |path| try self.processAnalysisPath(&tasks, path),
                .resume_switch_continuation => |resume_task| try self.processAnalysisSwitchContinuation(&tasks, resume_task),
            }
        }
    }

    fn analyzeJumpsToJoin(
        self: *Inserter,
        start: LIR.CFStmtId,
        owned: *const OwnedSet,
        target: LIR.JoinPointId,
        exits: *std.ArrayList(OwnedSet),
        loop_keep: ?*const OwnedSet,
    ) ResourceError!void {
        var tasks = std.ArrayList(AnalysisTask).empty;
        defer {
            while (tasks.pop()) |task| self.destroyAnalysisTask(task);
            tasks.deinit(self.store.allocator);
        }

        var seen = AnalysisSeen.init(self.store.allocator);
        defer self.deinitAnalysisSeen(&seen);
        var scoped_joins = AnalysisScopedJoinMap.init(self.store.allocator);
        defer self.deinitAnalysisScopedJoins(&scoped_joins);

        try self.pushAnalysisPath(&tasks, start, .{ .jump_to = target }, owned, exits, loop_keep, &scoped_joins, &seen);
        while (tasks.pop()) |task| {
            switch (task) {
                .path => |path| try self.processAnalysisPath(&tasks, path),
                .resume_switch_continuation => |resume_task| try self.processAnalysisSwitchContinuation(&tasks, resume_task),
            }
        }
    }

    fn pushAnalysisPath(
        self: *Inserter,
        tasks: *std.ArrayList(AnalysisTask),
        start: LIR.CFStmtId,
        stop: AnalysisStop,
        owned: *const OwnedSet,
        exits: *std.ArrayList(OwnedSet),
        loop_keep: ?*const OwnedSet,
        scoped_joins: ?*AnalysisScopedJoinMap,
        seen: ?*AnalysisSeen,
    ) ResourceError!void {
        const task = try self.store.allocator.create(AnalysisPathTask);
        errdefer self.store.allocator.destroy(task);
        task.* = .{
            .cursor = start,
            .stop = stop,
            .owned = try owned.clone(),
            .exits = exits,
            .loop_keep = loop_keep,
            .scoped_joins = scoped_joins,
            .seen = seen,
        };
        errdefer task.owned.deinit();
        try tasks.append(self.store.allocator, .{ .path = task });
    }

    fn processAnalysisPath(self: *Inserter, tasks: *std.ArrayList(AnalysisTask), path: *AnalysisPathTask) ResourceError!void {
        errdefer self.destroyAnalysisPath(path);

        while (true) {
            if (path.stop == .stmt and path.cursor == path.stop.stmt) {
                try path.exits.append(self.store.allocator, try path.owned.clone());
                self.destroyAnalysisPath(path);
                return;
            }
            if (path.seen) |seen| {
                if (try self.analysisSeenContainsOrAppend(seen, path.cursor, &path.owned)) {
                    self.destroyAnalysisPath(path);
                    return;
                }
            }

            const stmt = self.store.getCFStmt(path.cursor);
            switch (stmt) {
                .assign_ref => |assign| {
                    if (!self.isBindingBorrowed(assign.target)) {
                        switch (assign.op) {
                            .local => |source| {
                                if (assign.target != source) {
                                    const move_value = try self.canMoveSetLocalValue(&path.owned, source, assign.next, path.loop_keep);
                                    if (move_value) self.unsetOwnedUnit(&path.owned, source);
                                    self.addOwnedIfRc(&path.owned, assign.target);
                                }
                            },
                            else => self.addOwnedIfRc(&path.owned, assign.target),
                        }
                    }
                    const singles = [_]LIR.LocalId{ refOpSource(assign.op), assign.target };
                    try self.postStmtDeaths(&path.owned, &singles, null, assign.next, path.loop_keep, null);
                    path.cursor = assign.next;
                },
                .init_uninitialized => |uninit| {
                    path.owned.unset(uninit.target);
                    path.cursor = uninit.next;
                },
                .assign_literal => |assign| {
                    self.addOwnedIfRc(&path.owned, assign.target);
                    const singles = [_]LIR.LocalId{assign.target};
                    try self.postStmtDeaths(&path.owned, &singles, null, assign.next, path.loop_keep, null);
                    path.cursor = assign.next;
                },
                .assign_call => |assign| {
                    // The demanded vector is unused on analysis paths, so
                    // skip the unique-demand scan.
                    var arg_ownership = try self.callArgOwnership(null, &path.owned, self.solution.sigOf(assign.proc), false, assign.args, assign.next, assign.target, path.loop_keep);
                    defer arg_ownership.deinit(self.store.allocator);
                    self.unsetArgs(&path.owned, arg_ownership.transfer_args.items);
                    self.addCallResultOwnedIfRc(&path.owned, assign.target, arg_ownership.demanded.ret_mode);
                    try self.noteCallResultDeathIfUnused(&path.owned, assign.target, arg_ownership.demanded.ret_mode, assign.next, path.loop_keep, null);
                    try self.postStmtDeaths(&path.owned, &.{}, assign.args, assign.next, path.loop_keep, null);
                    path.cursor = assign.next;
                },
                .assign_call_erased => |assign| {
                    var arg_ownership = try self.callArgOwnership(null, &path.owned, arc_sig.RcSig.all_owned, false, assign.args, assign.next, assign.target, path.loop_keep);
                    defer arg_ownership.deinit(self.store.allocator);
                    self.unsetArgs(&path.owned, arg_ownership.transfer_args.items);
                    self.addCallResultOwnedIfRc(&path.owned, assign.target, .owned);
                    try self.noteCallResultDeathIfUnused(&path.owned, assign.target, .owned, assign.next, path.loop_keep, null);
                    const singles = [_]LIR.LocalId{assign.closure};
                    try self.postStmtDeaths(&path.owned, &singles, assign.args, assign.next, path.loop_keep, null);
                    path.cursor = assign.next;
                },
                .assign_packed_erased_fn => |assign| {
                    if (assign.capture) |capture| {
                        _ = try self.singleTransfer(capture, assign.next, assign.target, &path.owned, path.loop_keep);
                    }
                    self.addOwnedIfRc(&path.owned, assign.target);
                    const singles = [_]LIR.LocalId{ assign.capture orelse assign.target, assign.target };
                    try self.postStmtDeaths(&path.owned, &singles, null, assign.next, path.loop_keep, null);
                    path.cursor = assign.next;
                },
                .assign_low_level => |assign| {
                    const preserve_consumed_args = try self.preserveConsumedArgMask(
                        assign.args,
                        assign.rc_effect.consume_args,
                        assign.next,
                        assign.target,
                        path.loop_keep,
                    );
                    const target_consumed = self.maskedArgsContainLocal(assign.args, assign.rc_effect.consume_args, assign.target);
                    if (target_consumed) {
                        self.unsetOwnedUnit(&path.owned, assign.target);
                    }
                    self.unsetMaskedArgsExcept(&path.owned, assign.args, assign.rc_effect.consume_args & ~preserve_consumed_args, assign.target);
                    if (assign.rc_effect.retain_args != 0) {
                        _ = try self.spanTransferMask(assign.args, assign.rc_effect.retain_args, assign.next, assign.target, &path.owned, path.loop_keep);
                    }
                    self.addOwnedIfRc(&path.owned, assign.target);
                    const singles = [_]LIR.LocalId{assign.target};
                    try self.postStmtDeaths(&path.owned, &singles, assign.args, assign.next, path.loop_keep, null);
                    path.cursor = assign.next;
                },
                .assign_list => |assign| {
                    _ = try self.spanTransferMask(assign.elems, ~@as(u64, 0), assign.next, assign.target, &path.owned, path.loop_keep);
                    self.addOwnedIfRc(&path.owned, assign.target);
                    const singles = [_]LIR.LocalId{assign.target};
                    try self.postStmtDeaths(&path.owned, &singles, assign.elems, assign.next, path.loop_keep, null);
                    path.cursor = assign.next;
                },
                .assign_struct => |assign| {
                    _ = try self.spanTransferMask(assign.fields, ~@as(u64, 0), assign.next, assign.target, &path.owned, path.loop_keep);
                    self.addOwnedIfRc(&path.owned, assign.target);
                    const singles = [_]LIR.LocalId{assign.target};
                    try self.postStmtDeaths(&path.owned, &singles, assign.fields, assign.next, path.loop_keep, null);
                    path.cursor = assign.next;
                },
                .assign_tag => |assign| {
                    if (assign.payload) |payload| {
                        _ = try self.singleTransfer(payload, assign.next, assign.target, &path.owned, path.loop_keep);
                    }
                    self.addOwnedIfRc(&path.owned, assign.target);
                    const singles = [_]LIR.LocalId{ assign.payload orelse assign.target, assign.target };
                    try self.postStmtDeaths(&path.owned, &singles, null, assign.next, path.loop_keep, null);
                    path.cursor = assign.next;
                },
                .set_local => |assign| {
                    if (assign.target != assign.value) {
                        const move_value = try self.canMoveSetLocalValue(&path.owned, assign.value, assign.next, path.loop_keep);
                        switch (assign.mode) {
                            .replace_existing, .initialize_join_param => path.owned.unset(assign.target),
                            .initialize_join_result => {},
                        }
                        if (move_value) self.unsetOwnedUnit(&path.owned, assign.value);
                    }
                    self.addOwnedIfRc(&path.owned, assign.target);
                    const singles = [_]LIR.LocalId{ assign.value, assign.target };
                    try self.postStmtDeaths(&path.owned, &singles, null, assign.next, path.loop_keep, null);
                    path.cursor = assign.next;
                },
                .debug => |debug_stmt| {
                    const singles = [_]LIR.LocalId{debug_stmt.message};
                    try self.postStmtDeaths(&path.owned, &singles, null, debug_stmt.next, path.loop_keep, null);
                    path.cursor = debug_stmt.next;
                },
                .expect => |expect_stmt| {
                    const singles = [_]LIR.LocalId{expect_stmt.condition};
                    try self.postStmtDeaths(&path.owned, &singles, null, expect_stmt.next, path.loop_keep, null);
                    path.cursor = expect_stmt.next;
                },
                .incref => |rc| {
                    self.addOwnedIfRc(&path.owned, rc.value);
                    path.cursor = rc.next;
                },
                .decref => |rc| {
                    path.owned.unset(rc.value);
                    path.cursor = rc.next;
                },
                .decref_if_initialized => |rc| {
                    path.owned.unset(rc.value);
                    path.cursor = rc.next;
                },
                .free => |rc| {
                    path.owned.unset(rc.value);
                    path.cursor = rc.next;
                },
                .switch_stmt => |switch_stmt| {
                    if (switch_stmt.continuation) |continuation| {
                        const resume_task = try self.store.allocator.create(AnalysisSwitchContinuationTask);
                        var queued = false;
                        errdefer if (!queued) self.store.allocator.destroy(resume_task);
                        resume_task.* = .{
                            .continuation = continuation,
                            .stop = path.stop,
                            .switch_exits = .empty,
                            .parent_exits = path.exits,
                            .loop_keep = path.loop_keep,
                            .scoped_joins = path.scoped_joins,
                            .seen = path.seen,
                        };
                        try tasks.append(self.store.allocator, .{ .resume_switch_continuation = resume_task });
                        queued = true;

                        for (self.store.getCFSwitchBranches(switch_stmt.branches)) |branch| {
                            try self.pushAnalysisPath(tasks, branch.body, .{ .stmt = continuation }, &path.owned, &resume_task.switch_exits, path.loop_keep, path.scoped_joins, path.seen);
                        }
                        try self.pushAnalysisPath(tasks, switch_stmt.default_branch, .{ .stmt = continuation }, &path.owned, &resume_task.switch_exits, path.loop_keep, path.scoped_joins, path.seen);
                        self.destroyAnalysisPath(path);
                        return;
                    }

                    for (self.store.getCFSwitchBranches(switch_stmt.branches)) |branch| {
                        try self.pushAnalysisPath(tasks, branch.body, path.stop, &path.owned, path.exits, path.loop_keep, path.scoped_joins, path.seen);
                    }
                    try self.pushAnalysisPath(tasks, switch_stmt.default_branch, path.stop, &path.owned, path.exits, path.loop_keep, path.scoped_joins, path.seen);
                    self.destroyAnalysisPath(path);
                    return;
                },
                .switch_initialized_payload => |switch_stmt| {
                    var initialized_owned = try path.owned.clone();
                    defer initialized_owned.deinit();
                    self.addOwnedIfRc(&initialized_owned, switch_stmt.payload);

                    var uninitialized_owned = try path.owned.clone();
                    defer uninitialized_owned.deinit();
                    uninitialized_owned.unset(switch_stmt.payload);

                    try self.pushAnalysisPath(tasks, switch_stmt.initialized_branch, path.stop, &initialized_owned, path.exits, path.loop_keep, path.scoped_joins, path.seen);
                    try self.pushAnalysisPath(tasks, switch_stmt.uninitialized_branch, path.stop, &uninitialized_owned, path.exits, path.loop_keep, path.scoped_joins, path.seen);
                    self.destroyAnalysisPath(path);
                    return;
                },
                .str_match => |str_match| {
                    var match_owned = try path.owned.clone();
                    defer match_owned.deinit();
                    for (self.store.getStrMatchSteps(str_match.steps)) |step| {
                        switch (step.capture) {
                            .discard => {},
                            .view => |local| self.addOwnedIfRc(&match_owned, local),
                        }
                    }
                    try self.pushAnalysisPath(tasks, str_match.on_match, path.stop, &match_owned, path.exits, path.loop_keep, path.scoped_joins, path.seen);
                    try self.pushAnalysisPath(tasks, str_match.on_miss, path.stop, &path.owned, path.exits, path.loop_keep, path.scoped_joins, path.seen);
                    self.destroyAnalysisPath(path);
                    return;
                },
                .str_match_set => |str_match_set| {
                    for (self.store.getStrMatchArms(str_match_set.arms)) |arm| {
                        var match_owned = try path.owned.clone();
                        defer match_owned.deinit();
                        for (self.store.getStrMatchSteps(arm.steps)) |step| {
                            switch (step.capture) {
                                .discard => {},
                                .view => |local| self.addOwnedIfRc(&match_owned, local),
                            }
                        }
                        try self.pushAnalysisPath(tasks, arm.on_match, path.stop, &match_owned, path.exits, path.loop_keep, path.scoped_joins, path.seen);
                    }
                    try self.pushAnalysisPath(tasks, str_match_set.on_miss, path.stop, &path.owned, path.exits, path.loop_keep, path.scoped_joins, path.seen);
                    self.destroyAnalysisPath(path);
                    return;
                },
                .join => |join_stmt| {
                    switch (path.stop) {
                        .stmt => {
                            // A join starts a separate loop/recursive ownership frame.
                            // Switch continuation analysis must not fold that frame into
                            // the parent switch's shared continuation.
                            self.destroyAnalysisPath(path);
                            return;
                        },
                        .jump_to => {
                            if (path.scoped_joins) |scoped_joins| {
                                const scoped_entry = try scoped_joins.getOrPut(join_stmt.id);
                                if (scoped_entry.found_existing) {
                                    if (scoped_entry.value_ptr.body != join_stmt.body) {
                                        arcInvariant("ARC jump analysis saw one join id with multiple bodies");
                                    }
                                    if (scoped_entry.value_ptr.remainder != join_stmt.remainder) {
                                        arcInvariant("ARC jump analysis saw one join id with multiple remainders");
                                    }
                                    if (!localSpanEql(scoped_entry.value_ptr.params, join_stmt.params)) {
                                        arcInvariant("ARC jump analysis saw one join id with multiple param spans");
                                    }
                                    if (!localSpanEql(scoped_entry.value_ptr.maybe_uninitialized_params, join_stmt.maybe_uninitialized_params)) {
                                        arcInvariant("ARC jump analysis saw one join id with multiple maybe-uninitialized param spans");
                                    }
                                    if (!scoped_entry.value_ptr.entry_owned.eql(&path.owned)) {
                                        scoped_entry.value_ptr.entry_owned.intersect(&path.owned);
                                        if (scoped_entry.value_ptr.keep) |*keep| {
                                            keep.deinit();
                                            scoped_entry.value_ptr.keep = null;
                                        }
                                    }
                                } else {
                                    errdefer _ = scoped_joins.remove(join_stmt.id);
                                    scoped_entry.value_ptr.* = .{
                                        .body = join_stmt.body,
                                        .remainder = join_stmt.remainder,
                                        .params = join_stmt.params,
                                        .maybe_uninitialized_params = join_stmt.maybe_uninitialized_params,
                                        .entry_owned = try path.owned.clone(),
                                        .keep = null,
                                    };
                                }
                            }
                            path.cursor = join_stmt.remainder;
                        },
                    }
                },
                .runtime_error, .comptime_exhaustiveness_failed, .loop_continue, .loop_break => {
                    self.destroyAnalysisPath(path);
                    return;
                },
                .jump => |jump_stmt| {
                    if (path.stop == .jump_to and jump_stmt.target == path.stop.jump_to) {
                        try path.exits.append(self.store.allocator, try path.owned.clone());
                    } else if (path.stop == .jump_to) {
                        if (path.scoped_joins) |scoped_joins| {
                            if (scoped_joins.getPtr(jump_stmt.target)) |target_join| {
                                if (target_join.keep == null) {
                                    target_join.keep = try self.joinBodyOwnedSet(
                                        &target_join.entry_owned,
                                        jump_stmt.target,
                                        target_join.params,
                                        target_join.maybe_uninitialized_params,
                                        target_join.remainder,
                                        target_join.body,
                                        null,
                                    );
                                }
                                const next_owned = try target_join.keep.?.clone();
                                path.owned.deinit();
                                path.owned = next_owned;
                                path.cursor = target_join.body;
                                continue;
                            }
                        }
                    }
                    self.destroyAnalysisPath(path);
                    return;
                },
                .ret, .crash, .expect_err => {
                    self.destroyAnalysisPath(path);
                    return;
                },
                .comptime_branch_taken => |marker| {
                    path.cursor = marker.next;
                },
            }
        }
    }

    fn processAnalysisSwitchContinuation(
        self: *Inserter,
        tasks: *std.ArrayList(AnalysisTask),
        resume_task: *AnalysisSwitchContinuationTask,
    ) ResourceError!void {
        errdefer self.destroyAnalysisSwitchContinuation(resume_task);

        if (resume_task.switch_exits.items.len == 0) {
            self.destroyAnalysisSwitchContinuation(resume_task);
            return;
        }

        var common = try resume_task.switch_exits.items[0].clone();
        defer common.deinit();
        for (resume_task.switch_exits.items[1..]) |*state| common.intersect(state);
        try self.pushAnalysisPath(tasks, resume_task.continuation, resume_task.stop, &common, resume_task.parent_exits, resume_task.loop_keep, resume_task.scoped_joins, resume_task.seen);
        self.destroyAnalysisSwitchContinuation(resume_task);
    }

    fn destroyRewriteTask(self: *Inserter, task: RewriteTask) void {
        switch (task) {
            .path => |path| self.destroyRewritePath(path),
            .join => |join| self.destroyRewriteJoin(join),
            .switch_no_continuation => |switch_| self.destroyRewriteSwitchNoContinuation(switch_),
            .switch_after_continuation => |switch_| self.destroyRewriteSwitchContinuation(switch_),
            .switch_finish_continuation => |switch_| self.destroyRewriteSwitchContinuation(switch_),
            .initialized_payload_switch => |switch_| self.destroyRewriteInitializedPayloadSwitch(switch_),
            .str_match => |str_match| self.destroyRewriteStrMatch(str_match),
            .str_match_set => |str_match_set| self.destroyRewriteStrMatchSet(str_match_set),
        }
    }

    fn destroyFrames(self: *Inserter, frames: *std.ArrayList(LinearRewriteFrame)) void {
        for (frames.items) |frame| {
            if (frame.post_release.len != 0) {
                self.store.allocator.free(frame.post_release);
            }
        }
        frames.deinit(self.store.allocator);
    }

    fn destroyRewritePath(self: *Inserter, path: *RewritePathTask) void {
        self.destroyFrames(&path.frames);
        path.owned.deinit();
        self.store.allocator.destroy(path);
    }

    fn destroyRewriteJoin(self: *Inserter, state: *RewriteJoinTask) void {
        _ = self.active_loop_keep_ids.remove(@intFromPtr(&state.body_keep));
        self.destroyFrames(&state.frames);
        if (state.join_keeps.len != 0) self.store.allocator.free(state.join_keeps);
        state.incoming_owned.deinit();
        state.entry_keep.deinit();
        state.body_keep.deinit();
        self.store.allocator.destroy(state);
    }

    fn destroyRewriteSwitchNoContinuation(self: *Inserter, state: *RewriteSwitchNoContinuationTask) void {
        self.destroyFrames(&state.frames);
        self.store.allocator.free(state.branch_results);
        self.store.allocator.destroy(state);
    }

    fn destroyRewriteSwitchContinuation(self: *Inserter, state: *RewriteSwitchContinuationTask) void {
        self.destroyFrames(&state.frames);
        state.entry_owned.deinit();
        state.common.deinit();
        if (state.nested_boundaries.len != 0) self.store.allocator.free(state.nested_boundaries);
        self.store.allocator.free(state.branch_results);
        self.store.allocator.destroy(state);
    }

    fn destroyRewriteInitializedPayloadSwitch(self: *Inserter, state: *RewriteInitializedPayloadSwitchTask) void {
        self.destroyFrames(&state.frames);
        self.store.allocator.destroy(state);
    }

    fn destroyRewriteStrMatch(self: *Inserter, state: *RewriteStrMatchTask) void {
        self.destroyFrames(&state.frames);
        self.store.allocator.destroy(state);
    }

    fn destroyRewriteStrMatchSet(self: *Inserter, state: *RewriteStrMatchSetTask) void {
        self.destroyFrames(&state.frames);
        self.store.allocator.free(state.on_matches);
        self.store.allocator.destroy(state);
    }

    fn destroyAnalysisTask(self: *Inserter, task: AnalysisTask) void {
        switch (task) {
            .path => |path| self.destroyAnalysisPath(path),
            .resume_switch_continuation => |resume_task| self.destroyAnalysisSwitchContinuation(resume_task),
        }
    }

    fn destroyAnalysisPath(self: *Inserter, path: *AnalysisPathTask) void {
        path.owned.deinit();
        self.store.allocator.destroy(path);
    }

    fn destroyAnalysisSwitchContinuation(self: *Inserter, resume_task: *AnalysisSwitchContinuationTask) void {
        for (resume_task.switch_exits.items) |*state| state.deinit();
        resume_task.switch_exits.deinit(self.store.allocator);
        self.store.allocator.destroy(resume_task);
    }

    fn analysisSeenContainsOrAppend(
        self: *Inserter,
        seen: *AnalysisSeen,
        stmt: LIR.CFStmtId,
        owned: *const OwnedSet,
    ) ResourceError!bool {
        const seen_id = AnalysisSeenId{
            .stmt = stmt,
            .owned_digest = ownedSetDigest(owned),
        };
        const seen_entry = try seen.getOrPut(seen_id);
        if (!seen_entry.found_existing) {
            seen_entry.value_ptr.* = .{ .entries = .empty };
        }
        errdefer if (!seen_entry.found_existing) {
            _ = seen.remove(seen_id);
        };

        for (seen_entry.value_ptr.entries.items) |*entry| {
            if (entry.owned.eql(owned)) return true;
        }

        var owned_clone = try owned.clone();
        errdefer owned_clone.deinit();
        try seen_entry.value_ptr.entries.append(self.store.allocator, .{
            .owned = owned_clone,
        });
        return false;
    }

    fn deinitAnalysisSeen(self: *Inserter, seen: *AnalysisSeen) void {
        var iter = seen.valueIterator();
        while (iter.next()) |bucket| {
            for (bucket.entries.items) |*entry| {
                entry.owned.deinit();
            }
            bucket.entries.deinit(self.store.allocator);
        }
        seen.deinit();
    }

    fn deinitAnalysisScopedJoins(_: *Inserter, scoped_joins: *AnalysisScopedJoinMap) void {
        var iter = scoped_joins.valueIterator();
        while (iter.next()) |entry| {
            entry.entry_owned.deinit();
            if (entry.keep) |*keep| {
                keep.deinit();
            }
        }
        scoped_joins.deinit();
    }

    fn deinitJoinBodyMemo(self: *Inserter, memo: *JoinBodyMemo) void {
        var iter = memo.valueIterator();
        while (iter.next()) |bucket| {
            for (bucket.entries.items) |*entry| {
                entry.entry_owned.deinit();
                entry.keep.deinit();
            }
            bucket.entries.deinit(self.store.allocator);
        }
        memo.deinit();
    }

    fn isBindingBorrowed(self: *const Inserter, local: LIR.LocalId) bool {
        if (!self.solution.isBorrowed(local)) return false;
        return !self.owned_param_override.contains(local);
    }

    fn addOwnedIfRc(self: *Inserter, owned: *OwnedSet, local: LIR.LocalId) void {
        if (!self.localContainsRefcounted(local)) return;
        if (self.isBindingBorrowed(local)) return;
        owned.set(local);
    }

    fn addConditionallyOwnedIfRc(self: *Inserter, owned: *OwnedSet, local: LIR.LocalId) void {
        if (!self.localContainsRefcounted(local)) return;
        owned.set(local);
    }

    fn addCallResultOwnedIfRc(
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

    fn ownsUnit(self: *const Inserter, owned: *const OwnedSet, local: LIR.LocalId) bool {
        return owned.contains(self.solution.unitLocalOf(local));
    }

    fn unsetOwnedUnit(self: *const Inserter, owned: *OwnedSet, local: LIR.LocalId) void {
        owned.unset(self.solution.unitLocalOf(local));
    }

    fn cachedJoinBodyOwnedSet(
        self: *Inserter,
        entry_owned: *const OwnedSet,
        join_id: LIR.JoinPointId,
        params: LIR.LocalSpan,
        maybe_uninitialized_params: LIR.LocalSpan,
        remainder: LIR.CFStmtId,
        body: LIR.CFStmtId,
        body_reachable: ?*bool,
    ) ResourceError!?OwnedSet {
        const memo = self.join_body_memo orelse return null;
        const memo_id = JoinBodyMemoId{
            .join = join_id,
            .owned_digest = ownedSetDigest(entry_owned),
        };
        const bucket = memo.getPtr(memo_id) orelse return null;
        for (bucket.entries.items) |*entry| {
            if (!entry.entry_owned.eql(entry_owned)) continue;
            checkJoinBodyMemoMetadata(entry, params, maybe_uninitialized_params, remainder, body);
            if (body_reachable) |reachable| reachable.* = entry.body_reachable;
            return try entry.keep.clone();
        }
        return null;
    }

    fn cacheJoinBodyOwnedSet(
        self: *Inserter,
        entry_owned: *const OwnedSet,
        join_id: LIR.JoinPointId,
        params: LIR.LocalSpan,
        maybe_uninitialized_params: LIR.LocalSpan,
        remainder: LIR.CFStmtId,
        body: LIR.CFStmtId,
        keep: *const OwnedSet,
        body_reachable: bool,
    ) ResourceError!void {
        const memo = self.join_body_memo orelse return;
        const memo_id = JoinBodyMemoId{
            .join = join_id,
            .owned_digest = ownedSetDigest(entry_owned),
        };
        const bucket = try memo.getOrPut(memo_id);
        if (!bucket.found_existing) {
            bucket.value_ptr.* = .{ .entries = .empty };
        }
        errdefer if (!bucket.found_existing) {
            _ = memo.remove(memo_id);
        };

        for (bucket.value_ptr.entries.items) |*entry| {
            if (!entry.entry_owned.eql(entry_owned)) continue;
            checkJoinBodyMemoMetadata(entry, params, maybe_uninitialized_params, remainder, body);
            return;
        }

        var entry_owned_clone = try entry_owned.clone();
        errdefer entry_owned_clone.deinit();
        var keep_clone = try keep.clone();
        errdefer keep_clone.deinit();
        try bucket.value_ptr.entries.append(self.store.allocator, .{
            .entry_owned = entry_owned_clone,
            .params = params,
            .maybe_uninitialized_params = maybe_uninitialized_params,
            .remainder = remainder,
            .body = body,
            .keep = keep_clone,
            .body_reachable = body_reachable,
        });
    }

    fn checkJoinBodyMemoMetadata(
        entry: *const JoinBodyMemoEntry,
        params: LIR.LocalSpan,
        maybe_uninitialized_params: LIR.LocalSpan,
        remainder: LIR.CFStmtId,
        body: LIR.CFStmtId,
    ) void {
        if (!localSpanEql(entry.params, params)) {
            arcInvariant("ARC join body memo saw one join id with multiple param spans");
        }
        if (!localSpanEql(entry.maybe_uninitialized_params, maybe_uninitialized_params)) {
            arcInvariant("ARC join body memo saw one join id with multiple maybe-uninitialized param spans");
        }
        if (entry.remainder != remainder) {
            arcInvariant("ARC join body memo saw one join id with multiple remainders");
        }
        if (entry.body != body) {
            arcInvariant("ARC join body memo saw one join id with multiple bodies");
        }
    }

    fn joinBodyOwnedSet(
        self: *Inserter,
        entry_owned: *const OwnedSet,
        join_id: LIR.JoinPointId,
        params: LIR.LocalSpan,
        maybe_uninitialized_params: LIR.LocalSpan,
        remainder: LIR.CFStmtId,
        body: LIR.CFStmtId,
        body_reachable: ?*bool,
    ) ResourceError!OwnedSet {
        if (try self.cachedJoinBodyOwnedSet(entry_owned, join_id, params, maybe_uninitialized_params, remainder, body, body_reachable)) |cached| {
            return cached;
        }

        // The emitted body is shared by every jump into the join. A non-param
        // local can enter that body as owned only when every jump reaches it
        // with that local owned, whether the local was already owned before
        // the join or born inside the run-once remainder.
        var jump_states = std.ArrayList(OwnedSet).empty;
        defer {
            for (jump_states.items) |*state| state.deinit();
            jump_states.deinit(self.store.allocator);
        }
        try self.analyzeJumpsToJoin(remainder, entry_owned, join_id, &jump_states, null);
        const reachable = jump_states.items.len != 0;
        if (body_reachable) |reachable_out| reachable_out.* = reachable;

        var owned = try OwnedSet.init(self.store.allocator, entry_owned.len());
        errdefer owned.deinit();

        if (jump_states.items.len != 0) {
            owned.deinit();
            owned = try jump_states.items[0].clone();
            for (jump_states.items[1..]) |*jump_owned| {
                owned.intersect(jump_owned);
            }

            var iter = owned.bits.iterator(.{});
            while (iter.next()) |i| {
                const local: LIR.LocalId = @enumFromInt(@as(u32, @intCast(i)));
                if (!try self.groupUsedInPath(body, local, null)) {
                    owned.unset(local);
                }
            }
        }

        for (self.store.getLocalSpan(params)) |param| {
            self.addOwnedIfRc(&owned, param);
        }
        for (self.store.getLocalSpan(maybe_uninitialized_params)) |param| {
            // A maybe-initialized join payload may be overwritten before it is
            // read. That overwrite is still the lifetime end of the previous
            // payload, so the loop body must enter with conditional ownership
            // even when read-before-rebind analysis would otherwise classify
            // the param as borrowed.
            self.addConditionallyOwnedIfRc(&owned, param);
        }
        try self.cacheJoinBodyOwnedSet(entry_owned, join_id, params, maybe_uninitialized_params, remainder, body, &owned, reachable);
        return owned;
    }

    fn joinEntryOwnedSet(
        self: *Inserter,
        entry_owned: *const OwnedSet,
        remainder: LIR.CFStmtId,
    ) ResourceError!OwnedSet {
        var owned = try OwnedSet.init(self.store.allocator, entry_owned.len());
        errdefer owned.deinit();
        var iter = entry_owned.bits.iterator(.{});
        while (iter.next()) |i| {
            const local: LIR.LocalId = @enumFromInt(@as(u32, @intCast(i)));
            const used = try self.groupUsedInPath(remainder, local, null);
            if (used) {
                owned.set(local);
            }
        }
        return owned;
    }

    /// Computes which operand positions in `span` (restricted to
    /// `position_mask`) can move their ownership unit into the value being
    /// constructed: the operand is owned and its liveness group has no use
    /// after this statement. Transferred operands leave the owned set.
    fn spanTransferMask(
        self: *Inserter,
        span: LIR.LocalSpan,
        position_mask: u64,
        next: LIR.CFStmtId,
        target: LIR.LocalId,
        owned: *OwnedSet,
        loop_keep: ?*const OwnedSet,
    ) ResourceError!u64 {
        var transfer: u64 = 0;
        if (!self.localContainsRefcounted(target)) return 0;
        const locals = self.store.getLocalSpan(span);
        for (locals, 0..) |local, i| {
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

    /// Single-operand variant of `spanTransferMask` for tag payloads and
    /// packed captures.
    fn singleTransfer(
        self: *Inserter,
        local: LIR.LocalId,
        next: LIR.CFStmtId,
        target: LIR.LocalId,
        owned: *OwnedSet,
        loop_keep: ?*const OwnedSet,
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
        loop_keep: ?*const OwnedSet,
        collected: ?*std.ArrayList(LIR.LocalId),
    ) ResourceError!void {
        for (singles) |local| {
            try self.noteDeathIfUnused(owned, local, next, loop_keep, collected);
        }
        if (span) |operand_span| {
            for (self.store.getLocalSpan(operand_span)) |local| {
                try self.noteDeathIfUnused(owned, local, next, loop_keep, collected);
            }
        }
    }

    fn noteDeathIfUnused(
        self: *Inserter,
        owned: *OwnedSet,
        local: LIR.LocalId,
        next: LIR.CFStmtId,
        loop_keep: ?*const OwnedSet,
        collected: ?*std.ArrayList(LIR.LocalId),
    ) ResourceError!void {
        // A borrowed operand's lifetime event belongs to its owning leader:
        // the leader dies when no group member has a later use.
        const owner = self.solution.leaderOf(local);
        if (!owned.contains(owner)) return;
        // Join parameters carry their unit into the join body, whose release
        // statements are not visible to use scans.
        if (self.solution.isJoinParam(owner) or self.solution.isJoinParam(local)) return;
        if (try self.groupUsedInPath(next, owner, loop_keep)) return;
        owned.unset(owner);
        if (collected) |list| {
            try list.append(self.store.allocator, owner);
        }
    }

    fn noteCallResultDeathIfUnused(
        self: *Inserter,
        owned: *OwnedSet,
        local: LIR.LocalId,
        ret_mode: arc_sig.Mode,
        next: LIR.CFStmtId,
        loop_keep: ?*const OwnedSet,
        collected: ?*std.ArrayList(LIR.LocalId),
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
        loop_keep: ?*const OwnedSet,
        collected: ?*std.ArrayList(LIR.LocalId),
    ) ResourceError!void {
        if (!owned.contains(local)) return;
        if (self.solution.isJoinParam(local)) return;
        if (try self.localValueUsedInPath(next, local, loop_keep)) return;
        owned.unset(local);
        if (collected) |list| {
            try list.append(self.store.allocator, local);
        }
    }

    fn takePostReleases(self: *Inserter, deaths: *std.ArrayList(LIR.LocalId)) ResourceError![]const LIR.LocalId {
        if (deaths.items.len == 0) return &.{};
        return try deaths.toOwnedSlice(self.store.allocator);
    }

    fn releaseOldTargetIfNeeded(self: *Inserter, target: LIR.LocalId, owned: *OwnedSet, next: LIR.CFStmtId) ResourceError!LIR.CFStmtId {
        if (!owned.contains(target)) return next;
        owned.unset(target);
        if (self.solution.maybeUninitializedCondition(target)) |condition| {
            return try self.releaseMaybeInitializedLocal(condition.local, condition.mask, target, next);
        }
        return try self.releaseLocalIfRc(target, next);
    }

    fn canMoveSetLocalValue(
        self: *Inserter,
        owned: *const OwnedSet,
        value: LIR.LocalId,
        next: LIR.CFStmtId,
        loop_keep: ?*const OwnedSet,
    ) ResourceError!bool {
        if (!self.ownsUnit(owned, value)) return false;
        if (!self.localContainsRefcounted(value)) return false;
        return !(try self.groupUsedInPath(next, value, loop_keep));
    }

    fn retainMaskedArgs(self: *Inserter, span: LIR.LocalSpan, mask: u64, next: LIR.CFStmtId) ResourceError!LIR.CFStmtId {
        var current = next;
        const locals = self.store.getLocalSpan(span);
        var i = locals.len;
        while (i > 0) {
            i -= 1;
            if (i >= 64) continue;
            if ((mask & argMaskBit(i)) != 0) {
                current = try self.retainLocalIfRc(locals[i], current);
            }
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
        next: LIR.CFStmtId,
        target: LIR.LocalId,
        loop_keep: ?*const OwnedSet,
    ) ResourceError!u64 {
        if (mask == 0) return 0;
        var preserve: u64 = 0;
        const locals = self.store.getLocalSpan(span);
        for (locals, 0..) |local, i| {
            if (i >= 64) break;
            const bit = argMaskBit(i);
            if ((mask & bit) == 0) continue;
            if (local == target) continue;
            if (try self.groupUsedInPath(next, local, loop_keep)) {
                preserve |= bit;
            }
        }
        return preserve;
    }

    /// Runtime uniqueness checks proven redundant at this low-level
    /// statement, by argument position: the argument's value is unique in
    /// the current emission view (born with count 1 — by a fresh
    /// allocation, a direct call to a unique-returning callee, or a variant
    /// parameter seed — and never given another holder), its single
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
        for (locals, 0..) |local, i| {
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
        locals: []const LIR.LocalId,
        position: usize,
        local: LIR.LocalId,
    ) bool {
        const leader = self.solution.leaderOf(local);
        for (locals, 0..) |other, j| {
            if (j == position) continue;
            if (other == local) return true;
            if (self.solution.leaderOf(other) == leader) return true;
        }
        return false;
    }

    fn procParamCanUseUniqueSeed(
        self: *Inserter,
        proc_id: LIR.LirProcSpecId,
        position: usize,
    ) ResourceError!bool {
        if (position >= 64) return false;
        const proc_index = @intFromEnum(proc_id);
        if (proc_index >= self.variants.unique_seed_masks.len) return false;
        if (self.variants.unique_seed_masks[proc_index]) |mask| {
            return (mask & argMaskBit(position)) != 0;
        }

        const mask = try self.computeProcUniqueSeedMask(proc_id);
        self.variants.unique_seed_masks[proc_index] = mask;
        return (mask & argMaskBit(position)) != 0;
    }

    fn computeProcUniqueSeedMask(self: *Inserter, proc_id: LIR.LirProcSpecId) ResourceError!u64 {
        const proc = self.store.getProcSpec(proc_id);
        const params = self.store.getLocalSpan(proc.args);
        if (params.len == 0) return 0;

        var mask: u64 = 0;
        var visited = try self.store.allocator.alloc(bool, self.store.cf_stmts.items.len);
        defer self.store.allocator.free(visited);
        @memset(visited, false);

        var stack = std.ArrayList(LIR.CFStmtId).empty;
        defer stack.deinit(self.store.allocator);

        if (self.variants.original_bodies[@intFromEnum(proc_id)]) |body| {
            try stack.append(self.store.allocator, body);
        }
        for (self.store.getJoinPointSpan(proc.join_points)) |join_point| {
            try stack.append(self.store.allocator, join_point.body);
        }

        while (stack.pop()) |stmt_id| {
            const stmt_index = @intFromEnum(stmt_id);
            if (stmt_index >= visited.len) arcInvariant("ARC unique-seed scan saw stmt outside store");
            if (visited[stmt_index]) continue;
            visited[stmt_index] = true;

            const stmt = self.store.getCFStmt(stmt_id);
            switch (stmt) {
                .assign_low_level => |assign| {
                    mask |= self.uniqueSeedMaskForLowLevel(params, assign.args, assign.rc_effect);
                    try stack.append(self.store.allocator, assign.next);
                },
                .switch_stmt => |switch_stmt| {
                    for (self.store.getCFSwitchBranches(switch_stmt.branches)) |branch| {
                        try stack.append(self.store.allocator, branch.body);
                    }
                    try stack.append(self.store.allocator, switch_stmt.default_branch);
                    if (switch_stmt.continuation) |continuation| {
                        try stack.append(self.store.allocator, continuation);
                    }
                },
                .switch_initialized_payload => |switch_stmt| {
                    try stack.append(self.store.allocator, switch_stmt.initialized_branch);
                    try stack.append(self.store.allocator, switch_stmt.uninitialized_branch);
                },
                .str_match => |str_match| {
                    try stack.append(self.store.allocator, str_match.on_match);
                    try stack.append(self.store.allocator, str_match.on_miss);
                },
                .str_match_set => |str_match_set| {
                    for (self.store.getStrMatchArms(str_match_set.arms)) |arm| {
                        try stack.append(self.store.allocator, arm.on_match);
                    }
                    try stack.append(self.store.allocator, str_match_set.on_miss);
                },
                .join => |join_stmt| {
                    try stack.append(self.store.allocator, join_stmt.body);
                    try stack.append(self.store.allocator, join_stmt.remainder);
                },
                inline .init_uninitialized,
                .assign_ref,
                .assign_literal,
                .assign_call,
                .assign_call_erased,
                .assign_packed_erased_fn,
                .assign_list,
                .assign_struct,
                .assign_tag,
                .set_local,
                .debug,
                .expect,
                .comptime_branch_taken,
                .incref,
                .decref,
                .decref_if_initialized,
                .free,
                => |linear| try stack.append(self.store.allocator, linear.next),
                .ret,
                .jump,
                .crash,
                .expect_err,
                .runtime_error,
                .comptime_exhaustiveness_failed,
                .loop_continue,
                .loop_break,
                => {},
            }
        }

        return mask;
    }

    fn uniqueSeedMaskForLowLevel(
        self: *const Inserter,
        params: []const LIR.LocalId,
        args: LIR.LocalSpan,
        rc_effect: LIR.LowLevel.RcEffect,
    ) u64 {
        const check_mask = rc_effect.may_runtime_uniqueness_check_args & rc_effect.consume_args;
        if (check_mask == 0) return 0;

        var mask: u64 = 0;
        const locals = self.store.getLocalSpan(args);
        for (locals, 0..) |arg, arg_position| {
            if (arg_position >= 64) break;
            if ((check_mask & argMaskBit(arg_position)) == 0) continue;
            for (params[0..@min(params.len, 64)], 0..) |param, param_position| {
                if (arg == param) {
                    mask |= argMaskBit(param_position);
                    break;
                }
            }
        }
        return mask;
    }

    fn callArgOwnership(
        self: *Inserter,
        callee: ?LIR.LirProcSpecId,
        owned: *const OwnedSet,
        callee_sig: arc_sig.RcSig,
        unique_demand: bool,
        span: LIR.LocalSpan,
        next: LIR.CFStmtId,
        target: LIR.LocalId,
        loop_keep: ?*const OwnedSet,
    ) ResourceError!CallArgOwnership {
        var result = CallArgOwnership{};
        errdefer result.deinit(self.store.allocator);
        var transferred = try OwnedSet.init(self.store.allocator, owned.len());
        defer transferred.deinit();

        result.demanded = callee_sig;
        const locals = self.store.getLocalSpan(span);
        for (locals, 0..) |local, position| {
            if (!self.localContainsRefcounted(local)) continue;
            if (callee_sig.paramMode(position) == .borrowed) {
                // Borrowed positions keep the caller's ownership untouched.
                // With specialization enabled, a final-use argument only
                // moves into an owned-demanding variant when that changes
                // runtime work inside the callee. Merely moving the caller's
                // post-call release into a clone preserves the same RC work
                // while growing live code.
                if (!self.variants.enabled) continue;
                if (position >= 64) continue;
                const used_after_call = local != target and try self.groupUsedInPath(next, local, loop_keep);
                const owner = self.solution.unitLocalOf(local);
                const can_transfer = owned.contains(owner) and !used_after_call and !transferred.contains(owner);
                if (!can_transfer) continue;
                const bit = @as(u64, 1) << @as(u6, @intCast(position));
                const return_borrows_param = callee_sig.ret_mode == .borrowed and (callee_sig.ret_lenders & bit) != 0;
                const seed_can_reach_check = if (callee) |direct| try self.procParamCanUseUniqueSeed(direct, position) else false;
                const seeds_unique_param = unique_demand and seed_can_reach_check and self.isLocalUniqueHere(local) and
                    !self.groupSharesOtherOperand(locals, position, local);
                if (!return_borrows_param and !seeds_unique_param) continue;
                result.demanded.borrowed_params &= ~bit;
                if (return_borrows_param) {
                    result.demanded.ret_mode = .owned;
                    result.demanded.ret_lenders = 0;
                }
                if (seeds_unique_param) {
                    result.demanded.unique_params |= bit;
                }
                try result.transfer_args.append(self.store.allocator, local);
                transferred.set(owner);
                continue;
            }

            const used_after_call = local != target and try self.groupUsedInPath(next, local, loop_keep);
            const owner = self.solution.unitLocalOf(local);
            const can_transfer = owned.contains(owner) and !used_after_call and !transferred.contains(owner);

            if (can_transfer) {
                // A dying argument moving into an owned position that is
                // statically unique with no borrow live at the call demands
                // a variant whose parameter is seeded born-unique, so
                // checked ops it reaches in the body go check-free.
                const seed_can_reach_check = if (position < 64) blk: {
                    const direct = callee orelse break :blk false;
                    break :blk try self.procParamCanUseUniqueSeed(direct, position);
                } else false;
                if (unique_demand and seed_can_reach_check and self.isLocalUniqueHere(local) and
                    !self.groupSharesOtherOperand(locals, position, local))
                {
                    result.demanded.unique_params |= @as(u64, 1) << @as(u6, @intCast(position));
                }
                try result.transfer_args.append(self.store.allocator, local);
                transferred.set(owner);
            } else {
                try result.retain_args.append(self.store.allocator, local);
            }
        }

        return result;
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
            demanded.unique_params == 0)
        {
            return null;
        }
        const selector = VariantSelector{
            .source = callee,
            .borrowed_params = demanded.borrowed_params,
            .ret_mode = demanded.ret_mode,
            .unique_params = demanded.unique_params,
        };
        const entry = try self.variants.map.getOrPut(selector);
        if (entry.found_existing) return entry.value_ptr.*;

        const source_spec = self.store.getProcSpec(callee);
        const variant = try self.store.addProcSpec(.{
            .name = self.store.freshSyntheticSymbol(),
            .args = source_spec.args,
            .frame_locals = source_spec.frame_locals,
            .body = self.variants.original_bodies[@intFromEnum(callee)],
            .ret_layout = source_spec.ret_layout,
            .abi = source_spec.abi,
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
        for (locals, 0..) |local, i| {
            if (i >= 64) break;
            if ((mask & argMaskBit(i)) != 0 and local == needle) return true;
        }
        return false;
    }

    fn unsetArgs(self: *Inserter, owned: *OwnedSet, args: []const LIR.LocalId) void {
        for (args) |local| {
            self.unsetOwnedUnit(owned, local);
        }
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
        for (locals, 0..) |local, i| {
            if (i >= 64) break;
            if ((mask & argMaskBit(i)) != 0 and local != except) {
                self.unsetOwnedUnit(owned, local);
            }
        }
    }

    fn collectJoinBodies(
        self: *Inserter,
        start: LIR.CFStmtId,
        join_bodies: *JoinBodyMap,
        visited: *std.AutoHashMap(LIR.CFStmtId, void),
    ) ResourceError!void {
        var stack = std.ArrayList(LIR.CFStmtId).empty;
        defer stack.deinit(self.store.allocator);
        try stack.append(self.store.allocator, start);

        while (stack.pop()) |current| {
            if (visited.contains(current)) continue;
            try visited.put(current, {});

            const stmt = self.store.getCFStmt(current);
            switch (stmt) {
                .assign_ref => |assign| try stack.append(self.store.allocator, assign.next),
                .assign_literal => |assign| try stack.append(self.store.allocator, assign.next),
                .init_uninitialized => |uninit| try stack.append(self.store.allocator, uninit.next),
                .assign_call => |assign| try stack.append(self.store.allocator, assign.next),
                .assign_call_erased => |assign| try stack.append(self.store.allocator, assign.next),
                .assign_packed_erased_fn => |assign| try stack.append(self.store.allocator, assign.next),
                .assign_low_level => |assign| try stack.append(self.store.allocator, assign.next),
                .assign_list => |assign| try stack.append(self.store.allocator, assign.next),
                .assign_struct => |assign| try stack.append(self.store.allocator, assign.next),
                .assign_tag => |assign| try stack.append(self.store.allocator, assign.next),
                .set_local => |assign| try stack.append(self.store.allocator, assign.next),
                .debug => |debug_stmt| try stack.append(self.store.allocator, debug_stmt.next),
                .expect => |expect_stmt| try stack.append(self.store.allocator, expect_stmt.next),
                .comptime_branch_taken => |taken| try stack.append(self.store.allocator, taken.next),
                .decref_if_initialized => |rc| try stack.append(self.store.allocator, rc.next),
                .switch_stmt => |switch_stmt| {
                    if (switch_stmt.continuation) |continuation| {
                        try stack.append(self.store.allocator, continuation);
                    }
                    try stack.append(self.store.allocator, switch_stmt.default_branch);
                    for (self.store.getCFSwitchBranches(switch_stmt.branches)) |branch| {
                        try stack.append(self.store.allocator, branch.body);
                    }
                },
                .switch_initialized_payload => |switch_stmt| {
                    try stack.append(self.store.allocator, switch_stmt.initialized_branch);
                    try stack.append(self.store.allocator, switch_stmt.uninitialized_branch);
                },
                .str_match => |str_match| {
                    try stack.append(self.store.allocator, str_match.on_match);
                    try stack.append(self.store.allocator, str_match.on_miss);
                },
                .str_match_set => |str_match_set| {
                    for (self.store.getStrMatchArms(str_match_set.arms)) |arm| {
                        try stack.append(self.store.allocator, arm.on_match);
                    }
                    try stack.append(self.store.allocator, str_match_set.on_miss);
                },
                .join => |join_stmt| {
                    const previous = try join_bodies.getOrPut(join_stmt.id);
                    if (previous.found_existing and previous.value_ptr.* != join_stmt.body) {
                        arcInvariant("ARC join-body collection saw one join id with multiple bodies");
                    }
                    previous.value_ptr.* = join_stmt.body;
                    try stack.append(self.store.allocator, join_stmt.remainder);
                    try stack.append(self.store.allocator, join_stmt.body);
                },
                .jump,
                .ret,
                .runtime_error,
                .comptime_exhaustiveness_failed,
                .loop_continue,
                .loop_break,
                .crash,
                .expect_err,
                => {},
                .incref, .decref, .free => arcInvariant("ARC join-body collection received already-reference-counted LIR"),
            }
        }
    }

    fn writeProcJoinPoints(self: *Inserter, proc: *LIR.LirProcSpec) ResourceError!void {
        const body = proc.body orelse {
            proc.join_points = LIR.JoinPointSpan.empty();
            return;
        };

        var joins: FinalJoinMap = .empty;
        defer joins.deinit(self.store.allocator);
        var visited = std.AutoHashMap(LIR.CFStmtId, void).init(self.store.allocator);
        defer visited.deinit();
        try self.collectFinalJoinPoints(body, &joins, &visited);

        const sorted = try self.store.allocator.alloc(LIR.JoinPoint, joins.count());
        defer self.store.allocator.free(sorted);
        for (joins.values(), 0..) |join, index| {
            sorted[index] = join;
        }
        std.mem.sort(LIR.JoinPoint, sorted, {}, joinPointLessThan);
        proc.join_points = try self.store.addJoinPointSpan(sorted);
    }

    fn collectFinalJoinPoints(
        self: *Inserter,
        start: LIR.CFStmtId,
        joins: *FinalJoinMap,
        visited: *std.AutoHashMap(LIR.CFStmtId, void),
    ) ResourceError!void {
        var stack = std.ArrayList(LIR.CFStmtId).empty;
        defer stack.deinit(self.store.allocator);
        try stack.append(self.store.allocator, start);

        while (stack.pop()) |current| {
            if (visited.contains(current)) continue;
            try visited.put(current, {});

            const stmt = self.store.getCFStmt(current);
            switch (stmt) {
                .assign_ref => |assign| try stack.append(self.store.allocator, assign.next),
                .assign_literal => |assign| try stack.append(self.store.allocator, assign.next),
                .init_uninitialized => |uninit| try stack.append(self.store.allocator, uninit.next),
                .assign_call => |assign| try stack.append(self.store.allocator, assign.next),
                .assign_call_erased => |assign| try stack.append(self.store.allocator, assign.next),
                .assign_packed_erased_fn => |assign| try stack.append(self.store.allocator, assign.next),
                .assign_low_level => |assign| try stack.append(self.store.allocator, assign.next),
                .assign_list => |assign| try stack.append(self.store.allocator, assign.next),
                .assign_struct => |assign| try stack.append(self.store.allocator, assign.next),
                .assign_tag => |assign| try stack.append(self.store.allocator, assign.next),
                .set_local => |assign| try stack.append(self.store.allocator, assign.next),
                .debug => |debug_stmt| try stack.append(self.store.allocator, debug_stmt.next),
                .expect => |expect_stmt| try stack.append(self.store.allocator, expect_stmt.next),
                .incref => |rc| try stack.append(self.store.allocator, rc.next),
                .decref => |rc| try stack.append(self.store.allocator, rc.next),
                .decref_if_initialized => |rc| try stack.append(self.store.allocator, rc.next),
                .free => |rc| try stack.append(self.store.allocator, rc.next),
                .comptime_branch_taken => |taken| try stack.append(self.store.allocator, taken.next),
                .switch_stmt => |switch_stmt| {
                    if (switch_stmt.continuation) |continuation| {
                        try stack.append(self.store.allocator, continuation);
                    }
                    try stack.append(self.store.allocator, switch_stmt.default_branch);
                    for (self.store.getCFSwitchBranches(switch_stmt.branches)) |branch| {
                        try stack.append(self.store.allocator, branch.body);
                    }
                },
                .switch_initialized_payload => |switch_stmt| {
                    try stack.append(self.store.allocator, switch_stmt.initialized_branch);
                    try stack.append(self.store.allocator, switch_stmt.uninitialized_branch);
                },
                .str_match => |str_match| {
                    try stack.append(self.store.allocator, str_match.on_match);
                    try stack.append(self.store.allocator, str_match.on_miss);
                },
                .str_match_set => |str_match_set| {
                    for (self.store.getStrMatchArms(str_match_set.arms)) |arm| {
                        try stack.append(self.store.allocator, arm.on_match);
                    }
                    try stack.append(self.store.allocator, str_match_set.on_miss);
                },
                .join => |join_stmt| {
                    const entry = try joins.getOrPut(self.store.allocator, join_stmt.id);
                    const join_point = LIR.JoinPoint{
                        .id = join_stmt.id,
                        .params = join_stmt.params,
                        .body = join_stmt.body,
                    };
                    if (entry.found_existing and !joinPointEql(entry.value_ptr.*, join_point)) {
                        arcInvariant("ARC final join-point output saw one join id with different data");
                    }
                    entry.value_ptr.* = join_point;
                    try stack.append(self.store.allocator, join_stmt.remainder);
                    try stack.append(self.store.allocator, join_stmt.body);
                },
                .jump,
                .ret,
                .runtime_error,
                .comptime_exhaustiveness_failed,
                .loop_continue,
                .loop_break,
                .crash,
                .expect_err,
                => {},
            }
        }
    }

    const ReadBeforeRebindNode = struct {
        stmt: LIR.CFStmtId,
        reads: std.bit_set.DynamicBitSetUnmanaged,
        exposed: std.bit_set.DynamicBitSetUnmanaged,
        successor_start: usize,
        successor_len: u32,
        def: ?LIR.LocalId,
    };

    const ReadBeforeRebindGraph = struct {
        allocator: Allocator,
        nodes: std.ArrayList(ReadBeforeRebindNode),
        successors: std.ArrayList(LIR.CFStmtId),
        indices: std.AutoHashMap(LIR.CFStmtId, usize),

        fn init(allocator: Allocator) ReadBeforeRebindGraph {
            return .{
                .allocator = allocator,
                .nodes = .empty,
                .successors = .empty,
                .indices = std.AutoHashMap(LIR.CFStmtId, usize).init(allocator),
            };
        }
    };

    fn clearReadsBeforeRebindCache(self: *Inserter) void {
        self.reads_before_rebind_cache.clearRetainingCapacity();
        _ = self.read_cache_arena.reset(.retain_capacity);
    }

    fn ensureReadBeforeRebindNode(
        self: *Inserter,
        graph: *ReadBeforeRebindGraph,
        work: *std.ArrayList(LIR.CFStmtId),
        stmt: LIR.CFStmtId,
    ) ResourceError!void {
        if (graph.indices.contains(stmt)) return;

        var reads = try std.bit_set.DynamicBitSetUnmanaged.initEmpty(graph.allocator, self.store.locals.items.len);
        errdefer reads.deinit(graph.allocator);
        var exposed = try std.bit_set.DynamicBitSetUnmanaged.initEmpty(graph.allocator, self.store.locals.items.len);
        errdefer exposed.deinit(graph.allocator);

        const index = graph.nodes.items.len;
        try graph.indices.put(stmt, index);
        errdefer _ = graph.indices.remove(stmt);

        try graph.nodes.append(graph.allocator, .{
            .stmt = stmt,
            .reads = reads,
            .exposed = exposed,
            .successor_start = 0,
            .successor_len = 0,
            .def = null,
        });
        try work.append(graph.allocator, stmt);
    }

    fn appendReadBeforeRebindSuccessor(
        self: *Inserter,
        graph: *ReadBeforeRebindGraph,
        work: *std.ArrayList(LIR.CFStmtId),
        node_index: usize,
        successor: LIR.CFStmtId,
    ) ResourceError!void {
        const successor_index = graph.successors.items.len;
        if (graph.nodes.items[node_index].successor_len == 0) {
            graph.nodes.items[node_index].successor_start = successor_index;
        }
        try graph.successors.append(graph.allocator, successor);
        graph.nodes.items[node_index].successor_len += 1;
        try self.ensureReadBeforeRebindNode(graph, work, successor);
    }

    fn noteReadBeforeRebindLocal(reads: *std.bit_set.DynamicBitSetUnmanaged, local: LIR.LocalId) void {
        reads.set(@intFromEnum(local));
    }

    fn noteReadBeforeRebindSpan(self: *Inserter, reads: *std.bit_set.DynamicBitSetUnmanaged, span: LIR.LocalSpan) void {
        for (self.store.getLocalSpan(span)) |local| {
            noteReadBeforeRebindLocal(reads, local);
        }
    }

    fn noteReadBeforeRebindRefOp(reads: *std.bit_set.DynamicBitSetUnmanaged, op: LIR.RefOp) void {
        noteReadBeforeRebindLocal(reads, refOpSource(op));
    }

    fn setReadBeforeRebindDef(
        graph: *ReadBeforeRebindGraph,
        node_index: usize,
        local: LIR.LocalId,
    ) void {
        graph.nodes.items[node_index].def = local;
    }

    fn computeReadsBeforeRebind(
        self: *Inserter,
        start: LIR.CFStmtId,
        loop_keep: ?*const OwnedSet,
        loop_keep_id: u32,
    ) ResourceError!*const std.bit_set.DynamicBitSetUnmanaged {
        const key = ReadBeforeRebindKey{
            .start = start,
            .loop_keep_id = loop_keep_id,
        };
        if (self.reads_before_rebind_cache.getPtr(key)) |cached| return cached;

        var graph_arena = std.heap.ArenaAllocator.init(self.store.allocator);
        defer graph_arena.deinit();
        const graph_allocator = graph_arena.allocator();

        var graph = ReadBeforeRebindGraph.init(graph_allocator);
        var work = std.ArrayList(LIR.CFStmtId).empty;

        // Without a loop keep-set, start from the whole proc so one dataflow
        // run answers most repeated local/group liveness questions. With a
        // loop keep-set, the keep-set semantics belong to one loop body, so
        // root the graph at the queried statement instead of applying that
        // loop's exits to unrelated loops elsewhere in the proc.
        if (loop_keep == null) {
            try self.ensureReadBeforeRebindNode(&graph, &work, self.current_proc_body);
        }
        try self.ensureReadBeforeRebindNode(&graph, &work, start);

        while (work.pop()) |stmt| {
            const node_index = graph.indices.get(stmt) orelse unreachable;

            switch (self.store.getCFStmt(stmt)) {
                .assign_ref => |assign| {
                    noteReadBeforeRebindRefOp(&graph.nodes.items[node_index].reads, assign.op);
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
                    self.noteReadBeforeRebindSpan(&graph.nodes.items[node_index].reads, assign.args);
                    setReadBeforeRebindDef(&graph, node_index, assign.target);
                    try self.appendReadBeforeRebindSuccessor(&graph, &work, node_index, assign.next);
                },
                .assign_call_erased => |assign| {
                    noteReadBeforeRebindLocal(&graph.nodes.items[node_index].reads, assign.closure);
                    self.noteReadBeforeRebindSpan(&graph.nodes.items[node_index].reads, assign.args);
                    setReadBeforeRebindDef(&graph, node_index, assign.target);
                    try self.appendReadBeforeRebindSuccessor(&graph, &work, node_index, assign.next);
                },
                .assign_packed_erased_fn => |assign| {
                    if (assign.capture) |capture| noteReadBeforeRebindLocal(&graph.nodes.items[node_index].reads, capture);
                    setReadBeforeRebindDef(&graph, node_index, assign.target);
                    try self.appendReadBeforeRebindSuccessor(&graph, &work, node_index, assign.next);
                },
                .assign_low_level => |assign| {
                    self.noteReadBeforeRebindSpan(&graph.nodes.items[node_index].reads, assign.args);
                    setReadBeforeRebindDef(&graph, node_index, assign.target);
                    try self.appendReadBeforeRebindSuccessor(&graph, &work, node_index, assign.next);
                },
                .assign_list => |assign| {
                    self.noteReadBeforeRebindSpan(&graph.nodes.items[node_index].reads, assign.elems);
                    setReadBeforeRebindDef(&graph, node_index, assign.target);
                    try self.appendReadBeforeRebindSuccessor(&graph, &work, node_index, assign.next);
                },
                .assign_struct => |assign| {
                    self.noteReadBeforeRebindSpan(&graph.nodes.items[node_index].reads, assign.fields);
                    setReadBeforeRebindDef(&graph, node_index, assign.target);
                    try self.appendReadBeforeRebindSuccessor(&graph, &work, node_index, assign.next);
                },
                .assign_tag => |assign| {
                    if (assign.payload) |payload| noteReadBeforeRebindLocal(&graph.nodes.items[node_index].reads, payload);
                    setReadBeforeRebindDef(&graph, node_index, assign.target);
                    try self.appendReadBeforeRebindSuccessor(&graph, &work, node_index, assign.next);
                },
                .set_local => |assign| {
                    noteReadBeforeRebindLocal(&graph.nodes.items[node_index].reads, assign.value);
                    setReadBeforeRebindDef(&graph, node_index, assign.target);
                    try self.appendReadBeforeRebindSuccessor(&graph, &work, node_index, assign.next);
                },
                .debug => |debug_stmt| {
                    noteReadBeforeRebindLocal(&graph.nodes.items[node_index].reads, debug_stmt.message);
                    try self.appendReadBeforeRebindSuccessor(&graph, &work, node_index, debug_stmt.next);
                },
                .expect => |expect_stmt| {
                    noteReadBeforeRebindLocal(&graph.nodes.items[node_index].reads, expect_stmt.condition);
                    try self.appendReadBeforeRebindSuccessor(&graph, &work, node_index, expect_stmt.next);
                },
                .expect_err => |expect_err_stmt| {
                    noteReadBeforeRebindLocal(&graph.nodes.items[node_index].reads, expect_err_stmt.message);
                },
                .incref => |rc| {
                    noteReadBeforeRebindLocal(&graph.nodes.items[node_index].reads, rc.value);
                    try self.appendReadBeforeRebindSuccessor(&graph, &work, node_index, rc.next);
                },
                .decref => |rc| {
                    noteReadBeforeRebindLocal(&graph.nodes.items[node_index].reads, rc.value);
                    try self.appendReadBeforeRebindSuccessor(&graph, &work, node_index, rc.next);
                },
                .decref_if_initialized => |rc| {
                    noteReadBeforeRebindLocal(&graph.nodes.items[node_index].reads, rc.cond);
                    noteReadBeforeRebindLocal(&graph.nodes.items[node_index].reads, rc.value);
                    try self.appendReadBeforeRebindSuccessor(&graph, &work, node_index, rc.next);
                },
                .free => |rc| {
                    noteReadBeforeRebindLocal(&graph.nodes.items[node_index].reads, rc.value);
                    try self.appendReadBeforeRebindSuccessor(&graph, &work, node_index, rc.next);
                },
                .switch_stmt => |switch_stmt| {
                    noteReadBeforeRebindLocal(&graph.nodes.items[node_index].reads, switch_stmt.cond);
                    if (switch_stmt.continuation) |continuation| {
                        try self.appendReadBeforeRebindSuccessor(&graph, &work, node_index, continuation);
                    }
                    try self.appendReadBeforeRebindSuccessor(&graph, &work, node_index, switch_stmt.default_branch);
                    for (self.store.getCFSwitchBranches(switch_stmt.branches)) |branch| {
                        try self.appendReadBeforeRebindSuccessor(&graph, &work, node_index, branch.body);
                    }
                },
                .switch_initialized_payload => |switch_stmt| {
                    noteReadBeforeRebindLocal(&graph.nodes.items[node_index].reads, switch_stmt.cond);
                    noteReadBeforeRebindLocal(&graph.nodes.items[node_index].reads, switch_stmt.payload);
                    try self.appendReadBeforeRebindSuccessor(&graph, &work, node_index, switch_stmt.initialized_branch);
                    try self.appendReadBeforeRebindSuccessor(&graph, &work, node_index, switch_stmt.uninitialized_branch);
                },
                .str_match => |str_match| {
                    noteReadBeforeRebindLocal(&graph.nodes.items[node_index].reads, str_match.source);
                    try self.appendReadBeforeRebindSuccessor(&graph, &work, node_index, str_match.on_match);
                    try self.appendReadBeforeRebindSuccessor(&graph, &work, node_index, str_match.on_miss);
                },
                .str_match_set => |str_match_set| {
                    noteReadBeforeRebindLocal(&graph.nodes.items[node_index].reads, str_match_set.source);
                    for (self.store.getStrMatchArms(str_match_set.arms)) |arm| {
                        try self.appendReadBeforeRebindSuccessor(&graph, &work, node_index, arm.on_match);
                    }
                    try self.appendReadBeforeRebindSuccessor(&graph, &work, node_index, str_match_set.on_miss);
                },
                .join => |join_stmt| {
                    // Entering a join statement itself continues with the
                    // remainder. The body is not a normal successor; it only
                    // runs through `.jump`, whose transfer semantics are
                    // modeled by entering the collected body below. Still add
                    // the body as an independent root so direct queries for
                    // `groupUsedInPath(join.body, ...)` are cached by this run.
                    try self.ensureReadBeforeRebindNode(&graph, &work, join_stmt.body);
                    try self.appendReadBeforeRebindSuccessor(&graph, &work, node_index, join_stmt.remainder);
                },
                .jump => |jump_stmt| {
                    const join_bodies = self.join_bodies orelse arcInvariant("ARC read-before-rebind reached jump without collected join bodies");
                    const target_body = join_bodies.get(jump_stmt.target) orelse arcInvariant("ARC read-before-rebind reached jump to unknown join point");
                    try self.appendReadBeforeRebindSuccessor(&graph, &work, node_index, target_body);
                },
                .ret => |ret_stmt| {
                    noteReadBeforeRebindLocal(&graph.nodes.items[node_index].reads, ret_stmt.value);
                },
                .loop_continue,
                .loop_break,
                => if (loop_keep) |keep| {
                    graph.nodes.items[node_index].reads.setUnion(keep.bits);
                },
                .runtime_error,
                .comptime_exhaustiveness_failed,
                .crash,
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
                const successor_index = graph.indices.get(successor) orelse unreachable;
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
                const successor_index = graph.indices.get(successor) orelse unreachable;
                const write_index = pred_writes[successor_index];
                predecessors[write_index] = predecessor_index;
                pred_writes[successor_index] += 1;
            }
        }

        var scratch = try std.bit_set.DynamicBitSetUnmanaged.initEmpty(graph_allocator, self.store.locals.items.len);
        var in_work = try std.bit_set.DynamicBitSetUnmanaged.initEmpty(graph_allocator, node_count);
        var node_work = std.ArrayList(usize).empty;
        try node_work.ensureTotalCapacity(graph_allocator, node_count);
        for (0..node_count) |node_index| {
            node_work.appendAssumeCapacity(node_index);
            in_work.set(node_index);
        }

        while (node_work.pop()) |node_index| {
            in_work.unset(node_index);
            const node = &graph.nodes.items[node_index];

            scratch.unsetAll();
            const successor_start = node.successor_start;
            const successor_end = successor_start + @as(usize, node.successor_len);
            for (graph.successors.items[successor_start..successor_end]) |successor| {
                const successor_index = graph.indices.get(successor) orelse unreachable;
                scratch.setUnion(graph.nodes.items[successor_index].exposed);
            }
            if (node.def) |local| {
                scratch.unset(@intFromEnum(local));
            }
            scratch.setUnion(node.reads);

            if (!node.exposed.eql(scratch)) {
                node.exposed.unsetAll();
                node.exposed.setUnion(scratch);

                const pred_start = pred_starts[node_index];
                const pred_end = pred_starts[node_index + 1];
                for (predecessors[pred_start..pred_end]) |predecessor_index| {
                    if (in_work.isSet(predecessor_index)) continue;
                    try node_work.append(graph_allocator, predecessor_index);
                    in_work.set(predecessor_index);
                }
            }
        }

        for (graph.nodes.items) |node| {
            const node_key = ReadBeforeRebindKey{
                .start = node.stmt,
                .loop_keep_id = loop_keep_id,
            };
            if (self.reads_before_rebind_cache.contains(node_key)) continue;
            const cached = try node.exposed.clone(self.read_cache_allocator);
            try self.reads_before_rebind_cache.put(node_key, cached);
        }

        return self.reads_before_rebind_cache.getPtr(key) orelse
            arcInvariant("ARC read-before-rebind cache did not include requested start");
    }

    fn localValueUsedInPath(
        self: *Inserter,
        start: LIR.CFStmtId,
        needle: LIR.LocalId,
        loop_keep: ?*const OwnedSet,
    ) ResourceError!bool {
        var visited = self.scan_visited;
        visited.clearRetainingCapacity();
        var stack = self.scan_stack;
        stack.clearRetainingCapacity();
        try stack.append(self.store.allocator, start);

        while (stack.pop()) |current| {
            if (visited.contains(current)) continue;
            try visited.put(current, {});

            const stmt = self.store.getCFStmt(current);
            switch (stmt) {
                .assign_ref => |assign| {
                    if (refOpUsesLocal(assign.op, needle)) return true;
                    if (assign.target == needle) continue;
                    try stack.append(self.store.allocator, assign.next);
                },
                .assign_literal => |assign| {
                    if (assign.target == needle) continue;
                    try stack.append(self.store.allocator, assign.next);
                },
                .init_uninitialized => |uninit| {
                    if (uninit.target == needle) continue;
                    try stack.append(self.store.allocator, uninit.next);
                },
                .assign_call => |assign| {
                    if (self.spanUsesLocal(assign.args, needle)) return true;
                    if (assign.target == needle) continue;
                    try stack.append(self.store.allocator, assign.next);
                },
                .assign_call_erased => |assign| {
                    if (assign.closure == needle or self.spanUsesLocal(assign.args, needle)) return true;
                    if (assign.target == needle) continue;
                    try stack.append(self.store.allocator, assign.next);
                },
                .assign_packed_erased_fn => |assign| {
                    if (assign.capture != null and assign.capture.? == needle) return true;
                    if (assign.target == needle) continue;
                    try stack.append(self.store.allocator, assign.next);
                },
                .assign_low_level => |assign| {
                    if (self.spanUsesLocal(assign.args, needle)) return true;
                    if (assign.target == needle) continue;
                    try stack.append(self.store.allocator, assign.next);
                },
                .assign_list => |assign| {
                    if (self.spanUsesLocal(assign.elems, needle)) return true;
                    if (assign.target == needle) continue;
                    try stack.append(self.store.allocator, assign.next);
                },
                .assign_struct => |assign| {
                    if (self.spanUsesLocal(assign.fields, needle)) return true;
                    if (assign.target == needle) continue;
                    try stack.append(self.store.allocator, assign.next);
                },
                .assign_tag => |assign| {
                    if (assign.payload != null and assign.payload.? == needle) return true;
                    if (assign.target == needle) continue;
                    try stack.append(self.store.allocator, assign.next);
                },
                .set_local => |assign| {
                    if (assign.value == needle) return true;
                    if (assign.target == needle) continue;
                    try stack.append(self.store.allocator, assign.next);
                },
                .debug => |debug_stmt| {
                    if (debug_stmt.message == needle) return true;
                    try stack.append(self.store.allocator, debug_stmt.next);
                },
                .expect => |expect_stmt| {
                    if (expect_stmt.condition == needle) return true;
                    try stack.append(self.store.allocator, expect_stmt.next);
                },
                .expect_err => |expect_err_stmt| {
                    if (expect_err_stmt.message == needle) return true;
                },
                .switch_stmt => |switch_stmt| {
                    if (switch_stmt.cond == needle) return true;
                    if (switch_stmt.continuation) |continuation| {
                        try stack.append(self.store.allocator, continuation);
                    }
                    try stack.append(self.store.allocator, switch_stmt.default_branch);
                    for (self.store.getCFSwitchBranches(switch_stmt.branches)) |branch| {
                        try stack.append(self.store.allocator, branch.body);
                    }
                },
                .switch_initialized_payload => |switch_stmt| {
                    if (switch_stmt.cond == needle or switch_stmt.payload == needle) return true;
                    try stack.append(self.store.allocator, switch_stmt.initialized_branch);
                    try stack.append(self.store.allocator, switch_stmt.uninitialized_branch);
                },
                .str_match => |str_match| {
                    if (str_match.source == needle) return true;
                    var defines_needle_on_match = false;
                    for (self.store.getStrMatchSteps(str_match.steps)) |step| {
                        switch (step.capture) {
                            .discard => {},
                            .view => |local| {
                                if (local == needle) defines_needle_on_match = true;
                            },
                        }
                    }
                    if (!defines_needle_on_match) try stack.append(self.store.allocator, str_match.on_match);
                    try stack.append(self.store.allocator, str_match.on_miss);
                },
                .str_match_set => |str_match_set| {
                    if (str_match_set.source == needle) return true;
                    for (self.store.getStrMatchArms(str_match_set.arms)) |arm| {
                        var defines_needle_on_match = false;
                        for (self.store.getStrMatchSteps(arm.steps)) |step| {
                            switch (step.capture) {
                                .discard => {},
                                .view => |local| {
                                    if (local == needle) defines_needle_on_match = true;
                                },
                            }
                        }
                        if (!defines_needle_on_match) try stack.append(self.store.allocator, arm.on_match);
                    }
                    try stack.append(self.store.allocator, str_match_set.on_miss);
                },
                .join => |join_stmt| {
                    // A join body runs only via jumps to it, and the `.jump`
                    // case enters bodies through the collected map. Entering
                    // a body here would skip the jump site's parameter
                    // rebinds, manufacturing next-activation uses of a
                    // parameter this activation's value never sees.
                    try stack.append(self.store.allocator, join_stmt.remainder);
                },
                .jump => |jump_stmt| {
                    const join_bodies = self.join_bodies orelse arcInvariant("ARC liveness reached jump without collected join bodies");
                    const target_body = join_bodies.get(jump_stmt.target) orelse arcInvariant("ARC liveness reached jump to unknown join point");
                    try stack.append(self.store.allocator, target_body);
                },
                .ret => |ret_stmt| if (ret_stmt.value == needle) return true,
                .loop_continue,
                .loop_break,
                => if (loop_keep) |keep| {
                    if (keep.contains(needle)) return true;
                },
                .runtime_error,
                .comptime_exhaustiveness_failed,
                .crash,
                => {},
                .comptime_branch_taken => |marker| try stack.append(self.store.allocator, marker.next),
                .incref => |rc| try stack.append(self.store.allocator, rc.next),
                .decref => |rc| try stack.append(self.store.allocator, rc.next),
                .decref_if_initialized => |rc| {
                    if (rc.cond == needle or rc.value == needle) return true;
                    try stack.append(self.store.allocator, rc.next);
                },
                .free => |rc| try stack.append(self.store.allocator, rc.next),
            }
        }

        return false;
    }

    fn spanUsesLocal(self: *Inserter, span: LIR.LocalSpan, needle: LIR.LocalId) bool {
        for (self.store.getLocalSpan(span)) |local| {
            if (local == needle) return true;
        }
        return false;
    }

    /// Liveness for one owned local extended over its borrow group: the
    /// local's value must stay live while the local itself or any borrow
    /// anchored on it is still used.
    fn groupUsedInPath(
        self: *Inserter,
        start: LIR.CFStmtId,
        local: LIR.LocalId,
        loop_keep: ?*const OwnedSet,
    ) ResourceError!bool {
        const members = self.solution.groupMembers(self.solution.leaderOf(local));
        if (members.len <= 1) {
            const loop_keep_id: ?u32 = if (loop_keep) |keep|
                self.active_loop_keep_ids.get(@intFromPtr(keep))
            else
                0;
            if (loop_keep_id) |keep_id| {
                const reads = try self.computeReadsBeforeRebind(start, loop_keep, keep_id);
                return reads.isSet(@intFromEnum(local));
            }
            return self.localValueUsedInPath(start, local, loop_keep);
        }
        for (members) |member| {
            self.scan_needles.set(@enumFromInt(member));
        }
        defer for (members) |member| {
            self.scan_needles.unset(@enumFromInt(member));
        };
        return self.anyNeedleUsedInPath(start, loop_keep);
    }

    /// Multi-needle variant of `localValueUsedInPath` over the scratch
    /// needle set. Group members are bound exactly once (the solver excludes
    /// multi-bound locals from borrow groups), so rebinding never invalidates
    /// a needle mid-scan; re-encountering a defining statement on a loop back
    /// edge conservatively counts later reads as uses.
    fn anyNeedleUsedInPath(
        self: *Inserter,
        start: LIR.CFStmtId,
        loop_keep: ?*const OwnedSet,
    ) ResourceError!bool {
        const needles = self.scan_needles;
        var visited = self.scan_visited;
        visited.clearRetainingCapacity();
        var stack = self.scan_stack;
        stack.clearRetainingCapacity();
        try stack.append(self.store.allocator, start);

        while (stack.pop()) |current| {
            if (visited.contains(current)) continue;
            try visited.put(current, {});

            const stmt = self.store.getCFStmt(current);
            switch (stmt) {
                .assign_ref => |assign| {
                    if (refOpUsesAny(assign.op, needles)) return true;
                    try stack.append(self.store.allocator, assign.next);
                },
                .assign_literal => |assign| {
                    try stack.append(self.store.allocator, assign.next);
                },
                .init_uninitialized => |uninit| {
                    try stack.append(self.store.allocator, uninit.next);
                },
                .assign_call => |assign| {
                    if (self.spanUsesAny(assign.args, needles)) return true;
                    try stack.append(self.store.allocator, assign.next);
                },
                .assign_call_erased => |assign| {
                    if (needles.contains(assign.closure) or self.spanUsesAny(assign.args, needles)) return true;
                    try stack.append(self.store.allocator, assign.next);
                },
                .assign_packed_erased_fn => |assign| {
                    if (assign.capture != null and needles.contains(assign.capture.?)) return true;
                    try stack.append(self.store.allocator, assign.next);
                },
                .assign_low_level => |assign| {
                    if (self.spanUsesAny(assign.args, needles)) return true;
                    try stack.append(self.store.allocator, assign.next);
                },
                .assign_list => |assign| {
                    if (self.spanUsesAny(assign.elems, needles)) return true;
                    try stack.append(self.store.allocator, assign.next);
                },
                .assign_struct => |assign| {
                    if (self.spanUsesAny(assign.fields, needles)) return true;
                    try stack.append(self.store.allocator, assign.next);
                },
                .assign_tag => |assign| {
                    if (assign.payload != null and needles.contains(assign.payload.?)) return true;
                    try stack.append(self.store.allocator, assign.next);
                },
                .set_local => |assign| {
                    if (needles.contains(assign.value)) return true;
                    try stack.append(self.store.allocator, assign.next);
                },
                .debug => |debug_stmt| {
                    if (needles.contains(debug_stmt.message)) return true;
                    try stack.append(self.store.allocator, debug_stmt.next);
                },
                .expect => |expect_stmt| {
                    if (needles.contains(expect_stmt.condition)) return true;
                    try stack.append(self.store.allocator, expect_stmt.next);
                },
                .expect_err => |expect_err_stmt| {
                    if (needles.contains(expect_err_stmt.message)) return true;
                },
                .switch_stmt => |switch_stmt| {
                    if (needles.contains(switch_stmt.cond)) return true;
                    if (switch_stmt.continuation) |continuation| {
                        try stack.append(self.store.allocator, continuation);
                    }
                    try stack.append(self.store.allocator, switch_stmt.default_branch);
                    for (self.store.getCFSwitchBranches(switch_stmt.branches)) |branch| {
                        try stack.append(self.store.allocator, branch.body);
                    }
                },
                .switch_initialized_payload => |switch_stmt| {
                    if (needles.contains(switch_stmt.cond) or needles.contains(switch_stmt.payload)) return true;
                    try stack.append(self.store.allocator, switch_stmt.initialized_branch);
                    try stack.append(self.store.allocator, switch_stmt.uninitialized_branch);
                },
                .str_match => |str_match| {
                    if (needles.contains(str_match.source)) return true;
                    try stack.append(self.store.allocator, str_match.on_match);
                    try stack.append(self.store.allocator, str_match.on_miss);
                },
                .str_match_set => |str_match_set| {
                    if (needles.contains(str_match_set.source)) return true;
                    for (self.store.getStrMatchArms(str_match_set.arms)) |arm| {
                        try stack.append(self.store.allocator, arm.on_match);
                    }
                    try stack.append(self.store.allocator, str_match_set.on_miss);
                },
                .join => |join_stmt| {
                    // Bodies enter via the `.jump` case only, exactly as in
                    // `localValueUsedInPath`.
                    try stack.append(self.store.allocator, join_stmt.remainder);
                },
                .jump => |jump_stmt| {
                    const join_bodies = self.join_bodies orelse arcInvariant("ARC liveness reached jump without collected join bodies");
                    const target_body = join_bodies.get(jump_stmt.target) orelse arcInvariant("ARC liveness reached jump to unknown join point");
                    try stack.append(self.store.allocator, target_body);
                },
                .ret => |ret_stmt| if (needles.contains(ret_stmt.value)) return true,
                .loop_continue,
                .loop_break,
                => if (loop_keep) |keep| {
                    var iter = needles.bits.iterator(.{});
                    while (iter.next()) |i| {
                        if (keep.bits.isSet(i)) return true;
                    }
                },
                .runtime_error,
                .comptime_exhaustiveness_failed,
                .crash,
                => {},
                .comptime_branch_taken => |marker| try stack.append(self.store.allocator, marker.next),
                .incref => |rc| try stack.append(self.store.allocator, rc.next),
                .decref => |rc| try stack.append(self.store.allocator, rc.next),
                .decref_if_initialized => |rc| {
                    if (needles.contains(rc.cond) or needles.contains(rc.value)) return true;
                    try stack.append(self.store.allocator, rc.next);
                },
                .free => |rc| try stack.append(self.store.allocator, rc.next),
            }
        }

        return false;
    }

    fn spanUsesAny(self: *Inserter, span: LIR.LocalSpan, needles: *const OwnedSet) bool {
        for (self.store.getLocalSpan(span)) |local| {
            if (needles.contains(local)) return true;
        }
        return false;
    }

    fn retainSpanExcept(self: *Inserter, span: LIR.LocalSpan, skip_mask: u64, next: LIR.CFStmtId) ResourceError!LIR.CFStmtId {
        var current = next;
        const locals = self.store.getLocalSpan(span);
        var i = locals.len;
        while (i > 0) {
            i -= 1;
            if (i < 64 and (skip_mask & argMaskBit(i)) != 0) continue;
            current = try self.retainLocalIfRc(locals[i], current);
        }
        return current;
    }

    fn releaseAll(self: *Inserter, owned: *const OwnedSet, next: LIR.CFStmtId) ResourceError!LIR.CFStmtId {
        var keep = try OwnedSet.init(self.store.allocator, owned.len());
        defer keep.deinit();
        return try self.releaseDifference(owned, &keep, next);
    }

    fn releaseDifference(self: *Inserter, owned: *const OwnedSet, keep: *const OwnedSet, next: LIR.CFStmtId) ResourceError!LIR.CFStmtId {
        var current = next;
        var iter = owned.bits.iterator(.{ .direction = .reverse });
        while (iter.next()) |i| {
            if (keep.bits.isSet(i)) continue;
            const local: LIR.LocalId = @enumFromInt(@as(u32, @intCast(i)));
            if (self.solution.maybeUninitializedCondition(local)) |condition| {
                current = try self.releaseMaybeInitializedLocal(condition.local, condition.mask, local, current);
            } else {
                current = try self.releaseLocalIfRc(local, current);
            }
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
        return try self.store.addCFStmt(.{ .incref = .{
            .value = local,
            .rc = rc,
            .count = count,
            .atomicity = self.rcAtomicity(local),
            .next = next,
        } });
    }

    fn retainStrMatchSourceForCaptures(self: *Inserter, source: LIR.LocalId, steps: LIR.StrMatchStepSpan, next: LIR.CFStmtId) ResourceError!LIR.CFStmtId {
        var count: u16 = 0;
        for (self.store.getStrMatchSteps(steps)) |step| {
            switch (step.capture) {
                .discard => {},
                .view => |local| {
                    if (self.localContainsRefcounted(local) and !self.isBindingBorrowed(local)) {
                        count +|= 1;
                    }
                },
            }
        }
        return try self.retainLocalIfRcCount(source, count, next);
    }

    fn releaseLocalIfRc(self: *Inserter, local: LIR.LocalId, next: LIR.CFStmtId) ResourceError!LIR.CFStmtId {
        if (!self.localContainsRefcounted(local)) return next;
        const rc = self.rcHelperForLocal(.decref, local);
        return try self.store.addCFStmt(.{ .decref = .{
            .value = local,
            .rc = rc,
            .atomicity = self.rcAtomicity(local),
            .next = next,
        } });
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

    fn rcHelperForLocal(self: *const Inserter, op: layout_mod.RcOp, local: LIR.LocalId) layout_mod.RcHelper {
        const local_layout = self.store.getLocal(local).layout_idx;
        const helper = self.rcHelperForLayout(op, local_layout);
        if (self.layouts.rcHelperPlan(helper) == .noop) {
            arcInvariant("ARC attempted to emit a noop RC helper for a refcounted local");
        }
        return helper;
    }

    fn rcHelperForLayout(self: *const Inserter, op: layout_mod.RcOp, layout_idx: layout_mod.Idx) layout_mod.RcHelper {
        const layout_val = self.layouts.getLayout(layout_idx);
        return switch (layout_val.tag) {
            .closure => self.rcHelperForLayout(nestedDropOp(op), layout_val.getClosure().captures_layout_idx),
            else => .{ .op = op, .layout_idx = layout_idx },
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
};

const JoinBodyMap = std.AutoHashMap(LIR.JoinPointId, LIR.CFStmtId);
const FinalJoinMap = std.AutoArrayHashMapUnmanaged(LIR.JoinPointId, LIR.JoinPoint);

fn joinPointLessThan(_: void, a: LIR.JoinPoint, b: LIR.JoinPoint) bool {
    return @intFromEnum(a.id) < @intFromEnum(b.id);
}

fn joinPointEql(a: LIR.JoinPoint, b: LIR.JoinPoint) bool {
    return a.id == b.id and a.body == b.body and localSpanEql(a.params, b.params);
}

fn localSpanEql(a: LIR.LocalSpan, b: LIR.LocalSpan) bool {
    return a.start == b.start and a.len == b.len;
}

const RewrittenJoin = struct {
    keep: OwnedSet,
    stmt: LIR.CFStmtId,
};

const RewrittenJoinMap = std.AutoHashMap(LIR.CFStmtId, RewrittenJoin);

const OwnedSet = struct {
    allocator: std.mem.Allocator,
    bits: std.bit_set.DynamicBitSetUnmanaged,

    fn init(allocator: std.mem.Allocator, bit_len: usize) ResourceError!OwnedSet {
        const bits = try std.bit_set.DynamicBitSetUnmanaged.initEmpty(allocator, bit_len);
        return .{ .allocator = allocator, .bits = bits };
    }

    fn deinit(self: *OwnedSet) void {
        self.bits.deinit(self.allocator);
        self.bits = .{};
    }

    fn clone(self: *const OwnedSet) ResourceError!OwnedSet {
        const bits = try self.bits.clone(self.allocator);
        return .{ .allocator = self.allocator, .bits = bits };
    }

    fn len(self: *const OwnedSet) usize {
        return self.bits.capacity();
    }

    fn set(self: *OwnedSet, local: LIR.LocalId) void {
        self.bits.set(@intFromEnum(local));
    }

    fn unset(self: *OwnedSet, local: LIR.LocalId) void {
        self.bits.unset(@intFromEnum(local));
    }

    fn contains(self: *const OwnedSet, local: LIR.LocalId) bool {
        return self.bits.isSet(@intFromEnum(local));
    }

    fn eql(self: *const OwnedSet, other: *const OwnedSet) bool {
        if (self.len() != other.len()) return false;
        return self.bits.eql(other.bits);
    }

    fn intersect(self: *OwnedSet, other: *const OwnedSet) void {
        if (self.len() != other.len()) arcInvariant("ARC owned-set intersection length mismatch");
        self.bits.setIntersection(other.bits);
    }

    fn unionWith(self: *OwnedSet, other: *const OwnedSet) void {
        if (self.len() != other.len()) arcInvariant("ARC owned-set union length mismatch");
        self.bits.setUnion(other.bits);
    }
};

fn ownedSetDigest(owned: *const OwnedSet) u64 {
    var hasher = std.hash.Wyhash.init(0x6172635f616e616c);
    var iter = owned.bits.iterator(.{});
    while (iter.next()) |index| {
        const local_index: u32 = @intCast(index);
        hasher.update(std.mem.asBytes(&local_index));
    }
    return hasher.final();
}

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

fn refOpUsesLocal(op: LIR.RefOp, needle: LIR.LocalId) bool {
    return switch (op) {
        .local => |local| local == needle,
        .discriminant => |ref| ref.source == needle,
        .field => |ref| ref.source == needle,
        .tag_payload => |ref| ref.source == needle,
        .tag_payload_struct => |ref| ref.source == needle,
        .list_reinterpret => |ref| ref.backing_ref == needle,
        .nominal => |ref| ref.backing_ref == needle,
    };
}

fn refOpUsesAny(op: LIR.RefOp, needles: *const OwnedSet) bool {
    return switch (op) {
        .local => |local| needles.contains(local),
        .discriminant => |ref| needles.contains(ref.source),
        .field => |ref| needles.contains(ref.source),
        .tag_payload => |ref| needles.contains(ref.source),
        .tag_payload_struct => |ref| needles.contains(ref.source),
        .list_reinterpret => |ref| needles.contains(ref.backing_ref),
        .nominal => |ref| needles.contains(ref.backing_ref),
    };
}

fn argMaskBit(index: usize) u64 {
    if (index >= 64) arcInvariant("ARC low-level runtime mutation argument mask exceeded 64 args");
    return @as(u64, 1) << @as(u6, @intCast(index));
}

fn arcInvariant(comptime message: []const u8) noreturn {
    if (@import("builtin").mode == .Debug) std.debug.panic(message, .{});
    unreachable;
}

test "arc insertion boundary exists" {
    std.testing.refAllDecls(@This());
}

const testing = std.testing;

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
        return try self.store.addProcSpec(.{
            .name = self.store.freshSyntheticSymbol(),
            .args = try self.span(args),
            .body = body,
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
        return try self.store.addCFStmt(.{ .crash = .{ .msg = try self.store.insertString(message) } });
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

    fn procBody(self: *const ArcTest) LIR.CFStmtId {
        for (self.store.proc_specs.items) |proc| {
            if (proc.body) |body| return body;
        }
        arcInvariant("ARC test fixture has no procedure body");
    }

    fn joinBody(self: *const ArcTest, join_id: LIR.JoinPointId) LIR.CFStmtId {
        var found: ?LIR.CFStmtId = null;
        for (self.store.cf_stmts.items) |stmt| {
            switch (stmt) {
                .join => |join_stmt| {
                    if (join_stmt.id == join_id) found = join_stmt.body;
                },
                else => {},
            }
        }
        return found orelse arcInvariant("ARC test fixture has no matching join body");
    }

    fn countRc(self: *const ArcTest, local_id: LIR.LocalId, kind: RcKind) usize {
        var count: usize = 0;
        for (self.store.cf_stmts.items) |stmt| {
            switch (stmt) {
                .incref => |rc| {
                    if (kind == .incref and rc.value == local_id) count += 1;
                },
                .decref => |rc| {
                    if (kind == .decref and rc.value == local_id) count += 1;
                },
                .decref_if_initialized => |rc| {
                    if (kind == .decref and rc.value == local_id) count += 1;
                },
                .free => |rc| {
                    if (kind == .free and rc.value == local_id) count += 1;
                },
                else => {},
            }
        }
        return count;
    }

    const ExpectError = error{ TestExpectedEqual, TestUnexpectedResult };

    fn expectRcAtomicity(self: *const ArcTest, local_id: LIR.LocalId, expected: LIR.RcAtomicity) ExpectError!void {
        var seen: usize = 0;
        for (self.store.cf_stmts.items) |stmt| {
            const found: LIR.RcAtomicity = switch (stmt) {
                .incref => |rc| if (rc.value == local_id) rc.atomicity else continue,
                .decref => |rc| if (rc.value == local_id) rc.atomicity else continue,
                .decref_if_initialized => |rc| if (rc.value == local_id) rc.atomicity else continue,
                .free => |rc| if (rc.value == local_id) rc.atomicity else continue,
                else => continue,
            };
            seen += 1;
            try testing.expectEqual(expected, found);
        }
        try testing.expect(seen > 0);
    }

    fn uniqueArgsFor(self: *const ArcTest, target: LIR.LocalId) u64 {
        var mask: u64 = 0;
        for (self.store.cf_stmts.items) |stmt| {
            switch (stmt) {
                .assign_low_level => |assign| {
                    if (assign.target == target) mask |= assign.unique_args;
                },
                else => {},
            }
        }
        return mask;
    }

    /// Like `uniqueArgsFor`, but restricted to statements reachable from one
    /// proc's body, so a variant and its base proc (which share locals) can
    /// be asserted separately.
    fn uniqueArgsInProc(self: *const ArcTest, proc_id: LIR.LirProcSpecId, target: LIR.LocalId) Allocator.Error!u64 {
        var mask: u64 = 0;
        var visited = std.AutoHashMap(LIR.CFStmtId, void).init(self.allocator);
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
                    for (self.store.getCFSwitchBranches(s.branches)) |branch| {
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
                    for (self.store.getStrMatchArms(s.arms)) |arm| {
                        try stack.append(self.allocator, arm.on_match);
                    }
                    try stack.append(self.allocator, s.on_miss);
                },
                .join => |j| {
                    try stack.append(self.allocator, j.body);
                    try stack.append(self.allocator, j.remainder);
                },
                inline .assign_ref, .assign_literal, .init_uninitialized, .assign_call, .assign_call_erased, .assign_packed_erased_fn, .assign_list, .assign_struct, .assign_tag, .set_local, .debug, .expect, .comptime_branch_taken, .incref, .decref, .decref_if_initialized, .free => |s| {
                    try stack.append(self.allocator, s.next);
                },
                .ret, .jump, .crash, .expect_err, .runtime_error, .comptime_exhaustiveness_failed, .loop_continue, .loop_break => {},
            }
        }
        return mask;
    }

    fn countAllRc(self: *const ArcTest) usize {
        var count: usize = 0;
        for (self.store.cf_stmts.items) |stmt| {
            switch (stmt) {
                .incref, .decref, .decref_if_initialized, .free => count += 1,
                else => {},
            }
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
        var remaining: usize = self.store.cf_stmts.items.len + 1;
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
                .assign_low_level => |assign| cursor = assign.next,
                .assign_list => |assign| cursor = assign.next,
                .assign_struct => |assign| cursor = assign.next,
                .assign_tag => |assign| cursor = assign.next,
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
                .expect_err, .runtime_error, .comptime_exhaustiveness_failed, .switch_stmt, .switch_initialized_payload, .str_match, .str_match_set, .loop_continue, .loop_break, .join, .jump => return error.NonLinearPath,
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
        var remaining: usize = self.store.cf_stmts.items.len + 1;
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
                .assign_low_level => |assign| cursor = assign.next,
                .assign_list => |assign| cursor = assign.next,
                .assign_struct => |assign| cursor = assign.next,
                .assign_tag => |assign| cursor = assign.next,
                .debug => |debug_stmt| cursor = debug_stmt.next,
                .expect => |expect_stmt| cursor = expect_stmt.next,
                .comptime_branch_taken => |marker| cursor = marker.next,
                .ret, .jump, .crash, .expect_err, .runtime_error, .comptime_exhaustiveness_failed, .loop_continue, .loop_break => return error.ExpectedConditionalDecref,
                .switch_stmt, .switch_initialized_payload, .str_match, .str_match_set, .join => return error.NonLinearPath,
            }
        }
        return error.CyclicPath;
    }
};

const RcKind = enum { incref, decref, free };
const RcStopKind = enum { ret, crash };

fn setupUnusedBinding(layout_idx: layout_mod.Idx) Allocator.Error!struct { fixture: ArcTest, value: LIR.LocalId } {
    var f = try ArcTest.init(testing.allocator);
    errdefer f.deinit();
    const value = try f.local(layout_idx);
    const result = try f.local(.i64);
    const ret = try f.ret(result);
    const body = switch (layout_idx) {
        .str => try f.assignStr(value, "tmp", ret),
        else => try f.assignList(value, &.{}, ret),
    };
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

test "RC branch-aware: symbol used in both match branches — no incref at binding" {
    var scenario = try setupSwitchUse(true, true, false, false);
    defer scenario.fixture.deinit();
    try scenario.fixture.expectRc(scenario.value, 0, 0, 0);
}

test "RC branch-aware: symbol used in one match branch only — decref in unused branch" {
    var scenario = try setupSwitchUse(true, false, false, false);
    defer scenario.fixture.deinit();
    try scenario.fixture.expectRc(scenario.value, 0, 1, 0);
}

test "RC branch-aware: symbol used twice in one branch — incref in that branch, decref in other" {
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

test "RC proc body: returning list param does not tail-decref it" {
    var f = try ArcTest.init(testing.allocator);
    defer f.deinit();
    const param = try f.local(f.list_str);
    const ret = try f.ret(param);
    _ = try f.addProc(&.{param}, ret, f.list_str);
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

test "RC mutation: reassigning refcounted var emits decref before mutation" {
    var scenario = try setupMutation(false);
    defer scenario.fixture.deinit();
    try scenario.fixture.expectRc(scenario.old_value, 0, 2, 0);
}

test "RC mutation: final use of reassignable refcounted var emits tail decref" {
    var scenario = try setupMutation(true);
    defer scenario.fixture.deinit();
    try testing.expect(scenario.fixture.countRc(scenario.target, .decref) >= 1);
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

test "RC if_then_else: symbol used in both branches — no extra incref" {
    var scenario = try setupSwitchUse(true, true, false, false);
    defer scenario.fixture.deinit();
    try scenario.fixture.expectRc(scenario.value, 0, 0, 0);
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

test "RC discriminant_switch: symbol used in switch branches gets per-branch RC" {
    var scenario = try setupSwitchUse(true, false, false, false);
    defer scenario.fixture.deinit();
    try scenario.fixture.expectRc(scenario.value, 0, 1, 0);
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

test "RC tag_payload_access: retained parent temp is released after extraction" {
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
    try f.expectRc(tag_value, 0, 1, 0);
    try testing.expect(f.countRc(extracted, .incref) >= 1);
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

test "RC early_return inside branch accounts for branch-level increfs" {
    var scenario = try setupSwitchUse(true, false, true, false);
    defer scenario.fixture.deinit();
    try scenario.fixture.expectRc(scenario.value, 1, 1, 0);
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

    const rewritten_body = f.procBody();
    const rewritten_switch = switch (f.store.getCFStmt(rewritten_body)) {
        .assign_literal => |assign| f.store.getCFStmt(assign.next).switch_stmt,
        else => arcInvariant("ARC cold-default switch test body shape changed"),
    };
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

    const rewritten_call = switch (f.store.getCFStmt(f.procBody())) {
        .assign_call => |assign| assign,
        else => arcInvariant("ARC cold-call test body shape changed"),
    };
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
    try f.expectRc(pair, 0, 1, 0);
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

    // list = []; pair = {list, list}; ret pair — the pair reaches the host
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

    // list = []; alias = list; call hosted(list); expect(alias); ret 1 —
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

    // appended = checked_op(param, elem); ret appended — the caller may
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
    // second = checked_op(first, elem) — the append's RcEffect marks its
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

    const base_proc_count = f.store.proc_specs.items.len;
    try insert(&f.store, &f.layouts, .{ .specialize = true });

    // One unique-seeded variant exists; its op runs check-free while the
    // base proc keeps the runtime check.
    try testing.expectEqual(base_proc_count + 1, f.store.proc_specs.items.len);
    try testing.expectEqual(@as(u64, 0), try f.uniqueArgsInProc(callee, appended));
    const variant: LIR.LirProcSpecId = @enumFromInt(@as(u32, @intCast(base_proc_count)));
    try testing.expectEqual(@as(u64, 1), try f.uniqueArgsInProc(variant, appended));
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

    const base_proc_count = f.store.proc_specs.items.len;
    try f.run();

    // Single-variant emission never sees unique parameters: no variant is
    // cloned and the callee keeps its runtime check.
    try testing.expectEqual(base_proc_count, f.store.proc_specs.items.len);
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
    // expect(list) — the original is read besides the alias, so the alias
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
    var remaining: usize = f.store.cf_stmts.items.len + 1;
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
            .assign_low_level => |a| cursor = a.next,
            .assign_list => |a| cursor = a.next,
            .assign_struct => |a| cursor = a.next,
            .assign_tag => |a| cursor = a.next,
            .set_local => |a| cursor = a.next,
            .debug => |a| cursor = a.next,
            .expect => |a| cursor = a.next,
            else => return error.NonLinearPath,
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
    // The consumed read pays one retain at the read and is then moved into
    // the call.
    try f.expectRc(field, 1, 0, 0);
    try f.expectRc(pair, 0, 1, 0);
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

    const base_proc_count = f.store.proc_specs.items.len;
    try insert(&f.store, &f.layouts, .{ .specialize = true });

    // Moving this argument into a variant would only relocate the release
    // from caller to callee. Keep the borrowed signature and avoid cloning
    // live code for no runtime RC reduction.
    try testing.expectEqual(base_proc_count, f.store.proc_specs.items.len);
    try f.expectRc(value, 0, 1, 0);
    try f.expectRc(param, 0, 0, 0);
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

    // Caller builds an owned string and passes it as its final use. The
    // variant turns the borrowed return into an owned return. It is appended
    // while this caller is being rewritten, so the caller body must be written
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

    const base_proc_count = f.store.proc_specs.items.len;
    f.store.proc_specs.shrinkAndFree(f.allocator, f.store.proc_specs.items.len);
    try insert(&f.store, &f.layouts, .{ .specialize = true });

    try testing.expectEqual(base_proc_count + 1, f.store.proc_specs.items.len);
    const variant: LIR.LirProcSpecId = @enumFromInt(@as(u32, @intCast(base_proc_count)));

    var cursor = f.store.getProcSpec(caller).body orelse return error.MissingCallerBody;
    var remaining = f.store.cf_stmts.items.len + 1;
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
            else => return error.ExpectedSpecializedCall,
        }
    }
    return error.ExpectedSpecializedCall;
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

    const base_proc_count = f.store.proc_specs.items.len;
    try f.run();

    // The single-variant build keeps the borrowed signature: the caller
    // retains ownership across the call and releases right after it.
    try testing.expectEqual(base_proc_count, f.store.proc_specs.items.len);
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

    const base_proc_count = f.store.proc_specs.items.len;
    try insert(&f.store, &f.layouts, .{ .specialize = true });

    try testing.expectEqual(base_proc_count + 1, f.store.proc_specs.items.len);
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
