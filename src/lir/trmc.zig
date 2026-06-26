//! Tail Recursion Modulo Constructor (TRMC) and plain tail-call elimination.
//!
//! Runs over the LIR after SolvedLirLower and before Arc.insert. For each
//! self-recursive proc it either:
//!
//! - applies TRMC, when at least one recursive call's result flows directly
//!   into a constructor of the proc's (recursive tag-union) return type that
//!   is itself returned: the proc becomes a join-point loop threading a
//!   `hole: ptr(Ret)` (where the next child value will be written) and a
//!   `head: ptr(Ret)` (the eventual result slot). Plain tail self-calls in
//!   the same proc also become jumps; or
//! - applies plain TCE, when the proc has tail self-calls but no constructor
//!   candidates; or
//! - leaves the proc untouched.
//!
//! Without this pass, list builders like `repeat`, `map`, and `filter` grow a
//! stack frame per element and overflow on large inputs — a correctness
//! issue, not just performance.
//!
//! ## Value holes (vs. the old Rust compiler's pointer holes)
//!
//! The old compiler represented a recursive union value as one tagged
//! pointer; heap cells held payloads only, so its TRMC hole was a
//! pointer-sized slot and a GEP expression (`UnionFieldPtrAtIndex`) computed
//! interior field addresses. This compiler represents a recursive union as a
//! by-value blob (payload struct + in-cell discriminant); recursion is broken
//! only at struct-field edges, which have layout `box(Union)`, and the heap
//! allocation in LIR is the `box_box` low-level — not the tag construction.
//!
//! The hole here is therefore "a pointer to where a child union VALUE lives":
//! initially a stack slot for the result (`ptr_alloca`), afterwards the
//! interior of a freshly allocated, zero-filled box cell
//! (`box_alloc_zeroed`). A box's data pointer already IS the address of the
//! child slot, so no GEP op is needed; per iteration the rewrite allocates
//! the empty child cell, builds the node with that cell as its box field,
//! copies the whole node through the current hole (`ptr_store`), and advances
//! the hole to the new cell (`ptr_cast`). The base case fills the final hole
//! and returns `ptr_load(head)`. Allocation counts and the final memory shape
//! are identical to the untransformed code.
//!
//! ## ARC contract
//!
//! The pass must run before Arc.insert (it deletes calls and changes an
//! allocation site, and ARC panics on pre-existing RC statements). The
//! `ptr(T)` layout is never refcounted, so hole/head locals are invisible to
//! ARC; the cell keeps its `box(U)` layout and ARC accounts it exactly like
//! the `box_box` it replaced. `ptr_store` consumes its value operand
//! (ownership moves into the structure), and the stored local is always the
//! chain head — dead on every loop path — so no protective retain is needed.
//! Zero-filled cells are decref-safe: in-flight box fields read as null, and
//! the RC runtime treats null as a no-op.
//!
//! Build debug options:
//! - `-Dprint-trmc=true`: one line per transformed proc on stderr.
//! - `-Dprint-ir-after-trmc=true`: full IR dump of each transformed proc.

const std = @import("std");
const builtin = @import("builtin");
const Allocator = std.mem.Allocator;
const build_options = @import("build_options");
const core = @import("lir_core");
const layout_mod = @import("layout");
const debug_print = @import("debug_print.zig");

const LIR = core.LIR;
const LirStore = core.LirStore;
const LocalId = LIR.LocalId;
const CFStmtId = LIR.CFStmtId;
const JoinPointId = LIR.JoinPointId;
const LowLevelOp = LIR.LowLevel;

pub const ResourceError = std.mem.Allocator.Error;

/// Apply TRMC/TCE to every eligible proc in the store.
pub fn run(store: *LirStore, layouts: *layout_mod.Store) ResourceError!void {
    const print_transforms = build_options.print_trmc;
    const print_ir = build_options.print_ir_after_trmc;

    // Every statement a proc's walk can reach exists before the pass runs;
    // statements appended by earlier transforms belong to already-processed
    // procs, so sizing the stamp array once up front is safe.
    var scratch = try Scratch.init(store.allocator, store.cf_stmts.items.len);
    defer scratch.deinit();

    const proc_count = store.proc_specs.items.len;
    var proc_index: usize = 0;
    while (proc_index < proc_count) : (proc_index += 1) {
        const proc_id: LIR.LirProcSpecId = @enumFromInt(proc_index);
        try transformProc(store, layouts, proc_id, &scratch, print_transforms, print_ir);
    }
}

fn transformProc(
    store: *LirStore,
    layouts: *layout_mod.Store,
    proc_id: LIR.LirProcSpecId,
    scratch: *Scratch,
    print_transforms: bool,
    print_ir: bool,
) ResourceError!void {
    const proc = store.getProcSpec(proc_id);
    if (proc.body == null or proc.hosted != null or proc.abi != .roc) return;

    var detection = Detection.init(store, layouts, proc_id, scratch);
    defer detection.deinit();
    try detection.detect();
    if (detection.bail) return;

    var construct_count: usize = 0;
    var tail_count: usize = 0;
    for (scratch.candidates.items) |candidate| {
        switch (candidate.state) {
            .confirmed_construct => construct_count += 1,
            .confirmed_tail => tail_count += 1,
            else => {},
        }
    }

    if (construct_count == 0 and tail_count == 0) return;
    if (builtin.mode == .Debug) detection.assertRewrittenStmtsUnshared();

    if (construct_count > 0) {
        var transform = Transform.init(store.allocator, store, layouts, proc_id, &detection);
        defer transform.deinit();
        try transform.applyTrmc();
        store.getProcSpecPtr(proc_id).tail_transform = .trmc;
    } else {
        var transform = Transform.init(store.allocator, store, layouts, proc_id, &detection);
        defer transform.deinit();
        try transform.applyTce();
        store.getProcSpecPtr(proc_id).tail_transform = .tce;
    }

    if (print_transforms) {
        const transformed = store.getProcSpec(proc_id);
        debugPrint("{s}: proc p{d} ({d} construct sites, {d} tail calls)\n", .{
            @tagName(transformed.tail_transform),
            @intFromEnum(proc_id),
            construct_count,
            tail_count,
        });
    }
    if (print_ir) {
        dumpProc(store, layouts, proc_id);
    }
}

// ═══════════════════════════════════════════════════════════════════════════
// Detection
// ═══════════════════════════════════════════════════════════════════════════

/// How a statement is reached: the incoming reference the transform redirects
/// to splice statements out of the graph. Statement graphs are DAGs (lowering
/// shares linear tails), so this is the FIRST-seen reference. TRMC/TCE
/// eligibility rejects candidates whose recorded edge or rewritten statements
/// are reachable through a shared tail; otherwise splicing or repurposing the
/// shared node would change another path through the proc.
const Edge = union(enum) {
    proc_body,
    stmt_next: CFStmtId,
    join_body: CFStmtId,
    join_remainder: CFStmtId,
    switch_branch: struct { stmt: CFStmtId, index: u16 },
    switch_default: CFStmtId,
    switch_continuation: CFStmtId,
    initialized_payload_branch: struct { stmt: CFStmtId, initialized: bool },
};

const CandidateState = enum {
    /// Call result tracked; no use seen yet.
    active,
    /// Result was heap-celled by box_box into the chain's box local.
    boxed,
    /// Box local placed (exactly once) into a payload struct.
    in_struct,
    /// Payload reached an assign_tag of the return layout (possibly followed
    /// by alias hops).
    tagged,
    confirmed_construct,
    confirmed_tail,
    invalid,
};

const max_chain = 8;
const max_alias_stmts = 4;
const max_candidates = 64;

const Candidate = struct {
    state: CandidateState,
    /// Value-flow chain: call result, box cell, payload struct, tag value,
    /// then alias hops. `chain[chain_len - 1]` is the local currently tracked.
    chain: [max_chain]LocalId,
    chain_len: u8,
    call_stmt: CFStmtId,
    call_edge: Edge,
    call_args: LIR.LocalSpan,
    /// The box_box statement (rewritten to box_alloc_zeroed). Valid from .boxed.
    box_stmt: CFStmtId,
    /// The assign_tag target — what gets stored through the hole. Valid from .tagged.
    head_local: LocalId,
    /// Alias-hop statements after the tag (unlinked on the rewritten path),
    /// with their incoming edges as recorded during the walk.
    alias_stmts: [max_alias_stmts]CFStmtId,
    alias_edges: [max_alias_stmts]Edge,
    alias_len: u8,
    /// The `ret`/`jump` terminal the rewrite overwrites. Valid when confirmed.
    terminal_stmt: CFStmtId,

    fn current(self: *const Candidate) LocalId {
        return self.chain[self.chain_len - 1];
    }

    fn chainContains(self: *const Candidate, local: LocalId) bool {
        for (self.chain[0..self.chain_len]) |tracked| {
            if (tracked == local) return true;
        }
        return false;
    }

    fn push(self: *Candidate, local: LocalId) bool {
        if (self.chain_len == max_chain) return false;
        self.chain[self.chain_len] = local;
        self.chain_len += 1;
        return true;
    }
};

const JoinInfo = struct {
    params: LIR.LocalSpan,
    body: CFStmtId,
};

/// A work-stack entry for the detection walk.
const WorkItem = struct { stmt: CFStmtId, edge: Edge };

/// Per-run reusable buffers for detection: the containers that would
/// otherwise be allocated and freed for every proc. The ArrayLists clear in
/// O(1) via clearRetainingCapacity, and the visited marks clear by bumping
/// `generation` — so after the high-water marks are reached, a proc's walk
/// allocates nothing and costs one array load per statement instead of a
/// hash probe. (A reused hashmap would be worse than no reuse for the
/// visited set: its clearRetainingCapacity is O(capacity), which stays at
/// the largest proc's high-water mark.)
const Scratch = struct {
    gpa: Allocator,
    /// Visited marks for the walk, indexed by CFStmtId:
    ///
    /// - generation: reached once on a unique path in this proc
    /// - generation + 1: reached through a shared tail, not yet propagated
    /// - generation + 2: reached through a shared tail, propagated
    ///
    /// Once a statement has two incoming references, every statement reachable
    /// through it is shared too: mutating any of them changes both paths.
    stamps: []u32,
    /// Bumped by 3 in beginProc so all stamp values are fresh.
    generation: u32 = 0,
    work: std.ArrayList(WorkItem) = .empty,
    candidates: std.ArrayList(Candidate) = .empty,
    /// Heads of shared tails discovered during the walk.
    shared_heads: std.ArrayList(CFStmtId) = .empty,
    /// Every original `ret` statement of the current proc (epilogue-rewritten
    /// by TRMC).
    rets: std.ArrayList(CFStmtId) = .empty,

    fn init(gpa: Allocator, stmt_count: usize) ResourceError!Scratch {
        const stamps = try gpa.alloc(u32, stmt_count);
        @memset(stamps, 0);
        return .{ .gpa = gpa, .stamps = stamps };
    }

    fn deinit(self: *Scratch) void {
        self.gpa.free(self.stamps);
        self.work.deinit(self.gpa);
        self.candidates.deinit(self.gpa);
        self.shared_heads.deinit(self.gpa);
        self.rets.deinit(self.gpa);
    }

    fn beginProc(self: *Scratch) void {
        self.work.clearRetainingCapacity();
        self.candidates.clearRetainingCapacity();
        self.shared_heads.clearRetainingCapacity();
        self.rets.clearRetainingCapacity();
        if (self.generation >= std.math.maxInt(u32) - 2) {
            // ~2 billion procs in one run; unreachable in practice, but wrap
            // would make stale stamps read as visited.
            @memset(self.stamps, 0);
            self.generation = 0;
        }
        self.generation += 3;
    }
};

const Detection = struct {
    store: *LirStore,
    layouts: *const layout_mod.Store,
    proc_id: LIR.LirProcSpecId,
    scratch: *Scratch,
    /// Whether the proc's return layout permits TRMC candidates at all.
    eligible_trmc: bool = false,
    /// Set when the proc exceeds a pass limit; the proc is left untouched.
    bail: bool = false,
    joins: std.AutoHashMap(JoinPointId, JoinInfo),
    max_join_id: u32 = 0,

    fn init(store: *LirStore, layouts: *const layout_mod.Store, proc_id: LIR.LirProcSpecId, scratch: *Scratch) Detection {
        scratch.beginProc();
        return .{
            .store = store,
            .layouts = layouts,
            .proc_id = proc_id,
            .scratch = scratch,
            .joins = std.AutoHashMap(JoinPointId, JoinInfo).init(scratch.gpa),
        };
    }

    fn deinit(self: *Detection) void {
        self.joins.deinit();
    }

    fn detect(self: *Detection) ResourceError!void {
        const proc = self.store.getProcSpec(self.proc_id);
        const ret_layout_val = self.layouts.getLayout(proc.ret_layout);
        self.eligible_trmc = ret_layout_val.tag == .tag_union;

        try self.walk(proc.body.?);
        if (self.bail) return;
        try self.propagateSharedTails();
        self.invalidateSharedRewriteCandidates();
    }

    /// Does jumping to `target` ultimately return `local`? True when the join
    /// has exactly `local` as its single param and its body (chasing through
    /// same-param forwarding jumps) is `ret local`.
    fn returnsLocal(self: *const Detection, target: JoinPointId, local: LocalId) bool {
        var join = self.joins.get(target) orelse return false;
        var steps: usize = 0;
        const limit = self.joins.count() + 1;
        while (steps < limit) : (steps += 1) {
            const params = self.store.getLocalSpan(join.params);
            if (params.len != 1 or params[0] != local) return false;
            switch (self.store.getCFStmt(join.body)) {
                .ret => |s| return s.value == local,
                .jump => |s| join = self.joins.get(s.target) orelse return false,
                else => return false,
            }
        }
        return false;
    }

    /// Single DFS over the proc body, doing two jobs per statement: record
    /// join points / ret statements / the highest join id (the wrapper join
    /// uses max + 1), and run the candidate walk. One shared mutable candidate
    /// set across all branches (matching the old Rust pass): a candidate
    /// confirmed on one path is invalidated retroactively if any other
    /// statement uses a chain local. Join bodies are walked before remainders
    /// so a done-join's `ret res` is seen before any branch makes `res` a
    /// tracked alias.
    ///
    /// Fusing join collection with the candidate walk is sound because a jump
    /// may only target an enclosing join point, and every enclosing join's
    /// statement is an ancestor on every DFS path to the jump — so it is
    /// recorded before the jump (or any returnsLocal forwarding chase through
    /// it) is processed.
    ///
    /// Statement graphs are DAGs, not trees: lowering shares linear tails
    /// (two predecessors pointing at the same `next` chain), so the visited
    /// stamps are what guarantees each statement is processed — and each ret
    /// epilogue-rewritten — exactly once.
    fn walk(self: *Detection, body: CFStmtId) ResourceError!void {
        const gpa = self.scratch.gpa;
        const work = &self.scratch.work;
        // No statement reachable from a proc body postdates the stamp array
        // (transforms only append to already-processed procs), so plain
        // indexing is in bounds.
        const stamps = self.scratch.stamps;
        const gen = self.scratch.generation;

        try work.append(gpa, .{ .stmt = body, .edge = .proc_body });
        while (work.pop()) |item| {
            const stamp = &stamps[@intFromEnum(item.stmt)];
            if (stamp.* >= gen) {
                // Second arrival: this statement is the head of a shared tail.
                // Record it so the sharing mark can be propagated to every
                // downstream statement before final candidate eligibility.
                if (stamp.* == gen) {
                    try self.scratch.shared_heads.append(gpa, item.stmt);
                    stamp.* = gen + 1;
                }
                continue;
            }
            stamp.* = gen;

            const stmt = self.store.getCFStmt(item.stmt);
            switch (stmt) {
                .join => |s| {
                    try self.joins.put(s.id, .{ .params = s.params, .body = s.body });
                    self.max_join_id = @max(self.max_join_id, @intFromEnum(s.id));
                },
                .ret => try self.scratch.rets.append(gpa, item.stmt),
                else => {},
            }

            try self.processStmt(item.stmt, item.edge, stmt);
            if (self.bail) return;

            switch (stmt) {
                .join => |s| {
                    // Pushed in reverse pop order: body is processed first.
                    try work.append(gpa, .{ .stmt = s.remainder, .edge = .{ .join_remainder = item.stmt } });
                    try work.append(gpa, .{ .stmt = s.body, .edge = .{ .join_body = item.stmt } });
                },
                .switch_stmt => |s| {
                    if (s.continuation) |continuation| {
                        try work.append(gpa, .{ .stmt = continuation, .edge = .{ .switch_continuation = item.stmt } });
                    }
                    try work.append(gpa, .{ .stmt = s.default_branch, .edge = .{ .switch_default = item.stmt } });
                    const branches = self.store.getCFSwitchBranches(s.branches);
                    var i = branches.len;
                    while (i > 0) {
                        i -= 1;
                        try work.append(gpa, .{ .stmt = branches[i].body, .edge = .{ .switch_branch = .{ .stmt = item.stmt, .index = @intCast(i) } } });
                    }
                },
                .switch_initialized_payload => |s| {
                    try work.append(gpa, .{ .stmt = s.uninitialized_branch, .edge = .{ .initialized_payload_branch = .{ .stmt = item.stmt, .initialized = false } } });
                    try work.append(gpa, .{ .stmt = s.initialized_branch, .edge = .{ .initialized_payload_branch = .{ .stmt = item.stmt, .initialized = true } } });
                },
                .str_match => |s| {
                    try work.append(gpa, .{ .stmt = s.on_miss, .edge = .{ .switch_default = item.stmt } });
                    try work.append(gpa, .{ .stmt = s.on_match, .edge = .{ .switch_branch = .{ .stmt = item.stmt, .index = 0 } } });
                },
                .str_match_set => |s| {
                    try work.append(gpa, .{ .stmt = s.on_miss, .edge = .{ .switch_default = item.stmt } });
                    const arms = self.store.getStrMatchArms(s.arms);
                    var i = arms.len;
                    while (i > 0) {
                        i -= 1;
                        try work.append(gpa, .{ .stmt = arms[i].on_match, .edge = .{ .switch_branch = .{ .stmt = item.stmt, .index = @intCast(i) } } });
                    }
                },
                .jump, .ret, .crash, .expect_err, .runtime_error, .comptime_exhaustiveness_failed, .loop_continue, .loop_break => {},
                inline .assign_ref, .assign_literal, .init_uninitialized, .assign_call, .assign_call_erased, .assign_packed_erased_fn, .assign_low_level, .assign_list, .assign_struct, .assign_tag, .set_local, .debug, .expect, .comptime_branch_taken, .incref, .decref, .decref_if_initialized, .free => |s| {
                    try work.append(gpa, .{ .stmt = s.next, .edge = .{ .stmt_next = item.stmt } });
                },
            }
        }
    }

    /// A shared tail head makes every reachable descendant shared for the
    /// purpose of TRMC/TCE rewrites. Direct in-degree is not enough: if two
    /// branches point at `a` and `a.next` is `b`, then mutating `b` still
    /// changes both branch paths even though only `a` points directly at `b`.
    fn propagateSharedTails(self: *Detection) ResourceError!void {
        const gpa = self.scratch.gpa;
        const work = &self.scratch.work;
        const stamps = self.scratch.stamps;
        const gen = self.scratch.generation;

        work.clearRetainingCapacity();
        for (self.scratch.shared_heads.items) |shared_head| {
            try work.append(gpa, .{ .stmt = shared_head, .edge = .proc_body });
        }

        while (work.pop()) |item| {
            const stamp = &stamps[@intFromEnum(item.stmt)];
            if (stamp.* == gen + 2) continue;
            stamp.* = gen + 2;

            const stmt = self.store.getCFStmt(item.stmt);
            try self.appendSuccessorsForSharedPropagation(work, stmt);
        }
    }

    fn appendSharedSuccessor(self: *Detection, work: *std.ArrayList(WorkItem), stmt_id: CFStmtId) ResourceError!void {
        if (self.scratch.stamps[@intFromEnum(stmt_id)] == self.scratch.generation + 2) return;
        try work.append(self.scratch.gpa, .{ .stmt = stmt_id, .edge = .proc_body });
    }

    fn appendSuccessorsForSharedPropagation(self: *Detection, work: *std.ArrayList(WorkItem), stmt: LIR.CFStmt) ResourceError!void {
        switch (stmt) {
            .join => |s| {
                try self.appendSharedSuccessor(work, s.remainder);
                try self.appendSharedSuccessor(work, s.body);
            },
            .switch_stmt => |s| {
                if (s.continuation) |continuation| {
                    try self.appendSharedSuccessor(work, continuation);
                }
                try self.appendSharedSuccessor(work, s.default_branch);
                const branches = self.store.getCFSwitchBranches(s.branches);
                for (branches) |branch| {
                    try self.appendSharedSuccessor(work, branch.body);
                }
            },
            .switch_initialized_payload => |s| {
                try self.appendSharedSuccessor(work, s.initialized_branch);
                try self.appendSharedSuccessor(work, s.uninitialized_branch);
            },
            .str_match => |s| {
                try self.appendSharedSuccessor(work, s.on_match);
                try self.appendSharedSuccessor(work, s.on_miss);
            },
            .str_match_set => |s| {
                for (self.store.getStrMatchArms(s.arms)) |arm| {
                    try self.appendSharedSuccessor(work, arm.on_match);
                }
                try self.appendSharedSuccessor(work, s.on_miss);
            },
            .jump, .ret, .crash, .expect_err, .runtime_error, .comptime_exhaustiveness_failed, .loop_continue, .loop_break => {},
            inline .assign_ref, .assign_literal, .init_uninitialized, .assign_call, .assign_call_erased, .assign_packed_erased_fn, .assign_low_level, .assign_list, .assign_struct, .assign_tag, .set_local, .debug, .expect, .comptime_branch_taken, .incref, .decref, .decref_if_initialized, .free => |s| {
                try self.appendSharedSuccessor(work, s.next);
            },
        }
    }

    fn processStmt(self: *Detection, stmt_id: CFStmtId, edge: Edge, stmt: LIR.CFStmt) ResourceError!void {
        // Advance or invalidate every live candidate. A statement can be the
        // blessed next step for at most ONE candidate: when two candidates'
        // chains meet (e.g. `Node(build(a), v, build(b))` placing two cells in
        // one payload struct), the first claims the constructor and the others
        // see it as an ordinary use of their tracked cell and drop out — their
        // recursive calls remain real calls, exactly like the old Rust pass.
        var claimed = false;
        for (self.scratch.candidates.items) |*candidate| {
            if (candidate.state == .invalid) continue;
            if (!claimed and self.blessedTransition(candidate, stmt_id, edge, stmt)) {
                claimed = true;
                continue;
            }
            if (self.stmtTouchesChain(stmt, candidate)) {
                candidate.state = .invalid;
            }
        }

        // A self-call starts a new candidate (after the use checks above, so
        // its arguments invalidate any candidate locals they mention).
        if (stmt == .assign_call and stmt.assign_call.proc == self.proc_id) {
            if (self.scratch.candidates.items.len == max_candidates) {
                self.bail = true;
                return;
            }
            var candidate = Candidate{
                .state = .active,
                .chain = undefined,
                .chain_len = 0,
                .call_stmt = stmt_id,
                .call_edge = edge,
                .call_args = stmt.assign_call.args,
                .box_stmt = undefined,
                .head_local = undefined,
                .alias_stmts = undefined,
                .alias_edges = undefined,
                .alias_len = 0,
                .terminal_stmt = undefined,
            };
            _ = candidate.push(stmt.assign_call.target);
            try self.scratch.candidates.append(self.scratch.gpa, candidate);
        }
    }

    /// Returns true when `stmt` is the unique blessed next step for this
    /// candidate (and advances it).
    fn blessedTransition(self: *Detection, candidate: *Candidate, stmt_id: CFStmtId, edge: Edge, stmt: LIR.CFStmt) bool {
        switch (stmt) {
            .assign_low_level => |s| {
                if (candidate.state != .active) return false;
                if (s.op != .box_box) return false;
                const args = self.store.getLocalSpan(s.args);
                if (args.len != 1 or args[0] != candidate.current()) return false;
                if (!self.eligible_trmc) return false;
                if (!self.isBoxOfRetLayout(s.target)) return false;
                if (!candidate.push(s.target)) {
                    candidate.state = .invalid;
                    return true;
                }
                candidate.box_stmt = stmt_id;
                candidate.state = .boxed;
                return true;
            },
            .assign_struct => |s| {
                if (candidate.state != .boxed) return false;
                var occurrences: usize = 0;
                for (self.store.getLocalSpan(s.fields)) |field| {
                    if (field == candidate.current()) occurrences += 1;
                }
                if (occurrences == 0) return false;
                if (occurrences > 1) {
                    // The cell may appear in a constructor exactly once; a
                    // duplicate would alias the hole.
                    candidate.state = .invalid;
                    return true;
                }
                if (!candidate.push(s.target)) {
                    candidate.state = .invalid;
                    return true;
                }
                candidate.state = .in_struct;
                return true;
            },
            .assign_tag => |s| {
                const payload = s.payload orelse return false;
                // The usual shape boxes into a payload struct; accept the cell
                // as a direct payload defensively.
                if (candidate.state != .in_struct and candidate.state != .boxed) return false;
                if (payload != candidate.current()) return false;
                if (!self.isRetLayoutLocal(s.target)) {
                    candidate.state = .invalid;
                    return true;
                }
                if (!candidate.push(s.target)) {
                    candidate.state = .invalid;
                    return true;
                }
                candidate.head_local = s.target;
                candidate.state = .tagged;
                return true;
            },
            .assign_ref => |s| {
                if (candidate.state != .tagged) return false;
                const source = switch (s.op) {
                    .local => |src| src,
                    .nominal => |n| n.backing_ref,
                    .list_reinterpret => |l| l.backing_ref,
                    else => return false,
                };
                if (source != candidate.current()) return false;
                if (candidate.alias_len == max_alias_stmts or !candidate.push(s.target)) {
                    candidate.state = .invalid;
                    return true;
                }
                candidate.alias_stmts[candidate.alias_len] = stmt_id;
                candidate.alias_edges[candidate.alias_len] = edge;
                candidate.alias_len += 1;
                return true;
            },
            .ret => |s| {
                if (s.value != candidate.current()) return false;
                return confirmAtTerminal(candidate, stmt_id);
            },
            .jump => |s| {
                if (!self.returnsLocal(s.target, candidate.current())) return false;
                return confirmAtTerminal(candidate, stmt_id);
            },
            else => return false,
        }
    }

    fn confirmAtTerminal(candidate: *Candidate, stmt_id: CFStmtId) bool {
        switch (candidate.state) {
            .active => {
                candidate.terminal_stmt = stmt_id;
                candidate.state = .confirmed_tail;
                return true;
            },
            .tagged => {
                candidate.terminal_stmt = stmt_id;
                candidate.state = .confirmed_construct;
                return true;
            },
            // Returning a half-built chain (the bare cell or payload struct)
            // is not a use we can rewrite.
            .boxed, .in_struct => {
                candidate.state = .invalid;
                return true;
            },
            else => return false,
        }
    }

    fn isBoxOfRetLayout(self: *const Detection, local: LocalId) bool {
        const local_layout = self.layouts.getLayout(self.store.getLocal(local).layout_idx);
        if (local_layout.tag != .box) return false;
        const inner = self.layouts.getLayout(local_layout.getIdx());
        const ret_layout = self.layouts.getLayout(self.store.getProcSpec(self.proc_id).ret_layout);
        return inner.eql(ret_layout);
    }

    fn isRetLayoutLocal(self: *const Detection, local: LocalId) bool {
        const local_layout = self.layouts.getLayout(self.store.getLocal(local).layout_idx);
        const ret_layout = self.layouts.getLayout(self.store.getProcSpec(self.proc_id).ret_layout);
        return local_layout.eql(ret_layout);
    }

    /// Any read of a chain local (or write over one mid-chain) outside the
    /// blessed transitions permanently invalidates the candidate.
    fn stmtTouchesChain(self: *const Detection, stmt: LIR.CFStmt, candidate: *const Candidate) bool {
        const c = candidate;
        return switch (stmt) {
            .assign_ref => |s| switch (s.op) {
                .local => |src| c.chainContains(src),
                .discriminant => |d| c.chainContains(d.source),
                .field => |f| c.chainContains(f.source),
                .tag_payload => |t| c.chainContains(t.source),
                .tag_payload_struct => |t| c.chainContains(t.source),
                .list_reinterpret => |l| c.chainContains(l.backing_ref),
                .nominal => |n| c.chainContains(n.backing_ref),
            },
            .assign_literal, .assign_packed_erased_fn => false,
            .init_uninitialized => |s| c.chainContains(s.target),
            .assign_call => |s| self.spanTouchesChain(s.args, c),
            .assign_call_erased => |s| c.chainContains(s.closure) or self.spanTouchesChain(s.args, c),
            .assign_low_level => |s| self.spanTouchesChain(s.args, c),
            .assign_list => |s| self.spanTouchesChain(s.elems, c),
            .assign_struct => |s| self.spanTouchesChain(s.fields, c),
            .assign_tag => |s| if (s.payload) |payload| c.chainContains(payload) else false,
            // Reading the value is a use; overwriting a tracked local would
            // corrupt the chain, so treat that as disqualifying too.
            .set_local => |s| c.chainContains(s.value) or c.chainContains(s.target),
            .debug => |s| c.chainContains(s.message),
            .expect => |s| c.chainContains(s.condition),
            .incref => |s| c.chainContains(s.value),
            .decref => |s| c.chainContains(s.value),
            .decref_if_initialized => |s| c.chainContains(s.cond) or c.chainContains(s.value),
            .free => |s| c.chainContains(s.value),
            .switch_stmt => |s| c.chainContains(s.cond),
            .switch_initialized_payload => |s| c.chainContains(s.cond) or c.chainContains(s.payload),
            .str_match => true,
            .str_match_set => true,
            .ret => |s| c.chainContains(s.value),
            .expect_err => |s| c.chainContains(s.message),
            .jump, .crash, .runtime_error, .comptime_exhaustiveness_failed, .comptime_branch_taken, .loop_continue, .loop_break, .join => false,
        };
    }

    fn spanTouchesChain(self: *const Detection, span: LIR.LocalSpan, candidate: *const Candidate) bool {
        for (self.store.getLocalSpan(span)) |local| {
            if (candidate.chainContains(local)) return true;
        }
        return false;
    }

    fn invalidateSharedRewriteCandidates(self: *Detection) void {
        for (self.scratch.candidates.items) |*candidate| {
            switch (candidate.state) {
                .confirmed_construct, .confirmed_tail => {},
                else => continue,
            }
            if (self.candidateTouchesSharedRewritePath(candidate)) {
                candidate.state = .invalid;
            }
        }
    }

    fn candidateTouchesSharedRewritePath(self: *const Detection, candidate: *const Candidate) bool {
        if (self.edgeOwnerIsShared(candidate.call_edge)) return true;
        if (self.isSharedPath(candidate.call_stmt)) return true;
        if (self.isSharedPath(candidate.terminal_stmt)) return true;

        if (candidate.state == .confirmed_construct) {
            if (self.isSharedPath(candidate.box_stmt)) return true;
            for (
                candidate.alias_stmts[0..candidate.alias_len],
                candidate.alias_edges[0..candidate.alias_len],
            ) |alias_stmt, alias_edge| {
                if (self.edgeOwnerIsShared(alias_edge)) return true;
                if (self.isSharedPath(alias_stmt)) return true;
            }
        }

        return false;
    }

    fn edgeOwnerIsShared(self: *const Detection, edge: Edge) bool {
        return switch (edge) {
            .proc_body => false,
            .stmt_next => |stmt| self.isSharedPath(stmt),
            .join_body => |stmt| self.isSharedPath(stmt),
            .join_remainder => |stmt| self.isSharedPath(stmt),
            .switch_branch => |info| self.isSharedPath(info.stmt),
            .switch_default => |stmt| self.isSharedPath(stmt),
            .switch_continuation => |stmt| self.isSharedPath(stmt),
            .initialized_payload_branch => |info| self.isSharedPath(info.stmt),
        };
    }

    fn isSharedPath(self: *const Detection, stmt_id: CFStmtId) bool {
        return self.scratch.stamps[@intFromEnum(stmt_id)] >= self.scratch.generation + 1;
    }

    /// Debug check, run just before transforming: candidate eligibility must
    /// have rejected every path-specific rewrite that touches a shared tail.
    /// Epilogue-rewritten rets are exempt: that rewrite is uniform for every
    /// path reaching them.
    fn assertRewrittenStmtsUnshared(self: *const Detection) void {
        for (self.scratch.candidates.items) |candidate| {
            switch (candidate.state) {
                .confirmed_construct, .confirmed_tail => {},
                else => continue,
            }
            if (self.candidateTouchesSharedRewritePath(&candidate)) {
                std.debug.panic(
                    "TRMC invariant violated: candidate reached transform after touching a shared rewrite path",
                    .{},
                );
            }
        }
    }
};

// ═══════════════════════════════════════════════════════════════════════════
// Transform
// ═══════════════════════════════════════════════════════════════════════════

const Transform = struct {
    gpa: Allocator,
    store: *LirStore,
    layouts: *layout_mod.Store,
    proc_id: LIR.LirProcSpecId,
    detection: *const Detection,
    /// Every local created by the transform, merged into frame_locals at the end.
    new_locals: std.ArrayList(LocalId),
    /// Owned copy of the original proc arg locals; they become the loop join's
    /// params while fresh locals take their place as proc args.
    old_args: []LocalId,
    join_id: JoinPointId,
    hole: LocalId = undefined,
    head: LocalId = undefined,

    fn init(gpa: Allocator, store: *LirStore, layouts: *layout_mod.Store, proc_id: LIR.LirProcSpecId, detection: *const Detection) Transform {
        return .{
            .gpa = gpa,
            .store = store,
            .layouts = layouts,
            .proc_id = proc_id,
            .detection = detection,
            .new_locals = .empty,
            .old_args = &.{},
            .join_id = @enumFromInt(detection.max_join_id + 1),
        };
    }

    fn deinit(self: *Transform) void {
        self.new_locals.deinit(self.gpa);
        self.gpa.free(self.old_args);
    }

    fn addLocal(self: *Transform, layout_idx: layout_mod.Idx) ResourceError!LocalId {
        const id = try self.store.addLocal(.{ .layout_idx = layout_idx });
        try self.new_locals.append(self.gpa, id);
        return id;
    }

    fn applyTrmc(self: *Transform) ResourceError!void {
        const proc = self.store.getProcSpec(self.proc_id);
        const ret_layout = proc.ret_layout;
        self.old_args = try self.gpa.dupe(LocalId, self.store.getLocalSpan(proc.args));

        const ptr_ret = try self.layouts.insertPtr(ret_layout);
        self.hole = try self.addLocal(ptr_ret);
        self.head = try self.addLocal(ptr_ret);
        const initial = try self.addLocal(ptr_ret);

        // Rewrite every original ret into the hole-fill epilogue. Confirmed
        // site terminals (which may be rets in hand-built shapes) are excluded:
        // they become loop-backs below.
        for (self.detection.scratch.rets.items) |ret_stmt| {
            if (self.isSiteTerminal(ret_stmt)) continue;
            try self.rewriteRetEpilogue(ret_stmt, ret_layout);
        }

        for (self.detection.scratch.candidates.items) |*candidate| {
            switch (candidate.state) {
                .confirmed_construct => try self.rewriteConstructSite(candidate, ptr_ret),
                .confirmed_tail => try self.rewriteTailSite(candidate),
                else => {},
            }
        }

        try self.installWrapper(true, initial);
    }

    fn applyTce(self: *Transform) ResourceError!void {
        const proc = self.store.getProcSpec(self.proc_id);
        self.old_args = try self.gpa.dupe(LocalId, self.store.getLocalSpan(proc.args));

        for (self.detection.scratch.candidates.items) |*candidate| {
            if (candidate.state == .confirmed_tail) {
                try self.rewriteTailSite(candidate);
            }
        }
        try self.installTceLoop();
    }

    fn isSiteTerminal(self: *const Transform, stmt_id: CFStmtId) bool {
        for (self.detection.scratch.candidates.items) |candidate| {
            switch (candidate.state) {
                .confirmed_construct, .confirmed_tail => {
                    if (candidate.terminal_stmt == stmt_id) return true;
                },
                else => {},
            }
        }
        return false;
    }

    /// `ret v` becomes: write v through the hole, load the head, return it.
    fn rewriteRetEpilogue(self: *Transform, ret_stmt: CFStmtId, ret_layout: layout_mod.Idx) ResourceError!void {
        const value = self.store.getCFStmt(ret_stmt).ret.value;

        const final = try self.addLocal(ret_layout);
        const st = try self.addLocal(.zst);
        const ret_final = try self.store.addCFStmt(.{ .ret = .{ .value = final } });
        const load = try self.store.addCFStmt(.{ .assign_low_level = .{
            .target = final,
            .op = .ptr_load,
            .rc_effect = LowLevelOp.ptr_load.rcEffect(),
            .args = try self.store.addLocalSpan(&.{self.head}),
            .next = ret_final,
        } });
        const store_args = try self.store.addLocalSpan(&.{ self.hole, value });
        self.store.getCFStmtPtr(ret_stmt).* = .{ .assign_low_level = .{
            .target = st,
            .op = .ptr_store,
            .rc_effect = LowLevelOp.ptr_store.rcEffect(),
            .args = store_args,
            .next = load,
        } };
    }

    fn rewriteConstructSite(self: *Transform, candidate: *const Candidate, ptr_ret: layout_mod.Idx) ResourceError!void {
        // 1. Splice out the recursive call.
        self.unlink(candidate.call_edge, self.nextOf(candidate.call_stmt));

        // 2. The heap cell is now allocated empty (and zeroed, so its own box
        //    fields read as null holes) instead of copying the call result in.
        const empty_args = try self.store.addLocalSpan(&.{});
        const cell = blk: {
            const box_ptr = self.store.getCFStmtPtr(candidate.box_stmt);
            box_ptr.assign_low_level.op = .box_alloc_zeroed;
            box_ptr.assign_low_level.rc_effect = LowLevelOp.box_alloc_zeroed.rcEffect();
            box_ptr.assign_low_level.args = empty_args;
            break :blk box_ptr.assign_low_level.target;
        };

        // 3. Peephole: take the next hole pointer right after the allocation
        //    so the constructor below is the cell's last use (sets up the ARC
        //    transfer-into-constructor follow-up).
        const next_hole = try self.addLocal(ptr_ret);
        const cast = try self.store.addCFStmt(.{ .assign_low_level = .{
            .target = next_hole,
            .op = .ptr_cast,
            .rc_effect = LowLevelOp.ptr_cast.rcEffect(),
            .args = try self.store.addLocalSpan(&.{cell}),
            .next = self.nextOf(candidate.box_stmt),
        } });
        self.setNext(candidate.box_stmt, cast);

        // 4. Unlink the alias hops on this path: the store below uses the
        //    chain head directly, and a surviving alias of the stored node
        //    would make ARC retain it with no consumer (a leak per iteration).
        //    Reverse order keeps each recorded predecessor edge valid.
        var i = candidate.alias_len;
        while (i > 0) {
            i -= 1;
            self.unlink(candidate.alias_edges[i], self.nextOf(candidate.alias_stmts[i]));
        }

        // 5. The terminal becomes: fill the current hole with the node value,
        //    thread the args + new hole, and loop.
        const st = try self.addLocal(.zst);
        const loop_back = try self.buildLoopBack(candidate, &.{.{ .target = self.hole, .value = next_hole }});
        const store_args = try self.store.addLocalSpan(&.{ self.hole, candidate.head_local });
        self.store.getCFStmtPtr(candidate.terminal_stmt).* = .{ .assign_low_level = .{
            .target = st,
            .op = .ptr_store,
            .rc_effect = LowLevelOp.ptr_store.rcEffect(),
            .args = store_args,
            .next = loop_back,
        } };
    }

    fn rewriteTailSite(self: *Transform, candidate: *const Candidate) ResourceError!void {
        self.unlink(candidate.call_edge, self.nextOf(candidate.call_stmt));
        const loop_back = try self.buildLoopBack(candidate, &.{});
        // Overwrite the terminal in place with the head of the loop-back chain
        // (the head statement object itself becomes garbage).
        self.store.getCFStmtPtr(candidate.terminal_stmt).* = self.store.getCFStmt(loop_back);
    }

    const ExtraParamWrite = struct { target: LocalId, value: LocalId };

    /// Build `set_local param := arg` writes for each recursive-call arg (plus
    /// extras), ending in `jump J`. Returns the chain head. Args that are
    /// themselves param locals are copied through fresh temps first, since the
    /// sequential writes would otherwise clobber a param another arg reads.
    fn buildLoopBack(self: *Transform, candidate: *const Candidate, extras: []const ExtraParamWrite) ResourceError!CFStmtId {
        const arg_view = self.store.getLocalSpan(candidate.call_args);
        const args = try self.gpa.alloc(LocalId, arg_view.len);
        defer self.gpa.free(args);
        @memcpy(args, arg_view);

        var copy_head: ?CFStmtId = null;
        var copy_tail: ?CFStmtId = null;
        for (args, 0..) |*arg, idx| {
            if (arg.* == self.old_args[idx]) continue; // self-assign, skipped below
            const is_param = std.mem.findScalar(LocalId, self.old_args, arg.*) != null;
            if (!is_param) continue;
            const tmp = try self.addLocal(self.store.getLocal(arg.*).layout_idx);
            const copy = try self.store.addCFStmt(.{ .assign_ref = .{
                .target = tmp,
                .op = .{ .local = arg.* },
                .next = undefined,
            } });
            if (copy_tail) |tail| {
                self.setNext(tail, copy);
            } else {
                copy_head = copy;
            }
            copy_tail = copy;
            arg.* = tmp;
        }

        var current = try self.store.addCFStmt(.{ .jump = .{ .target = self.join_id } });
        var i = extras.len;
        while (i > 0) {
            i -= 1;
            current = try self.store.addCFStmt(.{ .set_local = .{
                .target = extras[i].target,
                .value = extras[i].value,
                .mode = .initialize_join_param,
                .next = current,
            } });
        }
        var j = args.len;
        while (j > 0) {
            j -= 1;
            if (args[j] == self.old_args[j]) continue; // value already in the param
            current = try self.store.addCFStmt(.{ .set_local = .{
                .target = self.old_args[j],
                .value = args[j],
                .mode = .initialize_join_param,
                .next = current,
            } });
        }

        if (copy_tail) |tail| {
            self.setNext(tail, current);
            return copy_head.?;
        }
        return current;
    }

    /// Wrap the (rewritten) body in the loop join and re-point the proc at
    /// fresh argument locals; the original args become the join params.
    fn installWrapper(self: *Transform, comptime is_trmc: bool, initial: LocalId) ResourceError!void {
        const old_body = self.store.getProcSpec(self.proc_id).body.?;

        const fresh = try self.gpa.alloc(LocalId, self.old_args.len);
        defer self.gpa.free(fresh);
        for (self.old_args, fresh) |old_arg, *fresh_arg| {
            fresh_arg.* = try self.addLocal(self.store.getLocal(old_arg).layout_idx);
        }

        // Entry chain (built backward): [alloca initial;] set params; jump J.
        var current = try self.store.addCFStmt(.{ .jump = .{ .target = self.join_id } });
        if (is_trmc) {
            current = try self.store.addCFStmt(.{ .set_local = .{
                .target = self.head,
                .value = initial,
                .mode = .initialize_join_param,
                .next = current,
            } });
            current = try self.store.addCFStmt(.{ .set_local = .{
                .target = self.hole,
                .value = initial,
                .mode = .initialize_join_param,
                .next = current,
            } });
        }
        var i = self.old_args.len;
        while (i > 0) {
            i -= 1;
            current = try self.store.addCFStmt(.{ .set_local = .{
                .target = self.old_args[i],
                .value = fresh[i],
                .mode = .initialize_join_param,
                .next = current,
            } });
        }
        if (is_trmc) {
            current = try self.store.addCFStmt(.{ .assign_low_level = .{
                .target = initial,
                .op = .ptr_alloca,
                .rc_effect = LowLevelOp.ptr_alloca.rcEffect(),
                .args = try self.store.addLocalSpan(&.{}),
                .next = current,
            } });
        }

        const param_count = self.old_args.len + if (is_trmc) @as(usize, 2) else 0;
        const params = try self.gpa.alloc(LocalId, param_count);
        defer self.gpa.free(params);
        @memcpy(params[0..self.old_args.len], self.old_args);
        if (is_trmc) {
            params[self.old_args.len] = self.hole;
            params[self.old_args.len + 1] = self.head;
        }

        const join_stmt = try self.store.addCFStmt(.{ .join = .{
            .id = self.join_id,
            .params = try self.store.addLocalSpan(params),
            .body = old_body,
            .remainder = current,
        } });

        const fresh_span = try self.store.addLocalSpan(fresh);
        const frame_locals = try self.rebuildFrameLocals();
        const proc_ptr = self.store.getProcSpecPtr(self.proc_id);
        proc_ptr.args = fresh_span;
        proc_ptr.body = join_stmt;
        proc_ptr.frame_locals = frame_locals;
        self.requireStackProbeIfNeeded(proc_ptr);
    }

    /// Plain TCE does not need a wrapper argument frame. The proc's existing
    /// argument locals are already the initial loop-carried values, so enter
    /// the join directly and only rewrite recursive sites to rebind params.
    fn installTceLoop(self: *Transform) ResourceError!void {
        const proc_ptr = self.store.getProcSpecPtr(self.proc_id);
        const old_body = proc_ptr.body.?;

        const entry_jump = try self.store.addCFStmt(.{ .jump = .{ .target = self.join_id } });
        const join_stmt = try self.store.addCFStmt(.{ .join = .{
            .id = self.join_id,
            .params = proc_ptr.args,
            .body = old_body,
            .remainder = entry_jump,
        } });

        proc_ptr.body = join_stmt;
        proc_ptr.frame_locals = try self.rebuildFrameLocals();
        self.requireStackProbeIfNeeded(proc_ptr);
    }

    fn requireStackProbeIfNeeded(self: *const Transform, proc: *LIR.LirProcSpec) void {
        if (self.store.procNeedsStackProbe(self.layouts, proc.*)) {
            proc.stack_probe = .required;
        }
    }

    /// frame_locals must stay complete, unique, and sorted: the interpreter
    /// binary-searches it for every local in the proc.
    fn rebuildFrameLocals(self: *Transform) ResourceError!LIR.LocalSpan {
        const old_span = self.store.getProcSpec(self.proc_id).frame_locals;
        const old = self.store.getLocalSpan(old_span);
        var merged = try std.ArrayList(LocalId).initCapacity(self.gpa, old.len + self.new_locals.items.len);
        defer merged.deinit(self.gpa);
        merged.appendSliceAssumeCapacity(old);
        merged.appendSliceAssumeCapacity(self.new_locals.items);
        std.mem.sort(LocalId, merged.items, {}, localIdLessThan);
        var unique_len: usize = 0;
        for (merged.items, 0..) |local, idx| {
            if (idx > 0 and merged.items[unique_len - 1] == local) continue;
            merged.items[unique_len] = local;
            unique_len += 1;
        }
        return try self.store.addLocalSpan(merged.items[0..unique_len]);
    }

    fn localIdLessThan(_: void, a: LocalId, b: LocalId) bool {
        return @intFromEnum(a) < @intFromEnum(b);
    }

    fn nextOf(self: *const Transform, stmt_id: CFStmtId) CFStmtId {
        return switch (self.store.getCFStmt(stmt_id)) {
            inline .assign_ref, .assign_literal, .init_uninitialized, .assign_call, .assign_call_erased, .assign_packed_erased_fn, .assign_low_level, .assign_list, .assign_struct, .assign_tag, .set_local, .debug, .expect, .incref, .decref, .decref_if_initialized, .free => |s| s.next,
            else => unreachable,
        };
    }

    fn setNext(self: *Transform, stmt_id: CFStmtId, next: CFStmtId) void {
        const ptr = self.store.getCFStmtPtr(stmt_id);
        switch (ptr.*) {
            inline .assign_ref, .assign_literal, .init_uninitialized, .assign_call, .assign_call_erased, .assign_packed_erased_fn, .assign_low_level, .assign_list, .assign_struct, .assign_tag, .set_local, .debug, .expect, .incref, .decref, .decref_if_initialized, .free => |*s| s.next = next,
            else => unreachable,
        }
    }

    fn unlink(self: *Transform, edge: Edge, replacement: CFStmtId) void {
        switch (edge) {
            .proc_body => self.store.getProcSpecPtr(self.proc_id).body = replacement,
            .stmt_next => |pred| self.setNext(pred, replacement),
            .join_body => |join_stmt| self.store.getCFStmtPtr(join_stmt).join.body = replacement,
            .join_remainder => |join_stmt| self.store.getCFStmtPtr(join_stmt).join.remainder = replacement,
            .switch_branch => |info| {
                const span = self.store.getCFStmt(info.stmt).switch_stmt.branches;
                self.store.getCFSwitchBranchesMut(span)[info.index].body = replacement;
            },
            .switch_default => |switch_id| self.store.getCFStmtPtr(switch_id).switch_stmt.default_branch = replacement,
            .switch_continuation => |switch_id| self.store.getCFStmtPtr(switch_id).switch_stmt.continuation = replacement,
            .initialized_payload_branch => |info| {
                const switch_stmt = &self.store.getCFStmtPtr(info.stmt).switch_initialized_payload;
                if (info.initialized) {
                    switch_stmt.initialized_branch = replacement;
                } else {
                    switch_stmt.uninitialized_branch = replacement;
                }
            },
        }
    }
};

// ═══════════════════════════════════════════════════════════════════════════
// Debug output
// ═══════════════════════════════════════════════════════════════════════════

/// std.debug.print reaches stderr through std.Io.Threaded, which does not
/// compile for freestanding wasm. These build-option dumps are dev-only and
/// should not pull Io.Threaded into freestanding wasm builds.
fn debugPrint(comptime fmt: []const u8, args: anytype) void {
    if (comptime builtin.target.os.tag != .freestanding) {
        std.debug.print(fmt, args);
    }
}

fn dumpProc(store: *const LirStore, layouts: *const layout_mod.Store, proc_id: LIR.LirProcSpecId) void {
    var buffer: std.Io.Writer.Allocating = .init(store.allocator);
    defer buffer.deinit();
    debug_print.writeProc(store.allocator, store, layouts, proc_id, &buffer.writer) catch return;
    debugPrint("{s}", .{buffer.written()});
}
