//! Debug-only borrow certifier for ARC-complete LIR.
//!
//! The certifier re-checks emitted RC statements against the ownership rules
//! that ARC insertion is supposed to uphold, using only the emitted LIR and
//! the ARC-stage signature table:
//!
//! - every use of a refcounted local happens while its value is provably live
//!   (it carries an ownership unit, or it borrows from a value that does)
//! - every `decref`/`free` releases a unit that exists (no double-free, and
//!   never a release of a borrowed value)
//! - every ownership unit is released or transferred exactly once on every
//!   path (no leaks at `ret`, `crash`, or `runtime_error`)
//! - all jumps to one join point agree on which locals carry units, how local
//!   names alias one value, and where borrows take their liveness from
//!
//! Ownership units are tracked per value, not per local: a local-to-local
//! assignment makes both names share one value, and the RC statements on
//! either name act on that shared value's balance. Payload reads produce
//! fresh values that borrow from their source value. Aggregate construction
//! moves one unit per refcounted operand occurrence into the aggregate; the
//! emitted trailing increfs restore the operands' own units, so aggregate
//! consumption tolerates a transiently negative balance and relies on the
//! per-path terminal balance check to flag a missing restore.
//!
//! Conditional ownership is refined by explicit initialized-payload control
//! flow. The initialized edge promotes the payload to ordinary owned state;
//! the uninitialized edge removes its possible unit and binding. Keeping the
//! old condition on the initialized edge is both imprecise and non-convergent:
//! independent presence tests accumulate stale mode dimensions at later joins.
//!
//! ## Join points: dataflow fixpoint over a finite-height lattice
//!
//! Every jump into a join is summarized over the locals the join body reads
//! before rebinding (`summarizeForJoin`): per name, an ownership mode drawn
//! from `unbound`, `owned(units)`, `conditional_owned(cond, mask)`, or
//! `borrowed(anchor)`, plus the must-alias partition of the names (`repr`).
//! Rather than walking the body once per distinct summary—for a loop
//! carrying K refcounted mutable locals whose body merges and re-splits
//! their alias groups, the number of distinct partitions grows like the
//! Bell number of K (B(6) = 203, B(12) = 4.2 million; issue 9658 was a
//! valid program that outgrew the old per-summary enumeration cap)—
//! summaries are *joined* and the body is walked once per joined state
//! (`JoinGroup`, `absorbJoinSummary`):
//!
//! - Summaries agreeing on every name's mode (same class, same presence
//!   condition, same borrow anchor) share one group. The group's partition
//!   is the meet (common refinement) of the members' partitions, and each
//!   fine class's balance is attributed from the members' class balances by
//!   constraint propagation (`meetGroupSummary`).
//! - Summaries whose modes disagree go to separate groups. This is the
//!   refinement rule: the abstraction splits exactly on entry-state modes
//!   real in-edges disagree about, so every group is witnessed by at least one real jump
//!   and a group-walk finding always corresponds to a real entry state. In
//!   the worst case this degenerates to one group per distinct summary—
//!   the pre-fixpoint exact behavior—but with no cap and no skip path.
//!
//! Why one walk under the group's meet partition covers every member
//! (soundness of the join): a member's entry state is the group state with
//! some fine classes merged, each merged class's balance the sum of its
//! parts (that is exactly the attribution constraint). Every certifier
//! check is monotone under that merge: a release through name x needs a
//! unit on x's fine class, and the merged class holds the sum of the fine
//! remainders, which stays >= any single part on every walk prefix; the
//! per-path terminal check demands all fine balances hit exactly zero, so
//! their sums do too; and a fine value that is live (a unit remaining, a
//! live borrow anchor, or a live holder chain) only gains liveness sources
//! when merged with more names' units. So a passing walk under the finer
//! partition implies a passing walk under any member's coarser one; the
//! transfer functions are monotone in the entry state, and the walked state
//! only moves down the refinement order as members arrive.
//!
//! Termination and re-walk bound: a group's partition strictly refines at
//! most (relevant locals - 1) times, and a group is only re-walked when its
//! partition refined, so each group walks at most n times—the lattice
//! height replaces summary enumeration. Group *creation* is bounded by
//! Dickson's lemma: mode vectors and partitions are finite, and among
//! groups sharing both, balance vectors that keep growing pointwise are
//! reported as a per-iteration accumulation finding after
//! `balance_growth_finding_threshold` strictly-increasing witnesses (two
//! mode- and partition-identical entries with different balances cannot
//! both certify against the one shared body: every terminal path consumes a
//! fixed unit count, so divergent balances either fail a walk or are
//! shunted through jumps forever, which is unbounded refcount growth—a
//! leak either way).
//!
//! The guaranteed property on a clean return: for every procedure—with no
//! unverified residue and no capacity escape—every emitted schedule
//! balances ownership on all paths: each unit is released or transferred
//! exactly once, nothing is used after its value dies, and no borrowed
//! value is released.
//!
//! Certification also checks the structural contract of erased-callable proc
//! arguments. This keeps the hidden capture/reuse ABI and its ownership marker
//! synchronized across every transform that clones or rewrites proc specs.
//!
//! A certification failure is a compiler bug in ARC insertion. The production
//! entry point panics in debug builds; release builds never run the certifier.

const std = @import("std");
const builtin = @import("builtin");
const base = @import("base");
const collections = @import("collections");
const core = @import("lir_core");
const layout_mod = @import("layout");
const rc_effect_rules = base.rc_effect_rules;
const arc_sig = @import("arc_sig.zig");
const arc_dismantle = @import("arc_dismantle.zig");
const arc_solve = @import("arc_solve.zig");
const debug_print = @import("debug_print.zig");

const LIR = core.LIR;
const LirStore = core.LirStore;
const GuardedList = collections.GuardedList;
const Allocator = std.mem.Allocator;

/// Errors produced while certifying: allocation failure or a violation of
/// the ownership rules (a compiler bug in ARC insertion).
/// `Certification` is a positive finding: ARC insertion produced refcount-incorrect code
/// (a leak, a use-after-free, or a balance mismatch)—a real bug, so it aborts the build.
/// The certifier has no incompleteness escape hatch: every procedure is
/// verified to a fixpoint (see the join handling in `runSegment`), so a
/// clean return means every emitted RC schedule was checked.
pub const CertifyError = error{ OutOfMemory, Certification };

/// Holds the first violation message for test inspection.
pub const Diagnostic = struct {
    buffer: [512]u8 = undefined,
    len: usize = 0,
    /// Local implicated by the violation, for failure-context dumps.
    context_local: ?LIR.LocalId = null,
    /// Proc containing the violation.
    context_proc: ?LIR.LirProcSpecId = null,
    /// Statement where the violation was detected.
    context_stmt: ?LIR.CFStmtId = null,
    /// Lender/holder chain of the dead value at the violation.
    chain: [8]ChainLink = undefined,
    chain_len: usize = 0,
    pub const ChainLink = struct {
        value: u32,
        origin: LIR.LocalId,
        balance: i32,
        holder: u32,
        always_live: bool,
        lender_count: usize,
    };

    pub fn message(self: *const Diagnostic) []const u8 {
        return self.buffer[0..self.len];
    }

    fn set(self: *Diagnostic, comptime fmt: []const u8, args: anytype) void {
        self.len = (std.fmt.bufPrint(&self.buffer, fmt, args) catch {
            self.len = self.buffer.len;
            return;
        }).len;
    }
};

/// Certifies every proc body in the store. Returns `error.Certification`
/// with `diag` filled on the first violation.
pub fn certifyStore(
    allocator: Allocator,
    store: *const LirStore,
    layouts: *const layout_mod.Store,
    boxy_rc_descs: []const ?LIR.BoxyDescRef,
    sigs: arc_sig.SigTable,
    roots: []const LIR.LirProcSpecId,
    diag: *Diagnostic,
) CertifyError!void {
    return certifyStoreWithWorkStats(allocator, store, layouts, boxy_rc_descs, sigs, roots, diag, null);
}

/// Deterministic work counters used by certifier complexity regression tests.
/// Production certification passes no observer and performs no counter work.
const CertifierWorkStats = struct {
    work_items: usize = 0,
    conditional_payload_splits: usize = 0,
};

fn certifyStoreWithWorkStats(
    allocator: Allocator,
    store: *const LirStore,
    layouts: *const layout_mod.Store,
    boxy_rc_descs: []const ?LIR.BoxyDescRef,
    sigs: arc_sig.SigTable,
    roots: []const LIR.LirProcSpecId,
    diag: *Diagnostic,
    work_stats: ?*CertifierWorkStats,
) CertifyError!void {
    try certifyProcAbiMetadata(allocator, store, layouts, diag);

    const rc_local = try arc_solve.computeLocalContainsRefcounted(allocator, store, layouts, boxy_rc_descs);
    defer allocator.free(rc_local);

    try certifyRcAtomicity(allocator, store, rc_local, roots, diag);
    try certifyUniqueArgs(allocator, store, rc_local, sigs, diag);

    var certifier = Certifier{
        .allocator = allocator,
        .store = store,
        .layouts = layouts,
        .sigs = sigs,
        .rc_local = rc_local,
        .lender_arena = std.heap.ArenaAllocator.init(allocator),
        .records = collections.DenseMap(LIR.JoinPointId, JoinRecord).init(allocator),
        .memo = std.AutoHashMap(MemoEntry, void).init(allocator),
        .repr_scratch = collections.DenseMap(ValueId, u32).init(allocator),
        .join_bodies = collections.DenseMap(LIR.JoinPointId, LIR.CFStmtId).init(allocator),
        .reads_before_rebind_cache = collections.DenseMap(LIR.CFStmtId, std.bit_set.DynamicBitSetUnmanaged).init(allocator),
        .erased_owner_states = collections.DenseMap(LIR.LocalId, ErasedOwnerState).init(allocator),
        .seen_outcomes = std.AutoHashMap(u16, void).init(allocator),
        .diag = diag,
        .work_stats = work_stats,
    };
    defer certifier.deinit();

    for (0..store.procSpecCount()) |index| {
        const proc_id: LIR.LirProcSpecId = @enumFromInt(@as(u32, @intCast(index)));
        const proc = store.getProcSpec(proc_id);
        const body = proc.body orelse continue;
        try certifier.certifyProc(proc_id, proc, body);
    }
}

/// Verifies the structural ownership contract of procedure arguments.
///
/// Every erased-callable entry has two hidden trailing arguments: an opaque
/// borrowed capture pointer followed by the nullable reuse pointer. The reuse
/// pointer is always an ARC-visible ownership input: its exact final-argument
/// local is recorded in `erased_reuse_arg` and has erased-callable layout, even
/// when the result has no reusable callable slot and a non-null input must be
/// decrefed. Internal Roc-ABI destination variants may also carry the marker,
/// so every non-null marker names a final erased-callable argument.
fn certifyProcAbiMetadata(
    allocator: Allocator,
    store: *const LirStore,
    layouts: *const layout_mod.Store,
    diag: *Diagnostic,
) CertifyError!void {
    for (0..store.procSpecCount()) |proc_index| {
        const proc_id: LIR.LirProcSpecId = @enumFromInt(@as(u32, @intCast(proc_index)));
        const proc = store.getProcSpec(proc_id);
        const args = store.getLocalSpan(proc.args);

        if (proc.erased_reuse_arg) |marker| {
            if (GuardedList.borrowLen(args) == 0 or
                GuardedList.at(args, GuardedList.borrowLen(args) - 1) != marker)
            {
                diag.context_proc = proc_id;
                diag.set("proc={d}: erased reuse marker must name the final argument", .{proc_index});
                return error.Certification;
            }
            const marker_layout = store.getLocal(marker).layout_idx;
            if (layouts.getLayout(marker_layout).tag != .erased_callable) {
                diag.context_proc = proc_id;
                diag.set("proc={d}: marked erased reuse argument must have erased-callable layout", .{proc_index});
                return error.Certification;
            }
        }

        if (proc.abi != .erased_callable) {
            if (proc.erased_call_args != null) {
                diag.context_proc = proc_id;
                diag.set("proc={d}: ordinary Roc ABI proc carried an erased-call argument plan", .{proc_index});
                return error.Certification;
            }
            continue;
        }
        if (GuardedList.borrowLen(args) < 2) {
            diag.context_proc = proc_id;
            diag.set("proc={d}: erased-callable ABI requires trailing capture and reuse arguments", .{proc_index});
            return error.Certification;
        }

        const capture_arg = GuardedList.at(args, GuardedList.borrowLen(args) - 2);
        if (store.getLocal(capture_arg).layout_idx != .opaque_ptr) {
            diag.context_proc = proc_id;
            diag.set("proc={d}: erased-callable capture argument must have opaque-pointer layout", .{proc_index});
            return error.Certification;
        }

        if (proc.erased_reuse_arg == null) {
            diag.context_proc = proc_id;
            diag.set("proc={d}: erased-callable reuse argument must carry its ownership marker", .{proc_index});
            return error.Certification;
        }

        const arg_plan = proc.erased_call_args orelse {
            diag.context_proc = proc_id;
            diag.set("proc={d}: erased-callable ABI proc lacks an argument plan", .{proc_index});
            return error.Certification;
        };
        try certifyErasedCallArgsPlan(
            allocator,
            store,
            layouts,
            arg_plan,
            proc.args,
            GuardedList.borrowLen(args) - 2,
            diag,
        );
    }
}

fn certifyErasedCallArgsPlan(
    allocator: Allocator,
    store: *const LirStore,
    layouts: *const layout_mod.Store,
    plan_id: LIR.ErasedCallArgsPlanId,
    args_span: LIR.LocalSpan,
    explicit_count: usize,
    diag: *Diagnostic,
) CertifyError!void {
    if (@intFromEnum(plan_id) >= store.erasedCallArgsPlanCount()) {
        diag.set("erased-call argument plan index is out of bounds", .{});
        return error.Certification;
    }

    const args = store.getLocalSpan(args_span);
    if (explicit_count > GuardedList.borrowLen(args)) {
        diag.set("erased-call argument plan has more fields than the argument span", .{});
        return error.Certification;
    }
    const arg_layouts = try allocator.alloc(layout_mod.Idx, explicit_count);
    defer allocator.free(arg_layouts);
    for (0..explicit_count) |i| {
        arg_layouts[i] = store.getLocal(GuardedList.at(args, i)).layout_idx;
    }
    const expected_offsets = try allocator.alloc(u32, explicit_count);
    defer allocator.free(expected_offsets);
    const expected = layout_mod.erased_call_abi.plan(layouts, arg_layouts, expected_offsets);

    const actual = store.getErasedCallArgsPlan(plan_id);
    const actual_offsets = store.getErasedCallArgOffsets(actual);
    if (actual.size != expected.size or
        actual.alignment != expected.alignment or
        GuardedList.borrowLen(actual_offsets) != explicit_count)
    {
        diag.set("erased-call argument plan metrics do not match its arguments", .{});
        return error.Certification;
    }
    for (expected_offsets, 0..) |expected_offset, i| {
        if (GuardedList.at(actual_offsets, i) != expected_offset) {
            diag.set("erased-call argument plan offset {d} does not match its argument", .{i});
            return error.Certification;
        }
    }
}

/// Production wrapper: certifies and panics on violation. Callers gate this
/// behind debug builds; release builds never run the certifier.
/// Mirror of the host-visibility analysis: no single-thread RC statement may
/// name a local that is flow-connected to a host-visibility seed.
fn certifyRcAtomicity(
    allocator: Allocator,
    store: *const LirStore,
    rc_local: []const bool,
    roots: []const LIR.LirProcSpecId,
    diag: *Diagnostic,
) CertifyError!void {
    var pinned = try arc_solve.computePinnedProcs(allocator, store, roots);
    defer pinned.deinit(allocator);
    var visible = try arc_solve.computeVisibility(allocator, store, rc_local, &pinned);
    defer visible.deinit(allocator);

    for (0..store.cfStmtCount()) |stmt_index| {
        const stmt = store.getCFStmt(@enumFromInt(@as(u32, @intCast(stmt_index))));
        const checked: struct { value: LIR.LocalId, atomicity: LIR.RcAtomicity } = if (stmt == .incref)
            .{ .value = stmt.incref.value, .atomicity = stmt.incref.atomicity }
        else if (stmt == .decref)
            .{ .value = stmt.decref.value, .atomicity = stmt.decref.atomicity }
        else if (stmt == .decref_if_initialized)
            .{ .value = stmt.decref_if_initialized.value, .atomicity = stmt.decref_if_initialized.atomicity }
        else if (stmt == .free)
            .{ .value = stmt.free.value, .atomicity = stmt.free.atomicity }
        else
            continue;
        if (checked.atomicity == .atomic) continue;
        const index = @intFromEnum(checked.value);
        if (index < visible.capacity() and visible.isSet(index)) {
            diag.set("stmt={d}: single-thread RC statement on host-visible local {d}", .{ stmt_index, index });
            return error.Certification;
        }
    }
}

/// Mirror of the born-unique analysis: every `assign_low_level` claiming a
/// check-free unique argument must name a position the op may runtime-check
/// and a local whose every definition is a unique birth—a fresh
/// allocation, a direct call whose callee's signature returns unique, a
/// pure same-value alias whose source is born unique, or a parameter the
/// containing proc's signature seeds born-unique. The balance and borrow
/// conditions behind the claim are enforced by the per-value certification;
/// this rule covers the unique-origin claim. Variants share all source
/// LocalIds while cloning the body statements, so origins are re-derived
/// per proc body and parameter seeds use that proc's signature.
fn certifyUniqueArgs(
    allocator: Allocator,
    store: *const LirStore,
    rc_local: []const bool,
    sigs: arc_sig.SigTable,
    diag: *Diagnostic,
) CertifyError!void {
    const local_to_dense = try allocator.alloc(u32, store.localCount());
    defer allocator.free(local_to_dense);
    @memset(local_to_dense, no_dense);
    var dense_locals = std.ArrayList(LIR.LocalId).empty;
    defer dense_locals.deinit(allocator);
    var proc_stmts = std.ArrayList(LIR.CFStmtId).empty;
    defer proc_stmts.deinit(allocator);

    const addLocal = struct {
        fn go(
            alloc: Allocator,
            rc: []const bool,
            mapping: []u32,
            locals: *std.ArrayList(LIR.LocalId),
            local: LIR.LocalId,
        ) Allocator.Error!void {
            const raw = @intFromEnum(local);
            if (raw >= rc.len or !rc[raw] or mapping[raw] != no_dense) return;
            mapping[raw] = @intCast(locals.items.len);
            try locals.append(alloc, local);
        }
    }.go;

    for (0..store.procSpecCount()) |proc_index| {
        const proc_id: LIR.LirProcSpecId = @enumFromInt(@as(u32, @intCast(proc_index)));
        const proc = store.getProcSpec(proc_id);
        const body = proc.body orelse continue;
        const sig = sigs.get(proc_id);

        for (dense_locals.items) |local| local_to_dense[@intFromEnum(local)] = no_dense;
        dense_locals.clearRetainingCapacity();
        const params = store.getLocalSpan(proc.args);
        for (0..GuardedList.borrowLen(params)) |index| {
            try addLocal(allocator, rc_local, local_to_dense, &dense_locals, GuardedList.at(params, index));
        }
        const frame_locals = store.getLocalSpan(proc.frame_locals);
        for (0..GuardedList.borrowLen(frame_locals)) |index| {
            try addLocal(allocator, rc_local, local_to_dense, &dense_locals, GuardedList.at(frame_locals, index));
        }

        try arc_solve.collectProcStatements(allocator, store, body, &proc_stmts);
        var uniqueness = try arc_solve.computeProcUniqueness(
            allocator,
            store,
            rc_local,
            sigs,
            proc_id,
            proc_stmts.items,
            local_to_dense,
            dense_locals.items.len,
        );
        defer uniqueness.deinit(allocator);

        for (proc_stmts.items) |current| {
            const stmt = store.getCFStmt(current);
            if (stmt != .assign_low_level) continue;
            const assign = stmt.assign_low_level;
            if (assign.unique_args == 0) continue;
            const stmt_index = @intFromEnum(current);
            if ((assign.unique_args & ~assign.rc_effect.may_runtime_uniqueness_check_args) != 0) {
                diag.context_proc = proc_id;
                diag.context_stmt = current;
                diag.set("stmt={d}: unique_args bit outside the op's runtime-checked argument mask", .{stmt_index});
                return error.Certification;
            }
            const args = store.getLocalSpan(assign.args);
            for (0..GuardedList.borrowLen(args)) |position| {
                const arg = GuardedList.at(args, position);
                if (position >= 64) break;
                const bit = @as(u64, 1) << @as(u6, @intCast(position));
                if ((assign.unique_args & bit) == 0) continue;
                const raw = @intFromEnum(arg);
                const dense = if (raw < local_to_dense.len) local_to_dense[raw] else no_dense;
                if (dense != no_dense and uniqueness.born_unique.isSet(dense)) continue;
                if (paramSeededUnique(sig, params, arg)) continue;
                diag.context_proc = proc_id;
                diag.context_stmt = current;
                diag.set("stmt={d}: check-free uniqueness claim on argument {d} (local {d}) without a unique birth", .{ stmt_index, position, raw });
                return error.Certification;
            }
        }
    }
}

/// True when the local is a parameter of the proc and the proc's signature
/// seeds it born-unique (a mode-specialized variant whose caller proved the
/// dying argument unique).
fn paramSeededUnique(sig: arc_sig.RcSig, params: anytype, local: LIR.LocalId) bool {
    if (sig.unique_params == 0) return false;
    for (0..GuardedList.borrowLen(params)) |position| {
        const param = GuardedList.at(params, position);
        const bit = arc_sig.paramBit(position) orelse break;
        if (param != local) continue;
        return (sig.unique_params & bit) != 0;
    }
    return false;
}

/// Like `certifyStore`, but panics with a rendered failure context instead
/// of returning `error.Certification`.
pub fn certifyStoreOrPanic(
    allocator: Allocator,
    store: *const LirStore,
    layouts: *const layout_mod.Store,
    boxy_rc_descs: []const ?LIR.BoxyDescRef,
    sigs: arc_sig.SigTable,
    roots: []const LIR.LirProcSpecId,
) Allocator.Error!void {
    var diag = Diagnostic{};
    certifyStore(allocator, store, layouts, boxy_rc_descs, sigs, roots, &diag) catch |err| switch (err) {
        error.OutOfMemory => return error.OutOfMemory,
        error.Certification => if (comptime builtin.target.os.tag == .freestanding) {
            @panic("ARC certification failed");
        } else {
            var context = FailureContext{};
            if (diag.context_proc) |proc_id| {
                var extra_locals: [8]LIR.LocalId = undefined;
                for (diag.chain[0..diag.chain_len], 0..) |link, index| {
                    extra_locals[index] = link.origin;
                }
                writeFailureContext(&context, store, layouts, sigs, proc_id, diag.context_stmt, diag.context_local, extra_locals[0..diag.chain_len]);
                var buffer: std.Io.Writer.Allocating = .init(allocator);
                defer buffer.deinit();
                debug_print.writeProc(allocator, store, layouts, proc_id, &buffer.writer) catch {};
                std.debug.print("\n{s}\n", .{buffer.written()});
            }
            std.debug.panic("ARC: {s}{s}", .{ diag.message(), context.text() });
        },
    };
}

/// Bounded, allocation-free text buffer for panic context. Output past the
/// capacity is truncated.
const FailureContext = struct {
    buffer: [65536]u8 = undefined,
    len: usize = 0,

    fn text(self: *const FailureContext) []const u8 {
        return self.buffer[0..self.len];
    }

    fn append(self: *FailureContext, comptime fmt: []const u8, args: anytype) void {
        const remaining = self.buffer[self.len..];
        const written = std.fmt.bufPrint(remaining, fmt, args) catch return;
        self.len += written.len;
    }
};

/// Writes every statement of the failing proc that mentions the implicated
/// local, plus all join/jump structure, into the panic context buffer.
fn writeFailureContext(
    context: *FailureContext,
    store: *const LirStore,
    layouts: *const layout_mod.Store,
    sigs: arc_sig.SigTable,
    proc_id: LIR.LirProcSpecId,
    stmt_id: ?LIR.CFStmtId,
    local: ?LIR.LocalId,
    extra_locals: []const LIR.LocalId,
) void {
    const proc = store.getProcSpec(proc_id);
    context.append("\nfailure context: proc={d}", .{@intFromEnum(proc_id)});
    if (store.procDebugName(proc_id)) |name| context.append(" name={s}", .{name});
    if (local) |l| {
        context.append(" local={d} layout={d}", .{
            @intFromEnum(l),
            @intFromEnum(store.getLocal(l).layout_idx),
        });
        if (store.localName(l)) |name| context.append(" local_name={s}", .{name});
    }
    context.append("\n  args:", .{});
    const proc_args = store.getLocalSpan(proc.args);
    for (0..GuardedList.borrowLen(proc_args)) |arg_index| {
        const arg = GuardedList.at(proc_args, arg_index);
        context.append(" {d}", .{@intFromEnum(arg)});
    }
    context.append("\n", .{});

    var reachable = collections.DenseMap(LIR.CFStmtId, void).init(store.allocator);
    defer reachable.deinit();
    if (proc.body) |body| {
        var walk = std.ArrayList(LIR.CFStmtId).empty;
        defer walk.deinit(store.allocator);
        walk.append(store.allocator, body) catch return;
        while (walk.pop()) |current| {
            if (reachable.contains(current)) continue;
            reachable.put(current, {}) catch return;
            switch (store.getCFStmt(current)) {
                .runtime_error, .comptime_exhaustiveness_failed, .loop_continue, .loop_break, .jump, .ret, .crash, .expect_err => {},
                .comptime_branch_taken => |s| walk.append(store.allocator, s.next) catch return,
                .switch_stmt => |s| {
                    const branches = store.getCFSwitchBranches(s.branches);
                    for (0..GuardedList.borrowLen(branches)) |branch_index| {
                        const branch = GuardedList.at(branches, branch_index);
                        walk.append(store.allocator, branch.body) catch return;
                    }
                    walk.append(store.allocator, s.default_branch) catch return;
                    if (s.continuation) |continuation| {
                        walk.append(store.allocator, continuation) catch return;
                    }
                },
                .str_match => |s| {
                    walk.append(store.allocator, s.on_match) catch return;
                    walk.append(store.allocator, s.on_miss) catch return;
                },
                .boxy_tag_match => |s| {
                    walk.append(store.allocator, s.on_match) catch return;
                    walk.append(store.allocator, s.on_miss) catch return;
                },
                .str_match_set => |s| {
                    const arms = store.getStrMatchArms(s.arms);
                    for (0..GuardedList.borrowLen(arms)) |arm_index| {
                        const arm = GuardedList.at(arms, arm_index);
                        walk.append(store.allocator, arm.on_match) catch return;
                    }
                    walk.append(store.allocator, s.on_miss) catch return;
                },
                .switch_initialized_payload => |s| {
                    walk.append(store.allocator, s.initialized_branch) catch return;
                    walk.append(store.allocator, s.uninitialized_branch) catch return;
                },
                .join => |j| {
                    walk.append(store.allocator, j.body) catch return;
                    walk.append(store.allocator, j.remainder) catch return;
                },
                inline .assign_ref, .assign_literal, .init_uninitialized, .assign_call, .assign_call_erased, .assign_packed_erased_fn, .assign_boxy_desc_ref, .assign_boxy_dict_ref, .assign_boxy_box, .assign_boxy_reuse_box, .assign_boxy_unbox, .assign_boxy_adapt, .assign_boxy_inspect, .assign_boxy_eq, .assign_boxy_tag, .assign_boxy_tag_payload, .assign_call_dict, .assign_low_level, .assign_list, .assign_struct, .assign_tag, .store_struct, .store_tag, .set_local, .debug, .expect, .incref, .decref, .decref_if_initialized, .free => |s| {
                    walk.append(store.allocator, s.next) catch return;
                },
            }
        }
    }

    for (0..store.cfStmtCount()) |index| {
        if (!reachable.contains(@enumFromInt(@as(u32, @intCast(index))))) continue;
        const stmt = store.getCFStmt(@enumFromInt(@as(u32, @intCast(index))));
        var mentions = if (local) |l| stmtMentionsLocal(store, stmt, l) else false;
        if (local) |l| {
            if (stmt == .join) {
                const jp = store.getLocalSpan(stmt.join.params);
                for (0..GuardedList.borrowLen(jp)) |jpi| {
                    if (GuardedList.at(jp, jpi) == l) mentions = true;
                }
            }
        }
        for (extra_locals) |extra| {
            mentions = mentions or stmtMentionsLocal(store, stmt, extra);
        }
        const structural = false;
        const nearby = if (stmt_id) |focus_stmt| if (index > @intFromEnum(focus_stmt))
            index - @intFromEnum(focus_stmt) <= 50
        else
            @intFromEnum(focus_stmt) - index <= 50 else false;
        if (!mentions and !structural and !nearby) continue;
        context.append("  stmt {d}: {s}", .{ index, @tagName(stmt) });
        switch (stmt) {
            .join => |j| {
                context.append(" id={d} body={d} remainder={d} params=[", .{
                    @intFromEnum(j.id), @intFromEnum(j.body), @intFromEnum(j.remainder),
                });
                const jp = store.getLocalSpan(j.params);
                for (0..GuardedList.borrowLen(jp)) |jpi| {
                    context.append(" {d}", .{@intFromEnum(GuardedList.at(jp, jpi))});
                }
                context.append(" ]", .{});
            },
            .jump => |j| context.append(" target={d}", .{@intFromEnum(j.target)}),
            .assign_ref => |a| {
                context.append(" target={d} op=", .{@intFromEnum(a.target)});
                appendRefOp(context, a.op);
                context.append(" next={d}", .{@intFromEnum(a.next)});
            },
            .set_local => |a| context.append(" target={d} value={d} mode={s} next={d}", .{
                @intFromEnum(a.target), @intFromEnum(a.value), @tagName(a.mode), @intFromEnum(a.next),
            }),
            .init_uninitialized => |a| context.append(" target={d} next={d}", .{ @intFromEnum(a.target), @intFromEnum(a.next) }),
            .incref => |rc| context.append(" value={d} next={d}", .{ @intFromEnum(rc.value), @intFromEnum(rc.next) }),
            .decref => |rc| context.append(" value={d} next={d}", .{ @intFromEnum(rc.value), @intFromEnum(rc.next) }),
            .decref_if_initialized => |rc| context.append(" cond={d}/0x{x} value={d} next={d}", .{
                @intFromEnum(rc.cond),
                rc.cond_mask,
                @intFromEnum(rc.value),
                @intFromEnum(rc.next),
            }),
            .free => |rc| context.append(" value={d} next={d}", .{ @intFromEnum(rc.value), @intFromEnum(rc.next) }),
            .assign_call => |a| {
                const sig = sigs.get(a.proc);
                context.append(" target={d} proc={d} sig(borrowed=0x{x}, ret={s}) args=", .{
                    @intFromEnum(a.target),
                    @intFromEnum(a.proc),
                    sig.borrowed_params,
                    @tagName(sig.ret_mode),
                });
                appendLocalSpan(context, store, a.args);
                context.append(" next={d}", .{@intFromEnum(a.next)});
            },
            .assign_call_erased => |a| {
                context.append(" target={d} closure={d} args=", .{ @intFromEnum(a.target), @intFromEnum(a.closure) });
                appendLocalSpan(context, store, a.args);
                if (a.result_desc) |result_desc| {
                    context.append(" result_desc=", .{});
                    appendBoxyDescRef(context, result_desc);
                }
                if (a.out_desc) |out_desc| context.append(" out_desc={d}", .{@intFromEnum(out_desc)});
                context.append(" next={d}", .{@intFromEnum(a.next)});
            },
            .assign_low_level => |a| {
                context.append(" target={d} op={s} args=", .{ @intFromEnum(a.target), @tagName(a.op) });
                appendLocalSpan(context, store, a.args);
                context.append(" next={d}", .{@intFromEnum(a.next)});
            },
            .assign_call_dict => |a| {
                context.append(" target={d} method={d} slot={d} args=", .{ @intFromEnum(a.target), @intFromEnum(a.method), a.method_slot });
                appendLocalSpan(context, store, a.args);
                context.append(" arg_descs=", .{});
                appendLocalSpan(context, store, a.arg_descs);
                context.append(" hidden=", .{});
                appendLocalSpan(context, store, a.hidden_args);
                if (a.result_desc) |result_desc| {
                    context.append(" result_desc=", .{});
                    appendBoxyDescRef(context, result_desc);
                }
                context.append(" next={d}", .{@intFromEnum(a.next)});
            },
            .str_match => |a| context.append(" source={d} match={d} miss={d}", .{
                @intFromEnum(a.source), @intFromEnum(a.on_match), @intFromEnum(a.on_miss),
            }),
            .boxy_tag_match => |a| context.append(" source={d} match={d} miss={d}", .{
                @intFromEnum(a.source), @intFromEnum(a.on_match), @intFromEnum(a.on_miss),
            }),
            .str_match_set => |a| context.append(" source={d} arms={d} miss={d}", .{
                @intFromEnum(a.source), a.arms.len, @intFromEnum(a.on_miss),
            }),
            .switch_stmt => |s| {
                context.append(" cond={d} default={d}", .{ @intFromEnum(s.cond), @intFromEnum(s.default_branch) });
                const branches = store.getCFSwitchBranches(s.branches);
                for (0..GuardedList.borrowLen(branches)) |branch_index| {
                    const branch = GuardedList.at(branches, branch_index);
                    context.append(" branch({d}->{d})", .{ branch.value, @intFromEnum(branch.body) });
                }
                if (s.continuation) |continuation| context.append(" continuation={d}", .{@intFromEnum(continuation)});
            },
            .switch_initialized_payload => |s| context.append(" cond={d}/0x{x} payload={d} initialized={d} uninitialized={d}", .{
                @intFromEnum(s.cond),
                s.cond_mask,
                @intFromEnum(s.payload),
                @intFromEnum(s.initialized_branch),
                @intFromEnum(s.uninitialized_branch),
            }),
            .ret => |r| context.append(" value={d}", .{@intFromEnum(r.value)}),
            .assign_list => |a| {
                context.append(" target={d} elems=", .{@intFromEnum(a.target)});
                appendLocalSpan(context, store, a.elems);
                context.append(" next={d}", .{@intFromEnum(a.next)});
            },
            .assign_struct => |a| {
                context.append(" target={d} fields=", .{@intFromEnum(a.target)});
                appendLocalSpan(context, store, a.fields);
                context.append(" next={d}", .{@intFromEnum(a.next)});
            },
            .assign_boxy_tag => |a| {
                const target_layout_idx = store.getLocal(a.target).layout_idx;
                const target_layout = layouts.getLayout(target_layout_idx);
                context.append(" target={d} target_layout={d}:{s}:rc={}", .{
                    @intFromEnum(a.target),
                    @intFromEnum(target_layout_idx),
                    @tagName(target_layout.tag),
                    layouts.layoutContainsRefcounted(target_layout),
                });
                if (a.payload) |payload| context.append(" payload={d} payload_layout={d} mode={s}", .{
                    @intFromEnum(payload),
                    @intFromEnum(store.getLocal(payload).layout_idx),
                    @tagName(a.payload_mode),
                });
                if (a.payload_desc) |desc| {
                    context.append(" payload_desc=", .{});
                    appendBoxyDescRef(context, desc);
                }
                context.append(" next={d}", .{@intFromEnum(a.next)});
            },
            .assign_boxy_box => |a| {
                context.append(" target={d} payload={d} payload_layout={d} mode={s}", .{
                    @intFromEnum(a.target),
                    @intFromEnum(a.payload),
                    @intFromEnum(a.payload_layout),
                    @tagName(a.payload_mode),
                });
                if (a.payload_desc) |desc| {
                    context.append(" payload_desc=", .{});
                    appendBoxyDescRef(context, desc);
                }
                context.append(" next={d}", .{@intFromEnum(a.next)});
            },
            .assign_tag => |a| {
                const target_layout_idx = store.getLocal(a.target).layout_idx;
                const target_layout = layouts.getLayout(target_layout_idx);
                context.append(" target={d} target_layout={d}:{s}:rc={}", .{
                    @intFromEnum(a.target),
                    @intFromEnum(target_layout_idx),
                    @tagName(target_layout.tag),
                    layouts.layoutContainsRefcounted(target_layout),
                });
                if (a.payload) |payload| context.append(" payload={d} payload_layout={d}", .{
                    @intFromEnum(payload),
                    @intFromEnum(store.getLocal(payload).layout_idx),
                });
                context.append(" next={d}", .{@intFromEnum(a.next)});
            },
            .assign_boxy_unbox => |a| {
                context.append(" target={d} source={d} mode={s}", .{
                    @intFromEnum(a.target),
                    @intFromEnum(a.source),
                    @tagName(a.source_mode),
                });
                if (a.target_desc) |desc| {
                    context.append(" target_desc=", .{});
                    appendBoxyDescRef(context, desc);
                }
                context.append(" next={d}", .{@intFromEnum(a.next)});
            },
            .assign_boxy_tag_payload => |a| context.append(" target={d} source={d} mode={s} next={d}", .{
                @intFromEnum(a.target),
                @intFromEnum(a.source),
                @tagName(a.source_mode),
                @intFromEnum(a.next),
            }),
            .assign_boxy_inspect => |a| context.append(" target={d} source={d} mode={s} next={d}", .{
                @intFromEnum(a.target),
                @intFromEnum(a.source),
                @tagName(a.source_mode),
                @intFromEnum(a.next),
            }),
            .assign_boxy_eq => |a| context.append(" target={d} lhs={d} rhs={d} mode={s} next={d}", .{
                @intFromEnum(a.target),
                @intFromEnum(a.lhs),
                @intFromEnum(a.rhs),
                @tagName(a.source_mode),
                @intFromEnum(a.next),
            }),
            inline .assign_literal, .assign_packed_erased_fn, .assign_boxy_desc_ref, .assign_boxy_dict_ref, .assign_boxy_reuse_box, .assign_boxy_adapt => |a| context.append(" target={d} next={d}", .{ @intFromEnum(a.target), @intFromEnum(a.next) }),
            .store_struct,
            .store_tag,
            .debug,
            .expect,
            .expect_err,
            .runtime_error,
            .comptime_exhaustiveness_failed,
            .comptime_branch_taken,
            .loop_continue,
            .loop_break,
            .crash,
            => {},
        }
        context.append("\n", .{});
    }
}

fn appendBoxyDescRef(context: *FailureContext, desc: LIR.BoxyDescRef) void {
    switch (desc) {
        .static => |id| context.append("static:{d}", .{@intFromEnum(id)}),
        .local => |local| context.append("local:{d}", .{@intFromEnum(local)}),
        .runtime => |id| context.append("runtime:{d}", .{id}),
        .dict_method_arg => |projection| context.append("dict-arg:{d}:{d}:{d}:{d}", .{
            @intFromEnum(projection.dict),
            @intFromEnum(projection.method),
            projection.method_slot,
            projection.arg_index,
        }),
        .dict_method_hidden => |projection| context.append("dict-hidden:{d}:{d}:{d}:{d}:{s}", .{
            @intFromEnum(projection.dict),
            @intFromEnum(projection.method),
            projection.method_slot,
            projection.hidden_index,
            @tagName(projection.shape),
        }),
    }
}

fn appendLocalSpan(context: *FailureContext, store: *const LirStore, span: LIR.LocalSpan) void {
    context.append("[", .{});
    const locals = store.getLocalSpan(span);
    for (0..GuardedList.borrowLen(locals)) |index| {
        const local = GuardedList.at(locals, index);
        if (index > 0) context.append(", ", .{});
        context.append("{d}", .{@intFromEnum(local)});
    }
    context.append("]", .{});
}

fn appendRefOp(context: *FailureContext, op: LIR.RefOp) void {
    switch (op) {
        .local => |source| context.append("local({d})", .{@intFromEnum(source)}),
        .discriminant => |ref| context.append("discriminant({d})", .{@intFromEnum(ref.source)}),
        .field => |ref| context.append("field({d}, {d})", .{ @intFromEnum(ref.source), ref.field_idx }),
        .tag_payload => |ref| context.append("tag_payload({d}, variant={d}, payload={d}, disc={d})", .{
            @intFromEnum(ref.source),
            ref.variant_index,
            ref.payload_idx,
            ref.tag_discriminant,
        }),
        .tag_payload_struct => |ref| context.append("tag_payload_struct({d}, variant={d}, disc={d})", .{
            @intFromEnum(ref.source),
            ref.variant_index,
            ref.tag_discriminant,
        }),
        .list_reinterpret => |ref| context.append("list_reinterpret({d})", .{@intFromEnum(ref.backing_ref)}),
        .nominal => |ref| context.append("nominal({d})", .{@intFromEnum(ref.backing_ref)}),
    }
}

fn stmtMentionsLocal(store: *const LirStore, stmt: LIR.CFStmt, needle: LIR.LocalId) bool {
    return switch (stmt) {
        .assign_ref => |a| a.target == needle or refOpReadsLocal(a.op, needle),
        .assign_literal => |a| a.target == needle,
        .assign_call => |a| a.target == needle or spanHasLocal(store, a.args, needle),
        .assign_call_erased => |a| a.target == needle or a.closure == needle or (a.reuse_source != null and a.reuse_source.? == needle) or spanHasLocal(store, a.args, needle),
        .assign_packed_erased_fn => |a| a.target == needle or (a.capture != null and a.capture.? == needle) or (a.reuse != null and a.reuse.? == needle),
        .assign_boxy_desc_ref => |a| a.target == needle or boxyDescRefReadsLocal(a.desc, needle) or
            (a.tag_residual_for != null and boxyDescRefReadsLocal(a.tag_residual_for.?, needle)) or
            spanHasLocal(store, a.captures, needle),
        .assign_boxy_dict_ref => |a| a.target == needle or boxyDictRefReadsLocal(a.dict, needle),
        .assign_boxy_box => |a| a.target == needle or a.payload == needle or
            (a.source_desc != null and boxyDescRefReadsLocal(a.source_desc.?, needle)) or
            (a.payload_desc != null and boxyDescRefReadsLocal(a.payload_desc.?, needle)),
        .assign_boxy_reuse_box => |a| a.target == needle or a.source == needle or boxyDescRefReadsLocal(a.desc, needle),
        .assign_boxy_unbox => |a| a.target == needle or a.source == needle or boxyDescRefReadsLocal(a.source_desc, needle) or
            (a.target_desc != null and boxyDescRefReadsLocal(a.target_desc.?, needle)),
        .assign_boxy_adapt => |a| a.target == needle or a.source == needle or
            (a.source_desc != null and boxyDescRefReadsLocal(a.source_desc.?, needle)) or
            (a.target_desc != null and boxyDescRefReadsLocal(a.target_desc.?, needle)),
        .assign_boxy_inspect => |a| a.target == needle or a.source == needle or boxyDescRefReadsLocal(a.source_desc, needle),
        .assign_boxy_eq => |a| a.target == needle or a.lhs == needle or a.rhs == needle or boxyDescRefReadsLocal(a.source_desc, needle),
        .assign_boxy_tag => |a| a.target == needle or boxyDescRefReadsLocal(a.target_desc, needle) or
            (a.payload != null and a.payload.? == needle) or
            (a.payload_desc != null and boxyDescRefReadsLocal(a.payload_desc.?, needle)),
        .assign_boxy_tag_payload => |a| a.target == needle or (a.target_desc != null and a.target_desc.? == needle) or
            a.source == needle or boxyDescRefReadsLocal(a.source_desc, needle),
        .assign_call_dict => |a| a.target == needle or boxyDictRefReadsLocal(a.dict, needle) or
            spanHasLocal(store, a.args, needle) or spanHasLocal(store, a.arg_descs, needle) or
            spanHasLocal(store, a.hidden_args, needle) or
            (a.result_desc != null and boxyDescRefReadsLocal(a.result_desc.?, needle)),
        .assign_low_level => |a| a.target == needle or spanHasLocal(store, a.args, needle),
        .assign_list => |a| a.target == needle or spanHasLocal(store, a.elems, needle),
        .assign_struct => |a| a.target == needle or spanHasLocal(store, a.fields, needle),
        .assign_tag => |a| a.target == needle or
            (a.target_desc != null and boxyDescRefReadsLocal(a.target_desc.?, needle)) or
            (a.payload != null and a.payload.? == needle),
        .store_struct => |a| a.dest == needle or spanHasLocal(store, a.fields, needle),
        .store_tag => |a| a.dest == needle or (a.payload != null and a.payload.? == needle),
        .set_local => |a| a.target == needle or a.value == needle,
        .init_uninitialized => |a| a.target == needle,
        .debug => |d| d.message == needle,
        .expect_err => |e| e.message == needle,
        .expect => |e| e.condition == needle,
        .incref => |rc| rc.value == needle,
        .decref => |rc| rc.value == needle,
        .decref_if_initialized => |rc| rc.cond == needle or rc.value == needle,
        .free => |rc| rc.value == needle,
        .switch_stmt => |s| s.cond == needle,
        .switch_initialized_payload => |s| s.cond == needle or s.payload == needle,
        .str_match => |s| blk: {
            if (s.source == needle) break :blk true;
            const steps = store.getStrMatchSteps(s.steps);
            for (0..GuardedList.borrowLen(steps)) |step_index| {
                const step = GuardedList.at(steps, step_index);
                switch (step.capture) {
                    .discard => {},
                    .view => |local| if (local == needle) break :blk true,
                }
            }
            break :blk false;
        },
        .str_match_set => |s| blk: {
            if (s.source == needle) break :blk true;
            const arms = store.getStrMatchArms(s.arms);
            for (0..GuardedList.borrowLen(arms)) |arm_index| {
                const arm = GuardedList.at(arms, arm_index);
                const steps = store.getStrMatchSteps(arm.steps);
                for (0..GuardedList.borrowLen(steps)) |step_index| {
                    const step = GuardedList.at(steps, step_index);
                    switch (step.capture) {
                        .discard => {},
                        .view => |local| if (local == needle) break :blk true,
                    }
                }
            }
            break :blk false;
        },
        .boxy_tag_match => |s| s.source == needle or boxyDescRefReadsLocal(s.source_desc, needle),
        .ret => |r| r.value == needle,
        .crash => |s| if (s.msg.localId()) |message| message == needle else false,
        .join, .jump, .runtime_error, .comptime_exhaustiveness_failed, .comptime_branch_taken, .loop_continue, .loop_break => false,
    };
}

fn spanHasLocal(store: *const LirStore, span: LIR.LocalSpan, needle: LIR.LocalId) bool {
    const locals = store.getLocalSpan(span);
    for (0..GuardedList.borrowLen(locals)) |index| {
        const local = GuardedList.at(locals, index);
        if (local == needle) return true;
    }
    return false;
}

fn boxyDescRefReadsLocal(desc: LIR.BoxyDescRef, needle: LIR.LocalId) bool {
    return if (desc.localOrNull()) |local| local == needle else false;
}

fn boxyDictRefReadsLocal(dict: LIR.BoxyDictRef, needle: LIR.LocalId) bool {
    return if (dict.localOrNull()) |local| local == needle else false;
}

const ValueId = u32;
const no_value: ValueId = std.math.maxInt(u32);
const no_dense: u32 = std.math.maxInt(u32);

fn appendUniqueValueId(list: *std.ArrayList(ValueId), allocator: Allocator, value: ValueId) Allocator.Error!void {
    for (list.items) |existing| {
        if (existing == value) return;
    }
    try list.append(allocator, value);
}

fn appendUniqueU32(list: *std.ArrayList(u32), allocator: Allocator, value: u32) Allocator.Error!void {
    for (list.items) |existing| {
        if (existing == value) return;
    }
    try list.append(allocator, value);
}

const PresenceCondition = struct {
    local: LIR.LocalId,
    mask: u64,

    fn eql(self: PresenceCondition, other: PresenceCondition) bool {
        return self.local == other.local and self.mask == other.mask;
    }
};

/// One exact state mutation made while transferring a call argument. A
/// restituted outcome replays these mutations backwards, proving that the
/// returned unit is the same ownership place the call received (including a
/// unit claimed from a nested aggregate field), not a manufactured unit.
const OwnershipMutation = union(enum) {
    balance: struct { value: ValueId, before: i32, after: i32 },
    claims: struct { value: ValueId, before: u64, after: u64 },
};

const RestitutionReceipt = struct {
    value: ValueId = no_value,
    mutations: []const OwnershipMutation = &.{},
};

/// Primary local rebound by an emitted statement. Outcome certification keeps
/// a discriminant witness only for the exact current binding of the proc's
/// returned local; `assign_tag` establishes a replacement witness after this
/// generic kill runs.
fn resultBindingTarget(stmt: LIR.CFStmt) ?LIR.LocalId {
    return switch (stmt) {
        inline .init_uninitialized,
        .assign_ref,
        .assign_literal,
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
        .assign_call_dict,
        .assign_low_level,
        .assign_list,
        .assign_struct,
        .assign_tag,
        .set_local,
        => |binding| binding.target,
        .store_struct => |store_stmt| store_stmt.dest,
        .store_tag => |store_stmt| store_stmt.dest,
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
        .boxy_tag_match,
        .join,
        .jump,
        .ret,
        .crash,
        .loop_continue,
        .loop_break,
        => null,
    };
}

/// Immutable per-value data shared by every forked state in one proc walk.
const ValueInfo = struct {
    /// First local bound to this value; used for stable cross-path naming.
    origin: LIR.LocalId,
    /// Values this value borrows from. The borrow is live only while every
    /// lender is reachable-live.
    lenders: []const ValueId,
    /// True for borrowed proc parameters: live for the whole call by ABI.
    always_live: bool,
    /// Container value this value was field-read from, or `no_value`. A
    /// unit-less consume or release of this value may claim the container's
    /// stored unit for that field instead of failing (a field take).
    payload_source: ValueId = no_value,
    /// Aggregate projection read from `payload_source`, encoded by
    /// `arc_dismantle.encodeProjection`.
    payload_projection: u64 = arc_dismantle.no_projection,
    /// Complete outcome rows and exact argument-transfer receipts for a
    /// direct call result. Empty for every other value.
    call_outcomes: arc_sig.OutcomeSpan = .empty,
    call_restitution: []const RestitutionReceipt = &.{},
};

/// One forked ownership state along a control-flow path.
const State = struct {
    allocator: Allocator,
    /// Dense proc-local position per store local, owned by the certifier and
    /// stable for the lifetime of every state for the current proc.
    local_dense: []const u32,
    /// Value bound to each reference-counted local used by this proc;
    /// `no_value` when unbound.
    local_value: []ValueId,
    /// Ownership units per value. Values created after a fork are absent in
    /// sibling states; absent means zero.
    balance: std.ArrayList(i32),
    /// Aggregate value currently holding a moved-in unit of this value, or
    /// `no_value`. Keeps consumed operands live until the holder dies.
    holder: std.ArrayList(ValueId),
    /// Presence condition for a value that represents conditional ownership.
    /// `no_dense` means the value is ordinary. A conditional value carries a
    /// possible ownership unit: if the condition is true, the unit exists and
    /// must be released; if false, the payload was never initialized.
    conditional_condition: std.ArrayList(u32),
    conditional_condition_mask: std.ArrayList(u64),
    /// Fields of a value whose stored units have been claimed by field
    /// takes. A claimed value's remaining unit covers only its unclaimed
    /// fields: it can no longer be released or consumed whole, and at a
    /// terminal it must be fully claimed and residual-released instead.
    claims: std.AutoHashMapUnmanaged(ValueId, u64),
    /// Scalar discriminant locals explicitly read from a direct call result
    /// carrying outcome-conditioned ownership.
    outcome_discriminants: std.AutoHashMapUnmanaged(LIR.LocalId, ValueId),
    /// Statically known discriminant of the current proc's top-level result
    /// along this exact path. The initial restitution capability consumes it
    /// before a terminal jump/return, so it never crosses a join summary.
    result_discriminant: u32 = no_dense,

    fn init(allocator: Allocator, local_dense: []const u32, proc_local_count: usize) Allocator.Error!State {
        const local_value = try allocator.alloc(ValueId, proc_local_count);
        @memset(local_value, no_value);
        return .{
            .allocator = allocator,
            .local_dense = local_dense,
            .local_value = local_value,
            .balance = .empty,
            .holder = .empty,
            .conditional_condition = .empty,
            .conditional_condition_mask = .empty,
            .claims = .empty,
            .outcome_discriminants = .empty,
            .result_discriminant = no_dense,
        };
    }

    fn deinit(self: *State) void {
        self.allocator.free(self.local_value);
        self.balance.deinit(self.allocator);
        self.holder.deinit(self.allocator);
        self.conditional_condition.deinit(self.allocator);
        self.conditional_condition_mask.deinit(self.allocator);
        self.claims.deinit(self.allocator);
        self.outcome_discriminants.deinit(self.allocator);
    }

    fn clone(self: *const State) Allocator.Error!State {
        const local_value = try self.allocator.dupe(ValueId, self.local_value);
        errdefer self.allocator.free(local_value);
        var balance = try self.balance.clone(self.allocator);
        errdefer balance.deinit(self.allocator);
        var holder = try self.holder.clone(self.allocator);
        errdefer holder.deinit(self.allocator);
        var conditional_condition = try self.conditional_condition.clone(self.allocator);
        errdefer conditional_condition.deinit(self.allocator);
        var conditional_condition_mask = try self.conditional_condition_mask.clone(self.allocator);
        errdefer conditional_condition_mask.deinit(self.allocator);
        var claims = try self.claims.clone(self.allocator);
        errdefer claims.deinit(self.allocator);
        const outcome_discriminants = try self.outcome_discriminants.clone(self.allocator);
        return .{
            .allocator = self.allocator,
            .local_dense = self.local_dense,
            .local_value = local_value,
            .balance = balance,
            .holder = holder,
            .conditional_condition = conditional_condition,
            .conditional_condition_mask = conditional_condition_mask,
            .claims = claims,
            .outcome_discriminants = outcome_discriminants,
            .result_discriminant = self.result_discriminant,
        };
    }

    fn claimsOf(self: *const State, value: ValueId) u64 {
        return self.claims.get(value) orelse 0;
    }

    fn setClaims(self: *State, value: ValueId, mask: u64) Allocator.Error!void {
        try self.claims.put(self.allocator, value, mask);
    }

    fn denseIndex(self: *const State, local: LIR.LocalId) usize {
        const raw = @intFromEnum(local);
        if (raw >= self.local_dense.len or self.local_dense[raw] == no_dense) {
            std.debug.panic("ARC certifier invariant violated: local {d} is outside the current proc-local map", .{raw});
        }
        return @intCast(self.local_dense[raw]);
    }

    fn valueOf(self: *const State, local: LIR.LocalId) ValueId {
        return self.local_value[self.denseIndex(local)];
    }

    fn valueAtDense(self: *const State, dense: usize) ValueId {
        return self.local_value[dense];
    }

    fn bindValue(self: *State, local: LIR.LocalId, value: ValueId) void {
        self.local_value[self.denseIndex(local)] = value;
    }

    fn balanceOf(self: *const State, value: ValueId) i32 {
        if (value >= self.balance.items.len) return 0;
        return self.balance.items[value];
    }

    fn holderOf(self: *const State, value: ValueId) ValueId {
        if (value >= self.holder.items.len) return no_value;
        return self.holder.items[value];
    }

    fn conditionalConditionOf(self: *const State, value: ValueId) ?PresenceCondition {
        if (value >= self.conditional_condition.items.len) return null;
        const condition = self.conditional_condition.items[value];
        if (condition == no_dense) return null;
        return .{ .local = @enumFromInt(condition), .mask = self.conditional_condition_mask.items[value] };
    }

    fn growToValue(self: *State, value: ValueId) Allocator.Error!void {
        while (self.balance.items.len <= value) {
            try self.balance.append(self.allocator, 0);
        }
        while (self.holder.items.len <= value) {
            try self.holder.append(self.allocator, no_value);
        }
        while (self.conditional_condition.items.len <= value) {
            try self.conditional_condition.append(self.allocator, no_dense);
        }
        while (self.conditional_condition_mask.items.len <= value) {
            try self.conditional_condition_mask.append(self.allocator, 0);
        }
    }

    fn addBalance(self: *State, value: ValueId, delta: i32) Allocator.Error!void {
        try self.growToValue(value);
        self.balance.items[value] += delta;
    }

    fn setHolder(self: *State, value: ValueId, holder_value: ValueId) Allocator.Error!void {
        try self.growToValue(value);
        self.holder.items[value] = holder_value;
    }

    fn setConditional(self: *State, value: ValueId, condition: PresenceCondition) Allocator.Error!void {
        try self.growToValue(value);
        self.conditional_condition.items[value] = @intFromEnum(condition.local);
        self.conditional_condition_mask.items[value] = condition.mask;
    }

    fn markDefinitelyInitialized(self: *State, value: ValueId) void {
        if (value >= self.conditional_condition.items.len) return;
        self.conditional_condition.items[value] = no_dense;
        self.conditional_condition_mask.items[value] = 0;
    }
};

/// Per-proc-local quotient-state entry used to compare states at join points
/// and to deduplicate walks of shared statement chains. Indices are dense
/// proc-local positions, not store local ids.
const LocalSummary = struct {
    class: LocalClass,
    /// Lowest dense position bound to the same value (alias-set representative).
    repr: u32,
    /// Ownership units on the value (owned class only).
    balance: u32,
    /// For borrowed locals: dense positions of the locals anchoring every
    /// normalized live lender. A single entry equal to `repr` represents an
    /// ABI-borrowed parameter, which is self-anchored.
    lender_reprs: []const u32 = &.{},
    /// True when the summarized value is a borrowed proc parameter's value:
    /// live for the whole call by ABI even while it transiently carries an
    /// ownership unit, so rebuilt states must keep it readable after the
    /// unit moves on.
    abi_live: bool = false,
    /// For conditional-owned locals: raw local id of the presence condition.
    condition: u32,
    /// For conditional-owned locals: presence mask on `condition`.
    condition_mask: u64,
    /// For owned locals: fields of the value already claimed by field takes.
    /// Set identically on every member of the alias set.
    claims: u64 = 0,
    /// For borrowed locals born from a field read: dense position of the
    /// container local the read's later claim would target, or `no_dense`.
    /// Set identically on every member of the alias set.
    payload_source: u32 = no_dense,
    /// Aggregate projection read from `payload_source`.
    payload_projection: u64 = arc_dismantle.no_projection,
};

const LocalClass = enum(u8) {
    unbound,
    owned,
    conditional_owned,
    borrowed,
    /// An inline struct binding whose RC ownership unit is gone. Its
    /// representation remains available only for same-value aliases and
    /// non-RC field reads; no operation may observe or consume RC state
    /// through it.
    representation,
};

const JoinRecord = struct {
    body: LIR.CFStmtId,
    params: LIR.LocalSpan,
    /// Dense proc-local positions whose entry state the join body relies on:
    /// every local the body subtree reads before rebinding. Jump states must
    /// agree only on these; everything else was settled before the jump.
    relevant: std.bit_set.DynamicBitSetUnmanaged,
    maybe_uninitialized_params: LIR.LocalSpan,
    maybe_uninitialized_conditions: LIR.LocalSpan,
    maybe_uninitialized_condition_masks: LIR.U64Span,
    /// Joined entry-state abstractions. Every jump summary is absorbed into
    /// exactly one group (see `absorbJoinSummary`); the body is certified
    /// once per group state, re-walked only when absorbing a summary
    /// strictly refines the group's must-alias partition.
    groups: std.ArrayList(JoinGroup),
};

/// One joined entry-state abstraction at a join point, covering every
/// mode-compatible jump summary absorbed into it.
///
/// The group state is itself a summary vector: per dense proc-local, the
/// per-name ownership mode (`class`, `condition`/`condition_mask`,
/// `lender_reprs`) is identical across all absorbed summaries, `repr` encodes
/// the *meet* (common refinement) of their must-alias partitions, and
/// `balance` carries the per-fine-class attributed units. Walking the body
/// under this state certifies every absorbed summary at once—see the
/// module doc comment for why a walk under the finer partition covers the
/// coarser member states.
const JoinGroup = struct {
    summary: []LocalSummary,
    /// A body walk with the group's current state is queued on the work
    /// stack; refinements while queued are picked up when the walk starts.
    queued: bool,
};

/// Result of absorbing one jump summary into a join's groups.
const AbsorbOutcome = union(enum) {
    /// The summary is covered by an already-walked group state.
    covered,
    /// This group must be (re)walked: it was created for the summary, or
    /// absorbing the summary refined its partition.
    walk: usize,
};

const MemoEntry = struct {
    stmt: u32,
    digest: u64,
};

const WorkItem = union(enum) {
    segment: Segment,
    join_body: JoinWalk,
};

const JoinWalk = struct {
    join: LIR.JoinPointId,
    group: usize,
};

const Segment = struct {
    cursor: LIR.CFStmtId,
    state: State,
    /// Join whose body walk produced this segment, for diagnostics.
    origin_join: ?LIR.JoinPointId = null,
};

/// Flow-insensitive producer identity for an erased-callable local. Only exact
/// representation-transparent `assign_ref` operations create an alias edge;
/// every other definition starts a new allocation identity, and multiple
/// definitions make the identity unavailable for reuse certification.
const ErasedOwnerState = union(enum) {
    root,
    alias: LIR.LocalId,
    ambiguous,
};

const ErasedCallOwnerCheck = struct {
    stmt: LIR.CFStmtId,
    closure: LIR.LocalId,
    reuse_source: LIR.LocalId,
};

const Certifier = struct {
    allocator: Allocator,
    store: *const LirStore,
    layouts: *const layout_mod.Store,
    sigs: arc_sig.SigTable,
    rc_local: []const bool,
    values: std.ArrayList(ValueInfo) = .empty,
    lender_arena: std.heap.ArenaAllocator,
    records: collections.DenseMap(LIR.JoinPointId, JoinRecord),
    memo: std.AutoHashMap(MemoEntry, void),
    /// Statements with more than one structural predecessor. Only these
    /// statements can be revisited by distinct control-flow walks, so only
    /// these need quotient-state memoization.
    memo_points: std.bit_set.DynamicBitSetUnmanaged = .{},
    summary_scratch: std.ArrayList(LocalSummary) = .empty,
    repr_scratch: collections.DenseMap(ValueId, u32),
    /// Dense position per reference-counted store local used by the proc
    /// being certified, or `no_dense` otherwise.
    local_dense: std.ArrayList(u32) = .empty,
    /// Reference-counted store local id per dense position.
    proc_locals: std.ArrayList(LIR.LocalId) = .empty,
    /// Join bodies of the proc being certified, for jump-following scans.
    join_bodies: collections.DenseMap(LIR.JoinPointId, LIR.CFStmtId),
    /// Per-proc cache for join-body read-before-rebind sets. These bitsets use
    /// dense proc-local positions, so the cache is cleared at each proc boundary.
    reads_before_rebind_cache: collections.DenseMap(LIR.CFStmtId, std.bit_set.DynamicBitSetUnmanaged),
    /// Exact erased-allocation producer relation for the current proc, plus
    /// calls checked after every reachable definition has been collected.
    erased_owner_states: collections.DenseMap(LIR.LocalId, ErasedOwnerState),
    erased_call_owner_checks: std.ArrayList(ErasedCallOwnerCheck) = .empty,
    /// Result discriminants independently reached while certifying the
    /// current outcome-specialized proc.
    seen_outcomes: std.AutoHashMap(u16, void),
    /// Scratch bitset over dense proc-local positions, reused by
    /// join-relevance extension.
    relevant_scratch: std.bit_set.DynamicBitSetUnmanaged = .{},
    /// Scratch bitset over values, reused by the liveness and borrow-anchor
    /// chain walks. Both walks unset every bit they set on the way out, so
    /// the set is all zero between top-level calls and only ever needs to
    /// track the value count.
    value_walk_scratch: std.bit_set.DynamicBitSetUnmanaged = .{},
    diag: *Diagnostic,
    work_stats: ?*CertifierWorkStats,
    /// Proc and statement being certified; written by `certifyProc` and
    /// `runSegment` before any read.
    current_proc: LIR.LirProcSpecId = undefined,
    current_sig: arc_sig.RcSig = arc_sig.RcSig.all_owned,
    current_proc_body: LIR.CFStmtId = undefined,
    current_return_local: ?LIR.LocalId = null,
    current_stmt: LIR.CFStmtId = undefined,
    /// Join whose body the current segment certifies, for diagnostics.
    current_origin_join: ?LIR.JoinPointId = null,

    fn deinit(self: *Certifier) void {
        self.values.deinit(self.allocator);
        self.lender_arena.deinit();
        self.clearRecords();
        self.records.deinit();
        self.memo.deinit();
        self.memo_points.deinit(self.allocator);
        self.summary_scratch.deinit(self.allocator);
        self.repr_scratch.deinit();
        self.local_dense.deinit(self.allocator);
        self.proc_locals.deinit(self.allocator);
        self.join_bodies.deinit();
        self.clearReadsBeforeRebindCache();
        self.seen_outcomes.clearRetainingCapacity();
        self.reads_before_rebind_cache.deinit();
        self.erased_owner_states.deinit();
        self.erased_call_owner_checks.deinit(self.allocator);
        self.seen_outcomes.deinit();
        self.relevant_scratch.deinit(self.allocator);
        self.value_walk_scratch.deinit(self.allocator);
    }

    fn clearRecords(self: *Certifier) void {
        var iter = self.records.valueIterator();
        while (iter.next()) |record| {
            record.relevant.deinit(self.allocator);
            for (record.groups.items) |group| self.allocator.free(group.summary);
            record.groups.deinit(self.allocator);
        }
        self.records.clearRetainingCapacity();
    }

    fn clearReadsBeforeRebindCache(self: *Certifier) void {
        var iter = self.reads_before_rebind_cache.valueIterator();
        while (iter.next()) |bitset| bitset.deinit(self.allocator);
        self.reads_before_rebind_cache.clearRetainingCapacity();
    }

    fn fail(self: *Certifier, comptime fmt: []const u8, args: anytype) error{Certification} {
        const full_args = .{
            @intFromEnum(self.current_proc),
            @intFromEnum(self.current_stmt),
        } ++ args;
        self.diag.context_proc = self.current_proc;
        self.diag.context_stmt = self.current_stmt;
        self.diag.context_proc = self.current_proc;
        self.diag.set("proc={d} stmt={d}: " ++ fmt, full_args);
        return error.Certification;
    }

    fn isRc(self: *const Certifier, local: LIR.LocalId) bool {
        const index = @intFromEnum(local);
        if (index >= self.rc_local.len) return false;
        return self.rc_local[index];
    }

    fn denseOf(self: *const Certifier, local: LIR.LocalId) u32 {
        const index = @intFromEnum(local);
        if (index >= self.local_dense.items.len) return no_dense;
        return self.local_dense.items[index];
    }

    fn newValue(
        self: *Certifier,
        origin: LIR.LocalId,
        lenders: []const ValueId,
        always_live: bool,
    ) Allocator.Error!ValueId {
        const id: ValueId = @intCast(self.values.items.len);
        const stored_lenders: []const ValueId = if (lenders.len == 0)
            &.{}
        else
            try self.lender_arena.allocator().dupe(ValueId, lenders);
        try self.values.append(self.allocator, .{
            .origin = origin,
            .lenders = stored_lenders,
            .always_live = always_live,
        });
        return id;
    }

    fn bindFresh(
        self: *Certifier,
        state: *State,
        local: LIR.LocalId,
        units: i32,
        lenders: []const ValueId,
    ) CertifyError!ValueId {
        return self.bindFreshWithLifetime(state, local, units, lenders, false);
    }

    fn bindFreshWithLifetime(
        self: *Certifier,
        state: *State,
        local: LIR.LocalId,
        units: i32,
        lenders: []const ValueId,
        always_live: bool,
    ) CertifyError!ValueId {
        const value = try self.newValue(local, lenders, always_live);
        try state.addBalance(value, units);
        state.bindValue(local, value);
        return value;
    }

    fn bindBorrowedFromImplicitLive(
        self: *Certifier,
        state: *State,
        local: LIR.LocalId,
    ) CertifyError!ValueId {
        const value = try self.newValue(local, &.{}, true);
        try state.growToValue(value);
        state.bindValue(local, value);
        return value;
    }

    /// Reports whether the value is reachable-live: it carries a unit, is an
    /// ABI-borrowed parameter, sits inside a live holder, or borrows from
    /// values that are all reachable-live. A value with both a holder and
    /// lenders is live through either path: the holder keeps the moved unit's
    /// allocation alive, and live lenders keep the borrowed-from allocation
    /// alive.
    fn valueIsLive(self: *Certifier, state: *const State, value: ValueId) Allocator.Error!bool {
        const seen = try self.valueWalkScratch();
        return self.valueIsLiveSeen(state, value, seen);
    }

    /// The shared chain-walk scratch, grown to the current value count. The
    /// walks' unset-on-exit discipline keeps it all zero between top-level
    /// calls, so growing (zero-filled) is the only maintenance it needs.
    fn valueWalkScratch(self: *Certifier) Allocator.Error!*std.bit_set.DynamicBitSetUnmanaged {
        if (self.value_walk_scratch.bit_length < self.values.items.len) {
            try self.value_walk_scratch.resize(self.allocator, self.values.items.len, false);
        }
        return &self.value_walk_scratch;
    }

    fn valueIsLiveSeen(self: *Certifier, state: *const State, value: ValueId, seen: *std.bit_set.DynamicBitSetUnmanaged) Allocator.Error!bool {
        if (value >= self.values.items.len) return false;
        const value_index: usize = @intCast(value);
        if (seen.isSet(value_index)) return false;
        seen.set(value_index);
        defer seen.unset(value_index);

        const info = self.values.items[value];
        if (info.always_live) return true;
        if (state.balanceOf(value) > 0) return true;
        const holder = state.holderOf(value);
        if (holder != no_value and try self.valueIsLiveSeen(state, holder, seen)) {
            return true;
        }
        if (info.lenders.len == 0) return false;
        for (info.lenders) |lender| {
            if (!try self.valueIsLiveSeen(state, lender, seen)) return false;
        }
        return true;
    }

    /// Records the dead value's lender/holder chain in the diagnostic for
    /// panic context.
    fn describeValueChain(self: *Certifier, state: *const State, value: ValueId) void {
        var cursor = value;
        var steps: usize = 0;
        self.diag.chain_len = 0;
        while (steps < 8) : (steps += 1) {
            if (cursor >= self.values.items.len) return;
            const info = self.values.items[cursor];
            if (self.diag.chain_len < self.diag.chain.len) {
                self.diag.chain[self.diag.chain_len] = .{
                    .value = cursor,
                    .origin = info.origin,
                    .balance = state.balanceOf(cursor),
                    .holder = state.holderOf(cursor),
                    .always_live = info.always_live,
                    .lender_count = info.lenders.len,
                };
                self.diag.chain_len += 1;
            }
            const holder = state.holderOf(cursor);
            if (holder != no_value) {
                cursor = holder;
                continue;
            }
            if (info.lenders.len == 0) return;
            cursor = info.lenders[0];
        }
    }

    fn requireLive(self: *Certifier, state: *const State, local: LIR.LocalId) CertifyError!ValueId {
        if (!self.isRc(local)) return no_value;
        const value = state.valueOf(local);
        if (value == no_value) {
            self.diag.context_local = local;
            self.diag.context_proc = self.current_proc;
            return self.fail("use of unbound refcounted local {d}", .{@intFromEnum(local)});
        }
        if (!try self.valueIsLive(state, value)) {
            self.diag.context_local = local;
            self.diag.context_proc = self.current_proc;
            self.describeValueChain(state, value);
            if (self.current_origin_join) |join_id| {
                return self.fail("use of dead refcounted local {d} (walking body of join {d})", .{
                    @intFromEnum(local),
                    @intFromEnum(join_id),
                });
            }
            return self.fail("use of dead refcounted local {d}", .{@intFromEnum(local)});
        }
        return value;
    }

    fn isInlineStructRepresentation(self: *const Certifier, local: LIR.LocalId) bool {
        const layout = self.layouts.getLayout(self.store.getLocal(local).layout_idx);
        return layout.tag == .struct_;
    }

    /// Requires only the inline representation of a struct, not an RC unit
    /// reachable through it. ARC may move or release every stored RC unit and
    /// still read an inline scalar sibling; all operations that can observe RC
    /// state continue to use `requireLive` instead.
    fn requireStructRepresentation(
        self: *Certifier,
        state: *const State,
        local: LIR.LocalId,
    ) CertifyError!ValueId {
        if (!self.isRc(local)) return no_value;
        if (!self.isInlineStructRepresentation(local)) return self.requireLive(state, local);
        const value = state.valueOf(local);
        if (value == no_value) {
            self.diag.context_local = local;
            self.diag.context_proc = self.current_proc;
            return self.fail("use of unbound struct representation {d}", .{@intFromEnum(local)});
        }
        return value;
    }

    /// Strict consumption: a transferred unit must exist when it leaves this
    /// proc's hands (call arguments, consumed low-level arguments, returns).
    /// A unit-less field-read value may instead claim its container's stored
    /// unit for that field (a field take).
    fn consumeUnit(self: *Certifier, state: *State, value: ValueId, local: LIR.LocalId) CertifyError!void {
        try self.consumeUnitRecording(state, value, local, null);
    }

    fn consumeUnitRecording(
        self: *Certifier,
        state: *State,
        value: ValueId,
        local: LIR.LocalId,
        mutations: ?*std.ArrayList(OwnershipMutation),
    ) CertifyError!void {
        if (value == no_value) return;
        if (state.claimsOf(value) != 0 and !self.hasIntactSurplusUnit(state, value)) {
            return self.fail("consumed partially dismantled local {d}", .{@intFromEnum(local)});
        }
        if (state.balanceOf(value) < 1) {
            const seen = try self.valueWalkScratch();
            if (try self.tryClaimSeen(state, value, seen, mutations)) return;
            return self.fail("consumed local {d} without an ownership unit", .{@intFromEnum(local)});
        }
        const before = state.balanceOf(value);
        try state.addBalance(value, -1);
        if (mutations) |list| try list.append(self.allocator, .{ .balance = .{
            .value = value,
            .before = before,
            .after = before - 1,
        } });
    }

    /// Attempts to spend the container's stored unit for the field this value
    /// was read from. A unit-less container may itself claim a complete
    /// projection from its parent, which certifies nested ownership-place
    /// transfers without flattening their runtime representation. Each stored
    /// unit can be claimed once; complete claim sets mark the corresponding
    /// unit spent (`claimsSpendUnit`) rather than releasable.
    fn tryClaim(self: *Certifier, state: *State, value: ValueId) Allocator.Error!bool {
        const seen = try self.valueWalkScratch();
        return try self.tryClaimSeen(state, value, seen, null);
    }

    fn tryClaimSeen(
        self: *Certifier,
        state: *State,
        value: ValueId,
        seen: *std.bit_set.DynamicBitSetUnmanaged,
        mutations: ?*std.ArrayList(OwnershipMutation),
    ) Allocator.Error!bool {
        if (value >= self.values.items.len) return false;
        const value_index: usize = @intCast(value);
        if (seen.isSet(value_index)) return false;
        seen.set(value_index);
        defer seen.unset(value_index);

        const info = self.values.items[value];
        if (info.payload_source == no_value) return false;
        const container = info.payload_source;
        if (info.payload_projection == arc_dismantle.no_projection) return false;
        const container_origin = self.values.items[container].origin;
        const container_layout = self.layouts.getLayout(self.store.getLocal(container_origin).layout_idx);
        const bit: u64 = switch (container_layout.tag) {
            .struct_ => blk: {
                const field_idx: u16 = @intCast(info.payload_projection & 0xffff);
                if (field_idx >= 64) return false;
                break :blk @as(u64, 1) << @intCast(field_idx);
            },
            .tag_union => blk: {
                if (!arc_dismantle.projectionOwnsAllRc(
                    self.store,
                    self.layouts,
                    container_origin,
                    info.origin,
                    info.payload_projection,
                )) return false;
                break :blk 1;
            },
            .scalar,
            .box,
            .box_of_zst,
            .erased_box,
            .list,
            .list_of_zst,
            .closure,
            .erased_callable,
            .zst,
            .ptr,
            => return false,
        };
        const existing = state.claimsOf(container);
        if (existing & bit != 0) return false;
        if (!try self.ensureClaimContainerUnit(state, container, seen, mutations)) return false;
        try state.setClaims(container, existing | bit);
        if (mutations) |list| try list.append(self.allocator, .{ .claims = .{
            .value = container,
            .before = existing,
            .after = existing | bit,
        } });
        return true;
    }

    /// Makes a nested projection container's unit explicit in the certifier's
    /// state by claiming that complete container from its own parent. This is
    /// bookkeeping only: the parent claim and child balance are the two sides
    /// of the same single runtime ownership unit.
    fn ensureClaimContainerUnit(
        self: *Certifier,
        state: *State,
        container: ValueId,
        seen: *std.bit_set.DynamicBitSetUnmanaged,
        mutations: ?*std.ArrayList(OwnershipMutation),
    ) Allocator.Error!bool {
        if (state.balanceOf(container) >= 1) return true;
        if (state.conditionalConditionOf(container) != null) return false;
        if (!try self.tryClaimSeen(state, container, seen, mutations)) return false;
        const before = state.balanceOf(container);
        try state.addBalance(container, 1);
        if (mutations) |list| try list.append(self.allocator, .{ .balance = .{
            .value = container,
            .before = before,
            .after = before + 1,
        } });
        return true;
    }

    /// Whether the value's single unit is fully spent by claims: every
    /// refcounted field's stored unit was taken or residually released, so
    /// no whole release is owed and none is allowed.
    fn claimsSpendUnit(self: *Certifier, state: *const State, value: ValueId) bool {
        const claims = state.claimsOf(value);
        if (claims == 0) return false;
        if (state.balanceOf(value) != 1) return false;
        const required = self.requiredClaimMask(value) orelse return false;
        return claims == required;
    }

    /// The refcounted-field mask a fully dismantled value must have claimed:
    /// one bit per refcounted field of its struct layout. Null when the
    /// value's layout does not support claims at all.
    fn requiredClaimMask(self: *Certifier, value: ValueId) ?u64 {
        if (value >= self.values.items.len) return null;
        const origin = self.values.items[value].origin;
        const origin_layout = self.layouts.getLayout(self.store.getLocal(origin).layout_idx);
        return switch (origin_layout.tag) {
            .struct_ => blk: {
                const info = self.layouts.getStructInfo(origin_layout);
                var mask: u64 = 0;
                for (0..info.fields.len) |i| {
                    const field = info.fields.get(@intCast(i));
                    if (field.index >= 64) return null;
                    if (!self.layouts.layoutContainsRefcounted(self.layouts.getLayout(field.layout))) continue;
                    mask |= @as(u64, 1) << @intCast(field.index);
                }
                break :blk mask;
            },
            // A tag path can claim the unit only through a projection that
            // owns every refcounted byte of the proven active payload. One
            // such claim therefore spends the tag's whole unit.
            .tag_union => 1,
            .scalar,
            .box,
            .box_of_zst,
            .erased_box,
            .list,
            .list_of_zst,
            .closure,
            .erased_callable,
            .zst,
            .ptr,
            => null,
        };
    }

    /// Whether the value still holds an intact unit beyond the one being
    /// dismantled. A container's claim bits are unique, so an outstanding
    /// claim set can only ever describe a single unit's worth of field takes
    /// and residual releases; any further unit is untouched and may be
    /// transferred or released whole. ARC relies on this when a value is both
    /// projected into a sibling and moved whole - `{ level, world:
    /// World.new(level) }` - which it lowers as an incref of the aggregate
    /// followed by dismantling the surplus. The claims stay outstanding
    /// against the remaining unit, so the terminal leak check still proves
    /// every stored unit was spent exactly once.
    fn hasIntactSurplusUnit(self: *Certifier, state: *const State, value: ValueId) bool {
        if (state.claimsOf(value) == 0) return false;
        if (state.balanceOf(value) < 2) return false;
        return self.requiredClaimMask(value) != null;
    }

    /// Aggregate consumption: one unit moves into the holder. The emitted
    /// trailing incref restores the operand's own unit, so the balance may go
    /// transiently negative here; the per-path terminal balance check flags a
    /// missing restore.
    fn consumeIntoHolder(
        self: *Certifier,
        state: *State,
        value: ValueId,
        holder_value: ValueId,
    ) CertifyError!void {
        if (value == no_value) return;
        if (state.claimsOf(value) != 0 and !self.hasIntactSurplusUnit(state, value)) {
            return self.fail(
                "partially dismantled value originating at local {d} moved into an aggregate",
                .{@intFromEnum(self.values.items[value].origin)},
            );
        }
        try state.addBalance(value, -1);
        if (holder_value != no_value) {
            try state.setHolder(value, holder_value);
        }
    }

    /// Settles deferred takes: a field-read value driven negative by an
    /// aggregate move claims its container's stored unit once the path's
    /// outcome is fixed (a terminal or a jump quotient). Valid non-take
    /// emissions never reach a settlement point negative, so this only
    /// rescues balances that were already failures before field takes.
    fn settleNegativeClaims(self: *Certifier, state: *State) Allocator.Error!void {
        for (0..state.balance.items.len) |value_index| {
            while (state.balance.items[value_index] < 0) {
                if (!try self.tryClaim(state, @intCast(value_index))) break;
                state.balance.items[value_index] += 1;
            }
        }
    }

    fn checkLeaks(self: *Certifier, state: *State) CertifyError!void {
        try self.settleNegativeClaims(state);

        for (state.balance.items, 0..) |units, value_index| {
            const claims = state.claimsOf(@intCast(value_index));
            if (claims != 0) {
                // A dismantled value's own unit must still be in hand, and
                // every refcounted field's stored unit must have been spent
                // exactly once by a take or a residual release.
                if (self.claimsSpendUnit(state, @intCast(value_index))) continue;
                const origin = self.values.items[value_index].origin;
                self.diag.context_local = origin;
                self.diag.context_proc = self.current_proc;
                if (units != 1) {
                    self.describeValueChain(state, @intCast(value_index));
                    return self.fail(
                        "partially dismantled value originating at local {d} ended with balance {d}",
                        .{ @intFromEnum(origin), units },
                    );
                }
                return self.fail(
                    "dismantled value originating at local {d} left stored units unspent",
                    .{@intFromEnum(origin)},
                );
            }
            if (units == 0) continue;
            const origin = self.values.items[value_index].origin;
            if (units > 0) {
                self.diag.context_local = origin;
                self.diag.context_proc = self.current_proc;
                self.describeValueChain(state, @intCast(value_index));
                return self.fail(
                    "leaked {d} ownership unit(s) of value originating at local {d}",
                    .{ units, @intFromEnum(origin) },
                );
            }
            return self.fail(
                "negative ownership balance for value originating at local {d}",
                .{@intFromEnum(origin)},
            );
        }
    }

    fn restitutedParamsForDiscriminant(self: *const Certifier, discriminant: u16) ?arc_sig.ParamMask {
        const outcomes = self.sigs.outcomesOf(self.current_sig);
        for (outcomes) |outcome| {
            if (outcome.discriminant == discriminant) return outcome.restituted_params;
        }
        return null;
    }

    fn applyOutcomeRestitution(self: *Certifier, state: *State) CertifyError!void {
        if (self.current_sig.outcomes.isEmpty()) return;
        if (state.result_discriminant == no_dense) {
            return self.fail("outcome-specialized return lacked an exact current result discriminant witness", .{});
        }
        const discriminant: u16 = @intCast(state.result_discriminant);
        const mask = self.restitutedParamsForDiscriminant(discriminant) orelse {
            return self.fail("returned discriminant {d} was absent from the proc's complete ARC outcome signature", .{discriminant});
        };
        try self.seen_outcomes.put(discriminant, {});
        if (mask == 0) return;
        const params = self.store.getLocalSpan(self.store.getProcSpec(self.current_proc).args);
        for (0..GuardedList.borrowLen(params)) |position| {
            const bit = arc_sig.paramBit(position) orelse break;
            if ((mask & bit) == 0) continue;
            if (self.current_sig.paramMode(position) != .owned) {
                return self.fail("outcome restitution named non-owned parameter position {d}", .{position});
            }
            const param = GuardedList.at(params, position);
            if (!self.isRc(param)) {
                return self.fail("outcome restitution named non-refcounted parameter position {d}", .{position});
            }
            const value = try self.requireLive(state, param);
            if (self.values.items[value].origin != param or
                state.balanceOf(value) != 1 or
                state.claimsOf(value) != 0 or
                (value < state.holder.items.len and state.holder.items[value] != no_value))
            {
                return self.fail(
                    "outcome {d} did not preserve the exact entry unit of parameter {d}",
                    .{ discriminant, position },
                );
            }
            try state.addBalance(value, -1);
        }
    }

    fn callOutcomeMask(self: *const Certifier, value: ValueId, discriminant: u64) ?arc_sig.ParamMask {
        if (value >= self.values.items.len or discriminant > std.math.maxInt(u16)) return null;
        const info = self.values.items[value];
        const outcomes = self.sigs.outcomesOf(.{ .outcomes = info.call_outcomes });
        for (outcomes) |outcome| {
            if (outcome.discriminant == @as(u16, @intCast(discriminant))) return outcome.restituted_params;
        }
        return null;
    }

    fn defaultCallOutcomeMask(
        self: *const Certifier,
        value: ValueId,
        branches: anytype,
    ) ?arc_sig.ParamMask {
        if (value >= self.values.items.len) return null;
        const info = self.values.items[value];
        const outcomes = self.sigs.outcomesOf(.{ .outcomes = info.call_outcomes });
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

    fn restoreCallOutcome(
        self: *Certifier,
        state: *State,
        result: ValueId,
        mask: arc_sig.ParamMask,
    ) CertifyError!void {
        if (result >= self.values.items.len) return self.fail("outcome refinement named an unknown call result", .{});
        const receipts = self.values.items[result].call_restitution;
        var bits = mask;
        while (bits != 0) {
            const position: usize = @intCast(@ctz(bits));
            const bit = arc_sig.paramBit(position).?;
            bits &= ~bit;
            if (position >= receipts.len or receipts[position].value == no_value) {
                return self.fail("outcome refinement lacked argument receipt {d}", .{position});
            }
            const mutations = receipts[position].mutations;
            var mutation_index = mutations.len;
            while (mutation_index > 0) {
                mutation_index -= 1;
                switch (mutations[mutation_index]) {
                    .balance => |mutation| {
                        if (state.balanceOf(mutation.value) != mutation.after) {
                            return self.fail("outcome restitution argument {d} balance changed before refinement", .{position});
                        }
                        try state.addBalance(mutation.value, mutation.before - mutation.after);
                    },
                    .claims => |mutation| {
                        if (state.claimsOf(mutation.value) != mutation.after) {
                            return self.fail("outcome restitution argument {d} field claims changed before refinement", .{position});
                        }
                        try state.setClaims(mutation.value, mutation.before);
                    },
                }
            }
        }
    }

    /// Builds the per-proc-local quotient summary of a state into scratch
    /// storage. The returned slice is invalidated by the next call.
    fn summarize(self: *Certifier, state: *const State) Allocator.Error![]const LocalSummary {
        self.repr_scratch.clearRetainingCapacity();
        self.summary_scratch.clearRetainingCapacity();
        try self.summary_scratch.ensureTotalCapacity(self.allocator, self.proc_locals.items.len);

        for (0..self.proc_locals.items.len) |dense| {
            const value = state.valueAtDense(dense);
            if (value == no_value) continue;
            const entry = try self.repr_scratch.getOrPut(value);
            if (!entry.found_existing) entry.value_ptr.* = @intCast(dense);
        }

        for (0..self.proc_locals.items.len) |dense| {
            var summary = LocalSummary{ .class = .unbound, .repr = 0, .balance = 0, .lender_reprs = &.{}, .condition = no_dense, .condition_mask = 0 };
            const value = state.valueAtDense(dense);
            if (value != no_value) {
                const repr = self.repr_scratch.get(value) orelse 0;
                const units = state.balanceOf(value);
                if (units > 0) {
                    if (state.conditionalConditionOf(value)) |condition| {
                        summary = .{
                            .class = .conditional_owned,
                            .repr = repr,
                            .balance = @intCast(units),
                            .lender_reprs = &.{},
                            .condition = @intFromEnum(condition.local),
                            .condition_mask = condition.mask,
                        };
                    } else {
                        summary = .{ .class = .owned, .repr = repr, .balance = @intCast(units), .lender_reprs = &.{}, .condition = no_dense, .condition_mask = 0, .claims = state.claimsOf(value) };
                    }
                } else if (try self.valueIsLive(state, value)) {
                    summary = .{
                        .class = .borrowed,
                        .repr = repr,
                        .balance = 0,
                        .lender_reprs = try self.borrowSummaryAnchorReprs(state, value),
                        .condition = no_dense,
                        .condition_mask = 0,
                    };
                    self.addPayloadOriginToSummary(value, &summary);
                } else if (self.isInlineStructRepresentation(self.proc_locals.items[dense])) {
                    summary = .{
                        .class = .representation,
                        .repr = repr,
                        .balance = 0,
                        .lender_reprs = &.{},
                        .condition = no_dense,
                        .condition_mask = 0,
                    };
                }
                summary.abi_live = self.values.items[value].always_live;
            }
            self.summary_scratch.appendAssumeCapacity(summary);
        }

        return self.summary_scratch.items;
    }

    /// Carries a borrowed projection value's claim target across a state
    /// quotient: the container's dense representative and the projection,
    /// when the container is still bound in this state. Unit-less nested
    /// containers are retained here so their chain can ultimately reach an
    /// owned root. Requires `repr_scratch` to hold the current summary's value
    /// representatives.
    fn addPayloadOriginToSummary(self: *Certifier, value: ValueId, summary: *LocalSummary) void {
        if (value >= self.values.items.len) return;
        const info = self.values.items[value];
        if (info.payload_source == no_value) return;
        const source_repr = self.repr_scratch.get(info.payload_source) orelse return;
        summary.payload_source = source_repr;
        summary.payload_projection = info.payload_projection;
    }

    /// Collects every normalized value that anchors a borrowed value in a join
    /// summary. ABI-live sources are preferred through complete lender chains:
    /// a retained intermediate may carry a unit, but it is not the durable
    /// source of a borrow whose lender chain continues to an ABI-live value.
    /// If no complete ABI-live lender chain exists, fall back to the shallowest
    /// live carriers, preserving the old unit/holder proof for non-ABI borrows.
    fn collectBorrowSummaryAnchorValues(self: *Certifier, state: *const State, value: ValueId, anchors: *std.ArrayList(ValueId)) Allocator.Error!bool {
        const seen = try self.valueWalkScratch();
        anchors.clearRetainingCapacity();
        if (try self.collectBorrowSummaryAbiAnchorsSeen(state, value, seen, anchors)) {
            return true;
        }
        anchors.clearRetainingCapacity();
        return self.collectBorrowSummaryCarrierAnchorsSeen(state, value, seen, anchors);
    }

    fn collectBorrowSummaryAbiAnchorsSeen(self: *Certifier, state: *const State, value: ValueId, seen: *std.bit_set.DynamicBitSetUnmanaged, anchors: *std.ArrayList(ValueId)) Allocator.Error!bool {
        if (value >= self.values.items.len) return false;
        const value_index: usize = @intCast(value);
        if (seen.isSet(value_index)) return false;
        seen.set(value_index);
        defer seen.unset(value_index);

        const info = self.values.items[value];
        if (info.always_live) {
            try appendUniqueValueId(anchors, self.allocator, value);
            return true;
        }
        if (info.lenders.len == 0) return false;
        const start = anchors.items.len;
        for (info.lenders) |lender| {
            if (!try self.collectBorrowSummaryAbiAnchorsSeen(state, lender, seen, anchors)) {
                anchors.shrinkRetainingCapacity(start);
                return false;
            }
        }
        return true;
    }

    fn collectBorrowSummaryCarrierAnchorsSeen(self: *Certifier, state: *const State, value: ValueId, seen: *std.bit_set.DynamicBitSetUnmanaged, anchors: *std.ArrayList(ValueId)) Allocator.Error!bool {
        if (value >= self.values.items.len) return false;
        const value_index: usize = @intCast(value);
        if (seen.isSet(value_index)) return false;
        seen.set(value_index);
        defer seen.unset(value_index);

        const info = self.values.items[value];
        if (info.always_live or state.balanceOf(value) > 0) {
            try appendUniqueValueId(anchors, self.allocator, value);
            return true;
        }
        if (info.lenders.len != 0) {
            const start = anchors.items.len;
            var complete = true;
            for (info.lenders) |lender| {
                if (!try self.collectBorrowSummaryCarrierAnchorsSeen(state, lender, seen, anchors)) {
                    complete = false;
                    break;
                }
            }
            if (complete) return true;
            anchors.shrinkRetainingCapacity(start);
        }
        const holder = state.holderOf(value);
        if (holder != no_value) return try self.collectBorrowSummaryCarrierAnchorsSeen(state, holder, seen, anchors);
        return false;
    }

    fn borrowSummaryAnchorReprs(self: *Certifier, state: *const State, value: ValueId) Allocator.Error![]const u32 {
        var anchor_values = std.ArrayList(ValueId).empty;
        defer anchor_values.deinit(self.allocator);
        if (!try self.collectBorrowSummaryAnchorValues(state, value, &anchor_values)) return &.{};

        var anchor_reprs = std.ArrayList(u32).empty;
        defer anchor_reprs.deinit(self.allocator);
        for (anchor_values.items) |anchor| {
            const repr = self.repr_scratch.get(anchor) orelse self.denseOf(self.values.items[anchor].origin);
            try appendUniqueU32(&anchor_reprs, self.allocator, repr);
        }
        std.mem.sort(u32, anchor_reprs.items, {}, comptime std.sort.asc(u32));

        const stored = try self.lender_arena.allocator().alloc(u32, anchor_reprs.items.len);
        @memcpy(stored, anchor_reprs.items);
        return stored;
    }

    fn summaryDigest(cursor: LIR.CFStmtId, summary: []const LocalSummary) u64 {
        var hasher = std.hash.Wyhash.init(0x6172635f63657274);
        hasher.update(std.mem.asBytes(&cursor));
        for (summary, 0..) |entry, dense| {
            if (entry.class == .unbound) continue;
            const dense_u32: u32 = @intCast(dense);
            hasher.update(std.mem.asBytes(&dense_u32));
            hasher.update(std.mem.asBytes(&entry.class));
            hasher.update(std.mem.asBytes(&entry.repr));
            hasher.update(std.mem.asBytes(&entry.balance));
            const lender_count: u32 = @intCast(entry.lender_reprs.len);
            hasher.update(std.mem.asBytes(&lender_count));
            for (entry.lender_reprs) |lender_repr| {
                hasher.update(std.mem.asBytes(&lender_repr));
            }
            hasher.update(std.mem.asBytes(&entry.abi_live));
            hasher.update(std.mem.asBytes(&entry.condition));
            hasher.update(std.mem.asBytes(&entry.condition_mask));
            hasher.update(std.mem.asBytes(&entry.claims));
            hasher.update(std.mem.asBytes(&entry.payload_source));
            hasher.update(std.mem.asBytes(&entry.payload_projection));
        }
        return hasher.final();
    }

    /// Rebuilds a fresh state from an agreed join-entry summary. Alias sets
    /// share one fresh value; borrows are re-linked to the fresh values of the
    /// locals their liveness anchors on.
    fn stateFromSummary(self: *Certifier, summary: []const LocalSummary) CertifyError!State {
        var state = try State.init(self.allocator, self.local_dense.items, self.proc_locals.items.len);
        errdefer state.deinit();

        for (summary, 0..) |entry, dense| {
            if (entry.class != .owned or entry.repr != dense) continue;
            const local = self.proc_locals.items[dense];
            const value = try self.bindFresh(&state, local, @intCast(entry.balance), &.{});
            if (entry.abi_live) self.values.items[value].always_live = true;
            if (entry.claims != 0) try state.setClaims(value, entry.claims);
        }
        for (summary, 0..) |entry, dense| {
            if (entry.class != .conditional_owned or entry.repr != dense) continue;
            const local = self.proc_locals.items[dense];
            const value = try self.bindFresh(&state, local, 1, &.{});
            if (entry.abi_live) self.values.items[value].always_live = true;
            try state.setConditional(value, .{ .local = @enumFromInt(entry.condition), .mask = entry.condition_mask });
        }
        for (summary, 0..) |entry, dense| {
            if (entry.class != .owned or entry.repr == dense) continue;
            const local = self.proc_locals.items[dense];
            state.bindValue(local, state.valueAtDense(entry.repr));
        }
        for (summary, 0..) |entry, dense| {
            if (entry.class != .conditional_owned or entry.repr == dense) continue;
            const local = self.proc_locals.items[dense];
            state.bindValue(local, state.valueAtDense(entry.repr));
        }
        for (summary, 0..) |entry, dense| {
            if (entry.class != .representation or entry.repr != dense) continue;
            const local = self.proc_locals.items[dense];
            _ = try self.bindFresh(&state, local, 0, &.{});
        }
        for (summary, 0..) |entry, dense| {
            if (entry.class != .representation or entry.repr == dense) continue;
            const local = self.proc_locals.items[dense];
            state.bindValue(local, state.valueAtDense(entry.repr));
        }
        for (summary, 0..) |entry, dense| {
            if (entry.class != .borrowed or entry.repr != dense) continue;
            if (entry.lender_reprs.len != 1 or entry.lender_reprs[0] != dense) continue;
            const local = self.proc_locals.items[dense];
            const value = try self.newValue(local, &.{}, true);
            try state.growToValue(value);
            state.bindValue(local, value);
        }
        for (summary, 0..) |entry, dense| {
            if (entry.class != .borrowed or entry.repr != dense) continue;
            if (entry.lender_reprs.len == 1 and entry.lender_reprs[0] == dense) continue;
            const local = self.proc_locals.items[dense];
            if (entry.lender_reprs.len == 0) {
                return self.fail("borrowed local {d} crossed a join without a live owner local", .{@intFromEnum(local)});
            }
            var lenders = std.ArrayList(ValueId).empty;
            defer lenders.deinit(self.allocator);
            for (entry.lender_reprs) |anchor_dense| {
                if (anchor_dense >= self.proc_locals.items.len) {
                    return self.fail("borrowed local {d} crossed a join without a live owner local", .{@intFromEnum(local)});
                }
                const lender = state.valueAtDense(anchor_dense);
                if (lender == no_value) {
                    return self.fail("borrowed local {d} crossed a join without a live owner local", .{@intFromEnum(local)});
                }
                try lenders.append(self.allocator, lender);
            }
            _ = try self.bindFresh(&state, local, 0, lenders.items);
        }
        // Restore projection chains only after every representative has been
        // rebuilt. A nested container's representative may sort after its
        // child, so restoring during the construction loop would make the
        // result depend on local numbering.
        for (summary, 0..) |entry, dense| {
            if (entry.class != .borrowed or entry.repr != dense) continue;
            if (entry.payload_source == no_dense or entry.payload_source >= self.proc_locals.items.len) continue;
            const value = state.valueAtDense(dense);
            const container = state.valueAtDense(entry.payload_source);
            if (value == no_value or container == no_value) continue;
            const info = &self.values.items[value];
            info.payload_source = container;
            info.payload_projection = entry.payload_projection;
        }
        for (summary, 0..) |entry, dense| {
            if (entry.class != .borrowed or entry.repr == dense) continue;
            const local = self.proc_locals.items[dense];
            state.bindValue(local, state.valueAtDense(entry.repr));
        }
        return state;
    }

    /// Groups whose balance vectors sit pointwise strictly below a new
    /// mode-identical, partition-identical summary are per-iteration
    /// accumulation evidence: two such entries can never both certify
    /// against the one shared body (every terminal path consumes a fixed
    /// unit count), so distinct balances either fail a walk or are shunted
    /// through jumps forever—unbounded refcount growth, a leak. Valid
    /// inserter output keeps balances at 0 or 1 outside transient aggregate
    /// windows, so a strictly-growing set this deep is reported as a
    /// finding. Any infinite set of distinct balance vectors contains an
    /// ascending subsequence (Dickson's lemma), so this bound also makes
    /// group creation—and with it the whole fixpoint—structurally
    /// terminating.
    const balance_growth_finding_threshold = 8;

    /// Absorbs one jump-entry summary into the join's groups.
    ///
    /// A group accepts the summary when every name's ownership mode matches
    /// (`modesCompatible`); the group's must-alias partition then becomes
    /// the meet of the two partitions and per-fine-class balances are
    /// re-attributed (`meetGroupSummary`). A summary no group accepts
    /// starts a new group, so refinement splits exactly along the entry-state
    /// modes real in-edges disagree about—every group is witnessed by at least
    /// one real jump, and a group walk can never manufacture a false
    /// positive that no refinement traces back to a real entry state.
    fn absorbJoinSummary(
        self: *Certifier,
        record: *JoinRecord,
        summary: []const LocalSummary,
        join_id: LIR.JoinPointId,
    ) CertifyError!AbsorbOutcome {
        var growth_witnesses: usize = 0;
        for (record.groups.items, 0..) |*group, group_index| {
            if (!modesCompatible(group.summary, summary)) continue;
            switch (try self.meetGroupSummary(group, summary)) {
                .unchanged => return .covered,
                .refined => return .{ .walk = group_index },
                .conflict => {
                    if (summaryBalanceAbove(summary, group.summary)) {
                        growth_witnesses += 1;
                        if (growth_witnesses >= balance_growth_finding_threshold) {
                            self.diag.context_proc = self.current_proc;
                            return self.fail(
                                "ownership balance grows without bound across jumps to join {d}: per-iteration accumulation",
                                .{@intFromEnum(join_id)},
                            );
                        }
                    }
                },
            }
        }
        const copy = try self.allocator.dupe(LocalSummary, summary);
        errdefer self.allocator.free(copy);
        try record.groups.append(self.allocator, .{ .summary = copy, .queued = false });
        return .{ .walk = record.groups.items.len - 1 };
    }

    /// True when the two summaries assign every name the same ownership
    /// mode: same class, same presence condition for conditional ownership,
    /// and the same borrow anchor. Partition (`repr`) and balances are the
    /// joinable components and are deliberately not compared here.
    fn modesCompatible(a: []const LocalSummary, b: []const LocalSummary) bool {
        for (a, b) |ga, sb| {
            if (ga.class != sb.class) return false;
            if (ga.abi_live != sb.abi_live) return false;
            switch (ga.class) {
                .unbound => {},
                // Claims are per-field spend records, not attributable
                // balances; states disagreeing on them walk separately.
                .owned => if (ga.claims != sb.claims) return false,
                .conditional_owned => if (ga.condition != sb.condition or ga.condition_mask != sb.condition_mask) return false,
                .borrowed => if (!std.mem.eql(u32, ga.lender_reprs, sb.lender_reprs) or
                    ga.payload_source != sb.payload_source or
                    ga.payload_projection != sb.payload_projection) return false,
                .representation => {},
            }
        }
        return true;
    }

    /// True when `b` and `a` have identical partitions and `a`'s balances
    /// are pointwise >= `b`'s with at least one strictly greater.
    fn summaryBalanceAbove(a: []const LocalSummary, b: []const LocalSummary) bool {
        var strict = false;
        for (a, b) |ea, eb| {
            if (ea.repr != eb.repr) return false;
            if (ea.class != .owned and ea.class != .conditional_owned) continue;
            if (ea.balance < eb.balance) return false;
            if (ea.balance > eb.balance) strict = true;
        }
        return strict;
    }

    const MeetOutcome = enum {
        /// The summary is subsumed: same partition, same balances.
        unchanged,
        /// The group's partition was refined (and balances re-attributed);
        /// the group state strictly decreased and the body must be
        /// re-walked. This happens at most once per name, so a group is
        /// re-walked at most `proc_locals.len` times.
        refined,
        /// Balance attribution failed: the summary's units cannot be
        /// reconciled with the group's (divergent entry balances, or an
        /// attribution the constraints leave ambiguous). The summary starts
        /// its own group.
        conflict,
    };

    /// Meets the group's must-alias partition with the summary's and
    /// re-attributes per-fine-class balances by constraint propagation.
    ///
    /// Two names share a meet class iff they share a class in the group AND
    /// in the summary. Each group class and each summary class then
    /// constrains the sum of its member meet-class balances to its own
    /// balance; propagation solves constraints with a single unknown until
    /// nothing changes. A full, consistent solution updates the group in
    /// place; anything else is a conflict.
    fn meetGroupSummary(self: *Certifier, group: *JoinGroup, summary: []const LocalSummary) CertifyError!MeetOutcome {
        const g = group.summary;
        const n = g.len;

        // Meet partition: representative per dense position, keyed by the
        // (group repr, summary repr) pair; the representative is the first
        // member, so `repr[dense] <= dense` with equality exactly at class
        // leaders—the shape `stateFromSummary` expects.
        var meet_repr = try self.allocator.alloc(u32, n);
        defer self.allocator.free(meet_repr);
        var pair_repr = std.AutoHashMap(u64, u32).init(self.allocator);
        defer pair_repr.deinit();
        for (g, summary, 0..) |ge, se, dense| {
            if (ge.class == .unbound) {
                meet_repr[dense] = no_dense;
                continue;
            }
            const key = (@as(u64, ge.repr) << 32) | @as(u64, se.repr);
            const entry = try pair_repr.getOrPut(key);
            if (!entry.found_existing) entry.value_ptr.* = @intCast(dense);
            meet_repr[dense] = entry.value_ptr.*;
        }

        // Balance attribution over owned/conditional meet classes. Unknowns
        // are indexed by meet-class representative; each group class and
        // each summary class contributes one sum constraint.
        var solved = try self.allocator.alloc(?u64, n);
        defer self.allocator.free(solved);
        @memset(solved, null);

        const Constraint = struct {
            total: u64,
            /// Meet-class representatives (deduplicated, this constraint's
            /// members).
            members: []u32,
        };
        var arena = std.heap.ArenaAllocator.init(self.allocator);
        defer arena.deinit();
        const scratch = arena.allocator();

        var constraints = std.ArrayList(Constraint).empty;
        // Group classes and summary classes, keyed by their own repr.
        for (0..2) |side| {
            const source = if (side == 0) g else summary;
            var class_members = std.AutoHashMap(u32, std.ArrayList(u32)).init(scratch);
            var class_total = std.AutoHashMap(u32, u64).init(scratch);
            for (source, 0..) |entry, dense| {
                if (entry.class != .owned and entry.class != .conditional_owned) continue;
                const members = try class_members.getOrPut(entry.repr);
                if (!members.found_existing) {
                    members.value_ptr.* = .empty;
                    try class_total.put(entry.repr, entry.balance);
                }
                const meet_class = meet_repr[dense];
                var already = false;
                for (members.value_ptr.items) |existing| {
                    if (existing == meet_class) {
                        already = true;
                        break;
                    }
                }
                if (!already) try members.value_ptr.append(scratch, meet_class);
            }
            var iter = class_members.iterator();
            while (iter.next()) |entry| {
                try constraints.append(scratch, .{
                    .total = class_total.get(entry.key_ptr.*).?,
                    .members = entry.value_ptr.items,
                });
            }
        }

        // Propagate: solve any constraint with exactly one unknown member;
        // verify fully-solved constraints. Each round solves at least one
        // unknown or stops, so this terminates in at most n rounds.
        var progress = true;
        while (progress) {
            progress = false;
            for (constraints.items) |constraint| {
                var assigned_sum: u64 = 0;
                var unsolved: usize = 0;
                var unsolved_class: u32 = 0;
                for (constraint.members) |meet_class| {
                    if (solved[meet_class]) |units| {
                        assigned_sum += units;
                    } else {
                        unsolved += 1;
                        unsolved_class = meet_class;
                    }
                }
                if (unsolved == 0) {
                    if (assigned_sum != constraint.total) return .conflict;
                } else if (unsolved == 1) {
                    if (assigned_sum >= constraint.total) return .conflict;
                    const remaining = constraint.total - assigned_sum;
                    // A class carrying names summarized as owned always has
                    // at least one unit on every real edge.
                    if (remaining == 0) return .conflict;
                    solved[unsolved_class] = remaining;
                    progress = true;
                }
            }
        }
        for (constraints.items) |constraint| {
            for (constraint.members) |meet_class| {
                // Under-determined attribution (proper overlaps in both
                // partitions with no forcing constraint): fall back to an
                // exact per-summary group rather than guessing.
                if (solved[meet_class] == null) return .conflict;
            }
        }

        // Commit: rewrite the group's partition and balances in place.
        var changed = false;
        for (g, 0..) |*entry, dense| {
            if (entry.class == .unbound) continue;
            const new_repr = meet_repr[dense];
            if (entry.repr != new_repr) {
                entry.repr = new_repr;
                changed = true;
            }
            if (entry.class == .owned or entry.class == .conditional_owned) {
                const units: u32 = @intCast(solved[new_repr].?);
                if (entry.balance != units) {
                    entry.balance = units;
                    changed = true;
                }
            }
        }
        return if (changed) .refined else .unchanged;
    }

    fn noteErasedOwnerDefinition(self: *Certifier, target: LIR.LocalId, source: ?LIR.LocalId) Allocator.Error!void {
        const entry = try self.erased_owner_states.getOrPut(target);
        if (entry.found_existing) {
            entry.value_ptr.* = .ambiguous;
        } else {
            entry.value_ptr.* = if (source) |owner| .{ .alias = owner } else .root;
        }
    }

    fn transparentErasedOwnershipSource(self: *const Certifier, op: LIR.RefOp, target: LIR.LocalId) ?LIR.LocalId {
        const source = switch (op) {
            .local => |local| local,
            .nominal => |nominal| nominal.backing_ref,
            inline .tag_payload, .tag_payload_struct => |payload| blk: {
                if (payload.variant_index != 0) break :blk null;
                const source_layout = self.layouts.getLayout(self.store.getLocal(payload.source).layout_idx);
                if (source_layout.tag != .tag_union) break :blk null;
                const data = self.layouts.getTagUnionData(source_layout.getTagUnion().idx);
                if (data.discriminant_size != 0) break :blk null;
                break :blk payload.source;
            },
            .discriminant, .field, .list_reinterpret => null,
        } orelse return null;

        const source_layout = self.store.getLocal(source).layout_idx;
        const target_layout = self.store.getLocal(target).layout_idx;
        const source_size = self.layouts.layoutSizeAlign(self.layouts.getLayout(source_layout)).size;
        const target_size = self.layouts.layoutSizeAlign(self.layouts.getLayout(target_layout)).size;
        return if (source_size == self.layouts.targetUsize().size() and source_size == target_size) source else null;
    }

    fn resolvedErasedOwner(self: *const Certifier, initial: LIR.LocalId) ?LIR.LocalId {
        var current = initial;
        for (0..self.erased_owner_states.count() + 1) |_| {
            const state = self.erased_owner_states.get(current) orelse return self.refcountedErasedOwner(current);
            switch (state) {
                .root => return self.refcountedErasedOwner(current),
                .alias => |source| current = source,
                .ambiguous => return null,
            }
        }
        return null;
    }

    fn refcountedErasedOwner(self: *const Certifier, local: LIR.LocalId) ?LIR.LocalId {
        const local_layout = self.layouts.getLayout(self.store.getLocal(local).layout_idx);
        return if (self.layouts.layoutContainsRefcounted(local_layout)) local else null;
    }

    fn certifyErasedCallOwnerUses(self: *Certifier) CertifyError!void {
        for (self.erased_call_owner_checks.items) |check| {
            const expected = self.resolvedErasedOwner(check.closure) orelse check.closure;
            if (check.reuse_source == expected) continue;
            self.current_stmt = check.stmt;
            return self.fail(
                "erased call closure local {d} and reuse source local {d} do not denote the same allocation",
                .{ @intFromEnum(check.closure), @intFromEnum(check.reuse_source) },
            );
        }
    }

    fn collectProcLocals(self: *Certifier, proc: LIR.LirProcSpec, body: LIR.CFStmtId) CertifyError!void {
        for (self.proc_locals.items) |local| self.local_dense.items[@intFromEnum(local)] = no_dense;
        self.proc_locals.clearRetainingCapacity();
        self.erased_owner_states.clearRetainingCapacity();
        self.erased_call_owner_checks.clearRetainingCapacity();
        if (self.local_dense.items.len < self.store.localCount()) {
            const old_len = self.local_dense.items.len;
            try self.local_dense.resize(self.allocator, self.store.localCount());
            @memset(self.local_dense.items[old_len..], no_dense);
        }

        const proc_args = self.store.getLocalSpan(proc.args);
        for (0..GuardedList.borrowLen(proc_args)) |param_index| {
            const param = GuardedList.at(proc_args, param_index);
            try self.noteProcLocal(param);
            try self.noteErasedOwnerDefinition(param, null);
        }

        var visited = collections.DenseMap(LIR.CFStmtId, void).init(self.allocator);
        defer visited.deinit();
        var stack = std.ArrayList(LIR.CFStmtId).empty;
        defer stack.deinit(self.allocator);
        try stack.append(self.allocator, body);

        while (stack.pop()) |current| {
            if (visited.contains(current)) continue;
            try visited.put(current, {});

            switch (self.store.getCFStmt(current)) {
                .assign_ref => |assign| {
                    try self.noteProcLocal(assign.target);
                    try self.noteErasedOwnerDefinition(assign.target, self.transparentErasedOwnershipSource(assign.op, assign.target));
                    switch (assign.op) {
                        .local => |source| try self.noteProcLocal(source),
                        .discriminant => |op| try self.noteProcLocal(op.source),
                        .field => |op| try self.noteProcLocal(op.source),
                        .tag_payload => |op| try self.noteProcLocal(op.source),
                        .tag_payload_struct => |op| try self.noteProcLocal(op.source),
                        .list_reinterpret => |op| try self.noteProcLocal(op.backing_ref),
                        .nominal => |op| try self.noteProcLocal(op.backing_ref),
                    }
                    try stack.append(self.allocator, assign.next);
                },
                .assign_literal => |assign| {
                    try self.noteProcLocal(assign.target);
                    try self.noteErasedOwnerDefinition(assign.target, null);
                    try stack.append(self.allocator, assign.next);
                },
                .init_uninitialized => |init| {
                    try self.noteProcLocal(init.target);
                    try self.noteErasedOwnerDefinition(init.target, null);
                    try stack.append(self.allocator, init.next);
                },
                .assign_call => |assign| {
                    try self.noteProcLocal(assign.target);
                    try self.noteErasedOwnerDefinition(assign.target, null);
                    try self.noteProcLocalSpan(assign.args);
                    try stack.append(self.allocator, assign.next);
                },
                .assign_call_erased => |assign| {
                    self.diag.context_proc = self.current_proc;
                    self.diag.context_stmt = current;
                    try certifyErasedCallArgsPlan(
                        self.allocator,
                        self.store,
                        self.layouts,
                        assign.arg_plan,
                        assign.args,
                        self.store.getLocalSpan(assign.args).len,
                        self.diag,
                    );
                    try self.noteProcLocal(assign.target);
                    try self.noteErasedOwnerDefinition(assign.target, null);
                    try self.noteProcLocal(assign.closure);
                    if (assign.reuse_source) |reuse_source| {
                        try self.noteProcLocal(reuse_source);
                        try self.erased_call_owner_checks.append(self.allocator, .{
                            .stmt = current,
                            .closure = assign.closure,
                            .reuse_source = reuse_source,
                        });
                    }
                    try self.noteProcLocalSpan(assign.args);
                    try stack.append(self.allocator, assign.next);
                },
                .assign_packed_erased_fn => |assign| {
                    try self.noteProcLocal(assign.target);
                    try self.noteErasedOwnerDefinition(assign.target, null);
                    if (assign.capture) |capture| try self.noteProcLocal(capture);
                    if (assign.result_desc) |result_desc| {
                        if (result_desc.localOrNull()) |local| try self.noteProcLocal(local);
                    }
                    if (assign.reuse) |reuse| try self.noteProcLocal(reuse);
                    try stack.append(self.allocator, assign.next);
                },
                .assign_boxy_desc_ref => |assign| {
                    try self.noteProcLocal(assign.target);
                    if (assign.desc.localOrNull()) |local| try self.noteProcLocal(local);
                    if (assign.tag_residual_for) |desc| if (desc.localOrNull()) |local| try self.noteProcLocal(local);
                    try self.noteProcLocalSpan(assign.captures);
                    try stack.append(self.allocator, assign.next);
                },
                .assign_boxy_dict_ref => |assign| {
                    try self.noteProcLocal(assign.target);
                    if (assign.dict.localOrNull()) |local| try self.noteProcLocal(local);
                    try stack.append(self.allocator, assign.next);
                },
                .assign_boxy_box => |assign| {
                    try self.noteProcLocal(assign.target);
                    try self.noteProcLocal(assign.payload);
                    if (assign.payload_desc) |desc| if (desc.localOrNull()) |local| try self.noteProcLocal(local);
                    try stack.append(self.allocator, assign.next);
                },
                .assign_boxy_reuse_box => |assign| {
                    try self.noteProcLocal(assign.target);
                    try self.noteProcLocal(assign.source);
                    if (assign.desc.localOrNull()) |local| try self.noteProcLocal(local);
                    try stack.append(self.allocator, assign.next);
                },
                .assign_boxy_unbox => |assign| {
                    try self.noteProcLocal(assign.target);
                    try self.noteProcLocal(assign.source);
                    if (assign.source_desc.localOrNull()) |local| try self.noteProcLocal(local);
                    if (assign.target_desc) |desc| if (desc.localOrNull()) |local| try self.noteProcLocal(local);
                    try stack.append(self.allocator, assign.next);
                },
                .assign_boxy_adapt => |assign| {
                    try self.noteProcLocal(assign.target);
                    try self.noteProcLocal(assign.source);
                    if (assign.source_desc) |desc| if (desc.localOrNull()) |local| try self.noteProcLocal(local);
                    if (assign.target_desc) |desc| if (desc.localOrNull()) |local| try self.noteProcLocal(local);
                    try stack.append(self.allocator, assign.next);
                },
                .assign_boxy_inspect => |assign| {
                    try self.noteProcLocal(assign.target);
                    try self.noteProcLocal(assign.source);
                    if (assign.source_desc.localOrNull()) |local| try self.noteProcLocal(local);
                    try stack.append(self.allocator, assign.next);
                },
                .assign_boxy_eq => |assign| {
                    try self.noteProcLocal(assign.target);
                    try self.noteProcLocal(assign.lhs);
                    try self.noteProcLocal(assign.rhs);
                    if (assign.source_desc.localOrNull()) |local| try self.noteProcLocal(local);
                    try stack.append(self.allocator, assign.next);
                },
                .assign_boxy_tag => |assign| {
                    try self.noteProcLocal(assign.target);
                    if (assign.target_desc.localOrNull()) |local| try self.noteProcLocal(local);
                    if (assign.payload) |payload| try self.noteProcLocal(payload);
                    if (assign.payload_desc) |desc| {
                        if (desc.localOrNull()) |local| try self.noteProcLocal(local);
                    }
                    try stack.append(self.allocator, assign.next);
                },
                .assign_boxy_tag_payload => |assign| {
                    try self.noteProcLocal(assign.target);
                    if (assign.target_desc) |target_desc| try self.noteProcLocal(target_desc);
                    try self.noteProcLocal(assign.source);
                    if (assign.source_desc.localOrNull()) |local| try self.noteProcLocal(local);
                    try stack.append(self.allocator, assign.next);
                },
                .assign_call_dict => |assign| {
                    try self.noteProcLocal(assign.target);
                    if (assign.dict.localOrNull()) |local| try self.noteProcLocal(local);
                    if (assign.result_desc) |result_desc| {
                        if (result_desc.localOrNull()) |local| try self.noteProcLocal(local);
                    }
                    try self.noteProcLocalSpan(assign.args);
                    try self.noteProcLocalSpan(assign.arg_descs);
                    try self.noteProcLocalSpan(assign.hidden_args);
                    try stack.append(self.allocator, assign.next);
                },
                .assign_low_level => |assign| {
                    try self.noteProcLocal(assign.target);
                    try self.noteErasedOwnerDefinition(assign.target, null);
                    try self.noteProcLocalSpan(assign.args);
                    try stack.append(self.allocator, assign.next);
                },
                .assign_list => |assign| {
                    try self.noteProcLocal(assign.target);
                    try self.noteErasedOwnerDefinition(assign.target, null);
                    try self.noteProcLocalSpan(assign.elems);
                    try stack.append(self.allocator, assign.next);
                },
                .assign_struct => |assign| {
                    try self.noteProcLocal(assign.target);
                    try self.noteErasedOwnerDefinition(assign.target, null);
                    try self.noteProcLocalSpan(assign.fields);
                    try stack.append(self.allocator, assign.next);
                },
                .assign_tag => |assign| {
                    try self.noteProcLocal(assign.target);
                    try self.noteErasedOwnerDefinition(assign.target, null);
                    if (assign.payload) |payload| try self.noteProcLocal(payload);
                    try stack.append(self.allocator, assign.next);
                },
                .store_struct => |assign| {
                    try self.noteProcLocal(assign.dest);
                    try self.noteProcLocalSpan(assign.fields);
                    try stack.append(self.allocator, assign.next);
                },
                .store_tag => |assign| {
                    try self.noteProcLocal(assign.dest);
                    if (assign.payload) |payload| try self.noteProcLocal(payload);
                    try stack.append(self.allocator, assign.next);
                },
                .set_local => |assign| {
                    try self.noteProcLocal(assign.target);
                    try self.noteErasedOwnerDefinition(assign.target, null);
                    try self.noteProcLocal(assign.value);
                    try stack.append(self.allocator, assign.next);
                },
                .debug => |debug_stmt| {
                    try self.noteProcLocal(debug_stmt.message);
                    try stack.append(self.allocator, debug_stmt.next);
                },
                .expect_err => |expect_err_stmt| try self.noteProcLocal(expect_err_stmt.message),
                .expect => |expect_stmt| {
                    try self.noteProcLocal(expect_stmt.condition);
                    try stack.append(self.allocator, expect_stmt.next);
                },
                .incref => |rc| {
                    try self.noteProcLocal(rc.value);
                    try stack.append(self.allocator, rc.next);
                },
                .decref => |rc| {
                    try self.noteProcLocal(rc.value);
                    try stack.append(self.allocator, rc.next);
                },
                .decref_if_initialized => |rc| {
                    try self.noteProcLocal(rc.cond);
                    try self.noteProcLocal(rc.value);
                    try stack.append(self.allocator, rc.next);
                },
                .free => |rc| {
                    try self.noteProcLocal(rc.value);
                    try stack.append(self.allocator, rc.next);
                },
                .switch_stmt => |switch_stmt| {
                    try self.noteProcLocal(switch_stmt.cond);
                    const branches = self.store.getCFSwitchBranches(switch_stmt.branches);
                    for (0..GuardedList.borrowLen(branches)) |branch_index| {
                        const branch = GuardedList.at(branches, branch_index);
                        try stack.append(self.allocator, branch.body);
                    }
                    try stack.append(self.allocator, switch_stmt.default_branch);
                    if (switch_stmt.continuation) |continuation| {
                        try stack.append(self.allocator, continuation);
                    }
                },
                .switch_initialized_payload => |switch_stmt| {
                    try self.noteProcLocal(switch_stmt.cond);
                    try self.noteProcLocal(switch_stmt.payload);
                    try stack.append(self.allocator, switch_stmt.initialized_branch);
                    try stack.append(self.allocator, switch_stmt.uninitialized_branch);
                },
                .str_match => |str_match| {
                    try self.noteProcLocal(str_match.source);
                    const steps = self.store.getStrMatchSteps(str_match.steps);
                    for (0..GuardedList.borrowLen(steps)) |step_index| {
                        const step = GuardedList.at(steps, step_index);
                        switch (step.capture) {
                            .discard => {},
                            .view => |local| {
                                try self.noteProcLocal(local);
                                try self.noteErasedOwnerDefinition(local, null);
                            },
                        }
                    }
                    try stack.append(self.allocator, str_match.on_match);
                    try stack.append(self.allocator, str_match.on_miss);
                },
                .boxy_tag_match => |tag_match| {
                    try self.noteProcLocal(tag_match.source);
                    if (tag_match.source_desc.localOrNull()) |local| try self.noteProcLocal(local);
                    try stack.append(self.allocator, tag_match.on_match);
                    try stack.append(self.allocator, tag_match.on_miss);
                },
                .str_match_set => |str_match_set| {
                    try self.noteProcLocal(str_match_set.source);
                    const arms = self.store.getStrMatchArms(str_match_set.arms);
                    for (0..GuardedList.borrowLen(arms)) |arm_index| {
                        const arm = GuardedList.at(arms, arm_index);
                        const steps = self.store.getStrMatchSteps(arm.steps);
                        for (0..GuardedList.borrowLen(steps)) |step_index| {
                            const step = GuardedList.at(steps, step_index);
                            switch (step.capture) {
                                .discard => {},
                                .view => |local| {
                                    try self.noteProcLocal(local);
                                    try self.noteErasedOwnerDefinition(local, null);
                                },
                            }
                        }
                        try stack.append(self.allocator, arm.on_match);
                    }
                    try stack.append(self.allocator, str_match_set.on_miss);
                },
                .join => |join_stmt| {
                    try self.noteProcLocalSpan(join_stmt.params);
                    const params = self.store.getLocalSpan(join_stmt.params);
                    for (0..GuardedList.borrowLen(params)) |param_index| {
                        try self.noteErasedOwnerDefinition(GuardedList.at(params, param_index), null);
                    }
                    try self.join_bodies.put(join_stmt.id, join_stmt.body);
                    try stack.append(self.allocator, join_stmt.body);
                    try stack.append(self.allocator, join_stmt.remainder);
                },
                .ret => |ret_stmt| try self.noteProcLocal(ret_stmt.value),
                .crash => |crash_stmt| if (crash_stmt.msg.localId()) |message| try self.noteProcLocal(message),
                .jump, .runtime_error, .comptime_exhaustiveness_failed, .loop_continue, .loop_break => {},
                .comptime_branch_taken => |marker| try stack.append(self.allocator, marker.next),
            }
        }

        try self.certifyErasedCallOwnerUses();
    }

    /// Marks the control-flow convergence points that can be reached more
    /// than once. Straight-line statements have exactly one structural
    /// predecessor and cannot participate in a fixpoint or shared-tail
    /// deduplication, so summarizing the whole proc state at each one would
    /// turn large generated initializers into quadratic work.
    fn collectMemoPoints(self: *Certifier, body: LIR.CFStmtId) Allocator.Error!void {
        const stmt_count = self.store.cfStmtCount();
        try self.memo_points.resize(self.allocator, stmt_count, false);
        self.memo_points.unsetAll();

        const predecessor_counts = try self.allocator.alloc(u8, stmt_count);
        defer self.allocator.free(predecessor_counts);
        @memset(predecessor_counts, 0);

        var visited = try std.bit_set.DynamicBitSetUnmanaged.initEmpty(self.allocator, stmt_count);
        defer visited.deinit(self.allocator);
        var stack = std.ArrayList(LIR.CFStmtId).empty;
        defer stack.deinit(self.allocator);

        const Walk = struct {
            fn add(
                counts: []u8,
                work: *std.ArrayList(LIR.CFStmtId),
                allocator: Allocator,
                successor: LIR.CFStmtId,
            ) Allocator.Error!void {
                const index = @intFromEnum(successor);
                if (counts[index] < 2) counts[index] += 1;
                try work.append(allocator, successor);
            }
        };

        // The procedure entry is a structural predecessor. A back edge to
        // the entry therefore makes it a memo point like any other cycle.
        predecessor_counts[@intFromEnum(body)] = 1;
        try stack.append(self.allocator, body);

        while (stack.pop()) |current| {
            const current_index = @intFromEnum(current);
            if (visited.isSet(current_index)) continue;
            visited.set(current_index);

            switch (self.store.getCFStmt(current)) {
                inline .assign_ref,
                .assign_literal,
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
                .incref,
                .decref,
                .decref_if_initialized,
                .free,
                .comptime_branch_taken,
                => |stmt| try Walk.add(predecessor_counts, &stack, self.allocator, stmt.next),
                .switch_stmt => |switch_stmt| {
                    const branches = self.store.getCFSwitchBranches(switch_stmt.branches);
                    for (0..GuardedList.borrowLen(branches)) |branch_index| {
                        const branch = GuardedList.at(branches, branch_index);
                        try Walk.add(predecessor_counts, &stack, self.allocator, branch.body);
                    }
                    try Walk.add(predecessor_counts, &stack, self.allocator, switch_stmt.default_branch);
                    if (switch_stmt.continuation) |continuation| {
                        try Walk.add(predecessor_counts, &stack, self.allocator, continuation);
                    }
                },
                .switch_initialized_payload => |switch_stmt| {
                    try Walk.add(predecessor_counts, &stack, self.allocator, switch_stmt.initialized_branch);
                    try Walk.add(predecessor_counts, &stack, self.allocator, switch_stmt.uninitialized_branch);
                },
                .str_match => |str_match| {
                    try Walk.add(predecessor_counts, &stack, self.allocator, str_match.on_match);
                    try Walk.add(predecessor_counts, &stack, self.allocator, str_match.on_miss);
                },
                .boxy_tag_match => |tag_match| {
                    try Walk.add(predecessor_counts, &stack, self.allocator, tag_match.on_match);
                    try Walk.add(predecessor_counts, &stack, self.allocator, tag_match.on_miss);
                },
                .str_match_set => |str_match_set| {
                    const arms = self.store.getStrMatchArms(str_match_set.arms);
                    for (0..GuardedList.borrowLen(arms)) |arm_index| {
                        const arm = GuardedList.at(arms, arm_index);
                        try Walk.add(predecessor_counts, &stack, self.allocator, arm.on_match);
                    }
                    try Walk.add(predecessor_counts, &stack, self.allocator, str_match_set.on_miss);
                },
                .join => |join_stmt| {
                    // A join body is a separately scheduled control-flow
                    // root; jumps contribute its remaining predecessors.
                    try Walk.add(predecessor_counts, &stack, self.allocator, join_stmt.body);
                    try Walk.add(predecessor_counts, &stack, self.allocator, join_stmt.remainder);
                },
                .jump => |jump_stmt| {
                    if (self.join_bodies.get(jump_stmt.target)) |target_body| {
                        try Walk.add(predecessor_counts, &stack, self.allocator, target_body);
                    }
                },
                .expect_err,
                .ret,
                .crash,
                .runtime_error,
                .comptime_exhaustiveness_failed,
                .loop_continue,
                .loop_break,
                => {},
            }
        }

        for (predecessor_counts, 0..) |count, index| {
            if (count > 1) self.memo_points.set(index);
        }
    }

    fn noteExposedReadLocal(
        self: *const Certifier,
        relevant: *std.bit_set.DynamicBitSetUnmanaged,
        local: LIR.LocalId,
    ) void {
        if (!self.isRc(local)) return;
        const dense = self.denseOf(local);
        if (dense == no_dense) return;
        relevant.set(dense);
    }

    fn noteExposedReadSpan(
        self: *const Certifier,
        relevant: *std.bit_set.DynamicBitSetUnmanaged,
        span: LIR.LocalSpan,
    ) void {
        const locals = self.store.getLocalSpan(span);
        for (0..GuardedList.borrowLen(locals)) |index| {
            const local = GuardedList.at(locals, index);
            self.noteExposedReadLocal(relevant, local);
        }
    }

    fn noteExposedRefOpRead(
        self: *const Certifier,
        relevant: *std.bit_set.DynamicBitSetUnmanaged,
        op: LIR.RefOp,
    ) void {
        const local = switch (op) {
            .local => |source| source,
            .discriminant => |ref| ref.source,
            .field => |ref| ref.source,
            .tag_payload => |ref| ref.source,
            .tag_payload_struct => |ref| ref.source,
            .list_reinterpret => |ref| ref.backing_ref,
            .nominal => |ref| ref.backing_ref,
        };
        self.noteExposedReadLocal(relevant, local);
    }

    const ReadBeforeRebindNode = struct {
        stmt: LIR.CFStmtId,
        reads: std.bit_set.DynamicBitSetUnmanaged,
        exposed: std.bit_set.DynamicBitSetUnmanaged,
        successor_start: usize,
        successor_len: usize,
        def: ?LIR.LocalId,
    };

    const ReadBeforeRebindGraph = struct {
        allocator: Allocator,
        nodes: std.ArrayList(ReadBeforeRebindNode),
        successors: std.ArrayList(LIR.CFStmtId),
        indices: collections.DenseMap(LIR.CFStmtId, usize),

        fn init(allocator: Allocator) ReadBeforeRebindGraph {
            return .{
                .allocator = allocator,
                .nodes = .empty,
                .successors = .empty,
                .indices = collections.DenseMap(LIR.CFStmtId, usize).init(allocator),
            };
        }
    };

    fn ensureReadBeforeRebindNode(
        self: *Certifier,
        graph: *ReadBeforeRebindGraph,
        work: *std.ArrayList(LIR.CFStmtId),
        stmt: LIR.CFStmtId,
    ) Allocator.Error!void {
        if (graph.indices.contains(stmt)) return;

        var reads = try std.bit_set.DynamicBitSetUnmanaged.initEmpty(graph.allocator, self.proc_locals.items.len);
        errdefer reads.deinit(graph.allocator);
        var exposed = try std.bit_set.DynamicBitSetUnmanaged.initEmpty(graph.allocator, self.proc_locals.items.len);
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
        self: *Certifier,
        graph: *ReadBeforeRebindGraph,
        work: *std.ArrayList(LIR.CFStmtId),
        node_index: usize,
        successor: LIR.CFStmtId,
    ) Allocator.Error!void {
        const successor_index = graph.successors.items.len;
        if (graph.nodes.items[node_index].successor_len == 0) {
            graph.nodes.items[node_index].successor_start = successor_index;
        }
        try graph.successors.append(graph.allocator, successor);
        graph.nodes.items[node_index].successor_len += 1;
        try self.ensureReadBeforeRebindNode(graph, work, successor);
    }

    fn setReadBeforeRebindDef(
        self: *const Certifier,
        graph: *ReadBeforeRebindGraph,
        node_index: usize,
        local: LIR.LocalId,
    ) void {
        if (self.isRc(local)) graph.nodes.items[node_index].def = local;
    }

    fn computeReadsBeforeRebind(self: *Certifier, start: LIR.CFStmtId) Allocator.Error!*const std.bit_set.DynamicBitSetUnmanaged {
        if (self.reads_before_rebind_cache.getPtr(start)) |cached| {
            return cached;
        }
        errdefer self.clearReadsBeforeRebindCache();

        var graph_arena = std.heap.ArenaAllocator.init(self.allocator);
        defer graph_arena.deinit();
        const graph_allocator = graph_arena.allocator();

        var graph = ReadBeforeRebindGraph.init(graph_allocator);
        var work = std.ArrayList(LIR.CFStmtId).empty;
        var cache_roots = std.ArrayList(LIR.CFStmtId).empty;

        try self.ensureReadBeforeRebindNode(&graph, &work, self.current_proc_body);

        while (work.pop()) |stmt| {
            const node_index = graph.indices.get(stmt) orelse unreachable;

            switch (self.store.getCFStmt(stmt)) {
                .assign_ref => |assign| {
                    self.noteExposedRefOpRead(&graph.nodes.items[node_index].reads, assign.op);
                    self.setReadBeforeRebindDef(&graph, node_index, assign.target);
                    try self.appendReadBeforeRebindSuccessor(&graph, &work, node_index, assign.next);
                },
                .assign_literal => |assign| {
                    self.setReadBeforeRebindDef(&graph, node_index, assign.target);
                    try self.appendReadBeforeRebindSuccessor(&graph, &work, node_index, assign.next);
                },
                .init_uninitialized => |init| {
                    self.setReadBeforeRebindDef(&graph, node_index, init.target);
                    try self.appendReadBeforeRebindSuccessor(&graph, &work, node_index, init.next);
                },
                .assign_call => |assign| {
                    self.noteExposedReadSpan(&graph.nodes.items[node_index].reads, assign.args);
                    self.setReadBeforeRebindDef(&graph, node_index, assign.target);
                    try self.appendReadBeforeRebindSuccessor(&graph, &work, node_index, assign.next);
                },
                .assign_call_erased => |assign| {
                    self.noteExposedReadLocal(&graph.nodes.items[node_index].reads, assign.closure);
                    if (assign.reuse_source) |reuse_source| self.noteExposedReadLocal(&graph.nodes.items[node_index].reads, reuse_source);
                    self.noteExposedReadSpan(&graph.nodes.items[node_index].reads, assign.args);
                    if (assign.result_desc) |result_desc| {
                        if (result_desc.localOrNull()) |local| self.noteExposedReadLocal(&graph.nodes.items[node_index].reads, local);
                    }
                    self.setReadBeforeRebindDef(&graph, node_index, assign.target);
                    if (assign.out_desc) |out_desc| self.setReadBeforeRebindDef(&graph, node_index, out_desc);
                    try self.appendReadBeforeRebindSuccessor(&graph, &work, node_index, assign.next);
                },
                .assign_packed_erased_fn => |assign| {
                    if (assign.capture) |capture| self.noteExposedReadLocal(&graph.nodes.items[node_index].reads, capture);
                    if (assign.result_desc) |result_desc| {
                        if (result_desc.localOrNull()) |local| self.noteExposedReadLocal(&graph.nodes.items[node_index].reads, local);
                    }
                    if (assign.reuse) |reuse| self.noteExposedReadLocal(&graph.nodes.items[node_index].reads, reuse);
                    self.setReadBeforeRebindDef(&graph, node_index, assign.target);
                    try self.appendReadBeforeRebindSuccessor(&graph, &work, node_index, assign.next);
                },
                .assign_boxy_desc_ref => |assign| {
                    if (assign.desc.localOrNull()) |local| self.noteExposedReadLocal(&graph.nodes.items[node_index].reads, local);
                    const captures = self.store.getLocalSpan(assign.captures);
                    for (0..GuardedList.borrowLen(captures)) |index| {
                        const local = GuardedList.at(captures, index);
                        self.noteExposedReadLocal(&graph.nodes.items[node_index].reads, local);
                    }
                    self.setReadBeforeRebindDef(&graph, node_index, assign.target);
                    try self.appendReadBeforeRebindSuccessor(&graph, &work, node_index, assign.next);
                },
                .assign_boxy_dict_ref => |assign| {
                    if (assign.dict.localOrNull()) |local| self.noteExposedReadLocal(&graph.nodes.items[node_index].reads, local);
                    self.setReadBeforeRebindDef(&graph, node_index, assign.target);
                    try self.appendReadBeforeRebindSuccessor(&graph, &work, node_index, assign.next);
                },
                .assign_boxy_box => |assign| {
                    self.noteExposedReadLocal(&graph.nodes.items[node_index].reads, assign.payload);
                    if (assign.payload_desc) |desc| if (desc.localOrNull()) |local| self.noteExposedReadLocal(&graph.nodes.items[node_index].reads, local);
                    self.setReadBeforeRebindDef(&graph, node_index, assign.target);
                    try self.appendReadBeforeRebindSuccessor(&graph, &work, node_index, assign.next);
                },
                .assign_boxy_reuse_box => |assign| {
                    self.noteExposedReadLocal(&graph.nodes.items[node_index].reads, assign.source);
                    if (assign.desc.localOrNull()) |local| self.noteExposedReadLocal(&graph.nodes.items[node_index].reads, local);
                    self.setReadBeforeRebindDef(&graph, node_index, assign.target);
                    try self.appendReadBeforeRebindSuccessor(&graph, &work, node_index, assign.next);
                },
                .assign_boxy_unbox => |assign| {
                    self.noteExposedReadLocal(&graph.nodes.items[node_index].reads, assign.source);
                    if (assign.source_desc.localOrNull()) |local| self.noteExposedReadLocal(&graph.nodes.items[node_index].reads, local);
                    if (assign.target_desc) |desc| if (desc.localOrNull()) |local| self.noteExposedReadLocal(&graph.nodes.items[node_index].reads, local);
                    self.setReadBeforeRebindDef(&graph, node_index, assign.target);
                    try self.appendReadBeforeRebindSuccessor(&graph, &work, node_index, assign.next);
                },
                .assign_boxy_adapt => |assign| {
                    self.noteExposedReadLocal(&graph.nodes.items[node_index].reads, assign.source);
                    if (assign.source_desc) |desc| if (desc.localOrNull()) |local| self.noteExposedReadLocal(&graph.nodes.items[node_index].reads, local);
                    if (assign.target_desc) |desc| if (desc.localOrNull()) |local| self.noteExposedReadLocal(&graph.nodes.items[node_index].reads, local);
                    self.setReadBeforeRebindDef(&graph, node_index, assign.target);
                    try self.appendReadBeforeRebindSuccessor(&graph, &work, node_index, assign.next);
                },
                .assign_boxy_inspect => |assign| {
                    self.noteExposedReadLocal(&graph.nodes.items[node_index].reads, assign.source);
                    if (assign.source_desc.localOrNull()) |local| self.noteExposedReadLocal(&graph.nodes.items[node_index].reads, local);
                    self.setReadBeforeRebindDef(&graph, node_index, assign.target);
                    try self.appendReadBeforeRebindSuccessor(&graph, &work, node_index, assign.next);
                },
                .assign_boxy_eq => |assign| {
                    self.noteExposedReadLocal(&graph.nodes.items[node_index].reads, assign.lhs);
                    self.noteExposedReadLocal(&graph.nodes.items[node_index].reads, assign.rhs);
                    if (assign.source_desc.localOrNull()) |local| self.noteExposedReadLocal(&graph.nodes.items[node_index].reads, local);
                    self.setReadBeforeRebindDef(&graph, node_index, assign.target);
                    try self.appendReadBeforeRebindSuccessor(&graph, &work, node_index, assign.next);
                },
                .assign_boxy_tag => |assign| {
                    if (assign.target_desc.localOrNull()) |local| self.noteExposedReadLocal(&graph.nodes.items[node_index].reads, local);
                    if (assign.payload) |payload| self.noteExposedReadLocal(&graph.nodes.items[node_index].reads, payload);
                    if (assign.payload_desc) |desc| {
                        if (desc.localOrNull()) |local| self.noteExposedReadLocal(&graph.nodes.items[node_index].reads, local);
                    }
                    self.setReadBeforeRebindDef(&graph, node_index, assign.target);
                    try self.appendReadBeforeRebindSuccessor(&graph, &work, node_index, assign.next);
                },
                .assign_boxy_tag_payload => |assign| {
                    self.noteExposedReadLocal(&graph.nodes.items[node_index].reads, assign.source);
                    if (assign.source_desc.localOrNull()) |local| self.noteExposedReadLocal(&graph.nodes.items[node_index].reads, local);
                    self.setReadBeforeRebindDef(&graph, node_index, assign.target);
                    if (assign.target_desc) |target_desc| self.setReadBeforeRebindDef(&graph, node_index, target_desc);
                    try self.appendReadBeforeRebindSuccessor(&graph, &work, node_index, assign.next);
                },
                .boxy_tag_match => |tag_match| {
                    self.noteExposedReadLocal(&graph.nodes.items[node_index].reads, tag_match.source);
                    if (tag_match.source_desc.localOrNull()) |local| self.noteExposedReadLocal(&graph.nodes.items[node_index].reads, local);
                    try self.appendReadBeforeRebindSuccessor(&graph, &work, node_index, tag_match.on_match);
                    try self.appendReadBeforeRebindSuccessor(&graph, &work, node_index, tag_match.on_miss);
                },
                .assign_call_dict => |assign| {
                    if (assign.dict.localOrNull()) |local| self.noteExposedReadLocal(&graph.nodes.items[node_index].reads, local);
                    if (assign.result_desc) |result_desc| {
                        if (result_desc.localOrNull()) |local| self.noteExposedReadLocal(&graph.nodes.items[node_index].reads, local);
                    }
                    self.noteExposedReadSpan(&graph.nodes.items[node_index].reads, assign.args);
                    self.noteExposedReadSpan(&graph.nodes.items[node_index].reads, assign.arg_descs);
                    self.noteExposedReadSpan(&graph.nodes.items[node_index].reads, assign.hidden_args);
                    self.setReadBeforeRebindDef(&graph, node_index, assign.target);
                    try self.appendReadBeforeRebindSuccessor(&graph, &work, node_index, assign.next);
                },
                .assign_low_level => |assign| {
                    self.noteExposedReadSpan(&graph.nodes.items[node_index].reads, assign.args);
                    self.setReadBeforeRebindDef(&graph, node_index, assign.target);
                    try self.appendReadBeforeRebindSuccessor(&graph, &work, node_index, assign.next);
                },
                .assign_list => |assign| {
                    self.noteExposedReadSpan(&graph.nodes.items[node_index].reads, assign.elems);
                    self.setReadBeforeRebindDef(&graph, node_index, assign.target);
                    try self.appendReadBeforeRebindSuccessor(&graph, &work, node_index, assign.next);
                },
                .assign_struct => |assign| {
                    self.noteExposedReadSpan(&graph.nodes.items[node_index].reads, assign.fields);
                    self.setReadBeforeRebindDef(&graph, node_index, assign.target);
                    try self.appendReadBeforeRebindSuccessor(&graph, &work, node_index, assign.next);
                },
                .assign_tag => |assign| {
                    if (assign.target_desc) |target_desc| {
                        if (target_desc.localOrNull()) |local| self.noteExposedReadLocal(&graph.nodes.items[node_index].reads, local);
                    }
                    if (assign.payload) |payload| self.noteExposedReadLocal(&graph.nodes.items[node_index].reads, payload);
                    self.setReadBeforeRebindDef(&graph, node_index, assign.target);
                    try self.appendReadBeforeRebindSuccessor(&graph, &work, node_index, assign.next);
                },
                .store_struct => |assign| {
                    self.noteExposedReadLocal(&graph.nodes.items[node_index].reads, assign.dest);
                    self.noteExposedReadSpan(&graph.nodes.items[node_index].reads, assign.fields);
                    try self.appendReadBeforeRebindSuccessor(&graph, &work, node_index, assign.next);
                },
                .store_tag => |assign| {
                    self.noteExposedReadLocal(&graph.nodes.items[node_index].reads, assign.dest);
                    if (assign.payload) |payload| self.noteExposedReadLocal(&graph.nodes.items[node_index].reads, payload);
                    try self.appendReadBeforeRebindSuccessor(&graph, &work, node_index, assign.next);
                },
                .set_local => |assign| {
                    self.noteExposedReadLocal(&graph.nodes.items[node_index].reads, assign.value);
                    self.setReadBeforeRebindDef(&graph, node_index, assign.target);
                    try self.appendReadBeforeRebindSuccessor(&graph, &work, node_index, assign.next);
                },
                .debug => |debug_stmt| {
                    self.noteExposedReadLocal(&graph.nodes.items[node_index].reads, debug_stmt.message);
                    try self.appendReadBeforeRebindSuccessor(&graph, &work, node_index, debug_stmt.next);
                },
                .expect_err => |expect_err_stmt| self.noteExposedReadLocal(&graph.nodes.items[node_index].reads, expect_err_stmt.message),
                .expect => |expect_stmt| {
                    self.noteExposedReadLocal(&graph.nodes.items[node_index].reads, expect_stmt.condition);
                    try self.appendReadBeforeRebindSuccessor(&graph, &work, node_index, expect_stmt.next);
                },
                .incref => |rc| {
                    self.noteExposedReadLocal(&graph.nodes.items[node_index].reads, rc.value);
                    try self.appendReadBeforeRebindSuccessor(&graph, &work, node_index, rc.next);
                },
                .decref => |rc| {
                    self.noteExposedReadLocal(&graph.nodes.items[node_index].reads, rc.value);
                    try self.appendReadBeforeRebindSuccessor(&graph, &work, node_index, rc.next);
                },
                .decref_if_initialized => |rc| {
                    self.noteExposedReadLocal(&graph.nodes.items[node_index].reads, rc.cond);
                    self.noteExposedReadLocal(&graph.nodes.items[node_index].reads, rc.value);
                    try self.appendReadBeforeRebindSuccessor(&graph, &work, node_index, rc.next);
                },
                .free => |rc| {
                    self.noteExposedReadLocal(&graph.nodes.items[node_index].reads, rc.value);
                    try self.appendReadBeforeRebindSuccessor(&graph, &work, node_index, rc.next);
                },
                .switch_stmt => |switch_stmt| {
                    self.noteExposedReadLocal(&graph.nodes.items[node_index].reads, switch_stmt.cond);
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
                    self.noteExposedReadLocal(&graph.nodes.items[node_index].reads, switch_stmt.cond);
                    try self.appendReadBeforeRebindSuccessor(&graph, &work, node_index, switch_stmt.initialized_branch);
                    try self.appendReadBeforeRebindSuccessor(&graph, &work, node_index, switch_stmt.uninitialized_branch);
                },
                .str_match => |str_match| {
                    self.noteExposedReadLocal(&graph.nodes.items[node_index].reads, str_match.source);
                    const steps = self.store.getStrMatchSteps(str_match.steps);
                    for (0..GuardedList.borrowLen(steps)) |step_index| {
                        const step = GuardedList.at(steps, step_index);
                        switch (step.capture) {
                            .discard => {},
                            // Captures are branch-local definitions. The graph
                            // tracks defs per statement, not per edge, so we
                            // intentionally do not mark them as unconditional
                            // defs here; doing so would hide reads on the miss
                            // path. Over-reporting relevance is safe.
                            .view => {},
                        }
                    }
                    try self.appendReadBeforeRebindSuccessor(&graph, &work, node_index, str_match.on_match);
                    try self.appendReadBeforeRebindSuccessor(&graph, &work, node_index, str_match.on_miss);
                },
                .str_match_set => |str_match_set| {
                    self.noteExposedReadLocal(&graph.nodes.items[node_index].reads, str_match_set.source);
                    const arms = self.store.getStrMatchArms(str_match_set.arms);
                    for (0..GuardedList.borrowLen(arms)) |arm_index| {
                        const arm = GuardedList.at(arms, arm_index);
                        const steps = self.store.getStrMatchSteps(arm.steps);
                        for (0..GuardedList.borrowLen(steps)) |step_index| {
                            const step = GuardedList.at(steps, step_index);
                            switch (step.capture) {
                                .discard => {},
                                .view => {},
                            }
                        }
                        try self.appendReadBeforeRebindSuccessor(&graph, &work, node_index, arm.on_match);
                    }
                    try self.appendReadBeforeRebindSuccessor(&graph, &work, node_index, str_match_set.on_miss);
                },
                .join => |join_stmt| {
                    try self.ensureReadBeforeRebindNode(&graph, &work, join_stmt.body);
                    try cache_roots.append(graph_allocator, join_stmt.body);
                    try self.appendReadBeforeRebindSuccessor(&graph, &work, node_index, join_stmt.remainder);
                },
                .jump => |jump_stmt| {
                    if (self.join_bodies.get(jump_stmt.target)) |target_body| {
                        try self.appendReadBeforeRebindSuccessor(&graph, &work, node_index, target_body);
                    }
                },
                .ret => |ret_stmt| self.noteExposedReadLocal(&graph.nodes.items[node_index].reads, ret_stmt.value),
                .crash => |crash_stmt| if (crash_stmt.msg.localId()) |message| {
                    self.noteExposedReadLocal(&graph.nodes.items[node_index].reads, message);
                },
                .runtime_error, .comptime_exhaustiveness_failed, .loop_continue, .loop_break => {},
                .comptime_branch_taken => |marker| try self.appendReadBeforeRebindSuccessor(&graph, &work, node_index, marker.next),
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

        var scratch = try std.bit_set.DynamicBitSetUnmanaged.initEmpty(graph_allocator, self.proc_locals.items.len);
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
                const dense = self.denseOf(local);
                if (dense != no_dense) scratch.unset(dense);
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

        for (cache_roots.items) |root| {
            if (self.reads_before_rebind_cache.contains(root)) continue;
            const node_index = graph.indices.get(root) orelse unreachable;
            var cached = try graph.nodes.items[node_index].exposed.clone(self.allocator);
            errdefer cached.deinit(self.allocator);
            try self.reads_before_rebind_cache.put(root, cached);
        }

        const cached = self.reads_before_rebind_cache.getPtr(start) orelse {
            std.debug.panic("ARC borrow certifier invariant violated: read-before-rebind cache missing stmt {d}", .{@intFromEnum(start)});
        };
        return cached;
    }

    /// Computes the join's relevant-local set: every refcounted proc local the
    /// body subtree reads before rebinding. Join parameters are ordinary locals
    /// for this purpose; carrying every parameter unconditionally makes loops
    /// with conditionally initialized payload cells explode into one entry
    /// summary for every field-presence subset.
    fn computeJoinRelevant(
        self: *Certifier,
        body: LIR.CFStmtId,
    ) CertifyError!std.bit_set.DynamicBitSetUnmanaged {
        const reads = try self.computeReadsBeforeRebind(body);
        return reads.clone(self.allocator);
    }

    fn maybeUninitializedCondition(record: *const JoinRecord, store: *const LirStore, local: LIR.LocalId) ?PresenceCondition {
        const params = store.getLocalSpan(record.maybe_uninitialized_params);
        const conditions = store.getLocalSpan(record.maybe_uninitialized_conditions);
        const masks = store.getU64Span(record.maybe_uninitialized_condition_masks);
        if (params.len != conditions.len or params.len != masks.len) {
            std.debug.panic("ARC borrow certifier invariant violated: maybe-uninitialized join metadata arity mismatch", .{});
        }
        for (0..GuardedList.borrowLen(params)) |index| {
            const param = GuardedList.at(params, index);
            const condition = GuardedList.at(conditions, index);
            const mask = GuardedList.at(masks, index);
            if (param == local) return .{ .local = condition, .mask = mask };
        }
        return null;
    }

    /// Builds the jump-state summary restricted to the join's relevant
    /// locals, extending relevance through the lender anchors of relevant
    /// borrows, and verifies every outstanding ownership unit is carried into
    /// the join through a relevant local.
    fn summarizeForJoin(
        self: *Certifier,
        state: *State,
        record: *const JoinRecord,
        join_id: LIR.JoinPointId,
    ) CertifyError![]const LocalSummary {
        // A relevant join value can be the only carrier of storage selected
        // from an aggregate that is not itself relevant after this edge. Move
        // that stored unit into the join value before quotienting. When the
        // root is independently relevant this remains an ordinary borrow;
        // `tryClaim` declines a read whose exact claim is unavailable or
        // conditionally present.
        for (0..self.proc_locals.items.len) |dense| {
            if (!record.relevant.isSet(dense)) continue;
            const value = state.valueAtDense(dense);
            if (value == no_value or state.balanceOf(value) != 0) continue;
            if (self.values.items[value].payload_source == no_value) continue;

            var root = value;
            while (self.values.items[root].payload_source != no_value) {
                root = self.values.items[root].payload_source;
                if (state.balanceOf(root) > 0) break;
            }

            var root_independently_relevant = false;
            for (0..self.proc_locals.items.len) |candidate_dense| {
                if (!record.relevant.isSet(candidate_dense)) continue;
                if (state.valueAtDense(candidate_dense) == root) {
                    root_independently_relevant = true;
                    break;
                }
            }
            if (root_independently_relevant) continue;
            if (try self.tryClaim(state, value)) try state.addBalance(value, 1);
        }

        // Settle deferred takes before quotienting: a field-read value driven
        // negative by an aggregate move claims its container's stored unit
        // here, so the claim crosses the join on the container instead of a
        // negative balance failing below.
        try self.settleNegativeClaims(state);

        // Seed the working relevant set from the record.
        self.relevant_scratch.unsetAll();
        self.relevant_scratch.setUnion(record.relevant);

        // Extend through borrow anchors: a relevant borrowed local keeps its
        // lender's carrier local live, so the carrier joins the agreement.
        var changed = true;
        var anchor_values = std.ArrayList(ValueId).empty;
        defer anchor_values.deinit(self.allocator);
        while (changed) {
            changed = false;
            for (0..self.proc_locals.items.len) |dense| {
                if (!self.relevant_scratch.isSet(dense)) continue;
                const value = state.valueAtDense(dense);
                if (value == no_value) continue;
                if (state.balanceOf(value) > 0) continue;
                const info = self.values.items[value];
                if (info.payload_source != no_value) {
                    // Preserve each immediate link of a nested projection
                    // chain, not only its ultimate ownership lender.
                    for (0..self.proc_locals.items.len) |candidate_dense| {
                        if (state.valueAtDense(candidate_dense) != info.payload_source) continue;
                        if (!self.relevant_scratch.isSet(candidate_dense)) {
                            self.relevant_scratch.set(candidate_dense);
                            changed = true;
                        }
                        break;
                    }
                }
                if (!try self.collectBorrowSummaryAnchorValues(state, value, &anchor_values)) continue;
                for (anchor_values.items) |anchor| {
                    // Find a carrier local for the anchor value.
                    var carrier: u32 = no_dense;
                    for (0..self.proc_locals.items.len) |candidate_dense| {
                        if (state.valueAtDense(candidate_dense) == anchor) {
                            carrier = @intCast(candidate_dense);
                            break;
                        }
                    }
                    if (carrier == no_dense) continue;
                    if (!self.relevant_scratch.isSet(carrier)) {
                        self.relevant_scratch.set(carrier);
                        changed = true;
                    }
                }
            }
        }

        // Build the restricted summary.
        self.repr_scratch.clearRetainingCapacity();
        self.summary_scratch.clearRetainingCapacity();
        try self.summary_scratch.ensureTotalCapacity(self.allocator, self.proc_locals.items.len);

        for (0..self.proc_locals.items.len) |dense| {
            if (!self.relevant_scratch.isSet(dense)) continue;
            const value = state.valueAtDense(dense);
            if (value == no_value) continue;
            const entry = try self.repr_scratch.getOrPut(value);
            if (!entry.found_existing) entry.value_ptr.* = @intCast(dense);
        }

        for (self.proc_locals.items, 0..) |local, dense| {
            var summary = LocalSummary{ .class = .unbound, .repr = 0, .balance = 0, .lender_reprs = &.{}, .condition = no_dense, .condition_mask = 0 };
            if (self.relevant_scratch.isSet(dense)) {
                if (maybeUninitializedCondition(record, self.store, local)) |condition| {
                    summary = .{
                        .class = .conditional_owned,
                        .repr = self.denseOf(local),
                        .balance = 1,
                        .lender_reprs = &.{},
                        .condition = @intFromEnum(condition.local),
                        .condition_mask = condition.mask,
                    };
                } else {
                    const value = state.valueAtDense(dense);
                    if (value != no_value) {
                        const repr = self.repr_scratch.get(value) orelse 0;
                        const units = state.balanceOf(value);
                        const abi_live = self.values.items[value].always_live;
                        if (units > 0) {
                            if (state.conditionalConditionOf(value)) |condition| {
                                summary = .{
                                    .class = .conditional_owned,
                                    .repr = repr,
                                    .balance = @intCast(units),
                                    .lender_reprs = &.{},
                                    .abi_live = abi_live,
                                    .condition = @intFromEnum(condition.local),
                                    .condition_mask = condition.mask,
                                };
                            } else {
                                summary = .{ .class = .owned, .repr = repr, .balance = @intCast(units), .lender_reprs = &.{}, .abi_live = abi_live, .condition = no_dense, .condition_mask = 0, .claims = state.claimsOf(value) };
                            }
                        } else if (try self.valueIsLive(state, value)) {
                            summary = .{
                                .class = .borrowed,
                                .repr = repr,
                                .balance = 0,
                                .lender_reprs = try self.borrowSummaryAnchorReprs(state, value),
                                .abi_live = abi_live,
                                .condition = no_dense,
                                .condition_mask = 0,
                            };
                            self.addPayloadOriginToSummary(value, &summary);
                        } else if (self.isInlineStructRepresentation(local)) {
                            summary = .{
                                .class = .representation,
                                .repr = repr,
                                .balance = 0,
                                .lender_reprs = &.{},
                                .abi_live = abi_live,
                                .condition = no_dense,
                                .condition_mask = 0,
                            };
                        }
                        summary.abi_live = self.values.items[value].always_live;
                    }
                }
            }
            self.summary_scratch.appendAssumeCapacity(summary);
        }

        // Every outstanding ownership unit must be carried into the join by
        // a relevant local; anything else can never be released again. A
        // fully dismantled value is exempt: its unit is already spent by its
        // claims and owes no further release.
        for (state.balance.items, 0..) |units, value_index| {
            if (units == 0) continue;
            if (self.claimsSpendUnit(state, @intCast(value_index))) continue;
            const origin = self.values.items[value_index].origin;
            if (units < 0) {
                return self.fail(
                    "negative ownership balance for value originating at local {d} at jump to join {d}",
                    .{ @intFromEnum(origin), @intFromEnum(join_id) },
                );
            }
            if (!self.repr_scratch.contains(@intCast(value_index))) {
                self.diag.context_proc = self.current_proc;
                self.diag.context_local = origin;
                return self.fail(
                    "ownership unit of value originating at local {d} not carried into join {d}",
                    .{ @intFromEnum(origin), @intFromEnum(join_id) },
                );
            }
        }

        return self.summary_scratch.items;
    }

    fn noteProcLocal(self: *Certifier, local: LIR.LocalId) Allocator.Error!void {
        if (!self.isRc(local)) return;
        const index = @intFromEnum(local);
        if (index >= self.local_dense.items.len) return;
        if (self.local_dense.items[index] != no_dense) return;
        self.local_dense.items[index] = @intCast(self.proc_locals.items.len);
        try self.proc_locals.append(self.allocator, local);
    }

    fn noteProcLocalSpan(self: *Certifier, span: LIR.LocalSpan) Allocator.Error!void {
        const locals = self.store.getLocalSpan(span);
        for (0..GuardedList.borrowLen(locals)) |index| {
            const local = GuardedList.at(locals, index);
            try self.noteProcLocal(local);
        }
    }

    fn certifyProc(
        self: *Certifier,
        proc_id: LIR.LirProcSpecId,
        proc: LIR.LirProcSpec,
        body: LIR.CFStmtId,
    ) CertifyError!void {
        self.current_proc = proc_id;
        self.current_sig = self.sigs.get(proc_id);
        self.current_proc_body = body;
        self.current_return_local = null;
        self.values.clearRetainingCapacity();
        _ = self.lender_arena.reset(.retain_capacity);
        self.clearRecords();
        self.memo.clearRetainingCapacity();
        self.join_bodies.clearRetainingCapacity();
        self.clearReadsBeforeRebindCache();
        try self.collectProcLocals(proc, body);
        var proc_stmts = std.ArrayList(LIR.CFStmtId).empty;
        defer proc_stmts.deinit(self.allocator);
        try arc_solve.collectProcStatements(self.allocator, self.store, body, &proc_stmts);
        var return_local_mismatch = false;
        for (proc_stmts.items) |stmt_id| {
            const stmt = self.store.getCFStmt(stmt_id);
            if (stmt != .ret) continue;
            if (self.current_return_local) |expected| {
                if (expected != stmt.ret.value) {
                    return_local_mismatch = true;
                    break;
                }
            } else {
                self.current_return_local = stmt.ret.value;
            }
        }
        if (return_local_mismatch) self.current_return_local = null;
        const published_outcomes = self.sigs.outcomesOf(self.current_sig);
        if (published_outcomes.len != 0) {
            self.current_stmt = body;
            const return_local = self.current_return_local orelse
                return self.fail("outcome-specialized proc did not have one exact return local", .{});
            if (!self.isRc(return_local) or self.layouts.getLayout(proc.ret_layout).tag != .tag_union) {
                return self.fail("outcome-specialized proc did not return an RC-bearing top-level tag union", .{});
            }
            const params = self.store.getLocalSpan(proc.args);
            var previous: ?u16 = null;
            for (published_outcomes) |outcome| {
                if (previous != null and outcome.discriminant <= previous.?) {
                    return self.fail("outcome signature rows were not strictly discriminant-sorted", .{});
                }
                previous = outcome.discriminant;
                var bits = outcome.restituted_params;
                while (bits != 0) {
                    const position: usize = @intCast(@ctz(bits));
                    const bit = arc_sig.paramBit(position).?;
                    bits &= ~bit;
                    if (position >= GuardedList.borrowLen(params) or
                        self.current_sig.paramMode(position) != .owned or
                        !self.isRc(GuardedList.at(params, position)))
                    {
                        return self.fail("outcome signature named unavailable owned parameter position {d}", .{position});
                    }
                }
            }
        }
        try self.collectMemoPoints(body);
        try self.relevant_scratch.resize(self.allocator, self.proc_locals.items.len, false);

        var state = try State.init(self.allocator, self.local_dense.items, self.proc_locals.items.len);
        {
            errdefer state.deinit();
            const proc_args = self.store.getLocalSpan(proc.args);
            for (0..GuardedList.borrowLen(proc_args)) |index| {
                const param = GuardedList.at(proc_args, index);
                if (!self.isRc(param)) continue;
                switch (self.current_sig.paramMode(index)) {
                    .owned => _ = try self.bindFresh(&state, param, 1, &.{}),
                    .borrowed => {
                        const value = try self.newValue(param, &.{}, true);
                        try state.growToValue(value);
                        state.bindValue(param, value);
                    },
                }
            }
        }

        var work = std.ArrayList(WorkItem).empty;
        defer {
            while (work.pop()) |item| {
                switch (item) {
                    .segment => |segment| {
                        var owned_state = segment.state;
                        owned_state.deinit();
                    },
                    .join_body => {},
                }
            }
            work.deinit(self.allocator);
        }

        try work.append(self.allocator, .{ .segment = .{ .cursor = body, .state = state } });
        while (work.pop()) |item| {
            if (self.work_stats) |stats| stats.work_items += 1;
            switch (item) {
                .segment => |segment| try self.runSegment(&work, segment),
                .join_body => |walk| try self.scheduleJoinBody(&work, walk),
            }
        }
        for (published_outcomes) |outcome| {
            if (!self.seen_outcomes.contains(outcome.discriminant)) {
                return self.fail("published outcome discriminant {d} was not reached by emitted control flow", .{outcome.discriminant});
            }
        }
    }

    fn scheduleJoinBody(self: *Certifier, work: *std.ArrayList(WorkItem), walk: JoinWalk) CertifyError!void {
        const record = self.records.getPtr(walk.join) orelse return;
        const group = &record.groups.items[walk.group];
        // A refinement while this item sat on the stack re-queued the group;
        // the earlier of the two items walks the refined state, the later
        // one is a no-op.
        if (!group.queued) return;
        group.queued = false;
        var body_state = try self.stateFromSummary(group.summary);
        errdefer body_state.deinit();
        try work.append(self.allocator, .{ .segment = .{
            .cursor = record.body,
            .state = body_state,
            .origin_join = walk.join,
        } });
    }

    fn runSegment(self: *Certifier, work: *std.ArrayList(WorkItem), segment: Segment) CertifyError!void {
        var state = segment.state;
        defer state.deinit();
        var cursor = segment.cursor;
        self.current_origin_join = segment.origin_join;

        while (true) {
            self.current_stmt = cursor;

            if (self.memo_points.isSet(@intFromEnum(cursor))) {
                const summary = try self.summarize(&state);
                const memo_entry = MemoEntry{ .stmt = @intFromEnum(cursor), .digest = summaryDigest(cursor, summary) };
                const seen = try self.memo.getOrPut(memo_entry);
                if (seen.found_existing) return;
            }

            const stmt = self.store.getCFStmt(cursor);
            if (self.current_return_local) |return_local| {
                if (resultBindingTarget(stmt)) |target| {
                    if (target == return_local) state.result_discriminant = no_dense;
                }
            }
            if (state.outcome_discriminants.count() != 0) {
                const consumes_refinement = stmt == .switch_stmt and
                    state.outcome_discriminants.contains(stmt.switch_stmt.cond);
                // ARC may insert explicit RC bookkeeping after the source
                // discriminant read and before its immediately refining
                // switch. Those statements cannot change the established
                // result/discriminant relation; any balance mutation that
                // invalidates restitution is checked independently when the
                // switch replays its exact call receipt.
                const stmt_tag = std.meta.activeTag(stmt);
                const preserves_refinement = stmt_tag == .incref or
                    stmt_tag == .decref or
                    stmt_tag == .decref_if_initialized or
                    stmt_tag == .free;
                if (!consumes_refinement and !preserves_refinement) {
                    state.outcome_discriminants.clearRetainingCapacity();
                }
            }
            switch (stmt) {
                .assign_ref => |assign| {
                    try self.validateResidualShellFields(&state, assign);
                    switch (assign.op) {
                        .local => |source| {
                            if (assign.target != source) {
                                try self.bindLocalAlias(&state, assign.target, source);
                            }
                        },
                        .discriminant => |op| {
                            const source_value = try self.requireLive(&state, op.source);
                            _ = state.outcome_discriminants.remove(assign.target);
                            if (source_value != no_value and
                                !self.values.items[source_value].call_outcomes.isEmpty())
                            {
                                try state.outcome_discriminants.put(self.allocator, assign.target, source_value);
                            }
                        },
                        .field => |op| try self.bindPayloadRead(
                            &state,
                            assign.target,
                            op.source,
                            arc_dismantle.encodeProjection(assign.op).?,
                        ),
                        .tag_payload => |op| try self.bindPayloadRead(
                            &state,
                            assign.target,
                            op.source,
                            arc_dismantle.encodeProjection(assign.op).?,
                        ),
                        .tag_payload_struct => |op| try self.bindPayloadRead(
                            &state,
                            assign.target,
                            op.source,
                            arc_dismantle.encodeProjection(assign.op).?,
                        ),
                        .list_reinterpret => |op| try self.bindSameValue(&state, assign.target, op.backing_ref),
                        .nominal => |op| try self.bindSameValue(&state, assign.target, op.backing_ref),
                    }
                    cursor = assign.next;
                },
                .assign_literal => |assign| {
                    if (self.isRc(assign.target)) {
                        _ = try self.bindFresh(&state, assign.target, 1, &.{});
                    }
                    cursor = assign.next;
                },
                .init_uninitialized => |init| {
                    if (self.isRc(init.target)) {
                        state.bindValue(init.target, no_value);
                    }
                    cursor = init.next;
                },
                .assign_call => |assign| {
                    if (assign.result_desc) |result_desc| try self.requireBoxyDescRef(&state, result_desc);
                    try self.applyCall(&state, assign.target, self.sigs.get(assign.proc), assign.args);
                    cursor = assign.next;
                },
                .assign_call_erased => |assign| {
                    if (!LIR.erasedCallReuseFieldsMatch(assign)) {
                        return self.fail("erased call reuse flag and ownership source disagreed", .{});
                    }
                    _ = try self.requireLive(&state, assign.closure);
                    const reuse_value = if (assign.reuse_source) |reuse_source|
                        try self.requireLive(&state, reuse_source)
                    else
                        no_value;
                    try self.applyCall(&state, assign.target, arc_sig.RcSig.all_owned, assign.args);
                    if (assign.reuse_source) |reuse_source| try self.consumeUnit(&state, reuse_value, reuse_source);
                    cursor = assign.next;
                },
                .assign_packed_erased_fn => |assign| {
                    if (assign.result_desc) |result_desc| try self.requireBoxyDescRef(&state, result_desc);
                    const capture_value = if (assign.capture) |capture|
                        try self.requireLive(&state, capture)
                    else
                        no_value;
                    const reuse_value = if (assign.reuse) |reuse|
                        try self.requireLive(&state, reuse)
                    else
                        no_value;
                    if (self.isRc(assign.target)) {
                        const target_value = try self.bindFresh(&state, assign.target, 1, &.{});
                        if (assign.capture != null) {
                            try self.consumeIntoHolder(&state, capture_value, target_value);
                        }
                        if (assign.reuse != null) {
                            try self.consumeUnit(&state, reuse_value, assign.reuse.?);
                        }
                    } else if (assign.capture != null) {
                        try self.consumeIntoHolder(&state, capture_value, no_value);
                        if (assign.reuse != null) {
                            try self.consumeUnit(&state, reuse_value, assign.reuse.?);
                        }
                    } else if (assign.reuse != null) {
                        try self.consumeUnit(&state, reuse_value, assign.reuse.?);
                    }
                    cursor = assign.next;
                },
                .assign_boxy_desc_ref => |assign| {
                    try self.requireBoxyDescRef(&state, assign.desc);
                    const captures = self.store.getLocalSpan(assign.captures);
                    for (0..GuardedList.borrowLen(captures)) |index| {
                        const local = GuardedList.at(captures, index);
                        _ = try self.requireLive(&state, local);
                    }
                    _ = try self.bindBoxyOwnedTarget(&state, assign.target);
                    cursor = assign.next;
                },
                .assign_boxy_dict_ref => |assign| {
                    try self.requireBoxyDictRef(&state, assign.dict);
                    _ = try self.bindBoxyOwnedTarget(&state, assign.target);
                    cursor = assign.next;
                },
                .assign_boxy_box => |assign| {
                    if (assign.payload_desc) |desc| try self.requireBoxyDescRef(&state, desc);
                    const target_value = try self.bindBoxyOwnedTarget(&state, assign.target);
                    try self.consumeBoxyTransferIntoHolder(&state, assign.payload, assign.payload_mode, target_value);
                    cursor = assign.next;
                },
                .assign_boxy_reuse_box => |assign| {
                    try self.requireBoxyDescRef(&state, assign.desc);
                    _ = try self.bindBoxyOwnedTarget(&state, assign.target);
                    _ = try self.requireBoxyTransferSource(&state, assign.source, .move);
                    cursor = assign.next;
                },
                .assign_boxy_unbox => |assign| {
                    try self.requireBoxyDescRef(&state, assign.source_desc);
                    if (assign.target_desc) |desc| try self.requireBoxyDescRef(&state, desc);
                    const source_value = try self.requireBoxyTransferSource(&state, assign.source, assign.source_mode);
                    if (self.isRc(assign.target)) {
                        switch (assign.source_mode) {
                            .move => _ = try self.bindFresh(&state, assign.target, 1, &.{}),
                            .borrow => _ = try self.bindFresh(&state, assign.target, 0, &.{source_value}),
                            .copy => _ = try self.bindFresh(&state, assign.target, 1, &.{}),
                        }
                    }
                    cursor = assign.next;
                },
                .assign_boxy_adapt => |assign| {
                    if (assign.source_desc) |desc| try self.requireBoxyDescRef(&state, desc);
                    if (assign.target_desc) |desc| try self.requireBoxyDescRef(&state, desc);
                    _ = try self.bindBoxyOwnedTarget(&state, assign.target);
                    _ = try self.requireBoxyTransferSource(&state, assign.source, assign.source_mode);
                    cursor = assign.next;
                },
                .assign_boxy_inspect => |assign| {
                    try self.requireBoxyDescRef(&state, assign.source_desc);
                    _ = try self.bindBoxyOwnedTarget(&state, assign.target);
                    _ = try self.requireBoxyTransferSource(&state, assign.source, assign.source_mode);
                    cursor = assign.next;
                },
                .assign_boxy_eq => |assign| {
                    try self.requireBoxyDescRef(&state, assign.source_desc);
                    _ = try self.bindBoxyOwnedTarget(&state, assign.target);
                    _ = try self.requireBoxyTransferSource(&state, assign.lhs, assign.source_mode);
                    _ = try self.requireBoxyTransferSource(&state, assign.rhs, assign.source_mode);
                    cursor = assign.next;
                },
                .assign_boxy_tag => |assign| {
                    try self.requireBoxyDescRef(&state, assign.target_desc);
                    if (assign.payload_desc) |desc| try self.requireBoxyDescRef(&state, desc);
                    const target_value = try self.bindBoxyOwnedTarget(&state, assign.target);
                    if (assign.payload) |payload| {
                        try self.consumeBoxyTransferIntoHolder(&state, payload, assign.payload_mode, target_value);
                    }
                    cursor = assign.next;
                },
                .assign_boxy_tag_payload => |assign| {
                    try self.requireBoxyDescRef(&state, assign.source_desc);
                    const source_value = try self.requireBoxyTransferSource(&state, assign.source, assign.source_mode);
                    if (self.isRc(assign.target)) {
                        switch (assign.source_mode) {
                            .borrow => {
                                if (source_value == no_value) {
                                    _ = try self.bindBorrowedFromImplicitLive(&state, assign.target);
                                } else {
                                    _ = try self.bindFresh(&state, assign.target, 0, &.{source_value});
                                }
                            },
                            .copy, .move => _ = try self.bindFresh(&state, assign.target, 1, &.{}),
                        }
                    }
                    cursor = assign.next;
                },
                .boxy_tag_match => |tag_match| {
                    _ = try self.requireLive(&state, tag_match.source);
                    try self.requireBoxyDescRef(&state, tag_match.source_desc);
                    var match_state = try state.clone();
                    errdefer match_state.deinit();
                    try work.append(self.allocator, .{ .segment = .{ .cursor = tag_match.on_match, .state = match_state, .origin_join = segment.origin_join } });

                    var miss_state = try state.clone();
                    errdefer miss_state.deinit();
                    try work.append(self.allocator, .{ .segment = .{ .cursor = tag_match.on_miss, .state = miss_state, .origin_join = segment.origin_join } });
                    return;
                },
                .assign_call_dict => |assign| {
                    try self.requireBoxyDictRef(&state, assign.dict);
                    if (assign.result_desc) |result_desc| try self.requireBoxyDescRef(&state, result_desc);
                    try self.applyCall(&state, assign.target, arc_sig.RcSig.all_owned, assign.args);
                    const arg_descs = self.store.getLocalSpan(assign.arg_descs);
                    for (0..GuardedList.borrowLen(arg_descs)) |index| {
                        _ = try self.requireLive(&state, GuardedList.at(arg_descs, index));
                    }
                    const hidden_args = self.store.getLocalSpan(assign.hidden_args);
                    for (0..GuardedList.borrowLen(hidden_args)) |index| {
                        const hidden = GuardedList.at(hidden_args, index);
                        _ = try self.requireLive(&state, hidden);
                    }
                    cursor = assign.next;
                },
                .assign_low_level => |assign| {
                    try self.applyLowLevel(&state, assign);
                    cursor = assign.next;
                },
                .assign_list => |assign| {
                    try self.applyAggregate(&state, assign.target, self.store.getLocalSpan(assign.elems));
                    cursor = assign.next;
                },
                .assign_struct => |assign| {
                    try self.applyAggregate(&state, assign.target, self.store.getLocalSpan(assign.fields));
                    cursor = assign.next;
                },
                .assign_tag => |assign| {
                    if (assign.target_desc) |target_desc| try self.requireBoxyDescRef(&state, target_desc);
                    if (assign.payload) |payload| {
                        const operands = [_]LIR.LocalId{payload};
                        try self.applyAggregate(&state, assign.target, &operands);
                    } else {
                        const operands = [_]LIR.LocalId{};
                        try self.applyAggregate(&state, assign.target, &operands);
                    }
                    if (self.current_return_local != null and assign.target == self.current_return_local.?) {
                        state.result_discriminant = assign.discriminant;
                    }
                    cursor = assign.next;
                },
                .store_struct => |assign| {
                    try self.applyAggregateStore(&state, assign.dest, self.store.getLocalSpan(assign.fields));
                    cursor = assign.next;
                },
                .store_tag => |assign| {
                    if (assign.payload) |payload| {
                        try self.applyAggregateStore(&state, assign.dest, &[_]LIR.LocalId{payload});
                    } else {
                        try self.applyAggregateStore(&state, assign.dest, &[_]LIR.LocalId{});
                    }
                    cursor = assign.next;
                },
                .set_local => |assign| {
                    if (assign.target != assign.value) {
                        _ = try self.requireLive(&state, assign.value);
                        if (self.isRc(assign.target)) {
                            state.bindValue(assign.target, state.valueOf(assign.value));
                        }
                    }
                    cursor = assign.next;
                },
                .debug => |debug_stmt| {
                    _ = try self.requireLive(&state, debug_stmt.message);
                    cursor = debug_stmt.next;
                },
                .expect => |expect_stmt| {
                    _ = try self.requireLive(&state, expect_stmt.condition);
                    cursor = expect_stmt.next;
                },
                .comptime_branch_taken => |taken| {
                    cursor = taken.next;
                },
                .incref => |rc| {
                    if (!self.isRc(rc.value)) {
                        return self.fail("incref of non-refcounted local {d}", .{@intFromEnum(rc.value)});
                    }
                    const value = try self.requireLive(&state, rc.value);
                    try state.addBalance(value, rc.count);
                    cursor = rc.next;
                },
                .decref => |rc| {
                    try self.applyRelease(&state, rc.value);
                    cursor = rc.next;
                },
                .decref_if_initialized => |rc| {
                    _ = try self.requireLive(&state, rc.cond);
                    if (!self.isRc(rc.value)) {
                        return self.fail("decref_if_initialized of non-refcounted local {d}", .{@intFromEnum(rc.value)});
                    }
                    if (state.valueOf(rc.value) != no_value) {
                        try self.applyRelease(&state, rc.value);
                    }
                    cursor = rc.next;
                },
                .free => |rc| {
                    try self.applyRelease(&state, rc.value);
                    cursor = rc.next;
                },
                .switch_stmt => |switch_stmt| {
                    _ = try self.requireLive(&state, switch_stmt.cond);
                    const branches = self.store.getCFSwitchBranches(switch_stmt.branches);
                    const outcome_result = state.outcome_discriminants.get(switch_stmt.cond);
                    for (0..GuardedList.borrowLen(branches)) |branch_index| {
                        const branch = GuardedList.at(branches, branch_index);
                        var branch_state = try state.clone();
                        errdefer branch_state.deinit();
                        branch_state.outcome_discriminants.clearRetainingCapacity();
                        if (outcome_result) |result| {
                            if (self.callOutcomeMask(result, branch.value)) |mask| {
                                try self.restoreCallOutcome(&branch_state, result, mask);
                            }
                        }
                        try work.append(self.allocator, .{ .segment = .{ .cursor = branch.body, .state = branch_state, .origin_join = segment.origin_join } });
                    }
                    var default_state = try state.clone();
                    errdefer default_state.deinit();
                    default_state.outcome_discriminants.clearRetainingCapacity();
                    if (outcome_result) |result| {
                        if (self.defaultCallOutcomeMask(result, branches)) |mask| {
                            try self.restoreCallOutcome(&default_state, result, mask);
                        }
                    }
                    try work.append(self.allocator, .{ .segment = .{ .cursor = switch_stmt.default_branch, .state = default_state, .origin_join = segment.origin_join } });
                    return;
                },
                .switch_initialized_payload => |switch_stmt| {
                    _ = try self.requireLive(&state, switch_stmt.cond);
                    if (self.isRc(switch_stmt.payload)) {
                        const payload_value = state.valueOf(switch_stmt.payload);
                        if (payload_value != no_value) {
                            if (state.conditionalConditionOf(payload_value)) |condition| {
                                if (!condition.eql(.{ .local = switch_stmt.cond, .mask = switch_stmt.cond_mask })) {
                                    return self.fail(
                                        "initialized-payload switch condition l{d}/0x{x} did not match payload l{d} condition l{d}/0x{x}",
                                        .{ @intFromEnum(switch_stmt.cond), switch_stmt.cond_mask, @intFromEnum(switch_stmt.payload), @intFromEnum(condition.local), condition.mask },
                                    );
                                }

                                if (self.work_stats) |stats| stats.conditional_payload_splits += 1;

                                var initialized_state = try state.clone();
                                errdefer initialized_state.deinit();
                                initialized_state.markDefinitelyInitialized(payload_value);
                                try work.append(self.allocator, .{ .segment = .{ .cursor = switch_stmt.initialized_branch, .state = initialized_state, .origin_join = segment.origin_join } });

                                var uninitialized_state = try state.clone();
                                errdefer uninitialized_state.deinit();
                                const units = uninitialized_state.balanceOf(payload_value);
                                if (units > 0) try uninitialized_state.addBalance(payload_value, -units);
                                uninitialized_state.bindValue(switch_stmt.payload, no_value);
                                try work.append(self.allocator, .{ .segment = .{ .cursor = switch_stmt.uninitialized_branch, .state = uninitialized_state, .origin_join = segment.origin_join } });
                                return;
                            }
                        }

                        const payload_is_initialized = payload_value != no_value;
                        const target = if (payload_is_initialized)
                            switch_stmt.initialized_branch
                        else
                            switch_stmt.uninitialized_branch;
                        var branch_state = try state.clone();
                        errdefer branch_state.deinit();
                        try work.append(self.allocator, .{ .segment = .{ .cursor = target, .state = branch_state, .origin_join = segment.origin_join } });
                        return;
                    }
                    var initialized_state = try state.clone();
                    errdefer initialized_state.deinit();
                    try work.append(self.allocator, .{ .segment = .{ .cursor = switch_stmt.initialized_branch, .state = initialized_state, .origin_join = segment.origin_join } });
                    var uninitialized_state = try state.clone();
                    errdefer uninitialized_state.deinit();
                    try work.append(self.allocator, .{ .segment = .{ .cursor = switch_stmt.uninitialized_branch, .state = uninitialized_state, .origin_join = segment.origin_join } });
                    return;
                },
                .str_match => |str_match| {
                    const source_value = try self.requireLive(&state, str_match.source);
                    var match_state = try state.clone();
                    errdefer match_state.deinit();
                    const steps = self.store.getStrMatchSteps(str_match.steps);
                    for (0..GuardedList.borrowLen(steps)) |step_index| {
                        const step = GuardedList.at(steps, step_index);
                        switch (step.capture) {
                            .discard => {},
                            .view => |local| if (self.isRc(local)) {
                                match_state.bindValue(local, source_value);
                            },
                        }
                    }
                    try work.append(self.allocator, .{ .segment = .{ .cursor = str_match.on_match, .state = match_state, .origin_join = segment.origin_join } });

                    var miss_state = try state.clone();
                    errdefer miss_state.deinit();
                    try work.append(self.allocator, .{ .segment = .{ .cursor = str_match.on_miss, .state = miss_state, .origin_join = segment.origin_join } });
                    return;
                },
                .str_match_set => |str_match_set| {
                    const source_value = try self.requireLive(&state, str_match_set.source);
                    const arms = self.store.getStrMatchArms(str_match_set.arms);
                    for (0..GuardedList.borrowLen(arms)) |arm_index| {
                        const arm = GuardedList.at(arms, arm_index);
                        var match_state = try state.clone();
                        errdefer match_state.deinit();
                        const steps = self.store.getStrMatchSteps(arm.steps);
                        for (0..GuardedList.borrowLen(steps)) |step_index| {
                            const step = GuardedList.at(steps, step_index);
                            switch (step.capture) {
                                .discard => {},
                                .view => |local| if (self.isRc(local)) {
                                    match_state.bindValue(local, source_value);
                                },
                            }
                        }
                        try work.append(self.allocator, .{ .segment = .{ .cursor = arm.on_match, .state = match_state, .origin_join = segment.origin_join } });
                    }

                    var miss_state = try state.clone();
                    errdefer miss_state.deinit();
                    try work.append(self.allocator, .{ .segment = .{ .cursor = str_match_set.on_miss, .state = miss_state, .origin_join = segment.origin_join } });
                    return;
                },
                .join => |join_stmt| {
                    const record = try self.records.getOrPut(join_stmt.id);
                    if (record.found_existing) {
                        if (record.value_ptr.body != join_stmt.body) {
                            return self.fail("join {d} redefined with a different body", .{@intFromEnum(join_stmt.id)});
                        }
                    } else {
                        record.value_ptr.* = .{
                            .body = join_stmt.body,
                            .params = join_stmt.params,
                            .relevant = try self.computeJoinRelevant(join_stmt.body),
                            .maybe_uninitialized_params = join_stmt.maybe_uninitialized_params,
                            .maybe_uninitialized_conditions = join_stmt.maybe_uninitialized_conditions,
                            .maybe_uninitialized_condition_masks = join_stmt.maybe_uninitialized_condition_masks,
                            .groups = .empty,
                        };
                    }
                    cursor = join_stmt.remainder;
                },
                .jump => |jump_stmt| {
                    const record = self.records.getPtr(jump_stmt.target) orelse {
                        return self.fail("jump to join {d} before its definition", .{@intFromEnum(jump_stmt.target)});
                    };
                    if (self.current_return_local) |return_local| {
                        const target_stmt = self.store.getCFStmt(record.body);
                        if (target_stmt == .ret and target_stmt.ret.value == return_local) {
                            try self.applyOutcomeRestitution(&state);
                        }
                    }
                    const jump_summary = try self.summarizeForJoin(&state, record, jump_stmt.target);
                    switch (try self.absorbJoinSummary(record, jump_summary, jump_stmt.target)) {
                        .covered => {},
                        .walk => |group_index| {
                            const group = &record.groups.items[group_index];
                            if (!group.queued) {
                                group.queued = true;
                                try work.append(self.allocator, .{ .join_body = .{
                                    .join = jump_stmt.target,
                                    .group = group_index,
                                } });
                            }
                        },
                    }
                    return;
                },
                .ret => |ret_stmt| {
                    var restitution_applied_at_terminal_join = false;
                    if (!self.current_sig.outcomes.isEmpty() and
                        state.result_discriminant == no_dense)
                    {
                        if (self.current_origin_join) |join_id| {
                            if (self.records.getPtr(join_id)) |record| {
                                restitution_applied_at_terminal_join = record.body == cursor and
                                    self.current_return_local != null and
                                    ret_stmt.value == self.current_return_local.?;
                            }
                        }
                    }
                    if (!restitution_applied_at_terminal_join) {
                        try self.applyOutcomeRestitution(&state);
                    }
                    if (self.isRc(ret_stmt.value)) {
                        const value = try self.requireLive(&state, ret_stmt.value);
                        switch (self.current_sig.ret_mode) {
                            .owned => try self.consumeUnit(&state, value, ret_stmt.value),
                            .borrowed => {},
                        }
                    }
                    try self.checkLeaks(&state);
                    return;
                },
                .crash => |crash_stmt| {
                    if (crash_stmt.msg.localId()) |message| {
                        if (self.isRc(message)) {
                            const value = try self.requireLive(&state, message);
                            try self.consumeUnit(&state, value, message);
                        }
                    }
                    try self.checkLeaks(&state);
                    return;
                },
                .expect_err => |expect_err_stmt| {
                    // The failure report consumes the message's unit.
                    if (self.isRc(expect_err_stmt.message)) {
                        const value = try self.requireLive(&state, expect_err_stmt.message);
                        try self.consumeUnit(&state, value, expect_err_stmt.message);
                    }
                    try self.checkLeaks(&state);
                    return;
                },
                .runtime_error, .comptime_exhaustiveness_failed => {
                    try self.checkLeaks(&state);
                    return;
                },
                .loop_continue, .loop_break => {
                    // Control returns to an enclosing iteration engine that
                    // owns the kept values; per-path balance checking resumes
                    // at the statements that follow the engine.
                    return;
                },
            }
        }
    }

    fn bindPayloadRead(self: *Certifier, state: *State, target: LIR.LocalId, source: LIR.LocalId, projection: u64) CertifyError!void {
        if (!self.isRc(target) and self.isRc(source) and self.isInlineStructRepresentation(source)) {
            _ = try self.requireStructRepresentation(state, source);
            return;
        }
        const source_value = try self.requireLive(state, source);
        if (!self.isRc(target)) return;
        const value = if (source_value == no_value)
            try self.bindBorrowedFromImplicitLive(state, target)
        else
            try self.bindFresh(state, target, 0, &.{source_value});
        const info = &self.values.items[value];
        info.payload_source = source_value;
        info.payload_projection = projection;
    }

    fn validateResidualShellFields(
        self: *Certifier,
        state: *const State,
        assign: @FieldType(LIR.CFStmt, "assign_ref"),
    ) CertifyError!void {
        const absent_fields = self.store.getU32Span(assign.residual_shell_absent_fields);
        const source = switch (assign.op) {
            .local => |local| local,
            .discriminant,
            .field,
            .tag_payload,
            .tag_payload_struct,
            .list_reinterpret,
            .nominal,
            => {
                if (absent_fields.len != 0) {
                    return self.fail("residual-shell field metadata attached to a non-local alias", .{});
                }
                return;
            },
        };
        const target_layout = self.store.getLocal(assign.target).layout_idx;
        const source_layout = self.store.getLocal(source).layout_idx;
        const source_layout_value = self.layouts.getLayout(source_layout);
        if (!self.isRc(source) or !self.isRc(assign.target) or target_layout != source_layout or source_layout_value.tag != .struct_) {
            if (absent_fields.len != 0) {
                return self.fail("residual-shell field metadata attached to a non-struct or layout-changing alias", .{});
            }
            return;
        }

        const source_value = state.valueOf(source);
        if (source_value == no_value) {
            if (absent_fields.len != 0) {
                return self.fail("residual-shell field metadata named an unbound source", .{});
            }
            return;
        }
        const required = self.requiredClaimMask(source_value) orelse 0;
        var observed: u64 = 0;
        for (0..absent_fields.len) |index| {
            const field_index = GuardedList.at(absent_fields, index);
            if (field_index >= 64) {
                return self.fail("residual-shell field index {d} exceeds the certified field domain", .{field_index});
            }
            const field_mask = @as(u64, 1) << @intCast(field_index);
            if ((required & field_mask) == 0) {
                return self.fail("residual-shell metadata names non-RC or absent field {d}", .{field_index});
            }
            if ((observed & field_mask) != 0) {
                return self.fail("residual-shell metadata repeats field {d}", .{field_index});
            }
            observed |= field_mask;
        }

        // The certifier's field claims settle lazily at consumption and are
        // shared by every alias of one ValueId. They are therefore not the
        // path-local residual snapshot attached to this particular binding;
        // ARC's solved plan is the authority for partial masks. Once the
        // whole value is dead, however, every RC field must be absent.
        if (!try self.valueIsLive(state, source_value) and observed != required) {
            return self.fail("released struct representation is missing exact residual-shell metadata", .{});
        }
    }

    fn bindLocalAlias(self: *Certifier, state: *State, target: LIR.LocalId, source: LIR.LocalId) CertifyError!void {
        const target_layout = self.store.getLocal(target).layout_idx;
        const source_layout = self.store.getLocal(source).layout_idx;
        const source_value = if (self.isRc(source) and target_layout == source_layout and self.isInlineStructRepresentation(source))
            try self.requireStructRepresentation(state, source)
        else
            try self.requireLive(state, source);
        if (!self.isRc(target)) return;
        if (source_value == no_value) {
            self.diag.context_local = source;
            self.diag.context_proc = self.current_proc;
            self.diag.context_stmt = self.current_stmt;
            return self.fail(
                "reinterpret into refcounted local {d} from non-refcounted source {d}",
                .{ @intFromEnum(target), @intFromEnum(source) },
            );
        }
        state.bindValue(target, source_value);
    }

    fn bindSameValue(self: *Certifier, state: *State, target: LIR.LocalId, source: LIR.LocalId) CertifyError!void {
        const source_value = try self.requireLive(state, source);
        if (!self.isRc(target)) return;
        if (source_value == no_value) {
            self.diag.context_local = source;
            self.diag.context_proc = self.current_proc;
            self.diag.context_stmt = self.current_stmt;
            return self.fail(
                "reinterpret into refcounted local {d} from non-refcounted source {d}",
                .{ @intFromEnum(target), @intFromEnum(source) },
            );
        }
        state.bindValue(target, source_value);
    }

    fn applyRelease(self: *Certifier, state: *State, local: LIR.LocalId) CertifyError!void {
        if (!self.isRc(local)) {
            return self.fail("release of non-refcounted local {d}", .{@intFromEnum(local)});
        }
        const value = state.valueOf(local);
        if (value == no_value) {
            self.diag.context_local = local;
            self.diag.context_proc = self.current_proc;
            return self.fail("release of unbound local {d}", .{@intFromEnum(local)});
        }
        if (state.claimsOf(value) != 0 and !self.hasIntactSurplusUnit(state, value)) {
            self.diag.context_local = local;
            self.diag.context_proc = self.current_proc;
            return self.fail("whole release of partially dismantled local {d}", .{@intFromEnum(local)});
        }
        if (state.balanceOf(value) < 1) {
            if (try self.tryClaim(state, value)) return;
            self.diag.context_local = local;
            self.diag.context_proc = self.current_proc;
            return self.fail("release of local {d} without an ownership unit", .{@intFromEnum(local)});
        }
        try state.addBalance(value, -1);
    }

    fn applyCall(
        self: *Certifier,
        state: *State,
        target: LIR.LocalId,
        callee_sig: arc_sig.RcSig,
        args: LIR.LocalSpan,
    ) CertifyError!void {
        const arg_locals = self.store.getLocalSpan(args);
        const outcomes = self.sigs.outcomesOf(callee_sig);
        var restitutable_mask: arc_sig.ParamMask = 0;
        for (outcomes) |outcome| restitutable_mask |= outcome.restituted_params;
        if (outcomes.len != 0 and callee_sig.ret_mode != .owned) {
            return self.fail("outcome-conditioned call returned a borrow", .{});
        }

        var arg_values_buffer: [arc_sig.tracked_param_count]ValueId = undefined;
        var receipts_buffer = [_]RestitutionReceipt{.{}} ** arc_sig.tracked_param_count;
        for (0..GuardedList.borrowLen(arg_locals)) |index| {
            const arg = GuardedList.at(arg_locals, index);
            const value = try self.requireLive(state, arg);
            if (index < arg_values_buffer.len) arg_values_buffer[index] = value;
        }
        if (restitutable_mask != 0) {
            for (0..@min(GuardedList.borrowLen(arg_locals), arc_sig.tracked_param_count)) |position| {
                const bit = arc_sig.paramBit(position).?;
                if ((restitutable_mask & bit) == 0) continue;
                for (0..position) |earlier| {
                    const earlier_bit = arc_sig.paramBit(earlier).?;
                    if ((restitutable_mask & earlier_bit) == 0) continue;
                    if (arg_values_buffer[position] == arg_values_buffer[earlier]) {
                        return self.fail(
                            "outcome restitution positions {d} and {d} named the same ownership place",
                            .{ earlier, position },
                        );
                    }
                }
            }
        }

        for (0..GuardedList.borrowLen(arg_locals)) |index| {
            const arg = GuardedList.at(arg_locals, index);
            if (!self.isRc(arg)) continue;
            switch (callee_sig.paramMode(index)) {
                .owned => {
                    const value = if (index < arg_values_buffer.len)
                        arg_values_buffer[index]
                    else
                        state.valueOf(arg);
                    const bit = arc_sig.paramBit(index);
                    if (bit != null and (restitutable_mask & bit.?) != 0) {
                        var mutations = std.ArrayList(OwnershipMutation).empty;
                        defer mutations.deinit(self.allocator);
                        try self.consumeUnitRecording(state, value, arg, &mutations);
                        receipts_buffer[index] = .{
                            .value = value,
                            .mutations = try self.lender_arena.allocator().dupe(OwnershipMutation, mutations.items),
                        };
                    } else {
                        try self.consumeUnit(state, value, arg);
                    }
                },
                .borrowed => {},
            }
        }

        var target_value: ValueId = no_value;
        if (self.isRc(target)) {
            switch (callee_sig.ret_mode) {
                .owned => target_value = try self.bindFresh(state, target, 1, &.{}),
                .borrowed => {
                    var lenders_buffer: [arc_sig.tracked_param_count]ValueId = undefined;
                    var lender_count: usize = 0;
                    for (0..GuardedList.borrowLen(arg_locals)) |index| {
                        const arg = GuardedList.at(arg_locals, index);
                        const bit = arc_sig.paramBit(index) orelse break;
                        if ((callee_sig.ret_lenders & bit) == 0) continue;
                        if (!self.isRc(arg)) continue;
                        const value = arg_values_buffer[index];
                        if (value == no_value) continue;
                        lenders_buffer[lender_count] = value;
                        lender_count += 1;
                    }
                    target_value = try self.bindFresh(state, target, 0, lenders_buffer[0..lender_count]);
                },
            }
        }
        if (outcomes.len != 0) {
            if (target_value == no_value) {
                return self.fail("outcome-conditioned call result was not reference-counted", .{});
            }
            const receipt_len = @min(GuardedList.borrowLen(arg_locals), arc_sig.tracked_param_count);
            for (outcomes) |outcome| {
                var bits = outcome.restituted_params;
                while (bits != 0) {
                    const position: usize = @intCast(@ctz(bits));
                    const bit = arc_sig.paramBit(position).?;
                    bits &= ~bit;
                    if (position >= receipt_len or
                        callee_sig.paramMode(position) != .owned or
                        receipts_buffer[position].value == no_value)
                    {
                        return self.fail("outcome restitution named unavailable owned argument position {d}", .{position});
                    }
                }
            }
            const receipts = try self.lender_arena.allocator().dupe(
                RestitutionReceipt,
                receipts_buffer[0..receipt_len],
            );
            const info = &self.values.items[target_value];
            info.call_outcomes = callee_sig.outcomes;
            info.call_restitution = receipts;
        }
    }

    fn applyLowLevel(self: *Certifier, state: *State, assign: anytype) CertifyError!void {
        if (assign.op == .box_unbox) {
            return self.fail("post-ARC LIR retained a consuming Box.unbox instead of explicit RC statements", .{});
        }
        const arg_locals = self.store.getLocalSpan(assign.args);

        // The masks in an `RcEffect` row name argument positions, but the row
        // is written next to the op's name, not its signature. A bit above the
        // real argument count names nothing: the ownership it describes is
        // dropped instead of applied. This is the one place where a row and the
        // arguments it talks about are both in hand.
        if (rc_effect_rules.maskExceedsArgCount(assign.rc_effect, GuardedList.borrowLen(arg_locals))) |position| {
            return self.fail(
                "low-level op {s} has an RcEffect mask naming argument {d}, but it takes {d} arguments",
                .{ @tagName(assign.op), position, GuardedList.borrowLen(arg_locals) },
            );
        }

        var arg_values_buffer: [64]ValueId = undefined;
        for (0..GuardedList.borrowLen(arg_locals)) |index| {
            const arg = GuardedList.at(arg_locals, index);
            const value = try self.requireLive(state, arg);
            if (index < arg_values_buffer.len) arg_values_buffer[index] = value;
        }

        // Consumed positions transfer one unit each into the op.
        for (0..GuardedList.borrowLen(arg_locals)) |index| {
            const arg = GuardedList.at(arg_locals, index);
            if (index >= 64) break;
            if (!self.isRc(arg)) continue;
            const bit = @as(u64, 1) << @as(u6, @intCast(index));
            if ((assign.rc_effect.consume_args & bit) == 0) continue;
            try self.consumeUnit(state, arg_values_buffer[index], arg);
        }

        var target_value: ValueId = no_value;
        if (self.isRc(assign.target)) {
            if (assign.rc_effect.retain_result) {
                // The result reads payload data out of the op's refcounted
                // arguments; it borrows until the trailing incref lands.
                var lenders_buffer: [64]ValueId = undefined;
                var lender_count: usize = 0;
                for (0..GuardedList.borrowLen(arg_locals)) |index| {
                    const arg = GuardedList.at(arg_locals, index);
                    if (index >= lenders_buffer.len) break;
                    if (!self.isRc(arg)) continue;
                    lenders_buffer[lender_count] = arg_values_buffer[index];
                    lender_count += 1;
                }
                if (lender_count == 0) {
                    // The payload source is implicit (the executing frame's
                    // capture environment); it is live for the whole call.
                    target_value = try self.newValue(assign.target, &.{}, true);
                    try state.growToValue(target_value);
                    state.bindValue(assign.target, target_value);
                } else {
                    target_value = try self.bindFresh(state, assign.target, 0, lenders_buffer[0..lender_count]);
                }
            } else {
                target_value = try self.bindFresh(state, assign.target, 1, &.{});
            }
        }

        // Retained positions are stored inside the result; the trailing
        // incref restores the unit the op moved into its result.
        for (0..GuardedList.borrowLen(arg_locals)) |index| {
            const arg = GuardedList.at(arg_locals, index);
            if (index >= 64) break;
            if (!self.isRc(arg)) continue;
            const bit = @as(u64, 1) << @as(u6, @intCast(index));
            if ((assign.rc_effect.retain_args & bit) == 0) continue;
            try self.consumeIntoHolder(state, arg_values_buffer[index], target_value);
        }
    }

    fn requireBoxyDescRef(self: *Certifier, state: *State, desc: LIR.BoxyDescRef) CertifyError!void {
        if (desc.localOrNull()) |local| {
            _ = try self.requireLive(state, local);
        }
    }

    fn requireBoxyDictRef(self: *Certifier, state: *State, dict: LIR.BoxyDictRef) CertifyError!void {
        if (dict.localOrNull()) |local| {
            _ = try self.requireLive(state, local);
        }
    }

    fn requireBoxyTransferSource(
        self: *Certifier,
        state: *State,
        local: LIR.LocalId,
        mode: LIR.BoxyTransferMode,
    ) CertifyError!ValueId {
        const value = try self.requireLive(state, local);
        if (mode == .move) {
            try self.consumeUnit(state, value, local);
        }
        return value;
    }

    fn consumeBoxyTransferIntoHolder(
        self: *Certifier,
        state: *State,
        local: LIR.LocalId,
        mode: LIR.BoxyTransferMode,
        holder_value: ValueId,
    ) CertifyError!void {
        const value = try self.requireLive(state, local);
        if (mode == .move) {
            try self.consumeIntoHolder(state, value, holder_value);
        }
    }

    fn bindBoxyOwnedTarget(self: *Certifier, state: *State, target: LIR.LocalId) CertifyError!ValueId {
        if (!self.isRc(target)) return no_value;
        return try self.bindFresh(state, target, 1, &.{});
    }

    fn applyAggregate(
        self: *Certifier,
        state: *State,
        target: LIR.LocalId,
        operands: anytype,
    ) CertifyError!void {
        var operand_values_buffer: [64]ValueId = undefined;
        for (0..GuardedList.borrowLen(operands)) |index| {
            const operand = GuardedList.at(operands, index);
            const value = try self.requireLive(state, operand);
            if (index < operand_values_buffer.len) operand_values_buffer[index] = value;
        }

        var target_value: ValueId = no_value;
        if (self.isRc(target)) {
            target_value = try self.bindFresh(state, target, 1, &.{});
        }

        for (0..GuardedList.borrowLen(operands)) |index| {
            const operand = GuardedList.at(operands, index);
            if (!self.isRc(operand)) continue;
            const value = if (index < operand_values_buffer.len)
                operand_values_buffer[index]
            else
                state.valueOf(operand);
            try self.consumeIntoHolder(state, value, target_value);
        }
    }

    fn applyAggregateStore(
        self: *Certifier,
        state: *State,
        dest: LIR.LocalId,
        operands: anytype,
    ) CertifyError!void {
        _ = try self.requireLive(state, dest);

        var operand_values_buffer: [64]ValueId = undefined;
        for (0..GuardedList.borrowLen(operands)) |index| {
            const operand = GuardedList.at(operands, index);
            const value = try self.requireLive(state, operand);
            if (index < operand_values_buffer.len) operand_values_buffer[index] = value;
        }

        for (0..GuardedList.borrowLen(operands)) |index| {
            const operand = GuardedList.at(operands, index);
            if (!self.isRc(operand)) continue;
            const value = if (index < operand_values_buffer.len)
                operand_values_buffer[index]
            else
                state.valueOf(operand);
            try self.consumeIntoHolder(state, value, no_value);
        }
    }
};

fn refOpReadsLocal(op: LIR.RefOp, needle: LIR.LocalId) bool {
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

test "certifier declarations are referenced" {
    std.testing.refAllDecls(@This());
}

const testing = std.testing;

test "certifier state indexes explicit proc locals" {
    const first: LIR.LocalId = @enumFromInt(1);
    const second: LIR.LocalId = @enumFromInt(3);
    const local_dense = [_]u32{ no_dense, 0, no_dense, 1 };

    var state = try State.init(testing.allocator, &local_dense, 2);
    defer state.deinit();
    state.bindValue(first, 7);
    state.bindValue(second, 11);

    try testing.expectEqual(@as(ValueId, 7), state.valueOf(first));
    try testing.expectEqual(@as(ValueId, 11), state.valueOf(second));

    var cloned = try state.clone();
    defer cloned.deinit();
    cloned.bindValue(first, 13);
    try testing.expectEqual(@as(ValueId, 13), cloned.valueOf(first));
    try testing.expectEqual(@as(ValueId, 7), state.valueOf(first));
}

const CertifyTest = struct {
    allocator: Allocator,
    store: LirStore,
    layouts: layout_mod.Store,
    pair_str: layout_mod.Idx,
    diag: Diagnostic = .{},
    next_join_point: u32 = 0,

    fn init(allocator: Allocator) Allocator.Error!CertifyTest {
        var layouts = try layout_mod.Store.init(allocator, .u64);
        errdefer layouts.deinit();
        const pair_str = try layouts.putStructFields(&[_]layout_mod.StructField{
            .{ .index = 0, .layout = .str },
            .{ .index = 1, .layout = .str },
        });
        return .{
            .allocator = allocator,
            .store = LirStore.init(allocator),
            .layouts = layouts,
            .pair_str = pair_str,
        };
    }

    fn deinit(self: *CertifyTest) void {
        self.store.deinit();
        self.layouts.deinit();
    }

    fn local(self: *CertifyTest, layout_idx: layout_mod.Idx) Allocator.Error!LIR.LocalId {
        return try self.store.addLocal(.{ .layout_idx = layout_idx });
    }

    fn freshJoinPointId(self: *CertifyTest) LIR.JoinPointId {
        const id: LIR.JoinPointId = @enumFromInt(self.next_join_point);
        self.next_join_point += 1;
        return id;
    }

    fn rcHelper(op: layout_mod.RcOp, layout_idx: layout_mod.Idx) LIR.RcHelper {
        return LIR.RcHelper.fromConcrete(.{ .op = op, .layout_idx = layout_idx });
    }

    fn assignStr(self: *CertifyTest, target: LIR.LocalId, next: LIR.CFStmtId) Allocator.Error!LIR.CFStmtId {
        return try self.store.addCFStmt(.{ .assign_literal = .{
            .target = target,
            .value = .{ .str_literal = try self.store.insertStringView("cert", 0, 4) },
            .next = next,
        } });
    }

    fn assignI64(self: *CertifyTest, target: LIR.LocalId, next: LIR.CFStmtId) Allocator.Error!LIR.CFStmtId {
        return try self.store.addCFStmt(.{ .assign_literal = .{
            .target = target,
            .value = .{ .i64_literal = .{ .value = 1, .layout_idx = .i64 } },
            .next = next,
        } });
    }

    fn decrefStmt(self: *CertifyTest, value: LIR.LocalId, layout_idx: layout_mod.Idx, next: LIR.CFStmtId) Allocator.Error!LIR.CFStmtId {
        return try self.store.addCFStmt(.{ .decref = .{
            .value = value,
            .rc = rcHelper(.decref, layout_idx),
            .next = next,
        } });
    }

    fn decrefIfInitializedStmt(self: *CertifyTest, cond: LIR.LocalId, value: LIR.LocalId, layout_idx: layout_mod.Idx, next: LIR.CFStmtId) Allocator.Error!LIR.CFStmtId {
        return try self.store.addCFStmt(.{ .decref_if_initialized = .{
            .cond = cond,
            .value = value,
            .rc = rcHelper(.decref, layout_idx),
            .next = next,
        } });
    }

    fn increfStmt(self: *CertifyTest, value: LIR.LocalId, layout_idx: layout_mod.Idx, next: LIR.CFStmtId) Allocator.Error!LIR.CFStmtId {
        return try self.store.addCFStmt(.{ .incref = .{
            .value = value,
            .rc = rcHelper(.incref, layout_idx),
            .next = next,
        } });
    }

    fn ret(self: *CertifyTest, value: LIR.LocalId) Allocator.Error!LIR.CFStmtId {
        return try self.store.addCFStmt(.{ .ret = .{ .value = value } });
    }

    fn addProc(self: *CertifyTest, args: []const LIR.LocalId, body: LIR.CFStmtId, ret_layout: layout_mod.Idx) Allocator.Error!LIR.LirProcSpecId {
        var frame_locals = try std.ArrayList(LIR.LocalId).initCapacity(self.allocator, self.store.localCount());
        defer frame_locals.deinit(self.allocator);
        for (0..self.store.localCount()) |index| {
            frame_locals.appendAssumeCapacity(@enumFromInt(@as(u32, @intCast(index))));
        }
        return try self.store.addProcSpec(.{
            .name = self.store.freshSyntheticSymbol(),
            .args = try self.store.addLocalSpan(args),
            .frame_locals = try self.store.addLocalSpan(frame_locals.items),
            .body = body,
            .ret_layout = ret_layout,
        });
    }

    fn certify(self: *CertifyTest) CertifyError!void {
        return certifyStore(self.allocator, &self.store, &self.layouts, &.{}, arc_sig.SigTable.all_owned, &.{}, &self.diag);
    }

    fn certifyAndMeasureWork(self: *CertifyTest) CertifyError!CertifierWorkStats {
        var stats = CertifierWorkStats{};
        try certifyStoreWithWorkStats(self.allocator, &self.store, &self.layouts, &.{}, arc_sig.SigTable.all_owned, &.{}, &self.diag, &stats);
        return stats;
    }

    fn certifyWith(self: *CertifyTest, sigs: arc_sig.SigTable) CertifyError!void {
        return certifyStore(self.allocator, &self.store, &self.layouts, &.{}, sigs, &.{}, &self.diag);
    }

    fn certifyUniqueArgsOnly(self: *CertifyTest) CertifyError!void {
        const rc_local = try self.allocator.alloc(bool, self.store.localCount());
        defer self.allocator.free(rc_local);
        for (0..self.store.localCount()) |index| {
            const lir_local = self.store.getLocal(@enumFromInt(@as(u32, @intCast(index))));
            rc_local[index] = self.layouts.layoutContainsRefcounted(self.layouts.getLayout(lir_local.layout_idx));
        }
        return certifyUniqueArgs(self.allocator, &self.store, rc_local, arc_sig.SigTable.all_owned, &self.diag);
    }

    fn certifyProcAbiMetadataOnly(self: *CertifyTest) CertifyError!void {
        return certifyProcAbiMetadata(self.allocator, &self.store, &self.layouts, &self.diag);
    }
};

test "certify accepts consistent erased-callable proc ABI metadata" {
    {
        var f = try CertifyTest.init(testing.allocator);
        defer f.deinit();

        const erased_callable = try f.layouts.insertErasedCallable();
        const capture = try f.local(.opaque_ptr);
        const reuse = try f.local(erased_callable);
        const result = try f.local(.i64);
        const body = try f.ret(result);
        const arg_plan = try f.store.internErasedCallArgsPlan(&f.layouts, &.{});
        _ = try f.store.addProcSpec(.{
            .name = f.store.freshSyntheticSymbol(),
            .args = try f.store.addLocalSpan(&.{ capture, reuse }),
            .erased_reuse_arg = reuse,
            .erased_call_args = arg_plan,
            .body = body,
            .ret_layout = .i64,
            .abi = .erased_callable,
        });

        try f.certifyProcAbiMetadataOnly();
    }

    {
        var f = try CertifyTest.init(testing.allocator);
        defer f.deinit();

        const erased_callable = try f.layouts.insertErasedCallable();
        const capture = try f.local(.opaque_ptr);
        const reuse = try f.local(erased_callable);
        const body = try f.ret(reuse);
        const arg_plan = try f.store.internErasedCallArgsPlan(&f.layouts, &.{});
        _ = try f.store.addProcSpec(.{
            .name = f.store.freshSyntheticSymbol(),
            .args = try f.store.addLocalSpan(&.{ capture, reuse }),
            .erased_reuse_arg = reuse,
            .erased_call_args = arg_plan,
            .body = body,
            .ret_layout = erased_callable,
            .abi = .erased_callable,
        });

        try f.certifyProcAbiMetadataOnly();
    }
}

test "certify rejects erased-callable proc ABI metadata mismatches" {
    {
        var f = try CertifyTest.init(testing.allocator);
        defer f.deinit();

        const result = try f.local(.i64);
        const body = try f.ret(result);
        _ = try f.store.addProcSpec(.{
            .name = f.store.freshSyntheticSymbol(),
            .args = LIR.LocalSpan.empty(),
            .body = body,
            .ret_layout = .i64,
            .abi = .erased_callable,
        });

        try testing.expectError(error.Certification, f.certifyProcAbiMetadataOnly());
        try testing.expect(std.mem.find(u8, f.diag.message(), "requires trailing capture and reuse arguments") != null);
    }

    {
        var f = try CertifyTest.init(testing.allocator);
        defer f.deinit();

        const erased_callable = try f.layouts.insertErasedCallable();
        const capture = try f.local(.i64);
        const reuse = try f.local(erased_callable);
        const result = try f.local(.i64);
        const body = try f.ret(result);
        _ = try f.store.addProcSpec(.{
            .name = f.store.freshSyntheticSymbol(),
            .args = try f.store.addLocalSpan(&.{ capture, reuse }),
            .erased_reuse_arg = reuse,
            .body = body,
            .ret_layout = .i64,
            .abi = .erased_callable,
        });

        try testing.expectError(error.Certification, f.certifyProcAbiMetadataOnly());
        try testing.expect(std.mem.find(u8, f.diag.message(), "capture argument must have opaque-pointer layout") != null);
    }

    {
        var f = try CertifyTest.init(testing.allocator);
        defer f.deinit();

        const erased_callable = try f.layouts.insertErasedCallable();
        const capture = try f.local(.opaque_ptr);
        const marked_reuse = try f.local(erased_callable);
        const final_reuse = try f.local(.opaque_ptr);
        const body = try f.ret(marked_reuse);
        _ = try f.store.addProcSpec(.{
            .name = f.store.freshSyntheticSymbol(),
            .args = try f.store.addLocalSpan(&.{ capture, marked_reuse, final_reuse }),
            .erased_reuse_arg = marked_reuse,
            .body = body,
            .ret_layout = erased_callable,
            .abi = .erased_callable,
        });

        try testing.expectError(error.Certification, f.certifyProcAbiMetadataOnly());
        try testing.expect(std.mem.find(u8, f.diag.message(), "marker must name the final argument") != null);
    }

    {
        var f = try CertifyTest.init(testing.allocator);
        defer f.deinit();

        const capture = try f.local(.opaque_ptr);
        const reuse = try f.local(.opaque_ptr);
        const result = try f.local(.i64);
        const body = try f.ret(result);
        _ = try f.store.addProcSpec(.{
            .name = f.store.freshSyntheticSymbol(),
            .args = try f.store.addLocalSpan(&.{ capture, reuse }),
            .erased_reuse_arg = reuse,
            .body = body,
            .ret_layout = .i64,
            .abi = .erased_callable,
        });

        try testing.expectError(error.Certification, f.certifyProcAbiMetadataOnly());
        try testing.expect(std.mem.find(u8, f.diag.message(), "must have erased-callable layout") != null);
    }

    {
        var f = try CertifyTest.init(testing.allocator);
        defer f.deinit();

        const erased_callable = try f.layouts.insertErasedCallable();
        const capture = try f.local(.opaque_ptr);
        const reuse = try f.local(erased_callable);
        const body = try f.ret(reuse);
        _ = try f.store.addProcSpec(.{
            .name = f.store.freshSyntheticSymbol(),
            .args = try f.store.addLocalSpan(&.{ capture, reuse }),
            .body = body,
            .ret_layout = erased_callable,
            .abi = .erased_callable,
        });

        try testing.expectError(error.Certification, f.certifyProcAbiMetadataOnly());
        try testing.expect(std.mem.find(u8, f.diag.message(), "must carry its ownership marker") != null);
    }
}

test "certify rejects an erased-call argument plan that differs from the signature" {
    var f = try CertifyTest.init(testing.allocator);
    defer f.deinit();

    const first = try f.local(.u8);
    const second = try f.local(.u64);
    const capture = try f.local(.opaque_ptr);
    const erased_callable = try f.layouts.insertErasedCallable();
    const reuse = try f.local(erased_callable);
    const result = try f.local(.i64);
    const body = try f.ret(result);
    const wrong_plan = try f.store.internErasedCallArgsPlan(&f.layouts, &.{ .u8, .u8 });
    _ = try f.store.addProcSpec(.{
        .name = f.store.freshSyntheticSymbol(),
        .args = try f.store.addLocalSpan(&.{ first, second, capture, reuse }),
        .erased_reuse_arg = reuse,
        .erased_call_args = wrong_plan,
        .body = body,
        .ret_layout = .i64,
        .abi = .erased_callable,
    });

    try testing.expectError(error.Certification, f.certifyProcAbiMetadataOnly());
    try testing.expect(std.mem.find(u8, f.diag.message(), "plan metrics do not match") != null);
}

test "certify rejects an erased call site whose argument plan differs" {
    var f = try CertifyTest.init(testing.allocator);
    defer f.deinit();

    const erased_callable = try f.layouts.insertErasedCallable();
    const closure = try f.local(erased_callable);
    const first = try f.local(.u8);
    const second = try f.local(.u64);
    const result = try f.local(.i64);
    const ret = try f.ret(result);
    const args = try f.store.addLocalSpan(&.{ first, second });
    const wrong_plan = try f.store.internErasedCallArgsPlan(&f.layouts, &.{ .u8, .u8 });
    const body = try f.store.addCFStmt(.{ .assign_call_erased = .{
        .target = result,
        .closure = closure,
        .args = args,
        .arg_plan = wrong_plan,
        .next = ret,
    } });
    _ = try f.addProc(&.{ closure, first, second }, body, .i64);

    try testing.expectError(error.Certification, f.certify());
    try testing.expect(std.mem.find(u8, f.diag.message(), "plan metrics do not match") != null);
}

test "unique-argument certification isolates shared locals between procedures" {
    var f = try CertifyTest.init(testing.allocator);
    defer f.deinit();

    const left = try f.local(.str);
    const right = try f.local(.str);
    const fresh = try f.local(.str);
    const result = try f.local(.str);
    const args = try f.store.addLocalSpan(&.{ left, right });
    const checked_args = try f.store.addLocalSpan(&.{ fresh, right });

    // Model base and specialized ARC emissions: both procedure bodies bind
    // the same source LocalIds, but each body has its own statement clones.
    // A sibling's definitions and uses must not turn this procedure's one
    // fresh binding into a flow-sensitive multi-definition.
    for (0..2) |_| {
        const ret = try f.ret(result);
        const checked = try f.store.addCFStmt(.{ .assign_low_level = .{
            .target = result,
            .op = .str_concat,
            .rc_effect = LIR.LowLevel.str_concat.rcEffect(),
            .args = checked_args,
            .unique_args = 1,
            .next = ret,
        } });
        const birth = try f.store.addCFStmt(.{ .assign_low_level = .{
            .target = fresh,
            .op = .str_concat,
            .rc_effect = LIR.LowLevel.str_concat.rcEffect(),
            .args = args,
            .next = checked,
        } });
        _ = try f.addProc(&.{ left, right }, birth, .str);
    }

    // An unrelated sibling may bind that shared numeric LocalId from a
    // foreign/static origin. Its origin is irrelevant to the two procedures
    // above and must not poison their check-free claims.
    const foreign_ret = try f.ret(fresh);
    const foreign_body = try f.assignStr(fresh, foreign_ret);
    _ = try f.addProc(&.{}, foreign_body, .str);

    try f.certifyUniqueArgsOnly();
}

test "unique-argument certification rejects multiple births in one procedure" {
    var f = try CertifyTest.init(testing.allocator);
    defer f.deinit();

    const cond = try f.local(.bool);
    const left = try f.local(.str);
    const right = try f.local(.str);
    const fresh = try f.local(.str);
    const result = try f.local(.str);
    const args = try f.store.addLocalSpan(&.{ left, right });
    const checked_args = try f.store.addLocalSpan(&.{ fresh, right });
    const ret = try f.ret(result);
    const checked = try f.store.addCFStmt(.{ .assign_low_level = .{
        .target = result,
        .op = .str_concat,
        .rc_effect = LIR.LowLevel.str_concat.rcEffect(),
        .args = checked_args,
        .unique_args = 1,
        .next = ret,
    } });
    const first_birth = try f.store.addCFStmt(.{ .assign_low_level = .{
        .target = fresh,
        .op = .str_concat,
        .rc_effect = LIR.LowLevel.str_concat.rcEffect(),
        .args = args,
        .next = checked,
    } });
    const second_birth = try f.store.addCFStmt(.{ .assign_low_level = .{
        .target = fresh,
        .op = .str_concat,
        .rc_effect = LIR.LowLevel.str_concat.rcEffect(),
        .args = args,
        .next = checked,
    } });
    const body = try f.store.addCFStmt(.{ .switch_stmt = .{
        .cond = cond,
        .branches = try f.store.addCFSwitchBranches(&.{.{ .value = 1, .body = first_birth }}),
        .default_branch = second_birth,
        .continuation = checked,
    } });
    _ = try f.addProc(&.{ cond, left, right }, body, .str);

    try testing.expectError(error.Certification, f.certifyUniqueArgsOnly());
    try testing.expect(std.mem.find(u8, f.diag.message(), "without a unique birth") != null);
}

test "certify accepts owned binding released once" {
    var f = try CertifyTest.init(testing.allocator);
    defer f.deinit();
    const value = try f.local(.str);
    const result = try f.local(.i64);
    const ret = try f.ret(result);
    const release = try f.decrefStmt(value, .str, ret);
    const result_assign = try f.assignI64(result, release);
    const body = try f.assignStr(value, result_assign);
    _ = try f.addProc(&.{}, body, .i64);
    try f.certify();
}

test "certify rejects inconsistent erased call reuse fields" {
    {
        var f = try CertifyTest.init(testing.allocator);
        defer f.deinit();
        const erased_callable = try f.layouts.insertErasedCallable();
        const closure = try f.local(erased_callable);
        const result = try f.local(erased_callable);
        const ret = try f.ret(result);
        const arg_plan = try f.store.internErasedCallArgsPlan(&f.layouts, &.{});
        const body = try f.store.addCFStmt(.{ .assign_call_erased = .{
            .target = result,
            .closure = closure,
            .args = LIR.LocalSpan.empty(),
            .arg_plan = arg_plan,
            .reuse_closure = true,
            .reuse_source = null,
            .next = ret,
        } });
        _ = try f.addProc(&.{closure}, body, erased_callable);
        try testing.expectError(error.Certification, f.certify());
        try testing.expect(std.mem.find(u8, f.diag.message(), "reuse flag and ownership source disagreed") != null);
    }

    {
        var f = try CertifyTest.init(testing.allocator);
        defer f.deinit();
        const erased_callable = try f.layouts.insertErasedCallable();
        const closure = try f.local(erased_callable);
        const result = try f.local(erased_callable);
        const ret = try f.ret(result);
        const arg_plan = try f.store.internErasedCallArgsPlan(&f.layouts, &.{});
        const body = try f.store.addCFStmt(.{ .assign_call_erased = .{
            .target = result,
            .closure = closure,
            .args = LIR.LocalSpan.empty(),
            .arg_plan = arg_plan,
            .reuse_closure = false,
            .reuse_source = closure,
            .next = ret,
        } });
        _ = try f.addProc(&.{closure}, body, erased_callable);
        try testing.expectError(error.Certification, f.certify());
        try testing.expect(std.mem.find(u8, f.diag.message(), "reuse flag and ownership source disagreed") != null);
    }
}

test "certify accepts erased call reuse from a transparent outer owner" {
    var f = try CertifyTest.init(testing.allocator);
    defer f.deinit();
    const erased_callable = try f.layouts.insertErasedCallable();
    const owner = try f.local(erased_callable);
    const closure = try f.local(erased_callable);
    const result = try f.local(erased_callable);
    const ret = try f.ret(result);
    const arg_plan = try f.store.internErasedCallArgsPlan(&f.layouts, &.{});
    const call = try f.store.addCFStmt(.{ .assign_call_erased = .{
        .target = result,
        .closure = closure,
        .args = LIR.LocalSpan.empty(),
        .arg_plan = arg_plan,
        .reuse_closure = true,
        .reuse_source = owner,
        .next = ret,
    } });
    const body = try f.store.addCFStmt(.{ .assign_ref = .{
        .target = closure,
        .op = .{ .nominal = .{ .backing_ref = owner } },
        .next = call,
    } });
    _ = try f.addProc(&.{owner}, body, erased_callable);
    try f.certify();
}

test "certify rejects erased call reuse from a different allocation" {
    var f = try CertifyTest.init(testing.allocator);
    defer f.deinit();
    const erased_callable = try f.layouts.insertErasedCallable();
    const closure = try f.local(erased_callable);
    const unrelated = try f.local(erased_callable);
    const result = try f.local(erased_callable);
    const ret = try f.ret(result);
    const arg_plan = try f.store.internErasedCallArgsPlan(&f.layouts, &.{});
    const body = try f.store.addCFStmt(.{ .assign_call_erased = .{
        .target = result,
        .closure = closure,
        .args = LIR.LocalSpan.empty(),
        .arg_plan = arg_plan,
        .reuse_closure = true,
        .reuse_source = unrelated,
        .next = ret,
    } });
    _ = try f.addProc(&.{ closure, unrelated }, body, erased_callable);
    try testing.expectError(error.Certification, f.certify());
    try testing.expect(std.mem.find(u8, f.diag.message(), "do not denote the same allocation") != null);
}

test "certify flags a leaked binding" {
    var f = try CertifyTest.init(testing.allocator);
    defer f.deinit();
    const value = try f.local(.str);
    const result = try f.local(.i64);
    const ret = try f.ret(result);
    const result_assign = try f.assignI64(result, ret);
    const body = try f.assignStr(value, result_assign);
    _ = try f.addProc(&.{}, body, .i64);
    try testing.expectError(error.Certification, f.certify());
    try testing.expect(std.mem.find(u8, f.diag.message(), "leaked") != null);
}

test "certify flags a double release" {
    var f = try CertifyTest.init(testing.allocator);
    defer f.deinit();
    const value = try f.local(.str);
    const result = try f.local(.i64);
    const ret = try f.ret(result);
    const second = try f.decrefStmt(value, .str, ret);
    const first = try f.decrefStmt(value, .str, second);
    const result_assign = try f.assignI64(result, first);
    const body = try f.assignStr(value, result_assign);
    _ = try f.addProc(&.{}, body, .i64);
    try testing.expectError(error.Certification, f.certify());
    try testing.expect(std.mem.find(u8, f.diag.message(), "without an ownership unit") != null);
}

test "certify flags use after release" {
    var f = try CertifyTest.init(testing.allocator);
    defer f.deinit();
    const value = try f.local(.str);
    const ret = try f.ret(value);
    const release = try f.decrefStmt(value, .str, ret);
    const body = try f.assignStr(value, release);
    _ = try f.addProc(&.{}, body, .str);
    try testing.expectError(error.Certification, f.certify());
    try testing.expect(std.mem.find(u8, f.diag.message(), "dead refcounted local") != null);
}

test "certify accepts an aliased value released through either name" {
    var f = try CertifyTest.init(testing.allocator);
    defer f.deinit();
    const original = try f.local(.str);
    const alias = try f.local(.str);
    const result = try f.local(.i64);
    const ret = try f.ret(result);
    const release = try f.decrefStmt(alias, .str, ret);
    const result_assign = try f.assignI64(result, release);
    const alias_stmt = try f.store.addCFStmt(.{ .assign_ref = .{
        .target = alias,
        .op = .{ .local = original },
        .next = result_assign,
    } });
    const body = try f.assignStr(original, alias_stmt);
    _ = try f.addProc(&.{}, body, .i64);
    try f.certify();
}

test "certify flags releasing an aliased value through both names" {
    var f = try CertifyTest.init(testing.allocator);
    defer f.deinit();
    const original = try f.local(.str);
    const alias = try f.local(.str);
    const result = try f.local(.i64);
    const ret = try f.ret(result);
    const second = try f.decrefStmt(original, .str, ret);
    const first = try f.decrefStmt(alias, .str, second);
    const result_assign = try f.assignI64(result, first);
    const alias_stmt = try f.store.addCFStmt(.{ .assign_ref = .{
        .target = alias,
        .op = .{ .local = original },
        .next = result_assign,
    } });
    const body = try f.assignStr(original, alias_stmt);
    _ = try f.addProc(&.{}, body, .i64);
    try testing.expectError(error.Certification, f.certify());
}

test "certify accepts a payload borrow used while the owner is live" {
    var f = try CertifyTest.init(testing.allocator);
    defer f.deinit();
    const pair = try f.local(f.pair_str);
    const field = try f.local(.str);
    const result = try f.local(.i64);
    const a = try f.local(.str);
    const b = try f.local(.str);

    // assign a; assign b; incref a; incref b; pair = {a, b}; decref a;
    // decref b; field = pair.0 (borrow, no incref); expect field;
    // result = 1; decref pair; ret result
    const ret = try f.ret(result);
    const release_pair = try f.decrefStmt(pair, f.pair_str, ret);
    const result_assign = try f.assignI64(result, release_pair);
    const use_field = try f.store.addCFStmt(.{ .expect = .{
        .condition = field,
        .next = result_assign,
    } });
    const field_read = try f.store.addCFStmt(.{ .assign_ref = .{
        .target = field,
        .op = .{ .field = .{ .source = pair, .field_idx = 0 } },
        .next = use_field,
    } });
    const release_b = try f.decrefStmt(b, .str, field_read);
    const release_a = try f.decrefStmt(a, .str, release_b);
    const pair_assign = try f.store.addCFStmt(.{ .assign_struct = .{
        .target = pair,
        .fields = try f.store.addLocalSpan(&.{ a, b }),
        .next = release_a,
    } });
    const incref_b = try f.increfStmt(b, .str, pair_assign);
    const incref_a = try f.increfStmt(a, .str, incref_b);
    const assign_b = try f.assignStr(b, incref_a);
    const body = try f.assignStr(a, assign_b);
    _ = try f.addProc(&.{}, body, .i64);
    try f.certify();
}

test "certify accepts a retained Boxy field borrowed from implicit capture storage" {
    var f = try CertifyTest.init(testing.allocator);
    defer f.deinit();

    const erased_box = try f.layouts.insertLayout(layout_mod.Layout.erasedBox());
    const capture_layout = try f.layouts.putStructFields(&[_]layout_mod.StructField{
        .{ .index = 0, .layout = erased_box },
    });
    const capture = try f.local(capture_layout);
    const field = try f.local(erased_box);
    const result = try f.local(.i64);
    const desc_local = try f.local(.opaque_ptr);
    const desc = LIR.BoxyDescRef{ .local = desc_local };
    f.store.setLocalBoxyDesc(field, desc);

    const ret = try f.ret(result);
    const result_assign = try f.assignI64(result, ret);
    const release = try f.store.addCFStmt(.{ .decref = .{
        .value = field,
        .rc = .{ .boxy = desc },
        .next = result_assign,
    } });
    const retain = try f.store.addCFStmt(.{ .incref = .{
        .value = field,
        .rc = .{ .boxy = desc },
        .next = release,
    } });
    const field_read = try f.store.addCFStmt(.{ .assign_ref = .{
        .target = field,
        .op = .{ .field = .{ .source = capture, .field_idx = 0 } },
        .next = retain,
    } });
    _ = try f.addProc(&.{ capture, desc_local }, field_read, .i64);

    const boxy_descs = try f.allocator.alloc(?LIR.BoxyDescRef, f.store.localCount());
    defer f.allocator.free(boxy_descs);
    @memset(boxy_descs, null);
    boxy_descs[@intFromEnum(field)] = desc;
    const sigs = [_]arc_sig.RcSig{
        arc_sig.RcSig.all_owned.withBorrowedParam(0),
    };
    try certifyStore(
        f.allocator,
        &f.store,
        &f.layouts,
        boxy_descs,
        .{ .sigs = &sigs },
        &.{},
        &f.diag,
    );
}

test "certify flags a payload borrow used after the owner dies" {
    var f = try CertifyTest.init(testing.allocator);
    defer f.deinit();
    const pair = try f.local(f.pair_str);
    const field = try f.local(.str);
    const result = try f.local(.i64);
    const ret = try f.ret(result);
    const result_assign = try f.assignI64(result, ret);
    const use_field = try f.store.addCFStmt(.{ .expect = .{
        .condition = field,
        .next = result_assign,
    } });
    const release_pair = try f.decrefStmt(pair, f.pair_str, use_field);
    const field_read = try f.store.addCFStmt(.{ .assign_ref = .{
        .target = field,
        .op = .{ .field = .{ .source = pair, .field_idx = 0 } },
        .next = release_pair,
    } });
    const a = try f.local(.str);
    const b = try f.local(.str);
    const release_b = try f.decrefStmt(b, .str, field_read);
    const release_a = try f.decrefStmt(a, .str, release_b);
    const pair_assign = try f.store.addCFStmt(.{ .assign_struct = .{
        .target = pair,
        .fields = try f.store.addLocalSpan(&.{ a, b }),
        .next = release_a,
    } });
    const incref_b = try f.increfStmt(b, .str, pair_assign);
    const incref_a = try f.increfStmt(a, .str, incref_b);
    const assign_b = try f.assignStr(b, incref_a);
    const body = try f.assignStr(a, assign_b);
    _ = try f.addProc(&.{}, body, .i64);
    try testing.expectError(error.Certification, f.certify());
    try testing.expect(std.mem.find(u8, f.diag.message(), "dead refcounted local") != null);
}

test "certify flags an incref-restored payload borrow only when over-released" {
    var f = try CertifyTest.init(testing.allocator);
    defer f.deinit();
    const pair = try f.local(f.pair_str);
    const field = try f.local(.str);
    const result = try f.local(.i64);
    const ret = try f.ret(result);
    const result_assign = try f.assignI64(result, ret);
    const use_field = try f.store.addCFStmt(.{ .expect = .{
        .condition = field,
        .next = result_assign,
    } });
    const release_field = try f.decrefStmt(field, .str, use_field);
    const release_pair = try f.decrefStmt(pair, f.pair_str, release_field);
    const incref_field = try f.increfStmt(field, .str, release_pair);
    const field_read = try f.store.addCFStmt(.{ .assign_ref = .{
        .target = field,
        .op = .{ .field = .{ .source = pair, .field_idx = 0 } },
        .next = incref_field,
    } });
    const a = try f.local(.str);
    const b = try f.local(.str);
    const release_b = try f.decrefStmt(b, .str, field_read);
    const release_a = try f.decrefStmt(a, .str, release_b);
    const pair_assign = try f.store.addCFStmt(.{ .assign_struct = .{
        .target = pair,
        .fields = try f.store.addLocalSpan(&.{ a, b }),
        .next = release_a,
    } });
    const incref_b = try f.increfStmt(b, .str, pair_assign);
    const incref_a = try f.increfStmt(a, .str, incref_b);
    const assign_b = try f.assignStr(b, incref_a);
    const body = try f.assignStr(a, assign_b);
    _ = try f.addProc(&.{}, body, .i64);
    // The borrow took its own unit via incref before the owner died and the
    // use-then-release order is sound: incref field, release pair, release
    // field after use. The chain above releases field BEFORE its use, which
    // must fail.
    try testing.expectError(error.Certification, f.certify());
}

test "certify flags an unreleased owned argument consumed twice" {
    var f = try CertifyTest.init(testing.allocator);
    defer f.deinit();
    const value = try f.local(.str);
    const target = try f.local(.i64);
    const callee = try f.store.addProcSpec(.{
        .name = f.store.freshSyntheticSymbol(),
        .args = LIR.LocalSpan.empty(),
        .body = null,
        .ret_layout = .i64,
    });
    const ret = try f.ret(target);
    const call = try f.store.addCFStmt(.{ .assign_call = .{
        .target = target,
        .proc = callee,
        .args = try f.store.addLocalSpan(&.{ value, value }),
        .next = ret,
    } });
    const body = try f.assignStr(value, call);
    _ = try f.addProc(&.{}, body, .i64);
    try testing.expectError(error.Certification, f.certify());
    try testing.expect(std.mem.find(u8, f.diag.message(), "without an ownership unit") != null);
}

test "certify accepts a doubly-consumed argument with one incref" {
    var f = try CertifyTest.init(testing.allocator);
    defer f.deinit();
    const value = try f.local(.str);
    const target = try f.local(.i64);
    const callee = try f.store.addProcSpec(.{
        .name = f.store.freshSyntheticSymbol(),
        .args = LIR.LocalSpan.empty(),
        .body = null,
        .ret_layout = .i64,
    });
    const ret = try f.ret(target);
    const call = try f.store.addCFStmt(.{ .assign_call = .{
        .target = target,
        .proc = callee,
        .args = try f.store.addLocalSpan(&.{ value, value }),
        .next = ret,
    } });
    const retain = try f.increfStmt(value, .str, call);
    const body = try f.assignStr(value, retain);
    _ = try f.addProc(&.{}, body, .i64);
    try f.certify();
}

test "certify flags release of a borrowed parameter" {
    var f = try CertifyTest.init(testing.allocator);
    defer f.deinit();
    const param = try f.local(.str);
    const result = try f.local(.i64);
    const ret = try f.ret(result);
    const result_assign = try f.assignI64(result, ret);
    const release = try f.decrefStmt(param, .str, result_assign);
    _ = try f.addProc(&.{param}, release, .i64);

    const sigs = [_]arc_sig.RcSig{arc_sig.RcSig.all_owned.withBorrowedParam(0)};
    try testing.expectError(error.Certification, f.certifyWith(.{ .sigs = &sigs }));
    try testing.expect(std.mem.find(u8, f.diag.message(), "without an ownership unit") != null);
}

test "certify accepts a borrowed parameter used without RC statements" {
    var f = try CertifyTest.init(testing.allocator);
    defer f.deinit();
    const param = try f.local(.str);
    const result = try f.local(.i64);
    const ret = try f.ret(result);
    const result_assign = try f.assignI64(result, ret);
    const use_param = try f.store.addCFStmt(.{ .expect = .{
        .condition = param,
        .next = result_assign,
    } });
    _ = try f.addProc(&.{param}, use_param, .i64);

    const sigs = [_]arc_sig.RcSig{arc_sig.RcSig.all_owned.withBorrowedParam(0)};
    try f.certifyWith(.{ .sigs = &sigs });
}

test "certify preserves borrowed parameter lifetime through an owned alias join" {
    var f = try CertifyTest.init(testing.allocator);
    defer f.deinit();
    const param = try f.local(.str);
    const alias = try f.local(.str);
    const result = try f.local(.i64);

    const ret = try f.ret(result);
    const result_assign = try f.assignI64(result, ret);
    const release_replacement = try f.decrefStmt(alias, .str, result_assign);
    const use_param = try f.store.addCFStmt(.{ .expect = .{
        .condition = param,
        .next = release_replacement,
    } });
    const replace_alias = try f.assignStr(alias, use_param);
    const release_old_alias = try f.decrefStmt(alias, .str, replace_alias);

    const join_id = f.freshJoinPointId();
    const jump = try f.store.addCFStmt(.{ .jump = .{ .target = join_id } });
    const join_stmt = try f.store.addCFStmt(.{ .join = .{
        .id = join_id,
        .params = LIR.LocalSpan.empty(),
        .body = release_old_alias,
        .remainder = jump,
    } });
    const retain_alias = try f.increfStmt(alias, .str, join_stmt);
    const bind_alias = try f.store.addCFStmt(.{ .assign_ref = .{
        .target = alias,
        .op = .{ .local = param },
        .next = retain_alias,
    } });
    _ = try f.addProc(&.{param}, bind_alias, .i64);

    const sigs = [_]arc_sig.RcSig{arc_sig.RcSig.all_owned.withBorrowedParam(0)};
    try f.certifyWith(.{ .sigs = &sigs });
}

test "certify flags an owned parameter that is never released" {
    var f = try CertifyTest.init(testing.allocator);
    defer f.deinit();
    const param = try f.local(.str);
    const result = try f.local(.i64);
    const ret = try f.ret(result);
    const body = try f.assignI64(result, ret);
    _ = try f.addProc(&.{param}, body, .i64);
    try testing.expectError(error.Certification, f.certify());
    try testing.expect(std.mem.find(u8, f.diag.message(), "leaked") != null);
}

test "certify accepts conditional decref of live payload" {
    var f = try CertifyTest.init(testing.allocator);
    defer f.deinit();
    const payload = try f.local(.str);
    const cond = try f.local(.i64);
    const result = try f.local(.i64);

    const ret = try f.ret(result);
    const conditional_release = try f.decrefIfInitializedStmt(cond, payload, .str, ret);
    const result_assign = try f.assignI64(result, conditional_release);
    const cond_assign = try f.assignI64(cond, result_assign);
    const body = try f.assignStr(payload, cond_assign);
    _ = try f.addProc(&.{}, body, .i64);

    try f.certify();
}

test "certify accepts conditional decref of unbound payload" {
    var f = try CertifyTest.init(testing.allocator);
    defer f.deinit();
    const payload = try f.local(.str);
    const cond = try f.local(.i64);
    const result = try f.local(.i64);

    const ret = try f.ret(result);
    const conditional_release = try f.decrefIfInitializedStmt(cond, payload, .str, ret);
    const result_assign = try f.assignI64(result, conditional_release);
    const body = try f.assignI64(cond, result_assign);
    _ = try f.addProc(&.{}, body, .i64);

    try f.certify();
}

test "certify follows initialized payload switch branch when rc payload is live" {
    var f = try CertifyTest.init(testing.allocator);
    defer f.deinit();
    const payload = try f.local(.str);
    const cond = try f.local(.i64);
    const result = try f.local(.i64);

    const ret = try f.ret(result);
    const initialized_release = try f.decrefStmt(payload, .str, ret);
    const initialized_branch = try f.assignI64(result, initialized_release);
    const uninitialized_branch = try f.assignI64(result, ret);

    const switch_stmt = try f.store.addCFStmt(.{ .switch_initialized_payload = .{
        .cond = cond,
        .payload = payload,
        .initialized_branch = initialized_branch,
        .uninitialized_branch = uninitialized_branch,
    } });
    const cond_assign = try f.assignI64(cond, switch_stmt);
    const body = try f.assignStr(payload, cond_assign);
    _ = try f.addProc(&.{}, body, .i64);

    // The uninitialized branch would leak the live payload if the certifier
    // explored both branches. This proves the switch is an explicit
    // initialized-cell test, not an ordinary runtime value switch.
    try f.certify();
}

test "certify follows uninitialized payload switch branch when rc payload is unbound" {
    var f = try CertifyTest.init(testing.allocator);
    defer f.deinit();
    const payload = try f.local(.str);
    const cond = try f.local(.i64);
    const result = try f.local(.i64);

    const ret = try f.ret(result);
    const bad_initialized_release = try f.decrefStmt(payload, .str, ret);
    const initialized_branch = try f.assignI64(result, bad_initialized_release);
    const uninitialized_branch = try f.assignI64(result, ret);

    const switch_stmt = try f.store.addCFStmt(.{ .switch_initialized_payload = .{
        .cond = cond,
        .payload = payload,
        .initialized_branch = initialized_branch,
        .uninitialized_branch = uninitialized_branch,
    } });
    const body = try f.assignI64(cond, switch_stmt);
    _ = try f.addProc(&.{}, body, .i64);

    // The initialized branch reads an unbound RC local, so this only passes if
    // the certifier follows the uninitialized branch selected by ownership
    // state.
    try f.certify();
}

test "certify flags uninitialized payload switch branch that reads unbound payload" {
    var f = try CertifyTest.init(testing.allocator);
    defer f.deinit();
    const payload = try f.local(.str);
    const cond = try f.local(.i64);
    const result = try f.local(.i64);

    const ret = try f.ret(result);
    const initialized_branch = try f.assignI64(result, ret);
    const bad_uninitialized_release = try f.decrefStmt(payload, .str, ret);
    const uninitialized_branch = try f.assignI64(result, bad_uninitialized_release);

    const switch_stmt = try f.store.addCFStmt(.{ .switch_initialized_payload = .{
        .cond = cond,
        .payload = payload,
        .initialized_branch = initialized_branch,
        .uninitialized_branch = uninitialized_branch,
    } });
    const body = try f.assignI64(cond, switch_stmt);
    _ = try f.addProc(&.{}, body, .i64);

    try testing.expectError(error.Certification, f.certify());
    try testing.expect(std.mem.find(u8, f.diag.message(), "unbound") != null);
}

test "certify compresses maybe-initialized join payload states" {
    var f = try CertifyTest.init(testing.allocator);
    defer f.deinit();
    const payload = try f.local(.str);
    const cond = try f.local(.i64);
    const result = try f.local(.i64);

    const join_id = f.freshJoinPointId();
    const ret = try f.ret(result);
    const conditional_release = try f.decrefIfInitializedStmt(cond, payload, .str, ret);
    const result_assign = try f.assignI64(result, conditional_release);

    const jump_with_payload = try f.store.addCFStmt(.{ .jump = .{ .target = join_id } });
    const jump_without_payload = try f.store.addCFStmt(.{ .jump = .{ .target = join_id } });
    const release_before_jump = try f.decrefStmt(payload, .str, jump_without_payload);

    const switch_stmt = try f.store.addCFStmt(.{ .switch_stmt = .{
        .cond = cond,
        .branches = try f.store.addCFSwitchBranches(&[_]LIR.CFSwitchBranch{
            .{ .value = 1, .body = jump_with_payload },
        }),
        .default_branch = release_before_jump,
    } });
    const join_stmt = try f.store.addCFStmt(.{ .join = .{
        .id = join_id,
        .params = try f.store.addLocalSpan(&.{payload}),
        .maybe_uninitialized_params = try f.store.addLocalSpan(&.{payload}),
        .maybe_uninitialized_conditions = try f.store.addLocalSpan(&.{cond}),
        .maybe_uninitialized_condition_masks = try f.store.addU64Span(&.{1}),
        .body = result_assign,
        .remainder = switch_stmt,
    } });
    const cond_assign = try f.assignI64(cond, join_stmt);
    const body = try f.assignStr(payload, cond_assign);
    _ = try f.addProc(&.{}, body, .i64);

    try f.certify();
}

test "certify promotes conditional payload on initialized switch edge" {
    var f = try CertifyTest.init(testing.allocator);
    defer f.deinit();
    const payload = try f.local(.str);
    const presence = try f.local(.i64);
    const result = try f.local(.i64);

    const ret = try f.ret(result);
    const release = try f.decrefStmt(payload, .str, ret);
    const second_initialized = try f.assignI64(result, release);
    const second_uninitialized = try f.assignI64(result, release);
    const second_switch = try f.store.addCFStmt(.{ .switch_initialized_payload = .{
        .cond = presence,
        .payload = payload,
        .initialized_branch = second_initialized,
        .uninitialized_branch = second_uninitialized,
    } });
    const first_uninitialized = try f.assignI64(result, ret);
    const first_switch = try f.store.addCFStmt(.{ .switch_initialized_payload = .{
        .cond = presence,
        .payload = payload,
        .initialized_branch = second_switch,
        .uninitialized_branch = first_uninitialized,
    } });

    const join_id = f.freshJoinPointId();
    const jump_with_payload = try f.store.addCFStmt(.{ .jump = .{ .target = join_id } });
    const jump_without_payload = try f.store.addCFStmt(.{ .jump = .{ .target = join_id } });
    const release_before_jump = try f.decrefStmt(payload, .str, jump_without_payload);
    const choose_presence = try f.store.addCFStmt(.{ .switch_stmt = .{
        .cond = presence,
        .branches = try f.store.addCFSwitchBranches(&[_]LIR.CFSwitchBranch{
            .{ .value = 1, .body = jump_with_payload },
        }),
        .default_branch = release_before_jump,
    } });
    const join_stmt = try f.store.addCFStmt(.{ .join = .{
        .id = join_id,
        .params = try f.store.addLocalSpan(&.{payload}),
        .maybe_uninitialized_params = try f.store.addLocalSpan(&.{payload}),
        .maybe_uninitialized_conditions = try f.store.addLocalSpan(&.{presence}),
        .maybe_uninitialized_condition_masks = try f.store.addU64Span(&.{1}),
        .body = first_switch,
        .remainder = choose_presence,
    } });
    const presence_assign = try f.assignI64(presence, join_stmt);
    const body = try f.assignStr(payload, presence_assign);
    _ = try f.addProc(&.{}, body, .i64);

    // The first switch's initialized edge proves the payload exists. Re-testing
    // that condition therefore follows only the initialized edge; retaining
    // the stale conditional state also explores the unreachable uninitialized
    // edge, where the deliberately invalid release exposes the false path.
    try f.certify();
}

test "certify does not repeat conditional payload work after initialized edge" {
    var f = try CertifyTest.init(testing.allocator);
    defer f.deinit();
    const payload = try f.local(.str);
    const presence = try f.local(.i64);
    const result = try f.local(.i64);

    const ret = try f.ret(result);
    const release = try f.decrefStmt(payload, .str, ret);
    var initialized_branch = try f.assignI64(result, release);
    const uninitialized_branch = try f.assignI64(result, ret);

    // Re-check one proven presence condition enough times that retained
    // conditional state would deterministically exceed the structural work
    // bound below. Wall-clock speed is deliberately irrelevant.
    const repeated_checks = 32;
    for (0..repeated_checks) |_| {
        initialized_branch = try f.store.addCFStmt(.{ .switch_initialized_payload = .{
            .cond = presence,
            .payload = payload,
            .initialized_branch = initialized_branch,
            .uninitialized_branch = uninitialized_branch,
        } });
    }

    const join_id = f.freshJoinPointId();
    const jump_with_payload = try f.store.addCFStmt(.{ .jump = .{ .target = join_id } });
    const jump_without_payload = try f.store.addCFStmt(.{ .jump = .{ .target = join_id } });
    const release_before_jump = try f.decrefStmt(payload, .str, jump_without_payload);
    const choose_presence = try f.store.addCFStmt(.{ .switch_stmt = .{
        .cond = presence,
        .branches = try f.store.addCFSwitchBranches(&[_]LIR.CFSwitchBranch{
            .{ .value = 1, .body = jump_with_payload },
        }),
        .default_branch = release_before_jump,
    } });
    const join_stmt = try f.store.addCFStmt(.{ .join = .{
        .id = join_id,
        .params = try f.store.addLocalSpan(&.{payload}),
        .maybe_uninitialized_params = try f.store.addLocalSpan(&.{payload}),
        .maybe_uninitialized_conditions = try f.store.addLocalSpan(&.{presence}),
        .maybe_uninitialized_condition_masks = try f.store.addU64Span(&.{1}),
        .body = initialized_branch,
        .remainder = choose_presence,
    } });
    const presence_assign = try f.assignI64(presence, join_stmt);
    const body = try f.assignStr(payload, presence_assign);
    _ = try f.addProc(&.{}, body, .i64);

    const stats = try f.certifyAndMeasureWork();
    try testing.expectEqual(@as(usize, 1), stats.conditional_payload_splits);
    try testing.expectEqual(@as(usize, repeated_checks + 6), stats.work_items);
}

test "certify rejects a mismatched conditional payload guard before refinement" {
    var f = try CertifyTest.init(testing.allocator);
    defer f.deinit();
    const payload = try f.local(.str);
    const presence = try f.local(.i64);
    const unrelated = try f.local(.i64);
    const result = try f.local(.i64);

    const ret = try f.ret(result);
    const release = try f.decrefStmt(payload, .str, ret);
    const initialized = try f.assignI64(result, release);
    const uninitialized = try f.assignI64(result, ret);
    const mismatched_switch = try f.store.addCFStmt(.{ .switch_initialized_payload = .{
        .cond = unrelated,
        .payload = payload,
        .initialized_branch = initialized,
        .uninitialized_branch = uninitialized,
    } });

    const join_id = f.freshJoinPointId();
    const jump_with_payload = try f.store.addCFStmt(.{ .jump = .{ .target = join_id } });
    const jump_without_payload = try f.store.addCFStmt(.{ .jump = .{ .target = join_id } });
    const release_before_jump = try f.decrefStmt(payload, .str, jump_without_payload);
    const choose_presence = try f.store.addCFStmt(.{ .switch_stmt = .{
        .cond = presence,
        .branches = try f.store.addCFSwitchBranches(&[_]LIR.CFSwitchBranch{
            .{ .value = 1, .body = jump_with_payload },
        }),
        .default_branch = release_before_jump,
    } });
    const join_stmt = try f.store.addCFStmt(.{ .join = .{
        .id = join_id,
        .params = try f.store.addLocalSpan(&.{payload}),
        .maybe_uninitialized_params = try f.store.addLocalSpan(&.{payload}),
        .maybe_uninitialized_conditions = try f.store.addLocalSpan(&.{presence}),
        .maybe_uninitialized_condition_masks = try f.store.addU64Span(&.{1}),
        .body = mismatched_switch,
        .remainder = choose_presence,
    } });
    const unrelated_assign = try f.assignI64(unrelated, join_stmt);
    const presence_assign = try f.assignI64(presence, unrelated_assign);
    const body = try f.assignStr(payload, presence_assign);
    _ = try f.addProc(&.{}, body, .i64);

    try testing.expectError(error.Certification, f.certify());
    try testing.expect(std.mem.find(u8, f.diag.message(), "did not match") != null);
}

test "certify flags branches that disagree at a join" {
    var f = try CertifyTest.init(testing.allocator);
    defer f.deinit();
    const value = try f.local(.str);
    const cond = try f.local(.i64);
    const result = try f.local(.i64);

    const join_id = f.freshJoinPointId();
    const ret = try f.ret(result);
    const release_in_body = try f.decrefStmt(value, .str, ret);
    const result_assign = try f.assignI64(result, release_in_body);

    const jump_a = try f.store.addCFStmt(.{ .jump = .{ .target = join_id } });
    const jump_b = try f.store.addCFStmt(.{ .jump = .{ .target = join_id } });
    // Branch B releases the value before jumping; branch A does not.
    const branch_b = try f.decrefStmt(value, .str, jump_b);

    const switch_stmt = try f.store.addCFStmt(.{ .switch_stmt = .{
        .cond = cond,
        .branches = try f.store.addCFSwitchBranches(&[_]LIR.CFSwitchBranch{
            .{ .value = 1, .body = jump_a },
        }),
        .default_branch = branch_b,
    } });
    const join_stmt = try f.store.addCFStmt(.{ .join = .{
        .id = join_id,
        .params = LIR.LocalSpan.empty(),
        .body = result_assign,
        .remainder = switch_stmt,
    } });
    const cond_assign = try f.assignI64(cond, join_stmt);
    const body = try f.assignStr(value, cond_assign);
    _ = try f.addProc(&.{}, body, .i64);
    // The disagreement weakens the join's entry assumption to unbound, and
    // re-certifying the body flags the release of the unbound name.
    try testing.expectError(error.Certification, f.certify());
    try testing.expect(std.mem.find(u8, f.diag.message(), "unbound") != null);
}

test "certify accepts agreeing jumps through a join" {
    var f = try CertifyTest.init(testing.allocator);
    defer f.deinit();
    const value = try f.local(.str);
    const cond = try f.local(.i64);
    const result = try f.local(.i64);

    const join_id = f.freshJoinPointId();
    const ret = try f.ret(result);
    const release_in_body = try f.decrefStmt(value, .str, ret);
    const result_assign = try f.assignI64(result, release_in_body);

    const jump_a = try f.store.addCFStmt(.{ .jump = .{ .target = join_id } });
    const jump_b = try f.store.addCFStmt(.{ .jump = .{ .target = join_id } });

    const switch_stmt = try f.store.addCFStmt(.{ .switch_stmt = .{
        .cond = cond,
        .branches = try f.store.addCFSwitchBranches(&[_]LIR.CFSwitchBranch{
            .{ .value = 1, .body = jump_a },
        }),
        .default_branch = jump_b,
    } });
    const join_stmt = try f.store.addCFStmt(.{ .join = .{
        .id = join_id,
        .params = LIR.LocalSpan.empty(),
        .body = result_assign,
        .remainder = switch_stmt,
    } });
    const cond_assign = try f.assignI64(cond, join_stmt);
    const body = try f.assignStr(value, cond_assign);
    _ = try f.addProc(&.{}, body, .i64);
    try f.certify();
}

test "certify preserves payload lender when retained holder crosses join" {
    var f = try CertifyTest.init(testing.allocator);
    defer f.deinit();
    const owner = try f.local(f.pair_str);
    const field = try f.local(.str);
    const retained_holder = try f.local(f.pair_str);
    const holder_other = try f.local(.str);
    const result = try f.local(.i64);

    const join_id = f.freshJoinPointId();
    const ret = try f.ret(result);
    const result_assign = try f.assignI64(result, ret);
    const use_field = try f.store.addCFStmt(.{ .expect = .{
        .condition = field,
        .next = result_assign,
    } });
    const release_holder = try f.decrefStmt(retained_holder, f.pair_str, use_field);
    const jump = try f.store.addCFStmt(.{ .jump = .{ .target = join_id } });
    const join_stmt = try f.store.addCFStmt(.{ .join = .{
        .id = join_id,
        .params = LIR.LocalSpan.empty(),
        .body = release_holder,
        .remainder = jump,
    } });
    const holder_assign = try f.store.addCFStmt(.{ .assign_struct = .{
        .target = retained_holder,
        .fields = try f.store.addLocalSpan(&.{ field, holder_other }),
        .next = join_stmt,
    } });
    const assign_holder_other = try f.assignStr(holder_other, holder_assign);
    const retain_field = try f.increfStmt(field, .str, assign_holder_other);
    const field_read = try f.store.addCFStmt(.{ .assign_ref = .{
        .target = field,
        .op = .{ .field = .{ .source = owner, .field_idx = 0 } },
        .next = retain_field,
    } });
    _ = try f.addProc(&.{owner}, field_read, .i64);

    const sigs = [_]arc_sig.RcSig{arc_sig.RcSig.all_owned.withBorrowedParam(0)};
    try f.certifyWith(.{ .sigs = &sigs });
}

test "certify preserves deep ABI lender when join body releases retained intermediate" {
    // Repro for https://github.com/roc-lang/roc/issues/10471
    // The inner field remains borrowed from the ABI-live owner after the
    // retained intermediate field is released inside the join body.
    var f = try CertifyTest.init(testing.allocator);
    defer f.deinit();

    const nested_pair = try f.layouts.putStructFields(&[_]layout_mod.StructField{
        .{ .index = 0, .layout = f.pair_str },
    });
    const owner = try f.local(nested_pair);
    const retained_intermediate = try f.local(f.pair_str);
    const inner_field = try f.local(.str);
    const result = try f.local(.i64);

    const join_id = f.freshJoinPointId();
    const ret = try f.ret(result);
    const result_assign = try f.assignI64(result, ret);
    const use_inner_field = try f.store.addCFStmt(.{ .expect = .{
        .condition = inner_field,
        .next = result_assign,
    } });
    const release_intermediate = try f.decrefStmt(retained_intermediate, f.pair_str, use_inner_field);
    const jump = try f.store.addCFStmt(.{ .jump = .{ .target = join_id } });
    const join_stmt = try f.store.addCFStmt(.{ .join = .{
        .id = join_id,
        .params = LIR.LocalSpan.empty(),
        .body = release_intermediate,
        .remainder = jump,
    } });
    const read_inner_field = try f.store.addCFStmt(.{ .assign_ref = .{
        .target = inner_field,
        .op = .{ .field = .{ .source = retained_intermediate, .field_idx = 0 } },
        .next = join_stmt,
    } });
    const retain_intermediate = try f.increfStmt(retained_intermediate, f.pair_str, read_inner_field);
    const read_intermediate = try f.store.addCFStmt(.{ .assign_ref = .{
        .target = retained_intermediate,
        .op = .{ .field = .{ .source = owner, .field_idx = 0 } },
        .next = retain_intermediate,
    } });
    _ = try f.addProc(&.{owner}, read_intermediate, .i64);

    const sigs = [_]arc_sig.RcSig{arc_sig.RcSig.all_owned.withBorrowedParam(0)};
    try f.certifyWith(.{ .sigs = &sigs });
}

/// Builds the issue-9658 loop shape: `k` refcounted mutable locals whose
/// alias groups merge and re-split across nested loop iterations. Branch i
/// of the loop body retains x[i], releases x[i+1]'s old binding, re-aliases
/// x[i+1] onto x[i], and jumps back, so every name always carries one unit
/// and every alias class's balance equals its size—valid on every path,
/// but the distinct must-alias partitions reaching the join grow like the
/// Bell number of `k` when enumerated. The default branch releases every
/// name once and returns.
const AliasLoopInjection = enum {
    none,
    /// Branch 0 drops the release of x[1]'s old binding before re-aliasing:
    /// a per-iteration leak (the orphaned unit cannot reach the join).
    leak_on_rebind,
    /// The exit path releases x[0] twice.
    double_release_on_exit,
    /// The exit path uses x[0] after releasing every name.
    use_after_release_on_exit,
};

fn buildAliasLoop(f: *CertifyTest, comptime k: usize, injection: AliasLoopInjection) Allocator.Error!void {
    var locals: [k]LIR.LocalId = undefined;
    for (&locals) |*local| local.* = try f.local(.str);
    const cond = try f.local(.i64);
    const result = try f.local(.i64);
    const join_id = f.freshJoinPointId();

    // Exit path: release every name once, then return.
    const ret = try f.ret(result);
    var exit_head = try f.assignI64(result, ret);
    if (injection == .use_after_release_on_exit) {
        exit_head = try f.store.addCFStmt(.{ .expect = .{ .condition = locals[0], .next = exit_head } });
    }
    var index: usize = k;
    while (index > 0) {
        index -= 1;
        exit_head = try f.decrefStmt(locals[index], .str, exit_head);
    }
    if (injection == .double_release_on_exit) {
        exit_head = try f.decrefStmt(locals[0], .str, exit_head);
    }

    // Loop branches: branch i re-aliases x[i+1] onto x[i].
    var branches: [k - 1]LIR.CFSwitchBranch = undefined;
    for (&branches, 0..) |*branch, i| {
        const jump = try f.store.addCFStmt(.{ .jump = .{ .target = join_id } });
        const rebind = try f.store.addCFStmt(.{ .assign_ref = .{
            .target = locals[i + 1],
            .op = .{ .local = locals[i] },
            .next = jump,
        } });
        const release_old = if (injection == .leak_on_rebind and i == 0)
            rebind
        else
            try f.decrefStmt(locals[i + 1], .str, rebind);
        const retain = try f.increfStmt(locals[i], .str, release_old);
        branch.* = .{ .value = @intCast(i + 1), .body = retain };
    }

    const switch_stmt = try f.store.addCFStmt(.{ .switch_stmt = .{
        .cond = cond,
        .branches = try f.store.addCFSwitchBranches(&branches),
        .default_branch = exit_head,
    } });
    const first_jump = try f.store.addCFStmt(.{ .jump = .{ .target = join_id } });
    const join_stmt = try f.store.addCFStmt(.{ .join = .{
        .id = join_id,
        .params = LIR.LocalSpan.empty(),
        .body = switch_stmt,
        .remainder = first_jump,
    } });
    const cond_assign = try f.assignI64(cond, join_stmt);
    var body = cond_assign;
    index = k;
    while (index > 0) {
        index -= 1;
        body = try f.assignStr(locals[index], body);
    }
    _ = try f.addProc(&.{}, body, .i64);
}

test "certify converges on issue-9658 alias-merging loop over 6 locals" {
    var f = try CertifyTest.init(testing.allocator);
    defer f.deinit();
    try buildAliasLoop(&f, 6, .none);
    try f.certify();
}

test "certify converges on a join-heavy alias-merging loop over 12 locals" {
    // Enumerating distinct entry summaries here would need on the order of
    // Bell(12) = 4.2 million body walks (the old certifier gave up at
    // 4096 and left the proc unverified); the lattice join must converge in
    // a handful of walks.
    // There is no explicit timing assertion: enumeration at this size does
    // not finish in any tolerable test budget, so completing at all is the
    // bound.
    var f = try CertifyTest.init(testing.allocator);
    defer f.deinit();
    try buildAliasLoop(&f, 12, .none);
    try f.certify();
}

test "certify flags a per-iteration leak inside a state-complex loop" {
    // The shape the old capacity cap could have skipped: the leak is only
    // reachable through the alias-merging loop.
    var f = try CertifyTest.init(testing.allocator);
    defer f.deinit();
    try buildAliasLoop(&f, 6, .leak_on_rebind);
    try testing.expectError(error.Certification, f.certify());
    try testing.expect(std.mem.find(u8, f.diag.message(), "not carried into join") != null);
}

test "certify flags a double release inside a state-complex loop" {
    var f = try CertifyTest.init(testing.allocator);
    defer f.deinit();
    try buildAliasLoop(&f, 6, .double_release_on_exit);
    try testing.expectError(error.Certification, f.certify());
    try testing.expect(std.mem.find(u8, f.diag.message(), "without an ownership unit") != null);
}

test "certify flags a use after release inside a state-complex loop" {
    var f = try CertifyTest.init(testing.allocator);
    defer f.deinit();
    try buildAliasLoop(&f, 6, .use_after_release_on_exit);
    try testing.expectError(error.Certification, f.certify());
    try testing.expect(std.mem.find(u8, f.diag.message(), "dead refcounted local") != null);
}

test "certify joins entries whose partitions differ but balances agree" {
    // Jump A reaches the join with x and y as separate values carrying one
    // unit each; jump B re-aliases y onto x with the shared value carrying
    // two units. The shared body releases through each name once—valid on
    // both edges. The lattice join must absorb both into one group (meet
    // partition = singletons, balances attributed 2 = 1 + 1) and certify
    // with a single body walk.
    var f = try CertifyTest.init(testing.allocator);
    defer f.deinit();
    const x = try f.local(.str);
    const y = try f.local(.str);
    const cond = try f.local(.i64);
    const result = try f.local(.i64);
    const join_id = f.freshJoinPointId();

    const ret = try f.ret(result);
    const result_assign = try f.assignI64(result, ret);
    const release_y = try f.decrefStmt(y, .str, result_assign);
    const release_x = try f.decrefStmt(x, .str, release_y);

    const jump_separate = try f.store.addCFStmt(.{ .jump = .{ .target = join_id } });
    const jump_aliased = try f.store.addCFStmt(.{ .jump = .{ .target = join_id } });
    const rebind = try f.store.addCFStmt(.{ .assign_ref = .{
        .target = y,
        .op = .{ .local = x },
        .next = jump_aliased,
    } });
    const release_old_y = try f.decrefStmt(y, .str, rebind);
    const retain_x = try f.increfStmt(x, .str, release_old_y);

    const switch_stmt = try f.store.addCFStmt(.{ .switch_stmt = .{
        .cond = cond,
        .branches = try f.store.addCFSwitchBranches(&[_]LIR.CFSwitchBranch{
            .{ .value = 1, .body = jump_separate },
        }),
        .default_branch = retain_x,
    } });
    const join_stmt = try f.store.addCFStmt(.{ .join = .{
        .id = join_id,
        .params = LIR.LocalSpan.empty(),
        .body = release_x,
        .remainder = switch_stmt,
    } });
    const cond_assign = try f.assignI64(cond, join_stmt);
    const assign_y = try f.assignStr(y, cond_assign);
    const body = try f.assignStr(x, assign_y);
    _ = try f.addProc(&.{}, body, .i64);
    try f.certify();
}

test "certify flags unbounded per-iteration balance accumulation" {
    // A loop whose only effect is retaining one more unit per iteration,
    // with no exit: entry balances grow 1, 2, 3, ... forever. The old
    // certifier enumerated these summaries until its capacity cap and then
    // skipped the whole procedure; the fixpoint reports the growth itself
    // as a finding, because mode- and partition-identical entries with
    // diverging balances can never all certify against one shared body.
    var f = try CertifyTest.init(testing.allocator);
    defer f.deinit();
    const value = try f.local(.str);
    const join_id = f.freshJoinPointId();

    const jump_back = try f.store.addCFStmt(.{ .jump = .{ .target = join_id } });
    const retain = try f.increfStmt(value, .str, jump_back);
    const first_jump = try f.store.addCFStmt(.{ .jump = .{ .target = join_id } });
    const join_stmt = try f.store.addCFStmt(.{ .join = .{
        .id = join_id,
        .params = LIR.LocalSpan.empty(),
        .body = retain,
        .remainder = first_jump,
    } });
    const body = try f.assignStr(value, join_stmt);
    _ = try f.addProc(&.{}, body, .i64);
    try testing.expectError(error.Certification, f.certify());
    try testing.expect(std.mem.find(u8, f.diag.message(), "accumulation") != null);
}

fn fieldReadStmt(f: *CertifyTest, target: LIR.LocalId, source: LIR.LocalId, field_idx: u16, next: LIR.CFStmtId) Allocator.Error!LIR.CFStmtId {
    return try f.store.addCFStmt(.{ .assign_ref = .{
        .target = target,
        .op = .{ .field = .{ .source = source, .field_idx = field_idx } },
        .next = next,
    } });
}

fn tagPayloadStructReadStmt(
    f: *CertifyTest,
    target: LIR.LocalId,
    source: LIR.LocalId,
    variant_index: u16,
    next: LIR.CFStmtId,
) Allocator.Error!LIR.CFStmtId {
    return try f.store.addCFStmt(.{ .assign_ref = .{
        .target = target,
        .op = .{ .tag_payload_struct = .{
            .source = source,
            .variant_index = variant_index,
            .tag_discriminant = variant_index,
        } },
        .next = next,
    } });
}

test "certify carries a released struct representation across a join for scalar field reads" {
    var f = try CertifyTest.init(testing.allocator);
    defer f.deinit();
    const record_layout = try f.layouts.putStructFields(&[_]layout_mod.StructField{
        .{ .index = 0, .layout = .str },
        .{ .index = 1, .layout = .i64 },
    });
    const record = try f.local(record_layout);
    const alias = try f.local(record_layout);
    const scalar = try f.local(.i64);
    const join_id = f.freshJoinPointId();

    const ret = try f.ret(scalar);
    const read_scalar = try fieldReadStmt(&f, scalar, alias, 1, ret);
    const alias_shell = try f.store.addCFStmt(.{ .assign_ref = .{
        .target = alias,
        .op = .{ .local = record },
        .residual_shell_absent_fields = try f.store.addU32Span(&.{0}),
        .next = read_scalar,
    } });
    const jump = try f.store.addCFStmt(.{ .jump = .{ .target = join_id } });
    const release = try f.decrefStmt(record, record_layout, jump);
    const body = try f.store.addCFStmt(.{ .join = .{
        .id = join_id,
        .params = LIR.LocalSpan.empty(),
        .body = alias_shell,
        .remainder = release,
    } });
    _ = try f.addProc(&.{record}, body, .i64);
    try f.certify();
}

test "certify rejects a released struct alias without exact residual-shell fields" {
    var f = try CertifyTest.init(testing.allocator);
    defer f.deinit();
    const record_layout = try f.layouts.putStructFields(&[_]layout_mod.StructField{
        .{ .index = 0, .layout = .str },
        .{ .index = 1, .layout = .i64 },
    });
    const record = try f.local(record_layout);
    const alias = try f.local(record_layout);
    const scalar = try f.local(.i64);

    const ret = try f.ret(scalar);
    const read_scalar = try fieldReadStmt(&f, scalar, alias, 1, ret);
    const malformed_alias = try f.store.addCFStmt(.{ .assign_ref = .{
        .target = alias,
        .op = .{ .local = record },
        .next = read_scalar,
    } });
    const release = try f.decrefStmt(record, record_layout, malformed_alias);
    _ = try f.addProc(&.{record}, release, .i64);

    try testing.expectError(error.Certification, f.certify());
    try testing.expect(std.mem.find(u8, f.diag.message(), "missing exact residual-shell metadata") != null);
}

test "certify accepts shell fields transferred before their lazy claims settle" {
    var f = try CertifyTest.init(testing.allocator);
    defer f.deinit();
    const record_layout = try f.layouts.putStructFields(&[_]layout_mod.StructField{
        .{ .index = 0, .layout = .str },
        .{ .index = 1, .layout = .i64 },
    });
    const record = try f.local(record_layout);
    const alias = try f.local(record_layout);
    const field = try f.local(.str);
    const scalar = try f.local(.i64);

    const ret = try f.ret(scalar);
    const release_field = try f.decrefStmt(field, .str, ret);
    const read_scalar = try fieldReadStmt(&f, scalar, alias, 1, release_field);
    const alias_shell = try f.store.addCFStmt(.{ .assign_ref = .{
        .target = alias,
        .op = .{ .local = record },
        .residual_shell_absent_fields = try f.store.addU32Span(&.{0}),
        .next = read_scalar,
    } });
    const body = try fieldReadStmt(&f, field, record, 0, alias_shell);
    _ = try f.addProc(&.{record}, body, .i64);

    try f.certify();
}

test "certify rejects an RC field read through a released struct representation" {
    var f = try CertifyTest.init(testing.allocator);
    defer f.deinit();
    const record_layout = try f.layouts.putStructFields(&[_]layout_mod.StructField{
        .{ .index = 0, .layout = .str },
        .{ .index = 1, .layout = .i64 },
    });
    const record = try f.local(record_layout);
    const field = try f.local(.str);
    const result = try f.local(.i64);
    const ret = try f.ret(result);
    const assign_result = try f.assignI64(result, ret);
    const read_field = try fieldReadStmt(&f, field, record, 0, assign_result);
    const body = try f.decrefStmt(record, record_layout, read_field);
    _ = try f.addProc(&.{record}, body, .i64);
    try testing.expectError(error.Certification, f.certify());
    try testing.expect(std.mem.find(u8, f.diag.message(), "dead refcounted local") != null);
}

test "certify accepts a retained record moved whole beside a take of its field" {
    // ARC lowers `{ level, world: World.new(level) }` as an incref of the
    // whole record, a take of the field the sibling needs, and residual
    // releases of the rest. The claims describe the dismantled unit only, so
    // the retained surplus unit is still intact and may move into the holder.
    var f = try CertifyTest.init(testing.allocator);
    defer f.deinit();
    const holder_layout = try f.layouts.putStructFields(&[_]layout_mod.StructField{
        .{ .index = 0, .layout = .str },
        .{ .index = 1, .layout = f.pair_str },
    });
    const pair = try f.local(f.pair_str);
    const taken = try f.local(.str);
    const dropped = try f.local(.str);
    const holder = try f.local(holder_layout);

    const ret = try f.ret(holder);
    const holder_assign = try f.store.addCFStmt(.{ .assign_struct = .{
        .target = holder,
        .fields = try f.store.addLocalSpan(&.{ taken, pair }),
        .next = ret,
    } });
    const release_dropped = try f.decrefStmt(dropped, .str, holder_assign);
    const read_dropped = try fieldReadStmt(&f, dropped, pair, 1, release_dropped);
    const read_taken = try fieldReadStmt(&f, taken, pair, 0, read_dropped);
    const body = try f.increfStmt(pair, f.pair_str, read_taken);
    _ = try f.addProc(&.{pair}, body, holder_layout);
    try f.certify();
}

test "certify rejects a dismantled record moved whole without a retained surplus" {
    // The same shape without the incref has one unit only: the field take and
    // the residual release already spent it, so moving the record whole would
    // hand out ownership twice.
    var f = try CertifyTest.init(testing.allocator);
    defer f.deinit();
    const holder_layout = try f.layouts.putStructFields(&[_]layout_mod.StructField{
        .{ .index = 0, .layout = .str },
        .{ .index = 1, .layout = f.pair_str },
    });
    const pair = try f.local(f.pair_str);
    const taken = try f.local(.str);
    const dropped = try f.local(.str);
    const holder = try f.local(holder_layout);

    const ret = try f.ret(holder);
    const holder_assign = try f.store.addCFStmt(.{ .assign_struct = .{
        .target = holder,
        .fields = try f.store.addLocalSpan(&.{ taken, pair }),
        .next = ret,
    } });
    const release_dropped = try f.decrefStmt(dropped, .str, holder_assign);
    const read_dropped = try fieldReadStmt(&f, dropped, pair, 1, release_dropped);
    const body = try fieldReadStmt(&f, taken, pair, 0, read_dropped);
    _ = try f.addProc(&.{pair}, body, holder_layout);

    try testing.expectError(error.Certification, f.certify());
    try testing.expect(std.mem.find(u8, f.diag.message(), "partially dismantled") != null);
}

test "certify accepts a fully dismantled record via field takes" {
    // Both refcounted fields of a dying owned pair are read without retains
    // and released; each release claims the pair's stored unit for its
    // field, and the fully claimed pair needs no whole release.
    var f = try CertifyTest.init(testing.allocator);
    defer f.deinit();
    const pair = try f.local(f.pair_str);
    const first = try f.local(.str);
    const second = try f.local(.str);
    const result = try f.local(.i64);
    const ret = try f.ret(result);
    const result_assign = try f.assignI64(result, ret);
    const release_second = try f.decrefStmt(second, .str, result_assign);
    const release_first = try f.decrefStmt(first, .str, release_second);
    const read_second = try fieldReadStmt(&f, second, pair, 1, release_first);
    const body = try fieldReadStmt(&f, first, pair, 0, read_second);
    _ = try f.addProc(&.{pair}, body, .i64);
    try f.certify();
}

test "certify flags a whole release of a partially dismantled record" {
    var f = try CertifyTest.init(testing.allocator);
    defer f.deinit();
    const pair = try f.local(f.pair_str);
    const first = try f.local(.str);
    const result = try f.local(.i64);
    const ret = try f.ret(result);
    const result_assign = try f.assignI64(result, ret);
    const release_pair = try f.decrefStmt(pair, f.pair_str, result_assign);
    const release_first = try f.decrefStmt(first, .str, release_pair);
    const body = try fieldReadStmt(&f, first, pair, 0, release_first);
    _ = try f.addProc(&.{pair}, body, .i64);
    try testing.expectError(error.Certification, f.certify());
    try testing.expect(std.mem.find(u8, f.diag.message(), "partially dismantled") != null);
}

test "certify flags a dismantle that leaves a stored unit unspent" {
    // Only one of the pair's two refcounted fields is taken and the pair is
    // never released whole: the other field's stored unit leaks.
    var f = try CertifyTest.init(testing.allocator);
    defer f.deinit();
    const pair = try f.local(f.pair_str);
    const first = try f.local(.str);
    const result = try f.local(.i64);
    const ret = try f.ret(result);
    const result_assign = try f.assignI64(result, ret);
    const release_first = try f.decrefStmt(first, .str, result_assign);
    const body = try fieldReadStmt(&f, first, pair, 0, release_first);
    _ = try f.addProc(&.{pair}, body, .i64);
    try testing.expectError(error.Certification, f.certify());
    try testing.expect(std.mem.find(u8, f.diag.message(), "unspent") != null);
}

test "certify flags a double take of one field" {
    var f = try CertifyTest.init(testing.allocator);
    defer f.deinit();
    const pair = try f.local(f.pair_str);
    const first = try f.local(.str);
    const again = try f.local(.str);
    const result = try f.local(.i64);
    const ret = try f.ret(result);
    const result_assign = try f.assignI64(result, ret);
    const release_again = try f.decrefStmt(again, .str, result_assign);
    const release_first = try f.decrefStmt(first, .str, release_again);
    const read_again = try fieldReadStmt(&f, again, pair, 0, release_first);
    const body = try fieldReadStmt(&f, first, pair, 0, read_again);
    _ = try f.addProc(&.{pair}, body, .i64);
    try testing.expectError(error.Certification, f.certify());
}

test "certify accepts a take consumed by an owned call argument" {
    // The taken field's unit leaves through a call instead of a release; the
    // residual other field is released at the pair's death point.
    var f = try CertifyTest.init(testing.allocator);
    defer f.deinit();
    const pair = try f.local(f.pair_str);
    const taken = try f.local(.str);
    const residual = try f.local(.str);
    const callee_arg = try f.local(.str);
    const callee_result = try f.local(.i64);
    const callee_ret = try f.ret(callee_result);
    const callee_result_assign = try f.assignI64(callee_result, callee_ret);
    const callee_release = try f.decrefStmt(callee_arg, .str, callee_result_assign);
    const callee = try f.addProc(&.{callee_arg}, callee_release, .i64);

    const result = try f.local(.i64);
    const ret = try f.ret(result);
    const call = try f.store.addCFStmt(.{ .assign_call = .{
        .target = result,
        .proc = callee,
        .args = try f.store.addLocalSpan(&.{taken}),
        .next = ret,
    } });
    const release_residual = try f.decrefStmt(residual, .str, call);
    const read_residual = try fieldReadStmt(&f, residual, pair, 1, release_residual);
    const body = try fieldReadStmt(&f, taken, pair, 0, read_residual);
    _ = try f.addProc(&.{pair}, body, .i64);
    try f.certify();
}

test "certify accepts moving an active tag variant's complete RC payload" {
    var f = try CertifyTest.init(testing.allocator);
    defer f.deinit();
    const tag_pair = try f.layouts.putTagUnion(&[_]layout_mod.Idx{
        try f.layouts.ensureZstLayout(),
        f.pair_str,
    });

    const callee_arg = try f.local(f.pair_str);
    const callee_result = try f.local(.i64);
    const callee_ret = try f.ret(callee_result);
    const callee_result_assign = try f.assignI64(callee_result, callee_ret);
    const callee_release = try f.decrefStmt(callee_arg, f.pair_str, callee_result_assign);
    const callee = try f.addProc(&.{callee_arg}, callee_release, .i64);

    const tag = try f.local(tag_pair);
    const payload = try f.local(f.pair_str);
    const result = try f.local(.i64);
    const ret = try f.ret(result);
    const call = try f.store.addCFStmt(.{ .assign_call = .{
        .target = result,
        .proc = callee,
        .args = try f.store.addLocalSpan(&.{payload}),
        .next = ret,
    } });
    const body = try tagPayloadStructReadStmt(&f, payload, tag, 1, call);
    _ = try f.addProc(&.{tag}, body, .i64);
    try f.certify();
}

test "certify carries a complete tag payload's unit through a join cell" {
    var f = try CertifyTest.init(testing.allocator);
    defer f.deinit();
    const tag_pair = try f.layouts.putTagUnion(&[_]layout_mod.Idx{
        try f.layouts.ensureZstLayout(),
        f.pair_str,
    });

    const tag = try f.local(tag_pair);
    const payload = try f.local(f.pair_str);
    const join_payload = try f.local(f.pair_str);
    const result = try f.local(.i64);
    const join_id = f.freshJoinPointId();

    const ret = try f.ret(result);
    const result_assign = try f.assignI64(result, ret);
    const release_payload = try f.decrefStmt(join_payload, f.pair_str, result_assign);
    const jump = try f.store.addCFStmt(.{ .jump = .{ .target = join_id } });
    const set_payload = try f.store.addCFStmt(.{ .set_local = .{
        .target = join_payload,
        .value = payload,
        .mode = .initialize_join_param,
        .next = jump,
    } });
    const payload_read = try tagPayloadStructReadStmt(&f, payload, tag, 1, set_payload);
    const join_stmt = try f.store.addCFStmt(.{ .join = .{
        .id = join_id,
        .params = try f.store.addLocalSpan(&.{join_payload}),
        .body = release_payload,
        .remainder = payload_read,
    } });
    _ = try f.addProc(&.{tag}, join_stmt, .i64);
    try f.certify();
}

test "certify rejects moving only part of an active tag payload" {
    var f = try CertifyTest.init(testing.allocator);
    defer f.deinit();
    const tag_pair = try f.layouts.putTagUnion(&[_]layout_mod.Idx{
        try f.layouts.ensureZstLayout(),
        f.pair_str,
    });

    const tag = try f.local(tag_pair);
    const partial = try f.local(.str);
    const result = try f.local(.i64);
    const ret = try f.ret(result);
    const result_assign = try f.assignI64(result, ret);
    const release_partial = try f.decrefStmt(partial, .str, result_assign);
    const body = try f.store.addCFStmt(.{ .assign_ref = .{
        .target = partial,
        .op = .{ .tag_payload = .{
            .source = tag,
            .payload_idx = 0,
            .variant_index = 1,
            .tag_discriminant = 1,
        } },
        .next = release_partial,
    } });
    _ = try f.addProc(&.{tag}, body, .i64);
    try testing.expectError(error.Certification, f.certify());
    try testing.expect(std.mem.find(u8, f.diag.message(), "without an ownership unit") != null);
}

test "certify rejects an outcome-specialized return without an exact discriminant witness" {
    var f = try CertifyTest.init(testing.allocator);
    defer f.deinit();

    const result_layout = try f.layouts.putTagUnion(&[_]layout_mod.Idx{
        try f.layouts.ensureZstLayout(),
        .str,
    });
    const restitutable = try f.local(.str);
    const result = try f.local(result_layout);
    const body = try f.ret(result);
    _ = try f.addProc(&.{ restitutable, result }, body, result_layout);

    const sigs = [_]arc_sig.RcSig{.{
        .outcomes = .{ .start = 0, .len = 1 },
    }};
    const outcomes = [_]arc_sig.Outcome{.{
        .discriminant = 0,
        .restituted_params = arc_sig.paramBit(0).?,
    }};

    try testing.expectError(error.Certification, f.certifyWith(.{
        .sigs = &sigs,
        .outcomes = &outcomes,
    }));
    try testing.expect(std.mem.find(
        u8,
        f.diag.message(),
        "outcome-specialized return lacked an exact current result discriminant witness",
    ) != null);
}

test "certify rejects consuming Box.unbox after the ARC boundary" {
    var f = try CertifyTest.init(testing.allocator);
    defer f.deinit();

    const box_str = try f.layouts.insertBox(.str);
    const boxed = try f.local(box_str);
    const payload = try f.local(.str);
    const ret = try f.ret(payload);
    const body = try f.store.addCFStmt(.{ .assign_low_level = .{
        .target = payload,
        .op = .box_unbox,
        .rc_effect = LIR.LowLevel.box_unbox.rcEffect(),
        .args = try f.store.addLocalSpan(&.{boxed}),
        .next = ret,
    } });
    _ = try f.addProc(&.{boxed}, body, .str);

    try testing.expectError(error.Certification, f.certify());
    try testing.expect(std.mem.find(u8, f.diag.message(), "post-ARC LIR retained a consuming Box.unbox") != null);
}
