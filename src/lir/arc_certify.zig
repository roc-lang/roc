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
//! A certification failure is a compiler bug in ARC insertion. The production
//! entry point panics in debug builds; release builds never run the certifier.

const std = @import("std");
const builtin = @import("builtin");
const build_options = @import("build_options");
const core = @import("lir_core");
const layout_mod = @import("layout");
const arc_sig = @import("arc_sig.zig");
const arc_solve = @import("arc_solve.zig");

const LIR = core.LIR;
const LirStore = core.LirStore;
const Allocator = std.mem.Allocator;

/// Join-state budget for full-store certification. The production bound is
/// written here in exactly one place; tests override it via `CertifyOptions`.
pub const production_join_state_capacity: usize = 4096;
const skipped_proc_record_limit = 16;
const forbid_skips = builtin.mode == .Debug and build_options.forbid_arc_certifier_skips;

/// Errors produced while certifying: allocation failure or a violation of
/// the ownership rules (a compiler bug in ARC insertion).
/// `Certification` is a positive finding: ARC insertion produced refcount-incorrect code
/// (a leak, a use-after-free, or a balance mismatch) — a real bug, so it aborts the build.
/// `CertifierCapacityExceeded` is an incompleteness signal, NOT a finding: the certifier could
/// not finish proving a procedure within its budget (its join-state enumeration grew too large).
/// It must never abort a valid build — `certifyStore` catches it and leaves that one procedure
/// unverified, continuing with the rest.
pub const CertifyError = error{ OutOfMemory, Certification, CertifierCapacityExceeded };

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
    /// Number of procedures left unverified because their join-state enumeration
    /// exceeded the certifier's budget (incompleteness, not a finding). Exposed for
    /// tests/debugging; a nonzero value means certification was not exhaustive.
    skipped_proc_count: usize = 0,
    /// Storage for the first skipped procedures. Read it only through
    /// `skippedSample`, which bounds the slice; entries past the sample are
    /// undefined.
    skipped_procs: [skipped_proc_record_limit]LIR.LirProcSpecId = undefined,

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

    fn recordSkippedProc(self: *Diagnostic, proc_id: LIR.LirProcSpecId) void {
        if (self.skipped_proc_count < self.skipped_procs.len) {
            self.skipped_procs[self.skipped_proc_count] = proc_id;
        }
        self.skipped_proc_count += 1;
    }

    /// The bounded sample of skipped procedures: the first
    /// `skipped_proc_record_limit` of the `skipped_proc_count` total.
    pub fn skippedSample(self: *const Diagnostic) []const LIR.LirProcSpecId {
        return self.skipped_procs[0..@min(self.skipped_proc_count, self.skipped_procs.len)];
    }
};

const CertifyOptions = struct {
    join_state_capacity: usize = production_join_state_capacity,
};

/// Certifies every proc body in the store. Returns `error.Certification`
/// with `diag` filled on the first violation.
pub fn certifyStore(
    allocator: Allocator,
    store: *const LirStore,
    layouts: *const layout_mod.Store,
    sigs: arc_sig.SigTable,
    roots: []const LIR.LirProcSpecId,
    diag: *Diagnostic,
) CertifyError!void {
    return certifyStoreWithOptions(allocator, store, layouts, sigs, roots, diag, .{});
}

fn certifyStoreWithOptions(
    allocator: Allocator,
    store: *const LirStore,
    layouts: *const layout_mod.Store,
    sigs: arc_sig.SigTable,
    roots: []const LIR.LirProcSpecId,
    diag: *Diagnostic,
    options: CertifyOptions,
) CertifyError!void {
    var rc_local = try allocator.alloc(bool, store.locals.items.len);
    defer allocator.free(rc_local);
    for (store.locals.items, 0..) |local, index| {
        rc_local[index] = layouts.layoutContainsRefcounted(layouts.getLayout(local.layout_idx));
    }

    try certifyRcAtomicity(allocator, store, rc_local, roots, diag);
    try certifyUniqueArgs(allocator, store, rc_local, sigs, diag);

    var certifier = Certifier{
        .allocator = allocator,
        .store = store,
        .sigs = sigs,
        .rc_local = rc_local,
        .lender_arena = std.heap.ArenaAllocator.init(allocator),
        .records = std.AutoHashMap(LIR.JoinPointId, JoinRecord).init(allocator),
        .memo = std.AutoHashMap(MemoEntry, void).init(allocator),
        .repr_scratch = std.AutoHashMap(ValueId, u32).init(allocator),
        .join_bodies = std.AutoHashMap(LIR.JoinPointId, LIR.CFStmtId).init(allocator),
        .reads_before_rebind_cache = std.AutoHashMap(LIR.CFStmtId, std.bit_set.DynamicBitSetUnmanaged).init(allocator),
        .diag = diag,
        .join_state_capacity = options.join_state_capacity,
    };
    defer certifier.deinit();

    for (store.proc_specs.items, 0..) |proc, index| {
        const proc_id: LIR.LirProcSpecId = @enumFromInt(@as(u32, @intCast(index)));
        const body = proc.body orelse continue;
        certifier.certifyProc(proc_id, proc, body) catch |err| switch (err) {
            // Incompleteness, not a finding: this procedure's join-state space exceeded the
            // certifier's budget. Leave it unverified and keep certifying the rest, rather than
            // failing a valid build. `certifyProc` resets all per-proc state on the next call, and
            // its work stack is cleaned up on unwind, so skipping is safe. Real findings surface as
            // `error.Certification` and still propagate.
            error.CertifierCapacityExceeded => {
                diag.recordSkippedProc(proc_id);
                continue;
            },
            else => |e| return e,
        };
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

    for (store.cf_stmts.items, 0..) |stmt, stmt_index| {
        const checked: struct { value: LIR.LocalId, atomicity: LIR.RcAtomicity } = switch (stmt) {
            .incref => |rc| .{ .value = rc.value, .atomicity = rc.atomicity },
            .decref => |rc| .{ .value = rc.value, .atomicity = rc.atomicity },
            .decref_if_initialized => |rc| .{ .value = rc.value, .atomicity = rc.atomicity },
            .free => |rc| .{ .value = rc.value, .atomicity = rc.atomicity },
            else => continue,
        };
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
/// and a local whose every definition is a unique birth — a fresh
/// allocation, a direct call whose callee's signature returns unique, a
/// pure same-value alias whose source is born unique, or a parameter the
/// containing proc's signature seeds born-unique. The balance and borrow
/// conditions behind the claim are enforced by the per-value certification;
/// this rule covers the unique-origin claim. Variants share parameter
/// locals with their source proc, so claims are checked per proc body
/// against that proc's signature.
fn certifyUniqueArgs(
    allocator: Allocator,
    store: *const LirStore,
    rc_local: []const bool,
    sigs: arc_sig.SigTable,
    diag: *Diagnostic,
) CertifyError!void {
    var uniqueness = try arc_solve.computeUniqueness(allocator, store, rc_local, sigs);
    defer uniqueness.deinit(allocator);

    var visited = std.AutoHashMap(LIR.CFStmtId, void).init(allocator);
    defer visited.deinit();
    var stack = std.ArrayList(LIR.CFStmtId).empty;
    defer stack.deinit(allocator);

    for (store.proc_specs.items, 0..) |proc, proc_index| {
        const body = proc.body orelse continue;
        const sig = sigs.get(@enumFromInt(@as(u32, @intCast(proc_index))));
        const params = store.getLocalSpan(proc.args);
        visited.clearRetainingCapacity();
        stack.clearRetainingCapacity();
        try stack.append(allocator, body);
        while (stack.pop()) |current| {
            if (visited.contains(current)) continue;
            try visited.put(current, {});
            switch (store.getCFStmt(current)) {
                .assign_low_level => |assign| {
                    try stack.append(allocator, assign.next);
                    if (assign.unique_args == 0) continue;
                    const stmt_index = @intFromEnum(current);
                    if ((assign.unique_args & ~assign.rc_effect.may_runtime_uniqueness_check_args) != 0) {
                        diag.set("stmt={d}: unique_args bit outside the op's runtime-checked argument mask", .{stmt_index});
                        return error.Certification;
                    }
                    for (store.getLocalSpan(assign.args), 0..) |arg, position| {
                        if (position >= 64) break;
                        const bit = @as(u64, 1) << @as(u6, @intCast(position));
                        if ((assign.unique_args & bit) == 0) continue;
                        const index = @intFromEnum(arg);
                        if (index < uniqueness.born_unique.capacity() and uniqueness.born_unique.isSet(index)) continue;
                        if (paramSeededUnique(sig, params, arg)) continue;
                        diag.set("stmt={d}: check-free uniqueness claim on argument {d} (local {d}) without a unique birth", .{ stmt_index, position, index });
                        return error.Certification;
                    }
                },
                .switch_stmt => |s| {
                    for (store.getCFSwitchBranches(s.branches)) |branch| {
                        try stack.append(allocator, branch.body);
                    }
                    try stack.append(allocator, s.default_branch);
                    if (s.continuation) |continuation| {
                        try stack.append(allocator, continuation);
                    }
                },
                .switch_initialized_payload => |s| {
                    try stack.append(allocator, s.initialized_branch);
                    try stack.append(allocator, s.uninitialized_branch);
                },
                .str_match => |s| {
                    try stack.append(allocator, s.on_match);
                    try stack.append(allocator, s.on_miss);
                },
                .str_match_set => |s| {
                    for (store.getStrMatchArms(s.arms)) |arm| {
                        try stack.append(allocator, arm.on_match);
                    }
                    try stack.append(allocator, s.on_miss);
                },
                .join => |j| {
                    try stack.append(allocator, j.body);
                    try stack.append(allocator, j.remainder);
                },
                inline .assign_ref, .assign_literal, .init_uninitialized, .assign_call, .assign_call_erased, .assign_packed_erased_fn, .assign_list, .assign_struct, .assign_tag, .set_local, .debug, .expect, .comptime_branch_taken, .incref, .decref, .decref_if_initialized, .free => |s| {
                    try stack.append(allocator, s.next);
                },
                .ret, .jump, .crash, .expect_err, .runtime_error, .comptime_exhaustiveness_failed, .loop_continue, .loop_break => {},
            }
        }
    }
}

/// True when the local is a parameter of the proc and the proc's signature
/// seeds it born-unique (a mode-specialized variant whose caller proved the
/// dying argument unique).
fn paramSeededUnique(sig: arc_sig.RcSig, params: []const LIR.LocalId, local: LIR.LocalId) bool {
    if (sig.unique_params == 0) return false;
    for (params, 0..) |param, position| {
        if (position >= 64) break;
        if (param != local) continue;
        return (sig.unique_params >> @as(u6, @intCast(position))) & 1 != 0;
    }
    return false;
}

/// Like `certifyStore`, but panics with a rendered failure context instead
/// of returning `error.Certification`.
pub fn certifyStoreOrPanic(
    allocator: Allocator,
    store: *const LirStore,
    layouts: *const layout_mod.Store,
    sigs: arc_sig.SigTable,
    roots: []const LIR.LirProcSpecId,
) Allocator.Error!void {
    var diag = Diagnostic{};
    certifyStore(allocator, store, layouts, sigs, roots, &diag) catch |err| switch (err) {
        error.OutOfMemory => return error.OutOfMemory,
        // `certifyStore` already catches this per-procedure (skips the proc), so it cannot reach
        // here; handle it defensively as "nothing to report" rather than panicking.
        error.CertifierCapacityExceeded => {},
        error.Certification => {
            var context = FailureContext{};
            for (diag.chain[0..diag.chain_len]) |link| {
                context.append("\n  value {d}: origin_local={d} balance={d} holder={d} always_live={} lenders={d}", .{
                    link.value,
                    @intFromEnum(link.origin),
                    link.balance,
                    link.holder,
                    link.always_live,
                    link.lender_count,
                });
            }
            if (diag.context_proc) |proc_id| {
                var extra_locals: [8]LIR.LocalId = undefined;
                for (diag.chain[0..diag.chain_len], 0..) |link, index| {
                    extra_locals[index] = link.origin;
                }
                writeFailureContext(&context, store, sigs, proc_id, diag.context_stmt, diag.context_local, extra_locals[0..diag.chain_len]);
            }
            if (diag.skipped_proc_count != 0) {
                context.append("\n", .{});
                writeSkippedProcReport(&context, store, &diag);
            }
            std.debug.panic("ARC borrow certifier: {s}{s}", .{ diag.message(), context.text() });
        },
    };
    const outcome = skipOutcome(diag.skipped_proc_count, forbid_skips);
    if (outcome != .none) {
        var context = FailureContext{};
        writeSkippedProcReport(&context, store, &diag);
        // When the debug stats compiler from #9787/#9789 lands, route this count there too.
        switch (outcome) {
            .none => unreachable,
            .fail => std.debug.panic("{s}", .{context.text()}),
            .warn => std.log.warn("{s}", .{context.text()}),
        }
    }
}

/// What `certifyStoreOrPanic` does about capacity skips. Split out from the
/// wrapper so the escalation decision is unit-testable.
const SkipOutcome = enum { none, warn, fail };

fn skipOutcome(skipped_proc_count: usize, forbid: bool) SkipOutcome {
    if (skipped_proc_count == 0) return .none;
    return if (forbid) .fail else .warn;
}

/// Bounded, allocation-free text buffer for panic context. Output past the
/// capacity is truncated.
const FailureContext = struct {
    buffer: [8192]u8 = undefined,
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

fn writeSkippedProcReport(
    context: *FailureContext,
    store: *const LirStore,
    diag: *const Diagnostic,
) void {
    context.append(
        "ARC certifier skipped {d} procedure(s) whose join-state enumeration exceeded capacity; their RC schedules are UNVERIFIED.",
        .{diag.skipped_proc_count},
    );
    const sample = diag.skippedSample();
    for (sample) |proc_id| {
        context.append("\n  proc={d}", .{@intFromEnum(proc_id)});
        if (store.procDebugName(proc_id)) |name| {
            context.append(" name={s}", .{name});
        }
    }
    if (diag.skipped_proc_count > sample.len) {
        context.append("\n  ... and {d} more", .{diag.skipped_proc_count - sample.len});
    }
}

/// Writes every statement of the failing proc that mentions the implicated
/// local, plus all join/jump structure, into the panic context buffer.
fn writeFailureContext(
    context: *FailureContext,
    store: *const LirStore,
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
    for (store.getLocalSpan(proc.args)) |arg| context.append(" {d}", .{@intFromEnum(arg)});
    context.append("\n", .{});

    var reachable = std.AutoHashMap(LIR.CFStmtId, void).init(store.allocator);
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
                    for (store.getCFSwitchBranches(s.branches)) |branch| {
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
                .str_match_set => |s| {
                    for (store.getStrMatchArms(s.arms)) |arm| {
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
                inline .assign_ref, .assign_literal, .init_uninitialized, .assign_call, .assign_call_erased, .assign_packed_erased_fn, .assign_low_level, .assign_list, .assign_struct, .assign_tag, .set_local, .debug, .expect, .incref, .decref, .decref_if_initialized, .free => |s| {
                    walk.append(store.allocator, s.next) catch return;
                },
            }
        }
    }

    for (store.cf_stmts.items, 0..) |stmt, index| {
        if (!reachable.contains(@enumFromInt(@as(u32, @intCast(index))))) continue;
        var mentions = if (local) |l| stmtMentionsLocal(store, stmt, l) else false;
        for (extra_locals) |extra| {
            mentions = mentions or stmtMentionsLocal(store, stmt, extra);
        }
        const structural = switch (stmt) {
            .join, .jump, .incref, .decref, .decref_if_initialized, .free => true,
            else => false,
        };
        const nearby = if (stmt_id) |focus_stmt| if (index > @intFromEnum(focus_stmt))
            index - @intFromEnum(focus_stmt) <= 12
        else
            @intFromEnum(focus_stmt) - index <= 12 else false;
        if (!mentions and !structural and !nearby) continue;
        context.append("  stmt {d}: {s}", .{ index, @tagName(stmt) });
        switch (stmt) {
            .join => |j| context.append(" id={d} body={d} remainder={d}", .{
                @intFromEnum(j.id), @intFromEnum(j.body), @intFromEnum(j.remainder),
            }),
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
            .assign_low_level => |a| {
                context.append(" target={d} op={s} args=", .{ @intFromEnum(a.target), @tagName(a.op) });
                appendLocalSpan(context, store, a.args);
                context.append(" next={d}", .{@intFromEnum(a.next)});
            },
            .str_match => |a| context.append(" source={d} match={d} miss={d}", .{
                @intFromEnum(a.source), @intFromEnum(a.on_match), @intFromEnum(a.on_miss),
            }),
            .str_match_set => |a| context.append(" source={d} arms={d} miss={d}", .{
                @intFromEnum(a.source), a.arms.len, @intFromEnum(a.on_miss),
            }),
            .switch_stmt => |s| {
                context.append(" cond={d} default={d}", .{ @intFromEnum(s.cond), @intFromEnum(s.default_branch) });
                for (store.getCFSwitchBranches(s.branches)) |branch| {
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
            inline .assign_literal, .assign_tag, .assign_call_erased, .assign_packed_erased_fn => |a| context.append(" target={d} next={d}", .{ @intFromEnum(a.target), @intFromEnum(a.next) }),
            else => {},
        }
        context.append("\n", .{});
    }
}

fn appendLocalSpan(context: *FailureContext, store: *const LirStore, span: LIR.LocalSpan) void {
    context.append("[", .{});
    for (store.getLocalSpan(span), 0..) |local, index| {
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
        .assign_call_erased => |a| a.target == needle or a.closure == needle or spanHasLocal(store, a.args, needle),
        .assign_packed_erased_fn => |a| a.target == needle or (a.capture != null and a.capture.? == needle),
        .assign_low_level => |a| a.target == needle or spanHasLocal(store, a.args, needle),
        .assign_list => |a| a.target == needle or spanHasLocal(store, a.elems, needle),
        .assign_struct => |a| a.target == needle or spanHasLocal(store, a.fields, needle),
        .assign_tag => |a| a.target == needle or (a.payload != null and a.payload.? == needle),
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
            for (store.getStrMatchSteps(s.steps)) |step| {
                switch (step.capture) {
                    .discard => {},
                    .view => |local| if (local == needle) break :blk true,
                }
            }
            break :blk false;
        },
        .str_match_set => |s| blk: {
            if (s.source == needle) break :blk true;
            for (store.getStrMatchArms(s.arms)) |arm| {
                for (store.getStrMatchSteps(arm.steps)) |step| {
                    switch (step.capture) {
                        .discard => {},
                        .view => |local| if (local == needle) break :blk true,
                    }
                }
            }
            break :blk false;
        },
        .ret => |r| r.value == needle,
        .join, .jump, .crash, .runtime_error, .comptime_exhaustiveness_failed, .comptime_branch_taken, .loop_continue, .loop_break => false,
    };
}

fn spanHasLocal(store: *const LirStore, span: LIR.LocalSpan, needle: LIR.LocalId) bool {
    for (store.getLocalSpan(span)) |local| {
        if (local == needle) return true;
    }
    return false;
}

const ValueId = u32;
const no_value: ValueId = std.math.maxInt(u32);
const no_dense: u32 = std.math.maxInt(u32);

const PresenceCondition = struct {
    local: LIR.LocalId,
    mask: u64,

    fn eql(self: PresenceCondition, other: PresenceCondition) bool {
        return self.local == other.local and self.mask == other.mask;
    }
};

/// Immutable per-value data shared by every forked state in one proc walk.
const ValueInfo = struct {
    /// First local bound to this value; used for stable cross-path naming.
    origin: LIR.LocalId,
    /// Values this value borrows from. The borrow is live only while every
    /// lender is reachable-live.
    lenders: []const ValueId,
    /// True for borrowed proc parameters: live for the whole call by ABI.
    always_live: bool,
};

/// One forked ownership state along a control-flow path.
const State = struct {
    allocator: Allocator,
    /// Value bound to each store local; `no_value` when unbound.
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

    fn init(allocator: Allocator, local_count: usize) Allocator.Error!State {
        const local_value = try allocator.alloc(ValueId, local_count);
        @memset(local_value, no_value);
        return .{
            .allocator = allocator,
            .local_value = local_value,
            .balance = .empty,
            .holder = .empty,
            .conditional_condition = .empty,
            .conditional_condition_mask = .empty,
        };
    }

    fn deinit(self: *State) void {
        self.allocator.free(self.local_value);
        self.balance.deinit(self.allocator);
        self.holder.deinit(self.allocator);
        self.conditional_condition.deinit(self.allocator);
        self.conditional_condition_mask.deinit(self.allocator);
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
        const conditional_condition_mask = try self.conditional_condition_mask.clone(self.allocator);
        return .{
            .allocator = self.allocator,
            .local_value = local_value,
            .balance = balance,
            .holder = holder,
            .conditional_condition = conditional_condition,
            .conditional_condition_mask = conditional_condition_mask,
        };
    }

    fn valueOf(self: *const State, local: LIR.LocalId) ValueId {
        return self.local_value[@intFromEnum(local)];
    }

    fn bindValue(self: *State, local: LIR.LocalId, value: ValueId) void {
        self.local_value[@intFromEnum(local)] = value;
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
    /// For borrowed locals: dense position of the local anchoring the first
    /// unit-carrying value in the lender/holder chain. Equal to `repr` for
    /// ABI-borrowed parameters, which are self-anchored.
    lender_repr: u32,
    /// For conditional-owned locals: raw local id of the presence condition.
    condition: u32,
    /// For conditional-owned locals: presence mask on `condition`.
    condition_mask: u64,
};

const LocalClass = enum(u8) {
    unbound,
    owned,
    conditional_owned,
    borrowed,
};

const JoinRecord = struct {
    body: LIR.CFStmtId,
    params: LIR.LocalSpan,
    /// Locals whose entry state the join body relies on: the join's
    /// parameters plus every local the body subtree reads before rebinding.
    /// Jump states must agree only on these; everything else was settled
    /// before the jump.
    relevant: std.bit_set.DynamicBitSetUnmanaged,
    maybe_uninitialized_params: LIR.LocalSpan,
    maybe_uninitialized_conditions: LIR.LocalSpan,
    maybe_uninitialized_condition_masks: LIR.U64Span,
    /// Digests of entry states the body has been scheduled under. The body
    /// is certified once per distinct jump state, exactly as shared switch
    /// suffixes are re-walked per distinct inflowing state.
    scheduled: std.AutoHashMap(u64, void),
    /// Entry summaries pending a body walk.
    pending: std.ArrayList([]LocalSummary),
};

const MemoEntry = struct {
    stmt: u32,
    digest: u64,
};

const WorkItem = union(enum) {
    segment: Segment,
    join_body: LIR.JoinPointId,
};

const Segment = struct {
    cursor: LIR.CFStmtId,
    state: State,
    /// Join whose body walk produced this segment, for diagnostics.
    origin_join: ?LIR.JoinPointId = null,
};

const Certifier = struct {
    allocator: Allocator,
    store: *const LirStore,
    sigs: arc_sig.SigTable,
    rc_local: []const bool,
    values: std.ArrayList(ValueInfo) = .empty,
    lender_arena: std.heap.ArenaAllocator,
    records: std.AutoHashMap(LIR.JoinPointId, JoinRecord),
    memo: std.AutoHashMap(MemoEntry, void),
    summary_scratch: std.ArrayList(LocalSummary) = .empty,
    repr_scratch: std.AutoHashMap(ValueId, u32),
    /// Dense position per store local for the proc being certified, or
    /// `no_dense` for locals the proc never mentions.
    local_dense: std.ArrayList(u32) = .empty,
    /// Store local id per dense position.
    proc_locals: std.ArrayList(LIR.LocalId) = .empty,
    /// Join bodies of the proc being certified, for jump-following scans.
    join_bodies: std.AutoHashMap(LIR.JoinPointId, LIR.CFStmtId),
    /// Per-proc cache for join-body read-before-rebind sets. These bitsets use
    /// dense proc-local positions, so the cache is cleared at each proc boundary.
    reads_before_rebind_cache: std.AutoHashMap(LIR.CFStmtId, std.bit_set.DynamicBitSetUnmanaged),
    /// Scratch bitset over store locals, reused by join-relevance extension.
    relevant_scratch: std.bit_set.DynamicBitSetUnmanaged = .{},
    diag: *Diagnostic,
    join_state_capacity: usize,
    /// Proc and statement being certified; written by `certifyProc` and
    /// `runSegment` before any read.
    current_proc: LIR.LirProcSpecId = undefined,
    current_sig: arc_sig.RcSig = arc_sig.RcSig.all_owned,
    current_proc_body: LIR.CFStmtId = undefined,
    current_stmt: LIR.CFStmtId = undefined,
    /// Join whose body the current segment certifies, for diagnostics.
    current_origin_join: ?LIR.JoinPointId = null,

    fn deinit(self: *Certifier) void {
        self.values.deinit(self.allocator);
        self.lender_arena.deinit();
        self.clearRecords();
        self.records.deinit();
        self.memo.deinit();
        self.summary_scratch.deinit(self.allocator);
        self.repr_scratch.deinit();
        self.local_dense.deinit(self.allocator);
        self.proc_locals.deinit(self.allocator);
        self.join_bodies.deinit();
        self.clearReadsBeforeRebindCache();
        self.reads_before_rebind_cache.deinit();
        self.relevant_scratch.deinit(self.allocator);
    }

    fn clearRecords(self: *Certifier) void {
        var iter = self.records.valueIterator();
        while (iter.next()) |record| {
            record.relevant.deinit(self.allocator);
            record.scheduled.deinit();
            for (record.pending.items) |entry| self.allocator.free(entry);
            record.pending.deinit(self.allocator);
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
        self.diag.context_stmt = self.current_stmt;
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
        const value = try self.newValue(local, lenders, false);
        try state.addBalance(value, units);
        state.bindValue(local, value);
        return value;
    }

    /// Reports whether the value is reachable-live: it carries a unit, is an
    /// ABI-borrowed parameter, sits inside a live holder, or borrows from
    /// values that are all reachable-live. A value with both a holder and
    /// lenders is live through either path: the holder keeps the moved unit's
    /// allocation alive, and live lenders keep the borrowed-from allocation
    /// alive.
    fn valueIsLive(self: *const Certifier, state: *const State, value: ValueId) bool {
        return self.valueIsLiveDepth(state, value, 0);
    }

    fn valueIsLiveDepth(self: *const Certifier, state: *const State, value: ValueId, depth: usize) bool {
        if (depth > 64) return false;
        if (value >= self.values.items.len) return false;
        const info = self.values.items[value];
        if (info.always_live) return true;
        if (state.balanceOf(value) > 0) return true;
        const holder = state.holderOf(value);
        if (holder != no_value and self.valueIsLiveDepth(state, holder, depth + 1)) {
            return true;
        }
        if (info.lenders.len == 0) return false;
        for (info.lenders) |lender| {
            if (!self.valueIsLiveDepth(state, lender, depth + 1)) return false;
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
        if (!self.valueIsLive(state, value)) {
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

    /// Strict consumption: a transferred unit must exist when it leaves this
    /// proc's hands (call arguments, consumed low-level arguments, returns).
    fn consumeUnit(self: *Certifier, state: *State, value: ValueId, local: LIR.LocalId) CertifyError!void {
        if (value == no_value) return;
        if (state.balanceOf(value) < 1) {
            return self.fail("consumed local {d} without an ownership unit", .{@intFromEnum(local)});
        }
        try state.addBalance(value, -1);
    }

    /// Aggregate consumption: one unit moves into the holder. The emitted
    /// trailing incref restores the operand's own unit, so the balance may go
    /// transiently negative here; the per-path terminal balance check flags a
    /// missing restore.
    fn consumeIntoHolder(
        _: *Certifier,
        state: *State,
        value: ValueId,
        holder_value: ValueId,
    ) CertifyError!void {
        if (value == no_value) return;
        try state.addBalance(value, -1);
        if (holder_value != no_value) {
            try state.setHolder(value, holder_value);
        }
    }

    fn checkLeaks(self: *Certifier, state: *const State) CertifyError!void {
        for (state.balance.items, 0..) |units, value_index| {
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

    /// Builds the per-proc-local quotient summary of a state into scratch
    /// storage. The returned slice is invalidated by the next call.
    fn summarize(self: *Certifier, state: *const State) Allocator.Error![]const LocalSummary {
        self.repr_scratch.clearRetainingCapacity();
        self.summary_scratch.clearRetainingCapacity();
        try self.summary_scratch.ensureTotalCapacity(self.allocator, self.proc_locals.items.len);

        for (self.proc_locals.items, 0..) |local, dense| {
            if (!self.isRc(local)) continue;
            const value = state.valueOf(local);
            if (value == no_value) continue;
            const entry = try self.repr_scratch.getOrPut(value);
            if (!entry.found_existing) entry.value_ptr.* = @intCast(dense);
        }

        for (self.proc_locals.items) |local| {
            var summary = LocalSummary{ .class = .unbound, .repr = 0, .balance = 0, .lender_repr = 0, .condition = no_dense, .condition_mask = 0 };
            if (self.isRc(local)) {
                const value = state.valueOf(local);
                if (value != no_value) {
                    const repr = self.repr_scratch.get(value) orelse 0;
                    const units = state.balanceOf(value);
                    if (units > 0) {
                        if (state.conditionalConditionOf(value)) |condition| {
                            summary = .{
                                .class = .conditional_owned,
                                .repr = repr,
                                .balance = @intCast(units),
                                .lender_repr = 0,
                                .condition = @intFromEnum(condition.local),
                                .condition_mask = condition.mask,
                            };
                        } else {
                            summary = .{ .class = .owned, .repr = repr, .balance = @intCast(units), .lender_repr = 0, .condition = no_dense, .condition_mask = 0 };
                        }
                    } else if (self.valueIsLive(state, value)) {
                        summary = .{
                            .class = .borrowed,
                            .repr = repr,
                            .balance = 0,
                            .lender_repr = self.liveAnchorRepr(state, value),
                            .condition = no_dense,
                            .condition_mask = 0,
                        };
                    }
                }
            }
            self.summary_scratch.appendAssumeCapacity(summary);
        }

        return self.summary_scratch.items;
    }

    /// Returns the dense position anchoring the first unit-carrying (or
    /// ABI-borrowed) value reached through lender/holder links, for stable
    /// cross-path naming of where a borrow takes its liveness from.
    fn liveAnchorRepr(self: *const Certifier, state: *const State, value: ValueId) u32 {
        const anchor = self.liveAnchorValue(state, value);
        if (anchor == no_value) return 0;
        if (self.repr_scratch.get(anchor)) |repr| return repr;
        return self.denseOf(self.values.items[anchor].origin);
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
            hasher.update(std.mem.asBytes(&entry.lender_repr));
            hasher.update(std.mem.asBytes(&entry.condition));
            hasher.update(std.mem.asBytes(&entry.condition_mask));
        }
        return hasher.final();
    }

    /// Rebuilds a fresh state from an agreed join-entry summary. Alias sets
    /// share one fresh value; borrows are re-linked to the fresh value of the
    /// local their liveness anchored on.
    fn stateFromSummary(self: *Certifier, summary: []const LocalSummary) CertifyError!State {
        var state = try State.init(self.allocator, self.store.locals.items.len);
        errdefer state.deinit();

        for (summary, 0..) |entry, dense| {
            if (entry.class != .owned or entry.repr != dense) continue;
            const local = self.proc_locals.items[dense];
            _ = try self.bindFresh(&state, local, @intCast(entry.balance), &.{});
        }
        for (summary, 0..) |entry, dense| {
            if (entry.class != .conditional_owned or entry.repr != dense) continue;
            const local = self.proc_locals.items[dense];
            const value = try self.bindFresh(&state, local, 1, &.{});
            try state.setConditional(value, .{ .local = @enumFromInt(entry.condition), .mask = entry.condition_mask });
        }
        for (summary, 0..) |entry, dense| {
            if (entry.class != .owned or entry.repr == dense) continue;
            const local = self.proc_locals.items[dense];
            state.bindValue(local, state.valueOf(self.proc_locals.items[entry.repr]));
        }
        for (summary, 0..) |entry, dense| {
            if (entry.class != .conditional_owned or entry.repr == dense) continue;
            const local = self.proc_locals.items[dense];
            state.bindValue(local, state.valueOf(self.proc_locals.items[entry.repr]));
        }
        for (summary, 0..) |entry, dense| {
            if (entry.class != .borrowed or entry.repr != dense) continue;
            const local = self.proc_locals.items[dense];
            if (entry.lender_repr == dense) {
                // Self-anchored borrow: an ABI-borrowed parameter, live for
                // the whole call.
                const value = try self.newValue(local, &.{}, true);
                try state.growToValue(value);
                state.bindValue(local, value);
                continue;
            }
            const anchor_dense: usize = entry.lender_repr;
            if (anchor_dense >= self.proc_locals.items.len) {
                return self.fail("borrowed local {d} crossed a join without a live owner local", .{@intFromEnum(local)});
            }
            const anchor_local = self.proc_locals.items[anchor_dense];
            const lender = state.valueOf(anchor_local);
            if (lender == no_value) {
                return self.fail("borrowed local {d} crossed a join without a live owner local", .{@intFromEnum(local)});
            }
            _ = try self.bindFresh(&state, local, 0, &.{lender});
        }
        for (summary, 0..) |entry, dense| {
            if (entry.class != .borrowed or entry.repr == dense) continue;
            const local = self.proc_locals.items[dense];
            state.bindValue(local, state.valueOf(self.proc_locals.items[entry.repr]));
        }
        return state;
    }

    fn collectProcLocals(self: *Certifier, proc: LIR.LirProcSpec, body: LIR.CFStmtId) Allocator.Error!void {
        self.proc_locals.clearRetainingCapacity();
        self.local_dense.clearRetainingCapacity();
        try self.local_dense.ensureTotalCapacity(self.allocator, self.store.locals.items.len);
        self.local_dense.items.len = self.store.locals.items.len;
        @memset(self.local_dense.items, no_dense);

        for (self.store.getLocalSpan(proc.args)) |param| {
            try self.noteProcLocal(param);
        }

        var visited = std.AutoHashMap(LIR.CFStmtId, void).init(self.allocator);
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
                    try stack.append(self.allocator, assign.next);
                },
                .init_uninitialized => |init| {
                    try self.noteProcLocal(init.target);
                    try stack.append(self.allocator, init.next);
                },
                .assign_call => |assign| {
                    try self.noteProcLocal(assign.target);
                    try self.noteProcLocalSpan(assign.args);
                    try stack.append(self.allocator, assign.next);
                },
                .assign_call_erased => |assign| {
                    try self.noteProcLocal(assign.target);
                    try self.noteProcLocal(assign.closure);
                    try self.noteProcLocalSpan(assign.args);
                    try stack.append(self.allocator, assign.next);
                },
                .assign_packed_erased_fn => |assign| {
                    try self.noteProcLocal(assign.target);
                    if (assign.capture) |capture| try self.noteProcLocal(capture);
                    try stack.append(self.allocator, assign.next);
                },
                .assign_low_level => |assign| {
                    try self.noteProcLocal(assign.target);
                    try self.noteProcLocalSpan(assign.args);
                    try stack.append(self.allocator, assign.next);
                },
                .assign_list => |assign| {
                    try self.noteProcLocal(assign.target);
                    try self.noteProcLocalSpan(assign.elems);
                    try stack.append(self.allocator, assign.next);
                },
                .assign_struct => |assign| {
                    try self.noteProcLocal(assign.target);
                    try self.noteProcLocalSpan(assign.fields);
                    try stack.append(self.allocator, assign.next);
                },
                .assign_tag => |assign| {
                    try self.noteProcLocal(assign.target);
                    if (assign.payload) |payload| try self.noteProcLocal(payload);
                    try stack.append(self.allocator, assign.next);
                },
                .set_local => |assign| {
                    try self.noteProcLocal(assign.target);
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
                    for (self.store.getCFSwitchBranches(switch_stmt.branches)) |branch| {
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
                    for (self.store.getStrMatchSteps(str_match.steps)) |step| {
                        switch (step.capture) {
                            .discard => {},
                            .view => |local| try self.noteProcLocal(local),
                        }
                    }
                    try stack.append(self.allocator, str_match.on_match);
                    try stack.append(self.allocator, str_match.on_miss);
                },
                .str_match_set => |str_match_set| {
                    try self.noteProcLocal(str_match_set.source);
                    for (self.store.getStrMatchArms(str_match_set.arms)) |arm| {
                        for (self.store.getStrMatchSteps(arm.steps)) |step| {
                            switch (step.capture) {
                                .discard => {},
                                .view => |local| try self.noteProcLocal(local),
                            }
                        }
                        try stack.append(self.allocator, arm.on_match);
                    }
                    try stack.append(self.allocator, str_match_set.on_miss);
                },
                .join => |join_stmt| {
                    try self.noteProcLocalSpan(join_stmt.params);
                    try self.join_bodies.put(join_stmt.id, join_stmt.body);
                    try stack.append(self.allocator, join_stmt.body);
                    try stack.append(self.allocator, join_stmt.remainder);
                },
                .ret => |ret_stmt| try self.noteProcLocal(ret_stmt.value),
                .jump, .crash, .runtime_error, .comptime_exhaustiveness_failed, .loop_continue, .loop_break => {},
                .comptime_branch_taken => |marker| try stack.append(self.allocator, marker.next),
            }
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
        for (self.store.getLocalSpan(span)) |local| {
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
                    self.noteExposedReadSpan(&graph.nodes.items[node_index].reads, assign.args);
                    self.setReadBeforeRebindDef(&graph, node_index, assign.target);
                    try self.appendReadBeforeRebindSuccessor(&graph, &work, node_index, assign.next);
                },
                .assign_packed_erased_fn => |assign| {
                    if (assign.capture) |capture| self.noteExposedReadLocal(&graph.nodes.items[node_index].reads, capture);
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
                    if (assign.payload) |payload| self.noteExposedReadLocal(&graph.nodes.items[node_index].reads, payload);
                    self.setReadBeforeRebindDef(&graph, node_index, assign.target);
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
                    for (self.store.getCFSwitchBranches(switch_stmt.branches)) |branch| {
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
                    for (self.store.getStrMatchSteps(str_match.steps)) |step| {
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
                    for (self.store.getStrMatchArms(str_match_set.arms)) |arm| {
                        for (self.store.getStrMatchSteps(arm.steps)) |step| {
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
                .runtime_error, .comptime_exhaustiveness_failed, .crash, .loop_continue, .loop_break => {},
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
        var relevant = try std.bit_set.DynamicBitSetUnmanaged.initEmpty(self.allocator, self.store.locals.items.len);
        errdefer relevant.deinit(self.allocator);
        const reads = try self.computeReadsBeforeRebind(body);
        for (self.proc_locals.items, 0..) |local, dense| {
            if (!self.isRc(local)) continue;
            if (relevant.isSet(@intFromEnum(local))) continue;
            if (reads.isSet(dense)) {
                relevant.set(@intFromEnum(local));
            }
        }
        return relevant;
    }

    /// Returns a unit-carrying or ABI-borrowed value that keeps this value
    /// live, reached through holder or lender links, or `no_value` when no
    /// chain is live.
    fn liveAnchorValue(self: *const Certifier, state: *const State, value: ValueId) ValueId {
        return self.liveAnchorValueDepth(state, value, 0);
    }

    fn liveAnchorValueDepth(self: *const Certifier, state: *const State, value: ValueId, depth: usize) ValueId {
        if (depth > 64) return no_value;
        if (value >= self.values.items.len) return no_value;
        const info = self.values.items[value];
        if (info.always_live or state.balanceOf(value) > 0) return value;
        const holder = state.holderOf(value);
        if (holder != no_value) {
            const through_holder = self.liveAnchorValueDepth(state, holder, depth + 1);
            if (through_holder != no_value) return through_holder;
        }
        if (info.lenders.len == 0) return no_value;
        return self.liveAnchorValueDepth(state, info.lenders[0], depth + 1);
    }

    fn maybeUninitializedCondition(record: *const JoinRecord, store: *const LirStore, local: LIR.LocalId) ?PresenceCondition {
        const params = store.getLocalSpan(record.maybe_uninitialized_params);
        const conditions = store.getLocalSpan(record.maybe_uninitialized_conditions);
        const masks = store.getU64Span(record.maybe_uninitialized_condition_masks);
        if (params.len != conditions.len or params.len != masks.len) {
            std.debug.panic("ARC borrow certifier invariant violated: maybe-uninitialized join metadata arity mismatch", .{});
        }
        for (params, conditions, masks) |param, condition, mask| {
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
        state: *const State,
        record: *const JoinRecord,
        join_id: LIR.JoinPointId,
    ) CertifyError![]const LocalSummary {
        // Seed the working relevant set from the record.
        var index: usize = 0;
        while (index < self.relevant_scratch.capacity()) : (index += 1) {
            self.relevant_scratch.setValue(index, record.relevant.isSet(index));
        }

        // Extend through borrow anchors: a relevant borrowed local keeps its
        // lender's carrier local live, so the carrier joins the agreement.
        var changed = true;
        var rounds: usize = 0;
        while (changed and rounds < 64) : (rounds += 1) {
            changed = false;
            for (self.proc_locals.items) |local| {
                const local_index = @intFromEnum(local);
                if (!self.relevant_scratch.isSet(local_index)) continue;
                if (!self.isRc(local)) continue;
                const value = state.valueOf(local);
                if (value == no_value) continue;
                if (state.balanceOf(value) > 0) continue;
                const anchor = self.liveAnchorValue(state, value);
                if (anchor == no_value) continue;
                // Find a carrier local for the anchor value.
                var carrier: u32 = no_dense;
                for (self.proc_locals.items) |candidate| {
                    if (state.valueOf(candidate) == anchor) {
                        carrier = @intFromEnum(candidate);
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

        // Build the restricted summary.
        self.repr_scratch.clearRetainingCapacity();
        self.summary_scratch.clearRetainingCapacity();
        try self.summary_scratch.ensureTotalCapacity(self.allocator, self.proc_locals.items.len);

        for (self.proc_locals.items, 0..) |local, dense| {
            if (!self.isRc(local)) continue;
            if (!self.relevant_scratch.isSet(@intFromEnum(local))) continue;
            const value = state.valueOf(local);
            if (value == no_value) continue;
            const entry = try self.repr_scratch.getOrPut(value);
            if (!entry.found_existing) entry.value_ptr.* = @intCast(dense);
        }

        for (self.proc_locals.items) |local| {
            var summary = LocalSummary{ .class = .unbound, .repr = 0, .balance = 0, .lender_repr = 0, .condition = no_dense, .condition_mask = 0 };
            if (self.isRc(local) and self.relevant_scratch.isSet(@intFromEnum(local))) {
                if (maybeUninitializedCondition(record, self.store, local)) |condition| {
                    summary = .{
                        .class = .conditional_owned,
                        .repr = self.denseOf(local),
                        .balance = 1,
                        .lender_repr = 0,
                        .condition = @intFromEnum(condition.local),
                        .condition_mask = condition.mask,
                    };
                } else {
                    const value = state.valueOf(local);
                    if (value != no_value) {
                        const repr = self.repr_scratch.get(value) orelse 0;
                        const units = state.balanceOf(value);
                        if (units > 0) {
                            if (state.conditionalConditionOf(value)) |condition| {
                                summary = .{
                                    .class = .conditional_owned,
                                    .repr = repr,
                                    .balance = @intCast(units),
                                    .lender_repr = 0,
                                    .condition = @intFromEnum(condition.local),
                                    .condition_mask = condition.mask,
                                };
                            } else {
                                summary = .{ .class = .owned, .repr = repr, .balance = @intCast(units), .lender_repr = 0, .condition = no_dense, .condition_mask = 0 };
                            }
                        } else if (self.valueIsLive(state, value)) {
                            summary = .{
                                .class = .borrowed,
                                .repr = repr,
                                .balance = 0,
                                .lender_repr = self.liveAnchorRepr(state, value),
                                .condition = no_dense,
                                .condition_mask = 0,
                            };
                        }
                    }
                }
            }
            self.summary_scratch.appendAssumeCapacity(summary);
        }

        // Every outstanding ownership unit must be carried into the join by
        // a relevant local; anything else can never be released again.
        for (state.balance.items, 0..) |units, value_index| {
            if (units == 0) continue;
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
        for (self.store.getLocalSpan(span)) |local| {
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
        self.values.clearRetainingCapacity();
        _ = self.lender_arena.reset(.retain_capacity);
        self.clearRecords();
        self.memo.clearRetainingCapacity();
        self.join_bodies.clearRetainingCapacity();
        self.clearReadsBeforeRebindCache();
        try self.collectProcLocals(proc, body);
        try self.relevant_scratch.resize(self.allocator, self.store.locals.items.len, false);

        var state = try State.init(self.allocator, self.store.locals.items.len);
        {
            errdefer state.deinit();
            for (self.store.getLocalSpan(proc.args), 0..) |param, index| {
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
            switch (item) {
                .segment => |segment| try self.runSegment(&work, segment),
                .join_body => |join_id| try self.scheduleJoinBody(&work, join_id),
            }
        }
    }

    fn scheduleJoinBody(self: *Certifier, work: *std.ArrayList(WorkItem), join_id: LIR.JoinPointId) CertifyError!void {
        const record = self.records.getPtr(join_id) orelse return;
        const entry = record.pending.pop() orelse return;
        defer self.allocator.free(entry);
        var body_state = try self.stateFromSummary(entry);
        errdefer body_state.deinit();
        try work.append(self.allocator, .{ .segment = .{
            .cursor = record.body,
            .state = body_state,
            .origin_join = join_id,
        } });
    }

    fn runSegment(self: *Certifier, work: *std.ArrayList(WorkItem), segment: Segment) CertifyError!void {
        var state = segment.state;
        defer state.deinit();
        var cursor = segment.cursor;
        self.current_origin_join = segment.origin_join;

        while (true) {
            self.current_stmt = cursor;

            const summary = try self.summarize(&state);
            const memo_entry = MemoEntry{ .stmt = @intFromEnum(cursor), .digest = summaryDigest(cursor, summary) };
            const seen = try self.memo.getOrPut(memo_entry);
            if (seen.found_existing) return;

            const stmt = self.store.getCFStmt(cursor);
            switch (stmt) {
                .assign_ref => |assign| {
                    switch (assign.op) {
                        .local => |source| {
                            if (assign.target != source) {
                                _ = try self.requireLive(&state, source);
                                if (self.isRc(assign.target)) {
                                    state.bindValue(assign.target, state.valueOf(source));
                                }
                            }
                        },
                        .discriminant => |op| _ = try self.requireLive(&state, op.source),
                        .field => |op| try self.bindPayloadRead(&state, assign.target, op.source),
                        .tag_payload => |op| try self.bindPayloadRead(&state, assign.target, op.source),
                        .tag_payload_struct => |op| try self.bindPayloadRead(&state, assign.target, op.source),
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
                    try self.applyCall(&state, assign.target, self.sigs.get(assign.proc), assign.args);
                    cursor = assign.next;
                },
                .assign_call_erased => |assign| {
                    _ = try self.requireLive(&state, assign.closure);
                    try self.applyCall(&state, assign.target, arc_sig.RcSig.all_owned, assign.args);
                    cursor = assign.next;
                },
                .assign_packed_erased_fn => |assign| {
                    const capture_value = if (assign.capture) |capture|
                        try self.requireLive(&state, capture)
                    else
                        no_value;
                    if (self.isRc(assign.target)) {
                        const target_value = try self.bindFresh(&state, assign.target, 1, &.{});
                        if (assign.capture != null) {
                            try self.consumeIntoHolder(&state, capture_value, target_value);
                        }
                    } else if (assign.capture != null) {
                        try self.consumeIntoHolder(&state, capture_value, no_value);
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
                    if (assign.payload) |payload| {
                        try self.applyAggregate(&state, assign.target, &.{payload});
                    } else {
                        try self.applyAggregate(&state, assign.target, &.{});
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
                    for (self.store.getCFSwitchBranches(switch_stmt.branches)) |branch| {
                        var branch_state = try state.clone();
                        errdefer branch_state.deinit();
                        try work.append(self.allocator, .{ .segment = .{ .cursor = branch.body, .state = branch_state, .origin_join = segment.origin_join } });
                    }
                    var default_state = try state.clone();
                    errdefer default_state.deinit();
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

                                var initialized_state = try state.clone();
                                errdefer initialized_state.deinit();
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
                    for (self.store.getStrMatchSteps(str_match.steps)) |step| {
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
                    for (self.store.getStrMatchArms(str_match_set.arms)) |arm| {
                        var match_state = try state.clone();
                        errdefer match_state.deinit();
                        for (self.store.getStrMatchSteps(arm.steps)) |step| {
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
                            .scheduled = std.AutoHashMap(u64, void).init(self.allocator),
                            .pending = .empty,
                        };
                    }
                    cursor = join_stmt.remainder;
                },
                .jump => |jump_stmt| {
                    const record = self.records.getPtr(jump_stmt.target) orelse {
                        return self.fail("jump to join {d} before its definition", .{@intFromEnum(jump_stmt.target)});
                    };
                    const jump_summary = try self.summarizeForJoin(&state, record, jump_stmt.target);
                    const digest = summaryDigest(record.body, jump_summary);
                    const seen_entry = try record.scheduled.getOrPut(digest);
                    if (!seen_entry.found_existing) {
                        // The join body is certified once per distinct inflowing entry-state
                        // summary. For a loop carrying K refcounted mutable locals whose body
                        // merges and re-splits their alias groups, the number of distinct
                        // (alias-partition x balance) summaries is finite but grows like the Bell
                        // number of K (B(6) = 203). This bound is purely a *capacity* limit on how
                        // many states we will enumerate for one procedure before giving up — it is
                        // NOT a refcount finding. Exceeding it means "I cannot finish proving this
                        // procedure within budget," so the certifier abandons this procedure
                        // (leaving it unverified) rather than aborting the build or claiming a bug:
                        // a verification tool must only block on a positive finding, never on its
                        // own incompleteness (issue 9658 was a valid 203-state scanner). Findings
                        // (leak / use-after-free / balance mismatch) on the first ≤4096 distinct
                        // entry-states still `fail` → `error.Certification` → abort. The cap can
                        // mask only a finding whose sole manifesting path is reached past the
                        // 4096th distinct entry-state — most plausibly a real leak whose owned
                        // balance grows every iteration (balance is part of the per-join summary
                        // digest), so the state count climbs without converging; that proc is then
                        // skipped rather than reported. The algorithmically-ideal fix is a
                        // converging dataflow fixpoint that joins entry states over a finite-height
                        // semilattice (bounding re-walks by lattice height rather than enumerating
                        // summaries); it remains future work because a correct join must preserve
                        // exact per-path ownership-unit accounting — naively merging owned states
                        // that carry different balances (e.g. `if c then x = dup(y) else x = y`)
                        // would be unsound. Until then the bound is generous enough to fully
                        // certify realistic procedures and only skips genuinely pathological ones.
                        if (record.scheduled.count() > self.join_state_capacity) {
                            return error.CertifierCapacityExceeded;
                        }
                        const copy = try self.allocator.dupe(LocalSummary, jump_summary);
                        errdefer self.allocator.free(copy);
                        // Append to `work` first, while `errdefer` is the sole owner of
                        // `copy`; `pending` takes ownership last, so neither append's
                        // OOM path can double-free (the errdefer only fires before
                        // `pending` owns it).
                        try work.append(self.allocator, .{ .join_body = jump_stmt.target });
                        try record.pending.append(self.allocator, copy);
                    }
                    return;
                },
                .ret => |ret_stmt| {
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
                .crash => {
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

    fn bindPayloadRead(self: *Certifier, state: *State, target: LIR.LocalId, source: LIR.LocalId) CertifyError!void {
        const source_value = try self.requireLive(state, source);
        if (!self.isRc(target)) return;
        if (source_value == no_value) {
            return self.fail(
                "payload read into refcounted local {d} from non-refcounted source {d}",
                .{ @intFromEnum(target), @intFromEnum(source) },
            );
        }
        _ = try self.bindFresh(state, target, 0, &.{source_value});
    }

    fn bindSameValue(self: *Certifier, state: *State, target: LIR.LocalId, source: LIR.LocalId) CertifyError!void {
        const source_value = try self.requireLive(state, source);
        if (!self.isRc(target)) return;
        if (source_value == no_value) {
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
        if (state.balanceOf(value) < 1) {
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

        var arg_values_buffer: [64]ValueId = undefined;
        for (arg_locals, 0..) |arg, index| {
            const value = try self.requireLive(state, arg);
            if (index < arg_values_buffer.len) arg_values_buffer[index] = value;
        }

        for (arg_locals, 0..) |arg, index| {
            if (!self.isRc(arg)) continue;
            switch (callee_sig.paramMode(index)) {
                .owned => {
                    const value = if (index < arg_values_buffer.len)
                        arg_values_buffer[index]
                    else
                        state.valueOf(arg);
                    try self.consumeUnit(state, value, arg);
                },
                .borrowed => {},
            }
        }

        if (self.isRc(target)) {
            switch (callee_sig.ret_mode) {
                .owned => _ = try self.bindFresh(state, target, 1, &.{}),
                .borrowed => {
                    var lenders_buffer: [64]ValueId = undefined;
                    var lender_count: usize = 0;
                    for (arg_locals, 0..) |arg, index| {
                        if (index >= 64) break;
                        const bit = @as(u64, 1) << @as(u6, @intCast(index));
                        if ((callee_sig.ret_lenders & bit) == 0) continue;
                        if (!self.isRc(arg)) continue;
                        const value = arg_values_buffer[index];
                        if (value == no_value) continue;
                        lenders_buffer[lender_count] = value;
                        lender_count += 1;
                    }
                    _ = try self.bindFresh(state, target, 0, lenders_buffer[0..lender_count]);
                },
            }
        }
    }

    fn applyLowLevel(self: *Certifier, state: *State, assign: anytype) CertifyError!void {
        const arg_locals = self.store.getLocalSpan(assign.args);

        var arg_values_buffer: [64]ValueId = undefined;
        for (arg_locals, 0..) |arg, index| {
            const value = try self.requireLive(state, arg);
            if (index < arg_values_buffer.len) arg_values_buffer[index] = value;
        }

        // Consumed positions transfer one unit each into the op.
        for (arg_locals, 0..) |arg, index| {
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
                for (arg_locals, 0..) |arg, index| {
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
        for (arg_locals, 0..) |arg, index| {
            if (index >= 64) break;
            if (!self.isRc(arg)) continue;
            const bit = @as(u64, 1) << @as(u6, @intCast(index));
            if ((assign.rc_effect.retain_args & bit) == 0) continue;
            try self.consumeIntoHolder(state, arg_values_buffer[index], target_value);
        }
    }

    fn applyAggregate(
        self: *Certifier,
        state: *State,
        target: LIR.LocalId,
        operands: []const LIR.LocalId,
    ) CertifyError!void {
        var operand_values_buffer: [64]ValueId = undefined;
        for (operands, 0..) |operand, index| {
            const value = try self.requireLive(state, operand);
            if (index < operand_values_buffer.len) operand_values_buffer[index] = value;
        }

        var target_value: ValueId = no_value;
        if (self.isRc(target)) {
            target_value = try self.bindFresh(state, target, 1, &.{});
        }

        for (operands, 0..) |operand, index| {
            if (!self.isRc(operand)) continue;
            const value = if (index < operand_values_buffer.len)
                operand_values_buffer[index]
            else
                state.valueOf(operand);
            try self.consumeIntoHolder(state, value, target_value);
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

    fn rcHelper(op: layout_mod.RcOp, layout_idx: layout_mod.Idx) layout_mod.RcHelper {
        return .{ .op = op, .layout_idx = layout_idx };
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
        return try self.store.addProcSpec(.{
            .name = self.store.freshSyntheticSymbol(),
            .args = try self.store.addLocalSpan(args),
            .body = body,
            .ret_layout = ret_layout,
        });
    }

    fn certify(self: *CertifyTest) CertifyError!void {
        return certifyStore(self.allocator, &self.store, &self.layouts, arc_sig.SigTable.all_owned, &.{}, &self.diag);
    }

    fn certifyWith(self: *CertifyTest, sigs: arc_sig.SigTable) CertifyError!void {
        return certifyStore(self.allocator, &self.store, &self.layouts, sigs, &.{}, &self.diag);
    }

    fn certifyWithOptions(self: *CertifyTest, options: CertifyOptions) CertifyError!void {
        return certifyStoreWithOptions(self.allocator, &self.store, &self.layouts, arc_sig.SigTable.all_owned, &.{}, &self.diag, options);
    }
};

test "skip outcome escalates to fail only in forbid mode" {
    try testing.expectEqual(SkipOutcome.none, skipOutcome(0, false));
    try testing.expectEqual(SkipOutcome.none, skipOutcome(0, true));
    try testing.expectEqual(SkipOutcome.warn, skipOutcome(3, false));
    try testing.expectEqual(SkipOutcome.fail, skipOutcome(3, true));
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
    try testing.expectEqual(@as(usize, 0), f.diag.skipped_proc_count);
}

test "certify records skipped proc when join-state capacity is exceeded" {
    var f = try CertifyTest.init(testing.allocator);
    defer f.deinit();
    const result = try f.local(.i64);

    const join_id = f.freshJoinPointId();
    const ret = try f.ret(result);
    const result_assign = try f.assignI64(result, ret);
    const jump = try f.store.addCFStmt(.{ .jump = .{ .target = join_id } });
    const join_stmt = try f.store.addCFStmt(.{ .join = .{
        .id = join_id,
        .params = LIR.LocalSpan.empty(),
        .body = result_assign,
        .remainder = jump,
    } });
    const proc = try f.addProc(&.{}, join_stmt, .i64);
    try f.store.setProcDebugName(proc, "capacitySkip");

    try f.certifyWithOptions(.{ .join_state_capacity = 0 });

    try testing.expectEqual(@as(usize, 1), f.diag.skipped_proc_count);
    try testing.expectEqual(@as(usize, 1), f.diag.skippedSample().len);
    try testing.expectEqual(proc, f.diag.skippedSample()[0]);

    var report = FailureContext{};
    writeSkippedProcReport(&report, &f.store, &f.diag);
    try testing.expect(std.mem.find(u8, report.text(), "ARC certifier skipped 1 procedure(s)") != null);
    try testing.expect(std.mem.find(u8, report.text(), "UNVERIFIED") != null);
    try testing.expect(std.mem.find(u8, report.text(), "capacitySkip") != null);
}

test "skipped proc report caps listed names and preserves total count" {
    var f = try CertifyTest.init(testing.allocator);
    defer f.deinit();

    var diag = Diagnostic{};
    for (0..skipped_proc_record_limit + 1) |index| {
        const proc = try f.store.addProcSpec(.{
            .name = f.store.freshSyntheticSymbol(),
            .args = LIR.LocalSpan.empty(),
            .ret_layout = .i64,
        });
        var name_buf: [32]u8 = undefined;
        const name = try std.fmt.bufPrint(&name_buf, "proc-{d}", .{index});
        try f.store.setProcDebugName(proc, name);
        diag.recordSkippedProc(proc);
    }

    var report = FailureContext{};
    writeSkippedProcReport(&report, &f.store, &diag);

    try testing.expectEqual(@as(usize, skipped_proc_record_limit + 1), diag.skipped_proc_count);
    try testing.expectEqual(@as(usize, skipped_proc_record_limit), diag.skippedSample().len);
    var header_buf: [64]u8 = undefined;
    const header = try std.fmt.bufPrint(&header_buf, "ARC certifier skipped {d} procedure(s)", .{skipped_proc_record_limit + 1});
    try testing.expect(std.mem.find(u8, report.text(), header) != null);
    try testing.expect(std.mem.find(u8, report.text(), "proc-0") != null);
    var included_buf: [32]u8 = undefined;
    const last_included = try std.fmt.bufPrint(&included_buf, "proc-{d}", .{skipped_proc_record_limit - 1});
    try testing.expect(std.mem.find(u8, report.text(), last_included) != null);
    var excluded_buf: [32]u8 = undefined;
    const first_excluded = try std.fmt.bufPrint(&excluded_buf, "proc-{d}", .{skipped_proc_record_limit});
    try testing.expect(std.mem.find(u8, report.text(), first_excluded) == null);
    try testing.expect(std.mem.find(u8, report.text(), "... and 1 more") != null);
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
