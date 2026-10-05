//! Decide where pending erased calls are run.
//!
//! ARC leaves an erased call in tail position `deferred`: instead of making
//! the call, its procedure records it as pending and returns, so the frame is
//! gone before the callee runs. Something must then make that call. This pass
//! finds every statement after which a call can be pending and states, on the
//! statement itself, whether the pending calls are run there.
//!
//! A procedure may hand a pending call up to its caller only when that caller
//! is known to deal with it. A direct call from Roc code is such a caller,
//! because this pass sees it. The erased-call runtime is one too, and an
//! erased-callable procedure learns at entry whether that is who called it.
//! Anything else, such as a host calling a root, the runtime calling a dictionary
//! worker, a procedure whose address is taken, is not, so those procedures
//! run their own pending calls.
//!
//! A statement hands its pending call up when nothing but reference-count
//! statements and jumps into a join body separates it from the return of its
//! result. Those statements are safe to run before the pending call: a pending
//! call owns its closure and every argument, so nothing it reads is released
//! by them.

const std = @import("std");
const core = @import("lir_core");
const body_clone = @import("body_clone.zig");

const LIR = core.LIR;
const LirStore = core.LirStore;
const Allocator = std.mem.Allocator;

const DirectCall = struct {
    caller: u32,
    callee: u32,
    stmt: LIR.CFStmtId,
    /// The caller returns this call's value with nothing but reference-count
    /// statements in between.
    returns_unchanged: bool,
};

const DeferredCall = struct {
    caller: u32,
    stmt: LIR.CFStmtId,
};

/// Stamp `drive` on every statement that can be followed by a pending call.
/// `entered_from_outside` names the procedures something other than a direct
/// Roc call or the erased-call runtime can enter.
pub fn run(
    allocator: Allocator,
    store: *LirStore,
    entered_from_outside: []const []const LIR.LirProcSpecId,
) Allocator.Error!void {
    const proc_count = store.procSpecCount();
    var direct_calls = std.ArrayList(DirectCall).empty;
    defer direct_calls.deinit(allocator);
    var deferred_calls = std.ArrayList(DeferredCall).empty;
    defer deferred_calls.deinit(allocator);
    var outside = try std.bit_set.DynamicBitSetUnmanaged.initEmpty(allocator, proc_count);
    defer outside.deinit(allocator);
    for (entered_from_outside) |procs| {
        for (procs) |proc| outside.set(@intFromEnum(proc));
    }

    var seen = try std.bit_set.DynamicBitSetUnmanaged.initEmpty(allocator, store.cfStmtCount());
    defer seen.deinit(allocator);
    var work = std.ArrayList(LIR.CFStmtId).empty;
    defer work.deinit(allocator);
    // The body of each join of the procedure being walked, by join id. A
    // join encloses every jump to it, so it is recorded before they are read.
    var join_bodies = std.ArrayList(?LIR.CFStmtId).empty;
    defer join_bodies.deinit(allocator);
    for (0..proc_count) |proc_index| {
        const proc = store.getProcSpec(@enumFromInt(@as(u32, @intCast(proc_index))));
        const body = proc.body orelse continue;
        join_bodies.clearRetainingCapacity();
        try work.append(allocator, body);
        while (work.pop()) |stmt_id| {
            if (seen.isSet(@intFromEnum(stmt_id))) continue;
            seen.set(@intFromEnum(stmt_id));
            const stmt = store.getCFStmt(stmt_id);
            if (stmt == .join) {
                const join_index = @intFromEnum(stmt.join.id);
                if (join_index >= join_bodies.items.len) {
                    try join_bodies.appendNTimes(allocator, null, join_index + 1 - join_bodies.items.len);
                }
                join_bodies.items[join_index] = stmt.join.body;
            } else if (stmt == .assign_call) {
                try direct_calls.append(allocator, .{
                    .caller = @intCast(proc_index),
                    .callee = @intFromEnum(stmt.assign_call.proc),
                    .stmt = stmt_id,
                    .returns_unchanged = returnsUnchanged(store, join_bodies.items, stmt.assign_call.target, stmt.assign_call.next),
                });
            } else if (stmt == .assign_call_erased) {
                if (stmt.assign_call_erased.deferred) try deferred_calls.append(allocator, .{
                    .caller = @intCast(proc_index),
                    .stmt = stmt_id,
                });
            } else if (stmt == .assign_literal and stmt.assign_literal.value == .proc_ref) {
                outside.set(@intFromEnum(stmt.assign_literal.value.proc_ref));
            }
            try body_clone.appendSuccessorsWithAllocator(store, &work, stmt_id, allocator);
        }
    }
    if (deferred_calls.items.len == 0) return;

    // A procedure can return with a call pending when it defers one itself,
    // or hands up one a callee left.
    var may_pend = try std.bit_set.DynamicBitSetUnmanaged.initEmpty(allocator, proc_count);
    defer may_pend.deinit(allocator);
    for (deferred_calls.items) |deferred| {
        if (handsUp(store, &outside, deferred.caller) != .never) may_pend.set(deferred.caller);
    }
    var changed = true;
    while (changed) {
        changed = false;
        for (direct_calls.items) |call| {
            if (may_pend.isSet(call.caller) or !may_pend.isSet(call.callee)) continue;
            if (handsUp(store, &outside, call.caller) == .never) continue;
            if (!call.returns_unchanged) continue;
            may_pend.set(call.caller);
            changed = true;
        }
    }

    for (deferred_calls.items) |deferred| {
        const drive = driveAfter(store, &outside, deferred.caller, true);
        store.getCFStmtPtr(deferred.stmt).assign_call_erased.drive = drive;
        if (drive == .unless_caller_drives) store.getProcSpecPtr(@enumFromInt(deferred.caller)).reads_caller_drives = true;
    }
    for (direct_calls.items) |call| {
        if (!may_pend.isSet(call.callee)) continue;
        const drive = driveAfter(store, &outside, call.caller, call.returns_unchanged);
        const updated = &store.getCFStmtPtr(call.stmt).assign_call;
        updated.drive = drive;
        // A call that runs pending calls afterwards keeps its frame to do so.
        if (drive != .none) updated.replaces_frame = false;
        if (drive == .unless_caller_drives) store.getProcSpecPtr(@enumFromInt(call.caller)).reads_caller_drives = true;
    }
}

const HandsUp = enum { never, always, when_runtime_called };

/// Whether a procedure may return with a call pending.
fn handsUp(store: *const LirStore, outside: *const std.bit_set.DynamicBitSetUnmanaged, proc_index: u32) HandsUp {
    const proc = store.getProcSpec(@enumFromInt(proc_index));
    return switch (proc.abi) {
        .erased_callable => .when_runtime_called,
        .roc => if (outside.isSet(proc_index)) .never else .always,
    };
}

fn driveAfter(
    store: *const LirStore,
    outside: *const std.bit_set.DynamicBitSetUnmanaged,
    proc_index: u32,
    returns_unchanged: bool,
) LIR.PendingDrive {
    if (!returns_unchanged) return .always;
    return switch (handsUp(store, outside, proc_index)) {
        .never => .always,
        .always => .none,
        .when_runtime_called => .unless_caller_drives,
    };
}

/// Whether the procedure returns `value` after `start` with nothing but
/// reference-count statements and jumps to a join's body in between.
fn returnsUnchanged(store: *const LirStore, join_bodies: []const ?LIR.CFStmtId, value: LIR.LocalId, start: LIR.CFStmtId) bool {
    var current = start;
    // Each jump enters a join body, and a body that jumps back to its own
    // join never returns, so more jumps than joins is such a cycle.
    var jumps: usize = 0;
    while (true) {
        const stmt = store.getCFStmt(current);
        if (stmt == .ret) return stmt.ret.value == value;
        if (stmt == .jump) {
            const join_index = @intFromEnum(stmt.jump.target);
            if (jumps == join_bodies.len or join_index >= join_bodies.len) return false;
            jumps += 1;
            current = join_bodies[join_index] orelse return false;
            continue;
        }
        current = if (stmt == .incref)
            stmt.incref.next
        else if (stmt == .decref)
            stmt.decref.next
        else if (stmt == .decref_if_initialized)
            stmt.decref_if_initialized.next
        else if (stmt == .free)
            stmt.free.next
        else
            return false;
    }
}

test "tail drive declarations are referenced" {
    std.testing.refAllDecls(@This());
}
