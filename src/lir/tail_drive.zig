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
    /// How this call's value reaches the caller's return, if it does.
    returns: Returns,
};

/// A descriptor local written by a call or after it, and the descriptor it
/// is another name for, when it is one.
const LateDescLocal = struct {
    local: LIR.LocalId,
    names: ?LIR.BoxyDescRef,
};

const Returns = union(enum) {
    /// Something other than returning it reads the value.
    no,
    /// Nothing but reference-count statements and jumps into a join body
    /// separates the call from the return of its value.
    unchanged,
    /// The value is returned after representation conversions only. The
    /// only reference counts adjusted in between are the value's own, so
    /// returning right after the call leaves nothing the frame owns behind.
    /// The payload is the descriptor the last conversion stores the value as.
    converted: ?LIR.BoxyDescRef,
};

const DeferredCall = struct {
    caller: u32,
    stmt: LIR.CFStmtId,
    /// How the deferred call's value reaches the caller's return.
    returns: Returns,
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
    var late_desc_locals = std.ArrayList(LateDescLocal).empty;
    defer late_desc_locals.deinit(allocator);
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
                    .returns = try returnOf(allocator, store, join_bodies.items, &late_desc_locals, stmt.assign_call),
                });
            } else if (stmt == .assign_call_erased) {
                if (stmt.assign_call_erased.deferred) try deferred_calls.append(allocator, .{
                    .caller = @intCast(proc_index),
                    .stmt = stmt_id,
                    .returns = try returnOf(allocator, store, join_bodies.items, &late_desc_locals, stmt.assign_call_erased),
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
        if (deferred.returns == .no) continue;
        if (handsUp(store, &outside, deferred.caller) != .never) may_pend.set(deferred.caller);
    }
    var changed = true;
    while (changed) {
        changed = false;
        for (direct_calls.items) |call| {
            if (may_pend.isSet(call.caller) or !may_pend.isSet(call.callee)) continue;
            if (handsUp(store, &outside, call.caller) == .never) continue;
            if (call.returns == .no) continue;
            may_pend.set(call.caller);
            changed = true;
        }
    }

    for (deferred_calls.items) |deferred| {
        const drive = driveAfter(store, &outside, deferred.caller, deferred.returns != .no);
        const updated = &store.getCFStmtPtr(deferred.stmt).assign_call_erased;
        updated.drive = drive;
        // The conversions that follow would read a value that is not there
        // while the call is pending, so the procedure returns before them.
        if (deferred.returns == .converted and drive.canLeavePending()) {
            updated.returns_pending = .{ .result_desc = deferred.returns.converted };
        }
        if (drive == .unless_caller_drives) store.getProcSpecPtr(@enumFromInt(deferred.caller)).reads_caller_drives = true;
    }
    for (direct_calls.items) |call| {
        if (!may_pend.isSet(call.callee)) continue;
        const drive = driveAfter(store, &outside, call.caller, call.returns != .no);
        const updated = &store.getCFStmtPtr(call.stmt).assign_call;
        updated.drive = drive;
        // Whatever is still pending once this statement has run its drive is
        // the caller's to make, before the conversions that follow read a
        // value that is not there yet.
        if (call.returns == .converted and drive.canLeavePending()) {
            updated.returns_pending = .{ .result_desc = call.returns.converted };
        }
        // A call that runs pending calls afterwards keeps its frame to do so.
        if (drive.canDriveHere()) updated.replaces_frame = false;
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
        .always => .handed_up,
        .when_runtime_called => .unless_caller_drives,
    };
}

/// How the procedure returns `value` after `start`. A conversion consumes the
/// value it converts, and a descriptor reference reads no value, so a path
/// made of those, and of reference counts on the value being converted, owns
/// nothing but the value on the way to the return.
fn returnOf(
    allocator: Allocator,
    store: *const LirStore,
    join_bodies: []const ?LIR.CFStmtId,
    late_desc_locals: *std.ArrayList(LateDescLocal),
    call: anytype,
) Allocator.Error!Returns {
    const value: LIR.LocalId = call.target;
    var current: LIR.CFStmtId = call.next;
    var returned = value;
    // The value the latest conversion consumed; its counts bracket that
    // conversion.
    var converted_from = value;
    var converted_to: ?LIR.BoxyDescRef = null;
    var converts = false;
    var counts_other = false;
    // Descriptor locals written by the call or after it. A procedure that
    // returns right after a call that left another pending has none of them,
    // so the last conversion's descriptor is named without them.
    late_desc_locals.clearRetainingCapacity();
    if (call.out_desc) |out_desc| try late_desc_locals.append(allocator, .{ .local = out_desc, .names = null });
    // Each jump enters a join body, and a body that jumps back to its own
    // join never returns, so more jumps than joins is such a cycle.
    var jumps: usize = 0;
    while (true) {
        const stmt = store.getCFStmt(current);
        if (stmt == .ret) {
            if (stmt.ret.value != returned) return .no;
            if (!converts) return .unchanged;
            if (counts_other) return .no;
            // Each late local names an earlier descriptor or none, so this
            // reaches a descriptor that exists before the call in at most
            // one step per late local.
            var stored_as = converted_to;
            var index = late_desc_locals.items.len;
            while (index > 0) {
                index -= 1;
                const late = late_desc_locals.items[index];
                const named = stored_as orelse break;
                if (named.localOrNull() != late.local) continue;
                stored_as = late.names orelse return .no;
            }
            return .{ .converted = stored_as };
        }
        if (stmt == .jump) {
            const join_index = @intFromEnum(stmt.jump.target);
            if (jumps == join_bodies.len or join_index >= join_bodies.len) return .no;
            jumps += 1;
            current = join_bodies[join_index] orelse return .no;
            continue;
        }
        if (stmt == .assign_boxy_adapt) {
            const adapt = stmt.assign_boxy_adapt;
            if (adapt.source != returned or adapt.source_mode != .move) return .no;
            converted_from = returned;
            returned = adapt.target;
            converted_to = adapt.target_desc;
            converts = true;
            current = adapt.next;
            continue;
        }
        if (stmt == .assign_boxy_desc_ref) {
            const desc_ref = stmt.assign_boxy_desc_ref;
            const names_whole_descriptor = desc_ref.nested_index == null and
                desc_ref.box_payload_layout == null and
                desc_ref.tag_payload == null and
                !desc_ref.tag_ext and
                desc_ref.tag_residual_for == null and
                desc_ref.captures.len == 0;
            try late_desc_locals.append(allocator, .{
                .local = desc_ref.target,
                .names = if (names_whole_descriptor) desc_ref.desc else null,
            });
            current = stmt.assign_boxy_desc_ref.next;
            continue;
        }
        var counted: LIR.LocalId = undefined;
        if (stmt == .incref) {
            counted = stmt.incref.value;
            current = stmt.incref.next;
        } else if (stmt == .decref) {
            counted = stmt.decref.value;
            current = stmt.decref.next;
        } else if (stmt == .decref_if_initialized) {
            counted = stmt.decref_if_initialized.value;
            current = stmt.decref_if_initialized.next;
        } else if (stmt == .free) {
            counted = stmt.free.value;
            current = stmt.free.next;
        } else {
            return .no;
        }
        if (counted != returned and counted != converted_from) counts_other = true;
    }
}

test "tail drive declarations are referenced" {
    std.testing.refAllDecls(@This());
}
