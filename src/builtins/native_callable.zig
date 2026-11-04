//! Monomorphic indirect calls. All representation conversion is already LIR.
//! This module knows no layouts, descriptors, dictionaries, or procedure tables.
//!
//! An erased call in tail position is left pending: it is recorded and its
//! procedure returns, and whoever waits on that procedure makes the call. The
//! record lives in a `Pending` cell in the waiter's frame, passed down to the
//! procedures that can fill it. This module holds no state of its own: host
//! calls can run on several threads at once, and neither its target-neutral
//! bitcode nor a platform archive can name a thread (design.md "Pending Cells").
const std = @import("std");
const erased = @import("erased_callable.zig");
const host = @import("in_process_host.zig");

/// One waiter's pending call, in its frame. Empty when `callable` is null.
pub const Pending = extern struct {
    callable: ?erased.CallableFnPtr = null,
    closure: ?[*]u8 = null,
    args: ?[*]align(16) u8 = null,
    args_len: usize = 0,
    /// Nonzero when the call transfers the reference it owns to `closure`
    /// through the erased ABI's reuse channel (an owned deferred call), so
    /// the callee consumes it; zero when the driver releases it after the
    /// call.
    transfer: usize = 0,
    /// What a callee that writes the erased ABI's descriptor output writes.
    /// The tag on `invocation`'s address moves that word off its alignment,
    /// so it spans two of these.
    written_output: [2]usize = .{ 0, 0 },
};

/// The words of a `Pending`, for backends reserving one in a frame. Only
/// `callable` needs initializing.
pub const pending_words = @sizeOf(Pending) / @sizeOf(usize);

/// The erased ABI's last argument as this runtime passes it: an address in
/// `cell` with the low bit set, which a host's aligned output pointer never
/// has. The callee leaves its own pending call in `cell`.
fn invocation(cell: *Pending) usize {
    return @intFromPtr(&cell.written_output) | 1;
}

/// The ABI of `erased.CallableFnPtr` with its last argument by value: the
/// tagged word is not a pointer this runtime may form.
const InvokedFn = *const fn (*erased.RocOps, ?[*]u8, ?[*]const u8, ?[*]u8, ?[*]u8, usize) callconv(.c) void;

fn invoke(cell: *Pending, callable: erased.CallableFnPtr, ret: ?[*]u8, args: ?[*]const u8, capture: ?[*]u8, reuse: ?[*]u8) void {
    const invoked: InvokedFn = @ptrCast(callable);
    invoked(host.ops(), ret, args, capture, reuse, invocation(cell));
}

/// Ordinary indirect invocation with the producer's exact packed ABI. The
/// invocation is the hot path and stays inline at each call site; a pending
/// call the callee left behind is the cold path, driven out of line.
pub fn call(callable: erased.CallableFnPtr, ret: ?[*]u8, args: ?[*]const u8, capture: ?[*]u8, reuse: ?[*]u8) void {
    var pending: Pending = .{};
    invoke(&pending, callable, ret, args, capture, reuse);
    if (pending.callable != null) drivePending(&pending, ret);
}

/// ARC transfers one closure reference and the arguments to this pending
/// call; `transfer` says the driven call passes that reference on to the
/// callee as its reuse input.
pub fn deferCall(pending: *Pending, callable: erased.CallableFnPtr, closure: [*]u8, args: ?[*]const u8, args_len: usize, transfer: bool) void {
    std.debug.assert(pending.callable == null);
    // A call without arguments carries no argument bytes to copy out.
    var copied: ?[*]align(16) u8 = null;
    if (args_len != 0) {
        const bytes: [*]align(16) u8 = @ptrCast(@alignCast(host.ops().allocRaw(args_len, 16)));
        @memcpy(bytes[0..args_len], args.?[0..args_len]);
        copied = bytes;
    }
    pending.callable = callable;
    pending.closure = closure;
    pending.args = copied;
    pending.args_len = args_len;
    pending.transfer = @intFromBool(transfer);
}

/// Tail-drive's result slot has the same concrete ABI throughout the chain.
pub fn drive(pending: *Pending, ret: ?[*]u8) void {
    drivePending(pending, ret);
}

/// Run the chain of pending calls. A program makes one erased call per
/// literal argument and per call of a stored function, so a generic body
/// can hold thousands of call sites; the loop stays out of line so no site
/// carries a copy of it, and each site ends with the branch over it, which
/// keeps the body in blocks of one site each. LLVM's WebAssembly register
/// stackifier does work proportional to a block's length for each of a
/// register's uses in that block, and every stack-slot access in a function
/// uses its frame register, so a body kept in one block costs the square of
/// its size there.
noinline fn drivePending(pending: *Pending, ret: ?[*]u8) void {
    while (pending.callable) |callable| {
        pending.callable = null;
        const closure = pending.closure.?;
        const args = pending.args;
        const transfer = pending.transfer != 0;
        invoke(pending, callable, ret, args, erased.capturePtr(closure), if (transfer) closure else null);
        const ops = host.ops();
        // This is the ownership transfer explicitly requested by ARC's
        // deferred-call statement, not a backend-inferred capture release:
        // the callee consumed the reference an owned call passed it, and
        // the driver releases every other.
        if (!transfer) erased.decref(closure, ops);
        if (args) |bytes| ops.dealloc(bytes, 16);
    }
}

/// The cell of the waiter that made this invocation, from the erased ABI's
/// last argument. Null when anything but this runtime invoked the callee,
/// which then makes its own pending calls.
pub fn callerCell(out: ?*anyopaque) ?*Pending {
    const word = @intFromPtr(out);
    if (word & 1 == 0) return null;
    const written: *[2]usize = @ptrFromInt(word - 1);
    return @fieldParentPtr("written_output", written);
}

const TestCalls = struct {
    var drops: std.atomic.Value(usize) = .init(0);

    fn drop(_: ?[*]u8, _: *RocOps) callconv(.c) void {
        _ = drops.fetchAdd(1, .monotonic);
    }

    const RocOps = erased.RocOps;

    fn step(ops: *RocOps, ret: ?[*]u8, args: ?[*]const u8, _: ?[*]u8, _: ?[*]u8, out: *?*const anyopaque) callconv(.c) void {
        const pending = callerCell(@ptrCast(out)).?;
        const remaining = std.mem.readInt(u64, args.?[0..8], .little);
        if (remaining == 0) {
            std.mem.writeInt(u64, ret.?[0..8], 42, .little);
            return;
        }
        const closure = erased.allocate(&step, &drop, 0, ops);
        var next: [8]u8 = undefined;
        std.mem.writeInt(u64, &next, remaining - 1, .little);
        deferCall(pending, &step, closure, &next, next.len, false);
    }

    /// A host's callable, which writes the ABI's descriptor output.
    fn writesOutput(_: *RocOps, ret: ?[*]u8, _: ?[*]const u8, _: ?[*]u8, _: ?[*]u8, out: *?*const anyopaque) callconv(.c) void {
        std.mem.writeInt(u64, ret.?[0..8], 7, .little);
        out.* = @ptrFromInt(0xdead_bee8);
    }
};

test "native callable trampoline preserves arguments and releases every deferred closure" {
    var env = @import("utils.zig").TestEnv.init(std.testing.allocator);
    defer env.deinit();
    const saved = host.enter(env.getOps(), null);
    defer host.leave(saved);
    TestCalls.drops.store(0, .monotonic);
    var args: [8]u8 = undefined;
    std.mem.writeInt(u64, &args, 10000, .little);
    var result: [8]u8 = undefined;
    call(&TestCalls.step, &result, &args, null, null);
    try std.testing.expectEqual(@as(u64, 42), std.mem.readInt(u64, &result, .little));
    try std.testing.expectEqual(@as(usize, 10000), TestCalls.drops.load(.monotonic));
    try std.testing.expectEqual(@as(usize, 0), env.getAllocationCount());
}

test "a host's output word is no waiter's cell" {
    var word: ?*const anyopaque = null;
    try std.testing.expectEqual(@as(?*Pending, null), callerCell(@ptrCast(&word)));
    try std.testing.expectEqual(@as(?*Pending, null), callerCell(null));
}

test "a callable writing its descriptor output leaves the cell's pending call alone" {
    var env = @import("utils.zig").TestEnv.init(std.testing.allocator);
    defer env.deinit();
    const saved = host.enter(env.getOps(), null);
    defer host.leave(saved);
    var cell: Pending = .{};
    var result: [8]u8 = undefined;
    invoke(&cell, &TestCalls.writesOutput, &result, null, null, null);
    try std.testing.expectEqual(@as(?erased.CallableFnPtr, null), cell.callable);
    try std.testing.expectEqual(@as(?[*]u8, null), cell.closure);
    try std.testing.expectEqual(@as(?[*]align(16) u8, null), cell.args);
    try std.testing.expectEqual(@as(usize, 0), cell.args_len);
    try std.testing.expectEqual(@as(?*Pending, &cell), callerCell(@ptrFromInt(invocation(&cell))));
    try std.testing.expectEqual(@as(u64, 7), std.mem.readInt(u64, &result, .little));
}

/// One thread's chain of deferred calls, under its own host.
fn runChain(failed: *std.atomic.Value(bool)) void {
    var env = @import("utils.zig").TestEnv.init(std.heap.smp_allocator);
    defer env.deinit();
    const saved = host.enter(env.getOps(), null);
    defer host.leave(saved);
    var args: [8]u8 = undefined;
    std.mem.writeInt(u64, &args, 20000, .little);
    var result: [8]u8 = undefined;
    call(&TestCalls.step, &result, &args, null, null);
    if (std.mem.readInt(u64, &result, .little) != 42) failed.store(true, .monotonic);
}

test "threads driving deferred calls at once keep their chains apart" {
    var failed: std.atomic.Value(bool) = .init(false);
    var threads: [4]std.Thread = undefined;
    for (&threads) |*thread| thread.* = try std.Thread.spawn(.{}, runChain, .{&failed});
    for (threads) |thread| thread.join();
    try std.testing.expect(!failed.load(.monotonic));
}
