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

/// The words of argument bytes a `Pending` holds itself. A pending call with
/// more copies them to the heap.
pub const inline_args_words = 16;
const inline_args_bytes = inline_args_words * @sizeOf(usize);

/// The alignment a packed argument block may need: that of its most aligned
/// argument.
const args_alignment = 16;

/// One waiter's pending call, in its frame. Empty when `callable` is null.
///
/// Every field is a word, so the cell has the same number of words on every
/// target. A backend reserving one zeroes its first `zeroed_words` words.
pub const Pending = extern struct {
    callable: ?erased.CallableFnPtr = null,
    /// A heap block no call reads, `spare_capacity` bytes long, which the
    /// next argument block too large for `inline_args` is copied to. A chain
    /// of such calls reuses the blocks it frees, and `drive` frees this one
    /// when its chain ends.
    spare: ?[*]u8 = null,
    spare_capacity: usize = 0,
    closure: ?[*]u8 = null,
    /// The call's argument bytes: `inline_args` when they fit there, a heap
    /// block of `args_capacity` bytes otherwise, null when the call has none.
    args: ?[*]u8 = null,
    args_len: usize = 0,
    args_capacity: usize = 0,
    /// Nonzero when the call transfers the reference it owns to `closure`
    /// through the erased ABI's reuse channel (an owned deferred call), so
    /// the callee consumes it; zero when the driver releases it after the
    /// call.
    transfer: usize = 0,
    /// What a callee that writes the erased ABI's descriptor output writes.
    /// The tag on `invocation`'s address moves that word off its alignment,
    /// so it spans two of these.
    written_output: [2]usize = .{ 0, 0 },
    /// Only `drive` reads these, copying them out before it makes the call,
    /// so they need no more than word alignment.
    inline_args: [inline_args_words]usize = undefined,
};

/// The words of a `Pending`, for backends reserving one in a frame.
pub const pending_words = @sizeOf(Pending) / @sizeOf(usize);

/// The leading words of a `Pending` a backend zeroes when it reserves one:
/// `callable` and `spare`. The rest is written before it is read.
pub const zeroed_words = 2;

comptime {
    std.debug.assert(@offsetOf(Pending, "callable") == 0);
    std.debug.assert(@offsetOf(Pending, "spare") == @sizeOf(usize));
    std.debug.assert(@sizeOf(Pending) % @sizeOf(usize) == 0);
}

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
    var copied: ?[*]u8 = null;
    var capacity: usize = 0;
    if (args_len != 0) {
        const bytes: [*]u8 = if (args_len <= inline_args_bytes)
            @ptrCast(&pending.inline_args)
        else heap: {
            if (pending.spare) |spare| {
                pending.spare = null;
                if (pending.spare_capacity >= args_len) {
                    capacity = pending.spare_capacity;
                    break :heap spare;
                }
                host.ops().dealloc(spare, args_alignment);
            }
            capacity = args_len;
            break :heap @ptrCast(host.ops().allocRaw(args_len, args_alignment));
        };
        // Nothing is ever called with the cell's own bytes or its spare
        // block as its arguments (`drive` copies inline bytes out first, and
        // a heap block becomes spare only once its call returns), so these
        // never overlap.
        @memcpy(bytes[0..args_len], args.?[0..args_len]);
        copied = bytes;
    }
    pending.callable = callable;
    pending.closure = closure;
    pending.args = copied;
    pending.args_len = args_len;
    pending.args_capacity = capacity;
    pending.transfer = @intFromBool(transfer);
}

/// Keep `block`, which no call reads any more, as `pending`'s spare when it
/// is the larger, and free the other.
fn keepSpare(pending: *Pending, block: [*]u8, capacity: usize) void {
    if (pending.spare) |spare| {
        if (pending.spare_capacity >= capacity) {
            host.ops().dealloc(block, args_alignment);
            return;
        }
        host.ops().dealloc(spare, args_alignment);
    }
    pending.spare = block;
    pending.spare_capacity = capacity;
}

/// Tail-drive's result slot has the same concrete ABI throughout the chain.
///
/// A callee may read its arguments for as long as it runs: a dynamic argument
/// stays at the address it arrived at. So each call's arguments live outside
/// the cell until it returns, in this frame or in their heap block, leaving
/// the cell free for the call it leaves pending, and for any chain driven in
/// this same cell before it returns. A heap block becomes the cell's spare
/// once its call returns, for the next large block in the chain.
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
    var inline_copy: [inline_args_bytes]u8 align(args_alignment) = undefined;
    while (pending.callable) |callable| {
        pending.callable = null;
        const closure = pending.closure.?;
        const args_len = pending.args_len;
        const capacity = pending.args_capacity;
        const transfer = pending.transfer != 0;
        const args: ?[*]u8 = if (args_len == 0)
            null
        else if (args_len <= inline_args_bytes) copy: {
            @memcpy(inline_copy[0..args_len], pending.args.?[0..args_len]);
            break :copy &inline_copy;
        } else pending.args;
        invoke(pending, callable, ret, args, erased.capturePtr(closure), if (transfer) closure else null);
        // This is the ownership transfer explicitly requested by ARC's
        // deferred-call statement, not a backend-inferred capture release:
        // the callee consumed the reference an owned call passed it, and
        // the driver releases every other.
        if (!transfer) erased.decref(closure, host.ops());
        if (args_len > inline_args_bytes) keepSpare(pending, args.?, capacity);
    }
    if (pending.spare) |spare| {
        pending.spare = null;
        host.ops().dealloc(spare, args_alignment);
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

    // Match the runtime's tagged-word invocation ABI. Declaring an aligned
    // output pointer here would incorrectly promise its low bit is zero.
    fn step(ops: *RocOps, ret: ?[*]u8, args: ?[*]const u8, _: ?[*]u8, _: ?[*]u8, out: usize) callconv(.c) void {
        const pending = callerCell(@ptrFromInt(out)).?;
        const remaining = std.mem.readInt(u64, args.?[0..8], .little);
        if (remaining == 0) {
            std.mem.writeInt(u64, ret.?[0..8], 42, .little);
            return;
        }
        const closure = erased.allocate(@ptrCast(&step), &drop, 0, ops);
        var next: [8]u8 = undefined;
        std.mem.writeInt(u64, &next, remaining - 1, .little);
        deferCall(pending, @ptrCast(&step), closure, &next, next.len, false);
    }

    /// Like `step`, with an argument block too large for the cell, whose
    /// every word must arrive intact.
    fn stepWide(ops: *RocOps, ret: ?[*]u8, args: ?[*]const u8, _: ?[*]u8, _: ?[*]u8, out: usize) callconv(.c) void {
        const pending = callerCell(@ptrFromInt(out)).?;
        const remaining = std.mem.readInt(u64, args.?[0..8], .little);
        for (1..wide_words) |i| {
            if (std.mem.readInt(u64, args.?[i * 8 ..][0..8], .little) != remaining +% i) {
                std.mem.writeInt(u64, ret.?[0..8], 0, .little);
                return;
            }
        }
        if (remaining == 0) {
            std.mem.writeInt(u64, ret.?[0..8], 42, .little);
            return;
        }
        const closure = erased.allocate(@ptrCast(&stepWide), &drop, 0, ops);
        var next: [wide_words * 8]u8 align(16) = undefined;
        wideArgs(&next, remaining - 1);
        deferCall(pending, @ptrCast(&stepWide), closure, &next, next.len, false);
    }

    const wide_words = inline_args_bytes / 8 + 3;

    /// Like `stepWide`, with an argument block whose length cycles through
    /// three sizes too large for the cell. Its second word is its length in
    /// words.
    fn stepVaried(ops: *RocOps, ret: ?[*]u8, args: ?[*]const u8, _: ?[*]u8, _: ?[*]u8, out: usize) callconv(.c) void {
        const pending = callerCell(@ptrFromInt(out)).?;
        const remaining = std.mem.readInt(u64, args.?[0..8], .little);
        const words = std.mem.readInt(u64, args.?[8..16], .little);
        if (words != variedWords(remaining)) {
            std.mem.writeInt(u64, ret.?[0..8], 0, .little);
            return;
        }
        for (2..words) |i| {
            if (std.mem.readInt(u64, args.?[i * 8 ..][0..8], .little) != remaining +% i) {
                std.mem.writeInt(u64, ret.?[0..8], 0, .little);
                return;
            }
        }
        if (remaining == 0) {
            std.mem.writeInt(u64, ret.?[0..8], 42, .little);
            return;
        }
        const closure = erased.allocate(@ptrCast(&stepVaried), &drop, 0, ops);
        var next: [varied_max_words * 8]u8 align(16) = undefined;
        const len = variedArgs(&next, remaining - 1);
        deferCall(pending, @ptrCast(&stepVaried), closure, &next, len, false);
    }

    const varied_max_words = wide_words + 2 * 7;

    fn variedWords(remaining: u64) u64 {
        return wide_words + (remaining % 3) * 7;
    }

    /// Fills `bytes` for `stepVaried` and returns how many it used.
    fn variedArgs(bytes: *[varied_max_words * 8]u8, remaining: u64) usize {
        const words = variedWords(remaining);
        std.mem.writeInt(u64, bytes[0..8], remaining, .little);
        std.mem.writeInt(u64, bytes[8..16], words, .little);
        for (2..words) |i| std.mem.writeInt(u64, bytes[i * 8 ..][0..8], remaining +% i, .little);
        return words * 8;
    }

    fn wideArgs(bytes: *[wide_words * 8]u8, remaining: u64) void {
        for (0..wide_words) |i| std.mem.writeInt(u64, bytes[i * 8 ..][0..8], remaining +% i, .little);
    }

    var forwards: std.atomic.Value(usize) = .init(0);
    var overwritten: std.atomic.Value(bool) = .init(false);

    /// Leaves a call to itself pending with its own incoming arguments, and
    /// reads them again afterwards, as a callee does with a dynamic argument,
    /// until it has run 100 times.
    fn forward(ops: *RocOps, ret: ?[*]u8, args: ?[*]const u8, _: ?[*]u8, _: ?[*]u8, out: usize) callconv(.c) void {
        const pending = callerCell(@ptrFromInt(out)).?;
        if (forwards.fetchAdd(1, .monotonic) + 1 == 100) {
            @memcpy(ret.?[0..16], args.?[0..16]);
            return;
        }
        var before: [16]u8 = undefined;
        @memcpy(&before, args.?[0..16]);
        const closure = erased.allocate(@ptrCast(&forward), &drop, 0, ops);
        deferCall(pending, @ptrCast(&forward), closure, args, 16, false);
        if (!std.mem.eql(u8, &before, args.?[0..16])) overwritten.store(true, .monotonic);
    }

    /// Drives a chain of its own in the cell it was invoked from, as a
    /// procedure handed its caller's cell does, then reads its arguments.
    fn nested(ops: *RocOps, ret: ?[*]u8, args: ?[*]const u8, _: ?[*]u8, _: ?[*]u8, out: usize) callconv(.c) void {
        const pending = callerCell(@ptrFromInt(out)).?;
        var before: [16]u8 = undefined;
        @memcpy(&before, args.?[0..16]);
        const closure = erased.allocate(@ptrCast(&step), &drop, 0, ops);
        var chain: [8]u8 = undefined;
        std.mem.writeInt(u64, &chain, 5, .little);
        deferCall(pending, @ptrCast(&step), closure, &chain, chain.len, false);
        var chain_result: [8]u8 = undefined;
        drive(pending, &chain_result);
        if (!std.mem.eql(u8, &before, args.?[0..16])) overwritten.store(true, .monotonic);
        @memcpy(ret.?[0..8], &chain_result);
    }

    /// Leaves `nested` pending with 16 argument bytes.
    fn deferNested(ops: *RocOps, _: ?[*]u8, args: ?[*]const u8, _: ?[*]u8, _: ?[*]u8, out: usize) callconv(.c) void {
        const pending = callerCell(@ptrFromInt(out)).?;
        const closure = erased.allocate(@ptrCast(&nested), &drop, 0, ops);
        deferCall(pending, @ptrCast(&nested), closure, args, 16, false);
    }

    /// Like `nested`, with argument blocks too large for the cell: the
    /// chain it drives in its caller's cell takes and returns that cell's
    /// spare block while this callee still reads its own.
    fn nestedWide(ops: *RocOps, ret: ?[*]u8, args: ?[*]const u8, _: ?[*]u8, _: ?[*]u8, out: usize) callconv(.c) void {
        const pending = callerCell(@ptrFromInt(out)).?;
        var before: [wide_words * 8]u8 = undefined;
        @memcpy(&before, args.?[0 .. wide_words * 8]);
        const remaining = std.mem.readInt(u64, args.?[0..8], .little);
        const closure = erased.allocate(@ptrCast(&stepWide), &drop, 0, ops);
        var chain: [wide_words * 8]u8 align(16) = undefined;
        wideArgs(&chain, 50);
        deferCall(pending, @ptrCast(&stepWide), closure, &chain, chain.len, false);
        var chain_result: [8]u8 = undefined;
        drive(pending, &chain_result);
        if (!std.mem.eql(u8, &before, args.?[0 .. wide_words * 8])) overwritten.store(true, .monotonic);
        if (remaining == 0) {
            @memcpy(ret.?[0..8], &chain_result);
            return;
        }
        const next_closure = erased.allocate(@ptrCast(&nestedWide), &drop, 0, ops);
        var next: [wide_words * 8]u8 align(16) = undefined;
        wideArgs(&next, remaining - 1);
        deferCall(pending, @ptrCast(&nestedWide), next_closure, &next, next.len, false);
    }

    /// Leaves `nestedWide` pending with its own arguments.
    fn deferNestedWide(ops: *RocOps, _: ?[*]u8, args: ?[*]const u8, _: ?[*]u8, _: ?[*]u8, out: usize) callconv(.c) void {
        const pending = callerCell(@ptrFromInt(out)).?;
        const closure = erased.allocate(@ptrCast(&nestedWide), &drop, 0, ops);
        deferCall(pending, @ptrCast(&nestedWide), closure, args, wide_words * 8, false);
    }

    /// A host's callable, which writes the ABI's descriptor output.
    fn writesOutput(_: *RocOps, ret: ?[*]u8, _: ?[*]const u8, _: ?[*]u8, _: ?[*]u8, out: usize) callconv(.c) void {
        std.mem.writeInt(u64, ret.?[0..8], 7, .little);
        const output: *align(1) ?*const anyopaque = @ptrFromInt(out);
        output.* = @ptrFromInt(0xdead_bee8);
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
    call(@ptrCast(&TestCalls.step), &result, &args, null, null);
    try std.testing.expectEqual(@as(u64, 42), std.mem.readInt(u64, &result, .little));
    try std.testing.expectEqual(@as(usize, 10000), TestCalls.drops.load(.monotonic));
    try std.testing.expectEqual(@as(usize, 0), env.getAllocationCount());
}

test "native callable trampoline copies an argument block too large for the cell to the heap and frees it" {
    var env = @import("utils.zig").TestEnv.init(std.testing.allocator);
    defer env.deinit();
    const saved = host.enter(env.getOps(), null);
    defer host.leave(saved);
    TestCalls.drops.store(0, .monotonic);
    var args: [TestCalls.wide_words * 8]u8 align(16) = undefined;
    TestCalls.wideArgs(&args, 1000);
    var result: [8]u8 = undefined;
    call(@ptrCast(&TestCalls.stepWide), &result, &args, null, null);
    try std.testing.expectEqual(@as(u64, 42), std.mem.readInt(u64, &result, .little));
    try std.testing.expectEqual(@as(usize, 1000), TestCalls.drops.load(.monotonic));
    try std.testing.expectEqual(@as(usize, 0), env.getAllocationCount());
    // One closure per pending call, and two argument blocks for the whole
    // chain: the one a call is reading and the one it leaves pending.
    try std.testing.expectEqual(@as(usize, 1000 + 2), env.total_allocations);
}

test "a chain of argument blocks of varying sizes reuses its heap blocks, growing them only for a larger block" {
    var env = @import("utils.zig").TestEnv.init(std.testing.allocator);
    defer env.deinit();
    const saved = host.enter(env.getOps(), null);
    defer host.leave(saved);
    TestCalls.drops.store(0, .monotonic);
    var args: [TestCalls.varied_max_words * 8]u8 align(16) = undefined;
    _ = TestCalls.variedArgs(&args, 300);
    var result: [8]u8 = undefined;
    call(@ptrCast(&TestCalls.stepVaried), &result, &args, null, null);
    try std.testing.expectEqual(@as(u64, 42), std.mem.readInt(u64, &result, .little));
    try std.testing.expectEqual(@as(usize, 300), TestCalls.drops.load(.monotonic));
    try std.testing.expectEqual(@as(usize, 0), env.getAllocationCount());
    // Sizes cycle through three lengths, so after the first cycle the two
    // blocks the chain holds are both the largest.
    try std.testing.expect(env.total_allocations <= 300 + 6);
}

test "a chain of large argument blocks driven in the cell a callee was invoked from leaves the callee's arguments intact" {
    var env = @import("utils.zig").TestEnv.init(std.testing.allocator);
    defer env.deinit();
    const saved = host.enter(env.getOps(), null);
    defer host.leave(saved);
    TestCalls.drops.store(0, .monotonic);
    TestCalls.overwritten.store(false, .monotonic);
    var args: [TestCalls.wide_words * 8]u8 align(16) = undefined;
    TestCalls.wideArgs(&args, 3);
    var result: [8]u8 = undefined;
    call(@ptrCast(&TestCalls.deferNestedWide), &result, &args, null, null);
    try std.testing.expectEqual(@as(u64, 42), std.mem.readInt(u64, &result, .little));
    try std.testing.expect(!TestCalls.overwritten.load(.monotonic));
    try std.testing.expectEqual(@as(usize, 0), env.getAllocationCount());
}

test "a callee leaving a pending call with its own incoming arguments keeps them intact" {
    var env = @import("utils.zig").TestEnv.init(std.testing.allocator);
    defer env.deinit();
    const saved = host.enter(env.getOps(), null);
    defer host.leave(saved);
    TestCalls.drops.store(0, .monotonic);
    TestCalls.forwards.store(0, .monotonic);
    TestCalls.overwritten.store(false, .monotonic);
    var args: [16]u8 align(16) = undefined;
    for (&args, 0..) |*byte, i| byte.* = @intCast(i + 1);
    var result: [16]u8 = undefined;
    call(@ptrCast(&TestCalls.forward), &result, &args, null, null);
    try std.testing.expectEqualSlices(u8, &args, &result);
    try std.testing.expect(!TestCalls.overwritten.load(.monotonic));
    try std.testing.expectEqual(@as(usize, 99), TestCalls.drops.load(.monotonic));
    try std.testing.expectEqual(@as(usize, 0), env.getAllocationCount());
}

test "a chain driven in the cell a callee was invoked from leaves the callee's arguments intact" {
    var env = @import("utils.zig").TestEnv.init(std.testing.allocator);
    defer env.deinit();
    const saved = host.enter(env.getOps(), null);
    defer host.leave(saved);
    TestCalls.drops.store(0, .monotonic);
    TestCalls.overwritten.store(false, .monotonic);
    var args: [16]u8 align(16) = undefined;
    for (&args, 0..) |*byte, i| byte.* = @intCast(i + 1);
    var result: [8]u8 = undefined;
    call(@ptrCast(&TestCalls.deferNested), &result, &args, null, null);
    try std.testing.expectEqual(@as(u64, 42), std.mem.readInt(u64, &result, .little));
    try std.testing.expect(!TestCalls.overwritten.load(.monotonic));
    try std.testing.expectEqual(@as(usize, 7), TestCalls.drops.load(.monotonic));
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
    invoke(&cell, @ptrCast(&TestCalls.writesOutput), &result, null, null, null);
    try std.testing.expectEqual(@as(?erased.CallableFnPtr, null), cell.callable);
    try std.testing.expectEqual(@as(?[*]u8, null), cell.closure);
    try std.testing.expectEqual(@as(?[*]u8, null), cell.args);
    try std.testing.expectEqual(@as(usize, 0), cell.args_len);
    try std.testing.expectEqual(@as(?[*]u8, null), cell.spare);
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
    call(@ptrCast(&TestCalls.step), &result, &args, null, null);
    if (std.mem.readInt(u64, &result, .little) != 42) failed.store(true, .monotonic);
}

test "threads driving deferred calls at once keep their chains apart" {
    var failed: std.atomic.Value(bool) = .init(false);
    var threads: [4]std.Thread = undefined;
    for (&threads) |*thread| thread.* = try std.Thread.spawn(.{}, runChain, .{&failed});
    for (threads) |thread| thread.join();
    try std.testing.expect(!failed.load(.monotonic));
}

const PacketTest = struct {
    fn read(ops: *erased.RocOps, ret: ?[*]u8, args: ?[*]const u8, _: ?[*]u8, reuse: ?[*]u8, out: usize) callconv(.c) void {
        if (reuse) |owned| erased.decref(owned, ops);
        const bytes = args.?;
        const len: usize = bytes[0];
        const pending = callerCell(@ptrFromInt(out)).?;
        // Cross the representation boundary in both directions. Reading
        // the current packet AFTER deferral catches aliasing of active and
        // pending storage, even when both packets fit inline.
        if (len > 1) {
            var next: [97]u8 = undefined;
            const next_len: usize = if (len == 97) 33 else if (len == 33) 32 else if (len == 32) 31 else 1;
            @memset(next[0..next_len], @intCast(next_len));
            const closure = erased.allocate(@ptrCast(&read), &TestCalls.drop, 0, ops);
            deferCall(pending, @ptrCast(&read), closure, &next, next_len, false);
        }
        if (len == 33) {
            var nested: [8]u8 = undefined;
            call(@ptrCast(&TestCalls.writesOutput), &nested, null, null, null);
            if (std.mem.readInt(u64, &nested, .little) != 7) {
                ret.?[0] = 0;
                return;
            }
        }
        for (bytes[0..len]) |byte| {
            if (byte != len) {
                ret.?[0] = 0;
                return;
            }
        }
        ret.?[0] += 1;
    }
};

test "native callable packets preserve active bytes across inline and heap transitions" {
    var env = @import("utils.zig").TestEnv.init(std.testing.allocator);
    defer env.deinit();
    const saved = host.enter(env.getOps(), null);
    defer host.leave(saved);
    for ([_]usize{ 1, 31, 32, 33, 97 }, [_]u8{ 1, 2, 3, 4, 5 }) |len, expected| {
        var cell: Pending = .{};
        var source: [97]u8 = undefined;
        @memset(source[0..len], @intCast(len));
        const closure = erased.allocate(@ptrCast(&PacketTest.read), &TestCalls.drop, 0, env.getOps());
        const before = env.getAllocationCount();
        deferCall(&cell, @ptrCast(&PacketTest.read), closure, &source, len, false);
        try std.testing.expectEqual(before + @intFromBool(len > inline_args_bytes), env.getAllocationCount());
        @memset(&source, 0); // Deferral owns the bytes, not the source address.
        var result = [_]u8{0};
        drive(&cell, &result);
        try std.testing.expectEqual(expected, result[0]);
        try std.testing.expectEqual(@as(usize, 0), env.getAllocationCount());
        try std.testing.expectEqual(@as(?erased.CallableFnPtr, null), cell.callable);
    }
}

test "native callable empty packet makes no argument allocation" {
    var env = @import("utils.zig").TestEnv.init(std.testing.allocator);
    defer env.deinit();
    const saved = host.enter(env.getOps(), null);
    defer host.leave(saved);
    var cell: Pending = .{};
    const closure = erased.allocate(@ptrCast(&TestCalls.writesOutput), &TestCalls.drop, 0, env.getOps());
    const before = env.getAllocationCount();
    deferCall(&cell, @ptrCast(&TestCalls.writesOutput), closure, null, 0, false);
    try std.testing.expectEqual(before, env.getAllocationCount());
    var result: [8]u8 = undefined;
    drive(&cell, &result);
    try std.testing.expectEqual(@as(u64, 7), std.mem.readInt(u64, &result, .little));
    try std.testing.expectEqual(@as(usize, 0), env.getAllocationCount());
}

test "native callable inline packet preserves explicit closure ownership transfer" {
    var env = @import("utils.zig").TestEnv.init(std.testing.allocator);
    defer env.deinit();
    const saved = host.enter(env.getOps(), null);
    defer host.leave(saved);
    var cell: Pending = .{};
    const closure = erased.allocate(@ptrCast(&PacketTest.read), &TestCalls.drop, 0, env.getOps());
    const args = [_]u8{1};
    deferCall(&cell, @ptrCast(&PacketTest.read), closure, &args, args.len, true);
    var result = [_]u8{0};
    drive(&cell, &result);
    try std.testing.expectEqual(@as(u8, 1), result[0]);
    try std.testing.expectEqual(@as(usize, 0), env.getAllocationCount());
}
