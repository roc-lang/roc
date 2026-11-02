//! Monomorphic indirect calls. All representation conversion is already LIR.
//! This module knows no layouts, descriptors, dictionaries, or procedure tables.
const std = @import("std");
const builtin = @import("builtin");
const erased = @import("erased_callable.zig");
const host = @import("in_process_host.zig");

const Pending = struct {
    callable: erased.CallableFnPtr,
    closure: [*]u8,
    args: []align(16) u8,
};
const State = struct {
    pending: ?Pending = null,
    driven: ?erased.CallableFnPtr = null,
};

// Freestanding Linux platforms do not install TLS. Keep their control-flow
// state keyed by the OS thread; wasm has one execution thread. Hosted targets
// use their native TLS. No program or type metadata is initialized here.
const Selection = if (builtin.cpu.arch.isWasm()) struct {
    var state: State = .{};
    fn get() *State {
        return &state;
    }
} else if (builtin.os.tag == .linux and !builtin.link_libc) struct {
    const Entry = struct { tid: std.os.linux.pid_t, state: State = .{}, next: ?*Entry };
    var entries: ?*Entry = null;
    var lock: std.atomic.Mutex = .unlocked;
    fn get() *State {
        const tid = std.os.linux.gettid();
        while (!lock.tryLock()) std.atomic.spinLoopHint();
        defer lock.unlock();
        var current = entries;
        while (current) |entry| : (current = entry.next) {
            if (entry.tid == tid) return &entry.state;
        }
        const entry = std.heap.page_allocator.create(Entry) catch host.ops().crash("out of memory allocating native callable thread state");
        entry.* = .{ .tid = tid, .next = entries };
        entries = entry;
        return &entry.state;
    }
} else struct {
    threadlocal var state: State = .{};
    fn get() *State {
        return &state;
    }
};

fn invoke(state: *State, callable: erased.CallableFnPtr, ret: ?[*]u8, args: ?[*]const u8, capture: ?[*]u8, reuse: ?[*]u8) void {
    state.driven = callable;
    var unused: ?*const anyopaque = null;
    callable(host.ops(), ret, args, capture, reuse, &unused);
    state.driven = null;
}

/// Ordinary indirect invocation with the producer's exact packed ABI.
pub fn call(callable: erased.CallableFnPtr, ret: ?[*]u8, args: ?[*]const u8, capture: ?[*]u8, reuse: ?[*]u8) void {
    const state = Selection.get();
    invoke(state, callable, ret, args, capture, reuse);
    drive(ret);
}

/// ARC transfers one closure reference and the arguments to this pending call.
pub fn deferCall(callable: erased.CallableFnPtr, closure: [*]u8, args: ?[*]const u8, args_len: usize) void {
    const state = Selection.get();
    std.debug.assert(state.pending == null);
    const ops = host.ops();
    const bytes: [*]align(16) u8 = @ptrCast(@alignCast(ops.allocRaw(@max(args_len, 1), 16)));
    if (args_len != 0) @memcpy(bytes[0..args_len], args.?[0..args_len]);
    state.pending = .{ .callable = callable, .closure = closure, .args = bytes[0..args_len] };
}

/// Tail-drive's result slot has the same concrete ABI throughout the chain.
pub fn drive(ret: ?[*]u8) void {
    const state = Selection.get();
    while (state.pending) |pending| {
        state.pending = null;
        invoke(state, pending.callable, ret, if (pending.args.len == 0) null else pending.args.ptr, erased.capturePtr(pending.closure), null);
        const ops = host.ops();
        // This is the ownership transfer explicitly requested by ARC's
        // deferred-call statement, not a backend-inferred capture release.
        erased.decref(pending.closure, ops);
        ops.dealloc(pending.args.ptr, 16);
    }
}

/// Consume the invocation marker before any nested call can replace it.
pub fn callerDrives(callee: erased.CallableFnPtr) u8 {
    const state = Selection.get();
    if (state.driven != callee) return 0;
    state.driven = null;
    return 1;
}

const TestCalls = struct {
    var drops: usize = 0;

    fn drop(_: ?[*]u8, _: *RocOps) callconv(.c) void {
        drops += 1;
    }

    const RocOps = erased.RocOps;

    fn step(ops: *RocOps, ret: ?[*]u8, args: ?[*]const u8, _: ?[*]u8, _: ?[*]u8, _: *?*const anyopaque) callconv(.c) void {
        std.debug.assert(callerDrives(&step) == 1);
        const remaining = std.mem.readInt(u64, args.?[0..8], .little);
        if (remaining == 0) {
            std.mem.writeInt(u64, ret.?[0..8], 42, .little);
            return;
        }
        const closure = erased.allocate(&step, &drop, 0, ops);
        var next: [8]u8 = undefined;
        std.mem.writeInt(u64, &next, remaining - 1, .little);
        deferCall(&step, closure, &next, next.len);
    }
};

test "native callable trampoline preserves arguments and releases every deferred closure" {
    var env = @import("utils.zig").TestEnv.init(std.testing.allocator);
    defer env.deinit();
    const saved = host.enter(env.getOps(), null);
    defer host.leave(saved);
    TestCalls.drops = 0;
    var args: [8]u8 = undefined;
    std.mem.writeInt(u64, &args, 10000, .little);
    var result: [8]u8 = undefined;
    call(&TestCalls.step, &result, &args, null, null);
    try std.testing.expectEqual(@as(u64, 42), std.mem.readInt(u64, &result, .little));
    try std.testing.expectEqual(@as(usize, 10000), TestCalls.drops);
    try std.testing.expectEqual(@as(usize, 0), env.getAllocationCount());
}
