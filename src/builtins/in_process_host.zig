//! The compiler's own host for the Roc code it runs in-process.
//!
//! Compiled Roc code reaches its host the same way everywhere: through the
//! fixed runtime symbols (`roc_alloc`, `roc_crashed`, and the rest of
//! `shim_symbols.runtime_set`) and through hosted-function symbols. A platform
//! host defines those symbols once per process. The compiler runs many
//! evaluations in one process, on several threads at once, each with its own
//! allocator, crash target, and diagnostic sinks, so it defines the symbols
//! here as forwarders to whichever `RocOps` the current thread has entered.
//! Generated code never carries a host pointer: the dev backend embeds these
//! forwarders' addresses, object code and the shim resolve them by name, and
//! an LLVM in-process library reaches them through a table its loader fills.
//!
//! Only an in-process host root compiles the exports, and it compiles them
//! weak, so a platform host object linked into the same binary (a test host,
//! for instance) supplies the definitions with its strong ones. A platform
//! archive or shim declares `pub const roc_host_role = .platform;` at its
//! root and compiles no definition at all: a weak definition inside an
//! archive member would satisfy references before the host's member was
//! pulled in.

const host_abi = @import("host_abi.zig");
const shim_symbols = @import("shim_symbols.zig");

const RocOps = host_abi.RocOps;

/// An observer for a source-level `expect` executed under a test host:
/// the ops the evaluation entered, the expect site, and whether it passed.
pub const ExpectObserver = *const fn (*RocOps, u32, u8) callconv(.c) void;

/// The symbol under which a test host observes executed `expect`s.
pub const roc_expect_observed = "roc_expect_observed";

/// The symbol through which a top-level `expect` whose `?` evaluated an
/// `Err` records the `?` expression's source region before it crashes.
pub const roc_expect_err_region = shim_symbols.roc_expect_err_region;

/// Byte offsets into the failing module's source for a `?` expression.
pub const ExpectErrRegion = struct {
    start: u32,
    end: u32,
};

/// Records the `?` expression's source region before the expect crashes.
pub const ExpectErrRegionRecorder = *const fn (u32, u32) callconv(.c) void;

threadlocal var current_ops: ?*RocOps = null;
threadlocal var current_expect_observer: ?ExpectObserver = null;
threadlocal var last_expect_err_region: ?ExpectErrRegion = null;

/// What `enter` displaced; `leave` restores it.
pub const Saved = struct {
    ops: ?*RocOps,
    expect_observer: ?ExpectObserver,
};

/// Make `ops` the host of every Roc call on this thread until `leave`. An
/// evaluation that starts another evaluation nests: the inner one enters,
/// runs, and leaves, restoring the outer one.
pub fn enter(roc_ops: *RocOps, expect_observer: ?ExpectObserver) Saved {
    const saved = Saved{ .ops = current_ops, .expect_observer = current_expect_observer };
    current_ops = roc_ops;
    current_expect_observer = expect_observer;
    return saved;
}

/// Restore what `enter` displaced.
pub fn leave(saved: Saved) void {
    current_ops = saved.ops;
    current_expect_observer = saved.expect_observer;
}

/// The ops of the evaluation this thread is inside, if any. A platform never
/// enters one, and never touches the thread-local: it may run before any
/// thread-local storage exists.
pub fn current() ?*RocOps {
    if (host_abi.host_role == .platform) return null;
    return current_ops;
}

/// The ops a builtin called from compiled code uses: the entered evaluation's
/// in-process, or in a platform the process-wide table over the host's own
/// runtime symbols. Callers may invoke these operations, not modify the table.
pub fn ops() *RocOps {
    return current() orelse @constCast(&symbol_backed_ops);
}

/// Return and clear the `?` region recorded by the most recent
/// `roc_expect_err_region` call on this thread. The harness reads it back
/// after the crash unwinds to point its failure report at the `?` expression.
pub fn takeExpectErrRegion() ?ExpectErrRegion {
    if (host_abi.host_role == .platform) return null;
    const region = last_expect_err_region;
    last_expect_err_region = null;
    return region;
}

/// The in-process recorder compiled code calls with a `?` region. A platform
/// never reads the region back, so it has no recorder.
pub fn expectErrRegionRecorder() ?ExpectErrRegionRecorder {
    if (host_abi.host_role == .platform) return null;
    return &rocExpectErrRegion;
}

fn requireOps() *RocOps {
    // Entering a host is a compiler precondition, not a Roc crash: without
    // an entered host there is no crash handler to notify. Trap even in
    // optimized builds rather than continuing with an invalid host.
    return current_ops orelse @trap();
}

fn symbolAlloc(_: *RocOps, length: usize, alignment: usize) callconv(.c) ?*anyopaque {
    return host_abi.extern_host.roc_alloc(length, alignment);
}

fn symbolDealloc(_: *RocOps, ptr: *anyopaque, alignment: usize) callconv(.c) void {
    host_abi.extern_host.roc_dealloc(ptr, alignment);
}

fn symbolRealloc(_: *RocOps, ptr: *anyopaque, new_length: usize, alignment: usize) callconv(.c) ?*anyopaque {
    return host_abi.extern_host.roc_realloc(ptr, new_length, alignment);
}

fn symbolDbg(_: *RocOps, bytes: [*]const u8, len: usize) callconv(.c) void {
    host_abi.extern_host.roc_dbg(bytes, len);
}

fn symbolExpectFailed(_: *RocOps, bytes: [*]const u8, len: usize) callconv(.c) void {
    host_abi.extern_host.roc_expect_failed(bytes, len);
}

fn symbolCrashed(_: *RocOps, bytes: [*]const u8, len: usize) callconv(.c) void {
    host_abi.extern_host.roc_crashed(bytes, len);
}

/// An immutable adapter over runtime symbols, so compiled builtins can resolve
/// its callbacks statically. RocOps uses mutable self pointers for host-owned
/// state, but these callbacks ignore self and never mutate the adapter.
const symbol_backed_ops: RocOps = .{
    .env = @ptrCast(@constCast(&symbol_backed_ops)),
    .roc_alloc = &symbolAlloc,
    .roc_dealloc = &symbolDealloc,
    .roc_realloc = &symbolRealloc,
    .roc_dbg = &symbolDbg,
    .roc_expect_failed = &symbolExpectFailed,
    .roc_crashed = &symbolCrashed,
    .hosted_fns = .{ .count = 0, .fns = &no_hosted_fns },
};

fn missingHostedFn(_: *anyopaque, _: *anyopaque, _: *anyopaque) callconv(.c) void {}

var no_hosted_fns: [1]host_abi.HostedFn = .{host_abi.hostedFn(&missingHostedFn)};

fn rocAlloc(length: usize, alignment: usize) callconv(.c) ?*anyopaque {
    const o = requireOps();
    return o.roc_alloc(o, length, alignment);
}

fn rocDealloc(ptr: *anyopaque, alignment: usize) callconv(.c) void {
    const o = requireOps();
    o.roc_dealloc(o, ptr, alignment);
}

fn rocRealloc(ptr: *anyopaque, new_length: usize, alignment: usize) callconv(.c) ?*anyopaque {
    const o = requireOps();
    return o.roc_realloc(o, ptr, new_length, alignment);
}

fn rocDbg(bytes: [*]const u8, len: usize) callconv(.c) void {
    const o = requireOps();
    o.roc_dbg(o, bytes, len);
}

fn rocExpectFailed(bytes: [*]const u8, len: usize) callconv(.c) void {
    const o = requireOps();
    o.roc_expect_failed(o, bytes, len);
}

fn rocCrashed(bytes: [*]const u8, len: usize) callconv(.c) void {
    const o = requireOps();
    o.roc_crashed(o, bytes, len);
}

fn rocExpectObserved(site: u32, passed: u8) callconv(.c) void {
    const o = requireOps();
    const observer = current_expect_observer orelse {
        o.crash("a test expect ran under a host with no expect observer");
        unreachable;
    };
    observer(o, site, passed);
}

fn rocExpectErrRegion(start: u32, end: u32) callconv(.c) void {
    last_expect_err_region = .{ .start = start, .end = end };
}

test "in-process host scopes restore ops and expect observers" {
    const std = @import("std");
    const TestEnv = @import("utils.zig").TestEnv;
    const Observer = struct {
        fn observe(o: *RocOps, site: u32, passed: u8) callconv(.c) void {
            const count: *u32 = @ptrCast(@alignCast(o.env));
            count.* += site * passed;
        }
    };
    var outer = TestEnv.init(std.testing.allocator);
    defer outer.deinit();
    var inner = TestEnv.init(std.testing.allocator);
    defer inner.deinit();
    var count: u32 = 0;
    outer.getOps().env = &count;

    const before = enter(outer.getOps(), &Observer.observe);
    defer leave(before);
    try std.testing.expectEqual(outer.getOps(), requireOps());
    rocExpectObserved(3, 1);
    {
        const saved = enter(inner.getOps(), null);
        defer leave(saved);
        try std.testing.expectEqual(inner.getOps(), requireOps());
        try std.testing.expect(current_expect_observer == null);
    }
    try std.testing.expectEqual(outer.getOps(), requireOps());
    rocExpectObserved(4, 1);
    try std.testing.expectEqual(@as(u32, 7), count);
}

test "immutable symbol adapter uses the entered allocator across nested scopes" {
    const std = @import("std");
    const TestEnv = @import("utils.zig").TestEnv;
    var outer = TestEnv.init(std.testing.allocator);
    defer outer.deinit();
    var inner = TestEnv.init(std.testing.allocator);
    defer inner.deinit();
    const adapter: *RocOps = @constCast(&symbol_backed_ops);

    const before = enter(outer.getOps(), null);
    defer leave(before);
    try std.testing.expectEqual(outer.getOps(), ops());
    var allocation = adapter.tryAlloc(32, 8).?;
    try std.testing.expectEqual(@as(usize, 1), outer.getAllocationCount());
    {
        const saved = enter(inner.getOps(), null);
        defer leave(saved);
        try std.testing.expectEqual(inner.getOps(), ops());
        const nested = adapter.tryAlloc(16, 8).?;
        try std.testing.expectEqual(@as(usize, 1), inner.getAllocationCount());
        adapter.dealloc(nested, 8);
        try std.testing.expectEqual(@as(usize, 0), inner.getAllocationCount());
    }
    try std.testing.expectEqual(outer.getOps(), ops());
    allocation = adapter.tryRealloc(allocation, 64, 8).?;
    try std.testing.expectEqual(@as(usize, 1), outer.getAllocationCount());
    adapter.dealloc(allocation, 8);
    try std.testing.expectEqual(@as(usize, 0), outer.getAllocationCount());
}

/// The runtime symbols an in-process host defines, in `runtime_set` order,
/// followed by the test host's expect observer and the `?` region recorder.
pub const Symbol = enum(u8) {
    roc_alloc,
    roc_dealloc,
    roc_realloc,
    roc_dbg,
    roc_expect_failed,
    roc_crashed,
    roc_expect_observed,
    roc_expect_err_region,

    /// The symbol's name.
    pub fn name(self: Symbol) [:0]const u8 {
        return switch (self) {
            .roc_alloc => shim_symbols.roc_alloc,
            .roc_dealloc => shim_symbols.roc_dealloc,
            .roc_realloc => shim_symbols.roc_realloc,
            .roc_dbg => shim_symbols.roc_dbg,
            .roc_expect_failed => shim_symbols.roc_expect_failed,
            .roc_crashed => shim_symbols.roc_crashed,
            .roc_expect_observed => roc_expect_observed,
            .roc_expect_err_region => roc_expect_err_region,
        };
    }

    /// The in-process definition's address, for code that embeds it.
    pub fn address(self: Symbol) usize {
        return switch (self) {
            .roc_alloc => @intFromPtr(&rocAlloc),
            .roc_dealloc => @intFromPtr(&rocDealloc),
            .roc_realloc => @intFromPtr(&rocRealloc),
            .roc_dbg => @intFromPtr(&rocDbg),
            .roc_expect_failed => @intFromPtr(&rocExpectFailed),
            .roc_crashed => @intFromPtr(&rocCrashed),
            .roc_expect_observed => @intFromPtr(&rocExpectObserved),
            .roc_expect_err_region => @intFromPtr(&rocExpectErrRegion),
        };
    }

    /// The symbol named `symbol_name`, if it is one of these.
    pub fn fromName(symbol_name: []const u8) ?Symbol {
        inline for (@typeInfo(Symbol).@"enum".fields) |field| {
            const candidate: Symbol = @enumFromInt(field.value);
            if (eql(candidate.name(), symbol_name)) return candidate;
        }
        return null;
    }
};

fn eql(a: []const u8, b: []const u8) bool {
    if (a.len != b.len) return false;
    for (a, b) |x, y| if (x != y) return false;
    return true;
}

comptime {
    if (host_abi.host_role == .in_process) {
        @export(&rocAlloc, .{ .name = shim_symbols.roc_alloc, .linkage = .weak });
        @export(&rocDealloc, .{ .name = shim_symbols.roc_dealloc, .linkage = .weak });
        @export(&rocRealloc, .{ .name = shim_symbols.roc_realloc, .linkage = .weak });
        @export(&rocDbg, .{ .name = shim_symbols.roc_dbg, .linkage = .weak });
        @export(&rocExpectFailed, .{ .name = shim_symbols.roc_expect_failed, .linkage = .weak });
        @export(&rocCrashed, .{ .name = shim_symbols.roc_crashed, .linkage = .weak });
        @export(&rocExpectObserved, .{ .name = roc_expect_observed, .linkage = .weak });
        @export(&rocExpectErrRegion, .{ .name = roc_expect_err_region, .linkage = .weak });
    }
}
