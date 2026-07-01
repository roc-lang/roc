//! RocOps environment for native dev-backend compile-time evaluation.
//!
//! Each compile-time root gets its own host env. Runtime allocations go into a
//! root-local arena and are bulk-freed after the result has been copied into the
//! checked ConstStore. Host effects and crash state are recorded on the env so
//! the finalizer can replay them deterministically in root request order.

const std = @import("std");
const base = @import("base");
const builtins = @import("builtins");
const lir = @import("lir");
const sljmp = @import("sljmp");

const Allocator = std.mem.Allocator;
const RocOps = builtins.host_abi.RocOps;
const JmpBuf = sljmp.JmpBuf;
const setjmp = sljmp.setjmp;
const longjmp = sljmp.longjmp;

const CompileTimeHost = @This();

const Allocation = struct {
    size: usize,
    alignment: usize,
};

/// How a native compile-time root finished.
pub const Termination = enum {
    returned,
    crashed,
    comptime_exhaustiveness,
    host_oom,
};

/// Root-local host effects captured during native compile-time evaluation.
pub const HostEvent = union(enum) {
    dbg: []u8,
    expect_failed: []u8,
    crashed: []u8,

    pub fn bytes(self: HostEvent) []const u8 {
        return switch (self) {
            .dbg => |msg| msg,
            .expect_failed => |msg| msg,
            .crashed => |msg| msg,
        };
    }

    pub fn deinit(self: *HostEvent, allocator: Allocator) void {
        switch (self.*) {
            .dbg => |msg| allocator.free(msg),
            .expect_failed => |msg| allocator.free(msg),
            .crashed => |msg| allocator.free(msg),
        }
    }
};

/// A compile-time branch marker reached by one root.
pub const ComptimeBranchHit = struct {
    site: lir.LIR.ComptimeSiteId,
    branch_index: u32,
};

allocator: Allocator,
arena: base.SingleThreadArena,
allocations: std.AutoHashMap(usize, Allocation),
roc_ops: ?RocOps = null,
events: std.ArrayListUnmanaged(HostEvent) = .empty,
comptime_branch_hits: std.ArrayListUnmanaged(ComptimeBranchHit) = .empty,
call_regions: std.ArrayListUnmanaged(base.Region) = .empty,
comptime_failed_site: ?lir.LIR.ComptimeSiteId = null,
failed_region: ?base.Region = null,
jmp_buf: JmpBuf = undefined,
active_jmp_buf: ?*JmpBuf = null,
termination: Termination = .returned,

pub fn init(allocator: Allocator) CompileTimeHost {
    return .{
        .allocator = allocator,
        .arena = base.SingleThreadArena.init(allocator),
        .allocations = std.AutoHashMap(usize, Allocation).init(allocator),
    };
}

pub fn deinit(self: *CompileTimeHost) void {
    self.clearEvents();
    self.comptime_branch_hits.deinit(self.allocator);
    self.call_regions.deinit(self.allocator);
    self.allocations.deinit();
    self.arena.deinit();
    self.* = undefined;
}

/// Return the RocOps table passed to the generated root wrapper.
pub fn ops(self: *CompileTimeHost) *RocOps {
    if (self.roc_ops == null) {
        self.roc_ops = .{
            .env = @ptrCast(self),
            .roc_alloc = rocAlloc,
            .roc_dealloc = rocDealloc,
            .roc_realloc = rocRealloc,
            .roc_dbg = rocDbg,
            .roc_expect_failed = rocExpectFailed,
            .roc_crashed = rocCrashed,
            .hosted_fns = builtins.host_abi.emptyHostedFunctions(),
        };
    }
    return &self.roc_ops.?;
}

/// Clear per-run state while retaining allocated buffers for this root host.
pub fn resetForRun(self: *CompileTimeHost) void {
    self.clearEvents();
    self.comptime_branch_hits.clearRetainingCapacity();
    self.call_regions.clearRetainingCapacity();
    self.comptime_failed_site = null;
    self.failed_region = null;
    self.allocations.clearRetainingCapacity();
    _ = self.arena.reset(.free_all);
    self.termination = .returned;
    self.active_jmp_buf = null;
}

/// setjmp/longjmp boundary for catching root-local crashes.
pub const CrashBoundary = struct {
    env: *CompileTimeHost,
    prev_jmp_buf: ?*JmpBuf,

    pub fn init(env: *CompileTimeHost) CrashBoundary {
        return .{
            .env = env,
            .prev_jmp_buf = if (sljmp.supported) env.installJumpBuf(&env.jmp_buf) else null,
        };
    }

    pub fn deinit(self: *CrashBoundary) void {
        if (sljmp.supported) self.env.restoreJumpBuf(self.prev_jmp_buf);
    }

    pub inline fn set(self: *CrashBoundary) c_int {
        if (sljmp.supported) return setjmp(&self.env.jmp_buf);
        return 0;
    }
};

/// Install a crash boundary before calling generated root code.
pub fn enterCrashBoundary(self: *CompileTimeHost) CrashBoundary {
    return CrashBoundary.init(self);
}

/// Return the latest captured Roc crash message, if any.
pub fn crashMessage(self: *const CompileTimeHost) ?[]const u8 {
    for (0..self.events.items.len) |i| {
        const idx = self.events.items.len - 1 - i;
        switch (self.events.items[idx]) {
            .crashed => |msg| return msg,
            else => {},
        }
    }
    return null;
}

/// Dev-backend hook called when a compile-time branch marker is reached.
pub fn rocComptimeBranchTaken(roc_ops: *RocOps, site_raw: u32, branch_index: u32) callconv(.c) void {
    const self: *CompileTimeHost = @ptrCast(@alignCast(roc_ops.env));
    self.comptime_branch_hits.append(self.allocator, .{
        .site = @enumFromInt(site_raw),
        .branch_index = branch_index,
    }) catch {
        self.jump(.host_oom);
    };
}

/// Dev-backend hook called when empirical exhaustiveness fails.
pub fn rocComptimeExhaustivenessFailed(roc_ops: *RocOps, site_raw: u32) callconv(.c) void {
    const self: *CompileTimeHost = @ptrCast(@alignCast(roc_ops.env));
    self.comptime_failed_site = @enumFromInt(site_raw);
    self.jump(.comptime_exhaustiveness);
}

/// Dev-backend hook recording the source region for an imminent failure.
pub fn rocComptimeFailureRegion(roc_ops: *RocOps, start_offset: u32, end_offset: u32) callconv(.c) void {
    const self: *CompileTimeHost = @ptrCast(@alignCast(roc_ops.env));
    self.failed_region = base.Region.from_raw_offsets(start_offset, end_offset);
}

/// Dev-backend hook pushing a source region for a generated call frame.
pub fn rocComptimeCallEnter(roc_ops: *RocOps, start_offset: u32, end_offset: u32) callconv(.c) void {
    const self: *CompileTimeHost = @ptrCast(@alignCast(roc_ops.env));
    self.call_regions.append(self.allocator, base.Region.from_raw_offsets(start_offset, end_offset)) catch {
        self.jump(.host_oom);
    };
}

/// Dev-backend hook popping the source region for a generated call frame.
pub fn rocComptimeCallExit(roc_ops: *RocOps) callconv(.c) void {
    const self: *CompileTimeHost = @ptrCast(@alignCast(roc_ops.env));
    if (self.call_regions.items.len == 0) {
        @panic("compile-time call-region stack underflow");
    }
    _ = self.call_regions.pop();
}

fn installJumpBuf(self: *CompileTimeHost, jmp_buf: *JmpBuf) ?*JmpBuf {
    const prev = self.active_jmp_buf;
    self.active_jmp_buf = jmp_buf;
    return prev;
}

fn restoreJumpBuf(self: *CompileTimeHost, prev: ?*JmpBuf) void {
    self.active_jmp_buf = prev;
}

fn clearEvents(self: *CompileTimeHost) void {
    for (self.events.items) |*event| event.deinit(self.allocator);
    self.events.clearAndFree(self.allocator);
}

fn appendEvent(self: *CompileTimeHost, comptime tag: std.meta.Tag(HostEvent), bytes: []const u8) void {
    const owned = self.allocator.dupe(u8, bytes) catch {
        self.jump(.host_oom);
        unreachable;
    };
    self.events.append(self.allocator, @unionInit(HostEvent, @tagName(tag), owned)) catch {
        self.allocator.free(owned);
        self.jump(.host_oom);
        unreachable;
    };
}

fn jump(self: *CompileTimeHost, termination: Termination) noreturn {
    self.termination = termination;
    if (sljmp.supported) {
        if (self.active_jmp_buf) |active_jmp_buf| {
            self.active_jmp_buf = null;
            longjmp(active_jmp_buf, 1);
        }
    }
    @panic("compile-time host failure escaped without an active crash boundary");
}

fn rocAlloc(roc_ops: *RocOps, length: usize, alignment: usize) callconv(.c) ?*anyopaque {
    const self: *CompileTimeHost = @ptrCast(@alignCast(roc_ops.env));
    const alloc_len = @max(length, 1);
    const arena_allocator = self.arena.allocator();
    const ptr = allocateBytes(arena_allocator, alloc_len, alignment) orelse {
        self.jump(.host_oom);
    };
    self.allocations.put(@intFromPtr(ptr), .{ .size = alloc_len, .alignment = alignment }) catch {
        self.jump(.host_oom);
    };
    return @ptrCast(ptr);
}

fn rocDealloc(roc_ops: *RocOps, ptr: *anyopaque, _: usize) callconv(.c) void {
    const self: *CompileTimeHost = @ptrCast(@alignCast(roc_ops.env));
    _ = self.allocations.fetchRemove(@intFromPtr(ptr)) orelse {
        @panic("compile-time RocOps deallocated unknown pointer");
    };
}

fn rocRealloc(roc_ops: *RocOps, ptr: *anyopaque, new_length: usize, alignment: usize) callconv(.c) ?*anyopaque {
    const self: *CompileTimeHost = @ptrCast(@alignCast(roc_ops.env));
    const old_info = self.allocations.get(@intFromPtr(ptr)) orelse {
        @panic("compile-time RocOps reallocated unknown pointer");
    };
    const alloc_len = @max(new_length, 1);
    const arena_allocator = self.arena.allocator();
    const new_ptr = allocateBytes(arena_allocator, alloc_len, alignment) orelse {
        self.jump(.host_oom);
    };
    const old_bytes: [*]u8 = @ptrCast(@alignCast(ptr));
    @memcpy(new_ptr[0..@min(old_info.size, alloc_len)], old_bytes[0..@min(old_info.size, alloc_len)]);
    self.allocations.put(@intFromPtr(new_ptr), .{ .size = alloc_len, .alignment = alignment }) catch {
        self.jump(.host_oom);
    };
    _ = self.allocations.remove(@intFromPtr(ptr));
    return @ptrCast(new_ptr);
}

fn rocDbg(roc_ops: *RocOps, bytes: [*]const u8, len: usize) callconv(.c) void {
    const self: *CompileTimeHost = @ptrCast(@alignCast(roc_ops.env));
    self.appendEvent(.dbg, bytes[0..len]);
}

fn rocExpectFailed(roc_ops: *RocOps, bytes: [*]const u8, len: usize) callconv(.c) void {
    const self: *CompileTimeHost = @ptrCast(@alignCast(roc_ops.env));
    self.appendEvent(.expect_failed, bytes[0..len]);
}

fn rocCrashed(roc_ops: *RocOps, bytes: [*]const u8, len: usize) callconv(.c) void {
    const self: *CompileTimeHost = @ptrCast(@alignCast(roc_ops.env));
    if (self.failed_region == null and self.call_regions.items.len > 0) {
        self.failed_region = self.call_regions.items[self.call_regions.items.len - 1];
    }
    self.appendEvent(.crashed, bytes[0..len]);
    self.jump(.crashed);
}

fn allocateBytes(allocator: Allocator, len: usize, alignment: usize) ?[*]u8 {
    return switch (alignment) {
        1 => (allocator.alignedAlloc(u8, .@"1", len) catch return null).ptr,
        2 => (allocator.alignedAlloc(u8, .@"2", len) catch return null).ptr,
        4 => (allocator.alignedAlloc(u8, .@"4", len) catch return null).ptr,
        8 => (allocator.alignedAlloc(u8, .@"8", len) catch return null).ptr,
        16 => (allocator.alignedAlloc(u8, .@"16", len) catch return null).ptr,
        else => @panic("unsupported compile-time RocOps allocation alignment"),
    };
}

test "compile-time host declarations are referenced" {
    std.testing.refAllDecls(@This());
}
