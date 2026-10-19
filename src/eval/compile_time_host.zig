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
const collections = @import("collections");
const FinalizeError = @import("check").CheckedArtifact.CompileTimeFinalizer.Error;

/// Exact producer-completion callback for a compile-time static slot read.
pub const SlotDemand = struct {
    pub const Error = FinalizeError || error{CompileTimeDependencyCycle};
    context: *anyopaque,
    ensure: *const fn (*anyopaque, lir.LIR.StaticDataId) Error!void,
};
const sljmp = @import("sljmp");
const crash_boundary_mod = @import("crash_boundary.zig");

const Allocator = std.mem.Allocator;
const RocOps = builtins.host_abi.RocOps;
const JmpBuf = sljmp.JmpBuf;
const longjmp = sljmp.longjmp;

const CompileTimeHost = @This();

/// Every compile-time allocation carries its byte length in a header this
/// many bytes before the bytes handed to Roc. The header keeps the Roc bytes
/// at the strictest alignment a request may ask for, so one arena allocation
/// serves every request, reallocation reads the old length from the header,
/// and deallocation does nothing: the root's arena frees everything at once.
const allocation_header_bytes = 16;
const max_allocation_alignment = allocation_header_bytes;
/// Debug-only stamp a live header carries, so a pointer Roc hands back that
/// no compile-time allocation produced is caught.
const allocation_header_stamp: usize = 0x5f6d_6f63_7074_696d;

const AllocationHeader = extern struct {
    size: usize,
    stamp: usize,
};

comptime {
    std.debug.assert(@sizeOf(AllocationHeader) <= allocation_header_bytes);
    std.debug.assert(std.math.isPowerOfTwo(allocation_header_bytes));
}

/// How a native compile-time root finished.
pub const Termination = enum {
    returned,
    crashed,
    comptime_exhaustiveness,
    host_oom,
    host_error,
};

/// A failed inline `expect` observed during native compile-time evaluation,
/// with the failing statement's checked region and resolved location (whose
/// file entry names the declaring module) as emitted by the dev backend's
/// failure-region hook immediately before the expect-failed call.
pub const ExpectFailedEvent = struct {
    message: []u8,
    region: ?base.Region,
    loc: ?base.SourceLoc,
};

/// Root-local host effects captured during native compile-time evaluation.
pub const HostEvent = union(enum) {
    dbg: []u8,
    expect_failed: ExpectFailedEvent,
    crashed: []u8,

    pub fn bytes(self: HostEvent) []const u8 {
        return switch (self) {
            inline .dbg, .crashed => |msg| msg,
            .expect_failed => |event| event.message,
        };
    }
};

/// Published producer origins, keyed by the emitted LIR statement that reads
/// a failed slot.
pub const FailureOrigins = collections.DenseMap(lir.LIR.CFStmtId, lir.LIR.ComptimeFailureOrigin);

/// A compile-time branch marker reached by one root.
pub const ComptimeBranchHit = struct {
    site: lir.LIR.ComptimeSiteId,
    branch_index: u32,
};

/// A generated call frame's checked source region plus the resolved source
/// location stamped on the calling LIR statement. The location's file entry
/// carries the declaring module, so a failure region is never rendered
/// against another module's source.
pub const CallRegion = struct {
    region: base.Region,
    loc: base.SourceLoc,
};

arena: base.SingleThreadArena,
host_arena: base.SingleThreadArena,
roc_ops: ?RocOps = null,
events: std.ArrayListUnmanaged(HostEvent) = .empty,
comptime_branch_hits: std.ArrayListUnmanaged(ComptimeBranchHit) = .empty,
call_regions: std.ArrayListUnmanaged(CallRegion) = .empty,
comptime_failed_site: ?lir.LIR.ComptimeSiteId = null,
failed_region: ?base.Region = null,
/// Resolved source location of the failing LIR statement (file entry names
/// the declaring module), captured alongside `failed_region`.
failed_loc: ?base.SourceLoc = null,
/// Published producer origins of the emitted LIR statements that have one.
failure_origins: ?*const FailureOrigins = null,
/// The LIR statement whose failure-region hook ran last: the failing
/// statement once the run fails.
failed_stmt: ?lir.LIR.CFStmtId = null,
slot_demand: ?SlotDemand = null,
operational_error: ?FinalizeError = null,
timing_io: ?std.Io = null,
suspended_ns: u64 = 0,
jmp_buf: JmpBuf = undefined,
active_jmp_buf: ?*JmpBuf = null,
termination: Termination = .returned,

pub fn init(allocator: Allocator) CompileTimeHost {
    const backing_allocator = hostBytesAllocator(allocator);
    return .{
        .arena = base.SingleThreadArena.init(backing_allocator),
        .host_arena = base.SingleThreadArena.init(backing_allocator),
    };
}

pub fn deinit(self: *CompileTimeHost) void {
    self.clearHostState();
    self.host_arena.deinit();
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

/// Clear per-run state and release root-local compile-time host memory.
pub fn resetForRun(self: *CompileTimeHost) void {
    self.clearHostState();
    _ = self.host_arena.reset(.free_all);
    self.comptime_failed_site = null;
    self.failed_region = null;
    self.failed_loc = null;
    self.failed_stmt = null;
    self.operational_error = null;
    self.suspended_ns = 0;
    _ = self.arena.reset(.free_all);
    self.termination = .returned;
    self.active_jmp_buf = null;
}

/// setjmp/longjmp boundary for catching root-local crashes.
pub const CrashBoundary = crash_boundary_mod.CrashBoundary(CompileTimeHost, true);

/// Record a caught stack overflow exactly the way a Roc `crash` is recorded,
/// so the finalizer reports it as this root's compile-time crash.
pub fn noteStackOverflow(self: *CompileTimeHost) void {
    if (self.failed_region == null and self.call_regions.items.len > 0) {
        const frame = self.call_regions.items[self.call_regions.items.len - 1];
        self.failed_region = frame.region;
        self.failed_loc = frame.loc;
    }
    self.appendEvent(.crashed, crash_boundary_mod.STACK_OVERFLOW_MESSAGE);
    self.termination = .crashed;
}

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
            .dbg, .expect_failed => {},
        }
    }
    return null;
}

/// Start measuring time spent suspended while another root completes.
pub fn startDemandTiming(self: *CompileTimeHost) i128 {
    return if (self.timing_io) |io| std.Io.Timestamp.now(io, .awake).nanoseconds else 0;
}

/// Accumulate nested demand time for exclusive root timing.
pub fn finishDemandTiming(self: *CompileTimeHost, started: i128) void {
    if (self.timing_io) |io| self.suspended_ns += @intCast(@max(0, std.Io.Timestamp.now(io, .awake).nanoseconds - started));
}

/// A static read demands completion of its explicitly identified producer.
pub fn rocComptimeEnsureStaticValue(slot: u32) callconv(.c) void {
    const roc_ops = enteredOps();
    const self: *CompileTimeHost = @ptrCast(@alignCast(roc_ops.env));
    const demand = self.slot_demand orelse return;
    const started = self.startDemandTiming();
    const result = demand.ensure(demand.context, @fromBackingInt(@intCast(slot)));
    self.finishDemandTiming(started);
    result catch |err| switch (err) {
        error.CompileTimeDependencyCycle => {
            const message = "cyclic compile-time value dependency";
            rocCrashed(roc_ops, message.ptr, message.len);
        },
        else => |operational| {
            self.operational_error = operational;
            self.jump(.host_error);
        },
    };
}

/// Dev-backend hook called when a compile-time branch marker is reached.
pub fn rocComptimeBranchTaken(site_raw: u32, branch_index: u32) callconv(.c) void {
    const self = enteredHost();
    self.comptime_branch_hits.append(self.host_arena.allocator(), .{
        .site = @fromBackingInt(@intCast(site_raw)),
        .branch_index = branch_index,
    }) catch {
        self.jump(.host_oom);
    };
}

/// Dev-backend hook called when empirical exhaustiveness fails.
pub fn rocComptimeExhaustivenessFailed(site_raw: u32) callconv(.c) void {
    const self = enteredHost();
    self.comptime_failed_site = @fromBackingInt(@intCast(site_raw));
    self.jump(.comptime_exhaustiveness);
}

/// Dev-backend hook recording the source region for an imminent failure,
/// together with the failing statement's resolved location (whose file entry
/// names the declaring module).
pub fn rocComptimeFailureRegion(start_offset: u32, end_offset: u32, file: u32, line: u32, column: u32, stmt: u32) callconv(.c) void {
    const self = enteredHost();
    self.failed_stmt = @fromBackingInt(@intCast(stmt));
    if (self.failure_origins) |origins| {
        if (origins.get(@fromBackingInt(@intCast(stmt)))) |origin| {
            self.failed_region = origin.region;
            self.failed_loc = origin.loc;
            return;
        }
    }
    if (start_offset == end_offset) return;
    self.failed_region = base.Region.from_raw_offsets(start_offset, end_offset);
    self.failed_loc = .{ .file = file, .line = line, .column = column };
}

/// Dev-backend hook pushing a source region for a generated call frame.
pub fn rocComptimeCallEnter(start_offset: u32, end_offset: u32, file: u32, line: u32, column: u32) callconv(.c) void {
    const self = enteredHost();
    self.call_regions.append(self.host_arena.allocator(), .{
        .region = base.Region.from_raw_offsets(start_offset, end_offset),
        .loc = .{ .file = file, .line = line, .column = column },
    }) catch {
        self.jump(.host_oom);
    };
}

/// Dev-backend hook popping the source region for a generated call frame.
pub fn rocComptimeCallExit() callconv(.c) void {
    const self = enteredHost();
    if (self.call_regions.items.len == 0) {
        base.invariant("{s}", .{"compile-time call-region stack underflow"});
    }
    _ = self.call_regions.pop();
}

/// The ops of the evaluation this thread entered, which the hooks above are
/// only ever called from.
fn enteredOps() *RocOps {
    return builtins.in_process_host.current() orelse base.invariant("{s}", .{"compile-time hook ran on a thread that entered no host"});
}

fn enteredHost() *CompileTimeHost {
    return @ptrCast(@alignCast(enteredOps().env));
}

fn clearHostState(self: *CompileTimeHost) void {
    self.events = .empty;
    self.comptime_branch_hits = .empty;
    self.call_regions = .empty;
}

fn appendEvent(self: *CompileTimeHost, comptime tag: std.meta.Tag(HostEvent), bytes: []const u8) void {
    const owned = self.dupeEventBytes(bytes);
    const host_allocator = self.host_arena.allocator();
    self.events.append(host_allocator, @unionInit(HostEvent, @tagName(tag), owned)) catch {
        self.jump(.host_oom);
        unreachable;
    };
}

fn appendExpectFailedEvent(self: *CompileTimeHost, bytes: []const u8, region: ?base.Region, loc: ?base.SourceLoc) void {
    const owned = self.dupeEventBytes(bytes);
    const host_allocator = self.host_arena.allocator();
    self.events.append(host_allocator, .{ .expect_failed = .{
        .message = owned,
        .region = region,
        .loc = loc,
    } }) catch {
        self.jump(.host_oom);
        unreachable;
    };
}

fn dupeEventBytes(self: *CompileTimeHost, bytes: []const u8) []u8 {
    const host_allocator = self.host_arena.allocator();
    return host_allocator.dupe(u8, bytes) catch {
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
    base.invariant("{s}", .{"compile-time host failure escaped without an active crash boundary"});
}

fn rocAlloc(roc_ops: *RocOps, length: usize, alignment: usize) callconv(.c) *anyopaque {
    const self: *CompileTimeHost = @ptrCast(@alignCast(roc_ops.env));
    return @ptrCast(self.allocateRocBytes(length, alignment));
}

fn rocDealloc(_: *RocOps, ptr: *anyopaque, _: usize) callconv(.c) void {
    // The root's arena frees every allocation at once; the bytes only have
    // to be ones this host handed out and not handed back already.
    releaseAllocation(ptr);
}

fn rocRealloc(roc_ops: *RocOps, ptr: *anyopaque, new_length: usize, alignment: usize) callconv(.c) *anyopaque {
    const self: *CompileTimeHost = @ptrCast(@alignCast(roc_ops.env));
    const old_size = allocationHeader(ptr).size;
    const new_bytes = self.allocateRocBytes(new_length, alignment);
    const old_bytes: [*]u8 = @ptrCast(ptr);
    const copied = @min(old_size, @max(new_length, 1));
    @memcpy(new_bytes[0..copied], old_bytes[0..copied]);
    releaseAllocation(ptr);
    return @ptrCast(new_bytes);
}

fn releaseAllocation(ptr: *anyopaque) void {
    if (!std.debug.runtime_safety) return;
    const header = allocationHeader(ptr);
    std.debug.assert(header.stamp == allocation_header_stamp);
    header.stamp = 0;
}

/// Hand out `length` bytes (at least one) at `alignment` from the root's
/// arena, behind their header.
fn allocateRocBytes(self: *CompileTimeHost, length: usize, alignment: usize) [*]u8 {
    if (alignment > max_allocation_alignment or !std.math.isPowerOfTwo(alignment)) {
        base.invariant("{s}", .{"unsupported compile-time RocOps allocation alignment"});
    }
    const alloc_len = @max(length, 1);
    const total = std.math.add(usize, allocation_header_bytes, alloc_len) catch self.jump(.host_oom);
    const block = self.arena.allocator().alignedAlloc(u8, .fromByteUnits(allocation_header_bytes), total) catch {
        self.jump(.host_oom);
    };
    const header: *AllocationHeader = @ptrCast(@alignCast(block.ptr));
    header.size = alloc_len;
    if (std.debug.runtime_safety) header.stamp = allocation_header_stamp;
    return block.ptr + allocation_header_bytes;
}

fn allocationHeader(ptr: *anyopaque) *AllocationHeader {
    const bytes: [*]u8 = @ptrCast(ptr);
    return @ptrCast(@alignCast(bytes - allocation_header_bytes));
}

fn rocDbg(roc_ops: *RocOps, bytes: [*]const u8, len: usize) callconv(.c) void {
    const self: *CompileTimeHost = @ptrCast(@alignCast(roc_ops.env));
    self.appendEvent(.dbg, bytes[0..len]);
}

fn rocExpectFailed(roc_ops: *RocOps, bytes: [*]const u8, len: usize) callconv(.c) void {
    const self: *CompileTimeHost = @ptrCast(@alignCast(roc_ops.env));
    // The dev backend emits a failure-region hook call immediately before an
    // expect-failed call, exactly as for crashes. Consume that pending
    // location into this event: evaluation continues after a failed expect,
    // so leaving it set would misattribute a later crash to the expect.
    const region = self.failed_region;
    const loc = self.failed_loc;
    self.failed_region = null;
    self.failed_loc = null;
    self.failed_stmt = null;
    self.appendExpectFailedEvent(bytes[0..len], region, loc);
}

fn rocCrashed(roc_ops: *RocOps, bytes: [*]const u8, len: usize) callconv(.c) void {
    const self: *CompileTimeHost = @ptrCast(@alignCast(roc_ops.env));
    if (self.failed_region == null and self.call_regions.items.len > 0) {
        const frame = self.call_regions.items[self.call_regions.items.len - 1];
        self.failed_region = frame.region;
        self.failed_loc = frame.loc;
    }
    self.appendEvent(.crashed, bytes[0..len]);
    self.jump(.crashed);
}

fn hostBytesAllocator(allocator: Allocator) Allocator {
    return switch (@import("builtin").target.os.tag) {
        .freestanding => std.heap.wasm_allocator,
        .wiiu,
        .@"switch",
        .gba,
        .psx,
        .tios,
        .ashetos,
        .other,
        .contiki,
        .fuchsia,
        .hermit,
        .managarm,
        .haiku,
        .hurd,
        .illumos,
        .linux,
        .plan9,
        .rtems,
        .serenity,
        .dragonfly,
        .freebsd,
        .netbsd,
        .openbsd,
        .driverkit,
        .ios,
        .maccatalyst,
        .macos,
        .tvos,
        .visionos,
        .watchos,
        .windows,
        .uefi,
        .@"3ds",
        .ps3,
        .ps4,
        .ps5,
        .psp,
        .vita,
        .emscripten,
        .wasi,
        .amdhsa,
        .amdpal,
        .cuda,
        .mesa3d,
        .nvcl,
        .opencl,
        .opengl,
        .vulkan,
        => allocator,
    };
}

test "compile-time allocations honor alignment, keep their bytes across reallocation, and churn in constant time" {
    var host = CompileTimeHost.init(std.testing.allocator);
    defer host.deinit();
    const roc_ops = host.ops();

    inline for (.{ 1, 2, 4, 8, 16 }) |alignment| {
        const ptr = roc_ops.roc_alloc(roc_ops, 3, alignment);
        try std.testing.expect(@intFromPtr(ptr) % alignment == 0);
        roc_ops.roc_dealloc(roc_ops, ptr, alignment);
    }

    // Growing keeps every old byte; shrinking keeps the bytes that fit.
    const first = roc_ops.roc_alloc(roc_ops, 5, 1);
    const first_bytes: [*]u8 = @ptrCast(first);
    @memcpy(first_bytes[0..5], "hello");
    const grown = roc_ops.roc_realloc(roc_ops, first, 40, 8);
    const grown_bytes: [*]u8 = @ptrCast(grown);
    try std.testing.expectEqualStrings("hello", grown_bytes[0..5]);
    try std.testing.expect(@intFromPtr(grown) % 8 == 0);
    const shrunk = roc_ops.roc_realloc(roc_ops, grown, 2, 1);
    const shrunk_bytes: [*]u8 = @ptrCast(shrunk);
    try std.testing.expectEqualStrings("he", shrunk_bytes[0..2]);
    roc_ops.roc_dealloc(roc_ops, shrunk, 1);

    // A root that once held many blocks and then frees and allocates in a
    // loop must not pay for the blocks it once held on every operation.
    var live: [65_536]*anyopaque = undefined;
    for (&live) |*slot| slot.* = roc_ops.roc_alloc(roc_ops, 16, 8);
    for (live) |ptr| roc_ops.roc_dealloc(roc_ops, ptr, 8);
    var cycle: usize = 0;
    while (cycle < 1_000_000) : (cycle += 1) {
        const ptr = roc_ops.roc_alloc(roc_ops, 16, 8);
        roc_ops.roc_dealloc(roc_ops, ptr, 8);
    }
}

test "compile-time host declarations are referenced" {
    std.testing.refAllDecls(@This());
}
