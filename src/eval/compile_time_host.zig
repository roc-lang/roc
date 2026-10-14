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
const FinalizeError = @import("check").CheckedArtifact.CompileTimeFinalizer.Error;

const sljmp = @import("sljmp");
const crash_boundary_mod = @import("crash_boundary.zig");

const Allocator = std.mem.Allocator;
const RocOps = builtins.host_abi.RocOps;
const SourceEvents = builtins.source_events;
const NativeFailure = @import("native_failure.zig");
const JmpBuf = sljmp.JmpBuf;
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
    host_error,
};

/// A failed inline `expect` observed during native compile-time evaluation,
/// with the original failure event's region and bound source location.
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

/// Earlier binding admits total mappings before executing a native root.
/// Callbacks bind consumer IDs, never reconstruct missing producer semantics.
pub const SourceEventResolver = struct {
    context: *const anyopaque,
    source_file: *const fn (*const anyopaque, *const SourceEvents.Source) u32,
    site: *const fn (*const anyopaque, *const SourceEvents.Site, []align(1) const SourceEvents.Region) lir.LIR.ComptimeSiteId,
    failure: *const fn (*const anyopaque, *const SourceEvents.Descriptor) NativeFailure.Observation,
};

arena: base.SingleThreadArena,
host_arena: base.SingleThreadArena,
allocations: std.AutoHashMapUnmanaged(usize, Allocation) = .empty,
roc_ops: ?RocOps = null,
events: std.ArrayListUnmanaged(HostEvent) = .empty,
comptime_branch_hits: std.ArrayListUnmanaged(ComptimeBranchHit) = .empty,
call_regions: std.ArrayListUnmanaged(CallRegion) = .empty,
/// Absent source frames still own an exit; they must not pop a known caller.
source_event_frames: std.ArrayListUnmanaged(bool) = .empty,
source_event_resolver: ?SourceEventResolver = null,
comptime_failed_site: ?lir.LIR.ComptimeSiteId = null,
failed_region: ?base.Region = null,
/// Resolved source location of the failing LIR statement (file entry names
/// the declaring module), captured alongside `failed_region`.
failed_loc: ?base.SourceLoc = null,
/// Published producer origins, indexed by the emitted LIR statement.
failure_origins: []const ?lir.LIR.ComptimeFailureOrigin = &.{},
slot_failure_origins: []const ?lir.LIR.ComptimeFailureOrigin = &.{},
failed_observation: ?@import("native_failure.zig").Observation = null,
site_observations: ?[]const @import("native_failure.zig").Site = null,
/// Optional live statement provenance for direct consumers. Common source
/// events record semantic observations instead of an emitted statement ordinal.
failed_stmt: ?lir.LIR.CFStmtId = null,
/// Borrowed stable storage owned by the active evaluator's slot environment.
static_slot_addresses: []const usize = &.{},
/// Producer scheduling completes every needed slot before native execution.
static_slot_ready: []const bool = &.{},
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
    self.failed_observation = null;
    self.operational_error = null;
    self.suspended_ns = 0;
    _ = self.arena.reset(.free_all);
    self.termination = .returned;
    self.active_jmp_buf = null;
}

/// setjmp/longjmp boundary for catching root-local crashes.
pub const CrashBoundary = struct {
    inner: crash_boundary_mod.CrashBoundary(CompileTimeHost, true),
    observer_saved: SourceEvents.Saved,

    pub fn init(env: *CompileTimeHost) CrashBoundary {
        return .{
            .observer_saved = SourceEvents.enter(env.sourceEventObserver()),
            .inner = crash_boundary_mod.CrashBoundary(CompileTimeHost, true).init(env),
        };
    }

    pub fn deinit(self: *CrashBoundary) void {
        // A longjmp returns through this same boundary. Restore tracing before
        // any root-local host state can be released or another root can execute.
        SourceEvents.leave(self.observer_saved);
        self.inner.deinit();
    }

    pub inline fn set(self: *CrashBoundary) c_int {
        return self.inner.set();
    }
};

/// This context is explicitly the observing host, never an ordinary RocOps.env.
pub fn sourceEventObserver(self: *CompileTimeHost) SourceEvents.Observer {
    return .{ .context = self, .on_event = sourceEventObserved, .on_invalid = sourceEventInvalid };
}

fn sourceEventObserved(context: ?*anyopaque, header: *const SourceEvents.Header) callconv(.c) void {
    const self: *CompileTimeHost = @ptrCast(@alignCast(context orelse base.invariant("source observer has no host", .{})));
    self.handleSourceEvent(SourceEvents.decodeValidated(header));
}

fn sourceEventInvalid(_: ?*anyopaque, invalid_tag: u32) callconv(.c) void {
    std.debug.panic("invalid compiler-owned source event descriptor (tag {d})", .{invalid_tag});
}

/// The C service validates first; direct callers get the same checked decode.
/// Invalid descriptors are compiler invariants, not Roc failure observations.
pub fn observeSourceEvent(self: *CompileTimeHost, header: *const SourceEvents.Header) SourceEvents.Invalid!void {
    self.handleSourceEvent(try SourceEvents.validate(header));
}

fn handleSourceEvent(self: *CompileTimeHost, event: SourceEvents.Event) void {
    const descriptor = event.descriptor;
    const resolver = self.source_event_resolver orelse base.invariant("source event ran without admitted semantic bindings", .{});
    switch (event.kind) {
        .branch_taken => {
            self.comptime_branch_hits.append(self.host_arena.allocator(), .{
                .site = resolver.site(resolver.context, &descriptor.site, event.branches),
                .branch_index = descriptor.branch_index,
            }) catch self.jump(.host_oom);
        },
        .exhaustiveness_failed => {
            self.comptime_failed_site = resolver.site(resolver.context, &descriptor.site, event.branches);
            self.jump(.comptime_exhaustiveness);
        },
        .failure => {
            const observation = resolver.failure(resolver.context, descriptor);
            std.debug.assert(observation.checked_error == (descriptor.failure.checked_error == 1));
            std.debug.assert((observation.literal_rejection != null) == (descriptor.failure.has_literal_rejection == 1));
            std.debug.assert((observation.guard_producer != null) == (descriptor.failure.has_guard_root == 1));
            self.failed_observation = observation;
            self.failed_stmt = null;
            const statement: ?u32 = if (observation.origin_statement) |id| @backingInt(id) else null;
            if (self.applyFailureOrigin(statement)) return;
            self.failed_region = null;
            self.failed_loc = null;
            if (sourceLoc(resolver, &descriptor.source)) |loc| {
                self.failed_region = sourceRegion(descriptor.source);
                self.failed_loc = loc;
            }
        },
        .call_enter => {
            const loc = sourceLoc(resolver, &descriptor.source);
            const has_location = loc != null;
            self.source_event_frames.append(self.host_arena.allocator(), has_location) catch self.jump(.host_oom);
            if (has_location) {
                self.call_regions.append(self.host_arena.allocator(), .{
                    .region = sourceRegion(descriptor.source),
                    .loc = loc.?,
                }) catch self.jump(.host_oom);
            }
        },
        .call_exit => {
            if (self.source_event_frames.items.len == 0) base.invariant("source event call-frame stack underflow", .{});
            if (self.source_event_frames.pop().?) {
                if (self.call_regions.items.len == 0) base.invariant("source event known-frame stack underflow", .{});
                _ = self.call_regions.pop();
            }
        },
    }
}

fn sourceRegion(source: SourceEvents.Source) base.Region {
    return base.Region.from_raw_offsets(source.region.start, source.region.end);
}

fn sourceLoc(resolver: SourceEventResolver, source: *const SourceEvents.Source) ?base.SourceLoc {
    if (source.has_location == 0) return null;
    return .{ .file = resolver.source_file(resolver.context, source), .line = source.line, .column = source.column };
}

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

/// Read storage explicitly published ready by the scheduler. This host API
/// does not execute a producer and is not a generated-code runtime service.
pub fn staticSlotAddress(self: *const CompileTimeHost, slot: lir.LIR.StaticDataId) usize {
    const index = @backingInt(slot);
    std.debug.assert(index < self.static_slot_ready.len);
    std.debug.assert(index < self.static_slot_addresses.len);
    std.debug.assert(self.static_slot_ready[index]);
    std.debug.assert(self.static_slot_addresses[index] != 0);
    return self.static_slot_addresses[index];
}

fn applyFailureOrigin(self: *CompileTimeHost, origin_statement: ?u32) bool {
    if (self.failed_observation) |observation| {
        if (observation.origin_slot) |slot| {
            std.debug.assert(@backingInt(slot) < self.slot_failure_origins.len);
            if (self.slot_failure_origins[@backingInt(slot)]) |origin| {
                self.failed_region = origin.region;
                self.failed_loc = origin.loc;
                return true;
            }
        }
    }
    if (origin_statement) |origin_id| {
        if (origin_id < self.failure_origins.len) if (self.failure_origins[origin_id]) |origin| {
            self.failed_region = origin.region;
            self.failed_loc = origin.loc;
            return true;
        };
    }
    return false;
}

fn clearHostState(self: *CompileTimeHost) void {
    self.events = .empty;
    self.comptime_branch_hits = .empty;
    self.call_regions = .empty;
    self.source_event_frames = .empty;
    self.allocations = .empty;
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

/// Every allocation is preceded by a header holding its length, so a
/// reallocation copies exactly the live bytes without a lookup. The header
/// is as wide as the strictest supported alignment, which keeps the
/// returned pointer aligned.
const allocation_header_len = 16;

fn allocateTracked(self: *CompileTimeHost, length: usize, alignment: usize) [*]u8 {
    const alloc_len = @max(length, 1);
    const block = allocateBytes(self.arena.allocator(), allocation_header_len + alloc_len, @max(alignment, @alignOf(usize))) orelse {
        self.jump(.host_oom);
    };
    const ptr = block + allocation_header_len;
    std.mem.writeInt(usize, (ptr - @sizeOf(usize))[0..@sizeOf(usize)], alloc_len, .little);
    // Safety-checked builds keep the live set to verify the compiler's own
    // deallocation and reallocation calls.
    if (std.debug.runtime_safety) {
        self.allocations.put(self.host_arena.allocator(), @intFromPtr(ptr), .{ .size = alloc_len, .alignment = alignment }) catch {
            self.jump(.host_oom);
        };
    }
    return ptr;
}

fn allocationLength(ptr: *anyopaque) usize {
    const bytes: [*]u8 = @ptrCast(ptr);
    return std.mem.readInt(usize, (bytes - @sizeOf(usize))[0..@sizeOf(usize)], .little);
}

fn rocAlloc(roc_ops: *RocOps, length: usize, alignment: usize) callconv(.c) *anyopaque {
    const self: *CompileTimeHost = @ptrCast(@alignCast(roc_ops.env));
    return @ptrCast(self.allocateTracked(length, alignment));
}

fn rocDealloc(roc_ops: *RocOps, ptr: *anyopaque, _: usize) callconv(.c) void {
    const self: *CompileTimeHost = @ptrCast(@alignCast(roc_ops.env));
    if (std.debug.runtime_safety) {
        _ = self.allocations.fetchRemove(@intFromPtr(ptr)) orelse {
            base.invariant("{s}", .{"compile-time RocOps deallocated unknown pointer"});
        };
    }
}

fn rocRealloc(roc_ops: *RocOps, ptr: *anyopaque, new_length: usize, alignment: usize) callconv(.c) *anyopaque {
    const self: *CompileTimeHost = @ptrCast(@alignCast(roc_ops.env));
    if (std.debug.runtime_safety) {
        _ = self.allocations.fetchRemove(@intFromPtr(ptr)) orelse {
            base.invariant("{s}", .{"compile-time RocOps reallocated unknown pointer"});
        };
    }
    const old_len = allocationLength(ptr);
    const new_ptr = self.allocateTracked(new_length, alignment);
    const alloc_len = @max(new_length, 1);
    const old_bytes: [*]u8 = @ptrCast(@alignCast(ptr));
    @memcpy(new_ptr[0..@min(old_len, alloc_len)], old_bytes[0..@min(old_len, alloc_len)]);
    return @ptrCast(new_ptr);
}

fn rocDbg(roc_ops: *RocOps, bytes: [*]const u8, len: usize) callconv(.c) void {
    const self: *CompileTimeHost = @ptrCast(@alignCast(roc_ops.env));
    self.appendEvent(.dbg, bytes[0..len]);
}

fn rocExpectFailed(roc_ops: *RocOps, bytes: [*]const u8, len: usize) callconv(.c) void {
    const self: *CompileTimeHost = @ptrCast(@alignCast(roc_ops.env));
    // A failure source event precedes an expect-failed call, as for crashes.
    // Consume its pending location: evaluation continues after a failed expect,
    // so leaving it set would misattribute a later crash to the expect.
    const region = self.failed_region;
    const loc = self.failed_loc;
    self.failed_region = null;
    self.failed_loc = null;
    self.failed_stmt = null;
    self.failed_observation = null;
    self.appendExpectFailedEvent(bytes[0..len], region, loc);
}

test "ready static storage returns published addresses with displaced slot IDs" {
    var host = CompileTimeHost.init(std.testing.allocator);
    defer host.deinit();
    var current_value: u64 = 42;
    const addresses = [_]usize{ 0, 0, 0, @intFromPtr(&current_value) };
    const ready = [_]bool{ false, false, false, true };
    host.static_slot_addresses = &addresses;
    host.static_slot_ready = &ready;
    const address = host.staticSlotAddress(@fromBackingInt(@intCast(3)));
    try std.testing.expectEqual(@intFromPtr(&current_value), address);
    try std.testing.expectEqual(@as(u64, 42), @as(*const u64, @ptrFromInt(address)).*);
    try std.testing.expectEqual(address, host.staticSlotAddress(@fromBackingInt(@intCast(3))));
}

test "cached guard failure uses current slot publication origin and producer" {
    const producer = NativeFailure.GuardProducer{
        .module = .{ .bytes = @as([32]u8, @splat(17)) },
        .root = .{ .checked = @fromBackingInt(@intCast(8)) },
    };
    var host = CompileTimeHost.init(std.testing.allocator);
    defer host.deinit();
    const origin: lir.LIR.ComptimeFailureOrigin = .{
        .region = base.Region.from_raw_offsets(31, 38),
        .loc = .{ .file = 7, .line = 13, .column = 4 },
    };
    host.slot_failure_origins = &.{ null, null, origin };
    const binding: TestEventResolver = .{
        .observation = .{ .checked_error = false, .guard_producer = producer, .origin_slot = @fromBackingInt(@intCast(2)) },
    };
    host.source_event_resolver = binding.resolver();
    const descriptor: SourceEvents.Descriptor = .{
        .header = .{ .kind = @backingInt(SourceEvents.Kind.failure) },
        .source = .{ .has_location = 1, .region = .{ .start = 1, .end = 2 }, .line = 1, .column = 1 },
        .failure = .{
            .has_guard_root = 1,
            .guard_root = .{
                .module = @splat(17),
                .producer_kind = @backingInt(SourceEvents.ProducerKind.checked),
                .checked_expr = 8,
                .role = @backingInt(SourceEvents.RootRole.value),
            },
        },
    };
    try host.observeSourceEvent(&descriptor.header);
    try std.testing.expectEqualDeep(@as(?base.Region, origin.region), host.failed_region);
    try std.testing.expectEqualDeep(@as(?base.SourceLoc, origin.loc), host.failed_loc);
    try std.testing.expectEqualDeep(@as(?NativeFailure.GuardProducer, producer), host.failed_observation.?.guard_producer);
    try std.testing.expect(host.failed_stmt == null);
}

test "native expect consumes both live and bound pending failure context" {
    var host = CompileTimeHost.init(std.testing.allocator);
    defer host.deinit();
    const region: base.Region = .{ .start = .{ .offset = 1 }, .end = .{ .offset = 4 } };
    const loc: base.SourceLoc = .{ .file = 3, .line = 2, .column = 5 };
    host.failed_region = region;
    host.failed_loc = loc;
    host.failed_stmt = @as(lir.LIR.CFStmtId, @fromBackingInt(@intCast(8)));
    host.failed_observation = .{ .checked_error = true, .origin_statement = @as(lir.LIR.CFStmtId, @fromBackingInt(@intCast(9))) };
    const message = "fixture expect failure";
    rocExpectFailed(host.ops(), message.ptr, message.len);
    try std.testing.expectEqual(@as(usize, 1), host.events.items.len);
    const event = host.events.items[0].expect_failed;
    try std.testing.expectEqualStrings(message, event.message);
    try std.testing.expectEqualDeep(@as(?base.Region, region), event.region);
    try std.testing.expectEqualDeep(@as(?base.SourceLoc, loc), event.loc);
    try std.testing.expect(host.failed_region == null);
    try std.testing.expect(host.failed_loc == null);
    try std.testing.expect(host.failed_stmt == null);
    try std.testing.expect(host.failed_observation == null);
}

const TestEventResolver = struct {
    observation: NativeFailure.Observation = .{ .checked_error = false },
    expected_failure: ?*const SourceEvents.Descriptor = null,

    fn resolver(self: *const TestEventResolver) SourceEventResolver {
        return .{ .context = self, .source_file = sourceFile, .site = site, .failure = failure };
    }

    fn sourceFile(_: *const anyopaque, source: *const SourceEvents.Source) u32 {
        std.debug.assert(source.has_location == 1);
        return 19;
    }

    fn site(_: *const anyopaque, descriptor: *const SourceEvents.Site, branches: []align(1) const SourceEvents.Region) lir.LIR.ComptimeSiteId {
        std.debug.assert(descriptor.has_checked_site == 1 and descriptor.checked_site == 41);
        std.debug.assert(std.mem.eql(u8, &descriptor.checked_module, &@as([32]u8, @splat(3))));
        std.debug.assert(std.mem.eql(u8, &descriptor.procedure_identity, &@as([32]u8, @splat(4))));
        std.debug.assert(branches.len == 1 and branches[0].start == 32 and branches[0].end == 39);
        return @fromBackingInt(@intCast(21));
    }

    fn failure(context: *const anyopaque, descriptor: *const SourceEvents.Descriptor) NativeFailure.Observation {
        const self: *const TestEventResolver = @ptrCast(@alignCast(context));
        if (self.expected_failure) |expected| std.debug.assert(expected == descriptor);
        return self.observation;
    }
};

const TestSiteEvent = extern struct {
    descriptor: SourceEvents.Descriptor,
    branch: SourceEvents.Region,

    fn init(kind: SourceEvents.Kind) TestSiteEvent {
        return .{
            .descriptor = .{
                .header = .{ .kind = @backingInt(kind), .byte_size = @sizeOf(TestSiteEvent) },
                .site = .{
                    .checked_module = @splat(3),
                    .procedure_identity = @splat(4),
                    .has_checked_site = 1,
                    .checked_site = 41,
                    .kind = @backingInt(SourceEvents.SiteKind.match),
                    .region = .{ .start = 1, .end = 8 },
                    .branches_offset = @sizeOf(SourceEvents.Descriptor),
                    .branch_count = 1,
                },
            },
            .branch = .{ .start = 32, .end = 39 },
        };
    }
};

test "source event adapter preserves absent nested frames and ignores ordinary RocOps env" {
    var host = CompileTimeHost.init(std.testing.allocator);
    defer host.deinit();
    var other_host = CompileTimeHost.init(std.testing.allocator);
    defer other_host.deinit();
    const binding: TestEventResolver = .{};
    host.source_event_resolver = binding.resolver();
    const ops_saved = builtins.in_process_host.enter(other_host.ops(), null);
    defer builtins.in_process_host.leave(ops_saved);
    const present: SourceEvents.Descriptor = .{
        .header = .{ .kind = @backingInt(SourceEvents.Kind.call_enter) },
        .source = .{ .has_location = 1, .region = .{ .start = 12, .end = 24 }, .line = 7, .column = 9 },
    };
    const absent: SourceEvents.Descriptor = .{ .header = .{ .kind = @backingInt(SourceEvents.Kind.call_enter) } };
    const exit: SourceEvents.Descriptor = .{ .header = .{ .kind = @backingInt(SourceEvents.Kind.call_exit) } };
    {
        var boundary = host.enterCrashBoundary();
        defer boundary.deinit();
        SourceEvents.roc_source_event(&present.header);
        SourceEvents.roc_source_event(&absent.header);
        try std.testing.expectEqual(@as(usize, 2), host.source_event_frames.items.len);
        try std.testing.expectEqual(@as(usize, 1), host.call_regions.items.len);
        try std.testing.expectEqualDeep(base.SourceLoc{ .file = 19, .line = 7, .column = 9 }, host.call_regions.items[0].loc);
        try std.testing.expectEqualDeep(base.Region.from_raw_offsets(12, 24), host.call_regions.items[0].region);
        SourceEvents.roc_source_event(&exit.header);
        try std.testing.expectEqual(@as(usize, 1), host.call_regions.items.len);
        SourceEvents.roc_source_event(&exit.header);
        try std.testing.expectEqual(@as(usize, 0), host.call_regions.items.len);
        SourceEvents.roc_source_event(&absent.header);
        SourceEvents.roc_source_event(&present.header);
        SourceEvents.roc_source_event(&exit.header);
        SourceEvents.roc_source_event(&exit.header);
        try std.testing.expectEqual(@as(usize, 0), host.source_event_frames.items.len);
        try std.testing.expectEqual(@as(usize, 0), other_host.call_regions.items.len);
        SourceEvents.roc_source_event(&present.header);
    }
    host.resetForRun();
    try std.testing.expectEqual(@as(usize, 0), host.source_event_frames.items.len);
    try std.testing.expectEqual(@as(usize, 0), host.call_regions.items.len);
    SourceEvents.roc_source_event(@ptrFromInt(@alignOf(SourceEvents.Header)));
}

test "source event adapter binds semantic branches and guarded literal failure ownership" {
    var host = CompileTimeHost.init(std.testing.allocator);
    defer host.deinit();
    const producer: NativeFailure.GuardProducer = .{
        .module = .{ .bytes = @splat(17) },
        .root = .{ .checked = @fromBackingInt(@intCast(8)) },
    };
    const literal: lir.LIR.LiteralRejectionSite = .{ .owner = @fromBackingInt(@intCast(12)), .checked_expr = 33, .kind = .quote };
    const failure: SourceEvents.Descriptor = .{
        .header = .{ .kind = @backingInt(SourceEvents.Kind.failure) },
        .source = .{ .has_location = 1, .region = .{ .start = 1, .end = 2 }, .line = 1, .column = 1 },
        .procedure_identity = @splat(9),
        .failure = .{
            .checked_error = 1,
            .has_literal_rejection = 1,
            .literal_rejection = .{ .checked_module = @splat(7), .checked_expr = 33, .kind = @backingInt(SourceEvents.LiteralKind.quote) },
            .has_guard_root = 1,
            .guard_root = .{
                .module = @splat(17),
                .producer_kind = @backingInt(SourceEvents.ProducerKind.checked),
                .checked_expr = 8,
                .role = @backingInt(SourceEvents.RootRole.value),
            },
        },
    };
    const binding: TestEventResolver = .{
        .expected_failure = &failure,
        .observation = .{ .checked_error = true, .literal_rejection = literal, .guard_producer = producer, .origin_slot = @fromBackingInt(@intCast(2)) },
    };
    host.source_event_resolver = binding.resolver();
    const origin: lir.LIR.ComptimeFailureOrigin = .{
        .region = base.Region.from_raw_offsets(31, 38),
        .loc = .{ .file = 7, .line = 13, .column = 4 },
    };
    host.slot_failure_origins = &.{ null, null, origin };
    const branch = TestSiteEvent.init(.branch_taken);
    var boundary = host.enterCrashBoundary();
    defer boundary.deinit();
    SourceEvents.roc_source_event(&branch.descriptor.header);
    try std.testing.expectEqual(@as(usize, 1), host.comptime_branch_hits.items.len);
    try std.testing.expectEqual(@as(u32, 21), @backingInt(host.comptime_branch_hits.items[0].site));
    try std.testing.expectEqual(@as(u32, 0), host.comptime_branch_hits.items[0].branch_index);
    host.failed_stmt = @fromBackingInt(@intCast(3));
    SourceEvents.roc_source_event(&failure.header);
    try std.testing.expect(host.failed_stmt == null);
    try std.testing.expectEqualDeep(binding.observation, host.failed_observation.?);
    try std.testing.expectEqualDeep(origin.region, host.failed_region);
    try std.testing.expectEqualDeep(origin.loc, host.failed_loc);
}

test "source event adapter preserves original failure location and explicit absence" {
    var host = CompileTimeHost.init(std.testing.allocator);
    defer host.deinit();
    const binding: TestEventResolver = .{};
    host.source_event_resolver = binding.resolver();
    var failure: SourceEvents.Descriptor = .{
        .header = .{ .kind = @backingInt(SourceEvents.Kind.failure) },
        .source = .{ .has_location = 1, .region = .{ .start = 12, .end = 24 }, .line = 7, .column = 9 },
    };
    try host.observeSourceEvent(&failure.header);
    try std.testing.expectEqualDeep(@as(?base.SourceLoc, .{ .file = 19, .line = 7, .column = 9 }), host.failed_loc);
    try std.testing.expectEqualDeep(@as(?base.Region, base.Region.from_raw_offsets(12, 24)), host.failed_region);
    failure.source.has_location = 0;
    try host.observeSourceEvent(&failure.header);
    try std.testing.expect(host.failed_loc == null and host.failed_region == null);
    failure.header.version = 2;
    try std.testing.expectError(error.UnsupportedVersion, host.observeSourceEvent(&failure.header));
    try std.testing.expect(host.failed_loc == null and host.failed_region == null);
    try std.testing.expectEqual(@as(usize, 0), host.events.items.len);
}

test "source event adapter restores observer after exhaustiveness longjmp before teardown" {
    if (!sljmp.supported) return error.SkipZigTest;
    var host = CompileTimeHost.init(std.testing.allocator);
    defer host.deinit();
    const binding: TestEventResolver = .{};
    host.source_event_resolver = binding.resolver();
    const Counter = struct {
        count: usize = 0,
        fn record(context: ?*anyopaque, _: *const SourceEvents.Header) callconv(.c) void {
            const self: *@This() = @ptrCast(@alignCast(context.?));
            self.count += 1;
        }
        fn invalid(_: ?*anyopaque, _: u32) callconv(.c) void {
            @trap();
        }
    };
    var counter: Counter = .{};
    const original_count = SourceEvents.activeObserverCount();
    const descriptor = TestSiteEvent.init(.exhaustiveness_failed);
    const plain: SourceEvents.Descriptor = .{ .header = .{ .kind = @backingInt(SourceEvents.Kind.call_exit) } };
    {
        const saved = SourceEvents.enter(.{ .context = &counter, .on_event = Counter.record, .on_invalid = Counter.invalid });
        defer SourceEvents.leave(saved);
        {
            var boundary = host.enterCrashBoundary();
            defer boundary.deinit();
            if (boundary.set() == 0) {
                SourceEvents.roc_source_event(&descriptor.descriptor.header);
                return error.TestUnexpectedResult;
            }
            try std.testing.expectEqual(Termination.comptime_exhaustiveness, host.termination);
            try std.testing.expectEqual(@as(u32, 21), @backingInt(host.comptime_failed_site.?));
        }
        try std.testing.expectEqual(original_count + 1, SourceEvents.activeObserverCount());
        host.resetForRun();
        SourceEvents.roc_source_event(&plain.header);
        try std.testing.expectEqual(@as(usize, 1), counter.count);
    }
    try std.testing.expectEqual(original_count, SourceEvents.activeObserverCount());
    SourceEvents.roc_source_event(@ptrFromInt(@alignOf(SourceEvents.Header)));
}

test "source event adapter preserves stack provenance and restores after Roc crash" {
    if (!sljmp.supported) return error.SkipZigTest;
    var host = CompileTimeHost.init(std.testing.allocator);
    defer host.deinit();
    const binding: TestEventResolver = .{};
    host.source_event_resolver = binding.resolver();
    const frame: SourceEvents.Descriptor = .{
        .header = .{ .kind = @backingInt(SourceEvents.Kind.call_enter) },
        .source = .{ .has_location = 1, .region = .{ .start = 12, .end = 24 }, .line = 7, .column = 9 },
    };
    const message = "native root crashed";
    const original_count = SourceEvents.activeObserverCount();
    {
        var boundary = host.enterCrashBoundary();
        defer boundary.deinit();
        if (boundary.set() == 0) {
            SourceEvents.roc_source_event(&frame.header);
            rocCrashed(host.ops(), message.ptr, message.len);
        }
        try std.testing.expectEqual(Termination.crashed, host.termination);
        try std.testing.expectEqualStrings(message, host.crashMessage().?);
        try std.testing.expectEqualDeep(@as(?base.SourceLoc, .{ .file = 19, .line = 7, .column = 9 }), host.failed_loc);
        try std.testing.expectEqualDeep(@as(?base.Region, base.Region.from_raw_offsets(12, 24)), host.failed_region);
    }
    try std.testing.expectEqual(original_count, SourceEvents.activeObserverCount());
    SourceEvents.roc_source_event(@ptrFromInt(@alignOf(SourceEvents.Header)));
}

test "source event adapter allocation failure restores observer after host OOM" {
    if (!sljmp.supported) return error.SkipZigTest;
    var host = CompileTimeHost.init(std.testing.failing_allocator);
    defer host.deinit();
    const binding: TestEventResolver = .{};
    host.source_event_resolver = binding.resolver();
    const branch = TestSiteEvent.init(.branch_taken);
    const original_count = SourceEvents.activeObserverCount();
    {
        var boundary = host.enterCrashBoundary();
        defer boundary.deinit();
        if (boundary.set() == 0) {
            SourceEvents.roc_source_event(&branch.descriptor.header);
            return error.TestUnexpectedResult;
        }
        try std.testing.expectEqual(Termination.host_oom, host.termination);
        try std.testing.expectEqual(@as(usize, 0), host.comptime_branch_hits.items.len);
    }
    try std.testing.expectEqual(original_count, SourceEvents.activeObserverCount());
    SourceEvents.roc_source_event(@ptrFromInt(@alignOf(SourceEvents.Header)));
}

test "source event adapter runtime rejects invalid metadata before the shared handler" {
    var host = CompileTimeHost.init(std.testing.allocator);
    defer host.deinit();
    const binding: TestEventResolver = .{};
    host.source_event_resolver = binding.resolver();
    host.failed_stmt = @fromBackingInt(@intCast(13));
    host.failed_observation = .{ .checked_error = true };
    const InvalidCapture = struct {
        threadlocal var tag: u32 = 0;
        fn invalid(_: ?*anyopaque, value: u32) callconv(.c) void {
            tag = value;
        }
    };
    InvalidCapture.tag = 0;
    var observer = host.sourceEventObserver();
    observer.on_invalid = InvalidCapture.invalid;
    const saved = SourceEvents.enter(observer);
    defer SourceEvents.leave(saved);
    const descriptor: SourceEvents.Descriptor = .{
        .header = .{ .version = 2, .kind = @backingInt(SourceEvents.Kind.failure) },
    };
    SourceEvents.roc_source_event(&descriptor.header);
    try std.testing.expectEqual(@backingInt(SourceEvents.InvalidTag.UnsupportedVersion), InvalidCapture.tag);
    try std.testing.expectEqual(@as(?lir.LIR.CFStmtId, @fromBackingInt(@intCast(13))), host.failed_stmt);
    try std.testing.expect(host.failed_observation.?.checked_error);
    try std.testing.expectError(error.UnsupportedVersion, host.observeSourceEvent(&descriptor.header));
    try std.testing.expectEqual(@as(?lir.LIR.CFStmtId, @fromBackingInt(@intCast(13))), host.failed_stmt);
    try std.testing.expectEqual(@as(usize, 0), host.events.items.len);
}

test "source event adapter resets partial frame state after known frame allocation OOM" {
    if (!sljmp.supported) return error.SkipZigTest;
    var host = CompileTimeHost.init(std.testing.failing_allocator);
    defer host.deinit();
    const binding: TestEventResolver = .{};
    host.source_event_resolver = binding.resolver();
    // Reserve the presence push independently of arena chunk sizes so the next
    // allocation fails exactly at the known-frame push.
    var presence_storage: [1]bool = undefined;
    host.source_event_frames = .initBuffer(&presence_storage);
    const frame: SourceEvents.Descriptor = .{
        .header = .{ .kind = @backingInt(SourceEvents.Kind.call_enter) },
        .source = .{ .has_location = 1, .region = .{ .start = 12, .end = 24 }, .line = 7, .column = 9 },
    };
    const exit: SourceEvents.Descriptor = .{ .header = .{ .kind = @backingInt(SourceEvents.Kind.call_exit) } };
    const original_count = SourceEvents.activeObserverCount();
    {
        var boundary = host.enterCrashBoundary();
        defer boundary.deinit();
        if (boundary.set() == 0) {
            SourceEvents.roc_source_event(&frame.header);
            return error.TestUnexpectedResult;
        }
        try std.testing.expectEqual(Termination.host_oom, host.termination);
        try std.testing.expectEqual(@as(usize, 1), host.source_event_frames.items.len);
        try std.testing.expect(host.source_event_frames.items[0]);
        try std.testing.expectEqual(@as(usize, 0), host.call_regions.items.len);
    }
    try std.testing.expectEqual(original_count, SourceEvents.activeObserverCount());
    host.resetForRun();
    try std.testing.expectEqual(@as(usize, 0), host.source_event_frames.items.len);
    try std.testing.expectEqual(@as(usize, 0), host.call_regions.items.len);
    host.host_arena.deinit();
    host.host_arena = base.SingleThreadArena.init(std.testing.allocator);
    {
        var boundary = host.enterCrashBoundary();
        defer boundary.deinit();
        SourceEvents.roc_source_event(&frame.header);
        try std.testing.expectEqual(@as(usize, 1), host.call_regions.items.len);
        SourceEvents.roc_source_event(&exit.header);
        try std.testing.expectEqual(@as(usize, 0), host.source_event_frames.items.len);
        try std.testing.expectEqual(@as(usize, 0), host.call_regions.items.len);
    }
    try std.testing.expectEqual(original_count, SourceEvents.activeObserverCount());
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

fn allocateBytes(allocator: Allocator, len: usize, alignment: usize) ?[*]u8 {
    return switch (alignment) {
        1 => (allocator.alignedAlloc(u8, .@"1", len) catch return null).ptr,
        2 => (allocator.alignedAlloc(u8, .@"2", len) catch return null).ptr,
        4 => (allocator.alignedAlloc(u8, .@"4", len) catch return null).ptr,
        8 => (allocator.alignedAlloc(u8, .@"8", len) catch return null).ptr,
        16 => (allocator.alignedAlloc(u8, .@"16", len) catch return null).ptr,
        else => base.invariant("{s}", .{"unsupported compile-time RocOps allocation alignment"}),
    };
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

test "compile-time host declarations are referenced" {
    std.testing.refAllDecls(@This());
}
