//! Admission of bounded cache-maintenance tasks at foreground safe points.
//! No detached thread or helper process owns progress. Startup completes one
//! overdue allowance synchronously; coordinator waits can admit optional units.
//! Serial coordination and linking never grant optional capacity. A filesystem
//! operation can block: operation bounds are not wall-clock deadlines.

const std = @import("std");
const artifacts = @import("artifact_store.zig");
const maintenance = @import("cache_maintenance.zig");

pub const Limits = maintenance.Limits;
pub const CleanupStats = maintenance.Stats;

/// Explicit demand, including the coordinator's own CPU slot. `tasks` counts
/// queued plus executing tasks, never idle workers or a sampled queue length.
pub const Demand = union(enum) {
    serial,
    linking,
    waiting: struct { budget: usize, tasks: usize },

    pub fn hasCapacity(self: Demand) bool {
        return switch (self) {
            .serial, .linking => false,
            .waiting => |d| d.budget > 1 and d.tasks < d.budget,
        };
    }
};

var active: std.atomic.Value(?*Session) = .init(null);

/// Command-owned maintenance admission state, registered at coordinator safe points.
pub const Session = struct {
    store: artifacts.Store,
    limits: Limits,
    next_compulsory: i64 = 0,
    // Only maintenance admission uses this mutex. Compiler scheduling never
    // takes it, and concurrent callbacks skip it instead of waiting.
    mutex: std.atomic.Mutex = .unlocked,

    pub fn init(allocator: std.mem.Allocator, io: std.Io, root: []const u8, limits: Limits) std.mem.Allocator.Error!Session {
        return .{ .store = try artifacts.Store.init(allocator, io, root), .limits = limits };
    }

    pub fn start(self: *Session) void {
        active.store(self, .release);
        self.tick(.serial);
    }

    pub fn deinit(self: *Session) void {
        active.store(null, .release);
        self.tick(.serial);
        self.store.deinit();
    }

    pub fn tick(self: *Session, demand: Demand) void {
        var stats = maintenance.Stats{};
        self.tickAt(self.store.now(), demand, &stats);
    }

    fn tickAt(self: *Session, now: i64, demand: Demand, stats: *maintenance.Stats) void {
        if (!self.mutex.tryLock()) return;
        defer self.mutex.unlock();
        if (now >= self.next_compulsory) {
            self.next_compulsory = now +| maintenance.Policy.grant_interval;
            maintenance.run(&self.store, self.limits, now, .compulsory, stats);
        }
        if (demand.hasCapacity()) maintenance.run(&self.store, self.limits, now, .optional, stats);
    }
};

/// Called only at completed-task/await-result boundaries, without holding the
/// scheduler's channel locks. The caller resumes foreground work after at most
/// one optional unit; demand is reconsidered at the next boundary.
pub fn safePoint(demand: Demand) void {
    if (active.load(.acquire)) |session| session.tick(demand);
}

test "foreground events grant completed checkpoints and revoke optional admission between units" {
    const allocator = std.testing.allocator;
    const io = std.testing.io;
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    const root = try tmp.dir.realPathFileAlloc(io, ".", allocator);
    defer allocator.free(root);
    var session = try Session.init(allocator, io, root, .{ .object_pack_format_version = 1, .max_size_bytes = 1000 });
    defer session.store.deinit();
    try session.store.write(.{ .namespace = @splat(1), .family = .checked, .key = @splat(0) }, "entry", 1);
    const now = 31 * artifacts.day_ns;
    var stats = maintenance.Stats{};
    session.tickAt(now, .serial, &stats);
    try std.testing.expectEqual(@as(usize, 1), stats.units); // compulsory startup
    const events = [_]struct { demand: Demand, units: usize }{
        .{ .demand = .{ .waiting = .{ .budget = 2, .tasks = 1 } }, .units = 1 },
        .{ .demand = .serial, .units = 0 },
        .{ .demand = .linking, .units = 0 },
        .{ .demand = .{ .waiting = .{ .budget = 4, .tasks = 4 } }, .units = 0 },
        .{ .demand = .{ .waiting = .{ .budget = 1, .tasks = 0 } }, .units = 0 },
        .{ .demand = .{ .waiting = .{ .budget = 4, .tasks = 2 } }, .units = 1 },
    };
    for (events) |event| {
        stats = .{};
        session.tickAt(now, event.demand, &stats);
        try std.testing.expectEqual(event.units, stats.units);
        try std.testing.expectEqual(@as(usize, 0), stats.errors);
    }
    stats = .{};
    session.tickAt(now + maintenance.Policy.grant_interval, .{ .waiting = .{ .budget = 1, .tasks = 1 } }, &stats);
    try std.testing.expectEqual(@as(usize, 1), stats.units); // saturated one-worker progress
}
