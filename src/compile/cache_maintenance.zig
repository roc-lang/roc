//! Resumable cache maintenance. One owner and one fixed-size checkpoint cover
//! all shards and compiler compatibility identities in a shared store.

const std = @import("std");
const artifacts = @import("artifact_store.zig");
const catalog = artifacts.catalog;
const Io = std.Io;
const Dir = Io.Dir;
const put = catalog.put;
const get = catalog.get;

/// Current pack format and eventual payload-size target.
pub const Limits = struct { object_pack_format_version: u32, max_size_bytes: u64 };
/// Completed work and actual catalog I/O for one maintenance admission.
pub const Stats = struct {
    units: usize = 0,
    records: usize = 0,
    metadata: usize = 0,
    deletions: usize = 0,
    errors: usize = 0,
    io: catalog.Counters = .{},
};

/// Shared cadence and operation-count budgets for maintenance grants.
pub const Policy = struct {
    pub const interval: i64 = 5 * artifacts.minute_ns;
    pub const overdue: i64 = artifacts.minute_ns;
    pub const grant_interval: i64 = std.time.ns_per_s;
    pub const ordinary_units: usize = 1;
    pub const pressure_units: usize = 8;
};

/// Capacity offered by the caller, subject to the shared-root grant policy.
pub const Admission = enum { optional, compulsory };
const Phase = enum(u8) { boundaries, age, candidates, evict };
const state_size = 2048;

/// Fixed-size resumable traversal hints, revalidated before reclamation.
pub const Checkpoint = struct {
    due: i64 = 0,
    next_grant: i64 = 0,
    phase: Phase = .boundaries,
    shard: u8 = 0,
    generation: u64 = 0,
    boundaries: [artifacts.shard_count]u64 = @splat(0),
    bytes: [artifacts.shard_count]u64 = @splat(0),
    candidates: [artifacts.shard_count]?catalog.Key = @splat(null),

    fn oversized(self: *const Checkpoint, limit: u64) bool {
        var total: u64 = 0;
        for (self.bytes) |size| total +|= size;
        return total > limit;
    }

    fn encode(self: *const Checkpoint) [state_size]u8 {
        var bytes: [state_size]u8 = @splat(0);
        @memcpy(bytes[8..16], "ROCMAI01");
        put(i64, &bytes, 16, self.due);
        put(i64, &bytes, 24, self.next_grant);
        bytes[32] = @backingInt(self.phase);
        bytes[33] = self.shard;
        put(u64, &bytes, 40, self.generation);
        for (0..artifacts.shard_count) |i| {
            const offset = 48 + i * 88;
            put(u64, &bytes, offset, self.boundaries[i]);
            put(u64, &bytes, offset + 8, self.bytes[i]);
            if (self.candidates[i]) |key| {
                bytes[offset + 16] = 1;
                @memcpy(bytes[offset + 24 ..][0..64], &key);
            }
        }
        put(u64, &bytes, 0, std.hash.Wyhash.hash(0, bytes[8..]));
        return bytes;
    }

    fn decode(bytes: *const [state_size]u8) ?Checkpoint {
        if (!std.mem.eql(u8, bytes[8..16], "ROCMAI01") or get(u64, bytes, 0) != std.hash.Wyhash.hash(0, bytes[8..])) return null;
        var state: Checkpoint = .{
            .due = get(i64, bytes, 16),
            .next_grant = get(i64, bytes, 24),
            .phase = std.enums.fromInt(Phase, bytes[32]) orelse return null,
            .shard = bytes[33],
            .generation = get(u64, bytes, 40),
        };
        if (state.shard >= artifacts.shard_count) return null;
        for (0..artifacts.shard_count) |i| {
            const offset = 48 + i * 88;
            state.boundaries[i] = get(u64, bytes, offset);
            state.bytes[i] = get(u64, bytes, offset + 8);
            if (bytes[offset + 16] > 1) return null;
            if (bytes[offset + 16] == 1) state.candidates[i] = bytes[offset + 24 ..][0..64].*;
        }
        return state;
    }

    fn load(root: Dir, io: Io) artifacts.Error!Checkpoint {
        const file = root.openFile(io, "maintenance", .{}) catch |err| switch (err) {
            error.FileNotFound => return .{},
            else => return error.IoFailure,
        };
        defer file.close(io);
        var bytes: [state_size]u8 = undefined;
        const len = file.readPositionalAll(io, &bytes, 0) catch return error.IoFailure;
        // The cursor is disposable. Artifact metadata and live ownership are
        // revalidated independently before every deletion.
        return if (len == state_size) decode(&bytes) orelse .{} else .{};
    }

    fn save(self: *const Checkpoint, root: Dir, io: Io) artifacts.Error!void {
        const bytes = self.encode();
        root.writeFile(io, .{ .sub_path = "maintenance.tmp", .data = &bytes }) catch return error.IoFailure;
        root.rename("maintenance.tmp", root, "maintenance", io) catch return error.IoFailure;
    }

    fn nextShard(self: *Checkpoint, next: Phase) void {
        self.generation = 0;
        if (self.shard == artifacts.shard_count - 1) {
            self.shard = 0;
            self.phase = next;
        } else self.shard += 1;
    }
};

/// The admission caller provides explicit capacity. This function never polls
/// a compiler queue or guesses whether the machine is idle. A compulsory grant
/// is global to the root, not independently renewed by every parallel build.
pub fn run(store: *const artifacts.Store, limits: Limits, now: i64, admission: Admission, stats: *Stats) void {
    var root = store.openRoot(false) catch return;
    defer root.close(store.io);
    const owner = root.createFile(store.io, "maintenance.lock", .{
        .read = true,
        .truncate = false,
        .lock = .exclusive,
        .lock_nonblocking = true,
    }) catch return;
    defer owner.close(store.io);
    var checkpoint = Checkpoint.load(root, store.io) catch {
        stats.errors += 1;
        return;
    };
    // Detect a checkpoint from a clock that has moved backwards. The only
    // valid future deadlines are one interval/grant ahead of the last writer.
    // Reset traversal hints; artifact metadata still decides eligibility.
    if (checkpoint.due > now +| Policy.interval or checkpoint.next_grant > now +| Policy.grant_interval) checkpoint = .{};
    const pressure = checkpoint.oversized(limits.max_size_bytes);
    if (!pressure and now < checkpoint.due) return;
    if (admission == .compulsory) {
        if (now < checkpoint.next_grant) return;
        if (!pressure and checkpoint.due != 0 and @as(i128, now) - checkpoint.due < Policy.overdue) return;
        checkpoint.next_grant = now +| Policy.grant_interval;
    }
    const allowance = if (admission == .optional) 1 else if (pressure) Policy.pressure_units else Policy.ordinary_units;
    for (0..allowance) |_| {
        step(store, limits, now, &checkpoint, stats) catch {
            stats.errors += 1;
            // Do not retry a failing filesystem operation in this invocation.
            return;
        };
        checkpoint.save(root, store.io) catch {
            stats.errors += 1;
            return;
        };
        stats.units += 1;
        if (!checkpoint.oversized(limits.max_size_bytes) and now < checkpoint.due) break;
    }
}

fn shardBytes(shard: *const artifacts.Shard) u64 {
    return shard.index.header.user[1] +| shard.index.header.user[2] +| shard.index.header.user[3];
}

fn step(store: *const artifacts.Store, limits: Limits, now: i64, checkpoint: *Checkpoint, stats: *Stats) artifacts.Error!void {
    if (checkpoint.phase == .evict) return evict(store, limits, now, checkpoint, stats);
    const number: u4 = @intCast(checkpoint.shard);
    var shard = artifacts.Shard.open(store, number, false, &stats.io) catch |err| {
        switch (err) {
            error.Missing => {
                checkpoint.bytes[number] = 0;
                checkpoint.boundaries[number] = 0;
            },
            error.Busy => {},
            else => stats.errors += 1,
        }
        checkpoint.candidates[number] = null;
        checkpoint.nextShard(switch (checkpoint.phase) {
            .boundaries => .age,
            .age => .candidates,
            .candidates => .evict,
            .evict => unreachable, // evict returns before opening a traversal shard
        });
        return;
    };
    defer shard.close();
    checkpoint.bytes[number] = shardBytes(&shard);
    switch (checkpoint.phase) {
        .boundaries => {
            checkpoint.boundaries[number] = shard.index.header.user[0];
            checkpoint.nextShard(.age);
        },
        .age => {
            var tx = shard.index.transaction();
            const entry = (try tx.lowerBound(.owners, artifacts.storageKey(checkpoint.generation))) orelse {
                checkpoint.nextShard(.candidates);
                return;
            };
            const metadata = try artifacts.Metadata.decode(entry.value);
            if (metadata.generation > checkpoint.boundaries[number]) {
                checkpoint.nextShard(.candidates);
                return;
            }
            if (metadata.generation == std.math.maxInt(u64)) return error.CapacityExceeded;
            checkpoint.generation = metadata.generation + 1;
            stats.records += 1;
            var missing = false;
            if (metadata.state == .published and metadata.family != .scratch) {
                var stem_buffer: [128]u8 = undefined;
                const stem = try artifacts.slotStem(&stem_buffer, metadata.slot);
                var buffer: [140]u8 = undefined;
                const path = std.fmt.bufPrint(&buffer, "{s}.data", .{stem}) catch return error.PathTooLong;
                stats.metadata += 1;
                _ = shard.dir.statFile(store.io, path, .{}) catch |err| switch (err) {
                    error.FileNotFound => blk: {
                        missing = true;
                        break :blk null;
                    },
                    else => return error.IoFailure,
                };
            }
            if (metadata.state == .retiring or missing or metadata.expired(now, limits.object_pack_format_version)) {
                if (shard.retire(metadata.key, metadata.generation)) |removed| {
                    if (removed) stats.deletions += 1;
                    if (!removed and metadata.family == .scratch) checkpoint.generation = metadata.generation;
                } else |err| switch (err) {
                    error.Busy => {},
                    else => stats.errors += 1,
                }
                checkpoint.bytes[number] = shardBytes(&shard);
            }
        },
        .candidates => {
            var tx = shard.index.transaction();
            const candidate = try tx.lowerBound(.recency, @splat(0));
            checkpoint.candidates[number] = if (candidate) |entry| entry.key else null;
            checkpoint.nextShard(.evict);
        },
        .evict => unreachable, // evict returns before opening a traversal shard
    }
}

fn evict(store: *const artifacts.Store, limits: Limits, now: i64, checkpoint: *Checkpoint, stats: *Stats) artifacts.Error!void {
    var selected: ?u4 = null;
    if (checkpoint.oversized(limits.max_size_bytes)) {
        for (checkpoint.candidates, 0..) |candidate, number| {
            const key = candidate orelse continue;
            if (selected == null or std.mem.order(u8, &key, &checkpoint.candidates[selected.?].?) == .lt) selected = @intCast(number);
        }
    }
    const number = selected orelse {
        checkpoint.phase = .boundaries;
        checkpoint.shard = 0;
        checkpoint.generation = 0;
        checkpoint.due = now +| Policy.interval;
        return;
    };
    var shard = artifacts.Shard.open(store, number, false, &stats.io) catch |err| {
        checkpoint.candidates[number] = null;
        switch (err) {
            error.Missing => checkpoint.bytes[number] = 0,
            error.Busy => {},
            else => stats.errors += 1,
        }
        return;
    };
    defer shard.close();
    checkpoint.bytes[number] = shardBytes(&shard);
    var tx = shard.index.transaction();
    const oldest = (try tx.lowerBound(.recency, checkpoint.candidates[number].?)) orelse {
        checkpoint.candidates[number] = null;
        return;
    };
    // Acquisitions can move a candidate forward since the candidate pass.
    // Refresh that shard's lower bound before deciding to evict it.
    if (!std.mem.eql(u8, &oldest.key, &checkpoint.candidates[number].?)) {
        checkpoint.candidates[number] = oldest.key;
        return;
    }
    const key = oldest.value[0..64].*;
    const entry = (try tx.find(.artifacts, key)) orelse return error.CorruptCatalog;
    const metadata = try artifacts.currentMetadata(&tx, key, entry.value);
    if (!std.mem.eql(u8, &oldest.key, &metadata.recency(metadata.slot))) return error.CorruptCatalog;
    if (metadata.indexed_used != metadata.used) {
        try shard.refreshRecency(metadata);
        var refreshed = shard.index.transaction();
        const next = try refreshed.lowerBound(.recency, @splat(0));
        checkpoint.candidates[number] = if (next) |item| item.key else null;
        return;
    }
    if (shard.retire(key, metadata.generation)) |removed| {
        if (removed) stats.deletions += 1;
        checkpoint.bytes[number] = shardBytes(&shard);
        var refreshed = shard.index.transaction();
        const next = try refreshed.lowerBound(.recency, @splat(0));
        checkpoint.candidates[number] = if (next) |item| item.key else null;
    } else |err| switch (err) {
        error.Busy => {
            var after = oldest.key;
            checkpoint.candidates[number] = if (artifacts.advanceKey(&after)) after else null;
        },
        else => {
            stats.errors += 1;
            checkpoint.candidates[number] = null;
        },
    }
}

const Fixture = struct {
    tmp: std.testing.TmpDir,
    store: artifacts.Store,

    fn init() (std.Io.Dir.RealPathFileAllocError || artifacts.Error)!Fixture {
        var tmp = std.testing.tmpDir(.{});
        errdefer tmp.cleanup();
        const root = try tmp.dir.realPathFileAlloc(std.testing.io, ".", std.testing.allocator);
        defer std.testing.allocator.free(root);
        return .{ .tmp = tmp, .store = try artifacts.Store.init(std.testing.allocator, std.testing.io, root) };
    }

    fn deinit(self: *Fixture) void {
        self.store.deinit();
        self.tmp.cleanup();
    }

    fn identity(number: u32) artifacts.Identity {
        var key: [32]u8 = @splat(0);
        std.mem.writeInt(u32, key[0..4], number, .little);
        return .{ .namespace = @splat(1), .family = .checked, .key = key };
    }
};

test "maintenance repairs stale recency before evicting the actual oldest generation" {
    var fixture = try Fixture.init();
    defer fixture.deinit();
    const hot = Fixture.identity(1);
    const number = artifacts.Store.shardFor(hot.indexedKey());
    var other: u32 = 2;
    while (artifacts.Store.shardFor(Fixture.identity(other).indexedKey()) != number) : (other += 1) {}
    const cold = Fixture.identity(other);
    try fixture.store.write(hot, "hot!", 10);
    try fixture.store.write(cold, "cold", 20);
    {
        var read = (try fixture.store.acquire(hot, 100)).?;
        defer read.deinit();
    }
    var checkpoint: Checkpoint = .{ .phase = .evict };
    {
        var counters = catalog.Counters{};
        var shard = try artifacts.Shard.open(&fixture.store, number, false, &counters);
        defer shard.close();
        checkpoint.bytes[number] = shardBytes(&shard);
        var tx = shard.index.transaction();
        checkpoint.candidates[number] = (try tx.lowerBound(.recency, @splat(0))).?.key;
    }
    var stats = Stats{};
    const limits: Limits = .{ .max_size_bytes = 4, .object_pack_format_version = 1 };
    try evict(&fixture.store, limits, 100, &checkpoint, &stats);
    try std.testing.expectEqual(@as(usize, 0), stats.deletions);
    try evict(&fixture.store, limits, 100, &checkpoint, &stats);
    try std.testing.expectEqual(@as(usize, 1), stats.deletions);
    try std.testing.expectEqual(null, try fixture.store.acquire(cold, 100));
    var retained = (try fixture.store.acquire(hot, 100)).?;
    defer retained.deinit();
}

test "short saturated invocations checkpoint bounded units and reach the end of a large shard" {
    var fixture = try Fixture.init();
    defer fixture.deinit();
    for (0..150) |i| try fixture.store.write(Fixture.identity(@intCast(i * artifacts.shard_count)), "old", 1);
    var now: i64 = 31 * artifacts.day_ns;
    var removed: usize = 0;
    for (0..350) |_| {
        var stats = Stats{};
        run(&fixture.store, .{ .max_size_bytes = std.math.maxInt(u64), .object_pack_format_version = 1 }, now, .compulsory, &stats);
        try std.testing.expect(stats.units <= 1);
        try std.testing.expect(stats.records <= 1);
        try std.testing.expect(stats.metadata <= 1);
        try std.testing.expect(stats.deletions <= 1);
        try std.testing.expectEqual(@as(usize, 0), stats.errors);
        removed += stats.deletions;
        now += Policy.grant_interval;
        if (removed == 150) break;
    }
    try std.testing.expectEqual(@as(usize, 150), removed);
}

test "not due does no catalog I/O and pressure increases the root allowance" {
    var fixture = try Fixture.init();
    defer fixture.deinit();
    for (0..20) |i| try fixture.store.write(Fixture.identity(@intCast(i * artifacts.shard_count)), "data", 1);
    var root = try fixture.store.openRoot(false);
    defer root.close(std.testing.io);
    var checkpoint: Checkpoint = .{ .due = Policy.interval };
    try checkpoint.save(root, std.testing.io);
    var stats = Stats{};
    run(&fixture.store, .{ .max_size_bytes = 1, .object_pack_format_version = 1 }, 10, .compulsory, &stats);
    try std.testing.expectEqual(@as(usize, 0), stats.units);
    try std.testing.expectEqual(@as(usize, 0), stats.io.reads);
    checkpoint.due = 0;
    checkpoint.bytes[0] = 80;
    try checkpoint.save(root, std.testing.io);
    var removed: usize = 0;
    for (0..60) |i| {
        stats = .{};
        run(&fixture.store, .{ .max_size_bytes = 4, .object_pack_format_version = 1 }, @intCast(i * Policy.grant_interval), .compulsory, &stats);
        if (i == 0) try std.testing.expectEqual(Policy.pressure_units, stats.units);
        try std.testing.expectEqual(@as(usize, 0), stats.errors);
        removed += stats.deletions;
        if (removed == 19) break;
    }
    try std.testing.expectEqual(@as(usize, 19), removed);
}

test "corrupt checkpoint cannot authorize deleting live staging" {
    var fixture = try Fixture.init();
    defer fixture.deinit();
    var staging = try fixture.store.reserve(Fixture.identity(0), 4, 1);
    defer staging.deinit();
    try Dir.cwd().writeFile(std.testing.io, .{ .sub_path = staging.path, .data = "live" });
    var root = try fixture.store.openRoot(false);
    defer root.close(std.testing.io);
    try root.writeFile(std.testing.io, .{ .sub_path = "maintenance", .data = "truncated cursor" });
    for (0..100) |i| {
        var stats = Stats{};
        run(&fixture.store, .{ .max_size_bytes = 1, .object_pack_format_version = 1 }, 31 * artifacts.day_ns + @as(i64, @intCast(i)) * Policy.grant_interval, .compulsory, &stats);
        try std.testing.expectEqual(@as(usize, 0), stats.deletions);
    }
    try Dir.cwd().access(std.testing.io, staging.path, .{});
}

test "large abandoned scratch traversal resumes after lost and corrupt checkpoints" {
    var fixture = try Fixture.init();
    defer fixture.deinit();
    const allocator = std.testing.allocator;
    const io = std.testing.io;
    const root_path = try fixture.tmp.dir.realPathFileAlloc(io, ".", allocator);
    defer allocator.free(root_path);
    var scratch = try artifacts.Scratch.create(allocator, io, root_path, @splat(9));
    const path = try allocator.dupe(u8, scratch.path());
    defer allocator.free(path);
    var dir = try Dir.cwd().openDir(io, path, .{});
    try dir.createDirPath(io, "nested/deeper");
    for (0..80) |i| {
        var buffer: [64]u8 = undefined;
        const name = try std.fmt.bufPrint(&buffer, "nested/deeper/{d}", .{i});
        try dir.writeFile(io, .{ .sub_path = name, .data = "scratch" });
    }
    dir.close(io);
    const key = scratch.staging.key;
    const generation = scratch.staging.generation;
    const slot = scratch.staging.slot;
    scratch.deinit();
    var counters = artifacts.catalog.Counters{};
    // Simulate termination after a deletion but before the root checkpoint:
    // repeat the same logical generation; the per-scratch cursor resumes it.
    var done = false;
    for (0..150) |i| {
        var shard = try artifacts.Shard.open(&fixture.store, artifacts.Store.shardFor(key), false, &counters);
        defer shard.close();
        if (i == 30) {
            var stem_buffer: [128]u8 = undefined;
            const stem = try artifacts.slotStem(&stem_buffer, slot);
            var name_buffer: [140]u8 = undefined;
            const name = try std.fmt.bufPrint(&name_buffer, "{s}.cursor", .{stem});
            try shard.dir.writeFile(io, .{ .sub_path = name, .data = "corrupt" });
        }
        if (try shard.retire(key, generation)) {
            done = true;
            break;
        }
    }
    try std.testing.expect(done);
    try std.testing.expectError(error.FileNotFound, Dir.cwd().access(io, path, .{}));
}

test "missing payload reconciliation and obsolete namespaces use the same bounded traversal" {
    var fixture = try Fixture.init();
    defer fixture.deinit();
    const io = std.testing.io;
    var old = Fixture.identity(0);
    old.family = .local_pack;
    old.pack_format = 1;
    var fresh = Fixture.identity(16);
    fresh.family = .package_pack;
    fresh.pack_format = 2;
    fresh.namespace = @splat(99);
    try fixture.store.write(old, "obsolete", 1);
    try fixture.store.write(fresh, "fresh", 1);
    {
        var accepted = (try fixture.store.acquire(fresh, 100 * artifacts.day_ns)).?;
        defer accepted.deinit();
        // External eviction is reconciled by checking one owned record.
        try Dir.cwd().deleteFile(io, accepted.path);
    }
    var removed: usize = 0;
    for (0..100) |i| {
        var stats = Stats{};
        run(&fixture.store, .{ .max_size_bytes = std.math.maxInt(u64), .object_pack_format_version = 2 }, 101 * artifacts.day_ns + @as(i64, @intCast(i)) * Policy.grant_interval, .compulsory, &stats);
        try std.testing.expect(stats.records <= 1);
        try std.testing.expect(stats.metadata <= 1);
        try std.testing.expect(stats.deletions <= 1);
        try std.testing.expect(stats.io.reads < 20 * artifacts.catalog.key_bits + 100);
        try std.testing.expect(stats.io.writes <= 2 * (artifacts.catalog.max_changes + 1));
        removed += stats.deletions;
    }
    try std.testing.expectEqual(@as(usize, 2), removed);
    var counters = artifacts.catalog.Counters{};
    var shard = try artifacts.Shard.open(&fixture.store, 0, false, &counters);
    defer shard.close();
    try std.testing.expectEqual(@as(u64, 0), shard.index.header.user[1]);
}

test "maintenance ownership is shared across processes and released after abrupt death" {
    if (comptime @import("builtin").os.tag == .windows) return error.SkipZigTest;
    const C = struct {
        extern "c" fn fork() std.c.pid_t;
        extern "c" fn pipe(*[2]c_int) c_int;
        extern "c" fn open([*:0]const u8, c_int, ...) c_int;
        extern "c" fn flock(c_int, c_int) c_int;
        extern "c" fn read(c_int, [*]u8, usize) isize;
        extern "c" fn write(c_int, [*]const u8, usize) isize;
        extern "c" fn close(c_int) c_int;
        extern "c" fn pause() c_int;
        extern "c" fn kill(std.c.pid_t, c_int) c_int;
        extern "c" fn waitpid(std.c.pid_t, *c_int, c_int) std.c.pid_t;
        extern "c" fn _exit(c_int) noreturn;
    };
    var fixture = try Fixture.init();
    defer fixture.deinit();
    var root = try fixture.store.openRoot(true);
    defer root.close(std.testing.io);
    const file = try root.createFile(std.testing.io, "maintenance.lock", .{});
    file.close(std.testing.io);
    const path = try std.fmt.allocPrintSentinel(std.testing.allocator, "{s}/maintenance.lock", .{fixture.store.root}, 0);
    defer std.testing.allocator.free(path);
    var pipe: [2]c_int = undefined;
    try std.testing.expectEqual(@as(c_int, 0), C.pipe(&pipe));
    defer _ = C.close(pipe[0]);
    var writer_closed = false;
    defer if (!writer_closed) {
        _ = C.close(pipe[1]);
    };
    const pid = C.fork();
    if (pid == 0) {
        // Only async-signal-safe libc/syscalls in the fork child; no Zig Io,
        // allocator, or test harness state is used after fork.
        const fd = C.open(path.ptr, 2); // O_RDWR on supported POSIX hosts
        if (fd < 0 or C.flock(fd, std.posix.LOCK.EX | std.posix.LOCK.NB) != 0) C._exit(1);
        _ = C.write(pipe[1], "1", 1);
        _ = C.close(pipe[1]);
        while (true) _ = C.pause();
    }
    try std.testing.expect(pid > 0);
    _ = C.close(pipe[1]);
    writer_closed = true;
    var waited = false;
    defer if (!waited) {
        _ = C.kill(pid, @intCast(@backingInt(std.posix.SIG.KILL)));
        var status: c_int = 0;
        _ = C.waitpid(pid, &status, 0);
    };
    var ready: [1]u8 = undefined;
    try std.testing.expectEqual(@as(isize, 1), C.read(pipe[0], &ready, 1));
    var stats = Stats{};
    const limits: Limits = .{ .max_size_bytes = 1000, .object_pack_format_version = 1 };
    run(&fixture.store, limits, 1, .compulsory, &stats);
    try std.testing.expectEqual(@as(usize, 0), stats.units);
    _ = C.kill(pid, @intCast(@backingInt(std.posix.SIG.KILL)));
    var status: c_int = 0;
    try std.testing.expectEqual(pid, C.waitpid(pid, &status, 0));
    waited = true;
    run(&fixture.store, limits, 1, .compulsory, &stats);
    try std.testing.expectEqual(@as(usize, 1), stats.units);
}

test "a deletion whose checkpoint fails is safe to repeat" {
    var fixture = try Fixture.init();
    defer fixture.deinit();
    try fixture.store.write(Fixture.identity(0), "expired", 1);
    var root = try fixture.store.openRoot(false);
    defer root.close(std.testing.io);
    const shard = artifacts.Store.shardFor(Fixture.identity(0).indexedKey());
    var checkpoint: Checkpoint = .{ .phase = .age, .shard = shard };
    checkpoint.boundaries[shard] = 1;
    try checkpoint.save(root, std.testing.io);
    try root.createDir(std.testing.io, "maintenance.tmp", .default_dir);
    const limits: Limits = .{ .max_size_bytes = 100, .object_pack_format_version = 1 };
    var stats = Stats{};
    run(&fixture.store, limits, 31 * artifacts.day_ns, .compulsory, &stats);
    try std.testing.expectEqual(@as(usize, 1), stats.deletions);
    try std.testing.expectEqual(@as(usize, 1), stats.errors);
    try std.testing.expectEqual(@as(usize, 0), stats.units);
    try root.deleteDir(std.testing.io, "maintenance.tmp");
    stats = .{};
    run(&fixture.store, limits, 31 * artifacts.day_ns, .compulsory, &stats);
    try std.testing.expectEqual(@as(usize, 0), stats.deletions);
    try std.testing.expectEqual(@as(usize, 0), stats.errors);
    try std.testing.expectEqual(@as(usize, 1), stats.units);
}

test "allocation boundaries terminate passes and later passes cover concurrent insertion" {
    var fixture = try Fixture.init();
    defer fixture.deinit();
    const now = 31 * artifacts.day_ns;
    try fixture.store.write(Fixture.identity(0), "old generation", 1);
    const limits: Limits = .{ .max_size_bytes = 1000, .object_pack_format_version = 1 };
    for (0..artifacts.shard_count) |_| {
        var stats = Stats{};
        run(&fixture.store, limits, now, .optional, &stats);
        try std.testing.expectEqual(@as(usize, 1), stats.units);
    }
    try fixture.store.write(Fixture.identity(0), "fresh replacement", now);
    try fixture.store.write(Fixture.identity(16), "late expired insertion", 1);
    var removed: usize = 0;
    for (0..200) |i| {
        var stats = Stats{};
        run(&fixture.store, limits, now + @as(i64, @intCast(i)) * artifacts.minute_ns, .compulsory, &stats);
        removed += stats.deletions;
    }
    try std.testing.expectEqual(@as(usize, 2), removed);
    try std.testing.expect((try fixture.store.acquire(Fixture.identity(16), now)) == null);
    const fresh = (try fixture.store.read(std.testing.allocator, Fixture.identity(0), now)).?;
    defer std.testing.allocator.free(fresh);
    try std.testing.expectEqualStrings("fresh replacement", fresh);
}
