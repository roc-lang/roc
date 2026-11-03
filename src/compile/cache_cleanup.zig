//! Cache maintenance runs in a helper process, never in a compiler command's
//! own process. A command only asks, without writing anything, whether its
//! cache root wants maintenance (`maintenance.wanted`), and if so starts a
//! detached helper (`helper_command`) and goes on without waiting for it.
//!
//! Maintenance renames, deletes and rewrites cache files, and a single such
//! filesystem operation can block for many seconds while the kernel commits
//! unrelated writes. Run in the command's process, that wait delays a command
//! whose result is already finished, and a thread does not help: a process
//! whose thread is blocked in such an operation cannot exit until it returns.

const std = @import("std");
const builtin = @import("builtin");
const artifacts = @import("artifact_store.zig");
const maintenance = @import("cache_maintenance.zig");

pub const Limits = maintenance.Limits;
pub const CleanupStats = maintenance.Stats;

/// The internal command a maintenance helper runs:
/// `<compiler> __roc_cache_maintenance <cache root> <max size bytes>`.
pub const helper_command = "__roc_cache_maintenance";

var active: std.atomic.Value(?*Session) = .init(null);

/// Command-owned maintenance requests, made at command start and at
/// coordinator safe points.
pub const Session = struct {
    store: artifacts.Store,
    limits: Limits,
    /// The cache root a helper maintains, as the command was given it.
    cache_root: []const u8,
    /// The compiler executable a helper runs.
    exe: []const u8,
    /// Starts one helper for this session's root. Tests replace it.
    start_helper: *const fn (*Session) void = startHelperProcess,
    /// The monotonic time before which the session does not ask again.
    next_check: i96 = 0,
    /// Helpers started and not yet reaped (POSIX only).
    helpers: [max_tracked_helpers]HelperPid = undefined,
    helper_count: usize = 0,
    // Only maintenance requests use this mutex. Compiler scheduling never
    // takes it, and concurrent callbacks skip it instead of waiting.
    mutex: std.atomic.Mutex = .unlocked,

    const max_tracked_helpers = 8;
    const HelperPid = if (builtin.os.tag == .windows) void else std.posix.pid_t;

    pub fn init(allocator: std.mem.Allocator, io: std.Io, root: []const u8, exe: []const u8, limits: Limits) std.mem.Allocator.Error!Session {
        return .{ .store = try artifacts.Store.init(allocator, io, root), .limits = limits, .cache_root = root, .exe = exe };
    }

    pub fn start(self: *Session) void {
        active.store(self, .release);
        self.tick();
    }

    /// Stops requesting maintenance. A helper still running keeps running;
    /// nothing here waits for it.
    pub fn deinit(self: *Session) void {
        active.store(null, .release);
        self.reapHelpers();
        self.store.deinit();
    }

    /// Start a helper when the root wants one, asking at most once per
    /// grant interval.
    pub fn tick(self: *Session) void {
        if (!self.mutex.tryLock()) return;
        defer self.mutex.unlock();
        const now = std.Io.Timestamp.now(self.store.io, .awake).nanoseconds;
        if (now < self.next_check) return;
        self.next_check = now + maintenance.Policy.grant_interval;
        self.reapHelpers();
        if (maintenance.wanted(&self.store, self.limits, self.store.now())) self.start_helper(self);
    }

    /// Collect helpers that have exited, without waiting for any.
    fn reapHelpers(self: *Session) void {
        if (comptime builtin.os.tag == .windows) return;
        var kept: usize = 0;
        for (self.helpers[0..self.helper_count]) |pid| {
            if (std.c.waitpid(pid, null, std.c.W.NOHANG) == 0) {
                self.helpers[kept] = pid;
                kept += 1;
            }
        }
        self.helper_count = kept;
    }
};

/// Start a detached helper running `helper_command`. It reads and writes no
/// standard stream of the command and lives in its own process group, so
/// neither the command's exit nor its readers wait for it. A helper that
/// fails to start leaves the work for a later command.
fn startHelperProcess(session: *Session) void {
    var max_buffer: [24]u8 = undefined;
    const max = std.fmt.bufPrint(&max_buffer, "{d}", .{session.limits.max_size_bytes}) catch
        unreachable; // a u64 has at most 20 decimal digits
    const argv = [_][]const u8{ session.exe, helper_command, session.cache_root, max };
    const child = std.process.spawn(session.store.io, .{
        .argv = &argv,
        .stdin = .ignore,
        .stdout = .ignore,
        .stderr = .ignore,
        .pgid = if (comptime builtin.os.tag == .windows) null else 0,
        .create_no_window = true,
    }) catch return;
    if (comptime builtin.os.tag == .windows) {
        std.os.windows.CloseHandle(child.id.?);
        std.os.windows.CloseHandle(child.thread_handle);
        return;
    }
    if (session.helper_count < Session.max_tracked_helpers) {
        session.helpers[session.helper_count] = child.id.?;
        session.helper_count += 1;
    }
}

/// The body of a maintenance helper: one owner's run over `root`, up to a
/// pass's units.
pub fn runHelper(allocator: std.mem.Allocator, io: std.Io, root: []const u8, limits: Limits) std.mem.Allocator.Error!void {
    var store = try artifacts.Store.init(allocator, io, root);
    defer store.deinit();
    var stats = maintenance.Stats{};
    maintenance.run(&store, limits, store.now(), maintenance.Policy.helper_units, &stats);
}

/// Called at coordinator safe points, without holding the scheduler's channel
/// locks. It never performs maintenance and never waits for a helper.
pub fn safePoint() void {
    if (active.load(.acquire)) |session| session.tick();
}

const TestHelper = struct {
    var started: usize = 0;

    fn start(_: *Session) void {
        started += 1;
    }
};

test "a command starts a helper for due maintenance and performs none itself" {
    const allocator = std.testing.allocator;
    const io = std.testing.io;
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    const root = try tmp.dir.realPathFileAlloc(io, ".", allocator);
    defer allocator.free(root);
    const limits: Limits = .{ .object_pack_format_version = 1, .max_size_bytes = 1000 };
    const identity: artifacts.Identity = .{ .namespace = @splat(1), .family = .checked, .key = @splat(0) };
    var session = try Session.init(allocator, io, root, "roc", limits);
    session.start_helper = TestHelper.start;
    // An entry long past retention: a due pass deletes it.
    try session.store.write(identity, "entry", 1);
    TestHelper.started = 0;
    session.start();
    session.deinit();
    try std.testing.expectEqual(@as(usize, 1), TestHelper.started);

    // The command left the cache exactly as it found it.
    var store = try artifacts.Store.init(allocator, io, root);
    defer store.deinit();
    var root_dir = try store.openRoot(false);
    defer root_dir.close(io);
    try std.testing.expectError(error.FileNotFound, root_dir.access(io, "maintenance", .{}));
    try std.testing.expectError(error.FileNotFound, root_dir.access(io, "maintenance.lock", .{}));

    // The helper's run performs the pass.
    var stats = maintenance.Stats{};
    maintenance.run(&store, limits, 31 * artifacts.day_ns, maintenance.Policy.helper_units, &stats);
    try std.testing.expectEqual(@as(usize, 1), stats.deletions);
    try std.testing.expectEqual(null, try store.acquire(identity, 31 * artifacts.day_ns));
}

test "no helper starts while the root's owner runs or before the next grant" {
    const allocator = std.testing.allocator;
    const io = std.testing.io;
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    const root = try tmp.dir.realPathFileAlloc(io, ".", allocator);
    defer allocator.free(root);
    const limits: Limits = .{ .object_pack_format_version = 1, .max_size_bytes = 1000 };
    var store = try artifacts.Store.init(allocator, io, root);
    defer store.deinit();
    try store.write(.{ .namespace = @splat(1), .family = .checked, .key = @splat(0) }, "entry", 1);
    const now = 31 * artifacts.day_ns;
    try std.testing.expect(maintenance.wanted(&store, limits, now));
    {
        var root_dir = try store.openRoot(false);
        defer root_dir.close(io);
        const owner = try root_dir.createFile(io, "maintenance.lock", .{ .lock = .exclusive, .lock_nonblocking = true });
        defer owner.close(io);
        try std.testing.expect(!maintenance.wanted(&store, limits, now));
    }
    var stats = maintenance.Stats{};
    maintenance.run(&store, limits, now, 1, &stats);
    try std.testing.expect(!maintenance.wanted(&store, limits, now));
    try std.testing.expect(maintenance.wanted(&store, limits, now + maintenance.Policy.grant_interval));
}
