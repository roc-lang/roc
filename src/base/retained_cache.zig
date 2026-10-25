//! Owned, validated immutable cache records. This is an acquisition lifetime,
//! not a storage namespace: records stay at their existing cache paths.
const std = @import("std");
const Allocator = std.mem.Allocator;

/// The persisted format boundary validated by the owning native loader.
pub const Kind = enum { native_payload_v1, native_component_v1, native_offer_v3 };

/// Validation work and retained storage, separate from publication work.
pub const Metrics = struct {
    acquisitions: u64 = 0,
    bytes_read: u64 = 0,
    bytes_hashed: u64 = 0,
    retained_bytes: u64 = 0,
    retained_records: u64 = 0,
};

/// A compilation's acquisitions, also retained by captured native artifact sets.
pub const Owner = struct {
    allocator: Allocator,
    io: std.Io,
    domain: [32]u8,
    metrics: Metrics = .{},
    references: std.atomic.Value(usize) = .init(1),
    records: std.ArrayList(*Record) = .empty,
    resources: std.ArrayList(Resource) = .empty,
    pub const Resource = struct { context: *anyopaque, deinit: *const fn (*anyopaque) void };

    pub fn adopt(self: *Owner, resource: Resource) Allocator.Error!void {
        try self.resources.append(self.allocator, resource);
    }

    pub fn create(allocator: Allocator, io: std.Io, domain: [32]u8) Allocator.Error!*Owner {
        const owner = try allocator.create(Owner);
        owner.* = .{ .allocator = allocator, .io = io, .domain = domain };
        return owner;
    }

    pub fn retain(self: *Owner) void {
        _ = self.references.fetchAdd(1, .monotonic);
    }

    pub fn release(self: *Owner) void {
        if (self.references.fetchSub(1, .acq_rel) != 1) return;
        for (self.resources.items) |resource| resource.deinit(resource.context);
        self.resources.deinit(self.allocator);
        for (self.records.items) |record| {
            record.file.close(self.io);
            self.allocator.free(record.bytes);
            self.allocator.free(record.path);
            self.allocator.destroy(record);
        }
        self.records.deinit(self.allocator);
        self.allocator.destroy(self);
    }

    /// Own and validate bytes while holding their immutable file generation.
    /// Callers validate the bespoke format before using the reference.
    pub fn acquire(self: *Owner, path: []const u8, expected: [32]u8, kind: Kind) (Allocator.Error || error{ Unavailable, Corrupt })!*const Record {
        self.metrics.acquisitions += 1;
        const file = try pin(std.Io.Dir.cwd(), self.io, path, .shared);
        errdefer file.close(self.io);
        var reader = file.reader(self.io, &.{});
        const bytes = reader.interface.allocRemaining(self.allocator, .limited(1024 * 1024 * 1024)) catch |err| switch (err) {
            error.OutOfMemory => return error.OutOfMemory,
            else => return error.Unavailable,
        };
        errdefer self.allocator.free(bytes);
        self.metrics.bytes_read += bytes.len;
        self.metrics.bytes_hashed += bytes.len;
        var hash: [32]u8 = undefined;
        std.crypto.hash.sha2.Sha256.hash(bytes, &hash, .{});
        if (!std.mem.eql(u8, &hash, &expected)) return error.Corrupt;
        const owned_path = try self.allocator.dupe(u8, path);
        errdefer self.allocator.free(owned_path);
        const record = try self.allocator.create(Record);
        errdefer self.allocator.destroy(record);
        record.* = .{ .owner = self, .file = file, .bytes = bytes, .digest = hash, .path = owned_path, .kind = kind };
        try self.records.append(self.allocator, record);
        self.metrics.retained_bytes += bytes.len + owned_path.len + @sizeOf(Record);
        self.metrics.retained_records += 1;
        return record;
    }
};

/// Validated bytes and their retained storage generation. Only Owner constructs
/// these references. Retain owner when a reference crosses an ownership boundary.
pub const Record = struct {
    kind: Kind,
    owner: *Owner,
    file: std.Io.File,
    bytes: []const u8,
    digest: [32]u8,
    path: []const u8,
};

/// Cleanup never removes a generation retained by a reader or publisher.
/// Failure to acquire ownership leaves optional storage for a later sweep.
pub fn remove(dir: std.Io.Dir, io: std.Io, path: []const u8) bool {
    // Other cache families have their existing ownership protocols. Indexed
    // native records coordinate their name-to-generation mapping explicitly.
    const indexed = std.mem.endsWith(u8, path, ".rpa") or std.mem.endsWith(u8, path, ".rpm") or std.mem.endsWith(u8, path, ".rpi");
    var guard = if (indexed) Guard.acquire(dir, io, path) catch return false else null;
    defer if (guard) |*held| held.release();
    const file = dir.openFile(io, path, .{}) catch return false;
    defer file.close(io);
    if (!(file.tryLock(io, .exclusive) catch return false)) return false;
    dir.deleteFile(io, path) catch return false;
    return true;
}

/// Serializes only mapping changes (open-and-pin, replace, unlink). Readers
/// retain the record's own shared lock after releasing this short guard.
/// One stable guard covers the payload/metadata owner, another the offer index;
/// no lock is allocated per artifact and no guard spans encoding or hashing.
pub const Guard = struct {
    file: std.Io.File,
    io: std.Io,

    pub fn acquire(dir: std.Io.Dir, io: std.Io, path: []const u8) error{Unavailable}!Guard {
        // All three record kinds live one directory below their owner:
        // owner/{payload,metadata}/record and origin/offers/key/record.
        const parent = std.fs.path.dirname(path) orelse ".";
        var buffer: [std.fs.max_path_bytes]u8 = undefined;
        const lock_path = std.fmt.bufPrint(&buffer, "{s}/../records.lock", .{parent}) catch return error.Unavailable;
        const file = dir.createFile(io, lock_path, .{ .read = true, .truncate = false }) catch return error.Unavailable;
        errdefer file.close(io);
        file.lock(io, .exclusive) catch return error.Unavailable;
        return .{ .file = file, .io = io };
    }

    pub fn release(self: *Guard) void {
        self.file.close(self.io);
    }
};

/// Pin exactly the generation currently named by path, atomically with respect
/// to repair and cleanup. Byte validation occurs outside the mapping guard.
pub fn pin(dir: std.Io.Dir, io: std.Io, path: []const u8, mode: std.Io.File.Lock) error{Unavailable}!std.Io.File {
    var guard = try Guard.acquire(dir, io, path);
    defer guard.release();
    const file = dir.openFile(io, path, .{}) catch return error.Unavailable;
    errdefer file.close(io);
    if (!(file.tryLock(io, mode) catch return error.Unavailable)) return error.Unavailable;
    return file;
}
