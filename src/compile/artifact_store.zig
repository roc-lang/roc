//! Direct file storage for checked modules, Boxy objects and dev link hosts.
//! Each identity names one file. Writers stage complete bytes beside it and
//! atomically rename them into place; readers own an open file or mapping.

const std = @import("std");
const base = @import("base");
const Allocator = std.mem.Allocator;
const Io = std.Io;
const Dir = Io.Dir;

/// Separates direct files from the retired indexed store.
pub const format_directory = "files-v1";

/// Independent compiler cache families.
pub const Family = enum {
    checked,
    canonicalized,
    pairing,
    boxy_object,
    /// A platform host linked once for fast dev links of its executables.
    prelinked_host,
    /// The host executable a composed `.boxy` dev run launches.
    run_host,
};

/// Explicit producer identity for one file.
pub const Identity = struct {
    namespace: [32]u8,
    family: Family,
    group: [32]u8 = @splat(0),
    key: [32]u8,
};

/// Optional storage failures; allocation failures remain explicit.
pub const Error = error{ IoFailure, OutOfMemory };

/// Hash the bytes of a cache identity component.
pub fn hashBytes(bytes: []const u8) [32]u8 {
    var result: [32]u8 = undefined;
    base.Sha256.hash(bytes, &result, .{});
    return result;
}

/// Direct storage rooted in one cache directory.
pub const Store = struct {
    allocator: Allocator,
    io: Io,
    root: []u8,

    pub fn init(allocator: Allocator, io: Io, cache_root: []const u8) Allocator.Error!Store {
        return .{ .allocator = allocator, .io = io, .root = try std.fs.path.join(allocator, &.{ cache_root, format_directory }) };
    }

    pub fn deinit(self: *Store) void {
        self.allocator.free(self.root);
    }

    pub fn groupPath(self: *const Store, allocator: Allocator, identity: Identity) Allocator.Error![]u8 {
        return std.fs.path.join(allocator, &.{ self.root, &std.fmt.bytesToHex(identity.namespace, .lower), @tagName(identity.family), &std.fmt.bytesToHex(identity.group, .lower) });
    }

    pub fn entryPath(self: *const Store, allocator: Allocator, identity: Identity) Allocator.Error![]u8 {
        const group = try self.groupPath(allocator, identity);
        defer allocator.free(group);
        return std.fmt.allocPrint(allocator, "{s}{s}{s}.data", .{ group, std.fs.path.sep_str, std.fmt.bytesToHex(identity.key, .lower) });
    }

    pub fn reserve(self: *const Store, identity: Identity) Error!Staging {
        const destination = try self.entryPath(self.allocator, identity);
        errdefer self.allocator.free(destination);
        Dir.cwd().createDirPath(self.io, std.fs.path.dirname(destination).?) catch return error.IoFailure;
        while (true) {
            var nonce: [16]u8 = undefined;
            self.io.random(&nonce);
            const path = try std.fmt.allocPrint(self.allocator, "{s}.{s}.pending", .{ destination, std.fmt.bytesToHex(nonce, .lower) });
            const file = Dir.cwd().createFile(self.io, path, .{ .exclusive = true }) catch |err| {
                self.allocator.free(path);
                switch (err) {
                    error.PathAlreadyExists => continue,
                    else => return error.IoFailure,
                }
            };
            file.close(self.io);
            return .{ .allocator = self.allocator, .io = self.io, .path = path, .destination = destination };
        }
    }

    pub fn acquire(self: *const Store, identity: Identity) Error!?Artifact {
        const path = try self.entryPath(self.allocator, identity);
        errdefer self.allocator.free(path);
        const file = Dir.cwd().openFile(self.io, path, .{}) catch |err| {
            switch (err) {
                error.FileNotFound => {
                    self.allocator.free(path);
                    return null;
                },
                else => return error.IoFailure,
            }
        };
        errdefer file.close(self.io);
        const stat = file.stat(self.io) catch return error.IoFailure;
        if (stat.kind != .file) return error.IoFailure;
        return .{ .allocator = self.allocator, .io = self.io, .path = path, .file = file, .size = stat.size };
    }

    pub fn read(self: *const Store, allocator: Allocator, identity: Identity) Error!?[]u8 {
        var artifact = (try self.acquire(identity)) orelse return null;
        defer artifact.deinit();
        return try artifact.read(allocator);
    }

    pub fn write(self: *const Store, identity: Identity, bytes: []const u8) Error!void {
        var stage = try self.reserve(identity);
        defer stage.deinit();
        Dir.cwd().writeFile(self.io, .{ .sub_path = stage.path, .data = bytes }) catch return error.IoFailure;
        try stage.publish();
    }
};

/// An unpublished file owned by one writer.
pub const Staging = struct {
    allocator: Allocator,
    io: Io,
    path: []u8,
    destination: []u8,

    pub fn deinit(self: *Staging) void {
        Dir.cwd().deleteFile(self.io, self.path) catch {};
        self.allocator.free(self.path);
        self.allocator.free(self.destination);
    }

    pub fn publish(self: *Staging) Error!void {
        Dir.cwd().rename(self.path, Dir.cwd(), self.destination, self.io) catch return error.IoFailure;
    }
};

/// An open immutable cache payload owned by its reader.
pub const Artifact = struct {
    allocator: Allocator,
    io: Io,
    path: []u8,
    file: Io.File,
    size: u64,

    pub fn deinit(self: *Artifact) void {
        self.file.close(self.io);
        self.allocator.free(self.path);
    }

    pub fn read(self: *const Artifact, allocator: Allocator) Error![]u8 {
        const size = std.math.cast(usize, self.size) orelse return error.OutOfMemory;
        const bytes = try allocator.alloc(u8, size);
        errdefer allocator.free(bytes);
        const len = self.file.readPositionalAll(self.io, bytes, 0) catch return error.IoFailure;
        if (len != size) return error.IoFailure;
        return bytes;
    }
};

/// Invocation-owned scratch directory, independent of persistent cache entries.
pub const Scratch = struct {
    allocator: Allocator,
    io: Io,
    directory: []u8,
    keep: bool = false,

    pub fn create(allocator: Allocator, io: Io, cache_root: []const u8) Error!Scratch {
        const root = try std.fs.path.join(allocator, &.{ cache_root, "tmp" });
        defer allocator.free(root);
        Dir.cwd().createDirPath(io, root) catch return error.IoFailure;
        while (true) {
            var nonce: [16]u8 = undefined;
            io.random(&nonce);
            const directory = try std.fs.path.join(allocator, &.{ root, &std.fmt.bytesToHex(nonce, .lower) });
            Dir.cwd().createDir(io, directory, .default_dir) catch |err| {
                allocator.free(directory);
                switch (err) {
                    error.PathAlreadyExists => continue,
                    else => return error.IoFailure,
                }
            };
            return .{ .allocator = allocator, .io = io, .directory = directory };
        }
    }

    pub fn path(self: *const Scratch) []const u8 {
        return self.directory;
    }

    pub fn deinit(self: *Scratch) void {
        if (!self.keep) Dir.cwd().deleteTree(self.io, self.directory) catch {};
        self.allocator.free(self.directory);
    }
};

test "direct cache publication preserves readers and discards abandoned writes" {
    const gpa = std.testing.allocator;
    const io = std.testing.io;
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    const root = try tmp.dir.realPathFileAlloc(io, ".", gpa);
    defer gpa.free(root);
    var store = try Store.init(gpa, io, root);
    defer store.deinit();
    const identity = Identity{ .namespace = @splat(1), .family = .checked, .key = @splat(2) };
    try std.testing.expect((try store.acquire(identity)) == null);
    try store.write(identity, "first");
    var reader = (try store.acquire(identity)).?;
    defer reader.deinit();
    {
        var abandoned = try store.reserve(identity);
        defer abandoned.deinit();
        try Dir.cwd().writeFile(io, .{ .sub_path = abandoned.path, .data = "incomplete" });
        const current = (try store.read(gpa, identity)).?;
        defer gpa.free(current);
        try std.testing.expectEqualStrings("first", current);
    }
    try store.write(identity, "replacement");
    const old = try reader.read(gpa);
    defer gpa.free(old);
    try std.testing.expectEqualStrings("first", old);
    const current = (try store.read(gpa, identity)).?;
    defer gpa.free(current);
    try std.testing.expectEqualStrings("replacement", current);
    var dir = try Dir.cwd().openDir(io, store.root, .{ .iterate = true });
    defer dir.close(io);
    var walker = try dir.walk(gpa);
    defer walker.deinit();
    var files: usize = 0;
    while (try walker.next(io)) |entry| if (entry.kind == .file) {
        try std.testing.expect(std.mem.endsWith(u8, entry.basename, ".data"));
        files += 1;
    };
    try std.testing.expectEqual(@as(usize, 1), files);
}
