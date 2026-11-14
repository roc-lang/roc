//! Direct file storage for checked modules, Boxy objects, dev link hosts and
//! test outcomes.
//! Each identity names one file. Writers stage complete bytes beside it and
//! atomically rename them into place; readers own an open file or mapping.
//! Every file of a framed family begins and ends with its own total length
//! (`length_prefix_len`, `length_trailer_len`), which a reader compares with
//! the file it loaded before trusting any other byte.

const std = @import("std");
const base = @import("base");
const Allocator = std.mem.Allocator;
const Io = std.Io;
const Dir = Io.Dir;

/// Separates direct files from the retired indexed store, and names the
/// file framing: version 2 files of framed families begin and end with their
/// length.
pub const format_directory = "files-v2";

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
    /// The silently passing test roots of one checked module under one
    /// execution mode (`cli/test_outcome_file.zig`).
    test_outcomes,

    /// Whether this family's files are framed by their length. A prelinked
    /// host record is copied into outputs by file range from block-aligned
    /// offsets and its formats carry their own total size in their headers;
    /// a run host is an executable copied whole and launched. Neither can
    /// hold a prefix.
    pub fn framed(self: Family) bool {
        return switch (self) {
            .checked, .canonicalized, .pairing, .boxy_object, .test_outcomes => true,
            .prelinked_host, .run_host => false,
        };
    }
};

/// The bytes a framed file begins with: its total length in bytes
/// (prefix and trailer included), little-endian, then zeros. Sixteen bytes
/// keep the payload that follows at the 16-byte alignment in-place loads need.
pub const length_prefix_len: usize = 16;

/// The bytes a framed file ends with: its total length once more, read
/// unaligned. Entries are renamed into place without being synced, so after a
/// power loss a file can keep its full length while blocks that never reached
/// the disk read as zeros; flushing usually proceeds from the front, so the
/// trailer is what catches a file whose tail never reached the disk.
pub const length_trailer_len: usize = 8;

/// The bytes a framed file adds around its payload.
pub const framing_len: usize = length_prefix_len + length_trailer_len;

/// The prefix of a framed file whose payload is `payload_len` bytes.
pub fn lengthPrefix(payload_len: u64) [length_prefix_len]u8 {
    var prefix: [length_prefix_len]u8 = @splat(0);
    std.mem.writeInt(u64, prefix[0..8], payload_len + framing_len, .little);
    return prefix;
}

/// The trailer of a framed file whose payload is `payload_len` bytes.
pub fn lengthTrailer(payload_len: u64) [length_trailer_len]u8 {
    var trailer: [length_trailer_len]u8 = undefined;
    std.mem.writeInt(u64, &trailer, payload_len + framing_len, .little);
    return trailer;
}

/// Whether a framed file of `file_len` bytes, whose first
/// `length_prefix_len` bytes are `prefix` and last `length_trailer_len` bytes
/// are `trailer`, records its own length at both ends. This is the one
/// length-integrity check every framed file gets; nothing else in it is
/// trusted before it passes. It detects truncation, appended bytes and a
/// zeroed tail, not loss confined to the middle of a file, and computes no
/// checksum.
pub fn lengthsMatch(file_len: u64, prefix: *const [length_prefix_len]u8, trailer: *const [length_trailer_len]u8) bool {
    if (file_len < framing_len) return false;
    return std.mem.readInt(u64, prefix[0..8], .little) == file_len and
        std.mem.readInt(u64, trailer, .little) == file_len;
}

/// `lengthsMatch` over a complete file in memory.
pub fn fileLengthsMatch(file: []const u8) bool {
    if (file.len < framing_len) return false;
    return lengthsMatch(file.len, file[0..length_prefix_len], file[file.len - length_trailer_len ..][0..length_trailer_len]);
}

/// The payload of a framed file that passed `fileLengthsMatch`.
pub fn payload(file: []const u8) []const u8 {
    return file[length_prefix_len .. file.len - length_trailer_len];
}

/// Which file a reader loaded, so discarding it cannot remove a file another
/// writer has since renamed into its place.
pub const Stamp = struct {
    inode: Io.File.INode,
    size: u64,
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
        return .{ .allocator = self.allocator, .io = self.io, .path = path, .file = file, .size = stat.size, .inode = stat.inode };
    }

    pub fn read(self: *const Store, allocator: Allocator, identity: Identity) Error!?[]u8 {
        var artifact = (try self.acquire(identity)) orelse return null;
        defer artifact.deinit();
        return try artifact.read(allocator);
    }

    /// Delete the entry file for `identity` when it is still the file
    /// `stamp` describes. A writer that renamed a new file into place since
    /// it was loaded gives the path a new inode, which is left alone. Deleting
    /// is best effort: a failure leaves an entry the next reader rejects again.
    pub fn discard(self: *const Store, identity: Identity, stamp: Stamp) void {
        const path = self.entryPath(self.allocator, identity) catch return;
        defer self.allocator.free(path);
        const current = Dir.cwd().statFile(self.io, path, .{}) catch return;
        if (current.inode != stamp.inode or current.size != stamp.size) return;
        Dir.cwd().deleteFile(self.io, path) catch {};
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
    inode: Io.File.INode,

    pub fn deinit(self: *Artifact) void {
        self.file.close(self.io);
        self.allocator.free(self.path);
    }

    /// Which file this is, for `Store.discard`.
    pub fn stamp(self: *const Artifact) Stamp {
        return .{ .inode = self.inode, .size = self.size };
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

test "a framed file must record its length at both ends" {
    const payload_len = 5;
    var file: [framing_len + payload_len + 1]u8 = @splat(0xAB);
    const total = framing_len + payload_len;
    @memcpy(file[0..length_prefix_len], &lengthPrefix(payload_len));
    @memcpy(file[total - length_trailer_len .. total], &lengthTrailer(payload_len));
    try std.testing.expect(fileLengthsMatch(file[0..total]));
    try std.testing.expectEqualSlices(u8, file[length_prefix_len .. total - length_trailer_len], payload(file[0..total]));

    // Truncated by one to eight bytes, or one byte too long.
    for (1..length_trailer_len + 1) |cut| try std.testing.expect(!fileLengthsMatch(file[0 .. total - cut]));
    try std.testing.expect(!fileLengthsMatch(&file));
    // Too short to hold both lengths.
    try std.testing.expect(!fileLengthsMatch(file[0 .. framing_len - 1]));

    // A zeroed trailer, and a trailer that disagrees with the prefix.
    var damaged = file;
    @memset(damaged[total - length_trailer_len .. total], 0);
    try std.testing.expect(!fileLengthsMatch(damaged[0..total]));
    damaged = file;
    std.mem.writeInt(u64, damaged[total - length_trailer_len ..][0..8], total + 1, .little);
    try std.testing.expect(!fileLengthsMatch(damaged[0..total]));
    // A prefix that disagrees with the file.
    damaged = file;
    std.mem.writeInt(u64, damaged[0..8], total + 1, .little);
    try std.testing.expect(!fileLengthsMatch(damaged[0..total]));
}

test "discard removes the loaded file but not one renamed over it since" {
    const gpa = std.testing.allocator;
    const io = std.testing.io;
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    const root = try tmp.dir.realPathFileAlloc(io, ".", gpa);
    defer gpa.free(root);
    var store = try Store.init(gpa, io, root);
    defer store.deinit();
    const identity = Identity{ .namespace = @splat(1), .family = .checked, .key = @splat(3) };

    try store.write(identity, "stale");
    var stale = (try store.acquire(identity)).?;
    const stale_stamp = stale.stamp();
    stale.deinit();
    try store.write(identity, "fresh");
    store.discard(identity, stale_stamp);
    const kept = (try store.read(gpa, identity)).?;
    defer gpa.free(kept);
    try std.testing.expectEqualStrings("fresh", kept);

    var fresh = (try store.acquire(identity)).?;
    const fresh_stamp = fresh.stamp();
    fresh.deinit();
    store.discard(identity, fresh_stamp);
    try std.testing.expect((try store.acquire(identity)) == null);
}
