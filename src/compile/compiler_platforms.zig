//! Compiler-owned Roc platform sources.
//!
//! These platforms are not package URLs and are not resolved from user input.
//! The compiler embeds their source bytes, materializes them to an internal
//! source directory when the normal package pipeline needs file paths, and
//! gives them explicit package identities independent of that directory.

const std = @import("std");
const Sha256 = @import("base").Sha256;
const CoreCtx = @import("ctx").CoreCtx;
const cache_config = @import("cache_config.zig");
const compiler_platform_sources = @import("compiler_platform_sources");

const Allocator = std.mem.Allocator;

/// The platforms whose sources the compiler embeds and owns.
pub const CompilerOwnedPlatform = enum {
    glue,
};

/// Errors from materializing embedded platform sources to disk.
pub const MaterializeError = Allocator.Error || CoreCtx.MakePathError || CoreCtx.WriteError || CoreCtx.RenameError || error{NoHomeDirectory};

/// An embedded platform written out to the internal source directory.
pub const MaterializedPlatform = struct {
    root_file: []const u8,
    root_dir: []const u8,
    root_source_hash: [32]u8,
    content_bytes: u64,
};

const EmbeddedFile = compiler_platform_sources.File;

/// Look up a compiler-owned platform from its app-header ident (`glue`).
pub fn fromHeaderIdent(text: []const u8) ?CompilerOwnedPlatform {
    if (std.mem.eql(u8, text, "glue")) return .glue;
    return null;
}

/// The platform's stable package identity, independent of any file path.
pub fn identity(platform: CompilerOwnedPlatform) []const u8 {
    return switch (platform) {
        .glue => "roc:compiler/platform/glue",
    };
}

/// Look up a compiler-owned platform from its package identity string.
pub fn fromIdentity(text: []const u8) ?CompilerOwnedPlatform {
    if (std.mem.eql(u8, text, identity(.glue))) return .glue;
    return null;
}

/// The resolver group key for the platform's dependency-graph node.
pub fn groupKey(platform: CompilerOwnedPlatform) []const u8 {
    return switch (platform) {
        .glue => "c@roc:compiler/platform/glue",
    };
}

/// Look up a compiler-owned platform from its resolver group key.
pub fn fromGroupKey(group: []const u8) ?CompilerOwnedPlatform {
    if (std.mem.eql(u8, group, groupKey(.glue))) return .glue;
    return null;
}

/// The header spec text an app writes to reference this platform.
pub fn headerSpecText(platform: CompilerOwnedPlatform) []const u8 {
    return switch (platform) {
        .glue => "platform glue",
    };
}

/// The platform's embedded source files.
pub fn files(platform: CompilerOwnedPlatform) []const EmbeddedFile {
    return switch (platform) {
        .glue => &compiler_platform_sources.glue_files,
    };
}

/// A BLAKE3 hash over the platform's embedded sources, for cache keys
/// and plugin stamps.
pub fn sourceHash(platform: CompilerOwnedPlatform) [32]u8 {
    var hasher = std.crypto.hash.Blake3.init(.{});
    hasher.update(identity(platform));
    hasher.update(&[_]u8{0});
    for (files(platform)) |file| {
        hasher.update(file.path);
        hasher.update(&[_]u8{0});
        hasher.update(file.bytes);
        hasher.update(&[_]u8{0});
    }
    var digest: [32]u8 = undefined;
    hasher.final(&digest);
    return digest;
}

/// Write the platform's embedded sources into the internal source
/// directory so the package pipeline can consume them as files.
///
/// The directory is named by the hash of those sources and is shared by every
/// compiler process of this build, so several of them can materialize the same
/// platform at once while others are already reading it. A file at its final
/// path is therefore always complete: one that already holds the embedded
/// bytes is left alone, and any other is published by renaming a fully written
/// staging file over it.
pub fn materialize(
    allocator: Allocator,
    fs: CoreCtx,
    source_root_override: ?[]const u8,
    platform: CompilerOwnedPlatform,
) MaterializeError!MaterializedPlatform {
    const root = if (source_root_override) |dir|
        try allocator.dupe(u8, dir)
    else blk: {
        const config = cache_config.CacheConfig{ .roc_ctx = fs };
        break :blk try config.getModuleCacheDir(allocator);
    };
    defer allocator.free(root);

    const hash = sourceHash(platform);
    const hash_hex = std.fmt.bytesToHex(hash, .lower);
    const platform_dir_name = switch (platform) {
        .glue => "glue",
    };
    const root_dir = try std.fs.path.join(allocator, &.{ root, "compiler-platforms", platform_dir_name, hash_hex[0..] });
    errdefer allocator.free(root_dir);

    try fs.makePath(root_dir);

    var content_bytes: u64 = 0;
    var root_source_hash: [32]u8 = undefined;
    for (files(platform)) |file| {
        const dest = try std.fs.path.join(allocator, &.{ root_dir, file.path });
        defer allocator.free(dest);
        if (!try fileHoldsBytes(allocator, fs, dest, file.bytes)) {
            try publishFile(allocator, fs, dest, file.bytes);
        }
        content_bytes += file.bytes.len;
        if (std.mem.eql(u8, file.path, "main.roc")) {
            var sha = Sha256.init(.{});
            sha.update(file.bytes);
            root_source_hash = sha.finalResult();
        }
    }

    const root_file = try std.fs.path.join(allocator, &.{ root_dir, "main.roc" });
    errdefer allocator.free(root_file);

    return .{
        .root_file = root_file,
        .root_dir = root_dir,
        .root_source_hash = root_source_hash,
        .content_bytes = content_bytes,
    };
}

/// Whether the file at `path` exists and holds exactly `expected`.
fn fileHoldsBytes(allocator: Allocator, fs: CoreCtx, path: []const u8, expected: []const u8) MaterializeError!bool {
    const existing = fs.readFile(path, allocator) catch |err| switch (err) {
        // Nothing published yet, or something that cannot be these sources.
        error.FileNotFound, error.StreamTooLong => return false,
        error.OutOfMemory => return error.OutOfMemory,
        error.AccessDenied => return error.AccessDenied,
        error.IoError => return error.IoError,
    };
    defer allocator.free(existing);
    return std.mem.eql(u8, existing, expected);
}

/// Publish `bytes` at `dest` so that no reader can observe a partial file.
fn publishFile(allocator: Allocator, fs: CoreCtx, dest: []const u8, bytes: []const u8) MaterializeError!void {
    // Concurrent compilers can publish the same file. Each writer needs its
    // own staging file so one rename cannot remove another writer's input.
    var suffix: [16]u8 = undefined;
    fs.std_io.random(&suffix);
    const staging = try std.fmt.allocPrint(allocator, "{s}.{s}.tmp", .{ dest, std.fmt.bytesToHex(suffix, .lower) });
    defer allocator.free(staging);

    fs.writeFile(staging, bytes) catch |err| {
        removeStagingFile(fs, staging);
        return err;
    };
    fs.rename(staging, dest) catch |err| {
        removeStagingFile(fs, staging);
        return err;
    };
}

/// Remove a staging file whose publication failed. The publication error is
/// the one reported, and an unremoved staging file is never read, so a failure
/// here adds nothing to report.
fn removeStagingFile(fs: CoreCtx, staging: []const u8) void {
    fs.deleteFile(staging) catch |err| switch (err) {
        error.FileNotFound, error.AccessDenied, error.IoError => {},
    };
}

/// An in-memory filesystem recording how files reach their final paths.
const PublicationRecorder = struct {
    allocator: Allocator,
    contents: std.StringHashMapUnmanaged([]u8) = .empty,
    /// Paths handed to `writeFile`: the only places a partial file can be seen.
    written: std.ArrayList([]u8) = .empty,
    renames: usize = 0,

    fn deinit(self: *PublicationRecorder) void {
        var entries = self.contents.iterator();
        while (entries.next()) |entry| {
            self.allocator.free(entry.key_ptr.*);
            self.allocator.free(entry.value_ptr.*);
        }
        self.contents.deinit(self.allocator);
        self.clearWritten();
        self.written.deinit(self.allocator);
    }

    fn clearWritten(self: *PublicationRecorder) void {
        for (self.written.items) |path| self.allocator.free(path);
        self.written.clearRetainingCapacity();
    }

    fn put(self: *PublicationRecorder, path: []const u8, bytes: []const u8) Allocator.Error!void {
        const owned_bytes = try self.allocator.dupe(u8, bytes);
        errdefer self.allocator.free(owned_bytes);
        if (self.contents.getPtr(path)) |existing| {
            self.allocator.free(existing.*);
            existing.* = owned_bytes;
            return;
        }
        const owned_path = try self.allocator.dupe(u8, path);
        errdefer self.allocator.free(owned_path);
        try self.contents.put(self.allocator, owned_path, owned_bytes);
    }

    fn makePath(_: ?*anyopaque, _: std.Io, _: []const u8) CoreCtx.MakePathError!void {}

    fn readFile(ctx: ?*anyopaque, _: std.Io, path: []const u8, allocator: Allocator) CoreCtx.ReadError![]u8 {
        const self: *PublicationRecorder = @ptrCast(@alignCast(ctx.?));
        const bytes = self.contents.get(path) orelse return error.FileNotFound;
        return allocator.dupe(u8, bytes);
    }

    fn writeFile(ctx: ?*anyopaque, _: std.Io, path: []const u8, data: []const u8) CoreCtx.WriteError!void {
        const self: *PublicationRecorder = @ptrCast(@alignCast(ctx.?));
        const recorded = try self.allocator.dupe(u8, path);
        errdefer self.allocator.free(recorded);
        try self.written.append(self.allocator, recorded);
        try self.put(path, data);
    }

    fn rename(ctx: ?*anyopaque, _: std.Io, old_path: []const u8, new_path: []const u8) CoreCtx.RenameError!void {
        const self: *PublicationRecorder = @ptrCast(@alignCast(ctx.?));
        const moved = self.contents.fetchRemove(old_path) orelse return error.FileNotFound;
        defer self.allocator.free(moved.key);
        defer self.allocator.free(moved.value);
        self.put(new_path, moved.value) catch return error.IoError;
        self.renames += 1;
    }

    fn deleteFile(ctx: ?*anyopaque, _: std.Io, path: []const u8) CoreCtx.DeleteError!void {
        const self: *PublicationRecorder = @ptrCast(@alignCast(ctx.?));
        const removed = self.contents.fetchRemove(path) orelse return error.FileNotFound;
        self.allocator.free(removed.key);
        self.allocator.free(removed.value);
    }

    fn filesystem(self: *PublicationRecorder) CoreCtx {
        var fs = CoreCtx.testing(self.allocator, self.allocator);
        fs.std_io = std.testing.io;
        fs.ctx = self;
        fs.vtable.makePath = &makePath;
        fs.vtable.readFile = &readFile;
        fs.vtable.writeFile = &writeFile;
        fs.vtable.rename = &rename;
        fs.vtable.deleteFile = &deleteFile;
        return fs;
    }

    fn materializeGlue(self: *PublicationRecorder) MaterializeError!void {
        const materialized = try materialize(self.allocator, self.filesystem(), "root", .glue);
        self.allocator.free(materialized.root_file);
        self.allocator.free(materialized.root_dir);
    }

    /// Every embedded file is complete at its final path, nothing else is
    /// left beside them, and no final path was ever written in place.
    fn expectPublished(self: *PublicationRecorder) (Allocator.Error || error{ TestExpectedEqual, TestUnexpectedResult, TestExpectedPublishedFile })!void {
        const testing = std.testing;
        const hash_hex = std.fmt.bytesToHex(sourceHash(.glue), .lower);
        const root_dir = try std.fs.path.join(self.allocator, &.{ "root", "compiler-platforms", "glue", hash_hex[0..] });
        defer self.allocator.free(root_dir);

        try testing.expectEqual(files(.glue).len, self.contents.count());
        for (files(.glue)) |file| {
            const dest = try std.fs.path.join(self.allocator, &.{ root_dir, file.path });
            defer self.allocator.free(dest);
            try testing.expectEqualSlices(u8, file.bytes, self.contents.get(dest) orelse return error.TestExpectedPublishedFile);
            for (self.written.items) |path| try testing.expect(!std.mem.eql(u8, path, dest));
        }
    }
};

test "materialize publishes each embedded file whole and leaves a current one alone" {
    const testing = std.testing;
    var recorder = PublicationRecorder{ .allocator = testing.allocator };
    defer recorder.deinit();
    const file_count = files(.glue).len;

    // First materialization: every file arrives by rename from a staging file.
    try recorder.materializeGlue();
    try recorder.expectPublished();
    try testing.expectEqual(file_count, recorder.written.items.len);
    try testing.expectEqual(file_count, recorder.renames);

    // A second compiler process finds the files current and touches nothing,
    // so a concurrent reader of this directory never sees one change.
    recorder.clearWritten();
    recorder.renames = 0;
    try recorder.materializeGlue();
    try recorder.expectPublished();
    try testing.expectEqual(@as(usize, 0), recorder.written.items.len);
    try testing.expectEqual(@as(usize, 0), recorder.renames);

    // A file that does not hold the embedded bytes is replaced, again whole.
    const first = files(.glue)[0];
    const hash_hex = std.fmt.bytesToHex(sourceHash(.glue), .lower);
    const stale = try std.fs.path.join(testing.allocator, &.{ "root", "compiler-platforms", "glue", hash_hex[0..], first.path });
    defer testing.allocator.free(stale);
    try recorder.put(stale, first.bytes[0 .. first.bytes.len / 2]);
    try recorder.materializeGlue();
    try recorder.expectPublished();
    try testing.expectEqual(@as(usize, 1), recorder.written.items.len);
    try testing.expectEqual(@as(usize, 1), recorder.renames);
}
