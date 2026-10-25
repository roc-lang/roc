//! Immutable, directly linkable objects in the completed-program store.
//! A link owns hard links in its scratch directory, never writable aliases.
//! Evicting a cache name cannot remove an object from an in-progress link.

const std = @import("std");
const backend = @import("backend");
const CoreCtx = @import("ctx").CoreCtx;
const Allocator = std.mem.Allocator;

/// Content identity of a raw object in the completed-program store.
pub const Identity = struct {
    digest: [32]u8,
    size: u64,

    pub fn of(bytes: []const u8) Identity {
        var digest: [32]u8 = undefined;
        std.crypto.hash.Blake3.hash(bytes, &digest, .{});
        return .{ .digest = digest, .size = bytes.len };
    }
};

/// Byte counts for immutable-object publication, reuse, and validation.
pub const Counters = struct {
    written: u64 = 0,
    reused: u64 = 0,
    validated: u64 = 0,
};

/// Owns the scratch lifetime pins for one native build's immutable objects.
pub const Objects = struct {
    core: CoreCtx,
    arena: Allocator,
    /// Same compiler/format/target/optimization namespace as program records.
    root: ?[]const u8,
    scratch: []const u8,
    lease: std.Io.File,
    counters: Counters = .{},

    /// Why taking the link lease can fail.
    pub const InitError = Allocator.Error || std.Io.File.OpenError;
    /// Why writing an object into the scratch directory can fail.
    pub const MaterializeError = Allocator.Error || CoreCtx.DeleteError || std.Io.Dir.WriteFileError;

    pub fn init(core: CoreCtx, arena: Allocator, root: ?[]const u8, scratch: []const u8) InitError!Objects {
        const lock_path = try std.fs.path.join(arena, &.{ scratch, "link.lock" });
        const lease = try std.Io.Dir.cwd().createFile(core.std_io, lock_path, .{ .read = true, .exclusive = true, .lock = .exclusive });
        return .{ .core = core, .arena = arena, .root = root, .scratch = scratch, .lease = lease };
    }

    pub fn deinit(self: *Objects) void {
        self.lease.close(self.core.std_io);
    }

    fn name(self: *Objects, id: Identity) Allocator.Error![]const u8 {
        return std.fmt.allocPrint(self.arena, "{s}.o", .{std.fmt.bytesToHex(id.digest, .lower)});
    }

    fn cachePath(self: *Objects, id: Identity) Allocator.Error!?[]const u8 {
        const root = self.root orelse return null;
        return try std.fs.path.join(self.arena, &.{ root, "programs", try self.name(id) });
    }

    /// Pin before validating. The sweep may unlink the cache name at any time.
    /// No writer ever opens a published object (or its pin) for writing.
    pub fn find(self: *Objects, id: Identity) Allocator.Error!?[]const u8 {
        return self.findKnown(id, null);
    }

    fn findKnown(self: *Objects, id: Identity, expected: ?[]const u8) Allocator.Error!?[]const u8 {
        const cache_path = try self.cachePath(id) orelse return null;
        const path = try std.fs.path.join(self.arena, &.{ self.scratch, try self.name(id) });
        self.core.hardLink(cache_path, path) catch |err| switch (err) {
            error.PathAlreadyExists => {},
            else => return null,
        };
        if (!self.validate(path, id, expected)) {
            self.core.deleteFile(path) catch {};
            return null;
        }
        self.counters.reused += id.size;
        return path;
    }

    fn validate(self: *Objects, path: []const u8, id: Identity, expected: ?[]const u8) bool {
        const file = std.Io.Dir.cwd().openFile(self.core.std_io, path, .{}) catch return false;
        defer file.close(self.core.std_io);
        const stat = file.stat(self.core.std_io) catch return false;
        if (stat.kind != .file or stat.size != id.size) return false;
        var buffer: [64 * 1024]u8 = undefined;
        var offset: u64 = 0;
        var hash = std.crypto.hash.Blake3.init(.{});
        while (offset < id.size) {
            const size: usize = @intCast(@min(buffer.len, id.size - offset));
            const count = file.readPositionalAll(self.core.std_io, buffer[0..size], offset) catch return false;
            self.counters.validated += count;
            if (count != size) return false;
            if (expected) |bytes| {
                const start: usize = @intCast(offset);
                if (!std.mem.eql(u8, buffer[0..size], bytes[start..][0..size])) return false;
            } else hash.update(buffer[0..size]);
            offset += size;
        }
        if (expected != null) return true;
        var digest: [32]u8 = undefined;
        hash.final(&digest);
        return std.mem.eql(u8, &digest, &id.digest);
    }

    /// Publish once, straight from the backend's bytes. The scratch name is
    /// already a lifetime pin; publication only adds and atomically renames a
    /// directory entry. A failed cache publication does not lose the object.
    pub fn materialize(self: *Objects, bytes: []const u8) MaterializeError![]const u8 {
        return self.materializeKnown(bytes, Identity.of(bytes));
    }

    /// `id` was computed from these bytes by their producer, either during
    /// this build or when embedding compiler inputs. Never from path or stat.
    pub fn materializeKnown(self: *Objects, bytes: []const u8, id: Identity) MaterializeError![]const u8 {
        std.debug.assert(bytes.len == id.size);
        if (try self.findKnown(id, bytes)) |path| return path;
        const path = try std.fs.path.join(self.arena, &.{ self.scratch, try self.name(id) });
        // A failed lookup removed any invalid pin. Never truncate an inode
        // which another invocation could be linking.
        self.core.deleteFile(path) catch |err| switch (err) {
            error.FileNotFound => {},
            else => return err,
        };
        try backend.writeFileWindowsAvSafe(self.core.std_io, path, bytes);
        self.counters.written += bytes.len;
        if (try self.cachePath(id)) |cache_path| {
            self.core.makePath(std.fs.path.dirname(cache_path).?) catch return path;
            // Each invocation's scratch directory gives its staging name a
            // unique identity, even when many writers publish the same object.
            const staging = try std.fmt.allocPrint(self.arena, "{s}.{s}.tmp", .{ cache_path, std.fs.path.basename(self.scratch) });
            self.core.hardLink(path, staging) catch return path;
            defer self.core.deleteFile(staging) catch {};
            self.core.rename(staging, cache_path) catch return path;
        }
        return path;
    }
};

test "immutable objects pin bytes through eviction and repair corruption without changing readers" {
    const io = std.testing.io;
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const a = arena.allocator();
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    try tmp.dir.createDir(io, "first", .default_dir);
    try tmp.dir.createDir(io, "second", .default_dir);
    try tmp.dir.createDir(io, "third", .default_dir);
    const root = try tmp.dir.realPathFileAlloc(io, ".", a);
    const core = CoreCtx.os(std.testing.allocator, a, io);
    var first = try Objects.init(core, a, root, try std.fs.path.join(a, &.{ root, "first" }));
    defer first.deinit();
    var second = try Objects.init(core, a, root, try std.fs.path.join(a, &.{ root, "second" }));
    defer second.deinit();
    const data = "immutable object bytes";
    const id = Identity.of(data);
    const published = try first.materialize(data);
    try std.testing.expectEqual(@as(u64, data.len), first.counters.written);
    const pinned = (try second.find(id)).?;
    try std.testing.expectEqual(@as(u64, 0), second.counters.written);
    const cache_path = (try first.cachePath(id)).?;
    // The sweep removes names, not the inode retained by a link.
    try core.deleteFile(cache_path);
    try std.testing.expectEqualStrings(data, try core.readFile(pinned, a));
    try std.testing.expectEqualStrings(data, try core.readFile(published, a));
    var third = try Objects.init(core, a, root, try std.fs.path.join(a, &.{ root, "third" }));
    defer third.deinit();
    try std.testing.expect(try third.find(id) == null);
    // An interrupted or corrupt publication is a miss, even at the right name
    // and size; repairing it must not mutate another reader's inode.
    try core.writeFile(cache_path, "xxxxxxxxxxxxxxxxxxxxxx");
    try std.testing.expect(try third.find(id) == null);
    _ = try third.materialize(data);
    try std.testing.expectEqualStrings(data, try core.readFile(pinned, a));
    try std.testing.expectEqualStrings(data, try core.readFile(cache_path, a));
    // A second publisher takes the existing content without writing it.
    _ = try second.materialize(data);
    try std.testing.expectEqual(@as(u64, 0), second.counters.written);
    try std.testing.expect(try third.find(.{ .digest = id.digest, .size = id.size + 1 }) == null);
}

test "concurrent object publishers leave complete immutable objects" {
    const io = std.testing.io;
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const a = arena.allocator();
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    const root = try tmp.dir.realPathFileAlloc(io, ".", a);
    try tmp.dir.createDir(io, "a", .default_dir);
    try tmp.dir.createDir(io, "b", .default_dir);
    const Worker = struct {
        const PublishError = Objects.InitError || Objects.MaterializeError;

        fn run(thread_io: std.Io, cache_root: []const u8, scratch: []const u8, failure: *?PublishError) void {
            publish(thread_io, cache_root, scratch) catch |err| {
                failure.* = err;
            };
        }
        fn publish(thread_io: std.Io, cache_root: []const u8, scratch: []const u8) PublishError!void {
            var storage = std.heap.ArenaAllocator.init(std.heap.page_allocator);
            defer storage.deinit();
            const allocator = storage.allocator();
            var objects = try Objects.init(CoreCtx.os(allocator, allocator, thread_io), allocator, cache_root, scratch);
            defer objects.deinit();
            _ = try objects.materialize("concurrent content");
        }
    };
    var failures: [2]?Worker.PublishError = .{ null, null };
    const first = try std.Thread.spawn(.{}, Worker.run, .{ io, root, try std.fs.path.join(a, &.{ root, "a" }), &failures[0] });
    const second = std.Thread.spawn(.{}, Worker.run, .{ io, root, try std.fs.path.join(a, &.{ root, "b" }), &failures[1] }) catch |err| {
        first.join();
        return err;
    };
    first.join();
    second.join();
    for (failures) |failure| if (failure) |err| return err;
    const name = try std.fmt.allocPrint(a, "programs/{s}.o", .{std.fmt.bytesToHex(Identity.of("concurrent content").digest, .lower)});
    const content = try tmp.dir.readFileAlloc(io, name, a, .unlimited);
    try std.testing.expectEqualStrings("concurrent content", content);
}

test "a refused unlink never permits truncating a published inode" {
    const io = std.testing.io;
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const a = arena.allocator();
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    try tmp.dir.createDir(io, "scratch", .default_dir);
    const root = try tmp.dir.realPathFileAlloc(io, ".", a);
    const core = CoreCtx.os(std.testing.allocator, a, io);
    var objects = try Objects.init(core, a, root, try std.fs.path.join(a, &.{ root, "scratch" }));
    defer objects.deinit();
    const content = "retained by another linker";
    const pinned = try objects.materialize(content);
    const cache_path = (try objects.cachePath(Identity.of(content))).?;
    const Denied = struct {
        fn delete(_: ?*anyopaque, _: std.Io, _: []const u8) CoreCtx.DeleteError!void {
            return error.AccessDenied;
        }
    };
    // Simulate eviction followed by a Windows filter refusing to unlink the
    // previous pin. Re-publication must fail before opening it for writing.
    objects.root = null;
    objects.core.vtable.deleteFile = Denied.delete;
    try std.testing.expectError(error.AccessDenied, objects.materialize(content));
    try std.testing.expectEqualStrings(content, try core.readFile(pinned, a));
    try std.testing.expectEqualStrings(content, try core.readFile(cache_path, a));
}
