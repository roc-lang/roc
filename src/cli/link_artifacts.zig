//! Materialize the current build's link inputs in its scratch directory.

const std = @import("std");
const backend = @import("backend");
const CoreCtx = @import("ctx").CoreCtx;
const Allocator = std.mem.Allocator;

/// Content identity of a raw object in this build.
pub const Identity = struct {
    digest: [32]u8,
    size: u64,

    pub fn of(bytes: []const u8) Identity {
        var digest: [32]u8 = undefined;
        std.crypto.hash.Blake3.hash(bytes, &digest, .{});
        return .{ .digest = digest, .size = bytes.len };
    }
};

/// Bytes written to the scratch directory.
pub const Counters = struct {
    written: u64 = 0,
};

/// Materializes one native build's objects in its owned scratch directory.
pub const Objects = struct {
    core: CoreCtx,
    arena: Allocator,
    scratch: []const u8,
    counters: Counters = .{},
    /// Serializes writes of identical objects, arena allocation, and counters
    /// across post-check workers.
    mutex: std.Io.Mutex = .init,

    /// Why writing an object into the scratch directory can fail.
    pub const MaterializeError = Allocator.Error || CoreCtx.DeleteError || std.Io.Dir.WriteFileError;

    pub fn init(core: CoreCtx, arena: Allocator, scratch: []const u8) Objects {
        return .{ .core = core, .arena = arena, .scratch = scratch };
    }

    fn name(self: *Objects, id: Identity) Allocator.Error![]const u8 {
        return std.fmt.allocPrint(self.arena, "{s}.o", .{std.fmt.bytesToHex(id.digest, .lower)});
    }

    /// The object's scratch path; the caller holds the mutex.
    fn scratchPath(self: *Objects, id: Identity) Allocator.Error![]const u8 {
        return try std.fs.path.join(self.arena, &.{ self.scratch, try self.name(id) });
    }

    /// Write the backend's bytes to this invocation's scratch directory.
    pub fn materialize(self: *Objects, bytes: []const u8) MaterializeError![]const u8 {
        return self.materializeKnown(bytes, Identity.of(bytes));
    }

    /// `id` was computed from these bytes by their producer, either during
    /// this build or when embedding compiler inputs. Never from path or stat.
    pub fn materializeKnown(self: *Objects, bytes: []const u8, id: Identity) MaterializeError![]const u8 {
        std.debug.assert(bytes.len == id.size);
        self.mutex.lockUncancelable(self.core.std_io);
        defer self.mutex.unlock(self.core.std_io);
        const path = try self.scratchPath(id);
        self.core.deleteFile(path) catch |err| switch (err) {
            error.FileNotFound => {},
            else => return err,
        };
        try backend.writeFileWindowsAvSafe(self.core.std_io, path, bytes);
        self.counters.written += bytes.len;
        return path;
    }
};
