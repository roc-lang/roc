//! Runtime test environment for the LSP integration harness.

const std = @import("std");
const SyntaxChecker = @import("lsp").syntax.SyntaxChecker;

/// Allocator installed by the integration harness before specs run.
pub var allocator: std.mem.Allocator = undefined;
/// I/O interface installed by the integration harness before specs run.
pub var io: std.Io = undefined;

/// Installs the allocator and I/O interface used by integration specs.
pub fn init(new_allocator: std.mem.Allocator, new_io: std.Io) void {
    allocator = new_allocator;
    io = new_io;
}

/// Configures a syntax checker for integration tests under the given cache root.
pub fn configureChecker(checker: *SyntaxChecker, cache_root: []const u8) void {
    checker.cache_config.enabled = false;
    checker.cache_config.cache_dir = cache_root;
}

/// Temporary directory handle created under `.zig-cache/tmp` for a spec.
pub const TmpDir = struct {
    dir: std.Io.Dir,
    parent_dir: std.Io.Dir,
    sub_path: [sub_path_len]u8,

    const random_bytes_count = 12;
    const sub_path_len = std.base64.url_safe.Encoder.calcSize(random_bytes_count);

    pub fn cleanup(self: *TmpDir) void {
        self.dir.close(io);
        self.parent_dir.deleteTree(io, &self.sub_path) catch {};
        self.parent_dir.close(io);
        self.* = undefined;
    }
};

/// Creates a temporary directory for an integration spec.
pub fn tmpDir(opts: std.Io.Dir.OpenOptions) TmpDir {
    var random_bytes: [TmpDir.random_bytes_count]u8 = undefined;
    io.random(&random_bytes);
    var sub_path: [TmpDir.sub_path_len]u8 = undefined;
    _ = std.base64.url_safe.Encoder.encode(&sub_path, &random_bytes);

    const cwd = std.Io.Dir.cwd();
    var cache_dir = cwd.createDirPathOpen(io, ".zig-cache", .{}) catch
        @panic("unable to make tmp dir for LSP integration tests: unable to make and open .zig-cache dir");
    defer cache_dir.close(io);
    const parent_dir = cache_dir.createDirPathOpen(io, "tmp", .{}) catch
        @panic("unable to make tmp dir for LSP integration tests: unable to make and open .zig-cache/tmp dir");
    const dir = parent_dir.createDirPathOpen(io, &sub_path, .{ .open_options = opts }) catch
        @panic("unable to make tmp dir for LSP integration tests: unable to make and open the tmp dir");

    return .{
        .dir = dir,
        .parent_dir = parent_dir,
        .sub_path = sub_path,
    };
}
