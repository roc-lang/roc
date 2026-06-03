//! Runtime test environment for the LSP integration harness.

const std = @import("std");

pub var allocator: std.mem.Allocator = undefined;
pub var io: std.Io = undefined;

pub fn init(new_allocator: std.mem.Allocator, new_io: std.Io) void {
    allocator = new_allocator;
    io = new_io;
}

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
