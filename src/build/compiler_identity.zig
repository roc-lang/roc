//! Content identity for compiler application-cache compatibility.
//!
//! This is separate from the version displayed to people. A dirty compiler
//! source edit must invalidate artifacts even when the Git revision is equal.
//! The build stages the declared source tree and passes every extra file with
//! a stable logical name. Absolute paths, timestamps, and Git state are absent
//! from the digest. The tool's Run step must declare those inputs as well.

const std = @import("std");
const Allocator = std.mem.Allocator;
const Sha256 = std.crypto.hash.sha2.Sha256;

const NamedFile = struct { name: []const u8, path: []const u8 };

/// Hash declared source, toolchain, dependency, and semantic option inputs.
pub fn main(init: std.process.Init) !void {
    const allocator = init.arena.allocator();
    const args = try init.minimal.args.toSlice(allocator);
    var source_root: ?[]const u8 = null;
    var zig_exe: ?[]const u8 = null;
    var output: ?[]const u8 = null;
    var files: std.ArrayList(NamedFile) = .empty;
    var options: std.ArrayList([]const u8) = .empty;
    var i: usize = 1;
    while (i < args.len) {
        const flag = args[i];
        i += 1;
        if (i == args.len) return error.MissingArgument;
        if (std.mem.eql(u8, flag, "--source-root")) {
            if (source_root != null) return error.DuplicateArgument;
            source_root = args[i];
        } else if (std.mem.eql(u8, flag, "--zig-exe")) {
            if (zig_exe != null) return error.DuplicateArgument;
            zig_exe = args[i];
        } else if (std.mem.eql(u8, flag, "--output")) {
            if (output != null) return error.DuplicateArgument;
            output = args[i];
        } else if (std.mem.eql(u8, flag, "--option")) {
            try options.append(allocator, args[i]);
        } else if (std.mem.eql(u8, flag, "--file")) {
            if (i + 1 == args.len) return error.MissingArgument;
            try files.append(allocator, .{ .name = args[i], .path = args[i + 1] });
            i += 1;
        } else return error.UnknownArgument;
        i += 1;
    }
    var root = try std.Io.Dir.cwd().openDir(init.io, source_root orelse return error.MissingSourceRoot, .{ .iterate = true });
    defer root.close(init.io);
    const digest = try identity(allocator, init.io, root, zig_exe orelse return error.MissingZigExecutable, files.items, options.items);
    var out: std.Io.Writer.Allocating = .init(init.gpa);
    defer out.deinit();
    try out.writer.writeAll("//! Generated compiler compatibility identity.\n\n");
    try out.writer.writeAll("pub const compiler_compatibility_hash: [32]u8 = .{");
    for (digest, 0..) |byte, index| {
        try out.writer.print("{s}0x{x:0>2}", .{ if (index == 0) " " else ", ", byte });
    }
    try out.writer.writeAll(" };\n");
    const hex = std.fmt.bytesToHex(digest, .lower);
    try out.writer.print("pub const compiler_compatibility_id = \"{s}\";\n", .{hex});
    try std.Io.Dir.cwd().writeFile(init.io, .{ .sub_path = output orelse return error.MissingOutput, .data = out.written() });
}

fn lessString(_: void, left: []const u8, right: []const u8) bool {
    return std.mem.lessThan(u8, left, right);
}

fn lessFile(_: void, left: NamedFile, right: NamedFile) bool {
    return lessString({}, left.name, right.name);
}

fn field(hasher: *Sha256, value: []const u8) void {
    var length: [8]u8 = undefined;
    std.mem.writeInt(u64, &length, @intCast(value.len), .little);
    hasher.update(&length);
    hasher.update(value);
}

fn count(hasher: *Sha256, value: usize) void {
    var bytes: [8]u8 = undefined;
    std.mem.writeInt(u64, &bytes, @intCast(value), .little);
    hasher.update(&bytes);
}

fn fileDigest(allocator: Allocator, io: std.Io, directory: std.Io.Dir, path: []const u8) ![32]u8 {
    const contents = try directory.readFileAlloc(io, path, allocator, .unlimited);
    defer allocator.free(contents);
    var digest: [32]u8 = undefined;
    Sha256.hash(contents, &digest, .{});
    return digest;
}

fn identity(allocator: Allocator, io: std.Io, root: std.Io.Dir, zig_exe: []const u8, files: []NamedFile, options: [][]const u8) ![32]u8 {
    var hasher = Sha256.init(.{});
    field(&hasher, "roc-compiler-compatibility-v1");
    field(&hasher, @import("builtin").zig_version_string);
    field(&hasher, &try fileDigest(allocator, io, .cwd(), zig_exe));

    var paths: std.ArrayList([]const u8) = .empty;
    defer {
        for (paths.items) |path| allocator.free(path);
        paths.deinit(allocator);
    }
    var walker = try root.walk(allocator);
    defer walker.deinit();
    while (try walker.next(io)) |entry| {
        switch (entry.kind) {
            .file => try paths.append(allocator, try allocator.dupe(u8, entry.path)),
            .directory => {},
            .block_device,
            .character_device,
            .named_pipe,
            .sym_link,
            .unix_domain_socket,
            .whiteout,
            .door,
            .event_port,
            .unknown,
            => return error.NonRegularSourceInput,
        }
    }
    // Relative names participate in the identity so membership and renames
    // invalidate the cache too. Sort normalized names on every host platform.
    const SourceFile = struct { path: []const u8, name: []u8 };
    var sources: std.ArrayList(SourceFile) = .empty;
    defer {
        for (sources.items) |source| allocator.free(source.name);
        sources.deinit(allocator);
    }
    for (paths.items) |path| {
        const name = try allocator.dupe(u8, path);
        std.mem.replaceScalar(u8, name, '\\', '/');
        try sources.append(allocator, .{ .path = path, .name = name });
    }
    std.mem.sort(SourceFile, sources.items, {}, struct {
        fn less(_: void, left: SourceFile, right: SourceFile) bool {
            return std.mem.lessThan(u8, left.name, right.name);
        }
    }.less);
    field(&hasher, "source-files");
    count(&hasher, sources.items.len);
    for (sources.items) |source| {
        field(&hasher, source.name);
        field(&hasher, &try fileDigest(allocator, io, root, source.path));
    }

    field(&hasher, "named-dependencies");
    count(&hasher, files.len);
    std.mem.sort(NamedFile, files, {}, lessFile);
    for (files, 0..) |file, index| {
        if (index > 0 and std.mem.eql(u8, files[index - 1].name, file.name)) return error.DuplicateFileIdentity;
        field(&hasher, file.name);
        field(&hasher, &try fileDigest(allocator, io, .cwd(), file.path));
    }
    field(&hasher, "semantic-options");
    count(&hasher, options.len);
    std.mem.sort([]const u8, options, {}, lessString);
    for (options) |option| field(&hasher, option);
    return hasher.finalResult();
}

test "length-delimited fields distinguish ambiguous input concatenation" {
    var left = Sha256.init(.{});
    field(&left, "ab");
    field(&left, "c");
    var right = Sha256.init(.{});
    field(&right, "a");
    field(&right, "bc");
    try std.testing.expect(!std.mem.eql(u8, &left.finalResult(), &right.finalResult()));
}
