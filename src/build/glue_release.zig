//! Build helper: prepare release-ready glue package files.
//!
//! Usage:
//!   glue_release <release-tag> <bundle-dir> <output-dir>
//!
//! The output directory receives:
//!   - package/<hash>.tar.zst
//!   - glue/RustGlue.roc
//!   - glue/ZigGlue.roc
//!   - glue/CGlue.roc
//!   - glue/README.md
//!   - glue/env

const std = @import("std");

const glue_specs = [_]Spec{
    .{ .source = "src/glue/src/RustGlue.roc", .dest = "RustGlue.roc" },
    .{ .source = "src/glue/src/ZigGlue.roc", .dest = "ZigGlue.roc" },
    .{ .source = "src/glue/src/CGlue.roc", .dest = "CGlue.roc" },
};

const source_platform_header = "platform \"../platform/main.roc\"";

const Spec = struct {
    source: []const u8,
    dest: []const u8,
};

/// Copies the bundled glue platform and rewrites glue specs for a nightly release.
pub fn main(init: std.process.Init) !void {
    const io = init.io;

    var gpa_impl: std.heap.DebugAllocator(.{}) = .init;
    defer _ = gpa_impl.deinit();
    const gpa = gpa_impl.allocator();

    var arena_impl = std.heap.ArenaAllocator.init(gpa);
    defer arena_impl.deinit();
    const arena = arena_impl.allocator();

    const stderr_file: std.Io.File = .stderr();
    const args = try init.minimal.args.toSlice(arena);
    if (args.len != 4) {
        stderr_file.writeStreamingAll(io, "Usage: glue_release <release-tag> <bundle-dir> <output-dir>\n") catch {};
        std.process.exit(2);
    }

    const release_tag = args[1];
    const bundle_dir = args[2];
    const output_dir = args[3];

    if (!std.mem.startsWith(u8, release_tag, "nightly-")) {
        stderr_file.writeStreamingAll(io, "glue_release: release tag must start with nightly-\n") catch {};
        std.process.exit(2);
    }

    std.Io.Dir.cwd().deleteTree(io, output_dir) catch {};
    try std.Io.Dir.cwd().createDirPath(io, output_dir);

    const package_dir = try std.fs.path.join(arena, &.{ output_dir, "package" });
    const glue_dir = try std.fs.path.join(arena, &.{ output_dir, "glue" });
    try std.Io.Dir.cwd().createDirPath(io, package_dir);
    try std.Io.Dir.cwd().createDirPath(io, glue_dir);

    var bundle_source_dir = try std.Io.Dir.cwd().openDir(io, bundle_dir, .{ .iterate = true });
    defer bundle_source_dir.close(io);
    const bundle_name = try findBundleName(arena, &bundle_source_dir, io);
    const bundle_dest = try std.fs.path.join(arena, &.{ package_dir, bundle_name });
    try bundle_source_dir.copyFile(bundle_name, std.Io.Dir.cwd(), bundle_dest, io, .{});

    const package_url = try std.fmt.allocPrint(
        arena,
        "https://github.com/roc-lang/nightlies/releases/download/{s}/{s}",
        .{ release_tag, bundle_name },
    );

    for (glue_specs) |spec| {
        const source = try std.Io.Dir.cwd().readFileAlloc(io, spec.source, arena, .limited(16 * 1024 * 1024));
        const rewritten = try replacePlatformHeader(arena, source, package_url);
        const dest = try std.fs.path.join(arena, &.{ glue_dir, spec.dest });
        try std.Io.Dir.cwd().writeFile(io, .{ .sub_path = dest, .data = rewritten });
    }

    const readme = try std.fmt.allocPrint(arena,
        \\# Roc Glue
        \\
        \\These glue specs are generated for {s}.
        \\
        \\They reference this matching glue platform package:
        \\
        \\```text
        \\{s}
        \\```
        \\
        \\Use `roc glue` with one of these spec files:
        \\
        \\```sh
        \\roc glue "$ROC_RUST_GLUE" platform/main.roc --output-dir generated
        \\roc glue "$ROC_ZIG_GLUE" platform/main.roc --output-dir generated
        \\roc glue "$ROC_C_GLUE" platform/main.roc --output-dir generated
        \\```
        \\
        \\`setup-roc` exports `ROC_GLUE_DIR`, `ROC_RUST_GLUE`, `ROC_ZIG_GLUE`,
        \\`ROC_C_GLUE`, and `ROC_GLUE_PLATFORM_URL` when these files are present
        \\in the installed Roc archive.
        \\
    , .{ release_tag, package_url });
    try std.Io.Dir.cwd().writeFile(io, .{
        .sub_path = try std.fs.path.join(arena, &.{ glue_dir, "README.md" }),
        .data = readme,
    });

    const env = try std.fmt.allocPrint(arena,
        \\ROC_GLUE_PLATFORM_URL={s}
        \\ROC_GLUE_PLATFORM_PACKAGE={s}
        \\ROC_GLUE_DIR=glue
        \\ROC_RUST_GLUE=glue/RustGlue.roc
        \\ROC_ZIG_GLUE=glue/ZigGlue.roc
        \\ROC_C_GLUE=glue/CGlue.roc
        \\
    , .{ package_url, bundle_name });
    try std.Io.Dir.cwd().writeFile(io, .{
        .sub_path = try std.fs.path.join(arena, &.{ glue_dir, "env" }),
        .data = env,
    });
}

fn replacePlatformHeader(allocator: std.mem.Allocator, source: []const u8, package_url: []const u8) ![]u8 {
    var count: usize = 0;
    var search_start: usize = 0;
    while (std.mem.findPos(u8, source, search_start, source_platform_header)) |match_start| {
        count += 1;
        search_start = match_start + source_platform_header.len;
    }

    if (count != 1) return error.InvalidGlueSpecHeader;

    const replacement = try std.fmt.allocPrint(allocator, "platform \"{s}\"", .{package_url});
    return std.mem.replaceOwned(u8, allocator, source, source_platform_header, replacement);
}

fn findBundleName(allocator: std.mem.Allocator, dir: *std.Io.Dir, io: std.Io) ![]const u8 {
    var found: ?[]const u8 = null;
    var iterator = dir.iterate();
    while (try iterator.next(io)) |entry| {
        if (entry.kind != .file) continue;
        if (!std.mem.endsWith(u8, entry.name, ".tar.zst")) continue;
        if (found != null) return error.MultipleBundleFiles;
        found = try allocator.dupe(u8, entry.name);
    }
    return found orelse error.NoBundleFile;
}
