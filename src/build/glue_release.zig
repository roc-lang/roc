//! Build helper: prepare release-ready glue package files.
//!
//! Usage:
//!   glue_release <release-tag> <output-dir>
//!
//! The output directory receives:
//!   - glue/RustGlue.roc
//!   - glue/ZigGlue.roc
//!   - glue/CGlue.roc
//!   - glue/README.md
//!   - glue/env

const std = @import("std");
const build_options = @import("build_options");

const glue_specs = [_]Spec{
    .{ .source = "src/glue/src/RustGlue.roc", .dest = "RustGlue.roc" },
    .{ .source = "src/glue/src/ZigGlue.roc", .dest = "ZigGlue.roc" },
    .{ .source = "src/glue/src/CGlue.roc", .dest = "CGlue.roc" },
};

const Spec = struct {
    source: []const u8,
    dest: []const u8,
};

/// Copies compiler-owned-platform glue specs for a nightly release.
pub fn main(init: std.process.Init) !void {
    const io = init.io;

    var gpa_impl: std.heap.DebugAllocator(.{ .stack_trace_frames = build_options.debug_gpa_stack_trace_frames }) = .init;
    defer _ = build_options.debugGpaOk(gpa_impl.deinit());
    const gpa = gpa_impl.allocator();

    var arena_impl = std.heap.ArenaAllocator.init(gpa);
    defer arena_impl.deinit();
    const arena = arena_impl.allocator();

    const stderr_file: std.Io.File = .stderr();
    const args = try init.minimal.args.toSlice(arena);
    if (args.len != 3) {
        stderr_file.writeStreamingAll(io, "Usage: glue_release <release-tag> <output-dir>\n") catch {};
        std.process.exit(2);
    }

    const release_tag = args[1];
    const output_dir = args[2];

    if (!std.mem.startsWith(u8, release_tag, "nightly-")) {
        stderr_file.writeStreamingAll(io, "glue_release: release tag must start with nightly-\n") catch {};
        std.process.exit(2);
    }

    std.Io.Dir.cwd().deleteTree(io, output_dir) catch {};
    try std.Io.Dir.cwd().createDirPath(io, output_dir);

    const glue_dir = try std.fs.path.join(arena, &.{ output_dir, "glue" });
    try std.Io.Dir.cwd().createDirPath(io, glue_dir);

    for (glue_specs) |spec| {
        const source = try std.Io.Dir.cwd().readFileAlloc(io, spec.source, arena, .limited(16 * 1024 * 1024));
        const dest = try std.fs.path.join(arena, &.{ glue_dir, spec.dest });
        try std.Io.Dir.cwd().writeFile(io, .{ .sub_path = dest, .data = source });
    }

    const readme = try std.fmt.allocPrint(arena,
        \\# Roc Glue
        \\
        \\These glue specs are generated for {s}.
        \\
        \\They use the compiler-owned `platform glue` platform embedded in this
        \\same Roc compiler build.
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
        \\and `ROC_C_GLUE` when these files are present in the installed Roc archive.
        \\
    , .{release_tag});
    try std.Io.Dir.cwd().writeFile(io, .{
        .sub_path = try std.fs.path.join(arena, &.{ glue_dir, "README.md" }),
        .data = readme,
    });

    try std.Io.Dir.cwd().writeFile(io, .{ .sub_path = try std.fs.path.join(arena, &.{ glue_dir, "env" }), .data =
        \\ROC_GLUE_DIR=glue
        \\ROC_RUST_GLUE=glue/RustGlue.roc
        \\ROC_ZIG_GLUE=glue/ZigGlue.roc
        \\ROC_C_GLUE=glue/CGlue.roc
        \\
    });
}
