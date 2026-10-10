//! Exercise the compiler through Zig's fetched-package boundary.
const std = @import("std");
const builtin = @import("builtin");

/// Register separate preparation and execution steps for the fetched-package test.
pub fn create(b: *std.Build, host_tool_target: std.Build.ResolvedTarget) *std.Build.Step {
    const source = b.addWriteFiles();
    inline for (.{ "build.zig", "build.zig.zon", "LICENSE", "legal_details" }) |file| {
        _ = source.addCopyFile(b.path(file), file);
    }
    inline for (.{ "src", "vendor" }) |dir| {
        _ = source.addCopyDirectory(b.path(dir), dir, .{});
    }
    const fixture = b.addWriteFiles();
    _ = fixture.addCopyDirectory(b.path("test/downstream-compiler"), "consumer", .{});
    const helper = b.addExecutable(.{
        .name = "prepare-downstream-package",
        .root_module = b.createModule(.{
            .root_source_file = b.path("src/build/downstream_package.zig"),
            .target = host_tool_target,
            .optimize = .Debug,
        }),
    });
    const prepare = b.addRunArtifact(helper);
    prepare.addFileArg(.zig_exe);
    prepare.addDirectoryArg(.{ .relative = .{ .base = .global_cache } });
    prepare.addDirectoryArg(source.getDirectory());
    prepare.addDirectoryArg(fixture.getDirectory().path(b, "consumer"));
    const output = prepare.addOutputDirectoryArg("downstream-package");
    // The consumer is compiled on one machine and may run on another, so it is
    // built for the same explicit CPU as the compiler's own host tools rather
    // than the one `zig build` would detect where the helper happens to run.
    prepare.addArg(b.fmt("-Dtarget={s}", .{host_tool_target.query.zigTriple(b.allocator) catch @panic("OOM")}));
    prepare.addArg(b.fmt("-Dcpu={s}", .{host_tool_target.query.serializeCpuAlloc(b.allocator) catch @panic("OOM")}));
    const build_step = b.step("build-test-downstream-package", "Fetch Roc as a Zig package and build a separate compiler driver");
    build_step.dependOn(&prepare.step);

    const run = std.Build.Step.Run.create(b, "run downstream compiler driver");
    run.addFileArg(output.path(b, b.fmt("prefix/bin/downstream-compiler-smoke{s}", .{builtin.target.exeFileExt()})));
    run.addFileArg(output.path(b, "consumer/app.roc"));
    const run_step = b.step("run-test-downstream-package", "Check, lower, and execute a Roc program through fetched compiler modules");
    run_step.dependOn(&run.step);
    return build_step;
}

/// Fetch the source snapshot and build the standalone consumer against its archive.
pub fn main(init: std.process.Init) !void {
    const arena = init.arena.allocator();
    const io = init.io;
    const args = try init.minimal.args.toSlice(arena);
    if (args.len != 8) return error.ExpectedPackageArguments;
    const target_option = args[6];
    const cpu_option = args[7];
    // The build runner may pass paths relative to its own working directory
    // (the global cache included, when ZIG_GLOBAL_CACHE_DIR is relative), while
    // the consumer's `zig fetch --save-exact` and `zig build` run inside the
    // consumer directory. Every path handed to a child is therefore made
    // absolute first.
    const cwd = try std.process.currentPathAlloc(io, arena);
    const zig = try std.fs.path.resolve(arena, &.{ cwd, args[1] });
    const global_cache = try std.fs.path.resolve(arena, &.{ cwd, args[2] });
    const source = try std.fs.path.resolve(arena, &.{ cwd, args[3] });
    const fixture = try std.fs.path.resolve(arena, &.{ cwd, args[4] });
    const output = try std.fs.path.resolve(arena, &.{ cwd, args[5] });
    // Zig reads the global cache location from the environment. A relative
    // value would name a different directory inside the consumer, so children
    // are given the absolute one the build runner is using.
    try init.environ_map.put("ZIG_GLOBAL_CACHE_DIR", global_cache);
    const consumer = try std.fs.path.join(arena, &.{ output, "consumer" });
    const platform = try std.fs.path.join(arena, &.{ consumer, "platform" });
    try std.Io.Dir.cwd().createDirPath(io, platform);
    inline for (.{ "build.zig", "main.zig", "app.roc", "platform/main.roc" }) |file| {
        const contents = try std.Io.Dir.cwd().readFileAlloc(io, try std.fs.path.join(arena, &.{ fixture, file }), arena, .limited(1024 * 1024));
        try std.Io.Dir.cwd().writeFile(io, .{ .sub_path = try std.fs.path.join(arena, &.{ consumer, file }), .data = contents });
    }
    const fetched = try command(init, &.{ zig, "fetch", source }, null);
    const hash = std.mem.trim(u8, fetched, " \r\n\t");
    const archive = try std.fs.path.join(arena, &.{ global_cache, "p", try std.fmt.allocPrint(arena, "{s}.tar.gz", .{hash}) });
    // Fetch the canonical archive again with --save-exact so Zig writes a real
    // URL/hash dependency, rather than bypassing package filtering with .path.
    const manifest = try std.fs.path.join(arena, &.{ consumer, "build.zig.zon" });
    try std.Io.Dir.cwd().writeFile(io, .{ .sub_path = manifest, .data =
        \\.{
        \\    .name = .roc_compiler_consumer,
        \\    .version = "0.0.0",
        \\    .fingerprint = 0x7805301ef8b56f7f,
        \\    .dependencies = .{},
        \\    .paths = .{""},
        \\}
        \\
    });
    _ = try command(init, &.{ zig, "fetch", "--save-exact=roc", archive }, consumer);
    const prefix = try std.fs.path.join(arena, &.{ output, "prefix" });
    const cache = try std.fs.path.join(arena, &.{ output, "consumer-cache" });
    _ = try command(init, &.{ zig, "build", "--cache-dir", cache, "--prefix", prefix, target_option, cpu_option }, consumer);
}

fn command(init: std.process.Init, argv: []const []const u8, cwd: ?[]const u8) ![]const u8 {
    const result = try std.process.run(init.arena.allocator(), init.io, .{
        .argv = argv,
        .cwd = if (cwd) |path| .{ .path = path } else .inherit,
        .environ_map = init.environ_map,
        .stdout_limit = .limited(4 * 1024 * 1024),
        .stderr_limit = .limited(4 * 1024 * 1024),
    });
    switch (result.term) {
        .exited => |code| if (code == 0) return result.stdout,
        .signal, .stopped, .unknown => {},
    }
    std.debug.print("{s}\n{s}\n", .{ result.stdout, result.stderr });
    return error.DownstreamPackageCommandFailed;
}
