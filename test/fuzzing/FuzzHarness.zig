//! Shared harness helpers for in-process Roc fuzzers.

const std = @import("std");
const builtin = @import("builtin");
const compile = @import("compile");

const BuildEnv = compile.BuildEnv;

pub const GeneratedFile = struct {
    name: []const u8,
    source: []const u8,
};

pub fn printGeneratedFiles(files: []const GeneratedFile) void {
    for (files) |file| {
        std.debug.print("Generated {s}:\n==========\n{s}\n==========\n\n", .{ file.name, file.source });
    }
}

pub fn writeGeneratedFiles(
    allocator: std.mem.Allocator,
    fuzz_io: std.Io,
    tmp_root: []const u8,
    files: []const GeneratedFile,
) anyerror![][:0]u8 {
    std.Io.Dir.createDirAbsolute(fuzz_io, tmp_root, .default_dir) catch |err| switch (err) {
        error.PathAlreadyExists => {},
        else => return err,
    };

    var case_path_buf: [256]u8 = undefined;
    const case_path = try std.fmt.bufPrint(&case_path_buf, "{s}/{d}", .{ tmp_root, currentProcessId() });
    std.Io.Dir.createDirAbsolute(fuzz_io, case_path, .default_dir) catch |err| switch (err) {
        error.PathAlreadyExists => {},
        else => return err,
    };

    var tmp_dir = try std.Io.Dir.openDirAbsolute(fuzz_io, case_path, .{});
    defer tmp_dir.close(fuzz_io);

    const abs_paths = try allocator.alloc([:0]u8, files.len);
    errdefer allocator.free(abs_paths);
    var initialized: usize = 0;
    errdefer {
        for (abs_paths[0..initialized]) |path| allocator.free(path);
    }

    for (files, 0..) |file, index| {
        try tmp_dir.writeFile(fuzz_io, .{ .sub_path = file.name, .data = file.source });
        abs_paths[index] = try tmp_dir.realPathFileAlloc(fuzz_io, file.name, allocator);
        initialized += 1;
    }

    return abs_paths;
}

pub fn freeGeneratedPaths(allocator: std.mem.Allocator, abs_paths: [][:0]u8) void {
    for (abs_paths) |path| allocator.free(path);
    allocator.free(abs_paths);
}

pub fn assertNoBlockingReports(
    build_env: *BuildEnv,
    debug: bool,
    files: []const GeneratedFile,
    failure_context: []const u8,
) std.mem.Allocator.Error!void {
    const drained = try build_env.drainReports();
    defer build_env.freeDrainedReports(drained);

    var blocking_report_title: ?[]const u8 = null;
    for (drained) |mod| {
        for (mod.reports) |report| {
            if (debug) {
                std.debug.print("{s}: {s} ({s})\n", .{ mod.abs_path, report.title, @tagName(report.severity) });
            }
            switch (report.severity) {
                .runtime_error, .fatal => {
                    blocking_report_title = report.title;
                    break;
                },
                else => {},
            }
        }
        if (blocking_report_title != null) break;
    }

    if (blocking_report_title) |title| {
        if (!debug) printGeneratedFiles(files);
        std.debug.panic("{s}: {s}", .{ failure_context, title });
    }
}

fn currentProcessId() u64 {
    return if (builtin.os.tag == .windows)
        std.os.windows.GetCurrentProcessId()
    else
        @intCast(std.c.getpid());
}
