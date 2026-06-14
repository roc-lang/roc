//! Fuzzing for Roc type checking with generated well-typed source.
//!
//! To build just the repro executable:
//!   zig build build-repro-typecheck
//!
//! To run one input:
//!   ./zig-out/bin/repro-typecheck --verbose <seed_file>
//!
//! To run with AFL++:
//!   zig build -Dfuzz
//!   mkdir -p /tmp/roc-typecheck-corpus
//!   printf '\0' > /tmp/roc-typecheck-corpus/seed
//!   ./zig-out/AFLplusplus/bin/afl-fuzz -i /tmp/roc-typecheck-corpus -o /tmp/roc-typecheck-out zig-out/bin/fuzz-typecheck

const std = @import("std");
const builtin = @import("builtin");
const compile = @import("compile");
const roc_target = @import("roc_target");
const FuzzReader = @import("FuzzReader.zig");
const TypedCodeGenerator = @import("TypedCodeGenerator.zig");

const BuildEnv = compile.BuildEnv;

pub export fn zig_fuzz_init() void {}

pub export fn zig_fuzz_test(buf: [*]u8, len: isize) void {
    zig_fuzz_test_inner(buf, len, false);
}

pub fn zig_fuzz_test_inner(buf: [*]u8, len: isize, debug: bool) void {
    var gpa_impl = std.heap.DebugAllocator(.{}){};
    defer {
        _ = gpa_impl.deinit();
    }
    const gpa = gpa_impl.allocator();

    const input = buf[0..@intCast(len)];
    var reader = FuzzReader.init(input);

    var generator = TypedCodeGenerator.init(gpa, &reader);
    defer generator.deinit();
    generator.generateModule() catch |err| switch (err) {
        error.OutOfMemory => @panic("OOM during typecheck fuzz source generation"),
    };

    const generated = generator.getOutput();
    const root_file_name = generator.getRootFileName();
    if (debug) {
        std.debug.print("Input length: {d}, bytes consumed: {d}\n", .{ input.len, reader.position });
        std.debug.print("Generated {s}:\n==========\n{s}\n==========\n\n", .{ root_file_name, generated });
    }

    const fuzz_io = std.Io.Threaded.global_single_threaded.io();
    const tmp_root = "/tmp/roc-fuzz-typecheck";
    std.Io.Dir.createDirAbsolute(fuzz_io, tmp_root, .default_dir) catch |err| switch (err) {
        error.PathAlreadyExists => {},
        else => @panic("failed to create typecheck fuzz temp root"),
    };

    var case_path_buf: [128]u8 = undefined;
    const case_path = std.fmt.bufPrint(&case_path_buf, "{s}/{d}", .{ tmp_root, currentProcessId() }) catch @panic("typecheck fuzz temp path too long");
    std.Io.Dir.createDirAbsolute(fuzz_io, case_path, .default_dir) catch |err| switch (err) {
        error.PathAlreadyExists => {},
        else => @panic("failed to create typecheck fuzz case dir"),
    };

    var tmp_dir = std.Io.Dir.openDirAbsolute(fuzz_io, case_path, .{}) catch @panic("failed to open typecheck fuzz case dir");
    defer tmp_dir.close(fuzz_io);
    tmp_dir.writeFile(fuzz_io, .{ .sub_path = root_file_name, .data = generated }) catch @panic("failed to write generated typecheck fuzz source");

    const abs_path = tmp_dir.realPathFileAlloc(fuzz_io, root_file_name, gpa) catch @panic("failed to resolve generated typecheck fuzz source");
    defer gpa.free(abs_path);

    const cwd = std.Io.Dir.cwd().realPathFileAlloc(fuzz_io, ".", gpa) catch @panic("failed to resolve cwd");
    defer gpa.free(cwd);

    var build_env = BuildEnv.init(gpa, .single_threaded, 1, roc_target.RocTarget.detectNative(), cwd, fuzz_io) catch @panic("OOM during BuildEnv init");
    defer build_env.deinit();

    build_env.build(abs_path) catch |err| switch (err) {
        error.OutOfMemory => @panic("OOM during generated source typecheck"),
        else => {},
    };

    const drained = build_env.drainReports() catch @panic("failed to drain typecheck fuzz reports");
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
        if (!debug) {
            std.debug.print("Generated {s}:\n==========\n{s}\n==========\n\n", .{ root_file_name, generated });
        }
        std.debug.panic("generated typecheck fuzz source produced blocking report: {s}", .{title});
    }
}

fn currentProcessId() u64 {
    return if (builtin.os.tag == .windows)
        std.os.windows.GetCurrentProcessId()
    else
        @intCast(std.c.getpid());
}
