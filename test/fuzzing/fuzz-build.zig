//! Fuzzing for post-check build lowering with generated well-typed apps.
//!
//! To build just the repro executable:
//!   zig build build-repro-build
//!
//! To run one input:
//!   ./zig-out/bin/repro-build --verbose <seed_file>
//!
//! To run with AFL++:
//!   zig build -Dfuzz
//!   mkdir -p /tmp/roc-build-corpus
//!   printf '\0' > /tmp/roc-build-corpus/seed
//!   ./zig-out/AFLplusplus/bin/afl-fuzz -t 480000+ -i /tmp/roc-build-corpus -o /tmp/roc-build-out zig-out/bin/fuzz-build

const std = @import("std");
const builtin = @import("builtin");
const base = @import("base");
const check = @import("check");
const compile = @import("compile");
const lir = @import("lir");
const roc_target = @import("roc_target");
const BuildCodeGenerator = @import("BuildCodeGenerator.zig");
const FuzzReader = @import("FuzzReader.zig");

const BuildEnv = compile.BuildEnv;

pub export fn zig_fuzz_init() void {}

pub export fn zig_fuzz_test(buf: [*]u8, len: isize) void {
    zig_fuzz_test_inner(buf, len, false);
}

pub fn zig_fuzz_test_inner(buf: [*]u8, len: isize, debug: bool) void {
    var gpa_impl = std.heap.DebugAllocator(.{}){};
    defer {
        if (!builtin.fuzz or debug) {
            _ = gpa_impl.deinit();
        }
    }
    const gpa = if (builtin.fuzz and !debug) base.defaultGpa() else gpa_impl.allocator();

    const input = buf[0..@intCast(len)];
    var reader = FuzzReader.init(input);

    var generator = BuildCodeGenerator.init(gpa, &reader);
    defer generator.deinit();
    generator.generate() catch |err| switch (err) {
        error.OutOfMemory => @panic("OOM during build fuzz source generation"),
    };

    const app_source = generator.getAppOutput();
    const platform_source = generator.getPlatformOutput();
    const module_source = generator.getModuleOutput();
    const app_file_name = generator.getAppFileName();
    const platform_file_name = generator.getPlatformFileName();
    const module_file_name = generator.getModuleFileName();
    if (debug) {
        std.debug.print("Input length: {d}, bytes consumed: {d}\n", .{ input.len, reader.position });
        std.debug.print("Generated {s}:\n==========\n{s}\n==========\n\n", .{ app_file_name, app_source });
        std.debug.print("Generated {s}:\n==========\n{s}\n==========\n\n", .{ platform_file_name, platform_source });
        std.debug.print("Generated {s}:\n==========\n{s}\n==========\n\n", .{ module_file_name, module_source });
    }

    const fuzz_io = std.Io.Threaded.global_single_threaded.io();
    const tmp_root = "/tmp/roc-fuzz-build";
    std.Io.Dir.createDirAbsolute(fuzz_io, tmp_root, .default_dir) catch |err| switch (err) {
        error.PathAlreadyExists => {},
        else => @panic("failed to create build fuzz temp root"),
    };

    var case_path_buf: [128]u8 = undefined;
    const case_path = std.fmt.bufPrint(&case_path_buf, "{s}/{d}", .{ tmp_root, currentProcessId() }) catch @panic("build fuzz temp path too long");
    std.Io.Dir.createDirAbsolute(fuzz_io, case_path, .default_dir) catch |err| switch (err) {
        error.PathAlreadyExists => {},
        else => @panic("failed to create build fuzz case dir"),
    };

    var tmp_dir = std.Io.Dir.openDirAbsolute(fuzz_io, case_path, .{}) catch @panic("failed to open build fuzz case dir");
    defer tmp_dir.close(fuzz_io);
    tmp_dir.writeFile(fuzz_io, .{ .sub_path = app_file_name, .data = app_source }) catch @panic("failed to write generated build fuzz app source");
    tmp_dir.writeFile(fuzz_io, .{ .sub_path = platform_file_name, .data = platform_source }) catch @panic("failed to write generated build fuzz platform source");
    tmp_dir.writeFile(fuzz_io, .{ .sub_path = module_file_name, .data = module_source }) catch @panic("failed to write generated build fuzz module source");

    const abs_app_path = tmp_dir.realPathFileAlloc(fuzz_io, app_file_name, gpa) catch @panic("failed to resolve generated build fuzz app source");
    defer gpa.free(abs_app_path);

    const cwd = std.Io.Dir.cwd().realPathFileAlloc(fuzz_io, ".", gpa) catch @panic("failed to resolve cwd");
    defer gpa.free(cwd);

    var build_env = BuildEnv.init(gpa, .single_threaded, 1, roc_target.RocTarget.detectNative(), cwd, fuzz_io) catch @panic("OOM during BuildEnv init");
    defer build_env.deinit();

    build_env.discoverDependencies(abs_app_path) catch |err| switch (err) {
        error.OutOfMemory => @panic("OOM during generated app discovery"),
        else => @panic("generated build fuzz app failed dependency discovery"),
    };

    build_env.setTarget(roc_target.RocTarget.detectNative());
    build_env.compileDiscovered() catch |err| switch (err) {
        error.OutOfMemory => @panic("OOM during generated app checking"),
        else => {},
    };

    assertNoBlockingReports(&build_env, debug, app_file_name, app_source, platform_file_name, platform_source, module_file_name, module_source) catch |err| switch (err) {
        error.OutOfMemory => @panic("OOM while draining generated app reports"),
    };

    const root_checked = build_env.executableRootCheckedArtifact();
    const imported = build_env.collectImportedArtifactViews(gpa, root_checked) catch |err| switch (err) {
        error.OutOfMemory => @panic("OOM while collecting imported checked modules"),
    };
    defer gpa.free(imported);
    const relations = build_env.collectRelationArtifactViews(gpa, root_checked) catch |err| switch (err) {
        error.OutOfMemory => @panic("OOM while collecting relation checked modules"),
    };
    defer gpa.free(relations);

    const lir_roots = lir.CheckedPipeline.selectPlatformEntrypointRoots(gpa, root_checked.root_requests.runtime_requests) catch |err| switch (err) {
        error.OutOfMemory => @panic("OOM while selecting LIR roots"),
    };
    defer gpa.free(lir_roots);

    var lowered = lir.CheckedPipeline.lowerCheckedModulesToLir(
        gpa,
        .{
            .root = check.CheckedArtifact.loweringViewWithRelations(root_checked, relations),
            .imports = imported,
        },
        .{ .requests = lir_roots },
        .{
            .target_usize = base.target.TargetUsize.native,
            .inline_mode = .none,
            .list_in_place_map = false,
        },
    ) catch |err| switch (err) {
        error.OutOfMemory => @panic("OOM during generated app checked-to-LIR lowering"),
    };
    defer lowered.deinit();
}

fn assertNoBlockingReports(
    build_env: *BuildEnv,
    debug: bool,
    app_file_name: []const u8,
    app_source: []const u8,
    platform_file_name: []const u8,
    platform_source: []const u8,
    module_file_name: []const u8,
    module_source: []const u8,
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
        if (!debug) {
            std.debug.print("Generated {s}:\n==========\n{s}\n==========\n\n", .{ app_file_name, app_source });
            std.debug.print("Generated {s}:\n==========\n{s}\n==========\n\n", .{ platform_file_name, platform_source });
            std.debug.print("Generated {s}:\n==========\n{s}\n==========\n\n", .{ module_file_name, module_source });
        }
        std.debug.panic("generated build fuzz app produced blocking report: {s}", .{title});
    }
}

fn currentProcessId() u64 {
    return if (builtin.os.tag == .windows)
        std.os.windows.GetCurrentProcessId()
    else
        @intCast(std.c.getpid());
}
