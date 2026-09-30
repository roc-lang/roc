//! Shared driver for the build fuzzers: generate a typed module and its
//! platform/app wrapper, check it the way `roc build` does, and lower every
//! platform entrypoint to LIR.

const std = @import("std");
const build_options = @import("build_options");
const builtin = @import("builtin");
const base = @import("base");
const check = @import("check");
const compile = @import("compile");
const lir = @import("lir");
const roc_target = @import("roc_target");
const BuildWrapperGenerator = @import("BuildWrapperGenerator.zig");
const FuzzHarness = @import("FuzzHarness.zig");
const FuzzReader = @import("FuzzReader.zig");
const TypedCodeGenerator = @import("TypedCodeGenerator.zig");

const BuildEnv = compile.BuildEnv;

/// What the generated program is allowed to contain.
pub const Mode = enum {
    /// Only well-typed programs: any blocking report is a failure.
    well_typed,
    /// Programs where a few expressions are written at a wrong type. A
    /// rejected program must still lower to LIR, with checked crashes in
    /// place of the rejected code.
    type_errors,
};

pub fn run(buf: [*]u8, len: isize, debug: bool, mode: Mode) void {
    var gpa_impl = std.heap.DebugAllocator(.{ .stack_trace_frames = build_options.debug_gpa_stack_trace_frames }){};
    defer {
        if (!builtin.fuzz or debug) {
            _ = build_options.debugGpaOk(gpa_impl.deinit());
        }
    }
    const gpa = if (builtin.fuzz and !debug) base.defaultGpa() else gpa_impl.allocator();

    const input = buf[0..@intCast(len)];
    var reader = FuzzReader.init(input);
    const selected_target = BuildWrapperGenerator.chooseTarget(&reader);

    var module_generator = TypedCodeGenerator.init(gpa, &reader);
    defer module_generator.deinit();
    switch (mode) {
        .well_typed => {},
        .type_errors => module_generator.allowTypeErrors(reader.intRangeAtMost(u8, 1, 3)),
    }
    module_generator.generateModule() catch |err| switch (err) {
        error.OutOfMemory => @panic("OOM during shared typed module generation"),
    };

    var wrapper_generator = BuildWrapperGenerator.init(gpa, &reader, &module_generator);
    defer wrapper_generator.deinit();
    wrapper_generator.generate(selected_target) catch |err| switch (err) {
        error.OutOfMemory => @panic("OOM during build wrapper generation"),
    };

    const generated_files = [_]FuzzHarness.GeneratedFile{
        .{ .name = module_generator.getRootFileName(), .source = module_generator.getOutput() },
        .{ .name = wrapper_generator.getAppFileName(), .source = wrapper_generator.getAppOutput() },
        .{ .name = wrapper_generator.getPlatformFileName(), .source = wrapper_generator.getPlatformOutput() },
    };
    if (debug) {
        std.debug.print("Input length: {d}, bytes consumed: {d}, target: {s}\n", .{ input.len, reader.position, @tagName(selected_target) });
        FuzzHarness.printGeneratedFiles(&generated_files);
    }

    const fuzz_io = std.Io.Threaded.global_single_threaded.io();
    const abs_paths = FuzzHarness.writeGeneratedFiles(gpa, fuzz_io, switch (mode) {
        .well_typed => "/tmp/roc-fuzz-build",
        .type_errors => "/tmp/roc-fuzz-build-errors",
    }, &generated_files) catch @panic("failed to write generated build fuzz source");
    defer FuzzHarness.freeGeneratedPaths(gpa, abs_paths);

    const cwd = std.Io.Dir.cwd().realPathFileAlloc(fuzz_io, ".", gpa) catch @panic("failed to resolve cwd");
    defer gpa.free(cwd);

    var build_env = BuildEnv.init(gpa, .single_threaded, 1, roc_target.RocTarget.detectNative(), cwd, fuzz_io) catch @panic("OOM during BuildEnv init");
    defer build_env.deinit();

    build_env.discoverDependencies(abs_paths[1]) catch |err| switch (err) {
        error.OutOfMemory => @panic("OOM during generated app discovery"),
        else => @panic("generated build fuzz app failed dependency discovery"),
    };

    build_env.setTarget(selected_target);
    build_env.setPostCheckPublicationMode(.executable_artifacts);
    build_env.compileDiscovered() catch |err| switch (err) {
        error.OutOfMemory => @panic("OOM during generated app checking"),
        else => {},
    };

    const type_error_sites = module_generator.typeErrorSites();
    if (debug) {
        for (type_error_sites) |site| {
            std.debug.print("Wrote a {s} construct where {s} is required, at {s} byte {d}\n", .{
                @tagName(site.written),
                @tagName(site.expected),
                module_generator.getRootFileName(),
                site.output_offset,
            });
        }
    }
    // A position's generated type is not always a real constraint (an
    // unannotated record field can hold any type), so a program with an
    // ill-typed expression may still check cleanly. Either way it must lower.
    if (type_error_sites.len == 0) {
        FuzzHarness.assertNoBlockingReports(&build_env, debug, &generated_files, "generated build fuzz app produced blocking report") catch |err| switch (err) {
            error.OutOfMemory => @panic("OOM while draining generated app reports"),
        };
    }

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
            .target_usize = targetUsize(selected_target),
            .list_in_place_map = false,
        },
    ) catch |err| switch (err) {
        error.OutOfMemory => @panic("OOM during generated app checked-to-LIR lowering"),
        error.HostedFunctionNotBound => @panic("generated app declared a hosted function its platform header did not bind"),
    };
    defer lowered.deinit();
}

fn targetUsize(target: roc_target.RocTarget) base.target.TargetUsize {
    return switch (target.ptrBitWidth()) {
        32 => .u32,
        64 => .u64,
        else => unreachable,
    };
}
