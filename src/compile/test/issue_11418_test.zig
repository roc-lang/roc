//! Regression for #11418: a separate CTFE consumer must not lower runtime-only code.

const std = @import("std");
const base = @import("base");
const build_options = @import("build_options");
const eval = @import("eval");
const lir = @import("lir");
const roc_target = @import("roc_target");
const CoreCtx = @import("ctx").CoreCtx;
const Coordinator = @import("../coordinator.zig").Coordinator;
const is_freestanding = @import("../threading.zig").is_freestanding;

fn countNamedProcs(store: *const lir.LirStore, expected: []const u8) usize {
    var count: usize = 0;
    for (0..store.procSpecCount()) |index| {
        const name = store.procDebugName(@enumFromInt(index)) orelse continue;
        if (std.mem.eql(u8, name, expected)) count += 1;
    }
    return count;
}

test "issue 11418: shared LIR generation leaves runtime-only procedures to the runtime consumer" {
    if (is_freestanding) return error.SkipZigTest;
    const allocator = std.testing.allocator;
    const io = std.testing.io;
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();
    try tmp_dir.dir.createDirPath(io, ".roc_echo_platform");
    const files = [_]struct { path: []const u8, source: []const u8 }{
        .{ .path = ".roc_echo_platform/main.roc", .source =
        \\platform ""
        \\    requires {} { main! : List(Str) => Try({}, [Exit(I8), ..]) }
        \\    exposes [Echo]
        \\    packages {}
        \\    provides { "roc_main": main_for_host! }
        \\    hosted { "roc_echo_line": Echo.line! }
        \\import Echo
        \\main_for_host! : List(Str) => I8
        \\main_for_host! = |args|
        \\    match main!(args) {
        \\        Ok({}) => 0
        \\        Err(Exit(code)) => code
        \\        Err(other) => {
        \\            Echo.line!("Program exited with error: ${Str.inspect(other)}")
        \\            1
        \\        }
        \\    }
        },
        .{ .path = ".roc_echo_platform/Echo.roc", .source =
        \\Echo := [].{
        \\    line! : Str => {}
        \\}
        },
        .{ .path = "Helper.roc", .source =
        \\Helper := [].{
        \\    twice : I64 -> I64
        \\    twice = |n| n + n
        \\    answer : I64
        \\    answer = twice(21)
        \\}
        },
        .{ .path = "main.roc", .source =
        \\app [main!] { pf: platform "./.roc_echo_platform/main.roc" }
        \\import pf.Echo
        \\import Helper
        \\runtime_only_a : U64 -> U64
        \\runtime_only_a = |n| n + 1
        \\runtime_only_b : U64 -> U64
        \\runtime_only_b = |n| n * 3
        \\main! = |args| {
        \\    Echo.line!(Str.inspect(runtime_only_a(args.len())))
        \\    Echo.line!(Str.inspect(runtime_only_b(args.len())))
        \\    Echo.line!(Str.inspect(Helper.answer))
        \\    Ok({})
        \\}
        },
    };
    for (files) |file| try tmp_dir.dir.writeFile(io, .{ .sub_path = file.path, .data = file.source });
    const app_path = try tmp_dir.dir.realPathFileAlloc(io, "main.roc", allocator);
    defer allocator.free(app_path);

    var builtin_modules = try eval.BuiltinModules.init(allocator);
    defer builtin_modules.deinit();
    var coord = try Coordinator.init(
        allocator,
        .single_threaded,
        1,
        roc_target.RocTarget.detectNative(),
        &builtin_modules,
        build_options.compiler_version,
        null,
        CoreCtx.os(allocator, allocator, io),
    );
    defer coord.deinit();
    coord.enable_hosted_transform = true;
    coord.setExecutableFinalizationEnabled(false);
    var arena = base.SingleThreadArena.init(allocator);
    defer arena.deinit();
    try coord.start();
    try coord.discoverAppFromPath(arena.allocator(), .{ .entry_path = app_path });
    try coord.coordinatorLoop();
    try std.testing.expect(!coord.hasUserErrors());

    var metrics = lir.CheckedPipeline.WorkMetrics{};
    const target: lir.CheckedPipeline.TargetConfig = .{
        // --opt=speed omits runtime expects; CTFE must still run them.
        .inline_expects = .omit,
        // Keep the named procedures observable instead of inlining their bodies.
        .inline_mode = .none,
        .proc_debug_names = true,
        .work_metrics = &metrics,
    };
    coord.runtime_lowering = .{ .target = target };
    try coord.finishCheckedProgram(.executable_artifacts);
    try std.testing.expect(!coord.hasUserErrors());
    const session = &coord.program_session.?;
    try std.testing.expect(session.compile_time_root_count > 0);
    try std.testing.expect(session.runtime_prepared != null);
    try std.testing.expectEqual(@as(u32, 1), metrics.monotype_runs);
    try std.testing.expectEqual(@as(u32, 1), metrics.solved_runs);

    var runtime = try session.takeRuntime(allocator, session.runtime_roots, target);
    defer runtime.deinit();
    // Anchor both names in the runtime output before asserting their absence
    // from CTFE, so missing debug names cannot make the regression pass.
    for ([_][]const u8{ "runtime_only_a", "runtime_only_b" }) |name| {
        try std.testing.expectEqual(@as(usize, 1), countNamedProcs(&runtime.lir_result.store, name));
    }
    try std.testing.expectEqual(@as(u32, 1), metrics.monotype_runs);
    try std.testing.expectEqual(@as(u32, 1), metrics.solved_runs);
    const host_store = &session.host.?.lir_result.store;
    const runtime_only_count = countNamedProcs(host_store, "runtime_only_a") + countNamedProcs(host_store, "runtime_only_b");
    try std.testing.expectEqual(@as(usize, 0), runtime_only_count);
}
