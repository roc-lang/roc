//! Regression for #11527: reading a small uniform compile-time list constant
//! must not rebuild the list — and so allocate — at every use.

const std = @import("std");
const base = @import("base");
const build_options = @import("build_options");
const eval = @import("eval");
const lir = @import("lir");
const roc_target = @import("roc_target");
const CoreCtx = @import("ctx").CoreCtx;
const Coordinator = @import("../coordinator.zig").Coordinator;
const is_freestanding = @import("../threading.zig").is_freestanding;

fn countLowLevelOps(result: *const lir.Program.Result, op: lir.LIR.LowLevel) usize {
    var count: usize = 0;
    for (result.store.getCFStmts()) |stmt| {
        if (stmt == .assign_low_level and stmt.assign_low_level.op == op) count += 1;
    }
    return count;
}

test "issue 11527: a small uniform compile-time list constant is not rebuilt at every use" {
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
        .{ .path = "main.roc", .source =
        \\app [main!] { pf: platform "./.roc_echo_platform/main.roc" }
        \\import pf.Echo
        \\space_bytes : List(U8)
        \\space_bytes = [32]
        \\main! = |args| {
        \\    n = List.len(args)
        \\    Echo.line!(Str.inspect(List.get(space_bytes, n)))
        \\    Echo.line!(Str.inspect(List.get(space_bytes, n + 1)))
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

    // The default target matches the compile-time host program's width and
    // expect mode, so one program serves both consumers — the path a default
    // `roc build` takes.
    const target: lir.CheckedPipeline.TargetConfig = .{};
    coord.runtime_lowering = .{ .target = target };
    try coord.finishCheckedProgram(.executable_artifacts);
    try std.testing.expect(!coord.hasUserErrors());
    const session = &coord.program_session.?;
    try std.testing.expect(session.compile_time_root_count > 0);

    var runtime = try session.takeRuntime(allocator, session.runtime_roots, target);
    defer runtime.deinit();

    // A one-byte immutable constant rides in static data, so reading it must
    // not reserve and fill a fresh list per use. `list_with_capacity` and
    // `list_append_unsafe` are the repeat loop a uniform list's construction
    // lowers to; any occurrence in the runtime program means the constant is
    // reallocated as it is read.
    // https://github.com/roc-lang/roc/issues/11527
    try std.testing.expectEqual(@as(usize, 0), countLowLevelOps(&runtime.lir_result, .list_with_capacity));
    try std.testing.expectEqual(@as(usize, 0), countLowLevelOps(&runtime.lir_result, .list_append_unsafe));
    // The reads are still served: the constant's value occupies a static
    // data slot, as any non-uniform constant list already does.
    try std.testing.expect(runtime.lir_result.static_data_values.items.len > 0);
}