//! An optimized runtime program converts custom literals at compile time, as
//! every other build does: `--opt` never moves work between compile time and
//! runtime.

const std = @import("std");
const base = @import("base");
const build_options = @import("build_options");
const eval = @import("eval");
const lir = @import("lir");
const roc_target = @import("roc_target");
const CoreCtx = @import("ctx").CoreCtx;
const Coordinator = @import("../coordinator.zig").Coordinator;
const is_freestanding = @import("../threading.zig").is_freestanding;

fn countProcsNaming(store: *const lir.LirStore, fragment: []const u8) usize {
    var count: usize = 0;
    for (0..store.procSpecCount()) |index| {
        const name = store.procDebugName(@enumFromInt(index)) orelse continue;
        if (std.mem.find(u8, name, fragment) != null) count += 1;
    }
    return count;
}

test "an optimized runtime program reads a generic function's custom literal conversions as completed values" {
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
        \\Word := { text : Str }.{
        \\    is_eq : Word, Word -> Bool
        \\    is_eq = |a, b| a.text == b.text
        \\    from_quote : Str -> Try(Word, [BadQuotedBytes(Str)])
        \\    from_quote = |text| Ok({ text: Str.concat(text, "!") })
        \\}
        \\rank : a -> U64 where [a.from_quote : Str -> Try(a, [BadQuotedBytes(Str)]), a.is_eq : a, a -> Bool]
        \\rank = |value| match value {
        \\    "low" => 1
        \\    _ => 2
        \\}
        \\main! = |args| {
        \\    word : Word
        \\    word = Word.{ text: if List.len(args) > 5 "a" else "b" }
        \\    Echo.line!(Str.inspect(rank(word)))
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
    var arena = base.SingleThreadArena.init(allocator);
    defer arena.deinit();
    try coord.start();
    try coord.discoverAppFromPath(arena.allocator(), .{ .entry_path = app_path });
    try coord.coordinatorLoop();
    try std.testing.expect(!coord.hasUserErrors());

    // `--opt=speed`'s Solved policy: compile-time evaluation runs inside
    // this build's own specialized program.
    const target: lir.CheckedPipeline.TargetConfig = .{
        .inline_mode = .wrappers,
        .spec_constr_clone_inlining = .all_calls,
        .inline_expects = .omit,
        .proc_debug_names = true,
    };
    coord.runtime_lowering = .{ .target = target };
    try coord.finishCheckedProgram(.executable_artifacts);
    try std.testing.expect(!coord.hasUserErrors());
    const session = &coord.program_session.?;
    try std.testing.expect(session.runtime_prepared != null);

    var runtime = try session.takeRuntime(allocator, session.runtime_roots, target);
    defer runtime.deinit();
    try std.testing.expectEqual(@as(usize, 0), countProcsNaming(&runtime.lir_result.store, "from_quote"));
}
