//! Shared test harness: compile an app and lower it all the way to LIR,
//! asserting no checker errors and no ARC borrow-certifier violation. The
//! certifier runs inside `lowerCheckedModulesToLir` and panics on any
//! violation, so returning normally is the assertion.

const std = @import("std");
const base = @import("base");
const build_options = @import("build_options");
const check = @import("check");
const collections = @import("collections");
const eval = @import("eval");
const lir = @import("lir");
const roc_target = @import("roc_target");

const Coordinator = @import("../coordinator.zig").Coordinator;
const CoreCtx = @import("ctx").CoreCtx;

/// Lower an app whose body is `app_body` (everything after the platform header
/// and the echo wiring) to LIR. Reaching the end without a panic means the
/// program checked cleanly and passed ARC certification.
pub fn expectLowersToLir(app_body: []const u8) anyerror!void {
    try runToLir(app_body, null);
}

/// Lower an app at `app_path` to LIR. Reaching the end without a panic means
/// the app checked cleanly and passed ARC certification.
pub fn expectAppPathLowersToLir(app_path: []const u8) anyerror!void {
    try lowerAppPathToLir(std.testing.allocator, app_path, null);
}

/// Lower `app_body` twice and assert the two LIR dumps are byte-identical, so
/// a regression that made lowering (e.g. capture order) depend on iteration or
/// scheduling order would fail here rather than silently.
pub fn expectDeterministicLir(app_body: []const u8) anyerror!void {
    const gpa = std.testing.allocator;
    const cap = 1 << 22;
    const buf_a = try gpa.alloc(u8, cap);
    defer gpa.free(buf_a);
    const buf_b = try gpa.alloc(u8, cap);
    defer gpa.free(buf_b);
    var writer_a = std.Io.Writer.fixed(buf_a);
    var writer_b = std.Io.Writer.fixed(buf_b);
    try runToLir(app_body, &writer_a);
    try runToLir(app_body, &writer_b);
    try std.testing.expectEqualStrings(writer_a.buffered(), writer_b.buffered());
}

fn runToLir(app_body: []const u8, dump: ?*std.Io.Writer) anyerror!void {
    const gpa = std.testing.allocator;
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    try tmp_dir.dir.createDirPath(std.testing.io, ".roc_echo_platform");
    const synthetic_source = try std.fmt.allocPrint(
        gpa,
        "app [main!] {{ pf: platform \"./.roc_echo_platform/main.roc\" }}\n\n" ++
            "import pf.Echo\n\n" ++
            "echo! = |msg| Echo.line!(msg)\n\n" ++
            "{s}",
        .{app_body},
    );
    defer gpa.free(synthetic_source);
    try tmp_dir.dir.writeFile(std.testing.io, .{
        .sub_path = "main.roc",
        .data = synthetic_source,
    });
    try tmp_dir.dir.writeFile(std.testing.io, .{
        .sub_path = ".roc_echo_platform/main.roc",
        .data =
        \\platform ""
        \\    requires {} { main! : List(Str) => Try({}, [Exit(I8), ..]) }
        \\    exposes [Echo]
        \\    packages {}
        \\    provides { "roc_main": main_for_host! }
        \\    hosted { "roc_echo_line": Echo.line! }
        \\
        \\import Echo
        \\
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
        ,
    });
    try tmp_dir.dir.writeFile(std.testing.io, .{
        .sub_path = ".roc_echo_platform/Echo.roc",
        .data =
        \\Echo := [].{
        \\    line! : Str => {}
        \\}
        ,
    });
    const app_path = try tmp_dir.dir.realPathFileAlloc(std.testing.io, "main.roc", gpa);
    defer gpa.free(app_path);

    try lowerAppPathToLir(gpa, app_path, dump);
}

fn lowerAppPathToLir(gpa: std.mem.Allocator, app_path: []const u8, dump: ?*std.Io.Writer) anyerror!void {
    var arena_impl = collections.SingleThreadArena.init(gpa);
    defer arena_impl.deinit();
    const arena = arena_impl.allocator();

    var builtin_modules = try eval.BuiltinModules.init(gpa);
    defer builtin_modules.deinit();

    var coord = try Coordinator.init(
        gpa,
        .single_threaded,
        1,
        roc_target.RocTarget.detectNative(),
        &builtin_modules,
        build_options.compiler_version,
        null,
        CoreCtx.default(gpa, arena, std.testing.io),
    );
    defer coord.deinit();
    coord.enable_hosted_transform = true;

    try coord.start();
    try coord.discoverAppFromPath(arena, .{ .entry_path = app_path });
    try coord.coordinatorLoop();
    try std.testing.expect(!coord.hasUserErrors());

    try coord.finalizeExecutableArtifacts();
    try std.testing.expect(!coord.hasUserErrors());

    const root = coord.executableRootCheckedArtifact();
    const imports = try coord.collectImportedArtifactViews(arena, root);
    const relations = try coord.collectRelationArtifactViews(arena, root);

    const lir_roots = try lir.CheckedPipeline.selectPlatformEntrypointRoots(gpa, root.root_requests.runtime_requests);
    defer gpa.free(lir_roots);

    var lowered = try lir.CheckedPipeline.lowerCheckedModulesToLir(
        gpa,
        .{
            .root = check.CheckedArtifact.loweringViewWithRelations(root, relations),
            .imports = imports,
        },
        .{ .requests = lir_roots },
        .{ .target_usize = base.target.TargetUsize.native },
    );
    defer lowered.deinit();

    if (dump) |writer| {
        const store = &lowered.lir_result.store;
        const layouts = &lowered.lir_result.layouts;
        for (0..store.proc_specs.items.len) |index| {
            try lir.DebugPrint.writeProc(gpa, store, layouts, @enumFromInt(@as(u32, @intCast(index))), writer);
        }
    }
}
