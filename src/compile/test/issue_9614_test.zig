//! Regression test for issue #9614: a fold_until accumulator whose open tag
//! union is widened across dispatch-plan positions must lower to LIR.

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

test "issue 9614: default app list fold_until result tag narrowing lowers to LIR" {
    const original_source =
        \\solve = |problem| {
        \\    find_match = |chars| {
        \\        match chars {
        \\            [] => Err(InvalidAssignment)
        \\            [_, ..] => {
        \\                find_first_ok(chars, |_| Err(InvalidAssignment))
        \\            }
        \\        }
        \\    }
        \\    find_match(problem.to_utf8())
        \\}
        \\
        \\find_first_ok = |list, func| {
        \\    list.fold_until(
        \\        Err(InvalidAssignment),
        \\        |state, elem| {
        \\            match func(elem) {
        \\                Err(_) => Continue(state)
        \\                Ok(val) => Break(Ok(val))
        \\            }
        \\        },
        \\    )
        \\}
        \\
        \\main! = |_args| {
        \\    _ = solve("abc")
        \\    Ok({})
        \\}
    ;

    const gpa = std.testing.allocator;
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    try tmp_dir.dir.createDirPath(std.testing.io, ".roc_echo_platform");
    const synthetic_source = "app [main!] { pf: platform \"./.roc_echo_platform/main.roc\" }\n\n" ++
        "import pf.Echo\n\n" ++
        "echo! = |msg| Echo.line!(msg)\n\n" ++
        original_source;
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
        \\    provides { main_for_host!: "main" }
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

    var lowered = try lir.CheckedPipeline.lowerCheckedModulesToLir(
        gpa,
        .{
            .root = check.CheckedArtifact.loweringViewWithRelations(root, relations),
            .imports = imports,
        },
        .{ .requests = root.root_requests.runtime_requests },
        .{ .target_usize = base.target.TargetUsize.native },
    );
    defer lowered.deinit();
}
