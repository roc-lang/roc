//! A module's Boxy unit lowers after the units of every module its lowering
//! sees (design.md "Per-Module Output", `Coordinator.unitDependencyKeys`),
//! which can lie outside its import closure: an app's entrypoint takes its
//! platform's requirement types, so the app's lowering names workers of the
//! modules declaring them without importing those modules.
const std = @import("std");
const base = @import("base");
const build_options = @import("build_options");
const check = @import("check");
const eval = @import("eval");
const lir = @import("lir");
const roc_target = @import("roc_target");
const static_data_exports = @import("static_data");
const CoreCtx = @import("ctx").CoreCtx;
const Coordinator = @import("../coordinator.zig").Coordinator;
const is_freestanding = @import("../threading.zig").is_freestanding;

const Artifact = check.CheckedArtifact.CheckedModuleArtifact;

/// A platform whose entrypoint takes a list of `Arg`, a nominal type with a
/// method override: every descriptor of `Arg` names the override's worker.
const arg_platform = [_]struct { path: []const u8, source: []const u8 }{
    .{ .path = ".roc_arg_platform/main.roc", .source =
    \\platform ""
    \\    requires {} { main! : List(Arg) => Try({}, [Exit(I8), ..]) }
    \\    exposes [Echo, Arg]
    \\    packages {}
    \\    provides { "roc_main": main_for_host! }
    \\    hosted { "roc_echo_line": Echo.line! }
    \\import Echo
    \\import Arg
    \\main_for_host! : List(Str) => I8
    \\main_for_host! = |args|
    \\    match main!(args.map(Arg.from_str)) {
    \\        Ok({}) => 0
    \\        Err(Exit(code)) => code
    \\        Err(_) => 1
    \\    }
    },
    .{ .path = ".roc_arg_platform/Echo.roc", .source =
    \\Echo := [].{
    \\    line! : Str => {}
    \\}
    },
    .{ .path = ".roc_arg_platform/Arg.roc", .source =
    \\Arg := [Arg(Str)].{
    \\    from_str : Str -> Arg
    \\    from_str = |text| Arg.Arg(text)
    \\    to_inspect : Arg -> Str
    \\    to_inspect = |_| "arg"
    \\}
    },
};

fn containsKey(keys: []const [32]u8, key: [32]u8) bool {
    for (keys) |known| {
        if (std.mem.eql(u8, &known, &key)) return true;
    }
    return false;
}

/// Append `artifact` to `order` after every unit it depends on.
fn appendAfterDependencies(
    allocator: std.mem.Allocator,
    coord: *Coordinator,
    artifact: *const Artifact,
    placed: *std.AutoHashMapUnmanaged([32]u8, void),
    order: *std.ArrayList(*const Artifact),
) (std.mem.Allocator.Error || error{TestUnexpectedResult})!void {
    if ((try placed.getOrPut(allocator, artifact.key.bytes)).found_existing) return;
    const dependency_keys = try coord.unitDependencyKeys(allocator, artifact);
    defer allocator.free(dependency_keys);
    for (dependency_keys) |key| {
        const dependency = coord.checkedArtifactByKey(.{ .bytes = key }) orelse return error.TestUnexpectedResult;
        // A module with platform requirements has no unit of its own.
        if (dependency.platform_type_inputs.requirements.items().len != 0) continue;
        try appendAfterDependencies(allocator, coord, dependency, placed, order);
    }
    try order.append(allocator, artifact);
}

test "an app's unit depends on the unit of a platform requirement type's module it does not import" {
    if (is_freestanding) return error.SkipZigTest;
    const boxy = @import("postcheck").Boxy;
    const allocator = std.testing.allocator;
    const io = std.testing.io;
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();
    for (arg_platform) |file| {
        if (std.fs.path.dirname(file.path)) |dir| try tmp_dir.dir.createDirPath(io, dir);
        try tmp_dir.dir.writeFile(io, .{ .sub_path = file.path, .data = file.source });
    }
    // The app imports only `Echo`. Calling `len` on its arguments builds the
    // descriptor of `List(Arg)`, which names `Arg`'s `to_inspect` worker.
    try tmp_dir.dir.writeFile(io, .{ .sub_path = "main.roc", .data =
        \\app [main!] { pf: platform "./.roc_arg_platform/main.roc" }
        \\import pf.Echo
        \\main! = |args| {
        \\    Echo.line!(args.len().to_str())
        \\    Ok({})
        \\}
    });
    const app_path = try tmp_dir.dir.realPathFileAlloc(io, "main.roc", allocator);
    defer allocator.free(app_path);
    var builtin_modules = try eval.BuiltinModules.init(allocator, io);
    defer builtin_modules.deinit();
    var coord = try Coordinator.init(
        allocator,
        .single_threaded,
        1,
        roc_target.RocTarget.detectNative(),
        &builtin_modules,
        build_options.compiler_compatibility_id,
        null,
        CoreCtx.os(allocator, allocator, io),
        .boxy,
    );
    defer coord.deinit();
    coord.enable_hosted_transform = true;
    var arena = base.SingleThreadArena.init(allocator);
    defer arena.deinit();
    try coord.start();
    try coord.discoverAppFromPath(arena.allocator(), .{ .entry_path = app_path });
    try coord.coordinatorLoop();
    try std.testing.expect(!coord.hasUserErrors());
    try coord.finishCheckedProgram(.executable_artifacts);
    try std.testing.expect(!coord.hasUserErrors());

    const app = coord.appRootCheckedArtifact();
    const app_views = try coord.collectImportedArtifactViews(arena.allocator(), app);
    const arg = for (app_views) |view| {
        if (std.mem.eql(u8, view.module_env.module_name, "Arg")) break coord.checkedArtifactByKey(view.key.digest()) orelse return error.TestUnexpectedResult;
    } else return error.TestUnexpectedResult;

    // The app does not import `Arg`, and its unit depends on `Arg`'s all
    // the same.
    for (app.direct_import_module_keys.items()) |key| {
        try std.testing.expect(!std.mem.eql(u8, &key.bytes, &arg.key.bytes));
    }
    const app_dependencies = try coord.unitDependencyKeys(allocator, app);
    defer allocator.free(app_dependencies);
    try std.testing.expect(containsKey(app_dependencies, arg.key.bytes));
    for (app.direct_import_module_keys.items()) |key| {
        try std.testing.expect(containsKey(app_dependencies, key.bytes));
    }
    try std.testing.expect(!containsKey(app_dependencies, app.key.bytes));

    // Lowering each unit after its dependencies gives the app's lowering the
    // export interface of every worker it names: `Arg`'s unit is lowered
    // before the app's, whose descriptor of `Arg` binds `Arg.to_inspect` to
    // the ownership signature `Arg`'s unit solved.
    var placed: std.AutoHashMapUnmanaged([32]u8, void) = .empty;
    defer placed.deinit(allocator);
    var order: std.ArrayList(*const Artifact) = .empty;
    defer order.deinit(allocator);
    try appendAfterDependencies(allocator, &coord, app, &placed, &order);
    try std.testing.expect(order.items[order.items.len - 1] == app);
    var arg_position: ?usize = null;
    for (order.items, 0..) |artifact, position| {
        if (artifact == arg) arg_position = position;
    }
    try std.testing.expect(arg_position != null);

    const views = try allocator.alloc(check.CheckedArtifact.ImportedModuleView, order.items.len);
    defer allocator.free(views);
    for (order.items, views) |artifact, *view| view.* = check.CheckedArtifact.importedView(artifact);
    var names = try lir.CheckedPipeline.BoxyModuleNames.fromViews(allocator, views);
    defer names.deinit(allocator);
    var interfaces = lir.CheckedPipeline.BoxyExportInterfaces.init(allocator, io);
    defer interfaces.deinit();
    for (order.items) |artifact| {
        var unit = try boxy.run(allocator, .{
            .modules = .{ .root = check.CheckedArtifact.loweringView(artifact), .imports = try coord.collectImportedArtifactViews(arena.allocator(), artifact) },
            .roots = .{ .requests = &.{} },
            .target_usize = base.target.TargetUsize.native,
            .unit = .{ .module = .{
                .imports = &interfaces,
                .root = false,
                .names = &names,
                .hosted_bindings = try coord.libraryHostedBindings(artifact, arena.allocator()),
            } },
        });
        defer unit.lir_result.deinit();
        defer unit.runtime_schemas.deinit();
        defer static_data_exports.deinitStaticData(allocator, unit.static_data);
        defer if (unit.retained) |retained| retained.deinit();
        for (unit.lir_result.exported_procs.items) |proc| {
            const identity = unit.lir_result.store.getProcSpec(proc).identity;
            _ = try interfaces.putProc(&unit.lir_result.store, proc, unit.lir_result.boxy_table_needs.get(identity) orelse &.{}, unit.lir_result.boxy_exports.get(identity).?);
        }
    }
}
