//! Tests for compile-time evaluation of selected top-level-equivalent locals.

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

test "hoisted local constants are finalized and restored during runtime lowering" {
    const gpa = std.testing.allocator;

    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    try tmp_dir.dir.createDirPath(std.testing.io, ".roc_echo_platform");
    try tmp_dir.dir.writeFile(std.testing.io, .{
        .sub_path = "main.roc",
        .data =
        \\app [main!] { pf: platform "./.roc_echo_platform/main.roc" }
        \\
        \\import pf.Echo
        \\
        \\top = 40.I64
        \\
        \\main! = |_args| {
        \\    x = top + 1.I64
        \\    y = x + 1.I64
        \\    _ = y
        \\    Echo.line!("done")
        \\    Ok({})
        \\}
        ,
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
    if (coord.hasUserErrors()) {
        var debug_reports = coord.iterReports();
        while (debug_reports.next()) |entry| {
            std.debug.print("pre-finalize report: {s} in {s}\n", .{ entry.report.title, entry.module_name });
        }
    }
    try std.testing.expect(!coord.hasUserErrors());

    try coord.finalizeExecutableArtifacts();
    try std.testing.expect(!coord.hasUserErrors());

    const root = coord.executableRootCheckedArtifact();
    const imports = try coord.collectImportedArtifactViews(arena, root);
    const relations = try coord.collectRelationArtifactViews(arena, root);
    const root_view = check.CheckedArtifact.importedView(root);
    const app = if (root_view.hoisted_constants.entries.len >= 2)
        root_view
    else
        findHoistedArtifact(imports, 2) orelse
            findHoistedArtifact(relations, 2) orelse
            return error.AppHoistedConstantsNotFound;

    try expectTopLevelConstantsBeforeHoisted(app);

    const forty_one = findStoredI64(app, 41) orelse return error.HoistedFortyOneNotFound;
    const forty_two = findStoredI64(app, 42) orelse return error.HoistedFortyTwoNotFound;
    try std.testing.expect(@intFromEnum(forty_one.root) < @intFromEnum(forty_two.root));

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
}

test "imported checked bodies restore their module's hoisted constants" {
    const gpa = std.testing.allocator;

    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    try tmp_dir.dir.createDirPath(std.testing.io, ".roc_echo_platform");
    try tmp_dir.dir.writeFile(std.testing.io, .{
        .sub_path = "main.roc",
        .data =
        \\app [main!] { pf: platform "./.roc_echo_platform/main.roc" }
        \\
        \\import pf.Echo
        \\import Helper
        \\
        \\main! = |args| {
        \\    value = Helper.helper(List.len(args).to_i64_wrap())
        \\    _ = value
        \\    Echo.line!("done")
        \\    Ok({})
        \\}
        ,
    });
    try tmp_dir.dir.writeFile(std.testing.io, .{
        .sub_path = "Helper.roc",
        .data =
        \\module [helper]
        \\
        \\helper : I64 -> I64
        \\helper = |arg| {
        \\    x = 41.I64
        \\    y = x + 1.I64
        \\    y + arg
        \\}
        ,
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
    const helper = findHoistedArtifact(imports, 2) orelse return error.ImportedHoistedConstantsNotFound;

    const forty_one = findStoredI64(helper, 41) orelse return error.ImportedHoistedFortyOneNotFound;
    const forty_two = findStoredI64(helper, 42) orelse return error.ImportedHoistedFortyTwoNotFound;
    try std.testing.expect(@intFromEnum(forty_one.root) < @intFromEnum(forty_two.root));

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
}

test "hoisted constant crash reports original source region" {
    const gpa = std.testing.allocator;

    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    try tmp_dir.dir.createDirPath(std.testing.io, ".roc_echo_platform");
    try tmp_dir.dir.writeFile(std.testing.io, .{
        .sub_path = "main.roc",
        .data =
        \\app [main!] { pf: platform "./.roc_echo_platform/main.roc" }
        \\
        \\import pf.Echo
        \\
        \\main! = |args| {
        \\    x = 1.I64 // 0.I64
        \\    _ = x + List.len(args).to_i64_wrap()
        \\    Echo.line!("done")
        \\    Ok({})
        \\}
        ,
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
    try std.testing.expect(coord.hasUserErrors());

    var found = false;
    var report_iter = coord.iterReports();
    while (report_iter.next()) |entry| {
        if (!std.mem.eql(u8, entry.report.title, "COMPTIME CRASH")) continue;
        found = true;
        const region = entry.report.getRegionInfo() orelse return error.ComptimeCrashReportHadNoRegion;
        try std.testing.expectEqual(@as(u32, 6), region.start_line_idx);
        try std.testing.expectEqual(@as(u32, 6), region.end_line_idx);
        try std.testing.expectEqualStrings("main", entry.module_name);
    }
    try std.testing.expect(found);
}

fn findHoistedArtifact(
    views: []const check.CheckedArtifact.ImportedModuleView,
    min_count: usize,
) ?check.CheckedArtifact.ImportedModuleView {
    for (views) |view| {
        if (view.hoisted_constants.entries.len >= min_count) return view;
    }
    return null;
}

fn expectTopLevelConstantsBeforeHoisted(
    artifact: check.CheckedArtifact.ImportedModuleView,
) !void {
    var saw_top_level_constant = false;
    var saw_hoisted_constant = false;
    var seen_hoisted_root = false;

    for (artifact.compile_time_roots.roots) |root| {
        switch (root.kind) {
            .constant => {
                if (seen_hoisted_root) return error.TopLevelConstantScheduledAfterHoisted;
                saw_top_level_constant = true;
            },
            .hoisted_constant => {
                seen_hoisted_root = true;
                saw_hoisted_constant = true;
            },
            .callable_binding,
            .expect,
            .numeral_conversion,
            .quote_conversion,
            => {},
        }
    }

    try std.testing.expect(saw_top_level_constant);
    try std.testing.expect(saw_hoisted_constant);
}

fn findStoredI64(
    artifact: check.CheckedArtifact.ImportedModuleView,
    expected: i64,
) ?check.CheckedArtifact.HoistedConstEntry {
    for (artifact.hoisted_constants.entries) |entry| {
        if (storedI64(artifact, entry)) |actual| {
            if (actual == expected) return entry;
        } else |_| {}
    }
    return null;
}

fn storedI64(
    artifact: check.CheckedArtifact.ImportedModuleView,
    entry: check.CheckedArtifact.HoistedConstEntry,
) !i64 {
    const root = artifact.compile_time_roots.root(entry.root);
    if (root.kind != .hoisted_constant) return error.HoistedRootKindMismatch;

    const node_from_root = switch (root.payload) {
        .const_node => |node| node,
        .pending,
        .fn_value,
        .expect,
        => return error.HoistedRootDidNotStoreConstNode,
    };
    const template = artifact.const_templates.get(entry.const_ref);
    const node_from_template = switch (template.state) {
        .stored_const => |stored| stored.node,
        .reserved,
        .eval_template,
        => return error.HoistedTemplateWasNotStored,
    };
    try std.testing.expectEqual(node_from_root, node_from_template);

    const value = artifact.const_store.get(node_from_root);
    const actual = switch (value) {
        .scalar => |scalar| switch (scalar) {
            .i64 => |i| i,
            .i8,
            .i16,
            .i32,
            .i128,
            .u8,
            .u16,
            .u32,
            .u64,
            .u128,
            .f32_bits,
            .f64_bits,
            .dec_bits,
            => return error.HoistedConstWasNotI64,
        },
        .pending,
        .zst,
        .str,
        .list,
        .box,
        .tuple,
        .record,
        .crash,
        .tag,
        .nominal,
        .fn_value,
        => return error.HoistedConstWasNotScalar,
    };
    return actual;
}
