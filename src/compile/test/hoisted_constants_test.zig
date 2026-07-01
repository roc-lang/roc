//! Tests for compile-time evaluation of selected top-level-equivalent locals.

const std = @import("std");
const base = @import("base");
const build_options = @import("build_options");
const can = @import("can");
const check = @import("check");
const collections = @import("collections");
const eval = @import("eval");
const lir = @import("lir");
const roc_target = @import("roc_target");

const Coordinator = @import("../coordinator.zig").Coordinator;
const CoreCtx = @import("ctx").CoreCtx;

const HoistedConstantsTestError = std.mem.Allocator.Error ||
    std.Io.Dir.CreateDirPathError ||
    std.Io.Dir.WriteFileError ||
    std.Io.File.Writer.Error ||
    error{
        AfterRootHadNoRequest,
        BeforeRootHadNoRequest,
        ExportedRuntimeEntrypointNotFound,
        HoistedConstWasNotI64,
        HoistedConstWasNotScalar,
        HoistedRootDidNotStoreConstNode,
        HoistedRootKindMismatch,
        HoistedTemplateWasNotStored,
        OutOfMemory,
        PatternExtractionMissingCheckedRootPattern,
        PatternExtractionMissingSourcePattern,
        PatternExtractionRootValueWasNotSyntheticLookup,
        PatternExtractionRootWasNotSyntheticMatch,
        RootDidNotStoreConstNode,
        TestExpectedEqual,
        TestUnexpectedResult,
    };

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
        \\top_a = 40.I64
        \\top_b = top_a + 1.I64
        \\
        \\top_callable = if 1.I64 == 1.I64 {
        \\    |n| n + 1.I64
        \\} else {
        \\    |n| n + 2.I64
        \\}
        \\
        \\top_add_five = |n| n + 5.I64
        \\
        \\indirect_top_helper = |n| {
        \\    indirect_local = 123.I64
        \\    indirect_local + n
        \\}
        \\
        \\indirect_top_value = indirect_top_helper(333.I64)
        \\
        \\DispatchBox := [Val(I64)].{
        \\    add = |DispatchBox.Val(n), delta| DispatchBox.Val(n + delta)
        \\    unwrap = |DispatchBox.Val(n)| n
        \\}
        \\
        \\main! = |args| {
        \\    x = top_b + 1.I64
        \\    y = x + 1.I64
        \\    called = top_callable(41.I64)
        \\    called_unique = top_add_five(72.I64)
        \\    dispatched = DispatchBox.Val(89.I64).add(2.I64).unwrap()
        \\    block_value = {
        \\        block_x = 52.I64
        \\        block_y = block_x + 1.I64
        \\        block_y
        \\    }
        \\    { z } = { z: 44.I64 }
        \\    pair = (40.I64, 6.I64)
        \\    (left, right) = pair
        \\    tuple_total = left + right
        \\    Ok(tag_value) = Ok(45.I64)
        \\    match_tuple_total = match (50.I64, 8.I64) {
        \\        (match_left, match_right) => match_left + match_right + List.len(args).to_i64_wrap()
        \\    }
        \\    alias_total = match 46.I64 {
        \\        _n as whole => whole + List.len(args).to_i64_wrap()
        \\    }
        \\    rest_total = match [40.I64, 2.I64] {
        \\        [.. as rest] => List.len(rest).to_i64_wrap() + List.len(args).to_i64_wrap()
        \\    }
        \\    closed_match_input : Try(I64, I64)
        \\    closed_match_input = Ok(40.I64)
        \\    closed_match_total = match closed_match_input {
        \\        Ok(n) => n + 2.I64
        \\        Err(code) => code
        \\    }
        \\    _ = y + List.len(args).to_i64_wrap()
        \\    _ = called + List.len(args).to_i64_wrap()
        \\    _ = called_unique + List.len(args).to_i64_wrap()
        \\    _ = dispatched + List.len(args).to_i64_wrap()
        \\    _ = indirect_top_value + List.len(args).to_i64_wrap()
        \\    _ = block_value + List.len(args).to_i64_wrap()
        \\    _ = z + tuple_total + List.len(args).to_i64_wrap()
        \\    _ = tag_value + List.len(args).to_i64_wrap()
        \\    _ = match_tuple_total
        \\    _ = alias_total
        \\    _ = rest_total
        \\    _ = closed_match_total + List.len(args).to_i64_wrap()
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
    const app_artifact = coord.rootCheckedArtifact("app");
    const app_view = check.CheckedArtifact.importedView(app_artifact);
    const imports = try coord.collectImportedArtifactViews(arena, root);
    const relations = try coord.collectRelationArtifactViews(arena, root);
    const root_view = check.CheckedArtifact.importedView(root);
    const app = if (root_view.hoisted_constants.entries.len >= 2)
        root_view
    else
        findHoistedArtifact(imports, 2) orelse
            findHoistedArtifact(relations, 2) orelse
            return error.AppHoistedConstantsNotFound;

    try expectCompileTimeRootKindsPresent(app);
    try expectCompileTimeRootKindsPresent(app_view);
    try expectExportedRuntimeEntrypoint(app_artifact);

    const top_a = findStoredCompileTimeRootI64(app_view, .constant, 40) orelse return error.TopLevelFortyNotFound;
    const top_b = findStoredCompileTimeRootI64(app_view, .constant, 41) orelse return error.TopLevelFortyOneNotFound;
    const indirect_top = findStoredCompileTimeRootI64(app_view, .constant, 456) orelse return error.IndirectTopLevelConstNotFound;
    const tuple_left = findStoredI64(app_view, 40) orelse return error.HoistedTupleLeftExtractionNotFound;
    const tuple_right = findStoredI64(app_view, 6) orelse return error.HoistedTupleRightExtractionNotFound;
    const record_extraction = findStoredI64(app_view, 44) orelse return error.HoistedRecordExtractionNotFound;
    const tag_extraction = findStoredI64(app_view, 45) orelse return error.HoistedTagPayloadExtractionNotFound;
    const match_tuple_left = findStoredI64(app_view, 50) orelse return error.HoistedMatchTupleLeftExtractionNotFound;
    const match_tuple_right = findStoredI64(app_view, 8) orelse return error.HoistedMatchTupleRightExtractionNotFound;
    const match_alias = findStoredI64(app_view, 46) orelse return error.HoistedMatchAliasExtractionNotFound;
    const ordinary_call = findStoredI64(app_view, 77) orelse return error.HoistedOrdinaryCallNotFound;
    const block_value = findStoredI64(app_view, 53) orelse return error.HoistedBlockValueNotFound;
    const indirect_local = findStoredI64(app_view, 123) orelse return error.IndirectHoistedLocalNotFound;
    _ = findStoredI64(app_view, 91) orelse return error.HoistedStaticDispatchCallNotFound;
    try std.testing.expect(countStoredHoistedI64(app_view, 42) >= 2);
    try std.testing.expect(countCompileTimeRootKind(app_artifact, .callable_binding) >= 1);
    try std.testing.expect(countHoistedMatchRoots(app_artifact) >= 1);

    try expectRootRequestBefore(app_artifact, top_a.id, top_b.id);
    try expectRootRequestBefore(app_artifact, top_b.id, tuple_left.root);
    try expectRootRequestBefore(app_artifact, top_b.id, tuple_right.root);
    try expectRootRequestBefore(app_artifact, top_b.id, record_extraction.root);
    try expectRootRequestBefore(app_artifact, top_b.id, tag_extraction.root);
    try expectRootRequestBefore(app_artifact, top_b.id, match_tuple_left.root);
    try expectRootRequestBefore(app_artifact, top_b.id, match_tuple_right.root);
    try expectRootRequestBefore(app_artifact, top_b.id, match_alias.root);
    try expectRootRequestBefore(app_artifact, top_b.id, ordinary_call.root);
    try expectRootRequestBefore(app_artifact, top_b.id, block_value.root);
    try expectRootRequestBefore(app_artifact, indirect_local.root, indirect_top.id);
    try expectPatternExtractionSyntheticRegions(app_artifact);

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
        \\    local_from_import = Helper.base + 1.I64
        \\    value = Helper.helper(List.len(args).to_i64_wrap())
        \\    _ = local_from_import + List.len(args).to_i64_wrap()
        \\    _ = value
        \\    Echo.line!("done")
        \\    Ok({})
        \\}
        ,
    });
    try tmp_dir.dir.writeFile(std.testing.io, .{
        .sub_path = "Helper.roc",
        .data =
        \\Helper := [].{
        \\    base = 41.I64
        \\
        \\    helper : I64 -> I64
        \\    helper = |arg| {
        \\        x = 41.I64
        \\        y = x + 1.I64
        \\        y + arg
        \\    }
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
    const app_artifact = coord.rootCheckedArtifact("app");
    const app_view = check.CheckedArtifact.importedView(app_artifact);
    const imports = try coord.collectImportedArtifactViews(arena, root);
    const relations = try coord.collectRelationArtifactViews(arena, root);
    const helper = findHoistedArtifact(imports, 2) orelse return error.ImportedHoistedConstantsNotFound;

    const app_imported_const_use = findStoredI64(app_view, 42) orelse return error.AppHoistedImportedConstUseNotFound;
    const forty_one = findStoredI64(helper, 41) orelse return error.ImportedHoistedFortyOneNotFound;
    const forty_two = findStoredI64(helper, 42) orelse return error.ImportedHoistedFortyTwoNotFound;
    try std.testing.expectEqual(
        check.CheckedArtifact.CompileTimeRootKind.hoisted_constant,
        app_artifact.compile_time_roots.root(app_imported_const_use.root).kind,
    );
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
        if (!std.mem.eql(u8, entry.report.title, "Compile Time Crash")) continue;
        found = true;
        const region = entry.report.getRegionInfo() orelse return error.ComptimeCrashReportHadNoRegion;
        try std.testing.expectEqual(@as(u32, 6), region.start_line_idx);
        try std.testing.expectEqual(@as(u32, 6), region.end_line_idx);
        try std.testing.expectEqualStrings("main", entry.module_name);
    }
    try std.testing.expect(found);
}

test "inlined hoisted constant crash reports hoisted source region" {
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
        \\helper : I64 -> I64
        \\helper = |n| {
        \\    x = 1.I64 // 0.I64
        \\    n + x
        \\}
        \\
        \\value = helper(41.I64)
        \\
        \\main! = |args| {
        \\    _ = value + List.len(args).to_i64_wrap()
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
        if (!std.mem.eql(u8, entry.report.title, "Compile Time Crash")) continue;
        found = true;
        const region = entry.report.getRegionInfo() orelse return error.ComptimeCrashReportHadNoRegion;
        try std.testing.expectEqual(@as(u32, 7), region.start_line_idx);
        try std.testing.expectEqual(@as(u32, 7), region.end_line_idx);
        try std.testing.expectEqualStrings("main", entry.module_name);
    }
    try std.testing.expect(found);
}

test "hoisted pattern extraction failure reports original destructure region" {
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
        \\    x : Try(I64, Str)
        \\    x = Err("bad")
        \\    Ok(foo) = x
        \\    _ = foo + List.len(args).to_i64_wrap()
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
        if (!std.mem.eql(u8, entry.report.title, "Non Exhaustive Destructure")) continue;
        found = true;
        const region = entry.report.getRegionInfo() orelse return error.NonExhaustiveDestructureReportHadNoRegion;
        try std.testing.expectEqual(@as(u32, 8), region.start_line_idx);
        try std.testing.expectEqual(@as(u32, 8), region.end_line_idx);
        try std.testing.expectEqualStrings("main", entry.module_name);
    }
    try std.testing.expect(found);
}

test "hoisted pattern extraction base match failure reports match" {
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
        \\    x : Try(I64, Str)
        \\    x = Err("bad")
        \\    Ok(foo) = match x {
        \\        Ok(n) => Ok(n)
        \\    }
        \\    _ = foo + List.len(args).to_i64_wrap()
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

    var found_match = false;
    var found_destructure = false;
    var report_iter = coord.iterReports();
    while (report_iter.next()) |entry| {
        if (std.mem.eql(u8, entry.report.title, "Non Exhaustive Match")) {
            found_match = true;
            try std.testing.expectEqualStrings("main", entry.module_name);
        }
        if (std.mem.eql(u8, entry.report.title, "Non Exhaustive Destructure")) {
            found_destructure = true;
        }
    }
    try std.testing.expect(found_match);
    try std.testing.expect(!found_destructure);
}

test "hoisted pattern extraction successful base match resolves pending diagnostics" {
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
        \\    x : Try(I64, Str)
        \\    x = Ok(41.I64)
        \\    Ok(foo) = match x {
        \\        Ok(n) => Ok(n)
        \\    }
        \\    _ = foo + List.len(args).to_i64_wrap()
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
    try std.testing.expect(!coord.hasUserErrors());
}

test "hoisted match guard does not report unused branch warning" {
    const gpa = std.testing.allocator;

    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    try writeEchoPlatform(tmp_dir.dir);
    try tmp_dir.dir.writeFile(std.testing.io, .{
        .sub_path = "main.roc",
        .data =
        \\app [main!] { pf: platform "./.roc_echo_platform/main.roc" }
        \\
        \\import pf.Echo
        \\
        \\main! = |args| {
        \\    result = match 5.I64 {
        \\        x if x > 0 => "positive"
        \\        _ => "non-positive"
        \\    }
        \\    _ = List.len(args)
        \\    Echo.line!(result)
        \\    Ok({})
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

    var found_unused_branch = false;
    var report_iter = coord.iterReports();
    while (report_iter.next()) |entry| {
        if (std.mem.eql(u8, entry.report.title, "Unused Branch")) found_unused_branch = true;
    }
    try std.testing.expect(!found_unused_branch);

    const artifact = coord.rootCheckedArtifact("app");
    var found_hoisted_root = false;
    for (artifact.compile_time_roots.roots) |root| {
        if (root.kind == .hoisted_constant) found_hoisted_root = true;
    }
    try std.testing.expect(found_hoisted_root);
}

test "hoisted successful call does not clear runtime reachable helper exhaustiveness" {
    const gpa = std.testing.allocator;

    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    try writeEchoPlatform(tmp_dir.dir);
    try tmp_dir.dir.writeFile(std.testing.io, .{
        .sub_path = "main.roc",
        .data =
        \\app [main!] { pf: platform "./.roc_echo_platform/main.roc" }
        \\
        \\import pf.Echo
        \\
        \\helper : Try(I64, Str) -> I64
        \\helper = |result|
        \\    match result {
        \\        Ok(n) => n
        \\    }
        \\
        \\main! = |args| {
        \\    x = helper(Ok(1.I64))
        \\    runtime_input = if List.len(args) == 0 {
        \\        Ok(2.I64)
        \\    } else {
        \\        Err("runtime")
        \\    }
        \\    _ = helper(runtime_input)
        \\    _ = x + List.len(args).to_i64_wrap()
        \\    Echo.line!("done")
        \\    Ok({})
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
        if (!std.mem.eql(u8, entry.report.title, "Non Exhaustive Match")) continue;
        found = true;
        try std.testing.expectEqualStrings("main", entry.module_name);
    }
    try std.testing.expect(found);
}

test "hoisted failing call into runtime reachable helper reports static diagnostic" {
    const gpa = std.testing.allocator;

    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    try writeEchoPlatform(tmp_dir.dir);
    try tmp_dir.dir.writeFile(std.testing.io, .{
        .sub_path = "main.roc",
        .data =
        \\app [main!] { pf: platform "./.roc_echo_platform/main.roc" }
        \\
        \\import pf.Echo
        \\
        \\helper : Try(I64, Str) -> I64
        \\helper = |result|
        \\    match result {
        \\        Ok(n) => n
        \\    }
        \\
        \\main! = |args| {
        \\    x = helper(Err("compile-time"))
        \\    runtime_input = if List.len(args) == 0 {
        \\        Ok(2.I64)
        \\    } else {
        \\        Err("runtime")
        \\    }
        \\    _ = helper(runtime_input)
        \\    _ = x + List.len(args).to_i64_wrap()
        \\    Echo.line!("done")
        \\    Ok({})
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
        if (!std.mem.eql(u8, entry.report.title, "Non Exhaustive Match")) continue;
        found = true;
        try std.testing.expectEqualStrings("main", entry.module_name);
        try expectReportDoesNotContain(gpa, entry.report, "empirically during compile-time evaluation");
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

fn expectCompileTimeRootKindsPresent(
    artifact: check.CheckedArtifact.ImportedModuleView,
) HoistedConstantsTestError!void {
    var saw_top_level_constant = false;
    var saw_top_level_callable = false;
    var saw_hoisted_constant = false;

    for (artifact.compile_time_roots.roots) |root| {
        switch (root.kind) {
            .constant => {
                saw_top_level_constant = true;
            },
            .callable_binding => {
                saw_top_level_callable = true;
            },
            .hoisted_constant => {
                saw_hoisted_constant = true;
            },
            .expect,
            .numeral_conversion,
            .quote_conversion,
            => {},
        }
    }

    try std.testing.expect(saw_top_level_constant);
    try std.testing.expect(saw_top_level_callable);
    try std.testing.expect(saw_hoisted_constant);
}

fn expectExportedRuntimeEntrypoint(
    artifact: *const check.CheckedArtifact.CheckedModuleArtifact,
) HoistedConstantsTestError!void {
    for (artifact.root_requests.runtime_requests) |root| {
        if (root.kind == .runtime_entrypoint and
            root.abi == .roc and
            root.exposure == .exported and
            root.procedure_binding != null)
        {
            return;
        }
    }
    return error.ExportedRuntimeEntrypointNotFound;
}

fn expectRootRequestBefore(
    artifact: *const check.CheckedArtifact.CheckedModuleArtifact,
    before: check.CheckedArtifact.ComptimeRootId,
    after: check.CheckedArtifact.ComptimeRootId,
) HoistedConstantsTestError!void {
    const before_index = compileTimeRequestIndexForRoot(artifact, before) orelse return error.BeforeRootHadNoRequest;
    const after_index = compileTimeRequestIndexForRoot(artifact, after) orelse return error.AfterRootHadNoRequest;
    try std.testing.expect(before_index < after_index);
}

fn compileTimeRequestIndexForRoot(
    artifact: *const check.CheckedArtifact.CheckedModuleArtifact,
    root_id: check.CheckedArtifact.ComptimeRootId,
) ?usize {
    const root = artifact.compile_time_roots.root(root_id);
    for (artifact.root_requests.compile_time_requests, 0..) |request, i| {
        if (rootRequestMatchesCompileTimeRoot(root, request)) return i;
    }
    return null;
}

fn rootRequestMatchesCompileTimeRoot(
    root: check.CheckedArtifact.CompileTimeRoot,
    request: check.CheckedArtifact.RootRequest,
) bool {
    if (!compileTimeRootKindMatchesRequest(root.kind, request.kind)) return false;
    if (!rootSourceMatches(root.source, request.source)) return false;
    return true;
}

fn compileTimeRootKindMatchesRequest(
    root_kind: check.CheckedArtifact.CompileTimeRootKind,
    request_kind: check.CheckedArtifact.RootRequestKind,
) bool {
    return switch (root_kind) {
        .constant, .hoisted_constant => request_kind == .compile_time_constant,
        .callable_binding => request_kind == .compile_time_callable,
        .expect => request_kind == .test_expect,
        .numeral_conversion, .quote_conversion => request_kind == .compile_time_constant,
    };
}

fn rootSourceMatches(
    a: check.CheckedArtifact.RootSource,
    b: check.CheckedArtifact.RootSource,
) bool {
    if (std.meta.activeTag(a) != std.meta.activeTag(b)) return false;
    return switch (a) {
        .def => |def| def == b.def,
        .expr => |expr| expr == b.expr,
        .statement => |statement| statement == b.statement,
        .required_binding => |required_binding| required_binding == b.required_binding,
        .hoisted => |hoisted| hoisted.index == b.hoisted.index and hoisted.expr == b.hoisted.expr,
    };
}

fn expectPatternExtractionSyntheticRegions(
    artifact: *const check.CheckedArtifact.CheckedModuleArtifact,
) HoistedConstantsTestError!void {
    const ModuleEnv = can.ModuleEnv;
    const module_env = artifact.moduleEnvConst();
    var extraction_count: usize = 0;

    for (artifact.compile_time_roots.roots) |root| {
        if (root.kind != .hoisted_constant) continue;
        const body = root.hoisted_body orelse continue;
        const extraction = switch (body) {
            .expr => continue,
            .pattern_extraction => |payload| payload,
        };
        extraction_count += 1;

        try std.testing.expectEqual(extraction.result_pattern, root.source_pattern orelse return error.PatternExtractionMissingSourcePattern);
        const checked_result_pattern = root.pattern orelse return error.PatternExtractionMissingCheckedRootPattern;

        const synthetic_match = artifact.checked_bodies.expr(root.expr);
        try expectRegionEqual(
            module_env.store.getNodeRegion(ModuleEnv.nodeIdxFrom(extraction.scrutinee_pattern)),
            synthetic_match.source_region,
        );

        const match_data = switch (synthetic_match.data) {
            .match_ => |match_| match_,
            .pending,
            .num,
            .frac_f32,
            .frac_f64,
            .dec,
            .dec_small,
            .num_from_numeral,
            .typed_int,
            .typed_frac,
            .typed_num_from_numeral,
            .str_from_quote,
            .str_segment,
            .str,
            .bytes_literal,
            .lookup_local,
            .lookup_external,
            .lookup_required,
            .list,
            .empty_list,
            .tuple,
            .if_,
            .call,
            .record,
            .empty_record,
            .block,
            .tag,
            .nominal,
            .zero_argument_tag,
            .closure,
            .lambda,
            .binop,
            .unary_minus,
            .unary_not,
            .field_access,
            .dispatch_call,
            .interpolation,
            .structural_eq,
            .structural_hash,
            .method_eq,
            .type_dispatch_call,
            .tuple_access,
            .runtime_error,
            .crash,
            .dbg,
            .expect_err,
            .expect,
            .ellipsis,
            .anno_only,
            .break_,
            .return_,
            .for_,
            .hosted_lambda,
            .run_low_level,
            => return error.PatternExtractionRootWasNotSyntheticMatch,
        };
        const checked_base_expr = artifact.checked_bodies.expr(match_data.cond);
        try expectRegionEqual(
            module_env.store.getNodeRegion(ModuleEnv.nodeIdxFrom(extraction.base_expr)),
            checked_base_expr.source_region,
        );
        try std.testing.expectEqual(@as(usize, 1), match_data.branches.len);
        try std.testing.expect(!match_data.is_try_suffix);
        try std.testing.expect(!match_data.skip_exhaustiveness);

        const branch = match_data.branches[0];
        const branch_patterns = branch.patternsSlice(&artifact.checked_bodies);
        try std.testing.expectEqual(@as(usize, 1), branch_patterns.len);
        const checked_scrutinee_pattern = artifact.checked_bodies.pattern(branch_patterns[0].pattern);
        try expectRegionEqual(
            module_env.store.getNodeRegion(ModuleEnv.nodeIdxFrom(extraction.scrutinee_pattern)),
            checked_scrutinee_pattern.source_region,
        );
        try std.testing.expect(!branch_patterns[0].degenerate);
        try std.testing.expectEqual(@as(usize, 0), branch_patterns[0].binderRemapsSlice(&artifact.checked_bodies).len);
        try std.testing.expectEqual(@as(?check.CheckedArtifact.CheckedExprId, null), branch.guard);

        const synthetic_lookup = artifact.checked_bodies.expr(branch.value);
        try expectRegionEqual(
            module_env.store.getNodeRegion(ModuleEnv.nodeIdxFrom(extraction.result_pattern)),
            synthetic_lookup.source_region,
        );
        const lookup = switch (synthetic_lookup.data) {
            .lookup_local => |lookup| lookup,
            .pending,
            .num,
            .frac_f32,
            .frac_f64,
            .dec,
            .dec_small,
            .num_from_numeral,
            .typed_int,
            .typed_frac,
            .typed_num_from_numeral,
            .str_from_quote,
            .str_segment,
            .str,
            .bytes_literal,
            .lookup_external,
            .lookup_required,
            .list,
            .empty_list,
            .tuple,
            .match_,
            .if_,
            .call,
            .record,
            .empty_record,
            .block,
            .tag,
            .nominal,
            .zero_argument_tag,
            .closure,
            .lambda,
            .binop,
            .unary_minus,
            .unary_not,
            .field_access,
            .dispatch_call,
            .interpolation,
            .structural_eq,
            .structural_hash,
            .method_eq,
            .type_dispatch_call,
            .tuple_access,
            .runtime_error,
            .crash,
            .dbg,
            .expect_err,
            .expect,
            .ellipsis,
            .anno_only,
            .break_,
            .return_,
            .for_,
            .hosted_lambda,
            .run_low_level,
            => return error.PatternExtractionRootValueWasNotSyntheticLookup,
        };
        try std.testing.expectEqual(checked_result_pattern, lookup.pattern);
        try std.testing.expect(lookup.resolved != null);
    }

    try std.testing.expect(extraction_count >= 4);
}

fn expectRegionEqual(expected: base.Region, actual: base.Region) HoistedConstantsTestError!void {
    try std.testing.expectEqual(expected.start.offset, actual.start.offset);
    try std.testing.expectEqual(expected.end.offset, actual.end.offset);
}

fn writeEchoPlatform(dir: anytype) HoistedConstantsTestError!void {
    try dir.createDirPath(std.testing.io, ".roc_echo_platform");
    try dir.writeFile(std.testing.io, .{
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
    try dir.writeFile(std.testing.io, .{
        .sub_path = ".roc_echo_platform/Echo.roc",
        .data =
        \\Echo := [].{
        \\    line! : Str => {}
        \\}
        ,
    });
}

fn expectReportDoesNotContain(
    allocator: std.mem.Allocator,
    report: *const @import("reporting").Report,
    needle: []const u8,
) HoistedConstantsTestError!void {
    var rendered = std.array_list.Managed(u8).init(allocator);
    defer rendered.deinit();

    var unmanaged = rendered.moveToUnmanaged();
    defer rendered = unmanaged.toManaged(allocator);

    var writer_alloc = std.Io.Writer.Allocating.fromArrayList(allocator, &unmanaged);
    defer unmanaged = writer_alloc.toArrayList();

    report.render(&writer_alloc.writer, .markdown) catch |err| switch (err) {
        error.OutOfMemory,
        error.WriteFailed,
        => return error.OutOfMemory,
    };

    try std.testing.expect(std.mem.find(u8, writer_alloc.written(), needle) == null);
}

fn findStoredCompileTimeRootI64(
    artifact: check.CheckedArtifact.ImportedModuleView,
    kind: check.CheckedArtifact.CompileTimeRootKind,
    expected: i64,
) ?check.CheckedArtifact.CompileTimeRoot {
    for (artifact.compile_time_roots.roots) |root| {
        if (root.kind != kind) continue;
        if (rootStoredI64(artifact, root)) |actual| {
            if (actual == expected) return root;
        } else |_| {}
    }
    return null;
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

fn countStoredHoistedI64(
    artifact: check.CheckedArtifact.ImportedModuleView,
    expected: i64,
) usize {
    var count: usize = 0;
    for (artifact.hoisted_constants.entries) |entry| {
        if (storedI64(artifact, entry)) |actual| {
            if (actual == expected) count += 1;
        } else |_| {}
    }
    return count;
}

fn countHoistedMatchRoots(
    artifact: *const check.CheckedArtifact.CheckedModuleArtifact,
) usize {
    var count: usize = 0;
    for (artifact.compile_time_roots.roots) |root| {
        if (root.kind != .hoisted_constant) continue;
        if (std.meta.activeTag(artifact.checked_bodies.expr(root.expr).data) == .match_) {
            count += 1;
        }
    }
    return count;
}

fn countCompileTimeRootKind(
    artifact: *const check.CheckedArtifact.CheckedModuleArtifact,
    kind: check.CheckedArtifact.CompileTimeRootKind,
) usize {
    var count: usize = 0;
    for (artifact.compile_time_roots.roots) |root| {
        if (root.kind == kind) count += 1;
    }
    return count;
}

fn storedI64(
    artifact: check.CheckedArtifact.ImportedModuleView,
    entry: check.CheckedArtifact.HoistedConstEntry,
) HoistedConstantsTestError!i64 {
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

    return scalarConstNodeI64(artifact, node_from_root);
}

fn rootStoredI64(
    artifact: check.CheckedArtifact.ImportedModuleView,
    root: check.CheckedArtifact.CompileTimeRoot,
) HoistedConstantsTestError!i64 {
    const node = switch (root.payload) {
        .const_node => |const_node| const_node,
        .pending,
        .fn_value,
        .expect,
        => return error.RootDidNotStoreConstNode,
    };
    return scalarConstNodeI64(artifact, node);
}

fn scalarConstNodeI64(
    artifact: check.CheckedArtifact.ImportedModuleView,
    node: check.CheckedArtifact.ConstNodeId,
) HoistedConstantsTestError!i64 {
    const value = artifact.const_store.get(node);
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

test "issue 9733: nested expect statements are collected as test roots" {
    // https://github.com/roc-lang/roc/issues/9733
    // The module has two `expect`s: the outer one and the one nested inside its
    // block body. Both must be collected as compile-time `expect` roots so that
    // `roc test` evaluates the nested `expect 3 == 4` (which must fail). Today
    // only the top-level expect is collected, so this count is 1 and `roc test`
    // wrongly reports "All (1) tests passed".
    const gpa = std.testing.allocator;

    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    try writeEchoPlatform(tmp_dir.dir);
    try tmp_dir.dir.writeFile(std.testing.io, .{
        .sub_path = "main.roc",
        .data =
        \\app [main!] { pf: platform "./.roc_echo_platform/main.roc" }
        \\
        \\import pf.Echo
        \\
        \\expect {
        \\    expect 3.I64 == 4.I64
        \\    5.I64 == 5.I64
        \\}
        \\
        \\main! = |_args| Ok({})
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

    const app_artifact = coord.rootCheckedArtifact("app");

    try std.testing.expectEqual(
        @as(usize, 2),
        countCompileTimeRootKind(app_artifact, .expect),
    );
}
