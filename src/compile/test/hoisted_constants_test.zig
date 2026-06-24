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

const static_data_exports = @import("../static_data_exports.zig");
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
        \\top_a = 40.I64
        \\top_b = top_a + 1.I64
        \\
        \\top_callable : I64 -> I64
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
    try std.testing.expect(!coord.hasUserErrors());

    try coord.finalizeExecutableArtifacts();
    try std.testing.expect(!coord.hasUserErrors());

    const root = coord.executableRootCheckedArtifact();
    const app_artifact = coord.rootCheckedArtifact("app");
    const app_view = check.CheckedArtifact.importedView(app_artifact);
    const imports = try coord.collectImportedArtifactViews(arena, root);
    const relations = try coord.collectRelationArtifactViews(arena, root);
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
        \\module [helper, base]
        \\
        \\base = 41.I64
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

test "reachable top-level data lowers to internal static data exports" {
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
        \\top_bytes : List(U8)
        \\top_bytes = [17, 34, 51, 68, 85, 102]
        \\
        \\top_record = { data: top_bytes, width: 3.U32, height: 2.U32 }
        \\other_record = { data: top_bytes, width: 6.U32, height: 1.U32 }
        \\
        \\unused_bytes : List(U8)
        \\unused_bytes = [201, 202, 203, 204, 205, 206]
        \\
        \\main! = |args| {
        \\    index = List.len(args) % 6
        \\    reachable = match List.get(top_record.data, index) {
        \\        Ok(byte) => byte.to_i64()
        \\        Err(_) => 0
        \\    }
        \\    other_index = (index + 1) % 6
        \\    other = match List.get(other_record.data, other_index) {
        \\        Ok(byte) => byte.to_i64()
        \\        Err(_) => 0
        \\    }
        \\    _ = reachable + other
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
    try std.testing.expect(!coord.hasUserErrors());

    try coord.finalizeExecutableArtifacts();
    try std.testing.expect(!coord.hasUserErrors());

    const root = coord.executableRootCheckedArtifact();
    const app_artifact = coord.rootCheckedArtifact("app");
    const app_view = check.CheckedArtifact.importedView(app_artifact);
    const imports = try coord.collectImportedArtifactViews(arena, root);
    const relations = try coord.collectRelationArtifactViews(arena, root);
    const root_view = check.CheckedArtifact.loweringViewWithRelations(root, relations);

    try std.testing.expect(countStoredHoistedListLength(app_view, 6) >= 1);
    try std.testing.expectEqual(@as(usize, 1), countStoredTopLevelListU8(app_view, &.{ 17, 34, 51, 68, 85, 102 }));
    try std.testing.expectEqual(@as(usize, 1), countStoredTopLevelListU8(app_view, &.{ 201, 202, 203, 204, 205, 206 }));

    const lir_roots = try lir.CheckedPipeline.selectPlatformEntrypointRoots(gpa, root.root_requests.runtime_requests);
    defer gpa.free(lir_roots);

    var lowered = try lir.CheckedPipeline.lowerCheckedModulesToLir(
        gpa,
        .{
            .root = root_view,
            .imports = imports,
        },
        .{ .requests = lir_roots, .include_static_data_exports = true },
        .{ .target_usize = base.target.TargetUsize.native },
    );
    defer lowered.deinit();

    try std.testing.expect(lowered.lir_result.static_data_values.items.len >= 1);
    try std.testing.expect(countStaticDataLiteralAssignments(&lowered.lir_result.store) >= 1);
    try std.testing.expectEqual(@as(usize, 0), countStaticDataValuesContainingListU8(root_view, imports, &lowered, &.{ 201, 202, 203, 204, 205, 206 }));
    try std.testing.expectEqual(@as(usize, 0), countStaticDataLiteralAssignmentsToListU8(root_view, imports, &lowered, &.{ 201, 202, 203, 204, 205, 206 }));

    const exports = try static_data_exports.buildProvidedDataExports(
        gpa,
        .{
            .root = root_view,
            .imports = imports,
        },
        &lowered,
        roc_target.RocTarget.detectNative(),
    );
    defer static_data_exports.deinitProvidedDataExports(gpa, exports);

    try std.testing.expect(countInternalStaticValueExports(exports) >= 1);
    try expectInternalStaticValueExportsAreLinkableOnly(exports);
    const shared_payload = findExportContainingSequence(exports, &.{ 17, 34, 51, 68, 85, 102 }) orelse return error.SharedStaticPayloadNotFound;
    try std.testing.expectEqual(@as(usize, 1), countExportsContainingSequence(exports, &.{ 17, 34, 51, 68, 85, 102 }));
    try std.testing.expect(countInternalStaticValueRelocationsTo(exports, shared_payload.symbol_name) >= 2);
    try std.testing.expect(!exportsContainSequence(exports, &.{ 201, 202, 203, 204, 205, 206 }));
}

test "effectful parent still emits independent static child data" {
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
        \\tick! : {} => I64
        \\tick! = |_| 7
        \\
        \\main! = |args| {
        \\    index = List.len(args) % 4
        \\    record = { data: [17.U8, 34.U8, 51.U8, 68.U8], tick: tick!({}) }
        \\    byte_value = match List.get(record.data, index) {
        \\        Ok(byte) => byte.to_i64()
        \\        Err(_) => 0
        \\    }
        \\    _ = byte_value + record.tick
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
    try std.testing.expect(!coord.hasUserErrors());

    try coord.finalizeExecutableArtifacts();
    try std.testing.expect(!coord.hasUserErrors());

    const root = coord.executableRootCheckedArtifact();
    const imports = try coord.collectImportedArtifactViews(arena, root);
    const relations = try coord.collectRelationArtifactViews(arena, root);
    const root_view = check.CheckedArtifact.loweringViewWithRelations(root, relations);

    const lir_roots = try lir.CheckedPipeline.selectPlatformEntrypointRoots(gpa, root.root_requests.runtime_requests);
    defer gpa.free(lir_roots);

    var lowered = try lir.CheckedPipeline.lowerCheckedModulesToLir(
        gpa,
        .{
            .root = root_view,
            .imports = imports,
        },
        .{ .requests = lir_roots, .include_static_data_exports = true },
        .{ .target_usize = base.target.TargetUsize.native },
    );
    defer lowered.deinit();

    const exports = try static_data_exports.buildProvidedDataExports(
        gpa,
        .{
            .root = root_view,
            .imports = imports,
        },
        &lowered,
        roc_target.RocTarget.detectNative(),
    );
    defer static_data_exports.deinitProvidedDataExports(gpa, exports);

    const shared_payload = findExportContainingSequence(exports, &.{ 17, 34, 51, 68 }) orelse return error.StaticChildPayloadNotFound;
    try std.testing.expectEqual(@as(usize, 1), countExportsContainingSequence(exports, &.{ 17, 34, 51, 68 }));
    try std.testing.expect(countStaticDataRelocationsTo(exports, shared_payload.symbol_name) >= 1);
}

test "parent static root does not emit removed child root data" {
    const gpa = std.testing.allocator;

    const exports = try buildStaticDataExportsForAppSource(gpa,
        \\app [main!] { pf: platform "./.roc_echo_platform/main.roc" }
        \\
        \\import pf.Echo
        \\
        \\main! = |args| {
        \\    index = List.len(args) % 4
        \\    record = { data: [17.U8, 34.U8, 51.U8, 68.U8], width: 4.U64 }
        \\    byte_value = match List.get(record.data, index) {
        \\        Ok(byte) => byte.to_i64()
        \\        Err(_) => 0
        \\    }
        \\    _ = byte_value + record.width.to_i64_wrap()
        \\    Echo.line!("done")
        \\    Ok({})
        \\}
        \\
    );
    defer static_data_exports.deinitProvidedDataExports(gpa, exports);

    const shared_payload = findExportContainingSequence(exports, &.{ 17, 34, 51, 68 }) orelse return error.StaticPayloadNotFound;
    try std.testing.expectEqual(@as(usize, 1), countExportsContainingSequence(exports, &.{ 17, 34, 51, 68 }));
    try std.testing.expectEqual(@as(usize, 1), countInternalStaticValueExports(exports));
    try std.testing.expectEqual(@as(usize, 1), countInternalStaticValueRelocationsTo(exports, shared_payload.symbol_name));
}

test "nominal record backed by static data emits through checked output" {
    const gpa = std.testing.allocator;

    const exports = try buildStaticDataExportsForAppSource(gpa,
        \\app [main!] { pf: platform "./.roc_echo_platform/main.roc" }
        \\
        \\import pf.Echo
        \\
        \\Token := { raw : List(U8), width : U64 }.{
        \\    bytes : Token -> List(U8)
        \\    bytes = |token| token.raw
        \\
        \\    width : Token -> U64
        \\    width = |token| token.width
        \\}
        \\
        \\token : Token
        \\token = Token.{ raw: [17.U8, 34.U8, 51.U8, 68.U8], width: 4 }
        \\
        \\main! = |args| {
        \\    index = List.len(args) % Token.width(token)
        \\    byte_value = match List.get(Token.bytes(token), index) {
        \\        Ok(byte) => byte.to_i64()
        \\        Err(_) => 0
        \\    }
        \\    _ = byte_value
        \\    Echo.line!("done")
        \\    Ok({})
        \\}
        \\
    );
    defer static_data_exports.deinitProvidedDataExports(gpa, exports);

    const shared_payload = findExportContainingSequence(exports, &.{ 17, 34, 51, 68 }) orelse return error.StaticNominalPayloadNotFound;
    try std.testing.expectEqual(@as(usize, 1), countExportsContainingSequence(exports, &.{ 17, 34, 51, 68 }));
    try std.testing.expectEqual(@as(usize, 1), countInternalStaticValueExports(exports));
    try std.testing.expectEqual(@as(usize, 1), countInternalStaticValueRelocationsTo(exports, shared_payload.symbol_name));
}

test "tuple and tag static data share named and inline list payloads" {
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
        \\top_bytes : List(U8)
        \\top_bytes = [17, 34, 51, 68]
        \\
        \\top_scalar = 123.I64
        \\top_empty : List(U8)
        \\top_empty = []
        \\
        \\named_tuple = (top_bytes, 4.U32)
        \\named_tag = Frame(top_bytes, 4.U32)
        \\
        \\unused_tuple = ([201.U8, 202.U8, 203.U8, 204.U8], 4.U32)
        \\unused_tag = Frame([211.U8, 212.U8, 213.U8, 214.U8], 4.U32)
        \\
        \\read_tuple = |pair, index| {
        \\    (data, _) = pair
        \\    match List.get(data, index) {
        \\        Ok(byte) => byte.to_i64()
        \\        Err(_) => 0
        \\    }
        \\}
        \\
        \\read_tag = |frame, index| {
        \\    match frame {
        \\        Frame(data, _) => match List.get(data, index) {
        \\            Ok(byte) => byte.to_i64()
        \\            Err(_) => 0
        \\        }
        \\    }
        \\}
        \\
        \\main! = |args| {
        \\    index = List.len(args) % 4
        \\    named_tuple_value = read_tuple(named_tuple, index)
        \\    inline_tuple_value = read_tuple(([17.U8, 34.U8, 51.U8, 68.U8], 4.U32), index)
        \\    named_tag_value = read_tag(named_tag, index)
        \\    inline_tag_value = read_tag(Frame([17.U8, 34.U8, 51.U8, 68.U8], 4.U32), index)
        \\    scalar_value = top_scalar + List.len(top_empty).to_i64_wrap()
        \\    _ = named_tuple_value + inline_tuple_value + named_tag_value + inline_tag_value + scalar_value
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
    try std.testing.expect(!coord.hasUserErrors());

    try coord.finalizeExecutableArtifacts();
    try std.testing.expect(!coord.hasUserErrors());

    const root = coord.executableRootCheckedArtifact();
    const imports = try coord.collectImportedArtifactViews(arena, root);
    const relations = try coord.collectRelationArtifactViews(arena, root);
    const root_view = check.CheckedArtifact.loweringViewWithRelations(root, relations);

    const lir_roots = try lir.CheckedPipeline.selectPlatformEntrypointRoots(gpa, root.root_requests.runtime_requests);
    defer gpa.free(lir_roots);

    var lowered = try lir.CheckedPipeline.lowerCheckedModulesToLir(
        gpa,
        .{
            .root = root_view,
            .imports = imports,
        },
        .{ .requests = lir_roots, .include_static_data_exports = true },
        .{ .target_usize = base.target.TargetUsize.native },
    );
    defer lowered.deinit();

    try std.testing.expect(countStaticDataLiteralAssignments(&lowered.lir_result.store) >= 4);

    const exports = try static_data_exports.buildProvidedDataExports(
        gpa,
        .{
            .root = root_view,
            .imports = imports,
        },
        &lowered,
        roc_target.RocTarget.detectNative(),
    );
    defer static_data_exports.deinitProvidedDataExports(gpa, exports);

    const shared_payload = findExportContainingSequence(exports, &.{ 17, 34, 51, 68 }) orelse return error.SharedStaticPayloadNotFound;
    try std.testing.expectEqual(@as(usize, 1), countExportsContainingSequence(exports, &.{ 17, 34, 51, 68 }));
    try std.testing.expect(countInternalStaticValueRelocationsTo(exports, shared_payload.symbol_name) >= 4);
    try expectAllInternalStaticValueExportsRelocateTo(exports, shared_payload.symbol_name);
    try std.testing.expect(!exportsContainSequence(exports, &.{ 201, 202, 203, 204 }));
    try std.testing.expect(!exportsContainSequence(exports, &.{ 211, 212, 213, 214 }));
}

test "reachable function-valued constant restores without static data literal" {
    const gpa = std.testing.allocator;

    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    try writeEchoPlatform(tmp_dir.dir);
    try tmp_dir.dir.writeFile(std.testing.io, .{
        .sub_path = "main.roc",
        .data =
        \\app [main!] { pf: platform "./.roc_echo_platform/main.roc" }
        \\
        \\add_one = |n| n + 1.I64
        \\add_two = |n| n + 2.I64
        \\
        \\chosen = if 1.I64 == 1.I64 {
        \\    add_one
        \\} else {
        \\    add_two
        \\}
        \\
        \\main! = |args| {
        \\    value = chosen(List.len(args).to_i64_wrap())
        \\    _ = value
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

    try coord.finalizeExecutableArtifacts();
    try std.testing.expect(!coord.hasUserErrors());

    const root = coord.executableRootCheckedArtifact();
    const imports = try coord.collectImportedArtifactViews(arena, root);
    const relations = try coord.collectRelationArtifactViews(arena, root);
    const root_view = check.CheckedArtifact.loweringViewWithRelations(root, relations);

    const lir_roots = try lir.CheckedPipeline.selectPlatformEntrypointRoots(gpa, root.root_requests.runtime_requests);
    defer gpa.free(lir_roots);

    var lowered = try lir.CheckedPipeline.lowerCheckedModulesToLir(
        gpa,
        .{
            .root = root_view,
            .imports = imports,
        },
        .{ .requests = lir_roots, .include_static_data_exports = true },
        .{ .target_usize = base.target.TargetUsize.native },
    );
    defer lowered.deinit();

    try std.testing.expectEqual(@as(usize, 0), lowered.lir_result.static_data_values.items.len);
    try std.testing.expectEqual(@as(usize, 0), countStaticDataLiteralAssignments(&lowered.lir_result.store));
}

test "provided boxed erased callable captures static list data" {
    const gpa = std.testing.allocator;

    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    try tmp_dir.dir.createDirPath(std.testing.io, ".roc_box_platform");
    try tmp_dir.dir.writeFile(std.testing.io, .{
        .sub_path = "main.roc",
        .data =
        \\app [main!, boxed] { pf: platform "./.roc_box_platform/main.roc" }
        \\
        \\main! = || {}
        \\
        \\boxed : Box((I64 -> I64))
        \\boxed = {
        \\    base = [1.I64, 2.I64]
        \\    Box.box(|value|
        \\        match List.get(base, 0) {
        \\            Ok(first) => value + first
        \\            Err(_) => value
        \\        }
        \\    )
        \\}
        ,
    });
    try tmp_dir.dir.writeFile(std.testing.io, .{
        .sub_path = ".roc_box_platform/main.roc",
        .data =
        \\platform ""
        \\    requires {} { main! : () => {}, boxed : Box((I64 -> I64)) }
        \\    exposes []
        \\    packages {}
        \\    provides { "roc_main": main_for_host!, "roc_boxed": boxed_for_host }
        \\
        \\main_for_host! : () => {}
        \\main_for_host! = main!
        \\
        \\boxed_for_host : Box((I64 -> I64))
        \\boxed_for_host = boxed
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
    const root_view = check.CheckedArtifact.loweringViewWithRelations(root, relations);

    const lir_roots = try lir.CheckedPipeline.selectPlatformEntrypointRoots(gpa, root.root_requests.runtime_requests);
    defer gpa.free(lir_roots);

    var lowered = try lir.CheckedPipeline.lowerCheckedModulesToLir(
        gpa,
        .{
            .root = root_view,
            .imports = imports,
        },
        .{ .requests = lir_roots, .include_static_data_exports = true },
        .{ .target_usize = base.target.TargetUsize.native },
    );
    defer lowered.deinit();

    const exports = try static_data_exports.buildProvidedDataExports(
        gpa,
        .{
            .root = root_view,
            .imports = imports,
        },
        &lowered,
        roc_target.RocTarget.detectNative(),
    );
    defer static_data_exports.deinitProvidedDataExports(gpa, exports);

    const captured_list_payload = [_]u8{
        1, 0, 0, 0, 0, 0, 0, 0,
        2, 0, 0, 0, 0, 0, 0, 0,
    };
    const shared_payload = findExportContainingSequence(exports, &captured_list_payload) orelse return error.CapturedListPayloadNotFound;
    try std.testing.expectEqual(@as(usize, 1), countExportsContainingSequence(exports, &captured_list_payload));
    try std.testing.expect(countStaticDataRelocationsTo(exports, shared_payload.symbol_name) >= 1);
    _ = findExportWithFunctionPointerAndRelocationTo(exports, shared_payload.symbol_name) orelse return error.ErasedCallableAllocationNotFound;
    try std.testing.expect(countFunctionPointerRelocations(exports) >= 1);
}

test "reachable local List.iter hoist stores iterator const data" {
    const gpa = std.testing.allocator;

    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    try writeEchoPlatform(tmp_dir.dir);
    try tmp_dir.dir.writeFile(std.testing.io, .{
        .sub_path = "main.roc",
        .data =
        \\app [main!] { pf: platform "./.roc_echo_platform/main.roc" }
        \\
        \\main! = |args| {
        \\    base = [{ x: 1.I64, y: 2.I64 }, { x: 3.I64, y: 4.I64 }].iter()
        \\    total = Iter.fold(base, 0.I64, |acc, point| acc + point.x + point.y + List.len(args).to_i64_wrap())
        \\    Err(Exit(total.to_i8_wrap()))
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
    const root_view = check.CheckedArtifact.loweringViewWithRelations(root, relations);

    const root_const_view = check.CheckedArtifact.importedView(root_view.module);
    const stored_list_count =
        countStoredHoistedListLength(app_view, 2) +
        countStoredHoistedListLength(root_const_view, 2) +
        countStoredHoistedListLengthInModules(root_view.relation_modules, 2) +
        countStoredHoistedListLengthInModules(imports, 2);
    const stored_fn_count =
        countStoredHoistedFnValue(app_view) +
        countStoredHoistedFnValue(root_const_view) +
        countStoredHoistedFnValueInModules(root_view.relation_modules) +
        countStoredHoistedFnValueInModules(imports);
    const stored_fn_context_count =
        countStoredHoistedFnContext(app_view) +
        countStoredHoistedFnContext(root_const_view) +
        countStoredHoistedFnContextInModules(root_view.relation_modules) +
        countStoredHoistedFnContextInModules(imports);
    const stored_iter_root_count =
        countStoredHoistedListLengthAndFnValue(app_view, 2) +
        countStoredHoistedListLengthAndFnValue(root_const_view, 2) +
        countStoredHoistedListLengthAndFnValueInModules(root_view.relation_modules, 2) +
        countStoredHoistedListLengthAndFnValueInModules(imports, 2);
    try std.testing.expect(stored_list_count >= 1);
    try std.testing.expect(stored_fn_count >= 1);
    try std.testing.expect(stored_fn_context_count >= 1);
    try std.testing.expect(stored_iter_root_count >= 1);
    try std.testing.expect(countSelectedHoistedConstResolvedRefs(app_view) >= 1);
    try std.testing.expect(countSelectedHoistedConstResolvedRefsWithListAndFn(app_view, 2) >= 1);

    const lir_roots = try lir.CheckedPipeline.selectPlatformEntrypointRoots(gpa, root.root_requests.runtime_requests);
    defer gpa.free(lir_roots);

    var lowered = try lir.CheckedPipeline.lowerCheckedModulesToLir(
        gpa,
        .{
            .root = root_view,
            .imports = imports,
        },
        .{ .requests = lir_roots, .include_static_data_exports = true },
        .{ .target_usize = base.target.TargetUsize.native },
    );
    defer lowered.deinit();

    try std.testing.expect(countStaticDataLiteralAssignmentsToListLength(root_view, imports, &lowered, 2) >= 1);

    const exports = try static_data_exports.buildProvidedDataExports(
        gpa,
        .{
            .root = root_view,
            .imports = imports,
        },
        &lowered,
        roc_target.RocTarget.detectNative(),
    );
    defer static_data_exports.deinitProvidedDataExports(gpa, exports);

    const point_payload_bytes = [_]u8{
        1, 0, 0, 0, 0, 0, 0, 0,
        2, 0, 0, 0, 0, 0, 0, 0,
        3, 0, 0, 0, 0, 0, 0, 0,
        4, 0, 0, 0, 0, 0, 0, 0,
    };
    _ = findExportContainingSequence(exports, &point_payload_bytes) orelse return error.PointListStaticPayloadNotFound;
    try std.testing.expectEqual(@as(usize, 1), countExportsContainingSequence(exports, &point_payload_bytes));
}

test "inline sub_or_crash cells inside runtime record become shared static data" {
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
        \\Sprite : {
        \\    data : List(U8),
        \\    region : { src_x : U64, src_y : U64, width : U64, height : U64 },
        \\}
        \\
        \\Cell : { frames : U64, sprite : Sprite }
        \\
        \\sheet : Sprite
        \\sheet = {
        \\    data: [17.U8, 34.U8, 51.U8, 68.U8],
        \\    region: { src_x: 0, src_y: 0, width: 2, height: 2 },
        \\}
        \\
        \\other_sheet : Sprite
        \\other_sheet = {
        \\    data: [17.U8, 34.U8, 51.U8, 68.U8],
        \\    region: { src_x: 0, src_y: 0, width: 2, height: 2 },
        \\}
        \\
        \\sub : Sprite, { src_x : U64, src_y : U64, width : U64, height : U64 } -> Try(Sprite, {})
        \\sub = |sprite, region| Ok({ ..sprite, region })
        \\
        \\sub_or_crash : Sprite, { src_x : U64, src_y : U64, width : U64, height : U64 } -> Sprite
        \\sub_or_crash = |sprite, region|
        \\    match sub(sprite, region) {
        \\        Ok(sub_sprite) => sub_sprite
        \\        Err(_) => {
        \\            crash "bad sprite"
        \\        }
        \\    }
        \\
        \\make_anim = |frame_count| {
        \\    last_updated: frame_count,
        \\    cells: [
        \\        { frames: 5.U64, sprite: sub_or_crash(sheet, { src_x: 0, src_y: 0, width: 1, height: 1 }) },
        \\        { frames: 6.U64, sprite: sub_or_crash(sheet, { src_x: 1, src_y: 0, width: 1, height: 1 }) },
        \\        { frames: 7.U64, sprite: sub_or_crash(other_sheet, { src_x: 0, src_y: 1, width: 1, height: 1 }) },
        \\    ],
        \\}
        \\
        \\main! = |args| {
        \\    anim = make_anim(List.len(args))
        \\    first_x = match List.get(anim.cells, 0) {
        \\        Ok(cell) => cell.sprite.region.src_x.to_i64_wrap()
        \\        Err(_) => 0
        \\    }
        \\    _ = first_x + anim.last_updated.to_i64_wrap()
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
    try std.testing.expect(!coord.hasUserErrors());

    try coord.finalizeExecutableArtifacts();
    try std.testing.expect(!coord.hasUserErrors());

    const root = coord.executableRootCheckedArtifact();
    const imports = try coord.collectImportedArtifactViews(arena, root);
    const relations = try coord.collectRelationArtifactViews(arena, root);
    const root_view = check.CheckedArtifact.loweringViewWithRelations(root, relations);

    const lir_roots = try lir.CheckedPipeline.selectPlatformEntrypointRoots(gpa, root.root_requests.runtime_requests);
    defer gpa.free(lir_roots);

    var lowered = try lir.CheckedPipeline.lowerCheckedModulesToLir(
        gpa,
        .{
            .root = root_view,
            .imports = imports,
        },
        .{ .requests = lir_roots, .include_static_data_exports = true },
        .{ .target_usize = base.target.TargetUsize.native },
    );
    defer lowered.deinit();

    const exports = try static_data_exports.buildProvidedDataExports(
        gpa,
        .{
            .root = root_view,
            .imports = imports,
        },
        &lowered,
        roc_target.RocTarget.detectNative(),
    );
    defer static_data_exports.deinitProvidedDataExports(gpa, exports);

    const shared_payload = findExportContainingSequence(exports, &.{ 17, 34, 51, 68 }) orelse return error.SharedStaticPayloadNotFound;
    try std.testing.expectEqual(@as(usize, 1), countExportsContainingSequence(exports, &.{ 17, 34, 51, 68 }));
    try std.testing.expect(countInternalStaticValueExports(exports) >= 1);
    try std.testing.expect(countStaticDataRelocationsTo(exports, shared_payload.symbol_name) >= 3);
}

test "named and inline animation cells emit equivalent static data" {
    const gpa = std.testing.allocator;

    const common_prefix =
        \\app [main!] { pf: platform "./.roc_echo_platform/main.roc" }
        \\
        \\import pf.Echo
        \\
        \\Sprite : {
        \\    data : List(U8),
        \\    region : { src_x : U64, src_y : U64, width : U64, height : U64 },
        \\}
        \\
        \\Cell : { frames : U64, sprite : Sprite }
        \\
        \\sheet : Sprite
        \\sheet = {
        \\    data: [17.U8, 34.U8, 51.U8, 68.U8],
        \\    region: { src_x: 0, src_y: 0, width: 2, height: 2 },
        \\}
        \\
        \\sub : Sprite, { src_x : U64, src_y : U64, width : U64, height : U64 } -> Try(Sprite, {})
        \\sub = |sprite, region| Ok({ ..sprite, region })
        \\
        \\sub_or_crash : Sprite, { src_x : U64, src_y : U64, width : U64, height : U64 } -> Sprite
        \\sub_or_crash = |sprite, region|
        \\    match sub(sprite, region) {
        \\        Ok(sub_sprite) => sub_sprite
        \\        Err(_) => {
        \\            crash "bad sprite"
        \\        }
        \\    }
        \\
    ;

    const named_suffix =
        \\named_cells : List(Cell)
        \\named_cells = [
        \\    { frames: 5.U64, sprite: sub_or_crash(sheet, { src_x: 0, src_y: 0, width: 1, height: 1 }) },
        \\    { frames: 6.U64, sprite: sub_or_crash(sheet, { src_x: 1, src_y: 0, width: 1, height: 1 }) },
        \\]
        \\
        \\make_anim = |frame_count| {
        \\    last_updated: frame_count,
        \\    cells: named_cells,
        \\}
        \\
        \\main! = |args| {
        \\    anim = make_anim(List.len(args))
        \\    _ = match List.get(anim.cells, 0) {
        \\        Ok(cell) => cell.sprite.region.src_x.to_i64_wrap()
        \\        Err(_) => 0
        \\    }
        \\    Echo.line!("done")
        \\    Ok({})
        \\}
        \\
    ;
    const inline_suffix =
        \\make_anim = |frame_count| {
        \\    last_updated: frame_count,
        \\    cells: [
        \\        { frames: 5.U64, sprite: sub_or_crash(sheet, { src_x: 0, src_y: 0, width: 1, height: 1 }) },
        \\        { frames: 6.U64, sprite: sub_or_crash(sheet, { src_x: 1, src_y: 0, width: 1, height: 1 }) },
        \\    ],
        \\}
        \\
        \\main! = |args| {
        \\    anim = make_anim(List.len(args))
        \\    _ = match List.get(anim.cells, 0) {
        \\        Ok(cell) => cell.sprite.region.src_x.to_i64_wrap()
        \\        Err(_) => 0
        \\    }
        \\    Echo.line!("done")
        \\    Ok({})
        \\}
        \\
    ;

    const named_source = try std.mem.concat(gpa, u8, &.{ common_prefix, named_suffix });
    defer gpa.free(named_source);
    const inline_source = try std.mem.concat(gpa, u8, &.{ common_prefix, inline_suffix });
    defer gpa.free(inline_source);

    const named_exports = try buildStaticDataExportsForAppSource(gpa, named_source);
    defer static_data_exports.deinitProvidedDataExports(gpa, named_exports);
    const inline_exports = try buildStaticDataExportsForAppSource(gpa, inline_source);
    defer static_data_exports.deinitProvidedDataExports(gpa, inline_exports);

    try expectStaticDataExportsEquivalent(gpa, named_exports, inline_exports);
}

test "inline imported opaque cells through boxed model become static data" {
    const gpa = std.testing.allocator;

    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    try writeBoxedSpritePlatform(tmp_dir.dir);
    try tmp_dir.dir.writeFile(std.testing.io, .{
        .sub_path = "main.roc",
        .data =
        \\app [main] { pf: platform "./.roc_echo_platform/main.roc" }
        \\
        \\import pf.Sprite
        \\
        \\Model : [Running({ frame_count : U64, anim : Animation })]
        \\
        \\main = { init!, update! }
        \\
        \\init! : () => Model
        \\init! = || Running({ frame_count: 0, anim: make_anim(0) })
        \\
        \\update! : Model => Model
        \\update! = |model|
        \\    match model {
        \\        Running(state) =>
        \\            Running({
        \\                frame_count: state.frame_count + 1,
        \\                anim: make_anim(state.frame_count),
        \\            })
        \\    }
        \\
        \\AnimationState : [Completed, RunOnce, Loop]
        \\
        \\Animation : {
        \\    last_updated : U64,
        \\    index : U64,
        \\    cells : List(Cell),
        \\    state : AnimationState,
        \\}
        \\
        \\Cell : { frames : U64, sprite : Sprite }
        \\
        \\sheet : Sprite
        \\sheet =
        \\    Sprite.new({
        \\        data: [17.U8, 34.U8, 51.U8, 68.U8],
        \\        bpp: BPP2,
        \\        width: 2,
        \\        height: 2,
        \\    })
        \\
        \\make_anim : U64 -> Animation
        \\make_anim = |frame_count| {
        \\    last_updated: frame_count,
        \\    index: 0,
        \\    state: Loop,
        \\    cells: [
        \\        { frames: 5.U64, sprite: Sprite.sub_or_crash(sheet, { src_x: 0, src_y: 0, width: 1, height: 1 }) },
        \\        { frames: 6.U64, sprite: Sprite.sub_or_crash(sheet, { src_x: 1, src_y: 0, width: 1, height: 1 }) },
        \\    ],
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
    const root_view = check.CheckedArtifact.loweringViewWithRelations(root, relations);
    const root_list_count = countStoredHoistedListLength(check.CheckedArtifact.importedView(root_view.module), 2);
    const relation_list_count = countStoredHoistedListLengthInModules(root_view.relation_modules, 2);
    const import_list_count = countStoredHoistedListLengthInModules(imports, 2);
    try std.testing.expect(root_list_count >= 1 or relation_list_count >= 1 or import_list_count >= 1);

    const lir_roots = try lir.CheckedPipeline.selectPlatformEntrypointRoots(gpa, root.root_requests.runtime_requests);
    defer gpa.free(lir_roots);

    var lowered = try lir.CheckedPipeline.lowerCheckedModulesToLir(
        gpa,
        .{
            .root = root_view,
            .imports = imports,
        },
        .{ .requests = lir_roots, .include_static_data_exports = true },
        .{ .target_usize = base.target.TargetUsize.native },
    );
    defer lowered.deinit();

    try std.testing.expect(countStaticDataLiteralAssignmentsToListLength(root_view, imports, &lowered, 2) >= 1);
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
        if (!std.mem.eql(u8, entry.report.title, "COMPTIME CRASH")) continue;
        found = true;
        const region = entry.report.getRegionInfo() orelse return error.ComptimeCrashReportHadNoRegion;
        try std.testing.expectEqual(@as(u32, 7), region.start_line_idx);
        try std.testing.expectEqual(@as(u32, 7), region.end_line_idx);
        try std.testing.expectEqualStrings("main", entry.module_name);
    }
    try std.testing.expect(found);
}

test "unreachable top-level dbg reports during compile-time evaluation" {
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
        \\unused = {
        \\    dbg "unreachable top-level"
        \\    41.I64
        \\}
        \\
        \\main! = |_args| {
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
    try std.testing.expect(!coord.hasUserErrors());

    var found = false;
    var report_iter = coord.iterReports();
    while (report_iter.next()) |entry| {
        if (!std.mem.eql(u8, entry.report.title, "COMPTIME DBG")) continue;
        found = true;
        try std.testing.expectEqual(@import("reporting").Severity.info, entry.report.severity);
        try std.testing.expectEqualStrings("main", entry.module_name);
        try expectReportContains(gpa, entry.report, "\"unreachable top-level\"");
    }
    try std.testing.expect(found);
}

test "unreachable imported top-level dbg reports during compile-time evaluation" {
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
        \\import Helper
        \\
        \\main! = |_args| {
        \\    _ = Helper.value
        \\    Echo.line!("done")
        \\    Ok({})
        \\}
        ,
    });
    try tmp_dir.dir.writeFile(std.testing.io, .{
        .sub_path = "Helper.roc",
        .data =
        \\module [value]
        \\
        \\unused = {
        \\    dbg "unreachable imported top-level"
        \\    41.I64
        \\}
        \\
        \\value = 1.I64
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

    var found = false;
    var report_iter = coord.iterReports();
    while (report_iter.next()) |entry| {
        if (!std.mem.eql(u8, entry.report.title, "COMPTIME DBG")) continue;
        found = true;
        try std.testing.expectEqual(@import("reporting").Severity.info, entry.report.severity);
        try std.testing.expectEqualStrings("Helper", entry.module_name);
        try expectReportContains(gpa, entry.report, "\"unreachable imported top-level\"");
    }
    try std.testing.expect(found);
}

test "top-level expect failure reports during compile-time evaluation" {
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
        \\expect 1.I64 == 2.I64
        \\
        \\main! = |_args| {
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
        if (!std.mem.eql(u8, entry.report.title, "COMPTIME EXPECT FAILED")) continue;
        found = true;
        try std.testing.expectEqualStrings("main", entry.module_name);
    }
    try std.testing.expect(found);
}

test "unreachable top-level crash reports during compile-time evaluation" {
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
        \\unused = {
        \\    crash "unreachable top-level crash"
        \\}
        \\
        \\main! = |_args| {
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
        if (!std.mem.eql(u8, entry.report.title, "COMPTIME CRASH")) continue;
        found = true;
        try std.testing.expectEqualStrings("main", entry.module_name);
        try expectReportContains(gpa, entry.report, "unreachable top-level crash");
    }
    try std.testing.expect(found);
}

test "unreachable top-level expect failure reports during compile-time evaluation" {
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
        \\unused = {
        \\    expect 1.I64 == 2.I64
        \\    41.I64
        \\}
        \\
        \\main! = |_args| {
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
        if (!std.mem.eql(u8, entry.report.title, "COMPTIME EXPECT FAILED")) continue;
        found = true;
        try std.testing.expectEqualStrings("main", entry.module_name);
    }
    try std.testing.expect(found);
}

test "reachable selected-root dbg reports during compile-time evaluation" {
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
        \\    value = {
        \\        dbg "reachable selected root"
        \\        41.I64
        \\    }
        \\    _ = value + List.len(args).to_i64_wrap()
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
    try std.testing.expect(!coord.hasUserErrors());

    var found = false;
    var report_iter = coord.iterReports();
    while (report_iter.next()) |entry| {
        if (!std.mem.eql(u8, entry.report.title, "COMPTIME DBG")) continue;
        found = true;
        try std.testing.expectEqual(@import("reporting").Severity.info, entry.report.severity);
        try std.testing.expectEqualStrings("main", entry.module_name);
        try expectReportContains(gpa, entry.report, "\"reachable selected root\"");
    }
    try std.testing.expect(found);
}

test "reachable selected-root expect failure reports during compile-time evaluation" {
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
        \\    value = {
        \\        expect 1.I64 == 2.I64
        \\        41.I64
        \\    }
        \\    _ = value + List.len(args).to_i64_wrap()
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
        if (!std.mem.eql(u8, entry.report.title, "COMPTIME EXPECT FAILED")) continue;
        found = true;
        try std.testing.expectEqualStrings("main", entry.module_name);
    }
    try std.testing.expect(found);
}

test "selected compile-time branch crash reports during compile-time evaluation" {
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
        \\    value = if True {
        \\        crash "selected compile-time branch crash"
        \\    } else {
        \\        41.I64
        \\    }
        \\    _ = value + List.len(args).to_i64_wrap()
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
        if (!std.mem.eql(u8, entry.report.title, "COMPTIME CRASH")) continue;
        found = true;
        try std.testing.expectEqualStrings("main", entry.module_name);
        try expectReportContains(gpa, entry.report, "selected compile-time branch crash");
    }
    try std.testing.expect(found);
}

test "selected compile-time branch dbg reports during compile-time evaluation" {
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
        \\    value = if True {
        \\        dbg "selected compile-time branch"
        \\        41.I64
        \\    } else {
        \\        42.I64
        \\    }
        \\    _ = value + List.len(args).to_i64_wrap()
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
    try std.testing.expect(!coord.hasUserErrors());

    var found = false;
    var report_iter = coord.iterReports();
    while (report_iter.next()) |entry| {
        if (!std.mem.eql(u8, entry.report.title, "COMPTIME DBG")) continue;
        found = true;
        try std.testing.expectEqual(@import("reporting").Severity.info, entry.report.severity);
        try std.testing.expectEqualStrings("main", entry.module_name);
        try expectReportContains(gpa, entry.report, "\"selected compile-time branch\"");
    }
    try std.testing.expect(found);
}

test "selected compile-time branch expect failure reports during compile-time evaluation" {
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
        \\    value = if True {
        \\        expect 1.I64 == 2.I64
        \\        41.I64
        \\    } else {
        \\        42.I64
        \\    }
        \\    _ = value + List.len(args).to_i64_wrap()
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
        if (!std.mem.eql(u8, entry.report.title, "COMPTIME EXPECT FAILED")) continue;
        found = true;
        try std.testing.expectEqualStrings("main", entry.module_name);
    }
    try std.testing.expect(found);
}

test "top-level value shared with selected root reports compile-time dbg once" {
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
        \\shared = {
        \\    dbg "shared top-level root"
        \\    41.I64
        \\}
        \\
        \\main! = |args| {
        \\    _ = shared + List.len(args).to_i64_wrap()
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
    try std.testing.expect(!coord.hasUserErrors());

    var matching_dbg_reports: usize = 0;
    var report_iter = coord.iterReports();
    while (report_iter.next()) |entry| {
        if (!std.mem.eql(u8, entry.report.title, "COMPTIME DBG")) continue;
        try std.testing.expectEqual(@import("reporting").Severity.info, entry.report.severity);
        try std.testing.expectEqualStrings("main", entry.module_name);
        if (try reportContains(gpa, entry.report, "\"shared top-level root\"")) {
            matching_dbg_reports += 1;
        }
    }
    try std.testing.expectEqual(@as(usize, 1), matching_dbg_reports);
}

test "effectful top-level call is rejected before compile-time dbg can run" {
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
        \\tick! : {} => Str
        \\tick! = |_| "effectful debug should not run"
        \\
        \\bad = {
        \\    dbg tick!({})
        \\    41.I64
        \\}
        \\
        \\main! = |_args| {
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

    var found_effectful = false;
    var report_iter = coord.iterReports();
    while (report_iter.next()) |entry| {
        if (std.mem.eql(u8, entry.report.title, "EFFECTFUL TOP-LEVEL VALUE")) {
            found_effectful = true;
            try std.testing.expectEqualStrings("main", entry.module_name);
        }
        if (std.mem.eql(u8, entry.report.title, "COMPTIME DBG")) {
            try expectReportDoesNotContain(gpa, entry.report, "\"effectful debug should not run\"");
        }
    }
    try std.testing.expect(found_effectful);
}

test "effectful selected-root candidate is not evaluated by compile-time finalization" {
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
        \\tick! : {} => Str
        \\tick! = |_| "effectful debug should not run"
        \\
        \\sentinel = {
        \\    dbg "compile-time finalizer ran"
        \\    41.I64
        \\}
        \\
        \\main! = |_args| {
        \\    _ = dbg tick!({})
        \\    _ = sentinel
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
    try std.testing.expect(!coord.hasUserErrors());

    var saw_sentinel = false;
    var report_iter = coord.iterReports();
    while (report_iter.next()) |entry| {
        if (!std.mem.eql(u8, entry.report.title, "COMPTIME DBG")) continue;
        try std.testing.expectEqualStrings("main", entry.module_name);
        try expectReportDoesNotContain(gpa, entry.report, "\"effectful debug should not run\"");
        if (try reportContains(gpa, entry.report, "\"compile-time finalizer ran\"")) {
            saw_sentinel = true;
        }
    }
    try std.testing.expect(saw_sentinel);
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
        if (!std.mem.eql(u8, entry.report.title, "NON-EXHAUSTIVE DESTRUCTURE")) continue;
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
        if (std.mem.eql(u8, entry.report.title, "NON-EXHAUSTIVE MATCH")) {
            found_match = true;
            try std.testing.expectEqualStrings("main", entry.module_name);
        }
        if (std.mem.eql(u8, entry.report.title, "NON-EXHAUSTIVE DESTRUCTURE")) {
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
        if (std.mem.eql(u8, entry.report.title, "UNUSED BRANCH")) found_unused_branch = true;
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
        if (!std.mem.eql(u8, entry.report.title, "NON-EXHAUSTIVE MATCH")) continue;
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
        if (!std.mem.eql(u8, entry.report.title, "NON-EXHAUSTIVE MATCH")) continue;
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
) anyerror!void {
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
) anyerror!void {
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
) anyerror!void {
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
) anyerror!void {
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

fn expectRegionEqual(expected: base.Region, actual: base.Region) anyerror!void {
    try std.testing.expectEqual(expected.start.offset, actual.start.offset);
    try std.testing.expectEqual(expected.end.offset, actual.end.offset);
}

fn writeEchoPlatform(dir: anytype) anyerror!void {
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

fn writeBoxedSpritePlatform(dir: anytype) anyerror!void {
    try dir.createDirPath(std.testing.io, ".roc_echo_platform");
    try dir.writeFile(std.testing.io, .{
        .sub_path = ".roc_echo_platform/main.roc",
        .data =
        \\platform ""
        \\    requires {
        \\        [Model : model] for main : { init! : () => model, update! : model => model }
        \\    }
        \\    exposes [Sprite]
        \\    packages {}
        \\    provides { "init_for_host": init_for_host!, "update_for_host": update_for_host! }
        \\    hosted {}
        \\
        \\import Sprite
        \\
        \\init_for_host! : () => Box(Model)
        \\init_for_host! = || {
        \\    init_fn! = main.init!
        \\    Box.box(init_fn!())
        \\}
        \\
        \\update_for_host! : Box(Model) => Box(Model)
        \\update_for_host! = |boxed| {
        \\    update_fn! = main.update!
        \\    Box.box(update_fn!(Box.unbox(boxed)))
        \\}
        ,
    });
    try dir.writeFile(std.testing.io, .{
        .sub_path = ".roc_echo_platform/Sprite.roc",
        .data =
        \\Sprite := {
        \\    data : List(U8),
        \\    bpp : [BPP1, BPP2],
        \\    stride : U32,
        \\    region : { src_x : U32, src_y : U32, width : U32, height : U32 },
        \\}.{
        \\    SubRegion : { src_x : U32, src_y : U32, width : U32, height : U32 }
        \\
        \\    new : { data : List(U8), bpp : [BPP1, BPP2], width : U32, height : U32 } -> Sprite
        \\    new = |{ data, bpp, width, height }| {
        \\        data,
        \\        bpp,
        \\        stride: width,
        \\        region: { src_x: 0, src_y: 0, width, height },
        \\    }
        \\
        \\    sub : Sprite, SubRegion -> Try(Sprite, [OutOfBounds])
        \\    sub = |sprite, sub_region| {
        \\        current_region = sprite.region
        \\        out_of_bound_x = sub_region.src_x + sub_region.width > current_region.width
        \\        out_of_bound_y = sub_region.src_y + sub_region.height > current_region.height
        \\
        \\        if out_of_bound_x or out_of_bound_y {
        \\            Err(OutOfBounds)
        \\        } else {
        \\            Ok({
        \\                ..sprite,
        \\                region: {
        \\                    src_x: current_region.src_x + sub_region.src_x,
        \\                    src_y: current_region.src_y + sub_region.src_y,
        \\                    width: sub_region.width,
        \\                    height: sub_region.height,
        \\                },
        \\            })
        \\        }
        \\    }
        \\
        \\    sub_or_crash : Sprite, SubRegion -> Sprite
        \\    sub_or_crash = |sprite, sub_region|
        \\        match Sprite.sub(sprite, sub_region) {
        \\            Ok(sub_sprite) => sub_sprite
        \\            Err(OutOfBounds) => {
        \\                crash "bad sprite"
        \\            }
        \\        }
        \\}
        ,
    });
}

fn buildStaticDataExportsForAppSource(
    allocator: std.mem.Allocator,
    source: []const u8,
) anyerror![]@import("backend").StaticDataExport {
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    try writeEchoPlatform(tmp_dir.dir);
    try tmp_dir.dir.writeFile(std.testing.io, .{
        .sub_path = "main.roc",
        .data = source,
    });
    const app_path = try tmp_dir.dir.realPathFileAlloc(std.testing.io, "main.roc", allocator);
    defer allocator.free(app_path);

    var arena_impl = collections.SingleThreadArena.init(allocator);
    defer arena_impl.deinit();
    const arena = arena_impl.allocator();

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
        CoreCtx.default(allocator, arena, std.testing.io),
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
    const root_view = check.CheckedArtifact.loweringViewWithRelations(root, relations);

    const lir_roots = try lir.CheckedPipeline.selectPlatformEntrypointRoots(allocator, root.root_requests.runtime_requests);
    defer allocator.free(lir_roots);

    var lowered = try lir.CheckedPipeline.lowerCheckedModulesToLir(
        allocator,
        .{
            .root = root_view,
            .imports = imports,
        },
        .{ .requests = lir_roots, .include_static_data_exports = true },
        .{ .target_usize = base.target.TargetUsize.native },
    );
    defer lowered.deinit();

    return try static_data_exports.buildProvidedDataExports(
        allocator,
        .{
            .root = root_view,
            .imports = imports,
        },
        &lowered,
        roc_target.RocTarget.detectNative(),
    );
}

fn expectReportDoesNotContain(
    allocator: std.mem.Allocator,
    report: *const @import("reporting").Report,
    needle: []const u8,
) anyerror!void {
    try std.testing.expect(!try reportContains(allocator, report, needle));
}

fn expectReportContains(
    allocator: std.mem.Allocator,
    report: *const @import("reporting").Report,
    needle: []const u8,
) anyerror!void {
    try std.testing.expect(try reportContains(allocator, report, needle));
}

fn reportContains(
    allocator: std.mem.Allocator,
    report: *const @import("reporting").Report,
    needle: []const u8,
) anyerror!bool {
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

    return std.mem.find(u8, writer_alloc.written(), needle) != null;
}

fn countStaticDataLiteralAssignments(store: *const lir.LirStore) usize {
    var count: usize = 0;
    for (store.cf_stmts.items) |stmt| {
        switch (stmt) {
            .assign_literal => |assign| switch (assign.value) {
                .static_data => count += 1,
                else => {},
            },
            else => {},
        }
    }
    return count;
}

fn countStaticDataLiteralAssignmentsToListLength(
    root: check.CheckedArtifact.LoweringModuleView,
    imports: []const check.CheckedArtifact.ImportedModuleView,
    lowered: *const lir.CheckedPipeline.LoweredProgram,
    len: usize,
) usize {
    var count: usize = 0;
    for (lowered.lir_result.store.cf_stmts.items) |stmt| {
        switch (stmt) {
            .assign_literal => |assign| switch (assign.value) {
                .static_data => |id| {
                    const static_data = lowered.lir_result.static_data_values.items[@intFromEnum(id)];
                    const node = constNodeForStaticData(root, imports, static_data.const_ref, static_data.node);
                    if (constNodeContainsListLength(node.module, node.id, len)) count += 1;
                },
                else => {},
            },
            else => {},
        }
    }
    return count;
}

fn countStaticDataValuesContainingListU8(
    root: check.CheckedArtifact.LoweringModuleView,
    imports: []const check.CheckedArtifact.ImportedModuleView,
    lowered: *const lir.CheckedPipeline.LoweredProgram,
    bytes: []const u8,
) usize {
    var count: usize = 0;
    for (lowered.lir_result.static_data_values.items) |static_data| {
        const node = constNodeForStaticData(root, imports, static_data.const_ref, static_data.node);
        if (constNodeContainsListU8(node.module, node.id, bytes)) count += 1;
    }
    return count;
}

fn countStaticDataLiteralAssignmentsToListU8(
    root: check.CheckedArtifact.LoweringModuleView,
    imports: []const check.CheckedArtifact.ImportedModuleView,
    lowered: *const lir.CheckedPipeline.LoweredProgram,
    bytes: []const u8,
) usize {
    var count: usize = 0;
    for (lowered.lir_result.store.cf_stmts.items) |stmt| {
        switch (stmt) {
            .assign_literal => |assign| switch (assign.value) {
                .static_data => |id| {
                    const static_data = lowered.lir_result.static_data_values.items[@intFromEnum(id)];
                    const node = constNodeForStaticData(root, imports, static_data.const_ref, static_data.node);
                    if (constNodeContainsListU8(node.module, node.id, bytes)) count += 1;
                },
                else => {},
            },
            else => {},
        }
    }
    return count;
}

const StaticConstModule = struct {
    templates: *const check.CheckedArtifact.ConstTemplateTable,
    store: *const check.CheckedArtifact.ConstStore,
};

const StaticConstNode = struct {
    module: StaticConstModule,
    id: check.CheckedArtifact.ConstNodeId,
};

fn constNodeForStaticData(
    root: check.CheckedArtifact.LoweringModuleView,
    imports: []const check.CheckedArtifact.ImportedModuleView,
    const_ref: check.CheckedArtifact.ConstId,
    node: ?check.CheckedArtifact.ConstNodeId,
) StaticConstNode {
    const module = constModuleForStaticData(root, imports, const_ref);
    if (node) |id| return .{ .module = module, .id = id };
    const template = module.templates.get(const_ref);
    return switch (template.state) {
        .stored_const => |stored| .{ .module = module, .id = stored.node },
        .reserved,
        .eval_template,
        => @panic("static data literal referenced an unstored const"),
    };
}

fn constModuleForStaticData(
    root: check.CheckedArtifact.LoweringModuleView,
    imports: []const check.CheckedArtifact.ImportedModuleView,
    const_ref: check.CheckedArtifact.ConstId,
) StaticConstModule {
    if (moduleKeyEqual(root.module.key, const_ref.artifact)) return .{
        .templates = &root.module.const_templates,
        .store = &root.module.const_store,
    };
    for (root.relation_modules) |relation| {
        if (moduleKeyEqual(relation.key, const_ref.artifact)) return .{
            .templates = relation.const_templates,
            .store = relation.const_store,
        };
    }
    for (imports) |imported| {
        if (moduleKeyEqual(imported.key, const_ref.artifact)) return .{
            .templates = imported.const_templates,
            .store = imported.const_store,
        };
    }
    @panic("static data literal referenced a const outside the lowering module set");
}

fn constNodeContainsListLength(module: StaticConstModule, node: check.CheckedArtifact.ConstNodeId, len: usize) bool {
    return switch (module.store.get(node)) {
        .list => |items| {
            if (items.len == len) return true;
            for (items) |item| {
                if (constNodeContainsListLength(module, item, len)) return true;
            }
            return false;
        },
        .box => |payload| constNodeContainsListLength(module, payload, len),
        .tuple => |items| {
            for (items) |item| {
                if (constNodeContainsListLength(module, item, len)) return true;
            }
            return false;
        },
        .record => |items| {
            for (items) |item| {
                if (constNodeContainsListLength(module, item, len)) return true;
            }
            return false;
        },
        .tag => |tag| {
            for (tag.payloads) |payload| {
                if (constNodeContainsListLength(module, payload, len)) return true;
            }
            return false;
        },
        .nominal => |nominal| constNodeContainsListLength(module, nominal.backing, len),
        .fn_value => |fn_id| {
            const fn_value = module.store.getFn(fn_id);
            for (fn_value.captures) |capture| {
                if (constNodeContainsListLength(module, capture.value, len)) return true;
            }
            return false;
        },
        else => false,
    };
}

fn constNodeContainsListU8(module: StaticConstModule, node: check.CheckedArtifact.ConstNodeId, bytes: []const u8) bool {
    return switch (module.store.get(node)) {
        .list => |items| {
            if (constNodeIsListU8Items(module, items, bytes)) return true;
            for (items) |item| {
                if (constNodeContainsListU8(module, item, bytes)) return true;
            }
            return false;
        },
        .box => |payload| constNodeContainsListU8(module, payload, bytes),
        .tuple => |items| {
            for (items) |item| {
                if (constNodeContainsListU8(module, item, bytes)) return true;
            }
            return false;
        },
        .record => |items| {
            for (items) |item| {
                if (constNodeContainsListU8(module, item, bytes)) return true;
            }
            return false;
        },
        .tag => |tag| {
            for (tag.payloads) |payload| {
                if (constNodeContainsListU8(module, payload, bytes)) return true;
            }
            return false;
        },
        .nominal => |nominal| constNodeContainsListU8(module, nominal.backing, bytes),
        .fn_value => |fn_id| {
            const fn_value = module.store.getFn(fn_id);
            for (fn_value.captures) |capture| {
                if (constNodeContainsListU8(module, capture.value, bytes)) return true;
            }
            return false;
        },
        else => false,
    };
}

fn constNodeIsListU8(module: StaticConstModule, node: check.CheckedArtifact.ConstNodeId, bytes: []const u8) bool {
    return switch (module.store.get(node)) {
        .list => |items| constNodeIsListU8Items(module, items, bytes),
        .nominal => |nominal| constNodeIsListU8(module, nominal.backing, bytes),
        else => false,
    };
}

fn constNodeIsListU8Items(module: StaticConstModule, items: []const check.CheckedArtifact.ConstNodeId, bytes: []const u8) bool {
    if (items.len != bytes.len) return false;
    for (items, bytes) |item, expected| {
        const actual = switch (module.store.get(item)) {
            .scalar => |scalar| switch (scalar) {
                .u8 => |byte| byte,
                else => return false,
            },
            else => return false,
        };
        if (actual != expected) return false;
    }
    return true;
}

fn constNodeContainsFnValue(module: StaticConstModule, node: check.CheckedArtifact.ConstNodeId) bool {
    return switch (module.store.get(node)) {
        .fn_value => true,
        .list => |items| {
            for (items) |item| {
                if (constNodeContainsFnValue(module, item)) return true;
            }
            return false;
        },
        .box => |payload| constNodeContainsFnValue(module, payload),
        .tuple => |items| {
            for (items) |item| {
                if (constNodeContainsFnValue(module, item)) return true;
            }
            return false;
        },
        .record => |items| {
            for (items) |item| {
                if (constNodeContainsFnValue(module, item)) return true;
            }
            return false;
        },
        .tag => |tag| {
            for (tag.payloads) |payload| {
                if (constNodeContainsFnValue(module, payload)) return true;
            }
            return false;
        },
        .nominal => |nominal| constNodeContainsFnValue(module, nominal.backing),
        else => false,
    };
}

fn constNodeContainsFnContext(module: StaticConstModule, node: check.CheckedArtifact.ConstNodeId) bool {
    return switch (module.store.get(node)) {
        .fn_value => |fn_id| {
            const fn_value = module.store.getFn(fn_id);
            if (fn_value.local_proc_contexts.len > 0) return true;
            for (fn_value.captures) |capture| {
                if (constNodeContainsFnContext(module, capture.value)) return true;
            }
            return false;
        },
        .list => |items| {
            for (items) |item| {
                if (constNodeContainsFnContext(module, item)) return true;
            }
            return false;
        },
        .box => |payload| constNodeContainsFnContext(module, payload),
        .tuple => |items| {
            for (items) |item| {
                if (constNodeContainsFnContext(module, item)) return true;
            }
            return false;
        },
        .record => |items| {
            for (items) |item| {
                if (constNodeContainsFnContext(module, item)) return true;
            }
            return false;
        },
        .tag => |tag| {
            for (tag.payloads) |payload| {
                if (constNodeContainsFnContext(module, payload)) return true;
            }
            return false;
        },
        .nominal => |nominal| constNodeContainsFnContext(module, nominal.backing),
        else => false,
    };
}

fn moduleKeyEqual(a: check.CheckedArtifact.CheckedModuleArtifactKey, b: check.CheckedArtifact.CheckedModuleArtifactKey) bool {
    return std.mem.eql(u8, a.bytes[0..], b.bytes[0..]);
}

fn countInternalStaticValueExports(exports: []const @import("backend").StaticDataExport) usize {
    var count: usize = 0;
    for (exports) |static_export| {
        if (std.mem.startsWith(u8, static_export.symbol_name, "roc__static_const_value_")) {
            count += 1;
        }
    }
    return count;
}

fn expectInternalStaticValueExportsAreLinkableOnly(exports: []const @import("backend").StaticDataExport) anyerror!void {
    var found = false;
    for (exports) |static_export| {
        if (!std.mem.startsWith(u8, static_export.symbol_name, "roc__static_const_value_")) continue;

        found = true;
        try std.testing.expect(static_export.is_global);
        try std.testing.expect(!static_export.is_exported);
    }
    try std.testing.expect(found);
}

fn exportsContainSequence(exports: []const @import("backend").StaticDataExport, sequence: []const u8) bool {
    return countExportsContainingSequence(exports, sequence) != 0;
}

fn findExportContainingSequence(exports: []const @import("backend").StaticDataExport, sequence: []const u8) ?@import("backend").StaticDataExport {
    for (exports) |static_export| {
        if (std.mem.find(u8, static_export.bytes, sequence) != null) return static_export;
    }
    return null;
}

fn countExportsContainingSequence(exports: []const @import("backend").StaticDataExport, sequence: []const u8) usize {
    var count: usize = 0;
    for (exports) |static_export| {
        if (std.mem.find(u8, static_export.bytes, sequence) != null) count += 1;
    }
    return count;
}

fn expectStaticDataExportsEquivalent(
    allocator: std.mem.Allocator,
    left: []const @import("backend").StaticDataExport,
    right: []const @import("backend").StaticDataExport,
) anyerror!void {
    try std.testing.expectEqual(left.len, right.len);

    const matched = try allocator.alloc(bool, right.len);
    defer allocator.free(matched);
    @memset(matched, false);

    for (left) |left_export| {
        var found = false;
        for (right, matched) |right_export, *is_matched| {
            if (is_matched.*) continue;
            if (!staticDataExportEquivalentIgnoringSymbolNames(left, right, left_export, right_export)) continue;
            is_matched.* = true;
            found = true;
            break;
        }
        try std.testing.expect(found);
    }
}

fn staticDataExportEquivalentIgnoringSymbolNames(
    left_exports: []const @import("backend").StaticDataExport,
    right_exports: []const @import("backend").StaticDataExport,
    left: @import("backend").StaticDataExport,
    right: @import("backend").StaticDataExport,
) bool {
    if (left.symbol_offset != right.symbol_offset) return false;
    if (left.alignment != right.alignment) return false;
    if (left.is_global != right.is_global) return false;
    if (left.is_exported != right.is_exported) return false;
    if (!std.mem.eql(u8, left.bytes, right.bytes)) return false;
    if (left.relocations.len != right.relocations.len) return false;

    for (left.relocations, right.relocations) |left_relocation, right_relocation| {
        if (left_relocation.offset != right_relocation.offset) return false;
        if (left_relocation.addend != right_relocation.addend) return false;
        if (left_relocation.kind != right_relocation.kind) return false;

        const left_target = findStaticDataExportBySymbol(left_exports, left_relocation.target_symbol_name);
        const right_target = findStaticDataExportBySymbol(right_exports, right_relocation.target_symbol_name);
        if (left_target == null or right_target == null) {
            if (!std.mem.eql(u8, left_relocation.target_symbol_name, right_relocation.target_symbol_name)) return false;
            continue;
        }
        if (!staticDataRelocationTargetsEquivalent(left_target.?, right_target.?)) return false;
    }

    return true;
}

fn staticDataRelocationTargetsEquivalent(
    left: @import("backend").StaticDataExport,
    right: @import("backend").StaticDataExport,
) bool {
    return left.symbol_offset == right.symbol_offset and
        left.alignment == right.alignment and
        left.is_global == right.is_global and
        left.is_exported == right.is_exported and
        std.mem.eql(u8, left.bytes, right.bytes);
}

fn findStaticDataExportBySymbol(
    exports: []const @import("backend").StaticDataExport,
    symbol_name: []const u8,
) ?@import("backend").StaticDataExport {
    for (exports) |static_export| {
        if (std.mem.eql(u8, static_export.symbol_name, symbol_name)) return static_export;
    }
    return null;
}

fn findExportWithFunctionPointerAndRelocationTo(
    exports: []const @import("backend").StaticDataExport,
    symbol_name: []const u8,
) ?@import("backend").StaticDataExport {
    for (exports) |static_export| {
        var has_function_pointer = false;
        var has_symbol_relocation = false;
        for (static_export.relocations) |relocation| {
            if (relocation.kind == .function_pointer) has_function_pointer = true;
            if (std.mem.eql(u8, relocation.target_symbol_name, symbol_name)) has_symbol_relocation = true;
        }
        if (has_function_pointer and has_symbol_relocation) return static_export;
    }
    return null;
}

fn countFunctionPointerRelocations(exports: []const @import("backend").StaticDataExport) usize {
    var count: usize = 0;
    for (exports) |static_export| {
        for (static_export.relocations) |relocation| {
            if (relocation.kind == .function_pointer) count += 1;
        }
    }
    return count;
}

fn countInternalStaticValueRelocationsTo(exports: []const @import("backend").StaticDataExport, symbol_name: []const u8) usize {
    var count: usize = 0;
    for (exports) |static_export| {
        if (!std.mem.startsWith(u8, static_export.symbol_name, "roc__static_const_value_")) continue;
        for (static_export.relocations) |relocation| {
            if (std.mem.eql(u8, relocation.target_symbol_name, symbol_name)) count += 1;
        }
    }
    return count;
}

fn countStaticDataRelocationsTo(exports: []const @import("backend").StaticDataExport, symbol_name: []const u8) usize {
    var count: usize = 0;
    for (exports) |static_export| {
        for (static_export.relocations) |relocation| {
            if (std.mem.eql(u8, relocation.target_symbol_name, symbol_name)) count += 1;
        }
    }
    return count;
}

fn expectAllInternalStaticValueExportsRelocateTo(exports: []const @import("backend").StaticDataExport, symbol_name: []const u8) anyerror!void {
    var found = false;
    for (exports) |static_export| {
        if (!std.mem.startsWith(u8, static_export.symbol_name, "roc__static_const_value_")) continue;
        found = true;
        var relocates_to_symbol = false;
        for (static_export.relocations) |relocation| {
            if (std.mem.eql(u8, relocation.target_symbol_name, symbol_name)) {
                relocates_to_symbol = true;
            }
        }
        try std.testing.expect(relocates_to_symbol);
    }
    try std.testing.expect(found);
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

fn countStoredHoistedListLength(artifact: anytype, len: usize) usize {
    var count: usize = 0;
    for (artifact.hoisted_constants.entries) |entry| {
        const template = artifact.const_templates.get(entry.const_ref);
        const node = switch (template.state) {
            .stored_const => |stored| stored.node,
            .reserved,
            .eval_template,
            => continue,
        };
        if (constNodeContainsListLength(.{
            .templates = artifact.const_templates,
            .store = artifact.const_store,
        }, node, len)) count += 1;
    }
    return count;
}

fn countStoredTopLevelListU8(artifact: anytype, bytes: []const u8) usize {
    var count: usize = 0;
    const module = StaticConstModule{
        .templates = artifact.const_templates,
        .store = artifact.const_store,
    };
    for (artifact.compile_time_roots.roots) |root| {
        if (root.kind != .constant) continue;
        const node = switch (root.payload) {
            .const_node => |const_node| const_node,
            .pending,
            .fn_value,
            .expect,
            => continue,
        };
        if (constNodeIsListU8(module, node, bytes)) count += 1;
    }
    return count;
}

fn countStoredHoistedListLengthInModules(artifacts: anytype, len: usize) usize {
    var count: usize = 0;
    for (artifacts) |artifact| {
        count += countStoredHoistedListLength(artifact, len);
    }
    return count;
}

fn countStoredHoistedFnValue(artifact: anytype) usize {
    var count: usize = 0;
    for (artifact.hoisted_constants.entries) |entry| {
        const template = artifact.const_templates.get(entry.const_ref);
        const node = switch (template.state) {
            .stored_const => |stored| stored.node,
            .reserved,
            .eval_template,
            => continue,
        };
        if (constNodeContainsFnValue(.{
            .templates = artifact.const_templates,
            .store = artifact.const_store,
        }, node)) count += 1;
    }
    return count;
}

fn countStoredHoistedFnValueInModules(artifacts: anytype) usize {
    var count: usize = 0;
    for (artifacts) |artifact| {
        count += countStoredHoistedFnValue(artifact);
    }
    return count;
}

fn countStoredHoistedFnContext(artifact: anytype) usize {
    var count: usize = 0;
    for (artifact.hoisted_constants.entries) |entry| {
        const template = artifact.const_templates.get(entry.const_ref);
        const node = switch (template.state) {
            .stored_const => |stored| stored.node,
            .reserved,
            .eval_template,
            => continue,
        };
        if (constNodeContainsFnContext(.{
            .templates = artifact.const_templates,
            .store = artifact.const_store,
        }, node)) count += 1;
    }
    return count;
}

fn countStoredHoistedFnContextInModules(artifacts: anytype) usize {
    var count: usize = 0;
    for (artifacts) |artifact| {
        count += countStoredHoistedFnContext(artifact);
    }
    return count;
}

fn countStoredHoistedListLengthAndFnValue(artifact: anytype, len: usize) usize {
    var count: usize = 0;
    for (artifact.hoisted_constants.entries) |entry| {
        const template = artifact.const_templates.get(entry.const_ref);
        const node = switch (template.state) {
            .stored_const => |stored| stored.node,
            .reserved,
            .eval_template,
            => continue,
        };
        const module = StaticConstModule{
            .templates = artifact.const_templates,
            .store = artifact.const_store,
        };
        if (constNodeContainsListLength(module, node, len) and
            constNodeContainsFnValue(module, node))
        {
            count += 1;
        }
    }
    return count;
}

fn countStoredHoistedListLengthAndFnValueInModules(artifacts: anytype, len: usize) usize {
    var count: usize = 0;
    for (artifacts) |artifact| {
        count += countStoredHoistedListLengthAndFnValue(artifact, len);
    }
    return count;
}

fn countSelectedHoistedConstResolvedRefs(artifact: anytype) usize {
    var count: usize = 0;
    for (artifact.resolved_value_refs.records) |record| {
        switch (record.ref) {
            .selected_hoisted_const => count += 1,
            else => {},
        }
    }
    return count;
}

fn countSelectedHoistedConstResolvedRefsWithListAndFn(artifact: anytype, len: usize) usize {
    var count: usize = 0;
    for (artifact.resolved_value_refs.records) |record| {
        const selected = switch (record.ref) {
            .selected_hoisted_const => |selected| selected,
            else => continue,
        };
        const template = artifact.const_templates.get(selected.const_use.const_ref);
        const node = switch (template.state) {
            .stored_const => |stored| stored.node,
            .reserved,
            .eval_template,
            => continue,
        };
        const module = StaticConstModule{
            .templates = artifact.const_templates,
            .store = artifact.const_store,
        };
        if (constNodeContainsListLength(module, node, len) and
            constNodeContainsFnValue(module, node))
        {
            count += 1;
        }
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
) anyerror!i64 {
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
) anyerror!i64 {
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
) anyerror!i64 {
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
    // `roc check` evaluates the nested expect instead of only the outer one.
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
        \\    expect 3.I64 == 3.I64
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

test "nested expect failure is reported once" {
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
    try std.testing.expect(coord.hasUserErrors());

    var failures: usize = 0;
    var report_iter = coord.iterReports();
    while (report_iter.next()) |entry| {
        if (!std.mem.eql(u8, entry.report.title, "COMPTIME EXPECT FAILED")) continue;
        failures += 1;
        try std.testing.expectEqualStrings("main", entry.module_name);
        try expectReportContains(gpa, entry.report, "expect failed");
    }
    try std.testing.expectEqual(@as(usize, 1), failures);
}
