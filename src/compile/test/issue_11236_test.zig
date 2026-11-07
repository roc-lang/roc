//! Regression tests for issue #11236.

const expectLowersToLirWithOptions = @import("lower_to_lir_harness.zig").expectLowersToLirWithOptions;

test "issue 11236: a call to a function whose annotation extends its own tag-union parameter lowers to a checked crash" {
    try expectLowersToLirWithOptions(
        \\f : Try({}, [..a]) -> Try({}, [A, ..a])
        \\f = |x| x
        \\
        \\main! = |_args| {
        \\    f(Err(A))
        \\}
    , .{ .allow_user_errors = true });
}

const std = @import("std");
const lir = @import("lir");
const harness = @import("lower_to_lir_harness.zig");

// Inspect live procedure bodies, excluding detached statements in the append-only
// store. A rejected value must still fail when optimizations erase its consumer.
fn hasTerminalCheckedError(store: *const lir.LirStore) bool {
    for (0..store.procSpecCount()) |i| {
        var current = store.getProcSpecImmediate(@fromBackingInt(@intCast(@as(u32, @intCast(i))))).body orelse continue;
        var remaining = store.cfStmtCount() + 1;
        while (remaining > 0) : (remaining -= 1) {
            switch (store.getCFStmtImmediate(current).*) {
                .runtime_error => return true,
                .crash => |crash| {
                    if (crash.msg == .literal and std.mem.eql(u8, store.getString(crash.msg.literal), "runtime error")) return true;
                    break;
                },
                inline .init_uninitialized,
                .assign_ref,
                .assign_literal,
                .assign_call,
                .assign_call_erased,
                .assign_packed_erased_fn,
                .assign_low_level,
                .assign_list,
                .assign_struct,
                .assign_tag,
                .store_struct,
                .store_tag,
                .set_local,
                .debug,
                .expect,
                .incref,
                .decref,
                .decref_if_initialized,
                .free,
                .dyn,
                => |stmt| current = stmt.next,
                .expect_err, .comptime_exhaustiveness_failed, .switch_stmt, .switch_initialized_payload, .str_match, .str_match_set, .join, .jump, .ret => break,
            }
        }
    }
    return false;
}

fn expectCheckedError(store: *const lir.LirStore, _: *const @import("layout").Store) harness.LowerToLirHarnessError!void {
    try std.testing.expect(hasTerminalCheckedError(store));
}

fn expectNoCheckedError(store: *const lir.LirStore, _: *const @import("layout").Store) harness.LowerToLirHarnessError!void {
    try std.testing.expect(!hasTerminalCheckedError(store));
}

const base = @import("base");
const can = @import("can");
const check = @import("check");
const collections = @import("collections");
const roc_target = @import("roc_target");
const BuildEnv = @import("../compile_build.zig").BuildEnv;

const rejected_function =
    \\f : Try({}, [..a]) -> Try({}, [A, ..a])
    \\f = |x| x
    \\
;

test "issue 11236: rejected callable uses lower in both strategies and worker modes" {
    for ([_]base.SpecializationStrategy{ .lss, .boxy }) |strategy| {
        for ([_]usize{ 1, 2 }) |workers| {
            try harness.expectLirInspectionWithOptions(rejected_function ++
                \\main! = |_args| f(Err(A))
            , .{ .allow_user_errors = true, .specialization_strategy = strategy, .specialization_workers = workers }, expectCheckedError);
            try harness.expectLirInspectionWithOptions(rejected_function ++
                \\main! = |_args| {
                \\    _ = f(Err(A))
                \\    Ok({})
                \\}
            , .{ .allow_user_errors = true, .specialization_strategy = strategy, .specialization_workers = workers }, expectCheckedError);
        }
    }
}

test "issue 11236: passing a rejected callable is a checked error even without calling it" {
    for ([_]base.SpecializationStrategy{ .lss, .boxy }) |strategy| {
        try harness.expectLirInspectionWithOptions(
            \\f : U64 -> Str
            \\f = |x| x
            \\ignore = |_f| {}
            \\main! = |_args| {
            \\    ignore(f)
            \\    Ok({})
            \\}
        , .{ .allow_user_errors = true, .specialization_strategy = strategy }, expectCheckedError);
    }
}

test "issue 11236: aliases preserve rejected callable evaluation" {
    for ([_]base.SpecializationStrategy{ .lss, .boxy }) |strategy| {
        try harness.expectLirInspectionWithOptions(rejected_function ++
            \\g = h
            \\h = f
            \\main! = |_args| g(Err(A))
        , .{ .allow_user_errors = true, .specialization_strategy = strategy }, expectCheckedError);
    }
}

test "issue 11236: a rejected platform-required callable remains a checked error" {
    for ([_]base.SpecializationStrategy{ .lss, .boxy }) |strategy| {
        try harness.expectLirInspectionWithOptions(
            \\main! : List(Str) => Try({}, [Exit(I8)])
            \\main! = |_args| 123
        , .{ .allow_user_errors = true, .specialization_strategy = strategy }, expectCheckedError);
    }
}

test "issue 11236: independent valid code still lowers alongside a rejected binding" {
    for ([_]base.SpecializationStrategy{ .lss, .boxy }) |strategy| {
        try harness.expectLirInspectionWithOptions(rejected_function ++
            \\good : U64 -> U64
            \\good = |x| x
            \\main! = |_args| {
            \\    if good(42) == 42 { Ok({}) } else { Err(Exit(1)) }
            \\}
        , .{ .allow_user_errors = true, .specialization_strategy = strategy }, expectNoCheckedError);
        try harness.expectLirInspectionWithOptions(
            \\f : Try({}, [..a]) -> Try({}, [..a])
            \\f = |x| x
            \\main! = |_args| f(Err(A))
        , .{ .specialization_strategy = strategy }, expectNoCheckedError);
    }
}

fn expectRejectedBinding(artifact: *const check.Checked.CheckedModule) error{ TestUnexpectedResult, TestExpectedEqual, OutOfMemory }!void {
    var rejected: usize = 0;
    for ((try artifact.checkedTables()).top_level_procedure_bindings.bindings.items) |binding| {
        if (binding.body != .checked_error) continue;
        rejected += 1;
        const expr = binding.body.checked_error;
        try std.testing.expect((try artifact.bodies()).exprData(expr) == .runtime_error);
        for ((try artifact.checkedTables()).compile_time_roots.roots) |root| try std.testing.expect(root.expr != expr);
    }
    try std.testing.expectEqual(@as(usize, 1), rejected);
    try std.testing.expectEqual(@as(usize, 0), (try artifact.checkedTables()).callable_eval_templates.templates.items.len);
    var exported_rejected: usize = 0;
    var exported_valid: usize = 0;
    for ((try artifact.exportRows()).procedure_bindings) |binding| {
        switch (binding.body) {
            .checked_error => exported_rejected += 1,
            .direct_template => exported_valid += 1,
            .callable_eval_template => return error.TestUnexpectedResult,
        }
    }
    try std.testing.expectEqual(@as(usize, 1), exported_rejected);
    try std.testing.expectEqual(@as(usize, 1), exported_valid);
}

test "issue 11236: rejection survives exports and stored outputs without callable wrappers" {
    const gpa = std.testing.allocator;
    const io = std.testing.io;
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    try tmp.dir.writeFile(io, .{ .sub_path = "Bad.roc", .data =
        \\module [f, good]
        \\f : U64 -> Str
        \\f = |x| x
        \\good : U64 -> U64
        \\good = |x| x
    });
    try tmp.dir.writeFile(io, .{ .sub_path = "Alias.roc", .data =
        \\module [f]
        \\import Bad
        \\f = Bad.f
    });
    const imported_source =
        \\module [use]
        \\import Alias
        \\use : U64 -> Str
        \\use = |x| Alias.f(x)
    ;
    try tmp.dir.writeFile(io, .{ .sub_path = "Use.roc", .data = imported_source });
    const cwd = try tmp.dir.realPathFileAlloc(io, ".", gpa);
    defer gpa.free(cwd);
    const path = try tmp.dir.realPathFileAlloc(io, "Use.roc", gpa);
    defer gpa.free(path);
    const bad_path = try tmp.dir.realPathFileAlloc(io, "Bad.roc", gpa);
    defer gpa.free(bad_path);
    const alias_path = try tmp.dir.realPathFileAlloc(io, "Alias.roc", gpa);
    defer gpa.free(alias_path);
    var build = try BuildEnv.init(gpa, .single_threaded, 1, roc_target.RocTarget.detectNative(), cwd, io);
    defer build.deinit();
    try build.build(path);
    const reports = try build.drainReports();
    defer build.freeDrainedReports(reports);
    var mismatch = false;
    for (reports) |module_reports| for (module_reports.reports) |report| {
        if (std.mem.eql(u8, report.title, "Type Mismatch")) mismatch = true;
    };
    try std.testing.expect(mismatch);
    const artifact = build.findModuleByPath(bad_path).?.semanticData().?.checked_module.?;
    try expectRejectedBinding(artifact);
    const alias = build.findModuleByPath(alias_path).?.semanticData().?.checked_module.?;
    try std.testing.expectEqual(@as(usize, 1), (try alias.exportRows()).procedure_bindings.len);
    try std.testing.expect((try alias.exportRows()).procedure_bindings[0].body == .checked_error);
    // The alias's evaluation reaches the rejected binding and stores the crash.
    for ((try alias.checkedTables()).compile_time_roots.roots) |root| {
        try std.testing.expect(alias.evaluation.rootPayload(root.id) == .const_node);
        try std.testing.expect(alias.evaluation.const_store.get(alias.evaluation.rootPayload(root.id).const_node) == .checked_error);
    }
    const importer = build.findModuleByPath(path).?.semanticData().?.checked_module.?;
    var found_use = false;
    const importer_bodies = try importer.bodies();
    const importer_store = &importer.moduleEnvConst().store;
    for (0..importer_bodies.nodeCount()) |raw| {
        if (importer_bodies.index().nodeKind(@intCast(raw)) != .expr) continue;
        const expr: can.CIR.Expr.Idx = @fromBackingInt(@intCast(raw));
        const region = importer_store.getExprRegion(expr);
        const source = imported_source[region.start.offset..region.end.offset];
        if (std.mem.eql(u8, source, "Alias.f")) {
            try std.testing.expect(importer_bodies.exprData(expr) == .runtime_error);
            try std.testing.expect(try importer_bodies.exprDiverges(expr, .run));
            try std.testing.expect(try importer_bodies.exprContainsDiagnosticError(expr));
            try std.testing.expect(!try importer_bodies.exprEvaluationMayBeElidedForInspect(expr));
            found_use = true;
        }
    }
    try std.testing.expect(found_use);

    var arena = std.heap.ArenaAllocator.init(gpa);
    defer arena.deinit();
    const CompactWriter = collections.CompactWriter;
    var writer = CompactWriter.init();
    const Serialized = check.Checked.CheckedModule.Serialized;
    const header = try writer.appendAlloc(arena.allocator(), Serialized);
    try header.serialize(artifact, arena.allocator(), &writer);
    const buffer = try gpa.alignedAlloc(u8, CompactWriter.SERIALIZATION_ALIGNMENT, writer.total_bytes);
    _ = try writer.writeToBuffer(buffer);
    // `Bad` imports only Builtin, which its module set holds first.
    std.debug.assert(artifact.direct_import_module_keys.len == 1);
    const builtin_key = artifact.direct_import_module_keys[0];
    const imports = [_]check.Checked.CheckedImport{.{
        .module_idx = 0,
        .key = builtin_key,
        .view = build.moduleResolver().resolve(builtin_key.digest()).?,
    }};
    var loaded = try check.Checked.openCheckedModule(gpa, .{
        .module_env_storage = artifact.module_env,
        .module_idx = artifact.module_identity.module_idx,
        .io = io,
        .imports = &imports,
        .resolver = build.moduleResolver(),
        .outputs = .{ .stored = .{ .backing = buffer, .backing_borrowed = false } },
    });
    defer loaded.deinitRetainingModuleEnv(gpa);
    try expectRejectedBinding(&loaded);
}
