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
        var current = store.getProcSpec(@enumFromInt(@as(u32, @intCast(i)))).body orelse continue;
        var remaining = store.cfStmtCount() + 1;
        while (remaining > 0) : (remaining -= 1) {
            switch (store.getCFStmt(current)) {
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
                .assign_boxy_desc_ref,
                .assign_boxy_dict_ref,
                .assign_boxy_box,
                .assign_boxy_reuse_box,
                .assign_boxy_unbox,
                .assign_boxy_adapt,
                .assign_boxy_inspect,
                .assign_boxy_eq,
                .assign_boxy_tag,
                .assign_boxy_tag_payload,
                .assign_call_dict,
                .assign_low_level,
                .assign_list,
                .assign_struct,
                .assign_tag,
                .store_struct,
                .store_tag,
                .set_local,
                .debug,
                .expect,
                .comptime_branch_taken,
                .incref,
                .decref,
                .decref_if_initialized,
                .free,
                => |stmt| current = stmt.next,
                .boxy_tag_match, .expect_err, .comptime_exhaustiveness_failed, .switch_stmt, .switch_initialized_payload, .str_match, .str_match_set, .loop_continue, .loop_break, .join, .jump, .ret => break,
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

fn expectRejectedBinding(artifact: *const check.CheckedArtifact.CheckedModuleArtifact) error{ TestUnexpectedResult, TestExpectedEqual }!void {
    var rejected: usize = 0;
    for (artifact.top_level_procedure_bindings.bindings.items) |binding| {
        if (binding.body != .checked_error) continue;
        rejected += 1;
        const expr = binding.body.checked_error;
        try std.testing.expect(artifact.checked_bodies.expr(expr).data == .runtime_error);
        for (artifact.compile_time_roots.roots) |root| try std.testing.expect(root.expr != expr);
        for (artifact.entry_wrappers.wrappers.items) |wrapper| try std.testing.expect(wrapper.body_expr != expr);
    }
    try std.testing.expectEqual(@as(usize, 1), rejected);
    try std.testing.expectEqual(@as(usize, 0), artifact.callable_eval_templates.templates.items.len);
    var exported_rejected: usize = 0;
    var exported_valid: usize = 0;
    for (artifact.exported_procedure_bindings.bindings) |binding| {
        switch (binding.body) {
            .checked_error => {
                exported_rejected += 1;
                const closure = artifact.exported_procedure_bindings.rowClosure(binding);
                try std.testing.expectEqual(@as(usize, 0), closure.checked_procedure_templates.len);
                try std.testing.expectEqual(@as(usize, 0), closure.callable_eval_templates.len);
            },
            .direct_template => exported_valid += 1,
            .callable_eval_template => return error.TestUnexpectedResult,
        }
    }
    try std.testing.expectEqual(@as(usize, 1), exported_rejected);
    try std.testing.expectEqual(@as(usize, 1), exported_valid);
}

test "issue 11236: rejection survives exports and serialization without callable wrappers" {
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
    const artifact = build.findModuleByPath(bad_path).?.semanticData().?.checked_artifact.?;
    try expectRejectedBinding(artifact);
    const alias = build.findModuleByPath(alias_path).?.semanticData().?.checked_artifact.?;
    try std.testing.expectEqual(@as(usize, 1), alias.exported_procedure_bindings.bindings.len);
    try std.testing.expect(alias.exported_procedure_bindings.bindings[0].body == .checked_error);
    for (alias.compile_time_roots.roots) |root| try std.testing.expect(root.request_eligibility == .ineligible);
    const importer = build.findModuleByPath(path).?.semanticData().?.checked_artifact.?;
    var found_use = false;
    for (importer.checked_bodies.stored_exprs.items) |expr| {
        const source = imported_source[expr.source_region.start.offset..expr.source_region.end.offset];
        if (std.mem.eql(u8, source, "Alias.f")) {
            try std.testing.expect(expr.data == .runtime_error);
            try std.testing.expect(expr.diverges);
            try std.testing.expect(expr.contains_diagnostic_error);
            try std.testing.expect(!expr.evaluation_may_be_elided_for_inspect);
            found_use = true;
        }
    }
    try std.testing.expect(found_use);

    var arena = std.heap.ArenaAllocator.init(gpa);
    defer arena.deinit();
    const CompactWriter = collections.CompactWriter;
    var writer = CompactWriter.init();
    const Serialized = check.CheckedArtifact.CheckedModuleArtifact.Serialized;
    const header = try writer.appendAlloc(arena.allocator(), Serialized);
    try header.serialize(artifact, arena.allocator(), &writer);
    const buffer = try gpa.alignedAlloc(u8, CompactWriter.SERIALIZATION_ALIGNMENT, writer.total_bytes);
    _ = try writer.writeToBuffer(buffer);
    const serialized: *const Serialized = @ptrCast(@alignCast(buffer.ptr));
    var loaded = serialized.deserialize(buffer, gpa, artifact.module_env);
    defer loaded.deinitRetainingModuleEnv(gpa);
    try expectRejectedBinding(&loaded);
}
