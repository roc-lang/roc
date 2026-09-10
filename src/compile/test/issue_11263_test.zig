//! Regression test for issue #11263: matching on a field access whose receiver
//! is not in scope must report the out-of-scope name instead of carrying an
//! erroneous checked type into Monotype instantiation.
//! repro for https://github.com/roc-lang/roc/issues/11263

const std = @import("std");
const roc_target = @import("roc_target");
const compile_build = @import("../compile_build.zig");
const BuildEnv = compile_build.BuildEnv;

test "issue 11263: match on a field access of an out-of-scope name reports rather than panicking" {
    const gpa = std.testing.allocator;
    const io = std.testing.io;
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    try tmp_dir.dir.writeFile(io, .{
        .sub_path = "Repro.roc",
        .data =
        \\describe : { x : U64, y : U64, z : U64 } -> U64
        \\describe = |rec| match rxo_b.ec {
        \\    { x, y } => x + y
        \\}
        \\
        ,
    });

    const cwd = try tmp_dir.dir.realPathFileAlloc(io, ".", gpa);
    defer gpa.free(cwd);
    const module_path = try tmp_dir.dir.realPathFileAlloc(io, "Repro.roc", gpa);
    defer gpa.free(module_path);

    var build_env = try BuildEnv.init(gpa, .single_threaded, 1, roc_target.RocTarget.detectNative(), cwd, io);
    defer build_env.deinit();
    try build_env.build(module_path);

    const drained = try build_env.drainReports();
    defer build_env.freeDrainedReports(drained);

    var found_name_not_in_scope = false;
    for (drained) |module_reports| {
        for (module_reports.reports) |report| {
            if (std.mem.eql(u8, report.title, "Name Not In Scope")) found_name_not_in_scope = true;
        }
    }
    try std.testing.expect(found_name_not_in_scope);
}

const base = @import("base");
const harness = @import("lower_to_lir_harness.zig");

test "issue 11263: erroneous chained and optional field accesses lower in both strategies" {
    const sources = [_][]const u8{
        \\main! = |_args| match missing.outer.inner {
        \\    { x, y } => if x + y == 42.U64 { Ok({}) } else { Err(Exit(1)) }
        \\}
        ,
        \\main! = |_args| match missing.?outer.inner {
        \\    Ok({ x, y }) => if x + y == 42.U64 { Ok({}) } else { Err(Exit(1)) }
        \\    Err(MissingField) => Err(Exit(2))
        \\}
        ,
        \\main! = |_args| {
        \\    bad = missing
        \\    match bad.inner {
        \\        { x, y } => if x + y == 42.U64 { Ok({}) } else { Err(Exit(1)) }
        \\    }
        \\}
        ,
    };
    for ([_]base.SpecializationStrategy{ .lss, .boxy }) |strategy| {
        for (sources) |source| {
            try harness.expectLowersToLirWithOptions(source, .{
                .allow_user_errors = true,
                .specialization_strategy = strategy,
            });
        }
    }
}

test "issue 11263: valid required and optional field access chains still evaluate" {
    for ([_]base.SpecializationStrategy{ .lss, .boxy }) |strategy| {
        try harness.expectLowersToLirWithOptions(
            \\read : { outer ?: { inner : U64 } } -> U64
            \\read = |rec| match rec.?outer.inner {
            \\    Ok(value) => value
            \\    Err(MissingField) => 0
            \\}
            \\expect read({ outer: { inner: 42 } }) == 42
            \\expect read({}) == 0
            \\expect { outer: { inner: 42.U64 } }.outer.inner == 42
            \\main! = |_args| Ok({})
        , .{ .specialization_strategy = strategy });
    }
}

test "issue 11263: a rejected field access preserves independent compile-time roots" {
    const source =
        \\module [describe, good]
        \\describe : { x : U64, y : U64, z : U64 } -> U64
        \\describe = |_rec| match missing.ec {
        \\    { x, y } => x + y
        \\}
        \\good : U64 -> U64
        \\good = |arg| {
        \\    independent = 40.U64 + 2
        \\    independent + arg
        \\}
    ;
    const gpa = std.testing.allocator;
    const io = std.testing.io;
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    try tmp.dir.writeFile(io, .{ .sub_path = "Repro.roc", .data = source });
    const cwd = try tmp.dir.realPathFileAlloc(io, ".", gpa);
    defer gpa.free(cwd);
    const path = try tmp.dir.realPathFileAlloc(io, "Repro.roc", gpa);
    defer gpa.free(path);
    var build = try BuildEnv.init(gpa, .single_threaded, 1, roc_target.RocTarget.detectNative(), cwd, io);
    defer build.deinit();
    try build.build(path);

    const reports = try build.drainReports();
    defer build.freeDrainedReports(reports);
    var name_errors: usize = 0;
    for (reports) |module_reports| {
        for (module_reports.reports) |report| {
            if (std.mem.eql(u8, report.title, "Name Not In Scope")) {
                name_errors += 1;
            } else {
                try std.testing.expectEqual(.warning, report.severity);
            }
        }
    }
    try std.testing.expectEqual(@as(usize, 1), name_errors);

    const artifact = build.findModuleByPath(path).?.semanticData().?.checked_artifact.?;
    var found_independent = false;
    for (artifact.compile_time_roots.roots) |root| {
        const expr = artifact.checked_bodies.expr(root.expr);
        if (artifact.checked_bodies.exprContainsDiagnosticError(root.expr)) {
            try std.testing.expectEqual(.ineligible, root.request_eligibility);
        }
        const text = source[expr.source_region.start.offset..expr.source_region.end.offset];
        if (!std.mem.eql(u8, text, "40.U64 + 2")) continue;
        try std.testing.expectEqual(.eligible, root.request_eligibility);
        try std.testing.expect(root.payload == .const_node);
        const value = artifact.const_store.get(root.payload.const_node);
        try std.testing.expect(value == .scalar);
        try std.testing.expect(value.scalar == .u64);
        try std.testing.expectEqual(@as(u64, 42), value.scalar.u64);
        found_independent = true;
    }
    try std.testing.expect(found_independent);
}
