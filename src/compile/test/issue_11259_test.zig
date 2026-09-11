//! Regression test for issue #11259: a typed string literal whose nominal type
//! has an ill-typed `from_quote` must report the type mismatch instead of
//! reaching Monotype lowering with an unresolved dispatch plan.
//! repro for https://github.com/roc-lang/roc/issues/11259

const std = @import("std");
const roc_target = @import("roc_target");
const compile_build = @import("../compile_build.zig");
const BuildEnv = compile_build.BuildEnv;

test "issue 11259: typed string literal with an ill-typed from_quote reports rather than aborting publication" {
    const gpa = std.testing.allocator;
    const io = std.testing.io;
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    try tmp_dir.dir.writeFile(io, .{
        .sub_path = "Repro.roc",
        .data =
        \\Tag := [Tag(Str)].{
        \\    from_quote : Str -> Try(Tag, [BadQuotedBytes(Str)])
        \\    from_quote = !|str| Ok(Tag(str))
        \\}
        \\
        \\single = "Roc".Tag
        \\
        \\multi =
        \\    \\line one
        \\    \\line two
        \\    .Tag
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

    var found_type_mismatch = false;
    for (drained) |module_reports| {
        for (module_reports.reports) |report| {
            if (std.mem.eql(u8, report.title, "Type Mismatch")) found_type_mismatch = true;
        }
    }
    try std.testing.expect(found_type_mismatch);
}

const rejected_quote_method =
    \\Tag := [Tag(Str)].{
    \\    from_quote : Str -> Try(Tag, [BadQuotedBytes(Str)])
    \\    from_quote = !|str| Ok(Tag(str))
    \\}
    \\
;

test "issue 11259: shared rejected conversions poison enclosing roots and preserve independent constants" {
    try expectConversionRecovery(rejected_quote_method ++
        \\bad = ["one".Tag, "two".Tag]
        \\good = 123.U64
        \\
    , null, .{ .rejections = 2 });
}

test "issue 11259: aliases of rejected conversions preserve independent constants" {
    try expectConversionRecovery(rejected_quote_method ++
        \\alias = bad
        \\bad = "one".Tag
        \\good = 123.U64
        \\
    , null, .{ .rejections = 1 });
}

test "issue 11259: a conversion selected before its method body is rejected does not evaluate" {
    try expectConversionRecovery("bad = \"one\".Tag\n" ++ rejected_quote_method ++
        \\good = 123.U64
        \\
    , null, .{ .rejections = 1 });
}

test "issue 11259: an imported rejected conversion preserves independent constants" {
    try expectConversionRecovery(
        \\import Broken
        \\bad : Broken.Tag
        \\bad = "one"
        \\good = 123.U64
        \\
    , "module [Tag]\n" ++ rejected_quote_method, .{ .rejections = 1 });
}

test "issue 11259: a rejected numeral conversion preserves independent constants" {
    try expectConversionRecovery(
        \\Tag := [Tag(U64)].{
        \\    from_numeral : Numeral -> Try(Tag, [InvalidNumeral(Str)])
        \\    from_numeral = !|_| Ok(Tag(0))
        \\}
        \\bad = 1.Tag
        \\good = 123.U64
        \\
    , null, .{ .rejections = 1 });
}

test "issue 11259: valid conversions still evaluate" {
    try expectConversionRecovery(
        \\Tag := [Tag(Str)].{
        \\    from_quote : Str -> Try(Tag, [BadQuotedBytes(Str)])
        \\    from_quote = |str| Ok(Tag(str))
        \\}
        \\value = "one".Tag
        \\good = 123.U64
        \\
    , null, .{ .rejections = 0 });
}

test "issue 11259: recovery preserves an independent recursive constant" {
    try expectConversionRecovery(rejected_quote_method ++
        \\Node := { next : {} -> Node }
        \\recursive : Node
        \\recursive = { next: |_| recursive }
        \\bad = "one".Tag
        \\good = 123.U64
        \\
    , null, .{ .rejections = 1, .independent_expr = "{ next: |_| recursive }" });
}

test "issue 11259: diagnostic propagation reaches a cycle through a delayed body" {
    try expectConversionRecovery(rejected_quote_method ++
        \\Node := { next : {} -> Node, value : Tag }
        \\alias = recursive
        \\recursive : Node
        \\recursive = { next: |_| recursive, value: "one".Tag }
        \\good = 123.U64
        \\
    , null, .{ .rejections = 1 });
}

test "issue 11259: rejected conversions remain lowerable runtime errors" {
    const harness = @import("lower_to_lir_harness.zig");
    for ([_]@import("base").SpecializationStrategy{ .lss, .boxy }) |strategy| {
        try harness.expectLowersToLirWithOptions(rejected_quote_method ++
            \\main! = |_args| {
            \\    _ = "one".Tag
            \\    Ok({})
            \\}
            \\
        , .{ .allow_user_errors = true, .specialization_strategy = strategy });
    }
}

const RecoveryExpectation = struct {
    rejections: usize,
    independent_expr: ?[]const u8 = null,
};

const ConversionRecoveryError = compile_build.InitError || compile_build.BuildRootError ||
    std.Io.Dir.WriteFileError || std.Io.Dir.RealPathFileAllocError ||
    error{ TestExpectedEqual, TestUnexpectedResult };

fn expectConversionRecovery(source: []const u8, imported_source: ?[]const u8, expected: RecoveryExpectation) ConversionRecoveryError!void {
    const expected_rejections = expected.rejections;
    const gpa = std.testing.allocator;
    const io = std.testing.io;
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    const module_source = try std.mem.concat(gpa, u8, &.{ "module []\n", source });
    defer gpa.free(module_source);
    try tmp.dir.writeFile(io, .{ .sub_path = "Repro.roc", .data = module_source });
    if (imported_source) |imported| try tmp.dir.writeFile(io, .{ .sub_path = "Broken.roc", .data = imported });
    const cwd = try tmp.dir.realPathFileAlloc(io, ".", gpa);
    defer gpa.free(cwd);
    const path = try tmp.dir.realPathFileAlloc(io, "Repro.roc", gpa);
    defer gpa.free(path);
    var build = try BuildEnv.init(gpa, .single_threaded, 1, roc_target.RocTarget.detectNative(), cwd, io);
    defer build.deinit();
    try build.build(path);
    const reports = try build.drainReports();
    defer build.freeDrainedReports(reports);
    var mismatches: usize = 0;
    for (reports) |module_reports| {
        for (module_reports.reports) |report| {
            if (std.mem.eql(u8, report.title, "Type Mismatch")) {
                mismatches += 1;
            } else {
                // Recovery must not add a secondary compile-time crash.
                if (report.severity != .warning) std.debug.print("unexpected report: {s}\n", .{report.title});
                try std.testing.expectEqual(.warning, report.severity);
            }
        }
    }
    try std.testing.expectEqual(@as(usize, if (expected_rejections == 0) 0 else 1), mismatches);

    const artifact = build.findModuleByPath(path).?.semanticData().?.checked_artifact.?;
    var rejected: usize = 0;
    for (artifact.static_dispatch_plans.plans) |plan| {
        if (plan.resolution != .checked_error) continue;
        rejected += 1;
        try std.testing.expect(artifact.checked_bodies.exprContainsDiagnosticError(plan.expr));
    }
    try std.testing.expectEqual(expected_rejections, rejected);

    var found_good = false;
    var found_independent = expected.independent_expr == null;
    var completed_conversions: usize = 0;
    for (artifact.compile_time_roots.roots) |root| {
        const expr = artifact.checked_bodies.expr(root.expr);
        if (artifact.checked_bodies.exprContainsDiagnosticError(root.expr)) {
            try std.testing.expectEqual(.ineligible, root.request_eligibility);
            for (artifact.root_requests.compile_time_requests) |request| {
                try std.testing.expect(request.compile_time_root != root.id);
            }
        }
        const expr_source = module_source[expr.source_region.start.offset..expr.source_region.end.offset];
        if (expected.independent_expr) |independent| {
            if (std.mem.eql(u8, expr_source, independent)) {
                try std.testing.expect(!artifact.checked_bodies.exprContainsDiagnosticError(root.expr));
                try std.testing.expect(root.payload == .const_node);
                found_independent = true;
            }
        }
        if (std.mem.eql(u8, expr_source, "123.U64")) {
            try std.testing.expectEqual(.eligible, root.request_eligibility);
            try std.testing.expect(root.payload == .const_node);
            found_good = true;
        }
        if (root.kind == .quote_conversion and root.payload == .const_node) completed_conversions += 1;
    }
    try std.testing.expect(found_good);
    try std.testing.expect(found_independent);
    try std.testing.expectEqual(@as(usize, if (expected_rejections == 0) 1 else 0), completed_conversions);
}
