//! Tests for the sealed scheme-variable slots the checked artifact records
//! for each procedure template (design.md "Sealed Result Rows").

const std = @import("std");
const check = @import("check");
const roc_target = @import("roc_target");
const BuildEnv = @import("../compile_build.zig").BuildEnv;

const CheckedArtifact = check.CheckedArtifact;

fn setupFailed(err: anytype) error{TestUnexpectedResult} {
    std.debug.print("building the test module failed: {s}\n", .{@errorName(err)});
    return error.TestUnexpectedResult;
}

const source =
    \\module [parse, many, run]
    \\parse : Str -> Try(U8, [InvalidU8])
    \\parse = |text| if Str.is_empty(text) Err(InvalidU8) else Ok(1)
    \\many : Str -> List([Oops])
    \\many = |_| [Oops]
    \\run : a -> Try(U8, [Oops]) where [a.go : a -> Try(U8, [Oops])]
    \\run = |x| x.go()
;

test "a template's stored sealed slots match the independent scalar oracle" {
    const gpa = std.testing.allocator;
    const io = std.testing.io;
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    tmp.dir.writeFile(io, .{ .sub_path = "Sealed.roc", .data = source }) catch |err| return setupFailed(err);
    const cwd = tmp.dir.realPathFileAlloc(io, ".", gpa) catch |err| return setupFailed(err);
    defer gpa.free(cwd);
    const path = tmp.dir.realPathFileAlloc(io, "Sealed.roc", gpa) catch |err| return setupFailed(err);
    defer gpa.free(path);
    var build = BuildEnv.init(gpa, .single_threaded, 1, roc_target.RocTarget.detectNative(), cwd, io) catch |err| return setupFailed(err);
    defer build.deinit();
    build.build(path) catch |err| return setupFailed(err);
    const reports = build.drainReports() catch |err| return setupFailed(err);
    defer build.freeDrainedReports(reports);
    for (reports) |module_reports| for (module_reports.reports) |report| {
        if (std.mem.eql(u8, report.title, "Module Header Deprecated")) continue;
        std.debug.print("unexpected report: {s}\n", .{report.title});
        return error.TestUnexpectedResult;
    };
    const artifact = build.findModuleByPath(path).?.semanticData().?.checked_artifact.?;
    const templates = &artifact.checked_procedure_templates;
    var walk = CheckedArtifact.SealedSchemeVarWalk.init(gpa, artifact.checked_types.view());
    defer walk.deinit();

    // The production batch and the original scalar traversal must agree on
    // every actual checked template, including its sealed source type view.
    var computed: std.ArrayList(u32) = .empty;
    defer computed.deinit(gpa);
    for (templates.templates.items) |*template| {
        computed.clearRetainingCapacity();
        for (templates.templateSchemeVars(template), 0..) |variable, slot| {
            if (try walk.sealed(variable, template.checked_fn_root, templates.templateEvidenceParams(template))) try computed.append(gpa, @intCast(slot));
        }
        try std.testing.expectEqualSlices(u32, computed.items, templates.templateSealedSlots(template));
    }

    // `parse`'s error row is sealed.
    const parse = findTemplate(artifact, "parse") orelse return error.TestUnexpectedResult;
    try std.testing.expectEqual(@as(usize, 1), templates.templateSealedSlots(parse).len);

    // `many`'s row is under a list, outside the result's reach: unsealed.
    const many = findTemplate(artifact, "many") orelse return error.TestUnexpectedResult;
    try std.testing.expect(templates.templateSchemeVars(many).len >= 1);
    try std.testing.expectEqual(@as(usize, 0), templates.templateSealedSlots(many).len);

    // `run`'s result row and its `where` method's row are separate
    // variables, so no evidence parameter mentions the result row: sealed.
    const run = findTemplate(artifact, "run") orelse return error.TestUnexpectedResult;
    try std.testing.expect(templates.templateEvidenceParams(run).len >= 1);
    try std.testing.expectEqual(@as(usize, 1), templates.templateSealedSlots(run).len);
}

fn findTemplate(artifact: *const CheckedArtifact.CheckedModuleArtifact, name: []const u8) ?*const CheckedArtifact.CheckedProcedureTemplate {
    for (artifact.checked_procedure_templates.templates.items) |*template| {
        const export_name = artifact.canonical_names.procBase(template.proc_base).export_name.get() orelse continue;
        if (std.mem.eql(u8, artifact.canonical_names.exportNameText(export_name), name)) return template;
    }
    return null;
}
