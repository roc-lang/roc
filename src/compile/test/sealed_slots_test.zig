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
    \\module [parse, parse_ok, outcome, imported_outcome, outcome_good, wrapped, held, kept, tree, many, run, make, unit, ok_unit, listed, apply_one, reader, both_sides, pattern_arg, folded, nested_fn]
    \\import Shape
    \\Outcome(good, bad) := [Good(good), Bad(bad)]
    \\Wrapped(e) := [Wrapped(Try(U8, e))]
    \\Held(e) := [Held(List(e))]
    \\Kept(e) := { kept : e }
    \\Tree(e) := [Leaf(e), Node(Tree(e))]
    \\parse : Str -> Try(U8, [InvalidU8])
    \\parse = |text| if Str.is_empty(text) Err(InvalidU8) else Ok(1)
    \\parse_ok : Str -> Try([Some(U8), None], Str)
    \\parse_ok = |text| if Str.is_empty(text) Ok(None) else Ok(Some(1))
    \\outcome : Str -> Outcome(U8, [InvalidU8])
    \\outcome = |text| if Str.is_empty(text) Outcome.Bad(InvalidU8) else Outcome.Good(1)
    \\imported_outcome : Str -> Shape(U8, [InvalidU8])
    \\imported_outcome = |text| if Str.is_empty(text) Shape.Bad(InvalidU8) else Shape.Good(1)
    \\outcome_good : Str -> Outcome([Some(U8), None], Str)
    \\outcome_good = |text| if Str.is_empty(text) Outcome.Good(None) else Outcome.Good(Some(1))
    \\wrapped : Str -> Wrapped([InvalidU8])
    \\wrapped = |text| Wrapped.Wrapped(if Str.is_empty(text) Err(InvalidU8) else Ok(1))
    \\held : Str -> Held([Oops])
    \\held = |_| Held.Held([Oops])
    \\kept : Str -> Kept([Oops])
    \\kept = |_| Kept.{ kept: Oops }
    \\tree : Str -> Tree([Oops])
    \\tree = |_| Tree.Node(Tree.Leaf(Oops))
    \\many : Str -> List([Oops])
    \\many = |_| [Oops]
    \\run : a -> Try(U8, [Oops]) where [a.go : a -> Try(U8, [Oops])]
    \\run = |x| Ok(x.go()?)
    \\make = |n| { count: n }
    \\unit = |_n| {}
    \\ok_unit = |_n| Ok({})
    \\listed = |n| [{ a: n }]
    \\apply_one = |n, f| f({ a: n })
    \\reader = |n| |m| { a: n, b: m }
    \\both_sides = |r, c| if c r else { a: 1.U8 }
    \\pattern_arg = |{ w, h }| w * h
    \\folded = |rs| List.fold(rs, 0, |acc, { w }| acc + w)
    \\nested_fn = |n| [|m| { a: n, b: m }]
;

test "a template's stored sealed slots match the independent scalar oracle" {
    const gpa = std.testing.allocator;
    const io = std.testing.io;
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    tmp.dir.writeFile(io, .{ .sub_path = "Shape.roc", .data = "Shape(good, bad) := [Good(good), Bad(bad)]\n" }) catch |err| return setupFailed(err);
    tmp.dir.writeFile(io, .{ .sub_path = "Sealed.roc", .data = source }) catch |err| return setupFailed(err);
    const cwd = tmp.dir.realPathFileAlloc(io, ".", gpa) catch |err| return setupFailed(err);
    defer gpa.free(cwd);
    const path = tmp.dir.realPathFileAlloc(io, "Sealed.roc", gpa) catch |err| return setupFailed(err);
    defer gpa.free(path);
    var build = BuildEnv.init(gpa, .single_threaded, 1, roc_target.RocTarget.detectNative(), cwd, io, .lss) catch |err| return setupFailed(err);
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
    var walk = CheckedArtifact.SealedSchemeVarWalk.init(gpa, &artifact.checked_types.view());
    defer walk.deinit();

    // The production batch and the scalar oracle must agree on every actual
    // checked template, including its sealed source type view.
    var computed: std.ArrayList(u32) = .empty;
    defer computed.deinit(gpa);
    for (templates.templates.items()) |*template| {
        computed.clearRetainingCapacity();
        for (templates.templateSchemeVars(template), 0..) |variable, slot| {
            if (try walk.sealed(variable, template.checked_fn_root, templates.templateEvidenceParams(template))) try computed.append(gpa, @intCast(slot));
        }
        errdefer if (artifact.canonical_names.procBase(template.proc_base).export_name.get()) |export_name| {
            std.debug.print("the stored sealed slots of {s} differ from the oracle's\n", .{artifact.canonical_names.exportNameText(export_name)});
        };
        try std.testing.expectEqualSlices(u32, computed.items, templates.templateSealedSlots(template));
    }

    // A row in an argument a named type's declared backing holds as a tag
    // payload is sealed, for `Try` and for a user nominal of its shape alike:
    // the error row, the ok row, and a row a declaration hands to `Try`, in
    // a type declared here or in another module.
    for ([_][]const u8{ "parse", "parse_ok", "outcome", "imported_outcome", "outcome_good", "wrapped" }) |name| {
        const template = findTemplate(artifact, name) orelse return error.TestUnexpectedResult;
        try std.testing.expectEqual(@as(usize, 1), templates.templateSchemeVars(template).len);
        try std.testing.expectEqual(@as(usize, 1), templates.templateSealedSlots(template).len);
    }

    // A row a declared backing holds under a list or a record, or hands to
    // its own recursion, is outside the reach of a conversion: unsealed.
    for ([_][]const u8{ "held", "kept", "tree" }) |name| {
        const template = findTemplate(artifact, name) orelse return error.TestUnexpectedResult;
        try std.testing.expectEqual(@as(usize, 1), templates.templateSchemeVars(template).len);
        try std.testing.expectEqual(@as(usize, 0), templates.templateSealedSlots(template).len);
    }

    // `many`'s row is under a list, outside the result's reach: unsealed.
    const many = findTemplate(artifact, "many") orelse return error.TestUnexpectedResult;
    try std.testing.expect(templates.templateSchemeVars(many).len >= 1);
    try std.testing.expectEqual(@as(usize, 0), templates.templateSealedSlots(many).len);

    // `run`'s result row and its `where` method's row are separate
    // variables, so no evidence parameter mentions the result row: sealed.
    const run = findTemplate(artifact, "run") orelse return error.TestUnexpectedResult;
    try std.testing.expect(templates.templateEvidenceParams(run).len >= 1);
    try std.testing.expectEqual(@as(usize, 1), templates.templateSealedSlots(run).len);

    // A record's uncommitted tail is sealed in each of these, wherever it
    // sits in the function's type (design.md "Uncommitted Record Tails"): a
    // use only closes it or makes its record a nominal type stored the same
    // way, so the worker takes no hidden descriptor for it.
    for ([_][]const u8{ "make", "unit", "ok_unit", "listed", "apply_one", "reader", "both_sides", "pattern_arg", "folded", "nested_fn" }) |name| {
        const template = findTemplate(artifact, name) orelse return error.TestUnexpectedResult;
        var tails: usize = 0;
        var sealed_tails: usize = 0;
        const sealed_slots = templates.templateSealedSlots(template);
        for (templates.templateSchemeVars(template), 0..) |variable, slot| {
            switch (artifact.checked_types.view().payload(variable)) {
                .flex => |flex| if (flex.row_tail == .uncommitted_record) {
                    tails += 1;
                    if (std.mem.findScalar(u32, sealed_slots, @intCast(slot)) != null) sealed_tails += 1;
                },
                .rigid, .pending, .field_kind, .err, .alias, .record, .tuple, .nominal, .function, .empty_record, .tag_union, .empty_tag_union => {},
            }
        }
        try std.testing.expectEqual(@as(usize, 1), tails);
        try std.testing.expectEqual(@as(usize, 1), sealed_tails);
    }
}

fn findTemplate(artifact: *const CheckedArtifact.CheckedModuleArtifact, name: []const u8) ?*const CheckedArtifact.CheckedProcedureTemplate {
    for (artifact.checked_procedure_templates.templates.items()) |*template| {
        const export_name = artifact.canonical_names.procBase(template.proc_base).export_name.get() orelse continue;
        if (std.mem.eql(u8, artifact.canonical_names.exportNameText(export_name), name)) return template;
    }
    return null;
}
