//! Regression test for issue #11291.

const std = @import("std");
const base = @import("base");
const postcheck = @import("postcheck");
const harness = @import("lower_to_lir_harness.zig");

// repro for https://github.com/roc-lang/roc/issues/11291
//
// `Container.to_list` forwards its `Container(a)` argument to the separately
// annotated `to_list_help`, so the concrete `Container(U8)` call site and the
// generic helper are two nominal representations of the same declaration. Both
// must carry the formal-to-actual mapping between the nominal backing and the
// nominal argument, so hidden-descriptor call planning can align the helper
// worker with the call site.
const app_body =
    \\Container(a) := { items : List(a) }.{
    \\    to_list : Container(a) -> List(a)
    \\    to_list = |value| to_list_help(value)
    \\}
    \\
    \\to_list_help : Container(a) -> List(a)
    \\to_list_help = |{ items }| items
    \\
    \\container : Container(U8)
    \\container = { items: Str.to_utf8("x") }
    \\
    \\main! = |_args| if Container.to_list(container) == Str.to_utf8("x") Ok({}) else Err(Exit(1))
;

test "issue 11291: a nominal method forwarding to an annotated helper keeps its backing substitutions" {
    for ([_]base.SpecializationStrategy{ .lss, .boxy }) |strategy| {
        try harness.expectLowersToLirWithOptions(app_body, .{
            .specialization_strategy = strategy,
        });
    }
}

test "issue 11291: nested and recursive nominal plans preserve positional backing arguments" {
    const source =
        \\Chain(a) := [End, Next(a, Chain(a))].{
        \\    first : Chain(a) -> Try(a, [Empty])
        \\    first = |chain| first_help(chain)
        \\}
        \\first_help : Chain(a) -> Try(a, [Empty])
        \\first_help = |chain| match chain {
        \\    End => Err(Empty)
        \\    Next(value, _) => Ok(value)
        \\}
        \\Pair(a, b, _unused) := { left: a, right: b }.{
        \\    right : Pair(a, b, c) -> b
        \\    right = |pair| right_help(pair)
        \\}
        \\right_help : Pair(a, b, c) -> b
        \\right_help = |{ left: _, right }| right
        \\value : Pair(Str, Chain(U8), U16)
        \\value = { left: "left", right: Next(42, End) }
        \\main! = |_args| if Chain.first(Pair.right(value)) == Ok(42) Ok({}) else Err(Exit(1))
    ;
    for ([_]base.SpecializationStrategy{ .lss, .boxy }) |strategy| {
        try harness.expectLowersToLirWithOptions(source, .{
            .specialization_strategy = strategy,
            .boxy_plan_inspect = if (strategy == .boxy) expectCompleteNominalSubstitutions else null,
        });
    }
}

test "issue 11291: imported nominal methods preserve backing arguments" {
    const gpa = std.testing.allocator;
    const io = std.testing.io;
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();
    try tmp_dir.dir.writeFile(io, .{ .sub_path = "Container.roc", .data =
        \\Container(a) := { items: List(a) }.{
        \\    to_list : Container(a) -> List(a)
        \\    to_list = |value| to_list_help(value)
        \\}
        \\to_list_help : Container(a) -> List(a)
        \\to_list_help = |{ items }| items
    });
    try tmp_dir.dir.writeFile(io, .{ .sub_path = "app.roc", .data =
        \\app [main!] { pf: platform "./platform.roc" }
        \\import Container exposing [Container]
        \\container : Container(U8)
        \\container = { items: Str.to_utf8("x") }
        \\main! = |_args| if Container.to_list(container) == Str.to_utf8("x") Ok({}) else Err(Exit(1))
    });
    try tmp_dir.dir.writeFile(io, .{ .sub_path = "platform.roc", .data =
        \\platform ""
        \\    requires {} { main! : List(Str) => Try({}, [Exit(I8), ..]) }
        \\    exposes []
        \\    packages {}
        \\    provides { "roc_main": main_for_host! }
        \\main_for_host! : List(Str) => I8
        \\main_for_host! = |args| match main!(args) {
        \\    Ok({}) => 0
        \\    Err(Exit(code)) => code
        \\    Err(_) => 1
        \\}
    });
    const app_path = try tmp_dir.dir.realPathFileAlloc(io, "app.roc", gpa);
    defer gpa.free(app_path);
    for ([_]base.SpecializationStrategy{ .lss, .boxy }) |strategy| {
        try harness.expectAppPathLowersToLirWithOptions(app_path, .{ .specialization_strategy = strategy });
    }
}

// Check the changed producer boundary independently of recursive aggregate ARC.
fn expectCompleteNominalSubstitutions(plan: *const postcheck.Boxy.Plan.ProgramPlan) harness.LowerToLirHarnessError!void {
    var nominal_uses: usize = 0;
    // Stored constants have their own monomorphic representations and backing;
    // declaration substitutions belong to checked-type bindings.
    for (plan.type_reps.items) |binding| {
        const rep_id = binding.rep orelse continue;
        const rep = plan.representations.items[@intFromEnum(rep_id)];
        if (rep.kind != .nominal) continue;
        const span = rep.nominal_backing_arg_substitutions;
        var argument_count: u32 = 0;
        for (plan.childSlice(rep.children)) |child| {
            if (child.role == .nominal_arg) argument_count += 1;
        }
        try std.testing.expectEqual(argument_count, span.len);
        if (span.len == 0) continue;
        nominal_uses += 1;
        var substitutions = plan.nominalBackingSubstitutions(span);
        var next_argument: u32 = 0;
        while (substitutions.next()) |substitution| {
            try std.testing.expectEqual(next_argument, substitution.arg_index);
            try std.testing.expectEqual(substitution.actual_rep, plan.nominalBackingActual(span, next_argument).?);
            try std.testing.expect(@intFromEnum(substitution.actual_rep) < plan.representations.items.len);
            next_argument += 1;
        }
    }
    try std.testing.expect(nominal_uses > 0);
}
