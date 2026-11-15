//! Regression test for https://github.com/roc-lang/roc/issues/13009.
//!
//! Hashing a record whose fields are tag unions with several variants
//! (`Set.insert` derives `to_hash` for it) must produce a post-check program
//! that grows with the number of fields, not exponentially in it. Each added
//! field costs a bounded amount of code, so the code fields 5 and 6 add stays
//! in the same range as the code fields 3 and 4 add.
const std = @import("std");
const harness = @import("lower_to_lir_harness.zig");

fn app(comptime field_count: usize) []const u8 {
    @setEvalBranchQuota(100_000);
    var fields: []const u8 = "";
    var values: []const u8 = "";
    for (0..field_count) |index| {
        const separator = if (index == 0) "" else ", ";
        fields = fields ++ std.fmt.comptimePrint("{s}f{d} : [Loading, Ready(Str), Failed(Str)]", .{ separator, index });
        values = values ++ std.fmt.comptimePrint("{s}f{d}: Loading", .{ separator, index });
    }
    return "Big : { " ++ fields ++ " }\n\n" ++
        "val : Big\n" ++
        "val = { " ++ values ++ " }\n\n" ++
        \\main! = |args| {
        \\    v = if args.len() > 100 { crash "x" } else val
        \\    echo!(Set.empty().insert(v).len().to_str())
        \\    Ok({})
        \\}
    ;
}

/// Lower the app and report the size of the lifted program every later
/// post-check stage walks.
fn liftedExprCount(app_body: []const u8) harness.LowerToLirHarnessError!usize {
    var lifted_exprs: usize = 0;
    try harness.expectLowersToLirWithOptions(app_body, .{
        .lifted_expr_count_out = &lifted_exprs,
    });
    return lifted_exprs;
}

test "issue 13009: hashing a record of tag-union fields grows the post-check program linearly" {
    const at_two = try liftedExprCount(comptime app(2));
    const at_four = try liftedExprCount(comptime app(4));
    const at_six = try liftedExprCount(comptime app(6));

    try std.testing.expect(at_four > at_two);
    const first_stretch = at_four - at_two;
    const second_stretch = at_six - at_four;
    if (second_stretch > first_stretch * 2) {
        std.debug.print(
            "post-check program held {d}, {d} and {d} expressions at 2, 4 and 6 fields\n",
            .{ at_two, at_four, at_six },
        );
        return error.PostCheckProgramGrewSuperlinearly;
    }
}
