//! Generated code that reaches one value from several branches binds it to a
//! local or builds it once per branch, never handing one Monotype expression
//! to two parents (design.md, "The sharing invariant"). Each test grows one
//! shape that a producer could share an expression across branches in, and
//! checks the post-check program grows linearly with it.
const std = @import("std");
const harness = @import("lower_to_lir_harness.zig");

/// Lower the app and report the size of the lifted program every later
/// post-check stage walks.
fn liftedExprCount(app_body: []const u8) harness.LowerToLirHarnessError!usize {
    var lifted_exprs: usize = 0;
    try harness.expectLowersToLirWithOptions(app_body, .{
        .lifted_expr_count_out = &lifted_exprs,
    });
    return lifted_exprs;
}

/// Fail when the program grows by more between `sizes[1]` and `sizes[2]`
/// than twice what it grew between `sizes[0]` and `sizes[1]`.
fn expectLinear(comptime label: []const u8, sizes: [3]usize) error{ TestUnexpectedResult, PostCheckProgramGrewSuperlinearly }!void {
    try std.testing.expect(sizes[1] > sizes[0]);
    const first_stretch = sizes[1] - sizes[0];
    const second_stretch = sizes[2] - sizes[1];
    if (second_stretch > first_stretch * 2) {
        std.debug.print(label ++ ": post-check program held {d}, {d} and {d} expressions\n", .{ sizes[0], sizes[1], sizes[2] });
        return error.PostCheckProgramGrewSuperlinearly;
    }
}

/// A stored derived record parser over `field_count` fields whose names have
/// one length and differ only past their first eight bytes, for a format
/// that names each field it reads. Each field's name check is a probe of
/// those bytes and then a full comparison.
fn sameLengthFieldsParserApp(comptime field_count: usize) []const u8 {
    @setEvalBranchQuota(100_000);
    var fields: []const u8 = "";
    for (0..field_count) |index| {
        const separator = if (index == 0) "" else ", ";
        fields = fields ++ std.fmt.comptimePrint("{s}field_name_{c} : U64", .{ separator, 'a' + index });
    }
    return
    \\Format := [Default].{
    \\    missing_field : Format, Str -> [FormatError]
    \\    missing_field = |_, _| FormatError
    \\
    \\    rename_field : Format, Str -> Str
    \\    rename_field = |_, name| name
    \\
    \\    parse_u64 : Format, State -> Try({ value : U64, rest : State }, [FormatError])
    \\    parse_u64 = |_, _| Ok({ value: 1, rest: Done })
    \\
    \\    parse_record_start : Format, State -> Try([Counted({ len : U64, rest : State }), Uncounted(State)], [FormatError])
    \\    parse_record_start = |_, state| Ok(Uncounted(state))
    \\
    \\    parse_record_field : Format,
    \\    Encoding.FieldName.FieldNames(_shape),
    \\    State -> Try(
    \\        [
    \\            Field({ field : Encoding.FieldName(_shape), rest : State }),
    \\            TryField({ name : Str, rest : State }),
    \\            TryFieldCaseless({ name : Str, rest : State }),
    \\            Continue(State),
    \\            Done(State),
    \\        ],
    \\        [FormatError],
    \\    )
    \\    parse_record_field = |_, _, state|
    \\        match state {
    \\            Key(name) => Ok(TryField({ name, rest: Done }))
    \\            Done => Ok(Done(Done))
    \\        }
    \\
    \\    parse_record_after_field : Format, State -> Try([Continue(State), Done(State)], [FormatError])
    \\    parse_record_after_field = |_, state| Ok(Continue(state))
    \\
    \\    skip_record_field : Format, State -> Try(State, [FormatError])
    \\    skip_record_field = |_, _| Ok(Done)
    \\}
    \\
    \\State := [Key(Str), Done]
    \\
    \\
    ++ "Rec : { " ++ fields ++ " }\n\n" ++
        \\parse_rec : State -> Try({ value : Rec, rest : State }, [FormatError])
        \\parse_rec = Rec.parser_for(Format.Default)
        \\
        \\main! = |args| {
        \\    match parse_rec(Key(Str.join_with(args, ""))) {
        \\        Ok(_) => echo!("ok")
        \\        Err(_) => echo!("err")
        \\    }
        \\    Ok({})
        \\}
    ;
}

test "a stored record parser's field dispatch grows linearly with same-length field names" {
    const sizes = [3]usize{
        try liftedExprCount(comptime sameLengthFieldsParserApp(2)),
        try liftedExprCount(comptime sameLengthFieldsParserApp(4)),
        try liftedExprCount(comptime sameLengthFieldsParserApp(6)),
    };
    try expectLinear("same-length field names", sizes);
}
