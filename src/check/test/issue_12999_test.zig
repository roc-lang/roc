//! A derived codec relation that settled default materialization parks is
//! validated before the module's codec contracts freeze, including when a
//! widened use already read a value row holding its error row.

const TestEnv = @import("TestEnv.zig");

test "issue 12999: a late-validated parser relation whose error row a widened use read" {
    var env = try TestEnv.init("Test",
        \\parse_with : (p -> Str) -> Try(Str, [Bad]) where [p.parser_for : {} -> (Str -> Try({ value : p, rest : Str }, [Bad]))]
        \\parse_with = |decide| {
        \\    P : p
        \\    parse = P.parser_for({})
        \\    match parse("41") {
        \\        Ok(read) => Ok(decide(read.value))
        \\        Err(Bad) => Err(Bad)
        \\    }
        \\}
        \\
        \\result = parse_with(|n| (n + 1.U64).to_str())
        \\
        \\widen : {} -> Try(Str, [Bad, Other])
        \\widen = |_| result
    );
    defer env.deinit();
    try env.assertOneTypeProblemMsgContains("Missing Method", "never resolved to a concrete type");
    try env.assertDefTypeOptions("widen", "{} -> Try(Str, [Bad, Other])", .{ .allow_type_errors = true });
}
