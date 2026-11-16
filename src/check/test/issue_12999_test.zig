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
        \\result = parse_with(|n| (n + n).to_str())
        \\
        \\widen : {} -> Try(Str, [Bad, Other])
        \\widen = |_| result
    );
    defer env.deinit();
    try env.assertOneTypeErrorMsg(
        \\**Missing Method**
        \\A type this needs never resolved to a concrete type, so it has no `parser_for` method.
        \\```roc
        \\result = parse_with(|n| (n + n).to_str())
        \\```
        \\         ^^^^^^^^^^
        \\
        \\Its type needs all of these:
        \\
        \\    p
        \\      where [
        \\        p.parser_for : {} -> (Str -> Try({ rest: Str, value: p }, [Bad])),
        \\        p.plus : p, p -> p,
        \\        p.to_str : p -> Str,
        \\      ]
        \\
        \\Using the value somewhere that requires a specific type, or annotating it, will give it one.
        \\
        \\
    );
    try env.assertDefTypeOptions("widen", "{} -> Try(Str, [Bad, Other])", .{ .allow_type_errors = true });
}
