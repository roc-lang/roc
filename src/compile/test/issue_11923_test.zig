//! Regression tests for issue #11923: a definition whose body produces a tag
//! its annotation does not list keeps its annotated type, so lowering a call
//! that reaches it through a generated codec relates one signature.

const expectLowersToLirWithOptions = @import("lower_to_lir_harness.zig").expectLowersToLirWithOptions;

test "issue 11923: a runtime parser reaching a format method that produces an unlisted tag lowers" {
    try expectLowersToLirWithOptions(
        \\Format := [Default, Other].{
        \\    parse_u8 : Format, {} -> Try({ value : U8, rest : {} }, [Bad])
        \\    parse_u8 = |_, _| Err(OtherErr)
        \\}
        \\
        \\main! = |args| {
        \\    fmt = if args.len() > 5 Format.Other else Format.Default
        \\    result = (U8.parser_for(fmt))({})
        \\    echo!(if result == Err(Bad) "bad" else "other")
        \\    Ok({})
        \\}
    , .{ .allow_user_errors = true });
}
