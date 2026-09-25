//! Regression test for https://github.com/roc-lang/roc/issues/11528.

const harness = @import("lower_to_lir_harness.zig");

test "issue 11528: repeated closed Try calls before wrapped Json error lower to Monotype" {
    try harness.expectLowersToLirWithOptions(
        \\inner : {} -> Try({}, [Oops])
        \\inner = |{}| Ok({})
        \\
        \\outer : {} -> Try({}, [Oops])
        \\outer = |{}| inner({})
        \\
        \\check : Str -> Try({}, _)
        \\check = |body| {
        \\    result : Try({ foo : Str }, _)
        \\    result = Json.parse(body)
        \\    match result {
        \\        Err(err) => Err(Wrong(err))
        \\        Ok(_) => Ok({})
        \\    }
        \\}
        \\
        \\main! = |args| {
        \\    outer({})?
        \\    outer({})?
        \\    check(Str.join_with(args, ""))
        \\}
    , .{ .monotype_only = true });
}
