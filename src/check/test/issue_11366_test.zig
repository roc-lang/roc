//! Regression coverage for https://github.com/roc-lang/roc/issues/11366.
//! Accessing a nonexistent list field must fail during checking, including
//! when the access occurs in a comparison in a function's expect statement.
const TestEnv = @import("TestEnv.zig");

test "issue 11366 - inline expect reports invalid list field access" {
    const source =
        \\sorted : List(U64)
        \\sorted = [1, 2, 3, 4, 5]
        \\
        \\get : List(U64), U64 -> U64
        \\get = |array, idx| {
        \\    expect 3 >= sorted.len
        \\    array.get(idx).ok_or(0)
        \\}
    ;
    var env = try TestEnv.init("Test", source);
    defer env.deinit();

    try env.assertTypeErrorTitles(&.{"Type Mismatch"});
}
