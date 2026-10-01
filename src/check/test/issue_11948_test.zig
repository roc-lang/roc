//! Regression tests for https://github.com/roc-lang/roc/issues/11948.
//!
//! A literal's type suffix naming a builtin type resolves to that type's
//! declaration, so a suffix type without `from_numeral` is a type error rather
//! than silently checking as an erroneous type that crashes at runtime.
const TestEnv = @import("TestEnv.zig");

fn expectMissingFromNumeral(comptime source: []const u8) !void {
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();
    try test_env.assertOneTypeError("Missing Method");
}

test "issue 11948: numeric literal suffixed with List is a type error" {
    try expectMissingFromNumeral("page_size = 20.List");
}

test "issue 11948: numeric literal suffixed with Box is a type error" {
    try expectMissingFromNumeral("page_size = 20.Box");
}

test "issue 11948: numeric literal suffixed with Json is a type error" {
    try expectMissingFromNumeral("page_size = 20.Json");
}
