//! Regression test for issue 10338.

const TestEnv = @import("./TestEnv.zig");

test "issue 10338: forward referenced binding with out of order annotations does not panic checked artifact publication" {
    const source =
        \\t = f()
        \\f : U
        \\m : U
        \\f = () != {}
    ;

    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();
}
