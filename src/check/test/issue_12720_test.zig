//! Regression test for https://github.com/roc-lang/roc/issues/12720.
//!
//! Builtin type names are reserved module names: a module file named `Bool.roc`
//! must be rejected with an error, not accepted with a "builtin type shadowed"
//! warning.

const std = @import("std");
const TestEnv = @import("./TestEnv.zig");

fn expectReservedModuleNameError(module_name: []const u8, src: []const u8) TestEnv.TestEnvError!void {
    var test_env = try TestEnv.init(module_name, src);
    defer test_env.deinit();

    const diagnostics = try test_env.module_env.getDiagnostics();
    defer test_env.gpa.free(diagnostics);

    var error_count: usize = 0;
    for (diagnostics) |diagnostic| {
        var report = try test_env.module_env.diagnosticToReport(diagnostic, test_env.gpa, test_env.module_env.module_name);
        defer report.deinit();
        if (report.severity == .warning) {
            std.debug.print("module {s}: warning reported: {s}\n", .{ module_name, report.title });
        } else {
            error_count += 1;
        }
    }

    try std.testing.expect(error_count > 0);
}

test "issue 12720: a module named after the builtin Bool type is an error" {
    try expectReservedModuleNameError("Bool",
        \\Bool :: [].{
        \\    not = |s| "user not ${s}\n"
        \\    level = |s| "user level ${s}\n"
        \\}
    );
}

test "issue 12720: a module named after the builtin List type is an error" {
    try expectReservedModuleNameError("List",
        \\List :: { item : U8 }.{
        \\    get : List -> U8
        \\    get = |list| list.item
        \\}
    );
}
