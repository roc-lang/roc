//! Regression test for https://github.com/roc-lang/roc/issues/11863.

const import_cycle = @import("import_cycle_harness.zig");

test "issue 11863: app importing a module cycle finishes checking with an import-cycle report" {
    try import_cycle.expectImportCycleReport(&.{
        .{
            .sub_path = "main.roc",
            .data =
            \\app [main!] { pf: platform "./.roc_test_platform/main.roc" }
            \\
            \\import A
            \\
            \\main! = |_| {
            \\    Ok({})
            \\}
            ,
        },
        .{
            .sub_path = "A.roc",
            .data =
            \\module [f]
            \\
            \\import B
            \\
            \\f : U64 -> U64
            \\f = |x| B.g(x) + 1
            ,
        },
        .{
            .sub_path = "B.roc",
            .data =
            \\module [g]
            \\
            \\import A
            \\
            \\g : U64 -> U64
            \\g = |x| A.f(x)
            ,
        },
    });
}
