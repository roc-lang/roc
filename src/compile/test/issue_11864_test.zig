//! Regression test for https://github.com/roc-lang/roc/issues/11864.

const import_cycle = @import("import_cycle_harness.zig");

test "issue 11864: app importing a self-importing module finishes checking with an import-cycle report" {
    try import_cycle.expectImportCycleReport(&.{
        .{
            .sub_path = "main.roc",
            .data =
            \\app [main!] { pf: platform "./.roc_test_platform/main.roc" }
            \\
            \\import A exposing [A]
            \\
            \\main! = |_args| {
            \\    _ = A.foo(A.{ x: 1 })
            \\    Ok({})
            \\}
            ,
        },
        .{
            .sub_path = "A.roc",
            .data =
            \\import A exposing [A]
            \\
            \\A := { x : U64 }
            \\
            \\foo : A -> U64
            \\foo = |a| a.x
            ,
        },
    });
}
