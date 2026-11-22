//! Regression test for issue #13347.

const std = @import("std");
const base = @import("base");
const harness = @import("lower_to_lir_harness.zig");

test "issue 13347: platform nominal backed by a for-clause abstract type checks and lowers" {
    // Repro for https://github.com/roc-lang/roc/issues/13347.
    // The platform has no imports, so the for-clause binder is the first
    // thing canonicalized in the module. A nominal declaration whose backing
    // is that abstract type must publish and lower like any other.
    const gpa = std.testing.allocator;
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    try tmp_dir.dir.createDirPath(std.testing.io, "platform");
    try tmp_dir.dir.writeFile(std.testing.io, .{
        .sub_path = "app.roc",
        .data =
        \\app [main] { pf: platform "./platform/main.roc" }
        \\
        \\main : U64 -> U64
        \\main = |n| n
        ,
    });
    try tmp_dir.dir.writeFile(std.testing.io, .{
        .sub_path = "platform/main.roc",
        .data =
        \\platform ""
        \\    requires {
        \\        [Model : model] for main : model -> model
        \\    }
        \\    exposes []
        \\    packages {}
        \\    provides { "entry": entry }
        \\
        \\S := Model
        \\
        \\entry : U64 -> U64
        \\entry = |n| n
        ,
    });

    const app_path = try tmp_dir.dir.realPathFileAlloc(std.testing.io, "app.roc", gpa);
    defer gpa.free(app_path);

    try harness.expectAppPathLowersToLir(app_path);
}

test "issue 13347: provided function returning a platform nominal over a for-clause abstract type lowers" {
    // Repro for the follow-on in https://github.com/roc-lang/roc/issues/13347.
    // The platform has an import, and its provided function returns a nominal
    // whose backing mentions the for-clause abstract type. The app's inferred
    // type for that binder must reach the nominal's backing when lowering.
    const gpa = std.testing.allocator;
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    try tmp_dir.dir.createDirPath(std.testing.io, "platform");
    try tmp_dir.dir.writeFile(std.testing.io, .{
        .sub_path = "app.roc",
        .data =
        \\app [main] { pf: platform "./platform/main.roc" }
        \\
        \\Model : U64
        \\
        \\main = 0
        ,
    });
    try tmp_dir.dir.writeFile(std.testing.io, .{
        .sub_path = "platform/Extra.roc",
        .data =
        \\Extra := [Extra]
        ,
    });
    try tmp_dir.dir.writeFile(std.testing.io, .{
        .sub_path = "platform/main.roc",
        .data =
        \\platform ""
        \\    requires {
        \\        [Model : model] for main : model
        \\    }
        \\    exposes [Extra]
        \\    packages {}
        \\    provides { "entry": entry }
        \\
        \\import Extra
        \\
        \\S := { model : Model }
        \\
        \\entry : {} -> S
        \\entry = |{}| { model: main }
        ,
    });

    const app_path = try tmp_dir.dir.realPathFileAlloc(std.testing.io, "app.roc", gpa);
    defer gpa.free(app_path);

    try harness.expectAppPathLowersToLir(app_path);
}

/// Lower a platform whose declarations depend on the abstract type through
/// an alias (`Inner`), beside a declared parameter (`S(a)`), and through
/// another nominal (`T`), and which dispatches to a method of one.
fn expectNestedPlatformNominalsLower(strategy: base.SpecializationStrategy) harness.LowerToLirHarnessError!void {
    const gpa = std.testing.allocator;
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    try tmp_dir.dir.createDirPath(std.testing.io, "platform");
    try tmp_dir.dir.writeFile(std.testing.io, .{
        .sub_path = "app.roc",
        .data =
        \\app [main] { pf: platform "./platform/main.roc" }
        \\
        \\main : Str
        \\main = "a string long enough to live on the heap"
        ,
    });
    try tmp_dir.dir.writeFile(std.testing.io, .{
        .sub_path = "platform/main.roc",
        .data =
        \\platform ""
        \\    requires {
        \\        [Model : model] for main : model
        \\    }
        \\    exposes []
        \\    packages {}
        \\    provides { "entry": entry }
        \\
        \\Inner : { model : Model }
        \\
        \\S(a) := { inner : Inner, extra : a }.{
        \\    get_model : S(a) -> Model
        \\    get_model = |s| s.inner.model
        \\}
        \\
        \\T := [Wrapped(S(U64))]
        \\
        \\make : U64 -> S(U64)
        \\make = |extra| { inner: { model: main }, extra }
        \\
        \\rebuild : S(U64) -> S(U64)
        \\rebuild = |s| { inner: { model: s.get_model() }, extra: s.extra }
        \\
        \\entry : U64 -> T
        \\entry = |n| Wrapped(rebuild(make(n)))
        ,
    });

    const app_path = try tmp_dir.dir.realPathFileAlloc(std.testing.io, "app.roc", gpa);
    defer gpa.free(app_path);

    try harness.expectAppPathLowersToLirWithOptions(app_path, .{ .specialization_strategy = strategy });
}

test "issue 13347: platform nominals reach a for-clause abstract type through parameters, aliases and other nominals" {
    try expectNestedPlatformNominalsLower(.lss);
}

test "issue 13347: platform nominals over a for-clause abstract type lower without specialization" {
    try expectNestedPlatformNominalsLower(.boxy);
}
