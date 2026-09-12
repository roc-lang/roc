//! Regression tests for issue #11311: a generic annotated value whose body is a
//! bare reference to another generic function must check like its eta-expanded
//! form, instead of failing checked-artifact publication while resolving the
//! forwarding site's dispatch plan.
//! repro for https://github.com/roc-lang/roc/issues/11311

const std = @import("std");
const roc_target = @import("roc_target");
const compile_build = @import("../compile_build.zig");
const BuildEnv = compile_build.BuildEnv;

const SourceFile = struct {
    name: []const u8,
    data: []const u8,
};

const ForwardingError = compile_build.InitError || compile_build.BuildRootError ||
    std.Io.Dir.WriteFileError || std.Io.Dir.RealPathFileAllocError ||
    error{ TestExpectedEqual, TestUnexpectedResult };

/// Check `root` with its siblings on disk and require that checking reports no errors.
fn expectChecksWithoutErrors(files: []const SourceFile, root: []const u8) ForwardingError!void {
    const gpa = std.testing.allocator;
    const io = std.testing.io;
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    for (files) |file| {
        try tmp_dir.dir.writeFile(io, .{ .sub_path = file.name, .data = file.data });
    }

    const cwd = try tmp_dir.dir.realPathFileAlloc(io, ".", gpa);
    defer gpa.free(cwd);
    const root_path = try tmp_dir.dir.realPathFileAlloc(io, root, gpa);
    defer gpa.free(root_path);

    var build_env = try BuildEnv.init(gpa, .single_threaded, 1, roc_target.RocTarget.detectNative(), cwd, io);
    defer build_env.deinit();
    try build_env.build(root_path);

    const drained = try build_env.drainReports();
    defer build_env.freeDrainedReports(drained);
    for (drained) |module_reports| {
        for (module_reports.reports) |report| {
            if (report.severity != .warning) std.debug.print("unexpected report: {s}\n", .{report.title});
            try std.testing.expectEqual(.warning, report.severity);
        }
    }
}

test "issue 11311: a generic value forwarding a sibling generic function checks" {
    const factory_source =
        \\Model(a) := { parse : Str -> Try(a, Str), render : a -> Str }.{
        \\    define : (Str -> Try(a, Str)), (a -> Str) -> Model(a)
        \\    define = |parse, render| Model.{ parse, render }
        \\}
        \\
        \\Factory :: [].{
        \\    build : (Str -> Try(a, Str)), (a -> Str) -> Model(a)
        \\    build = Model.define
        \\}
        \\
    ;
    try expectChecksWithoutErrors(&.{
        .{ .name = "Factory.roc", .data = factory_source },
    }, "Factory.roc");
}

test "issue 11311: a generic value forwarding an imported generic function checks" {
    const app_source =
        \\app [main!] { pf: platform "./platform.roc" }
        \\
        \\import pf.Model as M
        \\
        \\Factory :: [].{
        \\    build : Str, Str, (Str -> Try(a, Str)), (a -> Str) -> M(a)
        \\    build = M.define
        \\}
        \\
        \\main! : List(Str) => Try({}, [Exit(I8), ..])
        \\main! = |_args| Ok({})
        \\
    ;
    const platform_source =
        \\platform ""
        \\    requires {} { main! : List(Str) => Try({}, [Exit(I8), ..]) }
        \\    exposes [Model]
        \\    packages {}
        \\    provides { "roc_main": main_for_host! }
        \\
        \\import Model
        \\
        \\main_for_host! : List(Str) => I8
        \\main_for_host! = |args| match main!(args) {
        \\    Ok({}) => 0
        \\    Err(Exit(code)) => code
        \\    Err(_) => 1
        \\}
        \\
    ;
    const model_source =
        \\Model(a) := { name : Str, description : Str, parse : Str -> Try(a, Str), render : a -> Str }.{
        \\    define : Str, Str, (Str -> Try(a, Str)), (a -> Str) -> Model(a)
        \\    define = |name, description, parse, render| Model.{ name, description, parse, render }
        \\}
        \\
    ;
    try expectChecksWithoutErrors(&.{
        .{ .name = "ForwardedFactory.roc", .data = app_source },
        .{ .name = "platform.roc", .data = platform_source },
        .{ .name = "Model.roc", .data = model_source },
    }, "ForwardedFactory.roc");
}

test "issue 11311: forwarded factories lower at independent types through LSS" {
    const source =
        \\Model(a) := { parse : Str -> Try(a, Str), render : a -> Str }.{
        \\    define : (Str -> Try(a, Str)), (a -> Str) -> Model(a)
        \\    define = |parse, render| Model.{ parse, render }
        \\    run : Model(a), Str -> Str
        \\    run = |Model.{parse, render}, text| match parse(text) {
        \\        Ok(value) => render(value)
        \\        Err(message) => message
        \\    }
        \\}
        \\Factory :: [].{
        \\    build : (Str -> Try(a, Str)), (a -> Str) -> Model(a)
        \\    build = Model.define
        \\}
        \\main! = |args| {
        \\    text = match args.first() {
        \\        Ok(value) => value
        \\        Err(_) => ""
        \\    }
        \\    number_model = Factory.build(|s| Ok(s.count_utf8_bytes()), |_n| "number")
        \\    text_model = Factory.build(|s| Ok(s), |s| s)
        \\    echo!(Model.run(number_model, text))
        \\    echo!(Model.run(text_model, text))
        \\    Ok({})
        \\}
    ;
    const harness = @import("lower_to_lir_harness.zig");
    try harness.expectLowersToLirWithOptions(source, .{ .specialization_strategy = .lss });
}
