//! Row subsumption for hosted functions (design.md "Row Subsumption").
//!
//! A hosted function's `Try` error row is closed by declaration, and every use
//! re-opens its own copy of it exactly as a use of a forwarding Roc function
//! does, so a caller may widen it through any channel. Its DIRECT result row
//! and its `Try` ok row are not coerced: lowering has no adapter for either at
//! a host boundary, so widening them stays an ordinary type mismatch. Each
//! accepted program is also lowered under both specialization strategies,
//! which stops the build if an extern is ever emitted at anything other than
//! its declared type (`requireHostedExternAtDeclaredAbi`).

const std = @import("std");
const base = @import("base");
const build_options = @import("build_options");
const collections = @import("collections");
const eval = @import("eval");
const lir = @import("lir");
const roc_target = @import("roc_target");

const Coordinator = @import("../coordinator.zig").Coordinator;
const CoreCtx = @import("ctx").CoreCtx;
const harness = @import("lower_to_lir_harness.zig");

const TestError = harness.LowerToLirHarnessError || error{SkipZigTest};

const platform_main =
    \\platform ""
    \\    requires {} { main! : () => U64 }
    \\    exposes [Host]
    \\    packages {}
    \\    provides { "result": result! }
    \\    hosted { "ok_row": Host.ok_row!, "probe": Host.probe!, "probe_alias": Host.probe_alias!, "tags": Host.tags! }
    \\
    \\import Host
    \\
    \\result! : () => U64
    \\result! = || main!()
;

const platform_module =
    \\IoResult(a) : Try(a, [HostErr(U64)])
    \\
    \\Host := [].{
    \\    probe! : U64 => Try(U64, [HostErr(U64)])
    \\    probe_alias! : U64 => IoResult(U64)
    \\    tags! : U64 => [Large, Small]
    \\    ok_row! : U64 => Try([Large, Small], [HostErr(U64)])
    \\}
;

const written_open_platform_main =
    \\platform ""
    \\    requires {} { main! : () => U64 }
    \\    exposes [Host]
    \\    packages {}
    \\    provides { "result": result! }
    \\    hosted { "probe": Host.probe! }
    \\
    \\import Host
    \\
    \\result! : () => U64
    \\result! = || main!()
;

const written_open_platform_module =
    \\Host := [].{
    \\    probe! : U64 => Try(U64, [HostErr(U64), ..])
    \\}
;

const PlatformFiles = struct {
    main: []const u8,
    module: []const u8,
};

const default_platform = PlatformFiles{ .main = platform_main, .module = platform_module };

const Workspace = struct {
    tmp_dir: std.testing.TmpDir,
    app_path: [:0]u8,

    fn init(app_source: []const u8, platform: PlatformFiles) TestError!Workspace {
        var tmp_dir = std.testing.tmpDir(.{});
        errdefer tmp_dir.cleanup();
        try tmp_dir.dir.createDirPath(std.testing.io, "platform");
        try tmp_dir.dir.writeFile(std.testing.io, .{ .sub_path = "main.roc", .data = app_source });
        try tmp_dir.dir.writeFile(std.testing.io, .{ .sub_path = "platform/main.roc", .data = platform.main });
        try tmp_dir.dir.writeFile(std.testing.io, .{ .sub_path = "platform/Host.roc", .data = platform.module });
        const app_path = try tmp_dir.dir.realPathFileAlloc(std.testing.io, "main.roc", std.testing.allocator);
        return .{ .tmp_dir = tmp_dir, .app_path = app_path };
    }

    fn deinit(self: *Workspace) void {
        std.testing.allocator.free(self.app_path);
        self.tmp_dir.cleanup();
    }
};

/// One report the check must produce: its title and the module it names.
const ExpectedReport = struct {
    title: []const u8,
    module_name: []const u8,
};

/// Checks the app and expects exactly `expected` reports, in any order.
fn expectReports(app_path: []const u8, expected: []const ExpectedReport) TestError!void {
    const gpa = std.testing.allocator;
    var arena_impl = collections.SingleThreadArena.init(gpa);
    defer arena_impl.deinit();
    const arena = arena_impl.allocator();
    var builtin_modules = try eval.BuiltinModules.init(gpa);
    defer builtin_modules.deinit();
    var coord = try Coordinator.init(
        gpa,
        .single_threaded,
        1,
        roc_target.RocTarget.detectNative(),
        &builtin_modules,
        build_options.compiler_version,
        null,
        CoreCtx.default(gpa, arena, std.testing.io),
    );
    defer coord.deinit();
    coord.enable_hosted_transform = true;

    try coord.start();
    try coord.discoverAppFromPath(arena, .{ .entry_path = app_path });
    try coord.coordinatorLoop();
    try coord.finishCheckedProgram(.none);

    var found = [_]bool{false} ** 4;
    std.debug.assert(expected.len <= found.len);
    var report_count: usize = 0;
    var reports = coord.iterReports();
    while (reports.next()) |entry| {
        report_count += 1;
        for (expected, 0..) |want, i| {
            if (found[i]) continue;
            if (!std.mem.eql(u8, entry.report.title, want.title)) continue;
            if (!std.mem.eql(u8, entry.module_name, want.module_name)) continue;
            found[i] = true;
            break;
        }
    }
    const matched = report_count == expected.len and for (found[0..expected.len]) |hit| {
        if (!hit) break false;
    } else true;
    if (!matched) {
        var all_reports = coord.iterReports();
        while (all_reports.next()) |entry| {
            std.debug.print("report: {s} in {s}\n", .{ entry.report.title, entry.module_name });
        }
    }
    try std.testing.expect(matched);
}

fn inspectNothing(_: *const lir.CheckedPipeline.LoweredProgram) harness.LowerToLirHarnessError!void {}

/// Checks the app cleanly, then lowers it under both specialization
/// strategies.
fn expectAccepted(app_source: []const u8) TestError!void {
    var workspace = try Workspace.init(app_source, default_platform);
    defer workspace.deinit();
    try expectReports(workspace.app_path, &.{});
    for ([_]base.SpecializationStrategy{ .lss, .boxy }) |strategy| {
        try harness.runAppPathLoweredInspection(workspace.app_path, .{ .specialization_strategy = strategy }, inspectNothing);
    }
}

fn expectRejected(app_source: []const u8, platform: PlatformFiles, expected: []const ExpectedReport) TestError!void {
    var workspace = try Workspace.init(app_source, platform);
    defer workspace.deinit();
    try expectReports(workspace.app_path, expected);
}

const app_mismatch = [_]ExpectedReport{.{ .title = "Type Mismatch", .module_name = "main" }};

test "hosted row subsumption: a hosted Try error row widens at an annotated binding" {
    try expectAccepted(
        \\app [main!] { pf: platform "./platform/main.roc" }
        \\import pf.Host
        \\
        \\main! : () => U64
        \\main! = || {
        \\    value : Try(U64, [HostErr(U64), Widened])
        \\    value = Host.probe!(1)
        \\    match value {
        \\        Ok(n) => n
        \\        Err(HostErr(n)) => n
        \\        Err(Widened) => 0
        \\    }
        \\}
    );
}

test "hosted row subsumption: a hosted Try error row declared through an alias widens" {
    try expectAccepted(
        \\app [main!] { pf: platform "./platform/main.roc" }
        \\import pf.Host
        \\
        \\main! : () => U64
        \\main! = || {
        \\    value : Try(U64, [HostErr(U64), Widened])
        \\    value = Host.probe_alias!(1)
        \\    match value {
        \\        Ok(n) => n
        \\        Err(HostErr(n)) => n
        \\        Err(Widened) => 0
        \\    }
        \\}
    );
}

test "hosted row subsumption: a hosted function named through an alias of its owner widens" {
    try expectAccepted(
        \\app [main!] { pf: platform "./platform/main.roc" }
        \\import pf.Host
        \\
        \\Named : Host
        \\
        \\main! : () => U64
        \\main! = || {
        \\    value : Try(U64, [HostErr(U64), Widened])
        \\    value = Named.probe!(1)
        \\    match value {
        \\        Ok(n) => n
        \\        Err(HostErr(n)) => n
        \\        Err(Widened) => 0
        \\    }
        \\}
    );
}

test "hosted row subsumption: a hosted function carried as a value widens" {
    try expectAccepted(
        \\app [main!] { pf: platform "./platform/main.roc" }
        \\import pf.Host
        \\
        \\call_wide! : (U64 => Try(U64, [HostErr(U64), Widened])), U64 => Try(U64, [HostErr(U64), Widened])
        \\call_wide! = |run!, n| run!(n)
        \\
        \\main! : () => U64
        \\main! = || {
        \\    boxed : Box((U64 => Try(U64, [HostErr(U64), Widened])))
        \\    boxed = Box.box(Host.probe!)
        \\    first = call_wide!(Host.probe!, 1)
        \\    second = Box.unbox(boxed)(2)
        \\    match (first, second) {
        \\        (Ok(a), Ok(b)) => a + b
        \\        _ => 0
        \\    }
        \\}
    );
}

test "hosted row subsumption: `?` on a hosted call in an unannotated function widens" {
    try expectAccepted(
        \\app [main!] { pf: platform "./platform/main.roc" }
        \\import pf.Host
        \\
        \\unwrap! = |n| Ok(Host.probe!(n)?)
        \\
        \\main! : () => U64
        \\main! = || {
        \\    value : Try(U64, [HostErr(U64), Widened])
        \\    value = unwrap!(1)
        \\    match value {
        \\        Ok(n) => n
        \\        Err(HostErr(n)) => n
        \\        Err(Widened) => 0
        \\    }
        \\}
    );
}

test "hosted row subsumption: a hosted direct result row does not widen" {
    try expectRejected(
        \\app [main!] { pf: platform "./platform/main.roc" }
        \\import pf.Host
        \\
        \\main! : () => U64
        \\main! = || {
        \\    value : [Huge, Large, Small]
        \\    value = Host.tags!(1)
        \\    match value {
        \\        Huge => 3
        \\        Large => 2
        \\        Small => 1
        \\    }
        \\}
    , default_platform, &app_mismatch);
}

test "hosted row subsumption: a hosted Try ok row does not widen" {
    try expectRejected(
        \\app [main!] { pf: platform "./platform/main.roc" }
        \\import pf.Host
        \\
        \\main! : () => U64
        \\main! = || {
        \\    value : Try([Huge, Large, Small], [HostErr(U64)])
        \\    value = Host.ok_row!(1)
        \\    match value {
        \\        Ok(Huge) => 3
        \\        Ok(Large) => 2
        \\        Ok(Small) => 1
        \\        Err(HostErr(n)) => n
        \\    }
        \\}
    , default_platform, &app_mismatch);
}

test "hosted row subsumption: a hosted error row written open is reported, not coerced" {
    // `..` is rejected at a host boundary, so the declaration is the one
    // error. The row is not one the annotation walk closed, so it records no
    // coercion, and no use tries to re-open a row that never ended in `[]`.
    try expectRejected(
        \\app [main!] { pf: platform "./platform/main.roc" }
        \\import pf.Host
        \\
        \\main! : () => U64
        \\main! = || {
        \\    value : Try(U64, [HostErr(U64), Widened])
        \\    value = Host.probe!(1)
        \\    match value {
        \\        Ok(n) => n
        \\        Err(HostErr(n)) => n
        \\        Err(Widened) => 0
        \\    }
        \\}
    , .{ .main = written_open_platform_main, .module = written_open_platform_module }, &.{
        .{ .title = "Host Boundary Requires Closed Rows", .module_name = "Host" },
    });
}
