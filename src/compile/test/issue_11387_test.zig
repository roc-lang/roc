//! Regression tests for issue #11387.
//!
//! Every function the host provides is effectful. A hosted declaration typed
//! with `->` instead of `=>` is reported as an error at the declaration, and
//! every use of it is code generated as a crash, so no compile-time root can
//! ever reach a host call. A hosted declaration's type variables may appear
//! only inside a `Box`. The same programs written with `=>` check cleanly and
//! run against a host under both specialization strategies, with every value
//! in a type variable slot crossing the host boundary intact.

const std = @import("std");
const base = @import("base");
const build_options = @import("build_options");
const collections = @import("collections");
const builtins = @import("builtins");
const eval = @import("eval");
const lir = @import("lir");
const reporting = @import("reporting");
const roc_target = @import("roc_target");

const Coordinator = @import("../coordinator.zig").Coordinator;
const CoreCtx = @import("ctx").CoreCtx;
const harness = @import("lower_to_lir_harness.zig");

const TestError = harness.LowerToLirHarnessError || error{SkipZigTest};
const HostRunError = std.mem.Allocator.Error || eval.LirInterpreter.Error || eval.RuntimeHostEnv.LeakError || error{ TestUnexpectedResult, TestExpectedEqual };

const erased_platform_main =
    \\platform ""
    \\    requires {} { main : () -> U64 }
    \\    exposes [Erased]
    \\    packages {}
    \\    provides { "result": result }
    \\    hosted { "erase": Erased.erase!, "restore": Erased.restore! }
    \\
    \\import Erased
    \\
    \\result : () -> U64
    \\result = || main()
;

const erased_platform_module =
    \\Erased := Box((() -> {})).{
    \\    erase! : Box((() -> a)) -> Erased
    \\    restore! : Erased -> Box((() -> a))
    \\}
;

const host_platform_main =
    \\platform ""
    \\    requires {} { main : () -> U64 }
    \\    exposes [Host]
    \\    packages {}
    \\    provides { "result": result }
    \\    hosted { "twice": Host.twice! }
    \\
    \\import Host
    \\
    \\result : () -> U64
    \\result = || main()
;

const host_platform_module =
    \\Host := [].{
    \\    twice! : U64 -> U64
    \\}
;

const effectful_erased_platform_main =
    \\platform ""
    \\    requires {} { main! : () => U64 }
    \\    exposes [Erased]
    \\    packages {}
    \\    provides { "result": result! }
    \\    hosted { "erase": Erased.erase!, "restore": Erased.restore!, "stash": Erased.stash!, "unstash": Erased.unstash! }
    \\
    \\import Erased
    \\
    \\result! : () => U64
    \\result! = || main!()
;

const effectful_erased_platform_module =
    \\Erased := Box((() -> {})).{
    \\    erase! : Box((() -> a)) => Erased
    \\    restore! : Erased => Box((() -> a))
    \\    stash! : Box(a) => Erased
    \\    unstash! : Erased => Box(a)
    \\}
;

const effectful_host_platform_main =
    \\platform ""
    \\    requires {} { main! : () => U64 }
    \\    exposes [Host]
    \\    packages {}
    \\    provides { "result": result! }
    \\    hosted { "twice": Host.twice! }
    \\
    \\import Host
    \\
    \\result! : () => U64
    \\result! = || main!()
;

const effectful_host_platform_module =
    \\Host := [].{
    \\    twice! : U64 => U64
    \\}
;

const PlatformFiles = struct {
    main: []const u8,
    module_path: []const u8,
    module: []const u8,
};

const Workspace = struct {
    tmp_dir: std.testing.TmpDir,
    app_path: [:0]u8,

    fn init(app_source: []const u8, platform: PlatformFiles) TestError!Workspace {
        var tmp_dir = std.testing.tmpDir(.{});
        errdefer tmp_dir.cleanup();
        try tmp_dir.dir.createDirPath(std.testing.io, "platform");
        try tmp_dir.dir.writeFile(std.testing.io, .{ .sub_path = "main.roc", .data = app_source });
        try tmp_dir.dir.writeFile(std.testing.io, .{ .sub_path = "platform/main.roc", .data = platform.main });
        try tmp_dir.dir.writeFile(std.testing.io, .{ .sub_path = platform.module_path, .data = platform.module });
        const app_path = try tmp_dir.dir.realPathFileAlloc(std.testing.io, "main.roc", std.testing.allocator);
        return .{ .tmp_dir = tmp_dir, .app_path = app_path };
    }

    fn deinit(self: *Workspace) void {
        std.testing.allocator.free(self.app_path);
        self.tmp_dir.cleanup();
    }
};

const ReportExpectation = struct {
    /// Module whose rejected hosted declarations must each be reported once.
    declaring_module: []const u8 = "",
    /// One-based line of each rejected hosted declaration in `declaring_module`.
    declaration_lines: []const u32 = &.{},
};

/// Checks the app and expects exactly one error report per listed hosted
/// declaration, each at its declaration line in its declaring module, and no
/// other report. In particular no use of those declarations may be evaluated at
/// compile time, and an app with no listed declarations must check cleanly.
fn expectReports(app_path: []const u8, expectation: ReportExpectation) TestError!void {
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

    const lines = expectation.declaration_lines;
    var report_count: usize = 0;
    var declaration_error_counts = [_]usize{0} ** 8;
    std.debug.assert(lines.len <= declaration_error_counts.len);
    var reports = coord.iterReports();
    while (reports.next()) |entry| {
        report_count += 1;
        if (entry.report.severity != reporting.Severity.runtime_error) continue;
        if (!std.mem.eql(u8, entry.module_name, expectation.declaring_module)) continue;
        const region = entry.report.getRegionInfo() orelse continue;
        for (lines, 0..) |line, i| {
            if (region.start_line_idx == line) declaration_error_counts[i] += 1;
        }
    }
    const reported_exactly = report_count == lines.len and
        for (declaration_error_counts[0..lines.len]) |count| {
            if (count != 1) break false;
        } else true;
    if (!reported_exactly) {
        var all_reports = coord.iterReports();
        while (all_reports.next()) |entry| {
            std.debug.print("report: {s} in {s}\n", .{ entry.report.title, entry.module_name });
        }
    }
    try std.testing.expect(reported_exactly);
}

fn expectHostedDeclarationsRejected(
    app_source: []const u8,
    platform: PlatformFiles,
    expectation: ReportExpectation,
) TestError!void {
    var workspace = try Workspace.init(app_source, platform);
    defer workspace.deinit();
    try expectReports(workspace.app_path, expectation);
}

// Host implementations for the `=>` platforms. Every `Erased` function hands
// back the box it receives, so the host's ownership of the argument becomes the
// caller's ownership of the result and their dispatch order is immaterial.
fn hostPassBox(box: ?*anyopaque) callconv(.c) ?*anyopaque {
    return box;
}

fn hostTwice(n: u64) callconv(.c) u64 {
    return n * 2;
}

const erased_hosts = [_]builtins.host_abi.HostedFn{builtins.host_abi.hostedFn(&hostPassBox)} ** 4;

var expected_result: u64 = 0;
var host_functions: []const builtins.host_abi.HostedFn = &.{};

fn runRootAgainstHost(lowered: *const lir.CheckedPipeline.LoweredProgram) harness.LowerToLirHarnessError!void {
    runRoot(lowered) catch |err| {
        std.log.err("running the root against the host failed: {s}", .{@errorName(err)});
        return error.TestUnexpectedResult;
    };
}

fn runRoot(lowered: *const lir.CheckedPipeline.LoweredProgram) HostRunError!void {
    var host = eval.RuntimeHostEnv.init(std.testing.allocator);
    defer host.deinit();
    var hosted_fns: [4]builtins.host_abi.HostedFn = undefined;
    std.debug.assert(host_functions.len <= hosted_fns.len);
    @memcpy(hosted_fns[0..host_functions.len], host_functions);
    const ops = host.get_ops();
    ops.hosted_fns = .{ .count = @intCast(host_functions.len), .fns = &hosted_fns };
    const program = &lowered.lir_result;
    var interpreter = try eval.LirInterpreter.initWithBoxyTables(std.testing.allocator, &program.store, &program.layouts, eval.LirInterpreter.BoxyTables.fromResult(program), ops);
    defer interpreter.deinit();
    try std.testing.expectEqual(@as(usize, 1), program.root_procs.items.len);
    const root_id = program.root_procs.items[0];
    const root = program.store.getProcSpec(root_id);
    var result: u64 = 0;
    _ = try interpreter.eval(.{ .proc_id = root_id, .ret_layout = root.ret_layout, .ret_ptr = @ptrCast(&result) });
    try std.testing.expectEqual(expected_result, result);
    try host.checkForLeaks();
}

/// Checks the app cleanly, then lowers it with each specialization strategy
/// and runs its root against `hosts`, expecting `expected`.
fn expectEffectfulHostedAppRuns(
    app_source: []const u8,
    platform: PlatformFiles,
    hosts: []const builtins.host_abi.HostedFn,
    expected: u64,
) TestError!void {
    var workspace = try Workspace.init(app_source, platform);
    defer workspace.deinit();
    try expectReports(workspace.app_path, .{});
    if (@sizeOf(usize) != 8) return error.SkipZigTest;
    expected_result = expected;
    host_functions = hosts;
    for ([_]base.SpecializationStrategy{ .lss, .boxy }) |strategy| {
        try harness.runAppPathLoweredInspection(workspace.app_path, .{ .specialization_strategy = strategy, .target_usize = .u64 }, runRootAgainstHost);
    }
}

test "issue 11387: constant call round-tripping a boxed getter through pure hosted functions is rejected at the hosted declaration" {
    try expectHostedDeclarationsRejected(
        \\app [main] { pf: platform "./platform/main.roc" }
        \\import pf.Erased
        \\
        \\roundtrip : U64 -> U64
        \\roundtrip = |n| {
        \\    hidden = Erased.erase!(Box.box(|| n))
        \\    getter : Box((() -> U64))
        \\    getter = Erased.restore!(hidden)
        \\    (Box.unbox(getter))()
        \\}
        \\
        \\main : () -> U64
        \\main = || roundtrip(41)
    , .{ .main = erased_platform_main, .module_path = "platform/Erased.roc", .module = erased_platform_module }, .{ .declaring_module = "Erased", .declaration_lines = &.{ 2, 3 } });
}

test "issue 11387: direct constant-argument call of a pure hosted function is rejected at the hosted declaration" {
    try expectHostedDeclarationsRejected(
        \\app [main] { pf: platform "./platform/main.roc" }
        \\import pf.Host
        \\
        \\main : () -> U64
        \\main = || Host.twice!(21)
    , .{ .main = host_platform_main, .module_path = "platform/Host.roc", .module = host_platform_module }, .{ .declaring_module = "Host", .declaration_lines = &.{2} });
}

test "issue 11387: pure hosted function passed as a value to a constant-argument call is rejected at the hosted declaration" {
    try expectHostedDeclarationsRejected(
        \\app [main] { pf: platform "./platform/main.roc" }
        \\import pf.Host
        \\
        \\apply : (U64 -> U64), U64 -> U64
        \\apply = |f, n| f(n)
        \\
        \\main : () -> U64
        \\main = || apply(Host.twice!, 21)
    , .{ .main = host_platform_main, .module_path = "platform/Host.roc", .module = host_platform_module }, .{ .declaring_module = "Host", .declaration_lines = &.{2} });
}

test "issue 11387: effectful round-trip of a boxed getter through hosted functions checks and runs" {
    try expectEffectfulHostedAppRuns(
        \\app [main!] { pf: platform "./platform/main.roc" }
        \\import pf.Erased
        \\
        \\roundtrip! : U64 => U64
        \\roundtrip! = |n| {
        \\    hidden = Erased.erase!(Box.box(|| n))
        \\    getter : Box((() -> U64))
        \\    getter = Erased.restore!(hidden)
        \\    (Box.unbox(getter))()
        \\}
        \\
        \\main! : () => U64
        \\main! = || roundtrip!(41)
    , .{ .main = effectful_erased_platform_main, .module_path = "platform/Erased.roc", .module = effectful_erased_platform_module }, &erased_hosts, 41);
}

test "issue 11387: direct constant-argument call of an effectful hosted function checks and runs" {
    try expectEffectfulHostedAppRuns(
        \\app [main!] { pf: platform "./platform/main.roc" }
        \\import pf.Host
        \\
        \\main! : () => U64
        \\main! = || Host.twice!(21)
    , .{ .main = effectful_host_platform_main, .module_path = "platform/Host.roc", .module = effectful_host_platform_module }, &.{builtins.host_abi.hostedFn(&hostTwice)}, 42);
}

test "issue 11387: effectful hosted function passed as a value to a constant-argument call checks and runs" {
    try expectEffectfulHostedAppRuns(
        \\app [main!] { pf: platform "./platform/main.roc" }
        \\import pf.Host
        \\
        \\apply! : (U64 => U64), U64 => U64
        \\apply! = |f!, n| f!(n)
        \\
        \\main! : () => U64
        \\main! = || apply!(Host.twice!, 21)
    , .{ .main = effectful_host_platform_main, .module_path = "platform/Host.roc", .module = effectful_host_platform_module }, &.{builtins.host_abi.hostedFn(&hostTwice)}, 42);
}

test "issue 11387: effectful round-trip of a capture-free boxed getter through hosted functions checks and runs" {
    try expectEffectfulHostedAppRuns(
        \\app [main!] { pf: platform "./platform/main.roc" }
        \\import pf.Erased
        \\
        \\main! : () => U64
        \\main! = || {
        \\    hidden = Erased.erase!(Box.box(|| 41.U64))
        \\    getter : Box((() -> U64))
        \\    getter = Erased.restore!(hidden)
        \\    (Box.unbox(getter))()
        \\}
    , .{ .main = effectful_erased_platform_main, .module_path = "platform/Erased.roc", .module = effectful_erased_platform_module }, &erased_hosts, 41);
}

test "issue 11387: effectful round-trip of a boxed value through hosted functions checks and runs" {
    try expectEffectfulHostedAppRuns(
        \\app [main!] { pf: platform "./platform/main.roc" }
        \\import pf.Erased
        \\
        \\roundtrip! : U64 => U64
        \\roundtrip! = |n| {
        \\    hidden = Erased.stash!(Box.box(n))
        \\    boxed : Box(U64)
        \\    boxed = Erased.unstash!(hidden)
        \\    Box.unbox(boxed)
        \\}
        \\
        \\main! : () => U64
        \\main! = || roundtrip!(41)
    , .{ .main = effectful_erased_platform_main, .module_path = "platform/Erased.roc", .module = effectful_erased_platform_module }, &erased_hosts, 41);
}

test "issue 11387: generic caller round-trips a boxed getter through hosted functions and runs" {
    try expectEffectfulHostedAppRuns(
        \\app [main!] { pf: platform "./platform/main.roc" }
        \\import pf.Erased
        \\
        \\hide! : a => Erased
        \\hide! = |value| Erased.erase!(Box.box(|| value))
        \\
        \\main! : () => U64
        \\main! = || {
        \\    getter : Box((() -> U64))
        \\    getter = Erased.restore!(hide!(41.U64))
        \\    (Box.unbox(getter))()
        \\}
    , .{ .main = effectful_erased_platform_main, .module_path = "platform/Erased.roc", .module = effectful_erased_platform_module }, &erased_hosts, 41);
}

test "issue 11387: hosted type variables must be boxed so one C signature covers every use" {
    try expectHostedDeclarationsRejected(
        \\app [main!] { pf: platform "./platform/main.roc" }
        \\
        \\main! : () => U64
        \\main! = || 0
    , .{
        .main =
        \\platform ""
        \\    requires {} { main! : () => U64 }
        \\    exposes [Host]
        \\    packages {}
        \\    provides { "result": result! }
        \\    hosted { "ident": Host.ident!, "idents": Host.idents!, "stash": Host.stash! }
        \\
        \\import Host
        \\
        \\result! : () => U64
        \\result! = || main!()
        ,
        .module_path = "platform/Host.roc",
        .module =
        \\Host := [].{
        \\    ident! : a => a
        \\    idents! : List(a) => List(a)
        \\    Stash(a) := Box(a)
        \\    stash! : Box(a) => Stash(a)
        \\}
        ,
    }, .{ .declaring_module = "Host", .declaration_lines = &.{ 2, 3 } });
}

test "issue 11387: generic hosted function passed as a value round-trips a boxed value and runs" {
    try expectEffectfulHostedAppRuns(
        \\app [main!] { pf: platform "./platform/main.roc" }
        \\import pf.Erased
        \\
        \\apply! : (Box(U64) => Erased), Box(U64) => Erased
        \\apply! = |f!, boxed| f!(boxed)
        \\
        \\main! : () => U64
        \\main! = || {
        \\    unboxed : Box(U64)
        \\    unboxed = Erased.unstash!(apply!(Erased.stash!, Box.box(41)))
        \\    Box.unbox(unboxed)
        \\}
    , .{ .main = effectful_erased_platform_main, .module_path = "platform/Erased.roc", .module = effectful_erased_platform_module }, &erased_hosts, 41);
}

test "issue 11387: nominal whose type variable is boxed round-trips through a hosted function and runs" {
    try expectEffectfulHostedAppRuns(
        \\app [main!] { pf: platform "./platform/main.roc" }
        \\import pf.Stash
        \\
        \\main! : () => U64
        \\main! = || Stash.unwrap(Stash.pass!(Stash.wrap(41.U64)))
    , .{
        .main =
        \\platform ""
        \\    requires {} { main! : () => U64 }
        \\    exposes [Stash]
        \\    packages {}
        \\    provides { "result": result! }
        \\    hosted { "pass": Stash.pass! }
        \\
        \\import Stash
        \\
        \\result! : () => U64
        \\result! = || main!()
        ,
        .module_path = "platform/Stash.roc",
        .module =
        \\Stash(a) := Box(a).{
        \\    wrap : a -> Stash(a)
        \\    wrap = |value| Stash.(Box.box(value))
        \\
        \\    unwrap : Stash(a) -> a
        \\    unwrap = |Stash.(boxed)| Box.unbox(boxed)
        \\
        \\    pass! : Stash(a) => Stash(a)
        \\}
        ,
    }, &.{builtins.host_abi.hostedFn(&hostPassBox)}, 41);
}

test "issue 11387: generic function over a nominal whose type variable is boxed runs" {
    try expectEffectfulHostedAppRuns(
        \\app [main!] { pf: platform "./platform/main.roc" }
        \\import pf.Stash
        \\
        \\main! : () => U64
        \\main! = || Stash.unwrap(Stash.same(Stash.wrap(41.U64)))
    , .{
        .main =
        \\platform ""
        \\    requires {} { main! : () => U64 }
        \\    exposes [Stash]
        \\    packages {}
        \\    provides { "result": result! }
        \\    hosted { "pass": Stash.pass! }
        \\
        \\import Stash
        \\
        \\result! : () => U64
        \\result! = || main!()
        ,
        .module_path = "platform/Stash.roc",
        .module =
        \\Stash(a) := Box(a).{
        \\    wrap : a -> Stash(a)
        \\    wrap = |value| Stash.(Box.box(value))
        \\
        \\    unwrap : Stash(a) -> a
        \\    unwrap = |Stash.(boxed)| Box.unbox(boxed)
        \\
        \\    pass! : Stash(a) => Stash(a)
        \\
        \\    same : Stash(a) -> Stash(a)
        \\    same = |stash| stash
        \\}
        ,
    }, &.{builtins.host_abi.hostedFn(&hostPassBox)}, 41);
}

test "issue 11387: generic caller of a generic function over a boxed-variable nominal runs" {
    try expectEffectfulHostedAppRuns(
        \\app [main!] { pf: platform "./platform/main.roc" }
        \\import pf.Stash
        \\
        \\forward : Stash(b) -> Stash(b)
        \\forward = |stash| Stash.same(stash)
        \\
        \\main! : () => U64
        \\main! = || Stash.unwrap(forward(Stash.wrap(41.U64)))
    , .{
        .main =
        \\platform ""
        \\    requires {} { main! : () => U64 }
        \\    exposes [Stash]
        \\    packages {}
        \\    provides { "result": result! }
        \\    hosted { "pass": Stash.pass! }
        \\
        \\import Stash
        \\
        \\result! : () => U64
        \\result! = || main!()
        ,
        .module_path = "platform/Stash.roc",
        .module =
        \\Stash(a) := Box(a).{
        \\    wrap : a -> Stash(a)
        \\    wrap = |value| Stash.(Box.box(value))
        \\
        \\    unwrap : Stash(a) -> a
        \\    unwrap = |Stash.(boxed)| Box.unbox(boxed)
        \\
        \\    pass! : Stash(a) => Stash(a)
        \\
        \\    same : Stash(a) -> Stash(a)
        \\    same = |stash| stash
        \\}
        ,
    }, &.{builtins.host_abi.hostedFn(&hostPassBox)}, 41);
}
