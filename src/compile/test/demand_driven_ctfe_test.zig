//! Checking specializes a runtime body for compile-time evaluation only where
//! that body can register a literal root (design.md "Demand-Driven
//! Compile-Time Specialization"), while still evaluating and reporting every
//! literal conversion a build would.

const std = @import("std");
const base = @import("base");
const build_options = @import("build_options");
const eval = @import("eval");
const lir = @import("lir");
const roc_target = @import("roc_target");
const CoreCtx = @import("ctx").CoreCtx;
const Coordinator = @import("../coordinator.zig").Coordinator;
const is_freestanding = @import("../threading.zig").is_freestanding;

const SourceFile = struct { path: []const u8, source: []const u8 };

const platform_files = [_]SourceFile{
    .{ .path = ".roc_echo_platform/main.roc", .source =
    \\platform ""
    \\    requires {} { main! : List(Str) => Try({}, [Exit(I8), ..]) }
    \\    exposes [Echo]
    \\    packages {}
    \\    provides { "roc_main": main_for_host! }
    \\    hosted { "roc_echo_line": Echo.line! }
    \\import Echo
    \\main_for_host! : List(Str) => I8
    \\main_for_host! = |args|
    \\    match main!(args) {
    \\        Ok({}) => 0
    \\        Err(Exit(code)) => code
    \\        Err(other) => {
    \\            Echo.line!("Program exited with error: ${Str.inspect(other)}")
    \\            1
    \\        }
    \\    }
    },
    .{ .path = ".roc_echo_platform/Echo.roc", .source =
    \\Echo := [].{
    \\    line! : Str => {}
    \\}
    },
};

/// A module with a substantial runtime helper and no literal conversion.
const helper_module: SourceFile = .{ .path = "Crunch.roc", .source =
    \\Crunch := [].{
    \\    crunch : List(U64) -> U64
    \\    crunch = |items| {
    \\        var $total = 0
    \\        for item in items {
    \\            $total = $total + (if item % 2 == 0 item // 2 else 3 * item + 1)
    \\        }
    \\        $total
    \\    }
    \\}
};

/// A quote-converting type whose conversion rejects the literal `"bad"`.
const word_type =
    \\Word := { text : Str }.{
    \\    from_quote : Str -> Try(Word, [BadQuotedBytes(Str)])
    \\    from_quote = |text| if text == "bad" Err(BadQuotedBytes("bad is not a word")) else Ok({ text: text })
    \\    show : Word -> Str
    \\    show = |word| word.text
    \\}
;

const Checked = struct {
    allocator: std.mem.Allocator,
    tmp_dir: std.testing.TmpDir,
    builtin_modules: *eval.BuiltinModules,
    coord: *Coordinator,
    arena: base.SingleThreadArena,

    fn deinit(self: *Checked) void {
        self.coord.deinit();
        self.allocator.destroy(self.coord);
        self.builtin_modules.deinit();
        self.allocator.destroy(self.builtin_modules);
        self.arena.deinit();
        self.tmp_dir.cleanup();
    }

    /// The titles of the reports checking produced, for a failing test to show.
    fn expectErrors(self: *Checked, expected: bool) !void {
        if (self.coord.hasUserErrors() == expected) return;
        var reports = self.coord.iterReports();
        while (reports.next()) |entry| std.debug.print("unexpected report: {s}\n", .{entry.report.title});
        return error.TestUnexpectedResult;
    }

    fn demand(self: *const Checked) lir.CheckedPipeline.DemandMetrics {
        return self.coord.ctfe_timing.lowering.snapshot().demand;
    }
};

/// Check (and, given a runtime target, prepare to build) an echo-platform app.
fn checkApp(allocator: std.mem.Allocator, app: []const u8, extra: []const SourceFile, runtime: ?lir.CheckedPipeline.TargetConfig) !Checked {
    const io = std.testing.io;
    var tmp_dir = std.testing.tmpDir(.{});
    errdefer tmp_dir.cleanup();
    try tmp_dir.dir.createDirPath(io, ".roc_echo_platform");
    for (platform_files) |file| try tmp_dir.dir.writeFile(io, .{ .sub_path = file.path, .data = file.source });
    for (extra) |file| try tmp_dir.dir.writeFile(io, .{ .sub_path = file.path, .data = file.source });
    try tmp_dir.dir.writeFile(io, .{ .sub_path = "main.roc", .data = app });
    const app_path = try tmp_dir.dir.realPathFileAlloc(io, "main.roc", allocator);
    defer allocator.free(app_path);

    const builtin_modules = try allocator.create(eval.BuiltinModules);
    errdefer allocator.destroy(builtin_modules);
    builtin_modules.* = try eval.BuiltinModules.init(allocator);
    errdefer builtin_modules.deinit();
    const coord = try allocator.create(Coordinator);
    errdefer allocator.destroy(coord);
    coord.* = try Coordinator.init(
        allocator,
        .single_threaded,
        1,
        roc_target.RocTarget.detectNative(),
        builtin_modules,
        build_options.compiler_compatibility_id,
        null,
        CoreCtx.os(allocator, allocator, io),
    );
    errdefer coord.deinit();
    coord.enable_hosted_transform = true;
    var arena = base.SingleThreadArena.init(allocator);
    errdefer arena.deinit();
    try coord.start();
    try coord.discoverAppFromPath(arena.allocator(), .{ .entry_path = app_path });
    try coord.coordinatorLoop();
    if (runtime) |target| coord.runtime_lowering = .{ .target = target };
    try coord.finishCheckedProgram(.executable_artifacts);
    return .{
        .allocator = allocator,
        .tmp_dir = tmp_dir,
        .builtin_modules = builtin_modules,
        .coord = coord,
        .arena = arena,
    };
}

test "checking specializes no runtime body when none can register a literal root" {
    if (is_freestanding) return error.SkipZigTest;
    var checked = try checkApp(std.testing.allocator,
        \\app [main!] { pf: platform "./.roc_echo_platform/main.roc" }
        \\import pf.Echo
        \\import Crunch
        \\main! = |args| {
        \\    Echo.line!(Str.inspect(Crunch.crunch([List.len(args), 7, 9])))
        \\    Ok({})
        \\}
    , &.{helper_module}, null);
    defer checked.deinit();
    try checked.expectErrors(false);
    const demand = checked.demand();
    try std.testing.expect(demand.program_roots != 0);
    try std.testing.expectEqual(@as(u64, 0), demand.discovery_roots);
    try std.testing.expectEqual(@as(u64, 0), demand.monotype.discovery_bodies);
}

test "checking validates a runtime function's closed conversion without the unrelated helper's body" {
    if (is_freestanding) return error.SkipZigTest;
    var checked = try checkApp(std.testing.allocator,
        \\app [main!] { pf: platform "./.roc_echo_platform/main.roc" }
        \\import pf.Echo
        \\import Crunch
    ++ "\n" ++ word_type ++ "\n" ++
        \\greeting : U64 -> Str
        \\greeting = |n| {
        \\    word : Word
        \\    word = "hello"
        \\    Str.concat(word.show(), Str.inspect(Crunch.crunch([n, 7, 9])))
        \\}
        \\main! = |args| {
        \\    Echo.line!(greeting(List.len(args)))
        \\    Ok({})
        \\}
    , &.{helper_module}, null);
    defer checked.deinit();
    try checked.expectErrors(false);
    // The conversion is closed, so checking evaluates it as its own root and
    // specializes no runtime body at all.
    const demand = checked.demand();
    try std.testing.expect(demand.program_roots != 0);
    try std.testing.expectEqual(@as(u64, 0), demand.discovery_roots);
    try std.testing.expectEqual(@as(u64, 0), demand.monotype.discovery_bodies);
}

test "checking reports a closed conversion rejected inside a runtime function" {
    if (is_freestanding) return error.SkipZigTest;
    var checked = try checkApp(std.testing.allocator,
        \\app [main!] { pf: platform "./.roc_echo_platform/main.roc" }
        \\import pf.Echo
    ++ "\n" ++ word_type ++ "\n" ++
        \\greeting : {} -> Str
        \\greeting = |{}| {
        \\    word : Word
        \\    word = "bad"
        \\    word.show()
        \\}
        \\main! = |_args| {
        \\    Echo.line!(greeting({}))
        \\    Ok({})
        \\}
    , &.{}, null);
    defer checked.deinit();
    try checked.expectErrors(true);
}

/// A generic function whose literal converts at its caller's type; only a
/// specialization at `Word` registers a literal root for it.
fn genericLabelApp(comptime literal: []const u8) []const u8 {
    return
    \\app [main!] { pf: platform "./.roc_echo_platform/main.roc" }
    \\import pf.Echo
    \\import Crunch
    ++ "\n" ++ word_type ++ "\n" ++
        \\label : a -> a where [a.from_quote : Str -> Try(a, [BadQuotedBytes(Str)])]
        \\label = |_| "
    ++ literal ++
        \\"
        \\main! = |args| {
        \\    word : Word
        \\    word = label(Word.{ text: "x" })
        \\    Echo.line!(Str.concat(word.show(), Str.inspect(Crunch.crunch([List.len(args)]))))
        \\    Ok({})
        \\}
    ;
}

test "checking evaluates a generic function's conversion at its concrete runtime instance" {
    if (is_freestanding) return error.SkipZigTest;
    var checked = try checkApp(std.testing.allocator, genericLabelApp("good"), &.{helper_module}, null);
    defer checked.deinit();
    try checked.expectErrors(false);
    // The generic function's literal depends on its instance, so the app's
    // runtime root is lowered for discovery; the unrelated helper's
    // specialization is parked and never lowered.
    const demand = checked.demand();
    try std.testing.expect(demand.discovery_roots != 0);
    try std.testing.expect(demand.monotype.parked != 0);
    try std.testing.expectEqual(demand.monotype.parked, demand.monotype.stubs + demand.monotype.unparked);
}

test "checking reports a generic function's conversion rejected at its concrete runtime instance" {
    if (is_freestanding) return error.SkipZigTest;
    var checked = try checkApp(std.testing.allocator, genericLabelApp("bad"), &.{helper_module}, null);
    defer checked.deinit();
    try checked.expectErrors(true);
}

test "a build continues the evaluation's program with every runtime body" {
    if (is_freestanding) return error.SkipZigTest;
    const target: lir.CheckedPipeline.TargetConfig = .{
        .inline_mode = .wrappers,
        .spec_constr_clone_inlining = .iterator_fusion,
        .proc_debug_names = true,
    };
    var checked = try checkApp(std.testing.allocator, genericLabelApp("good"), &.{helper_module}, target);
    defer checked.deinit();
    try checked.expectErrors(false);
    // A build's evaluation names its program roots in full: nothing is
    // discovered, and nothing is parked.
    const demand = checked.demand();
    try std.testing.expectEqual(@as(u64, 0), demand.discovery_roots);
    try std.testing.expectEqual(@as(u64, 0), demand.monotype.parked);
    const session = &checked.coord.program_session.?;
    try std.testing.expect(session.runtime_prepared != null);
    var runtime = try session.takeRuntime(std.testing.allocator, session.runtime_roots, target);
    defer runtime.deinit();
    try std.testing.expectEqual(session.runtime_roots.requests.len, runtime.lir_result.root_procs.items.len);
}

/// A compile-time root whose helper converts literals both directly and
/// through a generic function, and captures a callable constant.
fn nestedHelperApp(comptime literal: []const u8) []const u8 {
    return
    \\app [main!] { pf: platform "./.roc_echo_platform/main.roc" }
    \\import pf.Echo
    ++ "\n" ++ word_type ++ "\n" ++
        \\label : a -> a where [a.from_quote : Str -> Try(a, [BadQuotedBytes(Str)])]
        \\label = |_| "
    ++ literal ++
        \\"
        \\twice : U64 -> U64
        \\twice = |n| n * 2
        \\describe : (U64 -> U64) -> Str
        \\describe = |f| {
        \\    direct : Word
        \\    direct = "direct"
        \\    generic : Word
        \\    generic = label(direct)
        \\    Str.concat(Str.concat(direct.show(), generic.show()), Str.inspect(f(21)))
        \\}
        \\summary = describe(twice)
        \\main! = |_args| {
        \\    Echo.line!(summary)
        \\    Ok({})
        \\}
    ;
}

test "a compile-time root's helper evaluates nested and generic conversions with a captured callable" {
    if (is_freestanding) return error.SkipZigTest;
    var checked = try checkApp(std.testing.allocator, nestedHelperApp("good"), &.{}, null);
    defer checked.deinit();
    try checked.expectErrors(false);
}

test "a compile-time root's helper reports its generic conversion's rejection" {
    if (is_freestanding) return error.SkipZigTest;
    var checked = try checkApp(std.testing.allocator, nestedHelperApp("bad"), &.{}, null);
    defer checked.deinit();
    try checked.expectErrors(true);
}

/// A recursive generic function converting a literal at each step, reached
/// only through a callable value a runtime function builds.
fn recursiveCallableApp(comptime literal: []const u8) []const u8 {
    return
    \\app [main!] { pf: platform "./.roc_echo_platform/main.roc" }
    \\import pf.Echo
    ++ "\n" ++ word_type ++ "\n" ++
        \\repeat_label : a, U64 -> List(a) where [a.from_quote : Str -> Try(a, [BadQuotedBytes(Str)])]
        \\repeat_label = |seed, n| if n == 0 [] else List.append(repeat_label(seed, n - 1), "
    ++ literal ++
        \\")
        \\labeller : {} -> (Word, U64 -> List(Word))
        \\labeller = |{}| repeat_label
        \\main! = |args| {
        \\    make = labeller({})
        \\    words = make(Word.{ text: "x" }, List.len(args))
        \\    Echo.line!(Str.inspect(List.len(words)))
        \\    Ok({})
        \\}
    ;
}

test "checking evaluates a recursive generic conversion reached through a callable value" {
    if (is_freestanding) return error.SkipZigTest;
    var checked = try checkApp(std.testing.allocator, recursiveCallableApp("good"), &.{}, null);
    defer checked.deinit();
    try checked.expectErrors(false);
}

test "checking reports a recursive generic conversion's rejection reached through a callable value" {
    if (is_freestanding) return error.SkipZigTest;
    var checked = try checkApp(std.testing.allocator, recursiveCallableApp("bad"), &.{}, null);
    defer checked.deinit();
    try checked.expectErrors(true);
}
