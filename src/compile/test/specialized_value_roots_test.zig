//! A generalized top-level value is compile-time evaluated once per concrete
//! specialization the program uses (design.md "Specialization-Owned
//! Top-Level Values"): each specialization's value is a literal root of the
//! program, and a runtime program reads the completed values instead of
//! running the value's body.

const std = @import("std");
const base = @import("base");
const build_options = @import("build_options");
const eval = @import("eval");
const lir = @import("lir");
const roc_target = @import("roc_target");
const CoreCtx = @import("ctx").CoreCtx;
const Coordinator = @import("../coordinator.zig").Coordinator;
const CoordinatorError = @import("../coordinator.zig").CoordinatorError;
const is_freestanding = @import("../threading.zig").is_freestanding;

const platform_main =
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
;

const platform_echo =
    \\Echo := [].{
    \\    line! : Str => {}
    \\}
;

/// `--opt=speed`'s Solved policy: compile-time evaluation runs inside this
/// build's own specialized program.
const speed_target: lir.CheckedPipeline.TargetConfig = .{
    .inline_mode = .wrappers,
    .spec_constr_clone_inlining = .all_calls,
    .inline_expects = .omit,
    .proc_debug_names = true,
};

const CheckError = std.Io.Dir.CreateDirPathError ||
    std.Io.Dir.WriteFileError ||
    std.mem.Allocator.Error ||
    std.Io.Dir.RealPathFileAllocError ||
    Coordinator.AppDiscoveryError ||
    eval.BuiltinModules.InitError ||
    std.Thread.SpawnError ||
    CoordinatorError;

const App = struct {
    allocator: std.mem.Allocator,
    tmp_dir: std.testing.TmpDir,
    builtin_modules: eval.BuiltinModules,
    arena: base.SingleThreadArena,
    coord: *Coordinator,

    /// Check `source` as an echo-platform app and finalize its checked
    /// program for an optimized runtime build.
    fn check(self: *App, allocator: std.mem.Allocator, source: []const u8) CheckError!void {
        return self.checkOnPlatform(allocator, platform_main, platform_echo, source);
    }

    /// `check` against the echo platform's header and `Echo` module given
    /// here instead of the default ones.
    fn checkOnPlatform(
        self: *App,
        allocator: std.mem.Allocator,
        main_source: []const u8,
        echo_source: []const u8,
        source: []const u8,
    ) CheckError!void {
        const io = std.testing.io;
        self.allocator = allocator;
        self.tmp_dir = std.testing.tmpDir(.{});
        errdefer self.tmp_dir.cleanup();
        try self.tmp_dir.dir.createDirPath(io, ".roc_echo_platform");
        try self.tmp_dir.dir.writeFile(io, .{ .sub_path = ".roc_echo_platform/main.roc", .data = main_source });
        try self.tmp_dir.dir.writeFile(io, .{ .sub_path = ".roc_echo_platform/Echo.roc", .data = echo_source });
        try self.tmp_dir.dir.writeFile(io, .{ .sub_path = "main.roc", .data = source });
        const app_path = try self.tmp_dir.dir.realPathFileAlloc(io, "main.roc", allocator);
        defer allocator.free(app_path);

        self.builtin_modules = try eval.BuiltinModules.init(allocator);
        errdefer self.builtin_modules.deinit();
        self.coord = try allocator.create(Coordinator);
        errdefer allocator.destroy(self.coord);
        self.coord.* = try Coordinator.init(
            allocator,
            .single_threaded,
            1,
            roc_target.RocTarget.detectNative(),
            &self.builtin_modules,
            build_options.compiler_version,
            null,
            CoreCtx.os(allocator, allocator, io),
        );
        errdefer self.coord.deinit();
        self.coord.enable_hosted_transform = true;
        self.arena = base.SingleThreadArena.init(allocator);
        errdefer self.arena.deinit();
        try self.coord.start();
        try self.coord.discoverAppFromPath(self.arena.allocator(), .{ .entry_path = app_path });
        try self.coord.coordinatorLoop();
        self.coord.runtime_lowering = .{ .target = speed_target };
        try self.coord.finishCheckedProgram(.executable_artifacts);
    }

    fn deinit(self: *App) void {
        self.coord.deinit();
        self.allocator.destroy(self.coord);
        self.arena.deinit();
        self.builtin_modules.deinit();
        self.tmp_dir.cleanup();
    }

    /// The compile-time evaluation's literal roots whose subject is a
    /// specialization-owned value.
    fn valueRootCount(self: *App) usize {
        const host = &(self.coord.program_session.?.host orelse return 0);
        var count: usize = 0;
        for (host.lir_result.literal_roots.items) |root| {
            if (root.subject == .value) count += 1;
        }
        return count;
    }

    fn compileTimeCrashCount(self: *App) usize {
        return self.reportCount("Compile Time Crash");
    }

    fn reportCount(self: *App, title: []const u8) usize {
        var count: usize = 0;
        var reports = self.coord.iterReports();
        while (reports.next()) |entry| {
            if (std.mem.eql(u8, entry.report.title, title)) count += 1;
        }
        return count;
    }

    fn moduleReportCount(self: *App, module_name: []const u8, title: []const u8) usize {
        var count: usize = 0;
        var reports = self.coord.iterReports();
        while (reports.next()) |entry| {
            if (std.mem.eql(u8, entry.module_name, module_name) and std.mem.eql(u8, entry.report.title, title)) count += 1;
        }
        return count;
    }
};

fn countProcsNaming(store: *const lir.LirStore, fragment: []const u8) usize {
    var count: usize = 0;
    for (0..store.procSpecCount()) |index| {
        const name = store.procDebugName(@enumFromInt(index)) orelse continue;
        if (std.mem.find(u8, name, fragment) != null) count += 1;
    }
    return count;
}

test "an optimized runtime program reads each specialization of a generalized value as a completed value" {
    if (is_freestanding) return error.SkipZigTest;
    const allocator = std.testing.allocator;
    var app: App = undefined;
    try app.check(allocator,
        \\app [main!] { pf: platform "./.roc_echo_platform/main.roc" }
        \\import pf.Echo
        \\grow : U64, List(a) -> List(a)
        \\grow = |n, acc| if n == 0 { acc } else { grow(n - 1, acc) }
        \\made : List(a)
        \\made = grow(3, [])
        \\main! = |args| {
        \\    nums : List(U64)
        \\    nums = List.append(made, List.len(args))
        \\    strs : List(Str)
        \\    strs = List.concat(made, args)
        \\    Echo.line!(Str.inspect(nums))
        \\    Echo.line!(Str.inspect(List.len(strs)))
        \\    Ok({})
        \\}
    );
    defer app.deinit();
    try std.testing.expect(!app.coord.hasUserErrors());
    // One root per concrete type: `List(U64)` and `List(Str)`.
    try std.testing.expectEqual(@as(usize, 2), app.valueRootCount());

    const session = &app.coord.program_session.?;
    try std.testing.expect(session.runtime_prepared != null);
    var runtime = try session.takeRuntime(allocator, session.runtime_roots, speed_target);
    defer runtime.deinit();
    try std.testing.expectEqual(@as(usize, 0), countProcsNaming(&runtime.lir_result.store, "grow"));
}

test "a generalized callable binding is compile-time evaluated at its specialization" {
    if (is_freestanding) return error.SkipZigTest;
    const allocator = std.testing.allocator;
    // `validate`'s implicitly opened error row generalizes, so its type is
    // specialization-owned; its use in `main!` closes the row.
    var app: App = undefined;
    try app.check(allocator,
        \\app [main!] { pf: platform "./.roc_echo_platform/main.roc" }
        \\import pf.Echo
        \\make_validator = |limit| {
        \\    |n| if n > limit { Ok(n) } else { Err(Small("too small")) }
        \\}
        \\validate : I64 -> Try(I64, [Small(Str)])
        \\validate = make_validator(0.I64)
        \\main! = |args| {
        \\    validated = match validate(List.len(args).to_i64_wrap()) {
        \\        Ok(n) => n
        \\        Err(Small(_)) => 0.I64
        \\    }
        \\    Echo.line!(Str.inspect(validated))
        \\    Ok({})
        \\}
    );
    defer app.deinit();
    try std.testing.expect(!app.coord.hasUserErrors());
    try std.testing.expectEqual(@as(usize, 1), app.valueRootCount());

    const session = &app.coord.program_session.?;
    var runtime = try session.takeRuntime(allocator, session.runtime_roots, speed_target);
    defer runtime.deinit();
    try std.testing.expectEqual(@as(usize, 0), countProcsNaming(&runtime.lir_result.store, "make_validator"));
}

test "a generalized value's compile-time crash is reported at each specialization" {
    if (is_freestanding) return error.SkipZigTest;
    const allocator = std.testing.allocator;
    var app: App = undefined;
    try app.check(allocator,
        \\app [main!] { pf: platform "./.roc_echo_platform/main.roc" }
        \\import pf.Echo
        \\none : List(Str)
        \\none = []
        \\boom : List(a)
        \\boom = if List.is_empty(none) { crash "no list today" } else { [] }
        \\main! = |args| {
        \\    nums : List(U64)
        \\    nums = List.append(boom, List.len(args))
        \\    strs : List(Str)
        \\    strs = List.concat(boom, args)
        \\    Echo.line!(Str.inspect(List.len(nums) + List.len(strs)))
        \\    Ok({})
        \\}
    );
    defer app.deinit();
    try std.testing.expect(app.coord.hasUserErrors());
    // Both specializations are evaluated and each fails: distinct
    // specializations are distinct failures, whatever their messages.
    try std.testing.expectEqual(@as(usize, 2), app.valueRootCount());
    try std.testing.expectEqual(@as(usize, 2), app.compileTimeCrashCount());
}

test "a generalized value and a constant failing in one shared helper report separately" {
    if (is_freestanding) return error.SkipZigTest;
    const allocator = std.testing.allocator;
    // `mono` is a module-evaluated constant and `poly` a specialization-owned
    // value; both crash at `helper`'s one `crash`. They are different values,
    // so the site they share does not make them one diagnostic.
    var app: App = undefined;
    try app.check(allocator,
        \\app [main!] { pf: platform "./.roc_echo_platform/main.roc" }
        \\import pf.Echo
        \\helper : U64 -> U64
        \\helper = |n| if n > 100 { crash "too big" } else { n }
        \\mono : U64
        \\mono = helper(1000)
        \\poly : List(a)
        \\poly = {
        \\    _big = helper(1000)
        \\    []
        \\}
        \\main! = |args| {
        \\    nums : List(U64)
        \\    nums = List.append(poly, mono)
        \\    Echo.line!(Str.inspect(List.len(nums) + List.len(args)))
        \\    Ok({})
        \\}
    );
    defer app.deinit();
    try std.testing.expect(app.coord.hasUserErrors());
    try std.testing.expectEqual(@as(usize, 1), app.valueRootCount());
    try std.testing.expectEqual(@as(usize, 2), app.compileTimeCrashCount());
}

/// The echo platform's header with a compile-time constant, `platform_len`,
/// that uses `Echo.boom` at `List(U64)`: the platform's own finalization
/// program evaluates that specialization, and an app using `Echo.boom` at the
/// same type makes the pairing program evaluate it again.
const platform_main_using_boom =
    \\platform ""
    \\    requires {} { main! : List(Str) => Try({}, [Exit(I8), ..]) }
    \\    exposes [Echo]
    \\    packages {}
    \\    provides { "roc_main": main_for_host! }
    \\    hosted { "roc_echo_line": Echo.line! }
    \\import Echo
    \\platform_len : U64
    \\platform_len = List.len(List.append(Echo.boom, 1.U64))
    \\main_for_host! : List(Str) => I8
    \\main_for_host! = |args|
    \\    match main!(args) {
    \\        Ok({}) => if platform_len > 0 { 0 } else { 2 }
    \\        Err(Exit(code)) => code
    \\        Err(other) => {
    \\            Echo.line!("Program exited with error: ${Str.inspect(other)}")
    \\            1
    \\        }
    \\    }
;

const app_using_boom =
    \\app [main!] { pf: platform "./.roc_echo_platform/main.roc" }
    \\import pf.Echo
    \\main! = |args| {
    \\    nums : List(U64)
    \\    nums = List.append(Echo.boom, List.len(args))
    \\    Echo.line!(Str.inspect(nums))
    \\    Ok({})
    \\}
;

test "a platform value's specialization failing in both finalization programs is reported once" {
    if (is_freestanding) return error.SkipZigTest;
    const allocator = std.testing.allocator;
    // `boom`'s inline expect fails without failing the value, so neither
    // program's checked roots embed the failure: each program reports it at
    // the value. Both evaluate the same specialization (`List(U64)`) and fail
    // at the same expect, so the second program does not report it again.
    var app: App = undefined;
    try app.checkOnPlatform(allocator, platform_main_using_boom,
        \\Echo := [].{
        \\    line! : Str => {}
        \\
        \\    none : List(Str)
        \\    none = []
        \\
        \\    boom : List(a)
        \\    boom = {
        \\        expect List.len(none) == 1
        \\        []
        \\    }
        \\}
    , app_using_boom);
    defer app.deinit();
    try std.testing.expect(app.coord.hasUserErrors());
    try std.testing.expectEqual(@as(usize, 1), app.reportCount("Compile Time Expect Failed"));
}

test "a platform value's crash is evaluated by both finalization programs" {
    if (is_freestanding) return error.SkipZigTest;
    const allocator = std.testing.allocator;
    // The control for the test above: the same platform and app, with `boom`
    // crashing. Both programs evaluate the specialization; the platform's
    // program reports the failure at `platform_len`, whose checked root it
    // fails, and the pairing reports it at the value (design.md
    // "Specialization-Owned Top-Level Values", declared limitation).
    var app: App = undefined;
    try app.checkOnPlatform(allocator, platform_main_using_boom,
        \\Echo := [].{
        \\    line! : Str => {}
        \\
        \\    none : List(Str)
        \\    none = []
        \\
        \\    boom : List(a)
        \\    boom = if List.is_empty(none) { crash "platform boom" } else { [] }
        \\}
    , app_using_boom);
    defer app.deinit();
    try std.testing.expect(app.coord.hasUserErrors());
    try std.testing.expectEqual(@as(usize, 1), app.moduleReportCount("main", "Compile Time Crash"));
    try std.testing.expectEqual(@as(usize, 1), app.moduleReportCount("Echo", "Compile Time Crash"));
}

test "an always-diverging generalized value is evaluated at its use, not per specialization" {
    if (is_freestanding) return error.SkipZigTest;
    const allocator = std.testing.allocator;
    // `todo`'s checked body diverges on every path, so there is no value to
    // fold: it stays evaluated at its use, as on main, and the placeholder
    // crashes only when that use runs. The use here is in a branch the program
    // never takes, so compiling reports nothing.
    var app: App = undefined;
    try app.check(allocator,
        \\app [main!] { pf: platform "./.roc_echo_platform/main.roc" }
        \\import pf.Echo
        \\todo : a
        \\todo = crash "TODO"
        \\main! = |args| {
        \\    n : U64
        \\    n = if List.len(args) > 100 { todo } else { 1 }
        \\    Echo.line!(Str.inspect(n))
        \\    Ok({})
        \\}
    );
    defer app.deinit();
    try std.testing.expect(!app.coord.hasUserErrors());
    try std.testing.expectEqual(@as(usize, 0), app.compileTimeCrashCount());
    try std.testing.expectEqual(@as(usize, 0), app.valueRootCount());
    const artifact = app.coord.appRootCheckedArtifact();
    var saw_value = false;
    for (artifact.compile_time_roots.roots) |root| {
        if (root.kind != .constant or root.source != .def) continue;
        saw_value = true;
        try std.testing.expectEqual(.ineligible, root.request_eligibility);
    }
    try std.testing.expect(saw_value);
}

test "a sometimes-crashing generalized value is still evaluated per specialization" {
    if (is_freestanding) return error.SkipZigTest;
    const allocator = std.testing.allocator;
    // The control for the test above: `maybe` has a path that returns a
    // value, so it stays specialization-owned, and the specialization the
    // untaken branch names is evaluated at compile time, where it takes the
    // crashing path.
    var app: App = undefined;
    try app.check(allocator,
        \\app [main!] { pf: platform "./.roc_echo_platform/main.roc" }
        \\import pf.Echo
        \\none : List(Str)
        \\none = []
        \\maybe : List(a)
        \\maybe = if List.is_empty(none) { crash "sometimes" } else { [] }
        \\main! = |args| {
        \\    nums : List(U64)
        \\    nums = if List.len(args) > 100 { maybe } else { [] }
        \\    Echo.line!(Str.inspect(List.len(nums)))
        \\    Ok({})
        \\}
    );
    defer app.deinit();
    try std.testing.expect(app.coord.hasUserErrors());
    try std.testing.expectEqual(@as(usize, 1), app.valueRootCount());
    try std.testing.expectEqual(@as(usize, 1), app.compileTimeCrashCount());
}

test "a generalized value no specialization uses is not evaluated" {
    if (is_freestanding) return error.SkipZigTest;
    const allocator = std.testing.allocator;
    // There is no concrete type to evaluate `boom` at, so its crash is
    // never observed, exactly as at runtime.
    var app: App = undefined;
    try app.check(allocator,
        \\app [main!] { pf: platform "./.roc_echo_platform/main.roc" }
        \\import pf.Echo
        \\none : List(Str)
        \\none = []
        \\boom : List(a)
        \\boom = if List.is_empty(none) { crash "no list today" } else { [] }
        \\main! = |_args| {
        \\    Echo.line!("fine")
        \\    Ok({})
        \\}
    );
    defer app.deinit();
    try std.testing.expect(!app.coord.hasUserErrors());
    try std.testing.expectEqual(@as(usize, 0), app.compileTimeCrashCount());
    try std.testing.expectEqual(@as(usize, 0), app.valueRootCount());
}

test "a generalized value whose body has a checking error reports no compile-time crash per specialization" {
    if (is_freestanding) return error.SkipZigTest;
    const allocator = std.testing.allocator;
    // Checking owns the diagnostic. The specialization is still evaluated,
    // like any compile-time root, but its evaluation stops at the checked
    // error, so it reports no second, compile-time crash.
    var app: App = undefined;
    try app.check(allocator,
        \\app [main!] { pf: platform "./.roc_echo_platform/main.roc" }
        \\import pf.Echo
        \\none : List(Str)
        \\none = []
        \\broken : List(a)
        \\broken = if List.is_empty(none) { List.append([], "x" + 1) } else { [] }
        \\main! = |args| {
        \\    nums : List(U64)
        \\    nums = List.concat(broken, [List.len(args)])
        \\    Echo.line!(Str.inspect(List.len(nums)))
        \\    Ok({})
        \\}
    );
    defer app.deinit();
    try std.testing.expect(app.coord.hasUserErrors());
    try std.testing.expectEqual(@as(usize, 0), app.compileTimeCrashCount());
    try std.testing.expectEqual(@as(usize, 1), app.valueRootCount());
}

test "mutually recursive generalized callable values are evaluated at their specialization" {
    if (is_freestanding) return error.SkipZigTest;
    const allocator = std.testing.allocator;
    // Lowering `is_even`'s body reaches `is_odd`, whose body reaches back to
    // `is_even` while its recursive binding is still active. Each value's
    // root is its own definition, so neither may read the other's binding.
    var app: App = undefined;
    try app.check(allocator,
        \\app [main!] { pf: platform "./.roc_echo_platform/main.roc" }
        \\import pf.Echo
        \\make = |f| f
        \\is_even : U64 -> [Yes, No]
        \\is_even = make(|n| if n == 0 Yes else is_odd(n - 1))
        \\is_odd : U64 -> [Yes, No]
        \\is_odd = make(|n| if n == 0 No else is_even(n - 1))
        \\main! = |args| {
        \\    answer = match is_even(List.len(args)) {
        \\        Yes => "even"
        \\        No => "odd"
        \\    }
        \\    Echo.line!(answer)
        \\    Ok({})
        \\}
    );
    defer app.deinit();
    try std.testing.expect(!app.coord.hasUserErrors());
    const session = &app.coord.program_session.?;
    var runtime = try session.takeRuntime(allocator, session.runtime_roots, speed_target);
    defer runtime.deinit();
}

test "a generalized value whose annotation has an error is not evaluated per specialization" {
    if (is_freestanding) return error.SkipZigTest;
    const allocator = std.testing.allocator;
    var app: App = undefined;
    try app.check(allocator,
        \\app [main!] { pf: platform "./.roc_echo_platform/main.roc" }
        \\import pf.Echo
        \\nothing : List(Undefined)
        \\nothing = []
        \\main! = |_args| {
        \\    Echo.line!(Str.inspect(List.len(nothing)))
        \\    Ok({})
        \\}
    );
    defer app.deinit();
    try std.testing.expect(app.coord.hasUserErrors());
    try std.testing.expectEqual(@as(usize, 0), app.compileTimeCrashCount());
    try std.testing.expectEqual(@as(usize, 0), app.valueRootCount());
}

test "a generalized callable value recursive with a monomorphic one is evaluated at its specialization" {
    if (is_freestanding) return error.SkipZigTest;
    const allocator = std.testing.allocator;
    // `is_even` is context-free and module-evaluated; `is_odd` is
    // specialization-owned. Each producer is a block whose lambda captures
    // `z` and refers to the other value. `is_even`'s own root inlines
    // `is_odd` under `is_even`'s recursive binding, so only `main!`'s use of
    // `is_odd` is a value root.
    var app: App = undefined;
    try app.check(allocator,
        \\app [main!] { pf: platform "./.roc_echo_platform/main.roc" }
        \\import pf.Echo
        \\is_even : U64 -> Bool
        \\is_even = {
        \\    z = 0
        \\    |n| if n == z { Bool.True } else { match is_odd(n - 1) { Yes => Bool.True, No => Bool.False } }
        \\}
        \\is_odd : U64 -> [Yes, No]
        \\is_odd = {
        \\    z = 0
        \\    |n| if n == z { No } else { if is_even(n - 1) { Yes } else { No } }
        \\}
        \\main! = |args| {
        \\    answer = match is_odd(List.len(args)) {
        \\        Yes => "odd"
        \\        No => "even"
        \\    }
        \\    Echo.line!(answer)
        \\    Ok({})
        \\}
    );
    defer app.deinit();
    try std.testing.expect(!app.coord.hasUserErrors());
    try std.testing.expectEqual(@as(usize, 1), app.valueRootCount());
    const session = &app.coord.program_session.?;
    var runtime = try session.takeRuntime(allocator, session.runtime_roots, speed_target);
    defer runtime.deinit();
}

test "a generalized callable value recursive with a monomorphic one built by a call is evaluated at its specialization" {
    if (is_freestanding) return error.SkipZigTest;
    const allocator = std.testing.allocator;
    var app: App = undefined;
    try app.check(allocator,
        \\app [main!] { pf: platform "./.roc_echo_platform/main.roc" }
        \\import pf.Echo
        \\make = |f| f
        \\is_even : U64 -> Bool
        \\is_even = make(|n| if n == 0 Bool.True else match is_odd(n - 1) { Yes => Bool.True, No => Bool.False })
        \\is_odd : U64 -> [Yes, No]
        \\is_odd = make(|n| if n == 0 No else if is_even(n - 1) Yes else No)
        \\main! = |args| {
        \\    answer = match is_odd(List.len(args)) {
        \\        Yes => "odd"
        \\        No => "even"
        \\    }
        \\    Echo.line!(answer)
        \\    Ok({})
        \\}
    );
    defer app.deinit();
    try std.testing.expect(!app.coord.hasUserErrors());
    try std.testing.expectEqual(@as(usize, 1), app.valueRootCount());
    const session = &app.coord.program_session.?;
    var runtime = try session.takeRuntime(allocator, session.runtime_roots, speed_target);
    defer runtime.deinit();
}

test "a generalized value used only inside another generalized value is evaluated by the enclosing root" {
    if (is_freestanding) return error.SkipZigTest;
    const allocator = std.testing.allocator;
    // `made` has no use of its own outside `nested`'s body, so `nested`'s
    // root computes it: one value root, and no runtime `grow`.
    var app: App = undefined;
    try app.check(allocator,
        \\app [main!] { pf: platform "./.roc_echo_platform/main.roc" }
        \\import pf.Echo
        \\grow : U64, List(a) -> List(a)
        \\grow = |n, acc| if n == 0 { acc } else { grow(n - 1, acc) }
        \\made : List(a)
        \\made = grow(3, [])
        \\nested : List(List(a))
        \\nested = [made, made]
        \\main! = |args| {
        \\    lists : List(List(Str))
        \\    lists = List.append(nested, args)
        \\    Echo.line!(Str.inspect(List.len(lists)))
        \\    Ok({})
        \\}
    );
    defer app.deinit();
    try std.testing.expect(!app.coord.hasUserErrors());
    try std.testing.expectEqual(@as(usize, 1), app.valueRootCount());
    const session = &app.coord.program_session.?;
    var runtime = try session.takeRuntime(allocator, session.runtime_roots, speed_target);
    defer runtime.deinit();
    try std.testing.expectEqual(@as(usize, 0), countProcsNaming(&runtime.lir_result.store, "grow"));
}

test "a generalized local value with a hardcoded document is still a hoisted compile-time root" {
    if (is_freestanding) return error.SkipZigTest;
    // `r`'s annotation opens its error row, so `r` is a generalized local
    // value (design.md "Polarity"): its binder quantifies the row, which
    // seals to its row default when the value is evaluated once. That keeps
    // its right-hand side a hoisted constant evaluated at compile time, like
    // the unannotated local below it, rather than a runtime `Json.parse`.
    const allocator = std.testing.allocator;
    var app: App = undefined;
    try app.check(allocator,
        \\app [main!] { pf: platform "./.roc_echo_platform/main.roc" }
        \\import pf.Echo
        \\score : Try({ foo : Str }, [InvalidJson(Str), MissingRequiredField(Str)]) -> U64
        \\score = |r| match r {
        \\    Ok(_) => 1
        \\    Err(InvalidJson(_)) => 2
        \\    Err(_) => 3
        \\}
        \\main! = |args| {
        \\    r : Try({ foo : Str }, [InvalidJson(Str), MissingRequiredField(Str)])
        \\    r = Json.parse("{\"foo\":\"array\",\"skip\":[1,2]}")
        \\    Echo.line!(Str.inspect(score(r) + List.len(args)))
        \\    Ok({})
        \\}
    );
    defer app.deinit();
    try std.testing.expect(!app.coord.hasUserErrors());
    const artifact = app.coord.appRootCheckedArtifact();
    try std.testing.expectEqual(@as(usize, 2), artifact.compile_time_roots.roots.len);
    for (artifact.compile_time_roots.roots) |root| {
        try std.testing.expect(root.kind == .hoisted_constant);
        try std.testing.expect(root.request_eligibility == .eligible);
        try std.testing.expect(root.payload == .const_node);
    }
    var plain: App = undefined;
    try plain.check(allocator,
        \\app [main!] { pf: platform "./.roc_echo_platform/main.roc" }
        \\import pf.Echo
        \\score : Try({ foo : Str }, [InvalidJson(Str), MissingRequiredField(Str)]) -> U64
        \\score = |r| match r {
        \\    Ok(_) => 1
        \\    Err(InvalidJson(_)) => 2
        \\    Err(_) => 3
        \\}
        \\main! = |args| {
        \\    r = Json.parse("{\"foo\":\"array\",\"skip\":[1,2]}")
        \\    Echo.line!(Str.inspect(score(r) + List.len(args)))
        \\    Ok({})
        \\}
    );
    defer plain.deinit();
    try std.testing.expect(!plain.coord.hasUserErrors());
    const plain_artifact = plain.coord.appRootCheckedArtifact();
    try std.testing.expectEqual(artifact.compile_time_roots.roots.len, plain_artifact.compile_time_roots.roots.len);
    for (plain_artifact.compile_time_roots.roots) |root| {
        try std.testing.expect(root.kind == .hoisted_constant);
        try std.testing.expect(root.request_eligibility == .eligible);
        try std.testing.expect(root.payload == .const_node);
    }
}

test "a generalized local value's compile-time root carries no scheme" {
    if (is_freestanding) return error.SkipZigTest;
    // `boom`'s binder quantifies its implicitly opened row, but its hoisted
    // root is evaluated once at `[Boom]`, where that row seals to its row
    // default. The root's entry wrapper therefore quantifies nothing: a body
    // reading the evaluated value at the binder's type must see the row
    // default, not a variable some scheme binds (which Boxy would turn into a
    // descriptor no caller can supply).
    const allocator = std.testing.allocator;
    var app: App = undefined;
    try app.check(allocator,
        \\app [main!] { pf: platform "./.roc_echo_platform/main.roc" }
        \\import pf.Echo
        \\describe_first : [Boom, Zed] -> Str
        \\describe_first = |tag| match tag {
        \\    Boom => "boom-first"
        \\    Zed => "zed"
        \\}
        \\describe_last : [Aa, Ab, Boom] -> Str
        \\describe_last = |tag| match tag {
        \\    Aa => "aa"
        \\    Ab => "ab"
        \\    Boom => "boom-last"
        \\}
        \\main! = |args| {
        \\    boom : [Boom]
        \\    boom = Boom
        \\    first = describe_first(boom)
        \\    last = describe_last(boom)
        \\    Echo.line!(if List.len(args) == 0 "${first} ${last}" else "x")
        \\    Ok({})
        \\}
    );
    defer app.deinit();
    try std.testing.expect(!app.coord.hasUserErrors());
    const artifact = app.coord.appRootCheckedArtifact();
    // Every hoisted constant here (`boom`, and the constant calls and strings
    // built from it) is evaluated once and quantifies nothing.
    var sealed_roots: usize = 0;
    for (artifact.compile_time_roots.roots) |root| {
        if (root.kind != .hoisted_constant) continue;
        try std.testing.expect(root.request_eligibility == .eligible);
        const wrapper = artifact.entry_wrappers.lookupByRoot(root.id) orelse return error.TestUnexpectedResult;
        const template = &artifact.checked_procedure_templates.templates.items[@intFromEnum(wrapper.template.template)];
        try std.testing.expectEqual(@as(usize, 0), artifact.checked_procedure_templates.templateSchemeVars(template).len);
        sealed_roots += 1;
    }
    try std.testing.expect(sealed_roots != 0);
}

test "a literal in an escaped receiver's dispatch constraint is decided with its receiver" {
    if (is_freestanding) return error.SkipZigTest;
    // `main!` is pinned by the platform's requirement, so at its
    // generalization boundary rank adjustment reaches the quote `"x"` from a
    // variable of the enclosing rank: the quote escapes. The `3` copied from
    // `run`'s scheme is related to it only through the receiver's `repeat`
    // constraint, which rank adjustment does not descend into. It escapes with
    // its receiver and is decided with it (`Str.repeat` makes it a U64), not
    // defaulted to Dec at `main!`'s boundary first (design.md "Static Dispatch
    // At The Checked Boundary").
    const allocator = std.testing.allocator;
    var app: App = undefined;
    try app.check(allocator,
        \\app [main!] { pf: platform "./.roc_echo_platform/main.roc" }
        \\run = |f| f({}).repeat(3)
        \\main! = |_args| {
        \\    _ = run(|{}| "x")
        \\    Ok({})
        \\}
    );
    defer app.deinit();
    try std.testing.expect(!app.coord.hasUserErrors());
}

test "a procedure alias binding is never evaluated per specialization" {
    if (is_freestanding) return error.SkipZigTest;
    const allocator = std.testing.allocator;
    // `mapping` is one resolved procedure lookup: there is no value to
    // compute, so its generalized uses forward to `List.map` and register no
    // value root.
    var app: App = undefined;
    try app.check(allocator,
        \\app [main!] { pf: platform "./.roc_echo_platform/main.roc" }
        \\import pf.Echo
        \\mapping = List.map
        \\main! = |args| {
        \\    lens = mapping(args, |a| Str.count_utf8_bytes(a))
        \\    words = mapping(args, |a| Str.concat(a, "!"))
        \\    Echo.line!(Str.inspect(lens))
        \\    Echo.line!(Str.inspect(words))
        \\    Ok({})
        \\}
    );
    defer app.deinit();
    try std.testing.expect(!app.coord.hasUserErrors());
    try std.testing.expectEqual(@as(usize, 0), app.valueRootCount());
    const artifact = app.coord.appRootCheckedArtifact();
    var saw_alias = false;
    for (artifact.compile_time_roots.roots) |root| {
        if (root.kind != .callable_binding) continue;
        saw_alias = true;
        try std.testing.expect(root.request_eligibility != .per_specialization);
    }
    try std.testing.expect(saw_alias);
}

test "mutually recursive generalized record values are evaluated at their specialization" {
    if (is_freestanding) return error.SkipZigTest;
    const allocator = std.testing.allocator;
    // The data-value twin of the callable case: each record's field lambda
    // reaches the other record, whose deferred const use is lowered while
    // the first record's binding is active.
    var app: App = undefined;
    try app.check(allocator,
        \\app [main!] { pf: platform "./.roc_echo_platform/main.roc" }
        \\import pf.Echo
        \\evens : { check : U64 -> [Yes, No] }
        \\evens = { check: |n| if n == 0 Yes else (odds.check)(n - 1) }
        \\odds : { check : U64 -> [Yes, No] }
        \\odds = { check: |n| if n == 0 No else (evens.check)(n - 1) }
        \\main! = |args| {
        \\    answer = match (evens.check)(List.len(args)) {
        \\        Yes => "even"
        \\        No => "odd"
        \\    }
        \\    Echo.line!(answer)
        \\    Ok({})
        \\}
    );
    defer app.deinit();
    try std.testing.expect(!app.coord.hasUserErrors());
    const session = &app.coord.program_session.?;
    var runtime = try session.takeRuntime(allocator, session.runtime_roots, speed_target);
    defer runtime.deinit();
}
