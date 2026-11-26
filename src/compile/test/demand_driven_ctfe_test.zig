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

/// Every way checking a fixture app can fail.
const CheckAppError = std.mem.Allocator.Error ||
    std.Io.Dir.CreateDirPathError ||
    std.Io.Dir.WriteFileError ||
    std.Io.Dir.RealPathFileAllocError ||
    eval.BuiltinModules.InitError ||
    std.Thread.SpawnError ||
    Coordinator.AppDiscoveryError ||
    @import("../coordinator.zig").CoordinatorError;

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
    fn expectErrors(self: *Checked, expected: bool) error{TestUnexpectedResult}!void {
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
fn checkApp(allocator: std.mem.Allocator, app: []const u8, extra: []const SourceFile, runtime: ?lir.CheckedPipeline.TargetConfig) CheckAppError!Checked {
    return checkAppUnder(allocator, app, extra, runtime, .lss);
}

/// `checkApp` with the compile-time evaluator of `strategy`.
fn checkAppUnder(allocator: std.mem.Allocator, app: []const u8, extra: []const SourceFile, runtime: ?lir.CheckedPipeline.TargetConfig, strategy: base.SpecializationStrategy) CheckAppError!Checked {
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
    builtin_modules.* = try eval.BuiltinModules.init(allocator, std.testing.io);
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
        strategy,
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
    // The generic function's literal depends on its instance, and `main!`'s
    // call fixes that instance to `Word`: checking records the call, so the
    // conversion is a literal argument root of the app, which evaluation
    // completes and stores (design.md "Literal Arguments"). No runtime body
    // can register a literal root, so nothing is specialized for discovery.
    const root = checked.coord.appRootCheckedArtifact();
    var stored: usize = 0;
    for (root.compile_time_roots.roots.items()) |compile_time_root| {
        if (compile_time_root.kind != .literal_argument) continue;
        try std.testing.expect(compile_time_root.payload == .const_node);
        stored += 1;
    }
    try std.testing.expectEqual(@as(usize, 1), stored);
    const demand = checked.demand();
    try std.testing.expectEqual(@as(u64, 0), demand.discovery_roots);
    try std.testing.expectEqual(@as(u64, 0), demand.monotype.discovery_bodies);
}

test "checking specializes no runtime body that cannot reach its module's dependent conversion" {
    if (is_freestanding) return error.SkipZigTest;
    var checked = try checkApp(std.testing.allocator,
        \\app [main!] { pf: platform "./.roc_echo_platform/main.roc" }
        \\import pf.Echo
        \\import Crunch
    ++ "\n" ++ word_type ++ "\n" ++
        \\label : a -> a where [a.from_quote : Str -> Try(a, [BadQuotedBytes(Str)])]
        \\label = |_| "good"
        \\main! = |args| {
        \\    Echo.line!(Str.inspect(Crunch.crunch([List.len(args)])))
        \\    Ok({})
        \\}
    , &.{helper_module}, null);
    defer checked.deinit();
    try checked.expectErrors(false);
    // The module has a conversion that depends on its instance, but the
    // runtime root reaches neither it nor the custom type it converts to.
    const demand = checked.demand();
    try std.testing.expect(demand.program_roots != 0);
    try std.testing.expectEqual(@as(u64, 0), demand.discovery_roots);
    try std.testing.expectEqual(@as(u64, 0), demand.monotype.discovery_bodies);
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
        .spec_constr_clone_inlining = .no_calls,
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

/// A quote-converting type with a parameter its values never mention, whose
/// conversion rejects the literal `"bad"`.
const sql_type =
    \\Sql(a) := { text : Str }.{
    \\    from_quote : Str -> Try(Sql(a), [BadQuotedBytes(Str)])
    \\    from_quote = |raw| if raw == "bad" Err(BadQuotedBytes("rejected")) else Ok(Sql.{ text: raw })
    \\    text : Sql(a) -> Str
    \\    text = |sql| sql.text
    \\}
    \\wrap : Sql(a) -> Sql(a)
    \\wrap = |sql| sql
;

const app_header =
    \\app [main!] { pf: platform "./.roc_echo_platform/main.roc" }
    \\import pf.Echo
;

/// The literal's type keeps a variable of `main!`'s own scheme, which the
/// platform's requirement does not fix.
fn sqlInMainResultApp(comptime literal: []const u8) []const u8 {
    return app_header ++ "\n" ++ sql_type ++ "\n" ++
        \\main! = |args| {
        \\    if args.len() > 5 {
        \\        return Err(Bad(wrap("
    ++ literal ++
        \\")))
        \\    }
        \\    Echo.line!("ran")
        \\    Ok({})
        \\}
    ;
}

/// As `sqlInMainResultApp`, with `main!` an exact alias of the procedure.
fn sqlInAliasedMainApp(comptime literal: []const u8) []const u8 {
    return app_header ++ "\n" ++ sql_type ++ "\n" ++
        \\run! = |args| {
        \\    if args.len() > 5 {
        \\        return Err(Bad(wrap("
    ++ literal ++
        \\")))
        \\    }
        \\    Echo.line!("ran")
        \\    Ok({})
        \\}
        \\main! = run!
    ;
}

/// As `sqlInAliasedMainApp`, with `main!` a computed value.
fn sqlInComputedMainApp(comptime literal: []const u8) []const u8 {
    return app_header ++ "\n" ++ sql_type ++ "\n" ++
        \\run! = |args| {
        \\    if args.len() > 5 {
        \\        return Err(Bad(wrap("
    ++ literal ++
        \\")))
        \\    }
        \\    Echo.line!("ran")
        \\    Ok({})
        \\}
        \\main! = {
        \\    f = run!
        \\    f
        \\}
    ;
}

/// The literal is a requirement of `helper`'s scheme, and the use's copy of
/// the variable is related to nothing else in the program.
fn sqlInHelperApp(comptime literal: []const u8) []const u8 {
    return app_header ++ "\n" ++ sql_type ++ "\n" ++
        \\helper = |{}| wrap("
    ++ literal ++
        \\")
        \\main! = |_args| {
        \\    x = helper({})
        \\    Echo.line!(x.text())
        \\    Ok({})
        \\}
    ;
}

/// As `sqlInHelperApp`, with a generalized local function.
fn sqlInLocalHelperApp(comptime literal: []const u8) []const u8 {
    return app_header ++ "\n" ++ sql_type ++ "\n" ++
        \\main! = |_args| {
        \\    helper = |{}| wrap("
    ++ literal ++
        \\")
        \\    x = helper({})
        \\    Echo.line!(x.text())
        \\    Ok({})
        \\}
    ;
}

/// As `sqlInHelperApp`, with the function stored in a top-level value.
fn sqlInStoredHelperApp(comptime literal: []const u8) []const u8 {
    return app_header ++ "\n" ++ sql_type ++ "\n" ++
        \\helper = |{}| wrap("
    ++ literal ++
        \\")
        \\stored = { f: helper }
        \\main! = |_args| {
        \\    x = (stored.f)({})
        \\    Echo.line!(x.text())
        \\    Ok({})
        \\}
    ;
}

/// As `sqlInHelperApp`, with the call made by a top-level value.
fn sqlInTopLevelCallApp(comptime literal: []const u8) []const u8 {
    return app_header ++ "\n" ++ sql_type ++ "\n" ++
        \\helper = |{}| wrap("
    ++ literal ++
        \\")
        \\top = helper({})
        \\main! = |_args| {
        \\    Echo.line!(top.text())
        \\    Ok({})
        \\}
    ;
}

fn expectLiteralOutcomeUnderBothStrategies(comptime app: fn (comptime []const u8) []const u8) (CheckAppError || error{ SkipZigTest, TestUnexpectedResult })!void {
    if (is_freestanding) return error.SkipZigTest;
    for ([_]base.SpecializationStrategy{ .lss, .boxy }) |strategy| {
        var accepted = try checkAppUnder(std.testing.allocator, app("good"), &.{}, null, strategy);
        defer accepted.deinit();
        try accepted.expectErrors(false);
        var rejected = try checkAppUnder(std.testing.allocator, app("bad"), &.{}, null, strategy);
        defer rejected.deinit();
        try rejected.expectErrors(true);
    }
}

test "issue 13054: a conversion at a variable the platform's requirement does not fix is checked under both strategies" {
    try expectLiteralOutcomeUnderBothStrategies(sqlInMainResultApp);
}

test "issue 13054: a conversion in a procedure the platform invokes through an alias is checked under both strategies" {
    try expectLiteralOutcomeUnderBothStrategies(sqlInAliasedMainApp);
}

test "issue 13054: a conversion in a procedure the platform invokes through a computed value is checked under both strategies" {
    try expectLiteralOutcomeUnderBothStrategies(sqlInComputedMainApp);
}

test "issue 13054: a conversion at a use's variable nothing fixes is checked under both strategies" {
    try expectLiteralOutcomeUnderBothStrategies(sqlInHelperApp);
}

/// `helper`'s literal is at `Sql(a)` for an `a` that carries a requirement,
/// and `x = helper({})` relates `a` to nothing else: the use's copy of `a` is
/// a constrained variable no scheme quantifies, which is not decided.
const sealed_sql_app = app_header ++ "\n" ++ sql_type ++ "\n" ++
    \\tagged : Sql(a) -> Sql(a) where [a.label : a -> Str]
    \\tagged = |sql| sql
    \\helper = |{}| tagged("good")
    \\main! = |_args| {
    \\    x = helper({})
    \\    Echo.line!(x.text())
    \\    Ok({})
    \\}
;

test "a literal argument root at a sealed type stores its converted value under both strategies" {
    if (is_freestanding) return error.SkipZigTest;
    for ([_]base.SpecializationStrategy{ .lss, .boxy }) |strategy| {
        var checked = try checkAppUnder(std.testing.allocator, sealed_sql_app, &.{}, null, strategy);
        defer checked.deinit();
        try checked.expectErrors(false);
        // `x = helper({})` fixes no `n` of `Sql(n)`: the use's edge converts
        // the literal at the type `n` seals to, and the root stores the
        // value it converted, which every read restores.
        const root = checked.coord.appRootCheckedArtifact();
        var sealed: usize = 0;
        for (root.compile_time_roots.roots.items()) |compile_time_root| {
            const argument = compile_time_root.literal_argument orelse continue;
            if (!argument.sealed) continue;
            try std.testing.expect(!argument.validate_only);
            try std.testing.expect(compile_time_root.payload == .const_node);
            // A sealed root is requested although its type keeps the
            // variable no use fixes.
            try std.testing.expect(try root.checked_types.rootContainsIdentityVariables(argument.ty));
            sealed += 1;
        }
        try std.testing.expectEqual(@as(usize, 1), sealed);
    }
}

test "issue 13054: a local function's conversion at a use's variable nothing fixes is checked under both strategies" {
    try expectLiteralOutcomeUnderBothStrategies(sqlInLocalHelperApp);
}

test "issue 13054: a stored function's conversion at a variable nothing fixes is checked under both strategies" {
    try expectLiteralOutcomeUnderBothStrategies(sqlInStoredHelperApp);
}

test "issue 13054: a top-level call's conversion at a variable nothing fixes is checked under both strategies" {
    try expectLiteralOutcomeUnderBothStrategies(sqlInTopLevelCallApp);
}

/// A quote-converting type with a parameter its conversion never fixes,
/// whose conversion rejects the literal `"bad"`.
const decided_sql_type =
    \\Sql(a) := { text : Str }.{
    \\    from_quote : Str -> Try(Sql(a), [BadQuotedBytes(Str)])
    \\    from_quote = |text| if text == "bad" Err(BadQuotedBytes("bad is not sql")) else Ok(Sql.{ text: text })
    \\    show : Sql(a) -> Str
    \\    show = |sql| sql.text
    \\}
;

/// A helper whose literal is at `Sql(v)` for a `v` that no scheme quantifies
/// and nothing constrains (design.md "Decided Variables").
fn decidedLiteralApp(comptime literal: []const u8) []const u8 {
    return
    \\app [main!] { pf: platform "./.roc_echo_platform/main.roc" }
    \\import pf.Echo
    ++ "\n" ++ decided_sql_type ++ "\n" ++
        \\describe : Sql(a) -> Str
        \\describe = |sql| sql.show()
        \\helper : U64 -> Str
        \\helper = |_n| describe("
    ++ literal ++
        \\")
        \\main! = |args| {
        \\    Echo.line!(helper(List.len(args)))
        \\    Ok({})
        \\}
    ;
}

test "a literal at a decided type is a conversion root checking evaluates" {
    if (is_freestanding) return error.SkipZigTest;
    var checked = try checkApp(std.testing.allocator, decidedLiteralApp("good"), &.{}, null);
    defer checked.deinit();
    try checked.expectErrors(false);
    // The literal's variable is decided, so its conversion is closed: one
    // root at the type the variable seals to, which names no variable and
    // which evaluation completes and stores. No scheme carries the literal
    // as a requirement, and no runtime body is specialized to find it.
    const root = checked.coord.appRootCheckedArtifact();
    var stored: usize = 0;
    for (root.compile_time_roots.roots.items()) |compile_time_root| {
        if (compile_time_root.kind != .quote_conversion) continue;
        try std.testing.expectEqual(.eligible, compile_time_root.request_eligibility);
        try std.testing.expect(compile_time_root.payload == .const_node);
        try std.testing.expect(!try root.checked_types.rootContainsIdentityVariables(compile_time_root.checked_type));
        stored += 1;
    }
    try std.testing.expectEqual(@as(usize, 1), stored);
    for (root.checked_procedure_templates.templates.items()) |*template| {
        try std.testing.expectEqual(@as(usize, 0), root.checked_procedure_templates.templateLiteralRequirements(template).len);
    }
    const demand = checked.demand();
    try std.testing.expectEqual(@as(u64, 0), demand.discovery_roots);
    try std.testing.expectEqual(@as(u64, 0), demand.monotype.discovery_bodies);
}

test "checking reports a literal at a decided type that its conversion rejects" {
    if (is_freestanding) return error.SkipZigTest;
    var checked = try checkApp(std.testing.allocator, decidedLiteralApp("bad"), &.{}, null);
    defer checked.deinit();
    try checked.expectErrors(true);
}

/// A generic function whose literal converts at its own variable, called
/// where nothing fixes that variable: the call instantiates the literal's
/// requirement at a decided type.
fn decidedLiteralArgumentApp(comptime literal: []const u8) []const u8 {
    return
    \\app [main!] { pf: platform "./.roc_echo_platform/main.roc" }
    \\import pf.Echo
    ++ "\n" ++ decided_sql_type ++ "\n" ++
        \\make : U64 -> Sql(a)
        \\make = |_n| "
    ++ literal ++
        \\"
        \\main! = |args| {
        \\    Echo.line!(make(List.len(args)).show())
        \\    Ok({})
        \\}
    ;
}

test "a literal requirement instantiated at a decided type is a literal argument root checking evaluates" {
    if (is_freestanding) return error.SkipZigTest;
    var checked = try checkApp(std.testing.allocator, decidedLiteralArgumentApp("good"), &.{}, null);
    defer checked.deinit();
    try checked.expectErrors(false);
    // `make`'s scheme carries the literal, and `main!`'s call instantiates
    // its variable at a decided one: the instantiation is closed, so the
    // call's argument is a root at the type that variable seals to.
    const root = checked.coord.appRootCheckedArtifact();
    var stored: usize = 0;
    for (root.compile_time_roots.roots.items()) |compile_time_root| {
        if (compile_time_root.kind != .literal_argument) continue;
        try std.testing.expectEqual(.eligible, compile_time_root.request_eligibility);
        try std.testing.expect(compile_time_root.payload == .const_node);
        try std.testing.expect(!try root.checked_types.rootContainsIdentityVariables(compile_time_root.checked_type));
        stored += 1;
    }
    try std.testing.expectEqual(@as(usize, 1), stored);
    const demand = checked.demand();
    try std.testing.expectEqual(@as(u64, 0), demand.discovery_roots);
    try std.testing.expectEqual(@as(u64, 0), demand.monotype.discovery_bodies);
}

test "checking reports a literal requirement rejected at a decided instantiation" {
    if (is_freestanding) return error.SkipZigTest;
    var checked = try checkApp(std.testing.allocator, decidedLiteralArgumentApp("bad"), &.{}, null);
    defer checked.deinit();
    try checked.expectErrors(true);
}
