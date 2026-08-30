//! Security regression tests for the package/platform effect boundary.
//!
//! Roc's central safety property for dependencies is that a *package* is pure:
//! only a *platform* can reach the host, so adding a third-party package cannot
//! give that dependency the ability to read files, open sockets, or otherwise
//! perform effects. That property is what makes a Roc dependency tree safe to
//! grow without auditing every package for supply-chain behavior.
//!
//! Nothing enforces that property in one place; it falls out of several
//! independent checks spread across parsing, package resolution, module
//! discovery, canonicalization, and type checking. Each test below drives one
//! escape route an attacker would try and asserts the compiler still refuses
//! it, so that a refactor which quietly drops one of those checks fails here
//! rather than in a released compiler.
//!
//! The last test is a positive control: a package may still *receive* an
//! effectful function from the app and call it. Capability passing is the
//! sanctioned way for a package to perform effects, and it must keep working;
//! otherwise these tests could "pass" against a compiler that simply rejects
//! everything.
//!
//! Three more routes are covered from `test/package-effect-boundary/` instead
//! of here: a package naming a platform as a dependency, a package header using
//! the app-only `platform` keyword, and a package shipping a module called
//! `Builtin`. Those are refused by package resolution and module discovery,
//! which `Coordinator.discoverAppFromPath` does not run, so only driving the
//! real `roc check` exercises them. Keep the two sets in sync: together they
//! are the regression suite for this boundary.

const std = @import("std");
const build_options = @import("build_options");
const collections = @import("collections");
const eval = @import("eval");
const roc_target = @import("roc_target");

const Coordinator = @import("../coordinator.zig").Coordinator;
const CoreCtx = @import("ctx").CoreCtx;

const File = struct {
    path: []const u8,
    data: []const u8,
};

/// Failures from staging a fixture's files on disk.
const StageError = std.Io.Dir.CreateDirPathError || std.Io.Dir.WriteFileError;

/// Everything `compileApp` can fail with: staging the fixture, then driving the
/// compiler over it.
const HarnessError = StageError ||
    std.mem.Allocator.Error ||
    std.Io.Dir.RealPathFileAllocError ||
    Coordinator.AppDiscoveryError ||
    eval.BuiltinModules.InitError ||
    std.Thread.SpawnError ||
    error{
        BuiltinLowLevelAnnotationMustBeFunction,
        LowLevelOperationsNotFound,
        UnsupportedBuiltinAnnotationOnly,
    };

/// Ways a fixture can fail its assertion.
const AssertionError = error{
    /// The compiler accepted code that breaks the package effect boundary.
    AttackNotBlocked,
    /// The attack was rejected, but not for the reason this test is guarding.
    WrongRejectionReason,
};

/// A minimal platform that publishes one real hosted effect (`Echo.line!`).
/// Tests point their app at this so the boundary they probe is a genuine
/// host boundary rather than a stub.
const platform_files = [_]File{
    .{
        .path = "pfroot/main.roc",
        .data =
        \\platform ""
        \\    requires {} { main! : List(Str) => Try(_, [Exit(I8), ..]) }
        \\    exposes [Echo]
        \\    packages {}
        \\    provides { "roc_main": main_for_host! }
        \\    hosted { "roc_echo_line": Echo.line! }
        \\
        \\import Echo
        \\
        \\main_for_host! : List(Str) => I8
        \\main_for_host! = |args|
        \\    match main!(args) {
        \\        Ok(_) => 0
        \\        Err(Exit(code)) => code
        \\        Err(_) => 1
        \\    }
        ,
    },
    .{
        .path = "pfroot/Echo.roc",
        .data =
        \\Echo := [].{
        \\    line! : Str => {}
        \\}
        ,
    },
};

/// What the compiler reported for one fixture.
const Outcome = struct {
    gpa: std.mem.Allocator,
    has_user_errors: bool,
    titles: std.ArrayList([]const u8),

    fn deinit(self: *Outcome) void {
        for (self.titles.items) |title| self.gpa.free(title);
        self.titles.deinit(self.gpa);
    }

    fn hasTitle(self: *const Outcome, want: []const u8) bool {
        for (self.titles.items) |title| {
            if (std.mem.eql(u8, title, want)) return true;
        }
        return false;
    }

    /// Assert the attack was refused, and refused for the stated reason.
    ///
    /// Matching the report title rather than just "some error happened" is
    /// deliberate: a fixture that starts failing for an unrelated reason (a
    /// syntax change, a renamed builtin) would still "reject" the attack while
    /// silently no longer testing the boundary at all.
    fn expectBlocked(self: *const Outcome, want_title: []const u8) AssertionError!void {
        if (!self.has_user_errors) {
            std.debug.print("expected the attack to be rejected, but it compiled clean\n", .{});
            return error.AttackNotBlocked;
        }
        if (self.hasTitle(want_title)) return;
        std.debug.print("expected a report titled \"{s}\", got:\n", .{want_title});
        for (self.titles.items) |title| std.debug.print("  - {s}\n", .{title});
        return error.WrongRejectionReason;
    }
};

fn writeFiles(io: std.Io, dir: std.Io.Dir, files: []const File) StageError!void {
    for (files) |file| {
        if (std.fs.path.dirname(file.path)) |sub_dir| {
            try dir.createDirPath(io, sub_dir);
        }
        try dir.writeFile(io, .{ .sub_path = file.path, .data = file.data });
    }
}

/// Write `files` alongside the shared test platform into a temp dir, compile
/// `entry_rel` as an app, and collect the resulting diagnostics.
fn compileApp(gpa: std.mem.Allocator, files: []const File, entry_rel: []const u8) HarnessError!Outcome {
    const io = std.testing.io;

    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    try writeFiles(io, tmp_dir.dir, &platform_files);
    try writeFiles(io, tmp_dir.dir, files);

    const app_path = try tmp_dir.dir.realPathFileAlloc(io, entry_rel, gpa);
    defer gpa.free(app_path);

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
        CoreCtx.default(gpa, arena, io),
    );
    defer coord.deinit();
    coord.enable_hosted_transform = true;

    try coord.start();
    try coord.discoverAppFromPath(arena, .{ .entry_path = app_path });
    try coord.coordinatorLoop();

    var titles = std.ArrayList([]const u8).empty;
    errdefer {
        for (titles.items) |title| gpa.free(title);
        titles.deinit(gpa);
    }

    var reports = coord.iterReports();
    while (reports.next()) |entry| {
        try titles.append(gpa, try gpa.dupe(u8, entry.report.title));
    }

    return .{
        .gpa = gpa,
        .has_user_errors = coord.hasUserErrors(),
        .titles = titles,
    };
}

test "package cannot declare its own hosted function" {
    // The most direct attack: a package writes the same annotation-only
    // declaration a platform uses to publish a hosted effect, then calls it.
    // Only the platform root package gets annotation-only declarations turned
    // into host-bound lambdas, so in a package these stay unimplemented.
    var outcome = try compileApp(std.testing.allocator, &.{
        .{ .path = "evil/main.roc", .data = "package [Evil] {}" },
        .{
            .path = "evil/Evil.roc",
            .data =
            \\Evil := [].{
            \\    leak! : Str => {}
            \\
            \\    steal! : Str => {}
            \\    steal! = |s| Evil.leak!(s)
            \\}
            ,
        },
        .{
            .path = "app.roc",
            .data =
            \\app [main!] {
            \\    pf: platform "./pfroot/main.roc",
            \\    evil: "./evil/main.roc",
            \\}
            \\
            \\import evil.Evil
            \\
            \\main! = |_args| {
            \\    Evil.steal!("secret")
            \\    Ok({})
            \\}
            ,
        },
    }, "app.roc");
    defer outcome.deinit();

    try outcome.expectBlocked("Declaration Has No Value");
}

test "package cannot escape its source root with parent imports" {
    // Relative imports support `../`, so a package could otherwise walk out of
    // its own directory and import the platform's hosted module directly.
    var outcome = try compileApp(std.testing.allocator, &.{
        .{ .path = "evil/main.roc", .data = "package [Escape] {}" },
        .{
            .path = "evil/Escape.roc",
            .data =
            \\import ../pfroot/Echo
            \\
            \\Escape := [].{
            \\    steal! : Str => {}
            \\    steal! = |s| Echo.line!(s)
            \\}
            ,
        },
        .{
            .path = "app.roc",
            .data =
            \\app [main!] {
            \\    pf: platform "./pfroot/main.roc",
            \\    evil: "./evil/main.roc",
            \\}
            \\
            \\import evil.Escape
            \\
            \\main! = |_args| {
            \\    Escape.steal!("secret")
            \\    Ok({})
            \\}
            ,
        },
    }, "app.roc");
    defer outcome.deinit();

    try outcome.expectBlocked("Import Escapes Package Root");
}

test "package cannot ship a hosted-header module" {
    // `hosted` is a header kind in its own right. Shipping one inside a package
    // must not publish host bindings; the declarations stay unimplemented.
    var outcome = try compileApp(std.testing.allocator, &.{
        .{ .path = "evil/main.roc", .data = "package [Sneak] {}" },
        .{
            .path = "evil/Sneak.roc",
            .data =
            \\hosted [line!]
            \\
            \\line! : Str => {}
            ,
        },
        .{
            .path = "app.roc",
            .data =
            \\app [main!] {
            \\    pf: platform "./pfroot/main.roc",
            \\    evil: "./evil/main.roc",
            \\}
            \\
            \\import evil.Sneak
            \\
            \\main! = |_args| Ok({})
            ,
        },
    }, "app.roc");
    defer outcome.deinit();

    try outcome.expectBlocked("Exposed But Not Defined");
}

test "pure package function cannot call an effectful capability" {
    // Capability passing is the sanctioned route for a package to cause
    // effects, and the type system is what keeps it honest: a package that
    // takes an effectful callback must say so with `=>`. Otherwise a package
    // could advertise a pure API while performing effects behind it.
    var outcome = try compileApp(std.testing.allocator, &.{
        .{ .path = "cap/main.roc", .data = "package [Cap] {}" },
        .{
            .path = "cap/Cap.roc",
            .data =
            \\Cap := [].{
            \\    sneak : (Str => {}) -> Str
            \\    sneak = |f| {
            \\        f("exfiltrated")
            \\        "done"
            \\    }
            \\}
            ,
        },
        .{
            .path = "app.roc",
            .data =
            \\app [main!] {
            \\    pf: platform "./pfroot/main.roc",
            \\    cap: "./cap/main.roc",
            \\}
            \\
            \\import cap.Cap
            \\import pf.Echo
            \\
            \\main! = |_args| {
            \\    _ = Cap.sneak(Echo.line!)
            \\    Ok({})
            \\}
            ,
        },
    }, "app.roc");
    defer outcome.deinit();

    try outcome.expectBlocked("Type Mismatch");
}

test "package can use an effectful capability the app passes explicitly" {
    // Positive control for every test above. The boundary is meant to block
    // packages that manufacture effects, not packages that honestly declare
    // they run an effectful callback the app handed them.
    var outcome = try compileApp(std.testing.allocator, &.{
        .{ .path = "cap/main.roc", .data = "package [Cap] {}" },
        .{
            .path = "cap/Cap.roc",
            .data =
            \\Cap := [].{
            \\    run! : (Str => {}) => Str
            \\    run! = |f| {
            \\        f("legit")
            \\        "done"
            \\    }
            \\}
            ,
        },
        .{
            .path = "app.roc",
            .data =
            \\app [main!] {
            \\    pf: platform "./pfroot/main.roc",
            \\    cap: "./cap/main.roc",
            \\}
            \\
            \\import cap.Cap
            \\import pf.Echo
            \\
            \\main! = |_args| {
            \\    _ = Cap.run!(Echo.line!)
            \\    Ok({})
            \\}
            ,
        },
    }, "app.roc");
    defer outcome.deinit();

    if (outcome.has_user_errors) {
        std.debug.print("capability passing should still compile, got:\n", .{});
        for (outcome.titles.items) |title| std.debug.print("  - {s}\n", .{title});
        return error.CapabilityPassingRejected;
    }
}
