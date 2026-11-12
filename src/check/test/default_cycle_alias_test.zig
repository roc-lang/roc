//! Tests for default-value cycles that pass through a construction written
//! through an alias.
//!
//! A local alias whose annotation names a local nominal declaration is a
//! name canonicalization resolved, so a construction through it omits the
//! same defaults as one written through the nominal type's own name and the
//! cycle is reported the same way: one "Default Value Cycle" per cycle, from
//! canonicalization. An alias that names its nominal type any other way is
//! resolved only by checking, which reports the cycle itself.

const TestEnv = @import("./TestEnv.zig");

/// The diagnostics of a default-value cycle that canonicalization finds: one
/// report for the cycle, then the construction that can no longer omit the
/// dropped default.
fn expectCanonicalizationFindsCycle(source: []const u8) TestEnv.TestEnvError!void {
    var env = try TestEnv.init("Repro", source);
    defer env.deinit();

    try env.assertCanErrors(&.{"Default Value Cycle"});
    try env.assertTypeErrorTitles(&.{"Invalid Nominal Record"});
}

test "a default that reaches itself is one cycle through the nominal name or an alias" {
    try expectCanonicalizationFindsCycle(
        \\Repro :: [].{}
        \\
        \\Cfg := { name : Str, depth : U64 ?? base.depth }
        \\
        \\base = Cfg.{ name: "x" }
    );
    try expectCanonicalizationFindsCycle(
        \\Repro :: [].{}
        \\
        \\Cfg := { name : Str, depth : U64 ?? base.depth }
        \\
        \\C : Cfg
        \\
        \\base = C.{ name: "x" }
    );
}

test "two defaults that reach each other are one cycle through the nominal name or an alias" {
    try expectCanonicalizationFindsCycle(
        \\Repro :: [].{}
        \\
        \\Cfg := { name : Str, depth : U64 ?? base.width, width : U64 ?? base.depth }
        \\
        \\base = Cfg.{ name: "x" }
    );
    try expectCanonicalizationFindsCycle(
        \\Repro :: [].{}
        \\
        \\Cfg := { name : Str, depth : U64 ?? base.width, width : U64 ?? base.depth }
        \\
        \\C : Cfg
        \\
        \\base = C.{ name: "x" }
    );
}

test "a default cycle through a chain of aliases is found by canonicalization" {
    try expectCanonicalizationFindsCycle(
        \\Repro :: [].{}
        \\
        \\Outer : Middle
        \\
        \\Middle : (Inner)
        \\
        \\Inner : Cfg
        \\
        \\Cfg := { name : Str, depth : U64 ?? base.depth }
        \\
        \\base = Outer.{ name: "x" }
    );
}

test "a default cycle through a parameterized alias is found by canonicalization" {
    try expectCanonicalizationFindsCycle(
        \\Repro :: [].{}
        \\
        \\Cfg(a) := { name : a, depth : U64 ?? base.depth }
        \\
        \\Named(a) : Cfg(a)
        \\
        \\base = Named.{ name: "x" }
    );
}

test "a construction through an alias that supplies the field has no cycle" {
    var env = try TestEnv.init("Repro",
        \\Repro :: [].{}
        \\
        \\Cfg := { name : Str, depth : U64 ?? base.depth }
        \\
        \\C : Cfg
        \\
        \\base = C.{ name: "x", depth: 1 }
        \\
        \\other = C.{ name: "y" }
    );
    defer env.deinit();

    try env.assertNoErrors();
}

test "a default cycle through an alias that names its type by a type argument is found by checking" {
    var env = try TestEnv.init("Repro",
        \\Repro :: [].{}
        \\
        \\Cfg := { name : Str, depth : U64 ?? base.depth }
        \\
        \\Same(a) : a
        \\
        \\C : Same(Cfg)
        \\
        \\base = C.{ name: "x" }
    );
    defer env.deinit();

    try env.assertCanErrors(&.{});
    try env.assertTypeErrorTitles(&.{"Recursive Default Value"});
}

test "a construction through an alias of another module's type gets its defaults from checking" {
    var settings = try TestEnv.init("Settings",
        \\Settings :: [].{
        \\    Cfg := { name : Str, depth : U64 ?? 3 }
        \\}
    );
    defer settings.deinit();
    try settings.assertNoErrors();

    var cross = try TestEnv.initWithImport("Cross",
        \\import Settings
        \\
        \\Cross :: [].{}
        \\
        \\C : Settings.Cfg
        \\
        \\base = C.{ name: "x" }
        \\
        \\depth = base.depth
    , "Settings", &settings);
    defer cross.deinit();

    try cross.assertNoErrors();
    try cross.assertDefType("depth", "U64");
}
