//! Regression tests for https://github.com/roc-lang/roc/issues/12958.
//!
//! An alias of a nominal type names that nominal type, so constructing through
//! the alias (`Alias.{ ... }` or `Alias.Tag`) builds the aliased nominal value,
//! exactly as constructing through the nominal type's own name does. The alias
//! does not widen visibility: an alias of an opaque (`::`) type still cannot be
//! constructed outside the module that declares the type.

const std = @import("std");
const TestEnv = @import("./TestEnv.zig");

test "issue 12958 - record construction through a local alias builds the nominal" {
    var env = try TestEnv.init("Repro",
        \\Repro :: [].{}
        \\
        \\Point := { x : U64, y : U64 }
        \\
        \\Pt : Point
        \\
        \\a : Point
        \\a = Point.{ x: 1, y: 2 }
        \\
        \\b : Pt
        \\b = Pt.{ x: 1, y: 2 }
        \\
        \\c = Pt.{ x: 3, y: 4 }
        \\
        \\expect a.x == b.x
    );
    defer env.deinit();

    try env.assertNoErrors();
    try env.assertDefType("c", "Point");
}

test "issue 12958 - construction through a parameterized local alias builds the nominal" {
    var env = try TestEnv.init("Repro",
        \\Repro :: [].{}
        \\
        \\Pair(a) := { first : a, second : a }
        \\
        \\Both(a) : Pair(a)
        \\
        \\p = Both.{ first: "x", second: "y" }
    );
    defer env.deinit();

    try env.assertNoErrors();
    try env.assertDefType("p", "Pair(Str)");
}

test "issue 12958 - tag construction through a local alias of a nominal tag union" {
    var env = try TestEnv.init("Repro",
        \\Repro :: [].{}
        \\
        \\Color := [Red, Green]
        \\
        \\Hue : Color
        \\
        \\c = Hue.Red
    );
    defer env.deinit();

    try env.assertNoErrors();
    try env.assertDefType("c", "Color");
}

test "issue 12958 - construction through an imported alias builds the nominal" {
    var api = try TestEnv.init("Api",
        \\Api :: [].{
        \\    Shape(a) := { key : U64, content : a }
        \\
        \\    Item(a) : Shape(a)
        \\
        \\    count : List(Item(a)) -> U64
        \\    count = |items| List.len(items)
        \\}
    );
    defer api.deinit();
    try api.assertNoErrors();

    var cross = try TestEnv.initWithImport("Cross",
        \\import Api
        \\
        \\Cross :: [].{}
        \\
        \\n = Api.count([Api.Item.{ key: 1, content: "x" }])
        \\
        \\expect n == 1
    , "Api", &api);
    defer cross.deinit();

    try cross.assertNoErrors();
    try cross.assertDefType("n", "U64");
}

test "issue 12958 - an imported alias of an opaque type cannot construct it" {
    var api = try TestEnv.init("Api",
        \\Api :: [].{
        \\    Secret :: { key : U64 }
        \\
        \\    Hidden : Secret
        \\}
    );
    defer api.deinit();
    try api.assertNoErrors();

    var cross = try TestEnv.initWithImport("Cross",
        \\import Api
        \\
        \\Cross :: [].{}
        \\
        \\s = Api.Hidden.{ key: 1 }
    , "Api", &api);
    defer cross.deinit();

    const diagnostics = try cross.module_env.getDiagnostics();
    defer cross.gpa.free(diagnostics);
    try std.testing.expectEqual(@as(usize, 0), diagnostics.len);
    try cross.assertFirstTypeError("Cannot Use Opaque Nominal Type");
}

test "issue 12958 - construction through an alias of an alias builds the nominal" {
    var env = try TestEnv.init("Repro",
        \\Repro :: [].{}
        \\
        \\Color := [Red, Green]
        \\
        \\Hue : Color
        \\
        \\Shade : Hue
        \\
        \\c = Shade.Green
    );
    defer env.deinit();

    try env.assertNoErrors();
    try env.assertDefType("c", "Color");
}

test "issue 12958 - a pattern through a local alias matches the nominal" {
    var env = try TestEnv.init("Repro",
        \\Repro :: [].{}
        \\
        \\Color := [Red, Green]
        \\
        \\Hue : Color
        \\
        \\name : Color -> Str
        \\name = |color| match color {
        \\    Hue.Red => "red"
        \\    Hue.Green => "green"
        \\}
    );
    defer env.deinit();

    try env.assertNoErrors();
    try env.assertDefType("name", "Color -> Str");
}

test "issue 12958 - construction through an alias omits a defaulted field" {
    var env = try TestEnv.init("Repro",
        \\Repro :: [].{}
        \\
        \\Cfg := { count : U8 ?? 10, name : Str }
        \\
        \\Settings : Cfg
        \\
        \\s = Settings.{ name: "x" }
    );
    defer env.deinit();

    try env.assertNoErrors();
    try env.assertDefType("s", "Cfg");
}

test "issue 12958 - construction through an alias of a structural type is rejected" {
    var env = try TestEnv.init("Repro",
        \\Repro :: [].{}
        \\
        \\Rec : { x : U64 }
        \\
        \\r = Rec.{ x: 1 }
    );
    defer env.deinit();

    const diagnostics = try env.module_env.getDiagnostics();
    defer env.gpa.free(diagnostics);
    try std.testing.expectEqual(@as(usize, 0), diagnostics.len);
    try env.assertOneTypeError("Expected Nominal Type");
}

test "issue 12958 - an imported alias of an imported module's type builds that type" {
    var shapes = try TestEnv.init("Shapes",
        \\Shapes :: [].{
        \\    Shape := { key : U64 }
        \\}
    );
    defer shapes.deinit();
    try shapes.assertNoErrors();

    var api = try TestEnv.initWithImport("Api",
        \\import Shapes
        \\
        \\Api :: [].{
        \\    Item : Shapes.Shape
        \\}
    , "Shapes", &shapes);
    defer api.deinit();
    try api.assertNoErrors();

    var cross = try TestEnv.initWithImport("Cross",
        \\import Api
        \\
        \\Cross :: [].{}
        \\
        \\i = Api.Item.{ key: 1 }
    , "Api", &api);
    defer cross.deinit();

    try cross.assertNoErrors();
    try cross.assertDefType("i", "Shapes.Shape");
}

test "issue 12958 - a type reached through a re-exported alias can be constructed" {
    var resource = try TestEnv.init("Resource",
        \\Resource := [].{
        \\    Files := [].{
        \\        Dir := { path : Str }
        \\    }
        \\}
    );
    defer resource.deinit();
    try resource.assertNoErrors();

    var gui = try TestEnv.initWithImport("Gui",
        \\import Resource
        \\
        \\Gui := [].{
        \\    Files : Resource.Files
        \\}
    , "Resource", &resource);
    defer gui.deinit();
    try gui.assertNoErrors();

    var app = try TestEnv.initWithImport("App",
        \\import Gui
        \\
        \\App := [].{
        \\    dir = Gui.Files.Dir.{ path: "x" }
        \\}
    , "Gui", &gui);
    defer app.deinit();

    try app.assertNoErrors();
    try app.assertDefType("App.dir", "Resource.Files.Dir");
}
