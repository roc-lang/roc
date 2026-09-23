//! Regression tests for https://github.com/roc-lang/roc/issues/11464.
//!
//! A module can re-export a nested type of an imported type module under an
//! alias (`Files : Resource.Files`). Types nested below that alias resolve
//! through it the same way its associated values do, so `Gui.Files.Dir.Read`
//! names `Resource.Files.Dir.Read`.

const TestEnv = @import("./TestEnv.zig");

test "issue 11464 - nested type resolves through a re-exported type-module alias" {
    const resource_src =
        \\Resource := [].{
        \\    Files := [].{
        \\        Dir := [].{
        \\            Read : {}
        \\        }
        \\
        \\        pick_directory! : {} => Dir.Read
        \\        pick_directory! = |_| {}
        \\    }
        \\}
    ;

    var resource_module = try TestEnv.init("Resource", resource_src);
    defer resource_module.deinit();

    try resource_module.assertNoErrors();

    const gui_src =
        \\import Resource
        \\
        \\Gui := [].{
        \\    Files : Resource.Files
        \\}
    ;

    var gui_module = try TestEnv.initWithImport("Gui", gui_src, "Resource", &resource_module);
    defer gui_module.deinit();

    try gui_module.assertNoErrors();

    const app_src =
        \\import Gui
        \\
        \\App := [].{
        \\    call! : {} => Gui.Files.Dir.Read
        \\    call! = |arg| Gui.Files.pick_directory!(arg)
        \\}
    ;

    var app_module = try TestEnv.initWithImport("App", app_src, "Gui", &gui_module);
    defer app_module.deinit();

    try app_module.assertNoErrors();
    try app_module.assertDefType("App.call!", "{} => Resource.Files.Dir.Read");
}
