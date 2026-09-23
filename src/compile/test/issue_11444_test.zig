//! Regression test for issue #11444: imported method schemes retain their
//! complete generated-codec evidence contract.

const std = @import("std");
const harness = @import("lower_to_lir_harness.zig");

test "issue 11444: a package method returning the open error union of a file-level Json.to_str lowers" {
    var dir = std.testing.tmpDir(.{});
    defer dir.cleanup();

    try dir.dir.writeFile(std.testing.io, .{
        .sub_path = "platform.roc",
        .data =
        \\platform ""
        \\    requires {} { main! : Str => Str }
        \\    exposes []
        \\    packages {}
        \\    provides { "roc_main": run! }
        \\run! = |input| main!(input)
        ,
    });
    try dir.dir.createDirPath(std.testing.io, "browser");
    try dir.dir.writeFile(std.testing.io, .{
        .sub_path = "browser/main.roc",
        .data = "package [Browser] {}\n",
    });
    try dir.dir.writeFile(std.testing.io, .{
        .sub_path = "browser/Browser.roc",
        .data =
        \\Browser := [].{
        \\    Page := { guid : Str }
        \\
        \\    open : {} -> Page
        \\    open = |{}| Browser.Page.{ guid: "page" }
        \\
        \\    navigate : Page, Str -> Try(Str, [NavigateError, ..e])
        \\    navigate = |_page, url| encode({ url, referer: url })
        \\}
        \\
        \\encode : { url : Str, referer : Str } -> Try(Str, [NavigateError, ..e])
        \\encode = |msg| Ok(Json.to_str(msg))
        ,
    });
    try dir.dir.writeFile(std.testing.io, .{
        .sub_path = "main.roc",
        .data =
        \\app [main!] { pf: platform "./platform.roc", browser: "./browser/main.roc" }
        \\
        \\import browser.Browser
        \\
        \\main! = |input| {
        \\    page = Browser.open({})
        \\    match page.navigate(input) {
        \\        Ok(json) => json
        \\        Err(_) => "navigate failed"
        \\    }
        \\}
        ,
    });

    const path = try dir.dir.realPathFileAlloc(std.testing.io, "main.roc", std.testing.allocator);
    defer std.testing.allocator.free(path);
    try harness.expectAppPathLowersToLir(path);
}
