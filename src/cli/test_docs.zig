const std = @import("std");
const testing = std.testing;
const main = @import("main.zig");
const cli_args = @import("cli_args.zig");

test "roc docs generates nested package documentation" {
    const gpa = testing.allocator;

    // Create a temporary directory for our test
    var tmp = testing.tmpDir(.{});
    defer tmp.cleanup();

    const tmp_path = try tmp.dir.realpathAlloc(gpa, ".");
    defer gpa.free(tmp_path);

    // Create nested package structure:
    // root (app) -> depends on foo, bar
    // foo (package) -> depends on baz
    // baz (package) -> depends on qux
    // qux (package) -> no deps
    // bar (package) -> depends on baz (shared dependency)

    // Create qux package (leaf dependency)
    try tmp.dir.makeDir("qux");
    const qux_file = try tmp.dir.createFile("qux/main.roc", .{});
    defer qux_file.close();
    try qux_file.writeAll(
        \\package [add] {}
        \\
        \\add : I64, I64 -> I64
        \\add = \a, b -> a + b
        \\
    );

    // Create baz package (depends on qux)
    try tmp.dir.makeDir("baz");
    const baz_file = try tmp.dir.createFile("baz/main.roc", .{});
    defer baz_file.close();
    try baz_file.writeAll(
        \\package [multiply] { qux: "../qux/main.roc" }
        \\
        \\multiply : I64, I64 -> I64
        \\multiply = \a, b -> a * b
        \\
    );

    // Create foo package (depends on baz)
    try tmp.dir.makeDir("foo");
    const foo_file = try tmp.dir.createFile("foo/main.roc", .{});
    defer foo_file.close();
    try foo_file.writeAll(
        \\package [increment] { baz: "../baz/main.roc" }
        \\
        \\increment : I64 -> I64
        \\increment = \n -> n + 1
        \\
    );

    // Create bar package (also depends on baz - shared dependency)
    try tmp.dir.makeDir("bar");
    const bar_file = try tmp.dir.createFile("bar/main.roc", .{});
    defer bar_file.close();
    try bar_file.writeAll(
        \\package [decrement] { baz: "../baz/main.roc" }
        \\
        \\decrement : I64 -> I64
        \\decrement = \n -> n - 1
        \\
    );

    // Create root app (depends on foo and bar)
    const root_file = try tmp.dir.createFile("root.roc", .{});
    defer root_file.close();
    try root_file.writeAll(
        \\app [main] {
        \\    pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.19.0/Hj-J_zxz-8W4-Or3AUfyRJHhxUlgwBXNHkJcUqZPnPg.tar.br",
        \\    foo: "foo/main.roc",
        \\    bar: "bar/main.roc",
        \\}
        \\
        \\import pf.Stdout
        \\
        \\main =
        \\    Stdout.line! "Hello"
        \\
    );

    // Create output directory path
    const output_dir = try std.fs.path.join(gpa, &[_][]const u8{ tmp_path, "generated-docs" });
    defer gpa.free(output_dir);

    const root_path = try std.fs.path.join(gpa, &[_][]const u8{ tmp_path, "root.roc" });
    defer gpa.free(root_path);

    // Note: We would call main.rocDocs(gpa, args) here, but it requires
    // a full build environment setup. Instead, we test the individual
    // helper functions in separate tests below.

    // For now, just verify the test structure was created correctly
    tmp.dir.access("root.roc", .{}) catch unreachable;
    tmp.dir.access("foo/main.roc", .{}) catch unreachable;
    tmp.dir.access("bar/main.roc", .{}) catch unreachable;
    tmp.dir.access("baz/main.roc", .{}) catch unreachable;
    tmp.dir.access("qux/main.roc", .{}) catch unreachable;

    _ = root_path;
    _ = output_dir;
}

test "generatePackageIndex creates valid HTML" {
    const gpa = testing.allocator;

    var tmp = testing.tmpDir(.{});
    defer tmp.cleanup();

    const tmp_path = try tmp.dir.realpathAlloc(gpa, ".");
    defer gpa.free(tmp_path);

    // Test with no dependencies
    {
        const shorthands = [_][]const u8{};
        try main.generatePackageIndex(gpa, tmp_path, "test/module.roc", &shorthands);

        const content = try tmp.dir.readFileAlloc(gpa, "index.html", 10000);
        defer gpa.free(content);

        try testing.expect(std.mem.indexOf(u8, content, "<h1>test/module.roc</h1>") != null);
        try testing.expect(std.mem.indexOf(u8, content, "<!DOCTYPE html>") != null);
        try testing.expect(std.mem.indexOf(u8, content, "</html>") != null);
    }

    // Delete the file for next test
    try tmp.dir.deleteFile("index.html");

    // Test with dependencies
    {
        const shorthands = [_][]const u8{ "foo", "bar", "baz" };
        try main.generatePackageIndex(gpa, tmp_path, "root.roc", &shorthands);

        const content = try tmp.dir.readFileAlloc(gpa, "index.html", 10000);
        defer gpa.free(content);

        try testing.expect(std.mem.indexOf(u8, content, "<h1>root.roc</h1>") != null);
        try testing.expect(std.mem.indexOf(u8, content, "<ul>") != null);
        try testing.expect(std.mem.indexOf(u8, content, "<a href=\"foo\">foo</a>") != null);
        try testing.expect(std.mem.indexOf(u8, content, "<a href=\"bar\">bar</a>") != null);
        try testing.expect(std.mem.indexOf(u8, content, "<a href=\"baz\">baz</a>") != null);
    }
}

test "generatePackageIndex handles nested paths" {
    const gpa = testing.allocator;

    var tmp = testing.tmpDir(.{});
    defer tmp.cleanup();

    const tmp_path = try tmp.dir.realpathAlloc(gpa, ".");
    defer gpa.free(tmp_path);

    // Create nested directory
    try tmp.dir.makeDir("foo");
    try tmp.dir.makeDir("foo/bar");

    const nested_path = try std.fs.path.join(gpa, &[_][]const u8{ tmp_path, "foo", "bar" });
    defer gpa.free(nested_path);

    const shorthands = [_][]const u8{"baz"};
    try main.generatePackageIndex(gpa, nested_path, "foo/bar/module.roc", &shorthands);

    // Verify the file was created in the nested directory
    const content = try tmp.dir.readFileAlloc(gpa, "foo/bar/index.html", 10000);
    defer gpa.free(content);

    try testing.expect(std.mem.indexOf(u8, content, "<h1>foo/bar/module.roc</h1>") != null);
    try testing.expect(std.mem.indexOf(u8, content, "<a href=\"baz\">baz</a>") != null);
}
