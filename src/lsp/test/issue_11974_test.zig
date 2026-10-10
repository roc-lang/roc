//! Regression test for https://github.com/roc-lang/roc/issues/11974
//!
//! Goto definition on the method name of a method call (`message.upcase()`)
//! must resolve to the method's definition inside the nominal type's
//! associated block, just as it does for the qualified `Message.upcase(message)`.

const std = @import("std");
const lsp = @import("lsp");
const SyntaxChecker = lsp.syntax.SyntaxChecker;
const uri_util = lsp.uri;
const integration_spec = @import("integration_spec.zig");
const test_env = @import("integration_env.zig");

/// Issue 11974 integration specs exported to the LSP harness.
pub const specs = [_]integration_spec.Spec{
    .{ .name = "issue 11974: goto definition on a method call's method name resolves to the method", .run = gotoDefinitionOnMethodCallResolvesToMethod },
    .{ .name = "issue 11974: goto definition on a method call resolves to a method of an imported type", .run = gotoDefinitionOnImportedTypeMethodCall },
    .{ .name = "issue 11974: goto definition on a method call resolves to a builtin method", .run = gotoDefinitionOnBuiltinMethodCall },
};

/// `upcase` is defined on line 4. The method call is on line 10 and the
/// qualified call on line 11.
const source_template =
    \\app [process_string] {{ pf: platform "{s}" }}
    \\
    \\Message := Str.{{
    \\    upcase : Message -> Str
    \\    upcase = |Message.(str)| Str.with_ascii_uppercased(str)
    \\}}
    \\
    \\process_string = |input| {{
    \\    message : Message
    \\    message = Message.(input)
    \\    a = message.upcase()
    \\    b = Message.upcase(message)
    \\    Str.concat(a, b)
    \\}}
    \\
;

/// `shout` is defined on line 4 of `Greeting.roc`. The method call is on
/// line 4 of the app.
const imported_source_template =
    \\app [process_string] {{ pf: platform "{s}" }}
    \\
    \\import Greeting
    \\
    \\process_string = |input| Greeting.make(input).shout()
    \\
;

const greeting_source =
    \\Greeting := Str.{
    \\    make : Str -> Greeting
    \\    make = |str| Greeting.(str)
    \\    shout : Greeting -> Str
    \\    shout = |Greeting.(str)| Str.with_ascii_uppercased(str)
    \\}
    \\
;

/// The method call `.concat("!")` on a `Str` is on line 3.
const builtin_source_template =
    \\app [process_string] {{ pf: platform "{s}" }}
    \\
    \\process_string = |input|
    \\    input.concat("!")
    \\
;

/// One checked app document, with any sibling modules it imports.
const Fixture = struct {
    tmp: test_env.TmpDir,
    source: []u8,
    file_path: [:0]u8,
    file_uri: []u8,
    checker: SyntaxChecker,

    fn init(fixture: *Fixture, comptime template: []const u8, siblings: []const Sibling) integration_spec.SpecError!void {
        const allocator = test_env.allocator;
        fixture.tmp = test_env.tmpDir(.{});
        errdefer fixture.tmp.cleanup();

        for (siblings) |sibling| {
            try fixture.tmp.dir.writeFile(test_env.io, .{ .sub_path = sibling.name, .data = sibling.source });
        }

        fixture.source = try std.fmt.allocPrint(allocator, template, .{test_env.tmp_dir_platform_path});
        errdefer allocator.free(fixture.source);

        try fixture.tmp.dir.writeFile(test_env.io, .{ .sub_path = "main.roc", .data = fixture.source });
        fixture.file_path = try fixture.tmp.dir.realPathFileAlloc(test_env.io, "main.roc", allocator);
        errdefer allocator.free(fixture.file_path);
        fixture.file_uri = try uri_util.pathToUri(allocator, fixture.file_path);
        errdefer allocator.free(fixture.file_uri);

        fixture.checker = SyntaxChecker.init(allocator, test_env.io, .{}, null);
        errdefer fixture.checker.deinit();
        fixture.checker.cache_config.cache_dir = std.fs.path.dirname(fixture.file_path) orelse fixture.file_path;

        const publish_sets = try fixture.checker.check(fixture.file_uri, fixture.source, null);
        defer {
            for (publish_sets) |*set| set.deinit(allocator);
            allocator.free(publish_sets);
        }
        var problem_count: usize = 0;
        for (publish_sets) |set| problem_count += set.diagnostics.len;
        try std.testing.expectEqual(@as(usize, 0), problem_count);
    }

    fn deinit(fixture: *Fixture) void {
        const allocator = test_env.allocator;
        fixture.checker.deinit();
        allocator.free(fixture.file_uri);
        allocator.free(fixture.file_path);
        allocator.free(fixture.source);
        fixture.tmp.cleanup();
    }

    /// Goto definition at `line`:`character` must land on `definition_line`
    /// of the file whose URI ends with `uri_suffix`.
    fn expectDefinition(fixture: *Fixture, line: u32, character: u32, uri_suffix: []const u8, definition_line: u32) integration_spec.SpecError!void {
        var result = try fixture.checker.getDefinitionAtPosition(fixture.file_uri, fixture.source, line, character) orelse {
            std.debug.print("\ngoto definition returned null at {d}:{d}\n", .{ line, character });
            return error.TestUnexpectedResult;
        };
        defer result.deinit(test_env.allocator);
        if (!std.mem.endsWith(u8, result.uri, uri_suffix)) {
            std.debug.print("\ngoto definition at {d}:{d} went to {s}, expected a URI ending in {s}\n", .{ line, character, result.uri, uri_suffix });
            return error.TestUnexpectedResult;
        }
        try std.testing.expectEqual(definition_line, result.range.start_line);
    }
};

const Sibling = struct {
    name: []const u8,
    source: []const u8,
};

fn gotoDefinitionOnMethodCallResolvesToMethod() integration_spec.SpecError!void {
    var fixture: Fixture = undefined;
    try fixture.init(source_template, &.{});
    defer fixture.deinit();

    // `b = Message.upcase(message)`: character 16 is on `upcase`.
    try fixture.expectDefinition(11, 16, "/main.roc", 4);
    // `a = message.upcase()`: character 16 is on `upcase`.
    try fixture.expectDefinition(10, 16, "/main.roc", 4);
    // `a = message.upcase()`: character 8 is on the receiver `message`.
    try fixture.expectDefinition(10, 8, "/main.roc", 9);
}

fn gotoDefinitionOnImportedTypeMethodCall() integration_spec.SpecError!void {
    var fixture: Fixture = undefined;
    try fixture.init(imported_source_template, &.{.{ .name = "Greeting.roc", .source = greeting_source }});
    defer fixture.deinit();

    // `Greeting.make(input).shout()`: character 47 is on `shout`.
    try fixture.expectDefinition(4, 47, "/Greeting.roc", 4);
}

fn gotoDefinitionOnBuiltinMethodCall() integration_spec.SpecError!void {
    var fixture: Fixture = undefined;
    try fixture.init(builtin_source_template, &.{});
    defer fixture.deinit();

    // `input.concat("!")`: character 12 is on `concat`.
    var result = try fixture.checker.getDefinitionAtPosition(fixture.file_uri, fixture.source, 3, 12) orelse {
        std.debug.print("\ngoto definition returned null on a builtin method call\n", .{});
        return error.TestUnexpectedResult;
    };
    defer result.deinit(test_env.allocator);
    try std.testing.expect(std.mem.endsWith(u8, result.uri, "/Builtin.roc"));
}
