//! Regression test for https://github.com/roc-lang/roc/issues/12292
//!
//! Hover on a fully qualified builtin function reference such as `List.len`
//! must show the documentation of that type's function, not the documentation
//! of a same-name function attached to a different builtin type. Hover reads
//! the documentation of the declaration the reference was resolved to, so
//! method calls, aliased imports, and associated items through a type alias
//! show their own declaration's documentation too.

const std = @import("std");
const lsp = @import("lsp");
const SyntaxChecker = lsp.syntax.SyntaxChecker;
const uri_util = lsp.uri;
const integration_spec = @import("integration_spec.zig");
const test_env = @import("integration_env.zig");

/// Hover documentation specs for qualified builtin function references.
pub const specs = [_]integration_spec.Spec{
    .{ .name = "issue 12292: hover on qualified List.len shows List.len documentation", .run = hoverOnQualifiedListLenShowsListLenDocs },
    .{ .name = "issue 12292: hover on qualified I64.abs shows I64.abs documentation", .run = hoverOnQualifiedI64AbsShowsI64AbsDocs },
    .{ .name = "issue 12292: hover on a builtin method call shows that method's documentation", .run = hoverOnBuiltinMethodCallShowsItsDocs },
    .{ .name = "issue 12292: hover on a value of an aliased import shows its documentation", .run = hoverOnAliasedImportValueShowsItsDocs },
    .{ .name = "issue 12292: hover on an associated item through a type alias shows its documentation", .run = hoverOnAliasAssociatedItemShowsItsDocs },
    .{ .name = "issue 12292: hover on a literal shows no documentation of the enclosing definition", .run = hoverOnLiteralShowsNoEnclosingDocs },
};

const Sibling = struct {
    name: []const u8,
    source: []const u8,
};

/// One checked document that built without problems.
const Fixture = struct {
    tmp: test_env.TmpDir,
    source: []u8,
    file_path: [:0]u8,
    file_uri: []u8,
    checker: SyntaxChecker,

    fn init(fixture: *Fixture, comptime template: []const u8, file_name: []const u8, siblings: []const Sibling) integration_spec.SpecError!void {
        const allocator = test_env.allocator;
        fixture.tmp = test_env.tmpDir(.{});
        errdefer fixture.tmp.cleanup();

        for (siblings) |sibling| {
            try fixture.tmp.dir.writeFile(test_env.io, .{ .sub_path = sibling.name, .data = sibling.source });
        }

        fixture.source = try std.fmt.allocPrint(allocator, template, .{test_env.tmp_dir_platform_path});
        errdefer allocator.free(fixture.source);

        try fixture.tmp.dir.writeFile(test_env.io, .{ .sub_path = file_name, .data = fixture.source });
        fixture.file_path = try fixture.tmp.dir.realPathFileAlloc(test_env.io, file_name, allocator);
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

    fn expectHover(fixture: *Fixture, line: u32, character: u32, expected: []const u8, unexpected: []const u8) integration_spec.SpecError!void {
        const result = try fixture.checker.getTypeAtPosition(fixture.file_uri, fixture.source, line, character) orelse {
            std.debug.print("\nhover returned null at {d}:{d}\n", .{ line, character });
            return error.TestUnexpectedResult;
        };
        defer test_env.allocator.free(result.type_str);
        if (std.mem.find(u8, result.type_str, expected) == null or
            std.mem.find(u8, result.type_str, unexpected) != null)
        {
            std.debug.print("\nhover at {d}:{d} was:\n{s}\nexpected it to contain \"{s}\" and not \"{s}\"\n", .{ line, character, result.type_str, expected, unexpected });
            return error.TestUnexpectedResult;
        }
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

/// Hover on `len` in `List.len(...)`, where `Str.len` is defined earlier in
/// the builtins.
fn hoverOnQualifiedListLenShowsListLenDocs() integration_spec.SpecError!void {
    var fixture: Fixture = undefined;
    try fixture.init(
        \\app [process_string] {{ pf: platform "{s}" }}
        \\
        \\process_string : Str -> Str
        \\process_string = |text| List.len([text]).to_str()
        \\
    , "hover_list_len_12292.roc", &.{});
    defer fixture.deinit();

    try fixture.expectHover(3, 30, "Returns the length of the list", "Unicode");
}

/// Hover on `abs` in `I64.abs(...)`, where `I8.abs` is defined earlier in the
/// builtins.
fn hoverOnQualifiedI64AbsShowsI64AbsDocs() integration_spec.SpecError!void {
    var fixture: Fixture = undefined;
    try fixture.init(
        \\app [process_string] {{ pf: platform "{s}" }}
        \\
        \\process_string : Str -> Str
        \\process_string = |_text| I64.abs(-5).to_str()
        \\
    , "hover_i64_abs_12292.roc", &.{});
    defer fixture.deinit();

    try fixture.expectHover(3, 30, "I64.abs(3)", "I8.abs");
}

/// Hover on `len` in `[text].len()`.
fn hoverOnBuiltinMethodCallShowsItsDocs() integration_spec.SpecError!void {
    var fixture: Fixture = undefined;
    try fixture.init(
        \\app [process_string] {{ pf: platform "{s}" }}
        \\
        \\process_string : Str -> Str
        \\process_string = |text| [text].len().to_str()
        \\
    , "hover_list_len_method_12292.roc", &.{});
    defer fixture.deinit();

    try fixture.expectHover(3, 32, "Returns the length of the list", "Unicode");
}

const greeting_source =
    \\Greeting := Str.{
    \\    ## Builds a greeting for a name.
    \\    make : Str -> Greeting
    \\    make = |str| Greeting.(str)
    \\
    \\    ## Shouts the greeting.
    \\    shout : Greeting -> Str
    \\    shout = |Greeting.(str)| Str.with_ascii_uppercased(str)
    \\}
    \\
;

/// Hover on `make` in `G.make(text)`, where `G` aliases the `Greeting` import.
fn hoverOnAliasedImportValueShowsItsDocs() integration_spec.SpecError!void {
    var fixture: Fixture = undefined;
    try fixture.init(
        \\app [process_string] {{ pf: platform "{s}" }}
        \\
        \\import Greeting as G
        \\
        \\process_string : Str -> Str
        \\process_string = |text| G.make(text).shout()
        \\
    , "main.roc", &.{.{ .name = "Greeting.roc", .source = greeting_source }});
    defer fixture.deinit();

    try fixture.expectHover(5, 27, "Builds a greeting for a name.", "Shouts");
    try fixture.expectDefinition(5, 27, "/Greeting.roc", 3);
}

/// Hover on `upcase` in `Loud.upcase(message)`, where `Loud` aliases `Message`.
fn hoverOnAliasAssociatedItemShowsItsDocs() integration_spec.SpecError!void {
    var fixture: Fixture = undefined;
    try fixture.init(
        \\app [process_string] {{ pf: platform "{s}" }}
        \\
        \\Message := Str.{{
        \\    ## Shouts the message.
        \\    upcase : Message -> Str
        \\    upcase = |Message.(str)| Str.with_ascii_uppercased(str)
        \\}}
        \\
        \\Loud : Message
        \\
        \\## Processes the string.
        \\process_string : Str -> Str
        \\process_string = |text| Loud.upcase(Message.(text))
        \\
    , "hover_alias_associated_12292.roc", &.{});
    defer fixture.deinit();

    try fixture.expectHover(12, 30, "Shouts the message.", "Processes");
    try fixture.expectDefinition(12, 30, "/hover_alias_associated_12292.roc", 5);
}

/// Hover on the string literal inside a documented definition.
fn hoverOnLiteralShowsNoEnclosingDocs() integration_spec.SpecError!void {
    var fixture: Fixture = undefined;
    try fixture.init(
        \\app [process_string] {{ pf: platform "{s}" }}
        \\
        \\## Processes the string.
        \\process_string : Str -> Str
        \\process_string = |text| Str.concat(text, "!")
        \\
    , "hover_literal_12292.roc", &.{});
    defer fixture.deinit();

    try fixture.expectHover(4, 42, "Str", "Processes");
}
