//! Regression test for https://github.com/roc-lang/roc/issues/12654
//!
//! A lambda parameter has no doc comment of its own, so hover on it must show
//! the documentation of its type's declaration, never the documentation of
//! the definition the lambda is written in. Completion of a parameter must not
//! carry that definition's documentation either, declarations inside a block
//! keep their own documentation, and a type declaration's documentation never
//! attaches to the definition below it. Doc blocks are found from the
//! annotation's recorded name, which associated items record too.

const std = @import("std");
const lsp = @import("lsp");
const SyntaxChecker = lsp.syntax.SyntaxChecker;
const uri_util = lsp.uri;
const integration_spec = @import("integration_spec.zig");
const test_env = @import("integration_env.zig");

/// Hover and completion documentation specs for bindings.
pub const specs = [_]integration_spec.Spec{
    .{ .name = "issue 12654: hover on a lambda parameter shows its type's documentation", .run = hoverOnLambdaParameterShowsTypeDocs },
    .{ .name = "issue 12654: hover on a lambda parameter's binding site shows its type's documentation", .run = hoverOnLambdaParameterBindingShowsTypeDocs },
    .{ .name = "issue 12654: completion of a lambda parameter omits the enclosing definition's documentation", .run = completionOfLambdaParameterOmitsEnclosingDocs },
    .{ .name = "issue 12654: hover on a block declaration shows its own documentation", .run = hoverOnBlockDeclarationShowsItsDocs },
    .{ .name = "issue 12654: hover on a definition below a type alias omits the alias documentation", .run = hoverBelowTypeAliasOmitsAliasDocs },
    .{ .name = "issue 12654: an associated item's annotation name belongs to the item", .run = associatedItemAnnotationNameBelongsToItem },
};

const source_template =
    \\app [process_string] {{ pf: platform "{s}" }}
    \\
    \\## Doc comment for `LogLevel`
    \\LogLevel := [Urgent, Info, Debug]
    \\
    \\## Doc comment for `itself`
    \\itself : LogLevel -> LogLevel
    \\itself = |name| {{
    \\    name
    \\}}
    \\
    \\## Doc comment for `shout`
    \\shout : Str -> Str
    \\shout = |text| {{
    \\    ## Doc comment for `trimmed`
    \\    trimmed : Str
    \\    trimmed = Str.trim(text)
    \\    ## Doc comment for `loud`
    \\    loud = Str.concat(trimmed, "!")
    \\    loud
    \\}}
    \\
    \\process_string : Str -> Str
    \\process_string = |text| {{
    \\    _ = itself(Urgent)
    \\    shout(text)
    \\}}
    \\
    \\## Doc comment for `Color`
    \\Color : [Red, Blue]
    \\counter = 5
    \\
    \\Greeting := Str.{{
    \\    ## Doc comment for `make`
    \\    make : Str -> Greeting
    \\    make = |str| Greeting.(str)
    \\}}
    \\
;

/// The issue's program, checked without problems.
const Fixture = struct {
    tmp: test_env.TmpDir,
    source: []u8,
    file_path: [:0]u8,
    file_uri: []u8,
    checker: SyntaxChecker,

    fn init(self: *Fixture) integration_spec.SpecError!void {
        const allocator = test_env.allocator;
        self.tmp = test_env.tmpDir(.{});
        errdefer self.tmp.cleanup();

        self.source = try std.fmt.allocPrint(allocator, source_template, .{test_env.tmp_dir_platform_path});
        errdefer allocator.free(self.source);

        const file_name = "hover_lambda_param_12654.roc";
        try self.tmp.dir.writeFile(test_env.io, .{ .sub_path = file_name, .data = self.source });
        self.file_path = try self.tmp.dir.realPathFileAlloc(test_env.io, file_name, allocator);
        errdefer allocator.free(self.file_path);
        self.file_uri = try uri_util.pathToUri(allocator, self.file_path);
        errdefer allocator.free(self.file_uri);

        self.checker = SyntaxChecker.init(allocator, test_env.io, .{}, null);
        errdefer self.checker.deinit();
        self.checker.cache_config.cache_dir = std.fs.path.dirname(self.file_path) orelse self.file_path;

        const publish_sets = try self.checker.check(self.file_uri, self.source, null, null);
        defer {
            for (publish_sets) |*set| set.deinit(allocator);
            allocator.free(publish_sets);
        }
        var problem_count: usize = 0;
        for (publish_sets) |set| {
            problem_count += set.diagnostics.len;
            for (set.diagnostics) |diagnostic| std.debug.print("\n{s}\n", .{diagnostic.message});
        }
        try std.testing.expectEqual(@as(usize, 0), problem_count);
    }

    fn deinit(self: *Fixture) void {
        const allocator = test_env.allocator;
        self.checker.deinit();
        allocator.free(self.file_uri);
        allocator.free(self.file_path);
        allocator.free(self.source);
        self.tmp.cleanup();
    }

    /// Hover text at a zero-based line and character.
    fn hover(self: *Fixture, line: u32, character: u32) integration_spec.SpecError![]const u8 {
        const result = try self.checker.getTypeAtPosition(self.file_uri, self.source, line, character) orelse {
            std.debug.print("\nhover returned null at {d}:{d}\n", .{ line, character });
            return error.TestUnexpectedResult;
        };
        return result.type_str;
    }
};

/// Fails unless `text` contains every string in `present` and none in `absent`.
fn expectText(what: []const u8, text: []const u8, present: []const []const u8, absent: []const []const u8) integration_spec.SpecError!void {
    for (present) |needle| {
        if (std.mem.find(u8, text, needle) == null) {
            std.debug.print("\n{s} was:\n{s}\nexpected it to contain `{s}`\n", .{ what, text, needle });
            return error.TestUnexpectedResult;
        }
    }
    for (absent) |needle| {
        if (std.mem.find(u8, text, needle) != null) {
            std.debug.print("\n{s} was:\n{s}\nexpected it not to contain `{s}`\n", .{ what, text, needle });
            return error.TestUnexpectedResult;
        }
    }
}

/// Hover on `name` in the body of `itself = |name| { name }`, where `name` is a
/// `LogLevel`.
fn hoverOnLambdaParameterShowsTypeDocs() integration_spec.SpecError!void {
    var fixture: Fixture = undefined;
    try fixture.init();
    defer fixture.deinit();

    // Inside `name` on line 8 (zero-based).
    const text = try fixture.hover(8, 5);
    defer test_env.allocator.free(text);
    try expectText("hover at 8:5", text, &.{ "LogLevel", "Doc comment for `LogLevel`" }, &.{"Doc comment for `itself`"});
}

/// Hover on `name` where `itself = |name|` binds it.
fn hoverOnLambdaParameterBindingShowsTypeDocs() integration_spec.SpecError!void {
    var fixture: Fixture = undefined;
    try fixture.init();
    defer fixture.deinit();

    // Inside `name` in `itself = |name| {` on line 7 (zero-based).
    const text = try fixture.hover(7, 11);
    defer test_env.allocator.free(text);
    try expectText("hover at 7:11", text, &.{ "LogLevel", "Doc comment for `LogLevel`" }, &.{"Doc comment for `itself`"});
}

/// Complete `na` in the body of `itself`.
fn completionOfLambdaParameterOmitsEnclosingDocs() integration_spec.SpecError!void {
    var fixture: Fixture = undefined;
    try fixture.init();
    defer fixture.deinit();

    const incomplete = try std.mem.replaceOwned(u8, test_env.allocator, fixture.source, "{\n    name\n}", "{\n    na\n}");
    defer test_env.allocator.free(incomplete);

    // After `na` on line 8 (zero-based).
    const result = try fixture.checker.getCompletionsAtPosition(fixture.file_uri, incomplete, 8, 6) orelse
        return error.TestUnexpectedResult;
    defer result.deinit();

    for (result.items) |item| {
        if (!std.mem.eql(u8, item.label, "name")) continue;
        if (item.documentation) |doc| {
            try expectText("documentation of the `name` completion", doc, &.{}, &.{"Doc comment for `itself`"});
        }
        return;
    }
    std.debug.print("\nno `name` completion at 8:6\n", .{});
    return error.TestUnexpectedResult;
}

/// Hover on uses of an annotated and an unannotated declaration inside the
/// block of `shout`.
fn hoverOnBlockDeclarationShowsItsDocs() integration_spec.SpecError!void {
    var fixture: Fixture = undefined;
    try fixture.init();
    defer fixture.deinit();

    // Inside `trimmed` in `loud = Str.concat(trimmed, "!")` on line 18 (zero-based).
    const trimmed_text = try fixture.hover(18, 24);
    defer test_env.allocator.free(trimmed_text);
    try expectText("hover at 18:24", trimmed_text, &.{"Doc comment for `trimmed`"}, &.{"Doc comment for `shout`"});

    // Inside `loud` on line 19 (zero-based).
    const loud_text = try fixture.hover(19, 5);
    defer test_env.allocator.free(loud_text);
    try expectText("hover at 19:5", loud_text, &.{"Doc comment for `loud`"}, &.{ "Doc comment for `shout`", "Doc comment for `trimmed`" });
}

/// Hover on `counter`, an undocumented definition directly below the
/// declaration of the type alias `Color`.
fn hoverBelowTypeAliasOmitsAliasDocs() integration_spec.SpecError!void {
    var fixture: Fixture = undefined;
    try fixture.init();
    defer fixture.deinit();

    // Inside `counter` on line 30 (zero-based).
    const text = try fixture.hover(30, 2);
    defer test_env.allocator.free(text);
    try expectText("hover at 30:2", text, &.{}, &.{"Doc comment for `Color`"});
}

/// Hover and highlight on `make`, an annotated associated item of `Greeting`.
fn associatedItemAnnotationNameBelongsToItem() integration_spec.SpecError!void {
    var fixture: Fixture = undefined;
    try fixture.init();
    defer fixture.deinit();

    // Inside `make` in `make = |str| ...` on line 35 (zero-based).
    const text = try fixture.hover(35, 5);
    defer test_env.allocator.free(text);
    try expectText("hover at 35:5", text, &.{"Doc comment for `make`"}, &.{});

    const highlights = try fixture.checker.getHighlightsAtPosition(fixture.file_uri, fixture.source, 35, 5) orelse
        return error.TestUnexpectedResult;
    defer highlights.deinit(test_env.allocator);
    for (highlights.regions) |range| {
        if (range.start_line == 34 and range.start_col == 4) return;
    }
    std.debug.print("\nhighlights of `make` at 35:5 miss the annotation name at 34:4\n", .{});
    return error.TestUnexpectedResult;
}
