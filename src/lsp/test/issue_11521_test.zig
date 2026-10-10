//! Regression tests for #11521 in the LSP: rename, references, highlights,
//! and go-to-definition must see every occurrence of a name even where
//! checking rejected the code around it. Checking replaces a rejected
//! expression or statement with a runtime error, and the LSP reads through
//! that replacement to the code as written.

const std = @import("std");
const lsp = @import("lsp");
const SyntaxChecker = lsp.syntax.SyntaxChecker;
const LspRange = lsp.cir_queries.LspRange;
const uri_util = lsp.uri;
const integration_spec = @import("integration_spec.zig");
const test_env = @import("integration_env.zig");

/// Issue 11521 integration specs exported to the LSP harness.
pub const specs = [_]integration_spec.Spec{
    .{ .name = "issue 11521: a call whose value is unused keeps its occurrence", .run = unusedCallValueKeepsOccurrence },
    .{ .name = "issue 11521: a rejected destructure and the uses of its bindings keep their occurrences", .run = rejectedDestructureKeepsOccurrences },
    .{ .name = "issue 11521: a local used only inside a rejected statement keeps its occurrence", .run = localInsideRejectedStatementKeepsOccurrence },
};

/// The issue's shape: `main!` calls `greet!` without using the `Try` it
/// returns, which checking rejects because a statement must evaluate to `{}`.
const unused_call_value_body =
    \\main! : () => {}
    \\main! = || {
    \\    greet!("hello")
    \\    {}
    \\}
    \\
    \\greet! : Str => Try({}, _)
    \\greet! = |text| {
    \\    Stdout.line!(text)
    \\    Ok({})
    \\}
;

/// Line 6 is the call, lines 10 and 11 the annotation and definition.
const greet_occurrences = [_]LspRange{
    .{ .start_line = 6, .start_col = 4, .end_line = 6, .end_col = 10 },
    .{ .start_line = 10, .start_col = 0, .end_line = 10, .end_col = 6 },
    .{ .start_line = 11, .start_col = 0, .end_line = 11, .end_col = 6 },
};

/// The literal `5` cannot match a `Str`, so checking rejects the whole
/// destructuring statement and every use of the `label` it binds.
const rejected_destructure_body =
    \\main! : () => {}
    \\main! = || {
    \\    (label, 5) = ("hello", "x")
    \\    Stdout.line!(label)
    \\}
;

/// Line 6 is the definition, line 7 the use.
const label_occurrences = [_]LspRange{
    .{ .start_line = 6, .start_col = 5, .end_line = 6, .end_col = 10 },
    .{ .start_line = 7, .start_col = 17, .end_line = 7, .end_col = 22 },
};

/// `name`'s only use is inside the statement checking rejects.
const local_in_rejected_statement_body =
    \\main! : () => {}
    \\main! = || {
    \\    name = "hello"
    \\    greet!(name)
    \\    {}
    \\}
    \\
    \\greet! : Str => Try({}, _)
    \\greet! = |text| {
    \\    Stdout.line!(text)
    \\    Ok({})
    \\}
;

/// Line 6 is the binding, line 7 the use inside the rejected call.
const name_occurrences = [_]LspRange{
    .{ .start_line = 6, .start_col = 4, .end_line = 6, .end_col = 8 },
    .{ .start_line = 7, .start_col = 11, .end_line = 7, .end_col = 15 },
};

/// `test/fx/platform/main.roc` relative to a `TmpDir`, which sits at
/// `.zig-cache/tmp/<name>`.
const platform_path = "../../../test/fx/platform/main.roc";

/// A checked document built on the `test/fx` platform, whose checking is
/// confirmed to report the rejection the spec depends on. The body follows a
/// four-line app header, so its first line is line 4 of the document.
const Document = struct {
    tmp: test_env.TmpDir,
    uri: []u8,
    source: []u8,
    checker: SyntaxChecker,

    fn init(self: *Document, body: []const u8) integration_spec.SpecError!void {
        const allocator = test_env.allocator;
        self.tmp = test_env.tmpDir(.{});
        errdefer self.tmp.cleanup();

        const tmp_path = try self.tmp.dir.realPathFileAlloc(test_env.io, ".", allocator);
        defer allocator.free(tmp_path);
        const file_path = try std.fs.path.join(allocator, &.{ tmp_path, "app.roc" });
        defer allocator.free(file_path);
        self.uri = try uri_util.pathToUri(allocator, file_path);
        errdefer allocator.free(self.uri);

        self.source = try std.fmt.allocPrint(
            allocator,
            "app [main!] {{ pf: platform \"{s}\" }}\n\nimport pf.Stdout\n\n{s}",
            .{ platform_path, body },
        );
        errdefer allocator.free(self.source);
        try self.tmp.dir.writeFile(test_env.io, .{ .sub_path = "app.roc", .data = self.source });

        self.checker = SyntaxChecker.init(allocator, test_env.io, .{}, null);
        test_env.configureChecker(&self.checker, ".zig-cache/tmp");
        errdefer self.checker.deinit();

        const publish_sets = try self.checker.check(self.uri, null, null);
        defer {
            for (publish_sets) |*set| set.deinit(allocator);
            allocator.free(publish_sets);
        }
        var diagnostic_count: usize = 0;
        for (publish_sets) |set| diagnostic_count += set.diagnostics.len;
        try std.testing.expect(diagnostic_count > 0);
    }

    fn deinit(self: *Document) void {
        const allocator = test_env.allocator;
        self.checker.deinit();
        allocator.free(self.source);
        allocator.free(self.uri);
        self.tmp.cleanup();
    }
};

fn lessThan(_: void, a: LspRange, b: LspRange) bool {
    if (a.start_line != b.start_line) return a.start_line < b.start_line;
    return a.start_col < b.start_col;
}

/// Compare occurrence sets, which the queries report in no particular order.
fn expectOccurrences(expected: []const LspRange, actual: []LspRange) integration_spec.SpecError!void {
    std.mem.sort(LspRange, actual, {}, lessThan);
    try std.testing.expectEqualSlices(LspRange, expected, actual);
}

fn expectRename(
    document: *Document,
    line: u32,
    character: u32,
    new_name: []const u8,
    old_name: []const u8,
    expected: []const LspRange,
) integration_spec.SpecError!void {
    const outcome = (try document.checker.getRenameEditsAtPosition(document.uri, document.source, line, character, new_name)) orelse
        return error.TestUnexpectedResult;
    switch (outcome) {
        .rejected => |rejection| {
            std.debug.print("rename at ({d},{d}) rejected: {any}\n", .{ line, character, rejection });
            return error.TestUnexpectedResult;
        },
        .edits => |result| {
            defer result.deinit(test_env.allocator);
            try std.testing.expectEqualStrings(old_name, result.old_name);
            try expectOccurrences(expected, result.regions);
        },
    }
}

fn expectHighlights(document: *Document, line: u32, character: u32, expected: []const LspRange) integration_spec.SpecError!void {
    const result = (try document.checker.getHighlightsAtPosition(document.uri, document.source, line, character)) orelse
        return error.TestUnexpectedResult;
    defer result.deinit(test_env.allocator);
    try expectOccurrences(expected, result.regions);
}

fn expectReferences(document: *Document, line: u32, character: u32, expected: []const LspRange) integration_spec.SpecError!void {
    const result = (try document.checker.getReferencesAtPosition(document.uri, document.source, line, character, true)) orelse
        return error.TestUnexpectedResult;
    defer result.deinit(test_env.allocator);
    try expectOccurrences(expected, result.regions);
}

fn expectDefinitionLine(document: *Document, line: u32, character: u32, definition_line: u32) integration_spec.SpecError!void {
    const result = (try document.checker.getDefinitionAtPosition(document.uri, document.source, line, character)) orelse
        return error.TestUnexpectedResult;
    defer result.deinit(test_env.allocator);
    try std.testing.expectEqualStrings(document.uri, result.uri);
    try std.testing.expectEqual(definition_line, result.range.start_line);
}

fn unusedCallValueKeepsOccurrence() integration_spec.SpecError!void {
    var document: Document = undefined;
    try document.init(unused_call_value_body);
    defer document.deinit();

    // From the call and from the definition alike, every occurrence moves.
    try expectRename(&document, 6, 4, "salute!", "greet!", &greet_occurrences);
    try expectRename(&document, 11, 0, "salute!", "greet!", &greet_occurrences);
    try expectHighlights(&document, 6, 4, &greet_occurrences);
    try expectReferences(&document, 11, 0, &greet_occurrences);
    try expectDefinitionLine(&document, 6, 4, 11);
}

fn rejectedDestructureKeepsOccurrences() integration_spec.SpecError!void {
    var document: Document = undefined;
    try document.init(rejected_destructure_body);
    defer document.deinit();

    try expectRename(&document, 7, 17, "title", "label", &label_occurrences);
    try expectRename(&document, 6, 5, "title", "label", &label_occurrences);
    try expectHighlights(&document, 7, 17, &label_occurrences);
    try expectReferences(&document, 6, 5, &label_occurrences);
    try expectDefinitionLine(&document, 7, 17, 6);
}

fn localInsideRejectedStatementKeepsOccurrence() integration_spec.SpecError!void {
    var document: Document = undefined;
    try document.init(local_in_rejected_statement_body);
    defer document.deinit();

    try expectRename(&document, 7, 11, "greeting", "name", &name_occurrences);
    try expectRename(&document, 6, 4, "greeting", "name", &name_occurrences);
    try expectHighlights(&document, 7, 11, &name_occurrences);
    try expectReferences(&document, 6, 4, &name_occurrences);
    try expectDefinitionLine(&document, 7, 11, 6);
}
