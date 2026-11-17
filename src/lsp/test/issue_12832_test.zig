//! Regression tests for https://github.com/roc-lang/roc/issues/12832.
//! Renaming an associated function must use its source name and edit both
//! its annotation and definition, regardless of which occurrence starts it.

const std = @import("std");
const lsp = @import("lsp");
const SyntaxChecker = lsp.syntax.SyntaxChecker;
const LspRange = lsp.cir_queries.LspRange;
const integration_spec = @import("integration_spec.zig");
const test_env = @import("integration_env.zig");

/// Issue 12832 integration specs exported to the LSP harness.
pub const specs = [_]integration_spec.Spec{
    .{ .name = "issue 12832: prepare rename on an associated function annotation", .run = prepareAnnotation },
    .{ .name = "issue 12832: prepare rename on an associated function definition", .run = prepareDefinition },
    .{ .name = "issue 12832: rename an associated function from its annotation", .run = renameAnnotation },
    .{ .name = "issue 12832: rename an associated function from its definition", .run = renameDefinition },
    .{ .name = "issue 12832: rename preserves qualifiers and unrelated associated names", .run = qualifiedOccurrences },
    .{ .name = "issue 12832: rename rejects a sibling declaration conflict", .run = siblingConflict },
    .{ .name = "issue 12832: rename rejects capture of an edited reference", .run = editedReferenceCapture },
    .{ .name = "issue 12832: rename rejects capture of an unedited reference", .run = uneditedReferenceCapture },
    .{ .name = "issue 12832: rename includes concrete receiver method calls", .run = receiverMethod },
    .{ .name = "issue 12832: rename supports unannotated nested associated functions", .run = nestedUnannotated },
    .{ .name = "issue 12832: generic method uses prevent a partial rename", .run = genericMethod },
    .{ .name = "issue 12832: rename ranges respect UTF-16 and CRLF", .run = unicodeAndCrlf },
};

const source =
    \\Validate :: [].{
    \\    run : Str -> List(Str)
    \\    run = |_str| { ... }
    \\}
    \\
;

const occurrences = [_]LspRange{
    .{ .start_line = 1, .start_col = 4, .end_line = 1, .end_col = 7 },
    .{ .start_line = 2, .start_col = 4, .end_line = 2, .end_col = 7 },
};

const Fixture = struct {
    tmp: test_env.TmpDir,
    file_path: [:0]u8,
    uri: []u8,
    checker: SyntaxChecker,

    fn init(self: *Fixture) integration_spec.SpecError!void {
        try self.initSource(source);
    }

    fn initSource(self: *Fixture, text: []const u8) integration_spec.SpecError!void {
        const allocator = test_env.allocator;
        self.tmp = test_env.tmpDir(.{});
        errdefer self.tmp.cleanup();
        try self.tmp.dir.writeFile(test_env.io, .{ .sub_path = "Validate.roc", .data = text });
        self.file_path = try self.tmp.dir.realPathFileAlloc(test_env.io, "Validate.roc", allocator);
        errdefer allocator.free(self.file_path);
        self.uri = try lsp.uri.pathToUri(allocator, self.file_path);
        errdefer allocator.free(self.uri);
        self.checker = SyntaxChecker.init(allocator, test_env.io, .{}, null);
        errdefer self.checker.deinit();
        self.checker.cache_config.cache_dir = std.fs.path.dirname(self.file_path) orelse self.file_path;

        const publish_sets = try self.checker.check(self.uri, text, null, null);
        defer {
            for (publish_sets) |*set| set.deinit(allocator);
            allocator.free(publish_sets);
        }
        var diagnostic_count: usize = 0;
        for (publish_sets) |set| {
            diagnostic_count += set.diagnostics.len;
            for (set.diagnostics) |diagnostic| std.debug.print("\n{s}\n", .{diagnostic.message});
        }
        try std.testing.expectEqual(@as(usize, 0), diagnostic_count);
    }

    fn deinit(self: *Fixture) void {
        self.checker.deinit();
        test_env.allocator.free(self.uri);
        test_env.allocator.free(self.file_path);
        self.tmp.cleanup();
    }
};

fn expectPrepared(occurrence: LspRange) integration_spec.SpecError!void {
    var fixture: Fixture = undefined;
    try fixture.init();
    defer fixture.deinit();

    const prepared = (try fixture.checker.prepareRenameAtPosition(fixture.uri, source, occurrence.start_line, occurrence.start_col + 1)) orelse {
        std.debug.print("prepare rename returned null at {d}:{d}\n", .{ occurrence.start_line, occurrence.start_col + 1 });
        return error.TestUnexpectedResult;
    };
    defer prepared.deinit(test_env.allocator);
    try std.testing.expectEqualStrings("run", prepared.placeholder);
    try std.testing.expectEqualDeep(occurrence, prepared.range);
}

fn expectRenamed(occurrence: LspRange) integration_spec.SpecError!void {
    var fixture: Fixture = undefined;
    try fixture.init();
    defer fixture.deinit();

    const outcome = (try fixture.checker.getRenameEditsAtPosition(fixture.uri, source, occurrence.start_line, occurrence.start_col + 1, "execute")) orelse
        return error.TestUnexpectedResult;
    switch (outcome) {
        .rejected => |rejection| {
            std.debug.print("rename rejected at {d}:{d}: {any}\n", .{ occurrence.start_line, occurrence.start_col + 1, rejection });
            return error.TestUnexpectedResult;
        },
        .edits => |result| {
            defer result.deinit(test_env.allocator);
            try std.testing.expectEqualStrings("run", result.old_name);
            std.mem.sort(LspRange, result.regions, {}, struct {
                fn lessThan(_: void, a: LspRange, b: LspRange) bool {
                    if (a.start_line != b.start_line) return a.start_line < b.start_line;
                    return a.start_col < b.start_col;
                }
            }.lessThan);
            try std.testing.expectEqualSlices(LspRange, &occurrences, result.regions);
        },
    }
}

fn prepareAnnotation() integration_spec.SpecError!void {
    try expectPrepared(occurrences[0]);
}

fn prepareDefinition() integration_spec.SpecError!void {
    try expectPrepared(occurrences[1]);
}

fn renameAnnotation() integration_spec.SpecError!void {
    try expectRenamed(occurrences[0]);
}

fn renameDefinition() integration_spec.SpecError!void {
    try expectRenamed(occurrences[1]);
}

/// Apply the actual edits, so an extra edit or a lost qualifier fails loudly.
fn expectSourceRename(text: []const u8, line: u32, col: u32, expected: []const u8) integration_spec.SpecError!void {
    var fixture: Fixture = undefined;
    try fixture.initSource(text);
    defer fixture.deinit();
    const allocator = test_env.allocator;
    const outcome = (try fixture.checker.getRenameEditsAtPosition(fixture.uri, text, line, col, "execute")) orelse
        return error.TestUnexpectedResult;
    const result = switch (outcome) {
        .edits => |result| result,
        .rejected => |reason| {
            std.debug.print("rename rejected: {any}\n", .{reason});
            return error.TestUnexpectedResult;
        },
    };
    defer result.deinit(allocator);
    try std.testing.expectEqualStrings("run", result.old_name);
    var offsets = try lsp.position.LineOffsets.init(allocator, text);
    defer offsets.deinit();
    var edited: std.ArrayList(u8) = .empty;
    defer edited.deinit(allocator);
    var previous: usize = 0;
    for (result.regions) |range| {
        const start = offsets.offsetAt(range.start_line, range.start_col).?;
        const end = offsets.offsetAt(range.end_line, range.end_col).?;
        try std.testing.expect(start >= previous);
        try std.testing.expectEqualStrings("run", text[start..end]);
        try edited.appendSlice(allocator, text[previous..start]);
        try edited.appendSlice(allocator, "execute");
        previous = end;
    }
    try edited.appendSlice(allocator, text[previous..]);
    try std.testing.expectEqualStrings(expected, edited.items);

    // Speculative validation must not replace the open document's snapshot.
    const prepared = (try fixture.checker.prepareRenameAtPosition(fixture.uri, text, line, col)) orelse
        return error.TestUnexpectedResult;
    defer prepared.deinit(allocator);
    try std.testing.expectEqualStrings("run", prepared.placeholder);
}

fn qualifiedOccurrences() integration_spec.SpecError!void {
    const text =
        \\Validate :: [].{
        \\    run : Str -> Str
        \\    run = |str| str
        \\    call = |str| run(str)
        \\}
        \\Alias : Validate
        \\use = |str| Alias.run(Validate.run(str))
        \\Other :: [].{
        \\    execute = |str| str
        \\    run = |str| str
        \\}
        \\
    ;
    const expected =
        \\Validate :: [].{
        \\    execute : Str -> Str
        \\    execute = |str| str
        \\    call = |str| execute(str)
        \\}
        \\Alias : Validate
        \\use = |str| Alias.execute(Validate.execute(str))
        \\Other :: [].{
        \\    execute = |str| str
        \\    run = |str| str
        \\}
        \\
    ;
    try expectSourceRename(text, 6, 19, expected);

    var fixture: Fixture = undefined;
    try fixture.initSource(text);
    defer fixture.deinit();
    const refs = (try fixture.checker.getReferencesAtPosition(fixture.uri, text, 6, 19, true)) orelse
        return error.TestUnexpectedResult;
    defer refs.deinit(test_env.allocator);
    const highlights = (try fixture.checker.getHighlightsAtPosition(fixture.uri, text, 2, 5)) orelse
        return error.TestUnexpectedResult;
    defer highlights.deinit(test_env.allocator);
    try std.testing.expectEqual(@as(usize, 5), refs.regions.len);
    try std.testing.expectEqualSlices(LspRange, refs.regions, highlights.regions);
    // The type qualifier is not the function's editable name.
    if (try fixture.checker.prepareRenameAtPosition(fixture.uri, text, 6, 14)) |prepared| {
        defer prepared.deinit(test_env.allocator);
        return error.TestUnexpectedResult;
    }
}

fn expectRejected(text: []const u8, line: u32, col: u32) integration_spec.SpecError!void {
    var fixture: Fixture = undefined;
    try fixture.initSource(text);
    defer fixture.deinit();
    const outcome = (try fixture.checker.getRenameEditsAtPosition(fixture.uri, text, line, col, "execute")) orelse
        return error.TestUnexpectedResult;
    switch (outcome) {
        .edits => |result| {
            defer result.deinit(test_env.allocator);
            return error.TestUnexpectedResult;
        },
        .rejected => |reason| try std.testing.expectEqual(SyntaxChecker.RenameRejection.changes_resolution, reason),
    }
}

fn siblingConflict() integration_spec.SpecError!void {
    try expectRejected(
        \\Validate :: [].{
        \\    run = |str| str
        \\    execute = |str| str
        \\}
        \\
    , 1, 5);
}

fn editedReferenceCapture() integration_spec.SpecError!void {
    try expectRejected(
        \\Validate :: [].{
        \\    run = |str| str
        \\    call = |execute| run(execute)
        \\}
        \\
    , 1, 5);
}

fn uneditedReferenceCapture() integration_spec.SpecError!void {
    try expectRejected(
        \\execute = |str| str
        \\Validate :: [].{
        \\    run = |str| str
        \\    call = |str| execute(str)
        \\}
        \\
    , 2, 5);
}

fn receiverMethod() integration_spec.SpecError!void {
    try expectSourceRename(
        \\Validate := Str.{
        \\    run : Validate -> Str
        \\    run = |Validate.(str)| str
        \\}
        \\use = || Validate.("hello").run()
        \\
    , 2, 5,
        \\Validate := Str.{
        \\    execute : Validate -> Str
        \\    execute = |Validate.(str)| str
        \\}
        \\use = || Validate.("hello").execute()
        \\
    );
}

fn nestedUnannotated() integration_spec.SpecError!void {
    try expectSourceRename(
        \\Validate :: [].{
        \\    Inner :: [].{
        \\        run = |str| str
        \\    }
        \\}
        \\use = |str| Validate.Inner.run(str)
        \\
    , 2, 9,
        \\Validate :: [].{
        \\    Inner :: [].{
        \\        execute = |str| str
        \\    }
        \\}
        \\use = |str| Validate.Inner.execute(str)
        \\
    );
}

fn genericMethod() integration_spec.SpecError!void {
    const text =
        \\Validate := Str.{
        \\    run : Validate -> Str
        \\    run = |Validate.(str)| str
        \\}
        \\generic = |value| value.run()
        \\
    ;
    var fixture: Fixture = undefined;
    try fixture.initSource(text);
    defer fixture.deinit();
    const prepared = try fixture.checker.prepareRenameAtPosition(fixture.uri, text, 2, 5);
    if (prepared) |result| {
        defer result.deinit(test_env.allocator);
        return error.TestUnexpectedResult;
    }
    const outcome = (try fixture.checker.getRenameEditsAtPosition(fixture.uri, text, 2, 5, "execute")) orelse
        return error.TestUnexpectedResult;
    switch (outcome) {
        .edits => |result| {
            defer result.deinit(test_env.allocator);
            return error.TestUnexpectedResult;
        },
        .rejected => |reason| try std.testing.expectEqual(SyntaxChecker.RenameRejection.ambiguous_method, reason),
    }
}

fn unicodeAndCrlf() integration_spec.SpecError!void {
    try expectSourceRename(
        "Validate :: [].{\r\n" ++
            "    run = |str| str\r\n" ++
            "}\r\n" ++
            "use = || (\"😀\", Validate.run(\"hi\"))\r\n",
        1,
        5,
        "Validate :: [].{\r\n" ++
            "    execute = |str| str\r\n" ++
            "}\r\n" ++
            "use = || (\"😀\", Validate.execute(\"hi\"))\r\n",
    );
}
