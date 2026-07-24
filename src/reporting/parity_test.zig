//! Cross-target parity suite for the document renderer.
//!
//! Renders corpora that exercise every `DocumentElement` and `Annotation`
//! variant to all four render targets and asserts the target-independent
//! invariants: annotated content survives every target, the plain targets
//! (LSP, markup-stripped terminal) reproduce the bare text exactly, and the
//! caret rows drawn under source regions are byte-identical between the
//! targets that draw them. Both corpora are comptime-enumerated from the
//! types, so adding a variant fails compilation here until it is covered.
//! Exact per-target byte pins for a small fixture live at the bottom; they
//! are regenerated intentionally whenever output is meant to change.

const std = @import("std");
const testing = std.testing;

const Allocator = std.mem.Allocator;
const reporting = @import("mod.zig");
const Document = reporting.Document;
const DocumentElement = reporting.DocumentElement;
const Annotation = reporting.Annotation;
const ColorPalette = reporting.ColorPalette;
const ReportingConfig = reporting.ReportingConfig;

/// A document rendered to every target.
const Outputs = struct {
    terminal_ansi: []u8,
    terminal_plain: []u8,
    markdown: []u8,
    html: []u8,
    lsp: []u8,

    fn deinit(self: *Outputs, gpa: Allocator) void {
        gpa.free(self.terminal_ansi);
        gpa.free(self.terminal_plain);
        gpa.free(self.markdown);
        gpa.free(self.html);
        gpa.free(self.lsp);
    }
};

fn renderAllTargets(gpa: Allocator, document: *const Document) (Allocator.Error || error{WriteFailed})!Outputs {
    var terminal_ansi = std.Io.Writer.Allocating.init(gpa);
    errdefer terminal_ansi.deinit();
    try reporting.renderDocumentToTerminal(document, &terminal_ansi.writer, ColorPalette.ANSI, ReportingConfig.initColorTerminal());

    var terminal_plain = std.Io.Writer.Allocating.init(gpa);
    errdefer terminal_plain.deinit();
    try reporting.renderDocumentToTerminal(document, &terminal_plain.writer, ColorPalette.NO_COLOR, ReportingConfig.initMarkdown());

    var markdown = std.Io.Writer.Allocating.init(gpa);
    errdefer markdown.deinit();
    try reporting.renderDocumentToMarkdown(document, &markdown.writer, ReportingConfig.initMarkdown());

    var html = std.Io.Writer.Allocating.init(gpa);
    errdefer html.deinit();
    try reporting.renderDocumentToHtml(document, &html.writer, ReportingConfig.initHtml());

    var lsp = std.Io.Writer.Allocating.init(gpa);
    errdefer lsp.deinit();
    try reporting.renderDocumentToLsp(document, &lsp.writer, ReportingConfig.initLsp());

    return .{
        .terminal_ansi = try terminal_ansi.toOwnedSlice(),
        .terminal_plain = try terminal_plain.toOwnedSlice(),
        .markdown = try markdown.toOwnedSlice(),
        .html = try html.toOwnedSlice(),
        .lsp = try lsp.toOwnedSlice(),
    };
}

fn countOccurrences(haystack: []const u8, needle: []const u8) usize {
    var count: usize = 0;
    var i: usize = 0;
    while (std.mem.findPos(u8, haystack, i, needle)) |pos| {
        count += 1;
        i = pos + needle.len;
    }
    return count;
}

/// Copy `bytes` with every ANSI `ESC [ ... m` sequence removed.
fn stripAnsi(gpa: Allocator, bytes: []const u8) Allocator.Error![]u8 {
    var out = std.array_list.Managed(u8).init(gpa);
    errdefer out.deinit();
    var i: usize = 0;
    while (i < bytes.len) {
        if (bytes[i] == 0x1b and i + 1 < bytes.len and bytes[i + 1] == '[') {
            i += 2;
            while (i < bytes.len and bytes[i] != 'm') i += 1;
            if (i < bytes.len) i += 1;
        } else {
            try out.append(bytes[i]);
            i += 1;
        }
    }
    return out.toOwnedSlice();
}

/// Copy `bytes` with every occurrence of `char` removed.
fn stripChar(gpa: Allocator, bytes: []const u8, char: u8) Allocator.Error![]u8 {
    var out = std.array_list.Managed(u8).init(gpa);
    errdefer out.deinit();
    for (bytes) |b| {
        if (b != char) try out.append(b);
    }
    return out.toOwnedSlice();
}

test "every annotation preserves its content on every render target" {
    const gpa = testing.allocator;
    const payload = "AnnotationPayload";

    inline for (comptime std.enums.values(Annotation)) |annotation| {
        var doc = Document.init(gpa);
        defer doc.deinit();
        try doc.addText("before ");
        try doc.addAnnotated(payload, annotation);
        try doc.addText(" after");

        var outputs = try renderAllTargets(gpa, &doc);
        defer outputs.deinit(gpa);

        // Every target keeps the content, exactly once — no annotation may
        // drop or duplicate what it decorates.
        try testing.expectEqual(@as(usize, 1), countOccurrences(outputs.terminal_ansi, payload));
        try testing.expectEqual(@as(usize, 1), countOccurrences(outputs.terminal_plain, payload));
        try testing.expectEqual(@as(usize, 1), countOccurrences(outputs.markdown, payload));
        try testing.expectEqual(@as(usize, 1), countOccurrences(outputs.html, payload));
        try testing.expectEqual(@as(usize, 1), countOccurrences(outputs.lsp, payload));

        // The plain targets agree on the visible text: LSP strips all markup,
        // and the colored terminal is the same text once colors are removed.
        const plain = "before " ++ payload ++ " after";
        try testing.expectEqualStrings(plain, outputs.lsp);

        const stripped = try stripAnsi(gpa, outputs.terminal_ansi);
        defer gpa.free(stripped);
        try testing.expectEqualStrings(plain, stripped);

        // The no-color terminal may add backticks around code spans; nothing
        // else may differ from the plain text.
        const unticked = try stripChar(gpa, outputs.terminal_plain, '`');
        defer gpa.free(unticked);
        try testing.expectEqualStrings(plain, unticked);
    }
}

test "annotation regions preserve nested content on every render target" {
    const gpa = testing.allocator;

    inline for (comptime std.enums.values(Annotation)) |annotation| {
        var doc = Document.init(gpa);
        defer doc.deinit();
        try doc.startAnnotation(annotation);
        try doc.addText("outer one ");
        try doc.startAnnotation(.emphasized);
        try doc.addText("inner");
        try doc.endAnnotation();
        try doc.addText(" outer two");
        try doc.endAnnotation();

        var outputs = try renderAllTargets(gpa, &doc);
        defer outputs.deinit(gpa);

        const plain = "outer one inner outer two";
        try testing.expectEqualStrings(plain, outputs.lsp);
        try testing.expectEqualStrings(plain, outputs.markdown);
        try testing.expectEqualStrings(plain, outputs.terminal_plain);

        const stripped = try stripAnsi(gpa, outputs.terminal_ansi);
        defer gpa.free(stripped);
        try testing.expectEqualStrings(plain, stripped);

        // HTML interleaves region tags with the text, so the plain string is
        // split across them; each segment must still appear exactly once, with
        // one balanced tag pair per region.
        try testing.expectEqual(@as(usize, 1), countOccurrences(outputs.html, "outer one "));
        try testing.expectEqual(@as(usize, 1), countOccurrences(outputs.html, "inner"));
        try testing.expectEqual(@as(usize, 1), countOccurrences(outputs.html, " outer two"));
        try testing.expectEqual(@as(usize, 2), countOccurrences(outputs.html, "</"));
    }
}

/// The distinctive payload each element variant carries into the corpus, or
/// null for purely structural variants.
fn payloadFor(comptime tag: std.meta.Tag(DocumentElement)) ?[]const u8 {
    return switch (tag) {
        .text => "text payload",
        .annotated => "annotated payload",
        .raw => "raw payload",
        .reflowing_text => "reflowing payload",
        .link => "https://example.test/payload",
        .vertical_stack => "stack payload one",
        .horizontal_concat => "concat payload two",
        .source_code_region => "value payload",
        .source_code_with_underlines => "alpha payload",
        .source_code_multi_region => "multi payload",
        .line_break, .indent, .space, .horizontal_rule, .annotation_start, .annotation_end => null,
    };
}

/// Add an element exercising `tag` to `doc`. Exhaustive over the element
/// tags: adding a `DocumentElement` variant fails to compile here until the
/// parity corpus covers it.
fn addElementVariant(comptime tag: std.meta.Tag(DocumentElement), doc: *Document) Allocator.Error!void {
    switch (tag) {
        .text => try doc.addText("text payload"),
        .annotated => try doc.addAnnotated("annotated payload", .keyword),
        .line_break => try doc.addLineBreak(),
        .indent => try doc.addIndent(2),
        .space => try doc.addSpace(3),
        .horizontal_rule => try doc.addHorizontalRule(5),
        .annotation_start => try doc.startAnnotation(.suggestion),
        .annotation_end => try doc.endAnnotation(),
        .raw => try doc.addRaw("raw payload"),
        .reflowing_text => try doc.addReflowingText("reflowing payload"),
        .link => try doc.addLink("https://example.test/payload"),
        .vertical_stack => try doc.addVerticalStack(&.{
            .{ .text = "stack payload one" },
            .{ .text = "stack payload two" },
        }),
        .horizontal_concat => try doc.addHorizontalConcat(&.{
            .{ .text = "concat payload one " },
            .{ .text = "concat payload two" },
        }),
        .source_code_region => try doc.addSourceRegion(
            .{ .start_line_idx = 0, .start_col_idx = 6, .end_line_idx = 0, .end_col_idx = 13 },
            .error_highlight,
            "corpus.roc",
            "value payload = 1",
            &[_]u32{0},
        ),
        .source_code_with_underlines => {
            // `addSourceCodeWithUnderlines` takes ownership of the line text.
            const line_text = try doc.allocator.dupe(u8, "alpha payload\nbeta payload");
            try doc.addSourceCodeWithUnderlines(.{
                .line_text = line_text,
                .start_line = 1,
                .start_column = 1,
                .end_line = 2,
                .end_column = 13,
                .region_annotation = .error_highlight,
                .filename = "corpus.roc",
            }, &.{
                .{ .start_line = 1, .start_column = 7, .end_line = 1, .end_column = 14, .annotation = .error_highlight },
                .{ .start_line = 2, .start_column = 6, .end_line = 2, .end_column = 13, .annotation = .warning_highlight },
            });
        },
        .source_code_multi_region => try doc.addSourceMultiRegion(
            "multi payload source",
            &.{
                .{ .start_line = 1, .start_column = 1, .end_line = 1, .end_column = 6, .annotation = .error_highlight },
            },
            "corpus.roc",
        ),
    }
}

test "every document element variant renders on every target" {
    const gpa = testing.allocator;

    inline for (comptime std.enums.values(std.meta.Tag(DocumentElement))) |tag| {
        var doc = Document.init(gpa);
        defer doc.deinit();
        try addElementVariant(tag, &doc);

        var outputs = try renderAllTargets(gpa, &doc);
        defer outputs.deinit(gpa);

        if (comptime payloadFor(tag)) |payload| {
            try testing.expect(countOccurrences(outputs.terminal_ansi, payload) >= 1);
            try testing.expect(countOccurrences(outputs.terminal_plain, payload) >= 1);
            try testing.expect(countOccurrences(outputs.markdown, payload) >= 1);
            try testing.expect(countOccurrences(outputs.html, payload) >= 1);
            try testing.expect(countOccurrences(outputs.lsp, payload) >= 1);
        }
    }
}

/// Collect the caret rows (lines of only whitespace and `^`, with at least one
/// `^`) from a rendered output. For the terminal target the line-number gutter
/// (everything through `│ `) is stripped first, so rows are comparable with
/// markdown's gutterless rows. Returned slices point into `out`.
fn collectCaretRows(gpa: Allocator, out: []const u8, comptime strip_gutter: bool) Allocator.Error!std.array_list.Managed([]const u8) {
    var rows = std.array_list.Managed([]const u8).init(gpa);
    errdefer rows.deinit();
    var it = std.mem.splitScalar(u8, out, '\n');
    while (it.next()) |raw_line| {
        var line = raw_line;
        if (strip_gutter) {
            const gutter = "│ ";
            const pos = std.mem.find(u8, line, gutter) orelse continue;
            line = line[pos + gutter.len ..];
        }
        if (line.len == 0) continue;
        var has_caret = false;
        var only_caret_row_chars = true;
        for (line) |c| switch (c) {
            '^' => has_caret = true,
            ' ', '\t' => {},
            else => only_caret_row_chars = false,
        };
        if (has_caret and only_caret_row_chars) try rows.append(line);
    }
    return rows;
}

test "caret rows are byte-identical between markdown and plain terminal" {
    const gpa = testing.allocator;

    var doc = Document.init(gpa);
    defer doc.deinit();

    // A tab-led line: the caret row must mirror the tab so the carets stay
    // under the span on both targets.
    try doc.addSourceRegion(
        .{ .start_line_idx = 0, .start_col_idx = 10, .end_line_idx = 0, .end_col_idx = 15 },
        .error_highlight,
        "carets.roc",
        "\tresult = alpha + beta",
        &[_]u32{0},
    );
    try doc.addLineBreak();

    // Two spans on one line: gap padding between spans.
    const line_text = try gpa.dupe(u8, "one two three");
    try doc.addSourceCodeWithUnderlines(.{
        .line_text = line_text,
        .start_line = 1,
        .start_column = 1,
        .end_line = 1,
        .end_column = 14,
        .region_annotation = .error_highlight,
        .filename = "carets.roc",
    }, &.{
        .{ .start_line = 1, .start_column = 1, .end_line = 1, .end_column = 4, .annotation = .error_highlight },
        .{ .start_line = 1, .start_column = 9, .end_line = 1, .end_column = 14, .annotation = .warning_highlight },
    });

    var outputs = try renderAllTargets(gpa, &doc);
    defer outputs.deinit(gpa);

    var markdown_rows = try collectCaretRows(gpa, outputs.markdown, false);
    defer markdown_rows.deinit();
    var terminal_rows = try collectCaretRows(gpa, outputs.terminal_plain, true);
    defer terminal_rows.deinit();

    try testing.expectEqual(@as(usize, 2), markdown_rows.items.len);
    try testing.expectEqual(markdown_rows.items.len, terminal_rows.items.len);
    for (markdown_rows.items, terminal_rows.items) |md_row, term_row| {
        try testing.expectEqualStrings(md_row, term_row);
    }

    // The tab survives into both caret rows.
    try testing.expectEqual(@as(u8, '\t'), markdown_rows.items[0][0]);
}

test "source lines appear on every render target" {
    const gpa = testing.allocator;

    var doc = Document.init(gpa);
    defer doc.deinit();
    const line_text = try gpa.dupe(u8, "first source line\nsecond source line");
    try doc.addSourceCodeWithUnderlines(.{
        .line_text = line_text,
        .start_line = 3,
        .start_column = 1,
        .end_line = 4,
        .end_column = 18,
        .region_annotation = .error_highlight,
        .filename = "lines.roc",
    }, &.{
        .{ .start_line = 3, .start_column = 7, .end_line = 3, .end_column = 13, .annotation = .error_highlight },
    });

    var outputs = try renderAllTargets(gpa, &doc);
    defer outputs.deinit(gpa);

    inline for (.{ "terminal_ansi", "terminal_plain", "markdown", "html", "lsp" }) |field| {
        const out = @field(outputs, field);
        try testing.expectEqual(@as(usize, 1), countOccurrences(out, "first source line"));
        try testing.expectEqual(@as(usize, 1), countOccurrences(out, "second source line"));
    }
}

// Byte-identity pins: exact rendered output per target for one small fixture.
// These freeze the concrete markup so any unintended change to a style table
// or hook shows up as a diff here; update them deliberately when output is
// meant to change.

fn buildPinnedFixture(gpa: Allocator) Allocator.Error!Document {
    var doc = Document.init(gpa);
    errdefer doc.deinit();
    try doc.addText("Expected ");
    try doc.addAnnotated("Str", .type_variable);
    try doc.addText(" but got ");
    try doc.addAnnotated("U64", .error_highlight);
    try doc.addText(".");
    try doc.addLineBreak();
    try doc.addIndent(1);
    try doc.addAnnotated("main", .symbol);
    try doc.addLineBreak();
    try doc.addSourceRegion(
        .{ .start_line_idx = 0, .start_col_idx = 4, .end_line_idx = 0, .end_col_idx = 9 },
        .error_highlight,
        "pin.roc",
        "x = magic 42",
        &[_]u32{0},
    );
    return doc;
}

test "byte-identity pins per render target" {
    const gpa = testing.allocator;
    var doc = try buildPinnedFixture(gpa);
    defer doc.deinit();

    var outputs = try renderAllTargets(gpa, &doc);
    defer outputs.deinit(gpa);

    try testing.expectEqualStrings(
        "Expected _Str_ but got **U64**.\n" ++
            "    `main`\n" ++
            "**pin.roc:1:5:1:10:**\n" ++
            "```roc\nx = magic 42\n```\n" ++
            "    ^^^^^\n",
        outputs.markdown,
    );

    try testing.expectEqualStrings(
        "Expected Str but got U64.\n" ++
            "  main\n" ++
            "pin.roc:1:5:1:10: x = magic 42\n",
        outputs.lsp,
    );

    try testing.expectEqualStrings(
        "Expected <span class=\"type\">Str</span> but got <span class=\"error\">U64</span>.<br>\n" ++
            "&nbsp;&nbsp;&nbsp;&nbsp;<span class=\"symbol\">main</span><br>\n" ++
            "<div class=\"source-region\"><span class=\"filename\">pin.roc:1:5:1:10:</span> " ++
            "<pre class=\"error\">x = magic 42</pre></div>",
        outputs.html,
    );

    const sec = "\x1b[90m";
    const rst = "\x1b[0m";
    const red = "\x1b[31m";
    const blue = "\x1b[34m";
    const cyan = "\x1b[36m";
    try testing.expectEqualStrings(
        "Expected " ++ blue ++ "Str" ++ rst ++ " but got " ++ red ++ "U64" ++ rst ++ ".\n" ++
            "    " ++ cyan ++ "main" ++ rst ++ "\n" ++
            " " ++ sec ++ " ┌" ++ ("─" ** 65) ++ " pin.roc:1:5\n" ++ rst ++
            " " ++ sec ++ " │\n" ++ rst ++
            sec ++ "1 │ " ++ rst ++ red ++ "x = magic 42" ++ rst ++ "\n" ++
            sec ++ "  │ " ++ rst ++ "    " ++ red ++ "^^^^^" ++ rst ++ "\n",
        outputs.terminal_ansi,
    );

    // The no-color terminal is the same layout with backticks marking the
    // code span in place of color.
    try testing.expectEqualStrings(
        "Expected Str but got U64.\n" ++
            "    `main`\n" ++
            "  ┌" ++ ("─" ** 65) ++ " pin.roc:1:5\n" ++
            "  │\n" ++
            "1 │ x = magic 42\n" ++
            "  │     ^^^^^\n",
        outputs.terminal_plain,
    );
}
