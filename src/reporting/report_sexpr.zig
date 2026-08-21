//! Canonical S-expression serialization of diagnostic reports.
//!
//! This is the presentation-independent form of a `Report`: it captures the
//! semantic structure (severity, title, source regions, document elements,
//! annotations) without any renderer-specific details such as box-drawing
//! characters, ANSI escapes, wrapping, or markup. Snapshot `PROBLEMS`
//! sections use this form so that ordinary compiler snapshots change only
//! when diagnostic *semantics* change; renderer output is pinned separately
//! by `type=reporting` snapshots.
//!
//! The serialization is deterministic: fields appear in a fixed order, and
//! strings are escaped with a fixed scheme (`\\`, `\"`, `\n`, `\t`, `\r`,
//! and `\u{XX}` lowercase hex for other control bytes; bytes >= 0x80 pass
//! through untouched so UTF-8 content stays readable).
//!
//! Schema sketch:
//!
//! ```clojure
//! (reports
//!     (report
//!         (severity runtime_error)
//!         (title "...")
//!         (region (start 3 5) (end 3 8))  ; omitted when the report has no source region
//!         (headline (reflow "..."))
//!         (document ...elements...)))
//! ```
//!
//! Each `DocumentElement` variant maps to one node (kebab-case name); the
//! switches below are exhaustive, so adding a variant breaks compilation
//! here until the canonical form covers it.

const std = @import("std");
const base = @import("base");

const SExprTree = base.SExprTree;
const Report = @import("report.zig").Report;
const document_mod = @import("document.zig");
const DocumentElement = document_mod.DocumentElement;
const Annotation = document_mod.Annotation;
const SourceCodeDisplayRegion = document_mod.SourceCodeDisplayRegion;
const sanitisePathForSnapshots = @import("renderer.zig").sanitisePathForSnapshots;

const Allocator = std.mem.Allocator;

/// Serialize a list of reports as a single `(reports ...)` node.
pub fn pushReportsToSExprTree(reports: []const Report, tree: *SExprTree) Allocator.Error!void {
    const begin = tree.beginNode();
    try tree.pushStaticAtom("reports");
    const attrs = tree.beginNode();
    for (reports) |*report| {
        try pushReportToSExprTree(report, tree);
    }
    try tree.endNode(begin, attrs);
}

/// Serialize one report as a `(report ...)` node.
pub fn pushReportToSExprTree(report: *const Report, tree: *SExprTree) Allocator.Error!void {
    const begin = tree.beginNode();
    try tree.pushStaticAtom("report");
    const attrs = tree.beginNode();

    {
        const sev_begin = tree.beginNode();
        try tree.pushStaticAtom("severity");
        try tree.pushStaticAtom(@tagName(report.severity));
        const sev_attrs = tree.beginNode();
        try tree.endNode(sev_begin, sev_attrs);
    }

    try pushEscapedStringPair(tree, "title", report.title);

    // The report's overall region: the first source region in its document,
    // already 1-based (getRegionInfo passes display-region coordinates through).
    if (report.getRegionInfo()) |region_info| {
        const region_begin = tree.beginNode();
        try tree.pushStaticAtom("region");
        try pushLineColNode(tree, "start", region_info.start_line_idx, region_info.start_col_idx);
        try pushLineColNode(tree, "end", region_info.end_line_idx, region_info.end_col_idx);
        const region_attrs = tree.beginNode();
        try tree.endNode(region_begin, region_attrs);
    }

    try pushElementsNode(tree, "headline", report.headline.elements.items);
    try pushElementsNode(tree, "document", report.document.elements.items);

    try tree.endNode(begin, attrs);
}

/// A container node whose element children each render on their own line.
fn pushElementsNode(tree: *SExprTree, name: []const u8, elements: []const DocumentElement) Allocator.Error!void {
    const begin = tree.beginNode();
    try tree.pushStaticAtom(name);
    const attrs = tree.beginNode();
    for (elements) |*element| {
        try pushElement(tree, element);
    }
    try tree.endNode(begin, attrs);
}

fn pushElement(tree: *SExprTree, element: *const DocumentElement) Allocator.Error!void {
    switch (element.*) {
        .text => |text| try pushEscapedStringPair(tree, "text", text),
        .annotated => |annotated| {
            const begin = tree.beginNode();
            try tree.pushStaticAtom("annotated");
            try tree.pushStaticAtom(annotated.annotation.semanticName());
            try pushEscapedString(tree, annotated.content);
            const attrs = tree.beginNode();
            try tree.endNode(begin, attrs);
        },
        .line_break => try pushEmptyNode(tree, "line-break"),
        .indent => |levels| try tree.pushU64Pair("indent", levels),
        .space => |count| try tree.pushU64Pair("space", count),
        .horizontal_rule => |width| {
            const begin = tree.beginNode();
            try tree.pushStaticAtom("horizontal-rule");
            if (width) |w| try tree.pushU64(w);
            const attrs = tree.beginNode();
            try tree.endNode(begin, attrs);
        },
        .annotation_start => |annotation| {
            const begin = tree.beginNode();
            try tree.pushStaticAtom("annotation-start");
            try tree.pushStaticAtom(annotation.semanticName());
            const attrs = tree.beginNode();
            try tree.endNode(begin, attrs);
        },
        .annotation_end => try pushEmptyNode(tree, "annotation-end"),
        .raw => |content| try pushEscapedStringPair(tree, "raw", content),
        .reflowing_text => |text| try pushEscapedStringPair(tree, "reflow", text),
        .link => |url| try pushEscapedStringPair(tree, "link", url),
        .vertical_stack => |children| try pushElementsNode(tree, "vertical-stack", children),
        .horizontal_concat => |children| try pushElementsNode(tree, "horizontal-concat", children),
        .source_code_region => |region| try pushDisplayRegionNode(tree, "source-region", region),
        .source_code_multi_region => |multi| {
            const begin = tree.beginNode();
            try tree.pushStaticAtom("multi-region");
            const attrs = tree.beginNode();
            if (multi.filename) |filename| {
                try pushEscapedStringPair(tree, "file", sanitisePathForSnapshots(filename));
            }
            try pushEscapedStringPair(tree, "source", multi.source);
            for (multi.regions) |region| {
                const region_begin = tree.beginNode();
                try tree.pushStaticAtom("region");
                try pushLineColNode(tree, "start", region.start_line, region.start_column);
                try pushLineColNode(tree, "end", region.end_line, region.end_column);
                try pushAnnotationNode(tree, region.annotation);
                const region_attrs = tree.beginNode();
                try tree.endNode(region_begin, region_attrs);
            }
            try tree.endNode(begin, attrs);
        },
        .source_code_with_underlines => |underlines| {
            const begin = tree.beginNode();
            try tree.pushStaticAtom("source-underlines");
            const attrs = tree.beginNode();
            try pushDisplayRegionNode(tree, "display", underlines.display_region);
            for (underlines.underline_regions) |underline| {
                const underline_begin = tree.beginNode();
                try tree.pushStaticAtom("underline");
                try pushLineColNode(tree, "start", underline.start_line, underline.start_column);
                try pushLineColNode(tree, "end", underline.end_line, underline.end_column);
                try pushAnnotationNode(tree, underline.annotation);
                const underline_attrs = tree.beginNode();
                try tree.endNode(underline_begin, underline_attrs);
            }
            try tree.endNode(begin, attrs);
        },
        .source_location => |location| {
            const begin = tree.beginNode();
            try tree.pushStaticAtom("source-location");
            const attrs = tree.beginNode();
            if (location.filename) |filename| {
                try pushEscapedStringPair(tree, "file", sanitisePathForSnapshots(filename));
            }
            try tree.pushU64Pair("line", location.line);
            try tree.pushU64Pair("column", location.column);
            try tree.endNode(begin, attrs);
        },
    }
}

fn pushDisplayRegionNode(tree: *SExprTree, name: []const u8, region: SourceCodeDisplayRegion) Allocator.Error!void {
    const begin = tree.beginNode();
    try tree.pushStaticAtom(name);
    if (region.filename) |filename| {
        try pushEscapedStringPair(tree, "file", sanitisePathForSnapshots(filename));
    }
    try pushLineColNode(tree, "start", region.start_line, region.start_column);
    try pushLineColNode(tree, "end", region.end_line, region.end_column);
    try pushAnnotationNode(tree, region.region_annotation);
    try pushEscapedStringPair(tree, "line-text", region.line_text);
    const attrs = tree.beginNode();
    try tree.endNode(begin, attrs);
}

fn pushAnnotationNode(tree: *SExprTree, annotation: Annotation) Allocator.Error!void {
    const begin = tree.beginNode();
    try tree.pushStaticAtom("annotation");
    try tree.pushStaticAtom(annotation.semanticName());
    const attrs = tree.beginNode();
    try tree.endNode(begin, attrs);
}

fn pushLineColNode(tree: *SExprTree, name: []const u8, line: u32, column: u32) Allocator.Error!void {
    const begin = tree.beginNode();
    try tree.pushStaticAtom(name);
    try tree.pushU64(line);
    try tree.pushU64(column);
    const attrs = tree.beginNode();
    try tree.endNode(begin, attrs);
}

fn pushEmptyNode(tree: *SExprTree, name: []const u8) Allocator.Error!void {
    const begin = tree.beginNode();
    try tree.pushStaticAtom(name);
    const attrs = tree.beginNode();
    try tree.endNode(begin, attrs);
}

fn pushEscapedStringPair(tree: *SExprTree, key: []const u8, value: []const u8) Allocator.Error!void {
    const begin = tree.beginNode();
    try tree.pushStaticAtom(key);
    try pushEscapedString(tree, value);
    const attrs = tree.beginNode();
    try tree.endNode(begin, attrs);
}

/// `SExprTree` writes string bytes verbatim between quotes, so escaping
/// happens here, before the string enters the tree.
fn pushEscapedString(tree: *SExprTree, value: []const u8) Allocator.Error!void {
    var escaped = std.array_list.Managed(u8).init(tree.allocator);
    defer escaped.deinit();
    for (value) |byte| {
        switch (byte) {
            '\\' => try escaped.appendSlice("\\\\"),
            '"' => try escaped.appendSlice("\\\""),
            '\n' => try escaped.appendSlice("\\n"),
            '\t' => try escaped.appendSlice("\\t"),
            '\r' => try escaped.appendSlice("\\r"),
            0x00...0x08, 0x0b, 0x0c, 0x0e...0x1f, 0x7f => {
                var buf: [8]u8 = undefined;
                const formatted = std.fmt.bufPrint(&buf, "\\u{{{x:0>2}}}", .{byte}) catch unreachable;
                try escaped.appendSlice(formatted);
            },
            else => try escaped.append(byte),
        }
    }
    try tree.pushString(escaped.items);
}

fn treeToString(allocator: Allocator, tree: *const SExprTree) error{ OutOfMemory, WriteFailed }![]u8 {
    var buffer = std.Io.Writer.Allocating.init(allocator);
    errdefer buffer.deinit();
    try tree.toStringPretty(&buffer.writer, .skip_linecol);
    return buffer.toOwnedSlice();
}

test "canonical S-expression covers every document element variant" {
    const gpa = std.testing.allocator;

    var report = try Report.init(gpa, "Test Report", "Something went wrong here.", .runtime_error);
    defer report.deinit();

    const doc = &report.document;
    try doc.addText("plain text");
    try doc.addAnnotated("annotated text", .emphasized);
    try doc.addLineBreak();
    try doc.addIndent(2);
    try doc.addSpace(3);
    try doc.addHorizontalRule(null);
    try doc.addHorizontalRule(40);
    try doc.startAnnotation(.suggestion);
    try doc.endAnnotation();
    try doc.addRaw("raw content");
    try doc.addReflowingText("reflowing text");
    try doc.addLink("https://roc-lang.org");
    try doc.addVerticalStack(&.{ .line_break, .{ .indent = 1 } });
    try doc.addHorizontalConcat(&.{ .{ .space = 1 }, .annotation_end });

    // Builder-less variants: the document takes ownership of line_text, so dupe it.
    try doc.elements.append(.{
        .source_code_region = .{
            .line_text = try gpa.dupe(u8, "x = foo"),
            .start_line = 3,
            .start_column = 5,
            .end_line = 3,
            .end_column = 8,
            .region_annotation = .error_highlight,
            .filename = try gpa.dupe(u8, "/some/snapshots/example.md"),
        },
    });
    try doc.addSourceMultiRegion(
        "x = foo\ny = bar",
        &.{
            .{ .start_line = 1, .start_column = 5, .end_line = 1, .end_column = 8, .annotation = .error_highlight },
            .{ .start_line = 2, .start_column = 5, .end_line = 2, .end_column = 8, .annotation = .warning_highlight },
        },
        "example.md",
    );
    try doc.addSourceCodeWithUnderlines(
        .{
            .line_text = try gpa.dupe(u8, "y = bar"),
            .start_line = 4,
            .start_column = 1,
            .end_line = 4,
            .end_column = 8,
            .region_annotation = .source_region,
            .filename = null,
        },
        &.{
            .{ .start_line = 4, .start_column = 5, .end_line = 4, .end_column = 8, .annotation = .underline },
        },
    );

    var tree = SExprTree.init(gpa);
    defer tree.deinit();
    try pushReportToSExprTree(&report, &tree);

    const actual = try treeToString(gpa, &tree);
    defer gpa.free(actual);

    const expected =
        "(report\n" ++
        "\t(severity runtime_error)\n" ++
        "\t(title \"Test Report\")\n" ++
        "\t(region (start 3 5) (end 3 8))\n" ++
        "\t(headline\n" ++
        "\t\t(reflow \"Something went wrong here.\"))\n" ++
        "\t(document\n" ++
        "\t\t(text \"plain text\")\n" ++
        "\t\t(annotated emphasis \"annotated text\")\n" ++
        "\t\t(line-break)\n" ++
        "\t\t(indent 2)\n" ++
        "\t\t(space 3)\n" ++
        "\t\t(horizontal-rule)\n" ++
        "\t\t(horizontal-rule 40)\n" ++
        "\t\t(annotation-start suggestion)\n" ++
        "\t\t(annotation-end)\n" ++
        "\t\t(raw \"raw content\")\n" ++
        "\t\t(reflow \"reflowing text\")\n" ++
        "\t\t(link \"https://roc-lang.org\")\n" ++
        "\t\t(vertical-stack\n" ++
        "\t\t\t(line-break)\n" ++
        "\t\t\t(indent 1))\n" ++
        "\t\t(horizontal-concat\n" ++
        "\t\t\t(space 1)\n" ++
        "\t\t\t(annotation-end))\n" ++
        "\t\t(source-region (file \"example.md\") (start 3 5) (end 3 8) (annotation error) (line-text \"x = foo\"))\n" ++
        "\t\t(multi-region\n" ++
        "\t\t\t(file \"example.md\")\n" ++
        "\t\t\t(source \"x = foo\\ny = bar\")\n" ++
        "\t\t\t(region (start 1 5) (end 1 8) (annotation error))\n" ++
        "\t\t\t(region (start 2 5) (end 2 8) (annotation warning)))\n" ++
        "\t\t(source-underlines\n" ++
        "\t\t\t(display (start 4 1) (end 4 8) (annotation source-region) (line-text \"y = bar\"))\n" ++
        "\t\t\t(underline (start 4 5) (end 4 8) (annotation underline)))))";
    try std.testing.expectEqualStrings(expected, actual);
}

test "string escaping is deterministic and keeps UTF-8 readable" {
    const gpa = std.testing.allocator;

    var report = try Report.init(gpa, "Escape Test", "", .warning);
    defer report.deinit();
    try report.document.addText("quote:\" backslash:\\ newline:\n tab:\t cr:\r ctrl:\x01 del:\x7f unicode:héllo 🐢");

    var tree = SExprTree.init(gpa);
    defer tree.deinit();
    try pushReportToSExprTree(&report, &tree);

    const actual = try treeToString(gpa, &tree);
    defer gpa.free(actual);

    const expected =
        "(report\n" ++
        "\t(severity warning)\n" ++
        "\t(title \"Escape Test\")\n" ++
        "\t(headline)\n" ++
        "\t(document\n" ++
        "\t\t(text \"quote:\\\" backslash:\\\\ newline:\\n tab:\\t cr:\\r ctrl:\\u{01} del:\\u{7f} unicode:héllo 🐢\")))";
    try std.testing.expectEqualStrings(expected, actual);
}

test "empty report list serializes as (reports)" {
    const gpa = std.testing.allocator;

    var tree = SExprTree.init(gpa);
    defer tree.deinit();
    try pushReportsToSExprTree(&.{}, &tree);

    const actual = try treeToString(gpa, &tree);
    defer gpa.free(actual);
    try std.testing.expectEqualStrings("(reports)", actual);
}

test "serialization is deterministic across runs" {
    const gpa = std.testing.allocator;

    var report = try Report.init(gpa, "Determinism Test", "Same input gives same output.", .info);
    defer report.deinit();
    try report.document.addReflowingText("body text");

    var first: ?[]u8 = null;
    defer if (first) |f| gpa.free(f);
    for (0..2) |_| {
        var tree = SExprTree.init(gpa);
        defer tree.deinit();
        try pushReportsToSExprTree(&.{report}, &tree);
        const rendered = try treeToString(gpa, &tree);
        if (first) |f| {
            defer gpa.free(rendered);
            try std.testing.expectEqualStrings(f, rendered);
        } else {
            first = rendered;
        }
    }
}
