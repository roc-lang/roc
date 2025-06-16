//! Test entry point for the reporting module.
//!
//! This file serves as the main test entry point for all reporting-related
//! functionality including rendering, document generation, styling, and reports.

const std = @import("std");
const testing = std.testing;
const document = @import("document.zig");
const report = @import("report.zig");
const renderer = @import("renderer.zig");
const severity = @import("severity.zig");
const config = @import("config.zig");
const style = @import("style.zig");

const Allocator = std.mem.Allocator;
const Document = document.Document;
const DocumentBuilder = document.DocumentBuilder;
const Annotation = document.Annotation;
const DocumentElement = document.DocumentElement;
const SourceRegion = document.SourceRegion;
const Report = report.Report;
const Severity = severity.Severity;
const ReportingConfig = config.ReportingConfig;
const ColorPalette = style.ColorPalette;

test {
    // Reference all declarations in reporting modules
    testing.refAllDeclsRecursive(@import("renderer.zig"));
    testing.refAllDeclsRecursive(@import("report.zig"));
    testing.refAllDeclsRecursive(@import("document.zig"));
    testing.refAllDeclsRecursive(@import("style.zig"));
    testing.refAllDeclsRecursive(@import("severity.zig"));
    testing.refAllDeclsRecursive(@import("config.zig"));
    testing.refAllDeclsRecursive(@import("utf8_tests.zig"));
}

// Test cases for canonicalize error reports

test "SYNTAX_PROBLEM report along with all four render types" {
    const gpa = testing.allocator;
    var buffer = std.ArrayList(u8).init(gpa);
    defer buffer.deinit();

    // Setup configuration
    const reporting_config = ReportingConfig.initForTesting();

    // Create a Report
    var r = Report.init(gpa, "SYNTAX PROBLEM", .runtime_error, reporting_config);
    defer r.deinit();

    // Add the document which describes the problem
    r.document = try buildSyntaxProblemReport(gpa);
    try testing.expect(r.document.elementCount() > 0);
    try testing.expect(!r.document.isEmpty());

    // Plain Text
    try renderer.renderReportToPlainText(&r, buffer.writer());

    const expected =
        \\SYNTAX PROBLEM
        \\Using more than one + like this requires parentheses, to clarify how things should be grouped.
        \\example.roc:1-10:1: example.roc
        \\
    ;

    try expectMultilineEqual(expected, buffer.items);

    // HTML
    buffer.clearRetainingCapacity();

    try renderer.renderReportToHtml(&r, buffer.writer());

    const expected_html =
        \\<div class="report error">
        \\<h1 class="report-title">SYNTAX PROBLEM</h1>
        \\<div class="report-content">
        \\Using more than one <span class="operator">+</span> like this requires parentheses, to clarify how things should be grouped.<br>
        \\<div class="source-region"><span class="filename">example.roc:1-10:1:</span> <pre class="error">example.roc</pre></div></div>
        \\</div>
        \\
    ;

    try expectMultilineEqual(expected_html, buffer.items);

    // Language Server Protocol
    buffer.clearRetainingCapacity();

    try renderer.renderReportToLsp(&r, buffer.writer());

    const expected_lsp =
        \\SYNTAX PROBLEM
        \\
        \\Using more than one + like this requires parentheses, to clarify how things should be grouped.
        \\example.roc:1-10:1: example.roc
    ;

    try expectMultilineEqual(expected_lsp, buffer.items);

    // Terminal (TTY)
    buffer.clearRetainingCapacity();

    try renderer.renderReportToTerminal(&r, buffer.writer(), ColorPalette.ANSI);

    // let's forget about comparing with ansi escape codes present... doesn't seem worth the effort.
    // we'll have to QA the old fashioned way.

}

test "NAMING_PROBLEM report" {
    var doc = try buildNamingProblemReport(testing.allocator);
    defer doc.deinit();

    try testing.expect(doc.elementCount() > 0);
    try testing.expect(!doc.isEmpty());
}

test "UNRECOGNIZED_NAME report" {
    var doc = try buildUnrecognizedNameReport(testing.allocator, "foo");
    defer doc.deinit();

    try testing.expect(doc.elementCount() > 0);
    try testing.expect(!doc.isEmpty());
}

test "UNUSED_DEF report" {
    var doc = try buildUnusedDefReport(testing.allocator, "unusedFunction");
    defer doc.deinit();

    try testing.expect(doc.elementCount() > 0);
    try testing.expect(!doc.isEmpty());
}

test "UNUSED_IMPORT report" {
    var doc = try buildUnusedImportReport(testing.allocator, "List.map");
    defer doc.deinit();

    try testing.expect(doc.elementCount() > 0);
    try testing.expect(!doc.isEmpty());
}

test "IMPORT_NAME_CONFLICT report" {
    var doc = try buildImportNameConflictReport(testing.allocator, "Json");
    defer doc.deinit();

    try testing.expect(doc.elementCount() > 0);
    try testing.expect(!doc.isEmpty());
}

test "UNUSED_ARG report" {
    var doc = try buildUnusedArgReport(testing.allocator, "myFunction", "unusedArg");
    defer doc.deinit();

    try testing.expect(doc.elementCount() > 0);
    try testing.expect(!doc.isEmpty());
}

test "MISSING_DEFINITION report" {
    var doc = try buildMissingDefinitionReport(testing.allocator, "missingFunction");
    defer doc.deinit();

    try testing.expect(doc.elementCount() > 0);
    try testing.expect(!doc.isEmpty());
}

test "DUPLICATE_FIELD_NAME report" {
    var doc = try buildDuplicateFieldNameReport(testing.allocator, "name");
    defer doc.deinit();

    try testing.expect(doc.elementCount() > 0);
    try testing.expect(!doc.isEmpty());
}

test "DUPLICATE_TAG_NAME report" {
    var doc = try buildDuplicateTagNameReport(testing.allocator, "Ok");
    defer doc.deinit();

    try testing.expect(doc.elementCount() > 0);
    try testing.expect(!doc.isEmpty());
}

test "MISSING_EXCLAMATION report" {
    var doc = try buildMissingExclamationReport(testing.allocator);
    defer doc.deinit();

    try testing.expect(doc.elementCount() > 0);
    try testing.expect(!doc.isEmpty());
}

test "UNNECESSARY_EXCLAMATION report" {
    var doc = try buildUnnecessaryExclamationReport(testing.allocator);
    defer doc.deinit();

    try testing.expect(doc.elementCount() > 0);
    try testing.expect(!doc.isEmpty());
}

test "EMPTY_TUPLE_TYPE report" {
    var doc = try buildEmptyTupleTypeReport(testing.allocator);
    defer doc.deinit();

    try testing.expect(doc.elementCount() > 0);
    try testing.expect(!doc.isEmpty());
}

test "UNBOUND_TYPE_VARS_IN_AS report" {
    var doc = try buildUnboundTypeVarsInAsReport(testing.allocator);
    defer doc.deinit();

    try testing.expect(doc.elementCount() > 0);
    try testing.expect(!doc.isEmpty());
}

// Helper functions for building canonicalize error reports -- these are temporary I think
// at some point we will implement these in the actual Canonicalization and have snapshot tests that cover these.

fn buildSyntaxProblemReport(allocator: Allocator) !Document {
    var doc = Document.init(allocator);
    try doc.addText("Using more than one ");
    try doc.addBinaryOperator("+");
    try doc.addReflowingText(" like this requires parentheses, to clarify how things should be grouped.");
    try doc.addLineBreak();
    try doc.addSourceRegion("example.roc", 1, 10, 1, 20, .error_highlight, "example.roc");
    return doc;
}

fn buildNamingProblemReport(allocator: Allocator) !Document {
    var doc = Document.init(allocator);
    try doc.addReflowingText("This annotation does not match the definition immediately following it:");
    try doc.addLineBreak();
    try doc.addSourceRegion("example.roc", 1, 1, 2, 10, .error_highlight, "example.roc");
    try doc.addReflowingText("Is it a typo? If not, put either a newline or comment between them.");
    return doc;
}

fn buildUnrecognizedNameReport(allocator: Allocator, name: []const u8) !Document {
    var doc = Document.init(allocator);
    try doc.addText("Nothing is named `");
    try doc.addText(name);
    try doc.addText("` in this scope.");
    try doc.addLineBreak();
    try doc.addSourceRegion("example.roc", 1, 5, 1, 8, .error_highlight, "example.roc");
    try doc.addText("Is there an ");
    try doc.addKeyword("import");
    try doc.addText(" or ");
    try doc.addKeyword("exposing");
    try doc.addReflowingText(" missing up-top");
    return doc;
}

fn buildUnusedDefReport(allocator: Allocator, symbol: []const u8) !Document {
    var doc = Document.init(allocator);
    try doc.addUnqualifiedSymbol(symbol);
    try doc.addReflowingText(" is not used anywhere in your code.");
    try doc.addLineBreak();
    try doc.addSourceRegion("example.roc", 1, 1, 1, 10, .warning_highlight, "example.roc");
    try doc.addText("If you didn't intend on using ");
    try doc.addUnqualifiedSymbol(symbol);
    try doc.addReflowingText(" then remove it so future readers of your code don't wonder why it is there.");
    return doc;
}

fn buildUnusedImportReport(allocator: Allocator, symbol: []const u8) !Document {
    var doc = Document.init(allocator);
    try doc.addQualifiedSymbol(symbol);
    try doc.addReflowingText(" is not used in this module.");
    try doc.addLineBreak();
    try doc.addSourceRegion("example.roc", 1, 8, 1, 20, .warning_highlight, "example.roc");
    try doc.addText("Since ");
    try doc.addQualifiedSymbol(symbol);
    try doc.addReflowingText(" isn't used, you don't need to import it.");
    return doc;
}

fn buildImportNameConflictReport(allocator: Allocator, name: []const u8) !Document {
    var doc = Document.init(allocator);
    try doc.addModuleName("Json");
    try doc.addText(" was imported as ");
    try doc.addModuleName(name);
    try doc.addText(":");
    try doc.addLineBreak();
    try doc.addSourceRegion("example.roc", 1, 1, 1, 20, .error_highlight, "example.roc");
    try doc.addText("but ");
    try doc.addModuleName(name);
    try doc.addReflowingText(" is already used by a previous import:");
    try doc.addLineBreak();
    try doc.addSourceRegion("example.roc", 2, 1, 2, 15, .error_highlight, "example.roc");
    try doc.addReflowingText("Using the same name for both can make it hard to tell which module you are referring to.");
    try doc.addLineBreak();
    try doc.addReflowingText("Make sure each import has a unique alias or none at all.");
    return doc;
}

fn buildUnusedArgReport(allocator: Allocator, closure_symbol: []const u8, argument_symbol: []const u8) !Document {
    var doc = Document.init(allocator);
    try doc.addUnqualifiedSymbol(closure_symbol);
    try doc.addText(" doesn't use ");
    try doc.addUnqualifiedSymbol(argument_symbol);
    try doc.addText(".");
    try doc.addLineBreak();
    try doc.addSourceRegion("example.roc", 1, 10, 1, 15, .warning_highlight, "example.roc");
    try doc.addText("If you don't need ");
    try doc.addUnqualifiedSymbol(argument_symbol);
    try doc.addReflowingText(", then you can just remove it. However, if you really do need ");
    try doc.addUnqualifiedSymbol(argument_symbol);
    try doc.addText(" as an argument of ");
    try doc.addUnqualifiedSymbol(closure_symbol);
    try doc.addText(", prefix it with an underscore, like this: \"_");
    try doc.addUnqualifiedSymbol(argument_symbol);
    try doc.addReflowingText("\". Adding an underscore at the start of a variable name is a way of saying that the variable is not used.");
    return doc;
}

fn buildMissingDefinitionReport(allocator: Allocator, symbol: []const u8) !Document {
    var doc = Document.init(allocator);
    try doc.addUnqualifiedSymbol(symbol);
    try doc.addReflowingText(" is listed as exposed, but it isn't defined in this module.");
    try doc.addLineBreak();
    try doc.addText("You can fix this by adding a definition for ");
    try doc.addUnqualifiedSymbol(symbol);
    try doc.addText(", or by removing it from ");
    try doc.addKeyword("exposes");
    try doc.addText(".");
    return doc;
}

fn buildDuplicateFieldNameReport(allocator: Allocator, field_name: []const u8) !Document {
    var doc = Document.init(allocator);
    try doc.addText("This record defines the ");
    try doc.addRecordField(field_name);
    try doc.addReflowingText(" field twice!");
    try doc.addLineBreak();
    try doc.addSourceRegion("example.roc", 1, 1, 3, 1, .error_highlight, "example.roc");
    try doc.addReflowingText("In the rest of the program, I will only use the latter definition:");
    try doc.addLineBreak();
    try doc.addSourceRegion("example.roc", 2, 5, 2, 15, .suggestion, "example.roc");
    try doc.addText("For clarity, remove the previous ");
    try doc.addRecordField(field_name);
    try doc.addReflowingText(" definitions from this record.");
    return doc;
}

fn buildDuplicateTagNameReport(allocator: Allocator, tag_name: []const u8) !Document {
    var doc = Document.init(allocator);
    try doc.addText("This tag union type defines the ");
    try doc.addTagName(tag_name);
    try doc.addReflowingText(" tag twice!");
    try doc.addLineBreak();
    try doc.addSourceRegion("example.roc", 1, 1, 3, 1, .error_highlight, "example.roc");
    try doc.addReflowingText("In the rest of the program, I will only use the latter definition:");
    try doc.addLineBreak();
    try doc.addSourceRegion("example.roc", 2, 5, 2, 15, .suggestion, "example.roc");
    try doc.addText("For clarity, remove the previous ");
    try doc.addTagName(tag_name);
    try doc.addReflowingText(" definitions from this tag union type.");
    return doc;
}

fn buildMissingExclamationReport(allocator: Allocator) !Document {
    var doc = Document.init(allocator);
    try doc.addReflowingText("The type of this record field is an effectful function, but its name does not indicate so:");
    try doc.addLineBreak();
    try doc.addSourceRegion("example.roc", 1, 5, 1, 20, .error_highlight, "example.roc");
    try doc.addReflowingText("Add an exclamation mark at the end, like:");
    try doc.addLineBreak();
    try doc.addIndent(4);
    try doc.addInlineCode("{ read_file!: Str => Str }");
    try doc.addLineBreak();
    try doc.addReflowingText("This will help readers identify it as a source of effects.");
    return doc;
}

fn buildUnnecessaryExclamationReport(allocator: Allocator) !Document {
    var doc = Document.init(allocator);
    try doc.addReflowingText("The type of this record field is a pure function, but its name suggests otherwise:");
    try doc.addLineBreak();
    try doc.addSourceRegion("example.roc", 1, 5, 1, 20, .error_highlight, "example.roc");
    try doc.addReflowingText("The exclamation mark at the end is reserved for effectful functions.");
    try doc.addLineBreak();
    try doc.addText("Did you mean to use ");
    try doc.addKeyword("=>");
    try doc.addText(" instead of ");
    try doc.addKeyword("->");
    try doc.addText("?");
    return doc;
}

fn buildEmptyTupleTypeReport(allocator: Allocator) !Document {
    var doc = Document.init(allocator);
    try doc.addReflowingText("This tuple type is empty:");
    try doc.addLineBreak();
    try doc.addSourceRegion("example.roc", 1, 10, 1, 12, .error_highlight, "example.roc");
    try doc.addReflowingText("Empty tuples are not allowed in Roc.");
    return doc;
}

fn buildUnboundTypeVarsInAsReport(allocator: Allocator) !Document {
    var doc = Document.init(allocator);
    try doc.addReflowingText("This type annotation has unbound type variables:");
    try doc.addLineBreak();
    try doc.addSourceRegion("example.roc", 1, 10, 1, 20, .error_highlight, "example.roc");
    try doc.addReflowingText("Type variables must be bound in the same scope as the type annotation.");
    return doc;
}

// Test Helpers

/// Should only print out the debug copy-paste ready string if the string comparison fails.
fn expectMultilineEqual(expected: []const u8, actual: []const u8) !void {
    testing.expectEqualStrings(expected, actual) catch {
        std.debug.print("\n--- DEBUG STRING COMPARISON (copy-paste ready) ---\n", .{});
        std.debug.print("const expected = \n", .{});
        printAsMultilineString(actual);
        std.debug.print(";\n", .{});
    };
}

fn printAsMultilineString(s: []const u8) void {
    if (s.len == 0) {
        std.debug.print("        \\\\\n", .{});
        return;
    }

    var lines = std.mem.splitScalar(u8, s, '\n');
    var first = true;
    while (lines.next()) |line| {
        if (first) {
            first = false;
            std.debug.print("        \\\\", .{});
        } else {
            std.debug.print("        \\\\", .{});
        }

        // Print each character with proper escaping
        for (line) |c| {
            switch (c) {
                '\\' => std.debug.print("\\\\", .{}),
                '"' => std.debug.print("\\\"", .{}),
                '\'' => std.debug.print("\\'", .{}),
                '\t' => std.debug.print("\\t", .{}),
                '\r' => std.debug.print("\\r", .{}),
                else => std.debug.print("{c}", .{c}),
            }
        }
        std.debug.print("\n", .{});
    }
}
