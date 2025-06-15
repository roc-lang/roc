//! Comprehensive UTF-8 validation tests for the reporting module.
//!
//! This module tests all UTF-8 related functionality to ensure the reporting
//! system handles UTF-8 correctly and defensively, preventing invalid UTF-8
//! from being written to output.

const std = @import("std");
const testing = std.testing;
const Allocator = std.mem.Allocator;

const config = @import("config.zig");
const Report = @import("report.zig").Report;
const Templates = @import("report.zig").Templates;
const Document = @import("document.zig").Document;
const Severity = @import("severity.zig").Severity;
const ReportingConfig = config.ReportingConfig;
const base = @import("../base.zig");
const RegionInfo = base.RegionInfo;

// Test helper functions

fn expectValidUtf8(bytes: []const u8) !void {
    try testing.expect(std.unicode.utf8ValidateSlice(bytes));
}

fn expectInvalidUtf8(bytes: []const u8) !void {
    try testing.expect(!std.unicode.utf8ValidateSlice(bytes));
}

// UTF-8 validation tests

test "validateUtf8 with valid UTF-8" {
    // ASCII
    try config.validateUtf8("Hello, World!");

    // UTF-8 with various characters
    try config.validateUtf8("Hello, 世界!");
    try config.validateUtf8("Café");
    try config.validateUtf8("🎉 Emoji test");
    try config.validateUtf8("Ñandú");

    // Empty string
    try config.validateUtf8("");

    // Various Unicode ranges
    try config.validateUtf8("αβγδε"); // Greek
    try config.validateUtf8("こんにちは"); // Japanese Hiragana
    try config.validateUtf8("🔥💯✨"); // Multiple emojis
}

test "validateUtf8 with invalid UTF-8" {
    // Invalid start bytes
    const invalid1 = [_]u8{ 0xFF, 0xFE };
    try testing.expectError(error.InvalidUtf8, config.validateUtf8(&invalid1));

    // Incomplete multibyte sequence
    const invalid2 = [_]u8{0xC2}; // Start of 2-byte sequence without continuation
    try testing.expectError(error.InvalidUtf8, config.validateUtf8(&invalid2));

    // Invalid continuation byte
    const invalid3 = [_]u8{ 0xC2, 0x20 }; // Valid start, invalid continuation
    try testing.expectError(error.InvalidUtf8, config.validateUtf8(&invalid3));

    // Overlong encoding
    const invalid4 = [_]u8{ 0xC0, 0x80 }; // Overlong encoding of NULL
    try testing.expectError(error.InvalidUtf8, config.validateUtf8(&invalid4));

    // Surrogate pair (invalid in UTF-8)
    const invalid5 = [_]u8{ 0xED, 0xA0, 0x80 }; // High surrogate
    try testing.expectError(error.InvalidUtf8, config.validateUtf8(&invalid5));
}

test "truncateUtf8 preserves valid UTF-8" {
    const input = "Hello, 世界! This is a test.";

    // No truncation needed
    const result1 = try config.truncateUtf8(testing.allocator, input, 100);
    defer testing.allocator.free(result1);
    try testing.expectEqualStrings(input, result1);
    try expectValidUtf8(result1);

    // Truncation needed but can fit whole string
    const result2 = try config.truncateUtf8(testing.allocator, input, input.len);
    defer testing.allocator.free(result2);
    try testing.expectEqualStrings(input, result2);
    try expectValidUtf8(result2);
}

test "truncateUtf8 handles multibyte characters safely" {
    const input = "Hello, 世界!";

    // Truncate at multibyte boundary
    const result1 = try config.truncateUtf8(testing.allocator, input, 10);
    defer testing.allocator.free(result1);
    try expectValidUtf8(result1);
    try testing.expect(result1.len <= 10);

    // Truncate in the middle of a multibyte character
    const result2 = try config.truncateUtf8(testing.allocator, input, 8);
    defer testing.allocator.free(result2);
    try expectValidUtf8(result2);
    try testing.expect(result2.len <= 8);
}

test "truncateUtf8 with emoji" {
    const input = "Test 🎉🔥💯 emoji";

    const result = try config.truncateUtf8(testing.allocator, input, 10);
    defer testing.allocator.free(result);
    try expectValidUtf8(result);
    try testing.expect(result.len <= 10);
}

test "truncateUtf8 with very small limit" {
    const input = "世界";

    try testing.expectError(error.CannotTruncate, config.truncateUtf8(testing.allocator, input, 1));
}

test "truncateUtf8 with zero limit" {
    const input = "Hello";

    try testing.expectError(error.CannotTruncate, config.truncateUtf8(testing.allocator, input, 0));
}

test "formatUtf8 with valid input" {
    const result = try config.formatUtf8(testing.allocator, "Hello, {s}!", .{"世界"});
    defer testing.allocator.free(result);

    try testing.expectEqualStrings("Hello, 世界!", result);
    try expectValidUtf8(result);
}

test "formatUtf8Bounded with truncation" {
    const result = try config.formatUtf8Bounded(testing.allocator, 10, "Hello, {s}!", .{"世界"});
    defer testing.allocator.free(result);

    try expectValidUtf8(result);
    try testing.expect(result.len <= 10);
}

test "UTF-8 character boundary detection" {
    const input = "Hello, 世界!";

    // Test continuation byte detection
    try testing.expect(!config.isUtf8Continuation('H'));
    try testing.expect(!config.isUtf8Continuation('e'));

    // Test character start finding
    const start1 = config.findUtf8CharStart(input, 0);
    try testing.expectEqual(@as(usize, 0), start1);

    const start2 = config.findUtf8CharStart(input, 7); // Somewhere in "世"
    try testing.expect(start2 <= 7);

    // Test character end finding
    const end1 = config.findUtf8CharEnd(input, 0);
    try testing.expect(end1 >= 0);

    const end2 = config.findUtf8CharEnd(input, 7);
    try testing.expect(end2 >= 7);
}

// Report-level UTF-8 tests

test "Report with UTF-8 content" {
    const reporting_config = ReportingConfig.initForTesting();
    var report = Report.init(testing.allocator, "UTF-8 Test Error", .runtime_error, reporting_config);
    defer report.deinit();

    // Add UTF-8 content
    try report.document.addText("Error in file: café.roc");
    try report.document.addLineBreak();
    try report.document.addText("Function name: こんにちは");
    try report.document.addLineBreak();
    try report.document.addText("Expected: 🎉");
    try report.document.addLineBreak();
    try report.document.addText("Actual: 🔥");

    // Render to string buffer
    var buffer = std.ArrayList(u8).init(testing.allocator);
    defer buffer.deinit();

    try report.render(buffer.writer(), .plain_text);

    // Validate the output is valid UTF-8
    try expectValidUtf8(buffer.items);
}

test "Report source context with UTF-8" {
    const reporting_config = ReportingConfig.initForTesting();
    var report = Report.init(testing.allocator, "UTF-8 Source Error", .runtime_error, reporting_config);
    defer report.deinit();

    const region = RegionInfo{
        .start_line_idx = 0,
        .start_col_idx = 10,
        .end_line_idx = 0,
        .end_col_idx = 15,
        .line_text = "let name = \"世界\"",
    };

    try report.addSourceContext(region);

    // Render to string buffer
    var buffer = std.ArrayList(u8).init(testing.allocator);
    defer buffer.deinit();

    try report.render(buffer.writer(), .plain_text);

    // Validate the output is valid UTF-8
    try expectValidUtf8(buffer.items);
}

test "Report with invalid UTF-8 input handles gracefully" {
    const reporting_config = ReportingConfig.initForTesting();
    var report = Report.init(testing.allocator, "Invalid UTF-8 Test", .runtime_error, reporting_config);
    defer report.deinit();

    // Create invalid UTF-8 data
    const invalid_utf8 = [_]u8{ 0xFF, 0xFE, 0xFD };

    // This should not crash but should handle the error gracefully
    try report.addCodeSnippet(&invalid_utf8, 1);

    // Render to string buffer
    var buffer = std.ArrayList(u8).init(testing.allocator);
    defer buffer.deinit();

    try report.render(buffer.writer(), .plain_text);

    // The output should be valid UTF-8 even though input was invalid
    try expectValidUtf8(buffer.items);

    // Should contain error message about invalid UTF-8
    try testing.expect(std.mem.indexOf(u8, buffer.items, "Invalid UTF-8") != null);
}

test "Report suggestion with UTF-8 truncation" {
    const reporting_config = ReportingConfig{
        .color_preference = .never,
        .is_tty = false,
        .render_target = .plain_text,
        .max_line_width = 80,
        .show_line_numbers = false,
        .context_lines = 1,
        .validate_utf8 = true,
        .max_message_bytes = 20, // Very small limit
    };

    var report = Report.init(testing.allocator, "Truncation Test", .runtime_error, reporting_config);
    defer report.deinit();

    const long_suggestion = "This is a very long suggestion with UTF-8 characters: 世界 🎉 that should be truncated";
    try report.addSuggestion(long_suggestion);

    // Render to string buffer
    var buffer = std.ArrayList(u8).init(testing.allocator);
    defer buffer.deinit();

    try report.render(buffer.writer(), .plain_text);

    // Validate the output is valid UTF-8
    try expectValidUtf8(buffer.items);
}

test "Document with mixed UTF-8 content" {
    var doc = Document.init(testing.allocator);
    defer doc.deinit();

    // Add various UTF-8 content
    try doc.addText("English text");
    try doc.addLineBreak();
    try doc.addText("中文文本");
    try doc.addLineBreak();
    try doc.addText("العربية");
    try doc.addLineBreak();
    try doc.addText("Русский");
    try doc.addLineBreak();
    try doc.addText("🌍🌎🌏");

    // Render to string
    var buffer = std.ArrayList(u8).init(testing.allocator);
    defer buffer.deinit();

    const reporting_config = ReportingConfig.initForTesting();
    try doc.render(buffer.writer(), .plain_text, reporting_config);

    // Validate all output is valid UTF-8
    try expectValidUtf8(buffer.items);
}

test "Code snippet with UTF-8 identifiers" {
    const reporting_config = ReportingConfig.initForTesting();
    var report = Report.init(testing.allocator, "UTF-8 Code Test", .runtime_error, reporting_config);
    defer report.deinit();

    const code =
        \\fn 世界() {
        \\    let café = "☕";
        \\    print(café);
        \\}
    ;

    try report.addCodeSnippet(code, 1);

    // Render to string
    var buffer = std.ArrayList(u8).init(testing.allocator);
    defer buffer.deinit();

    try report.render(buffer.writer(), .plain_text);

    // Validate output
    try expectValidUtf8(buffer.items);
    try testing.expect(std.mem.indexOf(u8, buffer.items, "世界") != null);
    try testing.expect(std.mem.indexOf(u8, buffer.items, "café") != null);
    try testing.expect(std.mem.indexOf(u8, buffer.items, "☕") != null);
}

test "Error message with UTF-8 type names" {
    const reporting_config = ReportingConfig.initForTesting();
    var report = try Templates.typeMismatch(
        testing.allocator,
        "Número", // Spanish
        "文字列", // Japanese
        "файл.roc", // Cyrillic
        reporting_config,
    );
    defer report.deinit();

    // Render to string
    var buffer = std.ArrayList(u8).init(testing.allocator);
    defer buffer.deinit();

    try report.render(buffer.writer(), .plain_text);

    // Validate output
    try expectValidUtf8(buffer.items);
    try testing.expect(std.mem.indexOf(u8, buffer.items, "Número") != null);
    try testing.expect(std.mem.indexOf(u8, buffer.items, "文字列") != null);
    try testing.expect(std.mem.indexOf(u8, buffer.items, "файл.roc") != null);
}

// Edge case tests

test "Empty UTF-8 strings" {
    const reporting_config = ReportingConfig.initForTesting();
    var report = Report.init(testing.allocator, "", .runtime_error, reporting_config);
    defer report.deinit();

    try report.addSuggestion("");
    try report.addCodeSnippet("", null);

    // Should not crash and should produce valid UTF-8
    var buffer = std.ArrayList(u8).init(testing.allocator);
    defer buffer.deinit();

    try report.render(buffer.writer(), .plain_text);
    try expectValidUtf8(buffer.items);
}

test "Very long UTF-8 content" {
    const reporting_config = ReportingConfig.initForTesting();
    var report = Report.init(testing.allocator, "Long Content Test", .runtime_error, reporting_config);
    defer report.deinit();

    // Create a very long string with repeated UTF-8 characters
    var long_content = std.ArrayList(u8).init(testing.allocator);
    defer long_content.deinit();

    var i: usize = 0;
    while (i < 1000) : (i += 1) {
        try long_content.appendSlice("世界🎉");
    }

    try report.document.addText(long_content.items);

    // Should handle long content gracefully
    var buffer = std.ArrayList(u8).init(testing.allocator);
    defer buffer.deinit();

    try report.render(buffer.writer(), .plain_text);
    try expectValidUtf8(buffer.items);
}

test "UTF-8 at memory boundaries" {
    // Test UTF-8 handling at various memory boundaries
    const sizes = [_]usize{ 1, 2, 3, 4, 5, 15, 16, 17, 31, 32, 33, 63, 64, 65 };

    for (sizes) |size| {
        const input = "世界🎉";
        const result = config.truncateUtf8(testing.allocator, input, size) catch continue;
        defer testing.allocator.free(result);
        try expectValidUtf8(result);
    }
}

test "RegionInfo with UTF-8 line text" {
    const reporting_config = ReportingConfig.initForTesting();
    var report = Report.init(testing.allocator, "RegionInfo UTF-8 Test", .runtime_error, reporting_config);
    defer report.deinit();

    // Test with UTF-8 content that has complex character boundaries
    const utf8_line = "    let 変数 = \"🎉 Hello, 世界! 🔥\";";

    const region = RegionInfo{
        .start_line_idx = 0,
        .start_col_idx = 8,
        .end_line_idx = 0,
        .end_col_idx = 10,
        .line_text = utf8_line,
    };

    try report.addSourceContext(region);

    var buffer = std.ArrayList(u8).init(testing.allocator);
    defer buffer.deinit();

    try report.render(buffer.writer(), .plain_text);
    try expectValidUtf8(buffer.items);
}
