//! Report system for formatted warning and error reports.
//!
//! This module provides the Report struct and related functionality for creating
//! structured error reports that can be rendered to different output formats.
//!
//! Reports combine a title, severity level, and formatted document content.

const std = @import("std");
const Allocator = std.mem.Allocator;
const Severity = @import("severity.zig").Severity;
const Document = @import("document.zig").Document;
const Annotation = @import("document.zig").Annotation;
const renderer = @import("renderer.zig");
const RenderTarget = renderer.RenderTarget;
const ReportingConfig = @import("config.zig").ReportingConfig;
const base = @import("../base.zig");
const RegionInfo = base.RegionInfo;
const collections = @import("../collections.zig");
const exitOnOom = collections.utils.exitOnOom;

/// Default maximum message size in bytes for truncation
const DEFAULT_MAX_MESSAGE_BYTES: usize = 4096;

/// A structured report containing error information and formatted content.
pub const Report = struct {
    title: []const u8,
    severity: Severity,
    document: Document,
    allocator: Allocator,
    owned_strings: std.ArrayList([]const u8),

    pub fn init(allocator: Allocator, title: []const u8, severity: Severity) Report {
        return Report{
            .title = title,
            .severity = severity,
            .document = Document.init(allocator),
            .allocator = allocator,
            .owned_strings = std.ArrayList([]const u8).init(allocator),
        };
    }

    pub fn deinit(self: *Report) void {
        for (self.owned_strings.items) |owned_string| {
            self.allocator.free(@constCast(owned_string));
        }
        self.owned_strings.deinit();
        self.document.deinit();
    }

    /// Add a string that the report will take ownership of.
    /// Returns the owned copy of the string that can be safely used in the document.
    pub fn addOwnedString(self: *Report, string: []const u8) ![]const u8 {
        const owned_copy = try self.allocator.dupe(u8, string);
        try self.owned_strings.append(owned_copy);
        return owned_copy;
    }

    /// Render the report to the specified writer and target format.
    pub fn render(self: *const Report, writer: anytype, target: RenderTarget) !void {
        try renderer.renderReport(self, writer, target);
    }

    /// Add a section header to the report.
    pub fn addHeader(self: *Report, header: []const u8) !void {
        try self.document.addLineBreak();
        try self.document.addAnnotated(header, .emphasized);
        try self.document.addLineBreak();
    }

    /// Add a code snippet with proper formatting and UTF-8 validation.
    pub fn addCodeSnippet(self: *Report, code: []const u8, line_number: ?u32) !void {
        @import("config.zig").validateUtf8(code) catch |err| switch (err) {
            error.InvalidUtf8 => {
                try self.document.addError("[Invalid UTF-8 in code snippet]");
                try self.document.addLineBreak();
                return;
            },
        };

        if (line_number) |line_num| {
            // Manually format line number to avoid buffer corruption
            var line_num_buf: [32]u8 = undefined;
            const line_num_str = std.fmt.bufPrint(&line_num_buf, "{d}", .{line_num}) catch unreachable;
            try self.document.addText(line_num_str);
            try self.document.addText(" | ");
        } else {
            try self.document.addText("   | ");
        }
        try self.document.addCodeBlock(code);
        try self.document.addLineBreak();
    }

    /// Add source context using RegionInfo for better accuracy and simplicity.
    pub fn addSourceContext(self: *Report, region: RegionInfo) !void {
        @import("config.zig").validateUtf8(region.line_text) catch |err| switch (err) {
            error.InvalidUtf8 => {
                try self.document.addError("[Invalid UTF-8 in source context]");
                try self.document.addLineBreak();
                return;
            },
        };

        // Show line number with code block formatting
        try self.document.addText("```");
        try self.document.addLineBreak();

        const line_num = region.start_line_idx + 1; // Convert to 1-based
        // Format line number and make owned copy to avoid buffer corruption
        var line_num_buf: [32]u8 = undefined;
        const line_num_str = std.fmt.bufPrint(&line_num_buf, "{d}", .{line_num}) catch unreachable;
        const owned_line_num = try self.addOwnedString(line_num_str);
        try self.document.addText(owned_line_num);
        try self.document.addText(" | ");

        // Add the line content with highlighting
        const line_without_newline = std.mem.trimRight(u8, region.line_text, "\n\r");

        if (region.start_col_idx == region.end_col_idx) {
            // Single character or empty range
            try self.document.addText(line_without_newline);
        } else {
            // Multi-character range - split into parts
            const start_col = @min(region.start_col_idx, line_without_newline.len);
            const end_col = @min(region.end_col_idx, line_without_newline.len);

            // Before highlighted section
            if (start_col > 0) {
                try self.document.addText(line_without_newline[0..start_col]);
            }

            // Highlighted section
            if (end_col > start_col) {
                try self.document.startAnnotation(.error_highlight);
                try self.document.addText(line_without_newline[start_col..end_col]);
                try self.document.endAnnotation();
            }

            // After highlighted section
            if (end_col < line_without_newline.len) {
                try self.document.addText(line_without_newline[end_col..]);
            }
        }

        try self.document.addLineBreak();

        // Add underline for highlighted section
        if (region.start_col_idx < region.end_col_idx) {
            // Add padding for line number prefix
            const line_num_width: u32 = if (line_num < 10) 1 else if (line_num < 100) 2 else if (line_num < 1000) 3 else 4;
            try self.document.addSpace(line_num_width + 3); // number + " | "

            // Add spaces up to the start of the error
            try self.document.addSpace(region.start_col_idx);

            // Add underline
            try self.document.startAnnotation(.error_highlight);
            const underline_length = @min(region.end_col_idx - region.start_col_idx, @as(u32, @intCast(line_without_newline.len)) - region.start_col_idx);
            var i: u32 = 0;
            while (i < underline_length) : (i += 1) {
                try self.document.addText("^");
            }
            try self.document.endAnnotation();
            try self.document.addLineBreak();
        }

        // Close code block
        try self.document.addText("```");
        try self.document.addLineBreak();
    }

    /// Add a suggestion with proper formatting and UTF-8 validation.
    pub fn addSuggestion(self: *Report, suggestion: []const u8) !void {
        @import("config.zig").validateUtf8(suggestion) catch |err| switch (err) {
            error.InvalidUtf8 => {
                try self.document.addError("[Invalid UTF-8 in suggestion]");
                try self.document.addLineBreak();
                return;
            },
        };

        const truncated_suggestion = if (suggestion.len > DEFAULT_MAX_MESSAGE_BYTES) blk: {
            const truncated = @import("config.zig").truncateUtf8(self.allocator, suggestion, DEFAULT_MAX_MESSAGE_BYTES) catch suggestion;
            if (truncated.ptr != suggestion.ptr) {
                self.owned_strings.append(truncated) catch |err| exitOnOom(err);
            }
            break :blk truncated;
        } else suggestion;

        try self.document.addLineBreak();
        try self.document.addAnnotated("Hint: ", .suggestion);
        try self.document.addText(truncated_suggestion);
        try self.document.addLineBreak();
    }

    /// Add multiple suggestions as a list.
    pub fn addSuggestions(self: *Report, suggestions: []const []const u8) !void {
        if (suggestions.len == 0) return;

        try self.document.addLineBreak();
        if (suggestions.len == 1) {
            try self.document.addAnnotated("Hint: ", .suggestion);
            try self.document.addText(suggestions[0]);
        } else {
            try self.document.addAnnotated("Hints:", .suggestion);
            try self.document.addLineBreak();
            for (suggestions) |suggestion| {
                try self.document.addIndent(1);
                try self.document.addText("• ");
                try self.document.addText(suggestion);
                try self.document.addLineBreak();
            }
        }
        try self.document.addLineBreak();
    }

    /// Add a type comparison showing expected vs actual.
    pub fn addTypeComparison(self: *Report, expected: []const u8, actual: []const u8) !void {
        try self.document.addLineBreak();
        try self.document.addText("Expected type:");
        try self.document.addLineBreak();
        try self.document.addIndent(1);
        try self.document.addType(expected);
        try self.document.addLineBreak();
        try self.document.addLineBreak();
        try self.document.addText("But found type:");
        try self.document.addLineBreak();
        try self.document.addIndent(1);
        try self.document.addError(actual);
        try self.document.addLineBreak();
    }

    /// Add a note with dimmed styling.
    pub fn addNote(self: *Report, note: []const u8) !void {
        try self.document.addLineBreak();
        try self.document.addAnnotated("Note: ", .dimmed);
        try self.document.addText(note);
        try self.document.addLineBreak();
    }

    /// Add an error message with proper styling.
    pub fn addErrorMessage(self: *Report, message: []const u8) !void {
        try self.document.addError(message);
        try self.document.addLineBreak();
    }

    /// Add a warning message with proper styling.
    pub fn addWarningMessage(self: *Report, message: []const u8) !void {
        try self.document.addWarning(message);
        try self.document.addLineBreak();
    }

    /// Add a separator line.
    pub fn addSeparator(self: *Report) !void {
        try self.document.addLineBreak();
        try self.document.addHorizontalRule(40);
        try self.document.addLineBreak();
    }

    /// Check if the report is empty (has no content).
    pub fn isEmpty(self: *const Report) bool {
        return self.document.isEmpty();
    }

    /// Get the number of lines in the report (approximate).
    pub fn getLineCount(self: *const Report) usize {
        var count: usize = 2; // Title + blank line
        for (self.document.elements.items) |element| {
            switch (element) {
                .line_break => count += 1,
                else => {},
            }
        }
        return count;
    }
};

/// Builder for creating reports with a fluent interface.
pub const ReportBuilder = struct {
    report: Report,

    pub fn init(allocator: Allocator, title: []const u8, severity: Severity) ReportBuilder {
        return ReportBuilder{
            .report = Report.init(allocator, title, severity),
        };
    }

    pub fn deinit(self: *ReportBuilder) void {
        self.report.deinit();
    }

    pub fn header(self: *ReportBuilder, text: []const u8) *ReportBuilder {
        self.report.addHeader(text) catch |err| exitOnOom(err);
        return self;
    }

    pub fn message(self: *ReportBuilder, text: []const u8) *ReportBuilder {
        self.report.document.addText(text) catch |err| exitOnOom(err);
        self.report.document.addLineBreak() catch |err| exitOnOom(err);
        return self;
    }

    pub fn code(self: *ReportBuilder, code_text: []const u8) *ReportBuilder {
        self.report.addCodeSnippet(code_text, null) catch |err| switch (err) {
            error.OutOfMemory => exitOnOom(error.OutOfMemory),
            else => {
                @panic("unexpected error building code snippet");
            },
        };
        return self;
    }

    pub fn suggestion(self: *ReportBuilder, text: []const u8) *ReportBuilder {
        self.report.addSuggestion(text) catch |err| exitOnOom(err);
        return self;
    }

    pub fn note(self: *ReportBuilder, text: []const u8) *ReportBuilder {
        self.report.addNote(text) catch |err| exitOnOom(err);
        return self;
    }

    pub fn typeComparison(self: *ReportBuilder, expected: []const u8, actual: []const u8) *ReportBuilder {
        self.report.addTypeComparison(expected, actual) catch |err| exitOnOom(err);
        return self;
    }

    pub fn build(self: *ReportBuilder) Report {
        return self.report;
    }
};

// Tests
const testing = std.testing;

test "Report basic functionality" {
    var report = Report.init(testing.allocator, "TEST ERROR", .runtime_error);
    defer report.deinit();

    try report.document.addText("This is a test error message.");
    try report.addSuggestion("Try fixing the issue.");

    try testing.expect(!report.isEmpty());
    try testing.expect(report.getLineCount() > 2);
}

test "ReportBuilder fluent interface" {
    var builder = ReportBuilder.init(testing.allocator, "BUILD ERROR", .runtime_error);
    defer builder.deinit();

    var report = builder
        .message("Something went wrong")
        .suggestion("Try this fix")
        .note("This is just a note")
        .build();

    try testing.expect(!report.isEmpty());
}
