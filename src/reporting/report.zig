//! Report system for formatted error messages.
//!
//! This module provides the Report struct and related functionality for creating
//! structured error reports that can be rendered to different output formats.
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

/// A structured report containing error information and formatted content.
pub const Report = struct {
    title: []const u8,
    severity: Severity,
    document: Document,
    allocator: Allocator,
    config: ReportingConfig,
    owned_strings: std.ArrayList([]const u8),

    pub fn init(allocator: Allocator, title: []const u8, severity: Severity, config: ReportingConfig) Report {
        return Report{
            .title = title,
            .severity = severity,
            .document = Document.init(allocator),
            .allocator = allocator,
            .config = config,
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
        if (self.config.shouldValidateUtf8()) {
            @import("config.zig").validateUtf8(code) catch |err| switch (err) {
                error.InvalidUtf8 => {
                    try self.document.addError("[Invalid UTF-8 in code snippet]");
                    try self.document.addLineBreak();
                    return;
                },
            };
        }

        if (self.config.shouldShowLineNumbers()) {
            if (line_number) |line_num| {
                try self.document.addFormattedText("{d} | ", .{line_num});
            } else {
                try self.document.addText("   | ");
            }
        }
        try self.document.addCodeBlock(code);
        try self.document.addLineBreak();
    }

    /// Add source context using RegionInfo for better accuracy and simplicity.
    pub fn addSourceContext(self: *Report, region: RegionInfo) !void {
        if (self.config.shouldValidateUtf8()) {
            @import("config.zig").validateUtf8(region.line_text) catch |err| switch (err) {
                error.InvalidUtf8 => {
                    try self.document.addError("[Invalid UTF-8 in source context]");
                    try self.document.addLineBreak();
                    return;
                },
            };
        }

        // Show line number if enabled
        if (self.config.shouldShowLineNumbers()) {
            const line_num = region.start_line_idx + 1; // Convert to 1-based
            try self.document.addFormattedText("{d} | ", .{line_num});
        } else {
            try self.document.addText("   | ");
        }

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
            if (self.config.shouldShowLineNumbers()) {
                const line_num = region.start_line_idx + 1;
                const line_num_width: u32 = if (line_num < 10) 1 else if (line_num < 100) 2 else if (line_num < 1000) 3 else 4;
                try self.document.addSpace(line_num_width + 3); // number + " | "
            } else {
                try self.document.addSpace(5); // "   | "
            }

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
    }

    /// Add a suggestion with proper formatting and UTF-8 validation.
    pub fn addSuggestion(self: *Report, suggestion: []const u8) !void {
        if (self.config.shouldValidateUtf8()) {
            @import("config.zig").validateUtf8(suggestion) catch |err| switch (err) {
                error.InvalidUtf8 => {
                    try self.document.addError("[Invalid UTF-8 in suggestion]");
                    try self.document.addLineBreak();
                    return;
                },
            };
        }

        const truncated_suggestion = if (suggestion.len > self.config.getMaxMessageBytes()) blk: {
            const truncated = @import("config.zig").truncateUtf8(self.allocator, suggestion, self.config.getMaxMessageBytes()) catch suggestion;
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

    pub fn init(allocator: Allocator, title: []const u8, severity: Severity, config: ReportingConfig) ReportBuilder {
        return ReportBuilder{
            .report = Report.init(allocator, title, severity, config),
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

/// Predefined report templates for common error types
pub const Templates = struct {
    /// Create a type mismatch report.
    pub fn typeMismatch(
        allocator: Allocator,
        expected: []const u8,
        actual: []const u8,
        location: []const u8,
        config: ReportingConfig,
    ) !Report {
        var report = Report.init(allocator, "TYPE MISMATCH", .runtime_error, config);

        try report.document.addText("I expected this expression to have type:");
        try report.document.addLineBreak();
        try report.document.addIndent(1);
        try report.document.addType(expected);
        try report.document.addLineBreak();
        try report.document.addLineBreak();
        try report.document.addText("But it actually has type:");
        try report.document.addLineBreak();
        try report.document.addIndent(1);
        try report.document.addError(actual);
        try report.document.addLineBreak();

        if (location.len > 0) {
            try report.document.addLineBreak();
            try report.document.addText("At: ");
            try report.document.addAnnotated(location, .path);
        }

        return report;
    }

    /// Create an unrecognized name report.
    pub fn unrecognizedName(
        allocator: Allocator,
        name: []const u8,
        suggestions: []const []const u8,
        config: ReportingConfig,
    ) !Report {
        var report = Report.init(allocator, "NAMING ERROR", .runtime_error, config);

        try report.document.addText("I cannot find a ");
        try report.document.addError(name);
        try report.document.addText(" variable.");
        try report.document.addLineBreak();

        if (suggestions.len > 0) {
            try report.addSuggestions(suggestions);
        }

        return report;
    }

    /// Create a circular definition report.
    pub fn circularDefinition(
        allocator: Allocator,
        names: []const []const u8,
        config: ReportingConfig,
    ) !Report {
        var report = Report.init(allocator, "CIRCULAR DEFINITION", .runtime_error, config);

        try report.document.addText("These definitions depend on each other in a cycle:");
        try report.document.addLineBreak();
        try report.document.addLineBreak();

        for (names, 0..) |name, i| {
            try report.document.addIndent(1);
            try report.document.addError(name);
            if (i < names.len - 1) {
                try report.document.addText(" → ");
            } else {
                try report.document.addText(" → ");
                try report.document.addError(names[0]);
            }
            try report.document.addLineBreak();
        }

        try report.addNote("Roc cannot compile definitions that depend on themselves.");

        return report;
    }

    /// Create a compiler internal error report.
    pub fn internalError(
        allocator: Allocator,
        message: []const u8,
        location: []const u8,
        config: ReportingConfig,
    ) !Report {
        var report = Report.init(allocator, "INTERNAL COMPILER ERROR", .fatal, config);

        try report.document.addText("The compiler encountered an unexpected error:");
        try report.document.addLineBreak();
        try report.document.addLineBreak();
        try report.document.addError(message);
        try report.document.addLineBreak();

        if (location.len > 0) {
            try report.document.addLineBreak();
            try report.document.addText("Location: ");
            try report.document.addAnnotated(location, .path);
            try report.document.addLineBreak();
        }

        try report.document.addLineBreak();
        try report.document.addText("This is a bug in the Roc compiler. Please report it at:");
        try report.document.addLineBreak();
        try report.document.addAnnotated("https://github.com/roc-lang/roc/issues", .path);
        try report.document.addLineBreak();

        return report;
    }
};

// Tests
const testing = std.testing;

test "Report basic functionality" {
    const config = ReportingConfig.initForTesting();
    var report = Report.init(testing.allocator, "TEST ERROR", .runtime_error, config);
    defer report.deinit();

    try report.document.addText("This is a test error message.");
    try report.addSuggestion("Try fixing the issue.");

    try testing.expect(!report.isEmpty());
    try testing.expect(report.getLineCount() > 2);
}

test "ReportBuilder fluent interface" {
    const config = ReportingConfig.initForTesting();
    var builder = ReportBuilder.init(testing.allocator, "BUILD ERROR", .runtime_error, config);
    defer builder.deinit();

    var report = builder
        .message("Something went wrong")
        .suggestion("Try this fix")
        .note("This is just a note")
        .build();

    try testing.expect(!report.isEmpty());
}

test "Type mismatch template" {
    const config = ReportingConfig.initForTesting();
    var report = try Templates.typeMismatch(
        testing.allocator,
        "String",
        "Number",
        "main.roc:10:5",
        config,
    );
    defer report.deinit();

    try testing.expectEqualStrings("TYPE MISMATCH", report.title);
    try testing.expectEqual(Severity.runtime_error, report.severity);
}

test "Unrecognized name template" {
    const config = ReportingConfig.initForTesting();
    const suggestions = [_][]const u8{ "length", "len", "size" };
    var report = try Templates.unrecognizedName(
        testing.allocator,
        "length",
        &suggestions,
        config,
    );
    defer report.deinit();

    try testing.expectEqualStrings("NAMING ERROR", report.title);
}

test "Source context rendering with RegionInfo" {
    const config = ReportingConfig.initForTesting();
    var report = Report.init(testing.allocator, "TEST", .runtime_error, config);
    defer report.deinit();

    const region = RegionInfo{
        .start_line_idx = 1,
        .start_col_idx = 12,
        .end_line_idx = 1,
        .end_col_idx = 27,
        .line_text = "    let x = undefinedVariable",
    };

    try report.addSourceContext(region);
    try testing.expect(!report.isEmpty());
}
