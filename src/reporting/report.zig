//! Report system for formatted warning and error reports.
//!
//! This module provides the Report struct and related functionality for creating
//! structured error reports that can be rendered to different output formats.
//!
//! Reports combine a title, severity level, and formatted document content.

const std = @import("std");
const builtin = @import("builtin");
const base = @import("base");

const Allocator = std.mem.Allocator;
const Severity = @import("severity.zig").Severity;
const Document = @import("document.zig").Document;
const RenderTarget = @import("renderer.zig").RenderTarget;
const RegionInfo = base.RegionInfo;
const renderReport = @import("renderer.zig").renderReport;
const validateUtf8 = @import("config.zig").validateUtf8;
const truncateUtf8 = @import("config.zig").truncateUtf8;

/// Default maximum message size in bytes for truncation
const DEFAULT_MAX_MESSAGE_BYTES: usize = 4096;

/// True if `title` contains `needle` (which must be lowercase) as a substring,
/// compared case-insensitively.
fn titleContainsIgnoreCase(title: []const u8, needle: []const u8) bool {
    if (needle.len == 0 or title.len < needle.len) return false;
    var i: usize = 0;
    while (i + needle.len <= title.len) : (i += 1) {
        var matches = true;
        for (needle, 0..) |nc, j| {
            const tc = title[i + j];
            const lowered = if (tc >= 'A' and tc <= 'Z') tc + ('a' - 'A') else tc;
            if (lowered != nc) {
                matches = false;
                break;
            }
        }
        if (matches) return true;
    }
    return false;
}

/// In debug builds, enforce the house style for error-report title/description
/// text. These are compiled out of release builds.
///
/// A title must be:
///   - all ASCII;
///   - trimmed, with no newlines or consecutive spaces;
///   - title case outside backticks: every non-minor word begins with a capital
///     letter (or a digit), every word contains only alphanumeric characters,
///     except for `UTF-` prefixed words such as `UTF-8`, and the title has at
///     least one lowercase letter (so it reads as `Title Case`, not `ALL CAPS`
///     — the box/HTML/LSP renderers shout it back to ALL CAPS, while markdown
///     keeps the authored case);
///   - allowed to contain backticked inline code spans, whose contents must be
///     non-empty and trimmed, but are not title-cased;
///   - free of the word "comptime", which is a Zig term, not a Roc one, and so
///     must never reach user-facing text;
///   - free of any pairing of "annotation" with "need" or "miss" — Roc never
///     tells users they must annotate their types (see the panic below).
fn isTrimmed(text: []const u8) bool {
    return std.mem.eql(u8, text, std.mem.trim(u8, text, " \t\r\n"));
}

fn isAsciiLower(c: u8) bool {
    return c >= 'a' and c <= 'z';
}

fn isAsciiUpper(c: u8) bool {
    return c >= 'A' and c <= 'Z';
}

fn isAsciiDigit(c: u8) bool {
    return c >= '0' and c <= '9';
}

fn isAsciiAlphaNum(c: u8) bool {
    return isAsciiLower(c) or isAsciiUpper(c) or isAsciiDigit(c);
}

fn isUtfPrefixedTitleWord(word: []const u8) bool {
    const prefix = "UTF-";
    if (!std.mem.startsWith(u8, word, prefix)) return false;
    if (word.len == prefix.len) return false;

    for (word[prefix.len..]) |c| {
        if (!isAsciiAlphaNum(c)) return false;
    }

    return true;
}

fn containsNewline(text: []const u8) bool {
    return std.mem.findScalar(u8, text, '\n') != null or
        std.mem.findScalar(u8, text, '\r') != null;
}

fn isLowercaseTitleParticle(word: []const u8) bool {
    const particles = [_][]const u8{
        "a",   "an",   "and",    "as",      "at",      "but", "by",
        "for", "from", "in",     "into",    "nor",     "of",  "on",
        "or",  "over", "per",    "the",     "through", "to",  "via",
        "vs",  "with", "within", "without",
    };
    for (particles) |particle| {
        if (std.mem.eql(u8, word, particle)) return true;
    }
    return false;
}

fn isValidTitleWord(word: []const u8, is_first: bool, is_last: bool, has_lower: *bool) bool {
    if (word.len == 0) return false;

    const utf_prefixed = isUtfPrefixedTitleWord(word);
    for (word) |c| {
        if (!isAsciiAlphaNum(c) and !(utf_prefixed and c == '-')) return false;
        if (isAsciiLower(c)) has_lower.* = true;
    }

    if (isAsciiLower(word[0])) {
        return !is_first and !is_last and isLowercaseTitleParticle(word);
    }

    return true;
}

fn countTitleTokens(title: []const u8) ?usize {
    var count: usize = 0;
    var i: usize = 0;
    while (i < title.len) {
        if (title[i] == ' ') {
            i += 1;
            continue;
        }

        if (title[i] == '`') {
            const start = i + 1;
            i = start;
            while (i < title.len and title[i] != '`') : (i += 1) {}
            if (i == title.len) return null;

            const code = title[start..i];
            if (code.len == 0 or !isTrimmed(code)) return null;

            count += 1;
            i += 1;
        } else {
            const start = i;
            while (i < title.len and title[i] != ' ' and title[i] != '`') : (i += 1) {}
            if (start == i) return null;

            count += 1;
        }

        if (i < title.len and title[i] != ' ') return null;
    }

    return count;
}

fn validateTitleTokens(title: []const u8, token_count: usize) bool {
    var token_index: usize = 0;
    var has_lower = false;
    var i: usize = 0;
    while (i < title.len) {
        if (title[i] == ' ') {
            i += 1;
            continue;
        }

        if (title[i] == '`') {
            i += 1;
            while (i < title.len and title[i] != '`') : (i += 1) {}
            if (i == title.len) return false;
            i += 1;
            token_index += 1;
            continue;
        }

        const start = i;
        while (i < title.len and title[i] != ' ' and title[i] != '`') : (i += 1) {}
        if (!isValidTitleWord(title[start..i], token_index == 0, token_index + 1 == token_count, &has_lower)) {
            return false;
        }
        token_index += 1;
    }

    return token_index == token_count and has_lower;
}

fn isValidReportTitle(title: []const u8) bool {
    if (title.len == 0 or !isTrimmed(title) or containsNewline(title)) return false;
    if (std.mem.find(u8, title, "  ") != null) return false;

    for (title) |c| {
        if (c >= 0x80) return false;
    }

    const token_count = countTitleTokens(title) orelse return false;
    if (token_count == 0) return false;

    return validateTitleTokens(title, token_count);
}

fn isValidReportDescription(description: []const u8) bool {
    return isTrimmed(description);
}

fn assertValidTitleAndDescription(title: []const u8, description: []const u8) void {
    if (builtin.mode != .Debug) return;

    std.debug.assert(isValidReportTitle(title));
    std.debug.assert(isValidReportDescription(description));
    std.debug.assert(!titleContainsIgnoreCase(title, "comptime"));

    if (titleContainsIgnoreCase(title, "annotation") and
        (titleContainsIgnoreCase(title, "need") or titleContainsIgnoreCase(title, "miss")))
    {
        @panic(
            "Error-report title pairs \"annotation\" with \"need\"/\"miss\". Roc never tells " ++
                "users they NEED to annotate their types: unlike many languages, type annotations " ++
                "are never required as a matter of course, so no part of a diagnostic report should " ++
                "say a type needs, or is missing, an annotation. When a type is ambiguous, explain " ++
                "the ambiguity itself; at most, note that one way to make it unambiguous is to " ++
                "introduce a type annotation somewhere. Reword this report's title, headline, and " ++
                "body to describe the ambiguity rather than to demand an annotation.",
        );
    }
}

/// A structured report containing error information and formatted content.
pub const Report = struct {
    title: []const u8,
    /// One-sentence summary shown in the report's headline slot: the box's top
    /// edge in the terminal/snapshot layout, or a line under the title in the
    /// markdown/HTML/LSP renderers. Rich content, so inline code, type names,
    /// and operators keep their styling. Builders with a plain headline pass it
    /// as the `headline` string to `init`; builders that need inline styling
    /// pass `""` and add to this document directly.
    headline: Document,
    severity: Severity,
    document: Document,
    allocator: Allocator,
    owned_strings: std.array_list.Managed([]const u8),

    pub fn init(allocator: Allocator, title: []const u8, headline: []const u8, severity: Severity) Allocator.Error!Report {
        assertValidTitleAndDescription(title, headline);
        var report = Report{
            .title = title,
            .headline = Document.init(allocator),
            .severity = severity,
            .document = Document.init(allocator),
            .allocator = allocator,
            .owned_strings = std.array_list.Managed([]const u8).init(allocator),
        };
        if (headline.len > 0) {
            try report.headline.addReflowingText(headline);
        }
        return report;
    }

    pub fn deinit(self: *Report) void {
        for (self.owned_strings.items) |owned_string| {
            self.allocator.free(@constCast(owned_string));
        }
        self.owned_strings.deinit();
        self.headline.deinit();
        self.document.deinit();
    }

    /// Add a string that the report will take ownership of.
    /// Returns the owned copy of the string that can be safely used in the document.
    pub fn addOwnedString(self: *Report, string: []const u8) Allocator.Error![]const u8 {
        const owned_copy = try self.allocator.dupe(u8, string);
        try self.owned_strings.append(owned_copy);
        return owned_copy;
    }

    /// Render the report to the specified writer and target format.
    pub fn render(self: *const Report, writer: *std.Io.Writer, target: RenderTarget) (Allocator.Error || error{WriteFailed})!void {
        try renderReport(self, writer, target);
    }

    /// Add a section header to the report.
    pub fn addHeader(self: *Report, header: []const u8) Allocator.Error!void {
        try self.document.addLineBreak();
        try self.document.addAnnotated(header, .emphasized);
        try self.document.addLineBreak();
    }

    /// Add a code snippet with proper formatting and UTF-8 validation.
    pub fn addCodeSnippet(self: *Report, code: []const u8, line_number: ?u32) Allocator.Error!void {
        validateUtf8(code) catch |err| switch (err) {
            error.InvalidUtf8 => {
                try self.document.addError("[Invalid UTF-8 in code snippet]");
                try self.document.addLineBreak();
                return;
            },
        };

        if (line_number) |line_num| {
            try self.document.addFormattedText("{d}", .{line_num});
            try self.document.addText(" | ");
        } else {
            try self.document.addText("   | ");
        }
        try self.document.addCodeBlock(code);
        try self.document.addLineBreak();
    }

    /// Add source context using RegionInfo for better accuracy and simplicity.
    pub fn addSourceContext(self: *Report, region: RegionInfo, filename: ?[]const u8, source: []const u8, line_starts: []const u32) Allocator.Error!void {
        validateUtf8(region.calculateLineText(source, line_starts)) catch |err| switch (err) {
            error.InvalidUtf8 => {
                try self.document.addError("[Invalid UTF-8 in source context]");
                try self.document.addLineBreak();
                return;
            },
        };

        // Use proper source region API for consistent formatting
        // addSourceRegion now expects 0-based coordinates and handles conversion internally
        try self.document.addSourceRegion(
            region,
            .error_highlight,
            filename,
            source,
            line_starts,
        );
    }

    /// Add a suggestion with proper formatting and UTF-8 validation.
    pub fn addSuggestion(self: *Report, suggestion: []const u8) Allocator.Error!void {
        validateUtf8(suggestion) catch |err| switch (err) {
            error.InvalidUtf8 => {
                try self.document.addError("[Invalid UTF-8 in suggestion]");
                try self.document.addLineBreak();
                return;
            },
        };

        const truncated_suggestion = if (suggestion.len > DEFAULT_MAX_MESSAGE_BYTES) blk: {
            // `suggestion` is already validated as UTF-8 above, and the limit is
            // large enough that a truncation point always exists, so the only
            // failure `truncateUtf8` can produce here is running out of memory.
            const truncated = truncateUtf8(self.allocator, suggestion, DEFAULT_MAX_MESSAGE_BYTES) catch |err| switch (err) {
                error.OutOfMemory => return error.OutOfMemory,
                error.InvalidUtf8, error.CannotTruncate => unreachable,
            };
            if (truncated.ptr != suggestion.ptr) {
                try self.owned_strings.append(truncated);
            }
            break :blk truncated;
        } else suggestion;

        try self.document.addLineBreak();
        try self.document.addAnnotated("Hint: ", .suggestion);
        try self.document.addText(truncated_suggestion);
        try self.document.addLineBreak();
    }

    /// Add multiple suggestions as a list.
    pub fn addSuggestions(self: *Report, suggestions: []const []const u8) Allocator.Error!void {
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
    pub fn addTypeComparison(self: *Report, expected: []const u8, actual: []const u8) Allocator.Error!void {
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
    pub fn addNote(self: *Report, note: []const u8) Allocator.Error!void {
        try self.document.addLineBreak();
        try self.document.addAnnotated("Note: ", .dimmed);
        try self.document.addText(note);
        try self.document.addLineBreak();
    }

    /// Add an error message with proper styling.
    pub fn addErrorMessage(self: *Report, message: []const u8) Allocator.Error!void {
        try self.document.addError(message);
        try self.document.addLineBreak();
    }

    /// Add a warning message with proper styling.
    pub fn addWarningMessage(self: *Report, message: []const u8) Allocator.Error!void {
        try self.document.addWarning(message);
        try self.document.addLineBreak();
    }

    /// Add a separator line.
    pub fn addSeparator(self: *Report) Allocator.Error!void {
        try self.document.addLineBreak();
        try self.document.addHorizontalRule(40);
        try self.document.addLineBreak();
    }

    /// Extract region information from the document elements.
    /// Returns the first source region found, or null if none exists.
    pub fn getRegionInfo(self: *const Report) ?RegionInfo {
        for (self.document.elements.items) |element| {
            switch (element) {
                .source_code_region => |region_data| {
                    return RegionInfo{
                        .start_line_idx = region_data.start_line,
                        .start_col_idx = region_data.start_column,
                        .end_line_idx = region_data.end_line,
                        .end_col_idx = region_data.end_column,
                    };
                },
                .source_code_with_underlines => |underlines_data| {
                    return RegionInfo{
                        .start_line_idx = underlines_data.display_region.start_line,
                        .start_col_idx = underlines_data.display_region.start_column,
                        .end_line_idx = underlines_data.display_region.end_line,
                        .end_col_idx = underlines_data.display_region.end_column,
                    };
                },
                else => {},
            }
        }
        return null;
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

// Tests
const testing = std.testing;

test "Report title validation allows backticked code" {
    try testing.expect(isValidReportTitle("`dbg` in Optimized Build"));
    try testing.expect(isValidReportTitle("Type `main!` Should Take 1 Argument"));
    try testing.expect(isValidReportTitle("Try Operator Outside Function"));
    try testing.expect(isValidReportTitle("Invalid UTF-8"));
}

test "Report title validation rejects malformed titles" {
    try testing.expect(!isValidReportTitle(" Debug Error"));
    try testing.expect(!isValidReportTitle("Debug Error "));
    try testing.expect(!isValidReportTitle("Debug\nError"));
    try testing.expect(!isValidReportTitle("Debug  Error"));
    try testing.expect(!isValidReportTitle("Debug-Error"));
    try testing.expect(!isValidReportTitle("debug Error"));
    try testing.expect(!isValidReportTitle("Debug in"));
    try testing.expect(!isValidReportTitle("Debug ` dbg` Error"));
    try testing.expect(!isValidReportTitle("Debug `dbg ` Error"));
    try testing.expect(!isValidReportTitle("Debug `` Error"));
    try testing.expect(!isValidReportTitle("Debug `dbg Error"));
    try testing.expect(!isValidReportTitle("Debug UTF- Error"));
    try testing.expect(!isValidReportTitle("Debug UTF-8-16 Error"));
}

test "Report description validation checks trimming" {
    try testing.expect(isValidReportDescription(""));
    try testing.expect(isValidReportDescription("Something went wrong."));
    try testing.expect(!isValidReportDescription(" Something went wrong."));
    try testing.expect(!isValidReportDescription("Something went wrong. "));
}

test "Report basic functionality" {
    var report = try Report.init(testing.allocator, "Test Error", "Something went wrong in the test.", .runtime_error);
    defer report.deinit();

    try report.document.addText("This is a test error message.");
    try report.addSuggestion("Try fixing the issue.");

    try testing.expect(!report.isEmpty());
    try testing.expect(report.getLineCount() > 2);
}
