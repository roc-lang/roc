//! Diagnostics related to tokenization

const std = @import("std");
const base = @import("../../../base.zig");
const reporting = @import("../../../reporting.zig");

const Allocator = std.mem.Allocator;
const Region = base.Region;

const Diagnostic = @This();

tag: Tag,
begin: u32,
end: u32,

/// Represents the type of diagnostic message.
pub const Tag = enum {
    MisplacedCarriageReturn,
    AsciiControl,
    LeadingZero,
    UppercaseBase,
    InvalidUnicodeEscapeSequence,
    InvalidEscapeSequence,
    UnclosedString,
    UnclosedSingleQuote,
    OverClosedBrace,
    MismatchedBrace,
    NonPrintableUnicodeInStrLiteral,
};

/// Convert diagnostic to a string representation and write it to the provided writer.
pub fn toStr(self: Diagnostic, gpa: Allocator, source: []const u8, writer: anytype) !void {
    var newlines = try base.RegionInfo.findLineStarts(gpa, source);
    defer newlines.deinit();

    // Get position information
    const info = try base.RegionInfo.position(source, newlines.items, self.begin, self.end);

    // Strip trailing newline for display
    const display_text = if (info.line_text.len > 0 and
        (info.line_text[info.line_text.len - 1] == '\n' or
            info.line_text[info.line_text.len - 1] == '\r'))
        info.line_text[0 .. info.line_text.len - 1]
    else
        info.line_text;

    var spaces = std.ArrayList(u8).init(gpa);
    defer spaces.deinit();
    for (0..info.start_col_idx) |_| {
        try spaces.append(' ');
    }

    var carets = std.ArrayList(u8).init(gpa);
    defer carets.deinit();

    const caret_length = if (self.end > self.begin) self.end - self.begin else 1;
    for (0..caret_length) |_| {
        try carets.append('^');
    }

    const error_message = try std.fmt.allocPrint(
        gpa,
        "TOKENIZE: ({d}:{d}-{d}:{d}) {s}:\n{s}\n{s}{s}",
        .{
            // add one to display numbers instead of index
            info.start_line_idx + 1,
            info.start_col_idx + 1,
            info.end_line_idx + 1,
            info.end_col_idx + 1,
            @tagName(self.tag),
            display_text,
            spaces.items,
            carets.items,
        },
    );
    defer gpa.free(error_message);

    try writer.writeAll(error_message);
}

/// Convert this diagnostic to a Report for rendering
pub fn toReport(self: @This(), allocator: std.mem.Allocator, source: []const u8) !reporting.Report {
    const message = switch (self.tag) {
        .MisplacedCarriageReturn => "Misplaced carriage return character",
        .AsciiControl => "ASCII control character found",
        .LeadingZero => "Number literal cannot have leading zero",
        .UppercaseBase => "Number base prefix should be lowercase",
        .InvalidUnicodeEscapeSequence => "Invalid Unicode escape sequence",
        .InvalidEscapeSequence => "Invalid escape sequence",
        .UnclosedString => "Unclosed string literal",
        .UnclosedSingleQuote => "Unclosed single quote",
        .OverClosedBrace => "Too many closing braces",
        .MismatchedBrace => "Mismatched brace",
        .NonPrintableUnicodeInStrLiteral => "Non-printable Unicode character in string literal",
    };

    var report = reporting.Report.init(allocator, message, .runtime_error, reporting.ReportingConfig.initPlainText());

    // Add source context if we have a valid region
    if (self.begin < source.len and self.end <= source.len and self.begin <= self.end) {
        const problem_text = source[self.begin..self.end];
        try report.addCodeSnippet(problem_text, null);
    }

    return report;
}
