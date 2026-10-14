//! Configuration for formatting warning and error reports

const std = @import("std");
const Allocator = std.mem.Allocator;

/// Color preference for reporting output
pub const ColorPreference = enum {
    /// Never use colors
    never,
    /// Use colors only when outputting to a TTY
    auto,
    /// Always use colors
    always,
    /// Use high contrast colors
    high_contrast,
};

/// Render target preference
pub const RenderTargetPreference = enum {
    /// Markdown output
    markdown,
    /// Colored terminal output
    color_terminal,
    /// HTML output
    html,
    /// Language server protocol output
    language_server,
};

/// Configuration for the reporting system.
pub const ReportingConfig = struct {
    /// Absolute working directory for terminal source locations. Borrowed for
    /// the duration of rendering; null preserves the supplied paths.
    source_path_base: ?[]const u8 = null,

    /// Color preference
    color_preference: ColorPreference,

    /// Whether the output is a TTY
    is_tty: bool,

    /// Preferred render target
    render_target: RenderTargetPreference,

    /// Maximum line width for rendering
    max_line_width: u32,

    pub fn initMarkdown() ReportingConfig {
        return ReportingConfig{
            .color_preference = .never,
            .is_tty = false,
            .render_target = .markdown,
            .max_line_width = 80,
        };
    }

    pub fn initHtml() ReportingConfig {
        return ReportingConfig{
            .color_preference = .never,
            .is_tty = false,
            .render_target = .html,
            .max_line_width = 80,
        };
    }

    pub fn initLsp() ReportingConfig {
        return ReportingConfig{
            .color_preference = .never,
            .is_tty = false,
            .render_target = .language_server,
            .max_line_width = 120, // LSP clients often have wider displays
        };
    }

    pub fn initColorTerminal() ReportingConfig {
        return ReportingConfig{
            .color_preference = .always,
            .is_tty = true,
            .render_target = .color_terminal,
            .max_line_width = 80,
        };
    }

    pub fn initHighContrast() ReportingConfig {
        return ReportingConfig{
            .color_preference = .high_contrast,
            .is_tty = true,
            .render_target = .color_terminal,
            .max_line_width = 80,
        };
    }

    pub fn initForTesting() ReportingConfig {
        return ReportingConfig{
            .color_preference = .never,
            .is_tty = false,
            .render_target = .markdown,
            .max_line_width = 80,
        };
    }

    /// Check if colors should be used based on configuration
    pub fn shouldUseColors(self: ReportingConfig) bool {
        return switch (self.color_preference) {
            .never => false,
            .auto => self.is_tty,
            .always, .high_contrast => true,
        };
    }

    /// Check if high contrast mode is enabled
    pub fn isHighContrast(self: ReportingConfig) bool {
        return self.color_preference == .high_contrast;
    }

    /// Get the appropriate render target for this configuration
    pub fn getRenderTarget(self: ReportingConfig) RenderTargetPreference {
        return self.render_target;
    }

    /// Get the maximum line width for this configuration
    pub fn getMaxLineWidth(self: ReportingConfig) u32 {
        return self.max_line_width;
    }
};

/// Validate that a string contains valid UTF-8
pub fn validateUtf8(bytes: []const u8) error{InvalidUtf8}!void {
    if (!std.unicode.utf8ValidateSlice(bytes)) {
        return error.InvalidUtf8;
    }
}

/// Safely truncate a UTF-8 string to a maximum byte length
pub fn truncateUtf8(allocator: Allocator, input: []const u8, max_bytes: usize) (Allocator.Error || error{ InvalidUtf8, CannotTruncate })![]u8 {
    try validateUtf8(input);

    if (input.len <= max_bytes) {
        return try allocator.dupe(u8, input);
    }

    // Find a safe truncation point that doesn't break UTF-8
    var truncate_at = max_bytes;
    while (truncate_at > 0) {
        if (std.unicode.utf8ValidateSlice(input[0..truncate_at])) {
            break;
        }
        truncate_at -= 1;
    }

    if (truncate_at == 0) {
        return error.CannotTruncate;
    }

    return try allocator.dupe(u8, input[0..truncate_at]);
}

// Tests
const testing = std.testing;

test "ReportingConfig initialization" {
    const config = ReportingConfig.initMarkdown();
    try testing.expect(!config.shouldUseColors());
    try testing.expect(!config.is_tty);
    try testing.expectEqual(RenderTargetPreference.markdown, config.getRenderTarget());
}

test "ReportingConfig color terminal" {
    const config = ReportingConfig.initColorTerminal();
    try testing.expect(config.shouldUseColors());
    try testing.expect(config.is_tty);
    try testing.expectEqual(RenderTargetPreference.color_terminal, config.getRenderTarget());
}

test "ReportingConfig high contrast" {
    const config = ReportingConfig.initHighContrast();
    try testing.expect(config.shouldUseColors());
    try testing.expect(config.isHighContrast());
    try testing.expectEqual(RenderTargetPreference.color_terminal, config.getRenderTarget());
}

test "ReportingConfig HTML" {
    const config = ReportingConfig.initHtml();
    try testing.expect(!config.shouldUseColors());
    try testing.expect(!config.is_tty);
    try testing.expectEqual(RenderTargetPreference.html, config.getRenderTarget());
}

test "ReportingConfig LSP" {
    const config = ReportingConfig.initLsp();
    try testing.expect(!config.shouldUseColors());
    try testing.expect(!config.is_tty);
    try testing.expectEqual(RenderTargetPreference.language_server, config.getRenderTarget());
    try testing.expectEqual(@as(u32, 120), config.getMaxLineWidth());
}

test "UTF-8 validation" {
    // Valid UTF-8
    try validateUtf8("Hello, 世界!");

    // Invalid UTF-8
    const invalid = [_]u8{ 0xFF, 0xFE };
    try testing.expectError(error.InvalidUtf8, validateUtf8(&invalid));
}

test "UTF-8 truncation" {
    const input = "Hello, 世界!";

    // No truncation needed
    const result1 = try truncateUtf8(testing.allocator, input, 20);
    defer testing.allocator.free(result1);
    try testing.expectEqualStrings(input, result1);

    // Truncation needed
    const result2 = try truncateUtf8(testing.allocator, input, 8);
    defer testing.allocator.free(result2);
    try testing.expect(result2.len <= 8);
    try validateUtf8(result2);
}

test "ReportingConfig for testing" {
    const config = ReportingConfig.initForTesting();
    try testing.expect(!config.shouldUseColors());
}
