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
    /// Plain text output
    plain_text,
    /// Colored terminal output
    color_terminal,
    /// HTML output
    html,
    /// Language server protocol output
    language_server,
};

/// Configuration for the reporting system.
pub const ReportingConfig = struct {
    /// Color preference
    color_preference: ColorPreference,

    /// Whether the output is a TTY
    is_tty: bool,

    /// Preferred render target
    render_target: RenderTargetPreference,

    /// Maximum line width for rendering
    max_line_width: u32,

    /// Whether to show line numbers in source context
    show_line_numbers: bool,

    /// Number of context lines to show around errors
    context_lines: u32,

    /// Whether to validate UTF-8 output
    validate_utf8: bool,

    /// Maximum bytes for truncating error messages
    max_message_bytes: usize,

    pub fn init() ReportingConfig {
        return initFromEnv(std.heap.page_allocator) catch |err| switch (err) {
            error.OutOfMemory => @panic("Out of memory while initializing reporting config"),
        };
    }

    pub fn initFromEnv(allocator: Allocator) !ReportingConfig {
        var config = ReportingConfig{
            .color_preference = .auto,
            .is_tty = false,
            .render_target = .plain_text,
            .max_line_width = 80,
            .show_line_numbers = true,
            .context_lines = 3,
            .validate_utf8 = true,
            .max_message_bytes = 4096,
        };

        // Check if output is TTY
        config.is_tty = std.io.getStdOut().isTty();

        // Check NO_COLOR environment variable
        const no_color = std.process.getEnvVarOwned(allocator, "NO_COLOR") catch null;
        if (no_color) |value| {
            defer allocator.free(value);
            if (value.len > 0) {
                config.color_preference = .never;
            }
        }

        // Check FORCE_COLOR environment variable
        const force_color = std.process.getEnvVarOwned(allocator, "FORCE_COLOR") catch null;
        if (force_color) |value| {
            defer allocator.free(value);
            if (value.len > 0) {
                config.color_preference = .always;
            }
        }

        // Check ROC_HIGH_CONTRAST environment variable
        const high_contrast = std.process.getEnvVarOwned(allocator, "ROC_HIGH_CONTRAST") catch null;
        if (high_contrast) |value| {
            defer allocator.free(value);
            if (std.mem.eql(u8, value, "1")) {
                config.color_preference = .high_contrast;
            }
        }

        // Check ROC_MAX_LINE_WIDTH environment variable
        const max_width = std.process.getEnvVarOwned(allocator, "ROC_MAX_LINE_WIDTH") catch null;
        if (max_width) |value| {
            defer allocator.free(value);
            if (std.fmt.parseInt(u32, value, 10)) |width| {
                config.max_line_width = @max(40, @min(200, width)); // Clamp between 40-200
            } else |_| {
                // Invalid value, keep default
            }
        }

        // Set render target based on color preference and TTY status
        config.render_target = switch (config.color_preference) {
            .never => .plain_text,
            .auto => if (config.is_tty) .color_terminal else .plain_text,
            .always, .high_contrast => .color_terminal,
        };

        return config;
    }

    pub fn initPlainText() ReportingConfig {
        return ReportingConfig{
            .color_preference = .never,
            .is_tty = false,
            .render_target = .plain_text,
            .max_line_width = 80,
            .show_line_numbers = true,
            .context_lines = 3,
            .validate_utf8 = true,
            .max_message_bytes = 4096,
        };
    }

    pub fn initColorTerminal() ReportingConfig {
        return ReportingConfig{
            .color_preference = .always,
            .is_tty = true,
            .render_target = .color_terminal,
            .max_line_width = 80,
            .show_line_numbers = true,
            .context_lines = 3,
            .validate_utf8 = true,
            .max_message_bytes = 4096,
        };
    }

    pub fn initHighContrast() ReportingConfig {
        return ReportingConfig{
            .color_preference = .high_contrast,
            .is_tty = true,
            .render_target = .color_terminal,
            .max_line_width = 80,
            .show_line_numbers = true,
            .context_lines = 3,
            .validate_utf8 = true,
            .max_message_bytes = 4096,
        };
    }

    pub fn initForTesting() ReportingConfig {
        return ReportingConfig{
            .color_preference = .never,
            .is_tty = false,
            .render_target = .plain_text,
            .max_line_width = 80,
            .show_line_numbers = false, // Simpler output for tests
            .context_lines = 1,
            .validate_utf8 = true,
            .max_message_bytes = 1024,
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

    /// Get the number of context lines to show
    pub fn getContextLines(self: ReportingConfig) u32 {
        return self.context_lines;
    }

    /// Check if line numbers should be shown
    pub fn shouldShowLineNumbers(self: ReportingConfig) bool {
        return self.show_line_numbers;
    }

    /// Check if UTF-8 validation should be performed
    pub fn shouldValidateUtf8(self: ReportingConfig) bool {
        return self.validate_utf8;
    }

    /// Get the maximum message size in bytes
    pub fn getMaxMessageBytes(self: ReportingConfig) usize {
        return self.max_message_bytes;
    }
};

/// Validate that a string contains valid UTF-8
pub fn validateUtf8(bytes: []const u8) !void {
    if (!std.unicode.utf8ValidateSlice(bytes)) {
        return error.InvalidUtf8;
    }
}

/// Safely truncate a UTF-8 string to a maximum byte length
pub fn truncateUtf8(allocator: Allocator, input: []const u8, max_bytes: usize) ![]u8 {
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

/// Safely format a string with UTF-8 validation
pub fn formatUtf8(allocator: Allocator, comptime fmt: []const u8, args: anytype) ![]u8 {
    const result = try std.fmt.allocPrint(allocator, fmt, args);
    validateUtf8(result) catch |err| {
        allocator.free(result);
        return err;
    };
    return result;
}

/// Safely format a string with UTF-8 validation and truncation
pub fn formatUtf8Bounded(allocator: Allocator, max_bytes: usize, comptime fmt: []const u8, args: anytype) ![]u8 {
    const result = try std.fmt.allocPrint(allocator, fmt, args);
    defer allocator.free(result);

    return truncateUtf8(allocator, result, max_bytes);
}

/// Check if a byte is a UTF-8 continuation byte
pub fn isUtf8Continuation(byte: u8) bool {
    return (byte & 0b11000000) == 0b10000000;
}

/// Find the start of a UTF-8 character sequence at or before the given position
pub fn findUtf8CharStart(input: []const u8, pos: usize) usize {
    if (pos >= input.len) return input.len;

    var i = pos;
    while (i > 0 and isUtf8Continuation(input[i])) {
        i -= 1;
    }
    return i;
}

/// Find the end of a UTF-8 character sequence at or after the given position
pub fn findUtf8CharEnd(input: []const u8, pos: usize) usize {
    if (pos >= input.len) return input.len;

    var i = pos;
    while (i < input.len and isUtf8Continuation(input[i])) {
        i += 1;
    }
    return i;
}

// Tests
const testing = std.testing;

test "ReportingConfig initialization" {
    const config = ReportingConfig.initPlainText();
    try testing.expect(!config.shouldUseColors());
    try testing.expect(!config.is_tty);
    try testing.expectEqual(RenderTargetPreference.plain_text, config.getRenderTarget());
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

test "UTF-8 formatting" {
    const result = try formatUtf8(testing.allocator, "Hello, {s}!", .{"世界"});
    defer testing.allocator.free(result);
    try testing.expectEqualStrings("Hello, 世界!", result);
}

test "UTF-8 bounded formatting" {
    const result = try formatUtf8Bounded(testing.allocator, 10, "Hello, {s}!", .{"世界"});
    defer testing.allocator.free(result);
    try testing.expect(result.len <= 10);
    try validateUtf8(result);
}

test "UTF-8 character boundary detection" {
    const input = "Hello, 世界!";

    // Test continuation byte detection
    try testing.expect(!isUtf8Continuation('H'));
    try testing.expect(!isUtf8Continuation('e'));

    // Test with actual UTF-8 bytes - '世' is encoded as [0xE4, 0xB8, 0x96]
    try testing.expect(!isUtf8Continuation(0xE4)); // Start byte
    try testing.expect(isUtf8Continuation(0xB8)); // Continuation byte
    try testing.expect(isUtf8Continuation(0x96)); // Continuation byte

    // Test character start finding
    const start = findUtf8CharStart(input, 5);
    const end = findUtf8CharEnd(input, 5);
    try testing.expect(start <= 5);
    try testing.expect(end >= 5);
}

test "ReportingConfig for testing" {
    const config = ReportingConfig.initForTesting();
    try testing.expect(!config.shouldUseColors());
    try testing.expect(!config.shouldShowLineNumbers());
    try testing.expectEqual(@as(u32, 1), config.getContextLines());
    try testing.expectEqual(@as(usize, 1024), config.getMaxMessageBytes());
}
