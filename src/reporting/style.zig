//! Styls for reporting.
//!
//! This module provides color palettes, style definitions, and utilities for
//! rendering styled content across different output targets. It supports
//! ANSI terminal colors, HTML colors, and plain text fallbacks.

const std = @import("std");
const Annotation = @import("document.zig").Annotation;
const ReportingConfig = @import("config.zig").ReportingConfig;

/// ANSI escape codes for terminal styling.
pub const AnsiCodes = struct {
    // Colors
    pub const BLACK = "\x1b[30m";
    pub const RED = "\x1b[31m";
    pub const GREEN = "\x1b[32m";
    pub const YELLOW = "\x1b[33m";
    pub const BLUE = "\x1b[34m";
    pub const MAGENTA = "\x1b[35m";
    pub const CYAN = "\x1b[36m";
    pub const WHITE = "\x1b[37m";

    // Bright colors
    pub const BRIGHT_BLACK = "\x1b[90m";
    pub const BRIGHT_RED = "\x1b[91m";
    pub const BRIGHT_GREEN = "\x1b[92m";
    pub const BRIGHT_YELLOW = "\x1b[93m";
    pub const BRIGHT_BLUE = "\x1b[94m";
    pub const BRIGHT_MAGENTA = "\x1b[95m";
    pub const BRIGHT_CYAN = "\x1b[96m";
    pub const BRIGHT_WHITE = "\x1b[97m";

    // Styles
    pub const RESET = "\x1b[0m";
    pub const BOLD = "\x1b[1m";
    pub const DIM = "\x1b[2m";
    pub const ITALIC = "\x1b[3m";
    pub const UNDERLINE = "\x1b[4m";
    pub const BLINK = "\x1b[5m";
    pub const REVERSE = "\x1b[7m";
    pub const STRIKETHROUGH = "\x1b[9m";

    // Reset specific styles
    pub const RESET_BOLD = "\x1b[22m";
    pub const RESET_DIM = "\x1b[22m";
    pub const RESET_ITALIC = "\x1b[23m";
    pub const RESET_UNDERLINE = "\x1b[24m";
    pub const RESET_BLINK = "\x1b[25m";
    pub const RESET_REVERSE = "\x1b[27m";
    pub const RESET_STRIKETHROUGH = "\x1b[29m";
};

/// HTML color definitions.
pub const HtmlColors = struct {
    // Standard colors
    pub const BLACK = "#000000";
    pub const RED = "#dc322f";
    pub const GREEN = "#859900";
    pub const YELLOW = "#b58900";
    pub const BLUE = "#268bd2";
    pub const MAGENTA = "#d33682";
    pub const CYAN = "#2aa198";
    pub const WHITE = "#ffffff";

    // Bright colors
    pub const BRIGHT_RED = "#ff5555";
    pub const BRIGHT_GREEN = "#50fa7b";
    pub const BRIGHT_YELLOW = "#f1fa8c";
    pub const BRIGHT_BLUE = "#8be9fd";
    pub const BRIGHT_MAGENTA = "#ff79c6";
    pub const BRIGHT_CYAN = "#8be9fd";
    pub const BRIGHT_WHITE = "#f8f8f2";

    // Semantic colors
    pub const ERROR = "#ff5555";
    pub const WARNING = "#ffb86c";
    pub const INFO = "#8be9fd";
    pub const SUCCESS = "#50fa7b";
    pub const MUTED = "#6272a4";
};

/// A color palette that can be used across different rendering targets.
pub const ColorPalette = struct {
    // Core semantic colors
    primary: []const u8,
    secondary: []const u8,
    error_color: []const u8,
    warning: []const u8,
    info: []const u8,
    success: []const u8,
    suggestion: []const u8,
    muted: []const u8,

    // Syntax highlighting colors
    keyword: []const u8,
    type_variable: []const u8,
    literal: []const u8,
    comment: []const u8,
    symbol: []const u8,
    path: []const u8,

    // Style codes
    reset: []const u8,
    bold: []const u8,
    dim: []const u8,
    underline: []const u8,
    italic: []const u8,

    /// ANSI color palette for terminals.
    pub const ANSI = ColorPalette{
        // Core colors
        .primary = AnsiCodes.CYAN,
        .secondary = AnsiCodes.BRIGHT_BLACK,
        .error_color = AnsiCodes.RED,
        .warning = AnsiCodes.YELLOW,
        .info = AnsiCodes.BLUE,
        .success = AnsiCodes.GREEN,
        .suggestion = AnsiCodes.GREEN,
        .muted = AnsiCodes.BRIGHT_BLACK,

        // Syntax colors
        .keyword = AnsiCodes.MAGENTA,
        .type_variable = AnsiCodes.BLUE,
        .literal = AnsiCodes.GREEN,
        .comment = AnsiCodes.BRIGHT_BLACK,
        .symbol = AnsiCodes.CYAN,
        .path = AnsiCodes.YELLOW,

        // Styles
        .reset = AnsiCodes.RESET,
        .bold = AnsiCodes.BOLD,
        .dim = AnsiCodes.DIM,
        .underline = AnsiCodes.UNDERLINE,
        .italic = AnsiCodes.ITALIC,
    };

    /// Bright ANSI color palette for high-contrast terminals.
    pub const ANSI_BRIGHT = ColorPalette{
        // Core colors
        .primary = AnsiCodes.BRIGHT_CYAN,
        .secondary = AnsiCodes.WHITE,
        .error_color = AnsiCodes.BRIGHT_RED,
        .warning = AnsiCodes.BRIGHT_YELLOW,
        .info = AnsiCodes.BRIGHT_BLUE,
        .success = AnsiCodes.BRIGHT_GREEN,
        .suggestion = AnsiCodes.BRIGHT_GREEN,
        .muted = AnsiCodes.BRIGHT_BLACK,

        // Syntax colors
        .keyword = AnsiCodes.BRIGHT_MAGENTA,
        .type_variable = AnsiCodes.BRIGHT_BLUE,
        .literal = AnsiCodes.BRIGHT_GREEN,
        .comment = AnsiCodes.BRIGHT_BLACK,
        .symbol = AnsiCodes.BRIGHT_CYAN,
        .path = AnsiCodes.BRIGHT_YELLOW,

        // Styles
        .reset = AnsiCodes.RESET,
        .bold = AnsiCodes.BOLD,
        .dim = AnsiCodes.DIM,
        .underline = AnsiCodes.UNDERLINE,
        .italic = AnsiCodes.ITALIC,
    };

    /// Plain text palette with no colors.
    pub const NO_COLOR = ColorPalette{
        // All colors are empty strings
        .primary = "",
        .secondary = "",
        .error_color = "",
        .warning = "",
        .info = "",
        .success = "",
        .suggestion = "",
        .muted = "",
        .keyword = "",
        .type_variable = "",
        .literal = "",
        .comment = "",
        .symbol = "",
        .path = "",

        // Styles are also empty
        .reset = "",
        .bold = "",
        .dim = "",
        .underline = "",
        .italic = "",
    };

    /// HTML color palette for web rendering.
    pub const HTML = ColorPalette{
        // Core colors (CSS classes)
        .primary = "color: " ++ HtmlColors.CYAN ++ ";",
        .secondary = "color: " ++ HtmlColors.WHITE ++ ";",
        .error_color = "color: " ++ HtmlColors.ERROR ++ ";",
        .warning = "color: " ++ HtmlColors.WARNING ++ ";",
        .info = "color: " ++ HtmlColors.INFO ++ ";",
        .success = "color: " ++ HtmlColors.SUCCESS ++ ";",
        .suggestion = "color: " ++ HtmlColors.SUCCESS ++ ";",
        .muted = "color: " ++ HtmlColors.MUTED ++ ";",

        // Syntax colors
        .keyword = "color: " ++ HtmlColors.MAGENTA ++ ";",
        .type_variable = "color: " ++ HtmlColors.BLUE ++ ";",
        .literal = "color: " ++ HtmlColors.GREEN ++ ";",
        .comment = "color: " ++ HtmlColors.MUTED ++ ";",
        .symbol = "color: " ++ HtmlColors.CYAN ++ ";",
        .path = "color: " ++ HtmlColors.YELLOW ++ ";",

        // HTML styles
        .reset = "",
        .bold = "font-weight: bold;",
        .dim = "opacity: 0.6;",
        .underline = "text-decoration: underline;",
        .italic = "font-style: italic;",
    };

    /// Get the appropriate color for an annotation.
    pub fn colorForAnnotation(self: ColorPalette, annotation: Annotation) []const u8 {
        return switch (annotation) {
            .emphasized => self.bold,
            .keyword => self.keyword,
            .type_variable => self.type_variable,
            .error_highlight => self.error_color,
            .warning_highlight => self.warning,
            .suggestion => self.success,
            .code_block, .inline_code => self.primary,
            .symbol => self.symbol,
            .path => self.path,
            .literal => self.literal,
            .comment => self.comment,
            .underline => self.underline,
            .dimmed => self.dim,
            .symbol_qualified => self.symbol,
            .symbol_unqualified => self.symbol,
            .module_name => self.primary,
            .record_field => self.type_variable,
            .tag_name => self.type_variable,
            .binary_operator => self.keyword,
            .source_region => self.primary,
            .reflowing_text => self.reset,
        };
    }
};

/// Style information for rendering.
pub const Style = struct {
    color: []const u8,
    bold: bool = false,
    dim: bool = false,
    underline: bool = false,
    italic: bool = false,

    pub fn init(color: []const u8) Style {
        return Style{ .color = color };
    }

    pub fn withBold(self: Style) Style {
        return Style{
            .color = self.color,
            .bold = true,
            .dim = self.dim,
            .underline = self.underline,
            .italic = self.italic,
        };
    }

    pub fn withDim(self: Style) Style {
        return Style{
            .color = self.color,
            .bold = self.bold,
            .dim = true,
            .underline = self.underline,
            .italic = self.italic,
        };
    }

    pub fn withUnderline(self: Style) Style {
        return Style{
            .color = self.color,
            .bold = self.bold,
            .dim = self.dim,
            .underline = true,
            .italic = self.italic,
        };
    }

    pub fn withItalic(self: Style) Style {
        return Style{
            .color = self.color,
            .bold = self.bold,
            .dim = self.dim,
            .underline = self.underline,
            .italic = true,
        };
    }
};

/// Utilities for color handling.
pub const ColorUtils = struct {
    /// Get the appropriate color palette based on configuration.
    pub fn getPaletteForConfig(config: ReportingConfig) ColorPalette {
        if (!config.shouldUseColors()) {
            return ColorPalette.NO_COLOR;
        }

        if (config.isHighContrast()) {
            return ColorPalette.ANSI_BRIGHT;
        }

        return ColorPalette.ANSI;
    }
};

/// Style theme configuration.
pub const Theme = struct {
    name: []const u8,
    palette: ColorPalette,
    use_bold: bool = true,
    use_underline: bool = true,
    use_dim: bool = true,

    pub const DEFAULT = Theme{
        .name = "default",
        .palette = ColorPalette.ANSI,
    };

    pub const HIGH_CONTRAST = Theme{
        .name = "high-contrast",
        .palette = ColorPalette.ANSI_BRIGHT,
    };

    pub const NO_COLOR = Theme{
        .name = "no-color",
        .palette = ColorPalette.NO_COLOR,
        .use_bold = false,
        .use_underline = false,
        .use_dim = false,
    };
};

// Tests
const testing = std.testing;

test "ColorPalette annotation mapping" {
    const palette = ColorPalette.ANSI;

    try testing.expectEqualStrings(AnsiCodes.RED, palette.colorForAnnotation(.error_highlight));
    try testing.expectEqualStrings(AnsiCodes.YELLOW, palette.colorForAnnotation(.warning_highlight));
    try testing.expectEqualStrings(AnsiCodes.MAGENTA, palette.colorForAnnotation(.keyword));
    try testing.expectEqualStrings(AnsiCodes.BLUE, palette.colorForAnnotation(.type_variable));
}

test "Style composition" {
    const style = Style.init(AnsiCodes.RED)
        .withBold()
        .withUnderline();

    try testing.expectEqualStrings(AnsiCodes.RED, style.color);
    try testing.expect(style.bold);
    try testing.expect(style.underline);
    try testing.expect(!style.dim);
    try testing.expect(!style.italic);
}

test "ColorUtils palette selection" {
    const plain_config = ReportingConfig.initPlainText();
    const color_config = ReportingConfig.initColorTerminal();
    const contrast_config = ReportingConfig.initHighContrast();

    const plain_palette = ColorUtils.getPaletteForConfig(plain_config);
    const color_palette = ColorUtils.getPaletteForConfig(color_config);
    const contrast_palette = ColorUtils.getPaletteForConfig(contrast_config);

    // Plain text should have no colors
    try testing.expectEqualStrings("", plain_palette.error_color);

    // Color terminal should have colors
    try testing.expectEqualStrings(AnsiCodes.RED, color_palette.error_color);

    // High contrast should have bright colors
    try testing.expectEqualStrings(AnsiCodes.BRIGHT_RED, contrast_palette.error_color);
}

test "Theme configurations" {
    try testing.expectEqualStrings("default", Theme.DEFAULT.name);
    try testing.expectEqualStrings("high-contrast", Theme.HIGH_CONTRAST.name);
    try testing.expectEqualStrings("no-color", Theme.NO_COLOR.name);

    try testing.expect(Theme.DEFAULT.use_bold);
    try testing.expect(!Theme.NO_COLOR.use_bold);
}
