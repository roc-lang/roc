//! Document system for structured content rendering.
//!
//! This module provides a flexible document building system that allows creating
//! rich, structured content that can be rendered to different output formats.
//! Documents are composed of elements that can be text, annotations, formatting
//! directives, and structural elements.

const std = @import("std");
const Allocator = std.mem.Allocator;
const renderer = @import("renderer.zig");
const RenderTarget = renderer.RenderTarget;
const ReportingConfig = @import("config.zig").ReportingConfig;
const collections = @import("../collections.zig");
const exitOnOom = collections.utils.exitOnOom;

/// A source code region with highlighting information.
pub const SourceRegion = struct {
    start_line: u32,
    start_column: u32,
    end_line: u32,
    end_column: u32,
    annotation: Annotation,
};

/// A region to underline within displayed source code
pub const UnderlineRegion = struct {
    start_line: u32,
    start_column: u32,
    end_line: u32,
    end_column: u32,
    annotation: Annotation,
};

/// Display region for source code with underlines
pub const SourceCodeDisplayRegion = struct {
    start_line: u32,
    start_column: u32,
    end_line: u32,
    end_column: u32,
    region_annotation: Annotation,
    filename: ?[]const u8,
};

/// Annotations that can be applied to document content for styling and semantics.
pub const Annotation = enum {
    /// Basic emphasis (usually bold or bright)
    emphasized,

    /// Language keywords (if, when, etc.)
    keyword,

    /// Type variables and type names
    type_variable,

    /// Error highlighting
    error_highlight,

    /// Warning highlighting
    warning_highlight,

    /// Suggestion highlighting
    suggestion,

    /// Code blocks and inline code
    code_block,

    /// Inline code elements
    inline_code,

    /// Module and symbol names
    symbol,

    /// File paths and locations
    path,

    /// Numbers and literals
    literal,

    /// Comments
    comment,

    /// Region underlines for source code
    underline,

    /// Dimmed text for less important content
    dimmed,

    /// Qualified symbols (Module.symbol)
    symbol_qualified,

    /// Unqualified symbols (symbol)
    symbol_unqualified,

    /// Module names
    module_name,

    /// Record field names
    record_field,

    /// Tag names in unions
    tag_name,

    /// Binary operators
    binary_operator,

    /// Source code region highlighting
    source_region,

    /// Reflowing text that can wrap and format automatically
    reflowing_text,

    /// Returns true if this annotation typically uses color.
    pub fn usesColor(self: Annotation) bool {
        return switch (self) {
            .emphasized, .dimmed => false,
            else => true,
        };
    }

    /// Returns a semantic name for this annotation.
    pub fn semanticName(self: Annotation) []const u8 {
        return switch (self) {
            .emphasized => "emphasis",
            .keyword => "keyword",
            .type_variable => "type",
            .error_highlight => "error",
            .warning_highlight => "warning",
            .suggestion => "suggestion",
            .code_block => "code-block",
            .inline_code => "code",
            .symbol => "symbol",
            .path => "path",
            .literal => "literal",
            .comment => "comment",
            .underline => "underline",
            .dimmed => "dim",
            .symbol_qualified => "symbol-qualified",
            .symbol_unqualified => "symbol-unqualified",
            .module_name => "module",
            .record_field => "record-field",
            .tag_name => "tag",
            .binary_operator => "operator",
            .source_region => "source-region",
            .reflowing_text => "reflow",
        };
    }
};

/// Individual elements that make up a document.
pub const DocumentElement = union(enum) {
    /// Plain text content
    text: []const u8,

    /// Text with annotation for styling
    annotated: struct {
        content: []const u8,
        annotation: Annotation,
    },

    /// Line break
    line_break,

    /// Horizontal indentation
    indent: u32,

    /// Horizontal spacing
    space: u32,

    /// Horizontal rule/separator
    horizontal_rule: ?u32, // Optional width, null means full width

    /// Start of an annotation region
    annotation_start: Annotation,

    /// End of an annotation region
    annotation_end,

    /// Raw content that should not be processed
    raw: []const u8,

    /// Text that should be reflowed/wrapped automatically
    reflowing_text: []const u8,

    /// Link URL
    link: []const u8,

    /// Vertical stack of elements
    vertical_stack: []DocumentElement,

    /// Horizontal concatenation of elements
    horizontal_concat: []DocumentElement,

    /// Source code region display with highlighting
    source_code_region: struct {
        /// The source code to display
        source: []const u8,
        /// Overall region to display (determines which lines to show)
        display_region: SourceCodeDisplayRegion,
    },

    /// Multiple highlighted regions within the same source code
    source_code_multi_region: struct {
        /// The source code to display
        source: []const u8,
        /// Multiple regions to highlight
        regions: []SourceRegion,
        /// Optional filename for context
        filename: ?[]const u8,
    },
    source_code_with_underlines: struct {
        /// The source code to display
        source: []const u8,
        /// Overall region to display (determines which lines to show)
        display_region: SourceCodeDisplayRegion,
        /// Regions to underline within the displayed code
        underline_regions: []UnderlineRegion,
    },

    /// Get the text content if this is a text element, null otherwise.
    pub fn getText(self: DocumentElement) ?[]const u8 {
        return switch (self) {
            .text => |t| t,
            .annotated => |a| a.content,
            .raw => |r| r,
            .reflowing_text => |rt| rt,
            else => null,
        };
    }

    /// Returns true if this element represents actual content.
    pub fn hasContent(self: DocumentElement) bool {
        return switch (self) {
            .text, .annotated, .raw, .reflowing_text, .link, .vertical_stack, .horizontal_concat, .source_code_region, .source_code_multi_region => true,
            else => false,
        };
    }
};

/// A document composed of structured elements that can be rendered.
pub const Document = struct {
    elements: std.ArrayList(DocumentElement),
    allocator: Allocator,

    pub fn init(allocator: Allocator) Document {
        return Document{
            .elements = std.ArrayList(DocumentElement).init(allocator),
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *Document) void {
        // Free any allocated element arrays
        for (self.elements.items) |element| {
            switch (element) {
                .vertical_stack => |stack| self.allocator.free(stack),
                .horizontal_concat => |concat| self.allocator.free(concat),
                .source_code_multi_region => |multi| self.allocator.free(multi.regions),
                .source_code_with_underlines => |underlines| self.allocator.free(underlines.underline_regions),
                else => {},
            }
        }
        self.elements.deinit();
    }

    /// Add plain text to the document.
    pub fn addText(self: *Document, text: []const u8) !void {
        if (text.len == 0) return;
        try self.elements.append(.{ .text = text });
    }

    /// Add annotated text to the document.
    pub fn addAnnotated(self: *Document, text: []const u8, annotation: Annotation) !void {
        if (text.len == 0) return;
        try self.elements.append(.{ .annotated = .{ .content = text, .annotation = annotation } });
    }

    /// Add a line break to the document.
    pub fn addLineBreak(self: *Document) !void {
        try self.elements.append(.line_break);
    }

    /// Add indentation to the document.
    pub fn addIndent(self: *Document, levels: u32) !void {
        if (levels == 0) return;
        try self.elements.append(.{ .indent = levels });
    }

    /// Add horizontal spacing to the document.
    pub fn addSpace(self: *Document, count: u32) !void {
        if (count == 0) return;
        try self.elements.append(.{ .space = count });
    }

    /// Add a horizontal rule separator.
    pub fn addHorizontalRule(self: *Document, width: ?u32) !void {
        try self.elements.append(.{ .horizontal_rule = width });
    }

    /// Start an annotation region.
    pub fn startAnnotation(self: *Document, annotation: Annotation) !void {
        try self.elements.append(.{ .annotation_start = annotation });
    }

    /// End the current annotation region.
    pub fn endAnnotation(self: *Document) !void {
        try self.elements.append(.annotation_end);
    }

    /// Add raw content that should not be processed.
    pub fn addRaw(self: *Document, content: []const u8) !void {
        if (content.len == 0) return;
        try self.elements.append(.{ .raw = content });
    }

    /// Add reflowing text that can wrap automatically.
    pub fn addReflowingText(self: *Document, text: []const u8) !void {
        if (text.len == 0) return;
        try self.elements.append(.{ .reflowing_text = text });
    }

    /// Add a source code display with multiple underlines.
    pub fn addSourceCodeWithUnderlines(
        self: *Document,
        source: []const u8,
        display_region: SourceCodeDisplayRegion,
        underline_regions: []const UnderlineRegion,
    ) !void {
        const owned_regions = try self.allocator.dupe(UnderlineRegion, underline_regions);
        try self.elements.append(.{
            .source_code_with_underlines = .{
                .source = source,
                .display_region = display_region,
                .underline_regions = owned_regions,
            },
        });
    }

    /// Add a vertical stack of elements.
    pub fn addVerticalStack(self: *Document, elements: []const DocumentElement) !void {
        if (elements.len == 0) return;
        const owned_elements = try self.allocator.dupe(DocumentElement, elements);
        try self.elements.append(.{ .vertical_stack = owned_elements });
    }

    /// Add a horizontal concatenation of elements.
    pub fn addHorizontalConcat(self: *Document, elements: []const DocumentElement) !void {
        if (elements.len == 0) return;
        const owned_elements = try self.allocator.dupe(DocumentElement, elements);
        try self.elements.append(.{ .horizontal_concat = owned_elements });
    }

    /// Convenience method to add annotated text with automatic annotation boundaries.
    pub fn addAnnotatedText(self: *Document, text: []const u8, annotation: Annotation) !void {
        try self.startAnnotation(annotation);
        try self.addText(text);
        try self.endAnnotation();
    }

    /// Add a formatted string to the document.
    pub fn addFormattedText(self: *Document, comptime fmt: []const u8, args: anytype) !void {
        var buf: [1024]u8 = undefined;
        const text = try std.fmt.bufPrint(&buf, fmt, args);
        try self.addText(text);
    }

    /// Add multiple line breaks.
    pub fn addLineBreaks(self: *Document, count: u32) !void {
        var i: u32 = 0;
        while (i < count) : (i += 1) {
            try self.addLineBreak();
        }
    }

    /// Add a code block with proper formatting.
    pub fn addCodeBlock(self: *Document, code: []const u8) !void {
        try self.startAnnotation(.code_block);

        // Split code into lines and add each with proper indentation
        var lines = std.mem.splitScalar(u8, code, '\n');
        var first = true;
        while (lines.next()) |line| {
            if (!first) {
                try self.addLineBreak();
            }
            first = false;

            if (line.len > 0) {
                try self.addIndent(1);
                try self.addText(line);
            }
        }

        try self.endAnnotation();
    }

    /// Add an inline code element.
    pub fn addInlineCode(self: *Document, code: []const u8) !void {
        try self.addAnnotated(code, .inline_code);
    }

    /// Add a keyword with proper styling.
    pub fn addKeyword(self: *Document, keyword: []const u8) !void {
        try self.addAnnotated(keyword, .keyword);
    }

    /// Add a type name with proper styling.
    pub fn addType(self: *Document, type_name: []const u8) !void {
        try self.addAnnotated(type_name, .type_variable);
    }

    /// Add an error message with proper styling.
    pub fn addError(self: *Document, message: []const u8) !void {
        try self.addAnnotated(message, .error_highlight);
    }

    /// Add a warning message with proper styling.
    pub fn addWarning(self: *Document, message: []const u8) !void {
        try self.addAnnotated(message, .warning_highlight);
    }

    /// Add a suggestion with proper styling.
    pub fn addSuggestion(self: *Document, suggestion: []const u8) !void {
        try self.addAnnotated(suggestion, .suggestion);
    }

    /// Add a qualified symbol with proper styling.
    pub fn addQualifiedSymbol(self: *Document, symbol: []const u8) !void {
        try self.addAnnotated(symbol, .symbol_qualified);
    }

    /// Add an unqualified symbol with proper styling.
    pub fn addUnqualifiedSymbol(self: *Document, symbol: []const u8) !void {
        try self.addAnnotated(symbol, .symbol_unqualified);
    }

    /// Add a module name with proper styling.
    pub fn addModuleName(self: *Document, module_name: []const u8) !void {
        try self.addAnnotated(module_name, .module_name);
    }

    /// Add a record field name with proper styling.
    pub fn addRecordField(self: *Document, field_name: []const u8) !void {
        try self.addAnnotated(field_name, .record_field);
    }

    /// Add a tag name with proper styling.
    pub fn addTagName(self: *Document, tag_name: []const u8) !void {
        try self.addAnnotated(tag_name, .tag_name);
    }

    /// Add a binary operator with proper styling.
    pub fn addBinaryOperator(self: *Document, operator: []const u8) !void {
        try self.addAnnotated(operator, .binary_operator);
    }

    /// Add a link with proper formatting.
    pub fn addLink(self: *Document, url: []const u8) !void {
        if (url.len == 0) return;
        try self.elements.append(.{ .link = url });
    }

    /// Add a source code region with highlighting.
    /// Accepts 0-based line and column coordinates, converts to 1-based for display.
    pub fn addSourceRegion(
        self: *Document,
        source: []const u8,
        start_line: u32,
        start_column: u32,
        end_line: u32,
        end_column: u32,
        annotation: Annotation,
        filename: ?[]const u8,
    ) !void {
        // Validate coordinates to catch programming errors early
        std.debug.assert(end_line >= start_line);
        if (start_line == end_line) {
            std.debug.assert(end_column >= start_column);
        }

        try self.elements.append(.{
            .source_code_region = .{
                .source = source,
                .display_region = .{
                    .start_line = start_line + 1,
                    .start_column = start_column + 1,
                    .end_line = end_line + 1,
                    .end_column = end_column + 1,
                    .region_annotation = annotation,
                    .filename = filename,
                },
            },
        });
    }

    /// Add a source code display with multiple highlighted regions.
    pub fn addSourceMultiRegion(
        self: *Document,
        source: []const u8,
        regions: []const SourceRegion,
        filename: ?[]const u8,
    ) !void {
        if (regions.len == 0) return;
        const owned_regions = try self.allocator.dupe(SourceRegion, regions);
        try self.elements.append(.{
            .source_code_multi_region = .{
                .source = source,
                .regions = owned_regions,
                .filename = filename,
            },
        });
    }

    /// Get the total number of elements in the document.
    pub fn elementCount(self: *const Document) usize {
        return self.elements.items.len;
    }

    /// Check if the document is empty.
    pub fn isEmpty(self: *const Document) bool {
        return self.elements.items.len == 0;
    }

    /// Get an element by index.
    pub fn getElement(self: *const Document, index: usize) ?DocumentElement {
        if (index >= self.elements.items.len) return null;
        return self.elements.items[index];
    }

    /// Clear all elements from the document.
    pub fn clear(self: *Document) void {
        self.elements.clearRetainingCapacity();
    }

    /// Render the document to the specified writer and target format.
    pub fn render(self: *const Document, writer: anytype, target: RenderTarget, config: ReportingConfig) !void {
        try renderer.renderDocument(self, writer, target, config);
    }
};

/// A document builder that provides a fluent interface for creating documents.
pub const DocumentBuilder = struct {
    document: Document,

    pub fn init(allocator: Allocator) DocumentBuilder {
        return DocumentBuilder{
            .document = Document.init(allocator),
        };
    }

    pub fn deinit(self: *DocumentBuilder) void {
        self.document.deinit();
    }

    pub fn text(self: *DocumentBuilder, content: []const u8) *DocumentBuilder {
        self.document.addText(content) catch |err| exitOnOom(err);
        return self;
    }

    pub fn annotated(self: *DocumentBuilder, content: []const u8, annotation: Annotation) *DocumentBuilder {
        self.document.addAnnotated(content, annotation) catch |err| exitOnOom(err);
        return self;
    }

    pub fn lineBreak(self: *DocumentBuilder) *DocumentBuilder {
        self.document.addLineBreak() catch |err| exitOnOom(err);
        return self;
    }

    pub fn indent(self: *DocumentBuilder, levels: u32) *DocumentBuilder {
        self.document.addIndent(levels) catch |err| exitOnOom(err);
        return self;
    }

    pub fn space(self: *DocumentBuilder, count: u32) *DocumentBuilder {
        self.document.addSpace(count) catch |err| exitOnOom(err);
        return self;
    }

    pub fn rule(self: *DocumentBuilder, width: ?u32) *DocumentBuilder {
        self.document.addHorizontalRule(width) catch |err| exitOnOom(err);
        return self;
    }

    pub fn keyword(self: *DocumentBuilder, kw: []const u8) *DocumentBuilder {
        self.document.addKeyword(kw) catch |err| exitOnOom(err);
        return self;
    }

    pub fn typeText(self: *DocumentBuilder, type_name: []const u8) *DocumentBuilder {
        self.document.addType(type_name) catch |err| exitOnOom(err);
        return self;
    }

    pub fn errorText(self: *DocumentBuilder, message: []const u8) *DocumentBuilder {
        self.document.addError(message) catch |err| exitOnOom(err);
        return self;
    }

    pub fn warning(self: *DocumentBuilder, message: []const u8) *DocumentBuilder {
        self.document.addWarning(message) catch |err| exitOnOom(err);
        return self;
    }

    pub fn suggestion(self: *DocumentBuilder, sug: []const u8) *DocumentBuilder {
        self.document.addSuggestion(sug) catch |err| exitOnOom(err);
        return self;
    }

    pub fn reflow(self: *DocumentBuilder, content: []const u8) *DocumentBuilder {
        self.document.addReflowingText(content) catch |err| exitOnOom(err);
        return self;
    }

    pub fn qualifiedSymbol(self: *DocumentBuilder, symbol: []const u8) *DocumentBuilder {
        self.document.addQualifiedSymbol(symbol) catch |err| exitOnOom(err);
        return self;
    }

    pub fn unqualifiedSymbol(self: *DocumentBuilder, symbol: []const u8) *DocumentBuilder {
        self.document.addUnqualifiedSymbol(symbol) catch |err| exitOnOom(err);
        return self;
    }

    pub fn moduleName(self: *DocumentBuilder, module_name: []const u8) *DocumentBuilder {
        self.document.addModuleName(module_name) catch |err| exitOnOom(err);
        return self;
    }

    pub fn recordField(self: *DocumentBuilder, field_name: []const u8) *DocumentBuilder {
        self.document.addRecordField(field_name) catch |err| exitOnOom(err);
        return self;
    }

    pub fn tagName(self: *DocumentBuilder, tag_name: []const u8) *DocumentBuilder {
        self.document.addTagName(tag_name) catch |err| exitOnOom(err);
        return self;
    }

    pub fn binaryOperator(self: *DocumentBuilder, operator: []const u8) *DocumentBuilder {
        self.document.addBinaryOperator(operator) catch |err| exitOnOom(err);
        return self;
    }

    pub fn link(self: *DocumentBuilder, url: []const u8) *DocumentBuilder {
        self.document.addLink(url) catch |err| exitOnOom(err);
        return self;
    }

    pub fn verticalStack(self: *DocumentBuilder, elements: []const DocumentElement) *DocumentBuilder {
        self.document.addVerticalStack(elements) catch |err| exitOnOom(err);
        return self;
    }

    pub fn horizontalConcat(self: *DocumentBuilder, elements: []const DocumentElement) *DocumentBuilder {
        self.document.addHorizontalConcat(elements) catch |err| exitOnOom(err);
        return self;
    }

    pub fn sourceRegion(
        self: *DocumentBuilder,
        source: []const u8,
        start_line: u32,
        start_column: u32,
        end_line: u32,
        end_column: u32,
        annotation: Annotation,
        filename: ?[]const u8,
    ) *DocumentBuilder {
        self.document.addSourceRegion(source, start_line, start_column, end_line, end_column, annotation, filename) catch |err| exitOnOom(err);
        return self;
    }

    pub fn sourceMultiRegion(
        self: *DocumentBuilder,
        source: []const u8,
        regions: []const SourceRegion,
        filename: ?[]const u8,
    ) *DocumentBuilder {
        self.document.addSourceMultiRegion(source, regions, filename) catch |err| exitOnOom(err);
        return self;
    }

    pub fn build(self: *DocumentBuilder) Document {
        return self.document;
    }
};
