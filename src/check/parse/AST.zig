//!
//! This file implements the Intermediate Representation (IR) for Roc's parser.
//!
//! The IR provides a structured, tree-based representation of Roc source code after parsing
//!
//! The design uses an arena-based memory allocation strategy with a "multi-list" approach where nodes
//! are stored in a flat list but cross-referenced via indices rather than pointers. This improves
//! memory locality and efficiency.
//!
//! The implementation includes comprehensive facilities for building, manipulating, and traversing
//! the IR, as well as converting it to S-expressions for debugging and visualization.

const std = @import("std");
const base = @import("../../base.zig");
const sexpr = @import("../../base/sexpr.zig");
const tokenize = @import("tokenize.zig");
const Token = tokenize.Token;
const collections = @import("../../collections.zig");
const reporting = @import("../../reporting.zig");

const Node = @import("Node.zig");
const NodeStore = @import("NodeStore.zig");
const TokenizedBuffer = tokenize.TokenizedBuffer;
const exitOnOom = @import("../../collections/utils.zig").exitOnOom;

const testing = std.testing;
const Ident = base.Ident;
const Allocator = std.mem.Allocator;

const AST = @This();

source: []const u8,
tokens: TokenizedBuffer,
store: NodeStore,
root_node_idx: u32 = 0,
tokenize_diagnostics: std.ArrayListUnmanaged(tokenize.Diagnostic),
parse_diagnostics: std.ArrayListUnmanaged(AST.Diagnostic),

/// Calculate whether this region is - or will be - multiline
pub fn regionIsMultiline(self: *AST, region: TokenizedRegion) bool {
    var i = region.start;
    const tags = self.tokens.tokens.items(.tag);
    while (i <= region.end) {
        if (tags[i] == .Newline) {
            return true;
        }
        if (tags[i] == .Comma and (tags[i + 1] == .CloseSquare or
            tags[i + 1] == .CloseRound or
            tags[i + 1] == .CloseCurly))
        {
            return true;
        }
        i += 1;
    }
    return false;
}

/// Returns whether this AST has any diagnostic errors.
pub fn hasErrors(self: *AST) bool {
    return self.tokenize_diagnostics.items.len > 0 or self.parse_diagnostics.items.len > 0;
}

/// Returns diagnostic position information for the given region.
pub fn calcRegionInfo(self: *AST, region: TokenizedRegion, line_starts: []const u32) base.RegionInfo {
    const start = self.tokens.resolve(region.start);
    const end = self.tokens.resolve(region.end);
    const info = base.RegionInfo.position(self.source, line_starts, start.start.offset, end.end.offset) catch {
        // std.debug.panic("failed to calculate position info for region {?}, start: {}, end: {}", .{ region, start, end });
        return .{
            .start_line_idx = 0,
            .start_col_idx = 0,
            .end_line_idx = 0,
            .end_col_idx = 0,
            .line_text = "",
        };
    };

    return info;
}

pub fn deinit(self: *AST, gpa: std.mem.Allocator) void {
    defer self.tokens.deinit();
    defer self.store.deinit();
    defer self.tokenize_diagnostics.deinit(gpa);
    defer self.parse_diagnostics.deinit(gpa);
}

/// Convert a tokenize diagnostic to a Report for rendering
pub fn tokenizeDiagnosticToReport(self: *AST, diagnostic: tokenize.Diagnostic, allocator: std.mem.Allocator) !reporting.Report {
    _ = self; // TODO: Use self for source information
    const title = switch (diagnostic.tag) {
        .MisplacedCarriageReturn => "MISPLACED CARRIAGE RETURN",
        .AsciiControl => "ASCII CONTROL CHARACTER",
        .LeadingZero => "LEADING ZERO",
        .UppercaseBase => "UPPERCASE BASE",
        .InvalidUnicodeEscapeSequence => "INVALID UNICODE ESCAPE SEQUENCE",
        .InvalidEscapeSequence => "INVALID ESCAPE SEQUENCE",
        .UnclosedString => "UNCLOSED STRING",
        .UnclosedSingleQuote => "UNCLOSED SINGLE QUOTE",
        .OverClosedBrace => "OVER CLOSED BRACE",
        .MismatchedBrace => "MISMATCHED BRACE",
        .NonPrintableUnicodeInStrLiteral => "NON-PRINTABLE UNICODE IN STRING LITERAL",
    };

    const body = switch (diagnostic.tag) {
        .MisplacedCarriageReturn => "Carriage return characters (\\r) are not allowed in Roc source code.",
        .AsciiControl => "ASCII control characters are not allowed in Roc source code.",
        .LeadingZero => "Numbers cannot have leading zeros.",
        .UppercaseBase => "Number base prefixes must be lowercase (0x, 0o, 0b).",
        .InvalidUnicodeEscapeSequence => "This Unicode escape sequence is not valid.",
        .InvalidEscapeSequence => "This escape sequence is not recognized.",
        .UnclosedString => "This string is missing a closing quote.",
        .UnclosedSingleQuote => "This character literal is missing a closing single quote.",
        .OverClosedBrace => "There are too many closing braces here.",
        .MismatchedBrace => "This brace does not match the corresponding opening brace.",
        .NonPrintableUnicodeInStrLiteral => "Non-printable Unicode characters are not allowed in string literals.",
    };

    const config = reporting.ReportingConfig.initForTesting();
    var report = reporting.Report.init(allocator, title, .runtime_error, config);
    try report.document.addText(body);
    return report;
}

/// Convert a parse diagnostic to a Report for rendering
pub fn parseDiagnosticToReport(self: *AST, diagnostic: Diagnostic, allocator: std.mem.Allocator) !reporting.Report {
    _ = self; // TODO: Use self for source information
    const title = switch (diagnostic.tag) {
        .bad_indent => "BAD INDENTATION",
        .multiple_platforms => "MULTIPLE PLATFORMS",
        .no_platform => "NO PLATFORM",
        .missing_header => "MISSING HEADER",
        .list_not_closed => "LIST NOT CLOSED",
        .missing_arrow => "MISSING ARROW",
        .expected_exposes => "EXPECTED EXPOSES",
        .expected_exposes_close_square => "EXPECTED CLOSING BRACKET",
        .expected_exposes_open_square => "EXPECTED OPENING BRACKET",
        .expected_imports => "EXPECTED IMPORTS",
        .expected_imports_close_curly => "EXPECTED CLOSING BRACE",
        .expected_imports_open_curly => "EXPECTED OPENING BRACE",
        .header_unexpected_token => "UNEXPECTED TOKEN IN HEADER",
        .pattern_unexpected_token => "UNEXPECTED TOKEN IN PATTERN",
        .pattern_unexpected_eof => "UNEXPECTED END OF FILE IN PATTERN",
        .ty_anno_unexpected_token => "UNEXPECTED TOKEN IN TYPE ANNOTATION",
        .statement_unexpected_eof => "UNEXPECTED END OF FILE",
        .statement_unexpected_token => "UNEXPECTED TOKEN",
        .string_unexpected_token => "UNEXPECTED TOKEN IN STRING",
        .expr_unexpected_token => "UNEXPECTED TOKEN IN EXPRESSION",
        .import_must_be_top_level => "IMPORT MUST BE TOP LEVEL",
        else => "PARSE ERROR",
    };

    const body = switch (diagnostic.tag) {
        .missing_header => "Roc files must start with a module header like 'module [main]' or 'app [main] { pf: platform \"...\" }'.",
        .multiple_platforms => "Only one platform declaration is allowed per file.",
        .no_platform => "App files must specify a platform.",
        .bad_indent => "The indentation here is inconsistent with the surrounding code.",
        .list_not_closed => "This list is missing a closing bracket ']'.",
        .missing_arrow => "Expected an arrow '->' here.",
        .header_unexpected_token => "This token is not expected in a module header.",
        .pattern_unexpected_token => "This token is not expected in a pattern.",
        .statement_unexpected_token => "This token is not expected in a statement.",
        .expr_unexpected_token => "This token is not expected in an expression.",
        .import_must_be_top_level => "Import statements must appear at the top level of a module.",
        else => "A parsing error occurred.",
    };

    const config = reporting.ReportingConfig.initForTesting();
    var report = reporting.Report.init(allocator, title, .runtime_error, config);
    try report.document.addText(body);
    return report;
}

/// Diagnostics related to parsing
pub const Diagnostic = struct {
    tag: Tag,
    region: TokenizedRegion,

    /// different types of diagnostic errors
    pub const Tag = enum {
        bad_indent,
        multiple_platforms,
        no_platform,
        missing_header,
        list_not_closed,
        missing_arrow,
        expected_exposes,
        expected_exposes_close_square,
        expected_exposes_open_square,
        expected_imports,
        expected_imports_close_curly,
        expected_imports_open_curly,
        expected_package_or_platform_name,
        expected_package_or_platform_colon,
        expected_package_or_platform_string,
        expected_package_platform_close_curly,
        expected_package_platform_open_curly,
        expected_packages,
        expected_packages_close_curly,
        expected_packages_open_curly,
        expected_platform_name_end,
        expected_platform_name_start,
        expected_platform_name_string,
        expected_platform_string,
        expected_provides,
        expected_provides_close_square,
        expected_provides_open_square,
        expected_requires,
        expected_requires_rigids_close_curly,
        expected_requires_rigids_open_curly,
        expected_requires_signatures_close_curly,
        expected_requires_signatures_open_curly,
        expect_closing_paren,
        header_expected_open_square,
        header_expected_close_square,
        header_unexpected_token,
        pattern_unexpected_token,
        pattern_unexpected_eof,
        ty_anno_unexpected_token,
        statement_unexpected_eof,
        statement_unexpected_token,
        string_unexpected_token,
        string_expected_close_interpolation,
        expr_if_missing_else,
        expr_no_space_dot_int,
        import_exposing_no_open,
        import_exposing_no_close,
        no_else,
        expected_type_field_name,
        expected_colon_after_type_field_name,
        expected_arrow,
        expected_ty_close_curly_or_comma,
        expected_ty_close_square_or_comma,
        expected_lower_name_after_exposed_item_as,
        expected_upper_name_after_exposed_item_as,
        exposed_item_unexpected_token,
        expected_upper_name_after_import_as,
        expected_colon_after_type_annotation,
        expected_lower_ident_pat_field_name,
        expected_colon_after_pat_field_name,
        expected_expr_bar,
        expected_expr_close_curly_or_comma,
        expected_expr_close_round_or_comma,
        expected_expr_close_square_or_comma,
        expected_close_curly_at_end_of_match,
        expected_open_curly_after_match,
        expr_unexpected_token,
        expected_expr_record_field_name,
        expected_ty_apply_close_round,
        expected_ty_anno_end_of_function,
        expected_ty_anno_end,
        expected_expr_apply_close_round,
        where_expected_where,
        where_expected_mod_open,
        where_expected_var,
        where_expected_mod_close,
        where_expected_arg_open,
        where_expected_arg_close,
        where_expected_method_arrow,
        where_expected_method_or_alias_name,
        where_expected_var_or_module,
        import_must_be_top_level,
        invalid_type_arg,
        expr_arrow_expects_ident,
        var_only_allowed_in_a_body,
        var_must_have_ident,
        var_expected_equals,
        for_expected_in,
    };
};

/// The first and last token consumed by a Node
pub const TokenizedRegion = struct {
    start: Token.Idx,
    end: Token.Idx,

    pub fn empty() TokenizedRegion {
        return .{ .start = 0, .end = 0 };
    }

    pub fn spanAcross(self: TokenizedRegion, other: TokenizedRegion) TokenizedRegion {
        return .{
            .start = self.start,
            .end = other.end,
        };
    }

    pub fn toBase(self: TokenizedRegion) base.Region {
        return .{
            .start = base.Region.Position{ .offset = self.start },
            .end = base.Region.Position{ .offset = self.end },
        };
    }
};

/// Resolve a token index to a string slice from the source code.
pub fn resolve(self: *AST, token: Token.Idx) []const u8 {
    const range = self.tokens.resolve(token);
    return self.source[@intCast(range.start.offset)..@intCast(range.end.offset)];
}

/// Contains properties of the thing to the right of the `import` keyword.
pub const ImportRhs = packed struct {
    /// e.g. 1 in case we use import `as`: `import Module as Mod`
    aliased: u1,
    /// 1 in case the import is qualified, e.g. `pf` in `import pf.Stdout ...`
    qualified: u1,
    /// The number of things in the exposes list. e.g. 3 in `import SomeModule exposing [a1, a2, a3]`
    num_exposes: u30,
};

// Check that all packed structs are 4 bytes size as they as cast to
// and from a u32
comptime {
    std.debug.assert(@sizeOf(Header.AppHeaderRhs) == 4);
    std.debug.assert(@sizeOf(ImportRhs) == 4);
}

test {
    _ = std.testing.refAllDeclsRecursive(@This());
}

/// Helper function to convert the AST to a human friendly representation in S-expression format
pub fn toSExprStr(ast: *@This(), env: *base.ModuleEnv, writer: std.io.AnyWriter) !void {
    const file = ast.store.getFile();

    var node = file.toSExpr(env, ast);
    defer node.deinit(env.gpa);

    node.toStringPretty(writer);
}

/// Represents a statement.  Not all statements are valid in all positions.
pub const Statement = union(enum) {
    decl: Decl,
    @"var": struct {
        name: Token.Idx,
        body: Expr.Idx,
        region: TokenizedRegion,
    },
    expr: struct {
        expr: Expr.Idx,
        region: TokenizedRegion,
    },
    crash: struct {
        expr: Expr.Idx,
        region: TokenizedRegion,
    },
    expect: struct {
        body: Expr.Idx,
        region: TokenizedRegion,
    },
    @"for": struct {
        patt: Pattern.Idx,
        expr: Expr.Idx,
        body: Expr.Idx,
        region: TokenizedRegion,
    },
    @"return": struct {
        expr: Expr.Idx,
        region: TokenizedRegion,
    },
    import: struct {
        module_name_tok: Token.Idx,
        qualifier_tok: ?Token.Idx,
        alias_tok: ?Token.Idx,
        exposes: ExposedItem.Span,
        region: TokenizedRegion,
    },
    type_decl: struct {
        header: TypeHeader.Idx,
        anno: TypeAnno.Idx,
        where: ?Collection.Idx,
        region: TokenizedRegion,
    },
    type_anno: struct {
        name: Token.Idx,
        anno: TypeAnno.Idx,
        where: ?Collection.Idx,
        region: TokenizedRegion,
    },
    malformed: struct {
        reason: Diagnostic.Tag,
        region: TokenizedRegion,
    },

    pub const Idx = enum(u32) { _ };
    pub const Span = struct { span: base.DataSpan };

    pub const Decl = struct {
        pattern: Pattern.Idx,
        body: Expr.Idx,
        region: TokenizedRegion,
    };

    pub fn toSExpr(self: @This(), env: *base.ModuleEnv, ast: *AST) sexpr.Expr {
        switch (self) {
            .decl => |decl| {
                var node = sexpr.Expr.init(env.gpa, "decl");
                node.appendRegionInfo(env.gpa, ast.calcRegionInfo(decl.region, env.line_starts.items));
                // pattern
                {
                    const pattern = ast.store.getPattern(decl.pattern);
                    var pattern_node = pattern.toSExpr(env, ast);
                    node.appendNode(env.gpa, &pattern_node);
                }
                // body
                {
                    const body = ast.store.getExpr(decl.body);
                    var body_node = body.toSExpr(env, ast);
                    node.appendNode(env.gpa, &body_node);
                }
                return node;
            },
            .@"var" => |v| {
                var node = sexpr.Expr.init(env.gpa, "var");
                node.appendRegionInfo(env.gpa, ast.calcRegionInfo(v.region, env.line_starts.items));
                // name
                {
                    const name_str = ast.resolve(v.name);
                    var child = sexpr.Expr.init(env.gpa, "name");
                    child.appendString(env.gpa, name_str);
                    node.appendNode(env.gpa, &child);
                }
                // body
                {
                    const body = ast.store.getExpr(v.body);
                    var body_node = body.toSExpr(env, ast);
                    node.appendNode(env.gpa, &body_node);
                }
                return node;
            },
            .expr => |expr| {
                return ast.store.getExpr(expr.expr).toSExpr(env, ast);
            },
            .import => |import| {
                var node = sexpr.Expr.init(env.gpa, "import");
                node.appendRegionInfo(env.gpa, ast.calcRegionInfo(import.region, env.line_starts.items));
                // name e.g. `Stdout` in `import pf.Stdout`
                node.appendString(env.gpa, ast.resolve(import.module_name_tok));
                // qualifier e.g. `pf` in `import pf.Stdout`
                if (import.qualifier_tok) |tok| {
                    const qualifier_str = ast.resolve(tok);
                    var child = sexpr.Expr.init(env.gpa, "qualifier");
                    child.appendString(env.gpa, qualifier_str);
                    node.appendNode(env.gpa, &child);
                }
                // alias e.g. `OUT` in `import pf.Stdout as OUT`
                if (import.alias_tok) |tok| {
                    const qualifier_str = ast.resolve(tok);
                    var child = sexpr.Expr.init(env.gpa, "alias");
                    child.appendString(env.gpa, qualifier_str);
                    node.appendNode(env.gpa, &child);
                }
                // exposed identifiers e.g. [foo, bar] in `import pf.Stdout exposing [foo, bar]`
                const exposed_slice = ast.store.exposedItemSlice(import.exposes);
                if (exposed_slice.len > 0) {
                    var exposed = sexpr.Expr.init(env.gpa, "exposing");
                    for (ast.store.exposedItemSlice(import.exposes)) |e| {
                        var exposed_item = &ast.store.getExposedItem(e);
                        var exposed_item_sexpr = exposed_item.toSExpr(env, ast);
                        exposed.appendNode(env.gpa, &exposed_item_sexpr);
                    }
                    node.appendNode(env.gpa, &exposed);
                }
                return node;
            },
            // (type_decl (header <name> [<args>]) <annotation>)
            .type_decl => |a| {
                var node = sexpr.Expr.init(env.gpa, "type_decl");
                node.appendRegionInfo(env.gpa, ast.calcRegionInfo(a.region, env.line_starts.items));
                var header = sexpr.Expr.init(env.gpa, "header");
                // pattern
                {
                    const ty_header = ast.store.getTypeHeader(a.header);
                    header.appendRegionInfo(env.gpa, ast.calcRegionInfo(ty_header.region, env.line_starts.items));

                    header.appendString(env.gpa, ast.resolve(ty_header.name));

                    var args_node = sexpr.Expr.init(env.gpa, "args");

                    for (ast.store.typeAnnoSlice(ty_header.args)) |b| {
                        const anno = ast.store.getTypeAnno(b);
                        var anno_sexpr = anno.toSExpr(env, ast);
                        args_node.appendNode(env.gpa, &anno_sexpr);
                    }
                    header.appendNode(env.gpa, &args_node);

                    node.appendNode(env.gpa, &header);
                }
                // annotation
                {
                    var annotation = ast.store.getTypeAnno(a.anno).toSExpr(env, ast);
                    node.appendNode(env.gpa, &annotation);
                }
                return node;
            },
            // (crash <expr>)
            .crash => |a| {
                var node = sexpr.Expr.init(env.gpa, "crash");

                node.appendRegionInfo(env.gpa, ast.calcRegionInfo(a.region, env.line_starts.items));

                var child = ast.store.getExpr(a.expr).toSExpr(env, ast);
                node.appendNode(env.gpa, &child);
                return node;
            },
            // (expect <body>)
            .expect => |a| {
                var node = sexpr.Expr.init(env.gpa, "expect");

                node.appendRegionInfo(env.gpa, ast.calcRegionInfo(a.region, env.line_starts.items));

                var child = ast.store.getExpr(a.body).toSExpr(env, ast);
                node.appendNode(env.gpa, &child);
                return node;
            },
            .@"for" => |a| {
                var node = sexpr.Expr.init(env.gpa, "for");

                // patt
                {
                    var child = ast.store.getPattern(a.patt).toSExpr(env, ast);
                    node.appendNode(env.gpa, &child);
                }
                // expr
                {
                    var child = ast.store.getExpr(a.expr).toSExpr(env, ast);
                    node.appendNode(env.gpa, &child);
                }
                // body
                {
                    var child = ast.store.getExpr(a.body).toSExpr(env, ast);
                    node.appendNode(env.gpa, &child);
                }

                return node;
            },
            // (return <expr>)
            .@"return" => |a| {
                var node = sexpr.Expr.init(env.gpa, "return");

                node.appendRegionInfo(env.gpa, ast.calcRegionInfo(a.region, env.line_starts.items));

                var child = ast.store.getExpr(a.expr).toSExpr(env, ast);
                node.appendNode(env.gpa, &child);
                return node;
            },
            // (type_anno <annotation>)
            .type_anno => |a| {
                var node = sexpr.Expr.init(env.gpa, "type_anno");

                node.appendRegionInfo(env.gpa, ast.calcRegionInfo(a.region, env.line_starts.items));

                node.appendString(env.gpa, ast.resolve(a.name));
                var child = ast.store.getTypeAnno(a.anno).toSExpr(env, ast);
                node.appendNode(env.gpa, &child);
                return node;
            },
            .malformed => |a| {
                var node = sexpr.Expr.init(env.gpa, "malformed_stmt");
                node.appendRegionInfo(env.gpa, ast.calcRegionInfo(a.region, env.line_starts.items));
                node.appendString(env.gpa, @tagName(a.reason));
                return node;
            },
        }
    }
};

/// Represents a Body, or a block of statements.
pub const Body = struct {
    /// The statements that constitute the block
    statements: Statement.Span,
    region: TokenizedRegion,

    pub fn toSExpr(self: @This(), env: *base.ModuleEnv, ast: *AST) sexpr.Expr {
        var block_node = sexpr.Expr.init(env.gpa, "block");
        block_node.appendRegionInfo(env.gpa, ast.calcRegionInfo(self.region, env.line_starts.items));
        var statements_node = sexpr.Expr.init(env.gpa, "statements");

        for (ast.store.statementSlice(self.statements)) |stmt_idx| {
            const stmt = ast.store.getStatement(stmt_idx);

            var stmt_node = stmt.toSExpr(env, ast);

            statements_node.appendNode(env.gpa, &stmt_node);
        }

        block_node.appendNode(env.gpa, &statements_node);

        return block_node;
    }
};

/// Represents a Pattern used in pattern matching.
pub const Pattern = union(enum) {
    ident: struct {
        ident_tok: Token.Idx,
        region: TokenizedRegion,
    },
    tag: struct {
        tag_tok: Token.Idx,
        args: Pattern.Span,
        region: TokenizedRegion,
    },
    number: struct {
        number_tok: Token.Idx,
        region: TokenizedRegion,
    },
    string: struct {
        string_tok: Token.Idx,
        region: TokenizedRegion,
        expr: Expr.Idx,
    },
    record: struct {
        fields: PatternRecordField.Span,
        region: TokenizedRegion,
    },
    list: struct {
        patterns: Pattern.Span,
        region: TokenizedRegion,
    },
    list_rest: struct {
        name: ?Token.Idx,
        region: TokenizedRegion,
    },
    tuple: struct {
        patterns: Pattern.Span,
        region: TokenizedRegion,
    },
    underscore: struct {
        region: TokenizedRegion,
    },
    alternatives: struct {
        patterns: Pattern.Span,
        region: TokenizedRegion,
    },
    malformed: struct {
        reason: Diagnostic.Tag,
        region: TokenizedRegion,
    },

    pub const Idx = enum(u32) { _ };
    pub const Span = struct { span: base.DataSpan };

    pub fn to_tokenized_region(self: @This()) TokenizedRegion {
        return switch (self) {
            .ident => |p| p.region,
            .tag => |p| p.region,
            .number => |p| p.region,
            .string => |p| p.region,
            .record => |p| p.region,
            .list => |p| p.region,
            .list_rest => |p| p.region,
            .tuple => |p| p.region,
            .underscore => |p| p.region,
            .alternatives => |p| p.region,
            .malformed => |p| p.region,
        };
    }

    pub fn toSExpr(self: @This(), env: *base.ModuleEnv, ast: *AST) sexpr.Expr {
        switch (self) {
            .ident => |ident| {
                var node = sexpr.Expr.init(env.gpa, "ident");

                node.appendRegionInfo(env.gpa, ast.calcRegionInfo(ident.region, env.line_starts.items));

                node.appendString(env.gpa, ast.resolve(ident.ident_tok));

                return node;
            },
            .tag => |tag| {
                var node = sexpr.Expr.init(env.gpa, "tag");

                node.appendRegionInfo(env.gpa, ast.calcRegionInfo(tag.region, env.line_starts.items));

                node.appendString(env.gpa, ast.resolve(tag.tag_tok));

                // Add arguments if there are any
                for (ast.store.patternSlice(tag.args)) |arg| {
                    var arg_node = ast.store.getPattern(arg).toSExpr(env, ast);
                    node.appendNode(env.gpa, &arg_node);
                }

                return node;
            },
            .number => |num| {
                var node = sexpr.Expr.init(env.gpa, "number");
                node.appendRegionInfo(env.gpa, ast.calcRegionInfo(num.region, env.line_starts.items));
                node.appendString(env.gpa, ast.resolve(num.number_tok));
                return node;
            },
            .string => |str| {
                var node = sexpr.Expr.init(env.gpa, "string");
                node.appendRegionInfo(env.gpa, ast.calcRegionInfo(str.region, env.line_starts.items));
                node.appendString(env.gpa, ast.resolve(str.string_tok));
                return node;
            },
            .record => |rec| {
                var node = sexpr.Expr.init(env.gpa, "record");
                node.appendRegionInfo(env.gpa, ast.calcRegionInfo(rec.region, env.line_starts.items));

                for (ast.store.patternRecordFieldSlice(rec.fields)) |field_idx| {
                    const field = ast.store.getPatternRecordField(field_idx);
                    var field_node = sexpr.Expr.init(env.gpa, "field");
                    field_node.appendRegionInfo(env.gpa, ast.calcRegionInfo(field.region, env.line_starts.items));
                    field_node.appendString(env.gpa, ast.resolve(field.name));

                    if (field.value) |value| {
                        var value_node = ast.store.getPattern(value).toSExpr(env, ast);
                        field_node.appendNode(env.gpa, &value_node);
                    }

                    if (field.rest) {
                        field_node.appendString(env.gpa, "rest");
                    }

                    node.appendNode(env.gpa, &field_node);
                }

                return node;
            },
            .list => |list| {
                var node = sexpr.Expr.init(env.gpa, "list");
                node.appendRegionInfo(env.gpa, ast.calcRegionInfo(list.region, env.line_starts.items));

                for (ast.store.patternSlice(list.patterns)) |pat| {
                    var pattern_node = ast.store.getPattern(pat).toSExpr(env, ast);
                    node.appendNode(env.gpa, &pattern_node);
                }

                return node;
            },
            .list_rest => |rest| {
                var node = sexpr.Expr.init(env.gpa, "list_rest");
                node.appendRegionInfo(env.gpa, ast.calcRegionInfo(rest.region, env.line_starts.items));

                if (rest.name) |name_tok| {
                    node.appendString(env.gpa, ast.resolve(name_tok));
                }

                return node;
            },
            .tuple => |tuple| {
                var node = sexpr.Expr.init(env.gpa, "tuple");
                node.appendRegionInfo(env.gpa, ast.calcRegionInfo(tuple.region, env.line_starts.items));

                for (ast.store.patternSlice(tuple.patterns)) |pat| {
                    var pattern_node = ast.store.getPattern(pat).toSExpr(env, ast);
                    node.appendNode(env.gpa, &pattern_node);
                }

                return node;
            },
            .underscore => {
                return sexpr.Expr.init(env.gpa, "underscore");
            },
            .alternatives => |a| {
                // '|' separated list of patterns
                var node = sexpr.Expr.init(env.gpa, "alternatives");
                for (ast.store.patternSlice(a.patterns)) |pat| {
                    var patNode = ast.store.getPattern(pat).toSExpr(env, ast);
                    node.appendNode(env.gpa, &patNode);
                }
                return node;
            },
            .malformed => |a| {
                var node = sexpr.Expr.init(env.gpa, "malformed_pattern");
                node.appendRegionInfo(env.gpa, ast.calcRegionInfo(a.region, env.line_starts.items));
                node.appendString(env.gpa, @tagName(a.reason));
                return node;
            },
        }
    }
};

/// TODO
pub const BinOp = struct {
    left: Expr.Idx,
    right: Expr.Idx,
    operator: Token.Idx,
    region: TokenizedRegion,

    /// (binop <op> <left> <right>) e.g. (binop '+' 1 2)
    pub fn toSExpr(self: *const @This(), env: *base.ModuleEnv, ast: *AST) sexpr.Expr {
        var node = sexpr.Expr.init(env.gpa, "binop");
        node.appendRegionInfo(env.gpa, ast.calcRegionInfo(self.region, env.line_starts.items));
        node.appendString(env.gpa, ast.resolve(self.operator));

        var left = ast.store.getExpr(self.left).toSExpr(env, ast);
        node.appendNode(env.gpa, &left);

        var right = ast.store.getExpr(self.right).toSExpr(env, ast);
        node.appendNode(env.gpa, &right);
        return node;
    }
};

/// TODO
pub const Unary = struct {
    operator: Token.Idx,
    expr: Expr.Idx,
    region: TokenizedRegion,

    pub fn toSExpr(self: *const @This(), env: *base.ModuleEnv, ast: *AST) sexpr.Expr {
        var node = sexpr.Expr.init(env.gpa, "unary");
        node.appendRegionInfo(env.gpa, ast.calcRegionInfo(self.region, env.line_starts.items));
        node.appendString(env.gpa, ast.resolve(self.operator));

        var expr = ast.store.getExpr(self.expr).toSExpr(env, ast);
        node.appendNode(env.gpa, &expr);

        return node;
    }
};

/// Represents a delimited collection of other nodes
pub const Collection = struct {
    span: base.DataSpan,
    region: TokenizedRegion,

    pub const Idx = enum(u32) { _ };
};

/// Represents a Roc file.
pub const File = struct {
    header: Header.Idx,
    statements: Statement.Span,
    region: TokenizedRegion,

    pub fn toSExpr(self: @This(), env: *base.ModuleEnv, ast: *AST) sexpr.Expr {
        var file_node = sexpr.Expr.init(env.gpa, "file");

        file_node.appendRegionInfo(env.gpa, ast.calcRegionInfo(self.region, env.line_starts.items));

        const header = ast.store.getHeader(self.header);
        var header_node = header.toSExpr(env, ast);

        file_node.appendNode(env.gpa, &header_node);

        var statements_node = sexpr.Expr.init(env.gpa, "statements");

        for (ast.store.statementSlice(self.statements)) |stmt_id| {
            const stmt = ast.store.getStatement(stmt_id);
            var stmt_node = stmt.toSExpr(env, ast);
            statements_node.appendNode(env.gpa, &stmt_node);
        }

        file_node.appendNode(env.gpa, &statements_node);

        return file_node;
    }
};

/// Represents a module header.
pub const Header = union(enum) {
    app: struct {
        provides: Collection.Idx,
        platform_idx: RecordField.Idx,
        packages: Collection.Idx,
        region: TokenizedRegion,
    },
    module: struct {
        exposes: Collection.Idx,
        region: TokenizedRegion,
    },
    package: struct {
        exposes: Collection.Idx,
        packages: Collection.Idx,
        region: TokenizedRegion,
    },
    platform: struct {
        // TODO: complete this
        name: Token.Idx,
        requires_rigids: Collection.Idx,
        requires_signatures: TypeAnno.Idx,
        exposes: Collection.Idx,
        packages: Collection.Idx,
        provides: Collection.Idx,
        region: TokenizedRegion,
    },
    hosted: struct {
        exposes: Collection.Idx,
        region: TokenizedRegion,
    },
    malformed: struct {
        reason: Diagnostic.Tag,
        region: TokenizedRegion,
    },

    pub const Idx = enum(u32) { _ };

    pub const AppHeaderRhs = packed struct { num_packages: u10, num_provides: u22 };

    pub fn toSExpr(self: @This(), env: *base.ModuleEnv, ast: *AST) sexpr.Expr {
        switch (self) {
            .app => |a| {
                var node = sexpr.Expr.init(env.gpa, "app");
                node.appendRegionInfo(env.gpa, ast.calcRegionInfo(a.region, env.line_starts.items));
                // Provides
                const provides_coll = ast.store.getCollection(a.provides);
                const provides_items = ast.store.exposedItemSlice(.{ .span = provides_coll.span });
                var provides_node = sexpr.Expr.init(env.gpa, "provides");
                provides_node.appendRegionInfo(env.gpa, ast.calcRegionInfo(provides_coll.region, env.line_starts.items));
                for (provides_items) |item_idx| {
                    const item = ast.store.getExposedItem(item_idx);
                    var item_node = item.toSExpr(env, ast);
                    provides_node.appendNode(env.gpa, &item_node);
                }
                node.appendNode(env.gpa, &provides_node);
                // Platform
                const platform = ast.store.getRecordField(a.platform_idx);
                var platform_node = platform.toSExpr(env, ast);
                node.appendNode(env.gpa, &platform_node);
                // Packages
                const packages_coll = ast.store.getCollection(a.packages);
                const packages_items = ast.store.recordFieldSlice(.{ .span = packages_coll.span });
                var packages_node = sexpr.Expr.init(env.gpa, "packages");
                packages_node.appendRegionInfo(env.gpa, ast.calcRegionInfo(packages_coll.region, env.line_starts.items));
                for (packages_items) |item_idx| {
                    const item = ast.store.getRecordField(item_idx);
                    var item_node = item.toSExpr(env, ast);
                    packages_node.appendNode(env.gpa, &item_node);
                }
                node.appendNode(env.gpa, &packages_node);
                return node;
            },
            .module => |module| {
                var node = sexpr.Expr.init(env.gpa, "module");
                node.appendRegionInfo(env.gpa, ast.calcRegionInfo(module.region, env.line_starts.items));
                const exposes = ast.store.getCollection(module.exposes);
                var exposes_node = sexpr.Expr.init(env.gpa, "exposes");
                exposes_node.appendRegionInfo(env.gpa, ast.calcRegionInfo(exposes.region, env.line_starts.items));
                for (ast.store.exposedItemSlice(.{ .span = exposes.span })) |exposed| {
                    const item = ast.store.getExposedItem(exposed);
                    var item_node = item.toSExpr(env, ast);
                    exposes_node.appendNode(env.gpa, &item_node);
                }
                node.appendNode(env.gpa, &exposes_node);
                return node;
            },
            .package => |a| {
                var node = sexpr.Expr.init(env.gpa, "package");
                node.appendRegionInfo(env.gpa, ast.calcRegionInfo(a.region, env.line_starts.items));
                // Exposes
                const exposes = ast.store.getCollection(a.exposes);
                var exposes_node = sexpr.Expr.init(env.gpa, "exposes");
                exposes_node.appendRegionInfo(env.gpa, ast.calcRegionInfo(exposes.region, env.line_starts.items));
                for (ast.store.exposedItemSlice(.{ .span = exposes.span })) |exposed| {
                    const item = ast.store.getExposedItem(exposed);
                    var item_node = item.toSExpr(env, ast);
                    exposes_node.appendNode(env.gpa, &item_node);
                }
                node.appendNode(env.gpa, &exposes_node);
                // Packages
                const packages_coll = ast.store.getCollection(a.packages);
                const packages_items = ast.store.recordFieldSlice(.{ .span = packages_coll.span });
                var packages_node = sexpr.Expr.init(env.gpa, "packages");
                packages_node.appendRegionInfo(env.gpa, ast.calcRegionInfo(packages_coll.region, env.line_starts.items));
                for (packages_items) |item_idx| {
                    const item = ast.store.getRecordField(item_idx);
                    var item_node = item.toSExpr(env, ast);
                    packages_node.appendNode(env.gpa, &item_node);
                }
                node.appendNode(env.gpa, &packages_node);
                return node;
            },
            .platform => |a| {
                var node = sexpr.Expr.init(env.gpa, "platform");
                node.appendRegionInfo(env.gpa, ast.calcRegionInfo(a.region, env.line_starts.items));
                // Name
                node.appendString(env.gpa, ast.resolve(a.name));
                // Requires Rigids
                const rigids = ast.store.getCollection(a.requires_rigids);
                var rigids_node = sexpr.Expr.init(env.gpa, "rigids");
                rigids_node.appendRegionInfo(env.gpa, ast.calcRegionInfo(rigids.region, env.line_starts.items));
                for (ast.store.exposedItemSlice(.{ .span = rigids.span })) |exposed| {
                    const item = ast.store.getExposedItem(exposed);
                    var item_node = item.toSExpr(env, ast);
                    rigids_node.appendNode(env.gpa, &item_node);
                }
                node.appendNode(env.gpa, &rigids_node);
                // Requires Signatures
                const signatures = ast.store.getTypeAnno(a.requires_signatures);
                var signatures_node = signatures.toSExpr(env, ast);
                node.appendNode(env.gpa, &signatures_node);
                // Exposes
                const exposes = ast.store.getCollection(a.exposes);
                var exposes_node = sexpr.Expr.init(env.gpa, "exposes");
                exposes_node.appendRegionInfo(env.gpa, ast.calcRegionInfo(exposes.region, env.line_starts.items));
                for (ast.store.exposedItemSlice(.{ .span = exposes.span })) |exposed| {
                    const item = ast.store.getExposedItem(exposed);
                    var item_node = item.toSExpr(env, ast);
                    exposes_node.appendNode(env.gpa, &item_node);
                }
                node.appendNode(env.gpa, &exposes_node);
                // Packages
                const packages_coll = ast.store.getCollection(a.packages);
                const packages_items = ast.store.recordFieldSlice(.{ .span = packages_coll.span });
                var packages_node = sexpr.Expr.init(env.gpa, "packages");
                packages_node.appendRegionInfo(env.gpa, ast.calcRegionInfo(packages_coll.region, env.line_starts.items));
                for (packages_items) |item_idx| {
                    const item = ast.store.getRecordField(item_idx);
                    var item_node = item.toSExpr(env, ast);
                    packages_node.appendNode(env.gpa, &item_node);
                }
                node.appendNode(env.gpa, &packages_node);
                // Provides
                const provides = ast.store.getCollection(a.provides);
                var provides_node = sexpr.Expr.init(env.gpa, "provides");
                provides_node.appendRegionInfo(env.gpa, ast.calcRegionInfo(provides.region, env.line_starts.items));
                for (ast.store.exposedItemSlice(.{ .span = provides.span })) |exposed| {
                    const item = ast.store.getExposedItem(exposed);
                    var item_node = item.toSExpr(env, ast);
                    provides_node.appendNode(env.gpa, &item_node);
                }
                node.appendNode(env.gpa, &provides_node);
                return node;
            },
            .hosted => |a| {
                var node = sexpr.Expr.init(env.gpa, "hosted");
                node.appendRegionInfo(env.gpa, ast.calcRegionInfo(a.region, env.line_starts.items));
                const exposes = ast.store.getCollection(a.exposes);
                var exposes_node = sexpr.Expr.init(env.gpa, "exposes");
                exposes_node.appendRegionInfo(env.gpa, ast.calcRegionInfo(exposes.region, env.line_starts.items));
                for (ast.store.exposedItemSlice(.{ .span = exposes.span })) |exposed| {
                    const item = ast.store.getExposedItem(exposed);
                    var item_node = item.toSExpr(env, ast);
                    exposes_node.appendNode(env.gpa, &item_node);
                }
                node.appendNode(env.gpa, &exposes_node);
                return node;
            },
            .malformed => |a| {
                var node = sexpr.Expr.init(env.gpa, "malformed_header");
                node.appendRegionInfo(env.gpa, ast.calcRegionInfo(a.region, env.line_starts.items));
                node.appendString(env.gpa, @tagName(a.reason));
                return node;
            },
        }
    }
};

/// TODO
pub const ExposedItem = union(enum) {
    lower_ident: struct {
        as: ?Token.Idx,
        ident: Token.Idx,
        region: TokenizedRegion,
    },
    upper_ident: struct {
        as: ?Token.Idx,
        ident: Token.Idx,
        region: TokenizedRegion,
    },
    upper_ident_star: struct {
        ident: Token.Idx,
        region: TokenizedRegion,
    },

    pub const Idx = enum(u32) { _ };
    pub const Span = struct { span: base.DataSpan };

    pub fn toSExpr(self: @This(), env: *base.ModuleEnv, ast: *AST) sexpr.Expr {
        _ = env.line_starts.items;
        var node = sexpr.Expr.init(env.gpa, "exposed_item");
        var inner_node = sexpr.Expr.init(env.gpa, @tagName(self));
        switch (self) {
            .lower_ident => |i| {
                const token = ast.tokens.tokens.get(i.ident);
                const text = env.idents.getText(token.extra.interned);
                inner_node.appendString(env.gpa, text);
                if (i.as) |a| {
                    const as_tok = ast.tokens.tokens.get(a);
                    const as_text = env.idents.getText(as_tok.extra.interned);
                    inner_node.appendString(env.gpa, as_text);
                }
            },
            .upper_ident => |i| {
                const token = ast.tokens.tokens.get(i.ident);
                const text = env.idents.getText(token.extra.interned);
                inner_node.appendString(env.gpa, text);
                if (i.as) |a| {
                    const as_tok = ast.tokens.tokens.get(a);
                    const as_text = env.idents.getText(as_tok.extra.interned);
                    inner_node.appendString(env.gpa, as_text);
                }
            },
            .upper_ident_star => |i| {
                const token = ast.tokens.tokens.get(i.ident);
                const text = env.idents.getText(token.extra.interned);
                inner_node.appendString(env.gpa, text);
            },
        }
        node.appendNode(env.gpa, &inner_node);
        return node;
    }
};

/// TODO
pub const TypeHeader = struct {
    name: Token.Idx,
    args: TypeAnno.Span,
    region: TokenizedRegion,

    pub const Idx = enum(u32) { _ };
};

/// TODO
pub const TypeAnno = union(enum) {
    apply: struct {
        args: TypeAnno.Span,
        region: TokenizedRegion,
    },
    ty_var: struct {
        tok: Token.Idx,
        region: TokenizedRegion,
    },
    underscore: struct {
        region: TokenizedRegion,
    },
    ty: struct {
        ident: base.Ident.Idx,
        // Region starts with the type token.
        region: TokenizedRegion,
    },
    mod_ty: struct {
        mod_ident: base.Ident.Idx,
        ty_ident: base.Ident.Idx,
        // Region starts with the mod token and ends with the type token.
        region: TokenizedRegion,
    },
    tag_union: struct {
        tags: TypeAnno.Span,
        open_anno: ?TypeAnno.Idx,
        region: TokenizedRegion,
    },
    tuple: struct {
        annos: TypeAnno.Span,
        region: TokenizedRegion,
    },
    record: struct {
        fields: AnnoRecordField.Span,
        region: TokenizedRegion,
    },
    @"fn": struct {
        args: TypeAnno.Span,
        ret: TypeAnno.Idx,
        effectful: bool,
        region: TokenizedRegion,
    },
    parens: struct {
        anno: TypeAnno.Idx,
        region: TokenizedRegion,
    },
    malformed: struct {
        reason: Diagnostic.Tag,
        region: TokenizedRegion,
    },

    pub const Idx = enum(u32) { _ };
    pub const Span = struct { span: base.DataSpan };

    pub const TagUnionRhs = packed struct { open: u1, tags_len: u31 };
    pub const TypeAnnoFnRhs = packed struct { effectful: u1, args_len: u31 };

    pub fn toSExpr(self: @This(), env: *base.ModuleEnv, ast: *AST) sexpr.Expr {
        switch (self) {
            // (apply <ty> [<args>])
            .apply => |a| {
                var node = sexpr.Expr.init(env.gpa, "apply");

                node.appendRegionInfo(env.gpa, ast.calcRegionInfo(a.region, env.line_starts.items));

                for (ast.store.typeAnnoSlice(a.args)) |b| {
                    var child = ast.store.getTypeAnno(b).toSExpr(env, ast);
                    node.appendNode(env.gpa, &child);
                }

                return node;
            },
            // (ty_var <var>)
            .ty_var => |a| {
                var node = sexpr.Expr.init(env.gpa, "ty_var");

                node.appendRegionInfo(env.gpa, ast.calcRegionInfo(a.region, env.line_starts.items));

                node.appendString(env.gpa, ast.resolve(a.tok));
                return node;
            },
            // (_)
            .underscore => {
                return sexpr.Expr.init(env.gpa, "_");
            },
            // (ty name)
            .ty => |a| {
                var node = sexpr.Expr.init(env.gpa, "ty");
                node.appendString(env.gpa, ast.resolve(a.region.start));
                return node;
            },
            // (mod_ty mod ty)
            .mod_ty => |a| {
                var node = sexpr.Expr.init(env.gpa, "mod_ty");
                node.appendString(env.gpa, ast.resolve(a.region.start));
                node.appendString(env.gpa, ast.resolve(a.region.end));
                return node;
            },
            .tag_union => |a| {
                var node = sexpr.Expr.init(env.gpa, "tag_union");

                node.appendRegionInfo(env.gpa, ast.calcRegionInfo(a.region, env.line_starts.items));

                const tags = ast.store.typeAnnoSlice(a.tags);
                var tags_node = sexpr.Expr.init(env.gpa, "tags");
                for (tags) |tag_idx| {
                    const tag = ast.store.getTypeAnno(tag_idx);
                    var tag_node = tag.toSExpr(env, ast);
                    tags_node.appendNode(env.gpa, &tag_node);
                }
                node.appendNode(env.gpa, &tags_node);
                if (a.open_anno) |anno_idx| {
                    const anno = ast.store.getTypeAnno(anno_idx);
                    var anno_node = anno.toSExpr(env, ast);
                    node.appendNode(env.gpa, &anno_node);
                }
                return node;
            },
            // (tuple [<elems>])
            .tuple => |a| {
                var node = sexpr.Expr.init(env.gpa, "tuple");

                node.appendRegionInfo(env.gpa, ast.calcRegionInfo(a.region, env.line_starts.items));

                for (ast.store.typeAnnoSlice(a.annos)) |b| {
                    var child = ast.store.getTypeAnno(b).toSExpr(env, ast);
                    node.appendNode(env.gpa, &child);
                }
                return node;
            },
            // (record [<fields>])
            .record => |a| {
                var node = sexpr.Expr.init(env.gpa, "record");

                node.appendRegionInfo(env.gpa, ast.calcRegionInfo(a.region, env.line_starts.items));

                for (ast.store.annoRecordFieldSlice(a.fields)) |f_idx| {
                    const field = ast.store.getAnnoRecordField(f_idx);
                    var field_node = field.toSExpr(env, ast);
                    node.appendNode(env.gpa, &field_node);
                }
                return node;
            },
            // (fn <ret> [<args>])
            .@"fn" => |a| {
                var node = sexpr.Expr.init(env.gpa, "fn");

                node.appendRegionInfo(env.gpa, ast.calcRegionInfo(a.region, env.line_starts.items));

                // arguments
                for (ast.store.typeAnnoSlice(a.args)) |b| {
                    var child = ast.store.getTypeAnno(b).toSExpr(env, ast);
                    node.appendNode(env.gpa, &child);
                }

                // return value
                var ret = ast.store.getTypeAnno(a.ret).toSExpr(env, ast);
                node.appendNode(env.gpa, &ret);

                return node;
            },
            // ignore parens... use inner
            .parens => |a| {
                return ast.store.getTypeAnno(a.anno).toSExpr(env, ast);
            },
            .malformed => |a| {
                var node = sexpr.Expr.init(env.gpa, "malformed_expr");
                node.appendRegionInfo(env.gpa, ast.calcRegionInfo(a.region, env.line_starts.items));
                node.appendString(env.gpa, @tagName(a.reason));
                return node;
            },
        }
    }
};

/// TODO
pub const AnnoRecordField = struct {
    name: Token.Idx,
    ty: TypeAnno.Idx,
    region: TokenizedRegion,

    pub const Idx = enum(u32) { _ };
    pub const Span = struct { span: base.DataSpan };

    pub fn toSExpr(self: @This(), env: *base.ModuleEnv, ast: *AST) sexpr.Expr {
        var node = sexpr.Expr.init(env.gpa, "anno_record_field");
        node.appendRegionInfo(env.gpa, ast.calcRegionInfo(self.region, env.line_starts.items));
        node.appendString(env.gpa, ast.resolve(self.name));
        const anno = ast.store.getTypeAnno(self.ty);
        var ty_node = anno.toSExpr(env, ast);
        node.appendNode(env.gpa, &ty_node);
        return node;
    }
};

/// The clause of a `where` constraint
///
/// e.g. `a.hash(hasher) -> hasher`
/// or   `a.Hash`
pub const WhereClause = union(enum) {
    alias: struct {
        var_tok: Token.Idx,
        alias_tok: Token.Idx,
        region: TokenizedRegion,
    },
    method: struct {
        var_tok: Token.Idx,
        name_tok: Token.Idx,
        args: Collection.Idx,
        ret_anno: TypeAnno.Idx,
        region: TokenizedRegion,
    },
    mod_method: struct {
        var_tok: Token.Idx,
        name_tok: Token.Idx,
        args: Collection.Idx,
        ret_anno: TypeAnno.Idx,
        region: TokenizedRegion,
    },
    malformed: struct {
        reason: Diagnostic.Tag,
        region: TokenizedRegion,
    },

    pub const Idx = enum(u32) { _ };
    pub const Span = struct { span: base.DataSpan };
};

/// Represents an expression.
pub const Expr = union(enum) {
    int: struct {
        token: Token.Idx,
        region: TokenizedRegion,
    },
    frac: struct {
        token: Token.Idx,
        region: TokenizedRegion,
    },
    string_part: struct { // TODO: this should be more properly represented in its own union enum
        token: Token.Idx,
        region: TokenizedRegion,
    },
    string: struct {
        token: Token.Idx,
        region: TokenizedRegion,
        parts: Expr.Span,
    },
    list: struct {
        items: Expr.Span,
        region: TokenizedRegion,
    },
    tuple: struct {
        items: Expr.Span,
        region: TokenizedRegion,
    },
    record: struct {
        fields: RecordField.Span,
        region: TokenizedRegion,
    },
    tag: struct {
        token: Token.Idx,
        region: TokenizedRegion,
    },
    lambda: struct {
        args: Pattern.Span,
        body: Expr.Idx,
        region: TokenizedRegion,
    },
    apply: struct {
        args: Expr.Span,
        @"fn": Expr.Idx,
        region: TokenizedRegion,
    },
    record_updater: struct {
        token: Token.Idx,
        region: TokenizedRegion,
    },
    field_access: BinOp,
    local_dispatch: BinOp,
    bin_op: BinOp,
    suffix_single_question: Unary,
    unary_op: Unary,
    if_then_else: struct {
        condition: Expr.Idx,
        then: Expr.Idx,
        @"else": Expr.Idx,
        region: TokenizedRegion,
    },
    match: struct {
        expr: Expr.Idx,
        branches: WhenBranch.Span,
        region: TokenizedRegion,
    },
    ident: struct {
        token: Token.Idx,
        qualifier: ?Token.Idx,
        region: TokenizedRegion,
    },
    dbg: struct {
        expr: Expr.Idx,
        region: TokenizedRegion,
    },
    record_builder: struct {
        mapper: Expr.Idx,
        fields: RecordField.Idx,
        region: TokenizedRegion,
    },
    ellipsis: struct {
        region: TokenizedRegion,
    },
    block: Body,
    malformed: struct {
        reason: Diagnostic.Tag,
        region: TokenizedRegion,
    },

    pub const Idx = enum(u32) { _ };
    pub const Span = struct { span: base.DataSpan };

    pub fn as_string_part_region(self: @This()) !TokenizedRegion {
        switch (self) {
            .string_part => |part| return part.region,
            else => return error.ExpectedStringPartRegion,
        }
    }

    pub fn to_tokenized_region(self: @This()) TokenizedRegion {
        return switch (self) {
            .ident => |e| e.region,
            .int => |e| e.region,
            .float => |e| e.region,
            .string => |e| e.region,
            .tag => |e| e.region,
            .list => |e| e.region,
            .record => |e| e.region,
            .tuple => |e| e.region,
            .field_access => |e| e.region,
            .local_dispatch => |e| e.region,
            .lambda => |e| e.region,
            .record_updater => |e| e.region,
            .bin_op => |e| e.region,
            .unary_op => |e| e.region,
            .suffix_single_question => |e| e.region,
            .apply => |e| e.region,
            .if_then_else => |e| e.region,
            .match => |e| e.region,
            .dbg => |e| e.region,
            .block => |e| e.region,
            .record_builder => |e| e.region,
            .ellipsis => |e| e.region,
            .malformed => |e| e.region,
            .string_part => |e| e.region,
        };
    }

    pub fn toSExpr(self: @This(), env: *base.ModuleEnv, ast: *AST) sexpr.Expr {
        switch (self) {
            .int => |int| {
                var node = sexpr.Expr.init(env.gpa, "int");
                node.appendRegionInfo(env.gpa, ast.calcRegionInfo(int.region, env.line_starts.items));
                node.appendString(env.gpa, ast.resolve(int.token));
                return node;
            },
            .string => |str| {
                var node = sexpr.Expr.init(env.gpa, "string");
                node.appendRegionInfo(env.gpa, ast.calcRegionInfo(str.region, env.line_starts.items));
                for (ast.store.exprSlice(str.parts)) |part_id| {
                    const part_expr = ast.store.getExpr(part_id);
                    var part_sexpr = part_expr.toSExpr(env, ast);
                    node.appendNode(env.gpa, &part_sexpr);
                }
                return node;
            },
            .string_part => |sp| {
                var node = sexpr.Expr.init(env.gpa, "string_part");
                node.appendRegionInfo(env.gpa, ast.calcRegionInfo(sp.region, env.line_starts.items));
                node.appendString(env.gpa, ast.resolve(sp.token));
                return node;
            },
            // (tag <tag>)
            .tag => |tag| {
                var node = sexpr.Expr.init(env.gpa, "tag");

                node.appendRegionInfo(env.gpa, ast.calcRegionInfo(tag.region, env.line_starts.items));

                node.appendString(env.gpa, ast.resolve(tag.token));
                return node;
            },
            .block => |block| {
                return block.toSExpr(env, ast);
            },
            // (if_then_else <condition> <then> <else>)
            .if_then_else => |stmt| {
                var node = sexpr.Expr.init(env.gpa, "if_then_else");

                node.appendRegionInfo(env.gpa, ast.calcRegionInfo(stmt.region, env.line_starts.items));

                var condition = ast.store.getExpr(stmt.condition).toSExpr(env, ast);
                var then = ast.store.getExpr(stmt.then).toSExpr(env, ast);
                var else_ = ast.store.getExpr(stmt.@"else").toSExpr(env, ast);

                node.appendNode(env.gpa, &condition);
                node.appendNode(env.gpa, &then);
                node.appendNode(env.gpa, &else_);

                return node;
            },
            .ident => |ident| {
                var node = sexpr.Expr.init(env.gpa, "ident");

                node.appendRegionInfo(env.gpa, ast.calcRegionInfo(ident.region, env.line_starts.items));

                node.appendString(env.gpa, if (ident.qualifier != null) ast.resolve(ident.qualifier.?) else "");
                node.appendString(env.gpa, ast.resolve(ident.token));
                return node;
            },
            // (list [<child>])
            .list => |a| {
                var node = sexpr.Expr.init(env.gpa, "list");

                node.appendRegionInfo(env.gpa, ast.calcRegionInfo(a.region, env.line_starts.items));

                for (ast.store.exprSlice(a.items)) |b| {
                    var child = ast.store.getExpr(b).toSExpr(env, ast);
                    node.appendNode(env.gpa, &child);
                }
                return node;
            },
            // (malformed_expr <reason>)
            .malformed => |a| {
                var node = sexpr.Expr.init(env.gpa, "malformed_expr");
                node.appendRegionInfo(env.gpa, ast.calcRegionInfo(a.region, env.line_starts.items));
                node.appendString(env.gpa, @tagName(a.reason));
                return node;
            },
            // (frac <value>)
            .frac => |a| {
                var node = sexpr.Expr.init(env.gpa, "frac");

                node.appendRegionInfo(env.gpa, ast.calcRegionInfo(a.region, env.line_starts.items));

                node.appendString(env.gpa, ast.resolve(a.token));
                return node;
            },
            // (tuple [<item>])
            .tuple => |a| {
                var node = sexpr.Expr.init(env.gpa, "tuple");

                node.appendRegionInfo(env.gpa, ast.calcRegionInfo(a.region, env.line_starts.items));

                for (ast.store.exprSlice(a.items)) |item| {
                    var child = ast.store.getExpr(item).toSExpr(env, ast);
                    node.appendNode(env.gpa, &child);
                }

                return node;
            },
            // (record [(field <name> <?value> ?optional)])
            .record => |a| {
                var node = sexpr.Expr.init(env.gpa, "record");

                node.appendRegionInfo(env.gpa, ast.calcRegionInfo(a.region, env.line_starts.items));

                for (ast.store.recordFieldSlice(a.fields)) |field_idx| {
                    const record_field = ast.store.getRecordField(field_idx);
                    var record_field_node = sexpr.Expr.init(env.gpa, "field");
                    record_field_node.appendString(env.gpa, ast.resolve(record_field.name));
                    if (record_field.value != null) {
                        var value_node = ast.store.getExpr(record_field.value.?).toSExpr(env, ast);
                        record_field_node.appendNode(env.gpa, &value_node);
                    }
                    if (record_field.optional) {
                        record_field_node.appendString(env.gpa, "optional");
                    }
                    node.appendNode(env.gpa, &record_field_node);
                }

                return node;
            },
            // (apply <fn> [<args>])
            .apply => |a| {
                var node = sexpr.Expr.init(env.gpa, "apply");

                node.appendRegionInfo(env.gpa, ast.calcRegionInfo(a.region, env.line_starts.items));

                var apply_fn = ast.store.getExpr(a.@"fn").toSExpr(env, ast);
                node.appendNode(env.gpa, &apply_fn);

                for (ast.store.exprSlice(a.args)) |arg| {
                    var arg_node = ast.store.getExpr(arg).toSExpr(env, ast);
                    node.appendNode(env.gpa, &arg_node);
                }

                return node;
            },
            .field_access => |a| {
                var node = sexpr.Expr.init(env.gpa, "field_access");

                node.appendRegionInfo(env.gpa, ast.calcRegionInfo(a.region, env.line_starts.items));

                var child = a.toSExpr(env, ast);
                node.appendNode(env.gpa, &child);
                return node;
            },
            .local_dispatch => |a| {
                var node = sexpr.Expr.init(env.gpa, "local_dispatch");
                node.appendRegionInfo(env.gpa, ast.calcRegionInfo(a.region, env.line_starts.items));

                var left = ast.store.getExpr(a.left).toSExpr(env, ast);
                var right = ast.store.getExpr(a.right).toSExpr(env, ast);
                node.appendNode(env.gpa, &left);
                node.appendNode(env.gpa, &right);
                return node;
            },
            // (binop <op> <lhs> <rhs>)
            .bin_op => |a| {
                return a.toSExpr(env, ast);
            },
            .lambda => |a| {
                var node = sexpr.Expr.init(env.gpa, "lambda");

                node.appendRegionInfo(env.gpa, ast.calcRegionInfo(a.region, env.line_starts.items));

                // arguments
                var args = sexpr.Expr.init(env.gpa, "args");
                for (ast.store.patternSlice(a.args)) |arg| {
                    var arg_node = ast.store.getPattern(arg).toSExpr(env, ast);
                    args.appendNode(env.gpa, &arg_node);
                }
                node.appendNode(env.gpa, &args);

                // body
                var body = ast.store.getExpr(a.body).toSExpr(env, ast);
                node.appendNode(env.gpa, &body);

                return node;
            },
            .dbg => |a| {
                var node = sexpr.Expr.init(env.gpa, "dbg");

                var arg = ast.store.getExpr(a.expr).toSExpr(env, ast);
                node.appendNode(env.gpa, &arg);

                return node;
            },
            .match => |a| {
                var node = sexpr.Expr.init(env.gpa, "match");

                var expr = ast.store.getExpr(a.expr).toSExpr(env, ast);

                // handle branches
                var branches = sexpr.Expr.init(env.gpa, "branches");
                for (ast.store.whenBranchSlice(a.branches)) |branch| {
                    var branch_node = ast.store.getBranch(branch).toSExpr(env, ast);
                    branches.appendNode(env.gpa, &branch_node);
                }

                node.appendNode(env.gpa, &expr);

                node.appendNode(env.gpa, &branches);

                return node;
            },
            .ellipsis => {
                return sexpr.Expr.init(env.gpa, "ellipsis");
            },
            .suffix_single_question => |a| {
                var node = sexpr.Expr.init(env.gpa, "suffix_single_question");

                node.appendRegionInfo(env.gpa, ast.calcRegionInfo(a.region, env.line_starts.items));

                var child = ast.store.getExpr(a.expr).toSExpr(env, ast);
                node.appendNode(env.gpa, &child);
                return node;
            },
            else => {
                std.debug.print("\n\n toSExpr not implement for Expr {}\n\n", .{self});
                @panic("not implemented yet");
            },
        }
    }
};

/// TODO
pub const PatternRecordField = struct {
    name: Token.Idx,
    value: ?Pattern.Idx,
    rest: bool,
    region: TokenizedRegion,

    pub const Idx = enum(u32) { _ };
    pub const Span = struct { span: base.DataSpan };
};

/// TODO
pub const RecordField = struct {
    name: Token.Idx,
    value: ?Expr.Idx,
    optional: bool,
    region: TokenizedRegion,

    pub const Idx = enum(u32) { _ };
    pub const Span = struct { span: base.DataSpan };

    pub fn toSExpr(self: @This(), env: *base.ModuleEnv, ast: *AST) sexpr.Expr {
        var node = sexpr.Expr.init(env.gpa, "record_field");
        node.appendRegionInfo(env.gpa, ast.calcRegionInfo(self.region, env.line_starts.items));
        node.appendString(env.gpa, ast.resolve(self.name));
        if (self.value) |idx| {
            const value = ast.store.getExpr(idx);
            var value_node = value.toSExpr(env, ast);
            node.appendNode(env.gpa, &value_node);
        }
        return node;
    }
};

/// TODO
pub const IfElse = struct {
    condition: Expr.Idx,
    body: Expr.Idx,
    region: TokenizedRegion,

    pub const Idx = enum(u32) { _ };
    pub const Span = struct { span: base.DataSpan };
};

/// TODO
pub const WhenBranch = struct {
    pattern: Pattern.Idx,
    body: Expr.Idx,
    region: TokenizedRegion,

    pub const Idx = enum(u32) { _ };
    pub const Span = struct { span: base.DataSpan };

    pub fn toSExpr(self: @This(), env: *base.ModuleEnv, ast: *AST) sexpr.Expr {
        var node = sexpr.Expr.init(env.gpa, "branch");
        node.appendRegionInfo(env.gpa, ast.calcRegionInfo(self.region, env.line_starts.items));
        var pattern = ast.store.getPattern(self.pattern).toSExpr(env, ast);
        node.appendNode(env.gpa, &pattern);
        var body = ast.store.getExpr(self.body).toSExpr(env, ast);
        node.appendNode(env.gpa, &body);
        return node;
    }
};
