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
const collections = @import("../../collections.zig");

const Node = @import("Node.zig");
const NodeStore = @import("NodeStore.zig");
const Token = tokenize.Token;
const TokenizedBuffer = tokenize.TokenizedBuffer;
const exitOnOom = @import("../../collections/utils.zig").exitOnOom;

const testing = std.testing;
const Ident = base.Ident;
const Allocator = std.mem.Allocator;

const AST = @This();

source: []const u8,
tokens: TokenizedBuffer,
store: NodeStore,
errors: []const Diagnostic,
root_node_idx: u32 = 0,

/// Calculate whether this region is - or will be - multiline
pub fn regionIsMultiline(self: *AST, region: Region) bool {
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

/// Returns diagnostic position information for the given region.
pub fn regionInfoFromTokenIds(self: *AST, region: Region, line_starts: []const u32) base.DiagnosticPosition {
    const start = self.tokens.resolve(region.start);
    const end = self.tokens.resolve(region.end);
    const info = base.DiagnosticPosition.position(self.source, line_starts, start.start.offset, end.end.offset) catch {
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

pub fn deinit(self: *AST) void {
    defer self.tokens.deinit();
    defer self.store.deinit();
    self.store.gpa.free(self.errors);
}

/// Diagnostics related to parsing
pub const Diagnostic = struct {
    tag: Tag,
    region: Region,

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
pub const Region = struct {
    start: Token.Idx,
    end: Token.Idx,

    pub fn empty() Region {
        return .{ .start = 0, .end = 0 };
    }

    pub fn spanAcross(self: Region, other: Region) Region {
        return .{
            .start = self.start,
            .end = other.end,
        };
    }

    pub fn toBase(self: Region) base.Region {
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

/// Helper function to convert an IR to a string in S-expression format
/// and write it to the given writer.
pub fn toSExprStr(ir: *@This(), env: *base.ModuleEnv, writer: std.io.AnyWriter) !void {
    const file = ir.store.getFile();

    var node = file.toSExpr(env, ir);
    defer node.deinit(env.gpa);

    node.toStringPretty(writer);
}

/// Represents a statement.  Not all statements are valid in all positions.
pub const Statement = union(enum) {
    decl: Decl,
    @"var": struct {
        name: Token.Idx,
        body: Expr.Idx,
        region: Region,
    },
    expr: struct {
        expr: Expr.Idx,
        region: Region,
    },
    crash: struct {
        expr: Expr.Idx,
        region: Region,
    },
    expect: struct {
        body: Expr.Idx,
        region: Region,
    },
    @"for": struct {
        patt: Pattern.Idx,
        expr: Expr.Idx,
        body: Expr.Idx,
        region: Region,
    },
    @"return": struct {
        expr: Expr.Idx,
        region: Region,
    },
    import: struct {
        module_name_tok: Token.Idx,
        qualifier_tok: ?Token.Idx,
        alias_tok: ?Token.Idx,
        exposes: ExposedItem.Span,
        region: Region,
    },
    type_decl: struct {
        header: TypeHeader.Idx,
        anno: TypeAnno.Idx,
        where: ?Collection.Idx,
        region: Region,
    },
    type_anno: struct {
        name: Token.Idx,
        anno: TypeAnno.Idx,
        where: ?Collection.Idx,
        region: Region,
    },
    malformed: struct {
        reason: Diagnostic.Tag,
        region: Region,
    },

    pub const Idx = enum(u32) { _ };
    pub const Span = struct { span: base.DataSpan };

    pub const Decl = struct {
        pattern: Pattern.Idx,
        body: Expr.Idx,
        region: Region,
    };

    pub fn toSExpr(self: @This(), env: *base.ModuleEnv, ir: *AST) sexpr.Expr {
        switch (self) {
            .decl => |decl| {
                var node = sexpr.Expr.init(env.gpa, "decl");
                node.appendRegionChild(env.gpa, ir.regionInfoFromTokenIds(decl.region, env.line_starts.items));
                // pattern
                {
                    const pattern = ir.store.getPattern(decl.pattern);
                    var pattern_node = pattern.toSExpr(env, ir);
                    node.appendNodeChild(env.gpa, &pattern_node);
                }
                // body
                {
                    const body = ir.store.getExpr(decl.body);
                    var body_node = body.toSExpr(env, ir);
                    node.appendNodeChild(env.gpa, &body_node);
                }
                return node;
            },
            .@"var" => |v| {
                var node = sexpr.Expr.init(env.gpa, "var");
                node.appendRegionChild(env.gpa, ir.regionInfoFromTokenIds(v.region, env.line_starts.items));
                // name
                {
                    const name_str = ir.resolve(v.name);
                    var child = sexpr.Expr.init(env.gpa, "name");
                    child.appendStringChild(env.gpa, name_str);
                    node.appendNodeChild(env.gpa, &child);
                }
                // body
                {
                    const body = ir.store.getExpr(v.body);
                    var body_node = body.toSExpr(env, ir);
                    node.appendNodeChild(env.gpa, &body_node);
                }
                return node;
            },
            .expr => |expr| {
                return ir.store.getExpr(expr.expr).toSExpr(env, ir);
            },
            .import => |import| {
                var node = sexpr.Expr.init(env.gpa, "import");
                node.appendRegionChild(env.gpa, ir.regionInfoFromTokenIds(import.region, env.line_starts.items));
                // name e.g. `Stdout` in `import pf.Stdout`
                node.appendStringChild(env.gpa, ir.resolve(import.module_name_tok));
                // qualifier e.g. `pf` in `import pf.Stdout`
                if (import.qualifier_tok) |tok| {
                    const qualifier_str = ir.resolve(tok);
                    var child = sexpr.Expr.init(env.gpa, "qualifier");
                    child.appendStringChild(env.gpa, qualifier_str);
                    node.appendNodeChild(env.gpa, &child);
                }
                // alias e.g. `OUT` in `import pf.Stdout as OUT`
                if (import.alias_tok) |tok| {
                    const qualifier_str = ir.resolve(tok);
                    var child = sexpr.Expr.init(env.gpa, "alias");
                    child.appendStringChild(env.gpa, qualifier_str);
                    node.appendNodeChild(env.gpa, &child);
                }
                // exposed identifiers e.g. [foo, bar] in `import pf.Stdout exposing [foo, bar]`
                const exposed_slice = ir.store.exposedItemSlice(import.exposes);
                if (exposed_slice.len > 0) {
                    var exposed = sexpr.Expr.init(env.gpa, "exposing");
                    for (ir.store.exposedItemSlice(import.exposes)) |e| {
                        var exposed_item = &ir.store.getExposedItem(e);
                        var exposed_item_sexpr = exposed_item.toSExpr(env, ir);
                        exposed.appendNodeChild(env.gpa, &exposed_item_sexpr);
                    }
                    node.appendNodeChild(env.gpa, &exposed);
                }
                return node;
            },
            // (type_decl (header <name> [<args>]) <annotation>)
            .type_decl => |a| {
                var node = sexpr.Expr.init(env.gpa, "type_decl");
                node.appendRegionChild(env.gpa, ir.regionInfoFromTokenIds(a.region, env.line_starts.items));
                var header = sexpr.Expr.init(env.gpa, "header");
                // pattern
                {
                    const ty_header = ir.store.getTypeHeader(a.header);
                    header.appendRegionChild(env.gpa, ir.regionInfoFromTokenIds(ty_header.region, env.line_starts.items));

                    header.appendStringChild(env.gpa, ir.resolve(ty_header.name));

                    var args_node = sexpr.Expr.init(env.gpa, "args");

                    for (ir.store.typeAnnoSlice(ty_header.args)) |b| {
                        const anno = ir.store.getTypeAnno(b);
                        var anno_sexpr = anno.toSExpr(env, ir);
                        args_node.appendNodeChild(env.gpa, &anno_sexpr);
                    }
                    header.appendNodeChild(env.gpa, &args_node);

                    node.appendNodeChild(env.gpa, &header);
                }
                // annotation
                {
                    var annotation = ir.store.getTypeAnno(a.anno).toSExpr(env, ir);
                    node.appendNodeChild(env.gpa, &annotation);
                }
                return node;
            },
            // (crash <expr>)
            .crash => |a| {
                var node = sexpr.Expr.init(env.gpa, "crash");

                node.appendRegionChild(env.gpa, ir.regionInfoFromTokenIds(a.region, env.line_starts.items));

                var child = ir.store.getExpr(a.expr).toSExpr(env, ir);
                node.appendNodeChild(env.gpa, &child);
                return node;
            },
            // (expect <body>)
            .expect => |a| {
                var node = sexpr.Expr.init(env.gpa, "expect");

                node.appendRegionChild(env.gpa, ir.regionInfoFromTokenIds(a.region, env.line_starts.items));

                var child = ir.store.getExpr(a.body).toSExpr(env, ir);
                node.appendNodeChild(env.gpa, &child);
                return node;
            },
            .@"for" => |a| {
                var node = sexpr.Expr.init(env.gpa, "for");

                // patt
                {
                    var child = ir.store.getPattern(a.patt).toSExpr(env, ir);
                    node.appendNodeChild(env.gpa, &child);
                }
                // expr
                {
                    var child = ir.store.getExpr(a.expr).toSExpr(env, ir);
                    node.appendNodeChild(env.gpa, &child);
                }
                // body
                {
                    var child = ir.store.getExpr(a.body).toSExpr(env, ir);
                    node.appendNodeChild(env.gpa, &child);
                }

                return node;
            },
            // (return <expr>)
            .@"return" => |a| {
                var node = sexpr.Expr.init(env.gpa, "return");

                node.appendRegionChild(env.gpa, ir.regionInfoFromTokenIds(a.region, env.line_starts.items));

                var child = ir.store.getExpr(a.expr).toSExpr(env, ir);
                node.appendNodeChild(env.gpa, &child);
                return node;
            },
            // (type_anno <annotation>)
            .type_anno => |a| {
                var node = sexpr.Expr.init(env.gpa, "type_anno");

                node.appendRegionChild(env.gpa, ir.regionInfoFromTokenIds(a.region, env.line_starts.items));

                node.appendStringChild(env.gpa, ir.resolve(a.name));
                var child = ir.store.getTypeAnno(a.anno).toSExpr(env, ir);
                node.appendNodeChild(env.gpa, &child);
                return node;
            },
            .malformed => |a| {
                var node = sexpr.Expr.init(env.gpa, "malformed_stmt");
                node.appendRegionChild(env.gpa, ir.regionInfoFromTokenIds(a.region, env.line_starts.items));
                node.appendStringChild(env.gpa, @tagName(a.reason));
                return node;
            },
        }
    }
};

/// Represents a Body, or a block of statements.
pub const Body = struct {
    /// The statements that constitute the block
    statements: Statement.Span,
    region: Region,

    pub fn toSExpr(self: @This(), env: *base.ModuleEnv, ir: *AST) sexpr.Expr {
        var block_node = sexpr.Expr.init(env.gpa, "block");
        block_node.appendRegionChild(env.gpa, ir.regionInfoFromTokenIds(self.region, env.line_starts.items));
        var statements_node = sexpr.Expr.init(env.gpa, "statements");

        for (ir.store.statementSlice(self.statements)) |stmt_idx| {
            const stmt = ir.store.getStatement(stmt_idx);

            var stmt_node = stmt.toSExpr(env, ir);

            statements_node.appendNodeChild(env.gpa, &stmt_node);
        }

        block_node.appendNodeChild(env.gpa, &statements_node);

        return block_node;
    }
};

/// Represents a Pattern used in pattern matching.
pub const Pattern = union(enum) {
    ident: struct {
        ident_tok: Token.Idx,
        region: Region,
    },
    tag: struct {
        tag_tok: Token.Idx,
        args: Pattern.Span,
        region: Region,
    },
    number: struct {
        number_tok: Token.Idx,
        region: Region,
    },
    string: struct {
        string_tok: Token.Idx,
        region: Region,
        expr: Expr.Idx,
    },
    record: struct {
        fields: PatternRecordField.Span,
        region: Region,
    },
    list: struct {
        patterns: Pattern.Span,
        region: Region,
    },
    list_rest: struct {
        name: ?Token.Idx,
        region: Region,
    },
    tuple: struct {
        patterns: Pattern.Span,
        region: Region,
    },
    underscore: struct {
        region: Region,
    },
    alternatives: struct {
        patterns: Pattern.Span,
        region: Region,
    },
    malformed: struct {
        reason: Diagnostic.Tag,
        region: Region,
    },

    pub const Idx = enum(u32) { _ };
    pub const Span = struct { span: base.DataSpan };

    pub fn to_region(self: @This()) Region {
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

    pub fn toSExpr(self: @This(), env: *base.ModuleEnv, ir: *AST) sexpr.Expr {
        switch (self) {
            .ident => |ident| {
                var node = sexpr.Expr.init(env.gpa, "ident");

                node.appendRegionChild(env.gpa, ir.regionInfoFromTokenIds(ident.region, env.line_starts.items));

                node.appendStringChild(env.gpa, ir.resolve(ident.ident_tok));

                return node;
            },
            .tag => |tag| {
                var node = sexpr.Expr.init(env.gpa, "tag");

                node.appendRegionChild(env.gpa, ir.regionInfoFromTokenIds(tag.region, env.line_starts.items));

                node.appendStringChild(env.gpa, ir.resolve(tag.tag_tok));

                // Add arguments if there are any
                for (ir.store.patternSlice(tag.args)) |arg| {
                    var arg_node = ir.store.getPattern(arg).toSExpr(env, ir);
                    node.appendNodeChild(env.gpa, &arg_node);
                }

                return node;
            },
            .number => |num| {
                var node = sexpr.Expr.init(env.gpa, "number");
                node.appendRegionChild(env.gpa, ir.regionInfoFromTokenIds(num.region, env.line_starts.items));
                node.appendStringChild(env.gpa, ir.resolve(num.number_tok));
                return node;
            },
            .string => |str| {
                var node = sexpr.Expr.init(env.gpa, "string");
                node.appendRegionChild(env.gpa, ir.regionInfoFromTokenIds(str.region, env.line_starts.items));
                node.appendStringChild(env.gpa, ir.resolve(str.string_tok));
                return node;
            },
            .record => |rec| {
                var node = sexpr.Expr.init(env.gpa, "record");
                node.appendRegionChild(env.gpa, ir.regionInfoFromTokenIds(rec.region, env.line_starts.items));

                for (ir.store.patternRecordFieldSlice(rec.fields)) |field_idx| {
                    const field = ir.store.getPatternRecordField(field_idx);
                    var field_node = sexpr.Expr.init(env.gpa, "field");
                    field_node.appendRegionChild(env.gpa, ir.regionInfoFromTokenIds(field.region, env.line_starts.items));
                    field_node.appendStringChild(env.gpa, ir.resolve(field.name));

                    if (field.value) |value| {
                        var value_node = ir.store.getPattern(value).toSExpr(env, ir);
                        field_node.appendNodeChild(env.gpa, &value_node);
                    }

                    if (field.rest) {
                        field_node.appendStringChild(env.gpa, "rest");
                    }

                    node.appendNodeChild(env.gpa, &field_node);
                }

                return node;
            },
            .list => |list| {
                var node = sexpr.Expr.init(env.gpa, "list");
                node.appendRegionChild(env.gpa, ir.regionInfoFromTokenIds(list.region, env.line_starts.items));

                for (ir.store.patternSlice(list.patterns)) |pat| {
                    var pattern_node = ir.store.getPattern(pat).toSExpr(env, ir);
                    node.appendNodeChild(env.gpa, &pattern_node);
                }

                return node;
            },
            .list_rest => |rest| {
                var node = sexpr.Expr.init(env.gpa, "list_rest");
                node.appendRegionChild(env.gpa, ir.regionInfoFromTokenIds(rest.region, env.line_starts.items));

                if (rest.name) |name_tok| {
                    node.appendStringChild(env.gpa, ir.resolve(name_tok));
                }

                return node;
            },
            .tuple => |tuple| {
                var node = sexpr.Expr.init(env.gpa, "tuple");
                node.appendRegionChild(env.gpa, ir.regionInfoFromTokenIds(tuple.region, env.line_starts.items));

                for (ir.store.patternSlice(tuple.patterns)) |pat| {
                    var pattern_node = ir.store.getPattern(pat).toSExpr(env, ir);
                    node.appendNodeChild(env.gpa, &pattern_node);
                }

                return node;
            },
            .underscore => {
                return sexpr.Expr.init(env.gpa, "underscore");
            },
            .alternatives => |a| {
                // '|' separated list of patterns
                var node = sexpr.Expr.init(env.gpa, "alternatives");
                for (ir.store.patternSlice(a.patterns)) |pat| {
                    var patNode = ir.store.getPattern(pat).toSExpr(env, ir);
                    node.appendNodeChild(env.gpa, &patNode);
                }
                return node;
            },
            .malformed => |a| {
                var node = sexpr.Expr.init(env.gpa, "malformed_pattern");
                node.appendRegionChild(env.gpa, ir.regionInfoFromTokenIds(a.region, env.line_starts.items));
                node.appendStringChild(env.gpa, @tagName(a.reason));
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
    region: Region,

    /// (binop <op> <left> <right>) e.g. (binop '+' 1 2)
    pub fn toSExpr(self: *const @This(), env: *base.ModuleEnv, ir: *AST) sexpr.Expr {
        var node = sexpr.Expr.init(env.gpa, "binop");
        node.appendRegionChild(env.gpa, ir.regionInfoFromTokenIds(self.region, env.line_starts.items));
        node.appendStringChild(env.gpa, ir.resolve(self.operator));

        var left = ir.store.getExpr(self.left).toSExpr(env, ir);
        node.appendNodeChild(env.gpa, &left);

        var right = ir.store.getExpr(self.right).toSExpr(env, ir);
        node.appendNodeChild(env.gpa, &right);
        return node;
    }
};

/// TODO
pub const Unary = struct {
    operator: Token.Idx,
    expr: Expr.Idx,
    region: Region,

    pub fn toSExpr(self: *const @This(), env: *base.ModuleEnv, ir: *AST) sexpr.Expr {
        var node = sexpr.Expr.init(env.gpa, "unary");
        node.appendRegionChild(env.gpa, ir.regionInfoFromTokenIds(self.region, env.line_starts.items));
        node.appendStringChild(env.gpa, ir.resolve(self.operator));

        var expr = ir.store.getExpr(self.expr).toSExpr(env, ir);
        node.appendNodeChild(env.gpa, &expr);

        return node;
    }
};

/// Represents a delimited collection of other nodes
pub const Collection = struct {
    span: base.DataSpan,
    region: Region,

    pub const Idx = enum(u32) { _ };
};

/// Represents a Roc file.
pub const File = struct {
    header: Header.Idx,
    statements: Statement.Span,
    region: Region,

    pub fn toSExpr(self: @This(), env: *base.ModuleEnv, ir: *AST) sexpr.Expr {
        var file_node = sexpr.Expr.init(env.gpa, "file");

        file_node.appendRegionChild(env.gpa, ir.regionInfoFromTokenIds(self.region, env.line_starts.items));

        const header = ir.store.getHeader(self.header);
        var header_node = header.toSExpr(env, ir);

        file_node.appendNodeChild(env.gpa, &header_node);

        var statements_node = sexpr.Expr.init(env.gpa, "statements");

        for (ir.store.statementSlice(self.statements)) |stmt_id| {
            const stmt = ir.store.getStatement(stmt_id);
            var stmt_node = stmt.toSExpr(env, ir);
            statements_node.appendNodeChild(env.gpa, &stmt_node);
        }

        file_node.appendNodeChild(env.gpa, &statements_node);

        return file_node;
    }
};

/// Represents a module header.
pub const Header = union(enum) {
    app: struct {
        provides: Collection.Idx,
        platform_idx: RecordField.Idx,
        packages: Collection.Idx,
        region: Region,
    },
    module: struct {
        exposes: Collection.Idx,
        region: Region,
    },
    package: struct {
        exposes: Collection.Idx,
        packages: Collection.Idx,
        region: Region,
    },
    platform: struct {
        // TODO: complete this
        name: Token.Idx,
        requires_rigids: Collection.Idx,
        requires_signatures: TypeAnno.Idx,
        exposes: Collection.Idx,
        packages: Collection.Idx,
        provides: Collection.Idx,
        region: Region,
    },
    hosted: struct {
        exposes: Collection.Idx,
        region: Region,
    },
    malformed: struct {
        reason: Diagnostic.Tag,
        region: Region,
    },

    pub const Idx = enum(u32) { _ };

    pub const AppHeaderRhs = packed struct { num_packages: u10, num_provides: u22 };

    pub fn toSExpr(self: @This(), env: *base.ModuleEnv, ir: *AST) sexpr.Expr {
        switch (self) {
            .app => |a| {
                var node = sexpr.Expr.init(env.gpa, "app");
                node.appendRegionChild(env.gpa, ir.regionInfoFromTokenIds(a.region, env.line_starts.items));
                // Provides
                const provides_coll = ir.store.getCollection(a.provides);
                const provides_items = ir.store.exposedItemSlice(.{ .span = provides_coll.span });
                var provides_node = sexpr.Expr.init(env.gpa, "provides");
                provides_node.appendRegionChild(env.gpa, ir.regionInfoFromTokenIds(provides_coll.region, env.line_starts.items));
                for (provides_items) |item_idx| {
                    const item = ir.store.getExposedItem(item_idx);
                    var item_node = item.toSExpr(env, ir);
                    provides_node.appendNodeChild(env.gpa, &item_node);
                }
                node.appendNodeChild(env.gpa, &provides_node);
                // Platform
                const platform = ir.store.getRecordField(a.platform_idx);
                var platform_node = platform.toSExpr(env, ir);
                node.appendNodeChild(env.gpa, &platform_node);
                // Packages
                const packages_coll = ir.store.getCollection(a.packages);
                const packages_items = ir.store.recordFieldSlice(.{ .span = packages_coll.span });
                var packages_node = sexpr.Expr.init(env.gpa, "packages");
                packages_node.appendRegionChild(env.gpa, ir.regionInfoFromTokenIds(packages_coll.region, env.line_starts.items));
                for (packages_items) |item_idx| {
                    const item = ir.store.getRecordField(item_idx);
                    var item_node = item.toSExpr(env, ir);
                    packages_node.appendNodeChild(env.gpa, &item_node);
                }
                node.appendNodeChild(env.gpa, &packages_node);
                return node;
            },
            .module => |module| {
                var node = sexpr.Expr.init(env.gpa, "module");
                node.appendRegionChild(env.gpa, ir.regionInfoFromTokenIds(module.region, env.line_starts.items));
                const exposes = ir.store.getCollection(module.exposes);
                var exposes_node = sexpr.Expr.init(env.gpa, "exposes");
                exposes_node.appendRegionChild(env.gpa, ir.regionInfoFromTokenIds(exposes.region, env.line_starts.items));
                for (ir.store.exposedItemSlice(.{ .span = exposes.span })) |exposed| {
                    const item = ir.store.getExposedItem(exposed);
                    var item_node = item.toSExpr(env, ir);
                    exposes_node.appendNodeChild(env.gpa, &item_node);
                }
                node.appendNodeChild(env.gpa, &exposes_node);
                return node;
            },
            .package => |a| {
                var node = sexpr.Expr.init(env.gpa, "package");
                node.appendRegionChild(env.gpa, ir.regionInfoFromTokenIds(a.region, env.line_starts.items));
                // Exposes
                const exposes = ir.store.getCollection(a.exposes);
                var exposes_node = sexpr.Expr.init(env.gpa, "exposes");
                exposes_node.appendRegionChild(env.gpa, ir.regionInfoFromTokenIds(exposes.region, env.line_starts.items));
                for (ir.store.exposedItemSlice(.{ .span = exposes.span })) |exposed| {
                    const item = ir.store.getExposedItem(exposed);
                    var item_node = item.toSExpr(env, ir);
                    exposes_node.appendNodeChild(env.gpa, &item_node);
                }
                node.appendNodeChild(env.gpa, &exposes_node);
                // Packages
                const packages_coll = ir.store.getCollection(a.packages);
                const packages_items = ir.store.recordFieldSlice(.{ .span = packages_coll.span });
                var packages_node = sexpr.Expr.init(env.gpa, "packages");
                packages_node.appendRegionChild(env.gpa, ir.regionInfoFromTokenIds(packages_coll.region, env.line_starts.items));
                for (packages_items) |item_idx| {
                    const item = ir.store.getRecordField(item_idx);
                    var item_node = item.toSExpr(env, ir);
                    packages_node.appendNodeChild(env.gpa, &item_node);
                }
                node.appendNodeChild(env.gpa, &packages_node);
                return node;
            },
            .platform => |a| {
                var node = sexpr.Expr.init(env.gpa, "platform");
                node.appendRegionChild(env.gpa, ir.regionInfoFromTokenIds(a.region, env.line_starts.items));
                // Name
                node.appendStringChild(env.gpa, ir.resolve(a.name));
                // Requires Rigids
                const rigids = ir.store.getCollection(a.requires_rigids);
                var rigids_node = sexpr.Expr.init(env.gpa, "rigids");
                rigids_node.appendRegionChild(env.gpa, ir.regionInfoFromTokenIds(rigids.region, env.line_starts.items));
                for (ir.store.exposedItemSlice(.{ .span = rigids.span })) |exposed| {
                    const item = ir.store.getExposedItem(exposed);
                    var item_node = item.toSExpr(env, ir);
                    rigids_node.appendNodeChild(env.gpa, &item_node);
                }
                node.appendNodeChild(env.gpa, &rigids_node);
                // Requires Signatures
                const signatures = ir.store.getTypeAnno(a.requires_signatures);
                var signatures_node = signatures.toSExpr(env, ir);
                node.appendNodeChild(env.gpa, &signatures_node);
                // Exposes
                const exposes = ir.store.getCollection(a.exposes);
                var exposes_node = sexpr.Expr.init(env.gpa, "exposes");
                exposes_node.appendRegionChild(env.gpa, ir.regionInfoFromTokenIds(exposes.region, env.line_starts.items));
                for (ir.store.exposedItemSlice(.{ .span = exposes.span })) |exposed| {
                    const item = ir.store.getExposedItem(exposed);
                    var item_node = item.toSExpr(env, ir);
                    exposes_node.appendNodeChild(env.gpa, &item_node);
                }
                node.appendNodeChild(env.gpa, &exposes_node);
                // Packages
                const packages_coll = ir.store.getCollection(a.packages);
                const packages_items = ir.store.recordFieldSlice(.{ .span = packages_coll.span });
                var packages_node = sexpr.Expr.init(env.gpa, "packages");
                packages_node.appendRegionChild(env.gpa, ir.regionInfoFromTokenIds(packages_coll.region, env.line_starts.items));
                for (packages_items) |item_idx| {
                    const item = ir.store.getRecordField(item_idx);
                    var item_node = item.toSExpr(env, ir);
                    packages_node.appendNodeChild(env.gpa, &item_node);
                }
                node.appendNodeChild(env.gpa, &packages_node);
                // Provides
                const provides = ir.store.getCollection(a.provides);
                var provides_node = sexpr.Expr.init(env.gpa, "provides");
                provides_node.appendRegionChild(env.gpa, ir.regionInfoFromTokenIds(provides.region, env.line_starts.items));
                for (ir.store.exposedItemSlice(.{ .span = provides.span })) |exposed| {
                    const item = ir.store.getExposedItem(exposed);
                    var item_node = item.toSExpr(env, ir);
                    provides_node.appendNodeChild(env.gpa, &item_node);
                }
                node.appendNodeChild(env.gpa, &provides_node);
                return node;
            },
            .hosted => |a| {
                var node = sexpr.Expr.init(env.gpa, "hosted");
                node.appendRegionChild(env.gpa, ir.regionInfoFromTokenIds(a.region, env.line_starts.items));
                const exposes = ir.store.getCollection(a.exposes);
                var exposes_node = sexpr.Expr.init(env.gpa, "exposes");
                exposes_node.appendRegionChild(env.gpa, ir.regionInfoFromTokenIds(exposes.region, env.line_starts.items));
                for (ir.store.exposedItemSlice(.{ .span = exposes.span })) |exposed| {
                    const item = ir.store.getExposedItem(exposed);
                    var item_node = item.toSExpr(env, ir);
                    exposes_node.appendNodeChild(env.gpa, &item_node);
                }
                node.appendNodeChild(env.gpa, &exposes_node);
                return node;
            },
            .malformed => |a| {
                var node = sexpr.Expr.init(env.gpa, "malformed_header");
                node.appendRegionChild(env.gpa, ir.regionInfoFromTokenIds(a.region, env.line_starts.items));
                node.appendStringChild(env.gpa, @tagName(a.reason));
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
        region: Region,
    },
    upper_ident: struct {
        as: ?Token.Idx,
        ident: Token.Idx,
        region: Region,
    },
    upper_ident_star: struct {
        ident: Token.Idx,
        region: Region,
    },

    pub const Idx = enum(u32) { _ };
    pub const Span = struct { span: base.DataSpan };

    pub fn toSExpr(self: @This(), env: *base.ModuleEnv, ir: *AST) sexpr.Expr {
        _ = env.line_starts.items;
        var node = sexpr.Expr.init(env.gpa, "exposed_item");
        var inner_node = sexpr.Expr.init(env.gpa, @tagName(self));
        switch (self) {
            .lower_ident => |i| {
                const token = ir.tokens.tokens.get(i.ident);
                const text = env.idents.getText(token.extra.interned);
                inner_node.appendStringChild(env.gpa, text);
                if (i.as) |a| {
                    const as_tok = ir.tokens.tokens.get(a);
                    const as_text = env.idents.getText(as_tok.extra.interned);
                    inner_node.appendStringChild(env.gpa, as_text);
                }
            },
            .upper_ident => |i| {
                const token = ir.tokens.tokens.get(i.ident);
                const text = env.idents.getText(token.extra.interned);
                inner_node.appendStringChild(env.gpa, text);
                if (i.as) |a| {
                    const as_tok = ir.tokens.tokens.get(a);
                    const as_text = env.idents.getText(as_tok.extra.interned);
                    inner_node.appendStringChild(env.gpa, as_text);
                }
            },
            .upper_ident_star => |i| {
                const token = ir.tokens.tokens.get(i.ident);
                const text = env.idents.getText(token.extra.interned);
                inner_node.appendStringChild(env.gpa, text);
            },
        }
        node.appendNodeChild(env.gpa, &inner_node);
        return node;
    }
};

/// TODO
pub const TypeHeader = struct {
    name: Token.Idx,
    args: TypeAnno.Span,
    region: Region,

    pub const Idx = enum(u32) { _ };
};

/// TODO
pub const TypeAnno = union(enum) {
    apply: struct {
        args: TypeAnno.Span,
        region: Region,
    },
    ty_var: struct {
        tok: Token.Idx,
        region: Region,
    },
    underscore: struct {
        region: Region,
    },
    ty: struct {
        ident: base.Ident.Idx,
        // Region starts with the type token.
        region: Region,
    },
    mod_ty: struct {
        mod_ident: base.Ident.Idx,
        ty_ident: base.Ident.Idx,
        // Region starts with the mod token and ends with the type token.
        region: Region,
    },
    tag_union: struct {
        tags: TypeAnno.Span,
        open_anno: ?TypeAnno.Idx,
        region: Region,
    },
    tuple: struct {
        annos: TypeAnno.Span,
        region: Region,
    },
    record: struct {
        fields: AnnoRecordField.Span,
        region: Region,
    },
    @"fn": struct {
        args: TypeAnno.Span,
        ret: TypeAnno.Idx,
        effectful: bool,
        region: Region,
    },
    parens: struct {
        anno: TypeAnno.Idx,
        region: Region,
    },
    malformed: struct {
        reason: Diagnostic.Tag,
        region: Region,
    },

    pub const Idx = enum(u32) { _ };
    pub const Span = struct { span: base.DataSpan };

    pub const TagUnionRhs = packed struct { open: u1, tags_len: u31 };
    pub const TypeAnnoFnRhs = packed struct { effectful: u1, args_len: u31 };

    pub fn toSExpr(self: @This(), env: *base.ModuleEnv, ir: *AST) sexpr.Expr {
        switch (self) {
            // (apply <ty> [<args>])
            .apply => |a| {
                var node = sexpr.Expr.init(env.gpa, "apply");

                node.appendRegionChild(env.gpa, ir.regionInfoFromTokenIds(a.region, env.line_starts.items));

                for (ir.store.typeAnnoSlice(a.args)) |b| {
                    var child = ir.store.getTypeAnno(b).toSExpr(env, ir);
                    node.appendNodeChild(env.gpa, &child);
                }

                return node;
            },
            // (ty_var <var>)
            .ty_var => |a| {
                var node = sexpr.Expr.init(env.gpa, "ty_var");

                node.appendRegionChild(env.gpa, ir.regionInfoFromTokenIds(a.region, env.line_starts.items));

                node.appendStringChild(env.gpa, ir.resolve(a.tok));
                return node;
            },
            // (_)
            .underscore => {
                return sexpr.Expr.init(env.gpa, "_");
            },
            // (ty name)
            .ty => |a| {
                var node = sexpr.Expr.init(env.gpa, "ty");
                node.appendStringChild(env.gpa, ir.resolve(a.region.start));
                return node;
            },
            // (mod_ty mod ty)
            .mod_ty => |a| {
                var node = sexpr.Expr.init(env.gpa, "mod_ty");
                node.appendStringChild(env.gpa, ir.resolve(a.region.start));
                node.appendStringChild(env.gpa, ir.resolve(a.region.end));
                return node;
            },
            .tag_union => |a| {
                var node = sexpr.Expr.init(env.gpa, "tag_union");

                node.appendRegionChild(env.gpa, ir.regionInfoFromTokenIds(a.region, env.line_starts.items));

                const tags = ir.store.typeAnnoSlice(a.tags);
                var tags_node = sexpr.Expr.init(env.gpa, "tags");
                for (tags) |tag_idx| {
                    const tag = ir.store.getTypeAnno(tag_idx);
                    var tag_node = tag.toSExpr(env, ir);
                    tags_node.appendNodeChild(env.gpa, &tag_node);
                }
                node.appendNodeChild(env.gpa, &tags_node);
                if (a.open_anno) |anno_idx| {
                    const anno = ir.store.getTypeAnno(anno_idx);
                    var anno_node = anno.toSExpr(env, ir);
                    node.appendNodeChild(env.gpa, &anno_node);
                }
                return node;
            },
            // (tuple [<elems>])
            .tuple => |a| {
                var node = sexpr.Expr.init(env.gpa, "tuple");

                node.appendRegionChild(env.gpa, ir.regionInfoFromTokenIds(a.region, env.line_starts.items));

                for (ir.store.typeAnnoSlice(a.annos)) |b| {
                    var child = ir.store.getTypeAnno(b).toSExpr(env, ir);
                    node.appendNodeChild(env.gpa, &child);
                }
                return node;
            },
            // (record [<fields>])
            .record => |a| {
                var node = sexpr.Expr.init(env.gpa, "record");

                node.appendRegionChild(env.gpa, ir.regionInfoFromTokenIds(a.region, env.line_starts.items));

                for (ir.store.annoRecordFieldSlice(a.fields)) |f_idx| {
                    const field = ir.store.getAnnoRecordField(f_idx);
                    var field_node = field.toSExpr(env, ir);
                    node.appendNodeChild(env.gpa, &field_node);
                }
                return node;
            },
            // (fn <ret> [<args>])
            .@"fn" => |a| {
                var node = sexpr.Expr.init(env.gpa, "fn");

                node.appendRegionChild(env.gpa, ir.regionInfoFromTokenIds(a.region, env.line_starts.items));

                // arguments
                for (ir.store.typeAnnoSlice(a.args)) |b| {
                    var child = ir.store.getTypeAnno(b).toSExpr(env, ir);
                    node.appendNodeChild(env.gpa, &child);
                }

                // return value
                var ret = ir.store.getTypeAnno(a.ret).toSExpr(env, ir);
                node.appendNodeChild(env.gpa, &ret);

                return node;
            },
            // ignore parens... use inner
            .parens => |a| {
                return ir.store.getTypeAnno(a.anno).toSExpr(env, ir);
            },
            .malformed => |a| {
                var node = sexpr.Expr.init(env.gpa, "malformed_expr");
                node.appendRegionChild(env.gpa, ir.regionInfoFromTokenIds(a.region, env.line_starts.items));
                node.appendStringChild(env.gpa, @tagName(a.reason));
                return node;
            },
        }
    }
};

/// TODO
pub const AnnoRecordField = struct {
    name: Token.Idx,
    ty: TypeAnno.Idx,
    region: Region,

    pub const Idx = enum(u32) { _ };
    pub const Span = struct { span: base.DataSpan };

    pub fn toSExpr(self: @This(), env: *base.ModuleEnv, ir: *AST) sexpr.Expr {
        var node = sexpr.Expr.init(env.gpa, "anno_record_field");
        node.appendRegionChild(env.gpa, ir.regionInfoFromTokenIds(self.region, env.line_starts.items));
        node.appendStringChild(env.gpa, ir.resolve(self.name));
        const anno = ir.store.getTypeAnno(self.ty);
        var ty_node = anno.toSExpr(env, ir);
        node.appendNodeChild(env.gpa, &ty_node);
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
        region: Region,
    },
    method: struct {
        var_tok: Token.Idx,
        name_tok: Token.Idx,
        args: Collection.Idx,
        ret_anno: TypeAnno.Idx,
        region: Region,
    },
    mod_method: struct {
        var_tok: Token.Idx,
        name_tok: Token.Idx,
        args: Collection.Idx,
        ret_anno: TypeAnno.Idx,
        region: Region,
    },
    malformed: struct {
        reason: Diagnostic.Tag,
        region: Region,
    },

    pub const Idx = enum(u32) { _ };
    pub const Span = struct { span: base.DataSpan };
};

/// Represents an expression.
pub const Expr = union(enum) {
    int: struct {
        token: Token.Idx,
        region: Region,
    },
    float: struct {
        token: Token.Idx,
        region: Region,
    },
    string_part: struct { // TODO: this should be more properly represented in its own union enum
        token: Token.Idx,
        region: Region,
    },
    string: struct {
        token: Token.Idx,
        region: Region,
        parts: Expr.Span,
    },
    list: struct {
        items: Expr.Span,
        region: Region,
    },
    tuple: struct {
        items: Expr.Span,
        region: Region,
    },
    record: struct {
        fields: RecordField.Span,
        region: Region,
    },
    tag: struct {
        token: Token.Idx,
        region: Region,
    },
    lambda: struct {
        args: Pattern.Span,
        body: Expr.Idx,
        region: Region,
    },
    apply: struct {
        args: Expr.Span,
        @"fn": Expr.Idx,
        region: Region,
    },
    record_updater: struct {
        token: Token.Idx,
        region: Region,
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
        region: Region,
    },
    match: struct {
        expr: Expr.Idx,
        branches: WhenBranch.Span,
        region: Region,
    },
    ident: struct {
        token: Token.Idx,
        qualifier: ?Token.Idx,
        region: Region,
    },
    dbg: struct {
        expr: Expr.Idx,
        region: Region,
    },
    record_builder: struct {
        mapper: Expr.Idx,
        fields: RecordField.Idx,
        region: Region,
    },
    ellipsis: struct {
        region: Region,
    },
    block: Body,
    malformed: struct {
        reason: Diagnostic.Tag,
        region: Region,
    },

    pub const Idx = enum(u32) { _ };
    pub const Span = struct { span: base.DataSpan };

    pub fn as_string_part_region(self: @This()) !Region {
        switch (self) {
            .string_part => |part| return part.region,
            else => return error.ExpectedStringPartRegion,
        }
    }

    pub fn to_region(self: @This()) Region {
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

    pub fn toSExpr(self: @This(), env: *base.ModuleEnv, ir: *AST) sexpr.Expr {
        switch (self) {
            .int => |int| {
                var node = sexpr.Expr.init(env.gpa, "int");
                node.appendRegionChild(env.gpa, ir.regionInfoFromTokenIds(int.region, env.line_starts.items));
                node.appendStringChild(env.gpa, ir.resolve(int.token));
                return node;
            },
            .string => |str| {
                var node = sexpr.Expr.init(env.gpa, "string");
                node.appendRegionChild(env.gpa, ir.regionInfoFromTokenIds(str.region, env.line_starts.items));
                for (ir.store.exprSlice(str.parts)) |part_id| {
                    const part_expr = ir.store.getExpr(part_id);
                    var part_sexpr = part_expr.toSExpr(env, ir);
                    node.appendNodeChild(env.gpa, &part_sexpr);
                }
                return node;
            },
            .string_part => |sp| {
                var node = sexpr.Expr.init(env.gpa, "string_part");
                node.appendRegionChild(env.gpa, ir.regionInfoFromTokenIds(sp.region, env.line_starts.items));
                node.appendStringChild(env.gpa, ir.resolve(sp.token));
                return node;
            },
            // (tag <tag>)
            .tag => |tag| {
                var node = sexpr.Expr.init(env.gpa, "tag");

                node.appendRegionChild(env.gpa, ir.regionInfoFromTokenIds(tag.region, env.line_starts.items));

                node.appendStringChild(env.gpa, ir.resolve(tag.token));
                return node;
            },
            .block => |block| {
                return block.toSExpr(env, ir);
            },
            // (if_then_else <condition> <then> <else>)
            .if_then_else => |stmt| {
                var node = sexpr.Expr.init(env.gpa, "if_then_else");

                node.appendRegionChild(env.gpa, ir.regionInfoFromTokenIds(stmt.region, env.line_starts.items));

                var condition = ir.store.getExpr(stmt.condition).toSExpr(env, ir);
                var then = ir.store.getExpr(stmt.then).toSExpr(env, ir);
                var else_ = ir.store.getExpr(stmt.@"else").toSExpr(env, ir);

                node.appendNodeChild(env.gpa, &condition);
                node.appendNodeChild(env.gpa, &then);
                node.appendNodeChild(env.gpa, &else_);

                return node;
            },
            .ident => |ident| {
                var node = sexpr.Expr.init(env.gpa, "ident");

                node.appendRegionChild(env.gpa, ir.regionInfoFromTokenIds(ident.region, env.line_starts.items));

                node.appendStringChild(env.gpa, if (ident.qualifier != null) ir.resolve(ident.qualifier.?) else "");
                node.appendStringChild(env.gpa, ir.resolve(ident.token));
                return node;
            },
            // (list [<child>])
            .list => |a| {
                var node = sexpr.Expr.init(env.gpa, "list");

                node.appendRegionChild(env.gpa, ir.regionInfoFromTokenIds(a.region, env.line_starts.items));

                for (ir.store.exprSlice(a.items)) |b| {
                    var child = ir.store.getExpr(b).toSExpr(env, ir);
                    node.appendNodeChild(env.gpa, &child);
                }
                return node;
            },
            // (malformed_expr <reason>)
            .malformed => |a| {
                var node = sexpr.Expr.init(env.gpa, "malformed_expr");
                node.appendRegionChild(env.gpa, ir.regionInfoFromTokenIds(a.region, env.line_starts.items));
                node.appendStringChild(env.gpa, @tagName(a.reason));
                return node;
            },
            // (float <value>)
            .float => |a| {
                var node = sexpr.Expr.init(env.gpa, "float");

                node.appendRegionChild(env.gpa, ir.regionInfoFromTokenIds(a.region, env.line_starts.items));

                node.appendStringChild(env.gpa, ir.resolve(a.token));
                return node;
            },
            // (tuple [<item>])
            .tuple => |a| {
                var node = sexpr.Expr.init(env.gpa, "tuple");

                node.appendRegionChild(env.gpa, ir.regionInfoFromTokenIds(a.region, env.line_starts.items));

                for (ir.store.exprSlice(a.items)) |item| {
                    var child = ir.store.getExpr(item).toSExpr(env, ir);
                    node.appendNodeChild(env.gpa, &child);
                }

                return node;
            },
            // (record [(field <name> <?value> ?optional)])
            .record => |a| {
                var node = sexpr.Expr.init(env.gpa, "record");

                node.appendRegionChild(env.gpa, ir.regionInfoFromTokenIds(a.region, env.line_starts.items));

                for (ir.store.recordFieldSlice(a.fields)) |field_idx| {
                    const record_field = ir.store.getRecordField(field_idx);
                    var record_field_node = sexpr.Expr.init(env.gpa, "field");
                    record_field_node.appendStringChild(env.gpa, ir.resolve(record_field.name));
                    if (record_field.value != null) {
                        var value_node = ir.store.getExpr(record_field.value.?).toSExpr(env, ir);
                        record_field_node.appendNodeChild(env.gpa, &value_node);
                    }
                    if (record_field.optional) {
                        record_field_node.appendStringChild(env.gpa, "optional");
                    }
                    node.appendNodeChild(env.gpa, &record_field_node);
                }

                return node;
            },
            // (apply <fn> [<args>])
            .apply => |a| {
                var node = sexpr.Expr.init(env.gpa, "apply");

                node.appendRegionChild(env.gpa, ir.regionInfoFromTokenIds(a.region, env.line_starts.items));

                var apply_fn = ir.store.getExpr(a.@"fn").toSExpr(env, ir);
                node.appendNodeChild(env.gpa, &apply_fn);

                for (ir.store.exprSlice(a.args)) |arg| {
                    var arg_node = ir.store.getExpr(arg).toSExpr(env, ir);
                    node.appendNodeChild(env.gpa, &arg_node);
                }

                return node;
            },
            .field_access => |a| {
                var node = sexpr.Expr.init(env.gpa, "field_access");

                node.appendRegionChild(env.gpa, ir.regionInfoFromTokenIds(a.region, env.line_starts.items));

                var child = a.toSExpr(env, ir);
                node.appendNodeChild(env.gpa, &child);
                return node;
            },
            .local_dispatch => |a| {
                var node = sexpr.Expr.init(env.gpa, "local_dispatch");
                node.appendRegionChild(env.gpa, ir.regionInfoFromTokenIds(a.region, env.line_starts.items));

                var left = ir.store.getExpr(a.left).toSExpr(env, ir);
                var right = ir.store.getExpr(a.right).toSExpr(env, ir);
                node.appendNodeChild(env.gpa, &left);
                node.appendNodeChild(env.gpa, &right);
                return node;
            },
            // (binop <op> <lhs> <rhs>)
            .bin_op => |a| {
                return a.toSExpr(env, ir);
            },
            .lambda => |a| {
                var node = sexpr.Expr.init(env.gpa, "lambda");

                node.appendRegionChild(env.gpa, ir.regionInfoFromTokenIds(a.region, env.line_starts.items));

                // arguments
                var args = sexpr.Expr.init(env.gpa, "args");
                for (ir.store.patternSlice(a.args)) |arg| {
                    var arg_node = ir.store.getPattern(arg).toSExpr(env, ir);
                    args.appendNodeChild(env.gpa, &arg_node);
                }
                node.appendNodeChild(env.gpa, &args);

                // body
                var body = ir.store.getExpr(a.body).toSExpr(env, ir);
                node.appendNodeChild(env.gpa, &body);

                return node;
            },
            .dbg => |a| {
                var node = sexpr.Expr.init(env.gpa, "dbg");

                var arg = ir.store.getExpr(a.expr).toSExpr(env, ir);
                node.appendNodeChild(env.gpa, &arg);

                return node;
            },
            .match => |a| {
                var node = sexpr.Expr.init(env.gpa, "match");

                var expr = ir.store.getExpr(a.expr).toSExpr(env, ir);

                // handle branches
                var branches = sexpr.Expr.init(env.gpa, "branches");
                for (ir.store.whenBranchSlice(a.branches)) |branch| {
                    var branch_node = ir.store.getBranch(branch).toSExpr(env, ir);
                    branches.appendNodeChild(env.gpa, &branch_node);
                }

                node.appendNodeChild(env.gpa, &expr);

                node.appendNodeChild(env.gpa, &branches);

                return node;
            },
            .ellipsis => {
                return sexpr.Expr.init(env.gpa, "ellipsis");
            },
            .suffix_single_question => |a| {
                var node = sexpr.Expr.init(env.gpa, "suffix_single_question");

                node.appendRegionChild(env.gpa, ir.regionInfoFromTokenIds(a.region, env.line_starts.items));

                var child = ir.store.getExpr(a.expr).toSExpr(env, ir);
                node.appendNodeChild(env.gpa, &child);
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
    region: Region,

    pub const Idx = enum(u32) { _ };
    pub const Span = struct { span: base.DataSpan };
};

/// TODO
pub const RecordField = struct {
    name: Token.Idx,
    value: ?Expr.Idx,
    optional: bool,
    region: Region,

    pub const Idx = enum(u32) { _ };
    pub const Span = struct { span: base.DataSpan };

    pub fn toSExpr(self: @This(), env: *base.ModuleEnv, ir: *AST) sexpr.Expr {
        var node = sexpr.Expr.init(env.gpa, "record_field");
        node.appendRegionChild(env.gpa, ir.regionInfoFromTokenIds(self.region, env.line_starts.items));
        node.appendStringChild(env.gpa, ir.resolve(self.name));
        if (self.value) |idx| {
            const value = ir.store.getExpr(idx);
            var value_node = value.toSExpr(env, ir);
            node.appendNodeChild(env.gpa, &value_node);
        }
        return node;
    }
};

/// TODO
pub const IfElse = struct {
    condition: Expr.Idx,
    body: Expr.Idx,
    region: Region,

    pub const Idx = enum(u32) { _ };
    pub const Span = struct { span: base.DataSpan };
};

/// TODO
pub const WhenBranch = struct {
    pattern: Pattern.Idx,
    body: Expr.Idx,
    region: Region,

    pub const Idx = enum(u32) { _ };
    pub const Span = struct { span: base.DataSpan };

    pub fn toSExpr(self: @This(), env: *base.ModuleEnv, ir: *AST) sexpr.Expr {
        var node = sexpr.Expr.init(env.gpa, "branch");
        node.appendRegionChild(env.gpa, ir.regionInfoFromTokenIds(self.region, env.line_starts.items));
        var pattern = ir.store.getPattern(self.pattern).toSExpr(env, ir);
        node.appendNodeChild(env.gpa, &pattern);
        var body = ir.store.getExpr(self.body).toSExpr(env, ir);
        node.appendNodeChild(env.gpa, &body);
        return node;
    }
};
