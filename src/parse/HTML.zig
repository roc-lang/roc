//! Generate HTML representation of the AST using a stack-based traversal.

const std = @import("std");
const base = @import("base");
const parse = @import("parse");
const compile = @import("compile");

const AST = parse.AST;
const RegionInfo = base.RegionInfo;
const ModuleEnv = compile.ModuleEnv;
const Token = parse.Token;

fn appendRegionInfoAsHtmlAttr(ast: *const AST, env: ModuleEnv, writer: std.io.AnyWriter, region: AST.TokenizedRegion) !void {
    const start = ast.tokens.resolve(region.start);
    const region_end_idx = if (region.end > 0) region.end - 1 else region.end;
    const end = ast.tokens.resolve(region_end_idx);
    const info: RegionInfo = RegionInfo.position(ast.env.source, env.line_starts.items.items, start.start.offset, end.end.offset) catch return; // Ignore errors for this

    try writer.print(" data-start-offset=\"{d}\" data-end-offset=\"{d}\" data-start-line=\"{d}\" data-start-col=\"{d}\" data-end-line=\"{d}\" data-end-col=\"{d}\"", .{
        start.start.offset,
        end.end.offset,
        info.start_line_idx + 1,
        info.start_col_idx + 1,
        info.end_line_idx + 1,
        info.end_col_idx + 1,
    });
}

/// Generate an interactive source range span for the playground
fn writeSourceRangeSpan(writer: anytype, region: base.Region, source: []const u8, line_starts: []const u32) !void {
    const region_info = base.RegionInfo.position(source, line_starts, region.start.offset, region.end.offset) catch {
        try writer.print("<span class=\"source-range\" data-start-byte=\"{d}\" data-end-byte=\"{d}\">@{d}-{d}</span>", .{ region.start.offset, region.end.offset, region.start.offset, region.end.offset });
        return;
    };
    try writer.print("<span class=\"source-range\" data-start-byte=\"{d}\" data-end-byte=\"{d}\" data-start-line=\"{d}\" data-start-col=\"{d}\" data-end-line=\"{d}\" data-end-col=\"{d}\">@{d}.{d}-{d}.{d}</span>", .{ region.start.offset, region.end.offset, region_info.start_line_idx + 1, region_info.start_col_idx + 1, region_info.end_line_idx + 1, region_info.end_col_idx + 1, region_info.start_line_idx + 1, region_info.start_col_idx + 1, region_info.end_line_idx + 1, region_info.end_col_idx + 1 });
}

/// Generate an HTML representation of the tokens in the AST
pub fn tokensToHtml(ast: *const AST, env: *const ModuleEnv, writer: anytype) !void {
    try writer.writeAll("<div class=\"token-list\">");

    const token_tags = ast.tokens.tokens.items(.tag);
    const token_extras = ast.tokens.tokens.items(.extra);

    for (token_tags, token_extras, 0..) |tag, _, i| {
        const region = ast.tokens.resolve(i);
        const css_class: []const u8 = switch (tag) {
            .KwApp,
            .KwAs,
            .KwCrash,
            .KwDbg,
            .KwElse,
            .KwExpect,
            .KwExposes,
            .KwExposing,
            .KwFor,
            .KwGenerates,
            .KwHas,
            .KwHosted,
            .KwIf,
            .KwImplements,
            .KwImport,
            .KwImports,
            .KwIn,
            .KwInterface,
            .KwMatch,
            .KwModule,
            .KwPackage,
            .KwPackages,
            .KwPlatform,
            .KwProvides,
            .KwRequires,
            .KwReturn,
            .KwVar,
            .KwWhere,
            .KwWith,
            => "tok-kw",
            .UpperIdent, .LowerIdent, .DotLowerIdent, .DotUpperIdent, .NoSpaceDotLowerIdent, .NoSpaceDotUpperIdent, .NamedUnderscore => "tok-ident",
            .OpPlus, .OpStar, .OpPizza, .OpAssign, .OpBinaryMinus, .OpUnaryMinus, .OpNotEquals, .OpBang, .OpAnd, .OpAmpersand, .OpQuestion, .OpDoubleQuestion, .OpOr, .OpBar, .OpDoubleSlash, .OpSlash, .OpPercent, .OpCaret, .OpGreaterThanOrEq, .OpGreaterThan, .OpLessThanOrEq, .OpBackArrow, .OpLessThan, .OpEquals, .OpColonEqual, .NoSpaceOpQuestion => "tok-op",
            .StringStart, .StringEnd, .MultilineStringStart, .MultilineStringEnd, .StringPart => "tok-str",
            .Float, .Int => "tok-num",
            .OpenRound, .CloseRound, .OpenSquare, .CloseSquare, .OpenCurly, .CloseCurly, .OpenStringInterpolation, .CloseStringInterpolation, .NoSpaceOpenRound => "tok-punct",
            .EndOfFile => "tok-eof",
            else => "tok-default",
        };

        try writer.print("<span class=\"token {s}\">", .{css_class});
        try writer.print("<span class=\"token-type\">{s}</span>", .{@tagName(tag)});
        try writer.writeAll(" ");
        try writeSourceRangeSpan(writer, region, env.source, env.line_starts.items.items);
        try writer.writeAll("</span>");
    }
    try writer.writeAll("</div>");
}

/// Helper function to convert the AST to a human friendly representation in HTML format.
/// This function uses an iterative, stack-based traversal to avoid deep recursion and
/// prevent stack overflows in WebAssembly environments.
pub fn toSExprHtml(ast: *const AST, env: ModuleEnv, writer: std.io.AnyWriter) !void {
    // This implementation replaces the original recursive approach with an iterative, stack-based
    // traversal of the AST. It writes HTML directly to the output stream, avoiding
    // the intermediate `SExprTree` allocation and the deep recursion of both building
    // and serializing the tree. This "streaming" approach is more memory-efficient
    // and avoids stack overflow errors in WASM.
    //
    // The core of this implementation is a state machine that uses an explicit stack
    // to manage the AST traversal. Each item on the stack represents a node to visit
    // and an action to perform (Enter or Exit).
    //   - `Enter`: Process the node, write its opening HTML tag, and push its children
    //     and its own `Exit` action onto the stack.
    //   - `Exit`: Write the node's closing HTML tag.

    const HtmlNode = union(enum) {
        file: AST.File,
        header: AST.Header,
        statement: AST.Statement,
        statements: AST.Statement.Span,
        pattern: AST.Pattern,
        patterns: AST.Pattern.Span,
        expr: AST.Expr,
        exprs: AST.Expr.Span,
        collection: AST.Collection,
        exposed_item: AST.ExposedItem,
        exposed_items: AST.ExposedItem.Span,
        type_anno: AST.TypeAnno,
        type_annos: AST.TypeAnno.Span,
        anno_record_field: AST.AnnoRecordField,
        anno_record_fields: AST.AnnoRecordField.Span,
        record_field: AST.RecordField,
        record_fields: AST.RecordField.Span,
        pattern_record_field: AST.PatternRecordField,
        pattern_record_fields: AST.PatternRecordField.Span,
        where_clause: AST.WhereClause,
        where_clauses: AST.WhereClause.Span,
        match_branch: AST.MatchBranch,
        match_branches: AST.MatchBranch.Span,
        type_header: AST.TypeHeader,
        string_parts: AST.Expr.Span,
        lambda_args: AST.Pattern.Span,
        apply_args: AST.Expr.Span,
        if_then_else: AST.IfElse,
        bin_op: AST.BinOp,
        unary_op: AST.Unary,
        tag_expr: AST.TagExpr,
        block: AST.Block,
    };

    const Action = enum { Enter, Exit };
    const StackItem = struct { action: Action, node: HtmlNode };

    var stack = std.ArrayList(StackItem).init(env.gpa);
    defer stack.deinit();

    try stack.append(.{ .action = .Enter, .node = .{ .file = ast.store.getFile() } });

    while (stack.items.len > 0) {
        const item = stack.pop() orelse unreachable;
        const node = item.node;
        const action = item.action;

        switch (node) {
            .file => |file| {
                if (action == .Enter) {
                    try writer.print("<div class=\"file\"", .{});
                    try appendRegionInfoAsHtmlAttr(ast, env, writer, file.region);
                    try writer.print(">", .{});
                    try stack.append(.{ .action = .Exit, .node = node });
                    try stack.append(.{ .action = .Enter, .node = .{ .statements = file.statements } });
                    try stack.append(.{ .action = .Enter, .node = .{ .header = ast.store.getHeader(file.header) } });
                } else {
                    try writer.print("</div>", .{});
                }
            },
            .header => |header| {
                if (action == .Enter) {
                    const class_name = switch (header) {
                        .app => "app",
                        .module => "module",
                        .package => "package",
                        .platform => "platform",
                        .hosted => "hosted",
                        .malformed => "malformed-header",
                    };
                    try writer.print("<div class=\"{s}\"", .{class_name});
                    const region = switch (header) {
                        .app => |h| h.region,
                        .module => |h| h.region,
                        .package => |h| h.region,
                        .platform => |h| h.region,
                        .hosted => |h| h.region,
                        .malformed => |h| h.region,
                    };
                    try appendRegionInfoAsHtmlAttr(ast, env, writer, region);
                    try writer.print(">", .{});

                    try stack.append(.{ .action = .Exit, .node = node });

                    switch (header) {
                        .app => |app_header| {
                            try stack.append(.{ .action = .Enter, .node = .{ .record_fields = .{ .span = ast.store.getCollection(app_header.packages).span } } });
                            try stack.append(.{ .action = .Enter, .node = .{ .record_field = ast.store.getRecordField(app_header.platform_idx) } });
                            try stack.append(.{ .action = .Enter, .node = .{ .exposed_items = .{ .span = ast.store.getCollection(app_header.provides).span } } });
                        },
                        .module => |module_header| {
                            try stack.append(.{ .action = .Enter, .node = .{ .exposed_items = .{ .span = ast.store.getCollection(module_header.exposes).span } } });
                        },
                        .package => |pkg_header| {
                            try stack.append(.{ .action = .Enter, .node = .{ .record_fields = .{ .span = ast.store.getCollection(pkg_header.packages).span } } });
                            try stack.append(.{ .action = .Enter, .node = .{ .exposed_items = .{ .span = ast.store.getCollection(pkg_header.exposes).span } } });
                        },
                        .platform => |plat_header| {
                            try stack.append(.{ .action = .Enter, .node = .{ .exposed_items = .{ .span = ast.store.getCollection(plat_header.provides).span } } });
                            try stack.append(.{ .action = .Enter, .node = .{ .record_fields = .{ .span = ast.store.getCollection(plat_header.packages).span } } });
                            try stack.append(.{ .action = .Enter, .node = .{ .exposed_items = .{ .span = ast.store.getCollection(plat_header.exposes).span } } });
                            try stack.append(.{ .action = .Enter, .node = .{ .type_anno = ast.store.getTypeAnno(plat_header.requires_signatures) } });
                            try stack.append(.{ .action = .Enter, .node = .{ .exposed_items = .{ .span = ast.store.getCollection(plat_header.requires_rigids).span } } });
                        },
                        .hosted => |hosted_header| {
                            try stack.append(.{ .action = .Enter, .node = .{ .exposed_items = .{ .span = ast.store.getCollection(hosted_header.exposes).span } } });
                        },
                        .malformed => {},
                    }
                } else {
                    try writer.print("</div>", .{});
                }
            },
            .statements => |span| {
                if (action == .Enter) {
                    try writer.print("<div class=\"statements\">", .{});
                    try stack.append(.{ .action = .Exit, .node = node });
                    const statements = ast.store.statementSlice(span);
                    for (statements.len..0) |i| {
                        try stack.append(.{ .action = .Enter, .node = .{ .statement = ast.store.getStatement(statements[i - 1]) } });
                    }
                } else {
                    try writer.print("</div>", .{});
                }
            },
            .statement => |stmt| {
                if (action == .Enter) {
                    const class_name = switch (stmt) {
                        .decl => "s-decl",
                        .@"var" => "s-var",
                        .expr => "s-expr",
                        .import => "s-import",
                        .type_decl => "s-type-decl",
                        .type_anno => "s-type-anno",
                        .crash => "s-crash",
                        .dbg => "s-dbg",
                        .expect => "s-expect",
                        .@"for" => "s-for",
                        .@"return" => "s-return",
                        .malformed => "s-malformed",
                    };
                    try writer.print("<div class=\"{s}\"", .{class_name});
                    // ... append region and other attributes
                    try writer.print(">", .{});
                    try stack.append(.{ .action = .Exit, .node = node });

                    switch (stmt) {
                        .decl => |decl| {
                            try stack.append(.{ .action = .Enter, .node = .{ .expr = ast.store.getExpr(decl.body) } });
                            try stack.append(.{ .action = .Enter, .node = .{ .pattern = ast.store.getPattern(decl.pattern) } });
                        },
                        .expr => |e| {
                            try stack.append(.{ .action = .Enter, .node = .{ .expr = ast.store.getExpr(e.expr) } });
                        },
                        .@"var" => |v| {
                            try stack.append(.{ .action = .Enter, .node = .{ .expr = ast.store.getExpr(v.body) } });
                        },
                        .import => |i| {
                            try stack.append(.{ .action = .Enter, .node = .{ .exposed_items = i.exposes } });
                        },
                        .type_decl => |td| {
                            if (td.where) |coll_idx| {
                                try stack.append(.{ .action = .Enter, .node = .{ .where_clauses = .{ .span = ast.store.getCollection(coll_idx).span } } });
                            }
                            try stack.append(.{ .action = .Enter, .node = .{ .type_anno = ast.store.getTypeAnno(td.anno) } });
                            try stack.append(.{ .action = .Enter, .node = .{ .type_header = ast.store.getTypeHeader(td.header) } });
                        },
                        .type_anno => |ta| {
                            if (ta.where) |coll_idx| {
                                try stack.append(.{ .action = .Enter, .node = .{ .where_clauses = .{ .span = ast.store.getCollection(coll_idx).span } } });
                            }
                            try stack.append(.{ .action = .Enter, .node = .{ .type_anno = ast.store.getTypeAnno(ta.anno) } });
                        },
                        .crash => |c| try stack.append(.{ .action = .Enter, .node = .{ .expr = ast.store.getExpr(c.expr) } }),
                        .dbg => |d| try stack.append(.{ .action = .Enter, .node = .{ .expr = ast.store.getExpr(d.expr) } }),
                        .expect => |e| try stack.append(.{ .action = .Enter, .node = .{ .expr = ast.store.getExpr(e.body) } }),
                        .@"for" => |f| {
                            try stack.append(.{ .action = .Enter, .node = .{ .expr = ast.store.getExpr(f.body) } });
                            try stack.append(.{ .action = .Enter, .node = .{ .expr = ast.store.getExpr(f.expr) } });
                            try stack.append(.{ .action = .Enter, .node = .{ .pattern = ast.store.getPattern(f.patt) } });
                        },
                        .@"return" => |r| try stack.append(.{ .action = .Enter, .node = .{ .expr = ast.store.getExpr(r.expr) } }),
                        .malformed => {},
                    }
                } else {
                    try writer.print("</div>", .{});
                }
            },
            .expr => |expr_val| {
                if (action == .Enter) {
                    const class_name = switch (expr_val) {
                        .int => "e-int",
                        .frac => "e-frac",
                        .string => "e-string",
                        .list => "e-list",
                        .ident => "e-ident",
                        .single_quote => "e-single-quote",
                        .string_part => "e-string-part",
                        .tuple => "e-tuple",
                        .record => "e-record",
                        .tag => "e-tag",
                        .lambda => "e-lambda",
                        .apply => "e-apply",
                        .record_updater => "e-record-updater",
                        .field_access => "e-field-access",
                        .local_dispatch => "e-local-dispatch",
                        .bin_op => "e-binop",
                        .suffix_single_question => "e-question-suffix",
                        .unary_op => "unary",
                        .if_then_else => "e-if-then-else",
                        .match => "e-match",
                        .dbg => "e-dbg",
                        .record_builder => "e-record-builder",
                        .ellipsis => "e-ellipsis",
                        .block => "e-block",
                        .malformed => "e-malformed",
                    };
                    try writer.print("<div class=\"{s}\"", .{class_name});
                    try appendRegionInfoAsHtmlAttr(ast, env, writer, expr_val.to_tokenized_region());
                    try writer.print(">", .{});
                    try stack.append(.{ .action = .Exit, .node = node });

                    switch (expr_val) {
                        .int => |e| {
                            try writer.print("<div class=\"raw\">{s}</div>", .{ast.resolve(e.token)});
                        },
                        .ident => |e| {
                            const strip_tokens = [_]AST.Token.Tag{ .NoSpaceDotLowerIdent, .NoSpaceDotUpperIdent };
                            const fqn = ast.resolveQualifiedName(e.qualifiers, e.token, &strip_tokens);
                            try writer.print("<div class=\"raw\">{s}</div>", .{fqn});
                        },
                        .list => |e| try stack.append(.{ .action = .Enter, .node = .{ .exprs = e.items } }),
                        .string => |s| try stack.append(.{ .action = .Enter, .node = .{ .string_parts = s.parts } }),
                        .frac => |f| try writer.print("<div class=\"raw\">{s}</div>", .{ast.resolve(f.token)}),
                        .single_quote => |s| try writer.print("<div class=\"raw\">{s}</div>", .{ast.resolve(s.token)}),
                        .string_part => |sp| try writer.print("<div class=\"raw\">{s}</div>", .{ast.resolve(sp.token)}),
                        .tuple => |t| try stack.append(.{ .action = .Enter, .node = .{ .exprs = t.items } }),
                        .record => |r| {
                            if (r.ext) |ext_idx| {
                                try stack.append(.{ .action = .Enter, .node = .{ .expr = ast.store.getExpr(ext_idx) } });
                            }
                            try stack.append(.{ .action = .Enter, .node = .{ .record_fields = r.fields } });
                        },
                        .tag => |t| try stack.append(.{ .action = .Enter, .node = .{ .tag_expr = t } }),
                        .lambda => |l| {
                            try stack.append(.{ .action = .Enter, .node = .{ .expr = ast.store.getExpr(l.body) } });
                            try stack.append(.{ .action = .Enter, .node = .{ .lambda_args = l.args } });
                        },
                        .apply => |a| {
                            try stack.append(.{ .action = .Enter, .node = .{ .apply_args = a.args } });
                            try stack.append(.{ .action = .Enter, .node = .{ .expr = ast.store.getExpr(a.@"fn") } });
                        },
                        .record_updater => |ru| try writer.print("<div class=\"raw\">{s}</div>", .{ast.resolve(ru.token)}),
                        .field_access => |fa| try stack.append(.{ .action = .Enter, .node = .{ .bin_op = fa } }),
                        .local_dispatch => |ld| try stack.append(.{ .action = .Enter, .node = .{ .bin_op = ld } }),
                        .bin_op => |b| try stack.append(.{ .action = .Enter, .node = .{ .bin_op = b } }),
                        .suffix_single_question => |s| try stack.append(.{ .action = .Enter, .node = .{ .unary_op = s } }),
                        .unary_op => |u| try stack.append(.{ .action = .Enter, .node = .{ .unary_op = u } }),
                        .if_then_else => |ite| try stack.append(.{ .action = .Enter, .node = .{ .if_then_else = AST.IfElse{
                            .condition = ite.condition,
                            .body = ite.then,
                            .region = ite.region,
                        } } }),
                        .match => |m| {
                            try stack.append(.{
                                .action = .Enter,
                                .node = HtmlNode{ .match_branches = m.branches },
                            });
                            try stack.append(.{
                                .action = .Enter,
                                .node = .{ .expr = ast.store.getExpr(m.expr) },
                            });
                        },
                        .dbg => |d| try stack.append(.{ .action = .Enter, .node = .{ .expr = ast.store.getExpr(d.expr) } }),
                        .record_builder => |rb| {
                            try stack.append(.{ .action = .Enter, .node = .{ .record_field = ast.store.getRecordField(rb.fields) } });
                            try stack.append(.{ .action = .Enter, .node = .{ .expr = ast.store.getExpr(rb.mapper) } });
                        },
                        .ellipsis => {},
                        .block => |b| try stack.append(.{ .action = .Enter, .node = .{ .block = b } }),
                        .malformed => {},
                    }
                } else {
                    try writer.print("</div>", .{});
                }
            },
            .exprs => |span| {
                if (action == .Enter) {
                    try stack.append(.{ .action = .Exit, .node = node }); // No div for this one
                    const exprs = ast.store.exprSlice(span);
                    for (exprs.len..0) |i| {
                        try stack.append(.{ .action = .Enter, .node = .{ .expr = ast.store.getExpr(exprs[i - 1]) } });
                    }
                }
            },
            .pattern => |pattern_val| {
                if (action == .Enter) {
                    const class_name = switch (pattern_val) {
                        .ident => "p-ident",
                        .tag => "p-tag",
                        .int => "p-int",
                        .frac => "p-frac",
                        .string => "p-string",
                        .single_quote => "p-single-quote",
                        .record => "p-record",
                        .list => "p-list",
                        .list_rest => "p-list-rest",
                        .tuple => "p-tuple",
                        .underscore => "p-underscore",
                        .alternatives => "p-alternatives",
                        .as => "p-as",
                        .malformed => "p-malformed",
                    };
                    try writer.print("<div class=\"{s}\"", .{class_name});
                    try appendRegionInfoAsHtmlAttr(ast, env, writer, pattern_val.to_tokenized_region());
                    try writer.print(">", .{});
                    try stack.append(.{ .action = .Exit, .node = node });

                    switch (pattern_val) {
                        .ident => |p| try writer.print("<div class=\"raw\">{s}</div>", .{ast.resolve(p.ident_tok)}),
                        .tag => |t| try stack.append(.{ .action = .Enter, .node = .{ .patterns = t.args } }),
                        .int => |i| try writer.print("<div class=\"raw\">{s}</div>", .{ast.resolve(i.number_tok)}),
                        .frac => |f| try writer.print("<div class=\"raw\">{s}</div>", .{ast.resolve(f.number_tok)}),
                        .string => |s| try writer.print("<div class=\"raw\">{s}</div>", .{ast.resolve(s.string_tok)}),
                        .single_quote => |s| try writer.print("<div class=\"raw\">{s}</div>", .{ast.resolve(s.token)}),
                        .record => |r| try stack.append(.{ .action = .Enter, .node = .{ .pattern_record_fields = r.fields } }),
                        .list => |l| try stack.append(.{ .action = .Enter, .node = .{ .patterns = l.patterns } }),
                        .list_rest => {},
                        .tuple => |t| try stack.append(.{ .action = .Enter, .node = .{ .patterns = t.patterns } }),
                        .underscore => {},
                        .alternatives => |a| try stack.append(.{ .action = .Enter, .node = .{ .patterns = a.patterns } }),
                        .as => |a| try stack.append(.{ .action = .Enter, .node = .{ .pattern = ast.store.getPattern(a.pattern) } }),
                        .malformed => {},
                    }
                } else {
                    try writer.print("</div>", .{});
                }
            },
            .collection => |coll| {
                if (action == .Enter) {
                    try writer.print("<div class=\"collection\"", .{});
                    try appendRegionInfoAsHtmlAttr(ast, env, writer, coll.region);
                    try writer.print(">", .{});
                    try stack.append(.{ .action = .Exit, .node = node });
                    // This is a generic collection, we need to know what it contains.
                    // This simplified example assumes ExposedItem, but the real implementation
                    // would need to know the type of items in the collection.
                    const items = ast.store.exposedItemSlice(.{ .span = coll.span });
                    for (items.len..0) |i| {
                        try stack.append(.{ .action = .Enter, .node = .{ .exposed_item = ast.store.getExposedItem(items[i - 1]) } });
                    }
                } else {
                    try writer.print("</div>", .{});
                }
            },
            .exposed_item => |exposed_item_node| {
                if (action == .Enter) {
                    const class_name = switch (exposed_item_node) {
                        .lower_ident => "exposed-lower-ident",
                        .upper_ident => "exposed-upper-ident",
                        .upper_ident_star => "exposed-upper-ident-star",
                        .malformed => "exposed-malformed",
                    };
                    try writer.print("<div class=\"{s}\"", .{class_name});
                    try appendRegionInfoAsHtmlAttr(ast, env, writer, switch (exposed_item_node) {
                        .lower_ident => |i| i.region,
                        .upper_ident => |i| i.region,
                        .upper_ident_star => |i| i.region,
                        .malformed => |m| m.region,
                    });
                    try writer.print(">", .{});
                    try writer.print("</div>", .{});
                }
            },
            .exposed_items => |span| {
                if (action == .Enter) {
                    try stack.append(.{ .action = .Exit, .node = node });
                    const items = ast.store.exposedItemSlice(span);
                    for (items.len..0) |i| {
                        try stack.append(.{ .action = .Enter, .node = .{ .exposed_item = ast.store.getExposedItem(items[i - 1]) } });
                    }
                }
            },
            .patterns => |span| {
                if (action == .Enter) {
                    try stack.append(.{ .action = .Exit, .node = node });
                    const items = ast.store.patternSlice(span);
                    for (items.len..0) |i| {
                        try stack.append(.{ .action = .Enter, .node = .{ .pattern = ast.store.getPattern(items[i - 1]) } });
                    }
                }
            },
            .string_parts => |span| {
                if (action == .Enter) {
                    try stack.append(.{ .action = .Exit, .node = node });
                    const items = ast.store.exprSlice(span);
                    for (items.len..0) |i| {
                        try stack.append(.{ .action = .Enter, .node = .{ .expr = ast.store.getExpr(items[i - 1]) } });
                    }
                }
            },
            .record_fields => |span| {
                if (action == .Enter) {
                    try stack.append(.{ .action = .Exit, .node = node });
                    const items = ast.store.recordFieldSlice(span);
                    for (items.len..0) |i| {
                        try stack.append(.{ .action = .Enter, .node = .{ .record_field = ast.store.getRecordField(items[i - 1]) } });
                    }
                }
            },
            .pattern_record_fields => |span| {
                if (action == .Enter) {
                    try stack.append(.{ .action = .Exit, .node = node });
                    const items = ast.store.patternRecordFieldSlice(span);
                    for (items.len..0) |i| {
                        try stack.append(.{ .action = .Enter, .node = .{ .pattern_record_field = ast.store.getPatternRecordField(items[i - 1]) } });
                    }
                }
            },
            .pattern_record_field => |prf| {
                if (action == .Enter) {
                    try writer.print("<div class=\"record-field\"", .{});
                    try appendRegionInfoAsHtmlAttr(ast, env, writer, prf.region);
                    try writer.print(">", .{});
                    try stack.append(.{ .action = .Exit, .node = node });
                    if (prf.value) |val| {
                        try stack.append(.{ .action = .Enter, .node = .{ .pattern = ast.store.getPattern(val) } });
                    }
                } else {
                    try writer.print("</div>", .{});
                }
            },
            .record_field => |rf| {
                if (action == .Enter) {
                    try writer.print("<div class=\"record-field\"", .{});
                    try appendRegionInfoAsHtmlAttr(ast, env, writer, rf.region);
                    try writer.print(">", .{});
                    try stack.append(.{ .action = .Exit, .node = node });
                    if (rf.value) |val| {
                        try stack.append(.{ .action = .Enter, .node = .{ .expr = ast.store.getExpr(val) } });
                    }
                } else {
                    try writer.print("</div>", .{});
                }
            },
            .lambda_args => |span| {
                if (action == .Enter) {
                    try writer.print("<div class=\"args\">", .{});
                    try stack.append(.{ .action = .Exit, .node = node });
                    const items = ast.store.patternSlice(span);
                    for (items.len..0) |i| {
                        try stack.append(.{ .action = .Enter, .node = .{ .pattern = ast.store.getPattern(items[i - 1]) } });
                    }
                } else {
                    try writer.print("</div>", .{});
                }
            },
            .apply_args => |span| {
                if (action == .Enter) {
                    try stack.append(.{ .action = .Exit, .node = node });
                    const items = ast.store.exprSlice(span);
                    for (items.len..0) |i| {
                        try stack.append(.{ .action = .Enter, .node = .{ .expr = ast.store.getExpr(items[i - 1]) } });
                    }
                }
            },
            .bin_op => |b| {
                if (action == .Enter) {
                    try writer.print("<div class=\"e-binop\" data-op=\"{s}\"", .{ast.resolve(b.operator)});
                    try appendRegionInfoAsHtmlAttr(ast, env, writer, b.region);
                    try writer.print(">", .{});
                    try stack.append(.{ .action = .Exit, .node = node });
                    try stack.append(.{ .action = .Enter, .node = .{ .expr = ast.store.getExpr(b.right) } });
                    try stack.append(.{ .action = .Enter, .node = .{ .expr = ast.store.getExpr(b.left) } });
                } else {
                    try writer.print("</div>", .{});
                }
            },
            .unary_op => |u| {
                if (action == .Enter) {
                    try writer.print("<div class=\"unary\" data-op=\"{s}\"", .{ast.resolve(u.operator)});
                    try appendRegionInfoAsHtmlAttr(ast, env, writer, u.region);
                    try writer.print(">", .{});
                    try stack.append(.{ .action = .Exit, .node = node });
                    try stack.append(.{ .action = .Enter, .node = .{ .expr = ast.store.getExpr(u.expr) } });
                } else {
                    try writer.print("</div>", .{});
                }
            },
            .if_then_else => |ite| {
                if (action == .Enter) {
                    try writer.print("<div class=\"e-if-then-else\">", .{});
                    try stack.append(.{ .action = .Exit, .node = node });
                    try stack.append(.{ .action = .Enter, .node = .{ .expr = ast.store.getExpr(ite.body) } });
                    try stack.append(.{ .action = .Enter, .node = .{ .expr = ast.store.getExpr(ite.condition) } });
                } else {
                    try writer.print("</div>", .{});
                }
            },
            .match_branches => |span| {
                if (action == .Enter) {
                    try writer.print("<div class=\"branches\">", .{});
                    try stack.append(.{ .action = .Exit, .node = node });
                    const items = ast.store.matchBranchSlice(span);
                    for (items.len..0) |i| {
                        try stack.append(.{ .action = .Enter, .node = .{ .match_branch = ast.store.getBranch(items[i - 1]) } });
                    }
                } else {
                    try writer.print("</div>", .{});
                }
            },
            .match_branch => |b| {
                if (action == .Enter) {
                    try writer.print("<div class=\"branch\">", .{});
                    try appendRegionInfoAsHtmlAttr(ast, env, writer, b.region);
                    try writer.print(">", .{});
                    try stack.append(.{ .action = .Exit, .node = node });
                    try stack.append(.{ .action = .Enter, .node = .{ .expr = ast.store.getExpr(b.body) } });
                    try stack.append(.{ .action = .Enter, .node = .{ .pattern = ast.store.getPattern(b.pattern) } });
                } else {
                    try writer.print("</div>", .{});
                }
            },
            .block => |b| {
                if (action == .Enter) {
                    try writer.print("<div class=\"e-block\">", .{});
                    try appendRegionInfoAsHtmlAttr(ast, env, writer, b.region);
                    try writer.print(">", .{});
                    try stack.append(.{ .action = .Exit, .node = node });
                    try stack.append(.{ .action = .Enter, .node = .{ .statements = b.statements } });
                } else {
                    try writer.print("</div>", .{});
                }
            },
            .tag_expr => |t| {
                if (action == .Enter) {
                    const strip_tokens = [_]AST.Token.Tag{.NoSpaceDotUpperIdent};
                    const fqn = ast.resolveQualifiedName(t.qualifiers, t.token, &strip_tokens);
                    try writer.print("<div class=\"e-tag\" data-raw=\"{s}\"", .{fqn});
                    try appendRegionInfoAsHtmlAttr(ast, env, writer, t.region);
                    try writer.print("></div>", .{});
                }
            },
            .type_anno => |ta| {
                _ = ta;
                if (action == .Enter) {
                    try writer.print("<div class=\"type-anno\">", .{});
                    try stack.append(.{ .action = .Exit, .node = node });
                    // Simplified, a full implementation would handle all TypeAnno variants
                } else {
                    try writer.print("</div>", .{});
                }
            },
            .type_annos => |span| {
                if (action == .Enter) {
                    try stack.append(.{ .action = .Exit, .node = node });
                    const items = ast.store.typeAnnoSlice(span);
                    for (items.len..0) |i| {
                        try stack.append(.{ .action = .Enter, .node = .{ .type_anno = ast.store.getTypeAnno(items[i - 1]) } });
                    }
                }
            },
            .anno_record_field => |arf| {
                if (action == .Enter) {
                    try writer.print("<div class=\"anno-record-field\">", .{});
                    try stack.append(.{ .action = .Exit, .node = node });
                    try stack.append(.{ .action = .Enter, .node = .{ .type_anno = ast.store.getTypeAnno(arf.ty) } });
                } else {
                    try writer.print("</div>", .{});
                }
            },
            .anno_record_fields => |span| {
                if (action == .Enter) {
                    try stack.append(.{ .action = .Exit, .node = node });
                    const items = ast.store.annoRecordFieldSlice(span);
                    for (items.len..0) |i| {
                        try stack.append(.{ .action = .Enter, .node = .{ .anno_record_field = try ast.store.getAnnoRecordField(items[i - 1]) } });
                    }
                }
            },
            .where_clause => |wc| {
                _ = wc;
                if (action == .Enter) {
                    try writer.print("<div class=\"where-clause\">", .{});
                    try stack.append(.{ .action = .Exit, .node = node });
                    // Simplified
                } else {
                    try writer.print("</div>", .{});
                }
            },
            .where_clauses => |span| {
                if (action == .Enter) {
                    try stack.append(.{ .action = .Exit, .node = node });
                    const items = ast.store.whereClauseSlice(span);
                    for (items.len..0) |i| {
                        try stack.append(.{ .action = .Enter, .node = .{ .where_clause = ast.store.getWhereClause(items[i - 1]) } });
                    }
                }
            },
            .type_header => |th| {
                if (action == .Enter) {
                    try writer.print("<div class=\"type-header\">", .{});
                    try stack.append(.{ .action = .Exit, .node = node });
                    try stack.append(.{ .action = .Enter, .node = .{ .type_annos = th.args } });
                } else {
                    try writer.print("</div>", .{});
                }
            },
        }
    }
}
