//! Formatting logic for Roc modules.

const std = @import("std");
const IR = @import("check/parse/IR.zig");
const Node = IR.Node;
const tokenizer = @import("check/parse/tokenize.zig");
const TokenizedBuffer = tokenizer.TokenizedBuffer;
const TokenIdx = tokenizer.Token.Idx;
const exitOnOom = @import("./collections/utils.zig").exitOnOom;
const base = @import("base.zig");

const NodeStore = IR.NodeStore;
const ExprIdx = NodeStore.ExprIdx;
const PatternIdx = NodeStore.PatternIdx;
const HeaderIdx = NodeStore.HeaderIdx;
const StatementIdx = NodeStore.StatementIdx;

const FormatFlags = enum { debug_binop, no_debug };

ast: IR,
gpa: std.mem.Allocator,
buffer: std.ArrayListUnmanaged(u8) = .{},
curr_indent: u32 = 0,
flags: FormatFlags = .no_debug,

const Formatter = @This();

/// Creates a new Formatter for the given parse IR.
pub fn init(ast: IR) Formatter {
    return .{
        .ast = ast,
        .gpa = ast.store.gpa,
        .buffer = std.ArrayListUnmanaged(u8).initCapacity(ast.store.gpa, ast.tokens.tokens.len * 15) catch |err| exitOnOom(err),
    };
}

/// Deinits all data owned by the formatter object.
pub fn deinit(fmt: *Formatter) void {
    fmt.buffer.deinit(fmt.gpa);
}

/// Sets the Formatter up fresh to work on a new parse IR.
pub fn resetWith(fmt: *Formatter, ast: IR) void {
    fmt.curr_indent = 0;
    fmt.buffer.shrinkRetainingCapacity(0);
    fmt.ast = ast;
}

/// Emits a string containing the well-formed source of a Roc parse IR (AST).
/// The resulting string is owned by the caller.
pub fn formatFile(fmt: *Formatter) []const u8 {
    fmt.ast.store.emptyScratch();
    const file = fmt.ast.store.getFile();
    const header_region = fmt.ast.store.nodes.items.items(.region)[file.header.id];
    _ = fmt.flushCommentsBefore(header_region.start);
    fmt.formatHeader(file.header);
    const statement_slice = fmt.ast.store.statementSlice(file.statements);
    for (statement_slice) |s| {
        const region = fmt.nodeRegion(s.id);
        _ = fmt.flushCommentsBefore(region.start);
        _ = fmt.formatStatement(s);
    }
    return fmt.buffer.toOwnedSlice(fmt.gpa) catch |err| exitOnOom(err);
}

const NewlineBehavior = enum { no_extra_newline, extra_newline_needed };

fn formatStatement(fmt: *Formatter, si: StatementIdx) NewlineBehavior {
    const statement = fmt.ast.store.getStatement(si);
    const node_region = fmt.nodeRegion(si.id);
    const multiline = fmt.isRegionMultiline(node_region);
    var flushed = false;
    const orig_indent = fmt.curr_indent;
    defer {
        fmt.curr_indent = orig_indent;
    }
    switch (statement) {
        .decl => |d| {
            const pattern_region = fmt.nodeRegion(d.pattern.id);
            _ = fmt.formatPattern(d.pattern);
            if (multiline and fmt.flushCommentsAfter(pattern_region.end)) {
                fmt.curr_indent += 1;
                fmt.ensureNewline();
                fmt.pushIndent();
                fmt.push('=');
            } else {
                fmt.pushAll(" = ");
            }
            const body_region = fmt.nodeRegion(d.body.id);
            if (multiline and fmt.flushCommentsBefore(body_region.start)) {
                fmt.curr_indent += 1;
                fmt.pushIndent();
            }
            _ = fmt.formatExpr(d.body);
            return .extra_newline_needed;
        },
        .expr => |e| {
            _ = fmt.formatExpr(e.expr);
            return .extra_newline_needed;
        },
        .import => |i| {
            fmt.pushAll("import");
            if (multiline) {
                flushed = fmt.flushCommentsBefore(if (i.qualifier_tok) |q| q else i.module_name_tok);
            }
            if (!flushed) {
                fmt.push(' ');
            } else {
                fmt.curr_indent += 1;
                fmt.pushIndent();
            }
            fmt.formatIdent(i.module_name_tok, i.qualifier_tok);
            if (multiline) {
                flushed = fmt.flushCommentsAfter(i.module_name_tok);
            }
            if (flushed) {
                fmt.curr_indent += 1;
                fmt.pushIndent();
            }
            if (i.alias_tok) |a| {
                if (multiline) {
                    fmt.pushAll("as");
                    flushed = fmt.flushCommentsBefore(a);
                    if (!flushed) {
                        fmt.push(' ');
                    } else {
                        fmt.pushIndent();
                    }
                } else {
                    fmt.pushAll(" as ");
                }
                fmt.pushTokenText(a);
                if (i.exposes.span.len > 0) {
                    flushed = fmt.flushCommentsAfter(a);
                }
            }
            if (i.exposes.span.len > 0) {
                if (flushed) {
                    fmt.pushAll("exposing ");
                } else {
                    fmt.pushAll(" exposing ");
                }
                const items = fmt.ast.store.exposedItemSlice(i.exposes);
                const items_region = fmt.regionInSlice(IR.NodeStore.ExposedItemIdx, items);
                // This is a near copy of formatCollection because to make that function
                // work correctly, the exposed items have to be in a new Node type that
                // will have its own region
                const items_multiline = fmt.ast.regionIsMultiline(items_region);
                const braces = Braces.square;
                fmt.push(braces.start());
                if (items.len == 0) {
                    fmt.push(braces.end());
                } else {
                    if (items_multiline) {
                        fmt.curr_indent += 1;
                    }
                    var x: usize = 0;
                    var arg_region = fmt.nodeRegion(items[0].id);
                    for (items) |item| {
                        arg_region = fmt.nodeRegion(item.id);
                        if (items_multiline) {
                            _ = fmt.flushCommentsBefore(arg_region.start);
                            fmt.pushIndent();
                        }
                        _ = fmt.formatExposedItem(item);
                        if (!items_multiline and x < (items.len - 1)) {
                            fmt.pushAll(", ");
                        }
                        if (items_multiline) {
                            fmt.push(',');
                        }
                        x += 1;
                    }
                    if (items_multiline) {
                        _ = fmt.flushCommentsAfter(arg_region.end);
                        fmt.curr_indent -= 1;
                        fmt.pushIndent();
                    }
                    fmt.push(braces.end());
                }
            }
            return .extra_newline_needed;
        },
        .type_decl => |d| {
            fmt.formatTypeHeader(d.header);
            fmt.pushAll(" : ");
            _ = fmt.formatTypeAnno(d.anno);
            return .extra_newline_needed;
        },
        .type_anno => |t| {
            fmt.pushTokenText(t.name);
            fmt.pushAll(" : ");
            _ = fmt.formatTypeAnno(t.anno);
            return .no_extra_newline;
        },
        .expect => |e| {
            fmt.pushAll("expect");
            const body_region = fmt.nodeRegion(e.body.id);
            if (multiline and fmt.flushCommentsBefore(body_region.start)) {
                fmt.curr_indent += 1;
                fmt.pushIndent();
            } else {
                fmt.push(' ');
            }
            _ = fmt.formatExpr(e.body);
            return .extra_newline_needed;
        },
        .crash => |c| {
            fmt.pushAll("crash");
            const body_region = fmt.nodeRegion(c.expr.id);
            if (multiline and fmt.flushCommentsBefore(body_region.start)) {
                fmt.curr_indent += 1;
                fmt.pushIndent();
            } else {
                fmt.push(' ');
            }
            _ = fmt.formatExpr(c.expr);
            return .extra_newline_needed;
        },
        .@"return" => |r| {
            fmt.pushAll("return");
            const body_region = fmt.nodeRegion(r.expr.id);
            if (multiline and fmt.flushCommentsBefore(body_region.start)) {
                fmt.curr_indent += 1;
                fmt.pushIndent();
            } else {
                fmt.push(' ');
            }
            _ = fmt.formatExpr(r.expr);
            return .extra_newline_needed;
        },
    }
}

fn formatIdent(fmt: *Formatter, ident: TokenIdx, qualifier: ?TokenIdx) void {
    const curr_indent = fmt.curr_indent;
    defer {
        fmt.curr_indent = curr_indent;
    }
    if (qualifier) |q| {
        const multiline = fmt.isRegionMultiline(IR.Region{ .start = q, .end = ident });
        fmt.pushTokenText(q);
        if (multiline and fmt.flushCommentsAfter(q)) {
            fmt.curr_indent += 1;
            fmt.pushIndent();
        }
        const ident_tag = fmt.ast.tokens.tokens.items(.tag)[ident];
        if (ident_tag == .NoSpaceDotUpperIdent or ident_tag == .NoSpaceDotLowerIdent) {
            fmt.push('.');
        }
    }
    fmt.pushTokenText(ident);
}

const Braces = enum {
    round,
    square,
    curly,
    bar,

    fn start(b: Braces) u8 {
        return switch (b) {
            .round => '(',
            .square => '[',
            .curly => '{',
            .bar => '|',
        };
    }

    fn end(b: Braces) u8 {
        return switch (b) {
            .round => ')',
            .square => ']',
            .curly => '}',
            .bar => '|',
        };
    }
};

fn formatCollection(fmt: *Formatter, region: IR.Region, braces: Braces, comptime T: type, items: []T, formatter: fn (*Formatter, T) IR.Region) void {
    const multiline = fmt.ast.regionIsMultiline(region);
    const curr_indent = fmt.curr_indent;
    defer {
        fmt.curr_indent = curr_indent;
    }
    fmt.push(braces.start());
    if (items.len == 0) {
        fmt.push(braces.end());
        return;
    }
    if (multiline) {
        fmt.curr_indent += 1;
    } else if (braces == .curly) {
        fmt.push(' ');
    }
    var i: usize = 0;
    for (items) |item| {
        const item_region = fmt.nodeRegion(item.id);
        if (multiline and fmt.flushCommentsBefore(item_region.start)) {
            fmt.ensureNewline();
            fmt.pushIndent();
        } else if (multiline) {
            fmt.newline();
            fmt.pushIndent();
        }
        _ = formatter(fmt, item);
        if (!multiline and i < (items.len - 1)) {
            fmt.pushAll(", ");
        }
        if (multiline) {
            fmt.push(',');
        }
        i += 1;
    }
    if (multiline) {
        _ = fmt.flushCommentsBefore(region.end);
        fmt.curr_indent -= 1;
        fmt.ensureNewline();
        fmt.pushIndent();
    } else if (braces == .curly) {
        fmt.push(' ');
    }
    fmt.push(braces.end());
}

fn formatRecordField(fmt: *Formatter, idx: IR.NodeStore.RecordFieldIdx) IR.Region {
    const field = fmt.ast.store.getRecordField(idx);
    fmt.pushTokenText(field.name);
    if (field.value) |v| {
        fmt.pushAll(if (field.optional) "? " else ": ");
        _ = fmt.formatExpr(v);
    }

    return field.region;
}

fn formatExpr(fmt: *Formatter, ei: ExprIdx) IR.Region {
    const expr = fmt.ast.store.getExpr(ei);
    const region = fmt.nodeRegion(ei.id);
    const multiline = fmt.isRegionMultiline(region);
    switch (expr) {
        .apply => |a| {
            _ = fmt.formatExpr(a.@"fn");
            fmt.formatCollection(region, .round, IR.NodeStore.ExprIdx, fmt.ast.store.exprSlice(a.args), Formatter.formatExpr);
        },
        .string_part => |s| {
            fmt.pushTokenText(s.token);
        },
        .string => |s| {
            fmt.push('"');
            var i: usize = 0;
            for (fmt.ast.store.exprSlice(s.parts)) |idx| {
                const e = fmt.ast.store.getExpr(idx);
                switch (e) {
                    .string_part => |str| {
                        fmt.pushTokenText(str.token);
                    },
                    else => {
                        fmt.pushAll("${");
                        const part_region = fmt.nodeRegion(idx.id);
                        const part_is_multiline = fmt.isRegionMultiline(part_region);
                        if (part_is_multiline) {
                            fmt.newline();
                            fmt.curr_indent += 1;
                            fmt.pushIndent();
                        }
                        _ = fmt.formatExpr(idx);
                        if (part_is_multiline) {
                            fmt.newline();
                            fmt.curr_indent -= 1;
                            fmt.pushIndent();
                        }
                        fmt.push('}');
                    },
                }
                i += 1;
            }
            fmt.push('"');
        },
        .ident => |i| {
            fmt.formatIdent(i.token, i.qualifier);
        },
        .field_access => |fa| {
            _ = fmt.formatExpr(fa.left);
            fmt.push('.');
            _ = fmt.formatExpr(fa.right);
        },
        .int => |i| {
            fmt.pushTokenText(i.token);
        },
        .list => |l| {
            fmt.formatCollection(region, .square, IR.NodeStore.ExprIdx, fmt.ast.store.exprSlice(l.items), Formatter.formatExpr);
        },
        .tuple => |t| {
            fmt.formatCollection(region, .round, IR.NodeStore.ExprIdx, fmt.ast.store.exprSlice(t.items), Formatter.formatExpr);
        },
        .record => |r| {
            fmt.formatCollection(region, .curly, IR.NodeStore.RecordFieldIdx, fmt.ast.store.recordFieldSlice(r.fields), Formatter.formatRecordField);
        },
        .lambda => |l| {
            const args = fmt.ast.store.patternSlice(l.args);
            const args_region = fmt.regionInSlice(IR.NodeStore.PatternIdx, args);
            fmt.formatCollection(args_region, .bar, IR.NodeStore.PatternIdx, args, Formatter.formatPattern);
            fmt.push(' ');
            _ = fmt.formatExpr(l.body);
        },
        .unary_op => |op| {
            fmt.pushTokenText(op.operator);
            _ = fmt.formatExpr(op.expr);
        },
        .bin_op => |op| {
            if (fmt.flags == .debug_binop) {
                fmt.push('(');
                if (multiline) {
                    fmt.newline();
                    fmt.curr_indent += 1;
                    fmt.pushIndent();
                }
            }
            _ = fmt.formatExpr(op.left);
            var pushed = false;
            if (multiline and fmt.flushCommentsBefore(op.operator)) {
                fmt.curr_indent += 1;
                fmt.pushIndent();
                pushed = true;
            } else {
                fmt.push(' ');
            }
            fmt.pushTokenText(op.operator);
            const right_region = fmt.nodeRegion(op.right.id);
            if (multiline and fmt.flushCommentsBefore(right_region.start)) {
                fmt.curr_indent += if (pushed) 0 else 1;
                fmt.pushIndent();
            } else {
                fmt.push(' ');
            }
            _ = fmt.formatExpr(op.right);
            if (fmt.flags == .debug_binop) {
                if (multiline) {
                    fmt.curr_indent -= 1;
                    fmt.pushIndent();
                }
                fmt.push(')');
            }
        },
        .suffix_single_question => |s| {
            _ = fmt.formatExpr(s.expr);
            fmt.push('?');
        },
        .tag => |t| {
            fmt.pushTokenText(t.token);
        },
        .if_then_else => |i| {
            fmt.pushAll("if ");
            _ = fmt.formatExpr(i.condition);
            fmt.push(' ');
            _ = fmt.formatExpr(i.then);
            fmt.pushAll(" else ");
            _ = fmt.formatExpr(i.@"else");
        },
        .match => |m| {
            fmt.pushAll("match ");
            _ = fmt.formatExpr(m.expr);
            fmt.pushAll(" {");
            const curr_indent = fmt.curr_indent;
            defer {
                fmt.curr_indent = curr_indent;
            }
            fmt.curr_indent += 1;
            const branch_indent = fmt.curr_indent;
            const branches = fmt.ast.store.whenBranchSlice(m.branches);
            if (branches.len == 0) {
                fmt.push('}');
                return region;
            }
            var branch_region = fmt.nodeRegion(branches[0].id);
            for (branches) |b| {
                fmt.curr_indent = branch_indent;
                branch_region = fmt.nodeRegion(b.id);
                const branch = fmt.ast.store.getBranch(b);
                _ = fmt.flushCommentsBefore(branch_region.start);
                fmt.pushIndent();
                const pattern_region = fmt.formatPattern(branch.pattern);
                var flushed = fmt.flushCommentsAfter(pattern_region.end);
                if (flushed) {
                    fmt.curr_indent += 1;
                    fmt.pushIndent();
                    fmt.pushAll("->");
                } else {
                    fmt.pushAll(" ->");
                }
                const body_region = fmt.nodeRegion(branch.body.id);
                flushed = fmt.flushCommentsBefore(body_region.start);
                if (flushed) {
                    fmt.curr_indent += 1;
                    fmt.pushIndent();
                } else {
                    fmt.push(' ');
                }
                _ = fmt.formatExpr(branch.body);
            }
            fmt.curr_indent -= 1;
            fmt.newline();
            fmt.pushIndent();
            fmt.push('}');
        },
        .dbg => |d| {
            fmt.pushAll("dbg ");
            _ = fmt.formatExpr(d.expr);
        },
        .block => |b| {
            fmt.formatBody(b);
        },
        .ellipsis => |_| {
            fmt.pushAll("...");
        },
        else => {
            std.debug.panic("TODO: Handle formatting {s}", .{@tagName(expr)});
        },
    }
    return region;
}

fn formatPatternRecordField(fmt: *Formatter, idx: IR.NodeStore.PatternRecordFieldIdx) IR.Region {
    const field = fmt.ast.store.getPatternRecordField(idx);
    if (field.rest) {
        fmt.pushAll("..");
        if (field.name != 0) {
            fmt.pushTokenText(field.name);
        }
    } else {
        fmt.pushTokenText(field.name);
        if (field.value) |v| {
            fmt.pushAll(": ");
            _ = fmt.formatPattern(v);
        }
    }
    return field.region;
}

fn formatPattern(fmt: *Formatter, pi: PatternIdx) IR.Region {
    const pattern = fmt.ast.store.getPattern(pi);
    var region = IR.Region{ .start = 0, .end = 0 };
    switch (pattern) {
        .ident => |i| {
            region = i.region;
            fmt.formatIdent(i.ident_tok, null);
        },
        .tag => |t| {
            region = t.region;
            fmt.formatIdent(t.tag_tok, null);
            if (t.args.span.len > 0) {
                fmt.formatCollection(region, .round, IR.NodeStore.PatternIdx, fmt.ast.store.patternSlice(t.args), Formatter.formatPattern);
            }
        },
        .string => |s| {
            region = s.region;
            _ = fmt.formatExpr(s.expr);
        },
        .number => |n| {
            region = n.region;
            fmt.formatIdent(n.number_tok, null);
        },
        .record => |r| {
            region = r.region;
            fmt.formatCollection(region, .curly, IR.NodeStore.PatternRecordFieldIdx, fmt.ast.store.patternRecordFieldSlice(r.fields), Formatter.formatPatternRecordField);
        },
        .list => |l| {
            region = l.region;
            fmt.formatCollection(region, .square, IR.NodeStore.PatternIdx, fmt.ast.store.patternSlice(l.patterns), Formatter.formatPattern);
        },
        .tuple => |t| {
            region = t.region;
            fmt.formatCollection(region, .round, IR.NodeStore.PatternIdx, fmt.ast.store.patternSlice(t.patterns), Formatter.formatPattern);
        },
        .list_rest => |r| {
            region = r.region;
            fmt.pushAll("..");
            if (r.name) |n| {
                fmt.pushAll(" as ");
                fmt.pushTokenText(n);
            }
        },
        .underscore => |u| {
            region = u.region;
            fmt.push('_');
        },
        .alternatives => |a| {
            region = a.region;
            var i: usize = 0;
            for (fmt.ast.store.patternSlice(a.patterns)) |p| {
                _ = fmt.formatPattern(p);
                if (i < (a.patterns.span.len - 1)) {
                    fmt.pushAll(" | ");
                }
                i += 1;
            }
        },
    }
    return region;
}

fn formatExposedItem(fmt: *Formatter, idx: IR.NodeStore.ExposedItemIdx) IR.Region {
    const item = fmt.ast.store.getExposedItem(idx);
    var region = IR.Region{ .start = 0, .end = 0 };
    switch (item) {
        .lower_ident => |i| {
            region = i.region;
            fmt.pushTokenText(i.ident);
            if (i.as) |a| {
                fmt.pushAll(" as ");
                fmt.pushTokenText(a);
            }
        },
        .upper_ident => |i| {
            region = i.region;
            fmt.pushTokenText(i.ident);
            if (i.as) |a| {
                fmt.pushAll(" as ");
                fmt.pushTokenText(a);
            }
        },
        .upper_ident_star => |i| {
            region = i.region;
            fmt.pushTokenText(i.ident);
            fmt.pushAll(".*");
        },
    }

    return region;
}

fn formatHeader(fmt: *Formatter, hi: HeaderIdx) void {
    const header = fmt.ast.store.getHeader(hi);
    switch (header) {
        .app => |a| {
            fmt.pushAll("app ");
            const provides = fmt.ast.store.exposedItemSlice(a.provides);
            const provides_region = fmt.regionInSlice(IR.NodeStore.ExposedItemIdx, provides);
            fmt.formatCollection(provides_region, .square, IR.NodeStore.ExposedItemIdx, provides, Formatter.formatExposedItem);
            fmt.pushAll(" { ");
            fmt.pushTokenText(a.platform_name);
            fmt.pushAll(": platform ");
            _ = fmt.formatExpr(a.platform);
            if (a.packages.span.len > 0) {
                fmt.push(',');
            }
            var i: usize = 0;
            for (fmt.ast.store.recordFieldSlice(a.packages)) |package| {
                const field = fmt.ast.store.getRecordField(package);
                fmt.pushTokenText(field.name);
                if (field.value) |v| {
                    fmt.pushAll(": ");
                    _ = fmt.formatExpr(v);
                }
                if (i < a.packages.span.len) {
                    fmt.pushAll(", ");
                }
                i += 1;
            }
            fmt.pushAll(" }");
        },
        .module => |m| {
            fmt.pushAll("module ");
            fmt.formatCollection(m.region, .square, IR.NodeStore.ExposedItemIdx, fmt.ast.store.exposedItemSlice(m.exposes), Formatter.formatExposedItem);
        },
        else => {
            std.debug.panic("TODO: Handle formatting {s}", .{@tagName(header)});
        },
    }
}

fn nodeRegion(fmt: *Formatter, idx: u32) IR.Region {
    return fmt.ast.store.nodes.items.items(.region)[idx];
}

fn formatBody(fmt: *Formatter, body: IR.NodeStore.Body) void {
    const multiline = fmt.ast.regionIsMultiline(body.region);
    if (multiline or body.statements.span.len > 1) {
        fmt.curr_indent += 1;
        fmt.buffer.append(fmt.gpa, '{') catch |err| exitOnOom(err);
        for (fmt.ast.store.statementSlice(body.statements)) |s| {
            const region = fmt.nodeRegion(s.id);
            _ = fmt.flushCommentsBefore(region.start);
            fmt.ensureNewline();
            fmt.pushIndent();
            _ = fmt.formatStatement(s);
        }
        fmt.ensureNewline();
        fmt.curr_indent -= 1;
        fmt.pushIndent();
        fmt.buffer.append(fmt.gpa, '}') catch |err| exitOnOom(err);
    } else if (body.statements.span.len == 1) {
        for (fmt.ast.store.statementSlice(body.statements)) |s| {
            _ = fmt.formatStatement(s);
        }
    } else {
        fmt.pushAll("{}");
    }
}

fn formatTypeHeader(fmt: *Formatter, header: IR.NodeStore.TypeHeaderIdx) void {
    const h = fmt.ast.store.getTypeHeader(header);
    fmt.pushTokenText(h.name);
    if (h.args.span.len > 0) {
        fmt.push(' ');
        var i: usize = 0;
        for (fmt.ast.store.tokenSlice(h.args)) |arg| {
            fmt.pushTokenText(arg);
            if (i < (h.args.span.len - 1)) {
                fmt.push(' ');
            }
            i += 1;
        }
    }
}

fn formatAnnoRecordField(fmt: *Formatter, idx: IR.NodeStore.AnnoRecordFieldIdx) IR.Region {
    const field = fmt.ast.store.getAnnoRecordField(idx);
    fmt.pushTokenText(field.name);
    fmt.pushAll(" : ");
    _ = fmt.formatTypeAnno(field.ty);
    return field.region;
}

fn formatTypeAnno(fmt: *Formatter, anno: IR.NodeStore.TypeAnnoIdx) IR.Region {
    const a = fmt.ast.store.getTypeAnno(anno);
    var region = IR.Region{ .start = 0, .end = 0 };
    switch (a) {
        .ty_var => |v| {
            region = v.region;
            fmt.pushTokenText(v.tok);
        },
        .tag => |t| {
            region = t.region;
            fmt.pushTokenText(t.tok);
            if (t.args.span.len > 0) {
                fmt.formatCollection(t.region, .round, IR.NodeStore.TypeAnnoIdx, fmt.ast.store.typeAnnoSlice(t.args), Formatter.formatTypeAnno);
            }
        },
        .tuple => |t| {
            region = t.region;
            fmt.formatCollection(t.region, .round, IR.NodeStore.TypeAnnoIdx, fmt.ast.store.typeAnnoSlice(t.annos), Formatter.formatTypeAnno);
        },
        .record => |r| {
            region = r.region;
            fmt.formatCollection(region, .curly, IR.NodeStore.AnnoRecordFieldIdx, fmt.ast.store.annoRecordFieldSlice(r.fields), Formatter.formatAnnoRecordField);
        },
        .tag_union => |t| {
            region = t.region;
            fmt.formatCollection(t.region, .square, IR.NodeStore.TypeAnnoIdx, fmt.ast.store.typeAnnoSlice(t.tags), Formatter.formatTypeAnno);
        },
        .@"fn" => |f| {
            region = f.region;
            var i: usize = 0;
            const args = fmt.ast.store.typeAnnoSlice(f.args);
            for (args) |idx| {
                _ = fmt.formatTypeAnno(idx);
                if (i < (f.args.span.len - 1)) {
                    fmt.pushAll(", ");
                }
                i += 1;
            }
            fmt.pushAll(" -> ");
            _ = fmt.formatTypeAnno(f.ret);
        },
        .parens => |p| {
            region = p.region;
            const multiline = fmt.isRegionMultiline(region);
            fmt.push('(');
            if (multiline) {
                _ = fmt.flushCommentsAfter(region.start);
                fmt.curr_indent += 1;
                fmt.newline();
                fmt.pushIndent();
            }
            const anno_region = fmt.formatTypeAnno(p.anno);
            _ = fmt.flushCommentsAfter(anno_region.end);
            fmt.push(')');
        },
        .underscore => |u| {
            region = u.region;
            fmt.push('_');
        },
    }

    return region;
}

fn ensureNewline(fmt: *Formatter) void {
    const last = fmt.buffer.getLastOrNull() orelse ' ';
    if (fmt.buffer.items.len == 0 or last == '\n') {
        return;
    }
    fmt.newline();
}

fn newline(fmt: *Formatter) void {
    fmt.buffer.append(fmt.gpa, '\n') catch |err| exitOnOom(err);
}

fn flushCommentsBefore(fmt: *Formatter, tokenIdx: TokenIdx) bool {
    if (tokenIdx == 0) {
        return false;
    }
    const tags = fmt.ast.tokens.tokens.items(.tag);
    const prevNewline = tokenIdx - 1;
    if (tags[prevNewline] != .Newline) {
        return false;
    }
    var first = prevNewline;
    // Go back as long as we see newlines
    while (first > 0 and tags[first - 1] == .Newline) {
        first -= 1;
    }
    var i = first;
    // Now print them in order
    while (i <= prevNewline) {
        const newline_tok = fmt.ast.tokens.tokens.get(i);
        std.debug.assert(newline_tok.tag == .Newline);
        const start = newline_tok.offset;
        const end = start + newline_tok.extra.length;
        if (end > start) {
            if (i == 0 or i > first) {
                fmt.pushIndent();
                fmt.push('#');
            } else {
                fmt.pushAll(" #");
            }
            const comment_text = fmt.ast.source[start..end];
            if (comment_text[0] != ' ') {
                fmt.push(' ');
            }
            fmt.pushAll(comment_text);
        }
        fmt.newline();
        i += 1;
    }
    return true;
}

fn flushCommentsAfter(fmt: *Formatter, tokenIdx: TokenIdx) bool {
    const tags = fmt.ast.tokens.tokens.items(.tag);
    var nextNewline = tokenIdx + 1;
    if (nextNewline >= tags.len) {
        return false;
    }
    if (tags[nextNewline] == .Comma) {
        nextNewline += 1;
    }
    if (nextNewline >= tags.len or tags[nextNewline] != .Newline) {
        return false;
    }
    while (nextNewline < tags.len and tags[nextNewline] == .Newline) {
        const newline_tok = fmt.ast.tokens.tokens.get(nextNewline);
        const start = newline_tok.offset;
        const end = start + newline_tok.extra.length;
        if (end > start) {
            fmt.pushAll(" #");
            const comment_text = fmt.ast.source[start..end];
            if (comment_text[0] != ' ') {
                fmt.push(' ');
            }
            fmt.pushAll(comment_text);
        }
        fmt.newline();
        nextNewline += 1;
    }
    return true;
}

fn isRegionMultiline(fmt: *Formatter, region: IR.Region) bool {
    for (fmt.ast.tokens.tokens.items(.tag)[region.start..region.end]) |t| {
        switch (t) {
            .Newline => {
                return true;
            },
            else => {},
        }
    }
    return false;
}

const indent = "    ";

fn push(fmt: *Formatter, c: u8) void {
    fmt.buffer.append(fmt.gpa, c) catch |err| exitOnOom(err);
}

fn pushAll(fmt: *Formatter, str: []const u8) void {
    fmt.buffer.appendSlice(fmt.gpa, str) catch |err| exitOnOom(err);
}

fn pushIndent(fmt: *Formatter) void {
    if (fmt.curr_indent == 0) {
        return;
    }
    for (0..fmt.curr_indent) |_| {
        fmt.buffer.appendSlice(fmt.gpa, indent) catch |err| exitOnOom(err);
    }
}

fn pushTokenText(fmt: *Formatter, ti: TokenIdx) void {
    const tag = fmt.ast.tokens.tokens.items(.tag)[ti];
    const region = fmt.ast.tokens.resolve(ti);
    var start = region.start.offset;
    switch (tag) {
        .NoSpaceDotLowerIdent, .NoSpaceDotUpperIdent => {
            start += 1;
        },
        else => {},
    }

    const text = fmt.ast.source[start..region.end.offset];
    fmt.buffer.appendSlice(fmt.gpa, text) catch |err| exitOnOom(err);
}

fn regionInSlice(fmt: *Formatter, comptime T: anytype, slice: []T) IR.Region {
    if (slice.len == 0) {
        return IR.NodeStore.emptyRegion();
    }
    const first: usize = @intCast(slice[0].id);
    const last: usize = @intCast(slice[slice.len - 1].id);
    const first_region = fmt.ast.store.nodes.items.items(.region)[first];
    const last_region = fmt.ast.store.nodes.items.items(.region)[last];
    return first_region.spanAcross(last_region);
}

fn displayRegion(fmt: *Formatter, region: IR.Region) void {
    const tags = fmt.ast.tokens.tokens.items(.tag);
    return std.debug.print("[{s}@{d}...{s}@{d}]\n", .{ @tagName(tags[region.start]), region.start, @tagName(tags[region.end]), region.end });
}

fn moduleFmtsSame(source: []const u8) !void {
    const parse = @import("check/parse.zig").parse;

    const gpa = std.testing.allocator;

    var env = base.ModuleEnv.init(gpa);
    defer env.deinit();

    var parse_ast = parse(&env, source);
    defer parse_ast.deinit();

    // @Anthony / @Josh shouldn't these be added to the ModuleEnv (env) so they are in the arena
    // and then they are cleaned up when the arena is deinitialized at the end of program compilation
    // or included in the cached build
    defer gpa.free(parse_ast.errors);

    try std.testing.expectEqualSlices(IR.Diagnostic, &[_]IR.Diagnostic{}, parse_ast.errors);

    var formatter = Formatter.init(parse_ast);
    defer formatter.deinit();

    const result = formatter.formatFile();
    defer gpa.free(result);

    try std.testing.expectEqualStrings(source, result);
}

const tokenize = @import("check/parse/tokenize.zig");

fn exprFmtsSame(source: []const u8, flags: FormatFlags) !void {
    try exprFmtsTo(source, source, flags);
}
fn exprFmtsTo(source: []const u8, expected: []const u8, flags: FormatFlags) !void {
    const Parser = @import("check/parse/Parser.zig").Parser;

    const gpa = std.testing.allocator;

    var env = base.ModuleEnv.init(gpa);
    defer env.deinit();

    var messages: [1]tokenize.Diagnostic = undefined;
    const msg_slice = messages[0..];

    var tokenizer_ = tokenize.Tokenizer.init(&env, source, msg_slice);
    tokenizer_.tokenize();
    const result = tokenizer_.finishAndDeinit();

    var parser = Parser.init(result.tokens);
    defer parser.deinit();

    const expr = parser.parseExpr();

    const errors = parser.diagnostics.toOwnedSlice(gpa) catch |err| exitOnOom(err);

    var parse_ast = IR{
        .source = source,
        .tokens = result.tokens,
        .store = parser.store,
        .errors = errors,
    };
    defer parse_ast.deinit();
    defer std.testing.allocator.free(parse_ast.errors);

    std.testing.expectEqualSlices(IR.Diagnostic, &[_]IR.Diagnostic{}, parse_ast.errors) catch {
        std.debug.print("Tokens:\n{any}", .{result.tokens.tokens.items(.tag)});
        std.debug.panic("Test failed with parse errors", .{});
    };

    var formatter = Formatter.init(parse_ast);
    formatter.flags = flags;
    defer formatter.deinit();

    _ = formatter.formatExpr(expr);

    const fmt_result = formatter.buffer.toOwnedSlice(gpa) catch |err| exitOnOom(err);
    defer gpa.free(fmt_result);

    try std.testing.expectEqualStrings(expected, fmt_result);
}

fn moduleFmtsTo(source: []const u8, to: []const u8) !void {
    const gpa = std.testing.allocator;
    const result = try moduleFmtsStable(gpa, source, false);
    defer gpa.free(result);
    try std.testing.expectEqualStrings(to, result);
}

/// Asserts a module when formatted twice in a row results in the same final output.
/// Returns that final output.
pub fn moduleFmtsStable(gpa: std.mem.Allocator, input: []const u8, debug: bool) ![]const u8 {
    if (debug) {
        std.debug.print("Original:\n==========\n{s}\n==========\n\n", .{input});
    }

    const formatted = try parseAndFmt(gpa, input, debug);
    defer gpa.free(formatted);

    const formatted_twice = parseAndFmt(gpa, formatted, debug) catch {
        return error.SecondParseFailed;
    };
    errdefer gpa.free(formatted_twice);

    std.testing.expectEqualStrings(formatted, formatted_twice) catch {
        return error.FormattingNotStable;
    };
    return formatted_twice;
}

fn parseAndFmt(gpa: std.mem.Allocator, input: []const u8, debug: bool) ![]const u8 {
    const parse = @import("check/parse.zig").parse;

    var module_env = base.ModuleEnv.init(gpa);
    defer module_env.deinit();

    var parse_ast = parse(&module_env, input);
    defer parse_ast.deinit();

    // Currently disabled cause SExpr are missing a lot of IR coverage resulting in panics.
    if (debug and false) {
        // shouldn't be required in future
        parse_ast.store.emptyScratch();

        std.debug.print("Parsed SExpr:\n==========\n", .{});
        parse_ast.toSExprStr(&module_env, std.io.getStdErr().writer().any()) catch @panic("Failed to print SExpr");
        std.debug.print("\n==========\n\n", .{});
    }

    std.testing.expectEqualSlices(IR.Diagnostic, parse_ast.errors, &[_]IR.Diagnostic{}) catch {
        return error.ParseFailed;
    };

    var formatter = init(parse_ast);
    defer formatter.deinit();

    const formatted = formatter.formatFile();

    if (debug) {
        std.debug.print("Formatted:\n==========\n{s}\n==========\n\n", .{formatted});
    }
    return formatted;
}

test "Hello world" {
    try moduleFmtsSame(
        \\app [main!] { pf: platform "../basic-cli/platform.roc" }
        \\
        \\import pf.Stdout
        \\
        \\main! = |_| Stdout.line!("Hello, world!")
    );
}

test "Hello world with block" {
    try moduleFmtsSame(
        \\# Hello world!
        \\
        \\# Multiline comments?
        \\app [main!] { pf: platform "../basic-cli/platform.roc" }
        \\
        \\import pf.Stdout
        \\
        \\main! = |_| {
        \\    world = "World"
        \\    # Hello
        \\    Stdout.line!("Hello, world!")
        \\}
    );
}

test "Plain module" {
    try moduleFmtsSame(
        \\module [hello!, world]
        \\
        \\import pf.Stdout
        \\
        \\hello! = Stdout.line!("Hello")
        \\
        \\world = "World"
    );
    try moduleFmtsSame(
        \\module [hello!, world]
        \\import pf.Stdout
        \\hello! = Stdout.line!("Hello")
        \\world = "World"
    );
}
//
test "Syntax grab bag" {
    try moduleFmtsSame(
        \\# This is a module comment!
        \\app [main!] { pf: platform "../basic-cli/platform.roc" }
        \\
        \\import pf.Stdout exposing [line!, write!]
        \\
        \\import # Comment after import keyword
        \\    pf # Comment after qualifier
        \\        .StdoutMultiline # Comment after ident
        \\        exposing [ # Comment after exposing open
        \\            line!, # Comment after exposed item
        \\            write!, # Another after exposed item
        \\        ] # Comment after exposing close
        \\
        \\import pkg.Something exposing [func as function, Type as ValueCategory, Custom.*]
        \\
        \\import BadName as GoodName
        \\import
        \\    BadNameMultiline
        \\        as
        \\        GoodNameMultiline
        \\
        \\Map a b : List(a), (a -> b) -> List(b)
        \\
        \\Foo : (Bar, Baz)
        \\
        \\FooMultiline : ( # Comment after pattern tuple open
        \\    Bar, # Comment after pattern tuple item
        \\    Baz, # Another after pattern tuple item
        \\) # Comment after pattern tuple close
        \\
        \\Some a : { foo : Ok(a), bar : Something }
        \\
        \\SomeMultiline a : { # Comment after pattern record open
        \\    foo : Ok(a), # Comment after pattern record field
        \\    bar : Something, # Another after pattern record field
        \\} # Comment after pattern record close
        \\
        \\Maybe a : [Some(a), None]
        \\
        \\MaybeMultiline a : [ # Comment after tag union open
        \\    Some(a), # Comment after tag union member
        \\    None, # Another after tag union member
        \\] # Comment after tag union close
        \\
        \\SomeFunc a : Maybe(a), a -> Maybe(a)
        \\
        \\add_one_oneline = |num| if num 2 else 5
        \\
        \\add_one : (U64 -> U64)
        \\add_one = |num| {
        \\    other = 1
        \\    if num {
        \\        dbg some_func()
        \\        0
        \\    } else {
        \\        dbg 123
        \\        other
        \\    }
        \\}
        \\
        \\match_time = |a| match a {
        \\    Blue | Green | Red -> {
        \\        x = 12
        \\        x
        \\    }
        \\    lower # After pattern comment
        \\        -> 1
        \\    "foo" -> # After arrow comment
        \\        100
        \\    "foo" | "bar" -> 200
        \\    [1, 2, 3, .. as rest] # After pattern comment
        \\        -> # After arrow comment
        \\            123 # After branch comment
        \\
        \\    # Just a random comment
        \\
        \\    [1, 2 | 5, 3, .. as rest] -> 123
        \\    3.14 -> 314
        \\    3.14 | 6.28 -> 314
        \\    (1, 2, 3) -> 123
        \\    (1, 2 | 5, 3) -> 123
        \\    { foo: 1, bar: 2, ..rest } -> 12
        \\    { foo: 1, bar: 2 | 7 } -> 12
        \\    Ok(123) -> 123
        \\    Ok(Some(dude)) -> dude
        \\    TwoArgs("hello", Some("world")) -> 1000
        \\}
        \\
        \\expect # Comment after expect keyword
        \\    blah == 1 # Comment after expect statement
        \\
        \\main! : List(String) -> Result({}, _)
        \\main! = |_| { # Yeah I can leave a comment here
        \\    world = "World"
        \\    number = 123
        \\    expect blah == 1
        \\    tag = Blue
        \\    return # Comment after return keyword
        \\        tag # Comment after return statement
        \\
        \\    # Just a random comment!
        \\
        \\    ...
        \\    match_time(...)
        \\    crash # Comment after crash keyword
        \\        "Unreachable!" # Comment after crash statement
        \\    tag_with_payload = Ok(number)
        \\    interpolated = "Hello, ${world}"
        \\    list = [
        \\        add_one(number), # Comment one
        \\        456, # Comment two
        \\        789, # Comment three
        \\    ]
        \\    record = { foo: 123, bar: "Hello", baz: tag, qux: Ok(world), punned }
        \\    tuple = (123, "World", tag, Ok(world), (nested, tuple), [1, 2, 3])
        \\    multiline_tuple = (
        \\        123,
        \\        "World",
        \\        tag1,
        \\        Ok(world), # This one has a comment
        \\        (nested, tuple),
        \\        [1, 2, 3],
        \\    )
        \\    bin_op_result = Err(foo) ?? 12 > 5 * 5 or 13 + 2 < 5 and 10 - 1 >= 16 or 12 <= 3 / 5
        \\    static_dispatch_style = some_fn(arg1)?.static_dispatch_method()?.next_static_dispatch_method()?.record_field?
        \\    Stdout.line!(interpolated)?
        \\    Stdout.line!("How about ${Num.toStr(number)} as a string?")
        \\} # Comment after top-level decl
        \\
        \\expect {
        \\    foo = 1 # This should work too
        \\    blah = 1
        \\    blah == foo
        \\}
    );
}

test "First BinOp" {
    const expr = "1 + 2";
    try exprFmtsSame(expr, .no_debug);
}

test "BinOp with higher BP right" {
    const expr = "1 + 2 * 3";
    try exprFmtsSame(expr, .no_debug);
    try exprFmtsTo(expr, "(1 + (2 * 3))", .debug_binop);
    // try exprBinOpIs(expr, .OpStar);
}

test "Multiline BinOp" {
    const expr =
        \\1 # One
        \\    + # Plus
        \\
        \\    # A comment in between
        \\
        \\    2 # Two
        \\        * # Times
        \\        3
    ;
    try exprFmtsSame(expr, .no_debug);
}

test "Multiline list formatting" {
    const expr = "[1,2,3,]";
    const expected =
        \\[
        \\    1,
        \\    2,
        \\    3,
        \\]
    ;
    try exprFmtsTo(expr, expected, .no_debug);
    const expr2 =
        \\[1, 2, # Foo
        \\  3]
    ;
    const expected2 =
        \\[
        \\    1,
        \\    2, # Foo
        \\    3,
        \\]
    ;
    try exprFmtsTo(expr2, expected2, .no_debug);
    const expr3 = "[[1], [2], [3,4,], [5]]";
    const expected3 =
        \\[
        \\    [1],
        \\    [2],
        \\    [
        \\        3,
        \\        4,
        \\    ],
        \\    [5],
        \\]
    ;
    try exprFmtsTo(expr3, expected3, .no_debug);
    const expr4 =
        \\[ # Open
        \\    1, # First
        \\
        \\    # A comment in the middle
        \\
        \\    2, # Second
        \\    # This comment has no blanks around it
        \\    3, # Third
        \\]
    ;
    try exprFmtsSame(expr4, .no_debug);
    try exprFmtsSame(expected, .no_debug);
    try exprFmtsSame(expected2, .no_debug);
    try exprFmtsSame(expected3, .no_debug);
}

test "string multiline formatting (due to templating, not multiline string literal)" {
    const expr =
        \\"This is a string with ${some_func(a, #This is a comment
        \\b)} lines of text due to the template parts"
    ;
    const expected =
        \\"This is a string with ${
        \\    some_func(
        \\        a, # This is a comment
        \\        b,
        \\    )
        \\} lines of text due to the template parts"
    ;
    try exprFmtsTo(expr, expected, .no_debug);
    try exprFmtsSame(expected, .no_debug);
}

test "BinOp omnibus" {
    const expr = "Err(foo) ?? 12 > 5 * 5 or 13 + 2 < 5 and 10 - 1 >= 16 or 12 <= 3 / 5";
    const expr_sloppy = "Err(foo)??12>5*5 or 13+2<5 and 10-1>=16 or 12<=3/5";
    const formatted = "((((Err(foo) ?? 12) > (5 * 5)) or (((13 + 2) < 5) and ((10 - 1) >= 16))) or (12 <= (3 / 5)))";
    try exprFmtsSame(expr, .no_debug);
    try exprFmtsTo(expr_sloppy, expr, .no_debug);
    try exprFmtsTo(expr, formatted, .debug_binop);
    try exprFmtsTo(expr_sloppy, formatted, .debug_binop);
}

test "Dot access super test" {
    const expr = "some_fn(arg1)?.static_dispatch_method()?.next_static_dispatch_method()?.record_field?";
    try exprFmtsSame(expr, .no_debug);
}
