const std = @import("std");

const IR = @import("IR.zig");
const NodeList = IR.NodeList;

const tokenize = @import("tokenize.zig");
const TokenizedBuffer = tokenize.TokenizedBuffer;
const Token = tokenize.Token;
const TokenIdx = Token.List.Idx;

const exit_on_oom = @import("../../collections/exit_on_oom.zig").exit_on_oom;

pub const Parser = @This();

allocator: std.mem.Allocator,
pos: TokenIdx,
tok_buf: TokenizedBuffer,
store: IR.NodeStore,
scratch_nodes: std.ArrayList(IR.Node.Idx),
diagnostics: std.ArrayList(IR.Diagnostic),

pub fn init(gpa: std.mem.Allocator, tokens: TokenizedBuffer) Parser {
    const estimated_node_count = (tokens.tokens.items.len + 2) / 2;
    const store = IR.NodeStore.initWithCapacity(gpa, estimated_node_count);

    return Parser{
        .allocator = gpa,
        .pos = .{ .id = 0 },
        .tok_buf = tokens,
        .store = store,
        .scratch_nodes = std.ArrayList(IR.Node.Idx).init(gpa),
        .diagnostics = std.ArrayList(IR.Diagnostic).init(gpa),
    };
}

pub fn deinit(parser: *Parser) void {
    parser.scratch_nodes.deinit();
    parser.diagnostics.deinit();
}

const TestError = error{TestError};
fn test_parser(source: []const u8, run: fn (parser: Parser) TestError!void) TestError!void {
    const gc = try @import("GenCatData").GenCatData.init(std.testing.allocator);
    defer gc.deinit();
    const messages = [128]tokenize.Diagnostic;
    const tokenizer = tokenize.Tokenizer.init(source, messages[0..], gc, std.testing.allocator);
    tokenizer.tokenize();
    const tok_result = tokenizer.finalize_and_deinit();
    defer tok_result.tokens.deinit();
    const parser = Parser.init(std.testing.allocaor, tok_result.tokens);
    defer parser.store.deinit();
    defer parser.scratch_nodes.deinit();
    defer parser.diagnostics.deinit();
    try run(parser);
}

pub fn advance(self: *Parser) void {
    if (self.pos.id >= self.tok_buf.tokens.len()) {
        return;
    }
    self.pos = .{ .id = self.pos.id + 1 };
}

pub fn peek(self: *Parser) Token.Tag {
    if (self.pos.id >= self.tok_buf.tokens.len()) {
        return .EndOfFile;
    }
    return self.tok_buf.tokens.items.items(.tag)[self.pos.id];
}

pub fn peekNext(self: Parser) Token.Tag {
    const next = .{ .id = self.pos.id + 1 };
    if (next.id >= self.tok_buf.tokens.len()) {
        return .EndOfFile;
    }
    return self.tok_buf.tokens.items.items(.tag)[next.id];
}

// If the next token is a newline, consume it
// Returns the indent level of the next line if it is a newline, otherwise null
pub fn consumeNewline(self: *Parser) ?u16 {
    if (self.peek() != .Newline) {
        return null;
    }
    var indent: u32 = 0;
    const t = self.tok_buf.tokens.get(self.pos);
    indent = t.offset;
    self.advance();
    return @intCast(indent);
}

// Returns the indent level of the next line if the next token is a newline, otherwise null
pub fn peekNewline(self: *Parser) ?u16 {
    if (self.peek() != .Newline) {
        return null;
    }
    const indent = self.tok_buf.tokens.items.items(.offset)[self.pos.id];
    return @intCast(indent);
}

pub fn parseFile(self: *Parser) void {
    self.store.emptyScratch();
    _ = self.store.addFile(.{
        .header = .{ .id = 0 },
        .statements = &[0]IR.NodeStore.StatementIdx{},
        .region = .{ .start = .{ .id = 0 }, .end = .{ .id = 0 } },
    });

    const header = self.parseHeader();

    while (self.peek() != .EndOfFile) {
        if (self.consumeNewline()) |indent| {
            std.debug.assert(indent == 0); // TODO: report an error
        }
        if (self.peek() == .EndOfFile) {
            break;
        }
        const scratch_top = self.store.scratch_statements.items.len;
        if (self.parseStmt(0)) |idx| {
            if (self.store.scratch_statements.items.len > scratch_top) {
                self.store.scratch_statements.shrinkRetainingCapacity(scratch_top);
            }
            // ddd
            self.store.scratch_statements.append(idx) catch exit_on_oom();
        } else {
            if (self.store.scratch_statements.items.len > scratch_top) {
                self.store.scratch_statements.shrinkRetainingCapacity(scratch_top);
            }
            break;
        }
    }

    std.debug.assert(self.store.scratch_statements.items.len > 0);

    _ = self.store.addFile(.{
        .header = header,
        .statements = self.store.scratch_statements.items[0..],
        .region = .{ .start = .{ .id = 0 }, .end = .{ .id = @as(u32, @intCast(self.tok_buf.tokens.len() - 1)) } },
    });
}

fn parseCollection(comptime T: type, self: *Parser, end_token: Token.Tag, parser: fn (*Parser) T) void {
    _ = self;
    _ = end_token;
    _ = parser;
}

/// Parses a module header using the following grammar:
///
/// provides_entry :: [LowerIdent|UpperIdent] Comma Newline*
/// package_entry :: LowerIdent Comma "platform"? String Comma
/// app_header :: KwApp Newline* OpenSquare provides_entry* CloseSquare OpenCurly package_entry CloseCurly
pub fn parseHeader(self: *Parser) IR.NodeStore.HeaderIdx {
    if (self.peek() != .KwApp) {
        std.debug.panic("TODO: Support other headers besides app", .{});
    }
    var provides: []TokenIdx = &.{};
    var packages: []IR.NodeStore.RecordFieldIdx = &.{};
    var platform: ?TokenIdx = null;
    var platform_name: ?TokenIdx = null;

    self.advance();
    const start_indent = 0;
    var indent = self.consumeNewline() orelse start_indent;

    // Get provides
    if (self.peek() != .OpenSquare) {
        std.debug.panic("TODO: Handle header with no provides open bracket: {s}", .{@tagName(self.peek())});
    }
    self.advance();
    indent = self.consumeNewline() orelse indent;
    const scratch_top = self.store.scratch_tokens.items.len;
    while (self.peek() != .CloseSquare) {
        if (self.peek() != .LowerIdent and self.peek() != .UpperIdent) {
            std.debug.panic("TODO: Handler header bad provides contents: {s}", .{@tagName(self.peek())});
        }

        self.store.scratch_tokens.append(self.pos) catch exit_on_oom();
        self.advance();

        if (self.peek() != .Comma) {
            provides = self.store.scratch_tokens.items[scratch_top..];
            self.store.scratch_tokens.shrinkRetainingCapacity(scratch_top);
            break;
        } else if (self.consumeNewline()) |i| {
            if (i <= indent) {
                std.debug.panic("TODO: Handle bad indent: {s}", .{@tagName(self.peek())});
            }
        }
    }
    if (self.consumeNewline()) |i| {
        if (i > indent) {
            std.debug.panic("TODO: Handle bad indent: {s}", .{@tagName(self.peek())});
        }
        indent = i;
    }
    if (self.peek() != .CloseSquare) {
        std.debug.panic("TODO: Handle Bad header no closing provides bracket: {s}", .{@tagName(self.peek())});
    }
    self.advance();

    // Get platform and packages
    const statement_scratch_top = self.store.scratch_record_fields.items.len;
    if (self.peek() != .OpenCurly) {
        std.debug.panic("TODO: Handle Bad header no packages curly: {s}", .{@tagName(self.peek())});
    }
    self.advance();
    while (self.peek() != .CloseCurly) {
        indent = self.consumeNewline() orelse indent;
        if (self.peek() != .LowerIdent) {
            std.debug.panic("TODO: Handle bad package identifier: {s}", .{@tagName(self.peek())});
        }
        const name_tok = self.pos;
        self.advance();
        if (self.peek() != .OpColon) {
            std.debug.panic("TODO: Handle bad package identifier: {s}", .{@tagName(self.peek())});
        }
        self.advance();
        if (self.peek() == .KwPlatform) {
            if (platform != null) {
                std.debug.panic("TODO: Handle multiple platforms in app header: {s}", .{@tagName(self.peek())});
            }
            self.advance();
            if (self.peek() != .String) {
                std.debug.panic("TODO: Handle bad package URI in app header: {s}", .{@tagName(self.peek())});
            }
            platform = self.pos;
            platform_name = name_tok;
        } else {
            if (self.peek() != .String) {
                std.debug.panic("TODO: Handle bad package URI in app header: {s}", .{@tagName(self.peek())});
            }
            const value = self.store.addExpr(.{ .string = .{
                .token = self.pos,
                .region = .{ .start = self.pos, .end = self.pos },
            } });
            self.store.scratch_record_fields.append(self.store.addRecordField(.{
                .name = name_tok,
                .value = value,
                .optional = false,
            })) catch exit_on_oom();
        }
        self.advance();
        if (self.peek() != .Comma) {
            packages = self.store.scratch_record_fields.items[statement_scratch_top..];
            self.store.scratch_record_fields.shrinkRetainingCapacity(statement_scratch_top);
        } else {
            self.advance();
        }
    }
    if (self.consumeNewline()) |i| {
        if (i > indent) {
            std.debug.panic("TODO: Handle bad indent: {s}", .{@tagName(self.peek())});
        }
        indent = i;
    }
    if (self.peek() != .CloseCurly) {
        std.debug.panic("TODO: Handle Bad header no end curly in packages: {s}", .{@tagName(self.peek())});
    }
    self.advance();

    if (platform) |p| {
        if (platform_name) |pn| {
            const header = IR.NodeStore.Header{
                .app = .{
                    .platform = p,
                    .platform_name = pn,
                    .provides = provides,
                    .packages = packages,
                    .region = .{ .start = .{ .id = 0 }, .end = .{ .id = 0 } },
                },
            };
            const idx = self.store.addHeader(header);
            return idx;
        }
    }
    std.debug.panic("TODO: Handle header with no platform: {s}", .{@tagName(self.peek())});
}

pub fn parseStmt(self: *Parser, base_indent: u16) ?IR.NodeStore.StatementIdx {
    switch (self.peek()) {
        .LowerIdent => {
            const start = self.pos;
            if (self.peekNext() == .OpAssign) {
                self.advance();
                if (self.finishParseAssign(base_indent)) |idx| {
                    const patt_idx = self.store.addPattern(.{ .ident = .{
                        .ident_tok = start,
                        .region = .{ .start = start, .end = start },
                    } });
                    const statement_idx = self.store.addStatement(.{ .decl = .{
                        .pattern = patt_idx,
                        .body = idx,
                        .region = .{ .start = start, .end = self.pos },
                    } });
                    return statement_idx;
                }
                return null;
            } else {
                // If not a decl
                const expr = self.store.addExpr(.{ .ident = .{
                    .token = start,
                    .qualifier = null,
                    .region = .{ .start = start, .end = start },
                } });
                const statement_idx = self.store.addStatement(.{ .expr = .{
                    .expr = expr,
                    .region = .{ .start = start, .end = start },
                } });
                return statement_idx;
            }
        },
        .KwImport => {
            const start = self.pos;
            self.advance();
            var qualifier: ?TokenIdx = null;
            if (self.peek() == .LowerIdent) {
                qualifier = self.pos;
                self.advance();
            }
            if (self.peek() == .UpperIdent or (qualifier != null and self.peek() == .NoSpaceDotUpperIdent)) {
                const statement_idx = self.store.addStatement(.{ .import = .{
                    .module_name_tok = self.pos,
                    .qualifier_tok = qualifier,
                    .alias_tok = null,
                    .exposes = &[0]TokenIdx{},
                    .region = .{ .start = start, .end = self.pos },
                } });
                self.advance();

                return statement_idx;
            }
            return null;
        },
        .KwExpect => {
            return null;
        },
        .KwCrash => {
            return null;
        },
        .KwIf => {
            return null;
        },
        .KwWhen => {
            return null;
        },
        .UpperIdent => {
            const start = self.pos;
            const expr = self.parseExpr(base_indent);
            const statement_idx = self.store.addStatement(.{ .expr = .{
                .expr = expr,
                .region = .{ .start = start, .end = self.pos },
            } });
            return statement_idx;
        },
        else => {
            std.debug.print("Tokens:\n{any}", .{self.tok_buf.tokens.items.items(.tag)});
            std.debug.panic("todo: emit error, unexpected token {s}", .{@tagName(self.peek())});
            return null;
        },
    }
}

pub fn parseExpr(self: *Parser, min_indent: u16) IR.NodeStore.ExprIdx {
    const start = self.pos;
    var expr: ?IR.NodeStore.ExprIdx = null;
    switch (self.peek()) {
        .UpperIdent => {
            self.advance();

            if (self.peek() == .NoSpaceDotLowerIdent) {
                // This is a qualified lowercase ident
                const id = self.pos;
                self.advance();
                const ident = self.store.addExpr(.{ .ident = .{
                    .token = id,
                    .qualifier = start,
                    .region = .{ .start = start, .end = id },
                } });

                expr = ident;
            } else {
                // This is a Tag
                const tag = self.store.addExpr(.{ .tag = .{
                    .token = start,
                    .region = .{ .start = start, .end = start },
                } });

                expr = tag;
            }
        },
        .LowerIdent => {
            self.advance();
            const ident = self.store.addExpr(.{ .ident = .{
                .token = start,
                .qualifier = null,
                .region = .{ .start = start, .end = start },
            } });

            expr = ident;
        },
        .Int => {
            self.advance();
            expr = self.store.addExpr(.{ .int = .{
                .token = start,
                .region = .{ .start = start, .end = start },
            } });
        },
        .String => {
            self.advance();
            expr = self.store.addExpr(.{ .string = .{
                .token = start,
                .region = .{ .start = start, .end = start },
            } });
        },
        else => {
            std.debug.print("Tokens:\n{any}", .{self.tok_buf.tokens.items.items(.tag)});
            std.debug.panic("todo: emit error - {s}", .{@tagName(self.peek())});
        },
    }
    if (expr) |e| {
        var expression = e;
        // Check for an apply...
        if (self.peek() == .OpenRound) {
            const scratch_top = self.store.scratch_exprs.items.len;
            while (self.peek() != .CloseRound) {
                self.advance();
                var indent = min_indent;
                if (self.consumeNewline()) |i| {
                    if (i < min_indent) {
                        std.debug.panic("TODO: Indent problem", .{});
                        indent = i;
                    }
                }
                const arg_expression = self.parseExpr(indent);
                self.store.scratch_exprs.append(arg_expression) catch exit_on_oom();
            }
            self.advance();
            const args = self.store.scratch_exprs.items[scratch_top..];
            expression = self.store.addExpr(.{ .apply = .{
                .args = args,
                .@"fn" = e,
                .region = .{ .start = start, .end = self.pos },
            } });
        }
        // Check for try suffix...
        return expression;
    }
    std.debug.panic("todo: Malformed Expr", .{});
}

pub fn finishParseAssign(self: *Parser, base_indent: u16) ?IR.NodeStore.BodyIdx {
    self.advance();
    if (self.consumeNewline()) |indent| {
        if (indent <= base_indent) {
            std.debug.panic("todo: emit error", .{});
        }

        const scratch_top = self.store.scratch_statements.items.len;
        defer self.store.scratch_statements.shrinkRetainingCapacity(scratch_top);

        while (true) {
            const statement = self.parseStmt(indent) orelse break;
            self.store.scratch_statements.append(statement) catch exit_on_oom();
            if (self.peekNewline()) |i| {
                if (i <= base_indent) {
                    break;
                }
                self.advance();
            } else {
                break;
            }
        }

        const statements = self.store.scratch_statements.items[scratch_top..];

        const body = self.store.addBody(.{ .statements = statements, .whitespace = .{ .id = self.pos.id - 1 } });
        return body;
    } else {
        const start = self.pos;
        const expr = self.parseExpr(base_indent);
        const statement = self.store.addStatement(.{ .expr = .{
            .expr = expr,
            .region = .{ .start = start, .end = self.pos },
        } });
        const body = self.store.addBody(.{ .statements = &.{statement}, .whitespace = null });
        return body;
    }
}
