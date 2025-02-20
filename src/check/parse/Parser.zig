const std = @import("std");

const IR = @import("IR.zig");
const NodeList = IR.NodeList;

const tokenize = @import("tokenize.zig");
const TokenizedBuffer = tokenize.TokenizedBuffer;
const Token = tokenize.Token;
const TokenIdx = Token.Idx;

const exitOnOom = @import("../../collections/utils.zig").exitOnOom;

pub const Parser = @This();

allocator: std.mem.Allocator,
pos: TokenIdx,
tok_buf: TokenizedBuffer,
store: IR.NodeStore,
scratch_nodes: std.ArrayList(IR.Node.Idx),
diagnostics: std.ArrayList(IR.Diagnostic),

pub fn init(gpa: std.mem.Allocator, tokens: TokenizedBuffer) Parser {
    const estimated_node_count = (tokens.tokens.len + 2) / 2;
    const store = IR.NodeStore.initWithCapacity(gpa, estimated_node_count);

    return Parser{
        .allocator = gpa,
        .pos = 0,
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
    const messages = [128]tokenize.Diagnostic;
    const tokenizer = tokenize.Tokenizer.init(source, messages[0..], std.testing.allocator);
    tokenizer.tokenize();
    const tok_result = tokenizer.finalize_and_deinit();
    defer tok_result.tokens.deinit();
    const parser = Parser.init(std.testing.allocator, tok_result.tokens);
    defer parser.store.deinit();
    defer parser.scratch_nodes.deinit();
    defer parser.diagnostics.deinit();
    try run(parser);
}

pub fn advance(self: *Parser) void {
    while (true) {
        self.pos += 1;
        if (self.peek() != .Newline) {
            break;
        }
    }
    // We have an EndOfFile token that we never expect to advance past
    std.debug.assert(self.pos < self.tok_buf.tokens.len);
}

pub fn advanceOne(self: *Parser) void {
    self.pos += 1;
    // We have an EndOfFile token that we never expect to advance past
    std.debug.assert(self.pos < self.tok_buf.tokens.len);
}

pub fn peek(self: *Parser) Token.Tag {
    std.debug.assert(self.pos < self.tok_buf.tokens.len);
    return self.tok_buf.tokens.items(.tag)[self.pos];
}

pub fn peekNext(self: Parser) Token.Tag {
    const next = self.pos + 1;
    if (next >= self.tok_buf.tokens.len) {
        return .EndOfFile;
    }
    return self.tok_buf.tokens.items(.tag)[next];
}
pub fn pushDiagnostic(self: *Parser, tag: IR.Diagnostic.Tag, region: IR.Region) void {
    self.diagnostics.append(.{
        .tag = tag,
        .region = region,
    }) catch exitOnOom();
}

pub fn pushMalformed(self: *Parser, comptime t: type, tag: IR.Diagnostic.Tag) t {
    self.diagnostics.append(.{
        .tag = tag,
        .region = .{ .start = self.pos, .end = self.pos },
    }) catch exitOnOom();
    return self.store.addMalformed(t, tag, self.pos);
}

pub fn parseFile(self: *Parser) void {
    self.store.emptyScratch();
    _ = self.store.addFile(.{
        .header = IR.NodeStore.HeaderIdx{ .id = 0 },
        .statements = &[0]IR.NodeStore.StatementIdx{},
        .region = .{ .start = 0, .end = 0 },
    });

    const header = self.parseHeader();

    while (self.peek() != .EndOfFile) {
        if (self.peek() == .EndOfFile) {
            break;
        }
        const scratch_top = self.store.scratch_statements.items.len;
        if (self.parseStmt()) |idx| {
            if (self.store.scratch_statements.items.len > scratch_top) {
                self.store.scratch_statements.shrinkRetainingCapacity(scratch_top);
            }
            self.store.scratch_statements.append(idx) catch exitOnOom();
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
        .region = .{ .start = 0, .end = @intCast(self.tok_buf.tokens.len - 1) },
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
    switch (self.peek()) {
        .KwApp => {
            return self.parseAppHeader();
        },
        .KwModule => {
            return self.parseModuleHeader();
        },
        // .KwPackage => {},
        // .KwHosted => {},
        else => {
            return self.store.addMalformed(IR.NodeStore.HeaderIdx, .missing_header, self.pos);
        },
    }
}

fn parseModuleHeader(self: *Parser) IR.NodeStore.HeaderIdx {
    if (self.peek() != .KwModule) {
        std.debug.panic("This should never happen: parseModuleHeader does not start with `module`\n", .{});
    }
    const start = self.pos;
    var exposes: []TokenIdx = &.{};

    self.advance();

    // Get exposes
    if (self.peek() != .OpenSquare) {
        std.debug.panic("TODO: Handle header with no exposes open bracket: {s}", .{@tagName(self.peek())});
    }
    self.advance();
    const scratch_top = self.store.scratch_tokens.items.len;
    defer self.store.scratch_tokens.shrinkRetainingCapacity(scratch_top);
    while (self.peek() != .CloseSquare) {
        if (self.peek() != .LowerIdent and self.peek() != .UpperIdent) {
            std.debug.panic("TODO: Handler header bad exposes contents: {s}", .{@tagName(self.peek())});
        }

        self.store.scratch_tokens.append(self.pos) catch exitOnOom();
        self.advance();

        if (self.peek() != .Comma) {
            break;
        }
        self.advance();
    }
    if (self.peek() != .CloseSquare) {
        std.debug.panic("TODO: Handle Bad header no closing exposes bracket: {s}", .{@tagName(self.peek())});
    }
    exposes = self.store.scratch_tokens.items[scratch_top..];
    self.advance();

    return self.store.addHeader(.{ .module = .{
        .region = .{ .start = start, .end = self.pos },
        .exposes = exposes,
    } });
}

pub fn parseAppHeader(self: *Parser) IR.NodeStore.HeaderIdx {
    var provides: []TokenIdx = &.{};
    var packages: []IR.NodeStore.RecordFieldIdx = &.{};
    var platform: ?TokenIdx = null;
    var platform_name: ?TokenIdx = null;

    self.advance();

    // Get provides
    if (self.peek() != .OpenSquare) {
        return self.pushMalformed(IR.NodeStore.HeaderIdx, .unexpected_token);
    }
    self.advance();
    const scratch_top = self.store.scratch_tokens.items.len;
    defer self.store.scratch_tokens.shrinkRetainingCapacity(scratch_top);
    while (self.peek() != .CloseSquare) {
        if (self.peek() != .LowerIdent and self.peek() != .UpperIdent) {
            return self.pushMalformed(IR.NodeStore.HeaderIdx, .unexpected_token);
        }

        self.store.scratch_tokens.append(self.pos) catch exitOnOom();
        self.advance();

        if (self.peek() != .Comma) {
            break;
        }
        self.advance();
    }
    if (self.peek() != .CloseSquare) {
        return self.pushMalformed(IR.NodeStore.HeaderIdx, .unexpected_token);
    }
    provides = self.store.scratch_tokens.items[scratch_top..];
    self.advance();

    // Get platform and packages
    const statement_scratch_top = self.store.scratch_record_fields.items.len;
    if (self.peek() != .OpenCurly) {
        return self.pushMalformed(IR.NodeStore.HeaderIdx, .unexpected_token);
    }
    self.advance();
    while (self.peek() != .CloseCurly) {
        if (self.peek() != .LowerIdent) {
            return self.pushMalformed(IR.NodeStore.HeaderIdx, .unexpected_token);
        }
        const name_tok = self.pos;
        self.advance();
        if (self.peek() != .OpColon) {
            return self.pushMalformed(IR.NodeStore.HeaderIdx, .unexpected_token);
        }
        self.advance();
        if (self.peek() == .KwPlatform) {
            if (platform != null) {
                return self.pushMalformed(IR.NodeStore.HeaderIdx, .multiple_platforms);
            }
            self.advance();
            if (self.peek() != .String) {
                return self.pushMalformed(IR.NodeStore.HeaderIdx, .unexpected_token);
            }
            platform = self.pos;
            platform_name = name_tok;
        } else {
            if (self.peek() != .String) {
                return self.pushMalformed(IR.NodeStore.HeaderIdx, .unexpected_token);
            }
            const value = self.store.addExpr(.{ .string = .{
                .token = self.pos,
                .parts = &.{},
                .region = .{ .start = self.pos, .end = self.pos },
            } });
            self.store.scratch_record_fields.append(self.store.addRecordField(.{
                .name = name_tok,
                .value = value,
                .optional = false,
            })) catch exitOnOom();
        }
        self.advance();
        if (self.peek() != .Comma) {
            packages = self.store.scratch_record_fields.items[statement_scratch_top..];
        } else {
            self.advance();
        }
    }
    if (self.peek() != .CloseCurly) {
        return self.pushMalformed(IR.NodeStore.HeaderIdx, .unexpected_token);
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
                    .region = .{ .start = 0, .end = 0 },
                },
            };
            const idx = self.store.addHeader(header);
            return idx;
        }
    }
    return self.pushMalformed(IR.NodeStore.HeaderIdx, .no_platform);
}

pub fn parseStmt(self: *Parser) ?IR.NodeStore.StatementIdx {
    switch (self.peek()) {
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
        .LowerIdent => {
            const start = self.pos;
            if (self.peekNext() == .OpAssign) {
                self.advance();
                self.advance();
                const idx = self.parseBody();
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
            } else {
                // If not a decl
                const expr = self.parseExpr();
                const statement_idx = self.store.addStatement(.{ .expr = .{
                    .expr = expr,
                    .region = .{ .start = start, .end = start },
                } });
                return statement_idx;
            }
        },
        .KwExpect => {
            return null;
        },
        else => {
            const start = self.pos;
            const expr = self.parseExpr();
            const statement_idx = self.store.addStatement(.{ .expr = .{
                .expr = expr,
                .region = .{ .start = start, .end = self.pos },
            } });
            return statement_idx;
        },
    }
}

pub fn parsePattern(self: *Parser) IR.NodeStore.PatternIdx {
    const start = self.pos;
    var pattern: ?IR.NodeStore.PatternIdx = null;
    switch (self.peek()) {
        .LowerIdent => {
            pattern = self.store.addPattern(.{ .ident = .{
                .ident_tok = start,
                .region = .{ .start = start, .end = self.pos },
            } });
            self.advance();
        },
        .Underscore => {
            pattern = self.store.addPattern(.{ .underscore = .{
                .region = .{ .start = start, .end = start },
            } });
            self.advance();
        },
        else => {},
    }

    if (pattern) |p| {
        return p;
    }
    std.debug.panic("Should have gotten a valid pattern", .{});
}

pub fn parseExpr(self: *Parser) IR.NodeStore.ExprIdx {
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
                .parts = &.{},
                .region = .{ .start = start, .end = start },
            } });
        },
        .StringBegin => {
            // Start parsing string interpolation
            // StringBegin, OpenCurly, <expr>, CloseCurly, StringPart, OpenCurly, <expr>, CloseCurly, StringEnd
            const string_begin_node_idx = self.store.addExpr(.{ .string = .{
                .token = start,
                .parts = &.{},
                .region = .{ .start = start, .end = start },
            } });
            self.advanceOne();
            const scratch_top = self.store.scratch_exprs.items.len;
            defer self.store.scratch_exprs.shrinkRetainingCapacity(scratch_top);
            self.store.scratch_exprs.append(string_begin_node_idx) catch exitOnOom();
            while (self.peek() != .StringEnd and self.peek() != .EndOfFile) {
                if (self.peek() != .OpenCurly) {
                    break;
                }
                self.advanceOne();
                const ex = self.parseExpr();
                self.store.scratch_exprs.append(ex) catch exitOnOom();
                if (self.peek() != .CloseCurly) {
                    continue;
                }
                self.advanceOne();
                if (self.peek() == .StringPart) {
                    self.store.scratch_exprs.append(self.store.addExpr(.{ .string = .{
                        .token = self.pos,
                        .parts = &.{},
                        .region = .{ .start = self.pos, .end = self.pos },
                    } })) catch exitOnOom();
                    self.advanceOne();
                }
            }
            if (self.peek() == .StringEnd) {
                self.store.scratch_exprs.append(self.store.addExpr(.{ .string = .{
                    .token = self.pos,
                    .parts = &.{},
                    .region = .{ .start = self.pos, .end = self.pos },
                } })) catch exitOnOom();
                self.advance();
            }
            const parts = self.store.scratch_exprs.items[scratch_top..];
            expr = self.store.addExpr(.{ .string = .{
                .token = start,
                .parts = parts,
                .region = .{ .start = start, .end = self.pos },
            } });
            self.store.scratch_exprs.shrinkRetainingCapacity(scratch_top);
        },
        .OpenSquare => {
            const scratch_top = self.store.scratch_exprs.items.len;
            defer self.store.scratch_exprs.shrinkRetainingCapacity(scratch_top);
            self.advance();
            while (self.peek() != .CloseSquare) {
                self.store.scratch_exprs.append(self.parseExpr()) catch exitOnOom();
                if (self.peek() != .Comma) {
                    break;
                }
                self.advance();
            }
            if (self.peek() != .CloseSquare) {
                // No close problem
                self.addProblem(
                    .{ .region = .{ .start = start, .end = self.pos }, .tag = .list_not_closed },
                );
            }
            self.advance();
            const items = self.store.scratch_exprs.items[scratch_top..];
            expr = self.store.addExpr(.{ .list = .{
                .items = items,
                .region = .{ .start = start, .end = self.pos },
            } });
        },
        .OpBar => {
            const scratch_top = self.store.scratch_patterns.items.len;
            defer self.store.scratch_patterns.shrinkRetainingCapacity(scratch_top);
            self.advance();
            while (self.peek() != .OpBar) {
                self.store.scratch_patterns.append(self.parsePattern()) catch exitOnOom();
                if (self.peek() != .Comma) {
                    break;
                }
                self.advance();
            }
            if (self.peek() != .OpBar) {
                // self.addProblem()
                std.debug.panic("TODO: Add problem for unclosed args, got {s}\n", .{@tagName(self.peek())});
            }
            const args = self.store.scratch_patterns.items[scratch_top..];
            self.advance();
            const body = self.parseBody();
            expr = self.store.addExpr(.{ .lambda = .{
                .body = body,
                .args = args,
                .region = .{ .start = start, .end = self.pos },
            } });
        },
        .KwIf => {
            self.advance();
            const condition = self.parseExpr();
            const then = self.parseBody();
            if (self.peek() != .KwElse) {
                std.debug.panic("TODO: problem for no else", .{});
            }
            self.advance();
            const else_idx = self.parseBody();
            expr = self.store.addExpr(.{ .if_then_else = .{
                .region = .{ .start = start, .end = self.pos },
                .condition = condition,
                .then = then,
                .@"else" = else_idx,
            } });
        },
        .KwDbg => {
            self.advance();
            const e = self.parseExpr();
            expr = self.store.addExpr(.{ .dbg = .{
                .region = .{ .start = start, .end = self.pos },
                .expr = e,
            } });
        },
        else => {
            return self.pushMalformed(IR.NodeStore.ExprIdx, .unexpected_token);
        },
    }
    if (expr) |e| {
        var expression = e;
        // Check for an apply...
        if (self.peek() == .OpenRound) {
            const scratch_top = self.store.scratch_exprs.items.len;
            self.advance();
            while (self.peek() != .CloseRound) {
                const arg_expression = self.parseExpr();
                self.store.scratch_exprs.append(arg_expression) catch exitOnOom();
                if (self.peek() != .Comma) {
                    break;
                }
                self.advance();
            }
            if (self.peek() != .CloseRound) {
                // Problem
                std.debug.print("Tokens:\n{any}\n", .{self.tok_buf.tokens.items(.tag)});
                std.debug.panic("TODO: malformed apply @ {d}, got {s}", .{ self.pos, @tagName(self.peek()) });
            }
            self.advance();
            const args = self.store.scratch_exprs.items[scratch_top..];
            expression = self.store.addExpr(.{ .apply = .{
                .args = args,
                .@"fn" = e,
                .region = .{ .start = start, .end = self.pos },
            } });
            self.store.scratch_exprs.shrinkRetainingCapacity(scratch_top);
        }
        if (self.peek() == .NoSpaceOpQuestion) {
            expression = self.store.addExpr(.{ .suffix_single_question = .{
                .expr = expression,
                .region = .{ .start = start, .end = self.pos },
            } });
            self.advance();
        }
        // Check for try suffix...
        return expression;
    }
    return self.pushMalformed(IR.NodeStore.ExprIdx, .unexpected_token);
}

pub fn parseBody(self: *Parser) IR.NodeStore.BodyIdx {
    if (self.peek() == .OpenCurly) {
        self.advance();
        const scratch_top = self.store.scratch_statements.items.len;
        defer self.store.scratch_statements.shrinkRetainingCapacity(scratch_top);

        while (true) {
            const statement = self.parseStmt() orelse break;
            self.store.scratch_statements.append(statement) catch exitOnOom();
            if (self.peek() == .CloseCurly) {
                self.advance();
                break;
            }
        }

        const statements = self.store.scratch_statements.items[scratch_top..];

        const body = self.store.addBody(.{ .statements = statements, .whitespace = self.pos - 1 });
        return body;
    } else {
        const start = self.pos;
        const expr = self.parseExpr();
        const statement = self.store.addStatement(.{ .expr = .{
            .expr = expr,
            .region = .{ .start = start, .end = self.pos },
        } });
        const body = self.store.addBody(.{ .statements = &.{statement}, .whitespace = null });
        return body;
    }
}

pub fn addProblem(self: *Parser, diagnostic: IR.Diagnostic) void {
    self.diagnostics.append(diagnostic) catch exitOnOom();
}
