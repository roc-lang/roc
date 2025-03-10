const std = @import("std");

const IR = @import("IR.zig");
const NodeList = IR.NodeList;

const tokenize = @import("tokenize.zig");
const TokenizedBuffer = tokenize.TokenizedBuffer;
const Token = tokenize.Token;
const TokenIdx = Token.Idx;

const exitOnOom = @import("../../collections/utils.zig").exitOnOom;

/// A parser which tokenizes and parses source code into an abstract syntax tree.
pub const Parser = @This();

gpa: std.mem.Allocator,
pos: TokenIdx,
tok_buf: TokenizedBuffer,
store: IR.NodeStore,
scratch_nodes: std.ArrayListUnmanaged(IR.Node.Idx),
diagnostics: std.ArrayListUnmanaged(IR.Diagnostic),

/// init the parser from a buffer of tokens
pub fn init(tokens: TokenizedBuffer) Parser {
    const estimated_node_count = (tokens.tokens.len + 2) / 2;
    const store = IR.NodeStore.initWithCapacity(tokens.env.gpa, estimated_node_count);

    return Parser{
        .gpa = tokens.env.gpa,
        .pos = 0,
        .tok_buf = tokens,
        .store = store,
        .scratch_nodes = .{},
        .diagnostics = .{},
    };
}

/// deninit the parser memory
pub fn deinit(parser: *Parser) void {
    parser.scratch_nodes.deinit(parser.gpa);
    parser.diagnostics.deinit(parser.gpa);
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

/// helper to advance the parser until a non-newline token is encountered
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

///
pub fn advanceOne(self: *Parser) void {
    self.pos += 1;
    // We have an EndOfFile token that we never expect to advance past
    std.debug.assert(self.pos < self.tok_buf.tokens.len);
}

const ExpectError = error{expected_not_found};

/// look ahead at the next token and return an error if it does not have the expected tag
pub fn expect(self: *Parser, expected: Token.Tag) !void {
    if (self.peek() != expected) {
        return ExpectError.expected_not_found;
    }
    self.advance();
}

/// look ahead at the next token
///
/// **note** caller is responsible to ensure this isn't the last token
pub fn peek(self: *Parser) Token.Tag {
    std.debug.assert(self.pos < self.tok_buf.tokens.len);
    return self.tok_buf.tokens.items(.tag)[self.pos];
}
/// todo -- what is this?
pub fn peekLast(self: Parser) ?Token.Tag {
    if (self.pos == 0) {
        return null;
    }
    return self.tok_buf.tokens.items(.tag)[self.pos - 1];
}
/// peek at the next available token
pub fn peekNext(self: Parser) Token.Tag {
    const next = self.pos + 1;
    if (next >= self.tok_buf.tokens.len) {
        return .EndOfFile;
    }
    return self.tok_buf.tokens.items(.tag)[next];
}
/// add a diagnostic error
pub fn pushDiagnostic(self: *Parser, tag: IR.Diagnostic.Tag, region: IR.Region) void {
    self.diagnostics.append(self.gpa, .{
        .tag = tag,
        .region = region,
    }) catch |err| exitOnOom(err);
}
/// add a malformed token
pub fn pushMalformed(self: *Parser, comptime t: type, tag: IR.Diagnostic.Tag) t {
    const pos = self.pos;
    if (self.peek() != .EndOfFile) {
        self.advanceOne(); // TODO: find a better point to advance to
    }
    self.diagnostics.append(self.gpa, .{
        .tag = tag,
        .region = .{ .start = pos, .end = pos },
    }) catch |err| exitOnOom(err);
    return self.store.addMalformed(t, tag, pos);
}
/// parse a `.roc` module
///
/// the tokens are provided at Parser initialisation
pub fn parseFile(self: *Parser) void {
    self.store.emptyScratch();
    _ = self.store.addFile(.{
        .header = IR.NodeStore.HeaderIdx{ .id = 0 },
        .statements = .{ .span = .{
            .start = 0,
            .len = 0,
        } },
        .region = .{ .start = 0, .end = 0 },
    });

    const header = self.parseHeader();
    const scratch_top = self.store.scratchStatementTop();

    while (self.peek() != .EndOfFile) {
        const current_scratch_top = self.store.scratchStatementTop();
        if (self.parseStmt()) |idx| {
            std.debug.assert(self.store.scratchStatementTop() == current_scratch_top);
            self.store.addScratchStatement(idx);
        } else {
            std.debug.assert(self.store.scratchStatementTop() == current_scratch_top);
            break;
        }
    }

    // TODO: fix me, blows up on empty input
    // std.debug.assert(self.store.scratch_statements.items.len > 0);

    _ = self.store.addFile(.{
        .header = header,
        .statements = self.store.statementSpanFrom(scratch_top),
        .region = .{ .start = 0, .end = @intCast(self.tok_buf.tokens.len - 1) },
    });
}

fn parseCollection(self: *Parser, comptime T: type, end_token: Token.Tag, scratch: *std.ArrayListUnmanaged(T), parser: fn (*Parser) T) ExpectError!usize {
    const scratch_top = scratch.items.len;
    while (self.peek() != end_token and self.peek() != .EndOfFile) {
        scratch.append(self.gpa, parser(self)) catch |err| exitOnOom(err);
        self.expect(.Comma) catch {
            break;
        };
    }
    self.expect(end_token) catch {
        return ExpectError.expected_not_found;
    };
    return scratch_top;
}

/// Parses the items of type T until we encounter end_token, with each item separated by a Comma token
fn parseCollectionSpan(self: *Parser, comptime T: type, end_token: Token.Tag, scratch_fn: fn (*IR.NodeStore, T) void, parser: fn (*Parser) T) ExpectError!void {
    while (self.peek() != end_token and self.peek() != .EndOfFile) {
        scratch_fn(&self.store, parser(self));
        self.expect(.Comma) catch {
            break;
        };
    }
    self.expect(end_token) catch {
        return ExpectError.expected_not_found;
    };
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
    std.debug.assert(self.peek() == .KwModule);

    const start = self.pos;

    self.advance();

    // Get exposes
    if (self.peek() != .OpenSquare) {
        // std.debug.panic("TODO: Handle header with no exposes open bracket: {s}", .{@tagName(self.peek())});
        const reason: IR.Diagnostic.Tag = .header_expected_open_bracket;
        self.pushDiagnostic(reason, .{
            .start = self.pos,
            .end = self.pos,
        });
        return self.store.addHeader(.{ .malformed = .{ .reason = reason } });
    }
    self.advance();
    const scratch_top = self.store.scratchTokenTop();
    while (self.peek() != .CloseSquare and self.peek() != .EndOfFile) {
        if (self.peek() != .LowerIdent and self.peek() != .UpperIdent) {
            // std.debug.panic("TODO: Handler header bad exposes contents: {s}", .{@tagName(self.peek())});
            const reason: IR.Diagnostic.Tag = .header_unexpected_token;
            self.pushDiagnostic(reason, .{
                .start = self.pos,
                .end = self.pos,
            });
            return self.store.addHeader(.{ .malformed = .{ .reason = reason } });
        }

        self.store.addScratchToken(self.pos);
        self.advance();

        if (self.peek() != .Comma) {
            break;
        }
        self.advance();
    }
    if (self.peek() != .CloseSquare) {
        // std.debug.panic("TODO: Handle Bad header no closing exposes bracket: {s}", .{@tagName(self.peek())});
        const reason: IR.Diagnostic.Tag = .header_expected_close_bracket;
        self.pushDiagnostic(reason, .{
            .start = self.pos,
            .end = self.pos,
        });
        return self.store.addHeader(.{ .malformed = .{ .reason = reason } });
    }

    const exposes = self.store.tokenSpanFrom(scratch_top);
    self.advance();

    return self.store.addHeader(.{ .module = .{
        .region = .{ .start = start, .end = self.pos },
        .exposes = exposes,
    } });
}

/// parse an `.roc` application header
///
/// e.g. `module [ foo ]`
pub fn parseAppHeader(self: *Parser) IR.NodeStore.HeaderIdx {
    var platform: ?IR.NodeStore.ExprIdx = null;
    var platform_name: ?TokenIdx = null;

    std.debug.assert(self.peek() == .KwApp);
    self.advance();

    // Get provides
    if (self.peek() != .OpenSquare) {
        return self.pushMalformed(IR.NodeStore.HeaderIdx, .expected_provides_open_square);
    }
    self.advance();
    const scratch_top = self.store.scratchTokenTop();
    while (self.peek() != .CloseSquare and self.peek() != .EndOfFile) {
        if (self.peek() != .LowerIdent and self.peek() != .UpperIdent) {
            self.store.clearScratchTokensFrom(scratch_top);
            return self.pushMalformed(IR.NodeStore.HeaderIdx, .expected_provides);
        }

        self.store.addScratchToken(self.pos);
        self.advance();

        if (self.peek() != .Comma) {
            break;
        }
        self.advance();
    }
    if (self.peek() != .CloseSquare) {
        self.store.clearScratchTokensFrom(scratch_top);
        return self.pushMalformed(IR.NodeStore.HeaderIdx, .expected_provides_close_square);
    }
    const provides = self.store.tokenSpanFrom(scratch_top);
    self.advance();

    // Get platform and packages
    const fields_scratch_top = self.store.scratchRecordFieldTop();
    if (self.peek() != .OpenCurly) {
        return self.pushMalformed(IR.NodeStore.HeaderIdx, .expected_package_platform_open_curly);
    }
    self.advance();
    while (self.peek() != .CloseCurly and self.peek() != .EndOfFile) {
        const entry_start = self.pos;
        if (self.peek() != .LowerIdent) {
            self.store.clearScratchRecordFieldsFrom(fields_scratch_top);
            return self.pushMalformed(IR.NodeStore.HeaderIdx, .expected_package_or_platform_name);
        }
        const name_tok = self.pos;
        self.advance();
        if (self.peek() != .OpColon) {
            self.store.clearScratchRecordFieldsFrom(fields_scratch_top);
            return self.pushMalformed(IR.NodeStore.HeaderIdx, .expected_package_or_platform_colon);
        }
        self.advance();
        if (self.peek() == .KwPlatform) {
            if (platform != null) {
                self.store.clearScratchRecordFieldsFrom(fields_scratch_top);
                return self.pushMalformed(IR.NodeStore.HeaderIdx, .multiple_platforms);
            }
            self.advance();
            if (self.peek() != .StringStart) {
                self.store.clearScratchRecordFieldsFrom(fields_scratch_top);
                return self.pushMalformed(IR.NodeStore.HeaderIdx, .expected_platform_string);
            }
            platform = self.parseStringExpr();
            platform_name = name_tok;
        } else {
            if (self.peek() != .StringStart) {
                self.store.clearScratchRecordFieldsFrom(fields_scratch_top);
                return self.pushMalformed(IR.NodeStore.HeaderIdx, .expected_package_or_platform_string);
            }
            const value = self.parseStringExpr();
            self.store.addScratchRecordField(self.store.addRecordField(.{
                .name = name_tok,
                .value = value,
                .optional = false,
                .region = .{ .start = entry_start, .end = self.pos },
            }));
        }
        if (self.peek() != .Comma) {
            break;
        } else {
            self.advance();
        }
    }
    if (self.peek() != .CloseCurly) {
        self.store.clearScratchRecordFieldsFrom(fields_scratch_top);
        return self.pushMalformed(IR.NodeStore.HeaderIdx, .expected_package_platform_close_curly);
    }
    const packages = self.store.recordFieldSpanFrom(fields_scratch_top);
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

/// parse a roc statement
///
/// e.g. `import Foo`, or `foo = 2 + x`
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
                    .exposes = .{ .span = .{ .start = 0, .len = 0 } },
                    .region = .{ .start = start, .end = self.pos },
                } });
                self.advance();
                if (self.peek() == .Newline) {
                    self.advance();
                }
                return statement_idx;
            }
            if (self.peek() == .Newline) {
                self.advance();
            }
            return null;
        },
        .KwExpect => {
            const start = self.pos;
            self.advance();
            const body = self.parseExpr();
            const statement_idx = self.store.addStatement(.{ .expect = .{
                .body = body,
                .region = .{ .start = start, .end = self.pos },
            } });
            if (self.peek() == .Newline) {
                self.advance();
            }
            return statement_idx;
        },
        .KwCrash => {
            const start = self.pos;
            self.advance();
            const expr = self.parseExpr();
            const statement_idx = self.store.addStatement(.{ .crash = .{
                .expr = expr,
                .region = .{ .start = start, .end = self.pos },
            } });
            if (self.peek() == .Newline) {
                self.advance();
            }
            return statement_idx;
        },
        .KwReturn => {
            const start = self.pos;
            self.advance();
            const expr = self.parseExpr();
            const statement_idx = self.store.addStatement(.{ .@"return" = .{
                .expr = expr,
                .region = .{ .start = start, .end = self.pos },
            } });
            if (self.peek() == .Newline) {
                self.advance();
            }
            return statement_idx;
        },
        .LowerIdent => {
            const start = self.pos;
            if (self.peekNext() == .OpAssign) {
                self.advance(); // Advance past LowerIdent
                self.advance(); // Advance past OpAssign
                const idx = self.parseExpr();
                const patt_idx = self.store.addPattern(.{ .ident = .{
                    .ident_tok = start,
                    .region = .{ .start = start, .end = start },
                } });
                const statement_idx = self.store.addStatement(.{ .decl = .{
                    .pattern = patt_idx,
                    .body = idx,
                    .region = .{ .start = start, .end = self.pos },
                } });
                if (self.peek() == .Newline) {
                    self.advance();
                }
                return statement_idx;
            } else if (self.peekNext() == .OpColon) {
                self.advance(); // Advance past LowerIdent
                self.advance(); // Advance past OpColon
                const anno = self.parseTypeAnno(.not_looking_for_args);
                const statement_idx = self.store.addStatement(.{ .type_anno = .{
                    .anno = anno,
                    .name = start,
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
                if (self.peek() == .Newline) {
                    self.advance();
                }
                return statement_idx;
            }
        },
        .UpperIdent => {
            const start = self.pos;
            if (self.peekNext() == .OpColon or self.peekNext() == .LowerIdent) {
                const header = self.parseTypeHeader();
                if (self.peek() != .OpColon) {
                    return self.pushMalformed(IR.NodeStore.StatementIdx, .unexpected_token);
                }
                self.advance();
                const anno = self.parseTypeAnno(.not_looking_for_args);
                const statement_idx = self.store.addStatement(.{ .type_decl = .{
                    .header = header,
                    .anno = anno,
                    .region = .{ .start = start, .end = self.pos },
                } });
                return statement_idx;
            }
            const expr = self.parseExpr();
            const statement_idx = self.store.addStatement(.{ .expr = .{
                .expr = expr,
                .region = .{ .start = start, .end = self.pos },
            } });
            return statement_idx;
        },
        else => {
            const start = self.pos;
            const expr = self.parseExpr();
            const statement_idx = self.store.addStatement(.{ .expr = .{
                .expr = expr,
                .region = .{ .start = start, .end = self.pos },
            } });
            if (self.peek() == .Newline) {
                self.advance();
            }
            return statement_idx;
        },
    }
}

/// Whether Pattern Alternatives are allowed in the current context
const Alternatives = enum {
    alternatives_allowed,
    alternatives_forbidden,
};

/// todo -- what does this do?
pub fn parsePattern(self: *Parser, alternatives: Alternatives) IR.NodeStore.PatternIdx {
    const outer_start = self.pos;
    const patterns_scratch_top = self.store.scratchPatternTop();
    errdefer self.store.clearScratchPatternsFrom(patterns_scratch_top);
    while (self.peek() != .EndOfFile) {
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
            .UpperIdent => {
                if (self.peekNext() != .NoSpaceOpenRound) {
                    pattern = self.store.addPattern(.{ .tag = .{
                        .region = .{ .start = start, .end = self.pos },
                        .args = .{ .span = .{
                            .start = 0,
                            .len = 0,
                        } },
                        .tag_tok = start,
                    } });
                    self.advance();
                } else {
                    self.advance(); // Advance past Upperident
                    self.advance(); // Advance past NoSpaceOpenRound
                    // Parse args
                    const scratch_top = self.store.scratchPatternTop();
                    self.parseCollectionSpan(IR.NodeStore.PatternIdx, .CloseRound, IR.NodeStore.addScratchPattern, parsePatternWithAlts) catch {
                        while (self.peek() != .CloseRound and self.peek() != .EndOfFile) {
                            self.advance();
                        }
                        self.store.clearScratchPatternsFrom(scratch_top);
                        return self.pushMalformed(IR.NodeStore.PatternIdx, .unexpected_token);
                    };
                    const args = self.store.patternSpanFrom(scratch_top);
                    pattern = self.store.addPattern(.{ .tag = .{
                        .region = .{ .start = start, .end = self.pos },
                        .args = args,
                        .tag_tok = start,
                    } });
                }
            },
            .StringStart => {
                pattern = self.parseStringPattern();
            },
            .Int => {
                // Should be number
                pattern = self.store.addPattern(.{ .number = .{
                    .region = .{ .start = start, .end = self.pos },
                    .number_tok = start,
                } });
                self.advance();
            },
            .Float => {
                // Should be number
                pattern = self.store.addPattern(.{ .number = .{
                    .region = .{ .start = start, .end = self.pos },
                    .number_tok = start,
                } });
                self.advance();
            },
            .OpenSquare => {
                // List
                self.advance();
                const scratch_top = self.store.scratchPatternTop();
                self.parseCollectionSpan(IR.NodeStore.PatternIdx, .CloseSquare, IR.NodeStore.addScratchPattern, parsePatternWithAlts) catch {
                    while (self.peek() != .CloseSquare and self.peek() != .EndOfFile) {
                        self.advance();
                    }
                    self.store.clearScratchPatternsFrom(scratch_top);
                    return self.pushMalformed(IR.NodeStore.PatternIdx, .unexpected_token);
                };
                const patterns = self.store.patternSpanFrom(scratch_top);

                pattern = self.store.addPattern(.{ .list = .{
                    .region = .{ .start = start, .end = self.pos },
                    .patterns = patterns,
                } });
            },
            .OpenCurly => {
                self.advance();
                const scratch_top = self.store.scratchPatternRecordFieldTop();
                while (self.peek() != .CloseCurly) {
                    self.store.addScratchPatternRecordField(self.parsePatternRecordField(alternatives));
                    if (self.peek() != .Comma) {
                        break;
                    }
                    self.advance();
                }
                const fields = self.store.patternRecordFieldSpanFrom(scratch_top);
                if (self.peek() != .CloseCurly) {
                    return self.pushMalformed(IR.NodeStore.PatternIdx, .unexpected_token);
                }
                self.advance();
                pattern = self.store.addPattern(.{ .record = .{
                    .region = .{ .start = start, .end = self.pos },
                    .fields = fields,
                } });
            },
            .DoubleDot => {
                var name: ?TokenIdx = null;
                var end: u32 = self.pos;
                self.advance();
                if (self.peek() == .KwAs) {
                    self.advance();
                    if (self.peek() != .LowerIdent) {
                        return self.pushMalformed(IR.NodeStore.PatternIdx, .unexpected_token);
                    }
                    name = self.pos;
                    end = self.pos;
                    self.advance();
                }
                pattern = self.store.addPattern(.{ .list_rest = .{
                    .region = .{ .start = start, .end = end },
                    .name = name,
                } });
            },
            .Underscore => {
                pattern = self.store.addPattern(.{ .underscore = .{
                    .region = .{ .start = start, .end = start },
                } });
                self.advance();
            },
            .OpenRound, .NoSpaceOpenRound => {
                self.advance();
                const scratch_top = self.store.scratchPatternTop();
                self.parseCollectionSpan(IR.NodeStore.PatternIdx, .CloseRound, IR.NodeStore.addScratchPattern, parsePatternWithAlts) catch {
                    while (self.peek() != .CloseRound and self.peek() != .EndOfFile) {
                        self.advance();
                    }
                    self.store.clearScratchPatternsFrom(scratch_top);
                    return self.pushMalformed(IR.NodeStore.PatternIdx, .unexpected_token);
                };
                const patterns = self.store.patternSpanFrom(scratch_top);

                pattern = self.store.addPattern(.{ .tuple = .{
                    .patterns = patterns,
                    .region = .{ .start = start, .end = self.pos },
                } });
            },
            else => {
                // std.debug.panic("TODO: Handle parsing pattern starting with: {s}", .{@tagName(self.peek())})
                const reason: IR.Diagnostic.Tag = .pattern_unexpected_token;
                self.pushDiagnostic(reason, .{
                    .start = self.pos,
                    .end = self.pos,
                });
                return self.store.addPattern(.{ .malformed = .{ .reason = reason } });
            },
        }

        if (pattern) |p| {
            if (alternatives == .alternatives_forbidden) {
                return p;
            }
            if (self.peek() != .OpBar) {
                if ((self.store.scratchPatternTop() - patterns_scratch_top) == 0) {
                    return p;
                }
                self.store.addScratchPattern(p);
                break;
            }
            self.store.addScratchPattern(p);
            self.advance();
        }
    }
    if ((self.store.scratchPatternTop() - patterns_scratch_top) == 0) {
        std.debug.panic("Should have gotten a valid pattern, pos={d} peek={s}", .{ self.pos, @tagName(self.peek()) });
    }
    const patterns = self.store.patternSpanFrom(patterns_scratch_top);
    return self.store.addPattern(.{ .alternatives = .{
        .region = .{ .start = outer_start, .end = self.pos },
        .patterns = patterns,
    } });
}

fn parsePatternNoAlts(self: *Parser) IR.NodeStore.PatternIdx {
    return self.parsePattern(.alternatives_forbidden);
}
fn parsePatternWithAlts(self: *Parser) IR.NodeStore.PatternIdx {
    return self.parsePattern(.alternatives_allowed);
}

/// todo
pub fn parsePatternRecordField(self: *Parser, alternatives: Alternatives) IR.NodeStore.PatternRecordFieldIdx {
    const field_start = self.pos;
    if (self.peek() == .DoubleDot) {
        self.advance();
        var name: u32 = 0;
        if (self.peek() == .LowerIdent) {
            name = self.pos;
            self.advance();
        }
        return self.store.addPatternRecordField(.{
            .name = name,
            .value = null,
            .rest = true,
            .region = .{ .start = field_start, .end = self.pos },
        });
    }
    if (self.peek() != .LowerIdent) {
        while (self.peek() != .CloseCurly and self.peek() != .EndOfFile) {
            self.advance();
        }
        return self.pushMalformed(IR.NodeStore.PatternRecordFieldIdx, .unexpected_token);
    }
    const name = self.pos;
    self.advance();
    var value: ?IR.NodeStore.PatternIdx = null;
    if (self.peek() != .OpColon and (self.peekNext() != .Comma or self.peekNext() != .CloseCurly)) {
        while (self.peek() != .CloseCurly and self.peek() != .EndOfFile) {
            self.advance();
        }
        return self.pushMalformed(IR.NodeStore.PatternRecordFieldIdx, .unexpected_token);
    }
    self.advance();
    if (self.peekNext() != .Comma or self.peekNext() != .CloseCurly) {
        const patt = self.parsePattern(alternatives);
        value = patt;
    }

    return self.store.addPatternRecordField(.{
        .name = name,
        .value = value,
        .rest = false,
        .region = .{ .start = field_start, .end = self.pos },
    });
}

/// todo
pub fn parseExpr(self: *Parser) IR.NodeStore.ExprIdx {
    return self.parseExprWithBp(0);
}

/// todo
pub fn parseExprWithBp(self: *Parser, min_bp: u8) IR.NodeStore.ExprIdx {
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
        .StringStart => {
            expr = self.parseStringExpr();
        },
        .OpenSquare => {
            self.advance();
            const scratch_top = self.store.scratchExprTop();
            self.parseCollectionSpan(IR.NodeStore.ExprIdx, .CloseSquare, IR.NodeStore.addScratchExpr, parseExpr) catch {
                while (self.peek() != .CloseSquare and self.peek() != .EndOfFile) {
                    self.advance();
                }
                self.store.clearScratchExprsFrom(scratch_top);
                return self.pushMalformed(IR.NodeStore.ExprIdx, .unexpected_token);
            };
            const items = self.store.exprSpanFrom(scratch_top);
            expr = self.store.addExpr(.{ .list = .{
                .items = items,
                .region = .{ .start = start, .end = self.pos },
            } });
        },
        .OpenRound => {
            self.advance();
            // TODO: Parenthesized expressions
            const scratch_top = self.store.scratchExprTop();
            self.parseCollectionSpan(IR.NodeStore.ExprIdx, .CloseRound, IR.NodeStore.addScratchExpr, parseExpr) catch {
                while (self.peek() != .CloseRound and self.peek() != .EndOfFile) {
                    self.advance();
                }
                self.store.clearScratchExprsFrom(scratch_top);
                return self.pushMalformed(IR.NodeStore.ExprIdx, .unexpected_token);
            };
            const items = self.store.exprSpanFrom(scratch_top);
            expr = self.store.addExpr(.{ .tuple = .{
                .items = items,
                .region = .{ .start = start, .end = self.pos },
            } });
        },
        .OpenCurly => {
            self.advance();
            // Is this a Record or a Block?
            if (self.peek() == .LowerIdent and (self.peekNext() == .OpColon or self.peekNext() == .Comma)) {
                // This is the best guesstimation of this being a Record for now.  I believe we have to have a NoSpaceOpColon
                // for this to be full-proof without backtracking.
                const scratch_top = self.store.scratchRecordFieldTop();
                self.parseCollectionSpan(IR.NodeStore.RecordFieldIdx, .CloseCurly, IR.NodeStore.addScratchRecordField, parseRecordField) catch {
                    self.store.clearScratchRecordFieldsFrom(scratch_top);
                    return self.pushMalformed(IR.NodeStore.ExprIdx, .unexpected_token);
                };
                const fields = self.store.recordFieldSpanFrom(scratch_top);
                expr = self.store.addExpr(.{ .record = .{
                    .fields = fields,
                    .region = .{ .start = start, .end = self.pos },
                } });
            } else {
                const scratch_top = self.store.scratchStatementTop();

                while (true) {
                    const statement = self.parseStmt() orelse break;
                    self.store.addScratchStatement(statement);
                    if (self.peek() == .CloseCurly) {
                        self.advance();
                        break;
                    }
                }

                const statements = self.store.statementSpanFrom(scratch_top);

                expr = self.store.addExpr(.{ .block = .{
                    .statements = statements,
                    .region = .{ .start = start, .end = self.pos },
                } });
            }
        },
        .OpBar => {
            self.advance();
            const scratch_top = self.store.scratchPatternTop();
            self.parseCollectionSpan(IR.NodeStore.PatternIdx, .OpBar, IR.NodeStore.addScratchPattern, parsePatternNoAlts) catch {
                self.store.clearScratchPatternsFrom(scratch_top);
                return self.pushMalformed(IR.NodeStore.ExprIdx, .unexpected_token);
            };
            const args = self.store.patternSpanFrom(scratch_top);

            const body = self.parseExpr();
            expr = self.store.addExpr(.{ .lambda = .{
                .body = body,
                .args = args,
                .region = .{ .start = start, .end = self.pos },
            } });
        },
        .KwIf => {
            self.advance();
            const condition = self.parseExpr();
            const then = self.parseExpr();
            if (self.peek() != .KwElse) {
                // std.debug.panic("TODO: problem for no else {s}@{d}", .{ @tagName(self.peek()), self.pos });
                const reason: IR.Diagnostic.Tag = .expr_if_missing_else;
                self.pushDiagnostic(reason, .{
                    .start = self.pos,
                    .end = self.pos,
                });
                return self.store.addExpr(.{ .malformed = .{ .reason = reason } });
            }
            self.advance();
            const else_idx = self.parseExpr();
            expr = self.store.addExpr(.{ .if_then_else = .{
                .region = .{ .start = start, .end = self.pos },
                .condition = condition,
                .then = then,
                .@"else" = else_idx,
            } });
        },
        .KwMatch => {
            self.advance();
            const e = self.parseExpr();

            self.expect(.OpenCurly) catch {
                return self.pushMalformed(IR.NodeStore.ExprIdx, .unexpected_token);
            };
            const scratch_top = self.store.scratchWhenBranchTop();
            while (self.peek() != .CloseCurly and self.peek() != .EndOfFile) {
                self.store.addScratchWhenBranch(self.parseBranch());
                if (self.peek() == .Comma) {
                    self.advance();
                }
            }
            const branches = self.store.whenBranchSpanFrom(scratch_top);
            if (self.peek() != .CloseCurly) {
                return self.pushMalformed(IR.NodeStore.ExprIdx, .unexpected_token);
            }
            self.advance();
            expr = self.store.addExpr(.{ .match = .{
                .region = .{ .start = start, .end = self.pos },
                .expr = e,
                .branches = branches,
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
        .TripleDot => {
            expr = self.store.addExpr(.{ .ellipsis = .{
                .region = .{ .start = start, .end = self.pos },
            } });
            self.advance();
        },
        else => {
            return self.pushMalformed(IR.NodeStore.ExprIdx, .unexpected_token);
        },
    }
    if (expr) |e| {
        var expression = self.parseExprSuffix(start, e);
        while (self.peek() == .NoSpaceDotInt or self.peek() == .NoSpaceDotLowerIdent) {
            const tok = self.peek();
            if (tok == .NoSpaceDotInt) { // NoSpaceDotInt
                // std.debug.panic("TODO: Handle NoSpaceDotInt case", .{});
                const reason: IR.Diagnostic.Tag = .expr_no_space_dot_int;
                self.pushDiagnostic(reason, .{
                    .start = self.pos,
                    .end = self.pos,
                });
                return self.store.addExpr(.{ .malformed = .{ .reason = reason } });
            } else { // NoSpaceDotLowerIdent
                const s = self.pos;
                const ident = self.store.addExpr(.{ .ident = .{
                    .region = .{ .start = self.pos, .end = self.pos },
                    .token = self.pos,
                    .qualifier = null,
                } });
                self.advance();
                const ident_suffixed = self.parseExprSuffix(s, ident);
                expression = self.store.addExpr(.{ .field_access = .{
                    .region = .{ .start = start, .end = self.pos },
                    .operator = start,
                    .left = expression,
                    .right = ident_suffixed,
                } });
            }
        }
        while (getTokenBP(self.peek())) |bp| {
            if (bp.left < min_bp) {
                break;
            }
            const opPos = self.pos;

            self.advance();

            const nextExpr = self.parseExprWithBp(bp.right);

            expression = self.store.addExpr(.{ .bin_op = .{
                .left = expression,
                .right = nextExpr,
                .operator = opPos,
                .region = .{ .start = start, .end = self.pos },
            } });
        }
        return expression;
    }
    return self.pushMalformed(IR.NodeStore.ExprIdx, .unexpected_token);
}

/// todo
fn parseExprSuffix(self: *Parser, start: u32, e: IR.NodeStore.ExprIdx) IR.NodeStore.ExprIdx {
    var expression = e;
    // Check for an apply...
    if (self.peek() == .NoSpaceOpenRound) {
        self.advance();
        const scratch_top = self.store.scratchExprTop();
        self.parseCollectionSpan(IR.NodeStore.ExprIdx, .CloseRound, IR.NodeStore.addScratchExpr, parseExpr) catch {
            self.store.clearScratchExprsFrom(scratch_top);
            return self.pushMalformed(IR.NodeStore.ExprIdx, .unexpected_token);
        };
        const args = self.store.exprSpanFrom(scratch_top);

        expression = self.store.addExpr(.{ .apply = .{
            .args = args,
            .@"fn" = e,
            .region = .{ .start = start, .end = self.pos },
        } });
    }
    if (self.peek() == .NoSpaceOpQuestion) {
        expression = self.store.addExpr(.{ .suffix_single_question = .{
            .expr = expression,
            .operator = start,
            .region = .{ .start = start, .end = self.pos },
        } });
        self.advance();
    }
    return expression;
}

/// todo
pub fn parseRecordField(self: *Parser) IR.NodeStore.RecordFieldIdx {
    const start = self.pos;
    self.expect(.LowerIdent) catch {
        return self.pushMalformed(IR.NodeStore.RecordFieldIdx, .unexpected_token);
    };
    const name = start;
    var value: ?IR.NodeStore.ExprIdx = null;
    if (self.peek() == .OpColon) {
        self.advance();
        value = self.parseExpr();
    }

    return self.store.addRecordField(.{
        .name = name,
        .value = value,
        .optional = false,
        .region = .{ .start = start, .end = self.pos },
    });
}

/// todo
pub fn parseBranch(self: *Parser) IR.NodeStore.WhenBranchIdx {
    const start = self.pos;
    const p = self.parsePattern(.alternatives_allowed);
    if (self.peek() == .OpArrow) {
        self.advance();
    }
    const b = self.parseExpr();
    return self.store.addWhenBranch(.{
        .region = .{ .start = start, .end = self.pos },
        .pattern = p,
        .body = b,
    });
}

/// todo
pub fn parseStringExpr(self: *Parser) IR.NodeStore.ExprIdx {
    std.debug.assert(self.peek() == .StringStart);
    const start = self.pos;
    // Start parsing string with possible interpolations
    // e.g.:
    // StringStart, StringPart, OpenStringInterpolation, <expr>, CloseStringInterpolation, StringPart, StringEnd
    self.advanceOne();
    const scratch_top = self.store.scratchExprTop();
    while (self.peek() != .EndOfFile) {
        switch (self.peek()) {
            .StringEnd => {
                self.advanceOne();
                break;
            },
            .StringPart => {
                const index = self.store.addExpr(.{ .string_part = .{
                    .token = self.pos,
                    .region = .{ .start = self.pos, .end = self.pos },
                } });
                self.advanceOne();
                self.store.addScratchExpr(index);
            },
            .OpenStringInterpolation => {
                self.advanceOne();
                const ex = self.parseExpr();
                self.store.addScratchExpr(ex);
                // This assert isn't really correct, but we'll have to turn this into a malformed expression
                // TODO...
                std.debug.assert(self.peek() == .CloseStringInterpolation);
                self.advanceOne();
            },
            else => {
                // Something is broken in the tokenizer if we get here!
                // std.debug.print("Unexpected token in string: {s}\n", .{@tagName(self.peek())});
                // unreachable;
                const reason: IR.Diagnostic.Tag = .string_unexpected_token;
                self.pushDiagnostic(reason, .{
                    .start = self.pos,
                    .end = self.pos,
                });
                return self.store.addExpr(.{ .malformed = .{ .reason = reason } });
            },
        }
    }
    const parts = self.store.exprSpanFrom(scratch_top);
    const expr = self.store.addExpr(.{ .string = .{
        .token = start,
        .parts = parts,
        .region = .{ .start = start, .end = self.pos },
    } });
    return expr;
}

/// todo
pub fn parseStringPattern(self: *Parser) IR.NodeStore.PatternIdx {
    const start = self.pos;
    const inner = parseStringExpr(self);
    const patt_idx = self.store.addPattern(.{ .string = .{
        .string_tok = start,
        .region = .{ .start = start, .end = start },
        .expr = inner,
    } });
    return patt_idx;
}

/// todo
pub fn parseTypeHeader(self: *Parser) IR.NodeStore.TypeHeaderIdx {
    const start = self.pos;
    std.debug.assert(self.peek() == .UpperIdent);
    self.advance(); // Advance past UpperIdent
    if (self.peek() != .LowerIdent) {
        return self.store.addTypeHeader(.{
            .name = start,
            .args = .{ .span = .{
                .start = 0,
                .len = 0,
            } },
            .region = .{ .start = start, .end = start },
        });
    }
    const scratch_top = self.store.scratchTokenTop();
    while (self.peek() == .LowerIdent) {
        self.store.addScratchToken(self.pos);
        self.advance(); // Advance past LowerIdent
    }
    const args = self.store.tokenSpanFrom(scratch_top);
    return self.store.addTypeHeader(.{
        .name = start,
        .args = args,
        .region = .{ .start = start, .end = self.pos },
    });
}

const TyFnArgs = enum {
    not_looking_for_args,
    looking_for_args,
};

/// todo
pub fn parseTypeAnno(self: *Parser, looking_for_args: TyFnArgs) IR.NodeStore.TypeAnnoIdx {
    const start = self.pos;
    var anno: ?IR.NodeStore.TypeAnnoIdx = null;

    switch (self.peek()) {
        .UpperIdent => {
            if (self.peekNext() != .NoSpaceOpenRound) {
                anno = self.store.addTypeAnno(.{ .tag = .{
                    .tok = self.pos,
                    .args = .{ .span = .{
                        .start = 0,
                        .len = 0,
                    } },
                    .region = .{ .start = start, .end = self.pos },
                } });
                self.advance(); // Advance past UpperIdent
            } else {
                self.advance(); // Advance past UpperIdent
                self.advance(); // Advance past NoSpaceOpenRound
                const scratch_top = self.store.scratchTypeAnnoTop();
                self.parseCollectionSpan(IR.NodeStore.TypeAnnoIdx, .CloseRound, IR.NodeStore.addScratchTypeAnno, parseTypeAnnoInCollection) catch {
                    self.store.clearScratchTypeAnnosFrom(scratch_top);
                    return self.pushMalformed(IR.NodeStore.TypeAnnoIdx, .unexpected_token);
                };
                const args = self.store.typeAnnoSpanFrom(scratch_top);
                anno = self.store.addTypeAnno(.{ .tag = .{
                    .region = .{ .start = start, .end = self.pos },
                    .tok = start,
                    .args = args,
                } });
            }
        },
        .LowerIdent => {
            anno = self.store.addTypeAnno(.{ .ty_var = .{
                .tok = self.pos,
                .region = .{ .start = start, .end = self.pos },
            } });
            self.advance(); // Advance past LowerIdent
        },
        .OpenRound => {
            // Probably a tuple
            self.advance(); // Advance past OpenRound
            const after_round = self.pos;
            const scratch_top = self.store.scratchTypeAnnoTop();
            while (self.peek() != .CloseRound and self.peek() != .OpArrow and self.peek() != .OpFatArrow and self.peek() != .EndOfFile) {
                // Looking for args here so that we don't capture an un-parenthesized fn's args
                self.store.addScratchTypeAnno(self.parseTypeAnno(.looking_for_args));
                if (self.peek() != .Comma) {
                    break;
                }
                self.advance(); // Advance past Comma
            }
            if (self.peek() == .OpArrow or self.peek() == .OpFatArrow) {
                // use the scratch for the args for the func, advance, get the ret and set this to be a fn
                // since it's a function, as long as we find the CloseRound we can just return here.
                const args = self.store.typeAnnoSpanFrom(scratch_top);
                self.advance();
                const ret = self.parseTypeAnno(.not_looking_for_args);
                if (self.peek() != .CloseRound) {
                    self.store.clearScratchTypeAnnosFrom(scratch_top);
                    return self.pushMalformed(IR.NodeStore.TypeAnnoIdx, .unexpected_token);
                }
                const function = self.store.addTypeAnno(.{ .@"fn" = .{
                    .args = args,
                    .ret = ret,
                    .region = .{ .start = after_round, .end = self.pos },
                } });
                self.advance();
                return self.store.addTypeAnno(.{ .parens = .{
                    .anno = function,
                    .region = .{ .start = start, .end = self.pos },
                } });
            }
            if (self.peek() != .CloseRound) {
                self.store.clearScratchTypeAnnosFrom(scratch_top);
                return self.pushMalformed(IR.NodeStore.TypeAnnoIdx, .unexpected_token);
            }
            self.advance(); // Advance past CloseRound
            const annos = self.store.typeAnnoSpanFrom(scratch_top);
            anno = self.store.addTypeAnno(.{ .tuple = .{
                .region = .{ .start = start, .end = self.pos },
                .annos = annos,
            } });
        },
        .OpenCurly => {
            self.advance(); // Advance past OpenCurly
            const scratch_top = self.store.scratchAnnoRecordFieldTop();
            self.parseCollectionSpan(IR.NodeStore.AnnoRecordFieldIdx, .CloseCurly, IR.NodeStore.addScratchAnnoRecordField, parseAnnoRecordField) catch {
                self.store.clearScratchAnnoRecordFieldsFrom(scratch_top);
                return self.pushMalformed(IR.NodeStore.TypeAnnoIdx, .unexpected_token);
            };
            const fields = self.store.annoRecordFieldSpanFrom(scratch_top);
            anno = self.store.addTypeAnno(.{ .record = .{
                .region = .{ .start = start, .end = self.pos },
                .fields = fields,
            } });
        },
        .OpenSquare => {
            self.advance(); // Advance past OpenSquare
            const scratch_top = self.store.scratchTypeAnnoTop();
            self.parseCollectionSpan(IR.NodeStore.TypeAnnoIdx, .CloseSquare, IR.NodeStore.addScratchTypeAnno, parseTypeAnnoInCollection) catch {
                self.store.clearScratchTypeAnnosFrom(scratch_top);
                return self.pushMalformed(IR.NodeStore.TypeAnnoIdx, .unexpected_token);
            };
            const tags = self.store.typeAnnoSpanFrom(scratch_top);
            anno = self.store.addTypeAnno(.{ .tag_union = .{
                .region = .{ .start = start, .end = self.pos },
                .open_anno = null,
                .tags = tags,
            } });
        },
        .Underscore => {
            anno = self.store.addTypeAnno(.{ .underscore = .{
                .region = .{ .start = start, .end = self.pos },
            } });
            self.advance(); // Advance past Underscore
        },
        else => {
            // std.debug.panic("Could not parse type annotation, got {s}@{d}", .{ @tagName(self.peek()), self.pos });
            const reason: IR.Diagnostic.Tag = .ty_anno_unexpected_token;
            self.pushDiagnostic(reason, .{
                .start = self.pos,
                .end = self.pos,
            });
            return self.store.addTypeAnno(.{ .malformed = .{ .reason = reason } });
        },
    }

    if (anno) |an| {
        if ((looking_for_args == .not_looking_for_args) and (self.peek() == .Comma or self.peek() == .OpArrow or self.peek() == .OpFatArrow)) {
            const scratch_top = self.store.scratchTypeAnnoTop();
            self.store.addScratchTypeAnno(an);
            while (self.peek() == .Comma) {
                self.advance(); // Advance past Comma
                self.store.addScratchTypeAnno(self.parseTypeAnno(.looking_for_args));
            }
            const args = self.store.typeAnnoSpanFrom(scratch_top);
            if (self.peek() != .OpArrow and self.peek() != .OpFatArrow) {
                return self.pushMalformed(IR.NodeStore.TypeAnnoIdx, .unexpected_token);
            }
            self.advance(); // Advance past arrow
            // TODO: Handle thin vs fat arrow
            const ret = self.parseTypeAnno(.not_looking_for_args);
            return self.store.addTypeAnno(.{ .@"fn" = .{
                .region = .{ .start = start, .end = self.pos },
                .args = args,
                .ret = ret,
            } });
        }
        return an;
    }

    std.debug.panic("Never handled type annotation", .{});
}

/// todo
pub fn parseTypeAnnoInCollection(self: *Parser) IR.NodeStore.TypeAnnoIdx {
    return self.parseTypeAnno(.looking_for_args);
}

/// todo
pub fn parseAnnoRecordField(self: *Parser) IR.NodeStore.AnnoRecordFieldIdx {
    const field_start = self.pos;
    if (self.peek() != .LowerIdent) {
        while (self.peek() != .CloseCurly and self.peek() != .Comma and self.peek() != .EndOfFile) {
            self.advance(); // Advance until we end this field or the record
        }
        return self.pushMalformed(IR.NodeStore.AnnoRecordFieldIdx, .unexpected_token);
    }
    const name = self.pos;
    self.advance(); // Advance past LowerIdent
    if (self.peek() != .OpColon) {
        while (self.peek() != .CloseCurly and self.peek() != .Comma and self.peek() != .EndOfFile) {
            self.advance(); // Advance until we end this field or the record
        }
        return self.pushMalformed(IR.NodeStore.AnnoRecordFieldIdx, .unexpected_token);
    }
    self.advance(); // Advance past OpColon
    const ty = self.parseTypeAnno(.looking_for_args);

    return self.store.addAnnoRecordField(.{
        .region = .{ .start = field_start, .end = self.pos },
        .name = name,
        .ty = ty,
    });
}

/// todo
pub fn addProblem(self: *Parser, diagnostic: IR.Diagnostic) void {
    self.diagnostics.append(diagnostic) catch |err| exitOnOom(err);
}

/// Binding power of the lhs and rhs of a particular operator.
const BinOpBp = struct { left: u8, right: u8 };

/// Get the binding power for a Token if it's a operator token, else return null.
fn getTokenBP(tok: Token.Tag) ?BinOpBp {
    return switch (tok) {
        .OpStar => .{ .left = 31, .right = 30 }, // 31 LEFT
        .OpSlash => .{ .left = 29, .right = 28 }, // 29 LEFT
        .OpDoubleSlash => .{ .left = 27, .right = 26 }, // 27 LEFT
        .OpPercent => .{ .left = 25, .right = 24 }, // 25 LEFT
        .OpPlus => .{ .left = 23, .right = 22 }, // 23 LEFT
        .OpBinaryMinus => .{ .left = 21, .right = 20 }, // 21 LEFT
        .OpDoubleQuestion => .{ .left = 19, .right = 18 }, // 19 LEFT
        .OpQuestion => .{ .left = 17, .right = 16 }, // 17 LEFT
        .OpEquals => .{ .left = 15, .right = 15 }, // 15 NOASSOC
        .OpNotEquals => .{ .left = 13, .right = 13 }, // 13 NOASSOC
        .OpLessThan => .{ .left = 11, .right = 11 }, // 11 NOASSOC
        .OpGreaterThan => .{ .left = 9, .right = 9 }, // 9 NOASSOC
        .OpLessThanOrEq => .{ .left = 7, .right = 7 }, // 7 NOASSOC
        .OpGreaterThanOrEq => .{ .left = 5, .right = 5 }, // 5 NOASSOC
        .OpAnd => .{ .left = 3, .right = 4 }, // 3 RIGHT
        .OpOr => .{ .left = 1, .right = 2 }, // 1 RIGHT
        else => null,
    };
}
