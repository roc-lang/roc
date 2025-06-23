const std = @import("std");
const tracy = @import("../../tracy.zig");
const tokenize = @import("tokenize.zig");
const collections = @import("../../collections.zig");
const base = @import("../../base.zig");

const AST = @import("AST.zig");
const Node = @import("Node.zig");
const NodeStore = @import("NodeStore.zig");
const NodeList = AST.NodeList;
const TokenizedBuffer = tokenize.TokenizedBuffer;
const Token = tokenize.Token;
const TokenIdx = Token.Idx;

const exitOnOom = collections.utils.exitOnOom;

/// A parser which tokenizes and parses source code into an abstract syntax tree.
pub const Parser = @This();

gpa: std.mem.Allocator,
pos: TokenIdx,
tok_buf: TokenizedBuffer,
store: NodeStore,
scratch_nodes: std.ArrayListUnmanaged(Node.Idx),
diagnostics: std.ArrayListUnmanaged(AST.Diagnostic),

/// init the parser from a buffer of tokens
pub fn init(tokens: TokenizedBuffer) Parser {
    const estimated_node_count = tokens.tokens.len;
    const store = NodeStore.initCapacity(tokens.env.gpa, estimated_node_count);

    return Parser{
        .gpa = tokens.env.gpa,
        .pos = 0,
        .tok_buf = tokens,
        .store = store,
        .scratch_nodes = .{},
        .diagnostics = .{},
    };
}

/// Deinit the parser.  The buffer of tokens and the store are still owned by the caller.
pub fn deinit(parser: *Parser) void {
    parser.scratch_nodes.deinit(parser.gpa);

    // diagnostics will be kept and passed to the following compiler stage
    // to be deinitialized by the caller when no longer required
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
    while (true and self.peek() != .EndOfFile) {
        self.pos += 1;
        if (self.peek() != .Newline) {
            break;
        }
    }
    // We have an EndOfFile token that we never expect to advance past
    std.debug.assert(self.pos < self.tok_buf.tokens.len);
}

/// Helper to advance the parser exactly one token, whether it is significant or not.
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

/// Peek at the token at the current position
///
/// **note** caller is responsible to ensure this isn't the last token
pub fn peek(self: *Parser) Token.Tag {
    std.debug.assert(self.pos < self.tok_buf.tokens.len);
    return self.tok_buf.tokens.items(.tag)[self.pos];
}

/// Peek at the next significant token
pub fn peekNext(self: *Parser) Token.Tag {
    var next = self.pos + 1;
    const tags = self.tok_buf.tokens.items(.tag);
    while (next < self.tok_buf.tokens.len and tags[next] == .Newline) {
        next += 1;
    }
    if (next >= self.tok_buf.tokens.len) {
        return .EndOfFile;
    }
    return tags[next];
}

/// Peek at `n` significant tokens forward
pub fn peekN(self: *Parser, n: u32) Token.Tag {
    if (n == 0) {
        return self.peek();
    }
    var left = n;
    var next = self.pos;
    const tags = self.tok_buf.tokens.items(.tag);
    while (left > 0) {
        next += 1;
        while (next < self.tok_buf.tokens.len and tags[next] == .Newline) {
            next += 1;
        }
        if (next >= self.tok_buf.tokens.len) {
            return .EndOfFile;
        }
        left -= 1;
    }
    return tags[next];
}

/// add a diagnostic error
pub fn pushDiagnostic(self: *Parser, tag: AST.Diagnostic.Tag, region: AST.TokenizedRegion) void {
    self.diagnostics.append(self.gpa, .{
        .tag = tag,
        .region = region,
    }) catch |err| exitOnOom(err);
}
/// add a malformed token
pub fn pushMalformed(self: *Parser, comptime t: type, tag: AST.Diagnostic.Tag, start: TokenIdx) t {
    const pos = self.pos;
    if (self.peek() != .EndOfFile) {
        self.advanceOne(); // TODO: find a better point to advance to
    }

    // Create a diagnostic region that points to the problematic token
    // If the parser has moved too far from the start, use the start token for better error location
    const diagnostic_start = if (self.pos > start and (self.pos - start) > 2) start else @min(pos, self.pos);
    const diagnostic_end = if (self.pos > start and (self.pos - start) > 2) start + 1 else @max(pos, self.pos);
    // If start equals end, make it a single-token region
    const diagnostic_region = AST.TokenizedRegion{ .start = diagnostic_start, .end = if (diagnostic_start == diagnostic_end) diagnostic_start + 1 else diagnostic_end };

    // AST node should span the entire malformed expression
    const ast_region = AST.TokenizedRegion{ .start = start, .end = self.pos };

    self.diagnostics.append(self.gpa, .{
        .tag = tag,
        .region = diagnostic_region,
    }) catch |err| exitOnOom(err);
    return self.store.addMalformed(t, tag, ast_region);
}
/// parse a `.roc` module
///
/// the tokens are provided at Parser initialisation
pub fn parseFile(self: *Parser) void {
    const trace = tracy.trace(@src());
    defer trace.end();

    self.store.emptyScratch();
    _ = self.store.addFile(.{
        .header = @as(AST.Header.Idx, @enumFromInt(0)),
        .statements = AST.Statement.Span{ .span = base.DataSpan.empty() },
        .region = AST.TokenizedRegion.empty(),
    });

    while (self.peek() == .Newline) {
        self.advanceOne();
    }

    const header = self.parseHeader();
    const scratch_top = self.store.scratchStatementTop();

    while (self.peek() != .EndOfFile) {
        const current_scratch_top = self.store.scratchStatementTop();
        if (self.parseTopLevelStatement()) |idx| {
            std.debug.assert(self.store.scratchStatementTop() == current_scratch_top);
            self.store.addScratchStatement(idx);
        } else {
            std.debug.assert(self.store.scratchStatementTop() == current_scratch_top);
            break;
        }
    }

    _ = self.store.addFile(.{
        .header = header,
        .statements = self.store.statementSpanFrom(scratch_top),
        .region = .{ .start = 0, .end = @intCast(self.tok_buf.tokens.len - 1) },
    });
}

/// Parses the items of type T until we encounter end_token, with each item separated by a Comma token
///
/// Returns the ending position of the collection
fn parseCollectionSpan(self: *Parser, comptime T: type, end_token: Token.Tag, scratch_fn: fn (*NodeStore, T) void, parser: fn (*Parser) T) ExpectError!u32 {
    while (self.peek() != end_token and self.peek() != .EndOfFile) {
        scratch_fn(&self.store, parser(self));
        self.expect(.Comma) catch {
            break;
        };
    }
    const collection_end = self.pos;
    self.expect(end_token) catch {
        return ExpectError.expected_not_found;
    };
    return collection_end;
}

/// Utility to see where a the current position is within the buffer of tokens
fn debugToken(self: *Parser, window: usize) void {
    const current = self.pos;
    const start = if (window > current) 0 else current - window;
    const end = std.math.clamp(current + window, current, self.tok_buf.tokens.len);
    const tags = self.tok_buf.tokens.items(.tag);
    const tok_extra = self.tok_buf.tokens.items(.extra);
    for (start..end) |i| {
        const tag = tags[i];
        var extra: []u8 = "";
        if (tag == .LowerIdent or tag == .UpperIdent) {
            const e = tok_extra[i];
            extra = self.tok_buf.env.idents.getText(e.interned);
        }
        std.debug.print("{s}{d}: {s} \"{s}\"\n", .{ if (i == current) "-->" else "   ", i, @tagName(tag), extra });
    }
}

/// Parses a module header using the following grammar:
///
/// provides_entry :: [LowerIdent|UpperIdent] Comma Newline*
/// package_entry :: LowerIdent Comma "platform"? String Comma
/// app_header :: KwApp Newline* OpenSquare provides_entry* CloseSquare OpenCurly package_entry CloseCurly
pub fn parseHeader(self: *Parser) AST.Header.Idx {
    switch (self.peek()) {
        .KwApp => {
            return self.parseAppHeader();
        },
        .KwModule => {
            return self.parseModuleHeader();
        },
        // .KwPackage => {},
        .KwHosted => {
            return self.parseHostedHeader();
        },
        .KwPackage => {
            return self.parsePackageHeader();
        },
        .KwPlatform => {
            return self.parsePlatformHeader();
        },
        else => {
            return self.pushMalformed(AST.Header.Idx, .missing_header, self.pos);
        },
    }
}

/// parse a `.roc` platform header
///
/// e.g:
/// ```roc
/// platform
///     requires {} { main! : List(Str) => {} }
///     exposes []
///     packages { foo: "../foo.roc" }
///     imports []
///     provides [main_for_host]
pub fn parsePlatformHeader(self: *Parser) AST.Header.Idx {
    const start = self.pos;
    std.debug.assert(self.peek() == .KwPlatform);
    self.advance(); // Advance past KwPlatform

    // Get name
    self.expect(.StringStart) catch {
        return self.pushMalformed(
            AST.Header.Idx,
            .expected_platform_name_start,
            self.pos,
        );
    };
    const name = self.pos;
    self.expect(.StringPart) catch {
        return self.pushMalformed(
            AST.Header.Idx,
            .expected_platform_name_string,
            self.pos,
        );
    };
    self.expect(.StringEnd) catch {
        return self.pushMalformed(
            AST.Header.Idx,
            .expected_platform_name_end,
            self.pos,
        );
    };

    self.expect(.KwRequires) catch {
        return self.pushMalformed(
            AST.Header.Idx,
            .expected_requires,
            self.pos,
        );
    };
    // Get requires rigids
    const rigids_start = self.pos;
    self.expect(.OpenCurly) catch {
        return self.pushMalformed(
            AST.Header.Idx,
            .expected_requires_rigids_open_curly,
            self.pos,
        );
    };
    const rigids_top = self.store.scratchExposedItemTop();
    const rigids_end = self.parseCollectionSpan(
        AST.ExposedItem.Idx,
        .CloseCurly,
        NodeStore.addScratchExposedItem,
        Parser.parseExposedItem,
    ) catch {
        self.store.clearScratchExposedItemsFrom(rigids_start);
        return self.pushMalformed(
            AST.Header.Idx,
            .expected_requires_rigids_close_curly,
            rigids_start,
        );
    };
    const rigids_span = self.store.exposedItemSpanFrom(rigids_top);
    const rigids = self.store.addCollection(
        .collection_exposed,
        .{
            .span = rigids_span.span,
            .region = .{
                .start = rigids_start,
                .end = rigids_end,
            },
        },
    );

    // Get requires signatures
    const signatures_start = self.pos;
    self.expect(.OpenCurly) catch {
        return self.pushMalformed(
            AST.Header.Idx,
            .expected_requires_signatures_open_curly,
            self.pos,
        );
    };
    const signatures_top = self.store.scratchAnnoRecordFieldTop();
    const signatures_end = self.parseCollectionSpan(
        AST.AnnoRecordField.Idx,
        .CloseCurly,
        NodeStore.addScratchAnnoRecordField,
        Parser.parseAnnoRecordField,
    ) catch {
        return self.pushMalformed(
            AST.Header.Idx,
            .expected_requires_signatures_close_curly,
            rigids_start,
        );
    };
    const signatures_span = self.store.annoRecordFieldSpanFrom(signatures_top);
    const signatures = self.store.addTypeAnno(.{ .record = .{
        .fields = signatures_span,
        .region = .{
            .start = signatures_start,
            .end = signatures_end,
        },
    } });

    // Get exposes
    self.expect(.KwExposes) catch {
        return self.pushMalformed(
            AST.Header.Idx,
            .expected_exposes,
            self.pos,
        );
    };
    const exposes_start = self.pos;
    self.expect(.OpenSquare) catch {
        return self.pushMalformed(
            AST.Header.Idx,
            .expected_exposes_open_square,
            self.pos,
        );
    };
    const exposes_top = self.store.scratchExposedItemTop();
    const exposes_end = self.parseCollectionSpan(
        AST.ExposedItem.Idx,
        .CloseSquare,
        NodeStore.addScratchExposedItem,
        Parser.parseExposedItem,
    ) catch {
        self.store.clearScratchExposedItemsFrom(exposes_start);
        return self.pushMalformed(
            AST.Header.Idx,
            .expected_exposes_close_square,
            exposes_start,
        );
    };
    const exposes_span = self.store.exposedItemSpanFrom(exposes_top);
    const exposes = self.store.addCollection(
        .collection_exposed,
        .{
            .span = exposes_span.span,
            .region = .{ .start = exposes_start, .end = exposes_end },
        },
    );

    // Get packages
    self.expect(.KwPackages) catch {
        return self.pushMalformed(
            AST.Header.Idx,
            .expected_imports,
            self.pos,
        );
    };
    const packages_start = self.pos;
    const packages_top = self.store.scratchRecordFieldTop();
    self.expect(.OpenCurly) catch {
        return self.pushMalformed(
            AST.Header.Idx,
            .expected_packages_open_curly,
            self.pos,
        );
    };
    const packages_end = self.parseCollectionSpan(
        AST.RecordField.Idx,
        .CloseCurly,
        NodeStore.addScratchRecordField,
        Parser.parseRecordField,
    ) catch {
        self.store.clearScratchRecordFieldsFrom(packages_top);
        return self.pushMalformed(
            AST.Header.Idx,
            .expected_packages_close_curly,
            self.pos,
        );
    };
    const packages_span = self.store.recordFieldSpanFrom(packages_top);
    const packages = self.store.addCollection(
        .collection_packages,
        .{
            .span = packages_span.span,
            .region = .{ .start = packages_start, .end = packages_end },
        },
    );

    // Get provides
    self.expect(.KwProvides) catch {
        return self.pushMalformed(
            AST.Header.Idx,
            .expected_provides,
            self.pos,
        );
    };
    const provides_start = self.pos;
    self.expect(.OpenSquare) catch {
        return self.pushMalformed(
            AST.Header.Idx,
            .expected_provides_open_square,
            self.pos,
        );
    };
    const provides_top = self.store.scratchExposedItemTop();
    const end = self.parseCollectionSpan(
        AST.ExposedItem.Idx,
        .CloseSquare,
        NodeStore.addScratchExposedItem,
        Parser.parseExposedItem,
    ) catch {
        self.store.clearScratchExposedItemsFrom(provides_start);
        return self.pushMalformed(
            AST.Header.Idx,
            .expected_provides_close_square,
            provides_start,
        );
    };
    const provides_span = self.store.exposedItemSpanFrom(provides_top);
    const provides = self.store.addCollection(
        .collection_exposed,
        .{
            .span = provides_span.span,
            .region = .{ .start = provides_start, .end = end },
        },
    );

    return self.store.addHeader(.{ .platform = .{
        .name = name,
        .requires_rigids = rigids,
        .requires_signatures = signatures,
        .exposes = exposes,
        .packages = packages,
        .provides = provides,
        .region = .{ .start = start, .end = end },
    } });
}

/// parse an `.roc` package header
///
/// e.g. `package [ foo ] { something: "package/path/main.roc" }`
pub fn parsePackageHeader(self: *Parser) AST.Header.Idx {
    const start = self.pos;

    std.debug.assert(self.peek() == .KwPackage);
    self.advance(); // Advance past KwApp

    // Get Exposes
    const exposes_start = self.pos;
    self.expect(.OpenSquare) catch {
        return self.pushMalformed(AST.Header.Idx, .expected_provides_open_square, start);
    };
    const scratch_top = self.store.scratchExposedItemTop();
    const exposes_end = self.parseCollectionSpan(AST.ExposedItem.Idx, .CloseSquare, NodeStore.addScratchExposedItem, Parser.parseExposedItem) catch {
        while (self.peek() != .CloseSquare and self.peek() != .EndOfFile) {
            self.advance();
        }
        self.expect(.CloseSquare) catch {
            return self.pushMalformed(AST.Header.Idx, .header_expected_close_square, start);
        };
        self.store.clearScratchExposedItemsFrom(scratch_top);
        return self.pushMalformed(AST.Header.Idx, .import_exposing_no_close, start);
    };
    const exposes_span = self.store.exposedItemSpanFrom(scratch_top);
    const exposes = self.store.addCollection(.collection_exposed, .{
        .span = exposes_span.span,
        .region = .{
            .start = exposes_start,
            .end = exposes_end,
        },
    });

    // Get Packages
    const packages_start = self.pos;
    self.expect(.OpenCurly) catch {
        return self.pushMalformed(AST.Header.Idx, .expected_package_platform_open_curly, start);
    };
    const fields_scratch_top = self.store.scratchRecordFieldTop();
    const end = self.parseCollectionSpan(AST.RecordField.Idx, .CloseCurly, NodeStore.addScratchRecordField, Parser.parseRecordField) catch {
        self.store.clearScratchRecordFieldsFrom(fields_scratch_top);
        return self.pushMalformed(AST.Header.Idx, .expected_package_platform_close_curly, start);
    };
    const packages_span = self.store.recordFieldSpanFrom(fields_scratch_top);
    const packages = self.store.addCollection(.collection_packages, .{
        .span = packages_span.span,
        .region = .{
            .start = packages_start,
            .end = end,
        },
    });

    self.advance();

    const header = AST.Header{ .package = .{
        .exposes = exposes,
        .packages = packages,
        .region = .{ .start = start, .end = end },
    } };
    const idx = self.store.addHeader(header);
    return idx;
}

/// Parse a Roc Hosted header
///
/// e.g. `hosted [foo]`
fn parseHostedHeader(self: *Parser) AST.Header.Idx {
    std.debug.assert(self.peek() == .KwHosted);

    const start = self.pos;

    self.advance(); // Advance past KwModule

    // Get exposes
    const exposes_start = self.pos;
    self.expect(.OpenSquare) catch {
        return self.pushMalformed(AST.Header.Idx, .header_expected_open_square, self.pos);
    };
    const scratch_top = self.store.scratchExposedItemTop();
    const end = self.parseCollectionSpan(AST.ExposedItem.Idx, .CloseSquare, NodeStore.addScratchExposedItem, Parser.parseExposedItem) catch {
        while (self.peek() != .CloseSquare and self.peek() != .EndOfFile) {
            self.advance();
        }
        self.expect(.CloseSquare) catch {
            return self.pushMalformed(AST.Header.Idx, .header_expected_close_square, self.pos);
        };
        self.store.clearScratchExposedItemsFrom(scratch_top);
        return self.pushMalformed(AST.Header.Idx, .import_exposing_no_close, self.pos);
    };
    const exposes_span = self.store.exposedItemSpanFrom(scratch_top);
    const exposes = self.store.addCollection(.collection_exposed, .{
        .span = exposes_span.span,
        .region = .{
            .start = exposes_start,
            .end = end,
        },
    });

    return self.store.addHeader(.{ .hosted = .{
        .region = .{ .start = start, .end = end },
        .exposes = exposes,
    } });
}

/// parse a Roc module header
///
/// e.g. `module [foo]`
fn parseModuleHeader(self: *Parser) AST.Header.Idx {
    std.debug.assert(self.peek() == .KwModule);

    const start = self.pos;

    self.advance(); // Advance past KwModule

    // Get exposes
    const exposes_start = self.pos;
    self.expect(.OpenSquare) catch {
        return self.pushMalformed(AST.Header.Idx, .header_expected_open_square, self.pos);
    };
    const scratch_top = self.store.scratchExposedItemTop();
    const end = self.parseCollectionSpan(AST.ExposedItem.Idx, .CloseSquare, NodeStore.addScratchExposedItem, Parser.parseExposedItem) catch {
        while (self.peek() != .CloseSquare and self.peek() != .EndOfFile) {
            self.advance();
        }
        self.expect(.CloseSquare) catch {
            return self.pushMalformed(AST.Header.Idx, .header_expected_close_square, self.pos);
        };
        self.store.clearScratchExposedItemsFrom(scratch_top);
        return self.pushMalformed(AST.Header.Idx, .import_exposing_no_close, self.pos);
    };
    const exposes_span = self.store.exposedItemSpanFrom(scratch_top);
    const exposes = self.store.addCollection(.collection_exposed, .{
        .span = exposes_span.span,
        .region = .{
            .start = exposes_start,
            .end = end,
        },
    });

    return self.store.addHeader(.{ .module = .{
        .region = .{ .start = start, .end = end },
        .exposes = exposes,
    } });
}

/// parse an `.roc` application header
///
/// e.g. `app [main!] { pf: "../some-platform.roc" }`
pub fn parseAppHeader(self: *Parser) AST.Header.Idx {
    var platform: ?AST.RecordField.Idx = null;
    const start = self.pos;

    std.debug.assert(self.peek() == .KwApp);
    self.advance(); // Advance past KwApp

    // Get provides
    self.expect(.OpenSquare) catch {
        return self.pushMalformed(AST.Header.Idx, .expected_provides_open_square, start);
    };
    const provides_start = self.pos;
    const scratch_top = self.store.scratchExposedItemTop();
    const provides_end = self.parseCollectionSpan(AST.ExposedItem.Idx, .CloseSquare, NodeStore.addScratchExposedItem, Parser.parseExposedItem) catch {
        while (self.peek() != .CloseSquare and self.peek() != .EndOfFile) {
            self.advance();
        }
        self.expect(.CloseSquare) catch {
            return self.pushMalformed(AST.Header.Idx, .header_expected_close_square, start);
        };
        self.store.clearScratchExposedItemsFrom(scratch_top);
        return self.pushMalformed(AST.Header.Idx, .import_exposing_no_close, start);
    };
    const provides_span = self.store.exposedItemSpanFrom(scratch_top);
    const provides_region = AST.TokenizedRegion{ .start = provides_start, .end = provides_end };
    const provides = self.store.addCollection(.collection_exposed, AST.Collection{
        .span = provides_span.span,
        .region = provides_region,
    });

    // Get platform and packages
    const fields_scratch_top = self.store.scratchRecordFieldTop();
    const packages_start = self.pos;
    self.expect(.OpenCurly) catch {
        return self.pushMalformed(AST.Header.Idx, .expected_package_platform_open_curly, start);
    };
    var i: usize = 0;

    while (self.peek() != .CloseCurly and self.peek() != .EndOfFile) {
        const entry_start = self.pos;
        if (self.peek() != .LowerIdent) {
            self.store.clearScratchRecordFieldsFrom(fields_scratch_top);
            return self.pushMalformed(AST.Header.Idx, .expected_package_or_platform_name, start);
        }
        const name_tok = self.pos;
        self.advance();
        if (self.peek() != .OpColon) {
            self.store.clearScratchRecordFieldsFrom(fields_scratch_top);
            return self.pushMalformed(AST.Header.Idx, .expected_package_or_platform_colon, start);
        }
        self.advance();
        if (self.peek() == .KwPlatform) {
            if (platform != null) {
                self.store.clearScratchRecordFieldsFrom(fields_scratch_top);
                return self.pushMalformed(AST.Header.Idx, .multiple_platforms, start);
            }
            self.advance();
            if (self.peek() != .StringStart) {
                self.store.clearScratchRecordFieldsFrom(fields_scratch_top);
                return self.pushMalformed(AST.Header.Idx, .expected_platform_string, start);
            }
            const value = self.parseStringExpr();
            const pidx = self.store.addRecordField(.{
                .name = name_tok,
                .value = value,
                .optional = false,
                .region = .{ .start = entry_start, .end = self.pos },
            });
            self.store.addScratchRecordField(pidx);
            platform = pidx;
        } else {
            if (self.peek() != .StringStart) {
                self.store.clearScratchRecordFieldsFrom(fields_scratch_top);
                return self.pushMalformed(AST.Header.Idx, .expected_package_or_platform_string, start);
            }
            const value = self.parseStringExpr();
            self.store.addScratchRecordField(self.store.addRecordField(.{
                .name = name_tok,
                .value = value,
                .optional = false,
                .region = .{ .start = entry_start, .end = self.pos },
            }));
        }
        while (self.peek() == .Newline) {
            self.advanceOne();
        }
        self.expect(.Comma) catch {
            break;
        };
        i = i + 1;
    }
    const packages_end = self.pos;
    if (self.peek() != .CloseCurly) {
        self.store.clearScratchRecordFieldsFrom(fields_scratch_top);
        std.debug.print("Tokens: {any}\n", .{self.tok_buf.tokens.items(.tag)[start..self.pos]});
        return self.pushMalformed(AST.Header.Idx, .expected_package_platform_close_curly, start);
    }
    self.advanceOne(); // Advance past CloseCurly
    const packages_span = self.store.recordFieldSpanFrom(fields_scratch_top);
    const packages = self.store.addCollection(.collection_packages, .{
        .span = packages_span.span,
        .region = .{
            .start = packages_start,
            .end = packages_end,
        },
    });
    self.advance();

    if (platform) |pidx| {
        const header = AST.Header{
            .app = .{
                .platform_idx = pidx,
                .provides = provides,
                .packages = packages,
                .region = .{ .start = start, .end = packages_end },
            },
        };
        const idx = self.store.addHeader(header);
        return idx;
    }
    return self.pushMalformed(AST.Header.Idx, .no_platform, start);
}

/// Parses an ExposedItem, adding it to the NodeStore and returning the Idx
pub fn parseExposedItem(self: *Parser) AST.ExposedItem.Idx {
    const start = self.pos;
    var end = start;
    switch (self.peek()) {
        .LowerIdent => {
            var as: ?TokenIdx = null;
            if (self.peekNext() == .KwAs) {
                self.advance(); // Advance past LowerIdent
                self.advance(); // Advance past KwAs
                as = self.pos;
                self.expect(.LowerIdent) catch {
                    return self.pushMalformed(AST.ExposedItem.Idx, .expected_lower_name_after_exposed_item_as, start);
                };
                end = self.pos;
            } else {
                self.advance(); // Advance past LowerIdent
            }
            const ei = self.store.addExposedItem(.{ .lower_ident = .{
                .region = .{ .start = start, .end = end },
                .ident = start,
                .as = as,
            } });

            return ei;
        },
        .UpperIdent => {
            var as: ?TokenIdx = null;
            if (self.peekNext() == .KwAs) {
                self.advance(); // Advance past UpperIdent
                self.advance(); // Advance past KwAs
                as = self.pos;
                self.expect(.UpperIdent) catch {
                    return self.pushMalformed(AST.ExposedItem.Idx, .expected_upper_name_after_exposed_item_as, start);
                };
                end = self.pos;
            } else if (self.peekNext() == .DotStar) {
                self.advance(); // Advance past UpperIdent
                self.advance(); // Advance past DotStar
                return self.store.addExposedItem(.{ .upper_ident_star = .{
                    .region = .{ .start = start, .end = self.pos },
                    .ident = start,
                } });
            } else {
                self.advance(); // Advance past UpperIdent
            }
            const ei = self.store.addExposedItem(.{ .upper_ident = .{
                .region = .{ .start = start, .end = end },
                .ident = start,
                .as = as,
            } });

            return ei;
        },
        else => {
            return self.pushMalformed(AST.ExposedItem.Idx, .exposed_item_unexpected_token, start);
        },
    }
}

const StatementType = enum { top_level, in_body };

/// Parse a top level roc statement
///
/// e.g. `import Foo`
pub fn parseTopLevelStatement(self: *Parser) ?AST.Statement.Idx {
    return self.parseStmtByType(.top_level);
}

/// parse a in-body roc statement
///
/// e.g. `foo = 2 + x`
pub fn parseStmt(self: *Parser) ?AST.Statement.Idx {
    return self.parseStmtByType(.in_body);
}

/// parse a roc statement
///
/// e.g. `import Foo`, or `foo = 2 + x`
fn parseStmtByType(self: *Parser, statementType: StatementType) ?AST.Statement.Idx {
    switch (self.peek()) {
        .KwImport => {
            if (statementType != .top_level) {
                return self.pushMalformed(AST.Statement.Idx, .import_must_be_top_level, self.pos);
            }
            const start = self.pos;
            self.advance(); // Advance past KwImport
            var qualifier: ?TokenIdx = null;
            var alias_tok: ?TokenIdx = null;
            if (self.peek() == .LowerIdent) {
                qualifier = self.pos;
                self.advance(); // Advance past LowerIdent
            }
            if (self.peek() == .UpperIdent or (qualifier != null and (self.peek() == .NoSpaceDotUpperIdent or self.peek() == .DotUpperIdent))) {
                var exposes = AST.ExposedItem.Span{ .span = base.DataSpan.empty() };
                const module_name_tok = self.pos;
                var end = self.pos;
                if (self.peekNext() == .KwAs) {
                    self.advance(); // Advance past UpperIdent
                    self.advance(); // Advance past KwAs
                    alias_tok = self.pos;
                    end = self.pos;
                    self.expect(.UpperIdent) catch {
                        const malformed = self.pushMalformed(AST.Statement.Idx, .expected_upper_name_after_import_as, start);
                        self.advance();
                        return malformed;
                    };
                } else if (self.peekNext() == .KwExposing) {
                    self.advance(); // Advance past ident
                    self.advance(); // Advance past KwExposing
                    self.expect(.OpenSquare) catch {
                        return self.pushMalformed(AST.Statement.Idx, .import_exposing_no_open, start);
                    };
                    const scratch_top = self.store.scratchExposedItemTop();
                    end = self.parseCollectionSpan(AST.ExposedItem.Idx, .CloseSquare, NodeStore.addScratchExposedItem, Parser.parseExposedItem) catch {
                        while (self.peek() != .CloseSquare and self.peek() != .EndOfFile) {
                            self.advance();
                        }
                        self.expect(.CloseSquare) catch {};
                        self.store.clearScratchExposedItemsFrom(scratch_top);
                        return self.pushMalformed(AST.Statement.Idx, .import_exposing_no_close, start);
                    };
                    exposes = self.store.exposedItemSpanFrom(scratch_top);
                } else {
                    self.advance(); // Advance past identifier
                }
                const statement_idx = self.store.addStatement(.{ .import = .{
                    .module_name_tok = module_name_tok,
                    .qualifier_tok = qualifier,
                    .alias_tok = alias_tok,
                    .exposes = exposes,
                    .region = .{ .start = start, .end = end },
                } });
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
        .KwFor => {
            const start = self.pos;
            self.advance();
            const patt = self.parsePattern(.alternatives_forbidden);
            self.expect(.KwIn) catch {
                return self.pushMalformed(AST.Statement.Idx, .for_expected_in, self.pos);
            };
            const expr = self.parseExpr();
            const body = self.parseExpr();
            const statement_idx = self.store.addStatement(.{ .@"for" = .{
                .region = .{ .start = start, .end = self.pos },
                .patt = patt,
                .expr = expr,
                .body = body,
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
        .KwVar => {
            const start = self.pos;
            if (statementType != .in_body) {
                return self.pushMalformed(AST.Statement.Idx, .var_only_allowed_in_a_body, self.pos);
            }
            self.advance();
            if (self.peek() != .LowerIdent) {
                return self.pushMalformed(AST.Statement.Idx, .var_must_have_ident, self.pos);
            }
            const name = self.pos;
            self.advance();
            self.expect(.OpAssign) catch {
                return self.pushMalformed(AST.Statement.Idx, .var_expected_equals, self.pos);
            };
            const body = self.parseExpr();
            const statement_idx = self.store.addStatement(.{ .@"var" = .{
                .name = name,
                .body = body,
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
                const expr_region = self.store.nodes.items.items(.region)[@intFromEnum(idx)];
                const patt_idx = self.store.addPattern(.{ .ident = .{
                    .ident_tok = start,
                    .region = .{ .start = start, .end = start },
                } });
                const statement_idx = self.store.addStatement(.{ .decl = .{
                    .pattern = patt_idx,
                    .body = idx,
                    .region = .{ .start = start, .end = expr_region.end },
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
                    .where = self.parseWhereConstraint(),
                    .region = .{ .start = start, .end = self.pos },
                } });
                return statement_idx;
            } else {
                // continue to parse final expression
            }
        },
        .NamedUnderscore => {
            const start = self.pos;
            if (self.peekNext() == .OpAssign) {
                self.advance(); // Advance past NamedUnderscore
                self.advance(); // Advance past OpAssign
                const idx = self.parseExpr();
                const expr_region = self.store.nodes.items.items(.region)[@intFromEnum(idx)];
                const patt_idx = self.store.addPattern(.{ .ident = .{
                    .ident_tok = start,
                    .region = .{ .start = start, .end = start },
                } });
                const statement_idx = self.store.addStatement(.{ .decl = .{
                    .pattern = patt_idx,
                    .body = idx,
                    .region = .{ .start = start, .end = expr_region.end },
                } });
                if (self.peek() == .Newline) {
                    self.advance();
                }
                return statement_idx;
            } else if (self.peekNext() == .OpColon) {
                self.advance(); // Advance past NamedUnderscore
                self.advance(); // Advance past OpColon
                const anno = self.parseTypeAnno(.not_looking_for_args);
                const statement_idx = self.store.addStatement(.{ .type_anno = .{
                    .anno = anno,
                    .name = start,
                    .where = self.parseWhereConstraint(),
                    .region = .{ .start = start, .end = self.pos },
                } });
                return statement_idx;
            } else {
                // continue to parse final expression
            }
        },
        // Expect to parse a Type Annotation, e.g. `Foo a : (a,a)`
        .UpperIdent => {
            const start = self.pos;
            if (statementType == .top_level) {
                const header = self.parseTypeHeader();
                if (self.peek() != .OpColon and self.peek() != .OpColonEqual) {
                    return self.pushMalformed(AST.Statement.Idx, .expected_colon_after_type_annotation, start);
                }
                const kind: AST.TypeDeclKind = if (self.peek() == .OpColonEqual) .nominal else .alias;
                self.advance();
                const anno = self.parseTypeAnno(.not_looking_for_args);
                const statement_idx = self.store.addStatement(.{ .type_decl = .{
                    .header = header,
                    .anno = anno,
                    .where = self.parseWhereConstraint(),
                    .kind = kind,
                    .region = .{ .start = start, .end = self.pos },
                } });
                return statement_idx;
            }
        },
        else => {},
    }

    // We didn't find any statements, so we must be parsing the final expression.
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
}

fn parseWhereConstraint(self: *Parser) ?AST.Collection.Idx {
    self.expect(.KwWhere) catch {
        return null;
    };
    const start = self.pos;
    const where_clauses_top = self.store.scratchWhereClauseTop();
    while (self.peek() != .Comma and self.peek() != .EndOfFile) {
        const curr = self.peek();
        const next = self.peekNext();
        const valid_lookahead = curr == .KwModule or (curr == .LowerIdent and (next == .NoSpaceDotLowerIdent or next == .DotLowerIdent or next == .NoSpaceDotUpperIdent or next == .DotUpperIdent));
        if (!valid_lookahead) {
            break;
        }
        const clause = self.parseWhereClause();
        self.store.addScratchWhereClause(clause);
        if (self.peek() != .Comma) {
            break;
        }
        self.advance();
    }
    const where_clauses = self.store.whereClauseSpanFrom(where_clauses_top);
    const coll_id = self.store.addCollection(.collection_where_clause, .{
        .region = .{ .start = start, .end = self.pos },
        .span = where_clauses.span,
    });
    return coll_id;
}

/// Whether Pattern Alternatives are allowed in the current context
const Alternatives = enum {
    alternatives_allowed,
    alternatives_forbidden,
};

/// todo -- what does this do?
pub fn parsePattern(self: *Parser, alternatives: Alternatives) AST.Pattern.Idx {
    const outer_start = self.pos;
    const patterns_scratch_top = self.store.scratchPatternTop();
    errdefer self.store.clearScratchPatternsFrom(patterns_scratch_top);
    while (self.peek() != .EndOfFile) {
        const start = self.pos;
        var pattern: ?AST.Pattern.Idx = null;
        switch (self.peek()) {
            .LowerIdent => {
                pattern = self.store.addPattern(.{ .ident = .{
                    .ident_tok = start,
                    .region = .{ .start = start, .end = self.pos },
                } });
                self.advance();
            },
            .NamedUnderscore => {
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
                    const args_end = self.parseCollectionSpan(AST.Pattern.Idx, .CloseRound, NodeStore.addScratchPattern, parsePatternWithAlts) catch {
                        while (self.peek() != .CloseRound and self.peek() != .EndOfFile) {
                            self.advance();
                        }
                        self.store.clearScratchPatternsFrom(scratch_top);
                        return self.pushMalformed(AST.Pattern.Idx, .pattern_unexpected_token, start);
                    };
                    const args = self.store.patternSpanFrom(scratch_top);
                    pattern = self.store.addPattern(.{ .tag = .{
                        .region = .{ .start = start, .end = args_end },
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
                const end = self.parseCollectionSpan(AST.Pattern.Idx, .CloseSquare, NodeStore.addScratchPattern, parsePatternWithAlts) catch {
                    while (self.peek() != .CloseSquare and self.peek() != .EndOfFile) {
                        self.advance();
                    }
                    self.store.clearScratchPatternsFrom(scratch_top);
                    return self.pushMalformed(AST.Pattern.Idx, .pattern_unexpected_token, start);
                };
                const patterns = self.store.patternSpanFrom(scratch_top);

                pattern = self.store.addPattern(.{ .list = .{
                    .region = .{ .start = start, .end = end },
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
                    return self.pushMalformed(AST.Pattern.Idx, .pattern_unexpected_token, start);
                }
                const end = self.pos;
                self.advance();
                pattern = self.store.addPattern(.{ .record = .{
                    .region = .{ .start = start, .end = end },
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
                        return self.pushMalformed(AST.Pattern.Idx, .pattern_unexpected_token, start);
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
                const end = self.parseCollectionSpan(AST.Pattern.Idx, .CloseRound, NodeStore.addScratchPattern, parsePatternWithAlts) catch {
                    while (self.peek() != .CloseRound and self.peek() != .EndOfFile) {
                        self.advance();
                    }
                    self.store.clearScratchPatternsFrom(scratch_top);
                    return self.pushMalformed(AST.Pattern.Idx, .pattern_unexpected_token, start);
                };
                const patterns = self.store.patternSpanFrom(scratch_top);

                pattern = self.store.addPattern(.{ .tuple = .{
                    .patterns = patterns,
                    .region = .{ .start = start, .end = end },
                } });
            },
            else => {
                return self.pushMalformed(AST.Pattern.Idx, .pattern_unexpected_token, self.pos);
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
        return self.store.addMalformed(AST.Pattern.Idx, .pattern_unexpected_eof, .{ .start = outer_start, .end = self.pos });
    }
    const last_pattern = self.store.scratch_patterns.items.items[self.store.scratchPatternTop() - 1];
    const last_pattern_region = self.store.nodes.items.items(.region)[@intFromEnum(last_pattern)];
    const patterns = self.store.patternSpanFrom(patterns_scratch_top);
    return self.store.addPattern(.{ .alternatives = .{
        .region = .{ .start = outer_start, .end = last_pattern_region.end },
        .patterns = patterns,
    } });
}

fn parsePatternNoAlts(self: *Parser) AST.Pattern.Idx {
    return self.parsePattern(.alternatives_forbidden);
}
fn parsePatternWithAlts(self: *Parser) AST.Pattern.Idx {
    return self.parsePattern(.alternatives_allowed);
}

/// todo
pub fn parsePatternRecordField(self: *Parser, alternatives: Alternatives) AST.PatternRecordField.Idx {
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
        return self.pushMalformed(AST.PatternRecordField.Idx, .expected_lower_ident_pat_field_name, field_start);
    }
    const name = self.pos;
    self.advance();
    var value: ?AST.Pattern.Idx = null;
    if (self.peek() != .OpColon and (self.peekNext() != .Comma or self.peekNext() != .CloseCurly)) {
        while (self.peek() != .CloseCurly and self.peek() != .EndOfFile) {
            self.advance();
        }
        return self.pushMalformed(AST.PatternRecordField.Idx, .expected_colon_after_pat_field_name, field_start);
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
pub fn parseExpr(self: *Parser) AST.Expr.Idx {
    return self.parseExprWithBp(0);
}

/// todo
pub fn parseExprWithBp(self: *Parser, min_bp: u8) AST.Expr.Idx {
    const start = self.pos;
    var expr: ?AST.Expr.Idx = null;
    const token = self.peek();
    switch (token) {
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
        .NamedUnderscore => {
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
        .Float => {
            self.advance();
            expr = self.store.addExpr(.{ .float = .{
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
            const list_end = self.parseCollectionSpan(AST.Expr.Idx, .CloseSquare, NodeStore.addScratchExpr, parseExpr) catch {
                while (self.peek() != .CloseSquare and self.peek() != .EndOfFile) {
                    self.advance();
                }
                self.store.clearScratchExprsFrom(scratch_top);
                return self.pushMalformed(AST.Expr.Idx, .expected_expr_close_square_or_comma, self.pos);
            };
            const items = self.store.exprSpanFrom(scratch_top);
            expr = self.store.addExpr(.{ .list = .{
                .items = items,
                .region = .{ .start = start, .end = list_end },
            } });
        },
        .NoSpaceOpenRound, .OpenRound => {
            self.advance();
            // TODO: Parenthesized expressions
            const scratch_top = self.store.scratchExprTop();
            const end = self.parseCollectionSpan(AST.Expr.Idx, .CloseRound, NodeStore.addScratchExpr, parseExpr) catch {
                while (self.peek() != .CloseRound and self.peek() != .EndOfFile) {
                    self.advance();
                }
                self.store.clearScratchExprsFrom(scratch_top);
                return self.pushMalformed(AST.Expr.Idx, .expected_expr_close_round_or_comma, self.pos);
            };
            const items = self.store.exprSpanFrom(scratch_top);
            expr = self.store.addExpr(.{ .tuple = .{
                .items = items,
                .region = .{ .start = start, .end = end },
            } });
        },
        .OpenCurly => {
            self.advance();
            // Is this a Record or a Block?
            if (self.peek() == .CloseCurly or (self.peek() == .LowerIdent and (self.peekNext() == .OpColon or self.peekNext() == .Comma))) {
                // This is the best guesstimation of this being a Record for now.  I believe we have to have a NoSpaceOpColon
                // for this to be full-proof without backtracking.
                const scratch_top = self.store.scratchRecordFieldTop();
                const end = self.parseCollectionSpan(AST.RecordField.Idx, .CloseCurly, NodeStore.addScratchRecordField, parseRecordField) catch {
                    self.store.clearScratchRecordFieldsFrom(scratch_top);
                    return self.pushMalformed(AST.Expr.Idx, .expected_expr_close_curly_or_comma, self.pos);
                };
                const fields = self.store.recordFieldSpanFrom(scratch_top);
                expr = self.store.addExpr(.{ .record = .{
                    .fields = fields,
                    .region = .{ .start = start, .end = end },
                } });
            } else {
                const scratch_top = self.store.scratchStatementTop();
                var end = self.pos;

                while (self.peek() != .EndOfFile) {
                    const statement = self.parseStmt() orelse break;
                    self.store.addScratchStatement(statement);
                    end = self.pos;
                    if (self.peek() == .CloseCurly) {
                        self.advance();
                        break;
                    }
                }

                const statements = self.store.statementSpanFrom(scratch_top);

                expr = self.store.addExpr(.{ .block = .{
                    .statements = statements,
                    .region = .{ .start = start, .end = end },
                } });
            }
        },
        .OpBar => {
            self.advance();
            const scratch_top = self.store.scratchPatternTop();
            _ = self.parseCollectionSpan(AST.Pattern.Idx, .OpBar, NodeStore.addScratchPattern, parsePatternNoAlts) catch {
                self.store.clearScratchPatternsFrom(scratch_top);
                return self.pushMalformed(AST.Expr.Idx, .expected_expr_bar, self.pos);
            };
            const args = self.store.patternSpanFrom(scratch_top);

            const body = self.parseExpr();
            const body_region = self.store.nodes.items.items(.region)[@intFromEnum(body)];
            expr = self.store.addExpr(.{ .lambda = .{
                .body = body,
                .args = args,
                .region = .{ .start = start, .end = body_region.end },
            } });
        },
        .KwIf => {
            self.advance();
            const condition = self.parseExpr();
            const then = self.parseExpr();
            if (self.peek() != .KwElse) {
                return self.pushMalformed(AST.Expr.Idx, .no_else, self.pos);
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
                return self.pushMalformed(AST.Expr.Idx, .expected_open_curly_after_match, self.pos);
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
                return self.pushMalformed(AST.Expr.Idx, .expected_close_curly_at_end_of_match, self.pos);
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
            return self.pushMalformed(AST.Expr.Idx, .expr_unexpected_token, start);
        },
    }
    if (expr) |e| {
        var expression = self.parseExprSuffix(start, e);
        while (self.peek() == .NoSpaceDotInt or self.peek() == .NoSpaceDotLowerIdent or self.peek() == .DotLowerIdent or self.peek() == .OpArrow) {
            const tok = self.peek();
            if (tok == .NoSpaceDotInt) {
                return self.pushMalformed(AST.Expr.Idx, .expr_no_space_dot_int, self.pos);
            } else if (self.peek() == .OpArrow) {
                const s = self.pos;
                self.advance();
                if (self.peek() == .LowerIdent) {
                    const ident = self.store.addExpr(.{ .ident = .{
                        .region = .{ .start = self.pos, .end = self.pos },
                        .token = self.pos,
                        .qualifier = null,
                    } });
                    self.advance();
                    const ident_suffixed = self.parseExprSuffix(s, ident);
                    expression = self.store.addExpr(.{ .local_dispatch = .{
                        .region = .{ .start = start, .end = self.pos },
                        .operator = s,
                        .left = expression,
                        .right = ident_suffixed,
                    } });
                } else if (self.peek() == .UpperIdent) { // UpperIdent - should be a tag
                    const tag = self.store.addExpr(.{ .tag = .{
                        .region = .{ .start = self.pos, .end = self.pos },
                        .token = self.pos,
                    } });
                    self.advance();
                    const ident_suffixed = self.parseExprSuffix(s, tag);
                    expression = self.store.addExpr(.{ .local_dispatch = .{
                        .region = .{ .start = start, .end = self.pos },
                        .operator = s,
                        .left = expression,
                        .right = ident_suffixed,
                    } });
                } else {
                    return self.pushMalformed(AST.Expr.Idx, .expr_arrow_expects_ident, self.pos);
                }
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
    return self.store.addMalformed(AST.Expr.Idx, .expr_unexpected_token, .{ .start = start, .end = self.pos });
}

/// todo
fn parseExprSuffix(self: *Parser, start: u32, e: AST.Expr.Idx) AST.Expr.Idx {
    var expression = e;
    // Check for an apply...
    if (self.peek() == .NoSpaceOpenRound) {
        self.advance();
        const scratch_top = self.store.scratchExprTop();
        const end = self.parseCollectionSpan(AST.Expr.Idx, .CloseRound, NodeStore.addScratchExpr, parseExpr) catch {
            self.store.clearScratchExprsFrom(scratch_top);
            return self.pushMalformed(AST.Expr.Idx, .expected_expr_apply_close_round, start);
        };
        const args = self.store.exprSpanFrom(scratch_top);

        expression = self.store.addExpr(.{ .apply = .{
            .args = args,
            .@"fn" = e,
            .region = .{ .start = start, .end = end },
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
pub fn parseRecordField(self: *Parser) AST.RecordField.Idx {
    const start = self.pos;
    self.expect(.LowerIdent) catch {
        return self.pushMalformed(AST.RecordField.Idx, .expected_expr_record_field_name, start);
    };
    const name = start;
    var value: ?AST.Expr.Idx = null;
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
pub fn parseBranch(self: *Parser) AST.WhenBranch.Idx {
    const start = self.pos;
    const p = self.parsePattern(.alternatives_allowed);
    if (self.peek() == .OpFatArrow) {
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
pub fn parseStringExpr(self: *Parser) AST.Expr.Idx {
    std.debug.assert(self.peek() == .StringStart);
    const start = self.pos;
    // Start parsing string with possible interpolations
    // e.g.:
    // StringStart, StringPart, OpenStringInterpolation, <expr>, CloseStringInterpolation, StringPart, StringEnd
    self.advanceOne();
    const scratch_top = self.store.scratchExprTop();
    while (self.peek() != .EndOfFile) {
        var string_end = self.pos;
        switch (self.peek()) {
            .StringEnd => {
                string_end = self.pos;
                self.advanceOne(); // Advance past ONLY the StringEnd
                break;
            },
            .StringPart => {
                const index = self.store.addExpr(.{ .string_part = .{
                    .token = self.pos,
                    .region = .{ .start = self.pos, .end = self.pos },
                } });
                self.advanceOne(); // Advance past ONLY the StringPart
                self.store.addScratchExpr(index);
            },
            .OpenStringInterpolation => {
                self.advance(); // Advance past OpenStringInterpolation
                const ex = self.parseExpr();
                self.store.addScratchExpr(ex);
                while (self.peek() == .Newline) {
                    self.advanceOne(); // Advance ONLY past the Newline
                }
                if (self.peek() != .CloseStringInterpolation) {
                    return self.pushMalformed(AST.Expr.Idx, .string_expected_close_interpolation, start);
                }
                self.advanceOne(); // Advance ONLY past the CloseString Interpolation
            },
            else => {
                // Something is broken in the tokenizer if we get here!
                return self.pushMalformed(AST.Expr.Idx, .string_unexpected_token, self.pos);
            },
        }
    }
    const parts = self.store.exprSpanFrom(scratch_top);
    const expr = self.store.addExpr(.{
        .string = .{
            .token = start,
            .parts = parts,
            .region = .{
                .start = start,
                .end = self.pos - 1, // we want the previous token's end position here
            },
        },
    });
    return expr;
}

/// todo
pub fn parseStringPattern(self: *Parser) AST.Pattern.Idx {
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
pub fn parseTypeHeader(self: *Parser) AST.TypeHeader.Idx {
    const start = self.pos;
    std.debug.assert(self.peek() == .UpperIdent);
    self.advance(); // Advance past UpperIdent
    if (self.peek() != .NoSpaceOpenRound and self.peek() != .OpenRound) {
        return self.store.addTypeHeader(.{
            .name = start,
            .args = .{ .span = .{
                .start = 0,
                .len = 0,
            } },
            .region = .{ .start = start, .end = start },
        });
    }
    self.advance();
    const scratch_top = self.store.scratchTypeAnnoTop();
    const end = self.parseCollectionSpan(AST.TypeAnno.Idx, .CloseRound, NodeStore.addScratchTypeAnno, Parser.parseTypeIdent) catch {
        self.store.clearScratchTypeAnnosFrom(scratch_top);
        return self.pushMalformed(AST.TypeHeader.Idx, .expected_ty_anno_end, start);
    };
    const args = self.store.typeAnnoSpanFrom(scratch_top);
    return self.store.addTypeHeader(.{
        .name = start,
        .args = args,
        .region = .{ .start = start, .end = end },
    });
}

fn parseTypeIdent(self: *Parser) AST.TypeAnno.Idx {
    if (self.peek() == .LowerIdent) {
        const tok = self.pos;
        self.advance();
        return self.store.addTypeAnno(.{ .ty_var = .{
            .region = .{ .start = tok, .end = tok },
            .tok = tok,
        } });
    }
    return self.pushMalformed(AST.TypeAnno.Idx, .invalid_type_arg, self.pos);
}

const TyFnArgs = enum {
    not_looking_for_args,
    looking_for_args,
};

/// Parse a type annotation, e.g. `Foo(a) : (a,Str,I64)`
pub fn parseTypeAnno(self: *Parser, looking_for_args: TyFnArgs) AST.TypeAnno.Idx {
    const start = self.pos;
    var anno: ?AST.TypeAnno.Idx = null;

    switch (self.peek()) {
        .UpperIdent => {
            self.advance(); // Advance past UpperIdent
            if (self.peek() == .NoSpaceDotUpperIdent or self.peek() == .DotUpperIdent) {
                const second_ident = self.pos;
                self.advance(); // Advance past NoSpaceDotUpperIdent
                anno = self.store.addTypeAnno(.{ .mod_ty = .{
                    .region = .{ .start = start, .end = second_ident },
                    .ty_ident = self.tok_buf.resolveIdentifier(start).?,
                    .mod_ident = self.tok_buf.resolveIdentifier(second_ident).?,
                } });
            } else {
                anno = self.store.addTypeAnno(.{ .ty = .{
                    .region = .{ .start = start, .end = start },
                    .ident = self.tok_buf.resolveIdentifier(start).?,
                } });
            }

            if (self.peek() == .NoSpaceOpenRound) {
                self.advance(); // Advance past NoSpaceOpenRound
                const scratch_top = self.store.scratchTypeAnnoTop();
                self.store.addScratchTypeAnno(anno orelse {
                    return self.store.addMalformed(AST.TypeAnno.Idx, .ty_anno_unexpected_token, .{ .start = start, .end = self.pos });
                });
                const end = self.parseCollectionSpan(AST.TypeAnno.Idx, .CloseRound, NodeStore.addScratchTypeAnno, parseTypeAnnoInCollection) catch {
                    self.store.clearScratchTypeAnnosFrom(scratch_top);
                    return self.pushMalformed(AST.TypeAnno.Idx, .expected_ty_apply_close_round, start);
                };

                anno = self.store.addTypeAnno(.{ .apply = .{
                    .region = .{ .start = start, .end = end },
                    .args = self.store.typeAnnoSpanFrom(scratch_top),
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
        .NoSpaceOpenRound, .OpenRound => {
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
                const effectful = self.peek() == .OpFatArrow;
                self.advance(); // Advance past arrow
                const ret = self.parseTypeAnno(.looking_for_args);
                const ret_region = self.store.nodes.items.items(.region)[@intFromEnum(ret)];
                if (self.peek() != .CloseRound) {
                    self.store.clearScratchTypeAnnosFrom(scratch_top);
                    return self.pushMalformed(AST.TypeAnno.Idx, .expected_ty_anno_end_of_function, start);
                }
                const function = self.store.addTypeAnno(.{ .@"fn" = .{
                    .args = args,
                    .ret = ret,
                    .effectful = effectful,
                    .region = .{ .start = after_round, .end = ret_region.end },
                } });
                const end = self.pos;
                self.advance();
                return self.store.addTypeAnno(.{ .parens = .{
                    .anno = function,
                    .region = .{ .start = start, .end = end },
                } });
            }
            if (self.peek() != .CloseRound) {
                self.store.clearScratchTypeAnnosFrom(scratch_top);
                return self.pushMalformed(AST.TypeAnno.Idx, .expected_ty_anno_end, start);
            }
            const end = self.pos;
            self.advance(); // Advance past CloseRound
            const annos = self.store.typeAnnoSpanFrom(scratch_top);
            anno = self.store.addTypeAnno(.{ .tuple = .{
                .region = .{ .start = start, .end = end },
                .annos = annos,
            } });
        },
        .OpenCurly => {
            self.advance(); // Advance past OpenCurly
            const scratch_top = self.store.scratchAnnoRecordFieldTop();
            const end = self.parseCollectionSpan(AST.AnnoRecordField.Idx, .CloseCurly, NodeStore.addScratchAnnoRecordField, parseAnnoRecordField) catch {
                self.store.clearScratchAnnoRecordFieldsFrom(scratch_top);
                return self.pushMalformed(AST.TypeAnno.Idx, .expected_ty_close_curly_or_comma, self.pos);
            };
            const fields = self.store.annoRecordFieldSpanFrom(scratch_top);
            anno = self.store.addTypeAnno(.{ .record = .{
                .region = .{ .start = start, .end = end },
                .fields = fields,
            } });
        },
        .OpenSquare => {
            self.advance(); // Advance past OpenSquare
            const scratch_top = self.store.scratchTypeAnnoTop();
            const end = self.parseCollectionSpan(AST.TypeAnno.Idx, .CloseSquare, NodeStore.addScratchTypeAnno, parseTypeAnnoInCollection) catch {
                self.store.clearScratchTypeAnnosFrom(scratch_top);
                return self.pushMalformed(AST.TypeAnno.Idx, .expected_ty_close_square_or_comma, self.pos);
            };
            const tags = self.store.typeAnnoSpanFrom(scratch_top);
            anno = self.store.addTypeAnno(.{ .tag_union = .{
                .region = .{ .start = start, .end = end },
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
            return self.pushMalformed(AST.TypeAnno.Idx, .ty_anno_unexpected_token, self.pos);
        },
    }

    if (anno) |an| {
        const curr = self.peek();
        const next_tok = self.peekNext();
        const two_away_tok = self.peekN(2);
        const curr_is_arrow = curr == .OpArrow or curr == .OpFatArrow;
        const next_is_not_lower_ident = next_tok != .LowerIdent;
        const not_followed_by_colon = two_away_tok != .OpColon;
        if ((looking_for_args == .not_looking_for_args) and
            (curr_is_arrow or
                (curr == .Comma and (next_is_not_lower_ident or not_followed_by_colon) and next_tok != .CloseCurly)))
        {
            const scratch_top = self.store.scratchTypeAnnoTop();
            self.store.addScratchTypeAnno(an);
            while (self.peek() == .Comma) {
                self.advance(); // Advance past Comma
                self.store.addScratchTypeAnno(self.parseTypeAnno(.looking_for_args));
            }
            const args = self.store.typeAnnoSpanFrom(scratch_top);
            if (self.peek() != .OpArrow and self.peek() != .OpFatArrow) {
                return self.pushMalformed(AST.TypeAnno.Idx, .expected_arrow, start);
            }
            const effectful = self.peek() == .OpFatArrow;
            self.advance(); // Advance past arrow
            // TODO: Handle thin vs fat arrow
            const ret = self.parseTypeAnno(.looking_for_args);
            const region = self.store.nodes.items.items(.region)[@intFromEnum(ret)];
            return self.store.addTypeAnno(.{ .@"fn" = .{
                .region = .{ .start = start, .end = region.end },
                .args = args,
                .ret = ret,
                .effectful = effectful,
            } });
        }
        return an;
    }

    return self.store.addMalformed(AST.TypeAnno.Idx, .ty_anno_unexpected_token, .{ .start = start, .end = self.pos });
}

/// todo
pub fn parseTypeAnnoInCollection(self: *Parser) AST.TypeAnno.Idx {
    return self.parseTypeAnno(.looking_for_args);
}

/// todo
pub fn parseAnnoRecordField(self: *Parser) AST.AnnoRecordField.Idx {
    const field_start = self.pos;
    if (self.peek() != .LowerIdent) {
        while (self.peek() != .CloseCurly and self.peek() != .Comma and self.peek() != .EndOfFile) {
            self.advance(); // Advance until we end this field or the record
        }
        return self.pushMalformed(AST.AnnoRecordField.Idx, .expected_type_field_name, field_start);
    }
    const name = self.pos;
    self.advance(); // Advance past LowerIdent
    if (self.peek() != .OpColon) {
        while (self.peek() != .CloseCurly and self.peek() != .Comma and self.peek() != .EndOfFile) {
            self.advance(); // Advance until we end this field or the record
        }
        return self.pushMalformed(AST.AnnoRecordField.Idx, .expected_colon_after_type_field_name, field_start);
    }
    self.advance(); // Advance past OpColon
    const ty = self.parseTypeAnno(.not_looking_for_args);

    return self.store.addAnnoRecordField(.{
        .region = .{ .start = field_start, .end = self.pos },
        .name = name,
        .ty = ty,
    });
}

/// Parse a where clause
///
/// e.g. `a.hash(hasher) -> hasher`
/// e.g. `hasher.Hasher`
/// e.g. `module(a).decode(List(U8)) -> a`
pub fn parseWhereClause(self: *Parser) AST.WhereClause.Idx {
    const start = self.pos;
    if (self.peek() == .KwModule) {
        // Parsing a mod_method clause
        self.advance();
        self.expect(.NoSpaceOpenRound) catch {
            return self.store.addMalformed(
                AST.WhereClause.Idx,
                .where_expected_mod_open,
                .{ .start = start, .end = self.pos },
            );
        };
        const var_tok = self.pos;
        self.expect(.LowerIdent) catch {
            return self.store.addMalformed(
                AST.WhereClause.Idx,
                .where_expected_var,
                .{ .start = start, .end = self.pos },
            );
        };
        self.expect(.CloseRound) catch {
            return self.store.addMalformed(
                AST.WhereClause.Idx,
                .where_expected_mod_close,
                .{ .start = start, .end = self.pos },
            );
        };
        const name_tok = self.pos;
        if (self.peek() != .NoSpaceDotLowerIdent and self.peek() != .DotLowerIdent) {
            return self.store.addMalformed(
                AST.WhereClause.Idx,
                .where_expected_method_or_alias_name,
                .{ .start = start, .end = self.pos },
            );
        }
        self.advance();
        const arg_start = self.pos;
        self.expect(.NoSpaceOpenRound) catch {
            return self.store.addMalformed(
                AST.WhereClause.Idx,
                .where_expected_arg_open,
                .{ .start = start, .end = self.pos },
            );
        };
        const ty_anno_top = self.store.scratchTypeAnnoTop();
        const arg_end = self.parseCollectionSpan(
            AST.TypeAnno.Idx,
            .CloseRound,
            NodeStore.addScratchTypeAnno,
            Parser.parseTypeAnnoInCollection,
        ) catch {
            self.store.clearScratchTypeAnnosFrom(ty_anno_top);
            return self.store.addMalformed(
                AST.WhereClause.Idx,
                .where_expected_arg_close,
                .{ .start = start, .end = self.pos },
            );
        };
        const arg_span = self.store.typeAnnoSpanFrom(ty_anno_top);
        const args = self.store.addCollection(
            .collection_ty_anno,
            .{
                .region = .{ .start = arg_start, .end = arg_end },
                .span = arg_span.span,
            },
        );
        self.expect(.OpArrow) catch {
            return self.store.addMalformed(
                AST.WhereClause.Idx,
                .where_expected_method_arrow,
                .{ .start = start, .end = self.pos },
            );
        };
        const ret_ty_anno = self.parseTypeAnnoInCollection();
        return self.store.addWhereClause(.{ .mod_method = .{
            .region = .{ .start = start, .end = self.pos },
            .name_tok = name_tok,
            .var_tok = var_tok,
            .args = args,
            .ret_anno = ret_ty_anno,
        } });
    } else if (self.peek() == .LowerIdent) {
        // Parsing either an alias or method clause
        const var_tok = self.pos;
        self.advance();
        const name_tok = self.pos;
        if (self.peek() == .NoSpaceDotLowerIdent or self.peek() == .DotLowerIdent) {
            self.advance();
            const arg_start = self.pos;
            self.expect(.NoSpaceOpenRound) catch {
                return self.store.addMalformed(
                    AST.WhereClause.Idx,
                    .where_expected_arg_open,
                    .{ .start = start, .end = self.pos },
                );
            };
            const ty_anno_top = self.store.scratchTypeAnnoTop();
            const arg_end = self.parseCollectionSpan(
                AST.TypeAnno.Idx,
                .CloseRound,
                NodeStore.addScratchTypeAnno,
                Parser.parseTypeAnnoInCollection,
            ) catch {
                self.store.clearScratchTypeAnnosFrom(ty_anno_top);
                return self.store.addMalformed(
                    AST.WhereClause.Idx,
                    .where_expected_arg_close,
                    .{ .start = start, .end = self.pos },
                );
            };
            const arg_span = self.store.typeAnnoSpanFrom(ty_anno_top);
            const args = self.store.addCollection(
                .collection_ty_anno,
                .{
                    .region = .{ .start = arg_start, .end = arg_end },
                    .span = arg_span.span,
                },
            );
            self.expect(.OpArrow) catch {
                return self.store.addMalformed(
                    AST.WhereClause.Idx,
                    .where_expected_method_arrow,
                    .{ .start = start, .end = self.pos },
                );
            };
            const ret_ty_anno = self.parseTypeAnnoInCollection();
            return self.store.addWhereClause(.{ .method = .{
                .region = .{ .start = start, .end = self.pos },
                .name_tok = name_tok,
                .var_tok = var_tok,
                .args = args,
                .ret_anno = ret_ty_anno,
            } });
        } else if (self.peek() == .NoSpaceDotUpperIdent or self.peek() == .DotUpperIdent) {
            const alias_tok = self.pos;
            self.advance();
            return self.store.addWhereClause(.{ .alias = .{
                .region = .{ .start = start, .end = alias_tok },
                .var_tok = var_tok,
                .alias_tok = alias_tok,
            } });
        } else {
            return self.store.addMalformed(
                AST.WhereClause.Idx,
                .where_expected_method_or_alias_name,
                .{ .start = start, .end = self.pos },
            );
        }
    } else {
        // malformed
        return self.store.addMalformed(
            AST.WhereClause.Idx,
            .where_expected_var_or_module,
            .{ .start = start, .end = self.pos },
        );
    }
}

/// todo
pub fn addProblem(self: *Parser, diagnostic: AST.Diagnostic) void {
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
