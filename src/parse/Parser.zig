//! Parser for converting tokenized Roc source code into an Abstract Syntax Tree.
//!
//! This module provides the Parser struct which takes a buffer of tokens and
//! transforms them into an AST representation. The parser handles syntax errors
//! gracefully by inserting malformed placeholders and continuing compilation,
//! following the "Inform Don't Block" philosophy.

const std = @import("std");
const base = @import("base");
const tracy = @import("tracy");
const collections = @import("collections");

const AST = @import("AST.zig");
const Node = @import("Node.zig");
const NodeStore = @import("NodeStore.zig");
const NodeList = AST.NodeList;
const TokenizedBuffer = tokenize.TokenizedBuffer;
const Token = tokenize.Token;
const TokenIdx = Token.Idx;
const tokenize = @import("tokenize.zig");

const MAX_PARSE_DIAGNOSTICS: usize = 1_000;
const MAX_NESTING_LEVELS: u8 = 128;

/// A parser which tokenizes and parses source code into an abstract syntax tree.
pub const Parser = @This();

gpa: std.mem.Allocator,
pos: TokenIdx,
tok_buf: TokenizedBuffer,
store: NodeStore,
scratch_nodes: std.ArrayListUnmanaged(Node.Idx),
diagnostics: std.ArrayListUnmanaged(AST.Diagnostic),
cached_malformed_node: ?Node.Idx,
nesting_counter: u8,

/// init the parser from a buffer of tokens
pub fn init(tokens: TokenizedBuffer, gpa: std.mem.Allocator) std.mem.Allocator.Error!Parser {
    const estimated_node_count = tokens.tokens.len;
    const store = try NodeStore.initCapacity(gpa, estimated_node_count);

    return Parser{
        .gpa = gpa,
        .pos = 0,
        .tok_buf = tokens,
        .store = store,
        .scratch_nodes = .{},
        .diagnostics = .{},
        .cached_malformed_node = null,
        .nesting_counter = MAX_NESTING_LEVELS,
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

/// helper to advance the parser by one token
pub fn advance(self: *Parser) void {
    self.pos += 1;
    // We have an EndOfFile token that we never expect to advance past
    std.debug.assert(self.pos < self.tok_buf.tokens.len);
}

/// look ahead at the next token and return an error if it does not have the expected tag
pub fn expect(self: *Parser, expected: Token.Tag) error{ExpectedNotFound}!void {
    if (self.peek() != expected) {
        return error.ExpectedNotFound;
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

/// Peek at the next token
pub fn peekNext(self: *Parser) Token.Tag {
    const next = self.pos + 1;
    if (next >= self.tok_buf.tokens.len) {
        return .EndOfFile;
    }
    return self.tok_buf.tokens.items(.tag)[next];
}

/// Peek at `n` tokens forward
pub fn peekN(self: *Parser, n: u32) Token.Tag {
    if (n == 0) {
        return self.peek();
    }
    const next = self.pos + n;
    if (next >= self.tok_buf.tokens.len) {
        return .EndOfFile;
    }
    return self.tok_buf.tokens.items(.tag)[next];
}

const StackError = error{TooNested};

/// The error set that methods of the Parser return
pub const Error = std.mem.Allocator.Error || StackError;

fn nest(self: *Parser) !void {
    if (self.nesting_counter == 0) {
        return StackError.TooNested;
    }
    self.nesting_counter = self.nesting_counter - 1;
}

fn unnest(self: *Parser) void {
    if (self.nesting_counter >= MAX_NESTING_LEVELS) {
        return;
    }
    self.nesting_counter = self.nesting_counter + 1;
}

/// add a diagnostic error
pub fn pushDiagnostic(self: *Parser, tag: AST.Diagnostic.Tag, region: AST.TokenizedRegion) Error!void {
    if (self.diagnostics.items.len < MAX_PARSE_DIAGNOSTICS) {
        try self.diagnostics.append(self.gpa, .{ .tag = tag, .region = region });
    }
}
/// add a malformed token
pub fn pushMalformed(self: *Parser, comptime T: type, tag: AST.Diagnostic.Tag, start: TokenIdx) Error!T {
    const pos = self.pos;

    if (self.peek() != .EndOfFile) {
        self.advance(); // TODO: find a better point to advance to
    }

    if (self.diagnostics.items.len < MAX_PARSE_DIAGNOSTICS) {
        // Create a diagnostic region that points to the problematic token
        // If the parser has moved too far from the start, use the start token for better error location
        const diagnostic_start = if (self.pos > start and (self.pos - start) > 2) start else @min(pos, self.pos);
        const diagnostic_end = if (self.pos > start and (self.pos - start) > 2) start + 1 else @max(pos, self.pos);
        // If start equals end, make it a single-token region
        const diagnostic_region = AST.TokenizedRegion{
            .start = diagnostic_start,
            .end = if (diagnostic_start == diagnostic_end) diagnostic_start + 1 else diagnostic_end,
        };

        // AST node should span the entire malformed expression
        const ast_region = AST.TokenizedRegion{ .start = start, .end = self.pos };

        try self.diagnostics.append(self.gpa, .{
            .tag = tag,
            .region = diagnostic_region,
        });

        return try self.store.addMalformed(T, tag, ast_region);
    } else {
        // Return a cached malformed node to avoid creating excessive nodes when diagnostic limit is exceeded
        if (self.cached_malformed_node == null) {
            // Create a generic malformed node with a fallback diagnostic tag
            const fallback_region = AST.TokenizedRegion{ .start = start, .end = self.pos };
            const nid = try self.store.nodes.append(self.gpa, .{
                .tag = .malformed,
                .main_token = 0,
                .data = .{ .lhs = @intFromEnum(AST.Diagnostic.Tag.expr_unexpected_token), .rhs = 0 },
                .region = fallback_region,
            });
            self.cached_malformed_node = nid;
        }
        // Cast the cached node to the requested type
        return @enumFromInt(@intFromEnum(self.cached_malformed_node.?));
    }
}
/// parse a `.roc` module
///
/// the tokens are provided at Parser initialisation
pub fn parseFile(self: *Parser) Error!void {
    const trace = tracy.trace(@src());
    defer trace.end();

    self.store.emptyScratch();
    try self.store.addFile(.{
        .header = @as(AST.Header.Idx, @enumFromInt(0)),
        .statements = AST.Statement.Span{ .span = base.DataSpan.empty() },
        .region = AST.TokenizedRegion.empty(),
    });

    const header = try self.parseHeader();
    const scratch_top = self.store.scratchStatementTop();

    while (self.peek() != .EndOfFile) {
        const current_scratch_top = self.store.scratchStatementTop();
        const idx = try self.parseTopLevelStatement();
        std.debug.assert(self.store.scratchStatementTop() == current_scratch_top);
        try self.store.addScratchStatement(idx);
    }

    try self.store.addFile(.{
        .header = header,
        .statements = try self.store.statementSpanFrom(scratch_top),
        .region = .{ .start = 0, .end = @intCast(self.tok_buf.tokens.len - 1) },
    });
}

/// Parses the items of type T until we encounter end_token, with each item separated by a Comma token
///
/// Returns the ending position of the collection
fn parseCollectionSpan(self: *Parser, comptime T: type, end_token: Token.Tag, scratch_fn: fn (*NodeStore, T) Error!void, parser: fn (*Parser) Error!T) !void {
    const trace = tracy.trace(@src());
    defer trace.end();

    while (self.peek() != end_token and self.peek() != .EndOfFile) {
        try scratch_fn(&self.store, try parser(self));
        self.expect(.Comma) catch {
            break;
        };
    }
    self.expect(end_token) catch {
        return error.ExpectedNotFound;
    };
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
            extra = self.tok_buf.env.getIdent(e.interned);
        }
        std.debug.print("{s}{d}: {s} \"{s}\"\n", .{ if (i == current) "-->" else "   ", i, @tagName(tag), extra });
    }
}

/// Parses a module header using the following grammar:
///
/// provides_entry :: [LowerIdent|UpperIdent] Comma Newline*
/// package_entry :: LowerIdent Comma "platform"? String Comma
/// app_header :: KwApp Newline* OpenSquare provides_entry* CloseSquare OpenCurly package_entry CloseCurly
pub fn parseHeader(self: *Parser) Error!AST.Header.Idx {
    const trace = tracy.trace(@src());
    defer trace.end();

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
            // No header keyword found - this is a type module
            return try self.store.addHeader(.{ .type_module = .{
                .region = .{ .start = 0, .end = 0 },
            } });
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
pub fn parsePlatformHeader(self: *Parser) Error!AST.Header.Idx {
    const trace = tracy.trace(@src());
    defer trace.end();

    const start = self.pos;
    std.debug.assert(self.peek() == .KwPlatform);
    self.advance(); // Advance past KwPlatform

    // Get name
    self.expect(.StringStart) catch {
        return try self.pushMalformed(
            AST.Header.Idx,
            .expected_platform_name_start,
            self.pos,
        );
    };
    const name = self.pos;
    self.expect(.StringPart) catch {
        return try self.pushMalformed(
            AST.Header.Idx,
            .expected_platform_name_string,
            self.pos,
        );
    };
    self.expect(.StringEnd) catch {
        return try self.pushMalformed(
            AST.Header.Idx,
            .expected_platform_name_end,
            self.pos,
        );
    };

    self.expect(.KwRequires) catch {
        return try self.pushMalformed(
            AST.Header.Idx,
            .expected_requires,
            self.pos,
        );
    };
    // Get requires rigids
    const rigids_start = self.pos;
    self.expect(.OpenCurly) catch {
        return try self.pushMalformed(
            AST.Header.Idx,
            .expected_requires_rigids_open_curly,
            self.pos,
        );
    };
    const rigids_top = self.store.scratchExposedItemTop();
    self.parseCollectionSpan(
        AST.ExposedItem.Idx,
        .CloseCurly,
        NodeStore.addScratchExposedItem,
        Parser.parseExposedItem,
    ) catch |err| {
        switch (err) {
            error.ExpectedNotFound => {
                self.store.clearScratchExposedItemsFrom(rigids_top);
                return try self.pushMalformed(
                    AST.Header.Idx,
                    .expected_requires_rigids_close_curly,
                    rigids_start,
                );
            },
            error.OutOfMemory => return error.OutOfMemory,
            error.TooNested => return error.TooNested,
        }
    };
    const rigids_span = try self.store.exposedItemSpanFrom(rigids_top);
    const rigids = try self.store.addCollection(
        .collection_exposed,
        .{
            .span = rigids_span.span,
            .region = .{
                .start = rigids_start,
                .end = self.pos,
            },
        },
    );

    // Get requires signatures
    const signatures_start = self.pos;
    self.expect(.OpenCurly) catch {
        return try self.pushMalformed(
            AST.Header.Idx,
            .expected_requires_signatures_open_curly,
            self.pos,
        );
    };
    const signatures_top = self.store.scratchAnnoRecordFieldTop();
    self.parseCollectionSpan(
        AST.AnnoRecordField.Idx,
        .CloseCurly,
        NodeStore.addScratchAnnoRecordField,
        Parser.parseAnnoRecordField,
    ) catch |err| {
        switch (err) {
            error.ExpectedNotFound => {
                return try self.pushMalformed(
                    AST.Header.Idx,
                    .expected_requires_signatures_close_curly,
                    signatures_start,
                );
            },
            error.OutOfMemory => return error.OutOfMemory,
            error.TooNested => return error.TooNested,
        }
    };
    const signatures_span = try self.store.annoRecordFieldSpanFrom(signatures_top);
    const signatures = try self.store.addTypeAnno(.{ .record = .{
        .fields = signatures_span,
        .region = .{
            .start = signatures_start,
            .end = self.pos,
        },
    } });

    // Get exposes
    self.expect(.KwExposes) catch {
        return try self.pushMalformed(
            AST.Header.Idx,
            .expected_exposes,
            self.pos,
        );
    };
    const exposes_start = self.pos;
    self.expect(.OpenSquare) catch {
        return try self.pushMalformed(
            AST.Header.Idx,
            .expected_exposes_open_square,
            self.pos,
        );
    };
    const exposes_top = self.store.scratchExposedItemTop();
    self.parseCollectionSpan(
        AST.ExposedItem.Idx,
        .CloseSquare,
        NodeStore.addScratchExposedItem,
        Parser.parseExposedItem,
    ) catch |err| {
        switch (err) {
            error.ExpectedNotFound => {
                self.store.clearScratchExposedItemsFrom(exposes_top);
                return try self.pushMalformed(
                    AST.Header.Idx,
                    .expected_exposes_close_square,
                    exposes_start,
                );
            },
            error.OutOfMemory => return error.OutOfMemory,
            error.TooNested => return error.TooNested,
        }
    };
    const exposes_span = try self.store.exposedItemSpanFrom(exposes_top);
    const exposes = try self.store.addCollection(
        .collection_exposed,
        .{
            .span = exposes_span.span,
            .region = .{ .start = exposes_start, .end = self.pos },
        },
    );

    // Get packages
    self.expect(.KwPackages) catch {
        return try self.pushMalformed(
            AST.Header.Idx,
            .expected_imports,
            self.pos,
        );
    };
    const packages_start = self.pos;
    const packages_top = self.store.scratchRecordFieldTop();
    self.expect(.OpenCurly) catch {
        return try self.pushMalformed(
            AST.Header.Idx,
            .expected_packages_open_curly,
            self.pos,
        );
    };
    self.parseCollectionSpan(
        AST.RecordField.Idx,
        .CloseCurly,
        NodeStore.addScratchRecordField,
        Parser.parseRecordField,
    ) catch |err| {
        switch (err) {
            error.ExpectedNotFound => {
                self.store.clearScratchRecordFieldsFrom(packages_top);
                return try self.pushMalformed(
                    AST.Header.Idx,
                    .expected_packages_close_curly,
                    self.pos,
                );
            },
            error.OutOfMemory => return error.OutOfMemory,
            error.TooNested => return error.TooNested,
        }
    };
    const packages_span = try self.store.recordFieldSpanFrom(packages_top);
    const packages = try self.store.addCollection(
        .collection_packages,
        .{
            .span = packages_span.span,
            .region = .{ .start = packages_start, .end = self.pos },
        },
    );

    // Get provides
    self.expect(.KwProvides) catch {
        return try self.pushMalformed(
            AST.Header.Idx,
            .expected_provides,
            self.pos,
        );
    };
    const provides_start = self.pos;
    self.expect(.OpenCurly) catch {
        return try self.pushMalformed(
            AST.Header.Idx,
            .expected_provides_open_curly,
            self.pos,
        );
    };
    const provides_top = self.store.scratchRecordFieldTop();
    self.parseCollectionSpan(
        AST.RecordField.Idx,
        .CloseCurly,
        NodeStore.addScratchRecordField,
        Parser.parseRecordField,
    ) catch |err| {
        switch (err) {
            error.ExpectedNotFound => {
                self.store.clearScratchRecordFieldsFrom(provides_top);
                return try self.pushMalformed(
                    AST.Header.Idx,
                    .expected_provides_close_curly,
                    provides_start,
                );
            },
            error.OutOfMemory => return error.OutOfMemory,
            error.TooNested => return error.TooNested,
        }
    };
    const provides_span = try self.store.recordFieldSpanFrom(provides_top);
    const provides = try self.store.addCollection(
        .collection_record_fields,
        .{
            .span = provides_span.span,
            .region = .{ .start = provides_start, .end = self.pos },
        },
    );

    return self.store.addHeader(.{ .platform = .{
        .name = name,
        .requires_rigids = rigids,
        .requires_signatures = signatures,
        .exposes = exposes,
        .packages = packages,
        .provides = provides,
        .region = .{ .start = start, .end = self.pos },
    } });
}

/// parse an `.roc` package header
///
/// e.g. `package [ foo ] { something: "package/path/main.roc" }`
pub fn parsePackageHeader(self: *Parser) Error!AST.Header.Idx {
    const trace = tracy.trace(@src());
    defer trace.end();

    const start = self.pos;

    std.debug.assert(self.peek() == .KwPackage);
    self.advance(); // Advance past KwApp

    // Get Exposes
    const exposes_start = self.pos;
    self.expect(.OpenSquare) catch {
        return try self.pushMalformed(AST.Header.Idx, .expected_provides_open_square, start);
    };
    const scratch_top = self.store.scratchExposedItemTop();
    self.parseCollectionSpan(AST.ExposedItem.Idx, .CloseSquare, NodeStore.addScratchExposedItem, Parser.parseExposedItem) catch |err| {
        switch (err) {
            error.ExpectedNotFound => {
                while (self.peek() != .CloseSquare and self.peek() != .EndOfFile) {
                    self.advance();
                }
                self.expect(.CloseSquare) catch {
                    return try self.pushMalformed(AST.Header.Idx, .header_expected_close_square, start);
                };
                self.store.clearScratchExposedItemsFrom(scratch_top);
                return try self.pushMalformed(AST.Header.Idx, .import_exposing_no_close, start);
            },
            error.OutOfMemory => return error.OutOfMemory,
            error.TooNested => return error.TooNested,
        }
    };
    const exposes_span = try self.store.exposedItemSpanFrom(scratch_top);
    const exposes = try self.store.addCollection(.collection_exposed, .{
        .span = exposes_span.span,
        .region = .{
            .start = exposes_start,
            .end = self.pos,
        },
    });

    // Get Packages
    const packages_start = self.pos;
    self.expect(.OpenCurly) catch {
        return try self.pushMalformed(AST.Header.Idx, .expected_package_platform_open_curly, start);
    };
    const fields_scratch_top = self.store.scratchRecordFieldTop();
    self.parseCollectionSpan(AST.RecordField.Idx, .CloseCurly, NodeStore.addScratchRecordField, Parser.parseRecordField) catch |err| {
        switch (err) {
            error.ExpectedNotFound => {
                self.store.clearScratchRecordFieldsFrom(fields_scratch_top);
                return try self.pushMalformed(AST.Header.Idx, .expected_package_platform_close_curly, start);
            },
            error.OutOfMemory => return error.OutOfMemory,
            error.TooNested => return error.TooNested,
        }
    };
    const packages_span = try self.store.recordFieldSpanFrom(fields_scratch_top);
    const packages = try self.store.addCollection(.collection_packages, .{
        .span = packages_span.span,
        .region = .{
            .start = packages_start,
            .end = self.pos,
        },
    });

    const header = AST.Header{ .package = .{
        .exposes = exposes,
        .packages = packages,
        .region = .{ .start = start, .end = self.pos },
    } };
    const idx = try self.store.addHeader(header);
    return idx;
}

/// Parse a Roc Hosted header
///
/// e.g. `hosted [foo]`
fn parseHostedHeader(self: *Parser) Error!AST.Header.Idx {
    const trace = tracy.trace(@src());
    defer trace.end();

    std.debug.assert(self.peek() == .KwHosted);

    const start = self.pos;

    self.advance(); // Advance past KwModule

    // Get exposes
    const exposes_start = self.pos;
    self.expect(.OpenSquare) catch {
        return try self.pushMalformed(AST.Header.Idx, .header_expected_open_square, self.pos);
    };
    const scratch_top = self.store.scratchExposedItemTop();
    self.parseCollectionSpan(AST.ExposedItem.Idx, .CloseSquare, NodeStore.addScratchExposedItem, Parser.parseExposedItem) catch |err| {
        switch (err) {
            error.ExpectedNotFound => {
                while (self.peek() != .CloseSquare and self.peek() != .EndOfFile) {
                    self.advance();
                }
                self.expect(.CloseSquare) catch {
                    return try self.pushMalformed(AST.Header.Idx, .header_expected_close_square, self.pos);
                };
                self.store.clearScratchExposedItemsFrom(scratch_top);
                return try self.pushMalformed(AST.Header.Idx, .import_exposing_no_close, self.pos);
            },
            error.OutOfMemory => return error.OutOfMemory,
            error.TooNested => return error.TooNested,
        }
    };
    const exposes_span = try self.store.exposedItemSpanFrom(scratch_top);
    const exposes = try self.store.addCollection(.collection_exposed, .{
        .span = exposes_span.span,
        .region = .{
            .start = exposes_start,
            .end = self.pos,
        },
    });

    return self.store.addHeader(.{ .hosted = .{
        .region = .{ .start = start, .end = self.pos },
        .exposes = exposes,
    } });
}

/// parse a Roc module header
///
/// e.g. `module [foo]`
fn parseModuleHeader(self: *Parser) Error!AST.Header.Idx {
    const trace = tracy.trace(@src());
    defer trace.end();

    std.debug.assert(self.peek() == .KwModule);

    const start = self.pos;

    self.advance(); // Advance past KwModule

    // Get exposes
    const exposes_start = self.pos;
    self.expect(.OpenSquare) catch {
        return try self.pushMalformed(AST.Header.Idx, .header_expected_open_square, self.pos);
    };
    const scratch_top = self.store.scratchExposedItemTop();
    self.parseCollectionSpan(AST.ExposedItem.Idx, .CloseSquare, NodeStore.addScratchExposedItem, Parser.parseExposedItem) catch |err| {
        switch (err) {
            error.ExpectedNotFound => {
                while (self.peek() != .CloseSquare and self.peek() != .EndOfFile) {
                    self.advance();
                }
                self.expect(.CloseSquare) catch {
                    return try self.pushMalformed(AST.Header.Idx, .header_expected_close_square, self.pos);
                };
                self.store.clearScratchExposedItemsFrom(scratch_top);
                return try self.pushMalformed(AST.Header.Idx, .import_exposing_no_close, self.pos);
            },
            error.OutOfMemory => return error.OutOfMemory,
            error.TooNested => return error.TooNested,
        }
    };
    const exposes_span = try self.store.exposedItemSpanFrom(scratch_top);
    const exposes = try self.store.addCollection(.collection_exposed, .{
        .span = exposes_span.span,
        .region = .{
            .start = exposes_start,
            .end = self.pos,
        },
    });

    return self.store.addHeader(.{ .module = .{
        .region = .{ .start = start, .end = self.pos },
        .exposes = exposes,
    } });
}

/// parse an `.roc` application header
///
/// e.g. `app [main!] { pf: "../some-platform.roc" }`
pub fn parseAppHeader(self: *Parser) Error!AST.Header.Idx {
    const trace = tracy.trace(@src());
    defer trace.end();

    var platform: ?AST.RecordField.Idx = null;
    const start = self.pos;

    std.debug.assert(self.peek() == .KwApp);
    self.advance(); // Advance past KwApp

    // Get provides
    const provides_start = self.pos;
    self.expect(.OpenSquare) catch {
        return try self.pushMalformed(AST.Header.Idx, .expected_provides_open_square, start);
    };
    const scratch_top = self.store.scratchExposedItemTop();
    self.parseCollectionSpan(AST.ExposedItem.Idx, .CloseSquare, NodeStore.addScratchExposedItem, Parser.parseExposedItem) catch |err| {
        switch (err) {
            error.ExpectedNotFound => {
                while (self.peek() != .CloseSquare and self.peek() != .EndOfFile) {
                    self.advance();
                }
                self.expect(.CloseSquare) catch {
                    return try self.pushMalformed(AST.Header.Idx, .header_expected_close_square, start);
                };
                self.store.clearScratchExposedItemsFrom(scratch_top);
                return try self.pushMalformed(AST.Header.Idx, .import_exposing_no_close, start);
            },
            error.OutOfMemory => return error.OutOfMemory,
            error.TooNested => return error.TooNested,
        }
    };
    const provides_span = try self.store.exposedItemSpanFrom(scratch_top);
    const provides_region = AST.TokenizedRegion{ .start = provides_start, .end = self.pos };
    const provides = try self.store.addCollection(.collection_exposed, AST.Collection{
        .span = provides_span.span,
        .region = provides_region,
    });

    // Get platform and packages
    const fields_scratch_top = self.store.scratchRecordFieldTop();
    const packages_start = self.pos;
    self.expect(.OpenCurly) catch {
        return try self.pushMalformed(AST.Header.Idx, .expected_package_platform_open_curly, start);
    };
    var i: usize = 0;

    while (self.peek() != .CloseCurly and self.peek() != .EndOfFile) {
        const entry_start = self.pos;
        if (self.peek() != .LowerIdent) {
            self.store.clearScratchRecordFieldsFrom(fields_scratch_top);
            return try self.pushMalformed(AST.Header.Idx, .expected_package_or_platform_name, start);
        }
        const name_tok = self.pos;
        self.advance();
        if (self.peek() != .OpColon) {
            self.store.clearScratchRecordFieldsFrom(fields_scratch_top);
            return try self.pushMalformed(AST.Header.Idx, .expected_package_or_platform_colon, start);
        }
        self.advance();
        if (self.peek() == .KwPlatform) {
            if (platform != null) {
                self.store.clearScratchRecordFieldsFrom(fields_scratch_top);
                return try self.pushMalformed(AST.Header.Idx, .multiple_platforms, start);
            }
            self.advance();
            if (self.peek() != .StringStart) {
                self.store.clearScratchRecordFieldsFrom(fields_scratch_top);
                return try self.pushMalformed(AST.Header.Idx, .expected_platform_string, start);
            }
            const value = try self.parseStringExpr();
            const pidx = try self.store.addRecordField(.{
                .name = name_tok,
                .value = value,
                .region = .{ .start = entry_start, .end = self.pos },
            });
            try self.store.addScratchRecordField(pidx);
            platform = pidx;
        } else {
            if (self.peek() != .StringStart) {
                self.store.clearScratchRecordFieldsFrom(fields_scratch_top);
                return try self.pushMalformed(AST.Header.Idx, .expected_package_or_platform_string, start);
            }
            const value = try self.parseStringExpr();
            try self.store.addScratchRecordField(try self.store.addRecordField(.{
                .name = name_tok,
                .value = value,
                .region = .{ .start = entry_start, .end = self.pos },
            }));
        }
        self.expect(.Comma) catch {
            break;
        };
        i = i + 1;
    }
    if (self.peek() != .CloseCurly) {
        self.store.clearScratchRecordFieldsFrom(fields_scratch_top);
        return try self.pushMalformed(AST.Header.Idx, .expected_package_platform_close_curly, start);
    }
    self.advance(); // Advance past CloseCurly
    const packages_span = try self.store.recordFieldSpanFrom(fields_scratch_top);
    const packages = try self.store.addCollection(.collection_packages, .{
        .span = packages_span.span,
        .region = .{
            .start = packages_start,
            .end = self.pos,
        },
    });

    if (platform) |pidx| {
        const header = AST.Header{
            .app = .{
                .platform_idx = pidx,
                .provides = provides,
                .packages = packages,
                .region = .{ .start = start, .end = self.pos },
            },
        };
        const idx = try self.store.addHeader(header);
        return idx;
    }
    return try self.pushMalformed(AST.Header.Idx, .no_platform, start);
}

/// Parses an ExposedItem, adding it to the NodeStore and returning the Idx
pub fn parseExposedItem(self: *Parser) Error!AST.ExposedItem.Idx {
    const trace = tracy.trace(@src());
    defer trace.end();

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
                    return try self.pushMalformed(AST.ExposedItem.Idx, .expected_lower_name_after_exposed_item_as, start);
                };
                end = self.pos;
            } else {
                self.advance(); // Advance past LowerIdent
            }
            const ei = try self.store.addExposedItem(.{ .lower_ident = .{
                .region = .{ .start = start, .end = self.pos },
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
                    return try self.pushMalformed(AST.ExposedItem.Idx, .expected_upper_name_after_exposed_item_as, start);
                };
                end = self.pos;
            } else if (self.peekNext() == .DotStar) {
                self.advance(); // Advance past UpperIdent
                self.advance(); // Advance past DotStar
                return try self.store.addExposedItem(.{ .upper_ident_star = .{
                    .region = .{ .start = start, .end = self.pos },
                    .ident = start,
                } });
            } else {
                self.advance(); // Advance past UpperIdent
            }
            const ei = try self.store.addExposedItem(.{ .upper_ident = .{
                .region = .{ .start = start, .end = self.pos },
                .ident = start,
                .as = as,
            } });

            return ei;
        },
        else => {
            return try self.pushMalformed(AST.ExposedItem.Idx, .exposed_item_unexpected_token, start);
        },
    }
}

const StatementType = enum { top_level, in_body };

/// Parse a top level roc statement
///
/// e.g. `import Foo`
pub fn parseTopLevelStatement(self: *Parser) Error!AST.Statement.Idx {
    const trace = tracy.trace(@src());
    defer trace.end();

    return try self.parseStmtByType(.top_level);
}

/// parse a in-body roc statement
///
/// e.g. `foo = 2 + x`
pub fn parseStmt(self: *Parser) Error!AST.Statement.Idx {
    const trace = tracy.trace(@src());
    defer trace.end();

    return try self.parseStmtByType(.in_body);
}

/// parse a roc statement
///
/// e.g. `import Foo`, or `foo = 2 + x`
fn parseStmtByType(self: *Parser, statementType: StatementType) Error!AST.Statement.Idx {
    const trace = tracy.trace(@src());
    defer trace.end();

    try self.nest();
    defer self.unnest();

    switch (self.peek()) {
        .KwImport => {
            if (statementType != .top_level) {
                return try self.pushMalformed(AST.Statement.Idx, .import_must_be_top_level, self.pos);
            }
            const start = self.pos;
            self.advance(); // Advance past KwImport
            var qualifier: ?TokenIdx = null;
            var alias_tok: ?TokenIdx = null;
            if (self.peek() == .LowerIdent) {
                qualifier = self.pos;
                self.advance(); // Advance past LowerIdent
            }
            if ((qualifier == null and self.peek() == .UpperIdent) or (qualifier != null and (self.peek() == .NoSpaceDotUpperIdent or self.peek() == .DotUpperIdent))) {
                var exposes = AST.ExposedItem.Span{ .span = base.DataSpan.empty() };
                const module_name_tok = self.pos;
                // Handle 'as' clause if present
                if (self.peekNext() == .KwAs) {
                    self.advance(); // Advance past UpperIdent
                    self.advance(); // Advance past KwAs
                    alias_tok = self.pos;
                    self.expect(.UpperIdent) catch {
                        const malformed = try self.pushMalformed(AST.Statement.Idx, .expected_upper_name_after_import_as, start);
                        return malformed;
                    };
                } else {
                    self.advance(); // Advance past identifier
                }

                // Handle 'exposing' clause if present (can occur with or without 'as')
                if (self.peek() == .KwExposing) {
                    self.advance(); // Advance past KwExposing
                    self.expect(.OpenSquare) catch {
                        return try self.pushMalformed(AST.Statement.Idx, .import_exposing_no_open, start);
                    };
                    const scratch_top = self.store.scratchExposedItemTop();
                    self.parseCollectionSpan(AST.ExposedItem.Idx, .CloseSquare, NodeStore.addScratchExposedItem, Parser.parseExposedItem) catch |err| {
                        switch (err) {
                            error.ExpectedNotFound => {
                                while (self.peek() != .CloseSquare and self.peek() != .EndOfFile) {
                                    self.advance();
                                }
                                self.expect(.CloseSquare) catch {};
                                self.store.clearScratchExposedItemsFrom(scratch_top);
                                return try self.pushMalformed(AST.Statement.Idx, .import_exposing_no_close, start);
                            },
                            error.OutOfMemory => return error.OutOfMemory,
                            error.TooNested => return error.TooNested,
                        }
                    };
                    exposes = try self.store.exposedItemSpanFrom(scratch_top);
                }
                const statement_idx = try self.store.addStatement(.{ .import = .{
                    .module_name_tok = module_name_tok,
                    .qualifier_tok = qualifier,
                    .alias_tok = alias_tok,
                    .exposes = exposes,
                    .region = .{ .start = start, .end = self.pos },
                } });
                return statement_idx;
            }
            return try self.pushMalformed(AST.Statement.Idx, .incomplete_import, start);
        },
        .KwExpect => {
            const start = self.pos;
            self.advance();
            const body = try self.parseExpr();
            const statement_idx = try self.store.addStatement(.{ .expect = .{
                .body = body,
                .region = .{ .start = start, .end = self.pos },
            } });
            return statement_idx;
        },
        .KwFor => {
            const start = self.pos;
            self.advance();
            const patt = try self.parsePattern(.alternatives_forbidden);
            self.expect(.KwIn) catch {
                return try self.pushMalformed(AST.Statement.Idx, .for_expected_in, self.pos);
            };
            const expr = try self.parseExpr();
            const body = try self.parseExpr();
            const statement_idx = try self.store.addStatement(.{ .@"for" = .{
                .region = .{ .start = start, .end = self.pos },
                .patt = patt,
                .expr = expr,
                .body = body,
            } });

            return statement_idx;
        },
        .KwCrash => {
            const start = self.pos;
            self.advance();
            const expr = try self.parseExpr();
            const statement_idx = try self.store.addStatement(.{ .crash = .{
                .expr = expr,
                .region = .{ .start = start, .end = self.pos },
            } });
            return statement_idx;
        },
        .KwDbg => {
            const start = self.pos;
            self.advance();
            const expr = try self.parseExpr();
            const statement_idx = try self.store.addStatement(.{ .dbg = .{
                .expr = expr,
                .region = .{ .start = start, .end = self.pos },
            } });
            return statement_idx;
        },
        .KwReturn => {
            const start = self.pos;
            self.advance();
            const expr = try self.parseExpr();
            const statement_idx = try self.store.addStatement(.{ .@"return" = .{
                .expr = expr,
                .region = .{ .start = start, .end = self.pos },
            } });
            return statement_idx;
        },
        .KwVar => {
            const start = self.pos;
            if (statementType != .in_body) {
                return try self.pushMalformed(AST.Statement.Idx, .var_only_allowed_in_a_body, self.pos);
            }
            self.advance();
            if (self.peek() != .LowerIdent) {
                return try self.pushMalformed(AST.Statement.Idx, .var_must_have_ident, self.pos);
            }
            const name = self.pos;
            self.advance();
            self.expect(.OpAssign) catch {
                return try self.pushMalformed(AST.Statement.Idx, .var_expected_equals, self.pos);
            };
            const body = try self.parseExpr();
            const statement_idx = try self.store.addStatement(.{ .@"var" = .{
                .name = name,
                .body = body,
                .region = .{ .start = start, .end = self.pos },
            } });
            return statement_idx;
        },
        .LowerIdent => {
            const start = self.pos;
            if (self.peekNext() == .OpAssign) {
                self.advance(); // Advance past LowerIdent
                const patt_idx = try self.store.addPattern(.{ .ident = .{
                    .ident_tok = start,
                    .region = .{ .start = start, .end = self.pos },
                } });
                self.advance(); // Advance past OpAssign
                const idx = try self.parseExpr();
                const statement_idx = try self.store.addStatement(.{ .decl = .{
                    .pattern = patt_idx,
                    .body = idx,
                    .region = .{ .start = start, .end = self.pos },
                } });
                return statement_idx;
            } else if (self.peekNext() == .OpColon) {
                self.advance(); // Advance past LowerIdent
                self.advance(); // Advance past OpColon
                const anno = try self.parseTypeAnno(.not_looking_for_args);
                const statement_idx = try self.store.addStatement(.{ .type_anno = .{
                    .anno = anno,
                    .name = start,
                    .where = try self.parseWhereConstraint(),
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
                const patt_idx = try self.store.addPattern(.{ .ident = .{
                    .ident_tok = start,
                    .region = .{ .start = start, .end = self.pos },
                } });
                self.advance(); // Advance past OpAssign
                const idx = try self.parseExpr();
                const statement_idx = try self.store.addStatement(.{ .decl = .{
                    .pattern = patt_idx,
                    .body = idx,
                    .region = .{ .start = start, .end = self.pos },
                } });
                return statement_idx;
            } else if (self.peekNext() == .OpColon) {
                self.advance(); // Advance past NamedUnderscore
                self.advance(); // Advance past OpColon
                const anno = try self.parseTypeAnno(.not_looking_for_args);
                const statement_idx = try self.store.addStatement(.{ .type_anno = .{
                    .anno = anno,
                    .name = start,
                    .where = try self.parseWhereConstraint(),
                    .region = .{ .start = start, .end = self.pos },
                } });
                return statement_idx;
            } else {
                // continue to parse final expression
            }
        },
        // Type Annotation (e.g. `Foo a : (a,a)`)
        .UpperIdent => {
            const start = self.pos;
            if (statementType == .top_level) {
                const header = try self.parseTypeHeader();
                if (self.peek() != .OpColon and self.peek() != .OpColonEqual) {
                    // Point to the unexpected token (e.g., "U8" in "List U8")
                    return try self.pushMalformed(AST.Statement.Idx, .expected_colon_after_type_annotation, self.pos);
                }
                const kind: AST.TypeDeclKind = if (self.peek() == .OpColonEqual) .nominal else .alias;
                self.advance();
                const anno = try self.parseTypeAnno(.not_looking_for_args);
                const where_clause = try self.parseWhereConstraint();

                // Check if there's a .{ associated } after the type annotation
                var associated: ?AST.Associated = null;
                if (self.peek() == .Dot and self.peekN(1) == .OpenCurly) {
                    const dot_pos = self.pos;
                    // Parse the associated items block
                    self.advance(); // consume .
                    self.advance(); // consume {
                    const associated_start = self.pos - 1;
                    associated = try self.parseStatementOnlyBlock(associated_start);

                    // Report error if this is a type alias
                    if (kind == .alias) {
                        try self.pushDiagnostic(.type_alias_cannot_have_associated, .{
                            .start = dot_pos,
                            .end = dot_pos + 1,
                        });
                    }
                }

                const statement_idx = try self.store.addStatement(.{ .type_decl = .{
                    .header = header,
                    .anno = anno,
                    .kind = kind,
                    .where = where_clause,
                    .associated = associated,
                    .region = .{ .start = start, .end = self.pos },
                } });
                return statement_idx;
            }
        },
        .OpenCurly, .OpenRound => {
            const isCurly = self.peek() == .OpenCurly;

            const start = self.pos;

            // Look for a similarly nested close token
            var is_destructure = false;
            var lookahead_pos = self.pos + 1;
            var depth: u32 = 0;
            while (lookahead_pos < self.tok_buf.tokens.len) {
                const tok = self.tok_buf.tokens.items(.tag)[lookahead_pos];
                if ((isCurly and tok == .OpenCurly) or (!isCurly and (tok == .OpenRound or tok == .NoSpaceOpenRound))) {
                    depth += 1;
                } else if ((isCurly and tok == .CloseCurly) or (!isCurly and tok == .CloseRound)) {
                    if (depth == 0) {
                        const token_after_close_curly = self.tok_buf.tokens.items(.tag)[lookahead_pos + 1];
                        if (token_after_close_curly == .OpAssign) {
                            is_destructure = true;
                        }
                        break;
                    }
                    depth -= 1;
                } else if (tok == .EndOfFile) {
                    break;
                } else {
                    // Ignore other tokens
                }
                lookahead_pos += 1;
            }

            // Restore parser position after lookahead
            self.pos = start;

            if (is_destructure) {
                const patt_idx = try self.parsePatternNoAlts();

                self.expect(.OpAssign) catch {
                    return try self.pushMalformed(AST.Statement.Idx, .statement_unexpected_token, self.pos);
                };

                const idx = try self.parseExpr();
                const statement_idx = try self.store.addStatement(.{ .decl = .{
                    .pattern = patt_idx,
                    .body = idx,
                    .region = .{ .start = start, .end = self.pos },
                } });
                return statement_idx;
            }
        },
        else => {},
    }

    if (statementType == .top_level) {
        // Check if this might be a multi-arrow type pattern
        if (self.peek() == .OpArrow or self.peek() == .OpFatArrow) {
            // Look back to see if the previous statement was a type annotation
            // If so, this is likely an attempt to write "a -> b -> c" style
            return try self.pushMalformed(AST.Statement.Idx, .multi_arrow_needs_parens, self.pos);
        }
        return try self.pushMalformed(AST.Statement.Idx, .statement_unexpected_token, self.pos);
    }

    // We didn't find any statements, so we must be parsing the final expression.
    const start = self.pos;
    const expr = try self.parseExpr();
    const statement_idx = try self.store.addStatement(.{ .expr = .{
        .expr = expr,
        .region = .{ .start = start, .end = self.pos },
    } });
    return statement_idx;
}

fn parseWhereConstraint(self: *Parser) Error!?AST.Collection.Idx {
    const trace = tracy.trace(@src());
    defer trace.end();

    const where_start = self.pos; // Position of the where keyword

    self.expect(.KwWhere) catch {
        return null;
    };

    const where_end = self.pos; // Position right after the where keyword
    const where_clauses_top = self.store.scratchWhereClauseTop();
    while (self.peek() != .Comma and self.peek() != .EndOfFile) {
        const curr = self.peek();
        const next = self.peekNext();

        // Check if we've reached the start of a new statement (e.g., "broken_fn2 : a -> b" or "process = ...")
        // This indicates we should stop parsing where clauses
        if (curr == .LowerIdent and (next == .OpColon or next == .OpAssign)) {
            break;
        }

        const valid_lookahead = curr == .KwModule or (curr == .LowerIdent and (next == .NoSpaceDotLowerIdent or next == .DotLowerIdent or next == .NoSpaceDotUpperIdent or next == .DotUpperIdent));
        if (!valid_lookahead) {
            // Check if we have a malformed constraint that starts with a type variable
            if (curr == .LowerIdent) {
                // Try to parse as a malformed where clause to provide better error messages
                const clause = try self.parseWhereClause();
                try self.store.addScratchWhereClause(clause);
                if (self.peek() != .Comma) {
                    break;
                }
                self.advance();
                continue;
            }
            break;
        }
        const clause = try self.parseWhereClause();
        try self.store.addScratchWhereClause(clause);
        if (self.peek() != .Comma) {
            break;
        }
        self.advance();
    }
    const where_clauses = try self.store.whereClauseSpanFrom(where_clauses_top);

    // Check if the where clause is empty
    if (where_clauses.span.len == 0) {
        // Create diagnostic region pointing to the where keyword
        const diagnostic_region = AST.TokenizedRegion{ .start = where_start, .end = where_end };

        // Create AST region for the malformed node
        const ast_region = AST.TokenizedRegion{ .start = where_start, .end = where_end };

        // Add the diagnostic
        try self.diagnostics.append(self.gpa, .{
            .tag = .where_expected_constraints,
            .region = diagnostic_region,
        });

        // Create the malformed where clause node
        const malformed_clause = try self.store.addMalformed(AST.WhereClause.Idx, .where_expected_constraints, ast_region);

        try self.store.addScratchWhereClause(malformed_clause);
        const updated_where_clauses = try self.store.whereClauseSpanFrom(where_clauses_top);
        const coll_id = try self.store.addCollection(.collection_where_clause, .{
            .region = .{ .start = where_start, .end = where_end },
            .span = updated_where_clauses.span,
        });
        return coll_id;
    }

    const coll_id = try self.store.addCollection(.collection_where_clause, .{
        .region = .{ .start = where_start, .end = self.pos },
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
pub fn parsePattern(self: *Parser, alternatives: Alternatives) Error!AST.Pattern.Idx {
    const trace = tracy.trace(@src());
    defer trace.end();

    try self.nest();
    defer self.unnest();

    const outer_start = self.pos;
    const patterns_scratch_top = self.store.scratchPatternTop();
    errdefer self.store.clearScratchPatternsFrom(patterns_scratch_top);
    while (self.peek() != .EndOfFile) {
        const start = self.pos;
        var pattern: ?AST.Pattern.Idx = null;
        switch (self.peek()) {
            .LowerIdent => {
                self.advance();
                pattern = try self.store.addPattern(.{ .ident = .{
                    .ident_tok = start,
                    .region = .{ .start = start, .end = self.pos },
                } });
            },
            .NamedUnderscore => {
                self.advance();
                pattern = try self.store.addPattern(.{ .ident = .{
                    .ident_tok = start,
                    .region = .{ .start = start, .end = self.pos },
                } });
            },
            .UpperIdent => {
                const qual_result = try self.parseQualificationChain();
                // Use final token as end position to avoid newline tokens
                self.pos = qual_result.final_token + 1;

                if (qual_result.is_upper) {
                    // This is a qualified or unqualified tag
                    if (self.peek() != .NoSpaceOpenRound) {
                        // Tag without args
                        pattern = try self.store.addPattern(.{ .tag = .{
                            .region = .{ .start = start, .end = self.pos },
                            .args = .{ .span = .{
                                .start = 0,
                                .len = 0,
                            } },
                            .tag_tok = qual_result.final_token,
                            .qualifiers = qual_result.qualifiers,
                        } });
                    } else {
                        // Tag with args
                        self.advance(); // Advance past NoSpaceOpenRound
                        const scratch_top = self.store.scratchPatternTop();
                        self.parseCollectionSpan(AST.Pattern.Idx, .CloseRound, NodeStore.addScratchPattern, parsePatternWithAlts) catch |err| {
                            switch (err) {
                                error.ExpectedNotFound => {
                                    while (self.peek() != .CloseRound and self.peek() != .EndOfFile) {
                                        self.advance();
                                    }
                                    self.store.clearScratchPatternsFrom(scratch_top);
                                    return try self.pushMalformed(AST.Pattern.Idx, .pattern_unexpected_token, start);
                                },
                                error.OutOfMemory => return error.OutOfMemory,
                                error.TooNested => return error.TooNested,
                            }
                        };
                        const args = try self.store.patternSpanFrom(scratch_top);
                        pattern = try self.store.addPattern(.{ .tag = .{
                            .region = .{ .start = start, .end = self.pos },
                            .args = args,
                            .tag_tok = qual_result.final_token,
                            .qualifiers = qual_result.qualifiers,
                        } });
                    }
                } else {
                    // This is a qualified lowercase ident, which shouldn't happen in patterns
                    return try self.pushMalformed(AST.Pattern.Idx, .pattern_unexpected_token, start);
                }
            },
            .StringStart => {
                pattern = try self.parseStringPattern();
            },
            .SingleQuote => {
                self.advance();
                pattern = try self.store.addPattern(.{ .single_quote = .{
                    .token = start,
                    .region = .{ .start = start, .end = self.pos },
                } });
            },
            .Int => {
                self.advance();
                pattern = try self.store.addPattern(.{ .int = .{
                    .region = .{ .start = start, .end = self.pos },
                    .number_tok = start,
                } });
            },
            .Float => {
                self.advance();
                pattern = try self.store.addPattern(.{ .frac = .{
                    .region = .{ .start = start, .end = self.pos },
                    .number_tok = start,
                } });
            },
            .OpenSquare => {
                // List - custom parsing to handle DoubleDot rest patterns
                self.advance();
                const scratch_top = self.store.scratchPatternTop();

                // Parse list elements with support for rest patterns
                while (self.peek() != .CloseSquare and self.peek() != .EndOfFile) {
                    if (self.peek() == .DoubleDot) {
                        // Handle rest pattern .. as name or .. (unnamed)
                        const rest_start = self.pos;
                        self.advance(); // consume DoubleDot

                        var rest_name: ?Token.Idx = null;
                        if (self.peek() == .KwAs) {
                            self.advance(); // consume KwAs
                            if (self.peek() == .LowerIdent) {
                                rest_name = self.pos;
                                self.advance();
                            }
                        } else if (self.peek() == .LowerIdent) {
                            // Old syntax ..name - add diagnostic but continue parsing
                            rest_name = self.pos;
                            self.advance();

                            // Add diagnostic for old syntax
                            try self.pushDiagnostic(.pattern_list_rest_old_syntax, .{
                                .start = rest_start,
                                .end = self.pos,
                            });
                        }

                        const rest_pattern = try self.store.addPattern(.{ .list_rest = .{
                            .name = rest_name,
                            .region = .{ .start = rest_start, .end = self.pos },
                        } });

                        try self.store.addScratchPattern(rest_pattern);
                    } else {
                        // Regular pattern
                        const pat = try self.parsePattern(.alternatives_allowed);
                        try self.store.addScratchPattern(pat);
                    }

                    // Handle comma or end of list
                    if (self.peek() == .Comma) {
                        self.advance();
                    } else {
                        break;
                    }
                }

                if (self.peek() == .CloseSquare) {
                    self.advance();
                } else {
                    // List not properly closed, but continue with "inform don't block"
                    self.store.clearScratchPatternsFrom(scratch_top);
                    return try self.pushMalformed(AST.Pattern.Idx, .pattern_unexpected_token, start);
                }

                const patterns = try self.store.patternSpanFrom(scratch_top);

                pattern = try self.store.addPattern(.{ .list = .{
                    .region = .{ .start = start, .end = self.pos },
                    .patterns = patterns,
                } });
            },
            .OpenCurly => {
                self.advance();
                const scratch_top = self.store.scratchPatternRecordFieldTop();
                while (self.peek() != .CloseCurly) {
                    try self.store.addScratchPatternRecordField(try self.parsePatternRecordField(alternatives));
                    if (self.peek() != .Comma) {
                        break;
                    }
                    self.advance();
                }
                const fields = try self.store.patternRecordFieldSpanFrom(scratch_top);
                if (self.peek() != .CloseCurly) {
                    return try self.pushMalformed(AST.Pattern.Idx, .pattern_unexpected_token, start);
                }
                self.advance();
                pattern = try self.store.addPattern(.{ .record = .{
                    .region = .{ .start = start, .end = self.pos },
                    .fields = fields,
                } });
            },
            .DoubleDot => {
                var name: ?Token.Idx = null;
                var end: u32 = self.pos;
                self.advance();
                if (self.peek() == .KwAs) {
                    self.advance();
                    if (self.peek() != .LowerIdent) {
                        return try self.pushMalformed(AST.Pattern.Idx, .pattern_unexpected_token, start);
                    }
                    name = self.pos;
                    end = self.pos;
                    self.advance();
                } else if (self.peek() == .LowerIdent) {
                    // Old syntax ..name - create malformed pattern with helpful diagnostic
                    return try self.pushMalformed(AST.Pattern.Idx, .pattern_list_rest_old_syntax, self.pos);
                }
                pattern = try self.store.addPattern(.{ .list_rest = .{
                    .region = .{ .start = start, .end = self.pos },
                    .name = name,
                } });
            },
            .Underscore => {
                self.advance();
                pattern = try self.store.addPattern(.{ .underscore = .{
                    .region = .{ .start = start, .end = self.pos },
                } });
            },
            .OpenRound, .NoSpaceOpenRound => {
                self.advance();
                const scratch_top = self.store.scratchPatternTop();
                self.parseCollectionSpan(AST.Pattern.Idx, .CloseRound, NodeStore.addScratchPattern, parsePatternWithAlts) catch |err| {
                    switch (err) {
                        error.ExpectedNotFound => {
                            while (self.peek() != .CloseRound and self.peek() != .EndOfFile) {
                                self.advance();
                            }
                            self.store.clearScratchPatternsFrom(scratch_top);
                            return try self.pushMalformed(AST.Pattern.Idx, .pattern_unexpected_token, start);
                        },
                        error.OutOfMemory => return error.OutOfMemory,
                        error.TooNested => return error.TooNested,
                    }
                };
                const patterns = try self.store.patternSpanFrom(scratch_top);

                pattern = try self.store.addPattern(.{ .tuple = .{
                    .patterns = patterns,
                    .region = .{ .start = start, .end = self.pos },
                } });
            },
            else => {
                return try self.pushMalformed(AST.Pattern.Idx, .pattern_unexpected_token, self.pos);
            },
        }

        if (pattern) |p| {
            if (alternatives == .alternatives_forbidden) {
                return try self.parseAsPattern(p);
            }
            if (self.peek() != .OpBar) {
                if ((self.store.scratchPatternTop() - patterns_scratch_top) == 0) {
                    return try self.parseAsPattern(p);
                }
                try self.store.addScratchPattern(p);
                break;
            }
            try self.store.addScratchPattern(p);
            self.advance();
        }
    }
    const pattern_count = self.store.scratchPatternTop() - patterns_scratch_top;
    if (pattern_count == 0) {
        return try self.store.addMalformed(AST.Pattern.Idx, .pattern_unexpected_eof, .{ .start = outer_start, .end = self.pos });
    }
    if (pattern_count == 1) {
        // Only one pattern, return it directly instead of wrapping in alternatives
        const single_pattern = self.store.scratch_patterns.items.items[self.store.scratchPatternTop() - 1];
        self.store.clearScratchPatternsFrom(patterns_scratch_top);
        return try self.parseAsPattern(single_pattern);
    }
    const patterns = try self.store.patternSpanFrom(patterns_scratch_top);
    return try self.store.addPattern(.{ .alternatives = .{
        .region = .{ .start = outer_start, .end = self.pos },
        .patterns = patterns,
    } });
}

fn parseAsPattern(self: *Parser, pattern: AST.Pattern.Idx) Error!AST.Pattern.Idx {
    const trace = tracy.trace(@src());
    defer trace.end();

    if (self.peek() != .KwAs) {
        return pattern;
    }
    self.advance(); // Advance past KwAs
    if (self.peek() != .LowerIdent) {
        // The name of a pattern can only be a lower ident
        return try self.pushMalformed(AST.Pattern.Idx, .bad_as_pattern_name, self.pos);
    }
    const parent_region = self.store.getPattern(pattern).to_tokenized_region();
    const p = try self.store.addPattern(.{ .as = .{
        .name = self.pos,
        .pattern = pattern,
        .region = .{ .start = parent_region.start, .end = self.pos },
    } });
    self.advance(); // Advance past LowerIdent;
    return p;
}

fn parsePatternNoAlts(self: *Parser) Error!AST.Pattern.Idx {
    const trace = tracy.trace(@src());
    defer trace.end();

    return try self.parsePattern(.alternatives_forbidden);
}
fn parsePatternWithAlts(self: *Parser) Error!AST.Pattern.Idx {
    const trace = tracy.trace(@src());
    defer trace.end();

    return try self.parsePattern(.alternatives_allowed);
}

/// todo
pub fn parsePatternRecordField(self: *Parser, alternatives: Alternatives) Error!AST.PatternRecordField.Idx {
    const trace = tracy.trace(@src());
    defer trace.end();

    const field_start = self.pos;
    if (self.peek() == .DoubleDot) {
        self.advance();
        var name: u32 = 0;
        if (self.peek() == .LowerIdent) {
            name = self.pos;
            self.advance();
        }
        return try self.store.addPatternRecordField(.{
            .name = name,
            .value = null,
            .rest = true,
            .region = .{ .start = field_start, .end = self.pos },
        });
    }
    if (self.peek() != .LowerIdent) {
        while (self.peek() != .EndOfFile) {
            if (self.peek() == .CloseCurly) {
                break;
            }
            self.advance();
        }
        return try self.pushMalformed(AST.PatternRecordField.Idx, .expected_lower_ident_pat_field_name, field_start);
    }
    const name = self.pos;
    self.advance();
    var value: ?AST.Pattern.Idx = null;
    // With shorthand the next token is a Comma or the ending CloseCurly
    if (self.peek() != .Comma and self.peek() != .CloseCurly) {
        // Otherwise we should see an OpColon to introduce the value
        if (self.peek() != .OpColon) {
            while (self.peek() != .EndOfFile) {
                if (self.peek() == .CloseCurly) break;
                self.advance();
            }
            return try self.pushMalformed(AST.PatternRecordField.Idx, .expected_colon_after_pat_field_name, field_start);
        }
        self.advance();
        const patt = try self.parsePattern(alternatives);
        value = patt;
    }

    return try self.store.addPatternRecordField(.{
        .name = name,
        .value = value,
        .rest = false,
        .region = .{ .start = field_start, .end = self.pos },
    });
}

const QualificationResult = struct {
    qualifiers: Token.Span,
    final_token: Token.Idx,
    is_upper: bool,
};

/// Parses a qualification chain (e.g., "json.Core.Utf8" -> ["json", "Core"])
/// Returns the qualifiers and the final token
fn parseQualificationChain(self: *Parser) Error!QualificationResult {
    std.debug.assert(self.peek() == .UpperIdent or self.peek() == .LowerIdent);

    const scratch_top = self.store.scratchTokenTop();
    var final_token = self.pos; // Capture position of the identifier
    var is_upper = true;

    // Check if there's a qualification chain by looking ahead
    const saved_pos = self.pos;
    self.advance();

    if (self.peek() == .NoSpaceDotUpperIdent or self.peek() == .NoSpaceDotLowerIdent) {
        // There is a qualification chain, continue parsing
        while (self.peek() == .NoSpaceDotUpperIdent or self.peek() == .NoSpaceDotLowerIdent) {
            // Add the current token as a qualifier before moving to the next
            try self.store.addScratchToken(final_token);

            // Capture position of the dot-prefixed token
            final_token = self.pos;
            is_upper = (self.tok_buf.tokens.items(.tag)[final_token] == .NoSpaceDotUpperIdent);

            // Move past this token
            self.advance();
        }
    } else {
        // No qualification chain, restore position
        self.pos = saved_pos;
    }

    const qualifiers = try self.store.tokenSpanFrom(scratch_top);

    return QualificationResult{
        .qualifiers = qualifiers,
        .final_token = final_token,
        .is_upper = is_upper,
    };
}

/// todo
pub fn parseExpr(self: *Parser) Error!AST.Expr.Idx {
    const trace = tracy.trace(@src());
    defer trace.end();

    return try self.parseExprWithBp(0);
}

/// todo
pub fn parseExprWithBp(self: *Parser, min_bp: u8) Error!AST.Expr.Idx {
    const trace = tracy.trace(@src());
    defer trace.end();
    try self.nest();
    defer self.unnest();

    const start = self.pos;

    var expr: ?AST.Expr.Idx = null;
    const token = self.peek();
    switch (token) {
        .UpperIdent => {
            const qual_result = try self.parseQualificationChain();
            // Use final token as end position to avoid newline tokens
            self.pos = qual_result.final_token + 1;

            if (qual_result.is_upper) {
                // This is a qualified or unqualified tag
                const tag = try self.store.addExpr(.{ .tag = .{
                    .token = qual_result.final_token,
                    .qualifiers = qual_result.qualifiers,
                    .region = .{ .start = start, .end = self.pos },
                } });
                expr = tag;
            } else {
                // This is a qualified lowercase ident
                const ident = try self.store.addExpr(.{ .ident = .{
                    .token = qual_result.final_token,
                    .qualifiers = qual_result.qualifiers,
                    .region = .{ .start = start, .end = self.pos },
                } });
                expr = ident;
            }
        },
        .LowerIdent => {
            self.advance();
            const empty_qualifiers = try self.store.tokenSpanFrom(self.store.scratchTokenTop());
            const ident = try self.store.addExpr(.{ .ident = .{
                .token = start,
                .qualifiers = empty_qualifiers,
                .region = .{ .start = start, .end = self.pos },
            } });

            expr = ident;
        },
        .NamedUnderscore => {
            self.advance();
            const empty_qualifiers = try self.store.tokenSpanFrom(self.store.scratchTokenTop());
            const ident = try self.store.addExpr(.{ .ident = .{
                .token = start,
                .qualifiers = empty_qualifiers,
                .region = .{ .start = start, .end = self.pos },
            } });

            expr = ident;
        },
        .Int => {
            self.advance();

            // Disallow dot suffixes after Int
            if (self.peek() == .NoSpaceDotInt or self.peek() == .NoSpaceDotLowerIdent or self.peek() == .DotLowerIdent) {
                return try self.pushMalformed(AST.Expr.Idx, .expr_dot_suffix_not_allowed, self.pos);
            }

            expr = try self.store.addExpr(.{ .int = .{
                .token = start,
                .region = .{ .start = start, .end = self.pos },
            } });
        },
        .Float => {
            self.advance();
            expr = try self.store.addExpr(.{ .frac = .{
                .token = start,
                .region = .{ .start = start, .end = self.pos },
            } });
        },
        .SingleQuote => {
            self.advance();
            expr = try self.store.addExpr(.{ .single_quote = .{
                .token = start,
                .region = .{ .start = start, .end = self.pos },
            } });
        },
        .StringStart => {
            expr = try self.parseStringExpr();
        },
        .MultilineStringStart => {
            expr = try self.parseMultiLineStringExpr();
        },
        .OpenSquare => {
            self.advance();
            const scratch_top = self.store.scratchExprTop();
            self.parseCollectionSpan(AST.Expr.Idx, .CloseSquare, NodeStore.addScratchExpr, parseExpr) catch |err| {
                switch (err) {
                    error.ExpectedNotFound => {
                        while (self.peek() != .CloseSquare and self.peek() != .EndOfFile) {
                            self.advance();
                        }
                        self.store.clearScratchExprsFrom(scratch_top);
                        return try self.pushMalformed(AST.Expr.Idx, .expected_expr_close_square_or_comma, self.pos);
                    },
                    error.OutOfMemory => return error.OutOfMemory,
                    error.TooNested => return error.TooNested,
                }
            };
            const items = try self.store.exprSpanFrom(scratch_top);
            expr = try self.store.addExpr(.{ .list = .{
                .items = items,
                .region = .{ .start = start, .end = self.pos },
            } });
        },
        .NoSpaceOpenRound, .OpenRound => {
            self.advance();
            // TODO: Parenthesized expressions
            const scratch_top = self.store.scratchExprTop();
            self.parseCollectionSpan(AST.Expr.Idx, .CloseRound, NodeStore.addScratchExpr, parseExpr) catch |err| {
                switch (err) {
                    error.ExpectedNotFound => {
                        while (self.peek() != .CloseRound and self.peek() != .EndOfFile) {
                            self.advance();
                        }
                        self.store.clearScratchExprsFrom(scratch_top);
                        return try self.pushMalformed(AST.Expr.Idx, .expected_expr_close_round_or_comma, self.pos);
                    },
                    error.OutOfMemory => return error.OutOfMemory,
                    error.TooNested => return error.TooNested,
                }
            };
            const items = try self.store.exprSpanFrom(scratch_top);
            expr = try self.store.addExpr(.{ .tuple = .{
                .items = items,
                .region = .{ .start = start, .end = self.pos },
            } });
        },
        .OpenCurly => {
            self.advance();

            if (self.peek() == .CloseCurly) {
                // Empty - treat as empty record
                expr = try self.parseRecord(start);
            } else if (self.peek() == .DoubleDot) {
                // Record is being extended

                self.advance(); // consume DoubleDot

                // Parse the extension
                const ext_expr = try self.parseExpr();

                // Expect comma after extension
                if (self.peek() != .Comma) {
                    return try self.pushMalformed(AST.Expr.Idx, .expected_expr_comma, self.pos);
                }
                self.advance(); // consume comma

                // Parse the remaining fields
                const scratch_top = self.store.scratchRecordFieldTop();
                self.parseCollectionSpan(AST.RecordField.Idx, .CloseCurly, NodeStore.addScratchRecordField, parseRecordField) catch |err| {
                    switch (err) {
                        error.ExpectedNotFound => {
                            self.store.clearScratchRecordFieldsFrom(scratch_top);
                            return try self.pushMalformed(AST.Expr.Idx, .expected_expr_close_curly_or_comma, self.pos);
                        },
                        error.OutOfMemory => return error.OutOfMemory,
                        error.TooNested => return error.TooNested,
                    }
                };
                const fields = try self.store.recordFieldSpanFrom(scratch_top);
                expr = try self.store.addExpr(.{ .record = .{
                    .fields = fields,
                    .ext = ext_expr,
                    .region = .{ .start = start, .end = self.pos },
                } });
            } else if (self.peek() == .LowerIdent and self.peekNext() == .Comma) {
                // Definitely a record - has comma-separated fields
                expr = try self.parseRecord(start);
            } else if (self.peek() == .LowerIdent and self.peekNext() == .OpColon) {
                // Ambiguous case: could be record field or type annotation
                // Use bounded lookahead to determine the context
                var is_block = false;

                // Save current position
                const saved_pos = self.pos;

                // Advance past LowerIdent : to see what follows
                var lookahead_pos = self.pos + 2;

                if (lookahead_pos < self.tok_buf.tokens.len) {
                    var depth: u32 = 0;

                    // Look ahead with proper bounds checking
                    while (lookahead_pos < self.tok_buf.tokens.len) {
                        const tok = self.tok_buf.tokens.items(.tag)[lookahead_pos];

                        switch (tok) {
                            .OpenRound, .NoSpaceOpenRound, .OpenSquare, .OpenCurly => depth += 1,
                            .CloseRound, .CloseSquare, .CloseCurly => {
                                if (depth == 0) break;
                                depth -= 1;
                            },
                            .LowerIdent => {
                                // At depth 0, check if this looks like an assignment (block) or record field
                                if (depth == 0) {
                                    const check_pos = lookahead_pos + 1;
                                    if (check_pos < self.tok_buf.tokens.len) {
                                        const after_ident = self.tok_buf.tokens.items(.tag)[check_pos];
                                        // If we see assignment, it's definitely a block
                                        if (after_ident == .OpAssign) {
                                            is_block = true;
                                            break;
                                        }
                                    }
                                }
                            },
                            .EndOfFile => break,
                            else => {
                                // Ignore other tokens
                            },
                        }
                        lookahead_pos += 1;

                        // Limit lookahead to prevent infinite loops
                        if (lookahead_pos > saved_pos + 100) break;
                    }
                }

                // Restore parser position after lookahead
                self.pos = saved_pos;

                if (is_block) {
                    // Parse as block
                    expr = try self.parseBlock(start);
                } else {
                    // Parse as record
                    expr = try self.parseRecord(start);
                }
            } else {
                // Not ambiguous - parse as block
                expr = try self.parseBlock(start);
            }
        },
        .OpBar => {
            self.advance();
            const scratch_top = self.store.scratchPatternTop();
            self.parseCollectionSpan(AST.Pattern.Idx, .OpBar, NodeStore.addScratchPattern, parsePatternNoAlts) catch |err| {
                switch (err) {
                    error.ExpectedNotFound => {
                        self.store.clearScratchPatternsFrom(scratch_top);
                        return try self.pushMalformed(AST.Expr.Idx, .expected_expr_bar, self.pos);
                    },
                    error.OutOfMemory => return error.OutOfMemory,
                    error.TooNested => return error.TooNested,
                }
            };
            const args = try self.store.patternSpanFrom(scratch_top);

            const body = try self.parseExpr();
            expr = try self.store.addExpr(.{ .lambda = .{
                .body = body,
                .args = args,
                .region = .{ .start = start, .end = self.pos },
            } });
        },
        .KwIf => {
            self.advance();
            const condition = try self.parseExpr();
            const then = try self.parseExpr();
            if (self.peek() != .KwElse) {
                // Point to the if keyword for missing else error
                return try self.pushMalformed(AST.Expr.Idx, .no_else, start);
            }
            self.advance();
            const else_idx = try self.parseExpr();
            expr = try self.store.addExpr(.{ .if_then_else = .{
                .region = .{ .start = start, .end = self.pos },
                .condition = condition,
                .then = then,
                .@"else" = else_idx,
            } });
        },
        .KwMatch => {
            self.advance();
            const e = try self.parseExpr();

            self.expect(.OpenCurly) catch {
                return try self.pushMalformed(AST.Expr.Idx, .expected_open_curly_after_match, self.pos);
            };
            const scratch_top = self.store.scratchMatchBranchTop();
            while (self.peek() != .CloseCurly and self.peek() != .EndOfFile) {
                try self.store.addScratchMatchBranch(try self.parseBranch());
                if (self.peek() == .Comma) {
                    self.advance();
                }
            }
            const branches = try self.store.matchBranchSpanFrom(scratch_top);
            if (self.peek() != .CloseCurly) {
                return try self.pushMalformed(AST.Expr.Idx, .expected_close_curly_at_end_of_match, self.pos);
            }
            self.advance();
            expr = try self.store.addExpr(.{ .match = .{
                .region = .{ .start = start, .end = self.pos },
                .expr = e,
                .branches = branches,
            } });
        },
        .KwDbg => {
            self.advance();
            const e = try self.parseExpr();
            expr = try self.store.addExpr(.{ .dbg = .{
                .region = .{ .start = start, .end = self.pos },
                .expr = e,
            } });
        },
        .TripleDot => {
            expr = try self.store.addExpr(.{ .ellipsis = .{
                .region = .{ .start = start, .end = self.pos },
            } });
            self.advance();
        },
        .OpUnaryMinus => {
            const operator_token = start;
            self.advance(); // consume the minus token
            // Parse the operand with high precedence (unary operators bind tightly)
            const operand = try self.parseExprWithBp(100);
            expr = try self.store.addExpr(.{ .unary_op = .{
                .operator = operator_token,
                .expr = operand,
                .region = .{ .start = start, .end = self.pos },
            } });
        },
        .OpBang => {
            const operator_token = start;
            self.advance(); // consume the bang token
            // Parse the operand with high precedence (unary operators bind tightly)
            const operand = try self.parseExprWithBp(100);
            expr = try self.store.addExpr(.{ .unary_op = .{
                .operator = operator_token,
                .expr = operand,
                .region = .{ .start = start, .end = self.pos },
            } });
        },
        else => {
            return try self.pushMalformed(AST.Expr.Idx, .expr_unexpected_token, start);
        },
    }
    if (expr) |e| {
        var expression = try self.parseExprSuffix(start, e);

        while (self.peek() == .NoSpaceDotInt or self.peek() == .NoSpaceDotLowerIdent or self.peek() == .DotLowerIdent or self.peek() == .OpArrow) {
            const tok = self.peek();
            if (tok == .NoSpaceDotInt) {
                return try self.pushMalformed(AST.Expr.Idx, .expr_no_space_dot_int, self.pos);
            } else if (self.peek() == .OpArrow) {
                const s = self.pos;
                self.advance();
                if (self.peek() == .LowerIdent) {
                    const empty_qualifiers = try self.store.tokenSpanFrom(self.store.scratchTokenTop());
                    const ident = try self.store.addExpr(.{ .ident = .{
                        .region = .{ .start = self.pos, .end = self.pos },
                        .token = self.pos,
                        .qualifiers = empty_qualifiers,
                    } });
                    self.advance();
                    const ident_suffixed = try self.parseExprSuffix(s, ident);
                    expression = try self.store.addExpr(.{ .local_dispatch = .{
                        .region = .{ .start = start, .end = self.pos },
                        .operator = s,
                        .left = expression,
                        .right = ident_suffixed,
                    } });
                } else if (self.peek() == .UpperIdent) { // UpperIdent - should be a tag
                    const empty_qualifiers = try self.store.tokenSpanFrom(self.store.scratchTokenTop());
                    const tag = try self.store.addExpr(.{ .tag = .{
                        .region = .{ .start = self.pos, .end = self.pos },
                        .token = self.pos,
                        .qualifiers = empty_qualifiers,
                    } });
                    self.advance();
                    const ident_suffixed = try self.parseExprSuffix(s, tag);
                    expression = try self.store.addExpr(.{ .local_dispatch = .{
                        .region = .{ .start = start, .end = self.pos },
                        .operator = s,
                        .left = expression,
                        .right = ident_suffixed,
                    } });
                } else {
                    return try self.pushMalformed(AST.Expr.Idx, .expr_arrow_expects_ident, self.pos);
                }
            } else { // NoSpaceDotLowerIdent
                const s = self.pos;
                self.advance();
                const empty_qualifiers = try self.store.tokenSpanFrom(self.store.scratchTokenTop());
                const ident = try self.store.addExpr(.{ .ident = .{
                    .region = .{ .start = s, .end = self.pos },
                    .token = s,
                    .qualifiers = empty_qualifiers,
                } });
                const ident_suffixed = try self.parseExprSuffix(s, ident);
                expression = try self.store.addExpr(.{ .field_access = .{
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

            const nextExpr = try self.parseExprWithBp(bp.right);

            expression = try self.store.addExpr(.{ .bin_op = .{
                .left = expression,
                .right = nextExpr,
                .operator = opPos,
                .region = .{ .start = start, .end = self.pos },
            } });
        }
        return expression;
    }
    return try self.store.addMalformed(AST.Expr.Idx, .expr_unexpected_token, .{ .start = start, .end = self.pos });
}

/// todo
fn parseExprSuffix(self: *Parser, start: u32, e: AST.Expr.Idx) Error!AST.Expr.Idx {
    const trace = tracy.trace(@src());
    defer trace.end();

    var expression = e;

    // Loop to handle multiple chained suffixes (applications, question marks, etc.)
    while (true) {
        if (self.peek() == .NoSpaceOpenRound) {
            // Handle function application
            self.advance();
            const scratch_top = self.store.scratchExprTop();
            self.parseCollectionSpan(AST.Expr.Idx, .CloseRound, NodeStore.addScratchExpr, parseExpr) catch |err| {
                switch (err) {
                    error.ExpectedNotFound => {
                        self.store.clearScratchExprsFrom(scratch_top);
                        return try self.pushMalformed(AST.Expr.Idx, .expected_expr_apply_close_round, start);
                    },
                    error.OutOfMemory => return error.OutOfMemory,
                    error.TooNested => return error.TooNested,
                }
            };
            const args = try self.store.exprSpanFrom(scratch_top);

            expression = try self.store.addExpr(.{
                .apply = .{
                    .args = args,
                    .@"fn" = expression, // Use current expression as function
                    .region = .{ .start = start, .end = self.pos },
                },
            });
            // Continue loop to check for more chained applications
        } else if (self.peek() == .NoSpaceOpQuestion) {
            self.advance();

            // Handle question mark suffix
            expression = try self.store.addExpr(.{ .suffix_single_question = .{
                .expr = expression,
                .operator = start,
                .region = .{ .start = start, .end = self.pos },
            } });

            // Continue loop to check for more suffixes
        } else {
            // No more suffixes to parse
            break;
        }
    }
    return expression;
}

/// todo
pub fn parseRecordField(self: *Parser) Error!AST.RecordField.Idx {
    const trace = tracy.trace(@src());
    defer trace.end();

    const start = self.pos;

    self.expect(.LowerIdent) catch {
        return try self.pushMalformed(AST.RecordField.Idx, .expected_expr_record_field_name, start);
    };
    const name = start;
    var value: ?AST.Expr.Idx = null;
    if (self.peek() == .OpColon) {
        self.advance();
        value = try self.parseExpr();
    }

    return try self.store.addRecordField(.{
        .name = name,
        .value = value,
        .region = .{ .start = start, .end = self.pos },
    });
}

/// todo
pub fn parseBranch(self: *Parser) Error!AST.MatchBranch.Idx {
    const trace = tracy.trace(@src());
    defer trace.end();

    const start = self.pos;
    const p = try self.parsePattern(.alternatives_allowed);
    if (self.peek() == .OpFatArrow) {
        self.advance();
    } else if (self.peek() == .OpArrow) {
        // Add diagnostic for wrong arrow
        try self.pushDiagnostic(.match_branch_wrong_arrow, .{
            .start = self.pos,
            .end = self.pos,
        });

        self.advance();
    } else {
        // Add diagnostic for missing arrow
        try self.pushDiagnostic(.match_branch_missing_arrow, .{
            .start = self.pos,
            .end = self.pos,
        });
    }
    const b = try self.parseExpr();
    return try self.store.addMatchBranch(.{
        .region = .{ .start = start, .end = self.pos },
        .pattern = p,
        .body = b,
    });
}

/// Parse a multiline string expression with optional interpolations
pub fn parseMultiLineStringExpr(self: *Parser) Error!AST.Expr.Idx {
    const trace = tracy.trace(@src());
    defer trace.end();
    std.debug.assert(self.peek() == .MultilineStringStart);
    const start = self.pos;
    self.advance();
    const scratch_top = self.store.scratchExprTop();
    while (self.peek() != .EndOfFile) {
        switch (self.peek()) {
            .MultilineStringStart => {
                self.advance();
            },
            .StringPart => {
                const part_start = self.pos;
                self.advance(); // Advance past the StringPart
                const index = try self.store.addExpr(.{ .string_part = .{
                    .token = part_start,
                    .region = .{ .start = part_start, .end = self.pos },
                } });
                try self.store.addScratchExpr(index);
            },
            .OpenStringInterpolation => {
                self.advance(); // Advance past OpenStringInterpolation
                const ex = try self.parseExpr();
                try self.store.addScratchExpr(ex);
                if (self.peek() != .CloseStringInterpolation) {
                    return try self.pushMalformed(AST.Expr.Idx, .string_expected_close_interpolation, start);
                }
                self.advance(); // Advance past the CloseString Interpolation
            },
            .MalformedStringPart => {
                self.advance();
                try self.pushDiagnostic(.string_unexpected_token, .{
                    .start = self.pos,
                    .end = self.pos,
                });
            },
            else => {
                // Multi lins strings just end
                break;
            },
        }
    }

    const parts = try self.store.exprSpanFrom(scratch_top);
    const expr = try self.store.addExpr(.{
        .multiline_string = .{
            .token = start,
            .parts = parts,
            .region = .{
                .start = start,
                .end = self.pos,
            },
        },
    });
    return expr;
}

/// todo
pub fn parseStringExpr(self: *Parser) Error!AST.Expr.Idx {
    const trace = tracy.trace(@src());
    defer trace.end();

    std.debug.assert(self.peek() == .StringStart);
    const start = self.pos;
    // Start parsing string with possible interpolations
    // e.g.:
    // StringStart, StringPart, OpenStringInterpolation, <expr>, CloseStringInterpolation, StringPart, StringEnd
    self.advance();
    const scratch_top = self.store.scratchExprTop();
    while (self.peek() != .EndOfFile) {
        switch (self.peek()) {
            .StringEnd => {
                break;
            },
            .StringPart => {
                const part_start = self.pos;
                self.advance(); // Advance past the StringPart
                const index = try self.store.addExpr(.{ .string_part = .{
                    .token = part_start,
                    .region = .{ .start = part_start, .end = self.pos },
                } });
                try self.store.addScratchExpr(index);
            },
            .OpenStringInterpolation => {
                self.advance(); // Advance past OpenStringInterpolation
                const ex = try self.parseExpr();
                try self.store.addScratchExpr(ex);
                if (self.peek() != .CloseStringInterpolation) {
                    return try self.pushMalformed(AST.Expr.Idx, .string_expected_close_interpolation, start);
                }
                self.advance(); // Advance past the CloseString Interpolation
            },
            .MalformedStringPart => {
                // Don't create a parser diagnostic - the tokenizer already created
                // a more precise diagnostic with the exact error location
                self.advance();
            },
            else => {
                // Something is broken in the tokenizer if we get here!
                return try self.pushMalformed(AST.Expr.Idx, .string_unexpected_token, self.pos);
            },
        }
    }

    self.expect(.StringEnd) catch {
        try self.pushDiagnostic(.string_unclosed, .{
            .start = self.pos,
            .end = self.pos,
        });
    };

    const parts = try self.store.exprSpanFrom(scratch_top);
    const expr = try self.store.addExpr(.{
        .string = .{
            .token = start,
            .parts = parts,
            .region = .{
                .start = start,
                .end = self.pos,
            },
        },
    });
    return expr;
}

/// todo
pub fn parseStringPattern(self: *Parser) Error!AST.Pattern.Idx {
    const trace = tracy.trace(@src());
    defer trace.end();

    const start = self.pos;
    const inner = try parseStringExpr(self);
    const patt_idx = try self.store.addPattern(.{ .string = .{
        .string_tok = start,
        .region = .{ .start = start, .end = self.pos },
        .expr = inner,
    } });
    return patt_idx;
}

/// todo
pub fn parseTypeHeader(self: *Parser) Error!AST.TypeHeader.Idx {
    const trace = tracy.trace(@src());
    defer trace.end();

    const start = self.pos;
    std.debug.assert(self.peek() == .UpperIdent);
    self.advance(); // Advance past UpperIdent
    if (self.peek() != .NoSpaceOpenRound and self.peek() != .OpenRound) {
        return try self.store.addTypeHeader(.{
            .name = start,
            .args = .{ .span = .{
                .start = 0,
                .len = 0,
            } },
            .region = .{ .start = start, .end = self.pos },
        });
    }
    self.advance();
    const scratch_top = self.store.scratchTypeAnnoTop();
    self.parseCollectionSpan(AST.TypeAnno.Idx, .CloseRound, NodeStore.addScratchTypeAnno, Parser.parseTypeIdent) catch |err| {
        switch (err) {
            error.ExpectedNotFound => {
                self.store.clearScratchTypeAnnosFrom(scratch_top);
                return try self.pushMalformed(AST.TypeHeader.Idx, .expected_ty_anno_close_round_or_comma, start);
            },
            error.OutOfMemory => return error.OutOfMemory,
            error.TooNested => return error.TooNested,
        }
    };
    const args = try self.store.typeAnnoSpanFrom(scratch_top);
    return try self.store.addTypeHeader(.{
        .name = start,
        .args = args,
        .region = .{ .start = start, .end = self.pos },
    });
}

fn parseTypeIdent(self: *Parser) Error!AST.TypeAnno.Idx {
    const trace = tracy.trace(@src());
    defer trace.end();

    switch (self.peek()) {
        .LowerIdent, .NamedUnderscore, .Underscore => {
            const tok = self.pos;
            self.advance();
            const region = AST.TokenizedRegion{ .start = tok, .end = self.pos };

            return try self.store.addTypeAnno(switch (self.tok_buf.tokens.items(.tag)[tok]) {
                .LowerIdent => .{ .ty_var = .{ .region = region, .tok = tok } },
                .NamedUnderscore => .{ .underscore_type_var = .{ .region = region, .tok = tok } },
                .Underscore => .{ .underscore = .{ .region = region } },
                else => unreachable,
            });
        },
        else => {
            return self.pushMalformed(AST.TypeAnno.Idx, .invalid_type_arg, self.pos);
        },
    }
}

const TyFnArgs = enum {
    not_looking_for_args,
    looking_for_args,
};

/// Parse a type annotation, e.g. `Foo(a) : (a,Str,I64)`
pub fn parseTypeAnno(self: *Parser, looking_for_args: TyFnArgs) Error!AST.TypeAnno.Idx {
    const trace = tracy.trace(@src());
    defer trace.end();

    try self.nest();
    defer self.unnest();

    const start = self.pos;
    var anno: ?AST.TypeAnno.Idx = null;

    const first_token_tag = self.peek();
    switch (first_token_tag) {
        .UpperIdent, .LowerIdent => blk: {
            const qual_result = try self.parseQualificationChain();
            // Use final token as end position to avoid newline tokens
            self.pos = qual_result.final_token + 1;

            if (first_token_tag == .LowerIdent and qual_result.qualifiers.span.len == 0) {
                anno = try self.store.addTypeAnno(.{ .ty_var = .{
                    .tok = qual_result.final_token,
                    .region = .{ .start = qual_result.final_token, .end = self.pos },
                } });
                break :blk;
            }

            anno = try self.store.addTypeAnno(.{ .ty = .{
                .region = .{ .start = start, .end = self.pos },
                .token = qual_result.final_token,
                .qualifiers = qual_result.qualifiers,
            } });

            if (self.peek() == .NoSpaceOpenRound) {
                self.advance(); // Advance past NoSpaceOpenRound
                const scratch_top = self.store.scratchTypeAnnoTop();
                try self.store.addScratchTypeAnno(anno orelse {
                    return try self.store.addMalformed(AST.TypeAnno.Idx, .ty_anno_unexpected_token, .{ .start = start, .end = self.pos });
                });
                self.parseCollectionSpan(AST.TypeAnno.Idx, .CloseRound, NodeStore.addScratchTypeAnno, parseTypeAnnoInCollection) catch |err| {
                    switch (err) {
                        error.ExpectedNotFound => {
                            self.store.clearScratchTypeAnnosFrom(scratch_top);
                            return try self.pushMalformed(AST.TypeAnno.Idx, .expected_ty_apply_close_round, start);
                        },
                        error.OutOfMemory => return error.OutOfMemory,
                        error.TooNested => return error.TooNested,
                    }
                };

                anno = try self.store.addTypeAnno(.{ .apply = .{
                    .region = .{ .start = start, .end = self.pos },
                    .args = try self.store.typeAnnoSpanFrom(scratch_top),
                } });
            }
        },
        .NamedUnderscore => {
            anno = try self.store.addTypeAnno(.{ .underscore_type_var = .{
                .tok = self.pos,
                .region = .{ .start = start, .end = self.pos + 1 },
            } });
            self.advance(); // Advance past NamedUnderscore
        },
        .NoSpaceOpenRound, .OpenRound => {
            // Probably a tuple
            self.advance(); // Advance past OpenRound
            const after_round = self.pos;
            const scratch_top = self.store.scratchTypeAnnoTop();
            while (self.peek() != .CloseRound and self.peek() != .OpArrow and self.peek() != .OpFatArrow and self.peek() != .EndOfFile) {
                // Looking for args here so that we don't capture an un-parenthesized fn's args
                try self.store.addScratchTypeAnno(try self.parseTypeAnno(.looking_for_args));
                if (self.peek() != .Comma) {
                    break;
                }
                self.advance(); // Advance past Comma
            }
            if (self.peek() == .OpArrow or self.peek() == .OpFatArrow) {
                // use the scratch for the args for the func, advance, get the ret and set this to be a fn
                // since it's a function, as long as we find the CloseRound we can just return here.
                const args = try self.store.typeAnnoSpanFrom(scratch_top);
                const effectful = self.peek() == .OpFatArrow;
                self.advance(); // Advance past arrow
                const ret = try self.parseTypeAnno(.looking_for_args);
                if (self.peek() != .CloseRound) {
                    self.store.clearScratchTypeAnnosFrom(scratch_top);
                    return try self.pushMalformed(AST.TypeAnno.Idx, .expected_ty_anno_close_round, start);
                }
                const function = try self.store.addTypeAnno(.{ .@"fn" = .{
                    .args = args,
                    .ret = ret,
                    .effectful = effectful,
                    .region = .{ .start = after_round, .end = self.pos },
                } });
                self.advance();
                anno = try self.store.addTypeAnno(.{ .parens = .{
                    .anno = function,
                    .region = .{ .start = start, .end = self.pos },
                } });
            } else {
                if (self.peek() != .CloseRound) {
                    self.store.clearScratchTypeAnnosFrom(scratch_top);
                    return try self.pushMalformed(AST.TypeAnno.Idx, .expected_ty_anno_close_round, start);
                }
                self.advance(); // Advance past CloseRound
                const annos = try self.store.typeAnnoSpanFrom(scratch_top);
                anno = try self.store.addTypeAnno(.{ .tuple = .{
                    .region = .{ .start = start, .end = self.pos },
                    .annos = annos,
                } });
            }
        },
        .OpenCurly => {
            self.advance(); // Advance past OpenCurly
            const scratch_top = self.store.scratchAnnoRecordFieldTop();
            self.parseCollectionSpan(AST.AnnoRecordField.Idx, .CloseCurly, NodeStore.addScratchAnnoRecordField, parseAnnoRecordField) catch |err| {
                switch (err) {
                    error.ExpectedNotFound => {
                        self.store.clearScratchAnnoRecordFieldsFrom(scratch_top);
                        return try self.pushMalformed(AST.TypeAnno.Idx, .expected_ty_close_curly_or_comma, self.pos);
                    },
                    error.OutOfMemory => return error.OutOfMemory,
                    error.TooNested => return error.TooNested,
                }
            };
            const fields = try self.store.annoRecordFieldSpanFrom(scratch_top);
            anno = try self.store.addTypeAnno(.{ .record = .{
                .region = .{ .start = start, .end = self.pos },
                .fields = fields,
            } });
        },
        .OpenSquare => {
            self.advance(); // Advance past OpenSquare
            const scratch_top = self.store.scratchTypeAnnoTop();
            self.parseCollectionSpan(AST.TypeAnno.Idx, .CloseSquare, NodeStore.addScratchTypeAnno, parseTypeAnnoInCollection) catch {
                self.store.clearScratchTypeAnnosFrom(scratch_top);
                return try self.pushMalformed(AST.TypeAnno.Idx, .expected_ty_close_square_or_comma, self.pos);
            };
            const tags = try self.store.typeAnnoSpanFrom(scratch_top);
            anno = try self.store.addTypeAnno(.{ .tag_union = .{
                .region = .{ .start = start, .end = self.pos },
                .open_anno = null,
                .tags = tags,
            } });
        },
        .Underscore => {
            anno = try self.store.addTypeAnno(.{ .underscore = .{
                .region = .{ .start = start, .end = self.pos },
            } });
            self.advance(); // Advance past Underscore
        },
        else => {
            return try self.pushMalformed(AST.TypeAnno.Idx, .ty_anno_unexpected_token, self.pos);
        },
    }

    if (anno) |an| {
        const curr = self.peek();
        const next_tok = self.peekNext();
        const two_away_tok = self.peekN(2);
        const curr_is_arrow = curr == .OpArrow or curr == .OpFatArrow;
        const next_is_not_lower_ident = next_tok != .LowerIdent;
        const not_followed_by_colon = two_away_tok != .OpColon;
        const two_away_is_arrow = two_away_tok == .OpArrow or two_away_tok == .OpFatArrow;
        if ((looking_for_args == .not_looking_for_args) and
            (curr_is_arrow or
                (curr == .Comma and (next_is_not_lower_ident or not_followed_by_colon or two_away_is_arrow) and next_tok != .CloseCurly)))
        {
            const scratch_top = self.store.scratchTypeAnnoTop();
            try self.store.addScratchTypeAnno(an);
            while (self.peek() == .Comma) {
                self.advance(); // Advance past Comma
                try self.store.addScratchTypeAnno(try self.parseTypeAnno(.looking_for_args));
            }
            const args = try self.store.typeAnnoSpanFrom(scratch_top);
            if (self.peek() != .OpArrow and self.peek() != .OpFatArrow) {
                return try self.pushMalformed(AST.TypeAnno.Idx, .expected_arrow, start);
            }
            const effectful = self.peek() == .OpFatArrow;
            self.advance(); // Advance past arrow
            // TODO: Handle thin vs fat arrow
            const ret = try self.parseTypeAnno(.looking_for_args);
            return try self.store.addTypeAnno(.{ .@"fn" = .{
                .region = .{ .start = start, .end = self.pos },
                .args = args,
                .ret = ret,
                .effectful = effectful,
            } });
        }
        return an;
    }

    return try self.store.addMalformed(AST.TypeAnno.Idx, .ty_anno_unexpected_token, .{ .start = start, .end = self.pos });
}

/// todo
pub fn parseTypeAnnoInCollection(self: *Parser) Error!AST.TypeAnno.Idx {
    const trace = tracy.trace(@src());
    defer trace.end();

    return try self.parseTypeAnno(.looking_for_args);
}

/// todo
pub fn parseAnnoRecordField(self: *Parser) Error!AST.AnnoRecordField.Idx {
    const trace = tracy.trace(@src());
    defer trace.end();

    const field_start = self.pos;
    if (self.peek() != .LowerIdent) {
        while (self.peek() != .CloseCurly and self.peek() != .Comma and self.peek() != .EndOfFile) {
            self.advance(); // Advance until we end this field or the record
        }
        return try self.pushMalformed(AST.AnnoRecordField.Idx, .expected_type_field_name, field_start);
    }
    const name = self.pos;
    self.advance(); // Advance past LowerIdent
    if (self.peek() != .OpColon) {
        while (self.peek() != .CloseCurly and self.peek() != .Comma and self.peek() != .EndOfFile) {
            self.advance(); // Advance until we end this field or the record
        }
        return try self.pushMalformed(AST.AnnoRecordField.Idx, .expected_colon_after_type_field_name, field_start);
    }
    self.advance(); // Advance past OpColon
    const ty = try self.parseTypeAnno(.not_looking_for_args);

    return try self.store.addAnnoRecordField(.{
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
pub fn parseWhereClause(self: *Parser) Error!AST.WhereClause.Idx {
    const trace = tracy.trace(@src());
    defer trace.end();

    const start = self.pos;
    if (self.peek() == .KwModule) {
        // Parsing a mod_method clause
        self.advance();
        self.expect(.NoSpaceOpenRound) catch {
            return try self.pushMalformed(
                AST.WhereClause.Idx,
                .where_expected_mod_open,
                start,
            );
        };
        const var_tok = self.pos;
        self.expect(.LowerIdent) catch {
            return try self.pushMalformed(
                AST.WhereClause.Idx,
                .where_expected_var,
                start,
            );
        };
        self.expect(.CloseRound) catch {
            return try self.pushMalformed(
                AST.WhereClause.Idx,
                .where_expected_mod_close,
                start,
            );
        };
        const name_tok = self.pos;
        if (self.peek() != .NoSpaceDotLowerIdent and self.peek() != .DotLowerIdent and self.peek() != .NoSpaceDotUpperIdent and self.peek() != .DotUpperIdent) {
            return try self.pushMalformed(
                AST.WhereClause.Idx,
                .where_expected_method_or_alias_name,
                start,
            );
        }
        self.advance();

        // Check if this is a type alias (uppercase identifier)
        const current_token_tag = self.tok_buf.tokens.items(.tag)[name_tok];
        if (current_token_tag == .NoSpaceDotUpperIdent or current_token_tag == .DotUpperIdent) {
            // Type alias case: module(a).TypeAlias
            return try self.store.addWhereClause(.{ .mod_alias = .{
                .region = .{ .start = start, .end = self.pos },
                .name_tok = name_tok,
                .var_tok = var_tok,
            } });
        }

        if (self.peek() == .OpColon) {
            // Handle type annotation syntax: module(a).method : type
            self.advance(); // advance past colon
            const args_start = self.pos;
            const method_type_anno = try self.parseTypeAnno(.not_looking_for_args);
            const method_type = self.store.getTypeAnno(method_type_anno);

            // Check if the type annotation is a function type
            if (method_type == .@"fn") {
                // Function type: extract args and return type
                const fn_type = method_type.@"fn";
                const args = try self.store.addCollection(
                    .collection_ty_anno,
                    .{
                        .region = .{ .start = args_start, .end = self.pos },
                        .span = fn_type.args.span,
                    },
                );
                return try self.store.addWhereClause(.{ .mod_method = .{
                    .region = .{ .start = start, .end = self.pos },
                    .name_tok = name_tok,
                    .var_tok = var_tok,
                    .args = args,
                    .ret_anno = fn_type.ret,
                } });
            } else {
                // Non-function type: treat as zero-argument method
                const empty_args = try self.store.addCollection(
                    .collection_ty_anno,
                    .{
                        .region = .{ .start = args_start, .end = self.pos },
                        .span = base.DataSpan.empty(),
                    },
                );
                return try self.store.addWhereClause(.{ .mod_method = .{
                    .region = .{ .start = start, .end = self.pos },
                    .name_tok = name_tok,
                    .var_tok = var_tok,
                    .args = empty_args,
                    .ret_anno = method_type_anno,
                } });
            }
        } else if (self.peek() == .OpArrow) {
            // Handle case where user forgot the colon: module(a).method -> Type
            return try self.pushMalformed(
                AST.WhereClause.Idx,
                .where_expected_colon,
                start,
            );
        } else {
            // Handle method call syntax: module(a).method(args) -> ret
            const arg_start = self.pos;
            self.expect(.NoSpaceOpenRound) catch {
                return try self.pushMalformed(
                    AST.WhereClause.Idx,
                    .where_expected_arg_open,
                    start,
                );
            };
            const ty_anno_top = self.store.scratchTypeAnnoTop();
            self.parseCollectionSpan(
                AST.TypeAnno.Idx,
                .CloseRound,
                NodeStore.addScratchTypeAnno,
                Parser.parseTypeAnnoInCollection,
            ) catch {
                self.store.clearScratchTypeAnnosFrom(ty_anno_top);
                return try self.pushMalformed(
                    AST.WhereClause.Idx,
                    .where_expected_arg_close,
                    start,
                );
            };
            const arg_span = try self.store.typeAnnoSpanFrom(ty_anno_top);
            const args = try self.store.addCollection(
                .collection_ty_anno,
                .{
                    .region = .{ .start = arg_start, .end = self.pos },
                    .span = arg_span.span,
                },
            );
            self.expect(.OpArrow) catch {
                return try self.pushMalformed(
                    AST.WhereClause.Idx,
                    .where_expected_method_arrow,
                    start,
                );
            };
            const ret_ty_anno = try self.parseTypeAnnoInCollection();
            return try self.store.addWhereClause(.{ .mod_method = .{
                .region = .{ .start = start, .end = self.pos },
                .name_tok = name_tok,
                .var_tok = var_tok,
                .args = args,
                .ret_anno = ret_ty_anno,
            } });
        }
    } else {
        // Only module(a).method syntax is supported
        return try self.pushMalformed(
            AST.WhereClause.Idx,
            .where_expected_module,
            start,
        );
    }
}

/// Parse a block of statements.
/// Check parseStmt to see how statements are parsed.
/// {
///     <stmt1>
///     ...
///     <stmtN>
/// }
pub fn parseBlock(self: *Parser, start: u32) Error!AST.Expr.Idx {
    const scratch_top = self.store.scratchStatementTop();

    while (self.peek() != .EndOfFile) {
        const statement = try self.parseStmt();
        try self.store.addScratchStatement(statement);
        if (self.peek() == .CloseCurly) {
            break;
        }
    }

    self.expect(.CloseCurly) catch {
        try self.pushDiagnostic(.expected_expr_close_curly, .{
            .start = self.pos,
            .end = self.pos,
        });
    };

    const statements = try self.store.statementSpanFrom(scratch_top);
    return try self.store.addExpr(.{ .block = .{
        .statements = statements,
        .region = .{ .start = start, .end = self.pos },
    } });
}

/// Parse a block that contains only statements, no ending expression.
/// This is used for nominal type associated items like `Foo := [A, B].{ x = 5 }`
/// {
///     <stmt1>
///     ...
///     <stmtN>
/// }
pub fn parseStatementOnlyBlock(self: *Parser, start: u32) Error!AST.Associated {
    const scratch_top = self.store.scratchStatementTop();

    while (self.peek() != .EndOfFile and self.peek() != .CloseCurly) {
        const statement_pos = self.pos;
        const statement = try self.parseStmt();

        // Check if this is an expression statement (the last statement must not be an expression)
        const stmt = self.store.getStatement(statement);
        if (stmt == .expr) {
            // Check if we're at the closing brace (this would be a final expression)
            if (self.peek() == .CloseCurly) {
                try self.pushDiagnostic(.nominal_associated_cannot_have_final_expression, .{
                    .start = statement_pos,
                    .end = self.pos,
                });
                // Still add it to maintain AST structure
            }
        }

        try self.store.addScratchStatement(statement);
    }

    self.expect(.CloseCurly) catch {
        try self.pushDiagnostic(.expected_expr_close_curly, .{
            .start = self.pos,
            .end = self.pos,
        });
    };

    const statements = try self.store.statementSpanFrom(scratch_top);
    return AST.Associated{
        .statements = statements,
        .region = .{ .start = start, .end = self.pos },
    };
}

/// Parse a record.
/// Check parseRecordField to see how record fields are parsed.
/// {
///     a,
///     b: <expr1>,
///     ...
///     <recordFieldN>
/// }
pub fn parseRecord(self: *Parser, start: u32) Error!AST.Expr.Idx {
    const scratch_top = self.store.scratchRecordFieldTop();
    self.parseCollectionSpan(AST.RecordField.Idx, .CloseCurly, NodeStore.addScratchRecordField, parseRecordField) catch |err| {
        switch (err) {
            error.ExpectedNotFound => {
                self.store.clearScratchRecordFieldsFrom(scratch_top);
                return try self.pushMalformed(AST.Expr.Idx, .expected_expr_close_curly_or_comma, self.pos);
            },
            error.OutOfMemory => return error.OutOfMemory,
            error.TooNested => return error.TooNested,
        }
    };
    const fields = try self.store.recordFieldSpanFrom(scratch_top);
    return try self.store.addExpr(.{ .record = .{
        .fields = fields,
        .ext = null,
        .region = .{ .start = start, .end = self.pos },
    } });
}

/// Binding power of the lhs and rhs of a particular operator.
const BinOpBp = struct { left: u8, right: u8 };

/// Get the binding power for a Token if it's a operator token, else return null.
fn getTokenBP(tok: Token.Tag) ?BinOpBp {
    return switch (tok) {
        .OpStar => .{ .left = 30, .right = 31 }, // 31 LEFT
        .OpSlash => .{ .left = 28, .right = 29 }, // 29 LEFT
        .OpDoubleSlash => .{ .left = 26, .right = 27 }, // 27 LEFT
        .OpPercent => .{ .left = 24, .right = 25 }, // 25 LEFT
        .OpPlus => .{ .left = 22, .right = 23 }, // 23 LEFT
        .OpBinaryMinus => .{ .left = 20, .right = 21 }, // 21 LEFT
        .OpDoubleQuestion => .{ .left = 18, .right = 19 }, // 19 LEFT
        .OpQuestion => .{ .left = 16, .right = 17 }, // 17 LEFT
        .OpEquals => .{ .left = 15, .right = 15 }, // 15 NOASSOC
        .OpNotEquals => .{ .left = 13, .right = 13 }, // 13 NOASSOC
        .OpLessThan => .{ .left = 11, .right = 11 }, // 11 NOASSOC
        .OpGreaterThan => .{ .left = 9, .right = 9 }, // 9 NOASSOC
        .OpLessThanOrEq => .{ .left = 7, .right = 7 }, // 7 NOASSOC
        .OpGreaterThanOrEq => .{ .left = 5, .right = 5 }, // 5 NOASSOC
        .OpAnd => .{ .left = 4, .right = 3 }, // 3 RIGHT
        .OpOr => .{ .left = 2, .right = 1 }, // 1 RIGHT
        else => null,
    };
}
