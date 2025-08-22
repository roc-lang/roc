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

const AST = @import("AST2.zig");
const Node = AST.Node;
const Position = base.Region.Position;
const TokenizedBuffer = tokenize.TokenizedBuffer;
const Token = tokenize.Token;
const TokenIdx = Token.Idx;
const tokenize = @import("tokenize2.zig");
const tokenize_iter = @import("tokenize_iter.zig");
const Ident = base.Ident;

const MAX_PARSE_DIAGNOSTICS: usize = 1_000;
const MAX_NESTING_LEVELS: u8 = 128;

/// A parser which tokenizes and parses source code into an abstract syntax tree.
///
/// IMPORTANT: This is an LL(1) parser by design. This means:
/// - We use exactly 1 token of lookahead (via peek())
/// - We NEVER look ahead more than 1 token
/// - We NEVER backtrack or rewind
/// - All parsing decisions are made based on the current token only
///
/// This design ensures:
/// - Predictable O(n) performance
/// - Minimal memory usage
/// - Simple, maintainable parsing logic
///
/// If you're tempted to add more lookahead or backtracking, please
/// reconsider the grammar or parsing strategy instead. The goal is
/// to keep this parser strictly LL(1).
pub const Parser = @This();

gpa: std.mem.Allocator,
token_iter: tokenize_iter.TokenIterator,
lookahead: ?Token, // Single token lookahead - LL(1) parser, no more!
last_position: Position, // Track last valid position for error reporting
ast: *AST,
byte_slices: *collections.ByteSlices, // Reference to tokenizer's ByteSlices
scratch_nodes: std.ArrayListUnmanaged(Node.Idx),
diagnostics: std.ArrayListUnmanaged(AST.Diagnostic),
cached_malformed_node: ?Node.Idx,
nesting_counter: u8,

/// init the parser from source text using TokenIterator
pub fn init(env: *base.CommonEnv, gpa: std.mem.Allocator, source: []const u8, messages: []tokenize.Diagnostic, ast: *AST, byte_slices: *collections.ByteSlices) std.mem.Allocator.Error!Parser {
    const token_iter = try tokenize_iter.TokenIterator.init(env, gpa, source, messages, byte_slices);

    var parser = Parser{
        .gpa = gpa,
        .token_iter = token_iter,
        .lookahead = null,
        .last_position = Position{ .offset = 0 },
        .ast = ast,
        .byte_slices = byte_slices,
        .scratch_nodes = .{},
        .diagnostics = .{},
        .cached_malformed_node = null,
        .nesting_counter = MAX_NESTING_LEVELS,
    };

    // Fill initial lookahead buffer
    try parser.fillLookahead();

    return parser;
}

/// init the parser from a buffer of tokens (legacy compatibility)
pub fn initFromTokens(tokens: TokenizedBuffer, gpa: std.mem.Allocator, ast: *AST, byte_slices: *collections.ByteSlices) std.mem.Allocator.Error!Parser {
    _ = tokens;
    _ = gpa;
    _ = ast;
    _ = byte_slices;
    @panic("initFromTokens is deprecated - use init with source text instead");
}

/// Fill the lookahead buffer
fn fillLookahead(self: *Parser) std.mem.Allocator.Error!void {
    if (self.lookahead == null) {
        self.lookahead = try self.token_iter.next(self.gpa);
        // Initialize last_position from first token if available
        if (self.lookahead) |token| {
            self.last_position = token.region.start;
        }
    }
}

pub fn deinit(parser: *Parser) void {
    parser.token_iter.deinit(parser.gpa);
    parser.scratch_nodes.deinit(parser.gpa);
    parser.diagnostics.deinit(parser.gpa);
    // diagnostics will be kept and passed to the following compiler stage
    // to be deinitialized by the caller when no longer required
}

/// Get the diagnostics collected during parsing
pub fn getDiagnostics(self: *const Parser) []const AST.Diagnostic {
    return self.diagnostics.items;
}

/// Take ownership of the diagnostics array
pub fn takeOwnedDiagnostics(self: *Parser) std.ArrayListUnmanaged(AST.Diagnostic) {
    const result = self.diagnostics;
    self.diagnostics = .{};
    return result;
}

/// helper to advance the parser by one token
pub fn advance(self: *Parser) void {
    // Save current position before advancing
    if (self.lookahead) |token| {
        self.last_position = token.region.end;
    }

    // Get next token
    self.lookahead = self.token_iter.next(self.gpa) catch null;
}

/// look ahead at the next token and return an error if it does not have the expected tag
pub fn expect(self: *Parser, expected: Token.Tag) error{ExpectedNotFound}!void {
    if (self.peek() != expected) {
        return error.ExpectedNotFound;
    }
    self.advance();
}

/// Peek at the token at the current position (1 token lookahead only!)
/// This is an LL(1) parser - we only ever look at the current token.
///
/// **note** caller is responsible to ensure this isn't the last token
pub fn peek(self: *Parser) Token.Tag {
    if (self.lookahead) |token| {
        return token.tag;
    }
    return .EndOfFile;
}

/// Get the current token (if available)
fn currentToken(self: *Parser) ?Token {
    return self.lookahead;
}

/// Get the current token's extra data
fn currentExtra(self: *Parser) Token.Extra {
    if (self.currentToken()) |token| {
        return token.extra;
    }
    return .{ .none = 0 };
}

/// Get the current token's region
fn currentRegion(self: *Parser) base.Region {
    if (self.currentToken()) |token| {
        return token.region;
    }
    return base.Region.from_raw_offsets(0, 0);
}

/// For backwards compatibility with code that used self.pos for error tracking
/// This will return the current position for error messages
fn getCurrentErrorPos(self: *Parser) Position {
    return self.currentPosition();
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

/// Get the current token's position
fn currentPosition(self: *Parser) Position {
    if (self.currentToken()) |token| {
        return token.region.start;
    }
    // If no current token, use last valid position
    // But make sure we have a reasonable position (not just 0)
    if (self.last_position.offset == 0) {
        // If we haven't seen any tokens yet, return position at end of source
        // This is better than returning 0 which points to nothing
        return Position{ .offset = @intCast(self.token_iter.env.source.len) };
    }
    return self.last_position;
}

/// Get the identifier at the current position (if it's an identifier token)
fn currentIdent(self: *Parser) ?Ident.Idx {
    if (self.currentToken()) |token| {
        return switch (token.extra) {
            .interned => |idx| idx,
            else => null,
        };
    }
    return null;
}

/// add a diagnostic error
pub fn pushDiagnostic(self: *Parser, tag: AST.Diagnostic.Tag, start_pos: Position, end_pos: Position) Error!void {
    if (self.diagnostics.items.len < MAX_PARSE_DIAGNOSTICS) {
        try self.diagnostics.append(self.gpa, .{ .tag = tag, .region = .{ .start = start_pos, .end = end_pos } });
    }
}

/// add a malformed node
pub fn pushMalformed(self: *Parser, tag: AST.Diagnostic.Tag, start_pos: Position) Error!Node.Idx {
    // Use current token position for the error if the provided start_pos is invalid
    const actual_start_pos = if (start_pos.offset == 0) blk: {
        if (self.currentPosition().offset > 0) {
            break :blk self.currentPosition();
        } else {
            break :blk self.last_position;
        }
    } else start_pos;

    const end_pos = self.currentPosition();

    if (self.peek() != .EndOfFile) {
        self.advance();
    }

    if (self.diagnostics.items.len < MAX_PARSE_DIAGNOSTICS) {
        try self.pushDiagnostic(tag, actual_start_pos, end_pos);
        return try self.ast.appendNode(self.gpa, actual_start_pos, .malformed, .{ .malformed = tag });
    } else {
        // Return a cached malformed node to avoid creating excessive nodes when diagnostic limit is exceeded
        if (self.cached_malformed_node == null) {
            self.cached_malformed_node = try self.ast.appendNode(self.gpa, actual_start_pos, .malformed, .{ .malformed = AST.Diagnostic.Tag.expr_unexpected_token });
        }
        return self.cached_malformed_node.?;
    }
}

/// parse a `.roc` module
///
/// the tokens are provided at Parser initialisation
pub fn parseFile(self: *Parser) Error!void {
    const trace = tracy.trace(@src());
    defer trace.end();

    // Parse the header if present
    try self.parseHeader();

    // Parse top-level statements
    const scratch_start = self.scratch_nodes.items.len;
    defer {
        self.scratch_nodes.items.len = scratch_start;
    }

    while (self.peek() != .EndOfFile) {
        const stmt = try self.parseTopLevelStatement();
        if (stmt) |s| {
            try self.scratch_nodes.append(self.gpa, s);
        }
    }

    // Store all statements as the module body
    const statements = self.scratch_nodes.items[scratch_start..];
    if (statements.len > 0) {
        const body_idx = try self.ast.appendNodeSlice(self.gpa, statements);
        _ = try self.ast.appendNode(self.gpa, Position{ .offset = 0 }, .block, .{ .nodes = body_idx });
    }
}

/// Parse module header
pub fn parseHeader(self: *Parser) Error!void {
    const start_pos = self.currentPosition();

    switch (self.peek()) {
        .KwApp => {
            self.advance();
            self.ast.header = try self.parseAppHeader(start_pos);
        },
        .KwModule => {
            self.advance();
            self.ast.header = try self.parseModuleHeader(start_pos);
        },
        .KwPackage => {
            self.advance();
            self.ast.header = try self.parsePackageHeader(start_pos);
        },
        .KwPlatform => {
            self.advance();
            self.ast.header = try self.parsePlatformHeader(start_pos);
        },
        .KwHosted => {
            self.advance();
            self.ast.header = try self.parseHostedHeader(start_pos);
        },
        else => {
            // No header - that's fine for standalone expressions
        },
    }
}

fn parseAppHeader(self: *Parser, start_pos: Position) Error!AST.Header {
    // Parse provides list: app [main!] ...
    self.expect(.OpenSquare) catch {
        try self.pushDiagnostic(.header_expected_open_square, start_pos, self.currentPosition());
        return AST.Header{ .app = .{
            .provides = @enumFromInt(0),
            .platform_idx = @enumFromInt(0),
            .packages = @enumFromInt(0),
            .region = start_pos,
        } };
    };

    const provides_idx = try self.parseExposedList(.CloseSquare);

    self.expect(.CloseSquare) catch {
        try self.pushDiagnostic(.header_expected_close_square, start_pos, self.currentPosition());
    };

    // Parse platform and packages: { pf: platform "...", package1: "...", ... }
    self.expect(.OpenCurly) catch {
        try self.pushDiagnostic(.expected_package_platform_open_curly, start_pos, self.currentPosition());
        return AST.Header{ .app = .{
            .provides = provides_idx,
            .platform_idx = @enumFromInt(0),
            .packages = @enumFromInt(0),
            .region = start_pos,
        } };
    };

    var platform_field: Node.Idx = @enumFromInt(0);
    const scratch_start = self.scratch_nodes.items.len;
    defer {
        self.scratch_nodes.items.len = scratch_start;
    }

    // Parse record fields inside {}
    while (self.peek() != .CloseCurly and self.peek() != .EndOfFile) {
        const field_start = self.currentPosition();

        // Expect field name
        if (self.peek() != .LowerIdent) {
            try self.pushDiagnostic(.expected_package_or_platform_name, start_pos, self.currentPosition());
            break;
        }

        const field_name = self.currentIdent();
        const name_pos = self.currentPosition();
        self.advance();

        // Expect colon
        self.expect(.OpColon) catch {
            try self.pushDiagnostic(.expected_package_or_platform_colon, start_pos, self.currentPosition());
            break;
        };

        // Check if this is the platform field
        if (self.peek() == .KwPlatform) {
            if (@intFromEnum(platform_field) != 0) {
                try self.pushDiagnostic(.multiple_platforms, start_pos, self.currentPosition());
                break;
            }
            self.advance(); // consume 'platform' keyword

            // Parse platform string
            if (self.peek() != .String) {
                try self.pushDiagnostic(.expected_platform_string, start_pos, self.currentPosition());
                break;
            }

            const platform_value = try self.parseExpr();

            // Create a record field node for the platform
            if (field_name) |name| {
                const field_node = try self.ast.appendNode(self.gpa, name_pos, .lc, .{ .ident = name });
                const binop_idx = try self.ast.appendBinOp(self.gpa, field_node, platform_value);
                platform_field = try self.ast.appendNode(self.gpa, field_start, .binop_colon, .{ .binop = binop_idx });
            }
        } else {
            // Regular package field
            if (self.peek() != .String) {
                try self.pushDiagnostic(.expected_package_or_platform_string, start_pos, self.currentPosition());
                break;
            }

            const package_value = try self.parseExpr();

            // Create a record field node for the package
            if (field_name) |name| {
                const field_node = try self.ast.appendNode(self.gpa, name_pos, .lc, .{ .ident = name });
                const binop_idx = try self.ast.appendBinOp(self.gpa, field_node, package_value);
                const field = try self.ast.appendNode(self.gpa, field_start, .binop_colon, .{ .binop = binop_idx });
                try self.scratch_nodes.append(self.gpa, field);
            }
        }

        // Check for comma (optional)
        if (self.peek() == .Comma) {
            self.advance();
        } else if (self.peek() != .CloseCurly) {
            // If not comma and not close curly, something's wrong
            break;
        }
    }

    self.expect(.CloseCurly) catch {
        try self.pushDiagnostic(.expected_package_platform_close_curly, start_pos, self.currentPosition());
    };

    // Create packages collection from scratch nodes
    const packages_nodes = self.scratch_nodes.items[scratch_start..];
    const packages_idx = if (packages_nodes.len > 0)
        try self.ast.appendNodeSlice(self.gpa, packages_nodes)
    else
        @as(AST.NodeSlices.Idx, @enumFromInt(0));

    // Check that we found a platform
    if (@intFromEnum(platform_field) == 0) {
        try self.pushDiagnostic(.no_platform, start_pos, self.currentPosition());
        return AST.Header{ .app = .{
            .provides = provides_idx,
            .platform_idx = @enumFromInt(0),
            .packages = packages_idx,
            .region = start_pos,
        } };
    }

    return AST.Header{ .app = .{
        .provides = provides_idx,
        .platform_idx = platform_field,
        .packages = packages_idx,
        .region = start_pos,
    } };
}

fn parseModuleHeader(self: *Parser, start_pos: Position) Error!AST.Header {
    self.expect(.OpenSquare) catch {
        try self.pushDiagnostic(.header_expected_open_square, start_pos, self.currentPosition());
        return AST.Header{ .module = .{
            .exposes = @enumFromInt(0),
            .region = start_pos,
        } };
    };

    const exposes_idx = try self.parseExposedList(.CloseSquare);

    self.expect(.CloseSquare) catch {
        try self.pushDiagnostic(.header_expected_close_square, start_pos, self.currentPosition());
    };

    return AST.Header{ .module = .{
        .exposes = exposes_idx,
        .region = start_pos,
    } };
}

fn parsePackageHeader(self: *Parser, start_pos: Position) Error!AST.Header {
    self.expect(.OpenSquare) catch {
        try self.pushDiagnostic(.header_expected_open_square, start_pos, self.currentPosition());
        return AST.Header{ .package = .{
            .exposes = @enumFromInt(0),
            .packages = @enumFromInt(0),
            .region = start_pos,
        } };
    };

    const exposes_idx = try self.parseExposedList(.CloseSquare);

    self.expect(.CloseSquare) catch {
        try self.pushDiagnostic(.header_expected_close_square, start_pos, self.currentPosition());
    };

    // Parse packages (optional)
    const packages_idx = if (self.peek() == .OpenCurly) blk: {
        self.advance();
        const idx = try self.parsePackageList();
        self.expect(.CloseCurly) catch {
            try self.pushDiagnostic(.expected_packages_close_curly, start_pos, self.currentPosition());
        };
        break :blk idx;
    } else @as(AST.NodeSlices.Idx, @enumFromInt(0));

    return AST.Header{ .package = .{
        .exposes = exposes_idx,
        .packages = packages_idx,
        .region = start_pos,
    } };
}

fn parsePlatformHeader(self: *Parser, start_pos: Position) Error!AST.Header {

    // Parse platform name
    const name_idx = try self.parseExpr();

    // Parse requires
    self.expect(.KwRequires) catch {
        try self.pushDiagnostic(.expected_requires, start_pos, self.currentPosition());
        return AST.Header{ .platform = .{
            .name = name_idx,
            .requires_rigids = @enumFromInt(0),
            .requires_signatures = @enumFromInt(0),
            .exposes = @enumFromInt(0),
            .packages = @enumFromInt(0),
            .provides = @enumFromInt(0),
            .region = start_pos,
        } };
    };

    // Parse rigids
    self.expect(.OpenCurly) catch {
        try self.pushDiagnostic(.expected_requires_rigids_open_curly, start_pos, self.currentPosition());
    };

    const rigids_idx = try self.parseTypeVariableList();

    self.expect(.CloseCurly) catch {
        try self.pushDiagnostic(.expected_requires_rigids_close_curly, start_pos, self.currentPosition());
    };

    // Parse signatures
    self.expect(.OpenCurly) catch {
        try self.pushDiagnostic(.expected_requires_signatures_open_curly, start_pos, self.currentPosition());
    };

    const signatures_idx = try self.parseExpr();

    self.expect(.CloseCurly) catch {
        try self.pushDiagnostic(.expected_requires_signatures_close_curly, start_pos, self.currentPosition());
    };

    // Parse exposes
    self.expect(.KwExposes) catch {
        try self.pushDiagnostic(.expected_exposes, start_pos, self.currentPosition());
    };

    self.expect(.OpenSquare) catch {
        try self.pushDiagnostic(.expected_exposes_open_square, start_pos, self.currentPosition());
    };

    const exposes_idx = try self.parseExposedList(.CloseSquare);

    self.expect(.CloseSquare) catch {
        try self.pushDiagnostic(.expected_exposes_close_square, start_pos, self.currentPosition());
    };

    // Parse packages
    self.expect(.KwPackages) catch {
        try self.pushDiagnostic(.expected_packages, start_pos, self.currentPosition());
    };

    self.expect(.OpenCurly) catch {
        try self.pushDiagnostic(.expected_packages_open_curly, start_pos, self.currentPosition());
    };

    const packages_idx = try self.parsePackageList();

    self.expect(.CloseCurly) catch {
        try self.pushDiagnostic(.expected_packages_close_curly, start_pos, self.currentPosition());
    };

    // Parse provides
    self.expect(.KwProvides) catch {
        try self.pushDiagnostic(.expected_provides, start_pos, self.currentPosition());
    };

    self.expect(.OpenSquare) catch {
        try self.pushDiagnostic(.expected_provides_open_square, start_pos, self.currentPosition());
    };

    const provides_idx = try self.parseExposedList(.CloseSquare);

    self.expect(.CloseSquare) catch {
        try self.pushDiagnostic(.expected_provides_close_square, start_pos, self.currentPosition());
    };

    return AST.Header{ .platform = .{
        .name = name_idx,
        .requires_rigids = rigids_idx,
        .requires_signatures = signatures_idx,
        .exposes = exposes_idx,
        .packages = packages_idx,
        .provides = provides_idx,
        .region = start_pos,
    } };
}

fn parseHostedHeader(self: *Parser, start_pos: Position) Error!AST.Header {
    self.expect(.KwExposes) catch {
        try self.pushDiagnostic(.expected_exposes, start_pos, self.currentPosition());
        return AST.Header{ .hosted = .{
            .exposes = @enumFromInt(0),
            .region = start_pos,
        } };
    };

    self.expect(.OpenSquare) catch {
        try self.pushDiagnostic(.expected_exposes_open_square, start_pos, self.currentPosition());
    };

    const exposes_idx = try self.parseExposedList(.CloseSquare);

    self.expect(.CloseSquare) catch {
        try self.pushDiagnostic(.expected_exposes_close_square, start_pos, self.currentPosition());
    };

    return AST.Header{ .hosted = .{
        .exposes = exposes_idx,
        .region = start_pos,
    } };
}

fn parsePlatformSpecification(self: *Parser) Error!Node.Idx {
    const start_position = self.currentPosition();

    // Parse platform specification like { pf: platform "..." } or just platform "..."
    if (self.peek() == .OpenCurly) {
        self.advance();

        const scratch_start = self.scratch_nodes.items.len;
        defer {
            self.scratch_nodes.items.len = scratch_start;
        }

        // Parse key-value pairs
        while (self.peek() != .CloseCurly and self.peek() != .EndOfFile) {
            // Parse field name
            if (self.peek() != .LowerIdent) {
                return self.pushMalformed(.expr_unexpected_token, start_position);
            }

            const field_ident = self.currentIdent();
            const field_pos = self.currentPosition();
            self.advance();

            self.expect(.OpColon) catch {
                return self.pushMalformed(.expected_colon_after_pat_field_name, start_position);
            };

            // Parse field value (could be platform keyword or expression)
            const value = if (self.peek() == .KwPlatform) blk: {
                self.advance();
                // Parse platform URL/path
                const path = try self.parseExpr();
                break :blk path;
            } else try self.parseExpr();

            // Create field node
            if (field_ident) |id| {
                const field_node = try self.ast.appendNode(self.gpa, field_pos, .lc, .{ .ident = id });
                try self.scratch_nodes.append(self.gpa, field_node);
                try self.scratch_nodes.append(self.gpa, value);
            }

            if (self.peek() == .Comma) {
                self.advance();
            } else {
                break;
            }
        }

        self.expect(.CloseCurly) catch {
            try self.pushDiagnostic(.expected_expr_close_curly, start_position, self.currentPosition());
        };

        const nodes = self.scratch_nodes.items[scratch_start..];
        const nodes_idx = try self.ast.appendNodeSlice(self.gpa, nodes);
        return try self.ast.appendNode(self.gpa, start_position, .record_literal, .{ .nodes = nodes_idx });
    } else {
        // Just parse as expression (e.g., platform "...")
        return try self.parseExpr();
    }
}

fn parsePackageList(self: *Parser) Error!AST.NodeSlices.Idx {
    const scratch_start = self.scratch_nodes.items.len;
    defer {
        self.scratch_nodes.items.len = scratch_start;
    }

    while (self.peek() != .CloseCurly and self.peek() != .EndOfFile) {
        // Parse package name
        if (self.peek() != .LowerIdent) {
            _ = try self.pushMalformed(.expr_unexpected_token, self.getCurrentErrorPos());
            break;
        }

        const pkg_ident = self.currentIdent();
        const pkg_pos = self.currentPosition();
        self.advance();

        self.expect(.OpColon) catch {
            _ = try self.pushMalformed(.expected_colon_after_pat_field_name, self.getCurrentErrorPos());
            break;
        };

        // Parse package path
        const path = try self.parseExpr();

        // Create package entry
        if (pkg_ident) |id| {
            const pkg_node = try self.ast.appendNode(self.gpa, pkg_pos, .lc, .{ .ident = id });
            try self.scratch_nodes.append(self.gpa, pkg_node);
            try self.scratch_nodes.append(self.gpa, path);
        }

        if (self.peek() == .Comma) {
            self.advance();
        } else {
            break;
        }
    }

    const packages = self.scratch_nodes.items[scratch_start..];
    if (packages.len == 0) {
        return @enumFromInt(0);
    }
    return try self.ast.appendNodeSlice(self.gpa, packages);
}

fn parseExposedList(self: *Parser, end_token: Token.Tag) Error!AST.NodeSlices.Idx {
    const scratch_start = self.scratch_nodes.items.len;
    defer {
        self.scratch_nodes.items.len = scratch_start;
    }

    while (self.peek() != end_token and self.peek() != .EndOfFile) {
        const item = try self.parseExposedItem();
        try self.scratch_nodes.append(self.gpa, item);

        if (self.peek() == .Comma) {
            self.advance();
        } else {
            break;
        }
    }

    const items = self.scratch_nodes.items[scratch_start..];
    if (items.len == 0) {
        return @enumFromInt(0);
    }
    return try self.ast.appendNodeSlice(self.gpa, items);
}

fn parseExposedItem(self: *Parser) Error!Node.Idx {
    const start_position = self.currentPosition();

    switch (self.peek()) {
        .UpperIdent => {
            const ident = self.currentIdent();
            const pos = self.currentPosition();
            self.advance();

            if (ident) |id| {
                return try self.ast.appendNode(self.gpa, pos, .uc, .{ .ident = id });
            } else {
                return self.pushMalformed(.exposed_item_unexpected_token, start_position);
            }
        },
        .LowerIdent => {
            const ident = self.currentIdent();
            const pos = self.currentPosition();
            self.advance();

            // Check for optional ! suffix (for effectful functions like main!)
            if (self.peek() == .OpBang) {
                self.advance(); // consume the !
            }

            if (ident) |id| {
                return try self.ast.appendNode(self.gpa, pos, .lc, .{ .ident = id });
            } else {
                return self.pushMalformed(.exposed_item_unexpected_token, start_position);
            }
        },
        else => return self.pushMalformed(.exposed_item_unexpected_token, start_position),
    }
}

fn parseTypeVariableList(self: *Parser) Error!AST.NodeSlices.Idx {
    const scratch_start = self.scratch_nodes.items.len;
    defer {
        self.scratch_nodes.items.len = scratch_start;
    }

    while (self.peek() != .CloseCurly and self.peek() != .EndOfFile) {
        // Just parse as expression - should be a lowercase identifier
        const type_var = try self.parsePrimaryExpr();
        try self.scratch_nodes.append(self.gpa, type_var);

        if (self.peek() == .Comma) {
            self.advance();
        } else {
            break;
        }
    }

    const vars = self.scratch_nodes.items[scratch_start..];
    if (vars.len == 0) {
        return @enumFromInt(0);
    }
    return try self.ast.appendNodeSlice(self.gpa, vars);
}

/// Parse a top-level statement
pub fn parseTopLevelStatement(self: *Parser) Error!?Node.Idx {
    try self.nest();
    defer self.unnest();

    return self.parseStmt();
}

/// Parse a statement
pub fn parseStmt(self: *Parser) Error!?Node.Idx {
    switch (self.peek()) {
        .EndOfFile => return null,
        .KwImport => return self.parseImport(),
        .KwExpect => return self.parseExpect(),
        else => {
            // Parse the left-hand side as an expression first
            // We need to stop at colons to avoid consuming them
            // Colons have bp=3, so we use min_bp=3 to stop at them
            const lhs = try self.parseExprWithBp(3);

            // Check if this is followed by : or := (making it a type annotation/declaration)
            if (self.peek() == .OpColon or self.peek() == .OpColonEqual) {
                const is_opaque = self.peek() == .OpColonEqual;
                const colon_pos = self.currentPosition();
                self.advance(); // consume : or :=

                // Parse the right-hand side - for type annotations, we need special handling
                // to support comma-separated parameters and where clauses
                const rhs = try self.parseTypeAnnotationRHS();

                // Create type annotation/declaration node
                const tag: AST.Node.Tag = if (is_opaque) .binop_colon_equals else .binop_colon;
                const binop_idx = try self.ast.appendBinOp(self.gpa, lhs, rhs);
                return try self.ast.appendNode(self.gpa, colon_pos, tag, .{ .binop = binop_idx });
            }

            // Otherwise, it's just a regular expression/statement
            return lhs;
        },
    }
}

fn parseExpect(self: *Parser) Error!?Node.Idx {
    _ = self.currentPosition(); // Will be used when we properly implement expect
    self.advance(); // consume expect

    // Parse the condition expression
    const condition = try self.parseExpr();

    // For now, just return the condition expression
    // TODO: Add proper expect support in AST2
    // This at least allows expect statements to parse without errors
    return condition;
}

fn parseImport(self: *Parser) Error!?Node.Idx {
    const start_pos = self.currentPosition();
    self.advance(); // consume import

    const scratch_start = self.scratch_nodes.items.len;
    defer {
        self.scratch_nodes.items.len = scratch_start;
    }

    // Parse module path (e.g., pf.Stdout)
    var path_parts = std.ArrayList(u8).init(self.gpa);
    defer path_parts.deinit();

    while (true) {
        const token = self.peek();
        if (token == .LowerIdent or token == .UpperIdent) {
            const ident = self.currentIdent();
            if (ident) |id| {
                // Use the correct node type based on identifier case
                const node_tag: AST.Node.Tag = if (token == .UpperIdent) .uc else .lc;
                const node = try self.ast.appendNode(self.gpa, self.currentPosition(), node_tag, .{ .ident = id });
                try self.scratch_nodes.append(self.gpa, node);
            }
            self.advance();

            if (self.peek() == .Dot or self.peek() == .NoSpaceDotUpperIdent or self.peek() == .NoSpaceDotLowerIdent) {
                self.advance();
            } else {
                break;
            }
        } else {
            break;
        }
    }

    // Parse optional as clause
    if (self.peek() == .KwAs) {
        self.advance();

        const alias_token = self.peek();
        if (alias_token == .LowerIdent or alias_token == .UpperIdent) {
            const alias_ident = self.currentIdent();
            if (alias_ident) |id| {
                const alias_tag: AST.Node.Tag = if (alias_token == .UpperIdent) .uc else .lc;
                const alias_node = try self.ast.appendNode(self.gpa, self.currentPosition(), alias_tag, .{ .ident = id });
                try self.scratch_nodes.append(self.gpa, alias_node);
            }
            self.advance();
        }
    }

    // Parse optional exposing clause
    if (self.peek() == .KwExposing) {
        self.advance();
        self.expect(.OpenSquare) catch {};

        while (self.peek() != .CloseSquare and self.peek() != .EndOfFile) {
            const exposed = try self.parseExposedItem();
            try self.scratch_nodes.append(self.gpa, exposed);

            if (self.peek() == .Comma) {
                self.advance();
            } else {
                break;
            }
        }

        self.expect(.CloseSquare) catch {};
    }

    const nodes = self.scratch_nodes.items[scratch_start..];
    if (nodes.len == 0) {
        return null;
    }
    const nodes_idx = try self.ast.appendNodeSlice(self.gpa, nodes);
    // Use the new import node type
    return try self.ast.appendNode(self.gpa, start_pos, .import, .{ .import_nodes = nodes_idx });
}

/// Parse an expression
pub fn parseExpr(self: *Parser) Error!Node.Idx {
    return self.parseExprWithBp(0);
}

/// Parse an expression with precedence
pub fn parseExprWithBp(self: *Parser, min_bp: u8) Error!Node.Idx {
    const start_position = self.currentPosition();

    // Parse prefix/primary expression
    var left = try self.parsePrimaryExpr();

    // Parse operators - both infix and postfix, interleaved by precedence
    while (true) {
        // First check for infix operators
        const op_tag = self.peek();
        const bp = getBindingPower(op_tag);

        if (bp.left > min_bp) {
            const op_pos = self.currentPosition();
            self.advance();

            const right = try self.parseExprWithBp(bp.right);

            const binop_tag = tokenToBinOpTag(op_tag) orelse {
                return self.pushMalformed(.expr_unexpected_token, op_pos);
            };

            const binop_idx = try self.ast.appendBinOp(self.gpa, left, right);
            left = try self.ast.appendNode(self.gpa, op_pos, binop_tag, .{ .binop = binop_idx });
            continue; // Check for more operators
        }

        // Then check for postfix operators
        switch (self.peek()) {
            .OpenRound => {
                left = try self.parseApply(left);
                continue; // Check for more operators after apply
            },
            .Dot => {
                const dot_pos = self.currentPosition();
                self.advance();

                if (self.peek() == .LowerIdent) {
                    const ident = self.currentIdent();
                    self.advance();

                    if (ident) |id| {
                        const field = try self.ast.appendNode(self.gpa, dot_pos, .dot_lc, .{ .ident = id });
                        const binop_idx = try self.ast.appendBinOp(self.gpa, left, field);
                        left = try self.ast.appendNode(self.gpa, dot_pos, .binop_pipe, .{ .binop = binop_idx });
                    } else {
                        return self.pushMalformed(.expr_dot_suffix_not_allowed, start_position);
                    }
                } else if (self.peek() == .UpperIdent) {
                    // Handle qualified tags like Color.Red
                    const ident = self.currentIdent();
                    self.advance();

                    if (ident) |id| {
                        const tag = try self.ast.appendNode(self.gpa, dot_pos, .uc, .{ .ident = id });
                        const binop_idx = try self.ast.appendBinOp(self.gpa, left, tag);
                        left = try self.ast.appendNode(self.gpa, dot_pos, .binop_pipe, .{ .binop = binop_idx });
                    } else {
                        return self.pushMalformed(.expr_dot_suffix_not_allowed, start_position);
                    }
                } else if (self.peek() == .Int or self.peek() == .Float) {
                    const dot_field_pos = self.currentPosition();

                    const num = try self.parseNumLiteral();
                    const binop_idx = try self.ast.appendBinOp(self.gpa, left, num);
                    left = try self.ast.appendNode(self.gpa, dot_field_pos, .binop_pipe, .{ .binop = binop_idx });
                } else {
                    return self.pushMalformed(.expr_dot_suffix_not_allowed, start_position);
                }
                continue; // Check for more operators after dot
            },
            else => break, // No more operators
        }
    }

    return left;
}

fn parsePrimaryExpr(self: *Parser) Error!Node.Idx {
    return switch (self.peek()) {
        .LowerIdent => {
            const ident = self.currentIdent();
            const pos = self.currentPosition();
            self.advance();

            if (ident) |id| {
                return try self.ast.appendNode(self.gpa, pos, .lc, .{ .ident = id });
            } else {
                return self.pushMalformed(.expr_unexpected_token, pos);
            }
        },
        .UpperIdent => {
            const ident = self.currentIdent();
            const pos = self.currentPosition();
            self.advance();

            if (ident) |id| {
                return try self.ast.appendNode(self.gpa, pos, .uc, .{ .ident = id });
            } else {
                return self.pushMalformed(.expr_unexpected_token, pos);
            }
        },
        .Underscore => {
            const pos = self.currentPosition();
            self.advance();
            // Use the new underscore node type
            return try self.ast.appendNode(self.gpa, pos, .underscore, .{ .src_bytes_end = pos });
        },
        .Int, .Float, .IntBase => return self.parseNumLiteral(),
        .String, .MultilineString, .SingleQuote => return self.parseStoredStringExpr(),
        .StringStart => return self.parseStringExpr(),
        .OpenSquare => return self.parseListLiteral(),
        .OpenCurly => return self.parseBlockOrRecord(),
        .OpenRound => return self.parseTupleOrParenthesized(),
        .KwIf => return self.parseIf(),
        .KwMatch => return self.parseMatch(),
        .OpBar => return self.parseLambda(),
        .KwVar => return self.parseVar(),
        .KwFor => return self.parseFor(),
        .KwWhile => return self.parseWhile(),
        .KwReturn => return self.parseReturn(),
        .KwCrash => return self.parseCrash(),
        .OpBang => {
            const pos = self.currentPosition();
            self.advance();

            const operand = try self.parseExprWithBp(100); // High precedence for unary
            const operand_slice = [_]Node.Idx{operand};
            const nodes_idx = try self.ast.appendNodeSlice(self.gpa, &operand_slice);
            return try self.ast.appendNode(self.gpa, pos, .unary_not, .{ .nodes = nodes_idx });
        },
        .OpBinaryMinus, .OpUnaryMinus => {
            const pos = self.currentPosition();
            self.advance();

            const operand = try self.parseExprWithBp(100); // High precedence for unary
            const operand_slice = [_]Node.Idx{operand};
            const nodes_idx = try self.ast.appendNodeSlice(self.gpa, &operand_slice);
            return try self.ast.appendNode(self.gpa, pos, .unary_neg, .{ .nodes = nodes_idx });
        },
        .DoubleDot => {
            const pos = self.currentPosition();
            self.advance();

            const operand = try self.parseExprWithBp(100); // High precedence for unary
            const operand_slice = [_]Node.Idx{operand};
            const nodes_idx = try self.ast.appendNodeSlice(self.gpa, &operand_slice);
            return try self.ast.appendNode(self.gpa, pos, .unary_double_dot, .{ .nodes = nodes_idx });
        },
        .KwModule => {
            // Handle module(arg) for where clauses
            const pos = self.currentPosition();
            self.advance();

            if (self.peek() == .OpenRound) {
                self.advance(); // consume (

                const scratch_start = self.scratch_nodes.items.len;
                defer {
                    self.scratch_nodes.items.len = scratch_start;
                }

                // Parse the argument(s)
                while (self.peek() != .CloseRound and self.peek() != .EndOfFile) {
                    const arg = try self.parseExpr();
                    try self.scratch_nodes.append(self.gpa, arg);

                    if (self.peek() == .Comma) {
                        self.advance();
                    } else {
                        break;
                    }
                }

                self.expect(.CloseRound) catch {
                    try self.pushDiagnostic(.expected_expr_apply_close_round, pos, self.currentPosition());
                };

                const nodes = self.scratch_nodes.items[scratch_start..];
                const nodes_idx = try self.ast.appendNodeSlice(self.gpa, nodes);
                return try self.ast.appendNode(self.gpa, pos, .apply_module, .{ .nodes = nodes_idx });
            } else {
                // module without parentheses is an error in expression context
                return self.pushMalformed(.expr_unexpected_token, pos);
            }
        },
        else => {
            // Use current token position for the error, not start_position
            const error_pos = if (self.currentPosition().offset > 0) self.currentPosition() else self.last_position;
            return self.pushMalformed(.expr_unexpected_token, error_pos);
        },
    };
}

fn parseNumLiteral(self: *Parser) Error!Node.Idx {
    const pos = self.currentPosition();
    const tag = self.peek();
    const extra = self.currentExtra();
    // const token_region = self.currentRegion();
    // const end_pos = token_region.end; // TODO: Use this for better region calculation

    self.advance();

    // Handle different number literal types based on token tag and extra data
    switch (tag) {
        .Int, .IntBase => {
            // Integer literals (base-10 or other bases)
            switch (extra) {
                .num_literal_i32 => |value| {
                    // Small integer that fits in i32
                    return try self.ast.appendNode(self.gpa, pos, .num_literal_i32, .{ .num_literal_i32 = value });
                },
                .bytes_idx => |idx| {
                    // Big integer stored in ByteSlices
                    // For IntBase, it's stored as base-10 in ByteSlices
                    const ast_tag: Node.Tag = if (tag == .IntBase) .int_literal_big else .num_literal_big;
                    return try self.ast.appendNode(self.gpa, pos, ast_tag, .{ .num_literal_big = idx });
                },
                else => {
                    // Shouldn't happen with well-formed tokens
                    return try self.ast.appendNode(self.gpa, pos, .num_literal_i32, .{ .num_literal_i32 = 0 });
                },
            }
        },
        .Float => {
            // Floating point literals
            switch (extra) {
                .frac_literal_small => |small_dec| {
                    // Small fraction that fits in SmallDec - convert from Token.SmallDec to AST.SmallDec
                    const ast_small_dec = AST.Node.SmallDec{
                        .numerator = small_dec.numerator,
                        .denominator_power_of_ten = small_dec.denominator_power_of_ten,
                    };
                    return try self.ast.appendNode(self.gpa, pos, .frac_literal_small, .{ .frac_literal_small = ast_small_dec });
                },
                .bytes_idx => |idx| {
                    // Big fraction stored in ByteSlices
                    return try self.ast.appendNode(self.gpa, pos, .frac_literal_big, .{ .frac_literal_big = idx });
                },
                else => {
                    // Shouldn't happen with well-formed tokens
                    return try self.ast.appendNode(self.gpa, pos, .num_literal_i32, .{ .num_literal_i32 = 0 });
                },
            }
        },
        else => {
            // Not a number literal token
            return try self.ast.appendNode(self.gpa, pos, .num_literal_i32, .{ .num_literal_i32 = 0 });
        },
    }
}

fn parseStoredStringExpr(self: *Parser) Error!Node.Idx {
    const pos = self.currentPosition();
    const extra = self.currentExtra();

    self.advance();

    // The string content is stored in ByteSlices with escapes already resolved
    switch (extra) {
        .bytes_idx => |idx| {
            // Get the string from ByteSlices
            const str_content = self.byte_slices.slice(idx);

            // Check if it fits in small string
            if (str_content.len <= 4 and std.mem.indexOfAny(u8, str_content, "\x00") == null) {
                // Small string that fits inline
                var buf: [4]u8 = [_]u8{0} ** 4;
                @memcpy(buf[0..str_content.len], str_content);
                return try self.ast.appendNode(self.gpa, pos, .str_literal_small, .{ .str_literal_small = buf });
            } else {
                // Store in AST's ByteSlices
                const ast_idx = try self.ast.byte_slices.append(self.gpa, str_content);
                return try self.ast.appendNode(self.gpa, pos, .str_literal_big, .{ .str_literal_big = ast_idx });
            }
        },
        else => {
            // Shouldn't happen with well-formed tokens
            const empty_buf: [4]u8 = [_]u8{0} ** 4;
            return try self.ast.appendNode(self.gpa, pos, .str_literal_small, .{ .str_literal_small = empty_buf });
        },
    }
}

fn parseStringExpr(self: *Parser) Error!Node.Idx {
    const start_pos = self.currentPosition();

    self.expect(.StringStart) catch {
        return self.pushMalformed(.expr_unexpected_token, start_pos);
    };

    const scratch_start = self.scratch_nodes.items.len;
    defer {
        self.scratch_nodes.items.len = scratch_start;
    }

    // Collect all string bytes
    var string_bytes = std.ArrayList(u8).init(self.gpa);
    defer string_bytes.deinit();

    // Parse string parts
    while (self.peek() != .StringEnd and self.peek() != .EndOfFile) {
        switch (self.peek()) {
            .StringPart => {
                // Get the string content from the token's ByteSlices
                const extra = self.currentExtra();
                switch (extra) {
                    .bytes_idx => |idx| {
                        const str_content = self.byte_slices.slice(idx);
                        try string_bytes.appendSlice(str_content);
                    },
                    else => {
                        // Shouldn't happen - StringPart should always have bytes_idx
                        try string_bytes.appendSlice("");
                    },
                }
                self.advance();
            },
            .OpenStringInterpolation => {
                // Handle string interpolation
                self.advance();
                const expr = try self.parseExpr();
                try self.scratch_nodes.append(self.gpa, expr);
                self.expect(.CloseStringInterpolation) catch {};
            },
            else => break,
        }
    }

    self.expect(.StringEnd) catch {
        return self.pushMalformed(.string_unclosed, start_pos);
    };

    // Store the actual string content
    const total_bytes = string_bytes.items.len;

    // First check length - most strings are longer than 4 bytes
    if (total_bytes <= 4) {
        // Only check byte eligibility for short strings
        // All bytes must be ASCII excluding null (1-127)
        var eligible = true;
        for (string_bytes.items) |byte| {
            if (byte == 0 or byte >= 128) {
                eligible = false;
                break;
            }
        }

        if (eligible) {
            var small_bytes: [4]u8 = .{0} ** 4;
            // Copy the actual string bytes (up to 4 bytes)
            @memcpy(small_bytes[0..total_bytes], string_bytes.items);
            // If last byte is non-zero, we know it's 4 bytes long
            // Otherwise, count backwards to find the length
            return try self.ast.appendNode(self.gpa, start_pos, .str_literal_small, .{ .str_literal_small = small_bytes });
        }
    }

    // For longer strings or those with special characters, store in ByteSlices
    const bytes_idx = try self.ast.appendByteSlice(self.gpa, string_bytes.items);
    return try self.ast.appendNode(self.gpa, start_pos, .str_literal_big, .{ .str_literal_big = bytes_idx });
}

fn parseListLiteral(self: *Parser) Error!Node.Idx {
    const start_pos = self.currentPosition();
    self.advance(); // consume [

    if (self.peek() == .CloseSquare) {
        self.advance();
        const empty_slice: []const Node.Idx = &.{};
        const nodes_idx = try self.ast.appendNodeSlice(self.gpa, empty_slice);
        return try self.ast.appendNode(self.gpa, start_pos, .list_literal, .{ .nodes = nodes_idx });
    }

    const scratch_start = self.scratch_nodes.items.len;
    defer {
        self.scratch_nodes.items.len = scratch_start;
    }

    while (self.peek() != .CloseSquare and self.peek() != .EndOfFile) {
        const elem = try self.parseExpr();
        try self.scratch_nodes.append(self.gpa, elem);

        if (self.peek() == .Comma) {
            self.advance();
        } else {
            break;
        }
    }

    self.expect(.CloseSquare) catch {
        try self.pushDiagnostic(.expected_expr_close_square_or_comma, start_pos, self.currentPosition());
    };

    const elems = self.scratch_nodes.items[scratch_start..];
    if (elems.len > 0) {
        _ = try self.ast.appendNodeSlice(self.gpa, elems);
    }
    return try self.ast.appendNode(self.gpa, start_pos, .list_literal, .{ .list_elems = @intCast(elems.len) });
}

fn parseBlockOrRecord(self: *Parser) Error!Node.Idx {
    const start_pos = self.currentPosition();
    self.advance(); // consume {

    if (self.peek() == .CloseCurly) {
        self.advance();
        const empty_slice: []const Node.Idx = &.{};
        const nodes_idx = try self.ast.appendNodeSlice(self.gpa, empty_slice);
        return try self.ast.appendNode(self.gpa, start_pos, .record_literal, .{ .nodes = nodes_idx });
    }

    const scratch_start = self.scratch_nodes.items.len;
    defer {
        self.scratch_nodes.items.len = scratch_start;
    }

    // Parse first element to determine if it's a block or record
    // We parse as a statement which will handle both:
    // - Type annotations (like `apple : Type`)
    // - Record fields (like `field : value`)
    // - Regular expressions in blocks
    const first_elem_opt = try self.parseStmt();

    if (first_elem_opt == null) {
        // Empty block/record body - shouldn't happen after we checked for CloseCurly
        const empty_slice: []const Node.Idx = &.{};
        const nodes_idx = try self.ast.appendNodeSlice(self.gpa, empty_slice);
        return try self.ast.appendNode(self.gpa, start_pos, .block, .{ .nodes = nodes_idx });
    }

    const first_elem = first_elem_opt.?;
    try self.scratch_nodes.append(self.gpa, first_elem);

    // Determine if this is a record by looking at the structure
    // Record fields have binop_colon and are followed by comma
    // Type annotations also have binop_colon but are statements in blocks
    var is_record = false;
    if (self.ast.tag(first_elem) == .binop_colon and self.peek() == .Comma) {
        // It's a record field if we have colon AND comma
        is_record = true;
    }

    if (is_record and self.peek() == .Comma) {
        self.advance(); // consume comma
    }

    if (is_record) {
        while (self.peek() != .CloseCurly and self.peek() != .EndOfFile) {
            // Check for rest syntax `..` or `..identifier`
            if (self.peek() == .DoubleDot) {
                const dot_pos = self.currentPosition();
                self.advance(); // consume ..

                // Check if there's an identifier after .. (like ..rest)
                if (self.peek() == .LowerIdent) {
                    const ident = self.currentIdent();
                    _ = self.currentPosition(); // Position tracked in dot_pos
                    self.advance();

                    // Create a double_dot_lc node for ..identifier
                    if (ident) |id| {
                        const rest_node = try self.ast.appendNode(self.gpa, dot_pos, .double_dot_lc, .{ .ident = id });
                        try self.scratch_nodes.append(self.gpa, rest_node);
                    }
                } else {
                    // Just .. without an identifier - create an underscore as placeholder
                    // This represents "ignore the rest"
                    const rest_node = try self.ast.appendNode(self.gpa, dot_pos, .underscore, .{ .src_bytes_end = dot_pos });
                    try self.scratch_nodes.append(self.gpa, rest_node);
                }

                // Check if there's a comma after the rest (optional, but should be last)
                if (self.peek() == .Comma) {
                    self.advance();
                }
                break; // rest should be the last element
            } else {
                // Parse record field as a statement (to handle field : value)
                const field_opt = try self.parseStmt();
                if (field_opt) |field| {
                    try self.scratch_nodes.append(self.gpa, field);
                }

                if (self.peek() == .Comma) {
                    self.advance();
                } else {
                    break;
                }
            }
        }
    } else {
        // It's a block - parse remaining statements
        while (self.peek() != .CloseCurly and self.peek() != .EndOfFile) {
            const stmt = try self.parseStmt();
            if (stmt) |s| {
                try self.scratch_nodes.append(self.gpa, s);
            }
        }
    }

    self.expect(.CloseCurly) catch {
        try self.pushDiagnostic(.expected_expr_close_curly, start_pos, self.currentPosition());
    };

    const nodes = self.scratch_nodes.items[scratch_start..];
    const nodes_idx = try self.ast.appendNodeSlice(self.gpa, nodes);

    const tag: Node.Tag = if (is_record) .record_literal else .block;
    return try self.ast.appendNode(self.gpa, start_pos, tag, .{ .nodes = nodes_idx });
}

fn parseTupleOrParenthesized(self: *Parser) Error!Node.Idx {
    const start_pos = self.currentPosition();
    self.advance(); // consume (

    const scratch_start = self.scratch_nodes.items.len;
    defer {
        self.scratch_nodes.items.len = scratch_start;
    }

    // Check for empty tuple
    if (self.peek() == .CloseRound) {
        self.advance();
        const empty_slice: []const Node.Idx = &.{};
        const nodes_idx = try self.ast.appendNodeSlice(self.gpa, empty_slice);
        return try self.ast.appendNode(self.gpa, start_pos, .tuple_literal, .{ .nodes = nodes_idx });
    }

    const first = try self.parseExpr();
    try self.scratch_nodes.append(self.gpa, first);

    // Check if it's a tuple (has comma) or just parenthesized expr
    if (self.peek() == .Comma) {
        self.advance();

        while (self.peek() != .CloseRound and self.peek() != .EndOfFile) {
            const elem = try self.parseExpr();
            try self.scratch_nodes.append(self.gpa, elem);

            if (self.peek() == .Comma) {
                self.advance();
            } else {
                break;
            }
        }

        self.expect(.CloseRound) catch {
            // Use current position for error, not start_pos
            const error_pos = if (self.currentPosition().offset > 0) self.currentPosition() else self.last_position;
            try self.pushDiagnostic(.expected_expr_close_round_or_comma, error_pos, error_pos);
        };

        const elems = self.scratch_nodes.items[scratch_start..];

        // Safety check: tuples should have at least 2 elements
        if (elems.len == 0) {
            const empty_slice: []const Node.Idx = &.{};
            const nodes_idx = try self.ast.appendNodeSlice(self.gpa, empty_slice);
            return try self.ast.appendNode(self.gpa, start_pos, .tuple_literal, .{ .nodes = nodes_idx });
        }

        const elems_idx = try self.ast.appendNodeSlice(self.gpa, elems);
        return try self.ast.appendNode(self.gpa, start_pos, .tuple_literal, .{ .nodes = elems_idx });
    } else {
        self.expect(.CloseRound) catch {
            // Use current position for error, not start_pos
            const error_pos = if (self.currentPosition().offset > 0) self.currentPosition() else self.last_position;
            try self.pushDiagnostic(.expected_expr_close_round_or_comma, error_pos, error_pos);
        };

        // Just a parenthesized expression
        return first;
    }
}

fn parseApply(self: *Parser, func: Node.Idx) Error!Node.Idx {
    const start_pos = self.ast.start(func);
    self.advance(); // consume (

    const scratch_start = self.scratch_nodes.items.len;
    defer {
        self.scratch_nodes.items.len = scratch_start;
    }

    // First element is the function
    try self.scratch_nodes.append(self.gpa, func);

    // Parse arguments
    while (self.peek() != .CloseRound and self.peek() != .EndOfFile) {
        const arg = try self.parseExpr();
        try self.scratch_nodes.append(self.gpa, arg);

        if (self.peek() == .Comma) {
            self.advance();
        } else {
            break;
        }
    }

    self.expect(.CloseRound) catch {
        try self.pushDiagnostic(.expected_expr_apply_close_round, start_pos, self.currentPosition());
    };

    const nodes = self.scratch_nodes.items[scratch_start..];

    // Determine the tag based on the function node
    const tag: Node.Tag = switch (self.ast.tag(func)) {
        .uc, .uc_dot_ucs => .apply_uc,
        .lc, .lc_dot_ucs, .var_lc => .apply_lc,
        else => .apply_anon,
    };

    const nodes_idx = try self.ast.appendNodeSlice(self.gpa, nodes);
    return try self.ast.appendNode(self.gpa, start_pos, tag, .{ .nodes = nodes_idx });
}

fn parseIf(self: *Parser) Error!Node.Idx {
    const start_pos = self.currentPosition();
    self.advance(); // consume if

    const scratch_start = self.scratch_nodes.items.len;
    defer {
        self.scratch_nodes.items.len = scratch_start;
    }

    // Parse condition
    const cond = try self.parseExpr();
    try self.scratch_nodes.append(self.gpa, cond);

    self.expect(.OpArrow) catch {
        try self.pushDiagnostic(.expected_arrow, start_pos, self.currentPosition());
    };

    // Parse then branch
    const then_branch = try self.parseExpr();
    try self.scratch_nodes.append(self.gpa, then_branch);

    // Check for else
    if (self.peek() == .KwElse) {
        self.advance();

        const else_branch = try self.parseExpr();
        try self.scratch_nodes.append(self.gpa, else_branch);

        const nodes = self.scratch_nodes.items[scratch_start..];
        const nodes_idx = try self.ast.appendNodeSlice(self.gpa, nodes);
        return try self.ast.appendNode(self.gpa, start_pos, .if_else, .{ .if_branches = @intFromEnum(nodes_idx) });
    } else {
        const nodes = self.scratch_nodes.items[scratch_start..];
        const nodes_idx = try self.ast.appendNodeSlice(self.gpa, nodes);
        return try self.ast.appendNode(self.gpa, start_pos, .if_without_else, .{ .if_branches = @intFromEnum(nodes_idx) });
    }
}

fn parseMatch(self: *Parser) Error!Node.Idx {
    const start_pos = self.currentPosition();
    self.advance(); // consume when

    const scratch_start = self.scratch_nodes.items.len;
    defer {
        self.scratch_nodes.items.len = scratch_start;
    }

    // Parse scrutinee
    const scrutinee = try self.parseExpr();
    try self.scratch_nodes.append(self.gpa, scrutinee);

    self.expect(.OpArrow) catch {
        try self.pushDiagnostic(.expected_open_curly_after_match, start_pos, self.currentPosition());
    };

    // Parse branches
    var branch_count: u32 = 0;
    while (self.peek() != .EndOfFile) {
        const pattern = try self.parseExpr();
        try self.scratch_nodes.append(self.gpa, pattern);

        self.expect(.OpFatArrow) catch {
            try self.pushDiagnostic(.match_branch_missing_arrow, start_pos, self.currentPosition());
        };

        const body = try self.parseExpr();
        try self.scratch_nodes.append(self.gpa, body);

        branch_count += 1;

        // Check if there are more branches
        if (self.peek() != .OpBar and self.peek() != .Comma) {
            break;
        }
        self.advance();
    }

    const nodes = self.scratch_nodes.items[scratch_start..];
    const nodes_idx = try self.ast.appendNodeSlice(self.gpa, nodes);
    return try self.ast.appendNode(self.gpa, start_pos, .match, .{ .match_branches = @intFromEnum(nodes_idx) });
}

fn parseLambda(self: *Parser) Error!Node.Idx {
    const start_pos = self.currentPosition();
    self.advance(); // consume |

    const scratch_start = self.scratch_nodes.items.len;
    defer {
        self.scratch_nodes.items.len = scratch_start;
    }

    // Parse parameters
    while (self.peek() != .OpBar and self.peek() != .EndOfFile) {
        // Parse parameter as an expression, but we need to be careful with precedence
        // We want to stop at | (precedence 10) but allow as (precedence 3)
        // So we use precedence 11 to stop at |
        const param = if (self.peek() == .OpenCurly) blk: {
            // Parse the record first
            const record = try self.parseBlockOrRecord();

            // Now continue parsing operators that might apply to the record
            // We manually handle operators here because parseExprWithBp expects to parse
            // the primary expression itself
            var result = record;
            while (true) {
                const op_tag = self.peek();
                const bp = getBindingPower(op_tag);

                // Stop at | (precedence 10) or comma (not an operator)
                // We want to continue if precedence is >= 11 (higher than |)
                // But 'as' has precedence 3, which is lower, so we still want to parse it
                // Actually, we want to stop ONLY at | and comma, not at low-precedence ops
                if (op_tag == .OpBar or op_tag == .Comma) {
                    break;
                }

                const op_pos = self.currentPosition();
                self.advance();

                const right = try self.parseExprWithBp(bp.right);

                const binop_tag = tokenToBinOpTag(op_tag) orelse break;
                const nodes = [_]Node.Idx{ result, right };
                const nodes_idx = try self.ast.appendNodeSlice(self.gpa, &nodes);
                result = try self.ast.appendNode(self.gpa, op_pos, binop_tag, .{ .binop = nodes_idx });
            }
            break :blk result;
        } else
            // For non-records, parse with precedence 11 to stop at |
            try self.parseExprWithBp(11);

        try self.scratch_nodes.append(self.gpa, param);

        if (self.peek() == .Comma) {
            self.advance();
        } else {
            break;
        }
    }

    self.expect(.OpBar) catch {
        try self.pushDiagnostic(.expected_expr_bar, start_pos, self.currentPosition());
    };

    // Parse body
    const body = try self.parseExpr();

    // Store body first, then args
    const params = self.scratch_nodes.items[scratch_start..];

    if (params.len == 0) {
        // No arguments - use lambda_no_args which is more memory efficient
        // Store the body node index directly in block_nodes
        return try self.ast.appendNode(self.gpa, start_pos, .lambda_no_args, .{ .nodes = @enumFromInt(@intFromEnum(body)) });
    }

    var body_then_args = try self.gpa.alloc(Node.Idx, 1 + params.len);
    defer self.gpa.free(body_then_args);

    body_then_args[0] = body;
    @memcpy(body_then_args[1..], params);

    const nodes_idx = try self.ast.appendNodeSlice(self.gpa, body_then_args);
    return try self.ast.appendNode(self.gpa, start_pos, .lambda, .{ .body_then_args = nodes_idx });
}

fn parseVar(self: *Parser) Error!Node.Idx {
    const start_pos = self.currentPosition();
    self.advance(); // consume var

    if (self.peek() != .LowerIdent) {
        return self.pushMalformed(.var_must_have_ident, start_pos);
    }

    const ident = self.currentIdent();
    self.advance();

    if (ident) |id| {
        return try self.ast.appendNode(self.gpa, start_pos, .var_lc, .{ .ident = id });
    } else {
        return self.pushMalformed(.var_must_have_ident, start_pos);
    }
}

fn parseFor(self: *Parser) Error!Node.Idx {
    const start_pos = self.currentPosition();
    self.advance(); // consume 'for'

    const scratch_start = self.scratch_nodes.items.len;
    defer {
        self.scratch_nodes.items.len = scratch_start;
    }

    // Parse the pattern (e.g., 'x' in 'for x in ...')
    const pattern = try self.parseExpr();
    try self.scratch_nodes.append(self.gpa, pattern);

    // Expect 'in'
    if (self.peek() != .KwIn) {
        try self.pushDiagnostic(.for_expected_in, start_pos, self.currentPosition());
        return try self.ast.appendNode(self.gpa, start_pos, .malformed, .{ .malformed = .for_expected_in });
    }
    self.advance(); // consume 'in'

    // Parse the iterable expression
    const iterable = try self.parseExpr();
    try self.scratch_nodes.append(self.gpa, iterable);

    // Parse the body (just an expression, similar to while and lambda body)
    const body = try self.parseExpr();
    try self.scratch_nodes.append(self.gpa, body);

    const nodes = self.scratch_nodes.items[scratch_start..];

    // A for loop must have exactly pattern, iterable, and body
    if (nodes.len != 3) {
        return try self.ast.appendNode(self.gpa, start_pos, .malformed, .{ .malformed = .statement_unexpected_token });
    }

    const nodes_idx = try self.ast.appendNodeSlice(self.gpa, nodes);
    return try self.ast.appendNode(self.gpa, start_pos, .for_loop, .{ .nodes = nodes_idx });
}

fn parseWhile(self: *Parser) Error!Node.Idx {
    const start_pos = self.currentPosition();
    self.advance(); // consume 'while'

    const scratch_start = self.scratch_nodes.items.len;
    defer {
        self.scratch_nodes.items.len = scratch_start;
    }

    // Parse the condition
    const condition = try self.parseExpr();
    try self.scratch_nodes.append(self.gpa, condition);

    // Parse the body (just an expression, similar to lambda body)
    const body = try self.parseExpr();
    try self.scratch_nodes.append(self.gpa, body);

    const nodes = self.scratch_nodes.items[scratch_start..];

    // A while loop must have exactly condition and body
    if (nodes.len != 2) {
        return try self.ast.appendNode(self.gpa, start_pos, .malformed, .{ .malformed = .statement_unexpected_token });
    }

    const nodes_idx = try self.ast.appendNodeSlice(self.gpa, nodes);
    return try self.ast.appendNode(self.gpa, start_pos, .while_loop, .{ .nodes = nodes_idx });
}

fn parseReturn(self: *Parser) Error!Node.Idx {
    const start_pos = self.currentPosition();
    self.advance(); // consume return

    // Parse the expression to return
    const expr = try self.parseExpr();
    const expr_slice = [_]Node.Idx{expr};
    const nodes_idx = try self.ast.appendNodeSlice(self.gpa, &expr_slice);
    return try self.ast.appendNode(self.gpa, start_pos, .ret, .{ .nodes = nodes_idx });
}

fn parseCrash(self: *Parser) Error!Node.Idx {
    const start_pos = self.currentPosition();
    self.advance(); // consume crash

    // Parse the expression to crash with
    const expr = try self.parseExpr();
    const expr_slice = [_]Node.Idx{expr};
    const nodes_idx = try self.ast.appendNodeSlice(self.gpa, &expr_slice);
    return try self.ast.appendNode(self.gpa, start_pos, .crash, .{ .nodes = nodes_idx });
}

/// Parse a type annotation
pub fn parseTypeAnno(self: *Parser) Error!Node.Idx {
    return self.parseExpr();
}

/// Parse the right-hand side of a type annotation, handling special cases
/// like comma-separated parameters and where clauses
fn parseTypeAnnotationRHS(self: *Parser) Error!Node.Idx {
    // First, check if we have comma-separated type parameters
    // e.g., f(a), (a -> b) -> g(b)
    const scratch_start = self.scratch_nodes.items.len;
    defer {
        self.scratch_nodes.items.len = scratch_start;
    }

    // Parse the first type term (stops at commas and arrows)
    const first_term = try self.parseTypeTerm();
    try self.scratch_nodes.append(self.gpa, first_term);

    // Check for more comma-separated parameters
    while (self.peek() == .Comma) {
        self.advance();
        const term = try self.parseTypeTerm();
        try self.scratch_nodes.append(self.gpa, term);
    }

    const params = self.scratch_nodes.items[scratch_start..];

    // Now check for arrow to make this a function type
    var result: Node.Idx = undefined;
    if (self.peek() == .OpArrow or self.peek() == .OpFatArrow) {
        const is_effectful = self.peek() == .OpFatArrow;
        const arrow_pos = self.currentPosition();
        self.advance();

        // Parse the return type recursively
        const return_type = try self.parseTypeAnnotationRHS();

        // For now, always create regular binary arrows
        // TODO: Support variadic arrows properly
        const arrow_tag: Node.Tag = if (is_effectful) .binop_thick_arrow else .binop_thin_arrow;

        if (params.len == 1) {
            // Single parameter - simple case
            const binop_idx = try self.ast.appendBinOp(self.gpa, params[0], return_type);
            result = try self.ast.appendNode(self.gpa, arrow_pos, arrow_tag, .{ .binop = binop_idx });
        } else {
            // Multiple parameters - create nested arrows for now
            // a, b -> c becomes a -> (b -> c)
            var current = return_type;
            var i = params.len;
            while (i > 0) {
                i -= 1;
                const binop_idx = try self.ast.appendBinOp(self.gpa, params[i], current);
                current = try self.ast.appendNode(self.gpa, arrow_pos, arrow_tag, .{ .binop = binop_idx });
            }
            result = current;
        }
    } else if (params.len == 1) {
        // No arrow, just a single type expression
        result = params[0];
    } else {
        // Multiple params without arrow - shouldn't happen in valid syntax
        // but we'll create a tuple for error recovery
        const nodes_idx = try self.ast.appendNodeSlice(self.gpa, params);
        result = try self.ast.appendNode(self.gpa, self.currentPosition(), .tuple_literal, .{ .nodes = nodes_idx });
    }

    // Check for where clause
    if (self.peek() == .KwWhere) {
        const where_pos = self.currentPosition();
        self.advance(); // consume 'where'

        // Parse the constraint as a full expression (including colon as binop)
        // This will parse module(a).foo : c -> d as one expression
        const constraint = try self.parseExpr();

        // Create binop_where node with the type as LHS and constraint as RHS
        const binop_idx = try self.ast.appendBinOp(self.gpa, result, constraint);
        result = try self.ast.appendNode(self.gpa, where_pos, .binop_where, .{ .binop = binop_idx });
    }

    return result;
}

/// Parse a single type term (stops at commas and arrows with low precedence)
fn parseTypeTerm(self: *Parser) Error!Node.Idx {
    // In Roc, type applications must use parentheses: Maybe(a), not Maybe a
    // So we just parse a regular expression with appropriate precedence
    return self.parseExprWithBp(2);
}

// Operator precedence
const BindingPower = struct {
    left: u8,
    right: u8,
};

fn getBindingPower(tag: Token.Tag) BindingPower {
    return switch (tag) {
        .OpBar => .{ .left = 10, .right = 11 },
        .OpOr => .{ .left = 20, .right = 21 },
        .OpAnd => .{ .left = 30, .right = 31 },
        .OpEquals, .OpNotEquals => .{ .left = 40, .right = 41 },
        .OpLessThan, .OpLessThanOrEq, .OpGreaterThan, .OpGreaterThanOrEq => .{ .left = 50, .right = 51 },
        .OpPlus, .OpBinaryMinus, .OpUnaryMinus => .{ .left = 60, .right = 61 },
        .OpStar, .OpSlash, .OpDoubleSlash => .{ .left = 70, .right = 71 },
        .OpDoubleQuestion => .{ .left = 80, .right = 81 },
        .OpAssign => .{ .left = 5, .right = 6 },
        .OpColon => .{ .left = 3, .right = 4 },
        .OpArrow => .{ .left = 2, .right = 3 },
        .OpFatArrow => .{ .left = 1, .right = 2 },
        .KwAs => .{ .left = 3, .right = 4 }, // Low precedence, binds loosely
        .KwWhere => .{ .left = 1, .right = 2 }, // Very low precedence, like fat arrow
        else => .{ .left = 0, .right = 0 },
    };
}

fn tokenToBinOpTag(tag: Token.Tag) ?Node.Tag {
    return switch (tag) {
        .OpAssign => .binop_equals,
        .OpEquals => .binop_double_equals,
        .OpNotEquals => .binop_not_equals,
        .OpColon => .binop_colon,
        .OpPlus => .binop_plus,
        .OpBinaryMinus, .OpUnaryMinus => .binop_minus,
        .OpStar => .binop_star,
        .OpSlash => .binop_slash,
        .OpDoubleSlash => .binop_double_slash,
        .OpDoubleQuestion => .binop_double_question,
        .OpGreaterThan => .binop_gt,
        .OpGreaterThanOrEq => .binop_gte,
        .OpLessThan => .binop_lt,
        .OpLessThanOrEq => .binop_lte,
        .OpFatArrow => .binop_thick_arrow,
        .OpArrow => .binop_thin_arrow,
        .OpAnd => .binop_and,
        .OpOr => .binop_or,
        .OpBar => .binop_pipe,
        .KwAs => .binop_as,
        .KwWhere => .binop_where,
        else => null,
    };
}
