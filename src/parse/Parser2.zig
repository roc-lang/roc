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
const tokenize = @import("tokenize.zig");
const Ident = base.Ident;

const MAX_PARSE_DIAGNOSTICS: usize = 1_000;
const MAX_NESTING_LEVELS: u8 = 128;

/// A parser which tokenizes and parses source code into an abstract syntax tree.
pub const Parser = @This();

gpa: std.mem.Allocator,
pos: TokenIdx,
tok_buf: TokenizedBuffer,
ast: *AST,
scratch_nodes: std.ArrayListUnmanaged(Node.Idx),
diagnostics: std.ArrayListUnmanaged(AST.Diagnostic),
cached_malformed_node: ?Node.Idx,
nesting_counter: u8,

/// init the parser from a buffer of tokens
pub fn init(tokens: TokenizedBuffer, gpa: std.mem.Allocator, ast: *AST) std.mem.Allocator.Error!Parser {
    return Parser{
        .gpa = gpa,
        .pos = 0,
        .tok_buf = tokens,
        .ast = ast,
        .scratch_nodes = .{},
        .diagnostics = .{},
        .cached_malformed_node = null,
        .nesting_counter = MAX_NESTING_LEVELS,
    };
}

/// Deinit the parser.  The buffer of tokens and the AST are still owned by the caller.
pub fn deinit(parser: *Parser) void {
    parser.scratch_nodes.deinit(parser.gpa);
    // diagnostics will be kept and passed to the following compiler stage
    // to be deinitialized by the caller when no longer required
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

/// Convert a token index to a source position
fn tokenToPosition(self: *Parser, token_idx: TokenIdx) Position {
    const region = self.tok_buf.tokens.items(.region)[token_idx];
    return region.start;
}

/// Get the current token's position
fn currentPosition(self: *Parser) Position {
    return self.tokenToPosition(self.pos);
}

/// Get the identifier at the current position (if it's an identifier token)
fn currentIdent(self: *Parser) ?Ident.Idx {
    return self.tok_buf.resolveIdentifier(self.pos);
}

/// add a diagnostic error
pub fn pushDiagnostic(self: *Parser, tag: AST.Diagnostic.Tag, start_pos: Position, end_pos: Position) Error!void {
    if (self.diagnostics.items.len < MAX_PARSE_DIAGNOSTICS) {
        try self.diagnostics.append(self.gpa, .{ 
            .tag = tag, 
            .region = .{ .start = start_pos, .end = end_pos }
        });
    }
}

/// add a malformed node
pub fn pushMalformed(self: *Parser, tag: AST.Diagnostic.Tag, start_token: TokenIdx) Error!Node.Idx {
    const start_pos = self.tokenToPosition(start_token);
    const end_pos = self.currentPosition();
    
    if (self.peek() != .EndOfFile) {
        self.advance();
    }
    
    if (self.diagnostics.items.len < MAX_PARSE_DIAGNOSTICS) {
        try self.pushDiagnostic(tag, start_pos, end_pos);
        return try self.ast.appendNode(self.gpa, start_pos, .malformed, .{ .malformed = tag });
    } else {
        // Return a cached malformed node to avoid creating excessive nodes when diagnostic limit is exceeded
        if (self.cached_malformed_node == null) {
            self.cached_malformed_node = try self.ast.appendNode(
                self.gpa, 
                start_pos, 
                .malformed, 
                .{ .malformed = AST.Diagnostic.Tag.expr_unexpected_token }
            );
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
    defer { self.scratch_nodes.items.len = scratch_start; }
    
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
        _ = try self.ast.appendNode(
            self.gpa,
            self.tokenToPosition(0),
            .block,
            .{ .block_nodes = body_idx }
        );
    }
}

/// Parse module header
pub fn parseHeader(self: *Parser) Error!void {
    const start_pos = self.pos;
    
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
        }
    }
}

fn parseAppHeader(self: *Parser, start_token: TokenIdx) Error!AST.Header {
    const start_pos = self.tokenToPosition(start_token);
    
    // Parse provides
    self.expect(.OpenSquare) catch {
        try self.pushDiagnostic(.header_expected_open_square, start_pos, self.currentPosition());
        return AST.Header{ .app = .{
            .provides = @enumFromInt(0),
            .platform_idx = @enumFromInt(0),
            .packages = @enumFromInt(0),
            .region = start_pos,
        }};
    };
    
    const provides_idx = try self.parseExposedList(.CloseSquare);
    
    self.expect(.CloseSquare) catch {
        try self.pushDiagnostic(.header_expected_close_square, start_pos, self.currentPosition());
    };
    
    // Parse platform
    const platform_idx = try self.parsePlatformSpecification();
    
    // Parse packages (optional)
    const packages_idx = if (self.peek() == .OpenCurly) blk: {
        self.advance();
        const idx = try self.parsePackageList();
        self.expect(.CloseCurly) catch {
            try self.pushDiagnostic(.expected_packages_close_curly, start_pos, self.currentPosition());
        };
        break :blk idx;
    } else @as(AST.NodeSlices.Idx, @enumFromInt(0));
    
    return AST.Header{ .app = .{
        .provides = provides_idx,
        .platform_idx = platform_idx,
        .packages = packages_idx,
        .region = start_pos,
    }};
}

fn parseModuleHeader(self: *Parser, start_token: TokenIdx) Error!AST.Header {
    const start_pos = self.tokenToPosition(start_token);
    
    self.expect(.OpenSquare) catch {
        try self.pushDiagnostic(.header_expected_open_square, start_pos, self.currentPosition());
        return AST.Header{ .module = .{
            .exposes = @enumFromInt(0),
            .region = start_pos,
        }};
    };
    
    const exposes_idx = try self.parseExposedList(.CloseSquare);
    
    self.expect(.CloseSquare) catch {
        try self.pushDiagnostic(.header_expected_close_square, start_pos, self.currentPosition());
    };
    
    return AST.Header{ .module = .{
        .exposes = exposes_idx,
        .region = start_pos,
    }};
}

fn parsePackageHeader(self: *Parser, start_token: TokenIdx) Error!AST.Header {
    const start_pos = self.tokenToPosition(start_token);
    
    self.expect(.OpenSquare) catch {
        try self.pushDiagnostic(.header_expected_open_square, start_pos, self.currentPosition());
        return AST.Header{ .package = .{
            .exposes = @enumFromInt(0),
            .packages = @enumFromInt(0),
            .region = start_pos,
        }};
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
    }};
}

fn parsePlatformHeader(self: *Parser, start_token: TokenIdx) Error!AST.Header {
    const start_pos = self.tokenToPosition(start_token);
    
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
        }};
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
    
    const signatures_idx = try self.parseTypeAnno();
    
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
    }};
}

fn parseHostedHeader(self: *Parser, start_token: TokenIdx) Error!AST.Header {
    const start_pos = self.tokenToPosition(start_token);
    
    self.expect(.KwExposes) catch {
        try self.pushDiagnostic(.expected_exposes, start_pos, self.currentPosition());
        return AST.Header{ .hosted = .{
            .exposes = @enumFromInt(0),
            .region = start_pos,
        }};
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
    }};
}

fn parsePlatformSpecification(self: *Parser) Error!Node.Idx {
    const start = self.pos;
    
    // Parse platform specification like { pf: platform "..." } or just platform "..."
    if (self.peek() == .OpenCurly) {
        self.advance();
        
        const scratch_start = self.scratch_nodes.items.len;
        defer { self.scratch_nodes.items.len = scratch_start; }
        
        // Parse key-value pairs
        while (self.peek() != .CloseCurly and self.peek() != .EndOfFile) {
            // Parse field name
            if (self.peek() != .LowerIdent) {
                return self.pushMalformed(.expr_unexpected_token, start);
            }
            
            const field_ident = self.currentIdent();
            const field_pos = self.currentPosition();
            self.advance();
            
            self.expect(.OpColon) catch {
                return self.pushMalformed(.expected_colon_after_pat_field_name, start);
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
            try self.pushDiagnostic(.expected_expr_close_curly, self.tokenToPosition(start), self.currentPosition());
        };
        
        const nodes = self.scratch_nodes.items[scratch_start..];
        const nodes_idx = try self.ast.appendNodeSlice(self.gpa, nodes);
        return try self.ast.appendNode(self.gpa, self.tokenToPosition(start), .record_literal, .{ .block_nodes = nodes_idx });
    } else {
        // Just parse as expression (e.g., platform "...")
        return try self.parseExpr();
    }
}

fn parsePackageList(self: *Parser) Error!AST.NodeSlices.Idx {
    const scratch_start = self.scratch_nodes.items.len;
    defer { self.scratch_nodes.items.len = scratch_start; }
    
    while (self.peek() != .CloseCurly and self.peek() != .EndOfFile) {
        // Parse package name
        if (self.peek() != .LowerIdent) {
            _ = try self.pushMalformed(.expr_unexpected_token, self.pos);
            break;
        }
        
        const pkg_ident = self.currentIdent();
        const pkg_pos = self.currentPosition();
        self.advance();
        
        self.expect(.OpColon) catch {
            _ = try self.pushMalformed(.expected_colon_after_pat_field_name, self.pos);
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
    return try self.ast.appendNodeSlice(self.gpa, packages);
}

fn parseExposedList(self: *Parser, end_token: Token.Tag) Error!AST.NodeSlices.Idx {
    const scratch_start = self.scratch_nodes.items.len;
    defer { self.scratch_nodes.items.len = scratch_start; }
    
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
    return try self.ast.appendNodeSlice(self.gpa, items);
}

fn parseExposedItem(self: *Parser) Error!Node.Idx {
    const start = self.pos;
    
    switch (self.peek()) {
        .UpperIdent => {
            const ident = self.currentIdent();
            const pos = self.currentPosition();
            self.advance();
            
            if (ident) |id| {
                return try self.ast.appendNode(self.gpa, pos, .uc, .{ .ident = id });
            } else {
                return self.pushMalformed(.exposed_item_unexpected_token, start);
            }
        },
        .LowerIdent => {
            const ident = self.currentIdent();
            const pos = self.currentPosition();
            self.advance();
            
            if (ident) |id| {
                return try self.ast.appendNode(self.gpa, pos, .lc, .{ .ident = id });
            } else {
                return self.pushMalformed(.exposed_item_unexpected_token, start);
            }
        },
        else => return self.pushMalformed(.exposed_item_unexpected_token, start),
    }
}

fn parseTypeVariableList(self: *Parser) Error!AST.NodeSlices.Idx {
    const scratch_start = self.scratch_nodes.items.len;
    defer { self.scratch_nodes.items.len = scratch_start; }
    
    while (self.peek() != .CloseCurly and self.peek() != .EndOfFile) {
        const type_var = try self.parseTypeVariable();
        try self.scratch_nodes.append(self.gpa, type_var);
        
        if (self.peek() == .Comma) {
            self.advance();
        } else {
            break;
        }
    }
    
    const vars = self.scratch_nodes.items[scratch_start..];
    return try self.ast.appendNodeSlice(self.gpa, vars);
}

fn parseTypeVariable(self: *Parser) Error!Node.Idx {
    const start = self.pos;
    
    if (self.peek() == .LowerIdent) {
        const ident = self.currentIdent();
        const pos = self.currentPosition();
        self.advance();
        
        if (ident) |id| {
            return try self.ast.appendNode(self.gpa, pos, .lc, .{ .ident = id });
        } else {
            return self.pushMalformed(.invalid_type_arg, start);
        }
    } else {
        return self.pushMalformed(.invalid_type_arg, start);
    }
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
        else => return try self.parseExpr(),
    }
}

fn parseImport(self: *Parser) Error!?Node.Idx {
    const start_pos = self.currentPosition();
    self.advance(); // consume import
    
    const scratch_start = self.scratch_nodes.items.len;
    defer { self.scratch_nodes.items.len = scratch_start; }
    
    // Parse module path (e.g., pf.Stdout)
    var path_parts = std.ArrayList(u8).init(self.gpa);
    defer path_parts.deinit();
    
    while (true) {
        if (self.peek() == .LowerIdent or self.peek() == .UpperIdent) {
            const ident = self.currentIdent();
            if (ident) |id| {
                // TODO: Properly handle module path
                const node = try self.ast.appendNode(self.gpa, self.currentPosition(), .lc, .{ .ident = id });
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
        
        if (self.peek() == .LowerIdent or self.peek() == .UpperIdent) {
            const alias_ident = self.currentIdent();
            if (alias_ident) |id| {
                const alias_node = try self.ast.appendNode(self.gpa, self.currentPosition(), .lc, .{ .ident = id });
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
    const nodes_idx = try self.ast.appendNodeSlice(self.gpa, nodes);
    // TODO: Add import node type to AST2
    return try self.ast.appendNode(self.gpa, start_pos, .block, .{ .block_nodes = nodes_idx });
}

/// Parse a pattern
pub fn parsePattern(self: *Parser) Error!Node.Idx {
    const start = self.pos;
    
    return switch (self.peek()) {
        .LowerIdent => {
            const ident = self.currentIdent();
            const pos = self.currentPosition();
            self.advance();
            
            if (ident) |id| {
                return try self.ast.appendNode(self.gpa, pos, .lc, .{ .ident = id });
            } else {
                return self.pushMalformed(.pattern_unexpected_token, start);
            }
        },
        .UpperIdent => {
            const ident = self.currentIdent();
            const pos = self.currentPosition();
            self.advance();
            
            // Could be a tag pattern or start of a tag application
            if (ident) |id| {
                const tag_node = try self.ast.appendNode(self.gpa, pos, .uc, .{ .ident = id });
                
                // Check for tag application
                if (self.peek() == .OpenRound) {
                    return self.parseApply(tag_node);
                } else {
                    return tag_node;
                }
            } else {
                return self.pushMalformed(.pattern_unexpected_token, start);
            }
        },
        .Underscore => {
            const pos = self.currentPosition();
            self.advance();
            // TODO: handle underscore specially - for now use a dummy identifier
            const dummy_ident = @as(Ident.Idx, @bitCast(@as(u32, 0)));
            return try self.ast.appendNode(self.gpa, pos, .lc, .{ .ident = dummy_ident });
        },
        .StringStart => return self.parseStringPattern(),
        .Int, .Float => return self.parseNumLiteral(),
        .OpenSquare => return self.parseListPattern(),
        .OpenCurly => return self.parseRecordPattern(),
        .OpenRound => return self.parseTupleOrParenthesizedPattern(),
        else => return self.pushMalformed(.pattern_unexpected_token, start),
    };
}

fn parseStringPattern(self: *Parser) Error!Node.Idx {
    // String patterns are parsed the same as string expressions
    return self.parseStringExpr();
}

fn parseListPattern(self: *Parser) Error!Node.Idx {
    const start_pos = self.currentPosition();
    self.advance(); // consume [
    
    if (self.peek() == .CloseSquare) {
        self.advance();
        return try self.ast.appendNode(self.gpa, start_pos, .empty_list, .{ .src_bytes_end = self.currentPosition() });
    }
    
    const scratch_start = self.scratch_nodes.items.len;
    defer { self.scratch_nodes.items.len = scratch_start; }
    
    while (self.peek() != .CloseSquare and self.peek() != .EndOfFile) {
        // Check for list rest pattern
        if (self.peek() == .DoubleDot) {
            const rest_pos = self.currentPosition();
            self.advance();
            
            // Parse rest pattern (e.g., ..rest or .. as rest)
            if (self.peek() == .KwAs) {
                self.advance();
                const rest_pattern = try self.parsePattern();
                try self.scratch_nodes.append(self.gpa, rest_pattern);
            } else if (self.peek() == .LowerIdent) {
                // Handle ..rest syntax (which should be an error per snapshot)
                const rest_pattern = try self.parsePattern();
                try self.scratch_nodes.append(self.gpa, rest_pattern);
                try self.pushDiagnostic(.pattern_list_rest_old_syntax, rest_pos, self.currentPosition());
            } else {
                // Just .. without binding - use double_dot_lc with a dummy ident
                const dummy_ident = @as(Ident.Idx, @bitCast(@as(u32, 0)));
                const rest_node = try self.ast.appendNode(self.gpa, rest_pos, .double_dot_lc, .{ .ident = dummy_ident });
                try self.scratch_nodes.append(self.gpa, rest_node);
            }
            
            // Rest should be last element
            break;
        } else {
            const elem = try self.parsePattern();
            try self.scratch_nodes.append(self.gpa, elem);
        }
        
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
    const elems_idx = try self.ast.appendNodeSlice(self.gpa, elems);
    return try self.ast.appendNode(self.gpa, start_pos, .list_literal, .{ .block_nodes = elems_idx });
}

fn parseRecordPattern(self: *Parser) Error!Node.Idx {
    const start_pos = self.currentPosition();
    self.advance(); // consume {
    
    if (self.peek() == .CloseCurly) {
        self.advance();
        return try self.ast.appendNode(self.gpa, start_pos, .empty_record, .{ .src_bytes_end = self.currentPosition() });
    }
    
    const scratch_start = self.scratch_nodes.items.len;
    defer { self.scratch_nodes.items.len = scratch_start; }
    
    while (self.peek() != .CloseCurly and self.peek() != .EndOfFile) {
        // Parse field pattern (could be just name or name: pattern)
        if (self.peek() == .LowerIdent) {
            const field_ident = self.currentIdent();
            const field_pos = self.currentPosition();
            self.advance();
            
            if (field_ident) |id| {
                const field_node = try self.ast.appendNode(self.gpa, field_pos, .lc, .{ .ident = id });
                try self.scratch_nodes.append(self.gpa, field_node);
                
                // Check for : pattern
                if (self.peek() == .OpColon) {
                    self.advance();
                    const pattern = try self.parsePattern();
                    try self.scratch_nodes.append(self.gpa, pattern);
                }
            }
        } else {
            _ = try self.pushMalformed(.pattern_unexpected_token, self.pos);
            break;
        }
        
        if (self.peek() == .Comma) {
            self.advance();
        } else {
            break;
        }
    }
    
    self.expect(.CloseCurly) catch {
        try self.pushDiagnostic(.expected_expr_close_curly, start_pos, self.currentPosition());
    };
    
    const fields = self.scratch_nodes.items[scratch_start..];
    const fields_idx = try self.ast.appendNodeSlice(self.gpa, fields);
    return try self.ast.appendNode(self.gpa, start_pos, .record_literal, .{ .block_nodes = fields_idx });
}

fn parseTupleOrParenthesizedPattern(self: *Parser) Error!Node.Idx {
    const start_pos = self.currentPosition();
    self.advance(); // consume (
    
    const scratch_start = self.scratch_nodes.items.len;
    defer { self.scratch_nodes.items.len = scratch_start; }
    
    // Check for empty tuple
    if (self.peek() == .CloseRound) {
        self.advance();
        // Empty tuple pattern
        return try self.ast.appendNode(self.gpa, start_pos, .tuple_literal, .{ .block_nodes = @enumFromInt(0) });
    }
    
    const first = try self.parsePattern();
    try self.scratch_nodes.append(self.gpa, first);
    
    // Check if it's a tuple (has comma) or just parenthesized pattern
    if (self.peek() == .Comma) {
        self.advance();
        
        while (self.peek() != .CloseRound and self.peek() != .EndOfFile) {
            const elem = try self.parsePattern();
            try self.scratch_nodes.append(self.gpa, elem);
            
            if (self.peek() == .Comma) {
                self.advance();
            } else {
                break;
            }
        }
        
        self.expect(.CloseRound) catch {
            try self.pushDiagnostic(.expected_expr_close_round_or_comma, start_pos, self.currentPosition());
        };
        
        const elems = self.scratch_nodes.items[scratch_start..];
        const elems_idx = try self.ast.appendNodeSlice(self.gpa, elems);
        return try self.ast.appendNode(self.gpa, start_pos, .tuple_literal, .{ .block_nodes = elems_idx });
    } else {
        self.expect(.CloseRound) catch {
            try self.pushDiagnostic(.expected_expr_close_round_or_comma, start_pos, self.currentPosition());
        };
        
        // Just a parenthesized pattern
        return first;
    }
}

/// Parse an expression
pub fn parseExpr(self: *Parser) Error!Node.Idx {
    return self.parseExprWithBp(0);
}

/// Parse an expression with precedence
pub fn parseExprWithBp(self: *Parser, min_bp: u8) Error!Node.Idx {
    const start = self.pos;
    
    // Parse prefix/primary expression
    var left = try self.parsePrimaryExpr();
    
    // Parse infix operators
    while (true) {
        const op_tag = self.peek();
        const bp = getBindingPower(op_tag);
        
        if (bp.left <= min_bp) {
            break;
        }
        
        const op_pos = self.currentPosition();
        self.advance();
        
        const right = try self.parseExprWithBp(bp.right);
        
        const binop_tag = tokenToBinOpTag(op_tag) orelse {
            return self.pushMalformed(.expr_unexpected_token, start);
        };
        
        const binop_idx = try self.ast.appendBinOp(self.gpa, left, right);
        left = try self.ast.appendNode(self.gpa, op_pos, binop_tag, .{ .binop = binop_idx });
    }
    
    // Parse postfix operators
    while (true) {
        switch (self.peek()) {
            .OpenRound => {
                left = try self.parseApply(left);
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
                        return self.pushMalformed(.expr_dot_suffix_not_allowed, start);
                    }
                } else if (self.peek() == .Int or self.peek() == .Float) {
                    const dot_field_pos = self.currentPosition();
                    
                    const num = try self.parseNumLiteral();
                    const binop_idx = try self.ast.appendBinOp(self.gpa, left, num);
                    left = try self.ast.appendNode(self.gpa, dot_field_pos, .binop_pipe, .{ .binop = binop_idx });
                } else {
                    return self.pushMalformed(.expr_dot_suffix_not_allowed, start);
                }
            },
            else => break,
        }
    }
    
    return left;
}

fn parsePrimaryExpr(self: *Parser) Error!Node.Idx {
    const start = self.pos;
    
    return switch (self.peek()) {
        .LowerIdent => {
            const ident = self.currentIdent();
            const pos = self.currentPosition();
            self.advance();
            
            if (ident) |id| {
                return try self.ast.appendNode(self.gpa, pos, .lc, .{ .ident = id });
            } else {
                return self.pushMalformed(.expr_unexpected_token, start);
            }
        },
        .UpperIdent => {
            const ident = self.currentIdent();
            const pos = self.currentPosition();
            self.advance();
            
            if (ident) |id| {
                return try self.ast.appendNode(self.gpa, pos, .uc, .{ .ident = id });
            } else {
                return self.pushMalformed(.expr_unexpected_token, start);
            }
        },
        .Int, .Float => return self.parseNumLiteral(),
        .StringStart => return self.parseStringExpr(),
        .OpenSquare => return self.parseListLiteral(),
        .OpenCurly => return self.parseBlockOrRecord(),
        .OpenRound => return self.parseTupleOrParenthesized(),
        .KwIf => return self.parseIf(),
        .KwMatch => return self.parseMatch(),
        .OpBar => return self.parseLambda(),
        .KwVar => return self.parseVar(),
        .KwFor => return self.parseFor(),
        .KwReturn => return self.parseReturn(),
        .KwCrash => return self.parseCrash(),
        .OpBang => {
            const pos = self.currentPosition();
            self.advance();
            
            _ = try self.parseExprWithBp(100); // High precedence for unary
            return try self.ast.appendNode(self.gpa, pos, .unary_not, .{ .src_bytes_end = pos });
        },
        .OpBinaryMinus, .OpUnaryMinus => {
            const pos = self.currentPosition();
            self.advance();
            
            _ = try self.parseExprWithBp(100); // High precedence for unary
            return try self.ast.appendNode(self.gpa, pos, .unary_neg, .{ .src_bytes_end = pos });
        },
        else => return self.pushMalformed(.expr_unexpected_token, start),
    };
}

fn parseNumLiteral(self: *Parser) Error!Node.Idx {
    const pos = self.currentPosition();
    const tag = self.peek();
    
    // Get the actual token text to parse the number
    const token_region = self.tok_buf.tokens.items(.region)[self.pos];
    const token_start = token_region.start.offset;
    const token_end = token_region.end.offset;
    
    const num_str = self.tok_buf.env.source[token_start..token_end];
    self.advance();
    
    // Parse based on token type
    if (tag == .Float) {
        // Parse as float
        const value = std.fmt.parseFloat(f64, num_str) catch 0.0;
        const bits = @as(u64, @bitCast(value));
        if (bits <= 0xFFFFFFFF) {
            // For now, store floats as i32
            return try self.ast.appendNode(self.gpa, pos, .num_literal_i32, .{ .num_literal_i32 = @as(i32, @intCast(bits)) });
        } else {
            // Store as big float
            const bytes = std.mem.asBytes(&value);
            const bytes_idx = try self.ast.appendByteSlice(self.gpa, bytes);
            return try self.ast.appendNode(self.gpa, pos, .num_literal_big, .{ .num_literal_big = bytes_idx });
        }
    } else {
        // Parse as integer
        const value = std.fmt.parseInt(i64, num_str, 10) catch 0;
        if (value >= std.math.minInt(i32) and value <= std.math.maxInt(i32)) {
            return try self.ast.appendNode(self.gpa, pos, .num_literal_i32, .{ .num_literal_i32 = @as(i32, @intCast(value)) });
        } else {
            // Store as big integer
            const bytes = std.mem.asBytes(&value);
            const bytes_idx = try self.ast.appendByteSlice(self.gpa, bytes);
            return try self.ast.appendNode(self.gpa, pos, .num_literal_big, .{ .num_literal_big = bytes_idx });
        }
    }
}

fn parseStringExpr(self: *Parser) Error!Node.Idx {
    const start_pos = self.currentPosition();
    const start = self.pos;
    
    self.expect(.StringStart) catch {
        return self.pushMalformed(.expr_unexpected_token, start);
    };
    
    const scratch_start = self.scratch_nodes.items.len;
    defer { self.scratch_nodes.items.len = scratch_start; }
    
    var total_bytes: usize = 0;
    
    // Parse string parts
    while (self.peek() != .StringEnd and self.peek() != .EndOfFile) {
        switch (self.peek()) {
            .StringPart => {
                // Get the string content from the token
                const token_region = self.tok_buf.tokens.items(.region)[self.pos];
                const token_start = token_region.start.offset;
                const token_end = token_region.end.offset;
                
                const str_bytes = self.tok_buf.env.source[token_start..token_end];
                total_bytes += str_bytes.len;
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
        return self.pushMalformed(.string_unclosed, start);
    };
    
    // For now, create a simple string literal node
    // TODO: Properly handle string content and interpolation
    if (total_bytes <= 3) {  // str_literal_small is [4]u8 with null terminator
        const small_bytes: [4]u8 = .{0} ** 4;
        // TODO: Actually copy the string bytes
        return try self.ast.appendNode(self.gpa, start_pos, .str_literal_small, .{ .str_literal_small = small_bytes });
    } else {
        // For larger strings, we need to store in ByteSlices
        const dummy_bytes = try self.gpa.alloc(u8, total_bytes);
        defer self.gpa.free(dummy_bytes);
        @memset(dummy_bytes, 'x'); // Placeholder
        
        const bytes_idx = try self.ast.appendByteSlice(self.gpa, dummy_bytes);
        return try self.ast.appendNode(self.gpa, start_pos, .str_literal_big, .{ .str_literal_big = bytes_idx });
    }
}

fn parseListLiteral(self: *Parser) Error!Node.Idx {
    const start_pos = self.currentPosition();
    self.advance(); // consume [
    
    if (self.peek() == .CloseSquare) {
        self.advance();
        return try self.ast.appendNode(self.gpa, start_pos, .empty_list, .{ .src_bytes_end = self.currentPosition() });
    }
    
    const scratch_start = self.scratch_nodes.items.len;
    defer { self.scratch_nodes.items.len = scratch_start; }
    
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
    _ = try self.ast.appendNodeSlice(self.gpa, elems);
    return try self.ast.appendNode(self.gpa, start_pos, .list_literal, .{ .list_elems = @intCast(elems.len) });
}

fn parseBlockOrRecord(self: *Parser) Error!Node.Idx {
    const start_pos = self.currentPosition();
    self.advance(); // consume {
    
    if (self.peek() == .CloseCurly) {
        self.advance();
        return try self.ast.appendNode(self.gpa, start_pos, .empty_record, .{ .src_bytes_end = self.currentPosition() });
    }
    
    const scratch_start = self.scratch_nodes.items.len;
    defer { self.scratch_nodes.items.len = scratch_start; }
    
    // Parse first element to determine if it's a block or record
    const first_elem = try self.parseExpr();
    try self.scratch_nodes.append(self.gpa, first_elem);
    
    const is_record = self.peek() == .Comma;
    
    if (is_record) {
        self.advance(); // consume comma
        
        while (self.peek() != .CloseCurly and self.peek() != .EndOfFile) {
            const field = try self.parseExpr();
            try self.scratch_nodes.append(self.gpa, field);
            
            if (self.peek() == .Comma) {
                self.advance();
            } else {
                break;
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
    return try self.ast.appendNode(self.gpa, start_pos, tag, .{ .block_nodes = nodes_idx });
}

fn parseTupleOrParenthesized(self: *Parser) Error!Node.Idx {
    const start_pos = self.currentPosition();
    self.advance(); // consume (
    
    const scratch_start = self.scratch_nodes.items.len;
    defer { self.scratch_nodes.items.len = scratch_start; }
    
    // Check for empty tuple
    if (self.peek() == .CloseRound) {
        self.advance();
        return try self.ast.appendNode(self.gpa, start_pos, .tuple_literal, .{ .block_nodes = @enumFromInt(0) });
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
            try self.pushDiagnostic(.expected_expr_close_round_or_comma, start_pos, self.currentPosition());
        };
        
        const elems = self.scratch_nodes.items[scratch_start..];
        const elems_idx = try self.ast.appendNodeSlice(self.gpa, elems);
        return try self.ast.appendNode(self.gpa, start_pos, .tuple_literal, .{ .block_nodes = elems_idx });
    } else {
        self.expect(.CloseRound) catch {
            try self.pushDiagnostic(.expected_expr_close_round_or_comma, start_pos, self.currentPosition());
        };
        
        // Just a parenthesized expression
        return first;
    }
}

fn parseApply(self: *Parser, func: Node.Idx) Error!Node.Idx {
    const start_pos = self.ast.start(func);
    self.advance(); // consume (
    
    const scratch_start = self.scratch_nodes.items.len;
    defer { self.scratch_nodes.items.len = scratch_start; }
    
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
    const nodes_idx = try self.ast.appendNodeSlice(self.gpa, nodes);
    return try self.ast.appendNode(self.gpa, start_pos, .apply, .{ .block_nodes = nodes_idx });
}

fn parseIf(self: *Parser) Error!Node.Idx {
    const start_pos = self.currentPosition();
    self.advance(); // consume if
    
    const scratch_start = self.scratch_nodes.items.len;
    defer { self.scratch_nodes.items.len = scratch_start; }
    
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
        _ = try self.ast.appendNodeSlice(self.gpa, nodes);
        return try self.ast.appendNode(self.gpa, start_pos, .if_else, .{ .if_branches = 1 });
    } else {
        const nodes = self.scratch_nodes.items[scratch_start..];
        _ = try self.ast.appendNodeSlice(self.gpa, nodes);
        return try self.ast.appendNode(self.gpa, start_pos, .if_without_else, .{ .if_branches = 1 });
    }
}

fn parseMatch(self: *Parser) Error!Node.Idx {
    const start_pos = self.currentPosition();
    self.advance(); // consume when
    
    const scratch_start = self.scratch_nodes.items.len;
    defer { self.scratch_nodes.items.len = scratch_start; }
    
    // Parse scrutinee
    const scrutinee = try self.parseExpr();
    try self.scratch_nodes.append(self.gpa, scrutinee);
    
    self.expect(.OpArrow) catch {
        try self.pushDiagnostic(.expected_open_curly_after_match, start_pos, self.currentPosition());
    };
    
    // Parse branches
    var branch_count: u32 = 0;
    while (self.peek() != .EndOfFile) {
        const pattern = try self.parsePattern();
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
    _ = try self.ast.appendNodeSlice(self.gpa, nodes);
    return try self.ast.appendNode(self.gpa, start_pos, .match, .{ .match_branches = branch_count });
}

fn parseLambda(self: *Parser) Error!Node.Idx {
    const start_pos = self.currentPosition();
    self.advance(); // consume |
    
    const scratch_start = self.scratch_nodes.items.len;
    defer { self.scratch_nodes.items.len = scratch_start; }
    
    // Parse parameters
    while (self.peek() != .OpBar and self.peek() != .EndOfFile) {
        const param = try self.parsePattern();
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
    var body_then_args = try self.gpa.alloc(Node.Idx, 1 + params.len);
    defer self.gpa.free(body_then_args);
    
    body_then_args[0] = body;
    @memcpy(body_then_args[1..], params);
    
    const nodes_idx = try self.ast.appendNodeSlice(self.gpa, body_then_args);
    return try self.ast.appendNode(self.gpa, start_pos, .lambda, .{ .body_then_args = nodes_idx });
}

fn parseVar(self: *Parser) Error!Node.Idx {
    const start_pos = self.currentPosition();
    const start = self.pos;
    self.advance(); // consume var
    
    if (self.peek() != .LowerIdent) {
        return self.pushMalformed(.var_must_have_ident, start);
    }
    
    const ident = self.currentIdent();
    self.advance();
    
    if (ident) |id| {
        return try self.ast.appendNode(self.gpa, start_pos, .var_lc, .{ .ident = id });
    } else {
        return self.pushMalformed(.var_must_have_ident, start);
    }
}

fn parseFor(self: *Parser) Error!Node.Idx {
    _ = self;
    @panic("TODO: parseFor");
}

fn parseWhile(self: *Parser) Error!Node.Idx {
    _ = self;
    @panic("TODO: parseWhile");
}

fn parseReturn(self: *Parser) Error!Node.Idx {
    const start_pos = self.currentPosition();
    self.advance(); // consume return
    
    _ = try self.parseExpr();
    return try self.ast.appendNode(self.gpa, start_pos, .ret, .{ .src_bytes_end = self.currentPosition() });
}

fn parseCrash(self: *Parser) Error!Node.Idx {
    const start_pos = self.currentPosition();
    self.advance(); // consume crash
    
    const expr = try self.parseExpr();
    _ = expr;
    return try self.ast.appendNode(self.gpa, start_pos, .crash, .{ .src_bytes_end = self.currentPosition() });
}

/// Parse a type annotation
pub fn parseTypeAnno(self: *Parser) Error!Node.Idx {
    return self.parseTypeAnnoWithBp(0);
}

fn parseTypeAnnoWithBp(self: *Parser, min_bp: u8) Error!Node.Idx {
    const start = self.pos;
    
    // Parse primary type
    var left = try self.parsePrimaryType();
    
    // Parse type operators
    while (true) {
        const op_tag = self.peek();
        const bp = getBindingPower(op_tag);
        
        if (bp.left <= min_bp) {
            break;
        }
        
        const op_pos = self.currentPosition();
        self.advance();
        
        const right = try self.parseTypeAnnoWithBp(bp.right);
        
        const binop_tag = tokenToBinOpTag(op_tag) orelse {
            return self.pushMalformed(.ty_anno_unexpected_token, start);
        };
        
        const binop_idx = try self.ast.appendBinOp(self.gpa, left, right);
        left = try self.ast.appendNode(self.gpa, op_pos, binop_tag, .{ .binop = binop_idx });
    }
    
    return left;
}

fn parsePrimaryType(self: *Parser) Error!Node.Idx {
    const start = self.pos;
    
    return switch (self.peek()) {
        .LowerIdent => {
            const ident = self.currentIdent();
            const pos = self.currentPosition();
            self.advance();
            
            if (ident) |id| {
                return try self.ast.appendNode(self.gpa, pos, .lc, .{ .ident = id });
            } else {
                return self.pushMalformed(.ty_anno_unexpected_token, start);
            }
        },
        .UpperIdent => {
            const ident = self.currentIdent();
            const pos = self.currentPosition();
            self.advance();
            
            if (ident) |id| {
                const type_node = try self.ast.appendNode(self.gpa, pos, .uc, .{ .ident = id });
                
                // Check for type application
                if (self.peek() == .OpenRound) {
                    return self.parseTypeApply(type_node);
                } else {
                    return type_node;
                }
            } else {
                return self.pushMalformed(.ty_anno_unexpected_token, start);
            }
        },
        .OpenCurly => return self.parseRecordType(),
        .OpenSquare => return self.parseListType(),
        .OpenRound => return self.parseTupleOrParenthesizedType(),
        else => return self.pushMalformed(.ty_anno_unexpected_token, start),
    };
}

fn parseTypeApply(self: *Parser, type_ctor: Node.Idx) Error!Node.Idx {
    _ = self;
    _ = type_ctor;
    @panic("TODO: parseTypeApply");
}

fn parseRecordType(self: *Parser) Error!Node.Idx {
    _ = self;
    @panic("TODO: parseRecordType");
}

fn parseListType(self: *Parser) Error!Node.Idx {
    _ = self;
    @panic("TODO: parseListType");
}

fn parseTupleOrParenthesizedType(self: *Parser) Error!Node.Idx {
    _ = self;
    @panic("TODO: parseTupleOrParenthesizedType");
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
        else => null,
    };
}