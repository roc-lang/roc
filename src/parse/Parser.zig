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
const Node = AST.Node;
const Region = base.Region;
const Position = base.Region.Position;
const TokenizedBuffer = tokenize.TokenizedBuffer;
const Token = tokenize.Token;
const TokenIdx = Token.Idx;
const tokenize = @import("tokenize.zig");
const tokenize_iter = @import("tokenize.zig");
const Ident = base.Ident;

const MAX_PARSE_DIAGNOSTICS: usize = 1_000;
const MAX_PARSE_STACK_SIZE: usize = 10_000;

/// Helper to create a Region from start and end positions
fn makeRegion(start: Position, end: Position) Region {
    // In debug mode, catch bugs where end is before start
    if (std.debug.runtime_safety) {
        if (end.offset < start.offset) {
            std.debug.print("makeRegion error: end.offset ({}) < start.offset ({})\n", .{ end.offset, start.offset });
            std.debug.assert(end.offset >= start.offset);
        }
    }
    return Region{ .start = start, .end = end };
}

/// Info about an operator for precedence parsing
const OpInfo = struct {
    left: ?Node.Idx, // null for unary operators
    op_tag: Token.Tag,
    op_region: Region,
    min_bp: u8,
};

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
source: []const u8, // Source code for extracting string literals
env: *base.CommonEnv, // Shared environment for identifiers, strings, etc.
current: ?Token = null, // Current token
lookahead: ?Token = null, // Lookahead token (LL(1))
last_position: Position, // Track last valid position for error reporting
last_region: Region, // Track last consumed token's region
ast: *AST,
byte_slices: *collections.ByteSlices, // Reference to tokenizer's ByteSlices
scratch_nodes: std.ArrayListUnmanaged(Node.Idx),
scratch_op_stack: std.ArrayListUnmanaged(OpInfo), // For operator precedence parsing
scratch_bytes: std.ArrayListUnmanaged(u8), // For string building
diagnostics: std.ArrayListUnmanaged(AST.Diagnostic),
// Token-fed parsing state
needs_token: bool = false, // Set to true when advance() needs a new token
token_iterator: ?*tokenize_iter.TokenIterator = null, // Optional token iterator for parseExprFromSource
// Lambda parsing context
is_in_lambda_args: bool = false, // Track when we're parsing lambda parameters for record shorthand

// State machine fields for stack-safe parsing
state_stack: std.ArrayListUnmanaged(ParseState),
value_stack: std.ArrayListUnmanaged(Node.Idx),
min_bp_stack: std.ArrayListUnmanaged(u8), // For operator precedence

/// Parsing states for the state machine
pub const ParseState = enum {
    // Top level
    file_start,
    file_header_done,
    file_stmt_loop,
    file_done,

    // Headers
    header_check,
    header_app,
    header_app_after_exposes,
    header_app_finish,
    header_module,
    header_package,
    header_platform,
    header_hosted,
    header_parse_exposed_list,
    header_parse_packages,

    // Statements
    stmt_start,
    stmt_check_colon,
    stmt_type_anno,
    stmt_type_anno_complete,
    stmt_complete,

    // Type expressions
    type_expr_start,

    // Expressions with precedence
    expr_start,
    expr_primary,
    expr_check_binop,
    expr_binop_rhs,
    expr_binop_combine,
    expr_combine,
    expr_done,
    expr_unary_complete,

    // Primary expressions
    primary_ident,
    primary_number,
    primary_string,
    primary_list,
    primary_record,
    primary_if,
    primary_match,
    primary_lambda,

    // Lambda parsing
    lambda_args,
    lambda_body,
    lambda_complete,

    // If expression parsing
    if_arrow,
    if_then_branch,
    if_else_branch,
    if_complete,

    // Match expression parsing
    match_branches,
    match_branch_start,
    pattern_start,
    pattern_tag_payload_close,
    match_branch_arrow,
    match_branch_complete,
    match_branches_cont,
    match_complete,

    // Loop parsing
    while_body,
    while_complete,
    for_in,
    for_body,
    for_complete,

    // Return/Crash
    return_complete,
    crash_complete,

    // List parsing
    list_elements,
    list_elements_cont,
    list_complete,

    // Parenthesized/Tuple parsing
    paren_content,
    paren_check_comma,
    paren_complete,

    // Record parsing
    record_fields,
    record_field_start,
    record_field_value,
    record_fields_cont,
    record_complete,

    // Utilities
    done,
    parse_error,
};

pub const StateValue = union(enum) {
    none: void,
    node: Node.Idx,
    nodes: []Node.Idx,
    region: Region,
    ident: Ident.Idx,
    precedence: u8,
};
/// Initialize a state machine parser
fn initStateMachine(env: *base.CommonEnv, gpa: std.mem.Allocator, source: []const u8, ast: *AST, byte_slices: *collections.ByteSlices) !Parser {
    return Parser{
        .gpa = gpa,
        .source = source,
        .env = env,
        .last_position = Position{ .offset = 0 },
        .last_region = Region{ .start = Position{ .offset = 0 }, .end = Position{ .offset = 0 } },
        .ast = ast,
        .byte_slices = byte_slices,
        .scratch_nodes = .{},
        .scratch_op_stack = .{},
        .scratch_bytes = .{},
        .diagnostics = .{},
        .state_stack = .{},
        .value_stack = .{},
        .min_bp_stack = .{},
    };
}

/// Parse and collect tokens - this is the main driver for token-fed parsing
/// Returns all tokens encountered during parsing (including comments)
pub fn parseAndCollectTokens(
    env: *base.CommonEnv,
    allocator: std.mem.Allocator,
    source: []const u8,
    messages: []tokenize_iter.Diagnostic,
    ast: *AST,
    byte_slices: *collections.ByteSlices,
) !struct { tokens: std.ArrayList(Token), root: ?Node.Idx } {
    // Initialize the parser in token-fed mode
    var parser = try initStateMachine(env, allocator, source, ast, byte_slices);
    defer parser.deinit();

    // Create tokenizer
    var token_iter = try tokenize_iter.TokenIterator.init(env, allocator, source, messages, byte_slices);
    defer token_iter.deinit(allocator);

    // Collect all tokens including comments
    // Pre-allocate based on source size heuristic (roughly 1 token per 10 bytes)
    var tokens = std.ArrayList(Token).init(allocator);
    errdefer tokens.deinit();
    try tokens.ensureTotalCapacity(@max(256, source.len / 10));

    // Helper to get next non-comment token while saving all tokens
    const getNextNonComment = struct {
        fn next(iter: *tokenize_iter.TokenIterator, alloc: std.mem.Allocator, token_list: *std.ArrayList(Token)) !?Token {
            while (true) {
                const token = try iter.next(alloc) orelse return null;
                try token_list.append(token);

                switch (token.tag) {
                    .LineComment, .DocComment, .BlankLine => continue, // Skip comments and blank lines for parsing
                    else => return token,
                }
            }
        }
    }.next;

    // Get first two NON-COMMENT tokens to start
    const first = try getNextNonComment(&token_iter, allocator, &tokens) orelse
        return .{ .tokens = tokens, .root = null };

    const second = try getNextNonComment(&token_iter, allocator, &tokens);

    var current = first;
    var lookahead = second;

    // Simple driver loop: feed current+lookahead, advance, repeat
    while (true) {
        // Feed current and lookahead to parser (only non-comment tokens)
        const needs_more = try parser.chompToken(current, lookahead);

        if (!needs_more) {
            // Parsing complete
            break;
        }

        // Check if we're at EOF
        if (lookahead == null or lookahead.?.tag == .EndOfFile) {
            // Feed EOF one more time to complete parsing
            _ = try parser.chompToken(current, lookahead);
            break;
        }

        // Advance: current becomes lookahead, get new non-comment lookahead
        current = lookahead.?;
        lookahead = try getNextNonComment(&token_iter, allocator, &tokens);
    }

    // Get the root node from the value stack (if any)
    const root = if (parser.value_stack.items.len > 0)
        parser.value_stack.items[parser.value_stack.items.len - 1]
    else
        null;

    return .{ .tokens = tokens, .root = root };
}

/// Compatibility init function for existing code
/// Creates a parser that expects to be used with parseAndCollectTokens
pub fn init(
    env: *base.CommonEnv,
    allocator: std.mem.Allocator,
    source: []const u8,
    messages: []tokenize_iter.Diagnostic,
    ast: *AST,
    byte_slices: *collections.ByteSlices,
) !Parser {
    _ = messages;
    return initStateMachine(env, allocator, source, ast, byte_slices);
}

// Helper functions for type conversions
inline fn nodeIdxToInt(idx: Node.Idx) i32 {
    return @intFromEnum(idx);
}

inline fn intToNodeIdx(val: i32) Node.Idx {
    return @enumFromInt(val);
}

inline fn nodeSlicesIdxFromInt(val: u32) collections.NodeSlices(Node.Idx).Idx {
    return @enumFromInt(val);
}

inline fn positionToInt(pos: Position) i32 {
    return @intCast(pos.offset);
}

inline fn intToPosition(val: i32) Position {
    return Position{ .offset = @intCast(val) };
}

fn handleInitial(self: *Parser) !void {
    _ = self.state_stack.pop();
    try self.state_stack.append(self.gpa, .expect_header_or_stmt);
}

fn handleExpectHeaderOrStmt(self: *Parser) !void {
    const curr = self.current.?;

    switch (curr.tag) {
        .KwApp => {
            _ = self.state_stack.pop();
            try self.state_stack.append(self.gpa, .parsing_header_app);
            self.advance();
        },
        .KwModule => {
            _ = self.state_stack.pop();
            try self.state_stack.append(self.gpa, .parsing_header_module);
            self.advance();
        },
        .KwPackage => {
            _ = self.state_stack.pop();
            try self.state_stack.append(self.gpa, .parsing_header_package);
            self.advance();
        },
        .KwPlatform => {
            _ = self.state_stack.pop();
            try self.state_stack.append(self.gpa, .parsing_header_platform);
            self.advance();
        },
        .KwHosted => {
            _ = self.state_stack.pop();
            try self.state_stack.append(self.gpa, .parsing_header_hosted);
            self.advance();
        },
        .EndOfFile => {
            _ = self.state_stack.pop();
            try self.state_stack.append(self.gpa, .done);
        },
        else => {
            _ = self.state_stack.pop();
            try self.state_stack.append(self.gpa, .parsing_stmt);
        },
    }
}

fn handleParsingHeaderApp(self: *Parser) !void {
    // Parse app header: app [exports] { packages }
    if (self.peek() == .OpenSquare) {
        _ = self.state_stack.pop();
        try self.state_stack.append(self.gpa, .parsing_packages_record);
        try self.state_stack.append(self.gpa, .parsing_exports_list);
    } else if (self.peek() == .OpenCurly) {
        _ = self.state_stack.pop();
        try self.state_stack.append(self.gpa, .parsing_packages_record);
    } else {
        // Error: expected [ or {
        try self.pushDiagnostic(.expected_header_app_open, self.currentPosition(), self.currentPosition());
        _ = self.state_stack.pop();
        try self.state_stack.append(self.gpa, .parse_error);
    }
}

fn handleParsingHeaderModule(self: *Parser) !void {
    // Parse module header: module [exports]
    if (self.peek() == .OpenSquare) {
        _ = self.state_stack.pop();
        try self.state_stack.append(self.gpa, .parsing_stmt); // After exports, parse statements
        try self.state_stack.append(self.gpa, .parsing_exports_list);
    } else {
        // Error: expected [
        try self.pushDiagnostic(.expected_header_module_open, self.currentPosition(), self.currentPosition());
        _ = self.state_stack.pop();
        try self.state_stack.append(self.gpa, .parse_error);
    }
}

fn handleParsingHeaderPackage(self: *Parser) !void {
    // Similar to module
    if (self.peek() == .OpenSquare) {
        _ = self.state_stack.pop();
        try self.state_stack.append(self.gpa, .parsing_packages_record);
        try self.state_stack.append(self.gpa, .parsing_exports_list);
    } else {
        try self.pushDiagnostic(.expected_header_package_open, self.currentPosition(), self.currentPosition());
        _ = self.state_stack.pop();
        try self.state_stack.append(self.gpa, .parse_error);
    }
}

fn handleParsingHeaderPlatform(self: *Parser) !void {
    // Parse platform header
    if (self.peek() == .OpenSquare) {
        _ = self.state_stack.pop();
        try self.state_stack.append(self.gpa, .parsing_packages_record);
        try self.state_stack.append(self.gpa, .parsing_exports_list);
    } else {
        try self.pushDiagnostic(.expected_header_platform_open, self.currentPosition(), self.currentPosition());
        _ = self.state_stack.pop();
        try self.state_stack.append(self.gpa, .parse_error);
    }
}

fn handleParsingHeaderHosted(self: *Parser) !void {
    // Parse hosted header
    if (self.peek() == .OpenSquare) {
        _ = self.state_stack.pop();
        try self.state_stack.append(self.gpa, .parsing_stmt);
        try self.state_stack.append(self.gpa, .parsing_exports_list);
    } else {
        try self.pushDiagnostic(.expected_header_hosted_open, self.currentPosition(), self.currentPosition());
        _ = self.state_stack.pop();
        try self.state_stack.append(self.gpa, .parse_error);
    }
}

fn handleParsingExportsList(self: *Parser) !void {
    // Parse [ident, ident, ...]
    if (self.peek() != .OpenSquare) {
        _ = self.state_stack.pop();
        return;
    }

    self.advance(); // consume [

    // Collect exports
    while (self.peek() != .CloseSquare and self.peek() != .EndOfFile) {
        if (self.peek() == .LowerIdent or self.peek() == .UpperIdent) {
            const ident = self.currentIdent();
            const region = self.currentRegion();
            self.advance();

            // Check for ! suffix
            const is_effectful = self.peek() == .OpBang;
            if (is_effectful) {
                self.advance();
            }

            // Create node for export
            if (ident) |id| {
                const tag: Node.Tag = if (is_effectful) .not_lc else .lc;
                const node = try self.ast.appendNode(self.gpa, region, tag, .{ .ident = id });
                try self.scratch_nodes.append(self.gpa, node);
            }
        }

        if (self.peek() == .Comma) {
            self.advance();
        } else {
            break;
        }
    }

    if (self.peek() == .CloseSquare) {
        self.advance();
    }

    _ = self.state_stack.pop();
}

fn handleParsingPackagesRecord(self: *Parser) !void {
    // Parse { name: "value", ... }
    if (self.peek() != .OpenCurly) {
        _ = self.state_stack.pop();
        return;
    }

    self.advance(); // consume {

    while (self.peek() != .CloseCurly and self.peek() != .EndOfFile) {
        // Parse field name
        if (self.peek() == .LowerIdent) {
            self.advance();

            if (self.peek() == .OpColon) {
                self.advance();

                // Parse field value
                try self.state_stack.append(self.gpa, .parsing_expr);
            }
        }

        if (self.peek() == .Comma) {
            self.advance();
        } else {
            break;
        }
    }

    if (self.peek() == .CloseCurly) {
        self.advance();
    }

    _ = self.state_stack.pop();
    // After packages, parse statements
    try self.state_stack.append(self.gpa, .parsing_stmt);
}

fn handleParsingStmt(self: *Parser) !void {
    if (self.peek() == .EndOfFile) {
        _ = self.state_stack.pop();
        try self.state_stack.append(self.gpa, .done);
        return;
    }

    // Parse a statement by parsing an expression
    try self.state_stack.append(self.gpa, .parsing_expr);
}

fn handleParsingExpr(self: *Parser) !void {
    // Handle expression parsing state transition
    // This function manages the parsing state stack during expression parsing
    if (self.current) |_| {
        self.advance();
    }
    _ = self.state_stack.pop();
}

fn handleParsingBinOp(self: *Parser) !void {
    _ = self.state_stack.pop();
}

fn handleParsingApply(self: *Parser) !void {
    _ = self.state_stack.pop();
}

fn handleParsingList(self: *Parser) !void {
    _ = self.state_stack.pop();
}

fn handleParsingRecord(self: *Parser) !void {
    _ = self.state_stack.pop();
}

fn handleParsingIf(self: *Parser) !void {
    _ = self.state_stack.pop();
}

fn handleParsingMatch(self: *Parser) !void {
    _ = self.state_stack.pop();
}

fn handleParsingLambda(self: *Parser) !void {
    _ = self.state_stack.pop();
}

/// Peek at current token tag (internal use only)
fn peek(self: *Parser) Token.Tag {
    if (self.current) |token| {
        return token.tag;
    }
    return .EndOfFile;
}

/// Advance to next token
/// In state machine mode, this clears lookahead to signal we need a new token
fn advance(self: *Parser) void {
    // Save the position of the token we're consuming
    if (self.current) |token| {
        self.last_position = token.region.end;
        self.last_region = token.region;
    }

    // Move lookahead to current
    self.current = self.lookahead;

    // If we have a token iterator, fetch the next non-comment token
    if (self.token_iterator) |iter| {
        while (true) {
            const token = iter.next(self.gpa) catch null;
            if (token == null) {
                self.lookahead = null;
                break;
            }
            // Skip comments and blank lines when parsing (not formatting)
            switch (token.?.tag) {
                .LineComment, .DocComment, .BlankLine => continue,
                else => {
                    self.lookahead = token;
                    break;
                },
            }
        }
    } else {
        // In state machine mode, clear lookahead to signal we need a new token
        self.lookahead = null;
    }
}

pub fn deinit(parser: *Parser) void {
    parser.scratch_nodes.deinit(parser.gpa);
    parser.scratch_op_stack.deinit(parser.gpa);
    parser.scratch_bytes.deinit(parser.gpa);
    parser.diagnostics.deinit(parser.gpa);
    parser.state_stack.deinit(parser.gpa);
    parser.value_stack.deinit(parser.gpa);
    parser.min_bp_stack.deinit(parser.gpa);
    // diagnostics will be kept and passed to the following compiler stage
    // to be deinitialized by the caller when no longer required
}

/// Get the diagnostics collected during parsing
fn getDiagnostics(self: *const Parser) []const AST.Diagnostic {
    return self.diagnostics.items;
}

/// Take ownership of the diagnostics array
fn takeOwnedDiagnostics(self: *Parser) std.ArrayListUnmanaged(AST.Diagnostic) {
    const result = self.diagnostics;
    self.diagnostics = .{};
    return result;
}

/// Helper to manage scratch node position
/// Usage: const marker = self.markScratchNodes();
///        defer self.restoreScratchNodes(marker);
fn markScratchNodes(self: *const Parser) usize {
    return self.scratch_nodes.items.len;
}

fn restoreScratchNodes(self: *Parser, marker: usize) void {
    self.scratch_nodes.items.len = marker;
}

/// Get nodes added since marker
fn getScratchNodesSince(self: *const Parser, marker: usize) []const Node.Idx {
    return self.scratch_nodes.items[marker..];
}

/// Parse an expression with specific operator precedence
/// This is the core expression parser that handles all operators and precedence
fn parseExprWithPrecedence(self: *Parser, initial_min_bp: u8) Error!Node.Idx {

    // Save current scratch position for cleanup
    const scratch_op_start = self.scratch_op_stack.items.len;
    defer {
        self.scratch_op_stack.items.len = scratch_op_start;
    }

    // Current parsing state
    var left: Node.Idx = undefined;
    var min_bp = initial_min_bp;

    // State machine using labeled switch
    const State = enum {
        parse_primary,
        check_operators,
        parse_rhs,
        combine_binary,
    };

    const state = State.parse_primary;

    parse: switch (state) {
        .parse_primary => {
            // Parse primary expression
            left = switch (self.peek()) {
                .LowerIdent => blk: {
                    const ident = self.currentIdent();
                    const region = self.currentRegion();
                    self.advance();

                    // Check for ! suffix for effectful functions/values
                    const is_effectful = self.peek() == .OpBang;
                    var final_region = region;
                    if (is_effectful) {
                        // Include the ! in the region
                        final_region.end = self.currentRegion().end;
                        self.advance(); // consume the !
                    }

                    if (ident) |id| {
                        const node_tag: Node.Tag = if (is_effectful) .not_lc else .lc;
                        break :blk try self.ast.appendNode(self.gpa, final_region, node_tag, .{ .ident = id });
                    } else {
                        break :blk try self.pushMalformed(.expr_unexpected_token, region.start);
                    }
                },
                .UpperIdent => blk: {
                    const ident = self.currentIdent();
                    const region = self.currentRegion();
                    self.advance();
                    if (ident) |id| {
                        break :blk try self.ast.appendNode(self.gpa, region, .uc, .{ .ident = id });
                    } else {
                        break :blk try self.pushMalformed(.expr_unexpected_token, region.start);
                    }
                },
                .Underscore => blk: {
                    const region = self.currentRegion();
                    self.advance();
                    break :blk try self.ast.appendNode(self.gpa, region, .underscore, .{ .src_bytes_end = region.start });
                },
                .TripleDot => blk: {
                    const region = self.currentRegion();
                    self.advance();
                    break :blk try self.ast.appendNode(self.gpa, region, .ellipsis, .{ .src_bytes_end = region.start });
                },
                .Int, .Float, .IntBase => try self.parseNumLiteral(),
                .String, .MultilineString, .SingleQuote => try self.parseStoredStringExpr(),
                .StringStart => try self.parseStringExpr(),
                .OpenSquare => try self.parseListLiteral(),
                .OpenCurly => try self.parseBlockOrRecord(),
                .OpenRound => try self.parseTupleOrParenthesized(),
                .KwIf => try self.parseIf(),
                .KwMatch => try self.parseMatch(),
                .OpBar => blk: {
                    // Lambda expression starting with |
                    const result = try self.parseLambda();
                    break :blk result;
                },
                .KwVar => try self.parseVar(),
                .KwFor => try self.parseFor(),
                .KwWhile => try self.parseWhile(),
                .KwReturn => try self.parseReturn(),
                .KwCrash => try self.parseCrash(),
                .OpBang => {
                    const region = self.currentRegion();
                    self.advance();
                    // For unary, we need to parse with high precedence
                    // Push current state and parse operand
                    try self.scratch_op_stack.append(self.gpa, .{
                        .left = null, // null for unary operator
                        .op_tag = .OpBang,
                        .op_region = region,
                        .min_bp = min_bp,
                    });
                    min_bp = 100; // High precedence for unary
                    continue :parse .parse_primary;
                },
                .OpBinaryMinus, .OpUnaryMinus => {
                    const region = self.currentRegion();
                    self.advance();
                    try self.scratch_op_stack.append(self.gpa, .{
                        .left = null, // null for unary operator
                        .op_tag = .OpUnaryMinus,
                        .op_region = region,
                        .min_bp = min_bp,
                    });
                    min_bp = 100; // High precedence for unary
                    continue :parse .parse_primary;
                },
                .DoubleDot => {
                    const region = self.currentRegion();
                    self.advance();
                    try self.scratch_op_stack.append(self.gpa, .{
                        .left = null, // null for unary operator
                        .op_tag = .DoubleDot,
                        .op_region = region,
                        .min_bp = min_bp,
                    });
                    min_bp = 100; // High precedence for unary
                    continue :parse .parse_primary;
                },
                .KwModule => blk: {
                    const pos = self.currentPosition();
                    self.advance();
                    if (self.peek() == .OpenRound) {
                        break :blk try self.parseModuleApply();
                    } else {
                        break :blk try self.pushMalformed(.expr_unexpected_token, pos);
                    }
                },
                .OpBackslash => blk: {
                    // Backslash is not valid lambda syntax in Roc
                    // Provide a helpful error message about the correct syntax
                    const error_pos = self.currentPosition();
                    self.advance();
                    // Push a diagnostic with a helpful message about Roc's lambda syntax
                    // Roc uses |arg1, arg2| body instead of \arg1, arg2 -> body
                    break :blk try self.pushMalformed(.backslash_not_valid_lambda_syntax, error_pos);
                },
                else => blk: {
                    // Always use current token position for error reporting
                    const error_pos = self.currentPosition();
                    break :blk try self.pushMalformed(.expr_unexpected_token, error_pos);
                },
            };

            // After parsing primary, check for operators
            continue :parse .check_operators;
        },

        .check_operators => {
            // Special case: comma before arrow creates curried functions
            // This works uniformly in both type and expression contexts
            // Only at top level (min_bp == 0) to avoid interfering with normal comma usage
            //
            // PERFORMANCE NOTE: This implementation is INTENTIONALLY designed to avoid O(n²) behavior.
            // We parse comma-separated items at a HIGHER precedence (10) than comma itself (4).
            // This prevents recursive comma handling - each comma is processed exactly once at its
            // nesting level. Without this, expressions like (a,(b,(c,(d,e)))) would cause quadratic
            // parsing time as each level would recursively reparse all inner levels.
            //
            // DO NOT change the precedence value without understanding this performance characteristic!
            if (self.peek() == .Comma and min_bp == 0) {
                const scratch_start = self.scratch_nodes.items.len;
                defer {
                    self.scratch_nodes.items.len = scratch_start;
                }

                // Collect the first item
                try self.scratch_nodes.append(self.gpa, left);

                // Parse comma-separated items at HIGHER precedence to prevent recursion
                // This is CRITICAL for O(n) performance - see comment above
                const COMMA_ITEM_PRECEDENCE = 10; // Higher than comma (4) but lower than most operators

                while (self.peek() == .Comma) {
                    self.advance(); // consume comma

                    // Check for trailing comma before closing delimiter
                    const next = self.peek();
                    if (next == .CloseRound or next == .CloseSquare or next == .CloseCurly or next == .EndOfFile) {
                        // Trailing comma - don't try to parse another element
                        break;
                    }

                    // Parse a single item - NO recursive comma handling because of high precedence
                    const item = try self.parseExprWithPrecedence(COMMA_ITEM_PRECEDENCE);
                    try self.scratch_nodes.append(self.gpa, item);
                }

                // Now check what follows the comma-separated items
                if (self.peek() == .OpArrow or self.peek() == .OpFatArrow) {
                    // It's a function type - build curried arrows
                    const is_effectful = self.peek() == .OpFatArrow;
                    const arrow_tag: Node.Tag = if (is_effectful) .binop_thick_arrow else .binop_thin_arrow;
                    const arrow_region = self.currentRegion();
                    self.advance();

                    // Parse return type at precedence 0 (allows it to have commas if needed)
                    const return_type = try self.parseExprWithPrecedence(0);

                    // Build right-associative arrows from the parameters
                    const params = self.scratch_nodes.items[scratch_start..];
                    var current = return_type;
                    var i = params.len;
                    while (i > 0) {
                        i -= 1;
                        const binop_idx = try self.ast.appendBinOp(self.gpa, params[i], current);
                        current = try self.ast.appendNode(self.gpa, arrow_region, arrow_tag, .{ .binop = binop_idx });
                    }
                    return current;
                } else {
                    // It's a tuple (or will be an error at a higher level)
                    const items = self.scratch_nodes.items[scratch_start..];
                    if (items.len == 1) {
                        left = items[0];
                    } else {
                        // For a tuple created from comma-separated items, the region spans from
                        // the first item to the last item
                        const first_region = self.ast.nodes.fieldItem(.region, @as(collections.SafeMultiList(Node).Idx, @enumFromInt(@intFromEnum(items[0]))));
                        const last_region = self.ast.nodes.fieldItem(.region, @as(collections.SafeMultiList(Node).Idx, @enumFromInt(@intFromEnum(items[items.len - 1]))));
                        const tuple_region = makeRegion(first_region.start, last_region.end);
                        const nodes_idx = try self.ast.appendNodeSlice(self.gpa, items);
                        left = try self.ast.appendNode(self.gpa, tuple_region, .tuple_literal, .{ .nodes = nodes_idx });
                    }
                    // Continue checking for more operators on the tuple
                    continue :parse .check_operators;
                }
            }

            // Check for infix operators
            const op_tag = self.peek();

            // In Roc, | is ONLY valid in lambda syntax, never as an infix operator
            // If we see OpBar here, it must be starting a lambda (since we're in expression context)
            // The lambda parser will handle the | delimiters
            if (op_tag == .OpBar) {
                // This is not an operator, stop trying to parse it as one
                break :parse;
            }

            const bp = getBindingPower(op_tag);

            if (bp.left > min_bp) {
                // We have an infix operator with sufficient precedence
                const op_region = self.currentRegion();
                self.advance();

                // Save current state to stack
                try self.scratch_op_stack.append(self.gpa, .{
                    .left = left,
                    .op_tag = op_tag,
                    .op_region = op_region,
                    .min_bp = min_bp,
                });

                // Parse RHS with higher precedence
                min_bp = bp.right;
                continue :parse .parse_primary;
            }

            // Check for postfix operators
            switch (self.peek()) {
                .OpenRound => {
                    left = try self.parseApply(left);
                    continue :parse .check_operators;
                },
                .Dot => {
                    const dot_region = self.currentRegion();
                    const dot_pos = dot_region.start;
                    self.advance();

                    if (self.peek() == .LowerIdent) {
                        const ident = self.currentIdent();
                        const ident_region = self.currentRegion();
                        self.advance();

                        // Check for ! suffix for effectful functions/values
                        const is_effectful = self.peek() == .OpBang;
                        var field_end = ident_region.end;
                        if (is_effectful) {
                            field_end = self.currentRegion().end;
                            self.advance(); // consume the !
                        }

                        if (ident) |id| {
                            const field_tag: Node.Tag = if (is_effectful) .not_lc else .dot_lc;
                            const field_region = makeRegion(dot_region.start, field_end);
                            const field = try self.ast.appendNode(self.gpa, field_region, field_tag, .{ .ident = id });
                            const binop_idx = try self.ast.appendBinOp(self.gpa, left, field);
                            // For the binop_pipe, use the region from left's start to field's end
                            const left_region = self.ast.nodes.fieldItem(.region, @as(collections.SafeMultiList(Node).Idx, @enumFromInt(@intFromEnum(left))));
                            const pipe_region = makeRegion(left_region.start, field_region.end);
                            left = try self.ast.appendNode(self.gpa, pipe_region, .binop_pipe, .{ .binop = binop_idx });
                        } else {
                            return self.pushMalformed(.expr_dot_suffix_not_allowed, dot_pos);
                        }
                    } else if (self.peek() == .UpperIdent) {
                        const ident = self.currentIdent();
                        const ident_region = self.currentRegion();
                        self.advance();

                        if (ident) |id| {
                            const field_region = makeRegion(dot_region.start, ident_region.end);
                            const field = try self.ast.appendNode(self.gpa, field_region, .uc, .{ .ident = id });
                            const binop_idx = try self.ast.appendBinOp(self.gpa, left, field);
                            // For the binop_pipe, use the region from left's start to field's end
                            const left_region = self.ast.nodes.fieldItem(.region, @as(collections.SafeMultiList(Node).Idx, @enumFromInt(@intFromEnum(left))));
                            const pipe_region = makeRegion(left_region.start, field_region.end);
                            left = try self.ast.appendNode(self.gpa, pipe_region, .binop_pipe, .{ .binop = binop_idx });
                        } else {
                            return self.pushMalformed(.expr_dot_suffix_not_allowed, dot_pos);
                        }
                    } else if (self.peek() == .Int or self.peek() == .Float) {
                        const num = try self.parseNumLiteral();
                        const binop_idx = try self.ast.appendBinOp(self.gpa, left, num);
                        // For the binop_pipe, use the region from left's start to num's end
                        const left_region = self.ast.nodes.fieldItem(.region, @as(collections.SafeMultiList(Node).Idx, @enumFromInt(@intFromEnum(left))));
                        const num_region = self.ast.nodes.fieldItem(.region, @as(collections.SafeMultiList(Node).Idx, @enumFromInt(@intFromEnum(num))));
                        const pipe_region = makeRegion(left_region.start, num_region.end);
                        left = try self.ast.appendNode(self.gpa, pipe_region, .binop_pipe, .{ .binop = binop_idx });
                    } else {
                        return self.pushMalformed(.expr_dot_suffix_not_allowed, dot_pos);
                    }
                    continue :parse .check_operators;
                },
                else => {
                    // No more operators at this level
                    // No more operators at this level
                    // Check if we need to combine with a stacked operator
                    if (self.scratch_op_stack.items.len > scratch_op_start) {
                        continue :parse .combine_binary;
                    }
                    // We're done!
                    return left;
                },
            }
        },

        .parse_rhs => {
            // This state should not be reached in the labeled switch version
            // If we get here, it's a logic error in the parser
            return self.pushMalformed(.internal_parser_error, self.currentPosition());
        },

        .combine_binary => {
            // Pop operator from stack and combine
            const op_info = self.scratch_op_stack.pop() orelse {
                // Stack underflow - should not happen with correct parser logic
                return self.pushMalformed(.internal_parser_error, self.currentPosition());
            };

            // Check if this is a unary operator (left is null)
            if (op_info.left == null) {
                // Unary operator
                const operand = left;
                const operand_slice = [_]Node.Idx{operand};
                const nodes_idx = try self.ast.appendNodeSlice(self.gpa, &operand_slice);

                left = switch (op_info.op_tag) {
                    .OpBang => try self.ast.appendNode(self.gpa, op_info.op_region, .unary_not, .{ .nodes = nodes_idx }),
                    .OpUnaryMinus => try self.ast.appendNode(self.gpa, op_info.op_region, .unary_neg, .{ .nodes = nodes_idx }),
                    .DoubleDot => try self.ast.appendNode(self.gpa, op_info.op_region, .unary_double_dot, .{ .nodes = nodes_idx }),
                    else => {
                        // Unexpected unary operator type
                        return self.pushMalformed(.expr_unexpected_token, op_info.op_region.start);
                    },
                };
            } else {
                // Binary operator - combine left and right
                const right = left;
                const binop_tag = tokenToBinOpTag(op_info.op_tag) orelse {
                    return self.pushMalformed(.expr_unexpected_token, op_info.op_region.start);
                };

                const binop_idx = try self.ast.appendBinOp(self.gpa, op_info.left.?, right);
                const full_region = makeRegion(self.ast.start(op_info.left.?), self.ast.getRegion(right).end);
                left = try self.ast.appendNode(self.gpa, full_region, binop_tag, .{ .binop = binop_idx });
            }

            // Restore min_bp
            min_bp = op_info.min_bp;

            // Continue checking for more operators
            continue :parse .check_operators;
        },
    }

    // Return the result after breaking out of the parse loop
    return left;
}

fn parseModuleApply(self: *Parser) Error!Node.Idx {
    const open_paren_region = self.currentRegion();
    const pos = self.currentPosition();
    self.advance(); // consume (

    const scratch_marker = self.markScratchNodes();
    defer self.restoreScratchNodes(scratch_marker);

    // Parse arguments
    while (self.peek() != .CloseRound and self.peek() != .EndOfFile) {
        const arg = try self.parseExprWithPrecedence(0);
        try self.scratch_nodes.append(self.gpa, arg);

        if (self.peek() == .Comma) {
            self.advance();
        } else {
            break;
        }
    }

    // Always continue parsing - never stop on errors
    var close_paren_region: Region = undefined;
    if (self.peek() != .CloseRound) {
        try self.pushDiagnostic(.expected_expr_apply_close_round, pos, self.currentPosition());
        // Try to recover by looking for the close paren
        while (self.peek() != .CloseRound and self.peek() != .EndOfFile) {
            self.advance();
        }
    }
    if (self.peek() == .CloseRound) {
        close_paren_region = self.currentRegion();
        self.advance();
    } else {
        // Use current position for missing close paren
        close_paren_region = makeRegion(self.currentPosition(), self.currentPosition());
    }

    const nodes = self.getScratchNodesSince(scratch_marker);
    const nodes_idx = try self.ast.appendNodeSlice(self.gpa, nodes);
    const full_region = makeRegion(open_paren_region.start, close_paren_region.end);
    return try self.ast.appendNode(self.gpa, full_region, .apply_module, .{ .nodes = nodes_idx });
}

/// Process tokens in state machine mode
/// Returns true if more tokens needed, false if parsing complete
fn chompToken(self: *Parser, current: Token, lookahead: ?Token) !bool {
    // Debug assertions: chompToken should never receive comments or blank lines
    if (std.debug.runtime_safety) {
        std.debug.assert(current.tag != .LineComment);
        std.debug.assert(current.tag != .DocComment);
        std.debug.assert(current.tag != .BlankLine);
        if (lookahead) |la| {
            std.debug.assert(la.tag != .LineComment);
            std.debug.assert(la.tag != .DocComment);
            std.debug.assert(la.tag != .BlankLine);
        }
    }

    // Update token state
    self.current = current;
    self.lookahead = lookahead;

    // If we haven't started, initialize
    if (self.state_stack.items.len == 0) {
        try self.state_stack.append(self.gpa, .file_start);
    }

    // Process states until we need a new token or are done
    while (self.state_stack.items.len > 0) {
        const state = self.state_stack.items[self.state_stack.items.len - 1];

        // Process current state
        const action = try self.processState(state);

        switch (action) {
            .need_token => return true, // Need more tokens
            .continue_processing => {}, // Keep processing states
            .complete => return false, // Parsing complete
        }
    }

    return false; // No more states, parsing complete
}

const StateAction = enum {
    need_token,
    continue_processing,
    complete,
};

/// Process a single state in the state machine
fn processState(self: *Parser, state: ParseState) !StateAction {
    switch (state) {
        .file_start => {
            // Pop this state and push next
            _ = self.state_stack.pop();
            try self.state_stack.append(self.gpa, .file_stmt_loop);
            try self.state_stack.append(self.gpa, .header_check);
            return .continue_processing;
        },

        .header_check => {
            // Check for header keywords
            switch (self.peek()) {
                .KwApp => {
                    _ = self.state_stack.pop();
                    try self.state_stack.append(self.gpa, .header_app);
                    return .continue_processing;
                },
                .KwModule => {
                    _ = self.state_stack.pop();
                    try self.state_stack.append(self.gpa, .header_module);
                    return .continue_processing;
                },
                .KwPackage => {
                    _ = self.state_stack.pop();
                    try self.state_stack.append(self.gpa, .header_package);
                    return .continue_processing;
                },
                .KwPlatform => {
                    _ = self.state_stack.pop();
                    try self.state_stack.append(self.gpa, .header_platform);
                    return .continue_processing;
                },
                .KwHosted => {
                    _ = self.state_stack.pop();
                    try self.state_stack.append(self.gpa, .header_hosted);
                    return .continue_processing;
                },
                else => {
                    // No header
                    _ = self.state_stack.pop();
                    return .continue_processing;
                },
            }
        },

        .file_stmt_loop => {
            // Parse statements until EOF
            if (self.peek() == .EndOfFile) {
                _ = self.state_stack.pop();
                try self.state_stack.append(self.gpa, .file_done);
                return .continue_processing;
            }

            // Parse a statement - pop and push back to continue loop after stmt
            _ = self.state_stack.pop();
            try self.state_stack.append(self.gpa, .file_stmt_loop);
            try self.state_stack.append(self.gpa, .stmt_complete);
            try self.state_stack.append(self.gpa, .stmt_start);
            return .continue_processing;
        },

        .stmt_start => {
            // Start parsing a statement
            _ = self.state_stack.pop();

            // First parse as expression
            try self.state_stack.append(self.gpa, .stmt_check_colon);
            try self.state_stack.append(self.gpa, .expr_start);
            try self.min_bp_stack.append(self.gpa, 3); // Stop at colons
            return .continue_processing;
        },

        .expr_start => {
            // Start expression parsing with precedence
            _ = self.state_stack.pop();
            try self.state_stack.append(self.gpa, .expr_check_binop);
            try self.state_stack.append(self.gpa, .expr_primary);
            return .continue_processing;
        },

        .expr_primary => {
            // Parse primary expression based on current token
            _ = self.state_stack.pop();

            switch (self.peek()) {
                .LowerIdent => {
                    const ident = self.currentIdent();
                    const region = self.currentRegion();
                    self.advance();

                    // Check for ! suffix
                    const is_effectful = self.peek() == .OpBang;
                    var final_region = region;
                    if (is_effectful) {
                        final_region.end = self.currentRegion().end;
                        self.advance();
                        return .need_token; // Need next token after consuming !
                    }

                    if (ident) |id| {
                        const node_tag: Node.Tag = if (is_effectful) .not_lc else .lc;
                        const node = try self.ast.appendNode(self.gpa, final_region, node_tag, .{ .ident = id });
                        try self.value_stack.append(self.gpa, node);
                    }
                    return .continue_processing;
                },

                .UpperIdent => {
                    const ident = self.currentIdent();
                    const region = self.currentRegion();
                    self.advance();

                    if (ident) |id| {
                        const node = try self.ast.appendNode(self.gpa, region, .uc, .{ .ident = id });
                        try self.value_stack.append(self.gpa, node);
                    }
                    return .continue_processing;
                },

                .Int, .IntBase, .Float => {
                    const region = self.currentRegion();
                    const extra = self.currentExtra();
                    self.advance();

                    // Extract the appropriate value from the Extra union
                    const payload = switch (extra) {
                        .num_literal_i32 => |val| Node.Payload{ .num_literal_i32 = val },
                        .frac_literal_small => |val| Node.Payload{ .frac_literal_small = .{
                            .numerator = val.numerator,
                            .denominator_power_of_ten = val.denominator_power_of_ten,
                        } },
                        .bytes_idx => |idx| Node.Payload{ .num_literal_big = idx },
                        else => Node.Payload{ .num_literal_i32 = 0 }, // Default fallback
                    };

                    // Determine node tag based on extra type
                    const node_tag: Node.Tag = switch (extra) {
                        .num_literal_i32 => .num_literal_i32,
                        .frac_literal_small => .frac_literal_small,
                        .bytes_idx => .num_literal_big,
                        else => .num_literal_i32,
                    };

                    const node = try self.ast.appendNode(self.gpa, region, node_tag, payload);
                    try self.value_stack.append(self.gpa, node);
                    return .continue_processing;
                },

                .String, .MultilineString => {
                    const region = self.currentRegion();
                    const extra = self.currentExtra();
                    self.advance();

                    // Extract string from Extra union
                    const payload = switch (extra) {
                        .bytes_idx => |idx| Node.Payload{ .str_literal_big = idx },
                        else => Node.Payload{ .str_literal_big = @enumFromInt(0) }, // Default fallback
                    };

                    const node = try self.ast.appendNode(self.gpa, region, .str_literal_big, payload);
                    try self.value_stack.append(self.gpa, node);
                    return .continue_processing;
                },

                .OpenRound => {
                    // Could be tuple or parenthesized expression
                    const start_region = self.currentRegion();
                    self.advance();

                    // Save start position for region calculation
                    try self.value_stack.append(self.gpa, @enumFromInt(@as(i32, @intCast(start_region.start.offset))));

                    // Push states to parse parenthesized content
                    try self.state_stack.append(self.gpa, .paren_complete);
                    try self.state_stack.append(self.gpa, .paren_content);

                    return .continue_processing;
                },

                .OpenSquare => {
                    // List literal
                    const list_start = self.currentRegion();
                    self.advance(); // consume [

                    // Save list start position
                    try self.value_stack.append(self.gpa, @enumFromInt(@as(i32, @intCast(list_start.start.offset))));

                    // Push states to parse list
                    try self.state_stack.append(self.gpa, .list_complete);
                    try self.state_stack.append(self.gpa, .list_elements);

                    return .continue_processing;
                },

                .KwIf => {
                    // Start parsing if expression
                    const if_region = self.currentRegion();
                    self.advance(); // consume 'if'

                    // Save if start position for later
                    try self.value_stack.append(self.gpa, @enumFromInt(@as(i32, @intCast(if_region.start.offset))));

                    // Push states: after condition, parse arrow, then branch, else branch
                    try self.state_stack.append(self.gpa, .if_complete);
                    try self.state_stack.append(self.gpa, .if_else_branch);
                    try self.state_stack.append(self.gpa, .if_then_branch);
                    try self.state_stack.append(self.gpa, .if_arrow);
                    try self.state_stack.append(self.gpa, .expr_start); // Parse condition

                    return .continue_processing;
                },

                .KwMatch => {
                    // Start parsing match expression
                    const match_region = self.currentRegion();
                    self.advance(); // consume 'match'

                    // Save match start position for later
                    try self.value_stack.append(self.gpa, @enumFromInt(@as(i32, @intCast(match_region.start.offset))));

                    // Push states to parse match expression
                    try self.state_stack.append(self.gpa, .match_complete);
                    try self.state_stack.append(self.gpa, .match_branches);
                    try self.state_stack.append(self.gpa, .expr_start); // Parse scrutinee directly (no 'is' keyword)

                    return .continue_processing;
                },

                .OpenCurly => {
                    // Record literal or block
                    const record_start = self.currentRegion();
                    self.advance(); // consume {

                    // Save record start position
                    try self.value_stack.append(self.gpa, @enumFromInt(@as(i32, @intCast(record_start.start.offset))));

                    // Push states to parse record
                    try self.state_stack.append(self.gpa, .record_complete);
                    try self.state_stack.append(self.gpa, .record_fields);

                    return .continue_processing;
                },

                .OpBar => {
                    // Lambda - push states to parse it
                    _ = self.state_stack.pop(); // Remove expr_primary
                    try self.state_stack.append(self.gpa, .lambda_complete);
                    try self.state_stack.append(self.gpa, .lambda_body);
                    try self.state_stack.append(self.gpa, .lambda_args);
                    return .continue_processing;
                },

                .OpBang => {
                    // Unary not
                    const op_region = self.currentRegion();
                    self.advance();

                    // Save operator info on value stack (as position to identify operator type)
                    try self.value_stack.append(self.gpa, @enumFromInt(op_region.start.offset)); // Use start offset as marker for OpBang

                    // Parse the operand
                    try self.state_stack.append(self.gpa, .expr_unary_complete);
                    try self.state_stack.append(self.gpa, .expr_primary);
                    return .continue_processing;
                },

                .OpUnaryMinus, .OpBinaryMinus => {
                    // Unary negation (OpBinaryMinus can also be unary at start of expression)
                    const op_region = self.currentRegion();
                    self.advance();

                    // Save operator info on value stack (as position + 1 to identify operator type)
                    try self.value_stack.append(self.gpa, @enumFromInt(op_region.start.offset + 1)); // Use start offset + 1 as marker for minus

                    // Parse the operand
                    try self.state_stack.append(self.gpa, .expr_unary_complete);
                    try self.state_stack.append(self.gpa, .expr_primary);
                    return .continue_processing;
                },

                .KwWhile => {
                    // While loop - parse condition and body
                    const while_region = self.currentRegion();
                    self.advance(); // consume 'while'

                    // Push states to parse condition then body
                    try self.state_stack.append(self.gpa, .while_complete);
                    try self.state_stack.append(self.gpa, .while_body);
                    try self.state_stack.append(self.gpa, .expr_start);

                    // Save the while region for later
                    const region_idx = @as(Node.Idx, @enumFromInt(@as(i32, @intCast(while_region.start.offset))));
                    try self.value_stack.append(self.gpa, region_idx);

                    return .continue_processing;
                },

                .KwFor => {
                    // For loop - parse pattern, iterator, and body
                    const for_region = self.currentRegion();
                    self.advance(); // consume 'for'

                    // Push states to parse pattern, then 'in', then iterator, then body
                    try self.state_stack.append(self.gpa, .for_complete);
                    try self.state_stack.append(self.gpa, .for_body);
                    try self.state_stack.append(self.gpa, .for_in);
                    try self.state_stack.append(self.gpa, .expr_start); // pattern

                    // Save the for region for later
                    const region_idx = @as(Node.Idx, @enumFromInt(@as(i32, @intCast(for_region.start.offset))));
                    try self.value_stack.append(self.gpa, region_idx);

                    return .continue_processing;
                },

                .Underscore => {
                    // Underscore pattern
                    const region = self.currentRegion();
                    self.advance();
                    const node = try self.ast.appendNode(self.gpa, region, .underscore, .{ .src_bytes_end = region.start });
                    try self.value_stack.append(self.gpa, node);
                    return .continue_processing;
                },

                .KwVar => {
                    // var keyword
                    const var_region = self.currentRegion();
                    self.advance();

                    // Expect a lowercase identifier after var
                    if (self.peek() == .LowerIdent) {
                        const ident = self.currentIdent();
                        const ident_region = self.currentRegion();
                        self.advance();

                        if (ident) |id| {
                            const full_region = makeRegion(var_region.start, ident_region.end);
                            const node = try self.ast.appendNode(self.gpa, full_region, .var_lc, .{ .ident = id });
                            try self.value_stack.append(self.gpa, node);
                        }
                    } else {
                        // Malformed var without identifier
                        const node = try self.ast.appendNode(self.gpa, var_region, .malformed, .{ .malformed = .var_must_have_ident });
                        try self.value_stack.append(self.gpa, node);
                    }
                    return .continue_processing;
                },

                .KwReturn => {
                    // Return statement
                    const return_region = self.currentRegion();
                    self.advance(); // consume 'return'

                    // Push states to parse the expression to return
                    try self.state_stack.append(self.gpa, .return_complete);
                    try self.state_stack.append(self.gpa, .expr_start);

                    // Save the return region for later
                    const region_idx = @as(Node.Idx, @enumFromInt(@as(i32, @intCast(return_region.start.offset))));
                    try self.value_stack.append(self.gpa, region_idx);

                    return .continue_processing;
                },

                .KwCrash => {
                    // Crash statement
                    const crash_region = self.currentRegion();
                    self.advance(); // consume 'crash'

                    // Push states to parse the expression to crash with
                    try self.state_stack.append(self.gpa, .crash_complete);
                    try self.state_stack.append(self.gpa, .expr_start);

                    // Save the crash region for later
                    const region_idx = @as(Node.Idx, @enumFromInt(@as(i32, @intCast(crash_region.start.offset))));
                    try self.value_stack.append(self.gpa, region_idx);

                    return .continue_processing;
                },

                else => {
                    // Unknown token - create malformed node
                    const region = self.currentRegion();
                    const node = try self.ast.appendNode(self.gpa, region, .malformed, .{ .malformed = .expr_unexpected_token });
                    try self.value_stack.append(self.gpa, node);
                    self.advance();
                    return .need_token;
                },
            }
        },

        .expr_check_binop => {
            // Check if there's a binary operator to handle
            _ = self.state_stack.pop();

            // Get the current minimum binding power (precedence)
            const min_bp = if (self.min_bp_stack.items.len > 0)
                self.min_bp_stack.items[self.min_bp_stack.items.len - 1]
            else
                0;

            // Check if current token is a binary operator
            const bp = getBindingPower(self.peek());

            if (bp.left > min_bp) {
                // We have a binary operator to parse
                // Save the operator info
                const op_token = self.peek();
                const op_region = self.currentRegion();

                // Get LHS from value stack
                const lhs = if (self.value_stack.items.len > 0)
                    self.value_stack.items[self.value_stack.items.len - 1]
                else
                    return .continue_processing; // No LHS, can't parse binop

                // Store operator info
                try self.scratch_op_stack.append(self.gpa, .{
                    .left = lhs,
                    .op_tag = op_token,
                    .op_region = op_region,
                    .min_bp = bp.right,
                });

                self.advance();

                // Push states to parse RHS then combine
                try self.state_stack.append(self.gpa, .expr_binop_combine);
                try self.state_stack.append(self.gpa, .expr_binop_rhs);

                return .need_token;
            } else {
                // No operator or lower precedence - done with this expression
                try self.state_stack.append(self.gpa, .expr_done);
                return .continue_processing;
            }
        },

        .expr_done => {
            // Expression parsing complete
            _ = self.state_stack.pop();
            if (self.min_bp_stack.items.len > 0) {
                _ = self.min_bp_stack.pop();
            }
            return .continue_processing;
        },

        .expr_unary_complete => {
            // Unary expression complete - combine operator with operand
            _ = self.state_stack.pop();

            // Pop the operand from value stack
            if (self.value_stack.items.len > 0) {
                const operand = self.value_stack.pop().?;

                // Pop the operator marker from value stack
                const op_marker = if (self.value_stack.items.len > 0)
                    self.value_stack.pop().?
                else
                    null;

                if (op_marker) |marker| {
                    // Determine operator type based on marker
                    const marker_val = @intFromEnum(marker);
                    const is_negation = (marker_val & 1) == 1; // Odd means negation

                    // Create nodes slice with operand
                    const operand_slice = try self.gpa.alloc(Node.Idx, 1);
                    operand_slice[0] = operand;
                    const nodes_idx = try self.ast.node_slices.append(self.gpa, operand_slice);

                    // Calculate region from operator position to operand end
                    const op_start_offset = if (is_negation) marker_val - 1 else marker_val;
                    const op_start = Position{ .offset = @intCast(op_start_offset) };
                    const operand_region = self.ast.getRegion(operand);
                    const full_region = makeRegion(op_start, operand_region.end);

                    // Create unary node with appropriate tag
                    const node_tag: Node.Tag = if (is_negation) .unary_neg else .unary_not;
                    const unary_node = try self.ast.appendNode(self.gpa, full_region, node_tag, .{ .nodes = nodes_idx });

                    try self.value_stack.append(self.gpa, unary_node);
                } else {
                    // No operator marker, just push operand back
                    try self.value_stack.append(self.gpa, operand);
                }
            }
            return .continue_processing;
        },

        .stmt_check_colon => {
            // Check if statement has type annotation
            _ = self.state_stack.pop();

            if (self.peek() == .OpColon) {
                // Has type annotation
                self.advance(); // consume :
                try self.state_stack.append(self.gpa, .stmt_type_anno);
                return .continue_processing;
            } else if (self.peek() == .OpColonEqual) {
                // := syntax (type inference)
                self.advance(); // consume :=
                // Continue with value expression
                try self.state_stack.append(self.gpa, .expr_start);
                return .continue_processing;
            }
            // Statement is just an expression, value is already on stack
            return .continue_processing;
        },

        .stmt_type_anno => {
            // Parse type annotation after :
            _ = self.state_stack.pop();

            // Parse the type expression
            // Type expressions use the same parsing logic as regular expressions
            // but are interpreted differently during canonicalization
            try self.state_stack.append(self.gpa, .stmt_type_anno_complete);
            try self.state_stack.append(self.gpa, .type_expr_start);

            return .continue_processing;
        },

        .stmt_type_anno_complete => {
            // Complete statement with type annotation
            _ = self.state_stack.pop();

            // Pop the type from value stack
            const type_expr = if (self.value_stack.items.len > 0) self.value_stack.pop() else null;
            // Pop the name/pattern from value stack
            const name_expr = if (self.value_stack.items.len > 0) self.value_stack.pop() else null;

            if (type_expr != null and name_expr != null) {
                // Create type annotation node
                const binop_idx = try self.ast.appendBinOp(self.gpa, name_expr.?, type_expr.?);
                const region = makeRegion(self.ast.getRegion(name_expr.?).start, self.ast.getRegion(type_expr.?).end);
                const anno_node = try self.ast.appendNode(self.gpa, region, .binop_colon, .{ .binop = binop_idx });
                try self.value_stack.append(self.gpa, anno_node);
            }

            return .continue_processing;
        },

        .stmt_complete => {
            // Statement parsing complete, add to scratch_nodes
            _ = self.state_stack.pop();

            // Pop the statement from value stack and add to scratch_nodes
            if (self.value_stack.items.len > 0) {
                const stmt = self.value_stack.items[self.value_stack.items.len - 1];
                _ = self.value_stack.pop();
                try self.scratch_nodes.append(self.gpa, stmt);
            }
            return .continue_processing;
        },

        .file_done => {
            // Create block node with all statements
            _ = self.state_stack.pop();

            const statements = self.getScratchNodesSince(0);
            if (statements.len > 0) {
                // Check if the first statement index is valid
                const first_stmt_idx = @intFromEnum(statements[0]);
                const nodes_len = self.ast.nodes.len();
                if (first_stmt_idx >= nodes_len) {
                    // Invalid index - return without creating block
                    return .complete;
                }

                const first_region = self.ast.nodes.fieldItem(.region, @as(collections.SafeMultiList(Node).Idx, @enumFromInt(@intFromEnum(statements[0]))));
                const region = makeRegion(
                    first_region.start,
                    self.last_position,
                );
                const statements_idx = try self.ast.node_slices.append(self.gpa, statements);
                const block = try self.ast.appendNode(self.gpa, region, .block, .{ .nodes = statements_idx });
                try self.value_stack.append(self.gpa, block);
            }
            return .complete;
        },

        .header_module => {
            // Parse module header
            _ = self.state_stack.pop();

            const start_region = self.currentRegion();
            self.advance(); // consume 'module'

            // Expect [exposes]
            if (self.peek() != .OpenSquare) {
                // Malformed header - still create it with empty exposes
                const empty_slice: []const Node.Idx = &.{};
                const exposes_idx = try self.ast.node_slices.append(self.gpa, empty_slice);
                self.ast.header = .{ .module = .{
                    .exposes = exposes_idx,
                    .region = start_region.start,
                } };
                return .need_token;
            }

            self.advance(); // consume [

            // Parse exposed items
            const scratch_marker = self.markScratchNodes();
            defer self.restoreScratchNodes(scratch_marker);

            while (self.peek() != .CloseSquare and self.peek() != .EndOfFile) {
                switch (self.peek()) {
                    .LowerIdent => {
                        const ident = self.currentIdent();
                        const region = self.currentRegion();
                        self.advance();

                        // Check for ! suffix
                        const is_effectful = self.peek() == .OpBang;
                        if (is_effectful) {
                            self.advance();
                        }

                        if (ident) |id| {
                            const node_tag: Node.Tag = if (is_effectful) .not_lc else .lc;
                            const node = try self.ast.appendNode(self.gpa, region, node_tag, .{ .ident = id });
                            try self.scratch_nodes.append(self.gpa, node);
                        }
                    },
                    .UpperIdent => {
                        const ident = self.currentIdent();
                        const region = self.currentRegion();
                        self.advance();

                        if (ident) |id| {
                            const node = try self.ast.appendNode(self.gpa, region, .uc, .{ .ident = id });
                            try self.scratch_nodes.append(self.gpa, node);
                        }
                    },
                    else => break,
                }

                if (self.peek() == .Comma) {
                    self.advance();
                } else if (self.peek() != .CloseSquare) {
                    break;
                }
            }

            // Expect ]
            if (self.peek() == .CloseSquare) {
                self.advance();
            }

            // Create header with exposed items
            const exposes = self.getScratchNodesSince(scratch_marker);
            const exposes_idx = try self.ast.node_slices.append(self.gpa, exposes);

            self.ast.header = .{ .module = .{
                .exposes = exposes_idx,
                .region = start_region.start,
            } };

            return .need_token;
        },

        .header_app => {
            // Start parsing app header
            _ = self.state_stack.pop();

            const start_region = self.currentRegion();
            self.advance(); // consume 'app'

            // Push state to parse exposed list
            try self.state_stack.append(self.gpa, .header_app_after_exposes);
            try self.state_stack.append(self.gpa, .header_parse_exposed_list);

            // Save start position for later
            try self.value_stack.append(self.gpa, @enumFromInt(@as(i32, @intCast(start_region.start.offset))));

            return .continue_processing;
        },

        .header_app_after_exposes => {
            // After parsing exposed list, now parse packages
            _ = self.state_stack.pop();

            // Get exposed list from value stack
            const exposes_idx_val = self.value_stack.pop();
            const exposes_idx: collections.NodeSlices(Node.Idx).Idx = if (exposes_idx_val) |val| blk: {
                const int_val = nodeIdxToInt(val);
                if (int_val < 0) {
                    break :blk nodeSlicesIdxFromInt(0);
                }
                break :blk nodeSlicesIdxFromInt(@intCast(int_val));
            } else nodeSlicesIdxFromInt(0);

            // Get start position
            const start_pos_val = self.value_stack.pop();
            const start_pos = if (start_pos_val) |val|
                intToPosition(nodeIdxToInt(val))
            else
                self.currentPosition();

            // Now parse packages if present
            if (self.peek() == .OpenCurly) {
                self.advance(); // consume {

                // Push state to finish app header after packages
                try self.state_stack.append(self.gpa, .header_app_finish);
                try self.state_stack.append(self.gpa, .header_parse_packages);

                // Save exposes and start pos for finish
                try self.value_stack.append(self.gpa, @enumFromInt(@intFromEnum(exposes_idx)));
                try self.value_stack.append(self.gpa, @enumFromInt(@as(i32, @intCast(start_pos.offset))));

                return .continue_processing;
            } else {
                // No packages, create header now
                const empty_slice: []const Node.Idx = &.{};
                const packages_idx = try self.ast.node_slices.append(self.gpa, empty_slice);

                self.ast.header = .{ .app = .{
                    .exposes = exposes_idx,
                    .packages = packages_idx,
                    .region = start_pos,
                } };
                return .continue_processing;
            }
        },

        .header_app_finish => {
            // Finish app header after parsing packages
            _ = self.state_stack.pop();

            // Get packages from value stack
            const packages_idx_val = self.value_stack.pop();
            const packages_idx: collections.NodeSlices(Node.Idx).Idx = if (packages_idx_val) |val| blk: {
                const int_val = nodeIdxToInt(val);
                if (int_val < 0) {
                    break :blk nodeSlicesIdxFromInt(0);
                }
                break :blk nodeSlicesIdxFromInt(@intCast(int_val));
            } else nodeSlicesIdxFromInt(0);

            // Get start position
            const start_pos_val = self.value_stack.pop();
            const start_pos = if (start_pos_val) |val|
                intToPosition(nodeIdxToInt(val))
            else
                self.currentPosition();

            // Get exposes
            const exposes_idx_val = self.value_stack.pop();
            const exposes_idx: collections.NodeSlices(Node.Idx).Idx = if (exposes_idx_val) |val| blk: {
                const int_val = nodeIdxToInt(val);
                if (int_val < 0) {
                    break :blk nodeSlicesIdxFromInt(0);
                }
                break :blk nodeSlicesIdxFromInt(@intCast(int_val));
            } else nodeSlicesIdxFromInt(0);

            self.ast.header = .{ .app = .{
                .exposes = exposes_idx,
                .packages = packages_idx,
                .region = start_pos,
            } };

            if (self.peek() == .CloseCurly) {
                self.advance(); // consume }
            }

            return .continue_processing;
        },

        .header_package => {
            // Parse package header
            _ = self.state_stack.pop();

            const region = self.currentRegion();
            self.advance(); // consume 'package'

            // Parse exposed list
            const exposes_idx = if (self.peek() == .OpenSquare) blk: {
                self.advance();
                const idx = try self.parseExposedList(.CloseSquare);
                if (self.peek() == .CloseSquare) {
                    self.advance();
                }
                break :blk idx;
            } else collections.NodeSlices(AST.Node.Idx).Idx.NIL;

            // Parse packages (optional)
            const packages_idx = if (self.peek() == .OpenCurly) blk: {
                self.advance();
                const idx = try self.parsePackageList();
                if (self.peek() == .CloseCurly) {
                    self.advance();
                }
                break :blk idx;
            } else collections.NodeSlices(AST.Node.Idx).Idx.NIL;

            self.ast.header = .{ .package = .{
                .exposes = exposes_idx,
                .packages = packages_idx,
                .region = region.start,
            } };
            return .need_token;
        },

        .header_parse_exposed_list => {
            // Parse exposed list [a, b!, c]
            _ = self.state_stack.pop();

            if (!self.eat(.OpenSquare)) {
                // Error: expected [
                // Push empty list
                const empty_slice: []const Node.Idx = &.{};
                const idx = try self.ast.node_slices.append(self.gpa, empty_slice);
                try self.value_stack.append(self.gpa, @enumFromInt(@intFromEnum(idx)));
                return .continue_processing;
            }

            // Start collecting exposed items
            try self.scratch_nodes.resize(self.gpa, 0);

            while (self.peek() != .CloseSquare and self.peek() != .EndOfFile) {
                const is_lower = self.peek() == .LowerIdent;
                const is_upper = self.peek() == .UpperIdent;

                if (is_lower or is_upper) {
                    const ident_token = self.currentToken() orelse continue;
                    _ = self.currentRegion();
                    self.advance();

                    // Check for effectful suffix
                    const effectful = self.eat(.OpBang);

                    const ident_idx: base.Ident.Idx = switch (ident_token.extra) {
                        .interned => |idx| blk: {
                            // Update attributes if effectful
                            if (effectful) {
                                break :blk .{
                                    .attributes = .{ .effectful = true, .ignored = false, .reassignable = false },
                                    .idx = idx.idx,
                                };
                            } else {
                                break :blk idx;
                            }
                        },
                        .ident_with_flags => |iwf| blk: {
                            if (effectful) {
                                break :blk .{
                                    .attributes = .{ .effectful = true, .ignored = false, .reassignable = false },
                                    .idx = iwf.ident.idx,
                                };
                            } else {
                                break :blk iwf.ident;
                            }
                        },
                        else => .{ .attributes = .{ .effectful = effectful, .ignored = false, .reassignable = false }, .idx = 0 },
                    };

                    const node_idx = try self.ast.appendNode(
                        self.gpa,
                        self.currentRegion(),
                        if (is_lower) .lc else .uc,
                        .{ .ident = ident_idx },
                    );

                    try self.scratch_nodes.append(self.gpa, node_idx);
                }

                // Skip commas
                _ = self.eat(.Comma);
            }

            if (!self.eat(.CloseSquare)) {
                // Error: expected ]
            }

            // Create slice from scratch nodes
            const exposed_slice = try self.gpa.alloc(Node.Idx, self.scratch_nodes.items.len);
            @memcpy(exposed_slice, self.scratch_nodes.items);
            const idx = try self.ast.node_slices.append(self.gpa, exposed_slice);
            try self.value_stack.append(self.gpa, @enumFromInt(@intFromEnum(idx)));

            return .continue_processing;
        },

        .header_parse_packages => {
            // Parse packages {pf: platform "../basic-cli/main.roc", a: "a"}
            _ = self.state_stack.pop();

            // Start collecting package nodes
            try self.scratch_nodes.resize(self.gpa, 0);

            while (self.peek() != .CloseCurly and self.peek() != .EndOfFile) {
                // Parse package name
                if (self.peek() == .LowerIdent) {
                    const pkg_name_token = self.currentToken() orelse continue;
                    const pkg_region = self.currentRegion();
                    self.advance();

                    if (!self.eat(.OpColon)) {
                        // Error: expected :
                        continue;
                    }

                    // Check for 'platform' keyword
                    _ = self.eat(.KwPlatform);

                    // Parse package path (string literal)
                    if (self.peek() == .String) {
                        const path_token = self.currentToken() orelse continue;
                        _ = self.currentRegion();
                        self.advance();

                        // Create string node for path
                        const str_val = switch (path_token.extra) {
                            .bytes_idx => |idx| idx,
                            else => @as(collections.ByteSlices.Idx, @enumFromInt(0)),
                        };

                        const path_region = self.currentRegion();
                        const path_node = try self.ast.appendNode(self.gpa, path_region, .str_literal_big, .{ .str_literal_big = str_val });

                        // Create package field node
                        const pkg_ident_idx: base.Ident.Idx = switch (pkg_name_token.extra) {
                            .interned => |idx| idx,
                            .ident_with_flags => |iwf| iwf.ident,
                            else => .{ .attributes = .{ .effectful = false, .ignored = false, .reassignable = false }, .idx = 0 },
                        };

                        const pkg_field_node = try self.ast.appendNode(self.gpa, pkg_region, .lc, .{ .ident = pkg_ident_idx });

                        // Create binop for field: name
                        const binop_idx = try self.ast.appendBinOp(self.gpa, pkg_field_node, path_node);

                        const field_region = makeRegion(pkg_region.start, path_region.end);
                        const field_node = try self.ast.appendNode(self.gpa, field_region, .binop_colon, .{ .binop = binop_idx });

                        try self.scratch_nodes.append(self.gpa, field_node);
                    }
                }

                // Skip commas
                _ = self.eat(.Comma);
            }

            // Create slice from scratch nodes
            const packages_slice = try self.gpa.alloc(Node.Idx, self.scratch_nodes.items.len);
            @memcpy(packages_slice, self.scratch_nodes.items);
            const idx = try self.ast.node_slices.append(self.gpa, packages_slice);
            try self.value_stack.append(self.gpa, @enumFromInt(@intFromEnum(idx)));

            return .continue_processing;
        },

        .header_platform => {
            // Parse platform header
            _ = self.state_stack.pop();

            const region = self.currentRegion();
            self.advance(); // consume 'platform'

            // Parse platform name (lowercase identifier or string)
            const name_node = if (self.peek() == .LowerIdent) blk: {
                const ident = self.currentIdent() orelse unreachable;
                const node = try self.ast.appendNode(self.gpa, self.currentRegion(), .lc, .{ .ident = ident });
                self.advance();
                break :blk node;
            } else if (self.peek() == .String) blk: {
                const node = try self.parseStoredStringExpr();
                break :blk node;
            } else blk: {
                // Error: expected name
                const dummy_ident = Ident.Idx{ .attributes = .{ .effectful = false, .ignored = false, .reassignable = false }, .idx = 0 };
                break :blk try self.ast.appendNode(self.gpa, region, .lc, .{ .ident = dummy_ident });
            };

            // Parse requires clause (optional)
            var rigids_idx = collections.NodeSlices(AST.Node.Idx).Idx.NIL;
            var requires_signatures = try self.ast.appendNode(self.gpa, region, .underscore, .{ .src_bytes_end = region.start });

            if (self.peek() == .KwRequires) {
                self.advance();
                // Parse rigids if present
                if (self.peek() == .OpenCurly) {
                    self.advance();
                    // Parse rigid type variables using scratch buffer
                    const scratch_marker = self.markScratchNodes();
                    defer self.restoreScratchNodes(scratch_marker);

                    while (self.peek() != .CloseCurly and self.peek() != .EndOfFile) {
                        if (self.peek() == .LowerIdent) {
                            const ident = self.currentIdent() orelse unreachable;
                            const node = try self.ast.appendNode(self.gpa, self.currentRegion(), .lc, .{ .ident = ident });
                            try self.scratch_nodes.append(self.gpa, node);
                            self.advance();
                        }
                        _ = self.eat(.Comma);
                    }
                    _ = self.eat(.CloseCurly);

                    const rigids = self.getScratchNodesSince(scratch_marker);
                    if (rigids.len > 0) {
                        rigids_idx = try self.ast.appendNodeSlice(self.gpa, rigids);
                    }
                }

                // Parse requires signatures as record expressions
                // The signatures are represented as record syntax during parsing
                if (self.peek() == .OpenCurly) {
                    // Parse record literal for requires signatures
                    requires_signatures = try self.parseBlockOrRecord();
                }
            }

            // Parse exposes list
            const exposes_idx = if (self.peek() == .OpenSquare) blk: {
                self.advance();
                const idx = try self.parseExposedList(.CloseSquare);
                _ = self.eat(.CloseSquare);
                break :blk idx;
            } else collections.NodeSlices(AST.Node.Idx).Idx.NIL;

            // Parse packages
            const packages_idx = if (self.peek() == .OpenCurly) blk: {
                self.advance();
                const idx = try self.parsePackageList();
                _ = self.eat(.CloseCurly);
                break :blk idx;
            } else collections.NodeSlices(AST.Node.Idx).Idx.NIL;

            // Parse provides (optional)
            const provides_idx = if (self.peek() == .KwProvides) blk: {
                self.advance();
                if (self.peek() == .OpenSquare) {
                    self.advance();
                    const idx = try self.parseExposedList(.CloseSquare);
                    _ = self.eat(.CloseSquare);
                    break :blk idx;
                }
                break :blk collections.NodeSlices(AST.Node.Idx).Idx.NIL;
            } else collections.NodeSlices(AST.Node.Idx).Idx.NIL;

            self.ast.header = .{ .platform = .{
                .name = name_node,
                .requires_rigids = rigids_idx,
                .requires_signatures = requires_signatures,
                .exposes = exposes_idx,
                .packages = packages_idx,
                .provides = provides_idx,
                .region = region.start,
            } };
            return .need_token;
        },

        .header_hosted => {
            // Parse hosted header
            _ = self.state_stack.pop();

            const region = self.currentRegion();
            self.advance(); // consume 'hosted'

            // Parse exposed list
            const exposes_idx = if (self.peek() == .OpenSquare) blk: {
                self.advance();
                const idx = try self.parseExposedList(.CloseSquare);
                _ = self.eat(.CloseSquare);
                break :blk idx;
            } else collections.NodeSlices(AST.Node.Idx).Idx.NIL;

            self.ast.header = .{ .hosted = .{
                .exposes = exposes_idx,
                .region = region.start,
            } };
            return .need_token;
        },

        .expr_binop_rhs => {
            // Parse the RHS of a binary operator
            _ = self.state_stack.pop();

            // Get the operator's right binding power
            const op_info = if (self.scratch_op_stack.items.len > 0)
                self.scratch_op_stack.items[self.scratch_op_stack.items.len - 1]
            else
                return .continue_processing;

            // Pop the LHS from value stack (it's stored in op_info)
            if (self.value_stack.items.len > 0) {
                _ = self.value_stack.pop();
            }

            // Parse RHS with the operator's right binding power
            try self.min_bp_stack.append(self.gpa, op_info.min_bp);
            try self.state_stack.append(self.gpa, .expr_start);
            return .continue_processing;
        },

        .expr_binop_combine => {
            // Combine LHS, operator, and RHS into a binop node
            _ = self.state_stack.pop();

            // Get operator info
            const op_info = if (self.scratch_op_stack.items.len > 0) blk: {
                const info = self.scratch_op_stack.items[self.scratch_op_stack.items.len - 1];
                _ = self.scratch_op_stack.pop();
                break :blk info;
            } else return .continue_processing;

            // Get RHS from value stack
            const rhs = if (self.value_stack.items.len > 0)
                self.value_stack.items[self.value_stack.items.len - 1]
            else
                return .continue_processing;
            _ = self.value_stack.pop();

            // Convert token to binop tag
            const binop_tag = tokenToBinOpTag(op_info.op_tag) orelse .binop_equals;

            // Create binop node
            const binop_idx = try self.ast.appendBinOp(self.gpa, op_info.left.?, rhs);
            const full_region = makeRegion(self.ast.start(op_info.left.?), self.ast.getRegion(rhs).end);
            const binop_node = try self.ast.appendNode(self.gpa, full_region, binop_tag, .{ .binop = binop_idx });

            // Push result back on value stack
            try self.value_stack.append(self.gpa, binop_node);

            // Continue checking for more operators
            try self.state_stack.append(self.gpa, .expr_check_binop);
            return .continue_processing;
        },

        .lambda_args => {
            // Parse lambda arguments between | |
            _ = self.state_stack.pop();

            // Expect opening |
            if (self.peek() != .OpBar) {
                // Malformed lambda
                const region = self.currentRegion();
                const node = try self.ast.appendNode(self.gpa, region, .malformed, .{ .malformed = .expr_unexpected_token });
                try self.value_stack.append(self.gpa, node);
                try self.state_stack.append(self.gpa, .expr_done);
                return .continue_processing;
            }

            self.advance(); // consume |

            // Collect arguments
            const scratch_marker = self.markScratchNodes();

            while (self.peek() != .OpBar and self.peek() != .EndOfFile) {
                // Parse lambda arguments (identifiers and underscores)
                switch (self.peek()) {
                    .LowerIdent => {
                        const ident = self.currentIdent();
                        const region = self.currentRegion();
                        self.advance();
                        if (ident) |id| {
                            const node = try self.ast.appendNode(self.gpa, region, .lc, .{ .ident = id });
                            try self.scratch_nodes.append(self.gpa, node);
                        }
                    },
                    .Underscore => {
                        const region = self.currentRegion();
                        self.advance();
                        const node = try self.ast.appendNode(self.gpa, region, .underscore, .{ .src_bytes_end = region.start });
                        try self.scratch_nodes.append(self.gpa, node);
                    },
                    else => break,
                }

                if (self.peek() == .Comma) {
                    self.advance();
                } else if (self.peek() != .OpBar) {
                    break;
                }
            }

            // Expect closing |
            if (self.peek() != .OpBar) {
                // Malformed lambda
                const region = self.currentRegion();
                const node = try self.ast.appendNode(self.gpa, region, .malformed, .{ .malformed = .expected_expr_bar });
                try self.value_stack.append(self.gpa, node);
                self.restoreScratchNodes(scratch_marker);
                try self.state_stack.append(self.gpa, .expr_done);
                return .continue_processing;
            }

            self.advance(); // consume closing |

            // Store args in scratch_nodes (they're already there)
            // The lambda_body state will collect them

            return .need_token;
        },

        .lambda_body => {
            // Parse lambda body
            _ = self.state_stack.pop();

            // Parse body as expression
            try self.state_stack.append(self.gpa, .expr_start);
            return .continue_processing;
        },

        .if_arrow => {
            // After parsing condition, expect arrow
            _ = self.state_stack.pop();

            if (self.peek() != .OpArrow) {
                // Error: expected arrow after if condition
                // But continue parsing anyway
            } else {
                self.advance(); // consume arrow
            }

            return .continue_processing;
        },

        .if_then_branch => {
            // Parse then branch expression
            _ = self.state_stack.pop();

            // Parse the then expression
            try self.state_stack.append(self.gpa, .expr_start);

            return .continue_processing;
        },

        .if_else_branch => {
            // Check for else keyword and parse else branch
            _ = self.state_stack.pop();

            if (self.peek() == .KwElse) {
                self.advance(); // consume 'else'

                // Parse else expression
                try self.state_stack.append(self.gpa, .expr_start);
            }
            // If no else, we'll create if without else branch

            return .continue_processing;
        },

        .if_complete => {
            // Complete if expression - combine all parts
            _ = self.state_stack.pop();

            // Pop else branch (if present)
            const else_branch = if (self.value_stack.items.len > 0) self.value_stack.pop() else null;

            // Pop then branch
            const then_branch = if (self.value_stack.items.len > 0) self.value_stack.pop() else null;

            // Pop condition
            const condition = if (self.value_stack.items.len > 0) self.value_stack.pop() else null;

            // Pop start position
            const start_pos_val = if (self.value_stack.items.len > 0) self.value_stack.pop() else null;
            const start_pos = if (start_pos_val) |val| intToPosition(nodeIdxToInt(val)) else Position{ .offset = 0 };

            // Build if expression node using scratch buffer
            const scratch_marker = self.markScratchNodes();
            defer self.restoreScratchNodes(scratch_marker);

            if (condition) |c| try self.scratch_nodes.append(self.gpa, c);
            if (then_branch) |t| try self.scratch_nodes.append(self.gpa, t);
            if (else_branch) |e| try self.scratch_nodes.append(self.gpa, e);

            const nodes = self.getScratchNodesSince(scratch_marker);
            const nodes_slice = try self.gpa.alloc(Node.Idx, nodes.len);
            @memcpy(nodes_slice, nodes);
            const nodes_idx = try self.ast.node_slices.append(self.gpa, nodes_slice);

            // Determine end position
            const end_pos = if (else_branch) |e|
                self.ast.getRegion(e).end
            else if (then_branch) |t|
                self.ast.getRegion(t).end
            else
                self.currentPosition();

            const region = makeRegion(start_pos, end_pos);
            const if_node = try self.ast.appendNode(self.gpa, region, if (else_branch != null) .if_else else .if_without_else, .{ .nodes = nodes_idx });

            try self.value_stack.append(self.gpa, if_node);

            return .continue_processing;
        },

        .match_branches => {
            // Parse match branches (pattern -> expr)
            _ = self.state_stack.pop();

            // Start parsing the first branch
            try self.state_stack.append(self.gpa, .match_branch_start);

            return .continue_processing;
        },

        .match_branch_start => {
            // Parse a single match branch (pattern -> expr)
            _ = self.state_stack.pop();

            // Push states to parse pattern first
            try self.state_stack.append(self.gpa, .match_branch_arrow);
            try self.state_stack.append(self.gpa, .pattern_start);

            return .continue_processing;
        },

        .pattern_start => {
            // Parse different pattern types
            const token = self.currentToken() orelse return .need_token;

            _ = self.state_stack.pop();

            switch (token.tag) {
                .LowerIdent => {
                    // Variable pattern - create identifier pattern
                    const ident = self.currentIdent();
                    const region = self.currentRegion();
                    self.advance();
                    if (ident) |id| {
                        const node = try self.ast.appendNode(self.gpa, region, .lc, .{ .ident = id });
                        try self.value_stack.append(self.gpa, node);
                    } else {
                        const node = try self.ast.appendNode(self.gpa, region, .malformed, .{ .malformed = .expr_unexpected_token });
                        try self.value_stack.append(self.gpa, node);
                    }
                },
                .UpperIdent => {
                    // Tag pattern - could be Tag or Tag(payload)
                    const ident = self.currentIdent();
                    const region = self.currentRegion();
                    self.advance();
                    if (ident) |id| {
                        const node = try self.ast.appendNode(self.gpa, region, .uc, .{ .ident = id });
                        try self.value_stack.append(self.gpa, node);

                        // Check if this is a tag with payload pattern like Ok(value) or Some(x)
                        if (self.peek() == .OpenRound) {
                            self.advance(); // consume (

                            // We need to parse the inner pattern and then create an apply node
                            // Push state to handle the closing paren and apply creation after parsing inner pattern
                            try self.state_stack.append(self.gpa, .pattern_tag_payload_close);
                            try self.state_stack.append(self.gpa, .pattern_start);
                        }
                    } else {
                        const node = try self.ast.appendNode(self.gpa, region, .malformed, .{ .malformed = .expr_unexpected_token });
                        try self.value_stack.append(self.gpa, node);
                    }
                },
                .Underscore => {
                    // Wildcard pattern - use underscore node type
                    const region = self.currentRegion();
                    const node = try self.ast.appendNode(self.gpa, region, .underscore, .{ .src_bytes_end = region.start });
                    try self.value_stack.append(self.gpa, node);
                    self.advance();
                },
                .Int, .Float, .String => {
                    // Literal pattern
                    const start_pos = self.currentPosition();
                    const region = self.currentRegion();
                    const node = switch (token.tag) {
                        .Int => try self.ast.appendNode(self.gpa, region, .num_literal_i32, .{ .num_literal_i32 = @intCast(start_pos.offset) }),
                        .Float => try self.ast.appendNode(self.gpa, region, .frac_literal_small, .{ .frac_literal_small = .{ .numerator = 0, .denominator_power_of_ten = 0 } }), // placeholder
                        .String => try self.ast.appendNode(self.gpa, region, .str_literal_small, .{ .str_literal_small = [4]u8{ 0, 0, 0, 0 } }), // placeholder
                        else => unreachable,
                    };
                    try self.value_stack.append(self.gpa, node);
                    self.advance();
                },
                else => {
                    // Invalid pattern token - create error node
                    const region = self.currentRegion();
                    const node = try self.ast.appendNode(self.gpa, region, .malformed, .{ .malformed = .expr_unexpected_token });
                    try self.value_stack.append(self.gpa, node);
                },
            }

            return .continue_processing;
        },

        .pattern_tag_payload_close => {
            // Handle closing parenthesis after tag payload pattern
            _ = self.state_stack.pop();

            // We should have the payload pattern on the value stack
            const payload = self.value_stack.pop() orelse {
                // Stack underflow - should not happen
                const malformed = try self.pushMalformed(.expr_unexpected_token, self.currentPosition());
                try self.value_stack.append(self.gpa, malformed);
                return .continue_processing;
            };

            // Expect closing paren
            if (self.peek() != .CloseRound) {
                try self.pushDiagnostic(.expected_close_round, self.currentPosition(), self.currentPosition());
                const malformed = try self.pushMalformed(.expected_close_round, self.currentPosition());
                try self.value_stack.append(self.gpa, malformed);
            } else {
                self.advance(); // consume )

                // The tag should be second on the stack (pushed before we started parsing payload)
                const tag_node = self.value_stack.pop() orelse {
                    // Stack underflow - should not happen
                    const malformed = try self.pushMalformed(.expr_unexpected_token, self.currentPosition());
                    try self.value_stack.append(self.gpa, malformed);
                    return .continue_processing;
                };

                // Create an apply node for the tag with payload
                // Get the region of the tag node
                const tag_node_idx_val = @intFromEnum(tag_node);
                const tag_node_u32_idx = @as(u32, @intCast(tag_node_idx_val));
                const tag_node_multilist_idx = @as(collections.SafeMultiList(Node).Idx, @enumFromInt(tag_node_u32_idx));
                const tag_node_data = self.ast.nodes.get(tag_node_multilist_idx);
                const apply_region = makeRegion(tag_node_data.region.start, self.last_position);
                const apply_node = try self.ast.appendNode(self.gpa, apply_region, .apply_uc, .{ .binop = try self.ast.appendBinOp(self.gpa, tag_node, payload) });
                try self.value_stack.append(self.gpa, apply_node);
            }

            return .continue_processing;
        },

        .match_branch_arrow => {
            // Expect arrow token between pattern and expression
            const token = self.currentToken() orelse return .need_token;

            _ = self.state_stack.pop();

            if (token.tag == .OpArrow) {
                self.advance();
                // Push state to parse the branch expression
                try self.state_stack.append(self.gpa, .match_branch_complete);
                try self.state_stack.append(self.gpa, .expr_start);
            } else {
                // Missing arrow - create error
                const region = self.currentRegion();
                const node = try self.ast.appendNode(self.gpa, region, .malformed, .{ .malformed = .expr_unexpected_token });
                try self.value_stack.append(self.gpa, node);
            }

            return .continue_processing;
        },

        .match_branch_complete => {
            // Combine pattern and expression into a branch
            _ = self.state_stack.pop();

            if (self.value_stack.items.len >= 2) {
                const expr = self.value_stack.items[self.value_stack.items.len - 1];
                const pattern = self.value_stack.items[self.value_stack.items.len - 2];
                self.value_stack.items.len -= 2; // Remove both items

                // Create branch node combining pattern and expression
                // Use current token position for branch region
                const region = self.currentRegion();

                // Create a tuple literal to represent the match branch (pattern, expr)
                const nodes = [_]Node.Idx{ pattern, expr };
                const slice_idx = try self.ast.node_slices.append(self.gpa, &nodes);
                const node = try self.ast.appendNode(self.gpa, region, .tuple_literal, .{ .nodes = slice_idx });
                try self.value_stack.append(self.gpa, node);
            } else {
                // Not enough values on stack - create error
                const region = self.currentRegion();
                const node = try self.ast.appendNode(self.gpa, region, .malformed, .{ .malformed = .expr_unexpected_token });
                try self.value_stack.append(self.gpa, node);
            }

            // Check if there are more branches (separated by newlines/commas)
            if (self.currentToken()) |next_token| {
                if (next_token.tag == .Comma) {
                    self.advance();
                    try self.state_stack.append(self.gpa, .match_branches_cont);
                }
            }

            return .continue_processing;
        },

        .match_branches_cont => {
            // Continue parsing additional match branches
            const token = self.currentToken() orelse return .need_token;

            _ = self.state_stack.pop();

            // Check if we have another pattern to parse
            switch (token.tag) {
                .LowerIdent, .UpperIdent, .Underscore, .Int, .Float, .String => {
                    // Start parsing another branch
                    try self.state_stack.append(self.gpa, .match_branch_start);
                },
                else => {
                    // No more branches - we're done
                    // The branches are already on the value stack
                },
            }

            return .continue_processing;
        },

        .while_body => {
            // Parse the body of the while loop
            _ = self.state_stack.pop();

            // We should have the condition on the value stack
            // Push state to parse the body expression
            try self.state_stack.append(self.gpa, .expr_start);

            return .continue_processing;
        },

        .while_complete => {
            // Complete the while loop
            _ = self.state_stack.pop();

            // We should have: region marker, condition, body on the stack
            if (self.value_stack.items.len >= 3) {
                const body = self.value_stack.pop() orelse return .continue_processing;
                const condition = self.value_stack.pop() orelse return .continue_processing;
                const region_marker = self.value_stack.pop() orelse return .continue_processing;

                // Reconstruct the region from the marker
                const start_offset = @as(u32, @intCast(@intFromEnum(region_marker)));
                const start_pos = Position{ .offset = start_offset };
                const body_region = self.ast.getRegion(body);
                const full_region = makeRegion(start_pos, body_region.end);

                // Create while node
                const nodes = [_]Node.Idx{ condition, body };
                const nodes_idx = try self.ast.appendNodeSlice(self.gpa, &nodes);
                const while_node = try self.ast.appendNode(self.gpa, full_region, .while_loop, .{ .nodes = nodes_idx });
                try self.value_stack.append(self.gpa, while_node);
            }

            return .continue_processing;
        },

        .for_in => {
            // Expect 'in' keyword
            _ = self.state_stack.pop();

            const token = self.currentToken() orelse return .need_token;

            if (token.tag != .KwIn) {
                try self.pushDiagnostic(.for_expected_in, self.currentPosition(), self.currentPosition());
                const malformed = try self.pushMalformed(.for_expected_in, self.currentPosition());
                try self.value_stack.append(self.gpa, malformed);
                return .continue_processing;
            }

            self.advance(); // consume 'in'

            // Push state to parse the iterator expression
            try self.state_stack.append(self.gpa, .expr_start);

            return .continue_processing;
        },

        .for_body => {
            // Parse the body of the for loop
            _ = self.state_stack.pop();

            // Push state to parse the body expression
            try self.state_stack.append(self.gpa, .expr_start);

            return .continue_processing;
        },

        .for_complete => {
            // Complete the for loop
            _ = self.state_stack.pop();

            // We should have: region marker, pattern, iterator, body on the stack
            if (self.value_stack.items.len >= 4) {
                const body = self.value_stack.pop() orelse return .continue_processing;
                const iterator = self.value_stack.pop() orelse return .continue_processing;
                const pattern = self.value_stack.pop() orelse return .continue_processing;
                const region_marker = self.value_stack.pop() orelse return .continue_processing;

                // Reconstruct the region from the marker
                const start_offset = @as(u32, @intCast(@intFromEnum(region_marker)));
                const start_pos = Position{ .offset = start_offset };
                const body_region = self.ast.getRegion(body);
                const full_region = makeRegion(start_pos, body_region.end);

                // Create for node
                const nodes = [_]Node.Idx{ pattern, iterator, body };
                const nodes_idx = try self.ast.appendNodeSlice(self.gpa, &nodes);
                const for_node = try self.ast.appendNode(self.gpa, full_region, .for_loop, .{ .nodes = nodes_idx });
                try self.value_stack.append(self.gpa, for_node);
            }

            return .continue_processing;
        },

        .return_complete => {
            // Complete the return statement
            _ = self.state_stack.pop();

            // We should have: region marker, expression on the stack
            if (self.value_stack.items.len >= 2) {
                const expr = self.value_stack.pop() orelse return .continue_processing;
                const region_marker = self.value_stack.pop() orelse return .continue_processing;

                // Reconstruct the region from the marker
                const start_offset = @as(u32, @intCast(@intFromEnum(region_marker)));
                const start_pos = Position{ .offset = start_offset };
                const expr_region = self.ast.getRegion(expr);
                const full_region = makeRegion(start_pos, expr_region.end);

                // Create return node
                const nodes = [_]Node.Idx{expr};
                const nodes_idx = try self.ast.appendNodeSlice(self.gpa, &nodes);
                const return_node = try self.ast.appendNode(self.gpa, full_region, .ret, .{ .nodes = nodes_idx });
                try self.value_stack.append(self.gpa, return_node);
            }

            return .continue_processing;
        },

        .crash_complete => {
            // Complete the crash statement
            _ = self.state_stack.pop();

            // We should have: region marker, expression on the stack
            if (self.value_stack.items.len >= 2) {
                const expr = self.value_stack.pop() orelse return .continue_processing;
                const region_marker = self.value_stack.pop() orelse return .continue_processing;

                // Reconstruct the region from the marker
                const start_offset = @as(u32, @intCast(@intFromEnum(region_marker)));
                const start_pos = Position{ .offset = start_offset };
                const expr_region = self.ast.getRegion(expr);
                const full_region = makeRegion(start_pos, expr_region.end);

                // Create crash node
                const nodes = [_]Node.Idx{expr};
                const nodes_idx = try self.ast.appendNodeSlice(self.gpa, &nodes);
                const crash_node = try self.ast.appendNode(self.gpa, full_region, .crash, .{ .nodes = nodes_idx });
                try self.value_stack.append(self.gpa, crash_node);
            }

            return .continue_processing;
        },

        .list_elements => {
            // Parse list elements
            _ = self.state_stack.pop();

            // Clear scratch for collecting elements
            try self.scratch_nodes.resize(self.gpa, 0);

            // Check if empty list
            if (self.peek() == .CloseSquare) {
                // Empty list - continue to list_complete
                return .continue_processing;
            }

            // Parse first element, then check for more
            try self.state_stack.append(self.gpa, .list_elements_cont);
            try self.state_stack.append(self.gpa, .expr_start);

            return .continue_processing;
        },

        .list_elements_cont => {
            // Continue parsing list elements after first one
            _ = self.state_stack.pop();

            // Pop the element we just parsed and add to scratch
            if (self.value_stack.items.len > 0) {
                const elem = self.value_stack.pop().?;
                try self.scratch_nodes.append(self.gpa, elem);
            }

            // Check for comma (more elements) or close square (done)
            if (self.peek() == .Comma) {
                self.advance(); // consume comma

                // Check if trailing comma before close
                if (self.peek() == .CloseSquare) {
                    // Trailing comma is fine, we're done
                    return .continue_processing;
                }

                // Parse next element
                try self.state_stack.append(self.gpa, .list_elements_cont);
                try self.state_stack.append(self.gpa, .expr_start);
                return .continue_processing;
            } else if (self.peek() == .CloseSquare) {
                // Done with elements
                return .continue_processing;
            } else {
                // Missing comma or close - error but continue
                // Try to parse another element anyway
                try self.state_stack.append(self.gpa, .list_elements_cont);
                try self.state_stack.append(self.gpa, .expr_start);
                return .continue_processing;
            }
        },

        .list_complete => {
            // Complete list literal
            _ = self.state_stack.pop();

            // Get start position
            const start_pos_val = if (self.value_stack.items.len > 0) self.value_stack.pop() else null;
            const start_pos = if (start_pos_val) |val| intToPosition(nodeIdxToInt(val)) else self.currentPosition();

            // Expect closing ]
            const end_pos = if (self.peek() == .CloseSquare) blk: {
                const r = self.currentRegion();
                self.advance();
                break :blk r.end;
            } else self.currentPosition();

            // Create list node
            const elements_slice = try self.gpa.alloc(Node.Idx, self.scratch_nodes.items.len);
            @memcpy(elements_slice, self.scratch_nodes.items);
            const nodes_idx = try self.ast.node_slices.append(self.gpa, elements_slice);

            // Ensure end is at least as far as start to avoid assertion failure
            const safe_end_pos = if (end_pos.offset < start_pos.offset) start_pos else end_pos;
            const region = makeRegion(start_pos, safe_end_pos);
            const list_node = try self.ast.appendNode(self.gpa, region, .list_literal, .{ .nodes = nodes_idx });

            try self.value_stack.append(self.gpa, list_node);

            return .continue_processing;
        },

        .paren_content => {
            // Parse content inside parentheses
            _ = self.state_stack.pop();

            // Check for empty tuple ()
            if (self.peek() == .CloseRound) {
                // Empty tuple
                try self.state_stack.append(self.gpa, .paren_complete);
                return .continue_processing;
            }

            // Parse first expression
            try self.state_stack.append(self.gpa, .paren_check_comma);
            try self.state_stack.append(self.gpa, .expr_start);

            return .continue_processing;
        },

        .paren_check_comma => {
            // Check if this is a tuple (has comma) or parenthesized expression
            _ = self.state_stack.pop();

            if (self.peek() == .Comma) {
                // It's a tuple - collect more elements
                self.advance(); // consume comma

                // Continue parsing tuple elements
                while (self.peek() != .CloseRound and self.peek() != .EndOfFile) {
                    // Parse next element
                    try self.state_stack.append(self.gpa, .expr_start);

                    // Process the expression synchronously since we're in a loop
                    while (self.state_stack.items.len > 0 and self.state_stack.getLast() == .expr_start) {
                        const action = try self.processState(.expr_start);
                        if (action == .need_token) {
                            return .need_token;
                        }
                    }

                    // Check for another comma
                    if (self.peek() == .Comma) {
                        self.advance();
                    } else {
                        break;
                    }
                }
            }
            // If no comma, it's a parenthesized expression (single element already on stack)

            try self.state_stack.append(self.gpa, .paren_complete);
            return .continue_processing;
        },

        .paren_complete => {
            // Complete parenthesized expression or tuple
            _ = self.state_stack.pop();

            // Get start position
            const start_pos_val = if (self.value_stack.items.len > 0) self.value_stack.items[self.value_stack.items.len - 2] else null;
            const start_pos = if (start_pos_val) |val| intToPosition(nodeIdxToInt(val)) else self.currentPosition();

            // Expect closing )
            const end_pos = if (self.peek() == .CloseRound) blk: {
                const r = self.currentRegion();
                self.advance();
                break :blk r.end;
            } else self.currentPosition();

            // Remove start position marker from value stack
            if (self.value_stack.items.len >= 2) {
                // Get the expression(s) between the start marker and now
                const expr_count = self.value_stack.items.len - 1; // -1 for start marker

                if (expr_count == 2) {
                    // Single expression - just parenthesized, return it as-is
                    const expr = self.value_stack.items[self.value_stack.items.len - 1];
                    self.value_stack.items.len -= 2; // Remove expr and start marker
                    try self.value_stack.append(self.gpa, expr);
                } else {
                    // Multiple expressions or empty - create tuple
                    const start_idx = self.value_stack.items.len - expr_count;
                    const elements = self.value_stack.items[start_idx + 1 ..]; // Skip start marker

                    const elements_slice = try self.gpa.alloc(Node.Idx, elements.len);
                    @memcpy(elements_slice, elements);
                    const nodes_idx = try self.ast.node_slices.append(self.gpa, elements_slice);

                    // Clear value stack of elements and start marker
                    self.value_stack.items.len = start_idx;

                    const region = makeRegion(start_pos, end_pos);
                    const tuple_node = try self.ast.appendNode(self.gpa, region, .tuple_literal, .{ .nodes = nodes_idx });
                    try self.value_stack.append(self.gpa, tuple_node);
                }
            }

            return .continue_processing;
        },

        .record_fields => {
            // Parse record fields
            _ = self.state_stack.pop();

            // Clear scratch for collecting fields
            try self.scratch_nodes.resize(self.gpa, 0);

            // Check if empty record
            if (self.peek() == .CloseCurly) {
                // Empty record - continue to record_complete
                return .continue_processing;
            }

            // Parse first field, then check for more
            try self.state_stack.append(self.gpa, .record_fields_cont);
            try self.state_stack.append(self.gpa, .record_field_start);

            return .continue_processing;
        },

        .record_field_start => {
            // Parse a record field name
            _ = self.state_stack.pop();

            if (self.peek() == .LowerIdent) {
                const ident = self.currentIdent();
                const region = self.currentRegion();
                self.advance();

                if (ident) |id| {
                    const field_name = try self.ast.appendNode(self.gpa, region, .lc, .{ .ident = id });

                    // Check for colon (field with value) or comma/close (shorthand)
                    if (self.peek() == .OpColon) {
                        self.advance(); // consume colon

                        // Parse field value expression
                        try self.value_stack.append(self.gpa, field_name); // Save field name
                        try self.state_stack.append(self.gpa, .record_field_value);
                        try self.state_stack.append(self.gpa, .expr_start);
                    } else {
                        // Shorthand field (name only)
                        try self.scratch_nodes.append(self.gpa, field_name);
                    }
                }
            } else {
                // Unexpected token in field position - skip it
                self.advance();
            }

            return .continue_processing;
        },

        .record_field_value => {
            // Complete a field with colon and value
            _ = self.state_stack.pop();

            // Get the value we just parsed
            const value = if (self.value_stack.items.len > 0) self.value_stack.pop() else null;
            // Get the field name we saved
            const field_name = if (self.value_stack.items.len > 0) self.value_stack.pop() else null;

            if (value != null and field_name != null) {
                // Create binop for field: value
                const binop_idx = try self.ast.appendBinOp(self.gpa, field_name.?, value.?);
                const field_region = makeRegion(self.ast.getRegion(field_name.?).start, self.ast.getRegion(value.?).end);
                const field_node = try self.ast.appendNode(self.gpa, field_region, .binop_colon, .{ .binop = binop_idx });
                try self.scratch_nodes.append(self.gpa, field_node);
            }

            return .continue_processing;
        },

        .record_fields_cont => {
            // Continue parsing record fields after first one
            _ = self.state_stack.pop();

            // Check for comma (more fields) or close curly (done)
            if (self.peek() == .Comma) {
                self.advance(); // consume comma

                // Check if trailing comma before close
                if (self.peek() == .CloseCurly) {
                    // Trailing comma is fine, we're done
                    return .continue_processing;
                }

                // Parse next field
                try self.state_stack.append(self.gpa, .record_fields_cont);
                try self.state_stack.append(self.gpa, .record_field_start);
                return .continue_processing;
            } else if (self.peek() == .CloseCurly) {
                // Done with fields
                return .continue_processing;
            } else {
                // Missing comma or close - error but continue
                // Try to parse another field anyway
                try self.state_stack.append(self.gpa, .record_fields_cont);
                try self.state_stack.append(self.gpa, .record_field_start);
                return .continue_processing;
            }
        },

        .record_complete => {
            // Complete record literal
            _ = self.state_stack.pop();

            // Get start position
            const start_pos_val = if (self.value_stack.items.len > 0) self.value_stack.pop() else null;
            const start_pos = if (start_pos_val) |val| intToPosition(nodeIdxToInt(val)) else self.currentPosition();

            // Expect closing }
            const end_pos = if (self.peek() == .CloseCurly) blk: {
                const r = self.currentRegion();
                self.advance();
                break :blk r.end;
            } else self.currentPosition();

            // Create record node
            const fields_slice = try self.gpa.alloc(Node.Idx, self.scratch_nodes.items.len);
            @memcpy(fields_slice, self.scratch_nodes.items);
            const nodes_idx = try self.ast.node_slices.append(self.gpa, fields_slice);

            // Ensure end is at least as far as start to avoid assertion failure
            const safe_end_pos = if (end_pos.offset < start_pos.offset) start_pos else end_pos;
            const region = makeRegion(start_pos, safe_end_pos);
            const record_node = try self.ast.appendNode(self.gpa, region, .record_literal, .{ .nodes = nodes_idx });

            try self.value_stack.append(self.gpa, record_node);

            return .continue_processing;
        },

        .match_complete => {
            // Complete match expression by combining scrutinee and branches
            _ = self.state_stack.pop();

            // We should have branches on the value stack and the match start position marker
            if (self.value_stack.items.len >= 2) {
                const branches = self.value_stack.items[self.value_stack.items.len - 1];
                const scrutinee = self.value_stack.items[self.value_stack.items.len - 2];

                // The first item should be the match start position marker (pushed as a node)
                if (self.value_stack.items.len >= 3) {
                    self.value_stack.items.len -= 3; // Remove all three items

                    // Create the complete match expression
                    // Match expressions are represented as a pair of (scrutinee, branches)
                    const match_nodes = [_]Node.Idx{ scrutinee, branches };
                    const slice_idx = try self.ast.node_slices.append(self.gpa, &match_nodes);

                    // Create match node using current region
                    const region = self.currentRegion();
                    const match_node = try self.ast.appendNode(self.gpa, region, .match, .{ .nodes = slice_idx });
                    try self.value_stack.append(self.gpa, match_node);
                } else {
                    // Fallback: just combine scrutinee and branches
                    self.value_stack.items.len -= 2;
                    const match_nodes = [_]Node.Idx{ scrutinee, branches };
                    const slice_idx = try self.ast.node_slices.append(self.gpa, &match_nodes);

                    const region = self.currentRegion();
                    const match_node = try self.ast.appendNode(self.gpa, region, .match, .{ .nodes = slice_idx });
                    try self.value_stack.append(self.gpa, match_node);
                }
            } else {
                // Not enough values - create error node
                const region = self.currentRegion();
                const node = try self.ast.appendNode(self.gpa, region, .malformed, .{ .malformed = .expr_unexpected_token });
                try self.value_stack.append(self.gpa, node);
            }

            return .continue_processing;
        },

        .lambda_complete => {
            // Combine args and body into lambda node
            _ = self.state_stack.pop();

            // Get body from value stack
            const body = if (self.value_stack.items.len > 0)
                self.value_stack.items[self.value_stack.items.len - 1]
            else blk: {
                // No body, create malformed
                const region = self.currentRegion();
                break :blk try self.ast.appendNode(self.gpa, region, .malformed, .{ .malformed = .expr_unexpected_token });
            };
            _ = self.value_stack.pop();

            // Get args from scratch_nodes
            const args = self.getScratchNodesSince(0);

            // Build body_then_args array using scratch buffer
            const scratch_marker = self.markScratchNodes();
            defer self.restoreScratchNodes(scratch_marker);

            try self.scratch_nodes.append(self.gpa, body);
            try self.scratch_nodes.appendSlice(self.gpa, args);

            const temp_nodes = self.getScratchNodesSince(scratch_marker);
            const nodes_idx = try self.ast.appendNodeSlice(self.gpa, temp_nodes);

            // Calculate region from args (first |) to body end
            const start_pos = if (args.len > 0)
                self.ast.getRegion(args[0]).start
            else
                self.currentPosition();
            const end_pos = self.ast.getRegion(body).end;
            const region = makeRegion(start_pos, end_pos);

            const lambda_node = try self.ast.appendNode(self.gpa, region, .lambda, .{ .body_then_args = nodes_idx });

            // Push lambda node to value stack
            try self.value_stack.append(self.gpa, lambda_node);

            // Continue with expression parsing
            try self.state_stack.append(self.gpa, .expr_check_binop);
            return .continue_processing;
        },

        .type_expr_start => {
            // Parse type expression
            _ = self.state_stack.pop();

            // Parse type expressions using the expression parser
            // Type-specific syntax (function types, records, etc.) is handled
            // by the expression parser and distinguished during canonicalization
            try self.state_stack.append(self.gpa, .expr_start);

            return .continue_processing;
        },

        .file_header_done => {
            // Header parsing is complete, transition to statement parsing
            _ = self.state_stack.pop();
            try self.state_stack.append(self.gpa, .file_stmt_loop);
            return .continue_processing;
        },

        .primary_ident, .primary_number, .primary_string, .primary_list, .primary_record, .primary_if, .primary_match, .primary_lambda => {
            // These primary expression states are deprecated
            // They've been consolidated into expr_primary which handles all primary expressions
            _ = self.state_stack.pop();
            try self.state_stack.append(self.gpa, .expr_primary);
            return .continue_processing;
        },

        .expr_combine => {
            // Expression combination state - deprecated, use expr_done instead
            _ = self.state_stack.pop();
            try self.state_stack.append(self.gpa, .expr_done);
            return .continue_processing;
        },

        .done => {
            // Parsing is complete
            _ = self.state_stack.pop();
            return .complete;
        },

        .parse_error => {
            // Parse error state - recover by continuing to next statement
            _ = self.state_stack.pop();

            // Create error node to mark the location
            const region = self.currentRegion();
            const error_node = try self.ast.appendNode(self.gpa, region, .malformed, .{ .malformed = .expr_unexpected_token });
            try self.value_stack.append(self.gpa, error_node);

            // Skip to next viable parsing position
            // Look for statement boundaries or structural tokens
            while (self.peek() != .EndOfFile) {
                switch (self.peek()) {
                    // Statement starters
                    .LowerIdent,
                    .UpperIdent,
                    .KwIf,
                    .KwMatch,
                    .KwFor,
                    .KwWhile,
                    .KwReturn,
                    .KwCrash,
                    // Structural tokens that might start new context
                    .OpenCurly,
                    .OpenSquare,
                    .OpenRound,
                    => break,
                    else => self.advance(),
                }
            }

            // Try to continue with statement parsing
            try self.state_stack.append(self.gpa, .stmt_start);
            return .continue_processing;
        },
    }
}

/// Get the region of the last consumed token
fn getLastConsumedRegion(self: *Parser) Region {
    return self.last_region;
}

/// look ahead at the next token and return an error if it does not have the expected tag
/// Following our philosophy to NEVER stop parsing, this returns an error that can be caught
/// but the caller should always handle it and continue parsing
fn expect(self: *Parser, expected: Token.Tag) error{ExpectedNotFound}!void {
    if (self.peek() != expected) {
        return error.ExpectedNotFound;
    }
    self.advance();
}

/// Peek at the token at the current position (1 token lookahead only!)
/// This is an LL(1) parser - we only ever look at the current token.
// duplicate peek function removed

/// Get the current token (if available)
fn currentToken(self: *Parser) ?Token {
    return self.current;
}

/// Get the current token's extra data
fn currentExtra(self: *Parser) Token.Extra {
    if (self.currentToken()) |token| {
        return token.extra;
    }
    return .{ .none = 0 };
}

/// Try to consume a token of a specific type
fn eat(self: *Parser, tag: Token.Tag) bool {
    if (self.peek() == tag) {
        self.advance();
        return true;
    }
    return false;
}

/// Get the current token's region
fn currentRegion(self: *Parser) base.Region {
    if (self.currentToken()) |token| {
        return token.region;
    }
    // Return a region at the current position when no token is available
    const pos = self.currentPosition();
    return base.Region.from_raw_offsets(pos.offset, pos.offset);
}

/// For backwards compatibility with code that used self.pos for error tracking
/// This will return the current position for error messages
fn getCurrentErrorPos(self: *Parser) Position {
    return self.currentPosition();
}

/// The error set that methods of the Parser return
pub const Error = std.mem.Allocator.Error;

/// Get the current token's position
fn currentPosition(self: *Parser) Position {
    if (self.currentToken()) |token| {
        return token.region.start;
    }
    // If no current token but we have lookahead, use its position
    if (self.lookahead) |token| {
        return token.region.start;
    }
    // If we have a last position, use it
    if (self.last_position.offset > 0) {
        return self.last_position;
    }
    // As a last resort, if we're at the very beginning and haven't parsed anything,
    // return position 0 (which will show the first character of the source)
    return Position{ .offset = 0 };
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
fn pushDiagnostic(self: *Parser, tag: AST.Diagnostic.Tag, start_pos: Position, end_pos: Position) Error!void {
    if (self.diagnostics.items.len < MAX_PARSE_DIAGNOSTICS) {
        try self.diagnostics.append(self.gpa, .{ .tag = tag, .region = .{ .start = start_pos, .end = end_pos } });
    }
}

/// add a malformed node
fn pushMalformed(self: *Parser, tag: AST.Diagnostic.Tag, start_pos: Position) Error!Node.Idx {
    // Use current token position for the error if the provided start_pos is invalid
    const actual_start_pos = if (start_pos.offset == 0) blk: {
        if (self.currentPosition().offset > 0) {
            break :blk self.currentPosition();
        } else {
            break :blk self.last_position;
        }
    } else start_pos;

    var end_pos = self.currentPosition();

    if (self.peek() != .EndOfFile) {
        self.advance();
        end_pos = self.currentPosition();
    }

    std.debug.assert(end_pos.offset >= actual_start_pos.offset);

    // Only add diagnostics if we haven't hit the limit
    if (self.diagnostics.items.len < MAX_PARSE_DIAGNOSTICS) {
        try self.pushDiagnostic(tag, actual_start_pos, end_pos);
    }

    // Always create a proper malformed node with the correct tag and position
    const malformed_region = makeRegion(actual_start_pos, end_pos);
    return try self.ast.appendNode(self.gpa, malformed_region, .malformed, .{ .malformed = tag });
}

/// parse a `.roc` module
///
/// the tokens are provided at Parser initialisation
pub fn parseFile(self: *Parser) Error!?Node.Idx {
    const trace = tracy.trace(@src());
    defer trace.end();

    // Check if already tokenized
    const already_tokenized = self.current != null;

    // Initialize tokenizer if not already initialized
    var messages: [128]tokenize_iter.Diagnostic = undefined;
    var token_iter: ?tokenize_iter.TokenIterator = null;

    if (!already_tokenized) {
        // Create tokenizer using the shared environment
        token_iter = try tokenize_iter.TokenIterator.init(self.env, self.gpa, self.source, &messages, self.byte_slices);

        // Get first two non-comment tokens to start
        var first: ?Token = null;
        while (true) {
            const token = try token_iter.?.next(self.gpa) orelse {
                // No tokens at all - empty file
                if (token_iter) |*t| t.deinit(self.gpa);
                return null;
            };
            // Skip comments and blank lines
            switch (token.tag) {
                .LineComment, .DocComment, .BlankLine => continue,
                else => {
                    first = token;
                    break;
                },
            }
        }

        // Get second non-comment token
        var second: ?Token = null;
        while (true) {
            const token = try token_iter.?.next(self.gpa);
            if (token == null) break;
            // Skip comments and blank lines
            switch (token.?.tag) {
                .LineComment, .DocComment, .BlankLine => continue,
                else => {
                    second = token;
                    break;
                },
            }
        }

        self.current = first;
        self.lookahead = second;

        // Store the token iterator so advance() can fetch new tokens
        self.token_iterator = &token_iter.?;
    }

    defer {
        if (!already_tokenized) {
            self.token_iterator = null;
            if (token_iter) |*t| t.deinit(self.gpa);
        }
    }

    // Parse the header if present (it will check for header keywords)
    // Check if the current token is a header keyword
    switch (self.peek()) {
        .KwApp, .KwModule, .KwPackage, .KwPlatform, .KwHosted, .KwInterface => {
            try self.parseHeaderFromCurrentToken();
        },
        else => {},
    }

    // Parse top-level statements
    const scratch_marker = self.markScratchNodes();
    defer self.restoreScratchNodes(scratch_marker);

    while (self.peek() != .EndOfFile) {
        const stmt = try self.parseTopLevelStatement();
        if (stmt) |s| {
            try self.scratch_nodes.append(self.gpa, s);
        }
    }

    // Store all statements as the module body
    const statements = self.getScratchNodesSince(scratch_marker);
    if (statements.len > 0) {
        const body_idx = try self.ast.appendNodeSlice(self.gpa, statements);
        // Module body spans from first to last statement
        const first_region = self.ast.nodes.fieldItem(.region, @as(collections.SafeMultiList(Node).Idx, @enumFromInt(@intFromEnum(statements[0]))));
        const last_region = self.ast.nodes.fieldItem(.region, @as(collections.SafeMultiList(Node).Idx, @enumFromInt(@intFromEnum(statements[statements.len - 1]))));
        const body_region = makeRegion(first_region.start, last_region.end);
        return try self.ast.appendNode(self.gpa, body_region, .block, .{ .nodes = body_idx });
    }
    return null; // No statements in file
}

/// Parse header from already-tokenized input (used by parseFile)
fn parseHeaderFromCurrentToken(self: *Parser) Error!void {
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
        .KwInterface => {
            // Interface is an obsolete keyword - we now just use 'module'
            // Treat it as an error
            try self.pushDiagnostic(.obsolete_interface_keyword, start_pos, self.currentPosition());
            _ = try self.pushMalformed(.obsolete_interface_keyword, start_pos);
            return;
        },
        else => {
            // No header - that's fine
        },
    }
}

/// Parse module header
pub fn parseHeader(self: *Parser) Error!void {
    var messages: [128]tokenize_iter.Diagnostic = undefined;

    // Create tokenizer using the shared environment
    var token_iter = try tokenize_iter.TokenIterator.init(self.env, self.gpa, self.source, &messages, self.byte_slices);
    defer token_iter.deinit(self.gpa);

    // Get first two non-comment tokens to start
    var first: ?Token = null;
    while (true) {
        const token = try token_iter.next(self.gpa) orelse {
            // No tokens at all - no header
            return;
        };
        switch (token.tag) {
            .LineComment, .DocComment, .BlankLine => continue,
            else => {
                first = token;
                break;
            },
        }
    }

    var second: ?Token = null;
    while (true) {
        const token = try token_iter.next(self.gpa);
        if (token == null) break;
        switch (token.?.tag) {
            .LineComment, .DocComment, .BlankLine => continue,
            else => {
                second = token;
                break;
            },
        }
    }

    self.current = first;
    self.lookahead = second;

    // Store the token iterator so advance() can fetch new tokens
    self.token_iterator = &token_iter;
    defer self.token_iterator = null;

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
    // Parse app [exports] { pf: platform "...", package1: "...", ... }

    // First, check for optional exports list
    var exports_idx: collections.NodeSlices(AST.Node.Idx).Idx = collections.NodeSlices(AST.Node.Idx).Idx.NIL;
    if (self.peek() == .OpenSquare) {
        self.advance(); // consume [
        exports_idx = try self.parseExposedList(.CloseSquare);
        self.expect(.CloseSquare) catch {
            try self.pushDiagnostic(.header_expected_close_square, start_pos, self.currentPosition());
        };
    }

    // Now parse the packages record
    self.expect(.OpenCurly) catch {
        try self.pushDiagnostic(.expected_package_platform_open_curly, start_pos, self.currentPosition());
        return AST.Header{ .app = .{
            .exposes = exports_idx,
            .packages = nodeSlicesIdxFromInt(0),
            .region = start_pos,
        } };
    };

    const scratch_marker = self.markScratchNodes();
    defer self.restoreScratchNodes(scratch_marker);

    // Parse record fields inside {}
    while (self.peek() != .CloseCurly and self.peek() != .EndOfFile) {
        const field_start = self.currentPosition();

        // Expect field name
        if (self.peek() != .LowerIdent) {
            try self.pushDiagnostic(.expected_package_or_platform_name, start_pos, self.currentPosition());
            break;
        }

        const field_name = self.currentIdent();
        const name_region = self.currentRegion();
        self.advance();

        // Expect colon
        self.expect(.OpColon) catch {
            try self.pushDiagnostic(.expected_package_or_platform_colon, start_pos, self.currentPosition());
            break;
        };

        // Parse the value part - handle both "platform <string>" and regular strings
        if (self.peek() == .KwPlatform) {
            // This is a platform field: pf: platform "path"
            self.advance(); // consume 'platform' keyword

            // Now expect a string for the platform path
            if (self.peek() != .String and self.peek() != .MultilineString) {
                try self.pushDiagnostic(.expected_package_or_platform_string, field_start, self.currentPosition());
                break;
            }

            const string_value = try self.parseStoredStringExpr();

            // For app headers with platform, we need to create a platform node
            // The exports list was already parsed at the beginning
            if (field_name) |name| {
                const field_node = try self.ast.appendNode(self.gpa, name_region, .lc, .{ .ident = name });

                // Create an empty block node for the provides (already handled in exports)
                const empty_slice: []const Node.Idx = &.{};
                const empty_nodes_idx = try self.ast.appendNodeSlice(self.gpa, empty_slice);
                const empty_block = try self.ast.appendNode(self.gpa, makeRegion(self.currentPosition(), self.currentPosition()), .block, .{ .nodes = empty_nodes_idx });

                // Create the platform binop: string_value platform empty_block
                const platform_binop_idx = try self.ast.appendBinOp(self.gpa, string_value, empty_block);
                const string_region = self.ast.nodes.fieldItem(.region, @as(collections.SafeMultiList(Node).Idx, @enumFromInt(@intFromEnum(string_value))));
                const platform_node = try self.ast.appendNode(self.gpa, string_region, .binop_platform, .{ .binop = platform_binop_idx });

                // Create the field: name : platform_node
                const field_binop_idx = try self.ast.appendBinOp(self.gpa, field_node, platform_node);
                const field_region = makeRegion(name_region.start, string_region.end);
                const field = try self.ast.appendNode(self.gpa, field_region, .binop_colon, .{ .binop = field_binop_idx });
                try self.scratch_nodes.append(self.gpa, field);
            }
        } else if (self.peek() == .String) {
            // This is a regular package string or old-style platform
            // Parse just the string literal, not a full expression
            const string_value = try self.parseStoredStringExpr();

            // Check if followed by 'platform' keyword (old style)
            if (self.peek() == .KwPlatform) {
                self.advance(); // consume 'platform' keyword

                // Parse provides list after platform keyword
                const provides_start = self.currentPosition();
                self.expect(.OpenSquare) catch {
                    try self.pushDiagnostic(.header_expected_open_square, field_start, self.currentPosition());
                    continue;
                };

                const provides_idx = try self.parseExposedList(.CloseSquare);

                self.expect(.CloseSquare) catch {
                    try self.pushDiagnostic(.header_expected_close_square, field_start, self.currentPosition());
                };

                // Create the provides list node (use block as a container for the exposed items)
                // The region for the provides list spans from the start to the last exposed item
                const provides_region = if (provides_idx != @as(collections.NodeSlices(Node.Idx).Idx, nodeSlicesIdxFromInt(0))) blk: {
                    // Get the region of the provides list (should have items)
                    var iter = self.ast.node_slices.nodes(&provides_idx);
                    var last_item: ?Node.Idx = null;
                    while (iter.next()) |item| {
                        last_item = item;
                    }
                    if (last_item) |last| {
                        const last_region = self.ast.nodes.fieldItem(.region, @as(collections.SafeMultiList(Node).Idx, @enumFromInt(@intFromEnum(last))));
                        break :blk makeRegion(provides_start, last_region.end);
                    } else {
                        // Empty list, use current position for end
                        const end_pos = self.currentPosition();
                        break :blk makeRegion(provides_start, end_pos);
                    }
                } else blk: {
                    // No items, use current position for end
                    const end_pos = self.currentPosition();
                    break :blk makeRegion(provides_start, end_pos);
                };
                const provides_node = try self.ast.appendNode(self.gpa, provides_region, .block, .{ .nodes = provides_idx });

                // Create the platform binop: string_value platform provides_node
                const platform_binop_idx = try self.ast.appendBinOp(self.gpa, string_value, provides_node);
                const string_region = self.ast.nodes.fieldItem(.region, @as(collections.SafeMultiList(Node).Idx, @enumFromInt(@intFromEnum(string_value))));
                const platform_region = makeRegion(string_region.start, provides_region.end);
                const platform_node = try self.ast.appendNode(self.gpa, platform_region, .binop_platform, .{ .binop = platform_binop_idx });

                // Create the record field: field_name : platform_node
                if (field_name) |name| {
                    const field_node = try self.ast.appendNode(self.gpa, name_region, .lc, .{ .ident = name });
                    const field_binop_idx = try self.ast.appendBinOp(self.gpa, field_node, platform_node);
                    const field_region = makeRegion(name_region.start, platform_region.end);
                    const field = try self.ast.appendNode(self.gpa, field_region, .binop_colon, .{ .binop = field_binop_idx });
                    try self.scratch_nodes.append(self.gpa, field);
                }
            } else {
                // Regular package field
                if (field_name) |name| {
                    const field_node = try self.ast.appendNode(self.gpa, name_region, .lc, .{ .ident = name });
                    const binop_idx = try self.ast.appendBinOp(self.gpa, field_node, string_value);
                    const string_region = self.ast.nodes.fieldItem(.region, @as(collections.SafeMultiList(Node).Idx, @enumFromInt(@intFromEnum(string_value))));
                    const field_region = makeRegion(name_region.start, string_region.end);
                    const field = try self.ast.appendNode(self.gpa, field_region, .binop_colon, .{ .binop = binop_idx });
                    try self.scratch_nodes.append(self.gpa, field);
                }
            }
        } else {
            try self.pushDiagnostic(.expected_package_or_platform_string, start_pos, self.currentPosition());
            break;
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
    const packages_nodes = self.getScratchNodesSince(scratch_marker);
    const packages_idx = if (packages_nodes.len > 0)
        try self.ast.appendNodeSlice(self.gpa, packages_nodes)
    else
        collections.NodeSlices(AST.Node.Idx).Idx.NIL;

    return AST.Header{ .app = .{
        .exposes = exports_idx,
        .packages = packages_idx,
        .region = start_pos,
    } };
}

fn parseModuleHeader(self: *Parser, start_pos: Position) Error!AST.Header {
    self.expect(.OpenSquare) catch {
        try self.pushDiagnostic(.header_expected_open_square, start_pos, self.currentPosition());
        return AST.Header{ .module = .{
            .exposes = nodeSlicesIdxFromInt(0),
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
            .exposes = nodeSlicesIdxFromInt(0),
            .packages = nodeSlicesIdxFromInt(0),
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
    } else collections.NodeSlices(AST.Node.Idx).Idx.NIL;

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
            .requires_rigids = collections.NodeSlices(AST.Node.Idx).Idx.NIL,
            .requires_signatures = @as(Node.Idx, @enumFromInt(std.math.minInt(i32))),
            .exposes = collections.NodeSlices(AST.Node.Idx).Idx.NIL,
            .packages = collections.NodeSlices(AST.Node.Idx).Idx.NIL,
            .provides = collections.NodeSlices(AST.Node.Idx).Idx.NIL,
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

    // Parse signatures record { field : Type, ... }
    const sig_start = self.currentPosition();
    self.expect(.OpenCurly) catch {
        try self.pushDiagnostic(.expected_requires_signatures_open_curly, start_pos, self.currentPosition());
    };

    // Parse the contents of the record manually since we already consumed the opening brace
    const scratch_marker = self.markScratchNodes();
    defer self.restoreScratchNodes(scratch_marker);

    while (self.peek() != .CloseCurly and self.peek() != .EndOfFile) {
        // Parse field name
        const field_start = self.currentPosition();
        if (self.peek() != .LowerIdent) {
            _ = try self.pushMalformed(.expr_unexpected_token, self.getCurrentErrorPos());
            break;
        }

        const field_ident = self.currentIdent();
        const field_region = self.currentRegion();
        self.advance();

        self.expect(.OpColon) catch {
            _ = try self.pushMalformed(.expected_colon_after_pat_field_name, self.getCurrentErrorPos());
            break;
        };

        // Parse type annotation
        // Comma has precedence 0/1, arrow has precedence 2/3
        // If we parse at precedence 1, we'll stop at commas but still parse arrows
        const type_expr = try self.parseExprWithPrecedence(1);

        // Create field : type node
        if (field_ident) |id| {
            const field_node = try self.ast.appendNode(self.gpa, field_region, .lc, .{ .ident = id });
            const binop_idx = try self.ast.appendBinOp(self.gpa, field_node, type_expr);
            const full_region = makeRegion(field_start, self.ast.getRegion(type_expr).end);
            const binop_node = try self.ast.appendNode(self.gpa, full_region, .binop_colon, .{ .binop = binop_idx });
            try self.scratch_nodes.append(self.gpa, binop_node);
        }

        if (self.peek() == .Comma) {
            self.advance();
        } else {
            break;
        }
    }

    const fields = self.getScratchNodesSince(scratch_marker);
    const fields_idx = if (fields.len > 0)
        try self.ast.appendNodeSlice(self.gpa, fields)
    else
        collections.NodeSlices(AST.Node.Idx).Idx.NIL;

    const sig_end = self.currentPosition();
    const signatures_idx = try self.ast.appendNode(self.gpa, makeRegion(sig_start, sig_end), .record_literal, .{ .nodes = fields_idx });

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
    // hosted [exports] - the exposes list comes directly after hosted, no "exposes" keyword
    self.expect(.OpenSquare) catch {
        try self.pushDiagnostic(.expected_exposes_open_square, start_pos, self.currentPosition());
        return AST.Header{ .hosted = .{
            .exposes = nodeSlicesIdxFromInt(0),
            .region = start_pos,
        } };
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

        const scratch_marker = self.markScratchNodes();
        defer self.restoreScratchNodes(scratch_marker);

        // Parse key-value pairs
        while (self.peek() != .CloseCurly and self.peek() != .EndOfFile) {
            // Parse field name
            if (self.peek() != .LowerIdent) {
                return self.pushMalformed(.expr_unexpected_token, start_position);
            }

            const field_ident = self.currentIdent();
            const field_region = self.currentRegion();
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
                const field_node = try self.ast.appendNode(self.gpa, field_region, .lc, .{ .ident = id });
                try self.scratch_nodes.append(self.gpa, field_node);
                try self.scratch_nodes.append(self.gpa, value);
            }

            if (self.peek() == .Comma) {
                self.advance();
            } else {
                break;
            }
        }

        var close_brace_region: Region = undefined;
        if (self.peek() == .CloseCurly) {
            close_brace_region = self.currentRegion();
            self.advance();
        } else {
            try self.pushDiagnostic(.expected_expr_close_curly, start_position, self.currentPosition());
            close_brace_region = makeRegion(self.currentPosition(), self.currentPosition());
        }

        const nodes = self.getScratchNodesSince(scratch_marker);
        const nodes_idx = try self.ast.appendNodeSlice(self.gpa, nodes);
        const full_region = makeRegion(start_position, close_brace_region.end);
        return try self.ast.appendNode(self.gpa, full_region, .record_literal, .{ .nodes = nodes_idx });
    } else {
        // Just parse as expression (e.g., platform "...")
        return try self.parseExpr();
    }
}

fn parsePackageList(self: *Parser) Error!collections.NodeSlices(AST.Node.Idx).Idx {
    const scratch_marker = self.markScratchNodes();
    defer self.restoreScratchNodes(scratch_marker);

    while (self.peek() != .CloseCurly and self.peek() != .EndOfFile) {
        // Parse package name
        if (self.peek() != .LowerIdent) {
            _ = try self.pushMalformed(.expr_unexpected_token, self.getCurrentErrorPos());
            break;
        }

        const pkg_ident = self.currentIdent();
        const pkg_region = self.currentRegion();
        self.advance();

        self.expect(.OpColon) catch {
            _ = try self.pushMalformed(.expected_colon_after_pat_field_name, self.getCurrentErrorPos());
            break;
        };

        // Parse package path
        const path = try self.parseExpr();

        // Create package entry
        if (pkg_ident) |id| {
            const pkg_node = try self.ast.appendNode(self.gpa, pkg_region, .lc, .{ .ident = id });
            try self.scratch_nodes.append(self.gpa, pkg_node);
            try self.scratch_nodes.append(self.gpa, path);
        }

        if (self.peek() == .Comma) {
            self.advance();
        } else {
            break;
        }
    }

    const packages = self.getScratchNodesSince(scratch_marker);
    if (packages.len == 0) {
        return collections.NodeSlices(AST.Node.Idx).Idx.NIL;
    }
    return try self.ast.appendNodeSlice(self.gpa, packages);
}

fn parseExposedList(self: *Parser, end_token: Token.Tag) Error!collections.NodeSlices(AST.Node.Idx).Idx {
    const scratch_marker = self.markScratchNodes();
    defer self.restoreScratchNodes(scratch_marker);

    while (self.peek() != end_token and self.peek() != .EndOfFile) {
        const item = try self.parseExposedItem();
        try self.scratch_nodes.append(self.gpa, item);

        if (self.peek() == .Comma) {
            self.advance();
        } else {
            break;
        }
    }

    const items = self.getScratchNodesSince(scratch_marker);
    if (items.len == 0) {
        return collections.NodeSlices(AST.Node.Idx).Idx.NIL;
    }
    const result = try self.ast.appendNodeSlice(self.gpa, items);
    return result;
}

fn parseExposedItem(self: *Parser) Error!Node.Idx {
    const start_position = self.currentPosition();

    switch (self.peek()) {
        .UpperIdent => {
            const ident = self.currentIdent();
            const region = self.currentRegion();
            self.advance();

            if (ident) |id| {
                return try self.ast.appendNode(self.gpa, region, .uc, .{ .ident = id });
            } else {
                return self.pushMalformed(.exposed_item_unexpected_token, start_position);
            }
        },
        .LowerIdent => {
            const ident = self.currentIdent();
            const region = self.currentRegion();
            self.advance();

            // Check for optional ! suffix (for effectful functions like main!)
            var final_region = region;
            var is_effectful = false;
            if (self.peek() == .OpBang) {
                is_effectful = true;
                final_region.end = self.currentRegion().end;
                self.advance(); // consume the !
            }

            if (ident) |id| {
                const node_tag: Node.Tag = if (is_effectful) .not_lc else .lc;
                return try self.ast.appendNode(self.gpa, final_region, node_tag, .{ .ident = id });
            } else {
                return self.pushMalformed(.exposed_item_unexpected_token, start_position);
            }
        },
        else => return self.pushMalformed(.exposed_item_unexpected_token, start_position),
    }
}

fn parseTypeVariableList(self: *Parser) Error!collections.NodeSlices(AST.Node.Idx).Idx {
    const scratch_marker = self.markScratchNodes();
    defer self.restoreScratchNodes(scratch_marker);

    while (self.peek() != .CloseCurly and self.peek() != .EndOfFile) {
        // Parse individual type variable (should be an uppercase identifier)

        if (self.peek() != .UpperIdent) {
            // If not an upper identifier, try to recover
            _ = try self.pushMalformed(.expr_unexpected_token, self.getCurrentErrorPos());
            break;
        }

        const ident = self.currentIdent();
        const region = self.currentRegion();
        self.advance();

        // Create an uppercase identifier node for the type variable
        if (ident) |id| {
            const type_var = try self.ast.appendNode(self.gpa, region, .uc, .{ .ident = id });
            try self.scratch_nodes.append(self.gpa, type_var);
        }

        if (self.peek() == .Comma) {
            self.advance();
        } else {
            break;
        }
    }

    const vars = self.getScratchNodesSince(scratch_marker);
    if (vars.len == 0) {
        return collections.NodeSlices(AST.Node.Idx).Idx.NIL;
    }
    return try self.ast.appendNodeSlice(self.gpa, vars);
}

/// Parse a top-level statement
fn parseTopLevelStatement(self: *Parser) Error!?Node.Idx {
    return self.parseStmt();
}

/// Parse a statement
/// Parse a statement or expression that might be a record field
/// When in_potential_record is true, we parse colons more carefully to avoid tuple issues
fn parseStmtOrRecordField(self: *Parser, in_potential_record: bool) Error!?Node.Idx {
    switch (self.peek()) {
        .EndOfFile => return null,
        .KwImport => return self.parseImport(),
        .KwExpect => return self.parseExpect(),
        else => {
            // Parse the left-hand side as an expression first
            // We need to stop at colons to avoid consuming them
            // Colons have bp=3, so we use min_bp=3 to stop at them
            const lhs = try self.parseExprWithPrecedence(3);

            // Check if this is followed by : or := or = (making it a type annotation/declaration/assignment)
            if (self.peek() == .OpColon or self.peek() == .OpColonEqual or self.peek() == .OpAssign) {
                const op_tag = self.peek();
                const is_nominal = op_tag == .OpColonEqual;
                const is_assignment = op_tag == .OpAssign;
                self.advance(); // consume : or := or =

                // Parse the right-hand side
                // If we're in a potential record, use higher precedence to stop at commas
                var rhs = if (in_potential_record)
                    try self.parseExprWithPrecedence(10) // Stop at commas (precedence 4)
                else
                    try self.parseExpr(); // Normal parsing

                // Check if this is an effectful type annotation with => or ->
                const arrow_token = self.peek();
                if (arrow_token == .OpArrow or arrow_token == .OpFatArrow) {
                    self.advance(); // consume the arrow

                    // Parse the return type
                    const return_type = if (in_potential_record)
                        try self.parseExprWithPrecedence(10) // Stop at commas
                    else
                        try self.parseExpr();

                    // Create arrow node for function type
                    const arrow_tag: AST.Node.Tag = if (arrow_token == .OpFatArrow) .binop_thick_arrow else .binop_thin_arrow;
                    const arrow_binop_idx = try self.ast.appendBinOp(self.gpa, rhs, return_type);
                    const arrow_region = makeRegion(self.ast.start(rhs), self.ast.getRegion(return_type).end);
                    rhs = try self.ast.appendNode(self.gpa, arrow_region, arrow_tag, .{ .binop = arrow_binop_idx });
                }

                // Create type annotation/declaration/assignment node
                const tag: AST.Node.Tag = if (is_nominal)
                    .binop_colon_equals
                else if (is_assignment)
                    .binop_equals
                else
                    .binop_colon;
                const binop_idx = try self.ast.appendBinOp(self.gpa, lhs, rhs);
                const full_region = makeRegion(self.ast.start(lhs), self.ast.getRegion(rhs).end);
                return try self.ast.appendNode(self.gpa, full_region, tag, .{ .binop = binop_idx });
            }

            // Otherwise, it's just a regular expression/statement
            return lhs;
        },
    }
}

pub fn parseStmt(self: *Parser) Error!?Node.Idx {
    switch (self.peek()) {
        .EndOfFile => return null,
        .KwImport => return self.parseImport(),
        .KwExpect => return self.parseExpect(),
        else => {
            // Parse the left-hand side as an expression first
            // We need to stop at colons to avoid consuming them
            // Colons have bp=3, so we use min_bp=3 to stop at them
            const lhs = try self.parseExprWithPrecedence(3);

            // Check if this is followed by : or := (making it a type annotation/declaration)
            if (self.peek() == .OpColon or self.peek() == .OpColonEqual) {
                const is_nominal = self.peek() == .OpColonEqual;
                const colon_region = self.currentRegion();
                self.advance(); // consume : or :=

                // Parse the right-hand side as a type expression
                // For type annotations, we need to handle arrows specially
                var rhs = try self.parseExprWithPrecedence(0);

                // Check for arrow operators (thin -> or thick =>) after the type
                // This handles effectful function types like {} => Result(...)
                const arrow_token = self.peek();
                if (arrow_token == .OpArrow or arrow_token == .OpFatArrow) {
                    self.advance(); // consume the arrow

                    // Parse the return type
                    const return_type = try self.parseExpr();

                    // Create the arrow node
                    const arrow_tag: AST.Node.Tag = if (arrow_token == .OpFatArrow) .binop_thick_arrow else .binop_thin_arrow;
                    const arrow_binop_idx = try self.ast.appendBinOp(self.gpa, rhs, return_type);
                    const full_arrow_region = makeRegion(self.ast.nodes.fieldItem(.region, @as(collections.SafeMultiList(Node).Idx, @enumFromInt(@intFromEnum(rhs)))).start, self.ast.nodes.fieldItem(.region, @as(collections.SafeMultiList(Node).Idx, @enumFromInt(@intFromEnum(return_type)))).end);
                    rhs = try self.ast.appendNode(self.gpa, full_arrow_region, arrow_tag, .{ .binop = arrow_binop_idx });
                }

                // Create type annotation/declaration node
                const tag: AST.Node.Tag = if (is_nominal) .binop_colon_equals else .binop_colon;
                const binop_idx = try self.ast.appendBinOp(self.gpa, lhs, rhs);
                return try self.ast.appendNode(self.gpa, colon_region, tag, .{ .binop = binop_idx });
            }

            // Otherwise, it's just a regular expression/statement
            return lhs;
        },
    }
}

fn parseExpect(self: *Parser) Error!?Node.Idx {
    const expect_region = self.currentRegion();
    self.advance(); // consume expect

    // Parse the condition expression
    const condition = try self.parseExpr();

    // Use scratch space for single node
    const scratch_marker = self.markScratchNodes();
    defer self.restoreScratchNodes(scratch_marker);
    try self.scratch_nodes.append(self.gpa, condition);

    const nodes_idx = try self.ast.appendNodeSlice(self.gpa, self.getScratchNodesSince(scratch_marker));

    // Create the expect node - region spans from expect keyword to end of condition
    const condition_region = self.ast.nodes.fieldItem(.region, @as(collections.SafeMultiList(Node).Idx, @enumFromInt(@intFromEnum(condition))));
    const full_region = makeRegion(expect_region.start, condition_region.end);
    return try self.ast.appendNode(self.gpa, full_region, .expect, .{ .nodes = nodes_idx });
}

fn parseImport(self: *Parser) Error!?Node.Idx {
    const start_pos = self.currentPosition();
    self.advance(); // consume import

    // Parse the module path first
    var module_path: Node.Idx = undefined;

    const token = self.peek();
    if (token == .String or token == .MultilineString or token == .SingleQuote) {
        // String import like "users.json"
        module_path = try self.parseStoredStringExpr();
    } else {
        // Parse module path (e.g., pf.Stdout)
        // Build the path using binop_pipe for qualified identifiers
        var path: ?Node.Idx = null;

        while (true) {
            const tok = self.peek();
            if (tok == .LowerIdent or tok == .UpperIdent) {
                const ident = self.currentIdent();
                const ident_region = self.currentRegion();
                if (ident) |id| {
                    // Use the correct node type based on identifier case
                    const node_tag: AST.Node.Tag = if (tok == .UpperIdent) .uc else .lc;
                    const node = try self.ast.appendNode(self.gpa, ident_region, node_tag, .{ .ident = id });

                    if (path) |existing_path| {
                        // Chain this identifier to the existing path using binop_pipe
                        const binop_idx = try self.ast.appendBinOp(self.gpa, existing_path, node);
                        const combined_region = makeRegion(self.ast.getRegion(existing_path).start, ident_region.end);
                        path = try self.ast.appendNode(self.gpa, combined_region, .binop_pipe, .{ .binop = binop_idx });
                    } else {
                        // First identifier in the path
                        path = node;
                    }
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

        if (path) |p| {
            module_path = p;
        } else {
            return null; // No path
        }
    }

    var result = module_path;

    // Parse optional "as alias" clause
    if (self.peek() == .KwAs) {
        self.advance(); // consume 'as'

        const alias_token = self.peek();
        if (alias_token == .LowerIdent or alias_token == .UpperIdent) {
            const alias_ident = self.currentIdent();
            const alias_region = self.currentRegion();
            if (alias_ident) |id| {
                const alias_tag: AST.Node.Tag = if (alias_token == .UpperIdent) .uc else .lc;
                var alias_node = try self.ast.appendNode(self.gpa, alias_region, alias_tag, .{ .ident = id });
                self.advance();

                // Check for optional type annotation (: Type)
                if (self.peek() == .OpColon) {
                    self.advance(); // consume :

                    // Parse the type expression
                    const type_expr = try self.parseExprWithPrecedence(10); // High precedence to stop at commas

                    // Create binop_colon node for "alias : Type"
                    const binop_idx = try self.ast.appendBinOp(self.gpa, alias_node, type_expr);
                    const full_region = makeRegion(alias_region.start, self.last_region.end);
                    alias_node = try self.ast.appendNode(self.gpa, full_region, .binop_colon, .{ .binop = binop_idx });
                }

                // Create binop_as node
                const as_binop_idx = try self.ast.appendBinOp(self.gpa, result, alias_node);
                const as_region = makeRegion(start_pos, self.last_region.end);
                result = try self.ast.appendNode(self.gpa, as_region, .binop_as, .{ .binop = as_binop_idx });
            }
        }
    }

    // Parse optional exposing clause
    if (self.peek() == .KwExposing) {
        self.advance();
        self.expect(.OpenSquare) catch {};

        const scratch_marker = self.markScratchNodes();
        defer self.restoreScratchNodes(scratch_marker);

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

        // Create a list node for the exposed items
        const exposed_nodes = self.getScratchNodesSince(scratch_marker);
        var exposing_list: Node.Idx = undefined;
        if (exposed_nodes.len > 0) {
            const nodes_idx = try self.ast.appendNodeSlice(self.gpa, exposed_nodes);
            const list_region = makeRegion(start_pos, self.last_region.end);
            // Use list_literal for the exposed items list
            exposing_list = try self.ast.appendNode(self.gpa, list_region, .list_literal, .{ .nodes = nodes_idx });
        } else {
            // Empty exposing list
            const list_region = makeRegion(start_pos, self.last_region.end);
            const empty_idx = collections.NodeSlices(Node.Idx).Idx.NIL;
            exposing_list = try self.ast.appendNode(self.gpa, list_region, .list_literal, .{ .nodes = empty_idx });
        }

        // Create binop_exposing node
        const exposing_binop_idx = try self.ast.appendBinOp(self.gpa, result, exposing_list);
        const exposing_region = makeRegion(start_pos, self.last_region.end);
        result = try self.ast.appendNode(self.gpa, exposing_region, .binop_exposing, .{ .binop = exposing_binop_idx });
    }

    // Wrap the result in an import node
    const import_region = makeRegion(start_pos, self.last_region.end);
    const scratch_marker = self.markScratchNodes();
    defer self.restoreScratchNodes(scratch_marker);
    try self.scratch_nodes.append(self.gpa, result);
    const nodes_idx = try self.ast.appendNodeSlice(self.gpa, self.getScratchNodesSince(scratch_marker));
    return try self.ast.appendNode(self.gpa, import_region, .import, .{ .import_nodes = nodes_idx });
}

/// Parse an expression
/// This function expects tokens to be already set up (current and lookahead)
/// Use parseExprFromSource for parsing from source text
pub fn parseExpr(self: *Parser) Error!Node.Idx {
    // If we don't have current token, we need to get one
    if (self.current == null) {
        // This means we're being called without tokens set up
        // This should not happen in normal flow - the caller should have set up tokens
        return try self.pushMalformed(.expr_unexpected_token, self.currentPosition());
    }

    // Parse the expression using precedence parser with minimum precedence 0
    return try self.parseExprWithPrecedence(0);
}

/// Parse an expression from source text (sets up tokenizer)
pub fn parseExprFromSource(self: *Parser, messages: []tokenize_iter.Diagnostic) Error!Node.Idx {
    // Create tokenizer using the shared environment
    var token_iter = try tokenize_iter.TokenIterator.init(self.env, self.gpa, self.source, messages, self.byte_slices);
    defer token_iter.deinit(self.gpa);

    // Get first two non-comment tokens to start
    var first: ?Token = null;
    while (true) {
        const token = try token_iter.next(self.gpa) orelse return @enumFromInt(0);
        switch (token.tag) {
            .LineComment, .DocComment, .BlankLine => continue,
            else => {
                first = token;
                break;
            },
        }
    }

    var second: ?Token = null;
    while (true) {
        const token = try token_iter.next(self.gpa);
        if (token == null) break;
        switch (token.?.tag) {
            .LineComment, .DocComment, .BlankLine => continue,
            else => {
                second = token;
                break;
            },
        }
    }

    self.current = first;
    self.lookahead = second;

    // Store the token iterator so advance() can fetch new tokens
    self.token_iterator = &token_iter;

    // Parse the expression
    const result = try self.parseExpr();

    // Check if we have unconsumed tokens (shouldn't happen for valid expressions)
    // Do this before clearing the token iterator
    if (self.peek() != .EndOfFile) {
        // Clear the token iterator reference
        self.token_iterator = null;
        // There are unconsumed tokens - this might indicate a parsing issue
        // Return an error instead of a partial parse
        const pos = self.currentPosition();
        return self.pushMalformed(.expr_unexpected_token, pos);
    }

    // Clear the token iterator reference
    self.token_iterator = null;

    return result;
}

fn parseNumLiteral(self: *Parser) Error!Node.Idx {
    const region = self.currentRegion();
    const tag = self.peek();
    const extra = self.currentExtra();

    self.advance();

    // Handle different number literal types based on token tag and extra data
    switch (tag) {
        .Int, .IntBase => {
            // Integer literals (base-10 or other bases)
            switch (extra) {
                .num_literal_i32 => |value| {
                    // Small integer that fits in i32
                    // Small integer that fits in i32
                    return try self.ast.appendNode(self.gpa, region, .num_literal_i32, .{ .num_literal_i32 = value });
                },
                .bytes_idx => |idx| {
                    // Big integer stored in ByteSlices
                    // For IntBase, it's stored as base-10 in ByteSlices
                    const ast_tag: Node.Tag = if (tag == .IntBase) .int_literal_big else .num_literal_big;
                    return try self.ast.appendNode(self.gpa, region, ast_tag, .{ .num_literal_big = idx });
                },
                else => {
                    // Shouldn't happen with well-formed tokens
                    return try self.ast.appendNode(self.gpa, region, .num_literal_i32, .{ .num_literal_i32 = 0 });
                },
            }
        },
        .Float => {
            // Floating point literals
            // Floating point literals
            switch (extra) {
                .frac_literal_small => |small_dec| {
                    // Small fraction that fits in SmallDec - convert from Token.SmallDec to AST.SmallDec
                    const ast_small_dec = AST.Node.SmallDec{
                        .numerator = small_dec.numerator,
                        .denominator_power_of_ten = small_dec.denominator_power_of_ten,
                    };
                    return try self.ast.appendNode(self.gpa, region, .frac_literal_small, .{ .frac_literal_small = ast_small_dec });
                },
                .bytes_idx => |idx| {
                    // Big fraction stored in ByteSlices
                    // Big fraction stored in ByteSlices
                    return try self.ast.appendNode(self.gpa, region, .frac_literal_big, .{ .frac_literal_big = idx });
                },
                else => {
                    // Shouldn't happen with well-formed tokens
                    return try self.ast.appendNode(self.gpa, region, .num_literal_i32, .{ .num_literal_i32 = 0 });
                },
            }
        },
        else => {
            // Not a number literal token
            return try self.ast.appendNode(self.gpa, region, .num_literal_i32, .{ .num_literal_i32 = 0 });
        },
    }
}

fn parseStoredStringExpr(self: *Parser) Error!Node.Idx {
    const region = self.currentRegion();
    const extra = self.currentExtra();
    const current_token = self.currentToken();
    const tag = if (current_token) |token| token.tag else .EndOfFile;

    self.advance();

    // Handle SingleQuote tokens specially - extract character from source
    if (tag == .SingleQuote) {
        // SingleQuote tokens are of the form 'x' where x is a single character
        // The region spans the entire token including quotes
        const source_slice = self.source[region.start.offset..region.end.offset];

        // Extract the character between the quotes
        // Format is 'x' where x could be an escaped character like '\n'
        var buf: [4]u8 = [_]u8{0} ** 4;

        if (source_slice.len >= 3 and source_slice[0] == '\'' and source_slice[source_slice.len - 1] == '\'') {
            const char_content = source_slice[1 .. source_slice.len - 1];

            if (char_content.len == 1) {
                // Simple character like 'a'
                buf[0] = char_content[0];
            } else if (char_content.len == 2 and char_content[0] == '\\') {
                // Escaped character like '\n'
                buf[0] = switch (char_content[1]) {
                    'n' => '\n',
                    't' => '\t',
                    'r' => '\r',
                    '\\' => '\\',
                    '\'' => '\'',
                    '"' => '"',
                    else => char_content[1], // Unknown escape, use literal
                };
            }
            // Else it's malformed, leave as empty
        }

        return try self.ast.appendNode(self.gpa, region, .str_literal_small, .{ .str_literal_small = buf });
    }

    // The string content is stored in ByteSlices with escapes already resolved
    switch (extra) {
        .bytes_idx => |idx| {
            // Get the string from ByteSlices
            // Check if ByteSlices is empty (can happen with certain parsing scenarios)
            if (self.byte_slices.entries.items.items.len == 0) {
                // ByteSlices is empty - use empty string as fallback
                return try self.ast.appendNode(self.gpa, region, .str_literal_small, .{ .str_literal_small = [_]u8{0} ** 4 });
            }
            const str_content = self.byte_slices.slice(idx);

            // Check if it fits in small string
            if (str_content.len <= 4 and std.mem.indexOfAny(u8, str_content, "\x00") == null) {
                // Small string that fits inline
                var buf: [4]u8 = [_]u8{0} ** 4;
                @memcpy(buf[0..str_content.len], str_content);
                return try self.ast.appendNode(self.gpa, region, .str_literal_small, .{ .str_literal_small = buf });
            } else {
                // Store in AST's ByteSlices
                const ast_idx = try self.ast.byte_slices.append(self.gpa, str_content);
                return try self.ast.appendNode(self.gpa, region, .str_literal_big, .{ .str_literal_big = ast_idx });
            }
        },
        else => {
            // Shouldn't happen with well-formed tokens
            const empty_buf: [4]u8 = [_]u8{0} ** 4;
            return try self.ast.appendNode(self.gpa, region, .str_literal_small, .{ .str_literal_small = empty_buf });
        },
    }
}

fn parseStringExpr(self: *Parser) Error!Node.Idx {
    const start_pos = self.currentPosition();
    const start_region = self.currentRegion();

    self.expect(.StringStart) catch {
        return self.pushMalformed(.expr_unexpected_token, start_pos);
    };

    const scratch_marker = self.markScratchNodes();
    defer self.restoreScratchNodes(scratch_marker);

    // Collect all string bytes using scratch buffer
    const scratch_bytes_start = self.scratch_bytes.items.len;
    defer {
        self.scratch_bytes.items.len = scratch_bytes_start;
    }

    // Parse string parts
    while (self.peek() != .StringEnd and self.peek() != .EndOfFile) {
        switch (self.peek()) {
            .StringPart => {
                // Get the string content from the token's ByteSlices
                const extra = self.currentExtra();
                switch (extra) {
                    .bytes_idx => |idx| {
                        // Check if ByteSlices is empty (can happen with certain parsing scenarios)
                        if (self.byte_slices.entries.items.items.len == 0) {
                            // ByteSlices is empty - skip this part
                        } else {
                            const str_content = self.byte_slices.slice(idx);
                            try self.scratch_bytes.appendSlice(self.gpa, str_content);
                        }
                    },
                    else => {
                        // Shouldn't happen - StringPart should always have bytes_idx
                        try self.scratch_bytes.appendSlice(self.gpa, "");
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

    const end_region = self.currentRegion();
    self.expect(.StringEnd) catch {
        return self.pushMalformed(.string_unclosed, start_pos);
    };
    const string_region = makeRegion(start_region.start, end_region.end);

    // Store the actual string content
    const string_bytes = self.scratch_bytes.items[scratch_bytes_start..];
    const total_bytes = string_bytes.len;

    // First check length - most strings are longer than 4 bytes
    if (total_bytes <= 4) {
        // Only check byte eligibility for short strings
        // All bytes must be ASCII excluding null (1-127)
        var eligible = true;
        for (string_bytes) |byte| {
            if (byte == 0 or byte >= 128) {
                eligible = false;
                break;
            }
        }

        if (eligible) {
            var small_bytes: [4]u8 = .{0} ** 4;
            // Copy the actual string bytes (up to 4 bytes)
            @memcpy(small_bytes[0..total_bytes], string_bytes);
            // If last byte is non-zero, we know it's 4 bytes long
            // Otherwise, count backwards to find the length
            return try self.ast.appendNode(self.gpa, string_region, .str_literal_small, .{ .str_literal_small = small_bytes });
        }
    }

    // For longer strings or those with special characters, store in ByteSlices
    const bytes_idx = try self.ast.appendByteSlice(self.gpa, string_bytes);
    return try self.ast.appendNode(self.gpa, string_region, .str_literal_big, .{ .str_literal_big = bytes_idx });
}

fn parseListLiteral(self: *Parser) Error!Node.Idx {
    const start_pos = self.currentPosition();
    const open_bracket_region = self.currentRegion(); // Region of [
    self.advance(); // consume [

    if (self.peek() == .CloseSquare) {
        const close_bracket_region = self.currentRegion(); // Region of ]
        self.advance();
        const empty_slice: []const Node.Idx = &.{};
        const nodes_idx = try self.ast.appendNodeSlice(self.gpa, empty_slice);
        const full_region = makeRegion(open_bracket_region.start, close_bracket_region.end);
        return try self.ast.appendNode(self.gpa, full_region, .list_literal, .{ .nodes = nodes_idx });
    }

    const scratch_marker = self.markScratchNodes();
    defer self.restoreScratchNodes(scratch_marker);

    // Parse list elements at a higher precedence than comma (4) to prevent
    // them from being parsed as tuples. We use precedence 10, same as
    // COMMA_ITEM_PRECEDENCE in parseExprWithPrecedence.
    const LIST_ELEM_PRECEDENCE = 10;

    while (self.peek() != .CloseSquare and self.peek() != .EndOfFile) {
        const elem = try self.parseExprWithPrecedence(LIST_ELEM_PRECEDENCE);
        try self.scratch_nodes.append(self.gpa, elem);

        if (self.peek() == .Comma) {
            self.advance();
        } else {
            break;
        }
    }

    var close_bracket_region: Region = undefined;
    if (self.peek() == .CloseSquare) {
        close_bracket_region = self.currentRegion();
        self.advance();
    } else {
        try self.pushDiagnostic(.expected_expr_close_square_or_comma, start_pos, self.currentPosition());
        // Use current position for missing close bracket
        close_bracket_region = makeRegion(self.currentPosition(), self.currentPosition());
    }

    const elems = self.getScratchNodesSince(scratch_marker);
    const nodes_idx = if (elems.len > 0)
        try self.ast.appendNodeSlice(self.gpa, elems)
    else blk: {
        const empty_slice: []const Node.Idx = &.{};
        break :blk try self.ast.appendNodeSlice(self.gpa, empty_slice);
    };
    const full_region = makeRegion(open_bracket_region.start, close_bracket_region.end);
    return try self.ast.appendNode(self.gpa, full_region, .list_literal, .{ .nodes = nodes_idx });
}

fn parseBlockOrRecord(self: *Parser) Error!Node.Idx {
    const start_pos = self.currentPosition();
    const open_brace_region = self.currentRegion(); // Region of {
    self.advance(); // consume {

    if (self.peek() == .CloseCurly) {
        const close_brace_region = self.currentRegion(); // Region of }
        self.advance();
        const empty_slice: []const Node.Idx = &.{};
        const nodes_idx = try self.ast.appendNodeSlice(self.gpa, empty_slice);
        const full_region = makeRegion(open_brace_region.start, close_brace_region.end);
        // Empty {} defaults to empty record
        return try self.ast.appendNode(self.gpa, full_region, .record_literal, .{ .nodes = nodes_idx });
    }

    const scratch_marker = self.markScratchNodes();
    defer self.restoreScratchNodes(scratch_marker);

    // Start by assuming it's a block
    var is_record = false;

    // Parse elements until we hit } or EOF
    while (self.peek() != .CloseCurly and self.peek() != .EndOfFile) {
        // Special handling for rest syntax in records
        if (self.peek() == .DoubleDot) {
            // Seeing .. means this is definitely a record
            is_record = true;
            const dot_pos = self.currentPosition();
            self.advance(); // consume ..

            // Check if there's an identifier after .. (like ..rest)
            if (self.peek() == .LowerIdent) {
                const ident = self.currentIdent();
                self.advance();

                // Create a double_dot_lc node for ..identifier
                if (ident) |id| {
                    const dot_region = makeRegion(dot_pos, self.getLastConsumedRegion().end);
                    const rest_node = try self.ast.appendNode(self.gpa, dot_region, .double_dot_lc, .{ .ident = id });
                    try self.scratch_nodes.append(self.gpa, rest_node);
                }
            } else {
                // Just .. without an identifier - create an underscore as placeholder
                const dot_region = makeRegion(dot_pos, dot_pos);
                const rest_node = try self.ast.appendNode(self.gpa, dot_region, .underscore, .{ .src_bytes_end = dot_pos });
                try self.scratch_nodes.append(self.gpa, rest_node);
            }

            // Check if there's a comma after the rest (optional, but should be last)
            if (self.peek() == .Comma) {
                self.advance();
            }
            break; // rest should be the last element
        } else {
            // Parse an element (could be expr or statement)
            const elem_opt = if (is_record) blk: {
                // If we already know it's a record, parse a record field properly
                // We need to handle name: value as a unit, stopping only at commas
                // First parse the field name/expression with precedence to stop at colon
                const lhs = try self.parseExprWithPrecedence(3); // Stop at colon (precedence 3)

                // Check if we have a colon for a field definition
                if (self.peek() == .OpColon) {
                    self.advance(); // consume :

                    // Parse the value with higher precedence to stop at comma
                    const FIELD_VALUE_PRECEDENCE = 10; // Higher than comma (4)
                    const rhs = try self.parseExprWithPrecedence(FIELD_VALUE_PRECEDENCE);

                    // Create the field node
                    const binop_idx = try self.ast.appendBinOp(self.gpa, lhs, rhs);
                    const colon_region = makeRegion(self.ast.start(lhs), self.ast.getRegion(rhs).end);
                    break :blk try self.ast.appendNode(self.gpa, colon_region, .binop_colon, .{ .binop = binop_idx });
                } else {
                    // No colon, just a bare identifier or expression
                    break :blk lhs;
                }
            } else blk: {
                // Parse as statement but be careful with colons in case this is a record
                // We pass true to indicate we're in a potential record context
                const before_pos = self.currentPosition();
                const stmt = try self.parseStmtOrRecordField(true);
                const after_pos = self.currentPosition();

                // Check if we actually advanced - if not, we're stuck
                if (before_pos.offset == after_pos.offset and stmt != null) {
                    // We got a statement but didn't advance - this shouldn't happen
                    // Break to avoid infinite loop
                    break :blk stmt;
                }
                break :blk stmt;
            };

            if (elem_opt) |elem| {
                // In lambda args, { a, b } is shorthand for { a: a, b: b }
                // If we're in lambda args and have a bare identifier, convert it
                const final_elem = if (self.is_in_lambda_args and self.ast.tag(elem) == .lc) blk: {
                    // Convert bare identifier to field: identifier
                    const elem_region = self.ast.getRegion(elem);
                    const binop_idx = try self.ast.appendBinOp(self.gpa, elem, elem);
                    break :blk try self.ast.appendNode(self.gpa, elem_region, .binop_colon, .{ .binop = binop_idx });
                } else elem;

                try self.scratch_nodes.append(self.gpa, final_elem);

                // Check for comma (records) or continue parsing (blocks)
                if (self.peek() == .Comma) {
                    is_record = true;
                    self.advance();
                } else if (!is_record) {
                    // In block mode, keep parsing statements
                    // They're separated by newlines in the source
                    continue;
                } else {
                    // In record mode, stop at non-comma
                    break;
                }
            } else {
                break;
            }
        }
    }

    const close_region = self.currentRegion();
    self.expect(.CloseCurly) catch {
        try self.pushDiagnostic(.expected_expr_close_curly, start_pos, self.currentPosition());
    };

    const nodes = self.getScratchNodesSince(scratch_marker);
    const nodes_idx = try self.ast.appendNodeSlice(self.gpa, nodes);

    const tag: Node.Tag = if (is_record) .record_literal else .block;
    const full_region = makeRegion(open_brace_region.start, close_region.end);
    return try self.ast.appendNode(self.gpa, full_region, tag, .{ .nodes = nodes_idx });
}

fn parseTupleOrParenthesized(self: *Parser) Error!Node.Idx {
    const open_paren_region = self.currentRegion();
    self.advance(); // consume (

    const scratch_marker = self.markScratchNodes();
    defer self.restoreScratchNodes(scratch_marker);

    // Check for empty tuple
    if (self.peek() == .CloseRound) {
        const close_region = self.currentRegion();
        self.advance();
        const empty_slice: []const Node.Idx = &.{};
        const nodes_idx = try self.ast.appendNodeSlice(self.gpa, empty_slice);
        const full_region = makeRegion(open_paren_region.start, close_region.end);
        return try self.ast.appendNode(self.gpa, full_region, .tuple_literal, .{ .nodes = nodes_idx });
    }

    // Parse the first expression
    const first = self.parseExpr() catch |err| {
        // If parsing fails, propagate the error with proper diagnostics
        try self.diagnostics.append(self.gpa, .{
            .tag = .expr_unexpected_token,
            .region = .{ .start = self.currentPosition(), .end = self.currentPosition() },
        });
        return err;
    };
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

        const close_region = self.currentRegion();
        self.expect(.CloseRound) catch {
            // Always use current position for error
            const error_pos = self.currentPosition();
            try self.pushDiagnostic(.expected_expr_close_round_or_comma, error_pos, error_pos);
        };

        const elems = self.getScratchNodesSince(scratch_marker);

        // Safety check: tuples should have at least 2 elements
        if (elems.len == 0) {
            const empty_slice: []const Node.Idx = &.{};
            const nodes_idx = try self.ast.appendNodeSlice(self.gpa, empty_slice);
            const full_region = makeRegion(open_paren_region.start, close_region.end);
            return try self.ast.appendNode(self.gpa, full_region, .tuple_literal, .{ .nodes = nodes_idx });
        }

        const elems_idx = try self.ast.appendNodeSlice(self.gpa, elems);
        const full_region = makeRegion(open_paren_region.start, close_region.end);
        return try self.ast.appendNode(self.gpa, full_region, .tuple_literal, .{ .nodes = elems_idx });
    } else {
        self.expect(.CloseRound) catch {
            // Always use current position for error
            const error_pos = self.currentPosition();
            try self.pushDiagnostic(.expected_expr_close_round_or_comma, error_pos, error_pos);
        };

        // Just a parenthesized expression
        return first;
    }
}

fn parseApply(self: *Parser, func: Node.Idx) Error!Node.Idx {
    const start_pos = self.ast.start(func);
    const func_region = self.ast.getRegion(func);
    self.advance(); // consume (

    const scratch_marker = self.markScratchNodes();
    defer self.restoreScratchNodes(scratch_marker);

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

    const close_region = self.currentRegion();
    self.expect(.CloseRound) catch {
        try self.pushDiagnostic(.expected_expr_apply_close_round, start_pos, self.currentPosition());
    };

    const nodes = self.getScratchNodesSince(scratch_marker);

    // Determine the tag based on the function node
    const tag: Node.Tag = switch (self.ast.tag(func)) {
        .uc, .uc_dot_ucs => .apply_uc,
        .lc, .lc_dot_ucs, .var_lc => .apply_lc,
        else => .apply_anon,
    };

    const nodes_idx = try self.ast.appendNodeSlice(self.gpa, nodes);
    const full_region = makeRegion(func_region.start, close_region.end);
    return try self.ast.appendNode(self.gpa, full_region, tag, .{ .nodes = nodes_idx });
}

fn parseExprStoppingAtElse(self: *Parser) Error!Node.Idx {
    // Parse an expression but stop if we see 'else' keyword
    // This is like parseExpr but with a custom termination condition

    // Start with a primary expression
    var expr = try self.parsePrimaryExpr();

    // Keep parsing operators while we can
    while (true) {
        // Check for 'else' keyword - stop if we see it
        if (self.peek() == .KwElse) {
            break;
        }

        // Check for operators
        const bp = getBindingPower(self.peek());
        if (bp.left == 0) {
            // No operator or not an operator token
            break;
        }

        // We have an operator - parse it as a binary operation
        const op_tag = self.peek();
        const op_region = self.currentRegion();
        self.advance();

        // Parse the right-hand side with appropriate precedence
        const rhs = if (self.peek() == .KwElse)
            // If the next token is 'else', we have a missing RHS
            try self.pushMalformed(.expr_unexpected_token, op_region.end)
        else
            try self.parseExprWithPrecedence(bp.right);

        // Create the binary operation node
        const binop_tag = tokenToBinOpTag(op_tag) orelse .binop_pipe;
        const binop_idx = try self.ast.appendBinOp(self.gpa, expr, rhs);
        const full_region = makeRegion(self.ast.start(expr), self.ast.getRegion(rhs).end);
        expr = try self.ast.appendNode(self.gpa, full_region, binop_tag, .{ .binop = binop_idx });
    }

    return expr;
}

fn parsePrimaryExpr(self: *Parser) Error!Node.Idx {
    // Parse a single primary expression (no binary operators)
    switch (self.peek()) {
        .LowerIdent => {
            const ident = self.currentIdent();
            const region = self.currentRegion();
            self.advance();

            if (ident) |id| {
                return try self.ast.appendNode(self.gpa, region, .lc, .{ .ident = id });
            } else {
                return try self.pushMalformed(.expr_unexpected_token, region.start);
            }
        },
        .UpperIdent => {
            const ident = self.currentIdent();
            const region = self.currentRegion();
            self.advance();
            if (ident) |id| {
                return try self.ast.appendNode(self.gpa, region, .uc, .{ .ident = id });
            } else {
                return try self.pushMalformed(.expr_unexpected_token, region.start);
            }
        },
        .Int, .Float, .IntBase => return self.parseNumLiteral(),
        .String, .MultilineString, .SingleQuote => return self.parseStoredStringExpr(),
        .OpenSquare => return self.parseListLiteral(),
        .OpenCurly => return self.parseBlockOrRecord(),
        .OpenRound => return self.parseTupleOrParenthesized(),
        else => {
            const pos = self.currentPosition();
            return self.pushMalformed(.expr_unexpected_token, pos);
        },
    }
}

fn parseIf(self: *Parser) Error!Node.Idx {
    const if_region = self.currentRegion();
    self.advance(); // consume if

    const scratch_marker = self.markScratchNodes();
    defer self.restoreScratchNodes(scratch_marker);

    // Parse condition
    // In Roc: if condition then_expr else else_expr
    // Parse the condition as a full expression
    // This will naturally stop at low-precedence boundaries
    const cond = try self.parseExpr();
    try self.scratch_nodes.append(self.gpa, cond);

    // Parse the then branch - parse until we hit 'else' or EOF
    // We need to be careful here to not consume the else keyword
    const then_start_pos = self.currentPosition();
    var then_branch: Node.Idx = undefined;

    // Check if next token is 'else' - if so, we have an empty then branch (error)
    if (self.peek() == .KwElse) {
        then_branch = try self.pushMalformed(.expr_unexpected_token, then_start_pos);
    } else {
        // Parse expression with custom stopping condition for 'else'
        // We can't use precedence alone since 'else' has no precedence
        // So we parse a primary expression, then check for operators
        then_branch = try self.parseExprStoppingAtElse();
    }
    try self.scratch_nodes.append(self.gpa, then_branch);

    // Check for else
    if (self.peek() == .KwElse) {
        self.advance();

        const else_branch = try self.parseExpr();
        try self.scratch_nodes.append(self.gpa, else_branch);

        const nodes = self.getScratchNodesSince(scratch_marker);
        const nodes_idx = try self.ast.appendNodeSlice(self.gpa, nodes);
        const else_end = self.ast.getRegion(else_branch).end;
        const full_region = makeRegion(if_region.start, else_end);
        return try self.ast.appendNode(self.gpa, full_region, .if_else, .{ .if_branches = @as(u32, @bitCast(@intFromEnum(nodes_idx))) });
    } else {
        const nodes = self.getScratchNodesSince(scratch_marker);
        const nodes_idx = try self.ast.appendNodeSlice(self.gpa, nodes);
        const then_end = self.ast.getRegion(then_branch).end;
        const full_region = makeRegion(if_region.start, then_end);
        return try self.ast.appendNode(self.gpa, full_region, .if_without_else, .{ .if_branches = @as(u32, @bitCast(@intFromEnum(nodes_idx))) });
    }
}

/// Parse a pattern in match expressions, function parameters, etc.
fn parsePattern(self: *Parser) Error!Node.Idx {
    return self.parsePatternWithGuard();
}

/// Parse a pattern that may have a guard (pattern if condition)
fn parsePatternWithGuard(self: *Parser) Error!Node.Idx {
    const pattern = try self.parsePatternAlternatives();

    // Check for guard: "if condition"
    if (self.peek() == .KwIf) {
        self.advance(); // consume if

        // Parse guard condition
        const condition = try self.parseExprWithPrecedence(1); // Stop at commas/arrows

        // Create an if_without_else node to represent the guard
        const scratch_marker = self.markScratchNodes();
        defer self.restoreScratchNodes(scratch_marker);

        try self.scratch_nodes.append(self.gpa, pattern);
        try self.scratch_nodes.append(self.gpa, condition);

        const nodes = self.getScratchNodesSince(scratch_marker);
        const nodes_idx = try self.ast.appendNodeSlice(self.gpa, nodes);
        const guard_region = makeRegion(self.ast.start(pattern), self.ast.getRegion(condition).end);

        return try self.ast.appendNode(self.gpa, guard_region, .if_without_else, .{ .if_branches = @as(u32, @bitCast(@intFromEnum(nodes_idx))) });
    }

    return pattern;
}

/// Parse pattern alternatives (Pattern1 | Pattern2 | ...)
fn parsePatternAlternatives(self: *Parser) Error!Node.Idx {
    var left = try self.parsePatternAs();

    // Check for | to parse alternatives
    while (self.peek() == .OpBar) {
        self.advance(); // consume |

        const right = try self.parsePatternAs();

        // Create binop_or node for pattern alternatives
        const binop_idx = try self.ast.appendBinOp(self.gpa, left, right);
        const alt_region = makeRegion(self.ast.start(left), self.ast.getRegion(right).end);
        left = try self.ast.appendNode(self.gpa, alt_region, .binop_or, .{ .binop = binop_idx });
    }

    return left;
}

/// Parse as-patterns (pattern as name)
fn parsePatternAs(self: *Parser) Error!Node.Idx {
    const pattern = try self.parsePatternPrimary();

    // Check for "as" keyword
    if (self.peek() == .KwAs) {
        self.advance(); // consume as

        // Expect an identifier after as
        if (self.peek() != .LowerIdent) {
            return self.pushMalformed(.expected_identifier_after_as, self.getCurrentErrorPos());
        }

        const ident = self.currentIdent();
        const ident_region = self.currentRegion();
        self.advance();

        if (ident) |id| {
            const name_node = try self.ast.appendNode(self.gpa, ident_region, .lc, .{ .ident = id });
            const binop_idx = try self.ast.appendBinOp(self.gpa, pattern, name_node);
            const as_region = makeRegion(self.ast.start(pattern), ident_region.end);
            return try self.ast.appendNode(self.gpa, as_region, .binop_as, .{ .binop = binop_idx });
        }
    }

    return pattern;
}

/// Parse primary patterns
fn parsePatternPrimary(self: *Parser) Error!Node.Idx {
    switch (self.peek()) {
        .Underscore => {
            const region = self.currentRegion();
            self.advance();
            return try self.ast.appendNode(self.gpa, region, .underscore, .{ .src_bytes_end = region.start });
        },

        .LowerIdent => {
            const ident = self.currentIdent();
            const region = self.currentRegion();
            self.advance();

            if (ident) |id| {
                return try self.ast.appendNode(self.gpa, region, .lc, .{ .ident = id });
            } else {
                return self.pushMalformed(.expr_unexpected_token, region.start);
            }
        },

        .UpperIdent => {
            // Constructor pattern - might have arguments
            const ident = self.currentIdent();
            const ctor_region = self.currentRegion();
            self.advance();

            if (ident) |id| {
                const ctor_node = try self.ast.appendNode(self.gpa, ctor_region, .uc, .{ .ident = id });

                // Check for constructor arguments
                if (self.peek() == .OpenRound) {
                    self.advance(); // consume (

                    const scratch_marker = self.markScratchNodes();
                    defer self.restoreScratchNodes(scratch_marker);

                    // Add constructor as first element
                    try self.scratch_nodes.append(self.gpa, ctor_node);

                    // Parse constructor arguments
                    if (self.peek() != .CloseRound) {
                        while (true) {
                            const arg = try self.parsePattern();
                            try self.scratch_nodes.append(self.gpa, arg);

                            if (self.peek() == .Comma) {
                                self.advance();
                            } else {
                                break;
                            }
                        }
                    }

                    self.expect(.CloseRound) catch {
                        _ = try self.pushMalformed(.expected_close_round, self.getCurrentErrorPos());
                    };

                    const nodes = self.getScratchNodesSince(scratch_marker);
                    const nodes_idx = try self.ast.appendNodeSlice(self.gpa, nodes);
                    const full_region = makeRegion(ctor_region.start, self.getLastConsumedRegion().end);

                    return try self.ast.appendNode(self.gpa, full_region, .apply_uc, .{ .nodes = nodes_idx });
                }

                return ctor_node;
            } else {
                return self.pushMalformed(.expr_unexpected_token, ctor_region.start);
            }
        },

        .OpenSquare => {
            // List pattern
            return self.parseListPattern();
        },

        .OpenCurly => {
            // Record pattern
            return self.parseRecordPattern();
        },

        .OpenRound => {
            // Tuple pattern or parenthesized pattern
            const open_region = self.currentRegion();
            self.advance(); // consume (

            const scratch_marker = self.markScratchNodes();
            defer self.restoreScratchNodes(scratch_marker);

            if (self.peek() == .CloseRound) {
                // Empty tuple
                self.advance();
                const empty_slice: []const Node.Idx = &.{};
                const nodes_idx = try self.ast.appendNodeSlice(self.gpa, empty_slice);
                const tuple_region = makeRegion(open_region.start, self.getLastConsumedRegion().end);
                return try self.ast.appendNode(self.gpa, tuple_region, .tuple_literal, .{ .nodes = nodes_idx });
            }

            // Parse first element
            const first = try self.parsePattern();

            if (self.peek() == .Comma) {
                // It's a tuple
                try self.scratch_nodes.append(self.gpa, first);

                while (self.peek() == .Comma) {
                    self.advance(); // consume comma

                    if (self.peek() == .CloseRound) {
                        // Trailing comma
                        break;
                    }

                    const elem = try self.parsePattern();
                    try self.scratch_nodes.append(self.gpa, elem);
                }

                self.expect(.CloseRound) catch {
                    _ = try self.pushMalformed(.expected_close_round, self.getCurrentErrorPos());
                };

                const elems = self.getScratchNodesSince(scratch_marker);
                const nodes_idx = try self.ast.appendNodeSlice(self.gpa, elems);
                const tuple_region = makeRegion(open_region.start, self.getLastConsumedRegion().end);

                return try self.ast.appendNode(self.gpa, tuple_region, .tuple_literal, .{ .nodes = nodes_idx });
            } else {
                // Just parenthesized pattern
                self.expect(.CloseRound) catch {
                    _ = try self.pushMalformed(.expected_close_round, self.getCurrentErrorPos());
                };

                return first;
            }
        },

        // Literal patterns
        .Int, .IntBase => {
            const token = self.currentToken() orelse return self.pushMalformed(.expr_unexpected_token, self.getCurrentErrorPos());
            const extra = self.currentExtra();
            const region = self.currentRegion();
            self.advance();

            // Handle different integer literal formats
            switch (extra) {
                .num_literal_i32 => |value| {
                    return try self.ast.appendNode(self.gpa, region, .num_literal_i32, .{ .num_literal_i32 = value });
                },
                .bytes_idx => |idx| {
                    const ast_tag: Node.Tag = if (token.tag == .IntBase) .int_literal_big else .num_literal_big;
                    return try self.ast.appendNode(self.gpa, region, ast_tag, .{ .num_literal_big = idx });
                },
                else => {
                    return try self.ast.appendNode(self.gpa, region, .num_literal_i32, .{ .num_literal_i32 = 0 });
                },
            }
        },
        .Float => {
            const extra = self.currentExtra();
            const region = self.currentRegion();
            self.advance();

            switch (extra) {
                .frac_literal_small => |frac| {
                    return try self.ast.appendNode(self.gpa, region, .frac_literal_small, .{ .frac_literal_small = .{
                        .numerator = frac.numerator,
                        .denominator_power_of_ten = frac.denominator_power_of_ten,
                    } });
                },
                .bytes_idx => |idx| {
                    return try self.ast.appendNode(self.gpa, region, .frac_literal_big, .{ .frac_literal_big = idx });
                },
                else => {
                    return try self.ast.appendNode(self.gpa, region, .frac_literal_small, .{ .frac_literal_small = .{ .numerator = 0, .denominator_power_of_ten = 0 } });
                },
            }
        },
        .String => {
            return self.parseStoredStringExpr();
        },

        else => {
            return self.pushMalformed(.pattern_unexpected_token, self.getCurrentErrorPos());
        },
    }
}

/// Parse list patterns including rest syntax
fn parseListPattern(self: *Parser) Error!Node.Idx {
    const open_region = self.currentRegion();
    self.advance(); // consume [

    const scratch_marker = self.markScratchNodes();
    defer self.restoreScratchNodes(scratch_marker);

    while (self.peek() != .CloseSquare and self.peek() != .EndOfFile) {
        // Check for rest pattern ..rest or ..
        if (self.peek() == .DoubleDot) {
            const dot_pos = self.currentPosition();
            const dot_region = self.currentRegion();
            self.advance(); // consume ..

            // Check if there's an identifier after ..
            if (self.peek() == .LowerIdent) {
                const ident = self.currentIdent();
                const ident_region = self.currentRegion();
                self.advance();

                if (ident) |id| {
                    const rest_region = makeRegion(dot_pos, ident_region.end);
                    const rest_node = try self.ast.appendNode(self.gpa, rest_region, .double_dot_lc, .{ .ident = id });
                    try self.scratch_nodes.append(self.gpa, rest_node);
                }
            } else {
                // Just .. without identifier
                const rest_node = try self.ast.appendNode(self.gpa, dot_region, .unary_double_dot, .{ .src_bytes_end = dot_region.end });
                try self.scratch_nodes.append(self.gpa, rest_node);
            }
        } else {
            // Regular pattern element
            const elem = try self.parsePattern();
            try self.scratch_nodes.append(self.gpa, elem);
        }

        if (self.peek() == .Comma) {
            self.advance();
        } else {
            break;
        }
    }

    const close_region = self.currentRegion();
    self.expect(.CloseSquare) catch {
        _ = try self.pushMalformed(.expected_expr_close_square_or_comma, self.getCurrentErrorPos());
    };

    const elems = self.getScratchNodesSince(scratch_marker);
    const nodes_idx = if (elems.len > 0)
        try self.ast.appendNodeSlice(self.gpa, elems)
    else blk: {
        const empty_slice: []const Node.Idx = &.{};
        break :blk try self.ast.appendNodeSlice(self.gpa, empty_slice);
    };

    const list_region = makeRegion(open_region.start, close_region.end);
    return try self.ast.appendNode(self.gpa, list_region, .list_literal, .{ .nodes = nodes_idx });
}

/// Parse record patterns including destructuring
fn parseRecordPattern(self: *Parser) Error!Node.Idx {
    const open_region = self.currentRegion();
    self.advance(); // consume {

    const scratch_marker = self.markScratchNodes();
    defer self.restoreScratchNodes(scratch_marker);

    while (self.peek() != .CloseCurly and self.peek() != .EndOfFile) {
        // Check for rest syntax ..
        if (self.peek() == .DoubleDot) {
            const dot_region = self.currentRegion();
            self.advance(); // consume ..

            // Check if there's an identifier after ..
            if (self.peek() == .LowerIdent) {
                const ident = self.currentIdent();
                const ident_region = self.currentRegion();
                self.advance();

                if (ident) |id| {
                    const rest_region = makeRegion(dot_region.start, ident_region.end);
                    const rest_node = try self.ast.appendNode(self.gpa, rest_region, .double_dot_lc, .{ .ident = id });
                    try self.scratch_nodes.append(self.gpa, rest_node);
                }
            } else {
                // Just .. without identifier
                const rest_node = try self.ast.appendNode(self.gpa, dot_region, .unary_double_dot, .{ .src_bytes_end = dot_region.end });
                try self.scratch_nodes.append(self.gpa, rest_node);
            }
        } else if (self.peek() == .LowerIdent) {
            const field_start = self.currentPosition();
            const ident = self.currentIdent();
            const ident_region = self.currentRegion();
            self.advance();

            if (ident) |id| {
                const field_node = try self.ast.appendNode(self.gpa, ident_region, .lc, .{ .ident = id });

                // Check for : pattern (destructuring)
                if (self.peek() == .OpColon) {
                    self.advance(); // consume :

                    const pattern = try self.parsePattern();
                    const binop_idx = try self.ast.appendBinOp(self.gpa, field_node, pattern);
                    const field_region = makeRegion(field_start, self.ast.getRegion(pattern).end);
                    const field_pattern = try self.ast.appendNode(self.gpa, field_region, .binop_colon, .{ .binop = binop_idx });
                    try self.scratch_nodes.append(self.gpa, field_pattern);
                } else {
                    // Just field name (shorthand)
                    try self.scratch_nodes.append(self.gpa, field_node);
                }
            }
        } else {
            // Unexpected token in record pattern
            _ = try self.pushMalformed(.pattern_unexpected_token, self.getCurrentErrorPos());
            break;
        }

        if (self.peek() == .Comma) {
            self.advance();
        } else {
            break;
        }
    }

    const close_region = self.currentRegion();
    self.expect(.CloseCurly) catch {
        _ = try self.pushMalformed(.expected_expr_close_curly, self.getCurrentErrorPos());
    };

    const fields = self.getScratchNodesSince(scratch_marker);
    const nodes_idx = if (fields.len > 0)
        try self.ast.appendNodeSlice(self.gpa, fields)
    else blk: {
        const empty_slice: []const Node.Idx = &.{};
        break :blk try self.ast.appendNodeSlice(self.gpa, empty_slice);
    };

    const record_region = makeRegion(open_region.start, close_region.end);
    return try self.ast.appendNode(self.gpa, record_region, .record_literal, .{ .nodes = nodes_idx });
}

fn parseMatch(self: *Parser) Error!Node.Idx {
    const match_region = self.currentRegion();
    self.advance(); // consume match keyword

    const scratch_marker = self.markScratchNodes();
    defer self.restoreScratchNodes(scratch_marker);

    // Parse scrutinee
    const scrutinee = try self.parseExpr();
    try self.scratch_nodes.append(self.gpa, scrutinee);

    // Check if we have curly braces for block-style match
    const has_braces = self.peek() == .OpenCurly;
    if (has_braces) {
        self.advance(); // consume {
    }

    // Parse branches - each branch is pattern => body
    var branch_count: u32 = 0;

    while (self.peek() != .EndOfFile) {
        // Check for closing brace if we have braces
        if (has_braces and self.peek() == .CloseCurly) {
            self.advance(); // consume }
            break;
        }

        // Parse pattern, then =>, then body
        const pattern = try self.parsePattern();

        // Expect => or ->
        const arrow_type = self.peek();
        if (arrow_type != .OpFatArrow and arrow_type != .OpArrow) {
            _ = try self.pushMalformed(.expected_arrow_after_pattern, self.getCurrentErrorPos());
            break;
        }
        const is_effectful = arrow_type == .OpFatArrow;
        const arrow_end = self.getLastConsumedRegion().end;
        self.advance(); // consume arrow

        // Parse body expression(s)
        // If the body starts on a new line, we may have multiple statements
        const body_start = self.currentPosition();
        const is_multiline_body = (body_start.offset > arrow_end.offset + 1); // Check if there's whitespace/newline after arrow

        const body = if (is_multiline_body) blk: {
            // Multi-line body - parse multiple statements until we hit the next pattern or closing brace
            const body_scratch_marker = self.markScratchNodes();
            defer self.restoreScratchNodes(body_scratch_marker);

            // Parse statements until we hit a pattern or closing brace
            while (self.peek() != .EndOfFile) {
                // Check for closing brace
                if (has_braces and self.peek() == .CloseCurly) {
                    break;
                }

                // Check if this could be the start of a new branch
                // A new branch starts with a pattern (typically UpperIdent or underscore)
                // followed by => or ->
                const could_be_new_branch = switch (self.peek()) {
                    .UpperIdent, .Underscore => true,
                    .LowerIdent => false, // LowerIdent is more likely to be part of the body
                    else => false,
                };

                if (could_be_new_branch) {
                    // We might be at the start of a new branch, stop parsing this body
                    break;
                }

                // Parse the next statement
                const stmt = self.parseExpr() catch break;
                try self.scratch_nodes.append(self.gpa, stmt);
            }

            // Create the body node
            const body_stmts = self.getScratchNodesSince(body_scratch_marker);
            if (body_stmts.len == 0) {
                // Empty body - create malformed node
                break :blk try self.pushMalformed(.expr_unexpected_token, self.getCurrentErrorPos());
            } else if (body_stmts.len == 1) {
                break :blk body_stmts[0];
            } else {
                // Multiple statements - create a block
                const nodes_idx = try self.ast.appendNodeSlice(self.gpa, body_stmts);
                const first_region = self.ast.getRegion(body_stmts[0]);
                const last_region = self.ast.getRegion(body_stmts[body_stmts.len - 1]);
                const block_region = makeRegion(first_region.start, last_region.end);
                break :blk try self.ast.appendNode(self.gpa, block_region, .block, .{ .nodes = nodes_idx });
            }
        } else blk: {
            // Single-line body
            break :blk try self.parseExpr();
        };

        // Create the branch as a binop_thick_arrow or binop_thin_arrow
        const binop_idx = try self.ast.appendBinOp(self.gpa, pattern, body);
        const branch_region = makeRegion(self.ast.start(pattern), self.ast.getRegion(body).end);
        const arrow_tag: Node.Tag = if (is_effectful) .binop_thick_arrow else .binop_thin_arrow;
        const branch = try self.ast.appendNode(self.gpa, branch_region, arrow_tag, .{ .binop = binop_idx });

        // Check if we actually parsed a branch (should be a thick arrow)
        // If not, we've gone past the match expression
        const branch_idx = @as(collections.SafeMultiList(Node).Idx, @enumFromInt(@intFromEnum(branch)));
        const branch_node = self.ast.nodes.get(branch_idx);

        if (branch_node.tag != .binop_thick_arrow) {
            // Not a branch - we've parsed something that's not a valid match branch
            // Since we don't backtrack, treat it as a malformed branch and report error
            // Then break out of branch parsing
            _ = try self.pushMalformed(.expr_unexpected_token, branch_node.region.start);
            break;
        }

        try self.scratch_nodes.append(self.gpa, branch);
        branch_count += 1;

        // Check if we should continue parsing branches
        if (!has_braces) {
            // Without braces, check if next line could be a pattern
            const next_token = self.peek();
            const could_be_pattern = switch (next_token) {
                .UpperIdent, .LowerIdent, .Underscore, .OpenSquare, .OpenCurly, .OpenRound => true,
                else => false,
            };

            if (!could_be_pattern) {
                break;
            }
        }
    }

    const nodes = self.getScratchNodesSince(scratch_marker);
    const nodes_idx = try self.ast.appendNodeSlice(self.gpa, nodes);
    // The region extends to the end of the last branch body
    const last_body = nodes[nodes.len - 1];
    const full_region = makeRegion(match_region.start, self.ast.getRegion(last_body).end);
    return try self.ast.appendNode(self.gpa, full_region, .match, .{ .nodes = nodes_idx });
}

fn parseLambda(self: *Parser) Error!Node.Idx {
    const start_pos = self.currentPosition();
    const start_region = self.currentRegion();

    // Make absolutely sure we're at a |
    if (self.peek() != .OpBar) {
        return self.pushMalformed(.expr_unexpected_token, start_pos);
    }

    self.advance(); // consume |

    const scratch_marker = self.markScratchNodes();
    defer self.restoreScratchNodes(scratch_marker);

    // Set flag to enable record shorthand in lambda parameters
    // In lambda args, { a, b } is shorthand for { a: a, b: b }
    self.is_in_lambda_args = true;
    defer self.is_in_lambda_args = false;

    // Parse parameters (as expressions since we don't distinguish patterns)
    while (self.peek() != .OpBar and self.peek() != .EndOfFile) {
        // Parse parameter with precedence to stop at commas and bars
        // Comma has bp=4, bar has bp=10, so we need min_bp > 10 to stop at bars
        const param = try self.parseExprWithPrecedence(11);
        try self.scratch_nodes.append(self.gpa, param);
        if (self.peek() == .Comma) {
            self.advance();
        } else if (self.peek() != .OpBar) {
            // Expected either comma or closing bar
            const error_pos = self.currentPosition();
            return self.pushMalformed(.expr_unexpected_token, error_pos);
        }
    }

    self.expect(.OpBar) catch {
        try self.pushDiagnostic(.expected_expr_bar, start_pos, self.currentPosition());
        // Return a malformed node if we can't find the closing bar
        return self.pushMalformed(.expr_unexpected_token, self.currentPosition());
    };

    // Parse body (flag is already reset by defer)
    const body = try self.parseExpr();

    const params = self.getScratchNodesSince(scratch_marker);

    // Use a new scratch marker to build body_then_args
    const temp_marker = self.markScratchNodes();
    defer self.restoreScratchNodes(temp_marker);

    try self.scratch_nodes.append(self.gpa, body);
    try self.scratch_nodes.appendSlice(self.gpa, params);

    const nodes_idx = try self.ast.appendNodeSlice(self.gpa, self.getScratchNodesSince(temp_marker));
    const full_region = makeRegion(start_region.start, self.ast.getRegion(body).end);
    return try self.ast.appendNode(self.gpa, full_region, .lambda, .{ .body_then_args = nodes_idx });
}

fn parseVar(self: *Parser) Error!Node.Idx {
    const var_region = self.currentRegion();
    self.advance(); // consume var

    if (self.peek() != .LowerIdent) {
        return self.pushMalformed(.var_must_have_ident, var_region.start);
    }

    const ident_region = self.currentRegion();
    const ident = self.currentIdent();
    self.advance();

    if (ident) |id| {
        const full_region = makeRegion(var_region.start, ident_region.end);
        return try self.ast.appendNode(self.gpa, full_region, .var_lc, .{ .ident = id });
    } else {
        return self.pushMalformed(.var_must_have_ident, var_region.start);
    }
}

fn parseFor(self: *Parser) Error!Node.Idx {
    const for_region = self.currentRegion();
    self.advance(); // consume 'for'

    const scratch_marker = self.markScratchNodes();
    defer self.restoreScratchNodes(scratch_marker);

    // Parse the pattern (e.g., 'x' in 'for x in ...')
    const pattern = try self.parseExpr();
    try self.scratch_nodes.append(self.gpa, pattern);

    // Expect 'in'
    if (self.peek() != .KwIn) {
        try self.pushDiagnostic(.for_expected_in, for_region.start, self.currentPosition());
        const error_region = makeRegion(for_region.start, self.currentPosition());
        return try self.ast.appendNode(self.gpa, error_region, .malformed, .{ .malformed = .for_expected_in });
    }
    self.advance(); // consume 'in'

    // Parse the iterable expression
    const iterable = try self.parseExpr();
    try self.scratch_nodes.append(self.gpa, iterable);

    // Parse the body (just an expression, similar to while and lambda body)
    const body = try self.parseExpr();
    try self.scratch_nodes.append(self.gpa, body);

    const nodes = self.getScratchNodesSince(scratch_marker);

    // A for loop must have exactly pattern, iterable, and body
    if (nodes.len != 3) {
        const error_region = makeRegion(for_region.start, self.currentPosition());
        return try self.ast.appendNode(self.gpa, error_region, .malformed, .{ .malformed = .statement_unexpected_token });
    }

    const nodes_idx = try self.ast.appendNodeSlice(self.gpa, nodes);
    const full_region = makeRegion(for_region.start, self.ast.getRegion(body).end);
    return try self.ast.appendNode(self.gpa, full_region, .for_loop, .{ .nodes = nodes_idx });
}

fn parseWhile(self: *Parser) Error!Node.Idx {
    const while_region = self.currentRegion();
    self.advance(); // consume 'while'

    const scratch_marker = self.markScratchNodes();
    defer self.restoreScratchNodes(scratch_marker);

    // Parse the condition
    const condition = try self.parseExpr();
    try self.scratch_nodes.append(self.gpa, condition);

    // Parse the body (just an expression, similar to lambda body)
    const body = try self.parseExpr();
    try self.scratch_nodes.append(self.gpa, body);

    const nodes = self.getScratchNodesSince(scratch_marker);

    // A while loop must have exactly condition and body
    if (nodes.len != 2) {
        const error_region = makeRegion(while_region.start, self.currentPosition());
        return try self.ast.appendNode(self.gpa, error_region, .malformed, .{ .malformed = .statement_unexpected_token });
    }

    const nodes_idx = try self.ast.appendNodeSlice(self.gpa, nodes);
    const full_region = makeRegion(while_region.start, self.ast.getRegion(body).end);
    return try self.ast.appendNode(self.gpa, full_region, .while_loop, .{ .nodes = nodes_idx });
}

fn parseReturn(self: *Parser) Error!Node.Idx {
    const return_region = self.currentRegion();
    self.advance(); // consume return

    // Parse the expression to return
    // This avoids mutual recursion since we're calling parseExprWithPrecedence directly
    const expr = try self.parseExprWithPrecedence(0);
    const expr_slice = [_]Node.Idx{expr};
    const nodes_idx = try self.ast.appendNodeSlice(self.gpa, &expr_slice);
    const full_region = makeRegion(return_region.start, self.ast.getRegion(expr).end);
    return try self.ast.appendNode(self.gpa, full_region, .ret, .{ .nodes = nodes_idx });
}

fn parseCrash(self: *Parser) Error!Node.Idx {
    const crash_region = self.currentRegion();
    self.advance(); // consume crash

    // Parse the expression to crash with
    // This avoids mutual recursion since we're calling parseExprWithPrecedence directly
    const expr = try self.parseExprWithPrecedence(0);
    const expr_slice = [_]Node.Idx{expr};
    const nodes_idx = try self.ast.appendNodeSlice(self.gpa, &expr_slice);
    const full_region = makeRegion(crash_region.start, self.ast.getRegion(expr).end);
    return try self.ast.appendNode(self.gpa, full_region, .crash, .{ .nodes = nodes_idx });
}

/// Parse a type annotation
fn parseTypeAnno(self: *Parser) Error!Node.Idx {
    return self.parseExpr();
}

// Operator precedence
pub const BindingPower = struct {
    left: u8,
    right: u8,
};

pub fn getBindingPower(tag: Token.Tag) BindingPower {
    return switch (tag) {
        // OpBar (|) is not an operator in Roc - it's only used in lambda syntax
        // It should never have binding power for operator precedence
        .OpBar => .{ .left = 0, .right = 0 },
        .OpOr => .{ .left = 20, .right = 21 },
        .OpAnd => .{ .left = 30, .right = 31 },
        .OpEquals, .OpNotEquals => .{ .left = 40, .right = 41 },
        .OpLessThan, .OpLessThanOrEq, .OpGreaterThan, .OpGreaterThanOrEq => .{ .left = 50, .right = 51 },
        .OpPlus, .OpBinaryMinus, .OpUnaryMinus => .{ .left = 60, .right = 61 },
        .OpStar, .OpSlash, .OpDoubleSlash => .{ .left = 70, .right = 71 },
        .OpDoubleQuestion => .{ .left = 80, .right = 81 },
        .OpAssign => .{ .left = 5, .right = 6 },
        .OpColon => .{ .left = 3, .right = 4 },
        .OpColonEqual => .{ .left = 3, .right = 4 },
        .OpArrow => .{ .left = 2, .right = 3 },
        .OpFatArrow => .{ .left = 1, .right = 2 },
        .KwAs => .{ .left = 3, .right = 4 },
        .KwWhere => .{ .left = 1, .right = 2 },
        else => .{ .left = 0, .right = 0 },
    };
}

fn tokenToBinOpTag(tag: Token.Tag) ?Node.Tag {
    return switch (tag) {
        .OpAssign => .binop_equals,
        .OpEquals => .binop_double_equals,
        .OpNotEquals => .binop_not_equals,
        .OpColon => .binop_colon,
        .OpColonEqual => .binop_colon_equals,
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
