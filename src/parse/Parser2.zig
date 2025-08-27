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
const Region = base.Region;
const Position = base.Region.Position;
const TokenizedBuffer = tokenize.TokenizedBuffer;
const Token = tokenize.Token;
const TokenIdx = Token.Idx;
const tokenize = @import("tokenize_iter.zig");
const tokenize_iter = @import("tokenize_iter.zig");
const Ident = base.Ident;

const MAX_PARSE_DIAGNOSTICS: usize = 1_000;
const MAX_PARSE_STACK_SIZE: usize = 10_000;

/// Temporary helper to create a Region from start and end positions
/// TODO: Update all callers to compute proper end positions
fn makeRegion(start: Position, end: Position) Region {
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
token_iter: tokenize_iter.TokenIterator,
lookahead: ?Token, // Single token lookahead - LL(1) parser, no more!
last_position: Position, // Track last valid position for error reporting
last_region: Region, // Track last consumed token's region
ast: *AST,
byte_slices: *collections.ByteSlices, // Reference to tokenizer's ByteSlices
scratch_nodes: std.ArrayListUnmanaged(Node.Idx),
scratch_op_stack: std.ArrayListUnmanaged(OpInfo), // For operator precedence parsing
scratch_bytes: std.ArrayListUnmanaged(u8), // For string building
is_in_lambda_args: bool = false, // Track if we're parsing lambda parameters
diagnostics: std.ArrayListUnmanaged(AST.Diagnostic),
/// init the parser from source text using TokenIterator
pub fn init(env: *base.CommonEnv, gpa: std.mem.Allocator, source: []const u8, messages: []tokenize.Diagnostic, ast: *AST, byte_slices: *collections.ByteSlices) std.mem.Allocator.Error!Parser {
    return initWithOptions(env, gpa, source, messages, ast, byte_slices);
}

/// init the parser with options (for testing)
pub fn initWithOptions(env: *base.CommonEnv, gpa: std.mem.Allocator, source: []const u8, messages: []tokenize.Diagnostic, ast: *AST, byte_slices: *collections.ByteSlices) std.mem.Allocator.Error!Parser {
    const token_iter = try tokenize_iter.TokenIterator.init(env, gpa, source, messages, byte_slices);

    var parser = Parser{
        .gpa = gpa,
        .token_iter = token_iter,
        .lookahead = null,
        .last_position = Position{ .offset = 0 },
        .last_region = Region{ .start = Position{ .offset = 0 }, .end = Position{ .offset = 0 } },
        .ast = ast,
        .byte_slices = byte_slices,
        .scratch_nodes = .{},
        .scratch_op_stack = .{},
        .scratch_bytes = .{},
        .diagnostics = .{},
    };

    // Fill initial lookahead buffer
    try parser.fillLookahead();

    return parser;
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
    parser.scratch_op_stack.deinit(parser.gpa);
    parser.scratch_bytes.deinit(parser.gpa);
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
pub fn parseExprWithPrecedence(self: *Parser, initial_min_bp: u8) Error!Node.Idx {
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
                    // This should be called when we see | in expression context
                    // If we're not getting here, something is wrong
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

            // If we're inside lambda args and see |, treat it as a delimiter, not an operator
            if (self.is_in_lambda_args and op_tag == .OpBar) {
                // Stop parsing - we've hit the closing | of the lambda parameters
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
                left = try self.ast.appendNode(self.gpa, op_info.op_region, binop_tag, .{ .binop = binop_idx });
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
        // Use last position as approximation
        close_paren_region = makeRegion(self.currentPosition(), self.currentPosition());
    }

    const nodes = self.getScratchNodesSince(scratch_marker);
    const nodes_idx = try self.ast.appendNodeSlice(self.gpa, nodes);
    const full_region = makeRegion(open_paren_region.start, close_paren_region.end);
    return try self.ast.appendNode(self.gpa, full_region, .apply_module, .{ .nodes = nodes_idx });
}

/// helper to advance the parser by one token
pub fn advance(self: *Parser) void {
    // Save current position before advancing
    if (self.lookahead) |token| {
        self.last_position = token.region.end;
        self.last_region = token.region;
    }

    // Get next token
    self.lookahead = self.token_iter.next(self.gpa) catch null;
}

/// Get the region of the last consumed token
fn getLastConsumedRegion(self: *Parser) Region {
    return self.last_region;
}

/// look ahead at the next token and return an error if it does not have the expected tag
/// Following our philosophy to NEVER stop parsing, this returns an error that can be caught
/// but the caller should always handle it and continue parsing
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

    // Parse the header if present
    try self.parseHeader();

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
    // Parse app { pf: "..." platform [main!], package1: "...", ... }
    self.expect(.OpenCurly) catch {
        try self.pushDiagnostic(.expected_package_platform_open_curly, start_pos, self.currentPosition());
        return AST.Header{ .app = .{
            .packages = @enumFromInt(0),
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

        // Parse the value part
        if (self.peek() == .String) {
            // This is a package or platform string
            const string_value = try self.parseExpr();

            // Check if followed by 'platform' keyword
            if (self.peek() == .KwPlatform) {
                self.advance(); // consume 'platform' keyword

                // Parse provides list after platform keyword
                self.expect(.OpenSquare) catch {
                    try self.pushDiagnostic(.header_expected_open_square, field_start, self.currentPosition());
                    continue;
                };

                const provides_idx = try self.parseExposedList(.CloseSquare);

                self.expect(.CloseSquare) catch {
                    try self.pushDiagnostic(.header_expected_close_square, field_start, self.currentPosition());
                };

                // Create the provides list node (use block as a container for the exposed items)
                // The region for the provides list spans from the current position to the last exposed item
                const provides_start = self.currentPosition();
                const provides_region = if (provides_idx != @as(collections.NodeSlices(Node.Idx).Idx, @enumFromInt(0))) blk: {
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
                        break :blk makeRegion(provides_start, provides_start);
                    }
                } else makeRegion(provides_start, provides_start);
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
            if (self.peek() == .OpBang) {
                final_region.end = self.currentRegion().end;
                self.advance(); // consume the !
            }

            if (ident) |id| {
                return try self.ast.appendNode(self.gpa, final_region, .lc, .{ .ident = id });
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
        // Just parse as expression - should be a lowercase identifier
        const type_var = try self.parseExprWithPrecedence(0);
        try self.scratch_nodes.append(self.gpa, type_var);

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
pub fn parseTopLevelStatement(self: *Parser) Error!?Node.Idx {
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

            // Check if this is followed by : or := (making it a type annotation/declaration)
            if (self.peek() == .OpColon or self.peek() == .OpColonEqual) {
                const is_nominal = self.peek() == .OpColonEqual;
                const colon_region = self.currentRegion();
                self.advance(); // consume : or :=

                // Parse the right-hand side
                // If we're in a potential record, use higher precedence to stop at commas
                const rhs = if (in_potential_record)
                    try self.parseExprWithPrecedence(10) // Stop at commas (precedence 4)
                else
                    try self.parseExpr(); // Normal parsing

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

                // Parse the right-hand side as a regular expression
                // The expression parser will handle comma-before-arrow uniformly
                const rhs = try self.parseExpr();

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

    const scratch_marker = self.markScratchNodes();
    defer self.restoreScratchNodes(scratch_marker);

    // Parse module path (e.g., pf.Stdout)
    while (true) {
        const token = self.peek();
        if (token == .LowerIdent or token == .UpperIdent) {
            const ident = self.currentIdent();
            const ident_region = self.currentRegion();
            if (ident) |id| {
                // Use the correct node type based on identifier case
                const node_tag: AST.Node.Tag = if (token == .UpperIdent) .uc else .lc;
                const node = try self.ast.appendNode(self.gpa, ident_region, node_tag, .{ .ident = id });
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
            const alias_region = self.currentRegion();
            if (alias_ident) |id| {
                const alias_tag: AST.Node.Tag = if (alias_token == .UpperIdent) .uc else .lc;
                const alias_node = try self.ast.appendNode(self.gpa, alias_region, alias_tag, .{ .ident = id });
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

    const nodes = self.getScratchNodesSince(scratch_marker);
    if (nodes.len == 0) {
        return null;
    }
    const nodes_idx = try self.ast.appendNodeSlice(self.gpa, nodes);
    // Use the new import node type - region spans from import keyword to last node
    const last_region = self.ast.nodes.fieldItem(.region, @as(collections.SafeMultiList(Node).Idx, @enumFromInt(@intFromEnum(nodes[nodes.len - 1]))));
    const full_region = makeRegion(start_pos, last_region.end);
    return try self.ast.appendNode(self.gpa, full_region, .import, .{ .import_nodes = nodes_idx });
}

/// Parse an expression
pub fn parseExpr(self: *Parser) Error!Node.Idx {
    return self.parseExprWithPrecedence(0);
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

    self.advance();

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
        // Use the current position as an approximation for error recovery
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
                const stmt = try self.parseStmtOrRecordField(true);
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

                // Check for comma - if we see one, this is a record!
                if (self.peek() == .Comma) {
                    is_record = true;
                    self.advance();
                } else if (!is_record) {
                    // In block mode, keep parsing statements
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

fn parseIf(self: *Parser) Error!Node.Idx {
    const start_pos = self.currentPosition();
    const if_region = self.currentRegion();
    self.advance(); // consume if

    const scratch_marker = self.markScratchNodes();
    defer self.restoreScratchNodes(scratch_marker);

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

fn parseMatch(self: *Parser) Error!Node.Idx {
    const match_region = self.currentRegion();
    self.advance(); // consume match keyword

    const scratch_marker = self.markScratchNodes();
    defer self.restoreScratchNodes(scratch_marker);

    // Parse scrutinee
    const scrutinee = try self.parseExpr();
    try self.scratch_nodes.append(self.gpa, scrutinee);

    // No arrow or curly braces after scrutinee in Roc's match syntax
    // Branches follow on indented lines

    // Parse branches - each branch is pattern => body
    // In Roc, match branches are indentation-based, not separated by commas or bars
    var branch_count: u32 = 0;

    // We need to parse pattern => body pairs as single expressions
    // The => operator will bind them together as binop_thick_arrow nodes
    // IMPORTANT: Due to how expression parsing works, multi-line function applications
    // may not be fully captured in the body. This is a known limitation that needs
    // fixing in the expression parser's handling of line continuations.
    while (self.peek() != .EndOfFile) {
        // Parse the entire branch (pattern => body) as one expression
        // This will naturally create a binop_thick_arrow node
        const branch = try self.parseExpr();

        // Check if we actually parsed a branch (should be a thick arrow)
        // If not, we've gone past the match expression
        const branch_idx = @as(collections.SafeMultiList(Node).Idx, @enumFromInt(@intFromEnum(branch)));
        const branch_node = self.ast.nodes.get(branch_idx);

        if (branch_node.tag != .binop_thick_arrow) {
            // Not a branch - restore position and break
            // Actually we can't easily restore position, so just break
            // This means we might have consumed a token that's not part of the match
            // TODO: Better error recovery here
            break;
        }

        try self.scratch_nodes.append(self.gpa, branch);
        branch_count += 1;

        // Check if we should continue parsing branches
        const next_token = self.peek();
        const could_be_pattern = switch (next_token) {
            .UpperIdent, .LowerIdent, .Underscore, .OpenSquare, .OpenCurly, .OpenRound => true,
            else => false,
        };

        if (!could_be_pattern) {
            break;
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

    // Set flag to treat | as a closing delimiter while parsing parameters
    self.is_in_lambda_args = true;
    defer self.is_in_lambda_args = false;

    // Parse parameters (as expressions since we don't distinguish patterns)
    while (self.peek() != .OpBar and self.peek() != .EndOfFile) {
        // Parse as a full expression - the | will act as a delimiter
        const param = try self.parseExpr();
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
pub fn parseTypeAnno(self: *Parser) Error!Node.Idx {
    return self.parseExpr();
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
