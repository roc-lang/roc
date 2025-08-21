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
const MAX_PARSE_STACK_SIZE: usize = 10_000;

/// Represents where a parse result should be stored
const ResultDest = union(enum) {
    /// Return this value as the final result
    return_value: void,
    /// Store in a variable
    store_in_var: *Node.Idx,
    /// Append to a list
    append_to_list: *std.ArrayListUnmanaged(Node.Idx),
    /// Store in a field
    store_in_field: struct {
        obj: *Node.Idx,
        field: []const u8,
    },
};

/// Parse state for the iterative parser
const ParseState = union(enum) {
    /// Parse an expression with binding power
    expr_with_bp: struct {
        min_bp: u8,
        dest: ResultDest,
    },

    /// Parse a primary expression
    primary_expr: struct {
        dest: ResultDest,
    },

    /// Continue parsing binary operator RHS
    parse_binary_rhs: struct {
        left: ?Node.Idx, // Optional - null when coming from expr_with_bp
        op_tag: Token.Tag,
        op_pos: Position,
        right_bp: u8,
        min_bp: u8,
        dest: ResultDest,
    },

    /// Combine binary operator results
    parse_binary_combine: struct {
        left: Node.Idx,
        op_tag: Token.Tag,
        op_pos: Position,
        min_bp: u8,
        dest: ResultDest,
    },

    /// Parse list elements
    parse_list_elements: struct {
        elements: *std.ArrayListUnmanaged(Node.Idx),
        dest: ResultDest,
    },

    /// Parse record fields
    parse_record_fields: struct {
        fields: *std.ArrayListUnmanaged(Node.Idx),
        dest: ResultDest,
    },

    /// Parse if condition
    parse_if_condition: struct {
        dest: ResultDest,
    },

    /// Parse if then branch
    parse_if_then: struct {
        condition: Node.Idx,
        dest: ResultDest,
    },

    /// Parse if else branch
    parse_if_else: struct {
        condition: Node.Idx,
        then_branch: Node.Idx,
        dest: ResultDest,
    },

    /// Parse function application arguments
    parse_apply_args: struct {
        func: Node.Idx,
        args: *std.ArrayListUnmanaged(Node.Idx),
        dest: ResultDest,
    },

    /// Parse match scrutinee
    parse_match_scrutinee: struct {
        dest: ResultDest,
    },

    /// Parse match arms
    parse_match_arms: struct {
        scrutinee: Node.Idx,
        arms: *std.ArrayListUnmanaged(Node.Idx),
        dest: ResultDest,
    },

    /// Parse lambda body
    parse_lambda_body: struct {
        params: []const Node.Idx,
        dest: ResultDest,
    },
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
ast: *AST,
byte_slices: *collections.ByteSlices, // Reference to tokenizer's ByteSlices
scratch_nodes: std.ArrayListUnmanaged(Node.Idx),
is_in_lambda_args: bool = false, // Track if we're parsing lambda parameters
diagnostics: std.ArrayListUnmanaged(AST.Diagnostic),
cached_malformed_node: ?Node.Idx,
/// Stack for iterative parsing to avoid recursion
parse_stack: std.ArrayListUnmanaged(ParseState),
/// Temporary results storage for intermediate values
temp_results: std.ArrayListUnmanaged(Node.Idx),
/// Use iterative parser (for testing)
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
        .ast = ast,
        .byte_slices = byte_slices,
        .scratch_nodes = .{},
        .diagnostics = .{},
        .cached_malformed_node = null,
        .parse_stack = .{},
        .temp_results = .{},
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
    parser.parse_stack.deinit(parser.gpa);
    parser.temp_results.deinit(parser.gpa);
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

/// Push a parse state onto the stack
fn pushParseState(self: *Parser, state: ParseState) Error!void {
    if (self.parse_stack.items.len >= MAX_PARSE_STACK_SIZE) {
        // Create malformed node instead of crashing
        return self.handleStackOverflow();
    }
    try self.parse_stack.append(self.gpa, state);
}

/// Handle stack overflow by creating a malformed node
fn handleStackOverflow(self: *Parser) Error!void {
    // Clear the parse stack to recover
    self.parse_stack.clearRetainingCapacity();

    // Add a diagnostic for the overflow - use expr_unexpected_token as a generic error
    const pos = self.currentPosition();
    try self.pushDiagnostic(.expr_unexpected_token, pos, pos);
}

/// Store a result based on the destination
fn storeResult(self: *Parser, result: Node.Idx, dest: ResultDest) Error!void {
    switch (dest) {
        .return_value => {
            try self.temp_results.append(self.gpa, result);
        },
        .store_in_var => |ptr| {
            ptr.* = result;
        },
        .append_to_list => |list| {
            try list.append(self.gpa, result);
        },
        .store_in_field => |field_info| {
            // This would require more complex handling
            // For now, we'll just store in the object
            field_info.obj.* = result;
        },
    }
}

/// Process a single parse state
fn processParseState(self: *Parser, state: ParseState) Error!void {
    switch (state) {
        .expr_with_bp => |expr_state| {
            // First push a state to continue with operators after we get the primary expr
            try self.pushParseState(.{
                .parse_binary_rhs = .{
                    .left = null, // Will be filled by primary_expr via temp_results
                    .op_tag = .EndOfFile, // Dummy value
                    .op_pos = Position{ .offset = 0 },
                    .right_bp = 0,
                    .min_bp = expr_state.min_bp,
                    .dest = expr_state.dest,
                },
            });

            // Then push state to parse primary expression
            try self.pushParseState(.{
                .primary_expr = .{
                    .dest = .return_value, // Store in temp_results
                },
            });
        },
        .primary_expr => |primary_state| {
            const result = try self.processPrimaryExprIterative();
            try self.storeResult(result, primary_state.dest);
        },
        .parse_binary_rhs => |rhs_state| {
            // Get the left operand from temp_results if needed
            var left = if (rhs_state.left) |l|
                l
            else blk: {
                if (self.temp_results.items.len > 0) {
                    const result = self.temp_results.items[self.temp_results.items.len - 1];
                    self.temp_results.items.len -= 1;
                    break :blk result;
                } else {
                    // Error: no left operand available
                    const pos = self.currentPosition();
                    break :blk try self.pushMalformed(.expr_unexpected_token, pos);
                }
            };

            // Check for operators
            while (true) {
                const op_tag = self.peek();
                const bp = getBindingPower(op_tag);

                if (bp.left > rhs_state.min_bp) {
                    const op_pos = self.currentPosition();
                    self.advance();

                    // We need to parse the right side with higher precedence
                    // Push a continuation to combine the results
                    try self.pushParseState(.{ .parse_binary_combine = .{
                        .left = left,
                        .op_tag = op_tag,
                        .op_pos = op_pos,
                        .min_bp = rhs_state.min_bp,
                        .dest = rhs_state.dest,
                    } });

                    // Parse the right operand with higher precedence
                    try self.pushParseState(.{ .expr_with_bp = .{
                        .min_bp = bp.right,
                        .dest = .return_value,
                    } });
                    return;
                }

                // Also check for postfix operators (function application, dot access)
                switch (self.peek()) {
                    .OpenRound => {
                        // Function application
                        left = try self.parseApply(left);
                        continue;
                    },
                    .Dot => {
                        // Dot access - handle inline for now
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
                                const pos = self.currentPosition();
                                left = try self.pushMalformed(.expr_dot_suffix_not_allowed, pos);
                            }
                        } else if (self.peek() == .UpperIdent) {
                            const ident = self.currentIdent();
                            self.advance();

                            if (ident) |id| {
                                const field = try self.ast.appendNode(self.gpa, dot_pos, .uc, .{ .ident = id });
                                const binop_idx = try self.ast.appendBinOp(self.gpa, left, field);
                                left = try self.ast.appendNode(self.gpa, dot_pos, .binop_pipe, .{ .binop = binop_idx });
                            } else {
                                const pos = self.currentPosition();
                                left = try self.pushMalformed(.expr_dot_suffix_not_allowed, pos);
                            }
                        } else {
                            const pos = self.currentPosition();
                            left = try self.pushMalformed(.expr_dot_suffix_not_allowed, pos);
                        }
                        continue;
                    },
                    else => {
                        // No more operators, store result
                        try self.storeResult(left, rhs_state.dest);
                        return;
                    },
                }
            }
        },
        .parse_binary_combine => |combine_state| {
            // Get the right operand from temp_results
            const right = if (self.temp_results.items.len > 0) blk: {
                const result = self.temp_results.items[self.temp_results.items.len - 1];
                self.temp_results.items.len -= 1;
                break :blk result;
            } else blk: {
                // Error: no right operand available
                const pos = self.currentPosition();
                break :blk try self.pushMalformed(.expr_unexpected_token, pos);
            };

            // Create the binary operation node
            const binop_tag = tokenToBinOpTag(combine_state.op_tag) orelse {
                const pos = self.currentPosition();
                const malformed = try self.pushMalformed(.expr_unexpected_token, pos);
                try self.storeResult(malformed, combine_state.dest);
                return;
            };

            const binop_idx = try self.ast.appendBinOp(self.gpa, combine_state.left, right);
            const result = try self.ast.appendNode(self.gpa, combine_state.op_pos, binop_tag, .{ .binop = binop_idx });

            // Continue parsing with the new left operand
            try self.pushParseState(.{
                .parse_binary_rhs = .{
                    .left = result,
                    .op_tag = .EndOfFile, // Dummy value
                    .op_pos = Position{ .offset = 0 },
                    .right_bp = 0,
                    .min_bp = combine_state.min_bp,
                    .dest = combine_state.dest,
                },
            });
        },
        .parse_list_elements => |list_state| {
            // Parse list elements iteratively
            _ = list_state;
        },
        .parse_record_fields => |record_state| {
            // Parse record fields iteratively
            _ = record_state;
        },
        .parse_if_condition => |if_state| {
            // Parse if condition
            _ = if_state;
        },
        .parse_if_then => |then_state| {
            // Parse then branch
            _ = then_state;
        },
        .parse_if_else => |else_state| {
            // Parse else branch
            _ = else_state;
        },
        .parse_apply_args => |apply_state| {
            // Parse function application arguments
            _ = apply_state;
        },
        .parse_match_scrutinee => |match_state| {
            // Parse match scrutinee
            _ = match_state;
        },
        .parse_match_arms => |arms_state| {
            // Parse match arms
            _ = arms_state;
        },
        .parse_lambda_body => |lambda_state| {
            // Parse lambda body
            _ = lambda_state;
        },
    }
}

/// Process primary expression iteratively
fn processPrimaryExprIterative(self: *Parser) Error!Node.Idx {
    const pos = self.currentPosition();

    return switch (self.peek()) {
        .LowerIdent => {
            const ident = self.currentIdent();
            self.advance();

            if (ident) |id| {
                return try self.ast.appendNode(self.gpa, pos, .lc, .{ .ident = id });
            } else {
                return self.pushMalformed(.expr_unexpected_token, pos);
            }
        },
        .UpperIdent => {
            const ident = self.currentIdent();
            self.advance();

            if (ident) |id| {
                return try self.ast.appendNode(self.gpa, pos, .uc, .{ .ident = id });
            } else {
                return self.pushMalformed(.expr_unexpected_token, pos);
            }
        },
        .Underscore => {
            self.advance();
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
            self.advance();
            // For unary operators, we need to parse the operand iteratively
            // For now, use the recursive version as a placeholder
            const operand = try self.parseExprWithBp(100);
            const operand_slice = [_]Node.Idx{operand};
            const nodes_idx = try self.ast.appendNodeSlice(self.gpa, &operand_slice);
            return try self.ast.appendNode(self.gpa, pos, .unary_not, .{ .nodes = nodes_idx });
        },
        .OpBinaryMinus, .OpUnaryMinus => {
            self.advance();
            // For unary operators, we need to parse the operand iteratively
            // For now, use the recursive version as a placeholder
            const operand = try self.parseExprWithBp(100);
            const operand_slice = [_]Node.Idx{operand};
            const nodes_idx = try self.ast.appendNodeSlice(self.gpa, &operand_slice);
            return try self.ast.appendNode(self.gpa, pos, .unary_neg, .{ .nodes = nodes_idx });
        },
        .DoubleDot => {
            self.advance();
            // For unary operators, we need to parse the operand iteratively
            // For now, use the recursive version as a placeholder
            const operand = try self.parseExprWithBp(100);
            const operand_slice = [_]Node.Idx{operand};
            const nodes_idx = try self.ast.appendNodeSlice(self.gpa, &operand_slice);
            return try self.ast.appendNode(self.gpa, pos, .unary_double_dot, .{ .nodes = nodes_idx });
        },
        .KwModule => {
            // Handle module(arg) for where clauses
            self.advance();

            if (self.peek() == .OpenRound) {
                return self.parseModuleApply();
            } else {
                // module without parentheses is an error in expression context
                return self.pushMalformed(.expr_unexpected_token, pos);
            }
        },
        else => {
            // Always use current token position for error reporting
            const error_pos = self.currentPosition();
            return self.pushMalformed(.expr_unexpected_token, error_pos);
        },
    };
}

/// Parse an expression with precedence iteratively using labeled switch
/// This version uses minimal heap allocation (only for operator stack) and direct computed jumps
pub fn parseExprWithBpIterativeLabeledSwitch(self: *Parser, initial_min_bp: u8) Error!Node.Idx {
    // Heap-allocated stack for operator precedence (can handle arbitrary nesting)
    const OpInfo = struct {
        left: ?Node.Idx, // null for unary operators
        op_tag: Token.Tag,
        op_pos: Position,
        min_bp: u8,
    };

    var op_stack = std.ArrayList(OpInfo).init(self.gpa);
    defer op_stack.deinit();

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
                    const pos = self.currentPosition();
                    self.advance();
                    if (ident) |id| {
                        break :blk try self.ast.appendNode(self.gpa, pos, .lc, .{ .ident = id });
                    } else {
                        break :blk try self.pushMalformed(.expr_unexpected_token, pos);
                    }
                },
                .UpperIdent => blk: {
                    const ident = self.currentIdent();
                    const pos = self.currentPosition();
                    self.advance();
                    if (ident) |id| {
                        break :blk try self.ast.appendNode(self.gpa, pos, .uc, .{ .ident = id });
                    } else {
                        break :blk try self.pushMalformed(.expr_unexpected_token, pos);
                    }
                },
                .Underscore => blk: {
                    const pos = self.currentPosition();
                    self.advance();
                    break :blk try self.ast.appendNode(self.gpa, pos, .underscore, .{ .src_bytes_end = pos });
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
                    const pos = self.currentPosition();
                    self.advance();
                    // For unary, we need to parse with high precedence
                    // Push current state and parse operand
                    try op_stack.append(.{
                        .left = null, // null for unary operator
                        .op_tag = .OpBang,
                        .op_pos = pos,
                        .min_bp = min_bp,
                    });
                    min_bp = 100; // High precedence for unary
                    continue :parse .parse_primary;
                },
                .OpBinaryMinus, .OpUnaryMinus => {
                    const pos = self.currentPosition();
                    self.advance();
                    try op_stack.append(.{
                        .left = null, // null for unary operator
                        .op_tag = .OpUnaryMinus,
                        .op_pos = pos,
                        .min_bp = min_bp,
                    });
                    min_bp = 100; // High precedence for unary
                    continue :parse .parse_primary;
                },
                .DoubleDot => {
                    const pos = self.currentPosition();
                    self.advance();
                    try op_stack.append(.{
                        .left = null, // null for unary operator
                        .op_tag = .DoubleDot,
                        .op_pos = pos,
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
                const op_pos = self.currentPosition();
                self.advance();

                // Save current state to stack
                try op_stack.append(.{
                    .left = left,
                    .op_tag = op_tag,
                    .op_pos = op_pos,
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
                            return self.pushMalformed(.expr_dot_suffix_not_allowed, dot_pos);
                        }
                    } else if (self.peek() == .UpperIdent) {
                        const ident = self.currentIdent();
                        self.advance();

                        if (ident) |id| {
                            const field = try self.ast.appendNode(self.gpa, dot_pos, .uc, .{ .ident = id });
                            const binop_idx = try self.ast.appendBinOp(self.gpa, left, field);
                            left = try self.ast.appendNode(self.gpa, dot_pos, .binop_pipe, .{ .binop = binop_idx });
                        } else {
                            return self.pushMalformed(.expr_dot_suffix_not_allowed, dot_pos);
                        }
                    } else if (self.peek() == .Int or self.peek() == .Float) {
                        const num = try self.parseNumLiteral();
                        const binop_idx = try self.ast.appendBinOp(self.gpa, left, num);
                        left = try self.ast.appendNode(self.gpa, dot_pos, .binop_pipe, .{ .binop = binop_idx });
                    } else {
                        return self.pushMalformed(.expr_dot_suffix_not_allowed, dot_pos);
                    }
                    continue :parse .check_operators;
                },
                else => {
                    // No more operators at this level
                    // Check if we need to combine with a stacked operator
                    if (op_stack.items.len > 0) {
                        continue :parse .combine_binary;
                    }
                    // We're done!
                    return left;
                },
            }
        },

        .parse_rhs => {
            // Not used in this version - we jump directly to parse_primary
            unreachable;
        },

        .combine_binary => {
            // Pop operator from stack and combine
            const op_info = op_stack.pop() orelse unreachable; // Stack should never be empty here

            // Check if this is a unary operator (left is null)
            if (op_info.left == null) {
                // Unary operator
                const operand = left;
                const operand_slice = [_]Node.Idx{operand};
                const nodes_idx = try self.ast.appendNodeSlice(self.gpa, &operand_slice);

                left = switch (op_info.op_tag) {
                    .OpBang => try self.ast.appendNode(self.gpa, op_info.op_pos, .unary_not, .{ .nodes = nodes_idx }),
                    .OpUnaryMinus => try self.ast.appendNode(self.gpa, op_info.op_pos, .unary_neg, .{ .nodes = nodes_idx }),
                    .DoubleDot => try self.ast.appendNode(self.gpa, op_info.op_pos, .unary_double_dot, .{ .nodes = nodes_idx }),
                    else => unreachable,
                };
            } else {
                // Binary operator - combine left and right
                const right = left;
                const binop_tag = tokenToBinOpTag(op_info.op_tag) orelse {
                    return self.pushMalformed(.expr_unexpected_token, op_info.op_pos);
                };

                const binop_idx = try self.ast.appendBinOp(self.gpa, op_info.left.?, right);
                left = try self.ast.appendNode(self.gpa, op_info.op_pos, binop_tag, .{ .binop = binop_idx });
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

/// Old heap-based iterative version (kept for reference/comparison)
pub fn parseExprWithBpIterativeHeapBased(self: *Parser, min_bp: u8) Error!Node.Idx {
    var result: Node.Idx = @enumFromInt(0);

    // Clear any previous state
    self.parse_stack.clearRetainingCapacity();
    self.temp_results.clearRetainingCapacity();

    // Push initial state
    try self.pushParseState(.{ .expr_with_bp = .{
        .min_bp = min_bp,
        .dest = .{ .store_in_var = &result },
    } });

    // Main iterative loop
    while (self.parse_stack.items.len > 0) {
        const state = self.parse_stack.items[self.parse_stack.items.len - 1];
        self.parse_stack.items.len -= 1;
        try self.processParseState(state);
    }

    // If we didn't get a result, return a malformed node
    if (@intFromEnum(result) == 0) {
        const pos = self.currentPosition();
        return self.pushMalformed(.expr_unexpected_token, pos);
    }

    return result;
}

// Placeholder iterative functions - these will be implemented to be fully iterative
// For now, they use the recursive versions or return malformed nodes

fn parseModuleApply(self: *Parser) Error!Node.Idx {
    // Temporarily create a minimal implementation
    const pos = self.currentPosition();
    self.advance(); // consume (

    const scratch_start = self.scratch_nodes.items.len;
    defer {
        self.scratch_nodes.items.len = scratch_start;
    }

    // Parse arguments using recursive version for now
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
    // Use the labeled switch version for zero allocations and better performance
    return self.parseExprWithBpIterativeLabeledSwitch(0);
}

/// Parse an expression with precedence
pub fn parseExprWithBp(self: *Parser, min_bp: u8) Error!Node.Idx {
    return self.parseExprWithBpIterativeLabeledSwitch(min_bp);
}

fn parsePrimaryExpr(self: *Parser) Error!Node.Idx {
    return self.processPrimaryExprIterative();
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
        // Empty {} defaults to empty record
        return try self.ast.appendNode(self.gpa, start_pos, .record_literal, .{ .nodes = nodes_idx });
    }

    const scratch_start = self.scratch_nodes.items.len;
    defer {
        self.scratch_nodes.items.len = scratch_start;
    }

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
                    const rest_node = try self.ast.appendNode(self.gpa, dot_pos, .double_dot_lc, .{ .ident = id });
                    try self.scratch_nodes.append(self.gpa, rest_node);
                }
            } else {
                // Just .. without an identifier - create an underscore as placeholder
                const rest_node = try self.ast.appendNode(self.gpa, dot_pos, .underscore, .{ .src_bytes_end = dot_pos });
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
                // If we already know it's a record, parse as expression
                const expr = try self.parseExpr();
                break :blk expr;
            } else blk: {
                // Otherwise parse as statement to allow both blocks and records
                const stmt = try self.parseStmt();
                break :blk stmt;
            };
            
            if (elem_opt) |elem| {
                // In lambda args, { a, b } is shorthand for { a: a, b: b }
                // If we're in lambda args and have a bare identifier, convert it
                const final_elem = if (self.is_in_lambda_args and self.ast.tag(elem) == .lc) blk: {
                    // Convert bare identifier to field: identifier
                    const elem_pos = self.ast.start(elem);
                    const binop_idx = try self.ast.appendBinOp(self.gpa, elem, elem);
                    break :blk try self.ast.appendNode(self.gpa, elem_pos, .binop_colon, .{ .binop = binop_idx });
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

    // DEBUG: Check what token we're about to parse
    const next_token = self.peek();
    if (next_token == .OpBar) {
        // We're about to parse a lambda!
        // This should be handled by parseExpr -> labeled switch -> parseLambda
        // If parseExpr fails here, there's a problem
    }

    const first = self.parseExpr() catch |err| {
        // If we get an error parsing a lambda, it's likely the comma issue
        if (next_token == .OpBar) {
            // This is the lambda parsing issue - return a dummy node for now
            const dummy = try self.ast.appendNode(self.gpa, start_pos, .malformed, .{ .malformed = .expr_unexpected_token });
            return dummy;
        }
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

        self.expect(.CloseRound) catch {
            // Always use current position for error
            const error_pos = self.currentPosition();
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

    // Make absolutely sure we're at a |
    if (self.peek() != .OpBar) {
        return self.pushMalformed(.expr_unexpected_token, start_pos);
    }

    self.advance(); // consume |

    const scratch_start = self.scratch_nodes.items.len;
    defer {
        self.scratch_nodes.items.len = scratch_start;
    }

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

    const params = self.scratch_nodes.items[scratch_start..];

    if (false) { // Temporarily disable the no-args case
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
